C--------------------------------------------------------------------
C Subroutine to smooth images with possibility of unsharp masking.
C  Version of 15-01-88
C
C Input :
C INPUT(NX,NY)
C NSM : Width of the smoothing grid (Odd number <19 )
C SIGMA : (If IOP=2)
C UNSHARP : (Logical) Possibility of unsharp masking
C
C Options :
C IOP=2	GAUSSIAN
C IOP=3 "TOP HAT"
C IOP=4 Grid entered by the user
C
C Output :
C OUTPUT(NX,NY)
C--------------------------------------------------------------------
	SUBROUTINE SMOOTH2(INPUT,OUTPUT,NX,NY,IDIM,IOP,GRID,
	1	NSM,SIGMA,UNSHARP,FRACT)
	REAL*4 INPUT(IDIM,*),OUTPUT(IDIM,*)
	REAL*4 GRID(19,19),GRID2(19*19)
	LOGICAL UNSHARP
	CHARACTER ANS*1
 
10	FORMAT(A)
 
C Check if NSM is odd :
	ITEST=(NSM/2)*2
	  IF(ITEST.EQ.NSM)THEN
	  NSM=NSM+1
	  PRINT 23,NSM
23	  FORMAT(' NSM IS NOT ODD, SO I TAKE NSM = ',I3)
	  ENDIF
 
	ICENTRE=(NSM/2)+1
 
C-------------------------------------------------------------
C IOP=2  Gaussian grid:
	IF(IOP.EQ.2)THEN
	  CTE=-2.*SIGMA*SIGMA
 
	  DO J=1,NSM
	    DO I=1,NSM
	      RAD2=FLOAT((J-ICENTRE)**2+(I-ICENTRE)**2)
	      GRID2(I+NSM*J)=EXP(RAD2/CTE)
	    END DO
	  END DO
 
	ENDIF
 
C-------------------------------------------------------------
C IOP=3  "Top Hat" :
	IF(IOP.EQ.3)THEN
	  CALL SMT_TOPHAT(INPUT,OUTPUT,NX,NY,IDIM,NSM,UNSHARP,
	1	FRACT)
	  RETURN
	ENDIF
 
C-------------------------------------------------------------
C IOP=4 Direct input of the grid :
 
C Copy to a smaller array (to accelerate the process)
	IF(IOP.EQ.4)THEN
	  DO J=1,NSM
	   DO I=1,NSM
	    GRID2(I+NSM*J)=GRID(I,J)
	   END DO
	  END DO
	ENDIF
 
C Normalization of the grid:
	SUM=0.
	  DO K=1,NSM*NSM
	    SUM=GRID2(K)+SUM
	  END DO
 
	IF(SUM.NE.0.)THEN
	 DO K=1,NSM*NSM
	   GRID2(K)=GRID2(K)/SUM
	 END DO
	ENDIF
 
C Call SMTH :
	CALL SMTH(INPUT,OUTPUT,NX,NY,IDIM,GRID2,NSM,UNSHARP,FRACT)
 
	RETURN
	END
C******************************************************************
C Subroutine SMTH
C
C Input:
C INPUT(IDIM,IDIM)
C NX,NY
C GRID(NSM,NSM) : Smoothing grid
C NSM : width of the box
C UNSHARP : unsharp option
C FRAC : Fraction of the smoothed image to be subtracted to the original image
C
C Output:
C OUTPUT(IDIM,IDIM)
C******************************************************************
	SUBROUTINE SMTH(INPUT,OUTPUT,NX,NY,IDIM,GRID,NSM,UNSHARP,
	1	FRACT)
	REAL*4 INPUT(IDIM,*),OUTPUT(IDIM,*)
	REAL*4 GRID(NSM,NSM)
	INTEGER IYMIN(4),IYMAX(4),IXMIN(4),IXMAX(4)
	LOGICAL UNSHARP

	ICENTRE=(NSM/2)+1
 
C Check if unsharp masking :
	   IF(UNSHARP)THEN
	    FRACT_IN=1.0
	    FRACT_SUM=-1.0*FRACT
	   ELSE
	    FRACT_IN=0.0
	    FRACT_SUM=1.0
	   ENDIF
 
C  NOW SMOOTH THE ARRAY
C Smoothing the edges :
	 print *,' Smoothing the edges'
 
C Lower edge
	IYMIN(1)=1
	IYMAX(1)=ICENTRE-1
	IXMIN(1)=1
	IXMAX(1)=NX
 
C Upper edge
	IYMIN(2)=NY-ICENTRE+1
	IYMAX(2)=NY
	IXMIN(2)=1
	IXMAX(2)=NX
 
C Left edge
	IYMIN(3)=1
	IYMAX(3)=NY
	IXMIN(3)=1
	IXMAX(3)=ICENTRE-1
 
C Right edge
	IYMIN(4)=1
	IYMAX(4)=NY
	IXMIN(4)=NX-ICENTRE+1
	IXMAX(4)=NX
 
C Loop on the edges :
	DO K=1,4
	 DO J=IYMIN(K),IYMAX(K)
	   JGMIN=MAX(1+ICENTRE-J,1)
	   JGMAX=MIN(ICENTRE+NY-J,NSM)
	  DO I=IXMIN(K),IXMAX(K)
	   IGMIN=MAX(1+ICENTRE-I,1)
	   IGMAX=MIN(ICENTRE+NX-I,NSM)
	   SUM=0.0
	     DO JGRID=JGMIN,JGMAX
	      JC=J+JGRID-ICENTRE
	        DO IGRID=IGMIN,IGMAX
	         IC=I+IGRID-ICENTRE
	         SUM=SUM+GRID(IGRID,JGRID)*INPUT(IC,JC)
	        END DO
	     END DO
 
	    OUTPUT(I,J)=FRACT_IN*INPUT(I,J)+FRACT_SUM*SUM
	
	  END DO
	 END DO
	END DO
 
C Central part :
	print *,' Now the central part'
 
	DO J=ICENTRE,NY-ICENTRE
	  DO I=ICENTRE,NX-ICENTRE
	    SUM=0.0
	     DO JGRID=1,NSM
	      JC=J+JGRID-ICENTRE
	        DO IGRID=1,NSM
	         IC=I+IGRID-ICENTRE
	         SUM=SUM+GRID(IGRID,JGRID)*INPUT(IC,JC)
	        END DO
	     END DO
 
	    OUTPUT(I,J)=FRACT_IN*INPUT(I,J)+FRACT_SUM*SUM
	
	  END DO
	  IF(MOD(J,20).EQ.0)PRINT *,' Smoothed up to line ',J
	END DO
 
 
	RETURN
	END
C******************************************************************
C Subroutine SMT_TOPHAT
C
C Input:
C INPUT(IDIM,IDIM)
C NX,NY
C NSM : width of the box
C UNSHARP : unsharp option
C FRAC : Fraction of the smoothed image to be subtracted to the original image
C
C Output:
C OUTPUT(IDIM,IDIM)
C******************************************************************
	SUBROUTINE SMT_TOPHAT(INPUT,OUTPUT,NX,NY,IDIM,NSM,UNSHARP,
	1	FRACT)
	REAL*4 INPUT(IDIM,*),OUTPUT(IDIM,*)
	INTEGER IYMIN(4),IYMAX(4),IXMIN(4),IXMAX(4)
	LOGICAL UNSHARP
 
	ICENTRE=(NSM/2)+1
 
C Normalization of the grid
C Check if unsharp masking :
	   IF(UNSHARP)THEN
	    FRACT_IN=1.0
	    FRACT_SUM=-1.0*FRACT/FLOAT(NSM*NSM)
	   ELSE
	    FRACT_IN=0.0
	    FRACT_SUM=1.0/FLOAT(NSM*NSM)
	   ENDIF
 
C  NOW SMOOTH THE ARRAY
C Smoothing the edges :
	 print *,' Smoothing the edges'
 
C Lower edge
	IYMIN(1)=1
	IYMAX(1)=ICENTRE-1
	IXMIN(1)=1
	IXMAX(1)=NX
 
C Upper edge
	IYMIN(2)=NY-ICENTRE+1
	IYMAX(2)=NY
	IXMIN(2)=1
	IXMAX(2)=NX
 
C Left edge
	IYMIN(3)=1
	IYMAX(3)=NY
	IXMIN(3)=1
	IXMAX(3)=ICENTRE-1
 
C Right edge
	IYMIN(4)=1
	IYMAX(4)=NY
	IXMIN(4)=NX-ICENTRE+1
	IXMAX(4)=NX
 
C Loop on the edges :
	DO K=1,4
	 DO J=IYMIN(K),IYMAX(K)
	   JCMIN=MAX(J+1-ICENTRE,1)
	   JCMAX=MIN(J+ICENTRE-1,NY)
	  DO I=IXMIN(K),IXMAX(K)
	   ICMIN=MAX(I+1-ICENTRE,1)
	   ICMAX=MIN(I+ICENTRE-1,NX)
	   SUM=0.0
	     DO JC=JCMIN,JCMAX
	        DO IC=ICMIN,ICMAX
	         SUM=SUM+INPUT(IC,JC)
	        END DO
	     END DO
 
	    OUTPUT(I,J)=FRACT_IN*INPUT(I,J)+FRACT_SUM*SUM
	
	  END DO
	 END DO
	END DO
 
C Central part :
	print *,' Now the central part'
 
	DO J=ICENTRE,NY-ICENTRE
	  DO I=ICENTRE,NX-ICENTRE
	    SUM=0.0
	     DO JGRID=1,NSM
	      JC=J+JGRID-ICENTRE
	        DO IGRID=1,NSM
	         IC=I+IGRID-ICENTRE
	         SUM=SUM+INPUT(IC,JC)
	        END DO
	     END DO
 
	    OUTPUT(I,J)=FRACT_IN*INPUT(I,J)+FRACT_SUM*SUM
	
	  END DO
	  IF(MOD(J,20).EQ.0)PRINT *,' Smoothed up to line ',J
	END DO
 
	RETURN
	END
