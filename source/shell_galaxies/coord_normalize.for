C++********************************************************************
C Program COORD_NORMALIZE to
C normalize lists of coordinates generated with GRINNEL or XYCURA
C Scale=1., centre=(0.,0.), north at 90 degrees.
C (For shell coordinates, to be used before EFIT6)
C
C JLP  Version of 20-01-88
C--********************************************************************
	PROGRAM COORD_NORMALIZE
	PARAMETER (IDIM=1000,IDIM1=2000)
	REAL*4 XTT(IDIM1),YTT(IDIM1),XOUT(IDIM1),YOUT(IDIM1)
	REAL*4 WORKX2(IDIM),WORKY2(IDIM)
	CHARACTER IN_FILE*40,IN_COMMENTS*80,OUTNAME*40,NAME*70,ANS*1
	LOGICAL INVERSE
 
10	FORMAT(A)
 
	PRINT 111
111	FORMAT(' Normalization of a set of coordinates',/,
     1	' Scale=1., centre=(0.,0.), north at 90 degrees')
 
C***************************
C Entering the positions :
C***************************
	PRINT 112
112	FORMAT(' ARE YOU WORKING WITH :',/,
     1	' 1. A SET OF SHELLS FROM "XYCURA1"',/,
     1	' 2. A SET OF SHELLS FROM "GRINNEL"',/,
     1	' 3. A SINGLE SHELL  ?')
	READ(5,*) IOPT
 
	PRINT *,' ENTER THE POSITION FILE :'
	 IF(IOPT.EQ.1)THEN
C Option 1 for the format :
	   CALL RREADFILE(XTT,YTT,NTT,WORKX2,WORKY2,I,IDIM1,
     1                    IN_FILE,IN_COMMENTS,1)
	 ELSEIF(IOPT.EQ.2)THEN
C Option 6 for the format :
	   CALL RREADFILE(XTT,YTT,NTT,WORKX2,WORKY2,I,IDIM1,
     1                    IN_FILE,IN_COMMENTS,6)
	 ELSE
C Option 0 (=unknown) for the format :
	    CALL RREADFILE(X,Y,NPOINT,WORKX2,WORKY2,I,IDIM1,
     1                     IN_FILE,IN_COMMENTS,0)
	 ENDIF
 
C Output file compatible with SHELL_PROC
	 PRINT *,' (THE OUTPUT WILL BE WRITTEN WITH THE "GRINNEL" FORMAT)'
302	 PRINT *,' NAME OF THE OUTPUT FILE ?'
	 READ(5,10) OUTNAME
	 OPEN(1,FILE=OUTNAME,STATUS='NEW',ERR=302)
 
C Entering the parameters for the transformation :
 
	PRINT *,' CENTRE OF THE GALAXY : X0, Y0 ?'
	READ(5,*) X0,Y0
 
	PRINT *,' Scale in arcsec/pixel ?'
	READ(5,*) SCALE
	
	PRINT *,' Position angle of the North in the frame ? (Degrees, x=0.)'
	READ(5,*) ANORTH
 
	PRINT *,' Is the image orientated as WEST-NORTH-EAST ? (Y)'
	READ(5,10) ANS
	INVERSE=(ANS.EQ.'n'.OR.ANS.EQ.'N')
 
	CALL CNRZ_TRANSFORM(XTT,YTT,NTT,X0,Y0,SCALE,ANORTH,INVERSE,
     1	XOUT,YOUT)
 
C Writing a header:
	 PRINT *,' NAME OF THE ORIGINAL FILE (FOR THE TITLE)?'
	 READ(5,10) NAME
	WRITE(1,98) NAME,X0,Y0,ANORTH,SCALE
98	FORMAT(' FILE :',A80,/,
     1	' CENTRE:',2(X,G10.3),' ANORTH: ',G10.3,
     1	' SCALE: ',G10.3)
 
C Writing the output on the file
	DO I=1,NTT
	  WRITE(1,*)NINT(XOUT(I)),NINT(YOUT(I)),0.0
	END DO
 
C****** End ********
999	CLOSE(1)
	PRINT *,' '
 
	STOP
	END
C********************************************************************
C Subroutine CNRZ_TRANSFORM
C normalize lists of coordinates
C Scale=1., centre=(0.,0.), north at 90 degrees.
C
C********************************************************************
	SUBROUTINE CNRZ_TRANSFORM(XTT,YTT,NTT,X0,Y0,SCALE,
     1	ANORTH,INVERSE,XOUT,YOUT)
	PARAMETER (IDIM=1000,IDIM1=2000)
	REAL*4 XTT(IDIM1),YTT(IDIM1),XOUT(IDIM1),YOUT(IDIM1)
	LOGICAL INVERSE
 
10	FORMAT(A)
 
C Normalizing the input list :
	DO I=1,NTT
	  IF(XTT(I).NE.-1000..OR.YTT(I).NE.-1000.)THEN
	    XTT(I)=(XTT(I)-X0)*SCALE
	    YTT(I)=(YTT(I)-Y0)*SCALE
	  ENDIF
	END DO
 
C Rotation of (90.-ANORTH) :
	  COSA=COSD(90.-ANORTH)
	  SINA=SIND(90.-ANORTH)
	  DO I=1,NTT
	    IF(XTT(I).NE.-1000..OR.YTT(I).NE.-1000.)THEN
	      XOUT(I)=COSA*XTT(I)-SINA*YTT(I)
	      YOUT(I)=SINA*XTT(I)+COSA*YTT(I)
	    ELSE
	      XOUT(I)=-1000.
	      YOUT(I)=-1000.
	    ENDIF
	  END DO
 
C Symmetry relative to the north axis:
	IF(INVERSE)THEN
	  DO I=1,NTT
	    IF(XOUT(I).NE.-1000..OR.YOUT(I).NE.-1000.)THEN
	      XOUT(I)=-1.*XOUT(I)
	    ENDIF
	  END DO
	ENDIF
 
	RETURN
	END
C**************************************************************************
