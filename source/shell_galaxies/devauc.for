C++*******************************************************************
C
C  Program DEVAUC :  to fit De Vaucouleurs' law
C  to surface photometry of elliptical galaxies
C
C  XC(1)= effective surface brightness B0
C  XC(2)= effective radius RE
C
C  B(R)=B0*10.0**{-3.33*[(R/RE)**.25-1]}
C or  -2.5LOG10(I-SKY)+CTMAG = Ie + 8.325 ( (RAD/RE)**1/4 -1 )
C
C JLP
C Version 22-05-91 
C--*******************************************************************
	PROGRAM DEVAUC
	PARAMETER (IDIM=1000)
	IMPLICIT REAL*8 (A-H,O-Z)
	REAL*8 ZMAG(IDIM),ZRAD(IDIM),XC(2)
	REAL*8 W(12000),RAD(IDIM),BRIGHT(IDIM),WORK(IDIM)
	REAL*4 XPLOT(IDIM),YPLOT1(IDIM),YPLOT2(IDIM),YPLOT3(IDIM)
	REAL*8 RMS,ZBI,CTMAG,RMIN,RMAX,SKY
	REAL*8 FSUMSQ
	INTEGER IW(100),NP1,IOP,NPTS,NPTS1
	INTEGER IMIN,IMAX,M,LIW,LW,N,IFAIL
	CHARACTER ANS*1,CHAR1*30,CHAR2*30,TITLE*40
	CHARACTER PLOTDEV*32,BUFFER*80,IN_FILE*40,IN_COMMENTS*80
	COMMON ZMAG,ZRAD
10	FORMAT(A)
 
C------------------------------------------------------------------
C Read the data:
	IOP=0
	CALL DREADFILE(RAD,BRIGHT,NP1,RAD,WORK,IWORK,IDIM,
     1                 IN_FILE,IN_COMMENTS,IOP)
 
C Entering the parameters :
	PRINT *,'Constant for the magnitudes?'
	READ(5,*) CTMAG
	PRINT *,'Give minimum and maximum radii for the fit'
	READ(5,*) RMIN,RMAX
	PRINT *,'Sky level you want to subtract to the input values?'
	READ(5,*) SKY
	PRINT *,'Give initial values for Be (mag) and r_equiv '
	READ(5,*) XC(1),XC(2)
 
C----------------------------------------------------------------------
C NPTS1 : max index value such as BRIGHT(I) .gt. SKY
C 512 : label for the main loop
C
512	NPTS1=NP1
	DO I=1,NP1
	  ZBI=BRIGHT(I)-SKY
	    IF(ZBI.LE.0.)THEN
	      PRINT *,' Profile troncated at R=',RAD(I-1)
	      NPTS1=I-1
	      GO TO 120
	    ELSE
              YPLOT1(I)=-2.5D0*DLOG10(ZBI)+CTMAG
	      IF(I.LE.5)PRINT *,'Rad, Mag',RAD(I),YPLOT1(I)
	    ENDIF
        END DO
 
120	CONTINUE
 
C---------------------------------------------------------------------
C Searching for the limiting indices : IMIN, IMAX
 
	CALL INDEX_MAX8(RAD,NPTS1,RMIN,IMIN)
	IMIN=IMIN+1
	CALL INDEX_MAX8(RAD,NPTS1,RMAX,IMAX)
	NPTS=IMAX-IMIN+1
	PRINT *,'IMIN, IMAX ',IMIN,IMAX
 
C Filling the arrays for the fit
	DO I=IMIN,IMAX
	  II=I-IMIN+1
	  ZMAG(II)=YPLOT1(I)
	  ZRAD(II)=RAD(I)
	END DO
C------------------------------------------------------------------
C Fitting stage :
C
	M=NPTS
	LIW=100
	LW=12000
	N=2
	IFAIL=1
	PRINT *,' FITTING WITH',M,' POINTS'
	CALL E04HFF(M,N,XC,FSUMSQ,IW,LIW,W,LW,IFAIL)
	 IF (IFAIL.NE.0) THEN
           PRINT *,' FAILURE IN E04HFF :  IFAIL = ',IFAIL
 	 END IF
	RMS=DSQRT(FSUMSQ/(M-N))
	WRITE(6,1000) RMS
1000	FORMAT(' SCATTER ABOUT FIT = ',F8.4,' MAGNITUDES  (RMS)')
	WRITE(6,1010) XC(1),XC(2)
1010	FORMAT(' EFFECTIVE SURFACE BRIGHTNESS : ',F8.3,' MAG/SQ ARCSEC',
     1	/,' EFFECTIVE RADIUS : ',F8.3)
 
C--------------------------------------------------------------
C Displaying the curves
C
	PRINT *,'Do you want to display the fit? (y)'
	READ(5,10) ANS
 
	IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN
	   DO I=1,NPTS1
	     XPLOT(I)=RAD(I)**0.25
             YPLOT2(I)=XC(1)+8.325D0*((RAD(I)/XC(2))**0.25-1.0D0)
	   END DO
	  CHAR1='r 1/4   (arcsec.)'
	  CHAR2='magni./sq.arcsec.'
	  PRINT *,' Title ?'
	  READ(5,10) TITLE
	  PRINT *,' Output graphic device: '
	  READ(5,10) PLOTDEV
	  CALL DISPLAY2(XPLOT,YPLOT1,1,NPTS1,XPLOT,YPLOT2,1,NPTS1,
     1	CHAR1,CHAR2,TITLE,PLOTDEV,'L','L2',IN_FILE,IN_COMMENTS)
 
C Saving the data: 
          OPEN(9,NAME='devauc_data.dat',STATUS='UNKNOWN')
	   DO I=1,NPTS1
            WRITE(9,18) XPLOT(I),YPLOT1(I),YPLOT2(I)
18          FORMAT(3(G12.5,1X))
           END DO
          CLOSE(9)
          WRITE(6,*) ' => Data saved in file devauc_data.dat'
	ELSE
	  GOTO 99
	ENDIF
C---------------------------------------------------------------------
C Residuals
 
	PRINT *,'Do you want to display the residuals? (Y)'
	READ(5,10) ANS
 
	IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN
	   DO I=1,NPTS1
	     YPLOT2(I)=YPLOT1(I)-YPLOT2(I)
	     YPLOT3(I)=0.
	   END DO
	  CHAR1=' r 1/4  (arcsec.)'
	  CHAR2='Profile-Model (mag.)'
	  PRINT *,'Title?'
	  READ(5,10) TITLE
	  PRINT *,'Output graphic device: '
	  READ(5,10) PLOTDEV
	  CALL DISPLAY2(XPLOT,YPLOT2,1,NPTS1,XPLOT,YPLOT3,1,NPTS1,
     1	CHAR1,CHAR2,TITLE,PLOTDEV,'L','L2',IN_FILE,IN_COMMENTS)
 
	ENDIF
 
C---------------------------------------------------------------
99	PRINT *,'DO YOU WANT ANOTHER TRY ? (Y)'
	READ(5,10) ANS
	IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN	
	  PRINT *,' GIVE MINIMUM AND MAXIMUM RADII FOR FIT'
	  READ(5,*) RMIN,RMAX
	  PRINT *,' GIVE INITIAL VALUES FOR B0 (MAG) AND Requi '
	  READ(5,*) XC(1),XC(2)
	  PRINT *,' CONSTANT YOU WANT TO SUBTRACT TO THE INPUT VALUES ?'
	  READ(5,*) SKY
	  GO TO 512
	ENDIF
 
      STOP
      END
C_____________________________________________________________________
      SUBROUTINE LSFUN2(M,N,XC,FVECC,FJACC,LJC)
C******************************************************************
C                                                                 *
C  WORKS OUT RESIDUALS AND JACOBIAN MATRIX FOR DE VAUCOULEURS     *
C  LAW FIT                                                        *
C                                                                 *
C******************************************************************
	PARAMETER (IDIM=1000)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 FVECC(M),FJACC(LJC,N),XC(N)
	REAL*8 ZMAG(IDIM),ZRAD(IDIM)
	COMMON ZMAG,ZRAD
	ZM0=XC(1)
	A=XC(2)
	DO I=1,M
        FVECC(I)=ZMAG(I)-ZM0-8.325D0*((ZRAD(I)/A)**0.25-1.0D0)
        FJACC(I,1)=-1.0D0
        FJACC(I,2)=2.08125D0*(ZRAD(I)**0.25)/A**1.25
	END DO
	RETURN
	END
C------------------------------------------------------------------
	SUBROUTINE LSHES2(M,N,FVECC,XC,B,LB)
C*****************************************************************
C                                                                *
C  WORKS OUT HESSIAN MATRIX FOR DE VAUCOULEURS LAW FIT           *
C                                                                *
C*****************************************************************
	PARAMETER (IDIM=1000)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 B(LB),FVECC(M),XC(N)
      REAL*8 ZMAG(IDIM),ZRAD(IDIM)
      COMMON ZMAG,ZRAD
      ZM0=XC(1)
      A=XC(2)
      B(1)=0.0D0
      B(2)=0.0D0
      SUM22=0.0D0
      DO I=1,M
        SUM22=SUM22-FVECC(I)*2.6015625D0*ZRAD(I)**0.25/A**2.25
      END DO
      B(3)=SUM22
      RETURN
      END
C---------------------------------------------------------------
