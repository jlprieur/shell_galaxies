       PROGRAM HUBBLE
C++:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C  Program HUBBLE
C  (From KING1)
C  Reads a profile and then fits a surface brightness profile (broadened
C  by the seeing function) following a modified Hubble law in density
C  (WARNING : Here I take the space density and not the surface brightness !)
C
C   Option 1 :
C   RHO=1/(1 + RAD/RC)**ETA
C   Option 2 :
C   RHO=1/(1 + (RAD/RC)**2)**ETA
C   Option 3 :
C   PHI=1/(1 + (RAD/RC)**2)**ETA
C
C   Fit in Log10(rad)/Log10(surf. brightness), output in Log10(rad)/Magnitude
C
C JLP Version of 18-11-86
C--:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	PARAMETER (IDIM=1000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 RHO(IDIM),RAD(IDIM),RAD1(IDIM),
     1	S(IDIM),K(IDIM),SPLINE(IDIM),
     1	XC(2),Y(IDIM),RADIUS(IDIM),PROF(IDIM),
     1	WK(5000),LOGR(IDIM),LOGPROF(IDIM)
	REAL*4 X1PLOT(IDIM),X2PLOT(IDIM),Y1PLOT(IDIM),Y2PLOT(IDIM)	
	REAL*4 X3PLOT(IDIM),Y3PLOT(IDIM)
	INTEGER IWK(5)
	CHARACTER NAME*60,COMMENTS*80,ANS*1
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,PLOTDEV*32
C
	COMMON/MODEL/S0,ETA,RHO,RAD
	COMMON/SPLINE/SPLINE,NCAP7,K
	COMMON/NAG/NSTART,LOGR,LOGPROF,ITERATION
	COMMON/CONSTANT/PI,PI1
 
C Common block with NEWPLOT
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
 
	PI=3.14159265358979323846D0
	PI1=DSQRT(2.D0*PI)
10	FORMAT(A)
C
C  Reads the profile.
C
	PRINT *,' FORMAT OF THE INPUT FILE :'
	PRINT *,' 1. NPTS,X,Y   2. NPTS,X,Y,VNUMBER'
	READ(5,*) IFORMT
1022	PRINT *,' NAME OF THE PROFILE ?'
	READ(5,10) NAME
	PRINT *,' CONSTANT YOU WANT TO SUBTRACT TO THE INPUT PROFILE',
     1	' (SKY LEVEL) :'
	READ(5,*) SKY
	PRINT *,' ZEROPOINT FOR THE MAGNITUDES (/ARCSEC**2) :'
	READ(5,*) CMAG
	OPEN (9,FILE=NAME,STATUS='OLD',ERR=1022)
	READ(9,*)NBINS
	
	IF(IFORMT.EQ.1)THEN
	   DO I=1,NBINS
            READ (9,*,END=11) RADIUS(I),PROF(I)
	    PROF(I)=PROF(I)-SKY
	   END DO
	ELSE
	   DO I=1,NBINS
            READ (9,*,END=11) RADIUS(I),PROF(I),WORK
	    PROF(I)=PROF(I)-SKY
	   END DO
	ENDIF
 
 
	GO TO 111
11	NBINS=I-1
	PRINT *,' WARNING : LESS POINTS THAN EXPECTED !!!!'
111	PRINT 112,NBINS,RADIUS(1),RADIUS(NBINS)
112	FORMAT(2X,I5,' POINTS RECEIVED',/,' PROFILE FROM ',
     1	F12.3,' ARCSECONDS TO ',F12.3,' ARCSECONDS',/)
 
C  Gets the observed profile ready for the fit (conversion in Log/Log)
 
	DO I=1,NBINS
	LOGR(I)=DLOG10(RADIUS(I))
	  IF(PROF(I).LE.0.D0)THEN
	  NBINS=I-1
	  PRINT 113,RADIUS(I-1)
113	  FORMAT(' NEGATIVE VALUES !',/,
     1	' PROFILE TRONCATED AT R= ',F12.3,' ARCSECONDS',/)
	  GO TO 12
	  ENDIF
        LOGPROF(I)=DLOG10(PROF(I))
	END DO
 
12	DO J=1,NBINS
	X1PLOT(J)=LOGR(J)
	Y1PLOT(J)=-2.5D0*LOGPROF(J)+CMAG
	END DO
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C  For a given value of ETA, use least squares to find the best values of
C  RCORE and E0 to fit the data over a given range.
C
C   Enter ETA plus guesses for E0 and RCORE
C
14	PRINT 15
15	FORMAT(' MENU :',/,
     1	' 1. RHO= (1+ R/RC)**(-ETA)',/,
     1	' 2. RHO= (1+ (R/RC)**2 )**(-ETA)',/,
     1	' 3. PHI= (1+ R/RC)**(-ETA)',/,
     1	' ENTER THE OPTION YOU WANT : ',$)
	READ(5,*) IOPT
	WRITE (6,334)
334	FORMAT(/' Enter sigma of seeing: ',$)
	READ (5,*) SIGMA
C
	WRITE (6,335)
335	FORMAT(/' Enter guesses for B0 and core radius: ',$)
	READ (5,*) B0,RCORE
	XC(1)=(CMAG-B0)/2.5
	XC(2)=DLOG10(RCORE)
	PRINT *,'XC(1) , XC(2) : ',XC(1),XC(2)
	WRITE (6,3355)
3355	FORMAT(' Enter value of ETA : ',$)
	READ (5,*) ETA
C
C Entering the limiting indices :
	PRINT *,' Do you want to display the input profile'
	PRINT *,' to enter the limiting points for the fit? (N)'
	READ(5,10) ANS
 
C Dislaying the profile in Log/Log
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
	CHAR1='LOG10(RADIUS)  (ARCSEC)'
	CHAR2='MAGNITUDE/ARCSEC**2'
	TITLE='ENTER STARTING AND ENDING POINTS FOR THE FIT'
	NOUT=0
	PRINT *,' Device: &xterm, ... ?'
	READ(5,10) PLOTDEV
	CALL DISPLAY1(X1PLOT,Y1PLOT,1,NBINS,CHAR1,CHAR2,TITLE,PLOTDEV)
	RSTART=DBLE(EXP(XOUT(1)))
	REND=DBLE(EXP(XOUT(2)))	
	PRINT 331,RSTART,REND
331	FORMAT(' RSTART =',F12.3,'   REND =',F12.3,/)
	ENDIF
 
C Or entering the indices directly :
	IF(ANS.EQ.'N'.OR.NOUT.EQ.0)THEN
	WRITE (6,339)
339	FORMAT(' Enter start radius and end radius for fitting: ')
	READ (5,*) RSTART,REND
	ENDIF
 
C--------------------------------------------------------------
C Finding the indices corresponding to the radii :
 
	I=1
94	IF(RADIUS(I).GE.RSTART.OR.I.GE.NBINS) GOTO 95
	I=I+1
        GOTO 94
95	NSTART=I
 
	I=NBINS
96	IF(RADIUS(I).LE.REND.OR.I.LT.1)GOTO 97
	I=I-1
	GOTO 96
97	NEND=I
 
	PRINT *,' NSTART, NEND : ',NSTART,NEND
 
C-----------------------------------------------------------------
C Calculating the model
 
C Entering the number of points for the model
	PRINT *,' NUMBER OF POINTS FOR THE MODEL ? (MINI=100)'
	READ(5,*) NPTS
 
C Generates the radius array with a logarithmic scale
C From 0.01*RCORE TO 1000.*RCORE (or so...)
 
      DO I=1,NPTS
         WORK=-2.0D0+5.0D0*FLOAT(I)/FLOAT(NPTS)   ! Set up radius array.
         RAD(I)=10**WORK
      END DO
C
      PRINT *,' '
      PRINT *,' Calculating the model...'
 
      CALL HUBBLE1(NPTS,IOPT)
 
C Getting the model scaled to the guessed value of RCORE
	DO I=1,NPTS
	RAD1(I)=RAD(I)*RCORE
	END DO
C
C  Put the density through the Abel integral to get the surface brightness
C in scaled radii
	CALL PROJECT_ABEL(RAD1,RHO,S,NPTS)
 
C Normalization with the first few points (normalization in the center
C before broadening)
	CNORM=(S(2)+S(3)+S(4))/3.D0
	DO I=1,NPTS
	S(I)=S(I)/CNORM
	X2PLOT(I)=DLOG10(RAD1(I))
	Y2PLOT(I)=DLOG10(S(I))
	END DO
C
C   Broadens the surface brightness using the Hankel transform.
C in scaled radii
 
	CALL BROADEN(RAD1,S,NPTS,SIGMA)
 
C-----------------------------------------------------------------
	PRINT *,' DO YOU WANT TO DISPLAY THE MODEL'
	PRINT *,'   AND ITS BROADENED FORM ?(N)'
	READ(5,10) ANS
 
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
	  DO I=1,NPTS
	  Y3PLOT(I)=DLOG10(S(I))
	  END DO
	 CHAR1='LOG10(RADIUS)  (ARCSEC)'
	 CHAR2='LOG10(SURF. BRIGHTNESS)'
	 PRINT *,' TITLE ?'
	 READ(5,10) TITLE
	 PRINT *,' DEVICE: &xterm, ... ?'
	 READ(5,10) PLOTDEV
         COMMENTS=' '
	 CALL DISPLAY2(X2PLOT,Y2PLOT,1,NPTS,X2PLOT,Y3PLOT,
     1	1,NPTS,CHAR1,CHAR2,TITLE,PLOTDEV,'L','L2',NAME,COMMENTS)
	ENDIF
 
C   Fits a spline to the surface density S of the calculated
C   profile as a function of RAD = LOG10(r/rcore).
 
999	PRINT *,' Calling Splinelogfit :'
	CALL SPLINELOGFIT(RAD,S,NPTS,SPLINE,NCAP7,K)
 
C   Now call minimization routine E04GEF (NAG).
C   M: number of points
C   N: number of parameters (here 2 : Io and Rc)
C   XC: parameters [ XC(1)=Io and XC(2)=Rc ]
C
	N=2
	LIWK=5		!LENTH OF IW
	LWK=5000	!LENTH OF WK
	IFAIL=1
	M=NEND-NSTART+1
	ITERATION=0
	PRINT *,' CALLING "E04GEF"'
	CALL E04GEF (M,N,XC,FSUMSQ,IWK,LIWK,WK,LWK,IFAIL)
	RMS=DSQRT(FSUMSQ/(M-N))
	WRITE (6,88) FSUMSQ,ITERATION,RMS
88	FORMAT (' FINAL SUM OF SQUARES = ',E12.5,/,
     1	' AFTER',I5,' ITERATIONS.  RMS =',E12.5)
C
	IF(IFAIL.NE.0) THEN
	WRITE (6,77) IFAIL
77	FORMAT (' ***** WARNING ***** : IFAIL =',I3)
	END IF
C
	WRITE (6,338)
338	FORMAT(' And the magic numbers are: ')
	B0=-2.5*XC(1)+CMAG
	WRITE (6,55) B0
55	FORMAT(' B0 = ',F11.4)
	RCORE=10.D0**XC(2)
	WRITE (6,66) RCORE
66	FORMAT(' RCORE = ',F11.4)
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C DISPLAYS THE CURVES
C
	PRINT *,'NCAP7=',NCAP7
 
	DO I=1,NPTS
	X2PLOT(I)=DLOG10(RAD(I))+XC(2)
	Y2PLOT(I)=-2.5D0*DLOG10(S(I))+B0
	END DO
C
	PRINT *,' DO YOU WANT TO DISPLAY THE TWO CURVES ? (Y)'
	READ(5,10) ANS
 
	IF(ANS.NE.'N')THEN
	CHAR1='LOG10(RADIUS)  (ARCSEC)'
	CHAR2='MAGNITUDE/ARCSEC**2'
	PRINT *,'TITLE ?'
	READ(5,10) TITLE
	PRINT *,' DEVICE : TEKTRO, CIFER_T5, CANON ?'
	READ(5,10) PLOTDEV
        COMMENTS=' '
	CALL DISPLAY2(X1PLOT,Y1PLOT,1,NBINS,X2PLOT,Y2PLOT,
     1	1,NPTS,CHAR1,CHAR2,TITLE,PLOTDEV,'L','L2',NAME,COMMENTS)
	ENDIF
 
C
C   Stores the graph in an output file
C
	PRINT *,' DO YOU WANT TO STORE THE TWO CURVES (FILE)? (Y)'
	READ(5,10) ANS
 
	IF(ANS.NE.'N')THEN
	KCURVE=2
	CALL STORE_GRAPH(X1PLOT,Y1PLOT,1,NBINS,X2PLOT,Y2PLOT,
     1	1,NPTS,KCURVE)
	ENDIF
C***********************************************************************
C Displaying the residuals :
	DO I=NBINS,1,-1
	IF(RADIUS(I).LT.RAD(NPTS))GO TO 900
	END DO
900	NMAX=I-1
 
	DO I=1,NMAX
C Calling E02BBF (NAG):
	IFAIL=1
C Determining the image of LOG10(RADIUS(I)/RCORE) with E02BBF :
	X=X1PLOT(I)-XC(2)
	CALL E02BBF (NCAP7,K,SPLINE,X,RESULT,IFAIL)
	IF(IFAIL.NE.0)PRINT 576,IFAIL
576	FORMAT(' WARNING : IN E02BBF  IFAIL=',I3)
	WORK=-2.5D0*RESULT+B0
	Y2PLOT(I)=Y1PLOT(I)-WORK
	END DO
 
	PRINT *,' DO YOU WANT TO DISPLAY THE RESIDUALS ? (Y)'
	READ(5,10) ANS
 
	IF(ANS.NE.'N')THEN
	CHAR1='LOG10(RADIUS) (ARCSEC)'
	CHAR2='PROFILE-MODEL (MAG/ARCSEC**2)'
	DO I=1,NMAX
	Y3PLOT(I)=0.0
	END DO
	PRINT *,'TITLE ?'
	READ(5,10) TITLE
	PRINT *,' DEVICE : TEKTRO, CIFER_T5, CANON ?'
	READ(5,10) PLOTDEV
        COMMENTS=' '
	CALL DISPLAY2(X1PLOT,Y2PLOT,1,NMAX,X1PLOT,Y3PLOT,
     1	CHAR1,CHAR2,TITLE,PLOTDEV,'L','L1',NAME,COMMENTS)
	ENDIF
 
C
C   Stores the graph in an output file
C
	PRINT *,' DO YOU WANT TO STORE THE TWO CURVES (FILE)? (Y)'
	READ(5,10) ANS
 
	IF(ANS.NE.'N')THEN
	KCURVE=1
	CALL STORE_GRAPH(X1PLOT,Y2PLOT,1,NMAX,X1PLOT,Y2PLOT,
     1	1,NMAX,KCURVE)
	ENDIF
 
C***********************************************************************
	PRINT *,' DO YOU WANT ANOTHER TRY ? (Y)'
	READ(5,10) ANS
	IF(ANS.NE.'N'.AND.ANS.NE.'n') GO TO 14
C
	CLOSE(9)
	STOP
	END
C---------------------------------------------------------------------
      SUBROUTINE LSFUN2 (M,N,XC,FVECC,FJACC,LJC)
C---------------------------------------------------------------------
C     Subroutine to produce the function value and the first-order
C     partial derivatives of the function to be minimized by E04GEF
C
C	M: Number of points
C	N: Number of parameters
C	FVECC: Residuals
C	FJACC: Jacobian matrix
C	LOGR: Log10(radius) of the input profile
C	LOGPROF: Log10(input profile)
C------------------------------------------------------------------------
      PARAMETER (IDIM=1000)
      REAL*8 XC(N),SPLINE(IDIM),FVECC(M),
     +     FJACC(M,LJC),K(IDIM),S(5),X,
     +     LOGR(IDIM),LOGPROF(IDIM)
      INTEGER M,N,IFAIL,NCAP7,NSTART
C
      COMMON/SPLINE/SPLINE,NCAP7,K
      COMMON/NAG/NSTART,LOGR,LOGPROF,ITERATION
C
	ITERATION=ITERATION+1
 
	DO J=NSTART,NSTART+M-1
	X=LOGR(J)-XC(2)
C  i.e. X = LOG10( RAD/Rcore )
 
C Attempt to avoid a failure in E02BCF when X is outside of the
C boundaries...
	IF(X.LT.K(5).OR.X.GT.K(NCAP7-3))THEN
	PRINT *,' PB WITH E02BCF!! (TRY WITH MORE POINTS)'
	PRINT *,'J=',J,'X=',X,'LOGR(J)=',LOGR(J)
	FVECC(J-NSTART+1)=100.
	FJACC(J-NSTART+1,1)=-1.0D0
	FJACC(J-NSTART+1,2)=+1.0D0
	GO TO 637
	ENDIF
 
C Calling E02BCF (NAG) 
	IFAIL=1
	LEFT=0
	CALL E02BCF (NCAP7,K,SPLINE,X,LEFT,S,IFAIL)
	IF(IFAIL.NE.0)PRINT 576,IFAIL
576	FORMAT(' WARNING : IN E02BCF  IFAIL=',I3)
	FVECC(J-NSTART+1)=LOGPROF(J)-XC(1)-S(1)
	FJACC(J-NSTART+1,1)=-1.0D0
	FJACC(J-NSTART+1,2)=S(2)
 
637	END DO
	RETURN
	END
C-----------------------------------------------------------------------
	SUBROUTINE HUBBLE1(NPTS,IOPT)
C-----------------------------------------------------------------------
C   Generates the array RHO of the space density
C   NPTS = number of points for the calculated profile
C
C   Option 1 :
C   RHO=1/(1 + RAD/RC)**ETA
C   Option 2 :
C   RHO=1/(1 + (RAD/RC)**2)**ETA
C   Option 3 :
C   PHI=1/(1 + (RAD/RC)**2)**ETA
C
C-----------------------------------------------------------------------
	PARAMETER (IDIM=1000)	
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 RHO(IDIM),RAD(IDIM)
C
	COMMON/MODEL/B0,ETA,RHO,RAD
 
 
C   Option 1 :
C   RHO=1/(1 + RAD/RC)**ETA
	IF(IOPT.EQ.1)THEN
	DO I=1,NPTS
	RHO(I)=1.D0/((1.D0+RAD(I))**ETA)
	END DO
	ENDIF
 
C   Option 2 :
C   RHO=1/(1 + (RAD/RC)**2)**ETA
	IF(IOPT.EQ.2)THEN
	DO I=1,NPTS
	RHO(I)=1.D0/((1.D0+RAD(I)**2)**ETA)
	END DO
	ENDIF
 
C   Option 3 :
C   PHI=1/(1 + (RAD/RC)**2)**ETA
	IF(IOPT.EQ.3)THEN
	CTE1=3.D0-2.D0*(1.D0+ETA)
	DO I=1,NPTS
	RR2=RAD(I)*RAD(I)
	RHO(I)=(RR2*CTE1+3.D0)*((1.D0+RR2)**(-2.-ETA))
	END DO
	ENDIF
 
	RETURN
	END
C---------------------------------------------------------------------
c	include 'jlpsub:project.for'
