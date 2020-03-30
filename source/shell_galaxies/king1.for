       PROGRAM KING1
C++::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C  This program, from [SAUL.PROGS.SOURCE]ISOXFIT and PROFILE, reads a
C  profile and then fits an surface brightness profile (broadened
C  by the seeing) consistent with a King model
C
C  All the fit is in LOG10/LOG10 and the output is converted in RAD/MAGNITUDE
C
C JLP Version of 10-10-86
C--::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	PARAMETER (IDIM=6000)
	PARAMETER (IDIM2=10000)		!For KING's model...
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 RHO(IDIM2),PHI(IDIM2),RAD(IDIM2),DPHI(IDIM2)
	REAL*8 S(IDIM),YY(IDIM),K(IDIM),RAD1(IDIM),
     1	SPLINE(IDIM),LOGRAD(IDIM),
     1	XC(2),BL(2),BU(2),
     1	RADIUS(IDIM),PROF(IDIM),
     1	WK(IDIM),LOGR(IDIM),LOGPROF(IDIM)
	REAL*4 X1PLOT(IDIM),X2PLOT(IDIM),Y1PLOT(IDIM),Y2PLOT(IDIM)
	REAL*4 X3PLOT(IDIM),Y3PLOT(IDIM)
	INTEGER IWK(5)
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,ANS*1,PLOTDEV*32
	CHARACTER NAME*60,COMMENTS*80
 
	COMMON/KING/RHO,PHI,RAD,NINDEX,DPHI
	COMMON/KING3/W0,RTIDAL,RHO0
	COMMON/SPLINE/ SPLINE,NCAP7,K
	COMMON/NAG/ NSTART,LOGR,LOGPROF,ITERATION
10	FORMAT(A)
 
C  Reads the profile with 2 possible formats.
C
	PRINT *,' FORMAT :  1. (NPTS,X,Y)      2. (NPTS,X,Y,VNUMB)  ?'
	READ(5,*) IFORMT
1022	PRINT *,' NAME OF THE PROFILE ?'
	READ(5,10) NAME
	PRINT *,' CONSTANT YOU WANT TO SUBTRACT TO THE INPUT PROFILE ',
     1	' (SKY LEVEL) ?'
	READ(5,*) SKY
	PRINT *,' ZERO POINT FOR THE MAGNITUDES ? (per squared arcsecond)'
	READ(5,*) CMAG
	OPEN (9,FILE=NAME,STATUS='OLD',ERR=1022)
	READ(9,*)NPOINTS
	
	IF(IFORMT.EQ.1)THEN
	  DO I=1,NPOINTS
	  READ (9,*,END=11) RADIUS(I),PROF(I)
	  PROF(I)=PROF(I)-SKY
	  END DO
	ELSE
	  DO I=1,NPOINTS
	  READ (9,*,END=11) RADIUS(I),PROF(I),WORK
	  PROF(I)=PROF(I)-SKY
	  END DO
	ENDIF
 
	GO TO 111
11	NPOINTS=I-1
	PRINT *,' WARNING : LESS POINTS THAN EXPECTED !!!!'
111	PRINT *,NPOINTS,' POINTS RECEIVED'
	PRINT *,'PROFILE FROM ',RADIUS(1),' ARCSECONDS TO ',
     1	RADIUS(NPOINTS),' ARCSECONDS'
 
C Conversion in Log/Log :
 
      DO I=1,NPOINTS
	LOGR(I)=DLOG10(RADIUS(I))
	X1PLOT(I)=LOGR(I)
	   IF(PROF(I).LE.0.D0)THEN
	   NPOINTS=I-1
	   PRINT *,' NEGATIVE VALUES !'
	   PRINT *,' PROFILE TRONCATED AT R= ',RADIUS(I-1),' ARCSECONDS'
	   GO TO 12
	   ENDIF
        LOGPROF(I)=DLOG10(PROF(I))
	Y1PLOT(I)=-2.5D0*LOGPROF(I)+CMAG
      END DO
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C  For a given value of W0, use least squares to find the best values of
C  RCORE and E0 to fit the data over a given range.
C
C   Enter W0 plus guesses for E0 and RCORE
C
12      WRITE (6,334)
 334  FORMAT(/' Enter sigma of the seeing: ',$)
      READ (5,*) SIGMA
C
      WRITE (6,335)
335	FORMAT(/' Enter guesses for B0 (central mag) ',
     1	'and RC (core radius): ',/,
     1	' (This guess must be good for RC in',
     1	' order to correct for the seeing effects')
	READ(5,*) B0,RCORE
	XC(1)=(CMAG-B0)/2.5
	XC(2)=DLOG10(RCORE)
	PRINT *,' XC(1),XC(2)',XC(1),XC(2)
	WRITE (6,3355)
3355	FORMAT(/,' Enter value of W0 (central potential): ',$)
	READ (5,*) W0
C
C
	WRITE (6,339)
339	FORMAT(' Enter start radius and end radius for fitting: ',$)
	READ (5,*) RSTART,RREND
	DO I=1,NPOINTS
	IF(RSTART.LE.RADIUS(I)) GO TO 22
	END DO
22	NSTART=I
	DO I=NPOINTS,1,-1
	  IF(RREND.GE.RADIUS(I)) GO TO 23
	END DO
23	NBINS=I
	PRINT *,NBINS,' POINTS FOR THE FIT IN THE PROFILE'
C
C NPTS = Number of computed points for the model
C
	PRINT *,' NUMBER OF POINTS FOR THE MODEL (310?) ?'
	READ(5,*) NPTS
 
C Setting up the radius array (from 1.E-2 to 1.E+3 or so)
C for the model :
	RAD(1)=0.D0
	DO I=2,NPTS
	WORK=-2.0D0 + 5.D0*FLOAT(I)/FLOAT(NPTS)
	RAD(I)=10.D0**WORK
	END DO
 
	PRINT *,' Calculating King model now...'
	CALL KING(NPTS)
C
	PRINT *,' MODEL CALCULATED '
	XX=RADIUS(NBINS)/RCORE
	IF(XX.GT.RTIDAL)THEN
	PRINT *,' WARNING : MAX. RADIUS (PROFILE) IS',
     1	' LARGER THAN RTIDAL (MODEL)'
	ENDIF
C
100	DO I=1,NPTS
	RAD1(I)=RAD(I)*RCORE                  ! REMEMBER: RAD = r/rcore
	END DO                                !             R = r
 
C Projection onto the plane of the sky
C (scaled radii)
	CALL PROJECT_ABEL(RAD1,RHO,S,NPTS)
	PRINT *,' DENSITY PROJECTED '
 
C
C   Discard the first few points in S which are generally bad.
C
       DO I=10,NPTS
          S(I-9)=S(I)
          RAD1(I-9)=RAD1(I)
          RAD(I-9)=RAD(I)
       END DO
 
       NPTS=NPTS-10    ! Also, the Abel integral leaves S(NPTS) as zero.
 
C Normalization of the model with the first few points :
	CNORM=(S(1)+S(2)+S(3))/3.D0
 
	DO I=1,NPTS
	S(I)=S(I)/CNORM
	X2PLOT(I)=DLOG10(RAD(I))
	Y2PLOT(I)=DLOG10(S(I))
	END DO
C
C Broadenning with a Hankel function
C on scaled radii
	CALL BROADEN(RAD1,S,NPTS,SIGMA)
 
C Possibility of displaying the broadened model
	CHAR1='LOG10(RADIUS)'
	CHAR2='LOG10(SURF. BRIGHT.)'
	TITLE=' BROADENING THE MODEL '
 
	PRINT *,' DO YOU WANT TO DISPLAY THE BROADENED MODEL ?(N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
	 PRINT *,' TITLE ?'
	 READ(5,10) TITLE
	   DO I=1,NPTS
	   Y3PLOT(I)=DLOG10(S(I))
	   END DO
	 PRINT *,' Plotting device: &xterm, ... ?'
         COMMENTS=' '
	 READ(5,10) PLOTDEV
	  CALL DISPLAY2(X2PLOT,Y2PLOT,1,NPTS,X2PLOT,Y3PLOT,
     1	1,NPTS,CHAR1,CHAR2,TITLE,PLOTDEV,'L','L1',NAME,COMMENTS)
	ENDIF
 
C Does a cubic-spline interpolation to the logarithm of S
C on RAD/RCORE
	CALL SPLINELOG_INTER(RAD,S,NPTS,SPLINE,NCAP7,K)
C
C   Now call minimization routine E04GEF (NAG).
C   M: number of points
C   N: number of parameters (here 2 : Io and Rc)
C   XC: parameters [ XC(1)=Io and XC(2)=Rc ]
C
	N=2
	LIWK=5
	LWK=IDIM
	IFAIL=1
	M=NBINS-NSTART+1
	ITERATION=0
	CALL E04GEF (M,N,XC,FSUMSQ,IWK,LIWK,WK,LWK,IFAIL)
C
	RMS=DSQRT(FSUMSQ/FLOAT(M-N))
	WRITE (6,88) FSUMSQ,RMS,ITERATION
88	FORMAT (/,' E04GEF HAS CONVERGED :',/,
     1	' FINAL SUM OF SQUARES = ',F10.3,6X,
     1	' RMS :',E12.4,/,
     1	' AFTER',I5,' ITERATIONS',/)
C
	IF(IFAIL.NE.0) THEN
	WRITE (6,77)IFAIL
77	FORMAT(' ***** WARNING ! ***** IN E04GEF  IFAIL =',I3)
	END IF
C
	WRITE (6,338)
338	FORMAT(/' And the magic numbers are: ')
 
	B0=-2.5D0*XC(1)+CMAG
	WRITE (6,55) B0
55	FORMAT(' B0 = ',F11.3)
	RCORE=10.D0**(XC(2))
	RRTIDAL=RTIDAL*RCORE
	WRITE (6,66) RCORE,RRTIDAL
66	FORMAT(' RCORE = ',F11.4,' RTIDAL = ',F11.4)
 
C******************************************************************
C Displaying the fit
C
	PRINT *,'NPTS (MODEL) =',NPTS
	PRINT *,'NPOINTS (PROFILE) =',NPOINTS
	PRINT *,'NCAP7 (KNOTS FOR SPLINE) =',NCAP7
 
	DO I=1,NPTS
	X2PLOT(I)=DLOG10(RAD(I))+XC(2)
	Y2PLOT(I)=-2.5D0*DLOG10(S(I))+B0
	END DO
 
	CHAR1='LOG10(RADIUS)'
	CHAR2='MAGNITUDE /ARCSEC**2'
	TITLE=' PROFILE AND MODEL '
 
	PRINT *,' DO YOU WANT TO DISPLAY THE FIT? (Y)'
	READ(5,10) ANS
	IF(ANS.NE.'N')THEN
	  PRINT *,' TITLE ?'
	  READ(5,10) TITLE
	  PRINT *,' DEVICE : TEKTRO, CIFER_T5, CANON ?'
	  READ(5,10) PLOTDEV
          COMMENTS=' '
	  CALL DISPLAY2(X1PLOT,Y1PLOT,1,NPOINTS,X2PLOT,Y2PLOT,
     1	1,NPTS,CHAR1,CHAR2,TITLE,PLOTDEV,'L','L2',NAME,COMMENTS)
	ENDIF
 
	PRINT *,' DO YOU WANT TO STORE THE CURVE IN A FILE ? (Y)'
	READ(5,10) ANS
	IF(ANS.NE.'N')THEN
	KCURVE=2
	CALL STORE_GRAPH(X1PLOT,Y1PLOT,1,NPOINTS,X2PLOT,Y2PLOT,
     1	1,NPTS,KCURVE)
	ENDIF
 
C********************************************************************
C Now displaying the residuals
 
C Computing the residuals :
 
	DO I=NPOINTS,1,-1
	  IF(RTIDAL.GT.RADIUS(I)) GO TO 900
	END DO
900	NMAX=I-1
 
	DO I=1,NMAX
	XX=X1PLOT(I)-XC(2)
	LEFT=1
	IFAIL=1
C Calling E02BCF (NAG) for the abscissa XX and getting the ordinate in YY(1)
	CALL E02BCF(NCAP7,K,SPLINE,XX,LEFT,YY,IFAIL)
	IF(IFAIL.NE.0) PRINT 45,IFAIL
45	FORMAT(' ***** WARNING ! ***** IN E02BCF  IFAIL =',I3)
	WORK=B0-2.5D0*YY(1)
	Y2PLOT(I)=Y1PLOT(I)-WORK
	END DO
 
	CHAR1='LOG10(RADIUS)'
	CHAR2='PROFILE - MODEL'
	TITLE=' RESIDUALS '
 
	PRINT *,' DO YOU WANT TO DISPLAY THE RESIDUALS ? (Y)'
	READ(5,10) ANS
	IF(ANS.NE.'N')THEN
	  DO I=1,NMAX
	  Y3PLOT(I)=0.0
	  END DO
	  PRINT *,' TITLE ?'
	  READ(5,10) TITLE
	  PRINT *,' DEVICE : TEKTRO, CIFER_T5, CANON ?'
	  READ(5,10) PLOTDEV
          COMMENTS=' '
	  CALL DISPLAY2(X1PLOT,Y2PLOT,1,NMAX,X1PLOT,Y3PLOT,1,NMAX,
     1	CHAR1,CHAR2,TITLE,PLOTDEV,'L','L1',NAME,COMMENTS)
	ENDIF
 
	PRINT *,' DO YOU WANT TO STORE THIS CURVE IN A FILE ? (Y)'
	READ(5,10) ANS
	IF(ANS.NE.'N')THEN
	KCURVE=1
	CALL STORE_GRAPH(X1PLOT,Y2PLOT,1,NMAX,X1PLOT,Y2PLOT,
     1	1,NPOINTS,KCURVE)
	ENDIF
 
	PRINT *,' DO YOU WANT ANOTHER TRY ? (Y)'
	READ(5,10) ANS
	IF(ANS.NE.'N')GO TO 12
 
 
	CLOSE(9)
	STOP
	END
C
C_________________________________________________________________________
 
      SUBROUTINE LSFUN2 (M,N,XC,FVECC,FJACC,LJC)
C--------------------------------------------------------------------------
C     Subroutine to produce the function value and the first-order
C     partial derivatives of the function to be minimized by E04GEF
C
C	M: Number of points
C	N: Number of parameters
C	FVECC: Residuals
C	FJACC: Jacobian matrix
C--------------------------------------------------------------------------
	PARAMETER (IDIM=6000)
	REAL*8 XC(N),SPLINE(IDIM),FVECC(M),
     1	FJACC(M,LJC),K(IDIM),S(5),X,
     1	LOGR(IDIM),LOGPROF(IDIM)
	INTEGER M,N,IFAIL,NCAP7,LEFT,NSTART
C
	COMMON/SPLINE/SPLINE,NCAP7,K
	COMMON/NAG/NSTART,LOGR,LOGPROF,ITERATION
C
	ITERATION=ITERATION+1
      DO J=NSTART,NSTART+M-1
           X=LOGR(J)-XC(2)
C  i.e. X = LOG10( RAD/Rcore )
C Calling E02BCF (NAG):
           LEFT=0
           IFAIL=0
           CALL E02BCF(NCAP7,K,SPLINE,X,LEFT,S,IFAIL)
C
           FVECC(J-NSTART+1)=LOGPROF(J)-XC(1)-S(1)
           FJACC(J-NSTART+1,1)=-1.0D0
           FJACC(J-NSTART+1,2)=S(2)
       END DO
       RETURN
       END
C
C-------------------------------------------------------------------
	include 'jlpsub:king_model.for'
C In Project.for there is "Splinelogfit" (No common blocks)
C	include 'jlpsub:project.for'
