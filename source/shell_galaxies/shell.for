C++****************************************************************************
C  Program SHELL
C  This program reads a profile, fits a baseline,
C  computes the intensity above this baseline,
C  and fits a model of a spherical shell (broadened
C  by the seeing) with a finite angular extent.
C  Can only be used in interactive mode
C
C  JLP
C Version of 24-01-98
C--****************************************************************************
C Calls : BACKGROUND, SHELL1, DISPLAY2
C******************************************************************************
	PROGRAM SHELL
	PARAMETER (IDIM=2000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 RAD(IDIM),RAD1(IDIM),SS(IDIM),PROF2(IDIM),
     1	PROF1(IDIM),XC(3),PROF(IDIM),VNUMB(IDIM)
	REAL*4 XPLOT1(IDIM),XPLOT2(IDIM),YPLOT1(IDIM),YPLOT2(IDIM)
	REAL*8 Y,XC1,XC2,ANG,ALPHA,RSTART,RREND,PI
	INTEGER*4 NBINS
	CHARACTER PROF_NAME*60,NAME*40,NAME2*40,ANS*1,COMMENTS*80
	CHARACTER CHAR1*30,CHAR2*30,PLOTDEV*32,TITLE1*40
	LOGICAL PROFILE1_FORMAT
C
	COMMON/DATA/RAD,PROF,VNUMB,NBINS,PLOTDEV,NAME
	COMMON/BLOCKA/Y,XC1,XC2,ANG,ALPHA
	COMMON/CONSTANT/PI,PI1
	PI=3.14159265358979323846
	PI1=DSQRT(2.D0*PI)
10	FORMAT(A)
 
	PRINT 84
84	FORMAT(' Program SHELL Version of 24-01-98')
 
	PRINT *,' Device for the display (&xterm)?'
	READ(5,10) PLOTDEV
	IF(PLOTDEV(1:1).EQ.' ')PLOTDEV='&xterm'
 
C	PRINT 85
85	FORMAT(' Possible formats for the input profiles:',/,
     1	'  1: npts, and 2 columns X,Y',/,
     1	'  2: 32 lines, npts, and 3 columns X,Y,Z',
     1	' as created by "PROFILE1"',/,
     1	' are-you working with the second format ? (Y)')
C	READ(5,10) ANS
        ANS='Y'
	PROFILE1_FORMAT=(ANS.NE.'n'.AND.ANS.NE.'N')
 
C Opens a record output file
	PRINT *,' Name of the output catalog:'
	READ(5,10) NAME2
C Opens a record output file
C	OPEN(10,FILE=NAME2,STATUS='UNKNOWN',ACCESS='APPEND')
C "APPEND" is unknown by IBM...
	OPEN(10,FILE=NAME2,STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
	DO I=1,1000000
	 READ(10,10,END=87)BUFFER
	ENDDO
 
C First menu (simplified to force the user to enter a profile):
87	PRINT 81
81	FORMAT(' Menu :',/,
     1	' 1. Input of a profile',/,
     1	' 4. Displaying a grid of models',/,
     1	' 10. Exit',/,
     1	' Enter the option you want:',$)
        GOTO 810

C Recurrent menu:
88	PRINT 82
82	FORMAT(' Menu :',/,
     1	' 1. Input of a profile',/,
     1	' 2. Fitting a background',/,
     1	' 3. Computing a model',/,
     1	' 4. Displaying a grid of models',/,
     1	' 10. Exit',/,
     1	' Enter the option you want:',$)
810	READ(5,*) IOPT
 
C--------------------------------------------------------------------
C  Reads the profile and returns NBINS, RAD, PROF and VNUMB
C
	IF(IOPT.EQ.1)THEN
 
	 CALL READ_PROFILE(PROFILE1_FORMAT,PROF_NAME)
 
	 PRINT *,' Enter title for the graphs:'
	 READ(5,10) TITLE1
 
	 GO TO 88
 
	ENDIF
 
C----------------------------------------------------------------------
C Fitting a background and returns PROF1 (difference), PROF2 (Background)
C and NSTART, NEND (the indices for the display).
 
	IF(IOPT.EQ.2)THEN
 
	 CALL BACKGROUND(PROF1,PROF2,NSTART,NEND,RSTART,RREND,TITLE1,PROF_NAME)
 
	 GO TO 88
	ENDIF
 
C---------------------------------------------------------------------
C  Calculates a model
 
	IF(IOPT.EQ.3)THEN
 
	 PRINT *,' Do you want 1. Gaussian  2. X**(-alpha)  distribution?'
	 READ(5,*) IOPT1
	   IF(IOPT1.EQ.1)THEN
	    WRITE(10,302)
302	    FORMAT(' Gaussian distribution')
	   ELSE
	    WRITE(10,304)
304	    FORMAT(' X**(-alpha) distribution')
	   ENDIF
 
C Conversion into real*4 for the display
	  DO I=1,NEND
	   XPLOT1(I)=RAD(I)
	   YPLOT1(I)=PROF1(I)
	  END DO
 
	 PRINT 301
301	 FORMAT(' Enter the FWHM seeing in arcseconds:')
	 READ(5,*) SIGMA
         SIGMA=SIGMA/2.
C
C  Enter first guess for RS,RG, and RHO0
300	  IF(IOPT1.EQ.1)THEN
            PRINT *,' Gaussian: RHO0 * exp (- (rad - RS)**2 / RG**2)'
	    PRINT *,' Enter the half angular extent (deg), RS, RG and RHO0:'
	    READ(5,*) ANGLE,RS,RG,RHO0
	  ELSE
	    PRINT *,' Enter the half angular extent (deg), RS, n (0.5 ?) and RHO0:'
	    READ(5,*) ANGLE,RS,ALPHA,RHO0
	    ALPHA=-1.*ALPHA
	  ENDIF
 
	 XC(1)=RS
	 XC(2)=-2.D0*RG*RG
C	 XC(3)=RHO0/(RG*PI1)
	 XC(3)=RHO0
C Number of points for the model:
         NPTS = 100
 
C Computes a model :
	 CALL SHELL1(RAD1,NPTS,XC,SS,SIGMA,ANGLE,RSTART,RREND,IOPT1)
 
	  RS=XC(1)
          RG=XC(2)/(-2.D0)
	  RG=DSQRT(RG)
C	  RHO0=XC(3)*RG*PI1
 
C Conversion into real*4 for the display
331	  DO I=1,NPTS
	   XPLOT2(I)=RAD1(I)
	   YPLOT2(I)=RHO0*SS(I)
	  END DO
 
	 CHAR1='Major axis'
	 CHAR2='Surface brightness'
         WRITE(6,305) 2*SIGMA,ANGLE,RS,RG,RHO0
         WRITE(COMMENTS,307) 2*SIGMA,ANGLE,RS,RG,RHO0
307      FORMAT('FWHM=',F4.1,'Ang=',F5.1,'Rs=',F6.2,'Rg=',F6.2,'Rho0=',F6.2)

	 CALL DISPLAY2(XPLOT1,YPLOT1,NSTART,NEND,XPLOT2,
     1	YPLOT2,1,NPTS,CHAR1,CHAR2,TITLE1,PLOTDEV,'L','L2',PROF_NAME,COMMENTS)
 
C	 PRINT *,' Do you want to load this graph into a file? (N)'
C	 READ(5,10) ANS
C	  IF(ANS.EQ.'Y'.OR.ANS.EQ.'y') THEN
C	    KCURVE=2
C	    CALL STORE_GRAPH(XPLOT1,YPLOT1,NSTART,NEND,
C     1	XPLOT2,YPLOT2,1,NPTS,KCURVE)
C	  ENDIF
 
	 PRINT 305,2*SIGMA,ANGLE,RS,RG,RHO0,ALPHA
305	 FORMAT(' Values used for the graph:',
     1	/,' FWHM,ANGLE,RS,RG,RHO0,ALPHA:',6(F6.2,1X))
 
	 PRINT *,' Do you want another try with the same FWHM ? (Y)'
	 READ(5,10) ANS
	  IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN
	    PRINT *,' Do you want just to change RHO0 ? (Y)'
	    READ(5,10) ANS
	      IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN
	        PRINT *,' New value for RHO0?'
	        READ(5,*) RHO0
	        GO TO 331
	      ENDIF
	    GO TO 300
	  ENDIF
 
	WRITE(10,306) 2*SIGMA,ANGLE,RS,RG,RHO0,ALPHA
306	FORMAT(' FIT PARAMETERS',5X,6(F6.2,1X))
	 GO TO 88
	ENDIF
 
C---------------------------------------------------------------------
C  Calculates a grid of models
 
	IF(IOPT.EQ.4)THEN
 
	  CALL GENE_GRID
	  GO TO 88
 
	ENDIF
 
C--------------------------------------------------------------
C End
999	CLOSE(9)
	CLOSE(10)
	PRINT 987,NAME2
987	FORMAT(' Output in "pst.tmp" and ',A)
	END
 
C************************************************************
C Subroutine BACKGROUND
C
C PROF1 : Real*8 array with the difference
C PROF2 : Real*8 array with the background
C
C Calls : DISPLAY1, DISPLAY2, INDEX_MAX8, CALPOLY, POLYFIT
C
C************************************************************
	SUBROUTINE BACKGROUND(PROF1,PROF2,NSTART,NEND,
     1	RSTART,RREND,TITLE1,PROF_NAME)
 
	 PARAMETER (IDIM=2000)
	 IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	 REAL*8 XXOUT(IDIM),YYOUT(IDIM),RAD(IDIM),
     1	PROF(IDIM),PROF1(IDIM),PROF2(IDIM),
     1	VNUMB(IDIM)
	 REAL*8 SUM1,SUM2,AREA,RSTART,RREND
	 REAL*8 XCOEF(10),SDEV(10),ERR,VVY
	 REAL*8 XOUTMIN,XOUTMAX,XMIN,XMAX
	 REAL*4 XOUT,YOUT
	 REAL*4 XPLOT(IDIM),XPLOT2(IDIM),YPLOT(IDIM),
     1	YPLOT1(IDIM),YPLOT2(IDIM)
	 INTEGER*4 NBINS
	 CHARACTER ANS*1,CHAR1*30,CHAR2*30,PLOTDEV*32
	 CHARACTER NAME*40,TITLE1*40,BUFFER*80,PROF_NAME*60,COMMENTS*80
C
	 COMMON/DATA/RAD,PROF,VNUMB,NBINS,PLOTDEV,NAME
 
C Common block with NEWPLOT :
	 COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
 
10	 FORMAT(A)
 
C DISPLAYING THE INPUT PROFILE AND FITTING A POLYNOMIAL
200	 WRITE (6,201)
201	 FORMAT(' Enter start radius and end radius for the display : ',
     1	'(To exit type 0,0)')
	 READ(5,*) WORK1,WORK2
 
C If 0.,0. return to the main menu (If for example the user has typed the wrong
C option, he has not to fit a new background ...)
	  IF(WORK1.EQ.0.AND.WORK2.EQ.0.)RETURN
	 RSTART=WORK1
	 RREND=WORK2
 
C Else computing the limiting indices :
	 CALL INDEX_MAX8(RAD,NBINS,RSTART,NSTART)
	 CALL INDEX_MAX8(RAD,NBINS,RREND,NEND)
	 RSTART=RAD(NSTART)
	 RREND=RAD(NEND)
 
C Conversion into real*4 for the display
	PRINT *,' Nstart,Nend :',NSTART,NEND
	 DO I=1,NEND
	  XPLOT(I)=RAD(I)
	  YPLOT(I)=PROF(I)
	 END DO
 
	CHAR1='Major axis'
	CHAR2='Surface brightness'
	CALL DISPLAY1(XPLOT,YPLOT,NSTART,NEND,
     1	CHAR1,CHAR2,TITLE1,PLOTDEV)
 
C If the user has not entered any points I ask him another set of parameters
C for the frame.
	IF(NOUT.LE.1) GO TO 200
	PRINT *,NOUT,' POINTS'
 
C Conversion into real*8
	DO I=1,NOUT
	 XXOUT(I)=XOUT(I)
	 YYOUT(I)=YOUT(I)
	END DO
	NNOUT=NOUT
	PRINT *,NNOUT,' POINTS'
 
C Fitting a polynomial
205	PRINT *,' Enter the order of the polynomial:'
	READ(5,*) KORDER
	IF(KORDER.GE.NNOUT)THEN
	  PRINT *,' Error, too few points, NPOINTS=',NNOUT
          GOTO 205
	ENDIF
	CALL POLYFIT(XXOUT,YYOUT,NNOUT,KORDER,XCOEF,SDEV,ERR)
	
C Computing the limits for plotting the polynomial
	XOUTMAX=XXOUT(1)
	XOUTMIN=XXOUT(1)
	DO I=1,NNOUT
	 XOUTMAX=DMAX1(XOUTMAX,XXOUT(I))
	 XOUTMIN=DMIN1(XOUTMIN,XXOUT(I))
	END DO
	CALL INDEX_MAX8(RAD,NBINS,XOUTMIN,NOUTSTART)
	CALL INDEX_MAX8(RAD,NBINS,XOUTMAX,NOUTEND)
 
C Computing the curves:
	DO I=NSTART,NEND
	 CALL CALPOLY(RAD(I),VVY,XCOEF,KORDER)
	 PROF1(I)=PROF(I)-VVY
	 YPLOT1(I)=SNGL(PROF1(I))
	 PROF2(I)=VVY
	 YPLOT2(I)=SNGL(PROF2(I))
	END DO
 
C Displaying the fit
	PRINT *,'Displaying the background, now...'
	PRINT 206
	  CHAR1='Major axis'
	  CHAR2='Surface brightness'
          COMMENTS=' '
	CALL DISPLAY2(XPLOT,YPLOT,NSTART,NEND,XPLOT,YPLOT2,
     1	NOUTSTART,NOUTEND,CHAR1,CHAR2,TITLE1,PLOTDEV,'L','L1',
     1  PROF_NAME,COMMENTS)
	  NOUT2=NOUT
	  IF(NOUT2.EQ.2)THEN
	    XMIN=XOUT(1)
	    XMAX=XOUT(2)
	  ENDIF
 
C	PRINT *,' DO YOU WANT TO LOAD THIS GRAPH IN A FILE ? (N)'
C	READ(5,10) ANS
C	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y') THEN
C	  KCURVE=2
C	  CALL STORE_GRAPH(XPLOT,YPLOT,NSTART,NEND,
C     1	XPLOT,YPLOT2,NSTART,NEND,KCURVE)
C	ENDIF
 
208	PRINT 209
209	FORMAT(' Menu :',/,
     1	' 1. Change the order of the polynomial',/,
     1	' 2. Displaying the difference',/,
     1	' 3. Calculating the intensity over the background',/,
     1	' 4. Changing the limits for the display',/,
     1	' 10. Return to the main menu',/,
     1	' Enter the option you want: ',$)
	READ(5,*) IOPT2
 
	IF(IOPT2.EQ.1)GO TO 205
	IF(IOPT2.EQ.10)RETURN
 
C------------------------------------------------------------------------
C Displaying the difference
	IF(IOPT2.EQ.2)THEN
 
	 PRINT *,' DISPLAYING THE DIFFERENCE...'
	 PRINT 206
206	 FORMAT(' Please enter the boundaries if you want to compute'
     1	' the total luminosity')
	 CHAR1='Major axis'
	 CHAR2='Surface brightness'
	 CALL DISPLAY1(XPLOT,YPLOT1,NSTART,NEND,
     1	CHAR1,CHAR2,TITLE1,PLOTDEV)
	  NOUT2=NOUT
	  IF(NOUT2.EQ.2)THEN
	    XMIN=XOUT(1)
	    XMAX=XOUT(2)
	  ENDIF
 
C	PRINT *,' DO YOU WANT TO LOAD THIS GRAPH IN A FILE ? (N)'
C	 READ(5,10) ANS
C	  IF(ANS.EQ.'Y'.OR.ANS.EQ.'y') THEN
C	    KCURVE=1
C	    CALL STORE_GRAPH(XPLOT,YPLOT1,NSTART,NEND,
C     1	XPLOT,YPLOT1,NSTART,NEND,KCURVE)
C	  ENDIF
 
	GO TO 208
	ENDIF
 
C-----------------------------------------------------------------
C Calculates the intensity
 
	IF(IOPT2.EQ.3)THEN
 
335	    PRINT 301,XMIN,XMAX
301	    FORMAT(' NOW, CALCULATING THE TOTAL INTENSITY : ',
     1	' ENTER THE LIMITS ',/,
     1	' (DEFAULT : XMIN = ',G12.5,' XMAX = ',G12.5)
	    BUFFER=' '
	    READ(5,10) BUFFER
	     IF(BUFFER(1:2).NE.'  ')THEN
	      READ(BUFFER,*,ERR=335)XMIN,XMAX
	     ENDIF
 
C Computing the boundaries :
	  CALL INDEX_MAX8(RAD,NEND,XMIN,NSTARTI)
	  CALL INDEX_MAX8(RAD,NEND,XMAX,NENDI)
 
	  PRINT 309,NSTARTI,NENDI,RAD(NSTARTI),RAD(NENDI)
309	  FORMAT(' LIMITING INDICES :',I5,2X,I5,/,
     1	' LIMITING RADII : ',G12.5,2X,G12.5)
 
	  PRINT *,' DO YOU WANT TO COMPUTE THE LUMINOSITY ',
     1	'WITH THESE VALUES ?(Y)'
	  READ(5,10) ANS
	  IF(ANS.EQ.'N'.OR.ANS.EQ.'n') GOTO 208 
 
C If O.K. store the coefficients of the polynomial :
 
	WRITE(10,925) KORDER,(XCOEF(K),K=1,KORDER+1)
925	FORMAT(/,' ORDER, COEFF.',T21,I2,1X,10(G12.5,1X))
 
C Compute the luminosities and store them in unit 10 :
 
	WRITE(10,915) RAD(NSTARTI),RAD(NENDI)
915	FORMAT(' LIMITING RADII',4X,G12.5,1X,G12.5)
 
	WORK1=RAD(NENDI)-RAD(NSTARTI)
	WORK2=NENDI-NSTARTI
	PRINT 318,WORK1,WORK2
318	FORMAT(' DELTA R IN ARCSECONDS : ',G12.5,
     1	'  IN "INDICES" : ',G12.5)
	 IF(WORK2.NE.0.)THEN
	   WORK=WORK1/WORK2
	   PRINT 319,WORK
319	   FORMAT(' RELATIVE SCALE IN ARCSEC/INDEX :',G12.5)
	   WRITE(10,339) WORK
339	   FORMAT(' SCALE "/INDEX',7X,G12.5)
	 ENDIF
 
C Computing the total intensity :
	SUM1=0.D0
	SUM2=0.D0
	AREA=0.D0
	  DO I=NSTARTI,NENDI
	    SUM1=SUM1+PROF(I)
	    SUM2=SUM2+PROF(I)*VNUMB(I)
	    AREA=AREA+VNUMB(I)
	  END DO
C	PRINT 302,SUM1,SUM2,AREA
302	FORMAT(/,' INTENSITY OF THE ORIGINAL PROFILE: ',G12.5,/,
     1	' TOTAL INTENSITY (WITH THE ANGULAR EXTENT',
     1	' OF THE PROFILE): ',
     1	G12.5,/,' AREA (PIXELS): ',G12.5,/)
 
	SUM1=0.D0
	SUM2=0.D0
	AREA=0.D0
	  DO I=NSTARTI,NENDI
	    SUM1=SUM1+PROF1(I)
	    SUM2=SUM2+PROF1(I)*VNUMB(I)
	    AREA=AREA+VNUMB(I)
	  END DO
	PRINT 303,SUM1,SUM2,AREA
303	FORMAT(' LUMINOSITY OVER THE BACKGROUND: ',G12.5,/,
     1	' TOTAL LUMINOSITY (WITH THE ANGULAR EXTENT',
     1	' OF THE PROFILE) OVER THE BACKGROUND: ',/,
     1	2X,G12.5,/,' AREA (PIXELS): ',G12.5,/)
	WRITE(10,917) SUM1,SUM2,AREA
917	FORMAT(' LUMINOSITIES, AREA',2X,3(G12.5,1X))
 
	GO TO 208
	ENDIF
 
C-------------------------------------------------------------------
	IF(IOPT2.EQ.4)THEN
 
400	WRITE (6,401) RSTART,RREND
401	FORMAT(' Previous values: ',2(G10.3,1X),
     1         ' Enter start radius and end radius for the display : ',$)
	READ(5,*) RSTART,RREND
	IF(RSTART.GE.RREND)THEN	
	  PRINT *,' ERROR: RSTART >= RREND'
	  GO TO 400 
	ENDIF
 
	 CALL INDEX_MAX8(RAD,NBINS,RSTART,NSTART)
	 CALL INDEX_MAX8(RAD,NBINS,RREND,NEND)
	 RSTART=RAD(NSTART)
	 RREND=RAD(NEND)
 
	GO TO 208
	ENDIF
 
	RETURN
	END
C-------------------------------------------------------------------
	SUBROUTINE SHELL1(RAD1,NPTS,XC,SSOUT,SIGMA,ANGLE,
     1	RSTART,RREND,IOPT1)
C************************************************************
C	Subroutine SHELL1 to calculate a model of a spherical
C	shell, with angular limits -ANGLE,+ANGLE
C IOPT1=1 Gaussian density distribution
C IOPT1=2 Distribution in X**(-alpha)
C
C INPUT :
C	 XC(1)=RS
C	 XC(2)=-2.D0*RG*RG
C	 XC(3)=RHO0
C
C Calls : BROADEN
C************************************************************
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER (IDIM=2000)
	REAL*8 XC(3),PI,PI1
	REAL*8 RAD1(IDIM),SSOUT(IDIM)
	COMMON/CONSTANT/PI,PI1
 
C Loading the array RAD1 for the model
C (For the limits we take the limits given by the user)
	DELTAR=(RREND-RSTART)/FLOAT(NPTS)
	PRINT 818,DELTAR
818	FORMAT(' INCREMENT IN RADIUS FOR THE MODEL :',G12.5)
 
	 DO I=1,NPTS
	   RADIUS=RSTART+DELTAR*FLOAT(I)
	   RAD1(I)=RADIUS
	   SSOUT(I)=PROJSHELL(RADIUS,XC,ANGLE,IOPT1)
	 END DO
 
	NPTS=NPTS-1    ! the Abel integral leaves S(NPTS) as zero.
C
C Broadenning with a Hankel transform of a gaussian Point Spread Function.
	CALL BROADEN(RAD1,SSOUT,NPTS,SIGMA)
 
	RETURN
	END
C---------------------------------------------------------------------------
C Function PROJSHELL
C
C            / dmax
C            |
C PROJSHELL= |          Rho(x) * x *dx / sqrt(x**2 - radius**2)
C            |
C           / radius
C
C
C With    dmax=radius/cos(angle)
C
C Rho(x) is the density distribution
C        as a function of the distance of the point in 3D
C
C INPUT :
C   RADIUS = Position where the integral has to be computed
C   XC(1)  = RS
C   XC(2)  = -2.D0*RG*RG
C   XC(3)  = RHO0
C   ANGLE  = Angular limit of the shell
C
C   IOPT =1 : Gaussian
C        =2 : X**-alpha
C
C Calls : D01AKF (NAG)
C---------------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION PROJSHELL(RADIUS,XC,ANGLE,IOPT)
 
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION W(4000),IW(502)
	REAL*8 XC(3),PI,PI1
	REAL*8 Y,XC1,XC2,ANG,ALPHA
	COMMON/BLOCKA/Y,XC1,XC2,ANG,ALPHA
	COMMON/CONSTANT/PI,PI1
	EXTERNAL FUN_SHELL1,FUN_SHELL2
 
	Y=RADIUS
	XC1=XC(1)
	XC2=XC(2)
        ANG=ANGLE
	RS=XC(1)
	RG=DSQRT(XC(2)/(-2.D0))
 
C Absolute and relative accuracy :
C        EPSABS=0.0001
C        EPSREL=0.001
	EPSABS=0.001
	EPSREL=0.001
C sizes of the work areas W(real) and IW(integer)
        LW=4000
        LIW=502
C Lower and upper limits for the integration
	XLOW=RADIUS
	COSANG=DCOS(ANG*PI/180.)
	COSANG=DABS(COSANG)
	COSANG=DMAX1(1.D-6,COSANG)
	XHIGH=RADIUS/COSANG
C Rather sophisticated integration routine D01AKF (NAG):
        IFAIL=1
	  IF(IOPT.EQ.1)THEN
            CALL D01AKF(FUN_SHELL1,XLOW,XHIGH,EPSABS,EPSREL,AL,
     1	ABSERR,W,LW,IW,LIW,IFAIL)
	    AL=AL/RG
	  ELSE
            CALL D01AKF(FUN_SHELL2,XLOW,XHIGH,EPSABS,EPSREL,AL,
     1	ABSERR,W,LW,IW,LIW,IFAIL)
	  ENDIF
 
          IF(IFAIL.NE.0) THEN
            PRINT *,' WARNING : Failure in D01AKF,  IFAIL =',IFAIL
          ENDIF
 
        PROJSHELL=AL
	RETURN
	END
C---------------------------------------------------------------------------
C Function FUN_SHELL1
C Called by D0AKF
C Generates the value to be integrated at the point X
C Rho(x)=exp( (x-xc1)**2 / xc2)
C---------------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION FUN_SHELL1(X)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 Y,XC1,XC2,ANG,ALPHA
	COMMON/BLOCKA/Y,XC1,XC2,ANG,ALPHA
 
C Computing the output :
	RHO=DEXP((X-XC1)*(X-XC1)/XC2)
	Z2=DSQRT(X*X-Y*Y)
	FUN_SHELL1=(X*RHO)/Z2
 
	RETURN
	END
C---------------------------------------------------------------------------
C Function FUN_SHELL2
C Called by D0AKF
C Generates the value to be integrated at the point X
C Rho(X)=(X-RS)**(-alpha)
C---------------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION FUN_SHELL2(X)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 Y,XC1,XC2,ANG,ALPHA
	COMMON/BLOCKA/Y,XC1,XC2,ANG,ALPHA
 
C Computing the output :
	IF(X.LT.XC1)THEN
	  RHO=(XC1-X)**ALPHA
	  Z2=DSQRT(X*X-Y*Y)
	  FUN_SHELL2=(X*RHO)/Z2
	ELSE
	  FUN_SHELL2=0.
	ENDIF
 
	RETURN
	END
C--------------------------------------------------------------------
C Subroutine READ_PROFILE
C Reads the profile and returns NBINS, RAD, PROF, VNUMB and NAME
C
C Input :
C PROFILE1_FORMAT : Logical
C
C Output :
C Common block DATA
C--------------------------------------------------------------------
	SUBROUTINE READ_PROFILE(PROFILE1_FORMAT,PROF_NAME)
	PARAMETER (IDIM=2000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 RAD(IDIM),PROF(IDIM),VNUMB(IDIM)
	INTEGER*4 NBINS
	CHARACTER IMAGE_NAME*40,PROF_NAME*60,BUFFER*80
	CHARACTER PLOTDEV*32,NAME*40
	LOGICAL PROFILE1_FORMAT
C
	COMMON/DATA/RAD,PROF,VNUMB,NBINS,PLOTDEV,NAME
 
10	FORMAT(A)
 
100	 PRINT 101
101	 FORMAT(' NAME OF THE PROFILE : ')
	 READ(5,10) PROF_NAME
102	 FORMAT(' PROFILE :',A)
	 OPEN (9,FILE=PROF_NAME,STATUS='OLD',ERR=100)
 
	 WRITE(10,905) PROF_NAME
905	 FORMAT('#',/,' PROFILE',13X,A)
 
C-------------------------------
C For profiles from "PROFILE1" read the 32 first lines :
	 IF(PROFILE1_FORMAT)THEN
  	    READ(9,10) BUFFER
	    READ(9,10) IMAGE_NAME
	    WRITE(10,906) IMAGE_NAME
906	    FORMAT(' IMAGE',14X,A)
	     DO I=3,9
 	      READ(9,10) BUFFER
	     END DO
	    READ(9,*) POSITION_ANGLE
	    WRITE(10,907) POSITION_ANGLE
907	    FORMAT(' POSITION ANGLE',4X,G12.5)
 
 	    READ(9,10) BUFFER
	    READ(9,*) AXIS_RATIO
	    WRITE(10,908) AXIS_RATIO
908	    FORMAT(' AXIS RATIO',9X,G12.5)
 
 	    READ(9,10) BUFFER
	    READ(9,*) X0,Y0
	    WRITE(10,909) X0,Y0
909	    FORMAT(' CENTRE',12X,G12.5,1X,G12.5)
 
	     DO I=15,19
	       READ(9,10) BUFFER
	     END DO
	    READ(9,*) AMIN,AMAX
	    WRITE(10,910) AMIN,AMAX
910	    FORMAT(' LIMITING ANGLES',3X,G12.5,1X,G12.5)
	     DO I=21,32
	      READ(9,10) BUFFER
	     END DO
 
C---------------------------
C Read the number of points :
	 READ(9,*)NBINS
	  DO I=1,NBINS
	   READ (9,*,END=111) RAD(I),PROF(I),VNUMB(I)
	   RAD(I)=RAD(I)/SQRT(1.-AXIS_RATIO/10.)
	  END DO
 
       ELSE
 
C---------------------------
C Simple format :
	 READ(9,*)NBINS
	  DO I=1,NBINS
	   READ (9,*,END=111) RAD(I),PROF(I)
	   VNUMB(I)=1.
	  END DO
 
	ENDIF
 
	 PRINT 103,NBINS
103	 FORMAT(2X,I4,' POINTS RECEIVED')
	 CLOSE(9)
	 RETURN
 
C Error message if not enough points :
111	 NBINS=I-1
	 PRINT 112
112	 FORMAT(' WARNING : LESS POINTS THAN EXPECTED !')
	 PRINT 103,NBINS
	 CLOSE(9)
 	 RETURN
 
	END
C-------------------------------------------------------------------------
C Subroutine GENE_GRID
C-------------------------------------------------------------------------
	SUBROUTINE GENE_GRID
	PARAMETER (IDIM=2000,IDIM3=200,KCUR=20)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 RAD(IDIM),RAD1(IDIM),SS(IDIM),
     1	XC(3),PROF(IDIM),VNUMB(IDIM)
	REAL*8 Y,XC1,XC2,ANG,ALPHA
	REAL*4 XPLOT(IDIM3,KCUR),YPLOT(IDIM3,KCUR),YMAX
	INTEGER*4 NPOINTS(KCUR)
	CHARACTER NCHAR(KCUR)*4,CHAR1*30,CHAR2*30,TITLE*40
	CHARACTER PCOLOR(KCUR)*30,PLOTDEV*32,ANS*1,NAME*40
C
	COMMON/DATA/RAD,PROF,VNUMB,NBINS,PLOTDEV,NAME
	COMMON/BLOCKA/Y,XC1,XC2,ANG,ALPHA
 
10	FORMAT(A)
	 PRINT *,' ENTER LIMITING RADII FOR THE DISPLAY',
     1	'  RSTART, RREND :'
	 READ(5,*) RSTART,RREND
	 PRINT *,' NUMBER OF POINTS FOR THE MODEL ? (<200)'
	 READ(5,*) NPTS_IN
 
	 PRINT *,' DO YOU WANT 1. GAUSSIAN  2. X**(-alpha)  DISTRIBUTION ?'
	 READ(5,*) IOPT1
	   IF(IOPT1.EQ.1)THEN
	    WRITE(10,302)
302	    FORMAT(' GAUSSIAN DISTRIBUTION')
	   ELSE
	    WRITE(10,304)
304	    FORMAT(' X**(-alpha) DISTRIBUTION')
	   ENDIF
 
	 JCUR=0
300	 JCUR=JCUR+1
	 PRINT 301
301	 FORMAT(' Enter the FWHM seeing, and half angular',
     1	' extent for the shell: ')
	 READ(5,*) SIGMA,ANGLE
         SIGMA=SIGMA/2.
C
C  Enter  guesses for RS,RG, AND RHO0
	  IF(IOPT1.EQ.1)THEN
	    PRINT *,' TYPE THE VALUES YOU WANT FOR RS, RG:'
	    READ(5,*) RS,RG
	  ELSE
	    PRINT *,' TYPE THE VALUES YOU WANT FOR RS, n (0.5 ?):'
	    READ(5,*) RS,ALPHA
	    ALPHA=-1.*ALPHA
	  ENDIF
 
	 XC(1)=RS
	 XC(2)=-2.D0*RG*RG
 
	 NPTS=NPTS_IN
	 CALL SHELL1(RAD1,NPTS,XC,SS,SIGMA,ANGLE,RSTART,RREND,IOPT1)
 
	  RS=XC(1)
	  RG=DSQRT(XC(2)/(-2.D0))
 
C Conversion into real*4 for the display
	 NPOINTS(JCUR)=NPTS
	  DO I=1,NPTS
	   XPLOT(I,JCUR)=RAD1(I)
	   YPLOT(I,JCUR)=SS(I)
	  END DO
 
	 PRINT 305,2*SIGMA,ANGLE,RS,RG,RHO0,ALPHA
305	 FORMAT(' VALUES USED FOR THE GRAPH :',
     1	/,' FWHM,ANGLE,RS,RG,RHO0,ALPHA:',6F10.3)
	 PRINT *,' SYMBOL FOR THE GRAPH ?'
	 READ(5,10) NCHAR(JCUR)
         PCOLOR(JCUR)='Default'
 
	WRITE(10,306) 2*SIGMA,ANGLE,RS,RG,ALPHA
306	FORMAT(' FIT PARAMETERS',5X,5(F6.2,1X))
 
	 PRINT *,' DO YOU WANT ANOTHER TRY ? (Y)'
	 READ(5,10) ANS
	  IF(ANS.NE.'N'.AND.ANS.NE.'n'.AND.JCUR.LT.50)THEN
	    GO TO 300
	  ENDIF
 
C------
C Possibility of normalization :
	  PRINT *,' DO YOU WANT TO NORMALIZE THE CURVES ?(Y)'
	  READ(5,10) ANS
 
	    IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN
 
	      DO J=1,JCUR
 
C Looking for the maximum :
	        YMAX=YPLOT(1,J)
	         DO I=2,NPOINTS(J)
	          YMAX=AMAX1(YPLOT(I,J),YMAX)
	         END DO
	      PRINT *,' CURVE J, NPTS, YMAX:',J,NPOINTS(J),YMAX
 
C Dividing by this maximum :
	         IF(YMAX.NE.0.)THEN
	           DO I=1,NPOINTS(J)
	             YPLOT(I,J)=100.*YPLOT(I,J)/YMAX
	           END DO
	         ENDIF
 
	      END DO
 
	    ENDIF
 
C-----------
C Now displaying the curves:
	 CHAR1='Major axis'
	 CHAR2='Surface brightness'
	 PRINT *,' TITLE ?'
	 READ(5,10) TITLE
 
	 CALL NEWPLOT(XPLOT,YPLOT,NPOINTS,IDIM3,JCUR,
     1	CHAR1,CHAR2,TITLE,NCHAR,PCOLOR,PLOTDEV,' ',' ')
 
	RETURN
	END
C------------------------------------------------------------------
	include 'jlpsub:project.for'
C In POLYFIT.FOR there is CALPOLY and POLYFIT
	include 'jlpsub:polyfit.for'
