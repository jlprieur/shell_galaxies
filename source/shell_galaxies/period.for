C++::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C       PROGRAM PERIOD
C
C  This program computes periods for different potentials
C
C JLP
C Version of 03-10-86
C--::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        PROGRAM PERIOD
	PARAMETER (IDIM=10000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 RHO(IDIM),RAD(IDIM),
     1	PHI(IDIM),DPHI(IDIM),
     1	WK(5000),LOGR(IDIM),LOGPROF(IDIM)
	REAL*8 PERIO(IDIM),APOCEN(IDIM),VMASS(IDIM),RMASS(IDIM)
	REAL*4 X1PLOT(IDIM),Y1PLOT(IDIM)
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,ANS*1,PLOTDEV*32
	CHARACTER NAME*25
 
	COMMON/KING/RHO,PHI,RAD,NINDEX,DPHI
	COMMON/KING3/W0,RTIDAL,RHO0
	COMMON/CONSTANT/PI,PIROOT
	PI=4.0D0*DATAN(1.0D0)
	PIROOT=DSQRT(PI)
10	FORMAT(A)
	OPEN(1,FILE='period.log',STATUS='unknown')
 
	WRITE(1,89)
88	PRINT 89
89	FORMAT(' MENU :',/,
     1	' 1. INPUT OF A MODEL (FILE)',/,
     1	' 2. COMPUTATION OF KING''S MODEL',/,
     1	' 3. COMPUTATION OF A KEPLERIAN MODEL',/,
C     1	' 4. COMPUTATION OF HUBBLE''S MODEL',/,
     1	' 8. COMPUTATION OF PERIODS',/,
     1	' 9. DISPLAYING THE CURVES',/,
     1	' 10. EXIT',/,
     1	' ENTER THE OPTION YOU WANT : ',$)
	READ(5,*) IOPT
	WRITE(1,90)IOPT
90	FORMAT(' OPTION = ',I3)
 
 
C---------------------------------------------------------------------
C OPTION 1 : Input of a model :
C  Reads the model with 2 possible formats.
C
	IF(IOPT.EQ.1)THEN
 
	PRINT *,' FORMAT :  1. (NPTS,X,Y)      2. (NPTS,X,Y,VNUMB)  ?'
	READ(5,*) IFORMT
1022	PRINT *,' NAME OF THE FILE ?'
	READ(5,10) NAME
	OPEN (9,FILE=NAME,STATUS='OLD',ERR=1022)
	READ(9,*)NPTS
	WRITE(1,1025)IFORMT,NAME,NPTS
1025	FORMAT(' FORMAT = ',I3,' NAME :',A,' NPTS :',I5)
	
	IF(IFORMT.EQ.1)THEN
	  DO I=1,NPTS
	  READ (9,*,END=111) RAD(I),PHI(I)
	  END DO
	ELSE
	  DO I=1,NPTS
	  READ (9,*,END=111) RAD(I),PHI(I),WORK
	  END DO
	ENDIF
 
	GO TO 101
111	NPTS=I-1
	PRINT 102
	WRITE(1,102)
102	FORMAT(' WARNING : LESS POINTS THAN EXPECTED !!!!')
101	PRINT 103,NPTS
	WRITE(1,103) NPTS
103	FORMAT(2X,I5,' POINTS RECEIVED')
	PRINT 104,RAD(1),RAD(NPTS)
	WRITE(1,104) RAD(1),RAD(NPTS)
104	FORMAT(' MODEL FROM ',F12.3,' ARCSECONDS TO ',
     1	F12.3,' ARCSECONDS')
	CLOSE(9)
 
	GO TO 88
	ENDIF
 
C------------------------------------------------------------------------------
C Option 2 : Computation of  King's model
C
	IF(IOPT.EQ.2)THEN
 
	WRITE (6,201)
201	FORMAT(' ENTER W0 (CENTRAL POTENTIAL) AND RCORE (KPC) : ',$)
	READ (5,*) W0,RCORE
 
C Number of points calculated for the model
	WRITE (6,202)
202	FORMAT(' Enter number of points for the model : (max 10000)',$)
	READ (5,*) NPTS
	WRITE (1,206)W0,RCORE,NPTS
206	FORMAT(' CALCULATING KING''S MODEL',/,
     1	' W0=',F10.4,'  RCORE=',F12.3,'  NPTS =',I5)
 
C Setting up the radius array for the model (From 1.E-1 to 1.E+3)
	RAD(1)=0.D0
	DO I=2,NPTS
	WORK=-1.D0 +4.D0*FLOAT(I)/FLOAT(NPTS)
	RAD(I)=10.D0**WORK
	END DO
 
	PRINT *,' CALCULATING THE MODEL... '
	CALL KING(NPTS)
 
C Computing the total mass :
	PRINT *,' PROPOSED RADMAX FOR THE MASS :',RTIDAL
276	PRINT *,' ENTER RADMAX (TO EXIT TYPE 0.)'
	READ(5,*) RADMAX
 
	IF(RADMAX.NE.0.)THEN
	CALL MASS_KING(RADMAX,XMU1,NPTS)
	XMU=RHO0*XMU1
	XMASS1=XMU*RCORE*RCORE*RCORE
	PRINT *,' MU =',XMU,'MASS1 =',XMASS1
	GO TO 276
	ENDIF
C
	PRINT *,' TOTAL MASS YOU WANT FOR THIS MODEL ?',
     1	' (UNITS: 10.D+10 SOLAR MASS)'
	READ(5,*) XMASS
	XRHO0=RHO0*XMASS/XMASS1
	PRINT *,' NEW RHO0 :',XRHO0
	WRITE(1,207)XMU,XRHO0,XMASS1,XMASS
207	FORMAT(' XMU =',E12.4,' NEW RHO0 =',E12.4,
     1	'  XMASS1 = ',E12.4,/,
     1	' TOTAL MASS = ',E12.4,' IN 1.E+10 SOLAR MASS')
 
C Computation of a mass profile
	PRINT *,' DO YOU WANT A MASS PROFILE ? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
	PRINT *,' NUMBER OF POINTS ?'
	READ(5,*) NVMASS
	XINC=RTIDAL/FLOAT(NVMASS)
	  DO I=1,NVMASS
	  RMASS(I)=XINC*FLOAT(I)
	  CALL MASS_KING(RMASS(I),XMU2,NPTS)
	  VMASS(I)=XMASS*XMU2/XMU1
	  END DO
	ENDIF
 
C Normalization of the potential to get the correct mass :
C (Potential energy is proportional to mass)
C PHI is multiplied by -1. because the output of KING is positive.
C PHI is shifted of G.M/RT because in King's model, it is assumed PHI=0.
C when RAD=RT
	GRAV=4.497D-02
 
	DO I=1,NPTS
	PHI(I)=(GRAV*XMASS/RTIDAL)-1.D0*XMASS*PHI(I)/XMASS1
	END DO
 
C Transformation of the computed radii in physical radii
	DO I=1,NPTS
	RAD(I)=RCORE*RAD(I)
	END DO
 
C Possibility of a Keplerian extension :
	PRINT *,' DO YOU WANT AN EXTENSION TO THE OUTER PARTS ? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
	PRINT *,' MAX. RADIUS AND NUMBER OF POINTS ?'
	READ(5,*) RADMAX,NUMBER
	CALL KEPLER_EXTENSION(RAD,PHI,NPTS,XMASS,
     1	RADMAX,NUMBER)
	WRITE(1,208)RADMAX,NUMBER
208	FORMAT(' KEPLERIAN EXTENSION IN THE OUTER PARTS',/,
     1	' UNTIL ',F12.3,' ARCSECONDS',/,
     1	' WITH ',I5,' MORE POINTS')
	ENDIF
 
	GO TO 88
	ENDIF
C---------------------------------------------------------------
C Option 4 : Computes a Keplerian model
	IF(IOPT.EQ.3)THEN
	WRITE (6,402)
402	FORMAT(' Enter number of points for the model : (max 10000) ',$)
	READ (5,*) NPTS
	PRINT *,' MASS OF THE GALAXY (UNIT=1.D+10 SOLAR MASS) ?'
	READ(5,*) XMASS
	PRINT *,' CALCULATING THE MODEL... '
	CALL KEPLER(RAD,PHI,NPTS,XMASS)
	WRITE(1,403)NPTS,XMASS
403	FORMAT(' CALCULATING A KEPLERIAN MODEL',/,
     1	' WITH :',I5,' POINTS.      MASS =',E12.4,
     1	' IN 1.D+10 SOLAR MASS')
	GO TO 88
	ENDIF
 
C------------------------------------------------------------------
C Option 8 : Computes the periods from the potential :
	IF(IOPT.EQ.8)THEN
	WRITE (6,802)
802	FORMAT(' Enter number of points  : (max 10000) ',$)
	READ (5,*) NPOINTS
	PRINT *,' CALCULATING THE PERIODS... '
	CALL PERIOD1(RAD,PHI,NPTS,PERIO,APOCEN,NPOINTS)
	PRINT *,NPOINTS,' POINTS'
	WRITE(1,803)NPOINTS
803	FORMAT(' CALCULATING THE PERIODS WITH ',I5,' POINTS')
	GO TO 88
	ENDIF
 
C--------------------------------------------------------------------
C Option 9 : Displays the periods or the potential:
	IF(IOPT.EQ.9)THEN
 
	PRINT 900
900	FORMAT(' MENU :',/,
     1	' 1. DISPLAYING THE POTENTIAL',/,
     1	' 2. DISPLAYING THE PERIODS',/,
     1	' 3. DISPLAYING THE MASS PROFILE',/,
     1	' ENTER THE OPTION YOU WANT : ',$)
	READ(5,*) IOPT9
	
C----
	IF(IOPT9.EQ.1)THEN
	NMAX=NPTS
	CHAR1='RADIUS'
	CHAR2='POTENTIAL'
	DO I=1,NMAX
	X1PLOT(I)=RAD(I)
	Y1PLOT(I)=PHI(I)
	END DO
	ENDIF
C----
	IF(IOPT9.EQ.2)THEN
	NMAX=NPOINTS
	CHAR1='APOCENTER (KPCS)'
	CHAR2='PERIOD (MILLION YEARS)'
	DO I=1,NMAX
	X1PLOT(I)=APOCEN(I)
	Y1PLOT(I)=PERIO(I)
	END DO
	ENDIF
C----
	IF(IOPT9.EQ.3)THEN
	NMAX=NVMASS
	CHAR1='RADIUS (KPCS)'
	CHAR2='MASS (1.D+10 SOLAR MASS)'
	DO I=1,NMAX
	X1PLOT(I)=RMASS(I)
	Y1PLOT(I)=VMASS(I)
	END DO
	ENDIF
 
C----
	TITLE=' '
	PRINT *,' TITLE ?'
	READ(5,10) TITLE
	PRINT *,' DEVICE : TEKTRO, CIFER_T5, CANON ?'
	READ(5,10) PLOTDEV
	CALL DISPLAY1(X1PLOT,Y1PLOT,1,NMAX,
     1	CHAR1,CHAR2,TITLE,PLOTDEV)
 
	PRINT *,' DO YOU WANT TO STORE THE CURVE IN A FILE ? (Y)'
	READ(5,10) ANS
	IF(ANS.NE.'N')THEN
	KCURVE=1
	CALL STORE_GRAPH(X1PLOT,Y1PLOT,1,NMAX,X1PLOT,Y1PLOT,
     1	1,NMAX,KCURVE)
	ENDIF
	
	GO TO 88
	ENDIF
 
	CLOSE(1)
	PRINT *,' Logfile in period.log"
	STOP
	END
C------------------------------------------------------------------
C Subroutine PERIOD1 to compute the periods from a given potential
C
C Input :
C  RAD : Radii for the potential
C  PHI : Potential
C  NPTS : Number of input points for the potential
C  NPOINTS
C
C Output :
C  PERIO : Periods
C  APOCEN : Apocenters
C  NPOINTS : Number of periods computed by this routine
C------------------------------------------------------------------
 
	SUBROUTINE PERIOD1(RAD,PHI,NPTS,PERIO,APOCEN,NPOINTS)
 
	PARAMETER (IDIM=10000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 PHI(IDIM),RAD(IDIM)
	REAL*8 K(IDIM),SPLINE(IDIM),SOL(5),XX(IDIM)
	REAL*8 YY(IDIM),PERIO(IDIM),APOCEN(IDIM)
	CHARACTER*1 ANS
	COMMON/CONSTANT/PI,PIROOT
10	FORMAT(A)
	CSTE=DSQRT(2.D0)
	II=0
 
	PRINT *,' PHI(1),PHI(NPTS)',PHI(1),PHI(NPTS)
 
C Preparing the radius arrays :
	NUMBER=NPOINTS
	DELTAJ=(RAD(NPTS)-RAD(1))/FLOAT(NUMBER)
	NUMBI=NPOINTS*10
	DELTAI=(RAD(NPTS)-RAD(1))/FLOAT(NUMBI+1)
	DO I=1,NUMBI
	XX(I)=RAD(1)+FLOAT(I)*DELTAI
	END DO
	
C-----
C Interpolates with a cubic-spline the input potential
	CALL SPLINE_INTER(RAD,PHI,NPTS,SPLINE,NCAP7,K)
 
C-----
	DO J=1,NUMBER
C Calling NAG routine E02BCF:
	LEFT=0
	IFAIL=1
	XX1=RAD(1)+FLOAT(J)*DELTAJ
	II=II+1
	APOCEN(II)=XX1
	CALL E02BCF(NCAP7,K,SPLINE,XX1,LEFT,SOL,IFAIL)
	IF(IFAIL.NE.0) PRINT 44,IFAIL
	PHI0=SOL(1)
 
	DO I=1,NUMBI
C Calling NAG routine E02BCF:
	LEFT=0
	IFAIL=1
	XX1=XX(I)
	CALL E02BCF(NCAP7,K,SPLINE,XX1,LEFT,SOL,IFAIL)
	IF(IFAIL.NE.0) PRINT 44,IFAIL
44	FORMAT(' ***** WARNING ! ***** IN E02BCF  IFAIL =',I3)
	WORK=PHI0-SOL(1)
	  IF(WORK.GT.0.D0)THEN
	  YY(I)=1.D0/DSQRT(WORK)
	  ELSE
	  NPOINT=I-1
	  GO TO 100
	  ENDIF
	END DO
	GO TO 999
 
C Integration of the array Y(NPOINT)
100	IF(NPOINT.GT.5)THEN
C Calling NAG routine D01GAF: 
	IFAIL=1
	CALL D01GAF(XX,YY,NPOINT,SUM,ERR,IFAIL)
	IF(IFAIL.NE.0) PRINT 45,IFAIL
45	FORMAT(' ***** WARNING ! ***** IN D01GAF  IFAIL =',I3)
	PERIO(II)=SUM*CSTE
	ELSE
	II=II-1
C	PRINT *,' TOO FEW POINTS (LESS THAN 5), FOR PHI0=',PHI0
	ENDIF
 
	END DO
 
999	NPOINTS=II
 
	RETURN
	END
 
C------------------------------------------------------------------
C Subroutine to calculate a Keplerian potential
C Pot = - GRAV * MASS / radius
C Units :
C    Apocentric distance (= RAD) in Kpcs
C    Mass in 1.D+10 Solar mass
C    Time in million years (1.D+06)
C So
C    GRAV=4.497D-02
C------------------------------------------------------------------
	SUBROUTINE KEPLER(RAD,PHI,NPTS,XMASS)
 
	PARAMETER(IDIM=10000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 PHI(IDIM),RAD(IDIM)
	COMMON/CONSTANT/PI,PIROOT
	GRAV=4.497D-02
	CST=-1.D0*XMASS*GRAV
 
	SCALE=1.D0
C In radius we take an exponential scale from 1.D-01 to 300.
C	XSTART=-1.D0
C	XEND=2.5D0
	XSTART=0.1
	XEND=300.
	XINC=(XEND-XSTART)/FLOAT(NPTS)
 
	DO I=1,NPTS
	WORK=XSTART+FLOAT(I)*XINC
	RAD(I)=SCALE*FLOAT(I)		!LINEAR
C	RAD(I)=SCALE*(10.D0**WORK)	!EXPON.
	PHI(I)=CST/RAD(I)
	END DO
 
	RETURN
	END
C------------------------------------------------------------------
C Subroutine to compute a Keplerian extension to the potential in its
C outer parts.
C
C Pot = - GRAV * MASS / radius
C Units :
C    Apocentric distance (= RAD) in Kpcs
C    Mass in 1.D+10 Solar mass
C    Time in million years (1.D+06)
C So
C    GRAV=4.497D-02
C------------------------------------------------------------------
	SUBROUTINE KEPLER_EXTENSION(RAD,PHI,NPTS,XMASS,
     1	RADMAX,NUMBER)
 
	PARAMETER(IDIM=10000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 PHI(IDIM),RAD(IDIM)
	COMMON/CONSTANT/PI,PIROOT
	GRAV=4.497D-02
 
	CST=-1.D0*XMASS*GRAV
 
C In radius we take an exponential scale from 1.D-01 to 300.
	SCALE=1.D0
	XSTART=RAD(NPTS)
	XEND=RADMAX
	XINC=(XEND-XSTART)/FLOAT(NUMBER)
	DO I=1,NUMBER
	RAD(I+NPTS)=XSTART+FLOAT(I)*XINC
	PHI(I+NPTS)=CST/RAD(I+NPTS)
	END DO
	NPTS=NPTS+NUMBER
	
	RETURN
	END
C--------------------------------------------------------------------
	include 'jlpsub:project.for'
	include 'jlpsub:king_model.for'
