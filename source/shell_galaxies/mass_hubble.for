C++:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C       PROGRAM MASS_HUBBLE
C
C  This program, from HUBBLE computes the mass of pseudo-Hubble models
C
C JLP
C Version of 19-11-86
C--:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        PROGRAM MASS_HUBBLE
	PARAMETER (IDIM=10000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 RHOS(IDIM),RAD(IDIM),XMAG(IDIM)
	REAL*4 ALPHA
	CHARACTER ANS*1,NAME*40
	COMMON/MODEL/ALPHA,RR2
 
	PI=3.14159265358979323846
10	FORMAT(A)
	OPEN(1,FILE='mass_hubble.log',STATUS='unknown')
	WRITE(1,20)
	PRINT 20
20	FORMAT(' PROGRAMME MASS_HUBBLE ',
     1	'---VERSION OF NOV 19TH 1986---')
 
C***********************************************************************
40	PRINT 50
50	FORMAT(' MENU :',/,
     1	' 1. COMPUTING THE MASS OF A PSEUDO-HUBBLE MODEL',/,
     1	' WITH A SPACE DENSITY IN 1/(1+S)**ALPHA',/,
     1	' (I.E. PROJECTED DENSITY IN 1/(1+S)**(ALPHA-1) )',/,
     1	' 2. INVERTING A HUBBLE PROFILE',/,
     1	' 3. COMPUTING THE MASS FROM THE SPACE DENSITY',
     1	' OBTAINED IN 2',/,
     1	' 4. COMPUTING THE TOTAL LUMINOSITY OF A HUBBLE'
     1	' MODEL',/,
     1	' 5. GENERATION OF A CURVE WITH THE LUMINOSITY',/,
     1	' 6. COMPUTING THE MASS OF A PSEUDO-HUBBLE MODEL',/,
     1	' WITH A POTENTIAL IN 1/(1+S**2)**ALPHA',/,
     1	' 10. EXIT',/,
     1	' ENTER OPTION : ',$)
	READ(5,*) IOPT
	
	IF(IOPT.NE.10.AND.IOPT.NE.3)THEN
	PRINT 55
55	FORMAT(' ALPHA : ',$)
	READ(5,*) ALPHA
	ENDIF
 
C---------------------------------------------------------------------
C Options 1 and 3 : Computing the mass
C--------------------------------------------------------------------
	IF(IOPT.EQ.1.OR.IOPT.EQ.3.OR.IOPT.EQ.4.OR.IOPT.EQ.6)THEN
100	PRINT 101
101	FORMAT(' LIMITS FOR THE INTEGRATION',
     1	'(to exit type 0.,0.) : ',$)
	READ(5,*) XLOW,XHIGH
 
	IF(XHIGH.NE.0.)THEN
	XMASS=XMASS_HUBBLE(XLOW,XHIGH,IOPT)
	WRITE(1,102)ALPHA,XLOW,XHIGH,XMASS
	PRINT 102,ALPHA,XLOW,XHIGH,XMASS
102	FORMAT(' ALPHA :',F11.4,/,' LOWER LIMIT :',E11.4,
     1	' UPPER LIMIT :',E11.4,' INTEGRAL :',E12.4,/)
	GO TO 100
	ENDIF
	
	GO TO 40
	ENDIF
 
C---------------------------------------------------------------------
C Option 2 : Inversion of a Hubble profile
C--------------------------------------------------------------------
	IF(IOPT.EQ.2)THEN
	PRINT *,' NUMBER OF POINTS FOR THE MODEL (100 ?) :'
	READ(5,*) NPOINT
	WRITE(1,201)NPOINT
201	FORMAT(' INVERSION OF A HUBBLE PROFILE WITH :',I5,
     1	' POINTS')
 
C Setting up the radius array :
	PRINT *,' LOWER AND UPPER LIMITS IN LOG10 FOR THE RADIUS :'
	READ(5,*) XXLOW,XXHIGH
	XSTEP=(XXHIGH-XXLOW)/FLOAT(NPOINT)
	DO I=1,NPOINT
	RAD(I)=10**(XXLOW+(FLOAT(I)*XSTEP))
	END DO
 
	PRINT 202,RAD(1),RAD(NPOINT)
	WRITE(1,202)RAD(1),RAD(NPOINT)
202	FORMAT(' RADMIN, RADMAX :',E12.4,2X,E12.4)
	CALL INVERT_HUBBLE2(RAD,RHOS,NPOINT)
 
	GO TO 40
	ENDIF
	
C---------------------------------------------------------------------
C Option 5 : luminosity
C--------------------------------------------------------------------
	IF(IOPT.EQ.5)THEN
500	PRINT 501
501	FORMAT(' OUTPUT FILE :')
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='NEW',ERR=500)
	PRINT *,' CORE RADIUS (ARCSEC.)',
     1	' AND CENTRAL SURFACE BRIGHT. (MAG/ARC.)?'
	READ(5,*) RCORE,B0
 
	PRINT *,' MAX RADIUS AND STEP (ARCSECONDS)'
	READ(5,*) RADMAX,STEP
	NPTS=JIDINT(RADMAX/STEP)
	WRITE(1,*)NPTS
 
C To call XMASS_HUBBLE I set IOPT to 4 (same option : luminosity)
	IOPT=4
	XLOW=0.
	CTE=2.D0*PI*RCORE*RCORE
 
	DO I=1,NPTS
	RAD(I)=FLOAT(I)*STEP
	XHIGH=RAD(I)/RCORE
	XWORK=XMASS_HUBBLE(XLOW,XHIGH,IOPT)
	XLUMINO=CTE*XWORK
	XMAG(I)=-2.5*DLOG10(XLUMINO)+B0
	WRITE(1,*)RAD(I),XMAG(I)
	END DO
	
	CLOSE(1)
	GO TO 40
	ENDIF
 
C---------------
	PRINT *,' Output in "mass_hubble.log".'
	CLOSE(1)
	STOP
	END
C---------------------------------------------------------------------------
C Function XMASS_HUBBLE
C--------------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION XMASS_HUBBLE(XLOW,XHIGH,IOPT)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 W(6000)
	INTEGER*4 IW(1000)
	EXTERNAL FUN_HUBBLE1
	EXTERNAL FUN_HUBBLE3
	EXTERNAL FUN_HUBBLE4
	EXTERNAL FUN_HUBBLE6
 
C Absolute and relative accuracy :
        EPSABS=0.0001
        EPSREL=0.001
 
C Size of the work areas W(real) and IW(integer)
        LW=6000
        LIW=1000
 
C Calling a rather sophisticated integration routine D01AKF :
        IFAIL=1
	IF(IOPT.EQ.1)THEN
	CALL D01AKF(FUN_HUBBLE1,XLOW,XHIGH,EPSABS,EPSREL,AL,
     1	ABSERR,W,LW,IW,LIW,IFAIL)
	ENDIF
	IF(IOPT.EQ.3)THEN
	CALL D01AKF(FUN_HUBBLE3,XLOW,XHIGH,EPSABS,EPSREL,AL,
     1	ABSERR,W,LW,IW,LIW,IFAIL)
	ENDIF
	IF(IOPT.EQ.4)THEN
	CALL D01AKF(FUN_HUBBLE4,XLOW,XHIGH,EPSABS,EPSREL,AL,
     1	ABSERR,W,LW,IW,LIW,IFAIL)
	ENDIF
	IF(IOPT.EQ.6)THEN
	CALL D01AKF(FUN_HUBBLE6,XLOW,XHIGH,EPSABS,EPSREL,AL,
     1	ABSERR,W,LW,IW,LIW,IFAIL)
	ENDIF
 
	IF(IFAIL.NE.0) THEN
	PRINT *,' WARNING : FAILURE IN D01AKF,  IFAIL =',IFAIL
	END IF
 
	XMASS_HUBBLE=AL
 
	RETURN
	END
C---------------------------------------------------------------------------
C Function FUN_HUBBLE1
C Called by D01AKF for option 1 (mass)
C Generates the value to be integrated at the point X
C---------------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION FUN_HUBBLE1(X)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*4 ALPHA
	COMMON/MODEL/ALPHA,RR2
	FUN_HUBBLE1=(X*X)/((1.D0+X)**ALPHA)
	RETURN
	END
C---------------------------------------------------------------------------
C Function FUN_HUBBLE3
C Called by D01AKF for option 3 (mass from spatial density)
C Generates the value to be integrated at the point X
C---------------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION FUN_HUBBLE3(X)
	PARAMETER (IDIM=10000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 SPLINE2(IDIM),K2(IDIM)
	COMMON/SPLINE2/NCAP72,SPLINE2,K2
	IFAIL=1
	CALL E02BBF(NCAP72,K2,SPLINE2,X,RESULT,IFAIL)
	  IF(IFAIL.NE.0) THEN
	  PRINT *,' WARNING : FAILURE IN E02BBF,  IFAIL =',IFAIL
	  RESULT=0.D0
	  END IF
 
	FUN_HUBBLE3=X*X*RESULT
	RETURN
	END
C---------------------------------------------------------------------------
C Function FUN_HUBBLE4
C Called by D01AKF for option 4 (Luminosity)
C Generates the value to be integrated at the point X
C---------------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION FUN_HUBBLE4(X)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*4 ALPHA
	COMMON/MODEL/ALPHA,RR2
	FUN_HUBBLE4=X/((1.D0+X)**ALPHA)
	RETURN
	END
C---------------------------------------------------------------------------
C Function FUN_HUBBLE6
C Called by D01AKF for option 6 (mass with a potential in (1+R**2)**(-ALPHA) )
C Generates the value to be integrated at the point X
C---------------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION FUN_HUBBLE6(X)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*4 ALPHA
	COMMON/MODEL/ALPHA,RR2
	RHO=((1+X*X)**(-2.-ALPHA))*(X*X*(3.D0-2.D0*(1.D0+ALPHA))+3.D0)
	FUN_HUBBLE6=X*X*RHO
	RETURN
	END
C---------------------------------------------------------------------------
C Subroutine INVERT_HUBBLE2 to compute spatial density RHOS
C Calls NAG routine D01AMF
C designed to integrate over a semi-infinite or infinite interval
C--------------------------------------------------------------------------
	SUBROUTINE INVERT_HUBBLE2(RAD,RHOS,NPOINT)
	PARAMETER (IDIM=10000)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 W(4000),SPLINE2(IDIM),K2(IDIM)
	REAL*8 RHOS(IDIM),RAD(IDIM)
	REAL*4 ALPHA
	INTEGER*4 IW(502)
	COMMON/MODEL/ALPHA,RR2
	COMMON/SPLINE2/NCAP72,SPLINE2,K2
	EXTERNAL FUN_HUBBLE2
 
C Loop on all the values contained in the array RAD(NPOINT)
	DO I=1,NPOINT
 
C Absolute and relative accuracy :
        EPSABS=1.E-06
        EPSREL=1.E-04
C Size of the work areas W(real) and IW(integer)
        LW=4000
        LIW=502
C Lower limit for the integration
        XLOW=0.
        IFAIL=1
C INF=1 (BOUND--> +INF) INF=-1 (-INF --> BOUND)  INF=2 (-INF --> +INF)
	INF=1
	BOUND=RAD(I)
	RR2=RAD(I)*RAD(I)
 
C Calling a rather sophisticated integration routine D01AMF :
        CALL D01AMF(FUN_HUBBLE2,BOUND,INF,EPSABS,EPSREL,
     1	RESULT,ABSERR,W,LW,IW,LIW,IFAIL)
 
	IF(IFAIL.NE.0) THEN
	PRINT *,' WARNING : FAILURE IN D01AMF,  IFAIL =',IFAIL
	ENDIF
 
	RHOS(I)=RESULT
	WRITE(1,200)I,RHOS(I)
200	FORMAT(' I=',I4,'RHO=',E12.4)
	END DO
 
C Interpolates with cubic-spline :
	CALL SPLINE_INTER(RAD,RHOS,NPOINT,SPLINE2,NCAP72,K2)
 
	RETURN
	END
C---------------------------------------------------------------------------
C Function FUN_HUBBLE2
C Called by D01AMF
C Generates the value to be integrated at the point X
C---------------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION FUN_HUBBLE2(X)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*4 ALPHA
	COMMON/MODEL/ALPHA,RR2
	FUN_HUBBLE2=(1.D0+X*(2.D0+ALPHA))*DSQRT(X*X-RR2)
     1	/(X*X*((1.D0+X)**(2.+ALPHA)))
	RETURN
	END
C---------------------------------------------------------------------------
	include 'jlpsub:project.for'
