C++------------------------------------------------------------------
C Program APP_FLATTENING
C (Cf Sandage et al 1970, Ap.J. 160, 831.
C  and Binney 1978, MNRAS
C
C JLP Version of 10-02-88
C------------------------------------------------------------------
	PROGRAM APP_FLATTENING
	IMPLICIT REAL*8 (A-H,O-Z)
 
	PARAMETER (IDIM=1000)
	REAL*8 WORK(6000)
	REAL*4 XPLOT(IDIM),YPLOT(IDIM)
	INTEGER*4 IWORK(1000)
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,PLOTDEV*32
	CHARACTER ANS*1,NAME*40
 
	EXTERNAL GAUSS_OBLATE,GAUSS_PROLATE,
     1	BINO_OBLATE,BINO_PROLATE
 
	COMMON/APFLAT_GAUSS/Q0,SIGMA,B1
	COMMON/APFLAT_BINO/AA,Q00,PP,B11
 
10	FORMAT(A)
	PRINT 11
11	FORMAT(' PROGRAM APP_FLATTENING   VERSION OF 10-02-88-',/,
     1	'        (Nota : E=10*(1-b/a),  q=b/a)',/)
 
C Number of points for the graphs:
	PRINT *,' NUMBER OF POINTS (50?)'
	READ(5,*) NPTS
 
88	PRINT 12
12	FORMAT(' MENU :',/,
     1	' 1. OBLATE OBJECTS WITH GAUSSIAN DISTRIBUTION',/,
     1	' 2. PROLATE OBJECTS WITH GAUSSIAN DISTRIBUTION',/,
     1	' 3. OBLATE OBJECTS WITH SKEWED BINOMIAL DISTRIBUTION',/,
     1	' 4. PROLATE OBJECTS WITH SKEWED BINOMIAL DISTRIBUTION',/,
     1	' 10. EXIT',/,
     1	' ENTER YOUR CHOICE')
	READ(5,*) IOPT
	IF(IOPT.GT.4.OR.IOPT.LT.1)GO TO 99
C-----
	IF(IOPT.EQ.1.OR.IOPT.EQ.2)THEN
	  PRINT *,' GAUSSIAN DISTRIBUTION'
	  PRINT *,' ENTER E0 (0.<E0<10.) AND SIGMA :'
	  READ(5,*) E0,SIGMA
	  Q0=1.-E0/10.
	  SIGMA=SIGMA/10.
	ELSEIF(IOPT.EQ.3.OR.IOPT.EQ.4)THEN
	  PRINT *,' SKEWED BINOMIAL DISTRIBUTION'
	  PRINT *,' ENTER a, q0 AND p     (0 < q0 < 1.  with a > q0)'
	  READ(5,*) AA,Q00,PP
	  Q0=Q00
	ENDIF
 
	DO IB=1,NPTS
	   B1=FLOAT(IB)/FLOAT(NPTS+1)
C For reasons internal to fortran common syntax :
	   B11=B1
 
C***************
C Calling NAG routine D01AKF to integrate the profile
 
C Absolute and relative accuracy :
	  EPSABS=0.0001
	  EPSREL=0.001
 
C Troncation : the apparent flattening is always larger (i.e. b1/a1 > q)
C (remember a1=1) than the real flattening og the galaxy.
	  QMIN=0.
	  QMAX=B1
 
C Size of work areas WORK (real*8) and IWORK (integer) :
	  LWORK=6000
	  LIWORK=1000
	  IFAIL=1
	  ISAFE=0
 
	    IF(IOPT.EQ.1)THEN
	      CALL D01AKF(GAUSS_OBLATE,QMIN,QMAX,EPSABS,EPSREL,RESULT,
     1	ABSERR,WORK,LWORK,IWORK,LIWORK,IFAIL)
	    ELSEIF(IOPT.EQ.2)THEN
	      CALL D01AKF(GAUSS_PROLATE,QMIN,QMAX,EPSABS,EPSREL,RESULT,
     1	ABSERR,WORK,LWORK,IWORK,LIWORK,IFAIL)
	    ELSEIF(IOPT.EQ.3)THEN
	      CALL D01AKF(BINO_OBLATE,QMIN,QMAX,EPSABS,EPSREL,RESULT,
     1	ABSERR,WORK,LWORK,IWORK,LIWORK,IFAIL)
	    ELSEIF(IOPT.EQ.4)THEN
	      CALL D01AKF(BINO_PROLATE,QMIN,QMAX,EPSABS,EPSREL,RESULT,
     1	ABSERR,WORK,LWORK,IWORK,LIWORK,IFAIL)
	    ENDIF
 
	  IF(IFAIL.NE.0)THEN
	   PRINT *,' WARNING : FAILURE IN D01AKF, IFAIL =',IFAIL
	  ENDIF
	
C Load the arrays XPLOT, YPLOT
	  XPLOT(IB)=10*(1.-B1)
	  YPLOT(IB)=RESULT
	END DO
 
C Normalization to 100 objects:
	SUM=0.
	 DO I=1,NPTS
	   SUM=YPLOT(I)+SUM
	 END DO
	 DO I=1,NPTS
	   YPLOT(I)=100.*YPLOT(I)/SUM
	 END DO
 
C Possibility of storing the output in a file :
	PRINT *,' DO YOU WANT TO STORE THE RESULT IN A FILE ?(N)'
	READ(5,10) ANS
	 IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
5	   PRINT *,'NAME OF THE FILE ?'
	   READ(5,10) NAME
	   OPEN(1,FILE=NAME,STATUS='NEW',ERR=5)
	   WRITE(1,*) NPTS
	   DO I=1,NPTS
	     WRITE(1,*) XPLOT(I),YPLOT(I)
	   END DO
	   CLOSE(1)
	 ENDIF
 
	PRINT *,' Now displaying the curve'
	PRINT *,' GRAPHIC DEVICE ? (TEKTRO, FILE, ...)'
	READ(5,10) PLOTDEV
	PRINT *,' TITLE ?'
	READ(5,10) TITLE
	CHAR1=' 10*(1 - B/A)'
	CHAR2=' Relative Frequency'
	CALL DISPLAY(XPLOT,YPLOT,1,NPTS,CHAR1,CHAR2,
     1	TITLE,PLOTDEV)
 
C Possibility of displaying the model :
	PRINT *,' DO YOU WANT TO DISPLAY THE DEPROJECTED',
     1	' DISTRIBUTION ?(N)'
	  READ(5,10) ANS
	  IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
	    PRINT *,' TITLE ?'
	    READ(5,10) TITLE
 
C Gaussian option :
	    IF(IOPT.EQ.1.OR.IOPT.EQ.2)THEN
	      CT0=-1./(2*SIGMA*SIGMA)
	        DO I=1,NPTS
	          Q=FLOAT(I)/FLOAT(NPTS+1)
	          XPLOT(I)=10*(1.-Q)
	          YPLOT(I)=DEXP(CT0*(Q-Q0)*(Q-Q0))
	        END DO
 
C Skewed binomial distribution:
	    ELSE
	      DO I=1,NPTS
	        Q=FLOAT(I)/FLOAT(NPTS+1)
	        XPLOT(I)=10*(1.-Q)
	        YPLOT(I)=((1.+(Q-Q0)/AA)**SNGL(PP))*DEXP(PP*(Q0-Q))
	      END DO
	    ENDIF
 
	    CALL DISPLAY(XPLOT,YPLOT,1,NPTS,CHAR1,CHAR2,
     1	TITLE,PLOTDEV)
 
	  ENDIF
 
	GO TO 88
 
99	STOP
 
	END
 
C--------------------------------------------------------------
C Function GAUSS_OBLATE, called by D01AKF.
C Generates the value to be integrated at the point X
C--------------------------------------------------------------
	REAL*8 FUNCTION GAUSS_OBLATE(Q)
	IMPLICIT REAL*8 (A-H,O-Z)
	COMMON/APFLAT_GAUSS/Q0,SIGMA,B1
 
C Gaussian distribution :
	  CT0=-1./(2*SIGMA*SIGMA)
	  FQ=DEXP(CT0*(Q-Q0)*(Q-Q0))
	  GAUSS_OBLATE=FQ*B1/DSQRT((B1*B1-Q*Q)*(1.-Q*Q))
 
	RETURN
	END
C--------------------------------------------------------------
C Function GAUSS_PROLATE, called by D01AKF.
C Generates the value to be integrated at the point X
C--------------------------------------------------------------
	REAL*8 FUNCTION GAUSS_PROLATE(Q)
	IMPLICIT REAL*8 (A-H,O-Z)
	COMMON/APFLAT_GAUSS/Q0,SIGMA,B1
 
C Gaussian distribution :
	  CT0=-1./(2*SIGMA*SIGMA)
	  FQ=DEXP(CT0*(Q-Q0)*(Q-Q0))
	  GAUSS_PROLATE=FQ*(Q/B1)*(Q/B1)/DSQRT((B1*B1-Q*Q)*(1.-Q*Q))
 
	RETURN
	END
C--------------------------------------------------------------
C Function BINO_OBLATE, called by D01AKF.
C Generates the value to be integrated at the point X
C--------------------------------------------------------------
	REAL*8 FUNCTION BINO_OBLATE(Q)
	IMPLICIT REAL*8 (A-H,O-Z)
	COMMON/APFLAT_BINO/AA,Q0,PP,B1
 
C Skewed binomial distribution :
	  FQ=((1.+(Q-Q0)/AA)**SNGL(PP))*DEXP(PP*(Q0-Q))
	  BINO_OBLATE=FQ*B1/DSQRT((B1*B1-Q*Q)*(1.-Q*Q))
 
	RETURN
	END
C--------------------------------------------------------------
C Function BINO_PROLATE, called by D01AKF.
C Generates the value to be integrated at the point X
C--------------------------------------------------------------
	REAL*8 FUNCTION BINO_PROLATE(Q)
	IMPLICIT REAL*8 (A-H,O-Z)
	COMMON/APFLAT_BINO/AA,Q0,PP,B1
 
C Skewed binomial distribution :
	  FQ=((1.+(Q-Q0)/AA)**SNGL(PP))*DEXP(PP*(Q0-Q))
	  BINO_PROLATE=FQ*(Q/B1)*(Q/B1)/DSQRT((B1*B1-Q*Q)*(1.-Q*Q))
 
	RETURN
	END
