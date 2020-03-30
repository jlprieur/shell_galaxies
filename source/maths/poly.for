C++***************************************************************
C	POLY
C Program to fit a Polynomial to a list of data points
C
C Version of 25-02-88
C--**************************************************************
	PROGRAM POLY
	IMPLICIT REAL*8 (A-H,O-Z)
	PARAMETER (IDIM=600)
	REAL*8 XX(IDIM),YY(IDIM),YY1(IDIM),CHEBY(20)
	REAL*8 XC(20),SDEV(20),ERR
        REAL*8 XMIN,XMAX,DELTA,OX,OY
	LOGICAL CHEBYCHEFF
        INTEGER IOPTION,IFORMAT
	CHARACTER ANS*1,IN_FILE*40,IN_COMMENTS*80
 
	COMMON /CHEBYFIT1/CHEBY,XMIN,XMAX,DELTA
10	FORMAT(A)
 
	PRINT 88
88	FORMAT(' Program POLY to fit a polynomial to a',
     1	' list of data points',/,
     1	' Version of 25-02-97',/)
 
C Reading the input file :
	IFORMAT=0
	CALL DREADFILE(XX,YY,NPTS,WORK1,WORK2,NWORK,IDIM,
     1                 IN_FILE,IN_COMMENTS,IFORMAT)
 
	PRINT *,' Nunmber of input points:',NPTS
	PRINT 90
90	FORMAT(' Two possibilities : Chebycheff or not ',/,
     1	' Do you want Chebycheff ?(Y)')
	READ(5,10) ANS
	CHEBYCHEFF=(ANS.NE.'N'.AND.ANS.NE.'n')	
 
	PRINT *,' Order of the polynomial?'
	READ(5,*) KK
 
	PRINT 89
89	FORMAT(' Do you want to fit the polynomial',/,
     1	' 1. without transformations',/,
     1  ' 2. in LOG/LOG plane',/,
     1  ' 3. in log-polar coordinates (log spiral?)',/,
     1  ' 4. in polar coordinates (linear spiral?)',/,
     1  ' Enter your choice:')
	READ(5,*) IOPTION 
 
C Taking the logarithm of the input values:

	IF(IOPTION.EQ.3)THEN
          WRITE(6,91)
91        FORMAT('Enter coordinates of center OX, OY:')
          READ(5,*) OX,OY
	  DO I=1,NPTS
           CALL DOUBLE_LOGPOLAR_DEG(XX(I),YY(I),XX(I),YY(I),OX,OY)
          END DO
	ELSEIF(IOPTION.EQ.4)THEN
          WRITE(6,92)
92        FORMAT('Enter coordinates of center OX, OY:')
          READ(5,*) OX,OY
	  DO I=1,NPTS
           CALL DOUBLE_POLAR_DEG(XX(I),YY(I),XX(I),YY(I),OX,OY)
          END DO
	ELSEIF(IOPTION.EQ.2)THEN
	  DO I=1,NPTS
	    XX(I)=DLOG(XX(I))
	    IF(YY(I).LE.0.)THEN
	      NPTS=I-1
	      PRINT *,' Warning: negative values !!!'
	      XXMAX=XX(NPTS)
	      PRINT *,' NPTS=',NPTS,' XXMAX=',XXMAX
	      GO TO 100
	    ELSE
	      YY(I)=DLOG(YY(I))
	    ENDIF
	  END DO
100	ENDIF
 
C Fitting a polynomial :
	IF(CHEBYCHEFF)THEN
 
C When ISORTED=1 CHEBYFIT sort the input data into increasing order:
	  ISORTED=1
	  PRINT *,' Fitting a Chebycheff polynomial...'
	  CALL CHEBYFIT(XX,YY,NPTS,KK,ISORTED)
 
	ELSE
 
	  PRINT *,' Fitting the Polynomial now ...'
	  CALL POLYFIT(XX,YY,NPTS,KK,XC,SDEV,ERR)
 
          PRINT *,' rms error: ',ERR
	  DO I=1,KK+1
	  PRINT *,' K:',I,' XC(K):',XC(I),' SDEV(K)',SDEV(I)
          END DO
	ENDIF
 
	PRINT *,' Number of output points:',NPTS
C Computing the model:	
	  IF(CHEBYCHEFF)THEN
	    DO I=1,NPTS
	      CALL CALCHEBY(XX(I),YY1(I),KK)
	    END DO
	  ELSE
	    DO I=1,NPTS
	      CALL CALPOLY(XX(I),YY1(I),XC,KK)
	    END DO
	  ENDIF
 
C Logarithm:
	  IF(IOPTION.EQ.2)THEN
	    DO I=1,NPTS
	      XX(I)=DEXP(XX(I))
	      YY1(I)=DEXP(YY1(I))
	    END DO
	  ENDIF
 
C Storing the output in a file :
	OPEN(2,FILE='poly.dat',STATUS='new')
	  DO I=1,NPTS
	   WRITE(2,*)XX(I),YY(I),YY1(I)
	  END DO
	CLOSE(2)
	PRINT *,' Output profile in poly.dat'
 
	END
C----------------------------------------------------------------------
	include 'jlpsub:chebyfit.for'
	include 'jlpsub:polyfit.for'
	include 'jlpsub:polar_deg.for'
