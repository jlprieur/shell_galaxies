C++------------------------------------------------------------------
C Program CALIBPDS2
C to transform densities into intensities.
C To be used after CALIBPDS1
C
C JLP  Version of 15-12-86
C--------------------------------------------------------------------
	PROGRAM CALIBPDS2
	PARAMETER (IDIM=600)
	REAL*8 XC(30),TEST,XX1,YY1
	REAL*4 XX,YY
	REAL*4 INPUT(IDIM,IDIM),OUTPUT(IDIM,IDIM)
	CHARACTER ANS*1,NAME*40,COMMENTS*80
 
10	FORMAT(A)
 
C Inquire the format of the files :
	CALL JLP_INQUIFMT
 
C Entering the image :
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
	CALL JLP_READIMAG(INPUT,NX1,NY1,IDIM,NAME,COMMENTS)
	PRINT *,' SKY LEVEL USED IN CALIBPDS1 FOR THE FIT :'
	READ(5,*) SKY
 
C To avoid too large exponents (10**YY ...) :
	PRINT *,' LOWER AND UPPER THRESHOLDS FOR THE CONVERSION'
	PRINT *,'   ( IN LOG10(DENSITY) ) :'
	READ(5,*) XLOW,XHIGH
 
C Entering the coefficients of the polynomial (increasing degree) :
	PRINT *,' DEGREE OF THE POLYNOMIAL (IN LOG10/LOG10):'
	READ(5,*) KDEGREE
 
	DO IK=1,KDEGREE+1
	PRINT 100,IK
100	FORMAT(' XC(',I3,') = ',$)
	READ(5,*) XC(IK)
	END DO
 
C Number of pixels with values less than the sky level :
	TEST=0.
 
C Main loop :
	DO J=1,NY1
	DO I=1,NX1
	X=INPUT(I,J)-SKY
	  IF(X.GT.0.)THEN
	  XX=ALOG10(X)
	    IF(XX.GT.XLOW.AND.XX.LT.XHIGH)THEN
	    XX1=XX
	    CALL CALPOLY(XX1,YY1,XC,KDEGREE)
	    YY=YY1
	    OUTPUT(I,J)=10**YY
	    ELSE
	    OUTPUT(I,J)=0.
	    ENDIF
	  ELSE
	  TEST=TEST+1.
	  OUTPUT(I,J)=0.
	  ENDIF
	END DO
	END DO
	
	IF(TEST.NE.0.)PRINT 203,TEST
203	FORMAT(' WARNING :',E12.4,' NEGATIVE VALUES !')
 
C Output of the image :
        WRITE(6,*) 'Output file: '
        READ(5,10) NAME
	CALL JLP_WRITEIMAG(OUTPUT,NX1,NY1,IDIM,NAME,COMMENTS)
 
	STOP
	END
C------------------------------------------------------------------------
	include 'jlpsub:polyfit.for'
