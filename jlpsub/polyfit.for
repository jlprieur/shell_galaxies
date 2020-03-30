C++--------------------------------------------------------------------
C Subroutine POLYFIT
C	Fits a polynomial to real*8 arrays XX and YY
C	Degree : KK (less than 9)
C YY = poly(XX)
C
C Contains:
C TEST_POLYFIT, POLYFIT, PREPPW, INVMAT, INTERCH, CALPOLY
C
C From now on, it is better (?) to use "polyfit.c" (JLP, Feb. 2007)
C
C JLP
C Version 07-09-90
C--------------------------------------------------------------------
	SUBROUTINE TEST_POLYFIT
C	PROGRAM TEST_POLYFIT
	PARAMETER(IDIM=50,NNMAX=50)
	REAL*8 XX(IDIM),YY(IDIM)
	REAL*8 A(NNMAX,NNMAX),B(NNMAX),C(NNMAX)
	REAL*8 XC(20),SDEV(20),ERR
	INTEGER*4 NPTS
		
	PRINT *,' First test:'
	PRINT *,' x1 + x2 + x3 = 1'
	PRINT *,' x1 + x2 + 2x3 = 2'
	PRINT *,' x1 + 2x2 + 2x3 = 1'
	A(1,1)=1.
	A(2,1)=1.
	A(3,1)=1.
	A(1,2)=1.
	A(2,2)=1.
	A(3,2)=2.
	A(1,3)=1.
	A(2,3)=2.
	A(3,3)=2.

	C(1)=1.
	C(2)=2.
	C(3)=1.

	NN=3
	IFAIL=-1
	CALL JLP_INVMAT(A,B,C,NN,NNMAX,IFAIL)

	PRINT *,' IFAIL=',IFAIL,' Solution should be: 1 -1 1'
	PRINT *,' B:',B(1),B(2),B(3)

	PRINT *,' Second test:'
	PRINT *,' x1 + x2 + x3 = 1'
	PRINT *,' 0.0001x2 + x3 = 1'
	PRINT *,'         9999.x3 = 10000.'
	A(1,1)=1.
	A(2,1)=1.
	A(3,1)=1.
	A(1,2)=0.
	A(2,2)=0.0001
	A(3,2)=1.
	A(1,3)=0.
	A(2,3)=0.
	A(3,3)=9999.

	C(1)=1.
	C(2)=1.
	C(3)=10000.

	NN=3
	IFAIL=-1
	CALL JLP_INVMAT(A,B,C,NN,NNMAX,IFAIL)

	PRINT *,' IFAIL=',IFAIL,' Solution should be: 1 -1 1'
	PRINT *,' B:',B(1),B(2),B(3)

	PRINT *,' Test of msq:   C0 + C1 X          (0.5+X) '
	XX(1)=1.
	YY(1)=1.5
	XX(2)=3.
	YY(2)=3.5
	XX(3)=4.
	YY(3)=4.5
	XX(4)=6.
	YY(4)=6.5
	XX(5)=7.
	YY(5)=7.5

	NPTS=5
	KK=1
	CALL POLYFIT(XX,YY,NPTS,KK,XC,SDEV,ERR)
	PRINT *,' IFAIL=',IFAIL,' Solution should be: 0.5 1.00'
	PRINT *,' XC: ',XC(1),XC(2)

	NPTS=5
	KK=2
	CALL POLYFIT(XX,YY,NPTS,KK,XC,SDEV,ERR)
	PRINT *,' IFAIL=',IFAIL,' Solution should be: 0.5 1.00 0.00'
	PRINT *,' XC: ',XC(1),XC(2),XC(3)

	PRINT *,' Test of msq:   C0 + C1 X + C2 X**2    0+X-X**2'
	XX(1)=0.
	YY(1)=0.
	XX(2)=1.
	YY(2)=0.
	XX(3)=2.
	YY(3)=-2.
	XX(4)=3.
	YY(4)=-6.
	XX(5)=4.
	YY(5)=-12.

	NPTS=5
	KK=2
	CALL POLYFIT(XX,YY,NPTS,KK,XC,SDEV,ERR)
	PRINT *,' IFAIL=',IFAIL,' Solution should be: 0. 1. -1.'
	PRINT *,' XC: ',XC(1),XC(2),XC(3)

	RETURN
	END

C***************************************************************************	
C Subroutine POLYFIT(XX,YY,NPTS,KK,XC,SDEV,ERR)
C YY = poly(XX)
C
C KK: Order of the polynomial
C M : This integer variable contains the number of residuals.
C N : This integer variable contains the number of variables. With M >= N.
C XX and YY : arrays to be fitted by a polynomial
C				
C XC : Array containing the parameters which we are minimizing with respect to.
C XC(I) is the coeff. of X**(I-1) (REAL*8)
C SDEV : standard deviation for the coeff XC(I) (REAL*8).
C ERR : mean deviation of the fit (REAL*8)
C
C FSUMSQ : sum of the squares of the final point.
C***************************************************************************	
	SUBROUTINE POLYFIT(XX,YY,NPTS,KK,XC,SDEV,ERR)
	PARAMETER(IDIM=500,NNMAX=50)
	REAL*8 XX(*),YY(*),XC(20),SDEV(20)
	REAL*8 XX1(IDIM,20),FSUMSQ,YVAL,ERR
	REAL*8 A(NNMAX,NNMAX),B(NNMAX),C(NNMAX),SCALE
	INTEGER*4 PNTR1
	CHARACTER ANS*1
 
10	FORMAT(A)
 
	IF(NPTS.LE.KK)THEN
	  WRITE(6,32) NPTS,KK
32        FORMAT(' Fatal error in polyfit : too few points for the fit',
     1           /,' npoints = ',I5,' whereas polynomial order = ',I3)
	  STOP
	ENDIF

	IF(NPTS.GT.IDIM)THEN
	  WRITE(6,33) NPTS,IDIM
33        FORMAT(' Fatal error in polyfit : too many points for the fit',
     1           /,' npoints = ',I8,' whereas idim = ',I8)
	  STOP
	ENDIF

C Normalization of XX to avoid integer overflow:
	SCALE=1.
	DO I=1,NPTS
	  SCALE=MAX(ABS(XX(I)),SCALE)
	END DO
C
	DO I=1,NPTS
	  XX(I)=XX(I)/SCALE
	END DO
	PRINT *,' POLYFIT/ Internal X scale:',SCALE

C Preparing the array of the powers:
	NN=KK+1
	CALL JLP_PREPPW(XX,NPTS,XX1,IDIM,KK,ISTATUS)
	IF(ISTATUS.NE.0)THEN
	  PRINT *,' Fatal error in polyfit : order is too big'
	  STOP
	ENDIF

C Computing the coefficients of the normal equations:
	DO J=1,NN
C C vector:
	 C(J)=0.
	 DO L=1,NPTS
	   C(J)=C(J)+YY(L)*XX1(L,J)
	 END DO
C A matrix (symmetrical)
	  DO I=1,J
	   A(I,J)=0.
	    DO L=1,NPTS
	      A(I,J)=A(I,J)+XX1(L,J)*XX1(L,I)
	    END DO
	  END DO
	END DO

C A matrix (symmetrical)
	DO J=1,NN
	  DO I=J+1,NN
	   A(I,J)=A(J,I)
	  END DO
	END DO

C Inversion of a matrix with pivotal elements:
	IFAIL=0
	CALL JLP_INVMAT(A,B,C,NN,NNMAX,IFAIL)

C Transfer, taking SCALE into account:
	XC(1)=B(1)
	DO K=2,KK+1
	  XC(K)=B(K)/(SCALE**(K-1))
	END DO	

C Errors:	
	FSUMSQ=0.
	DO L=1,NPTS
C Going back to the initial value of XX:
	 XX(L)=XX(L)*SCALE
	 CALL CALPOLY(XX(L),YVAL,XC,KK)
	 FSUMSQ=FSUMSQ+(YVAL-YY(L))*(YVAL-YY(L))
	END DO
	FSUMSQ=FSUMSQ/FLOAT(NPTS)
	ERR=SQRT(FSUMSQ)/FLOAT(KK+1)

	RETURN
	END
C***********************************************************************
C Preparing the array of the powers:
C***********************************************************************
	SUBROUTINE JLP_PREPPW(XX,NPTS,XX1,IDIM,KK,ISTATUS)
	REAL*8 XX(NPTS),XX1(IDIM,*)
	INTEGER*4 ISTATUS

C To start the process:
	 DO I=1,NPTS
	  XX1(I,1)=1.
	 END DO

	DO K=2,KK+1
	 DO I=1,NPTS
	  XX1(I,K)=XX1(I,K-1)*XX(I)
C Check if there is a risk of overflow:
C (Will be squared in A matrix):
	  IF(ABS(XX1(I,K)).GT.1.E+18)THEN
	   WRITE(6,*)' POLYFIT/Error: overflow'
	   ISTATUS=1
	   RETURN
	  ENDIF
	 END DO
	END DO

	ISTATUS=0
	RETURN
	END
C***********************************************************************
C JLP_INVMAT
C Inversion of a matrix with pivotal elements:
C AB=C
C
C***********************************************************************
	SUBROUTINE JLP_INVMAT(A,B,C,NN,NNMAX,IFAIL)
	REAL*8 A(NNMAX,NNMAX),B(NNMAX),C(NNMAX),AA,APIVOT,EPSILON

	EPSILON=1.E-12

C------------------
C FIRST STEP
C Loop on the rows, to obtain a triangular matrix:
	DO J=1,NN

C Partial pivoting: look for the biggest coefficient:
	  JPIVOT=J
	  DO JJ=J+1,NN
	    IF(ABS(A(J,JJ)).GT.ABS(A(J,JPIVOT)))THEN
	     JPIVOT=JJ
	    ENDIF
	  END DO

C When found, interchange rows JPIVOT and J:
	IF(JPIVOT.NE.J) CALL INTERCH(A,C,NN,NNMAX,JPIVOT,J)

C After this, test if pivotal element is too small:
	IF(ABS(A(J,J)).LT.EPSILON)THEN
	   WRITE(6,23)
23	   FORMAT(' JLP_INVMAT/Pivotal element too small')
	   IF(IFAIL.NE.-1)THEN
	     WRITE(6,24)
24	     FORMAT(' JLP_INVMAT/Fatal error')
	     STOP
	   ELSE
	     IFAIL=2
	     RETURN
	   ENDIF
	ENDIF

C When OK, divide all the coefficients of this row by the pivot:
	APIVOT=A(J,J)
	DO I=J,NN
	  A(I,J)=A(I,J)/APIVOT
	END DO
	C(J)=C(J)/APIVOT

C Then subtract this row to all the following rows (to obtain a triangular
C matrix in the end...)
	DO JJ=J+1,NN
	  AA=A(J,JJ)
	  IF(ABS(AA).GT.EPSILON)THEN
	    DO I=J,NN
	      A(I,JJ)=A(I,JJ)/AA-A(I,J)
	    END DO
	    C(JJ)=C(JJ)/AA-C(J)
	  ENDIF
	END DO

	END DO

C------------------
C SECOND STEP
C Inverse triangular matrix with all pivotal elements equal to 1. :
	DO J=NN,1,-1
	  B(J)=C(J)
	  DO I=NN,J+1,-1
	   B(J)=B(J)-A(I,J)*B(I)
	  END DO
	END DO


	IFAIL=0
	RETURN
	END
C*************************************************************
	SUBROUTINE INTERCH(A,C,NN,NNMAX,JNEW,JOLD)
	REAL*8 A(NNMAX,NNMAX),AA
	REAL*8 C(NNMAX),CC

	DO I=1,NN
	  AA=A(I,JNEW)
	  A(I,JNEW)=A(I,JOLD)
	  A(I,JOLD)=AA
	END DO

	CC=C(JNEW)
	C(JNEW)=C(JOLD)
	C(JOLD)=CC

	RETURN
	END
C*************************************************************
C Y = poly(X)
C*************************************************************
	SUBROUTINE CALPOLY(X,Y,XC,KDEGREE)
C For real*8
	REAL*8 X,Y
	REAL*8 XC(KDEGREE+1)
	Y=XC(1)
	WORK=1.
 
	DO I=2,KDEGREE+1
	WORK=WORK*X
	Y=Y+WORK*XC(I)
	END DO
 
	RETURN
	END
