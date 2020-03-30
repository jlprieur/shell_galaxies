C************************************************************************
	SUBROUTINE POLYFIT1(XX,YY,NPOINT,KK,XC,SDEV,ERR)
C
C JLP
C Version 07-09-90
C	Fits a polynomial to real*8 arrays XX and YY
C	Degree : KK (less than 9)
C************************************************************************
C      This subroutine calls the NAG Library routine E04HFF which uses
C a modified Gauss-Newton algorithm for finding an unconstrained minima
C of a sum of squares of M non-linear functions in N variables (M >= N).
C	CALL E04HFF(M,N,XC,FSUMSQ,IW,LIW,W,LW,IFAIL)
C The first and second derivatives of the function are required.
C      Variables :
C
C KK: Degree of the polynomial
C M : This integer variable contains the number of residuals.
C N : This integer variable contains the number of variables. With M >= N.
C XX and YY : arrays to be fitted by a polynomial
C				
C XC : Array containing the parameters which we are minimizing with respect to.
C XC(I) is the coeff. of X**(I-1) (REAL*8)
C SDEV : standard deviation for the coeff XC(I) (REAL*8).
C ERR : mean deviation of the fit (REAL*4)
C
C FSUMSQ : On exit of E04HFF this contains the sum of the squares of the
C final point.
C IFAIL : The return status of the function E04HFF.
C For a full description of this variable see the description
C in the NAG Library manual of E04HFF (page 4).   Success is signalled by
C IFAIL = 0.
C IW : Integer array of dimension LIW.   This is used by E04HFF as a workspace.
C LIW : The size of the array IW. (usually set to 1)
C W : Real array of dimension LW which is used by the function E04HFF
C as a workspace.
C LW : The size of the array W.  The appropriate values for LW are :
C        LW >= 8*N + 2*N*N + 2*M*N + 3*M  if N > 1
C	 LW >= 11 + 5*M                   if N = 1
C***************************************************************************	
	IMPLICIT REAL*8 (A-H,P-Z)
	PARAMETER (IDIM=50)
	PARAMETER (LW=8000)
	REAL*8 XX(*),YY(*),W(LW),XC(20),SDEV(20)
	REAL*8 XX1(IDIM),YY1(IDIM)
	REAL*8 SUM
	REAL*4 ERR
	INTEGER*4 IW(100)
	CHARACTER ANS*1
	COMMON/MY_NAG/XX1,YY1,ILSFUN2,ILSHES2
 
10	FORMAT(A)
 
	IF(NPOINT.LE.KK)THEN
	  PRINT *,' FATAL ERROR IN POLYFIT : TOO FEW POINTS FOR THE FIT'
	  STOP
	ENDIF

C WARNING: 50 points maximum for NAG/E04HFF since I put LW=8000!!!!!!!!!!!
C
	  IMOD=(NPOINT/50)+1
C Sum:
	  SUM=0.D0
	  II=0
	  DO I=1,NPOINT,IMOD
	    II=II+1
	    XX1(II)=XX(I)
	    YY1(II)=YY(I)
	    SUM=SUM+YY1(II)
	  END DO

	IF(NPOINT.GT.50)THEN
	  PRINT *,' Warning, too many points, NPTS=',NPOINT 
	  PRINT *,' (Max=50)  After resampling, NPTS=',II
	ENDIF

	NPOINT=II

C    Set up the values for the number of residuals (M) and the number of
C    variables (N).
 
	M=NPOINT
	N=KK+1
 
C    Set up some variables
 
	LIW=100
C First guess
C For XC(1) we take the mean of the input points.
	XC(1)=SUM/FLOAT(NPOINT)
C For the others, we take small values:
	DO I=2,N
	XC(I)=0.1**I
	END DO
 
C    Call NAG routine to find the minimum values
39	IFAIL=1
	ILSFUN2=0
	ILSHES2=0
	CALL E04HFF(M,N,XC,FSUMSQ,IW,LIW,W,LW,IFAIL)
	  IF(IFAIL.NE.0)THEN
	    PRINT 31,ILSFUN2,ILSHES2
31	    FORMAT(' ITERATIONS :   ILSFUN2',I4,
	1	'  ILSHES2',I4)
	    PRINT 101,IFAIL
101	    FORMAT(' IN POLYNOMIAL FITTING, ERROR IN E04HFF,',
	1	'  IFAIL =',I3)
	  IF(IFAIL.EQ.1.OR.IFAIL.EQ.9)STOP
	  ENDIF
C Mean deviation:
	  IF(M.GT.N)THEN
	    ERR=DSQRT(FSUMSQ/(M-N))
	  ELSE
	    ERR=DSQRT(FSUMSQ)
	  ENDIF
 
	  PRINT 32,ERR
32	  FORMAT(' MEAN DEVIATION FOR THE FIT :',E12.4,/)
C Computes the errors:
	  CALL ERROR_POLYFIT(M,N,XC,SDEV,FSUMSQ)
 
	DO I=1,N
	  PRINT 33,I,XC(I),SDEV(I)
33	  FORMAT(' XC(',I2,') :',E12.4,5X,'+/-',E12.4)
	END DO
 
C When IFAIL non equal to 0 :
	IF(IFAIL.EQ.2.OR.IFAIL.EQ.4)THEN
	  PRINT 30
30	  FORMAT(' DO YOU WANT TO TRY AGAIN ? (Y)')
	  READ(5,10) ANS
 
	IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN
C	  PRINT *,' DO YOU WANT TO USE THE PREVIOUS OUTPUT VALUES ? (Y)'
C	  READ(5,10) ANS
C	  IF(ANS.EQ.'N'.OR.ANS.EQ.'n')THEN
C	    DO I=1,N
C	      PRINT 34,I
C34	      FORMAT(' ENTER XC(',I2,') : ',$)
C	      READ(5,*) XC(I)
C	    END DO
C	  ENDIF
C New iteration :
	GO TO 39
	ENDIF
	
	ENDIF
 
	RETURN
	END
 
C--------------------------------------------------------------------------
C LSFUN2
C       This subroutine calculates the elements of the Jacobian matrix, and
C       the Residuals, storing them in the arrays FJACC and FVECC respectively.
C       LSFUN2 is called from within the NAG routine.
C
C       Local variables :
C	   XC		==      Array of dimension N containing the point at
C				which the values of the function and first
C				derivatives are required.
C XC(1) + XC(2)*XX(I) + XC(3)*XX(I)**2 + ...
C	   FVECC	==	Array of dimension M, which on exit must
C				contain the value of the residual at the point
C				XC.
C	   FJACC	==	Array of dimension (LJC,N), which must contain
C				the elements of the Jacobian upon exit.
C	   LJC		==	The first dimension of the array FJACC, it is
C				set somewhere in E04GEF (=M).
C****************************************************************************
 
	SUBROUTINE LSFUN2(M,N,XC,FVECC,FJACC,LJC)
	IMPLICIT REAL*8 (A-H,P-Z)
	PARAMETER (IDIM=50)
	REAL*8 FJACC(LJC,N),FVECC(M),XC(N)
	REAL*8 XX(IDIM),YY(IDIM)
 
	COMMON/MY_NAG/XX,YY,ILSFUN2,ILSHES2
	ILSFUN2=ILSFUN2+1
 
	DO 20 I=1,M
 
C Calculates the residuals.
C YY - ( XC(1)+XC(2)*XX+XC(3)*XX**2+...) 
	WORK1=1.
	WORK2=XC(1)
	DO J=2,N
	  WORK1=WORK1*XX(I)
	  WORK2=WORK2+XC(J)*WORK1
	END DO
 
	FVECC(I)=YY(I)-WORK2
 
C Calculates the Jacobian elements.
C for J=1  -1.
C for J=2  -X
C for J=3  -X**2
C...

	FJACC(I,1)=-1.
	DO J=2,N
	  FJACC(I,J)=FJACC(I,J-1)*XX(I)
	END DO
 
20	CONTINUE
 
	RETURN
	END
 
C*******************************************************
C Subroutine called by E04HFF to calculate the elements of
C the Hessian matrix multiplied by the residuals
C Here it is very simple, since there are no cross terms
C neither diagonal terms : they are all null !!
C*******************************************************
	SUBROUTINE LSHES2(M,N,FVECC,XC,BB,LBB)
	IMPLICIT REAL*8 (A-H,P-Z)
	PARAMETER (IDIM=50)
	REAL*8 BB(LBB),FVECC(M),XC(N)
	REAL*8 XX(IDIM),YY(IDIM)

	COMMON/MY_NAG/XX,YY,ILSFUN2,ILSHES2
	ILSHES2=ILSHES2+1
	DO I=1,LBB
	  BB(I)=0.0
	END DO
	RETURN
	END
 
C*******************************************************
      SUBROUTINE ERROR_POLYFIT(M,N,XC,SDEV,FSUMSQ)
C
C  This routine works out the formal errors on the fit by
C  inverting the Hessian matrix.
C  The matrix of second partial derivative (Hessian) is
C  in many least square problems adequately approximated
C  by A = 2 JT J where J is the jacobian
C  Let H be the inverse of A and S the sum of squares
C  then var xi = 2*S*Hii/(m-n)
C
	IMPLICIT REAL*8(A-H,P-Z)
	PARAMETER (IDIM=50)
	REAL*8 XX(IDIM),YY(IDIM),FJACC(IDIM,20),FVECC(IDIM)
	REAL*8 A(21,20),B(20,20),Z(20),SDEV(20),XC(20)
	COMMON/MY_NAG/XX,YY,ILSFUN2,ILSHES2
 
C Call LSFUN2 to compute the Jaccobian matrix for the solution :
	LJC=IDIM
	CALL LSFUN2(M,N,XC,FVECC,FJACC,LJC)
 
	DO I=1,N
	DO J=1,N
	SUM=0.0
	  DO K=1,M
	  SUM=SUM+FJACC(K,I)*FJACC(K,J)
	  END DO
	A(I,J)=SUM
	END DO
	END DO
 
	IA=21
	IB=20
	IFAIL=0
C
C  Matrix inversion
C
	CALL F01ABF(A,IA,N,B,IB,Z,IFAIL)
C Inversion of a real symmetric positive definite matrix A
C IA first dimension of A(IA,p) p>=N
C N order of array A
c B real*8 array used as work space B(IB,p) p>=N
	IF(IFAIL.NE.0)THEN
	PRINT 100,IFAIL
100	FORMAT(' ERROR DURING INVERSION OF THE MATRIX ',
	1	'(IN F01ABF) , IFAIL =',I4)
	RETURN
	ENDIF
 
	DO I=1,N
	WORK=DABS(FSUMSQ*B(I,I)/(M-N))
	SDEV(I)=DSQRT(WORK)
	END DO
 
C Test to check if the errors are not too large
	TEST=DMAX1(SDEV(1),SDEV(2),SDEV(3),SDEV(4))
 
	IF(TEST.GT.10.)THEN
	 PRINT 202
202	 FORMAT(' ERRORS TOO HIGH TO BE REASONABLE')
	ENDIF
 
C Possibility of printing the covariance matrix
 
C	DO I=1,N
C	DO J=1,N
C	B(I,J)=B(I,J)/(M-N)
C	END DO
C	END DO
C	PRINT 201
201	FORMAT(2X,10(1H*),'COVARIANCE MATRIX',10(1H*))
C	DO 4 I=1,5
C	PRINT 200,(B(I,J),J=1,I)
200	FORMAT(' ',5E12.4)
 
	RETURN
	END
C**********************************************************************
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
