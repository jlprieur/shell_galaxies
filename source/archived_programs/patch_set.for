C----------------------------------------------------------------
C Set of subroutines used by PATCH1 and PATCH_GRI
C Version of 20/07/87
C
C Warning : if you have error messages like "FLOATING UNDERFLOW"
C           don't worry and compile your program with
C	    the option FORTRAN/CHECK=(NOUNDERFLOW)
C
C Contains : NEQSOL,REJECT, and POLY
C------------------------------------------------------------------
      SUBROUTINE NEQSOL(X,Y,Z,NDIM,NPTS,NTERMS,NOTO,IOOR,D,SE,
     1 RDOE,SDOR)
C **************************************************************
C
C     SUBROUTINE NEQSOL(X,Y,Z,NDIM,NPTS,NTERMS,NOTO,IOOR,D,SE,RDOE,SDOR)
C
C THIS SUBROUTINE COMPUTES THE COEFFICIENTS, D, WHICH DEFINE THAT LINEAR
C FUNCTION, Y, OF LINEARLY INDEPENDENT FUNCTIONS WHICH BEST FITS, IN THE
C LEAST SQUARES SENSE, A GIVEN SET OF DATA. OR EQUIVALENTLY, IT FINDS
C THE SOLUTION TO THE SYSTEM OF NORMAL EQUATIONS WHICH IS CALLED THE
C NORMAL EQUATIONS SOLUTION.
C
C     WRITTEN AND DOCUMENTED BY:
C
C     JONES, W B, OBITTS, D L, GALLET, R M, AND DE VAUCOULEURS, G,
C     'ASTRONOMICAL SURFACE PHOTOMETRY BY NUMERICAL MAPPING
C     TECHNIQUES', PUBLICATION OF THE ASTRONOMY DEPARTMENT, UNIV.
C     OF TEXAS, AUSTIN, SERIES II, VOL. I, NO. 8, FEB. 1967
C
C     MODIFIED BY W D PENCE, UNIV. OF SUSSEX, SEPT. 1980
C
C     X(NPTS,2) = COORDINATES OF POINTS: X(I,1) = X COORD.  (R*4)
C                                      X(I,2) = Y COORD.  (R*4)
C     Y(NPTS) = VALUE OF POINT AT X(NPTS,1),X(NPTS,2)  (R*4)
C     Z = SCRATCH ARRAY  (DOUBLE PRECISION, R*8)
C     NDIM = FIRST DIMENSION OF X, Y AND Z (I*4)
C     NPTS = NO. OF POINTS IN LEAST SQUARES FIT  (I*4)
C     NTERMS = NO. OF COEFS TO BE SOLVED FOR  (I*4)
C     NOTO = NO. OF TOTAL ORTHOGONALIZATIONS TO PERFORM (I*4)
C     IOOR = POSITIVE INTEGER IF OPTIONAL OUTPUT IS REQUIRED
C     D(NTERMS) = COEFFICIENTS OF THE POLYNOMIAL (DOUBLE PRECISION, R*8)
C                 Maximum: NTERMS=30
C     SE(K) = RMS OF FIT USING ONLY THE FIRST K COEFFICIENTS (R*8)
C     RDOE(NTERMS) = INDICATES THE STATISTICAL SIGNIFICANCE OF EACH TERM (R*8)
C     SDOR = STANDARD DEVIATION OF RESIDUALS  (R*4)
C     VALMINI = SMALLEST VALUE TOLERABLE TO AVOID "FLOATING UNDERFLOW"
C              (Be carefull if you increase this value, the fit is bad...)
C
C ***************************************************************
 
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 X(NDIM,2),Y(NDIM),SDOR
      REAL*8 Z(NDIM),D(30),SUM
      REAL*8 A(30),AKJ(30,30),AA(30),CE(30),DT(30),DYGC(30)
      REAL*8 GGSQT(30),GG(30,30),Q(30,30,2),RDOE(30),REE(30)
      REAL*8 RREE(30),RYG(30),SD(30),SE(30),S(30,30),YGC(30),YG(30)
      DOUBLE PRECISION POLY
      EXTERNAL POLY
 
      IF(NTERMS.GT.30)THEN
        WRITE(6,*)'NEQSOL/Fatal error: too many terms: NTERMS=',NTERMS
        STOP
      ENDIF
C Smallest value to avoid floating underflow (in computing products of
C such values ...)
C Tried 0.1d-15 too large
C       0.1d-30 too large (where I use it ...)
C@	VALMINI=0.1D-30
 
      GO TO  (1,9)NOTO
C COMPUTE THE SQUARE OF THE NORM OF THE VECTOR OF THE DEPENDENT VARIABLE,
C COMPUTE THE MEAN OF THE DEPENDENT VARIABLE, YBAR.
1     FLNPTS=NPTS
      YY=Y(1)*Y(1)
      YBAR=Y(1)
      DO 2 I=2,NPTS
      YY=YY+Y(I)*Y(I)
2     YBAR=YBAR+Y(I)
C COMPUTE THE MATRIX OF INNER PRODUCTS OF THE NORMALIZED FITTING FUNCTIONS
      YBAR=YBAR/FLNPTS
      IX=0
      JY=-1
      DO 8 J=1,NTERMS
        IF (IX .NE. 0)THEN
          IX=IX-1
          JY=JY+1
        ELSE
          IX=JY+1
          JY=0
        END IF
       T=0.
       SUM=0.
        DO 4 I=1,NPTS
          GK=1.
          IF (IX .NE. 0) GK=X(I,1)**IX
          IF (JY .NE. 0) GK=GK*X(I,2)**JY
          Z(I)=GK
          T=T+GK*GK
4       SUM=SUM+Y(I)*GK
        GGSQT(J)=DSQRT(T)
        YG(J)=SUM/GGSQT(J)
        GG(J,J)=1.0
        IF (J-1)8,8,5
5       KUL=J-1
        IXX=0
        JYY=-1
        DO 7 K=1,KUL
         IF (IXX .NE. 0)THEN
          IXX=IXX-1
          JYY=JYY+1
         ELSE
          IXX=JYY+1
          JYY=0
         END IF
         T=0.
          DO 6 I=1,NPTS
           GK=1.
           IF (IXX .NE. 0) GK=X(I,1)**IXX
           IF (JYY .NE. 0) GK=GK*X(I,2)**JYY
6          T=T+Z(I)*GK
        GG(J,K)=T/(GGSQT(J)*GGSQT(K))
7       GG(K,J)=GG(J,K)
8     CONTINUE
C COMPUTE THE MATRIX OF COEFFICIENTS, Q, DEFINING THE ORTHOGONAL FUNCTIONS
C IN TERMS OF THE FITTING FUNCTIONS.
9     Q(1,1,NOTO)=1.
      S(1,1)=1.
      AA(1)=1.
      GO TO  (10,12)NOTO
10     DO 11 K=2,NTERMS
         AKJ(K,1)=-GG(K,1)
11     CONTINUE
      GO TO 15
12     DO 14 K=2,NTERMS
        SUM=0.
         DO 13 J=1,K
13       SUM=SUM+Q(K,J,1)*GG(J,1)
14     AKJ(K,1)=-SUM
15    DO 32 K=2,NTERMS
       Q(K,K,NOTO)=1.
       S(K,K)=1.
       JUL=K-1
        DO 19 J=1,JUL
         GO TO (16,17)NOTO
16       T=AKJ(K,J)
         GO TO 19
17       T=0.
           DO 18 L=J,JUL
18         T=T+AKJ(K,L)*S(L,J)
19      S(K,J)=T
        DO 23 J=1,JUL
         SUM=0.
          DO 20 L=J,JUL
20        SUM=SUM+S(K,L)*Q(L,J,1)
         GO TO (21,22)NOTO
21       Q(K,J,1)=SUM
         GO TO 23
22       Q(K,J,2)=SUM+Q(K,J,1)
23      CONTINUE
       SUM=0.
        DO 25 J=1,K
         T=0.
          DO 24 L=1,K
C COMPUTE THE VECTOR OF THE SQUARE OF THE NORM OF THE ORTHOGONAL FUNCTIONS
24        T=T+Q(K,L,NOTO)*GG(L,J)
 
C To avoid floating underflow :
C	   IF(ABS(T).LT.VALMINI)T=0.
 
         SUM=SUM+Q(K,J,NOTO)*T
25       CONTINUE
         AA(K)=SUM
         IF (K-NTERMS)26,32,32
26       KPO=K+1
          DO 31 J=KPO,NTERMS
           SUM=0.
            DO 30 L=1,K
             GO TO (27,28)NOTO
27           T=GG(J,L)
             GO TO 30
28           T=0.
               DO 29 M=1,J
29             T=T+Q(J,M,1)*GG(M,L)
30          SUM=SUM+Q(K,L,NOTO)*T
31        AKJ(J,K)=-SUM/AA(K)
32    CONTINUE
 
C COMPUTE THE LEAST SQUARES COEFFICIENTS, A, FOR THE SOLUTION IN TERMS OF
C THE ORTHOGONAL FUNCTIONS.
      DO 34 K=1,NTERMS
        SUM=0.
         DO 33 J=1,K
33       SUM=SUM+Q(K,J,NOTO)*YG(J)
         A(K)=SUM/AA(K)
C         PRINT *,'JLP2006/ AA(',K,')=',A(K),' SUM=',SUM
34       CONTINUE
 
C COMPUTE THE LEAST SQUARES COEFFICIENTS,D, FOR THE SOLUTION IN TERMS OF
C THE FITTING FUNCTIONS.
       DO 36 K=1,NTERMS
       SUM=0.
        DO 35 J=K,NTERMS
35      SUM=SUM+Q(J,K,NOTO)*A(J)
       D(K)=SUM/GGSQT(K)
36     DT(K)=SUM
C COMPUTE THE STANDARD DEVIATION OF THE RESIDUALS, SDOR.
      SDOR=0.
      DO 38 I=1,NPTS
        SUM=POLY(X(I,1),X(I,2),D)
C        IF(I.LE.5)PRINT *,'Y,X(I,1),X(I,2),SUM',Y(I),X(I,1),
C     1  X(I,2),SUM
        T=Y(I)-SUM
        SDOR=SDOR+T*T
38    CONTINUE
      SDOR=DSQRT(SDOR/(FLNPTS-NTERMS))
      IF (IOOR)99,99,39
C COMPUTE THE OPTIONAL OUTPUT ONLY IF (IOOR) IS POSITIVE.
39    DO 42 K=1,NTERMS
C COMPUTE THE CHECK FOR THE CONSISTENCY OF THE COEFFICIENTS D.
40     Z(K+200)=YG(K)
       SUM=0.
       DO 41 J=1,NTERMS
        SUM=SUM+DT(J)*GG(K,J)
41     CONTINUE
       YGC(K)=SUM
       DYGC(K)=SUM-YG(K)
       RYG(K)=DYGC(K)/YG(K)
42    CONTINUE
C Compute the orthonormal coefficients, the sum of the squares of residuals,
C the estimates of the standard deviation of the residuals, and the
C term significance ratio.
      SD(1)=A(1)
      CE(1)=YY-A(1)*A(1)*AA(1)
      DEN=NPTS-1
      SE(1)=DSQRT(CE(1)/DEN)
      RDOE(1)=SD(1)/SE(1)
      DO 50 K=2,NTERMS
      IF (AA(K)) 43,44,45
43    SD(K)=-DSQRT(-AA(K))*A(K)
      GO TO 46
44    SD(K)=0.
      GO TO 46
45    SD(K)=DSQRT(AA(K))*A(K)
46    CE(K)=CE(K-1)-A(K)*A(K)*AA(K)
      DEN=NPTS-K
      IF (CE(K))47,48,49
47    SE(K)=-DSQRT(-CE(K)/DEN)
      GO TO 50
48    SE(K)=1.0E-33
      GO TO 50
49    SE(K)=DSQRT(CE(K)/DEN)
50    RDOE(K)=SD(K)/SE(K)
C COMPUTE THE ESTIMATE OF THE ERROR DUE TO ROUNDING AND THE RELATIVE ERROR
C IN THE LEAST SQUARES COEFFICIENTS,A.
      DO 60 K=1,NTERMS
      KMO=K-1
      KPO=K+1
      T=0.
C
      IF (KMO)55,55,51
51    DO 54 J=1,KMO
       SUM=0.
        DO 53 L=1,J
         T1=0.
          DO 52 M=1,K
           T1=T1+Q(K,M,NOTO)*GG(L,M)
52        CONTINUE
         SUM=SUM+Q(J,L,NOTO)*T1
53      CONTINUE
       T=T+A(J)*SUM
54    CONTINUE
C
      IF(K-NTERMS)55,59,59
55    DO 58 J=KPO,NTERMS
      SUM=0.
      DO 57 L=1,J
       T1=0.
        DO 56 M=1,K
         T1=T1+Q(K,M,NOTO)*GG(L,M)
56      CONTINUE
       SUM=SUM+Q(J,L,NOTO)*T1
57    CONTINUE
      T=T+A(J)*SUM
58    CONTINUE
C
59    REE(K)=-T/AA(K)
60    RREE(K)=REE(K)/A(K)
 
99    CONTINUE
      RETURN
      END
C-------------------------------------------------------------------------
      SUBROUTINE REJECT(X,Y,NDIM,D,NN,KK,TT,M,J)
C*****************************************************************
C
C     SUBROUTINE REJECT(X,Y,NDIM,D,N,K,TT,M,J)
C
C     WRITTEN BY JONES ET AL., 1967
C     MODIFIED BY W D PENCE, 1980
C
C     REJECT ANY POINT WHOSE RESIDUAL IS GREATER THAN BETA*SIGMA
C     FROM THE POLYNOMIAL DEFINED BY THE COEFFICIENTS D.
C
C     X = INDEPENDENT VARIABLE (R*4)
C     Y = THE DEPENDENT VARIABLE (R*4)
C     NDIM = FIRST DIMENSION OF X AND Y  (I*4)
C     D(30) = ARRAY CONTAINING THE POLYNOMIAL COEFFICIENTS (R*8)
C     N = NUMBER OF VALUES TO BE TESTED
C     K = THE NUMBER OF TERMS IN THE POLYNOMIAL
C     TT = THE TEST VALUE USED TO SPECIFY THE REJECTION LEVEL
C     M = THE MODIFIED NUMBER OF VALUES AFTER REJECTION
C     J = AN INDICATOR WHICH IS ZERO ONLY ON THE LAST CALL
C         ( THIS PREVENTS ANY POINTS FROM BEING REJECTED)
C
C***********************************************************
      REAL*4 X(NDIM,2),Y(NDIM)
      INTEGER*4 INDXP(21),INDXN(21)
      INTEGER*4 KK,M,J,NN
      DOUBLE PRECISION D(30),S
      DOUBLE PRECISION POLY
      EXTERNAL POLY
C
      N=NN
      T=TT
      AVR=0.
      DO 9 M=1,21
       INDXN(M)=0
       INDXP(M)=0
9      CONTINUE
      U=0.
      V=0.
      NPOS=0
      NNEG=0
      M=1
      DO 23 I=1,N
      X(M,1)=X(I,1)
      X(M,2)=X(I,2)
      Y(M)=Y(I)
      S=POLY(X(I,1),X(I,2),D)
C
C     R=RESIDUAL FROM POLYNOMIAL FIT
C
      R=Y(I)-S
C      IF(I.LE.3)PRINT *,'Y,X(I,1),X(I,2),S',Y(I),X(I,1),X(I,2),S
      AVR=AVR+R
      IF (R)11,15,12
11    NNEG=NNEG+1
      S=-R
      GO TO 13
12    NPOS=NPOS+1
      S=R
C
C DO NOT INCREMENT COUNTER M IF RESIDUAL IS GREATER THAN T LIMIT
C
13    IF (S-T)15,15,14
14    IF (J)15,15,16
15    M=M+1
16    U=U+R*R
      V=V+R*R*R
      IF (R)17,22,20
17    IF (R+.2)18,19,19
18    INDXN(21)=INDXN(21)+1
      GO TO 23
19    JJ=1.-100.*R
      INDXN(JJ)=INDXN(JJ)+1
      GO TO  23
20    IF (R-0.2)22,22,21
21    INDXP(21)=INDXP(21)+1
      GO TO 23
22    JJ=1.+100.*R
      INDXP(JJ)=INDXP(JJ)+1
23    CONTINUE
      M=M-1
C
      RETURN
      END
C-----------------------------------------------------------------------
      DOUBLE PRECISION FUNCTION POLY(Z1,Z2,D)
C
C **********************************************************************
C
C    FUNCTION POLY(X,Y,D)
C
C    EVALUATES THE POLYNOMIAL DEFINED BY THE COEFFICIENTS D, AT
C    THE NORMALIZED COORDINATE (X,Y).
C
C    Z1 = NORMALIZED X COORDINATE  (R*4)
C    2 = NORMALIZED Y COORDINATE  (R*4)
C    D(30) = ARRAY CONTAINING THE COEFFICIENTS OF THE POLYNOMIAL (R*8)
C
C    WRITTEN BY W D PENCE, NOV. 1980
C
C **********************************************************************
C
      IMPLICIT REAL*8 (A-H,O-Y)
      REAL*4 Z1,Z2
      REAL*8 X,D(30)
      REAL*8 YLAST,Y1,Y2,Y3,Y4,Y5,Y6
      REAL*8 C0,C1,C2,C3,C4,C5,C6,C7
      DATA YLAST/-2./
      SAVE YLAST
C
C    FIRST, CALC 1-D POLYNOMIAL IN X FOR GIVEN Y VALUE, IF
C    NOT ALREADY DONE
C
      X=Z1
      Y1=Z2
C
      IF (Y1 .NE. YLAST)THEN
       Y2=Y1*Y1
       Y3=Y2*Y1
       Y4=Y3*Y1
       Y5=Y4*Y1
       Y6=Y5*Y1
C
       C0=D(1)+D(3)*Y1+D(6)*Y2+D(10)*Y3+D(15)*Y4+D(21)*Y5+D(28)*Y6
       C1=     D(2)   +D(5)*Y1+D(9) *Y2+D(14)*Y3+D(20)*Y4+D(27)*Y5
       C2=             D(4)   +D(8) *Y1+D(13)*Y2+D(19)*Y3+D(26)*Y4
       C3=                     D(7)    +D(12)*Y1+D(18)*Y2+D(25)*Y3
       C4=                              D(11)   +D(17)*Y1+D(24)*Y2
       C5=                                       D(16)   +D(23)*Y1
       C6=D(22)+D(30)*Y1
       C7=D(29)
 
      END IF
C
C     EVALUATE POLYNOMIAL IN X
C
      POLY=((((((C7*X+C6)*X+C5)*X+C4)*X+C3)*X+C2)*X+C1)*X+C0
C      IF(I.LE.5) PRINT *,' Z1,Z2,POLY',Z1,Z2,POLY
C
      YLAST=Y1
      RETURN
      END
