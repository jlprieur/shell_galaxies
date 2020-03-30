C----------------------------------------------------------------
C Set of subroutines used by KING1, HUBBLE and SHELL
C Contains : BROADEN, STORE_GRAPH,
C PROJECT_ABEL0, PROJECT_ABEL, PROJECT_ABEL1, PROJECT_ABEL3,
C SPLINELOGFIT, SPLINEFIT, and EXTRAPOL_2,
C SPLINELOG_INTER, and SPLINE_INTER.
C IDIM=6000
C_________________________________________________________________
 
	SUBROUTINE BROADEN(RAD1,SS,NPTS,SIGMA)
C_________________________________________________________________
C   Broadens the surface brightness using the Hankel transform of
C a gaussian Point Spread Function
C (From SAUL CAGANOFF's program.)
C
C  Version of October 8th 1986. Corrected from a previous bad normalization
C
C Input :
C   RAD1 : real*8 array with the radii
C   SS : real*8 array with the surface brightness
C   NPTS : number of points (remains unchanged)
C   SIGMA : sigma of the point response function
C
C Output :
C   SS : real*8 array with the broadened surface brightness
C---------------------------------------------------------------------
	PARAMETER (IDIM=6000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 S(IDIM),SS(IDIM),Y(IDIM),RAD1(IDIM),ERR(IDIM)
	REAL*8 WEIGHT(IDIM)
	PI=3.14159265358979323846
	PI1=DSQRT(2.D0*PI)
	SIGMA2=SIGMA*SIGMA
 
C Doesn't bother broadening if SIGMA=0.
	IF (SIGMA.LE.0.0D0)THEN
	PRINT *,' SIGMA EQUALS 0. : MODEL NOT BROADENED'
	RETURN
	ENDIF
 
C________________________________________________________________
	PRINT *,' BROADENING THE DENSITY...'
 
	DO J=1,NPTS
	X1=RAD1(J)
 
	DO 200 I=1,NPTS
	X2=RAD1(I)
	WTEST=(DABS(X1-X2))/(10.D0*SIGMA)
C Limit at 10. sigma
	IF(WTEST.GT.1.D0)THEN
	Y(I)=0.D0
	WEIGHT(I)=0.D0
	GO TO 200
	ENDIF
	Z=X1*X2/SIGMA2
C IF Z .LT. 10 uses a modified Bessel function I0. (This function can't be
C used outside of this range)
C Y(I)=[BESSI0(Z)*DEXP[{X1**2+X2**2}/-2.D0*SIGMA2]
	IF(Z.LT.10.0D0) THEN
	     IFAIL=0
	     BESSI0=S18AEF(Z,IFAIL)
	     WORK=(X1*X1+X2*X2)/(-2.0D0*SIGMA2)
	     Y(I)=BESSI0*DEXP(WORK)
	ELSE
C Else uses the assymptotic form (wich is very close to a normal gaussian
C for large values of X1, X2.
C Y(I)=[SIGMA*Y(I)/SQRT(2*PI*X1*X2)] * DEXP[{(X2-X1)**2}/-2.D0*SIGMA**2]
	    WORK=(X2-X1)*(X2-X1)/(-2.0D0*SIGMA2)
	    Y(I)=(SIGMA*DEXP(WORK))/(PI1*DSQRT(X2*X1))
	END IF
	WEIGHT(I)=X2*Y(I)
	Y(I)=X2*Y(I)*SS(I)
C
200	CONTINUE
 
C Then calculates the integral with NAG/D01GAF and puts the result in S(J)
	IFAIL=0
	CALL D01GAF(RAD1,Y,NPTS,S(J),ERR(J),IFAIL)
C Normalization by integrating WEIGHT(RAD1) :
	IFAIL=0
	CALL D01GAF(RAD1,WEIGHT,NPTS,WEIGHT1,ERR1,IFAIL)
C Division by WEIGHT1 of the result :
	S(J)=S(J)/WEIGHT1
C
	END DO
 
	DO I=1,NPTS
	SS(I)=S(I)
	END DO
 
	PRINT *,' PROFILE BROADENED'
	RETURN
	END
 
C**********************************************************************
	SUBROUTINE STORE_GRAPH(XPLOT1,YPLOT1,NSTART1,NEND1,
	1	XPLOT2,YPLOT2,NSTART2,NEND2,KCURVE)
C---------------------------------------------------------------------
C  Version of Aug 29th 1986
C
C Input :
C   XPLOT,YPLOT : real*4 X and Y arrays
C   NSTART,NEND : indices to start and to end the display
C   KCURVE : number of curves (here 1 or 2)	
C
C---------------------------------------------------------------------
	PARAMETER (IDIM=6000)
	REAL*4 XPLOT1(IDIM),YPLOT1(IDIM),XPLOT2(IDIM)
	REAL*4 YPLOT2(IDIM)
	REAL*8 WORK1,WORK2
	CHARACTER*30 NAME
10	FORMAT(A)
 
500	PRINT *,'NAME OF THE OUTPUT FILE ?'
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='NEW',ERR=500)
 
	NPTS1=NEND1-NSTART1+1
	WRITE(1,*)NPTS1
	DO J=NSTART1,NEND1
	WORK1=XPLOT1(J)
	WORK2=YPLOT1(J)
	WRITE(1,*)WORK1,WORK2
	END DO
 
	IF(KCURVE.EQ.2)THEN
	NPTS2=NEND2-NSTART2+1
	WRITE(1,*)NPTS2
	DO J=NSTART2,NEND2
	WORK1=XPLOT2(J)
	WORK2=YPLOT2(J)
	WRITE(1,*)WORK1,WORK2
	END DO
	ENDIF
 
	CLOSE(1)
	RETURN
	END
C******************************************************************
	SUBROUTINE PROJECT_ABEL0(R,RHO,S,NPTS)
C_________________________________________________________________
C  Projects the density through the Abel integral to get the surface brightness
C (From SAUL CAGANOFF's program.)
C  Version of June 1986
C
C Input :
C   R : real*8 array with the radii
C   RHO : real*8 array with the density
C   NPTS : number of points (remains unchanged)
C
C Output :
C   S : real*8 array with the surface brightness
C---------------------------------------------------------------------
	PARAMETER (IDIM=6000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 R(IDIM),RHO(IDIM),S(IDIM),ERR(IDIM),Y(IDIM),
	1	X(IDIM)
	DO J=1,NPTS-1
C         JJ=NPTS-J-1
C	WRITE (6,1) JJ
C	FORMAT('+',' Projecting the density now...',I6)
C
	DO I=J+1,NPTS
	II=I-J
	X(II)=R(I)
	Y(II)=RHO(I)*R(I)/SQRT(R(I)*R(I)-R(J)*R(J))
	END DO
 
C Calculates the integral with NAG/D01GAF
         IFAIL=0
	NPOINT=NPTS-J
	CALL D01GAF(X,Y,NPOINT,S(J),ERR(J),IFAIL)
C	ERR(J)=ERR(J)/S(J)
	S(J)=2.0D0*S(J)
C
	END DO
	RETURN
	END
 
C--------------------------------------------------------------
	SUBROUTINE PROJECT_ABEL(RAD1,RHO,S,NPTS)
C_________________________________________________________________
C  Projects the density through the Abel integral to get
C  the surface brightness
C  Version of Sept 15th 1986. Modified from PROJECT_ABEL0
C
C Input :
C   RAD1 : real*8 array with the radii
C   RHO : real*8 array with the density
C   NPTS : number of points (reduced by 4 in output)
C
C Output :
C   S : real*8 array with the surface brightness
C   NPTS : Initial NPTS minus 4
C---------------------------------------------------------------------
	PARAMETER (IDIM=6000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 RAD1(IDIM),RHO(IDIM),S(IDIM),ERR(IDIM),Y(IDIM)
	REAL*8 X(IDIM)
C
	DO J=1,NPTS-1
C
	   DO I=J+1,NPTS
	   II=I+1-J
	   X(II)=RAD1(I)
	   Y(II)=RHO(I)*RAD1(I)
	1	/DSQRT(RAD1(I)*RAD1(I)-RAD1(J)*RAD1(J))
	   END DO
 
C For I=J the integral would be undefined, so we take
C an approximation (linear interpolation of Y(1) and Y(2)
C (We take only half of this value since we multiply by
C 2 later this integral (axial symmetry)).
	   X(1)=RAD1(J)
	   Y(1)=Y(2)-0.5D0*Y(3)
 
C Calculates the integral (NAG/D01GAF requires at least 4 points)
	   IFAIL=0
	   IWORK=NPTS+1-J
	   IF(IWORK.GT.4)THEN
	   CALL D01GAF(X,Y,IWORK,S(J),ERR(J),IFAIL)
C	   ERR(J)=ERR(J)/S(J)
	   S(J)=2.0D0*S(J)
	   ELSE
	   S(J)=0.
	   ENDIF
C
	END DO
 
	NPTS=NPTS-4
	RETURN
	END
C*******************************************************************
	SUBROUTINE PROJECT_ABEL1(R,RHO,S,NPTS)
C_________________________________________________________________
C  Projects the density through the Abel integral to get the surface brightness
C  With King'quadrature (1966)
C  Version of Oct 6th 1986 : doesn't work because I don't know what t2 is ...
C
C Input :
C   R : real*8 array with the radii
C   RHO : real*8 array with the density
C   NPTS : number of points (remains unchanged)
C
C Output :
C   S : real*8 array with the surface brightness
C---------------------------------------------------------------------
	PARAMETER (IDIM=6000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 R(IDIM),RHO(IDIM),S(IDIM),ERR(IDIM),
	1	TT(IDIM),Y(IDIM),XC(20)
C
	DO J=1,NPTS-1
	X1=R(J)
	X12=X1*X1
 
	DO I=J+1,NPTS
	X2=R(I)
	X22=X2*X2
	II=I-J
	WORK=1.D0+X22
	TT(II)=(X22-X12)/WORK
	Y(II)=RHO(I)*(WORK**1.5)*DSQRT(WORK/(X22-X12))
	Y(II)=Y(II)/DSQRT(TT(II))
	END DO
 
C Calculates the integral for I > J (NAG/D01GAF)
	IFAIL=0
	NPOINT=NPTS-J
	CALL D01GAF(TT,Y,NPOINT,S(J),ERR(J),IFAIL)
	PRINT *,' J, S(J):',J,S(J)
C	ERR(J)=ERR(J)/S(J)
 
C And for the singularity in I=J, uses a quadrature formula : (King 1966)
C
C  / t1          -1/2             1/2
C  |    Phi(t) t     dt = 4/15 t1    [(5-(t1/t2)Phi0 + ((3t1-5t2)/2(t1-t2))Phi1
C  / 0
C                                        + {t1**2/(t2(t1-t2))}Phi2 ]
 
C But is there a constraint for t2 ???
C	FF0=IM(0)
C	FF1=IM(TT1)
C	FF2=IM(TT2)
 
	YY0=0.26666666667D0*DSQRT(TT1)*(FF0*(5.D0-(TT1/TT2)
	1	+FF1*(1.5D0*TT1-2.5D0*TT2)/(TT1-TT2))
	1	+FF2*TT1*TT1/(TT2*(TT1-TT2)))
	PRINT *,' YY0:',YY0
 
C Then add this value to the previous integral calculated for I > J
	S(J)=(YY0+2.0D0*S(J))/(1.D0+X12)
 
C
	END DO
	RETURN
	END
C--------------------------------------------------------------
	SUBROUTINE PROJECT_ABEL3(RAD1,RHO,S,NPTS)
C--------------------------------------------------------------
C  Projects the density through the Abel integral to get
C  the surface brightness
C
C  Version of Oct 6th 1986, with spline fit and better sampling.
C
C Input :
C   RAD1 : real*8 array with the radii
C   RHO : real*8 array with the density
C   NPTS : number of points (reduced by 4 in output)
C
C Output :
C   S : real*8 array with the surface brightness
C   NPTS : (Initial NPTS) - 4
C---------------------------------------------------------------------
	PARAMETER (IDIM=6000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 RAD1(IDIM),RHO(IDIM),S(IDIM),ERR(IDIM),Y(IDIM)
	REAL*8 X(IDIM)
	REAL*8 K(IDIM),SPLINE(IDIM),SOL(5)
	NPOINTS=NPTS
	NUMBJ=NPOINTS-4
	DELTAJ=(RAD1(NPTS)-RAD1(J))/FLOAT(NPOINTS)
 
	CALL SPLINEFIT(RAD1,PHI,NPTS,SPLINE,NCAP7,K)
C
	PRINT *,'NPTS,NPOINTS,NUMBJ,DELTAJ',NPTS,NPOINTS,NUMBJ,DELTAJ
 
	DO J=1,NUMBJ
	RADJ=RAD1(1)+DELTAJ*FLOAT(I-1)
	NUMBI=(NPOINTS-J)*10
	DELTAI=(RAD1(NPTS)-RAD1(J))/FLOAT(NUMBI+1)
	PRINT *,' J,RADJ,NUMBI,DELTAI',J,RADJ,NUMBI,DELTAI
	   DO I=1,NUMBI
	   RADI=RADJ+DELTAI*FLOAT(I)
C (For I=J the integral is undefined)
	   LEFT=0
	   IFAIL=1
C NAG/E02BCF
	   CALL E02BCF(NCAP7,K,SPLINE,XX1,LEFT,SOL,IFAIL)
	   IF(IFAIL.NE.0)PRINT 44,IFAIL
44	FORMAT(' WARNING :  IN E02BCF  IFAIL=',I3)
	   RHOWORK=SOL(1)
	   Y(I)=RHOWORK*RADI
	1	/DSQRT(RADI*RADI-RADJ*RADJ)
	   END DO
 
C Calculates the integral (NAG/D01GAF requires at least 4 points)
	   IF(IWORK.GT.4)THEN
	   IFAIL=1
	   CALL D01GAF(X,Y,IWORK,S(J),ERR(J),IFAIL)
	   IF(IFAIL.NE.0)PRINT 47,IFAIL
47	FORMAT(' WARNING :  IN D01GAF  IFAIL=',I3)
C	   ERR(J)=ERR(J)/S(J)
	   S(J)=2.0D0*S(J)
	   ELSE
	   S(J)=0.
	   ENDIF
C
	END DO
 
	NPTS=NPTS-4
	RETURN
	END
C*******************************************************************
	SUBROUTINE SPLINELOGFIT(RAD,SS,NPTS,SPLINE,NCAP7,K)
C
C   Fits a spline to the surface density of the calculated
C   profile as a function of LOG10(r/rcore).
C
	PARAMETER (IDIM=6000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 SPLINE(IDIM),K(IDIM),RAD(IDIM),SS(IDIM)
	REAL*8 LOGSS(IDIM),LOGRAD(IDIM),W(IDIM)
	REAL*8 WORK1(IDIM),WORK2(4,IDIM)
 
C Transformation in Log10/Log10 :
	DO I=1,NPTS
	IF(SS(I).GT.0.D0)THEN
	LOGSS(I)=DLOG10(SS(I))
	ELSE
	NPTS=I-1
	PRINT 200
200	FORMAT(' WARNING : NEGATIVE VALUES IN Y')
	GO TO 100
	ENDIF
	IF(RAD(I).GT.0.)THEN
	LOGRAD(I)=DLOG10(RAD(I))
	ELSE
	NPTS=I-1
	PRINT 201
201	FORMAT(' WARNING : NEGATIVE VALUES IN X')
	GO TO 100
	ENDIF
	END DO
 
100	PRINT *,' Calling NAG/E02BAF :  NPTS =',NPTS
C
      DO I=1,NPTS
         W(I)=1.0D0                    	! define the weights.
      END DO
C
C     Define the knots to lie on every fifth point.
C
      N=5           ! knots at every fifth data point.
      J=5           ! E02BAF wants the (I-4)th knot in K(I).
      DO I=5,NPTS-1
           IF(MOD(I,N).EQ.0) THEN
              K(J)=LOGRAD(I)
              J=J+1
           END IF
      END DO
C
C   NCAP7 : Number of intervals of the spline fit +7
C
      NCAP7=J+3
      IFAIL=1
C
C Generates the array SPLINE of the spline function
C
	CALL E02BAF (NPTS,NCAP7,LOGRAD,LOGSS,W,K,WORK1,WORK2,
	1	SPLINE,RMS,IFAIL)
	IF(IFAIL.NE.0)PRINT 44,IFAIL
44	FORMAT(' WARNING :  IN NAG/E02BAF  IFAIL=',I3)
	RMS=SQRT(RMS)/FLOAT(NPTS-1)
	WRITE (6,99) RMS
99	FORMAT(' RMS FOR SPLINE FIT = ',E12.5)
	RETURN
	END
C*******************************************************************
	SUBROUTINE SPLINEFIT(RAD,SS,NPTS,SPLINE,NCAP7,K)
C
C   Fits a spline to the surface density of the calculated
C   profile
C
	PARAMETER (IDIM=6000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 SPLINE(IDIM),K(IDIM),RAD(IDIM),SS(IDIM)
	REAL*8 WORK1(IDIM),WORK2(4,IDIM),W(IDIM)
 
C Defines the weights :
	DO I=1,NPTS-1
	W(I)=1.0D0
	END DO
C
C     Define the knots to lie on every fifth point.
C
      N=5           ! knots at every fifth data point.
      J=5           ! E02BAF wants the (I-4)th knot in K(I).
	DO I=5,NPTS-2
           IF(MOD(I,N).EQ.0) THEN
	   K(J)=RAD(I)
	   J=J+1
           END IF
	END DO
C
C   NCAP7 : Number of intervals of the spline fit +7
C
      NCAP7=J+3
      NPTS=NPTS-1
      IFAIL=1
C
C Generates the array SPLINE of the spline function
C
	CALL E02BAF (NPTS,NCAP7,RAD,SS,W,K,WORK1,WORK2,
	1	SPLINE,RMS,IFAIL)
	IF(IFAIL.NE.0)PRINT 44,IFAIL
44	FORMAT(' WARNING :  IN NAG/E02BAF  IFAIL=',I3)
	RMS=SQRT(RMS)/FLOAT(NPTS-1)
	WRITE (6,99) RMS
99	FORMAT(' RMS FOR SPLINE FIT = ',E12.5)
	RETURN
	END
C-----------------------------------------------------------------
C SUBROUTINE EXTRAPOL_2 : For second order extrapolation
C Version : October 4th 1986
C
C Resolution of the system
C
C  [ X1**2  X1  1 ]  [ COEF1 ]     [ Y1 ]
C  [ X2**2  X2  1 ]  [ COEF2 ]  =  [ Y2 ]
C  [ X3**2  X3  1 ]  [ COEF3 ]     [ Y3 ]
C
C [ I.E    COEF1*(X**2) + COEF2*X + COEF3 = Y ]
C-----------------------------------------------------------------
	SUBROUTINE EXTRAPOL_2(X1,X2,X3,Y1,Y2,Y3,COEF1,COEF2,COEF3)
 
	X12=X1-X2
	X23=X2-X3
 
	XX12=X1*X1-X2*X2
	XX23=X2*X2-X3*X3
 
	Y12=Y1-Y2
	Y23=Y2-Y3
 
	COEF1=(Y12*X23-Y23*X12)/(XX12*X23-XX23*X12)
	COEF2=(Y23-COEF1*XX23)/X23
	COEF3=Y3-COEF2*X3-COEF1*X3*X3
	
	RETURN
	END
C---------------------------------------------------------------------------
C Function PROJABEL
C---------------------------------------------------------------------------
	FUNCTION PROJABEL(RADIUS,RADSHELL,SIGMA,ANGLE)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION W(4000),IW(502)
	REAL*4 PROJSHELL,RADIUS,RADSHELL,SIGMA,ANGLE
	COMMON/BLKA/Y,RS,SIG,ANG
	EXTERNAL ALFUNC
	Y=RADIUS
	RS=RADSHELL
	SIG=SIGMA
        ANG=ANGLE
C Absolute and relative accuracy :
        EPSABS=0.0001
        EPSREL=0.001
C sizes of the work areas W(real) and IW(integer)
        LW=4000
        LIW=502
C Lower and upper values for the integration
        XLOW=DMAX1(Y,(RS-8.0D0*SIG))
        XHIGH=DMAX1(RS,Y)+8.0D0*SIG
C Rather sophisticated integration routine NAG/D01AKF :
        IFAIL=1
        CALL D01AKF(ALFUNC,XLOW,XHIGH,EPSABS,EPSREL,AL,ABSERR,W,LW,
	1	IW,LIW,IFAIL)
	AL=AL/SIG
        IF(IFAIL.NE.0) THEN
          PRINT *,' WARNING : FAILURE IN NAG/D01AKF,  IFAIL =',IFAIL
        END IF
        PROJSHELL=AL
	RETURN
	END
C---------------------------------------------------------------------------
C Function ALFUNC
C Called by D0AKF
C Generates the value to be integrated at the point X
C---------------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION ALFUNC(X)
	IMPLICIT REAL*8(A-H,O-Z)
	COMMON/BLKA/Y,RS,SIG,ANG
	Z1=((X-RS)*(X-RS)/(2.0D0*SIG*SIG))
	Z3=DEXP(-Z1)
	Z4=DACOS(Y/X)
	IF(ABS(Z4).LT.ANG) THEN
	 Z5=1.0
	ELSE
	 Z5=0.0
	END IF
	Z2=DSQRT(X*X-Y*Y)
	ALFUN=(X*Z3*Z5)/Z2
	RETURN
	END
C*******************************************************************
	SUBROUTINE SPLINELOG_INTER(RAD,SS,NPTS,SPLINE,NCAP7,K)
C
C   Determines a cubic-spline interpolant to a given set of data
C   after transforming it into Log10/Log10
C
C   Uses NAG routine E01BAF.
C   In input the array X must be in increasing order.
C   CALL E01BAF(M,X,Y,K,C,LCK,WRK,LWRK,IFAIL)
C   From it you can get the value at x calling :
C   CALL E02BBF(M+4,K,C,XVALUE,SVALUE) Output in SVALUE)
C   or the value and its first three derivatives in the array SDIF (dim=4),
C   CALL E02BCF(M+4,K,C,XVALUE,LEFT,SDIF,IFAIL)
C   or the integral of S(x) over the full range (from the first to the last
C   knot) in SINT (real*8) :
C   CALL E02BDF(M+4,K,C,SINT,IFAIL)
C
	PARAMETER (IDIM=10000)
	PARAMETER (IDIM1=61000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 SPLINE(IDIM),K(IDIM),SS(IDIM),WORK1(IDIM1)
	REAL*8 LOGSS(IDIM),LOGRAD(IDIM),RAD(IDIM)
 
C Transformation in Log10/Log10 :
	DO I=1,NPTS
	IF(SS(I).GT.0.D0)THEN
	LOGSS(I)=DLOG10(SS(I))
	ELSE
	NPTS=I-1
	PRINT 200
200	FORMAT(' WARNING : NEGATIVE VALUES IN Y')
	GO TO 100
	ENDIF
	IF(RAD(I).GT.0.)THEN
	LOGRAD(I)=DLOG10(RAD(I))
	ELSE
	NPTS=I-1
	PRINT 201
201	FORMAT(' WARNING : NEGATIVE VALUES IN X')
	GO TO 100
	ENDIF
	END DO
 
100	PRINT *,' Calling NAG/E01BAF :  NPTS =',NPTS
C
C Generates the array SPLINE of the spline function
C K array of the knots (dimension : NCAP7=NPTS+4)
	NCAP7=NPTS+4
	IFAIL=1
	CALL E01BAF(NPTS,LOGRAD,LOGSS,K,SPLINE,IDIM,WORK1,
	1	IDIM1,IFAIL)
	IF(IFAIL.NE.0)PRINT 45,IFAIL
45	FORMAT(' ****** WARNING **** IN NAG/E01BAF  IFAIL=',I3)
	RETURN
	END
C*******************************************************************
	SUBROUTINE SPLINE_INTER(RAD,SS,NPTS,SPLINE,NCAP7,K)
C
C   Determines a cubic-spline interpolating to a given set of data
C
C   Uses NAG routine E01BAF.
C   In input the array X must be in increasing order.
C   CALL E01BAF(M,X,Y,K,C,LCK,WRK,LWRK,IFAIL)
C   From it you can get the value at x calling :
C   CALL E02BBF(M+4,K,C,XVALUE,SVALUE) Output in SVALUE)
C   or the value and its first three derivatives in the array SDIF (dim=4),
C   CALL E02BCF(M+4,K,C,XVALUE,LEFT,SDIF,IFAIL)
C   or the integral of S(x) over the full range (from the first to the last
C   knot) in SINT (real*8) :
C   CALL E02BDF(M+4,K,C,SINT,IFAIL)
C
	PARAMETER (IDIM=10000)
	PARAMETER (IDIM1=61000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 SPLINE(IDIM),K(IDIM),WORK1(IDIM1)
	REAL*8 SS(IDIM),RAD(IDIM)
C
C Generates the array SPLINE of the spline function
C K array of the knots (dimension : NCAP7=NPTS+4)
	NCAP7=NPTS+4
	IFAIL=1
	CALL E01BAF(NPTS,RAD,SS,K,SPLINE,IDIM,WORK1,
	1	IDIM1,IFAIL)
	IF(IFAIL.NE.0)PRINT 45,IFAIL
45	FORMAT(' ****** WARNING **** IN NAG/E01BAF  IFAIL=',I3)
	RETURN
	END
