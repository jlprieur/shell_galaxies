C---------------------------------------------------------------------------
C KING_MODEL : Set of subroutines to calculate King's model
C Version of November 14th 1986
C---------------------------------------------------------------------------
      SUBROUTINE KING(NPTS)
C---------------------------------------------------------------------------
C   Solves the differential equations for King's model to obtain
C   the relative values of density (RHO) and potential (PHI).
C Input :
C   NPTS = on entry : max number of points for the model
C   W0   = Potential in the center (linked with the concentration)
C Output :
C   NPTS = on exit : effective number of points up to the tidal radius
C   RAD  = radii for the model
C   PHI  = potential
C   DPHI = derivate of the potential
C   RTIDAL
C   NINDEX = index incremented after each call in subroutine "OUT"
C
C
C--------------------------------------------------------------------------
      PARAMETER (IDIM=10000)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      LOGICAL FOUND
      DIMENSION RHO(IDIM),PHI(IDIM),RAD(IDIM),
     +          Y(2),W(2,10),DPHI(IDIM)
C
	COMMON /KING/RHO,PHI,RAD,NINDEX,DPHI
	COMMON /KING3/ W0,RTIDAL,RHO0
	COMMON/CONSTANT/PI,PIROOT
C  Subroutines called by NAG Routine D02BBF...
	EXTERNAL FCN,OUT
C     evaluate some numbers required in lower subroutines and used
C     to evaluate the relative density at a given potential
C     from an analytic expression. Passed around in /king2/
C
	PI=3.14159265358979323846
	PIROOT=DSQRT(PI)
	W0ROOT=DSQRT(W0)
	ERF0=S15AEF(W0ROOT,IFAIL)
	RHO0=0.75D0*PIROOT*DEXP(W0)*ERF0
	RHO0=RHO0 - 1.5D0*W0ROOT - W0**1.5D0
C
      TOL=1.0E-4
      NEQS=2
      IRELAB=0
      IFAIL=0
      REND=RAD(NPTS)
      NINDEX=2       ! increment index, first increment filled later
                     ! by initial conditions R=0, W=W0, PHI=1
	RSTART=1.0D-3  !starting conditions a bit away from R=0
C		       ! if not error due to division by 0 in FCN	
      TOL=1.0D-4
      Y(1)=W0-1.5D0*RSTART*RSTART    ! initial conditions for NAG/D02BBF
      Y(2)=-3.0D0*RSTART
C
C     NAG routine D02BBF solves Poisson's equation.  The relative densities
C     are evaluated as required in the subroutine FCN
C     Subroutine OUT fills the relevant arrays.
C     At the end of it all, arrays containing the potential and the
C     corresponding radius are filled, (except element 1, see RHORAD)
C     They are RAD and PHI
C
      CALL D02BBF(RSTART,REND,NEQS,Y,TOL,IRELAB,FCN,OUT,
     +             W,IFAIL)
C (Resolution of the system of 2 ordinary equations
C     dy1/dR = y2
C     dy2/dR = 9 Rho(y1)/Rho0  -  2 y2 / R
C
C  Calling FCN and OUT )
C
C     now evaluate the relative density corresponding to each radius
C     element.  We now have three arrays, RADIUS, POTENTIAL and
C     DENSITY (relative to the central value).
C     Also the tidal radius is found here and NPTS is set to its effective
C     value
C
	CALL RHORAD(NPTS)
	PRINT *,' MODEL CALCULATED WITH',NPTS,' POINTS'
	RETURN
	END
 
C--------------------------------------------------------------------
      SUBROUTINE FCN(R,Y,F)
C---------------------------------------------------------------------
C     subroutine to evaluate the values of the two first order
C     differential equations used to solve poissons equation
C     The relative density is evaluated here using the analytic
C     expression of Hills and Day Ap Lett 17,87, 1976
C     Y(1) = W
C     Y(2) = W1
C     F(1)=DY(1)/D(R) = W1
C     F(2)=DY(2)/D(R) = -9*RHO/RHO0 -2/R * W1
c---------------------------------------------------------------------
	IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 T,Y(2),F(2),RELRHO
C----------------------------------------------------------------------
C
C     trap potential going negative. if it does i.e. we have
C     reached the tidal radius then set the relative density to
C     zero else the analytic function will bomb whilst trying to
C     find the square root of the potential.
C
C
      IF(Y(1).GT.0.0D0) THEN
        CALL RHOEVAL(RELRHO,Y(1))     !find relative density at W=Y(1)
      ELSE
        RELRHO=0.0D0
      END IF
                                    ! the values of the de's
      F(1)=Y(2)
      F(2)=-9.0D0*RELRHO - (2.0D0*Y(2))/R
      RETURN
      END
C--------------------------------------------------------------------------
      SUBROUTINE OUT(R,Y)
C--------------------------------------------------------------------
C Stores the potential and its derivate corresponding to the radii R
C--------------------------------------------------------------------
	PARAMETER (IDIM=10000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	DIMENSION DPHI(IDIM)
	DIMENSION Y(2),RHO(IDIM),PHI(IDIM),RAD(IDIM)
	COMMON /KING/RHO,PHI,RAD,NINDEX,DPHI
 
	PHI(NINDEX)=Y(1)
	DPHI(NINDEX)=Y(2)
	NINDEX=NINDEX+1
	R=RAD(NINDEX)
 
	RETURN
	END
C--------------------------------------------------------------------
      SUBROUTINE RHORAD(NPTS)
C----------------------------------------------------------------------
C     subroutine to evaluate the relative density at each of the radius
C     increments and fill the array RHO.
C     Also finds tidal radius rtidal via a parabolic interpolation
C-----------------------------------------------------------------------
      PARAMETER (IDIM=10000)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL*8 PHI(IDIM),RHO(IDIM),RAD(IDIM)
      REAL*8 RTIDAL,DPHI(IDIM)
      CHARACTER*80 OUTPUT
      LOGICAL*4 FOUND_TIDAL
	COMMON /KING3/ W0,RTIDAL,RHO0
	COMMON /KING/RHO,PHI,RAD,NINDEX,DPHI
c----------------------------------------------------------------------
      FOUND_TIDAL=.FALSE.
C
C     fill element 1 of arrays with conditions at R=0
C
      PHI(1)=W0
      RHO(1)=1.0D0
 
      DO J=2,NINDEX-1     !NINDEX is incremented in OUT after last call
       IF(PHI(J).GT.0.0D0) THEN
         CALL RHOEVAL(RELRHO,PHI(J))  !find RELDENS at W=PHI(J)
         RHO(J)=RELRHO
       ELSE
           CALL FIND_TIDAL(J,RTIDAL)
           FOUND_TIDAL=.TRUE.
           GOTO 100
       END IF
      END DO
 100  NPTS=J-1
      IF(.NOT.FOUND_TIDAL) THEN
        PRINT *,' '
        PRINT *,'YOU HAVE NOT INTEGRATED FAR ENOUGH TO REACH'
        PRINT *,'THE TIDAL RADIUS (IN THE MODEL)'
        WRITE (6,33) RHO(J-1)
  33    FORMAT ('FINAL RELATIVE DENSITY VALUE = ',F20.10)
      ELSE
        PRINT *,' '
        WRITE (6,44) RTIDAL
44	FORMAT ('THE TIDAL RADIUS = ',F20.10)
      END IF
      RETURN
      END
C
C--------------------------------------------------------------------
 
      SUBROUTINE RHOEVAL(RELRHO,POT)
C--------------------------------------------------------------------
C     subroutine to evaluate the relative density at a given
C     potential pot.  Uses Hills and Day's analytic expression
C--------------------------------------------------------------------
      PARAMETER (IDIM=10000)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 RHO(IDIM),RAD(IDIM),DPHI(IDIM),PHI(IDIM)
      INTEGER*4 IFAIL
	COMMON/CONSTANT/PI,PIROOT
	COMMON/KING3/ W0,RTIDAL,RHO0
	COMMON/KING/RHO,PHI,RAD,NINDEX,DPHI
C----------------------------------------------------------------------
      POTROOT=DSQRT(POT)
      IFAIL=0
      ERF=S15AEF(POTROOT,IFAIL)
      RHO1=0.75D0*PIROOT*DEXP(POT)*ERF
      RHO1=RHO1 - 1.5D0*POTROOT - POT**1.5D0
      RELRHO=RHO1/RHO0     !THE RELATIVE DENSITY AT THE VALUE W=POT
      RETURN
      END
C---------------------------------------------------------------------
      SUBROUTINE FIND_TIDAL(INDEX,RTIDAL)
C---------------------------------------------------------------------
C     subroutine to evaluate the tidal radius position
C     uses parabolic interpolation of last three useful points. The
C     last point is the first point at which the potential becomes
C     negative.
C---------------------------------------------------------------------
	PARAMETER (IDIM=10000)
	IMPLICIT REAL*8 (A-H,O-Z)
	REAL*8 RAD(IDIM),PHI(IDIM),DPHI(IDIM),RHO(IDIM)
	INTEGER*4 INDEX
	COMMON /KING/RHO,PHI,RAD,NINDEX,DPHI
C-----------------------------------------------------------------------
 
C
C     index is the index of the arrays at which the potential becomes
C     negative.  Set up interpolation coefficients
C
      W1=PHI(INDEX-2)
      W2=PHI(INDEX-1)
      W3=PHI(INDEX)
      R1=RAD(INDEX-2)
      R2=RAD(INDEX-1)
      R3=RAD(INDEX)
 
      COEF1=W2*W3/( (W1-W2)*(W1-W3) )
      COEF2=W1*W3/( (W2-W1)*(W2-W3) )
      COEF3=W1*W2/( (W3-W1)*(W3-W2) )
 
 
C
C     evaluate tidal radius
C
      RTIDAL=R1*COEF1 + R2*COEF2 + R3*COEF3
      RETURN
      END
C**********************************************************************
C	FONCTION DERF DE CALCUL DE LA FONCTION D'ERREUR
C**********************************************************************
	DOUBLE PRECISION FUNCTION DERF(X)
	REAL*8 X,P,T,EER
	DOUBLE PRECISION A(5)
	DATA A/.254829592D0,-.284496736D0,1.421413741D0,
	1	-1.453152027D0,1.061405429D0/
	DATA P/0.3275911D0/
	T=1D0/(1D0+P*X)
	EER=DEXP(-1D0*X*X)
	EER=EER*((((A(5)*T+A(4))*T+A(3))*T+A(2))*T+A(1))*T
	DERF=1D0-EER
	END
C--------------------------------------------------------------------
      SUBROUTINE MASS_KING(RADMAX,XMU1,NPTS)
C--------------------------------------------------------------------
C Subroutine to evaluate the integrated mass MU1 up to a given
C radius RADMAX.
C--------------------------------------------------------------------
	PARAMETER (IDIM=10000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 RHO(IDIM),RAD(IDIM),DPHI(IDIM),PHI(IDIM)
	REAL*8 SPLINE1(IDIM),K1(IDIM)
	REAL*8 YY(IDIM)
	COMMON/CONSTANT/PI,PIROOT
	COMMON/KING3/W0,RTIDAL,RHO0
	COMMON/KING/RHO,PHI,RAD,NINDEX,DPHI
	COMMON/SPLINE1/NCAP71,K1,SPLINE1
 
	IF(RADMAX.GT.RTIDAL)THEN
	RADMAX=RTIDAL
	PRINT *,' RADMAX GREATER THAN RTIDAL, SO I SET IT TO RTIDAL'
	ENDIF
 
C Fills the arrays for further interpolation up to RTIDAL:
	RAD(NPTS+1)=RTIDAL
	RHO(NPTS+1)=0.D0
	NPOINT=NPTS+1
 
C Interpolation of the input points with a cubic-spline :
	CALL SPLINE_INTER(RAD,RHO,NPOINT,SPLINE1,NCAP71,K1)
 
C Integration from RADMIN to RADMAX
	RADMIN=RAD(1)
	PRINT *,' RADMIN =',RADMIN
	SUM=XMASS_KING1(RADMIN,RADMAX)
	PRINT *,' RHO0 :',RHO0
	XMU1=4.D0*PI*SUM/RHO0
 
	RETURN
	END
C---------------------------------------------------------------------------
C Function XMASS_KING1
C--------------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION XMASS_KING1(XLOW,XHIGH)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 W(4000)
	INTEGER*4 IW(502)
	EXTERNAL FUN_KING1
 
C Absolute and relative accuracy :
        EPSABS=0.000001
        EPSREL=0.0001
 
C Size of the work areas W(real) and IW(integer)
        LW=4000
        LIW=502
 
C Calling a rather sophisticated integration routine D01AKF :
        IFAIL=1
        CALL D01AKF(FUN_KING1,XLOW,XHIGH,EPSABS,EPSREL,SUM,
	1	ABSERR,W,LW,IW,LIW,IFAIL)
 
        IF(IFAIL.NE.0) THEN
          PRINT *,' WARNING : FAILURE IN D01AKF,  IFAIL =',IFAIL
        END IF
 
	XMASS_KING1=SUM
 
	RETURN
	END
C---------------------------------------------------------------------------
C Function FUN_KING1
C Called by D0AKF
C Generates the value to be integrated at the point X
C---------------------------------------------------------------------------
	DOUBLE PRECISION FUNCTION FUN_KING1(X)
	PARAMETER (IDIM=10000)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 SPLINE1(IDIM),K1(IDIM)
	COMMON/SPLINE1/NCAP71,K1,SPLINE1
	IFAIL=1
	CALL E02BBF(NCAP71,K1,SPLINE1,X,RHOX,IFAIL)
 
	IF(IFAIL.NE.0)THEN
	PRINT 20,IFAIL
20	FORMAT(' **** WARNING ! **** IN E02BBF  IFAIL =',I3,
	1	/,'   (MASS INTEGRATION)')
	PRINT *,' X=',X
	RHOX=0.
	ENDIF
 
C Integral of RHO*(RAD**2):
	FUN_KING1=RHOX*X*X
 
	RETURN
	END
