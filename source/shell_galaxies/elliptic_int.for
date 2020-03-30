C++------------------------------------------------------------------
C	PROGRAM ELLIPTIC_INT
C Program to check Quinn's results for shell profiles
C (Hernquist and Quinn 1988)
C
C JLP
C Version of 20-06-90
C--------------------------------------------------------------------
	PROGRAM ELLIPTIC_INT
	IMPLICIT REAL*8 (A-H,O-Z)
	PARAMETER (NPTS=100)
	REAL*4 RAD(NPTS),SURF_BRIGHT(NPTS)
 
C Tests :
	PRINT *,' XM ?'
	READ(5,*) XM
	XK=ELLIPTIC_K(XM)
	PRINT *,' ELLIPTIC_K :',XK
	XE=ELLIPTIC_E(XM)
	PRINT *,' ELLIPTIC_E :',XE
 
C Computing the profile:
	D=1.
	STEP=D/REAL(NPTS)
 
C My version :
	DO I=1,NPTS
	  X=REAL(I)*STEP
	  XM=(D-X)/(D+X)
	  SURF_BRIGHT(I)=-2.*X*ELLIPTIC_K(XM)/DSQRT(D+X)
     1	+ 2.*DSQRT(D+X)*ELLIPTIC_E(XM)
	  RAD(I)=X
	END DO
 
C Quinn's version :
C	DO I=1,100
C	  X=REAL(I)*STEP
C	  XM=(D-X)/(D+X)
C	  SURF_BRIGHT(I)=-2.*X*ELLIPTIC_K(XM)/DSQRT(D+X)
C     1	+ 2.*DSQRT(D+X)*ELLIPTIC_E(XM)*D/X
C	  RAD(I)=X
C	END DO
 
C Writing on the output file:
	OPEN(1,FILE='elliptic.dat',STATUS='unknown')
	WRITE(1,*)NPTS
	DO I=1,100
	  WRITE(1,*)RAD(I),SURF_BRIGHT(I)
	END DO
	CLOSE(1)
 
	PRINT *,' Output curve in "elliptic.dat"'
	END
C--------------------------------------------------------------------
C Real*8 function to approximate
C Complete Elliptic integral of the First Kind
C (Abramotwitz et Stegun) (+/- 3. E-05)
C--------------------------------------------------------------------
	REAL*8 FUNCTION ELLIPTIC_K(XM)
	IMPLICIT REAL*8 (A-H,O-Z)
	REAL*8 A(0:2),B(0:2)
	DATA A/1.3862944,0.1119723,0.0725296/
	DATA B/0.5,0.1213478,0.0288729/
	
	XM1=1.-XM
	ELLIPTIC_K=(A(0)+A(1)*XM1+A(2)*XM1*XM1)
     1	+ (B(0)+B(1)*XM1+B(2)*XM1*XM1)*DLOG(1./XM1)
 
	RETURN
	END
C--------------------------------------------------------------------
C Real*8 function to approximate
C Complete Elliptic integral of the Second Kind
C (Abramotwitz et Stegun)  (+/- 4.E-05)
C--------------------------------------------------------------------
	REAL*8 FUNCTION ELLIPTIC_E(XM)
	IMPLICIT REAL*8 (A-H,O-Z)
	REAL*8 A(1:2),B(1:2)
	DATA A/0.4630151,0.1077812/
	DATA B/0.2452727,0.0412496/
	
	XM1=1.-XM
	ELLIPTIC_E=(1.+A(1)*XM1+A(2)*XM1*XM1)
     1	+ (B(1)*XM1+B(2)*XM1*XM1)*DLOG(1./XM1)
 
	RETURN
	END
