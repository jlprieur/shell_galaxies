	PROGRAM INTMAGNI
C++--------------------------------------------------------------
C Program INTMAGNI to calculate the integrated magnitude
C of a profile created by PROFILE1, or any other.
C
C Remember, the format of the profiles from PROFILE1 is:
C   -  32 lines of comments,
C   -  NPTS (number of points),
C   -  and in each line:
C        radius,mean surf. bright., number of pixels per annulus
C
C Possibility also of computing the total magnitude
C without using the number of pixels per annulus
C
C JLP Version of 05-12-88
C--------------------------------------------------------------
	PARAMETER (IDIM=1000)
	IMPLICIT REAL*8 (A-H,O-Z)
	REAL*8 X1(IDIM),Y1(IDIM),Y2(IDIM),XX1(IDIM),YY1(IDIM)
	REAL*8 SUM(IDIM),MAGNI(IDIM)
	REAL*8 K1(IDIM),SPLINE1(IDIM),W(6000)
	INTEGER*4 IW(1000)
	CHARACTER ANS*1,NAME*25,IN_FILE*40,IN_COMMENTS*80
	COMMON/SPLINE1/NCAP71,SPLINE1,K1
	EXTERNAL FUN_MAGNI
 
	PI=3.14159265358979323846
 
10	FORMAT(A)
	PRINT *,' PROGRAM INTMAGNI -VERSION OF 05-12-88-'
 
C Reading the input profile ...
	CALL DREADFILE(X1,Y1,NPOINT,X1,Y2,NPOINT2,IDIM,
     1                 IN_FILE,IN_COMMENTS,0)
 
	PRINT *,' ZEROPOINT FOR THE MAGNITUDES (IN COUNTS PER PIXEL) ?'
	READ(5,*) CTMAG1
 
	PRINT *,' SCALE (IN ARCSEC PER PIXEL) ?'
	READ(5,*) SCALE
	CTMAG2=CTMAG1+5.*DLOG10(SCALE)
	PRINT *,' ZEROPOINT FOR MAGNITUDES/SQUARE_ARCSECOND:',CTMAG2
 
C Options :
80	IF(NPOINT2.NE.0)THEN
	  PRINT 31
31	  FORMAT(' MENU :',/,
     1	' 1. INTEGRATION WITH THE NUMBERS OF PIXELS',
     1	' INCLUDED IN THE INPUT PROFILE',/,
     1	' 2. INTEGRATION WITH CUBIC-SPLINE ',
     1	' INTERPOLATION ',/,
     1	' ENTER THE OPTION YOU WANT : ',$)
	  READ(5,*) IOPT
	ELSE
	  PRINT *,' INTEGRATION WITH CUBIC-SPLINE INTERPOLATION '
	  IOPT=2
	ENDIF
 
C
89	PRINT *,' MINIMUM AND MAXIMUM RADII FOR THE INTEGRATION (")'
	READ(5,*) RMIN,RMAX
	 IF(RMAX.LT.RMIN.OR.RMIN.LT.0.)THEN
	   PRINT *,' ERROR WITH THE SELECTED LIMITS :'
	   GO TO 89
	 ENDIF
	 IF(RMAX.GT.X1(NPOINT))THEN
	   PRINT *,' RMAX IS TOO LARGE, I SET IT TO THE MAX VALUE :'
	   RMAX=X1(NPOINT)
	 ENDIF
	PRINT *,' CONSTANT YOU WANT TO SUBTRACT TO THE INPUT VALUES ?'
	READ(5,*) SKY
 
C Search for the limiting indices
 
	IMIN=1
	DO I=1,NPOINT
	  IF(X1(I).GE.RMIN)THEN
	    IMIN=I
	    GO TO 700
	  ENDIF
	END DO
700	IMAX=NPOINT
	DO I=NPOINT,1,-1
	  IF(X1(I).LE.RMAX)THEN
	    IMAX=I
	    GO TO 701
	  ENDIF
	END DO
701	NPTS=IMAX-IMIN+1
 
C Displaying some information on the boundaries :
	PRINT 83,IMIN,IMAX
83	FORMAT(' I1, I2:',I4,2X,I4)
	PRINT 84,X1(IMIN),X1(IMAX)
84	FORMAT(' CORRESPONDING RADII :',F10.4,2X,F10.4)
	Y1MIN=Y1(IMIN)-SKY
	Y1MAX=Y1(IMAX)-SKY
	XMAGMAX=100.
	XMAGMIN=100.
	 IF(Y1MIN.GT.0.)THEN
	  XMAGMIN=-2.5*DLOG10(Y1MIN)+CTMAG2
	 ENDIF
	 IF(Y1MAX.GT.0.)THEN
	  XMAGMAX=-2.5*DLOG10(Y1MAX)+CTMAG2
	 ENDIF
	PRINT 29,Y1MIN,Y1MAX,XMAGMIN,XMAGMAX
29	FORMAT(' CORRESPONDING LEVELS :',E12.4,' TO :',E12.4,/,
     1	' OR IN MAGNITUDE/ARCSEC :',F10.4,3X,F10.4)
C-----------------------------------------------------------------
C Resetting the counters :
	SOM=0.
	DO I=1,IMAX
	 SUM(I)=0.
	 MAGNI(I)=32.
	END DO
 
C-------------------------------------------------------------------
C IOPT=1
C Simple integration, stored in a file SUM(J) where the total magnitude
C can be displayed versus the radius of integration
C-------------------------------------------------------------------
	IF(IOPT.EQ.1)THEN
	 DO I=IMIN,IMAX
	  VV=(Y1(I)-SKY)*Y2(I)
	   IF(VV.LT.0.)PRINT *,' NEGATIVE VALUE !'
	  SOM=SOM+VV
	    DO J=I,IMAX
	     SUM(J)=SUM(J)+VV
	    END DO
	 END DO
	ENDIF
 
C-------------------------------------------------------------------
C IOPT=2
C-------------------------------------------------------------------
	IF(IOPT.EQ.2)THEN
	 CTE1=2.D0*PI/(SCALE*SCALE)
 
C***************
C Interpolates with a cubic-spline :
	 XX1(1)=0.D0
	 YY1(1)=Y1(1)
	 IF(IMAX.LT.10)IMAX=10
	  DO I=1,IMAX+1
	    XX1(I+1)=X1(I)
	    YY1(I+1)=Y1(I)-SKY
	  END DO
	 NPTS=IMAX+2
	 CALL SPLINE_INTER(XX1,YY1,NPTS,SPLINE1,NCAP71,K1)
 
C	 PRINT *,' X ?'
C	 READ(5,*) X
C	 WORK=FUN_MAGNI(X)/X
C	 PRINT *,' Y =',WORK
C	 WORK=X*CTE1
C	 PRINT *,' N =',WORK
 
C***************
C Calling NAG routine D01AKF to integrate the profile
 
	 PRINT 88,RMIN,RMAX
88	 FORMAT(' INTEGRATING FROM RMIN = ',F10.4,
     1	' TO RMAX = ',F10.4)
 
C Absolute and relative accuracy :
	 EPSABS=0.0001
	 EPSREL=0.001
 
C Size of work areas W (real) and IW (integer) :
	 LW=6000
	 LIW=1000
	
	 IFAIL=1
	 ISAFE=0
	 CALL D01AKF(FUN_MAGNI,RMIN,RMAX,EPSABS,EPSREL,RESULT,
     1	ABSERR,W,LW,IW,LIW,IFAIL)
	  IF(IFAIL.NE.0)THEN
	   PRINT *,' WARNING : FAILURE IN D01AKF, IFAIL =',IFAIL
	  ENDIF
	 SOM=RESULT*CTE1
 
	ENDIF
 
C---------------------------------------------------------------------
C Output of the results :
 
	PRINT 86,SOM
86	FORMAT(/,' SUM=',E12.5)
	IF(SOM.GT.0.)THEN
	  VMAG=-2.5*DLOG10(SOM)+CTMAG1
	  PRINT 87,VMAG
87	  FORMAT(' INTEGRATED MAGNITUDE=',F10.4,/)
	ENDIF
 
C--------------------------------------------------------------------	
	IF(IOPT.EQ.1)THEN
	 PRINT *,' DO YOU WANT TO STORE THE RESULT IN A FILE ? (N)'
	 READ(5,10) ANS
	 IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
5	   PRINT *,'NAME OF THE FILE ?'
	   READ(5,10) NAME
	   OPEN(1,FILE=NAME,STATUS='NEW',ERR=5)
	   NPTS=IMAX-IMIN+1
	   WRITE(1,*)NPTS
	     DO I=IMIN,IMAX
	       IF(SUM(I).GT.0.)THEN
	        MAGNI(I)=-2.5*DLOG10(SUM(I))+CTMAG1
	       ENDIF
	      WRITE(1,*)X1(I),MAGNI(I)
	     END DO
	 ENDIF
	ENDIF
 
	PRINT *,' DO YOU WANT ANOTHER TRY ?(Y)'
	READ(5,10) ANS
	IF(ANS.NE.'N'.AND.ANS.NE.'n')GO TO 80
 
	END
C--------------------------------------------------------------
C Function FUN_MAGNI, called by D01AKF.
C Generates the value to be integrated
C at the point X : (2 pi r(pixels) profile)
C--------------------------------------------------------------
	REAL*8 FUNCTION FUN_MAGNI(X)
 
	PARAMETER (IDIM=1000)
	IMPLICIT REAL*8 (A-H,O-Z)
	REAL*8 K1(IDIM),SPLINE1(IDIM)
	COMMON/SPLINE1/NCAP71,SPLINE1,K1
C Calling E02BBF (NAG)
	IFAIL=1
	CALL E02BBF(NCAP71,K1,SPLINE1,X,RESULT,IFAIL)
	 IF(IFAIL.NE.0.AND.ISAFE.LT.20)THEN
 	  ISAFE=ISAFE+1
	  PRINT *,' E02BBF CALLED AT X =',X
	  PRINT *,' WARNING : FAILURE IN E02BBF, IFAIL =',IFAIL
	 ENDIF
 
	FUN_MAGNI=X*RESULT
	RETURN
	END
C--------------------------------------------------------------
C PROJECT.FOR contains SPLINE_INTER
C	include 'jlpsub:project.for'
