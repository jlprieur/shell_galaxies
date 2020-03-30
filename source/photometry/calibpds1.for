       PROGRAM CALIBPDS1
C++:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C Programs CALIBPDS1 and CALIBPDS2 to transform densities into
C intensities with a calibrated profile.
C This program computes a curve Log(Intensity) versus Log(Density) for PDS scans
C when 2 profiles of the same object are available : one from a calibrated plate
C or CCD picture, and the other from the scan. Then fits interactively
C a polynomial to this curve.
C
C Note : To avoid possible negative values in the raw profile
C give a negative value for the sky level of this file (Check this before
C running this program).
C
C JLP  Version of 15-12-86
C--:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	PARAMETER (IDIM=2000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 RAD1(IDIM),RAD2(IDIM),YY1(IDIM),YY2(IDIM)
        REAL*8 PROF1(IDIM),PROF2(IDIM),WORK1(IDIM),WORK2(IDIM)
        REAL*8 K1(IDIM),SPLINE1(IDIM),K2(IDIM),SPLINE2(IDIM)
	CHARACTER ANS*1,IN_FILE*40,IN_COMMENTS*80
 
10	FORMAT(A)
 
C-------------------------------------------------------------------------
C  Reads the calibrated profile.
C
	PRINT *,' CALIBRATED PROFILE :'
	CALL DREADFILE(RAD1,PROF1,NPTS1,WORK1,WORK2,IWORK,IDIM,
     1                 IN_FILE,IN_COMMENTS,0)
	PRINT *,' CONSTANT YOU WANT TO SUBTRACT TO THE INPUT PROFILE',
     1	' (SKY LEVEL) :'
	READ(5,*) SKY1
 
	DO I=1,NPTS1
	PROF1(I)=PROF1(I)-SKY1
	PRINT *,RAD1(I),PROF1(I)
	END DO
 
	PRINT 112,NPTS1,RAD1(1),RAD1(NPTS1)
112	FORMAT(2X,I5,' POINTS RECEIVED',/,' PROFILE FROM ',
     1	F12.3,' ARCSECONDS TO ',F12.3,' ARCSECONDS',/)
 
C Either interpolation or fit :
C	CALL SPLINELOG_INTER(RAD1,PROF1,NPTS1,SPLINE1,NCAP71,K1)
	CALL SPLINELOGFIT(RAD1,PROF1,NPTS1,SPLINE1,NCAP71,K1)
 
C-------------------------------------------------------------------------
C  Reads the density profile.
C
	PRINT *,' DENSITY PROFILE :'
	CALL DREADFILE(RAD2,PROF2,NPTS2,WORK1,WORK2,IWORK,IDIM,
     1                 IN_FILE,IN_COMMENTS,0)
	PRINT *,' CONSTANT YOU WANT TO SUBTRACT TO THE INPUT PROFILE',
     1	' (SKY LEVEL) :'
	READ(5,*) SKY2
 
	DO I=1,NPTS2
	PROF2(I)=PROF2(I)-SKY2
	END DO
 
	PRINT 112,NPTS2,RAD2(1),RAD2(NPTS2)
 
C Either interpolation or fit :
C	CALL SPLINELOG_INTER(RAD2,PROF2,NPTS2,SPLINE2,NCAP72,K2)
	CALL SPLINELOGFIT(RAD2,PROF2,NPTS2,SPLINE2,NCAP72,K2)
 
C-------------------------------------------------------------------------
C Entering the boundaries :
	WRITE (6,339)
339	FORMAT(' Enter start radius and end radius for the fit: ')
	READ (5,*) RSTART,REND
 
C Entering the number of points for the calibration
	PRINT *,' NUMBER OF POINTS FOR THE CALIBRATION ? (MINI=20)'
	READ(5,*) NPOINT
	STEP=(REND-RSTART)/FLOAT(NPOINT)
 
	DO I=1,NPOINT
	RADIUS=RSTART+STEP*FLOAT(I)
	XX=DLOG10(RADIUS)
	IFAIL=1
	CALL E02BBF(NCAP71,K1,SPLINE1,XX,YY1(I),IFAIL)
	IF(IFAIL.NE.0)PRINT 576,IFAIL
576	FORMAT(' WARNING : IN E02BBF  IFAIL=',I3)
	IFAIL=1
	CALL E02BBF(NCAP72,K2,SPLINE2,XX,YY2(I),IFAIL)
	IF(IFAIL.NE.0)PRINT 576,IFAIL
	END DO
 
	CALL INTERACTIVE_FIT(YY2,YY1,NPOINT)
 
	STOP
	END
C---------------------------------------------------------------------
C************************************************************
C Subroutine INTERACTIVE_FIT
C to fit a polynomial to the curve (XX,YY)
C
	SUBROUTINE INTERACTIVE_FIT(XX,YY,NPOINT)
 
	PARAMETER (IDIM=2000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 XXOUT(IDIM),YYOUT(IDIM),XX(IDIM),
     1	YY(IDIM)
	REAL*8 XCOEF(10),SDEV(10),ERR,VVY
	REAL*4 XOUT,YOUT
	REAL*4 XPLOT(IDIM),YPLOT(IDIM),YPLOT2(IDIM)
	CHARACTER ANS*1,TITLE*40,CHAR1*30,CHAR2*30,PLOTDEV*32
 
C Common block with NEWPLOT:
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
 
10	FORMAT(A)
 
C DISPLAYING THE INPUT PROFILE AND FITTING A POLYNOMIAL
	PRINT *,' SET UP 4040/4010, TYPE "RETURN" WHEN READY'
	READ(5,10) ANS
C
 
C Conversion into real*4 for the display
	DO I=1,NPOINT
	XPLOT(I)=XX(I)
	YPLOT(I)=YY(I)
	END DO
 
	CHAR1='LOG10(DENSITY)'
	CHAR2='LOG10(INTENSITY)'
	PLOTDEV='TEKTRO'	!TEKTRONIX
	TITLE='Enter the points for the calibration'
	CALL DISPLAY1(XPLOT,YPLOT,1,NPOINT,
     1	CHAR1,CHAR2,TITLE,PLOTDEV)
 
C Conversion into real*8
	DO I=1,NOUT
	XXOUT(I)=XOUT(I)
	YYOUT(I)=YOUT(I)
	END DO
 
C Fitting a polynomial
205	PRINT *,' ENTER THE DEGREE OF THE POLYNOMIAL :'
	READ(5,*) KDEGREE
	CALL POLYFIT(XXOUT,YYOUT,NOUT,KDEGREE,XCOEF,SDEV,ERR)
	PRINT *,NOUT,' POINTS'
 
	DO I=1,NPOINT
	CALL CALPOLY(XX(I),VVY,XCOEF,KDEGREE)
	YPLOT2(I)=SNGL(VVY)
	END DO
 
C Displaying the fit
	PRINT *,'DISPLAYING THE BACKGROUND, NOW...'
	CHAR1='LOG10(DENSITY)'
	CHAR2='LOG10(INTENSITY)'
	PRINT *,' TITLE ?'
	READ(5,10) TITLE
	CALL DISPLAY2(XPLOT,YPLOT,1,NPOINT,XPLOT,YPLOT2,
     1	1,NPOINT,CHAR1,CHAR2,TITLE,PLOTDEV,'L','L2',' ',' ')
 
208	PRINT 209
209	FORMAT(' DO YOU WANT TO ',
     1	'CHANGE THE DEGREE OF THE POLYNOMIAL ? (N)')
	READ(5,10) ANS
 
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GO TO 205
 
	RETURN
	END
C-------------------------------------------------------------------
c	include 'jlpsub:project.for'
C In POLYFIT.FOR there is CALPOLY and POLYFIT
c	include 'jlpsub:polyfit.for'
 
