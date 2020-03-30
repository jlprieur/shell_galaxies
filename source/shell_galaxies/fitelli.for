C++***************************************************************
C Program FITELLI
C  From LEMON (David Carter)
C
C  The procedure is to contour an image at a series of levels
C  determined by a maximum and minimum surface brightness.
C     Each contour is written to a array as a series of
C  coordinates, X and Y, in pixels. The programme EFIT is then
C  called to fit ellipses to the contours and to analyse the
C  deviations from ellipticity. If more than one
C  closed contour in an image exists then the one fitted will
C  be the longest one, provided that the initially specified
C  estimate of the centre lies inside this contour (approx definition).
C
C SYNTAX (for non-interactive use):
C  RUNS FITELLI N [Y/N: auto_sky],[Y/N: auto-improve]
C    input_file xcenter,ycenter scale [[sky_level]] [min,max,step]
C
C Examples:
C       RUNS FITELLI N Y,N test 160,256 0.492 1.7,4.3,0.2
C       RUNS FITELLI N N,N test 160,256 0.492 635.5 1.9,4.6,0.3
C
C Nota1: the value of the sky level is needed only when auto_sky is not wanted
C Nota2: the center of the ellipses is only approximative
C        when -1,-1 is entered, automatic determination with maximum in the image
C Nota3: min,max,step are to compute the levels (in log10 above the sky level)
C Nota4: auto-improve rejects stars on the contours, but requires cpu time...
C
C JLP
C Version of 29-04-98
C--***************************************************************
	PROGRAM FITELLI
        PARAMETER (MAXPOINTS=2000)
	REAL*4 X,Y,XXX,YYY,OX,OY
	REAL*4 XPLOT(1000),YPLOT(1000),AXIS_RATIO,P_ANGLE
	REAL*8 PI,THEMIN,THEMAX,Z,SCALE
	CHARACTER ANS*1,CHAR1*30,CHAR2*30,TITLE*40,PLOTDEV*32
	CHARACTER*60 NAME,NAMEIN,NAMEGRA,NAMEFIT,NAMEEPA,NAMEPAR,NAMEPRO
	CHARACTER COMMENTS*80,ANS2*3
	LOGICAL*4 LONGESTCONT,SKELETON
	LOGICAL*4 AUTOSKY,INTERAC,TALK
	LOGICAL*4 AUTO_GUESS,AUTO_IMPROVE,FOURIER,KAMIKAZE
	INTEGER*4 M,IFLG,MADRID(1),PNTR_IN,NGOOD_CONT
	COMMON /VMR/MADRID
 
C Common blockl with NEWPLOT :
	COMMON/STR_OUTPUT/XOUT(200),YOUT(200),NOUT
 
C Common blocks with CONTOUR_SET :
	COMMON/BLOCK9/ LMAX,NCON
	COMMON/BLOCKB1/XXX(6000),YYY(6000)
	COMMON/CONTOUR1/XCENTRE(1000),YCENTRE(1000),RADMAX(1000),
     1	RADMIN,NCONT1,SKELETON
 
C Common blocks with EFIT :
	COMMON/FIT1/Z(5),M,IFLG
	COMMON/BLOCKB/X(MAXPOINTS),Y(MAXPOINTS)
C    X and Y input arrays for the fit with EFIT
	COMMON/BLOCKA/PI,THEMIN,THEMAX
C    SCALE scale in arcseconds per pixel
	COMMON/BLOCKC/SCALE
 
	PI=3.14159265358979323846D0
10	FORMAT(A)
 
	CALL JLP_BEGIN
 
C Inquire the format of the files :
	CALL JLP_INQUIFMT
 
C Enters the input file
	PRINT *,' Do you want interactive version (y)?'
	READ(5,10) ANS
	  INTERAC=(ANS.NE.'N'.AND.ANS.NE.'n')
	  IF(INTERAC)THEN
C	   PRINT *,' Display device ? (&xterm, ,...)'
C	   READ(5,10) PLOTDEV
           PLOTDEV = '&xterm'
	  ENDIF
 
	PRINT 49
49	FORMAT(' Do you want automatic sky determination, and',
     1	'automatic rejection of bad points ? [Y,N]')
	READ(5,10) ANS2
	  AUTOSKY=(ANS2(1:1).NE.'N'.AND.ANS2(1:1).NE.'n')
	  AUTO_IMPROVE=(ANS2(3:3).EQ.'Y'.OR.ANS2(3:3).EQ.'y')
 
C JLP98: option "_c.fits" (CCDCLEAN corrected files)
C	PRINT *,' NAME of the generic input image: (Will process NAME_c.fits)'
	PRINT *,' NAME of the generic input image: (Will process NAME.fits)'
	READ(5,10) NAME
	LNAME=INDEX(NAME,'  ')-1
	LDOT=INDEX(NAME,'.')-1
	IF(LDOT.GT.1)LNAME=LDOT
C Input image file: 
	WRITE(NAMEIN,299) NAME(1:LNAME)
CJLP98 (CCDCLEAN corrected files)
C299	FORMAT(A,'_c.fits')
299	FORMAT(A,'.fits')
	CALL JLP_VM_READIMAG(PNTR_IN,NX,NY,NAMEIN,COMMENTS)
 
C  Open output files : details, measurements, parameters (for profile) and graphic
C Ascii file with parameters for further profile computation 
	WRITE(NAMEPAR,300) NAME(1:LNAME)
300	FORMAT(A,'.par')
C Graphic file with isophotes
	WRITE(NAMEGRA,301) NAME(1:LNAME)
301	FORMAT(A,'_elli.gra')
C Ascii file with isophote parameters
	WRITE(NAMEFIT,303) NAME(1:LNAME)
303	FORMAT(A,'_elli.fit')
C Special file for galaxy removal (EPA=Ellipse PArameters)
	WRITE(NAMEEPA,304) NAME(1:LNAME)
304	FORMAT(A,'_elli.epa')
C Profile name 
	WRITE(NAMEPRO,305) NAME(1:LNAME)
305	FORMAT(A,'.pro')
 
C File with the details of the program execution
	OPEN(UNIT=9,FILE='fitelli.log',ACCESS='SEQUENTIAL',STATUS='unknown')
	WRITE(9,63)NAME
63	FORMAT(2X,'Name of the generic input file :',A40,/)
 
C File with the output parameters
	OPEN(UNIT=11,FILE=NAMEFIT,ACCESS='SEQUENTIAL',STATUS='unknown')
	WRITE(11,10)NAME
 
C Special file for galaxy removal:
	OPEN(UNIT=15,FILE=NAMEEPA,
     1	ACCESS='SEQUENTIAL',STATUS='unknown')
 
C Graphic file :
	OPEN(UNIT=12,FORM='UNFORMATTED',FILE=NAMEGRA,
     1	ACCESS='SEQUENTIAL',STATUS='unknown')
C
C  Here you need to give the approximate centre of the galaxy in
C  pixels, this is for the initial estimate of the centre for
C  E04GAF; and to help FITELLI decide which contour to fit if more
C  than one exists at a given level. For the next guesses we take the
C  previous value found by E04GAF.
C
	PRINT *,' Approximate centre of the galaxy OX, OY ?'
	READ(5,*) OX,OY
C If automatic center determination:
        IF(OX.EQ.-1.OR.OY.EQ.-1)THEN
          CALL AUTO_CENTER(MADRID(PNTR_IN),NX,NY,NX,OX,OY)
          WRITE(6,298) OX,OY
298       FORMAT(' Automatic center determination: OX=',F6.2,' OY=',F6.2)
        ENDIF
 
C Initial guess for EFIT :
	Z(4)=OX
	Z(5)=OY
C
C  Here you need to give the scale in arcsec/pixel
	PRINT *,' Scale (arcsec/pix)?'
	READ(5,*) SCALE
	WRITE(9,605)SCALE
605	FORMAT(' SCALE =',F8.4,' arcsec/pixel ')
	WRITE(15,11)NAME
11	FORMAT(' FITELLI Version 11-04-98',/,
     1	' Input image: ',A,/,
     1	' Number of levels: (please fill the next line)',/,2X,/
     1	' 1/2 maj.axis (pix), b/a, Theta (deg. with OX=0',
     1	', OY=90), Xcent, Ycent, Level')
 
C  First calculates the sky level of the image KTA in 4 edge zones :
	IF(.NOT.AUTOSKY)THEN
	  PRINT *,' Sky level ?'
	  READ(5,*) SKY
	ELSE
	  CALL AUTO_SKY(MADRID(PNTR_IN),NX,NY,NX,SKY,SIGMA)
	ENDIF
 
	WRITE(9,437) SKY,SIGMA
437	FORMAT(' Sky level and std dev:',2(F12.4,2X))
 
        AXIS_RATIO = 0.
        P_ANGLE = 0.
C****************** Contouring the image ********************
C Entering the surface brightness levels (in log10) :
44	PRINT 440
440	FORMAT(' Enter min, max levels and step (in log10) :')
	READ(5,*) RMIN,RMAX,STEP
 
	WRITE(9,441) STEP,RMIN,RMAX
441	FORMAT(/,2X,'New series of levels:',5X,'STEP =',F8.3,
     1	' FROM',F8.3,' TO',F8.3,' [ in log10(intensity) ]',/)
 
        NGOOD_CONT=0
C Main loop on the levels... (isophote CV (real), level CK)
C First level :
	CK=RMIN
 
C Calling GGROPE to find the contour KV
41	CV=SKY+10.**CK
	LMAX=0
	NCON=0
	LONGESTCONT=.TRUE.
	SKELETON=.FALSE.
	CALL GGROPE(MADRID(PNTR_IN),NX,NY,NX,OX,OY,CV,LONGESTCONT)
 
	WRITE(9,750) CK,CV,NCON,LMAX
	WRITE(6,750) CK,CV,NCON,LMAX
750	FORMAT(/,'******** The level',F7.2,'(log10)  or',
     1	F12.3,' (int)     contains',I6,
     1	' contours',/,
     1	' Length of selected contour',I8,' points')
 
C Goes to next level if no "good" contour found
	IF(LMAX.EQ.0) GO TO 800
 
C  The maximum contour length which EFIT can handle is about 2600 points.
C We set MAXPOINTS=2000.
C  XXX and YYY : list X, Y of the contour
C Here we divide the number of input points by 2 if more than 1500 points :
94	IF(LMAX.LE.1500)GOTO 95
	  LMAX=(LMAX/2)-1
	   DO II=1,LMAX
	    II2=2*II
	    XXX(II)=XXX(II2)
	    YYY(II)=YYY(II2)
	   END DO 
	  WRITE (6,751)
	  WRITE (9,751)
751	  FORMAT(' More than 1500 points : we take one point',
     1	' every second point')
	GOTO 94
	
C-----------------------------------------------------------------------
C Checking the contour if in interactive mode
C-----------------------------------------------------------------------
C  Displays the isophote and gets the points (X,Y) to be fitted
95	IF(INTERAC)THEN
	 PRINT *,' Do you want to check the input contour ? (n)'
	 READ(5,10) ANS
	ELSE
	 ANS='N'
	ENDIF
 
C If the user doesn't want to display the contour goes directly to
C the fitting stage :
	IF(ANS.NE.'Y'.AND.ANS.NE.'y')THEN
	 NOUT=0
	 GO TO 804
	ENDIF
 
	PRINT 809
809	FORMAT(' Now displaying the contour...',/,/,
     1	' -Type "RETURN" without entering points if you want a fit',
     1	/,' with all the points of the calculated contour',/,/,
     1	' -Enter only 1 point if you don''t want to fit this image',
     1	/)
C
C Displaying the contour :
805	TITLE='Enter the points for the fit'
	CHAR1=' X '
	CHAR2=' Y '
	CALL DISPLAY1(XXX,YYY,1,LMAX,CHAR1,CHAR2,
     1	TITLE,PLOTDEV)
C--------------------------------------------------------------------
C If NOUT equals 0 (No points entered) uses the contour instead
C of XOUT, YOUT from DISPLAY1)
804	IF(NOUT.EQ.0)THEN
	  NOUT=LMAX
	   DO I=1,NOUT
	    X(I)=XXX(I)
	    Y(I)=YYY(I)
	   END DO
	ELSE
	  DO I=1,NOUT
	   X(I)=XOUT(I)
	   Y(I)=YOUT(I)
	  END DO
	ENDIF
 
C  Then EFIT is called to fit the contour with X and Y as input arrays
C
	FOURIER=.TRUE.
	KAMIKAZE=.FALSE.
	AUTO_GUESS=.TRUE.
	TALK=.TRUE.
	CALL EFIT(NOUT,5,AUTO_GUESS,AUTO_IMPROVE,FOURIER,KAMIKAZE,
     1	IFAIL,TALK)
 
C Checks IFAIL wich is either the value directly returned by E04HFF,
C or a negative value set when a serious problem is found by EFIT.
C (If 5 < IFAIL < 8, it is not really an error, it is simply a fit which
C is not perfect)
	IF((IFAIL.NE.0).AND.(IFAIL.LT.5.OR.IFAIL.GT.8))THEN
	   PRINT 876,CK
	   WRITE(9,876) CK
876	   FORMAT(' Failure with the level (log10):',F12.3)
 
C Recovering the initial guess for the next fit : (since anything is now
C in Z :
	   Z(4)=OX
	   Z(5)=OY
C If interactive, possibility of another try :
	    IF(INTERAC)THEN
	      GO TO 801
C Else goes to the next level :
	    ELSE
	      GO TO 800
	    ENDIF
	ELSE
C Storing the contour (only a subsample if more than 800)
C Defining the subsample :
	 LMAXPLOT=LMAX
	 ICNT = 1
96	  IF(LMAXPLOT.LE.800)GOTO 97
	   ICNT = ICNT*2
	   LMAXPLOT = LMAXPLOT/2
	  GOTO 96

97	  DO 101 I=1,LMAXPLOT
	   XPLOT(I)=XXX(ICNT*I)
	   YPLOT(I)=YYY(ICNT*I)
101	  CONTINUE
C
C Closes the isophote (if closed contour) :
	 XTEST=AMAX1(ABS(XPLOT(LMAXPLOT)-XPLOT(1)),
     1	ABS(YPLOT(LMAXPLOT)-YPLOT(1)))
	 XTEST=XTEST/Z(1)
	   IF(XTEST.LT.0.005)THEN
	    LMAXPLOT=LMAXPLOT+1
	    XPLOT(LMAXPLOT)=XPLOT(1)
	    YPLOT(LMAXPLOT)=YPLOT(1)
	   ENDIF
C The points are written to disc file unit 12 as a string
	 WRITE(12) LMAXPLOT,(XPLOT(I),YPLOT(I),I=1,LMAXPLOT)
C Stores the parameters :
	 WRITE(12) (Z(KK),KK=1,5)
	 WRITE(15,71) Z(1),Z(2)/Z(1),Z(3)*180./PI,Z(4),Z(5),CV
71	 FORMAT(5(G11.4,1X),G13.6)
C Improvement of the initial position of the centre of the galaxy
C (used for selecting the contours)
	 OX=Z(4)
	 OY=Z(5)
         NGOOD_CONT=NGOOD_CONT+1
	 AXIS_RATIO=AXIS_RATIO+(Z(2)/Z(1))
	 P_ANGLE=P_ANGLE+(Z(3)*180./PI)
	ENDIF
 
C Displaying the contour and the ellipse :
	IF(INTERAC)THEN
	  CHAR1=' X '
	  CHAR2=' Y '
	  TITLE=' Contour and fitted ellipse'
C IOPT=0, COMPLETE ELLIPSE :
	  CALL GENE_ELLIPSE(X,Y,NPOINT,Z,0.,360.,0)
 
	  PRINT *,' Now displaying the fitted ellipse and the contour'
	  CALL DISPLAY2(XXX,YYY,1,LMAX,X,Y,1,NPOINT,CHAR1,
     1	CHAR2,TITLE,PLOTDEV,'L','L',NAME,COMMENTS)
 
801	  PRINT *,' Do you want another try with the same level ? (n)'
	  READ(5,10) ANS
	  IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GO TO 805
	ENDIF
 
C Preparing the next step
800	CK=CK+STEP
	
	IF(CK.LE.RMAX) GO TO 41
 
	IF(INTERAC)THEN
	  PRINT *,' Terminated. do you want another level ? (n)'
	  READ(5,10) ANS
	  IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GO TO 44
	ENDIF
 
	WRITE(9,935)
935	FORMAT(/,' Job completed.',/,'1')
C
C  Two routines ECALC andf FFCALC are called at the end to tidy
C  up the ellipse fit output and Fourier output respectively, and
C  to print it out neatly, with radii in arcsec and position
C  angles in degrees etc in unit 9 (fitelli.log)
C
	CALL ECALC(KK)
	CALL FFCALC
 
C  Then stores the ellipse parameters in unit 11 (NAMEFIT)
	CLOSE(9)
	CLOSE(11)
	CLOSE(12)
	CLOSE(15)
 
	PRINT 936,NAMEGRA,NAMEFIT,NAMEEPA,NAMEPAR
936	FORMAT(' Output in : fitelli.log, fitelli.fou,',
     1    /,14X,A,/,14X,A,/,14X,A,/,14X,A)

C Fill the parameter file:
        IF(NGOOD_CONT.GT.0)THEN
           AXIS_RATIO = AXIS_RATIO / NGOOD_CONT
           P_ANGLE = P_ANGLE / NGOOD_CONT
           CALL WRITE_PARAM(NAMEIN,NAMEPRO,NAMEPAR,OX,OY,
     1                       AXIS_RATIO,P_ANGLE,SKY,SCALE)
        ENDIF

	CALL JLP_END
	STOP
	END
C**********************************************************************
C Subroutine WRITE_PARAM to generate parameters files used by PROFILE1
C
C SHELL_NAME : Shell name				[CHARACTER*40]
C COMMENTS : Comments on the shell			[CHARACTER*80]
C VALUES : Values X,Y					[2 REAL VALUES]*NPTS
C PARAMETERS : Z1(5,IDIM)				[5 REAL VALUES]
C ERRORS : ERRZ1(5,IDIM)				[5 REAL VALUES]
C ANGULAR LIMITS : THEMIN,THEMAX			[2 REAL VALUES]
C KOPT : option for creating the profiles
C
C ********* EXAMPLE OF INPUT PARAMETER FILE : ************************
C               (In "PROFILE1.DOC")
C
C INPUT FILE :
C *
C TYPE OF PROFILE : 1="NORMAL"  2="MEDIAN" PROFILE    (1) :
C *
C NAME OF THE OUTPUT PROFILE ?
C *
C OPTION : 1=ANGULAR SECTOR    2=COMPLETE ANNULI   3=SET OF 12 SECTORS (30 DEG)
C *
C ORIENTATION (TRIGONOMETRIC: OX=0., OY=90.) (DEGREES BETWEEN 0. AND 360.) :
C *
C AXIS RATIO B/A :
C *
C CENTER OF THE GALAXY XC, YC :
C *,*
C FIRST INCREMENT IN RADIUS (IN ARCSEC), AND RATIO OF TWO SUCCESSIVE INCREMENTS
C *,*
C STARTING AND ENDING EQUIVALENT RADIUS (SQRT(A*B), IN ARCSEC.) :
C *,*
C LIMITING ANGLES FOR THE ANGULAR SECTOR (IF COMPLETE ANNULI TYPE 0.,0.) :
C *,*
C SKY LEVEL (ONLY FOR "profile1.log") :
C *
C CONSTANT FOR THE MAGNITUDES (PER SQUARED ARCSEC.) :
C *
C SCALE IN ARCSEC. PER PIXEL :
C *
C NUMBER OF ITERATIONS, AND SIGMA REJECTION (3 is rather good) :
C *,*
C LEVEL OF THE POINTS TO REJECT  (PLAGAL: 0.,MOSAIC: 0.) :
C *
C DO YOU WANT TO PRINT THE RESULTS (IN "profile1.log")  (N) ?
C *
C
C***********************************************************************
	SUBROUTINE WRITE_PARAM(NAMEIN,NAMEPRO,NAMEPAR,XCENTRE,YCENTRE,
     1	AXIS_RATIO,P_ANGLE,SKY_LEVEL,SCALE)
	REAL*4 XCENTRE,YCENTRE,AXIS_RATIO,P_ANGLE,SKY_LEVEL
        REAL*8 SCALE
	CHARACTER NAMEIN*60,NAMEPAR*60,NAMEPRO*60
 
10	FORMAT(A)
 
C Open the new parameter file:
887	OPEN(UNIT=17,FILE=NAMEPAR,STATUS='NEW',ERR=888)
 
C When error try with another name:
	GO TO 889
888	WRITE(6,*) ' Fatal error: parameter file',NAMEPAR,' already created'
	WRITE(9,*) ' Fatal error: parameter file',NAMEPAR,' already created'
        STOP
 
C Image file:
889	WRITE(17,101)
101	FORMAT('INPUT FILE :')
	WRITE(17,10) NAMEIN
 
C IOP1=1 Mean profile
	WRITE(17,102)
102	FORMAT('TYPE OF PROFILE : 1="NORMAL"  2="MEDIAN" PROFILE',
     1	'    (1) :')
	WRITE(17,10) '1'
 
C Name of the output profile :
	WRITE(17,103)
103	FORMAT('NAME OF THE OUTPUT PROFILE ?')
	WRITE(17,10) NAMEPRO
 
C  2 : Complete annuli 
	WRITE(17,104)
104	FORMAT('OPTION : 1=ANGULAR SECTOR    2=COMPLETE ANNULI',
     1	'   3=SET OF 12 SECTORS (30 DEG)')
	WRITE(17,10) '2'
 
C  Position angle in degrees:
	WRITE(17,105) P_ANGLE
105	FORMAT('POSITION ANGLE (TRIGONOMETRIC: OX=0., OY=90.)',
     1	' (DEGREES BETWEEN 0. AND 360.) :',/,F6.1)
 
C  B/A ratio:
	WRITE(17,106) AXIS_RATIO
106	FORMAT('AXIS RATIO B/A :',/,F5.2)
 
C  Galaxy centre:
	WRITE(17,107) XCENTRE,YCENTRE
107	FORMAT('CENTER OF THE GALAXY XC, YC :',/,F6.1,',',F6.1)
 
C Increment for radmin and ratio of two successive increments
	WRITE(17,108)
108	FORMAT('FIRST INCREMENT IN RADIUS (IN ARCSEC), AND RATIO',
     1	' OF TWO SUCCESSIVE INCREMENTS')
	WRITE(17,10) '1.,1.1'
 
C Mean minimum and maximum radii (in arcseconds)
	WRITE(17,109)
109	FORMAT('STARTING AND ENDING EQUIVALENT RADIUS (SQRT(A*B),',
     1	' IN ARCSEC.) :')
	WRITE(17,10) '0.,120.' 
 
C  ANGULAR LIMITS OF THE SECTOR : MIN,MAX  (BETWEEN 0. AND +360.)'
	WRITE(17,110)
110	FORMAT('LIMITING ANGLES FOR THE ANGULAR SECTOR (IF COMPLETE',
     1	' ANNULI TYPE 0.,0.) :')
	WRITE(17,10) '0.,0.' 
 
C  SKY LEVEL (USED FOR THE OUTPUT IN "profile1.log", BUT NOT
C SUBTRACTED FROM THE PROFILE)
        WRITE(17,111) SKY_LEVEL
111	FORMAT('Sky level (only for "profile1.log") :',/,F8.2)
 
C  Zero for the magnitudes (per square arcsecond) :'
	WRITE(17,112)
112	FORMAT('CONSTANT FOR THE MAGNITUDES (PER SQUARED ARCSEC.) :')
	WRITE(17,10) '25.0'
 
C  Scale (in arcsec/pixel)
	WRITE(17,113) SCALE
113	FORMAT('SCALE IN ARCSEC. PER PIXEL :',/,F10.4)
 
C Number of iterations and sigma rejection
	WRITE(17,114)
114	FORMAT('NUMBER OF ITERATIONS, AND SIGMA REJECTION',
     1	' (3 is rather good) :')
	WRITE(17,10) '8,3'
 
C Bad value to reject (as it can be assigned by some programs fixed
C values when the values have to be rejected)
	WRITE(17,115)
115	FORMAT('LEVEL OF THE POINTS TO REJECT  ',
     1	'(PLAGAL: 0.,MOSAIC: 0.) :')
	WRITE(17,10) '-1000.'
 
	WRITE(17,116)
116	FORMAT('Do you want to print the results',
     1	' (in "profile1.log")  (N) ?')
	WRITE(17,10) 'Y'
 
	CLOSE(17)
 
	RETURN
	END
C------------------------------------------------------------------------
	include 'jlpsub:efit.for'
	include 'jlpsub:gene_ellipse.for'
	include 'jlpsub:auto_sky.for'
	include 'jlpsub:contour_set.for'
