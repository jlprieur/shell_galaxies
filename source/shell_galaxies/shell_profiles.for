C++************************************************************
C Program SHELL_PROFILES to generate parameter files for
C computing the profiles. Reads a file with the shell parameters
C (produced by EFIT6 or SHELL_GEOM).
C
C The angular extension for computing the profiles is 0.8 times
C the total extension as read in the shell parameter input file.
C The radial extent is 0.2 to 1.8 times the semi-major axis.
C
C The parameter files can be directly used by PROFILE1, but it can
C be better to reduce the angular extension to get a higher SNR.
C
C SYNTAX:
C      Use it in prompting mode, since there are too many parameters.
C
C JLP Version of 24-07-90
C--************************************************************
C  Structure of the files created by EFIT6 or SHELL_GEOM with all the
C shell parameters (*.EFI) :
C # SHELL_NAME : Shell name				[CHARACTER*40]
C & COMMENTS : Comments on the shell			[CHARACTER*80]
C > VALUES : Values X,Y					[2 REAL VALUES]*NPTS
C \ PARAMETERS : Z1(5,IDIM)				[5 REAL VALUES]
C / ERRORS : ERRZ1(5,IDIM)				[5 REAL VALUES]
C % ANGULAR LIMITS : THEMIN,THEMAX			[2 REAL VALUES]
C
C Format for 2 real values : (X,2(X,E13.6))
C Example for Z(5) : ('\',5(X,E13.6))
C
C Z(1) : Major axis (arcseconds)
C Z(2) : Ellipticity (10*(1-b/a))
C Z(3) : Position angle (degrees)
C Z(4),Z(5) : Position OX, OY of the centre of the galaxy
C             in the initial image (in pixels)
C**********************************************************************
	PROGRAM SHELL_PROFILES
	PARAMETER (IDIM=200,NMAX=200,MAXCOL=6)
	REAL*4 XX(NMAX,IDIM),YY(NMAX,IDIM),Z1(5,IDIM),ERRZ1(5,IDIM)
	REAL*4 THEMIN(IDIM),THEMAX(IDIM)
	REAL*4 X1(NMAX),Y1(NMAX),X2(NMAX),Y2(NMAX)
	REAL*4 XCENTRE(MAXCOL),YCENTRE(MAXCOL)
	INTEGER*4 NPTS(IDIM)
	CHARACTER SHELL_NAME(IDIM)*40,FILENAME(MAXCOL)*40
	CHARACTER*80 COMMENTS(IDIM)
	CHARACTER NAMECAT*40,ANS*1
	COMMON/SPR1/XX,YY,Z1,ERRZ1,THEMIN,THEMAX,SHELL_NAME,
     1	COMMENTS,NPTS,NAMECAT,NSHELL
 
10	FORMAT(A)
	CALL JLP_BEGIN
 
	NSHELL=0
	PRINT 11
11	FORMAT( ' Program SHELL_PROFILES',/,
     1	' Version of 24-07-90',/,
     1	' Before running this program, check that name.EFI',/,
     1	' has the right numbering for the shells...',
     1	' (outermost shell=1, and so on)',/)
 
C********************************************************************
C Reading a file with shell positions and parameters :
C********************************************************************
 
101	  PRINT *,' INPUT FILE ? (created by SHELL_GEOM, or EFIT6)',
     1	' (Convention is name.EFI)'
	  READ(5,10) NAMECAT
	  OPEN(1,FILE=NAMECAT,STATUS='OLD',ERR=101)
 
C Input of the file :
	  CALL SPR_INPUTFILE(1)
	  CLOSE(1)
 
	  PRINT 109,NSHELL
109	  FORMAT(' NUMBER OF SHELLS :',I5)
 
C Prompt for the parameters:
	  WRITE(6,*)' Number of colors you want to study (max=6)'
	  READ(5,*) NCOLORS
	  ILENGTH=INDEX(NAMECAT,'   ')-1
	  WRITE(6,13) NAMECAT(:ILENGTH)
13	  FORMAT(' Enter the names of the images  (without ".BDF")',/,
     1	' The first one should have the same centre as in the *.EFI',
     1	' file (',A,')')
 
	  DO ICOL=1,NCOLORS
	    WRITE(6,14)
14	    FORMAT(' Name :== ',$)
	    READ(5,10) FILENAME(ICOL)
	    WRITE(6,12)
12	    FORMAT(' Location of the center of the galaxy ',
     1	'(X,Y) :== ',$)
	    READ(5,*) XCENTRE(ICOL),YCENTRE(ICOL)
	  END DO
 
	  PRINT *,' Scale of these images: (arcsec/pixel)'
	  READ(5,*) SCALE
 
	  PRINT 16
16	  FORMAT('       Options: ',/,'  1. Shell profiles only ',/,
     1	'  2. Shell and background profiles on one side',/,
     1	'  3. Shell and background profiles on both sides',/,
     1	'       Enter the option you chose:')
	  READ(5,*) KOPT
 
	  CALL SPR_PROFILES(FILENAME,XCENTRE,YCENTRE,NCOLORS,SCALE,
     1	KOPT)
 
C End
	CALL JLP_END
	STOP
	END
 
C**********************************************************************
C Subroutine SPR_PROFILES to generate parameters files used by PROFILE1
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
	SUBROUTINE SPR_PROFILES(FILENAME,XCENTRE,YCENTRE,
     1	NCOLORS,SCALE,KOPT)
	PARAMETER (IDIM=200,NMAX=200,MAXCOL=6)
	REAL*4 XX(NMAX,IDIM),YY(NMAX,IDIM),Z1(5,IDIM),ERRZ1(5,IDIM)
	REAL*4 THEMIN(IDIM),THEMAX(IDIM)
	REAL*4 XCENTRE(MAXCOL),YCENTRE(MAXCOL)
	INTEGER*4 NPTS(IDIM)
	CHARACTER SHELL_NAME(IDIM)*40,COMMENTS(IDIM)*80
	CHARACTER BUFF*80,NAMECAT*40,FILENAME(MAXCOL)*40
	CHARACTER CODE(30)*2,PROF_NAME*40,PARAM_NAME*40
	CHARACTER NPROF(3)*5,NPAR(3)*5
	DATA CODE/'01','02','03','04','05','06','07','08','09',
     1	'10','11','12','13','14','15','16','17','18','19','20',
     1	'21','22','23','24','25','26','27','28','29','30'/
	DATA NPROF/'.PRO ','A.PRO','B.PRO'/
	DATA NPAR/'.PAR ','A.PAR','B.PAR'/
	COMMON/SPR1/XX,YY,Z1,ERRZ1,THEMIN,THEMAX,SHELL_NAME,
     1	COMMENTS,NPTS,NAMECAT,NSHELL
 
10	FORMAT(A)
 
C Main loop on the files (different colours)
	DO ICOL=1,NCOLORS
 
C Get the parameter name:
	   LENGTH_NAME=INDEX(FILENAME(ICOL),'   ')-1
	   IF(LENGTH_NAME.LT.1)THEN
	     PRINT *,' Fatal error: bad filename :',FILENAME(ICOL)
	     STOP
	   ENDIF
 
C According to the option, creates *S01.PRO, *S01A.PRO and *S01B.PRO
	DO KK=1,KOPT
 
C Loop on the shells:
	   DO IS=1,NSHELL
 
	     PARAM_NAME=FILENAME(ICOL)(:LENGTH_NAME)
     1	//'_S'//CODE(IS)//NPAR(KK)
	     PROF_NAME=FILENAME(ICOL)(:LENGTH_NAME)
     1	//'_S'//CODE(IS)//NPROF(KK)
 
C Open the new paramater file:
	    IFLAG=0
887	    OPEN(3,FILE=PARAM_NAME,STATUS='NEW',ERR=888)
 
C When error try with another name:
	    GO TO 889
888	      IFLAG=IFLAG+1
	      PRINT *,' ERROR OPENING THE OUTPUT PROFILE'
	      PARAM_NAME='PROF_'//CODE(IS)//'.PAR'
	      PRINT *,' ATTEMPT WITH ANOTHER NAME :',PARAM_NAME
	      IF(IFLAG.EQ.1)GOTO 887
 
C NAME1 : Name of the input image :
889	    WRITE(3,101)
101	    FORMAT('INPUT FILE :')
	    WRITE(3,10) FILENAME(ICOL)
 
C IOP1=1 Mean profile
	    WRITE(3,102)
102	    FORMAT('TYPE OF PROFILE : 1="NORMAL"  2="MEDIAN" PROFILE',
     1	'    (1) :')
	    WRITE(3,*) '1'
 
C Name of the output profile :
	    WRITE(3,103)
103	    FORMAT('NAME OF THE OUTPUT PROFILE ?')
	    WRITE(3,10) PROF_NAME
 
C  IOP2=1 : Angular sector
	    WRITE(3,104)
104	    FORMAT('OPTION : 1=ANGULAR SECTOR    2=COMPLETE ANNULI',
     1	'   3=SET OF 12 SECTORS (30 DEG)')
	    WRITE(3,*) '1'
 
C  Position angle in degrees :
	    WRITE(3,105)
105	    FORMAT('POSITION ANGLE (TRIGONOMETRIC: OX=0., OY=90.)',
     1	' (DEGREES BETWEEN 0. AND 360.) :')
C Z(3) : Theta (degrees)
	    WRITE(3,*) Z1(3,IS)
 
C  B/A ratio :
	    WRITE(3,106)
106	    FORMAT('AXIS RATIO B/A :')
C Z(2) : Ellipticity (10*(1-b/a))
	    AXRATIO=1.-Z1(2,IS)/10.
	    WRITE(3,*) AXRATIO
 
C  Galaxy centre : X0,Y0
	    WRITE(3,107)
107	    FORMAT('CENTER OF THE GALAXY XC, YC :')
C Z(4),Z(5) : Position OX, OY of the centre of the galaxy
C             in the initial image (in pixels)
	    XC=XCENTRE(ICOL)-XCENTRE(1)+Z1(4,IS)
	    YC=YCENTRE(ICOL)-YCENTRE(1)+Z1(5,IS)
	    WRITE(3,*) XC,YC
 
C Increment for radmin and ratio of two successive increments
	    WRITE(3,108)
108	    FORMAT('FIRST INCREMENT IN RADIUS (IN ARCSEC), AND RATIO',
     1	' OF TWO SUCCESSIVE INCREMENTS')
	    WRITE(3,*) '0.5,1.'
 
C Mean minimum and maximum radii (in arcseconds)
	    WRITE(3,109)
109	    FORMAT('STARTING AND ENDING EQUIVALENT RADIUS (SQRT(A*B),',
     1	' IN ARCSEC.) :')
 
C Z(1) : Major axis (arcseconds)
	    RADMIN1=0.2*Z1(1,IS)
	    RADMAX1=1.8*Z1(1,IS)
	    WRITE(3,*) RADMIN1,RADMAX1
 
C  ANGULAR LIMITS OF THE SECTOR : MIN,MAX  (BETWEEN 0. AND +360.)'
	    WRITE(3,110)
110	    FORMAT('LIMITING ANGLES FOR THE ANGULAR SECTOR (IF COMPLETE',
     1	' ANNULI TYPE 0.,0.) :')
	    IF(THEMIN(IS).GT.THEMAX(IS)) THEMAX(IS)=THEMAX(IS)+360.
 
	     IF(KK.EQ.1)THEN
	       CENTRAL_ANGLE=(THEMAX(IS)+THEMIN(IS))/2.
	       DELTA_ANGLE=0.4*(THEMAX(IS)-THEMIN(IS))
	       ANGLE_MIN = CENTRAL_ANGLE - DELTA_ANGLE
	       ANGLE_MAX = CENTRAL_ANGLE + DELTA_ANGLE
	     ELSEIF(KK.EQ.2)THEN
	       ANGLE_MIN = THEMIN(IS)-20.
	       ANGLE_MAX = THEMIN(IS)-5.
	     ELSE
	       ANGLE_MIN = THEMAX(IS)+5.
	       ANGLE_MAX = THEMAX(IS)+20.
	     ENDIF
	  WRITE(3,*) ANGLE_MIN,ANGLE_MAX
 
C  SKY LEVEL (USED FOR THE OUTPUT IN "profile1.log", BUT NOT
C SUBTRACTED FROM THE PROFILE)
	    WRITE(3,111)
111	    FORMAT('Sky level (only for "profile1.log") :')
	    WRITE(3,*) '0.0000'
 
C  Zero for the magnitudes (per square arcsecond) :'
	    WRITE(3,112)
112	    FORMAT('CONSTANT FOR THE MAGNITUDES (PER SQUARED ARCSEC.) :')
	    WRITE(3,*) '25.0000'
 
C  Scale (in arcsec/pixel)
	    WRITE(3,113)
113	    FORMAT('SCALE IN ARCSEC. PER PIXEL :')
	    WRITE(3,*) SCALE
 
C Number of iterations and sigma rejection
	    WRITE(3,114)
114	    FORMAT('NUMBER OF ITERATIONS, AND SIGMA REJECTION',
     1	' (3 is rather good) :')
	    WRITE(3,*) '8,3'
 
C Bad value to reject (as it can be assigned by some programs fixed
C values when the values have to be rejected)
	    WRITE(3,115)
115	    FORMAT('LEVEL OF THE POINTS TO REJECT  ',
     1	'(PLAGAL: 0.,MOSAIC: 0.) :')
	    WRITE(3,*) '0.0000'
 
	    WRITE(3,116)
116	    FORMAT('Do you want to print the results',
     1	' (in "profile1.log")  (N) ?')
	    WRITE(3,10) 'Y'
 
	    CLOSE(3)
 
	  END DO
	END DO
 
	END DO
 
	RETURN
	END
C**********************************************************************
C Subroutine SPR_INPUTFILE to input a file with the shell positions
C  (the same routine is used in SHELL_GEOM)
C  Structure of the files :
C # SHELL_NAME : Shell name				[CHARACTER*40]
C & COMMENTS : Comments on the shell			[CHARACTER*80]
C > VALUES : Values X,Y					[2 REAL VALUES]*NPTS
C \ PARAMETERS : Z1(5,IDIM)				[5 REAL VALUES]
C / ERRORS : ERRZ1(5,IDIM)				[5 REAL VALUES]
C % ANGULAR LIMITS : THEMIN,THEMAX			[2 REAL VALUES]
C
C Format for 2 real values : (X,2(X,E13.6))
C Example of format for Z(5) : ('\',5(X,E13.6))
C************************************************************
	SUBROUTINE SPR_INPUTFILE(LUNIT)
	PARAMETER (IDIM=200,NMAX=200,NMAX1=1000)
	REAL*4 XX(NMAX,IDIM),YY(NMAX,IDIM),Z1(5,IDIM),ERRZ1(5,IDIM)
	REAL*4 THEMIN(IDIM),THEMAX(IDIM),VALUE(5)
	INTEGER*4 NPTS(IDIM)
	CHARACTER SHELL_NAME(IDIM)*40,COMMENTS(IDIM)*80
	CHARACTER BUFF*80,NAMECAT*40
	COMMON/SPR1/XX,YY,Z1,ERRZ1,THEMIN,THEMAX,SHELL_NAME,
     1	COMMENTS,NPTS,NAMECAT,NSHELL
 
	LU=LUNIT
 
10	FORMAT(A)
	JMAX=IDIM*NMAX
	IS=NSHELL
 
C Main loop :
	DO J=1,JMAX
	  READ(LU,10,END=999)BUFF
C Reading the shell name :
	   IF(BUFF(1:1).EQ.'#')THEN
	    IS=IS+1
	    SHELL_NAME(IS)=BUFF(3:42)
	    PRINT *,IS,' ',SHELL_NAME(IS)
	    NPTS(IS)=0
	   ENDIF
C Reading the comments :
	   IF(BUFF(1:1).EQ.'&')THEN
	    COMMENTS(IS)=' '
	    COMMENTS(IS)=BUFF(3:68)
	    PRINT *,COMMENTS(IS)
	   ENDIF
C Reading the position points X,Y :
	   IF(BUFF(1:1).EQ.'>')THEN
	    NPTS(IS)=NPTS(IS)+1
	    READ(BUFF,202) XX(NPTS(IS),IS),YY(NPTS(IS),IS)
202	    FORMAT(X,2(X,E13.6))
	   ENDIF
C Reading the ellipse parameters Z1(5,IS) :
	   IF(BUFF(1:1).EQ.'\')THEN
	    READ(BUFF,205) (Z1(K,IS),K=1,5)
205	    FORMAT(X,5(X,E13.6))
	   ENDIF
C Reading the errors on these parameters ERRZ1(5,IS) :
	   IF(BUFF(1:1).EQ.'/')THEN
	    READ(BUFF,205) (ERRZ1(K,IS),K=1,5)
	   ENDIF
C Reading the angular limits THEMIN, THEMAX :
	   IF(BUFF(1:1).EQ.'%')THEN
	    READ(BUFF,202) THEMIN(IS),THEMAX(IS)
	   ENDIF
	END DO
 
999	NSHELL=IS
	RETURN
	END
