C++***************************************************************
C Set of subroutines used by PROFILE1, and PROFILE_QUICK
C
C Contains : MASK_MEAN, ANGLED, READ_PARAM, READ_PARAM_SIMPLEST,
C            OUTPUT_RESULTS, UTILITIES
C
C JLP
C Version of 02-02-94
C
C ******************************************************************
C Subroutine MASK_MEAN
C
C Create a mask where the accepted values are set to 1.,
C and the rejected ones to 0., for MEAN profiles.
C
C SIGMA : sigma of the last iteration of MEAN_PROF2D
C MEAN : mean in the last iteration of MEAN_PROF2D
C
C*******************************************************************
	SUBROUTINE MASK_MEAN(MASK1,ARRAY,IDIM)
	PARAMETER (IDIM1=2000)
	REAL*8 MEAN(IDIM1),RADFIRST(IDIM1),SIGMA(IDIM1)
	REAL*4 MASK1(IDIM,*),ARRAY(IDIM,*)
	INTEGER*4 NBER(IDIM1),NBFIRST(IDIM1)
	LOGICAL MASK_OUT,SECTOR,MEDIAN,MULT_SECT,NORMAL
	CHARACTER NAME1*40,COMMENTS1*160
 
	COMMON /PRF1_DATA/NX,NY,NAME1,COMMENTS1
	COMMON /PRF1_OPTIONS/MASK_OUT,SECTOR,MEDIAN,MULT_SECT
	COMMON /PRF1_PARAM/AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX,
	1	XINCREM,FACT,SCALE,SKY,NBINS,SIGREJEC,BADVALUE,ITERMAX,RVMAG
	COMMON /PRF1_RESULTS/MEAN,SIGMA,NBER,RADFIRST,NBFIRST
	COMMON /PRF1_UTILITIES/AXISMIN,AXISMAX,BB1,DD1,UU,VV,WW,
	1	IXMIN,IXMAX,IYMIN,IYMAX
 
C Logical:
	NORMAL=(AMIN.LE.AMAX)
 
C Erasing the array:
	DO IY=1,NY
	  DO IX=1,NX
	     MASK1(IX,IY)=0.
          ENDDO
        ENDDO

C**********************************************************************
C Main loop :
C Scan only the working area since VAX initializes the arrays to 0
C when they are created :
 
	DO IY=IYMIN,IYMAX
	Y1=FLOAT(IY)-Y0
 
	DO IX=IXMIN,IXMAX
	  X1=FLOAT(IX)-X0
 
	  IF(ARRAY(IX,IY).EQ.BADVALUE) GO TO 2
 
C Check if the pixel lies within the angular limits of the sector :
	    IF (SECTOR) THEN
	     CALL ANGLED(X1,Y1,APOINT)
	     IF(NORMAL)THEN
	       IF((APOINT.LT.AMIN).OR.(APOINT.GT.AMAX)) GO TO 2
	     ELSE
	       IF((APOINT.LT.AMIN).AND.(APOINT.GT.AMAX)) GO TO 2
	     ENDIF
	    ENDIF
 
C F2: Semi major axis of the ellipse containing the current pixel (in arcsec)
	  F2=SCALE*SQRT(UU*X1*X1+VV*Y1*Y1+WW*X1*Y1)
 
C KBIN : Index of the profile bin corresponding to the current pixel
	  KBIN=1+INT(SQRT(BB1*BB1+DD1*(F2-AXISMIN))-BB1)
 
	    IF(KBIN.GE.1.AND.KBIN.LE.NBINS) THEN
	     SIG3=SIGREJEC*SIGMA(KBIN)
	     WORK=ABS(ARRAY(IX,IY)-MEAN(KBIN))
 
C Set to 1. the points selected by the profile for the mean
	      IF(WORK.LE.SIG3.OR.NBFIRST(KBIN).LE.8)MASK1(IX,IY)=1.
	    ENDIF
 
2	 END DO
 
	END DO
 
	RETURN
	END
C***********************************************************************
C	SUBROUTINE ANGLED
C To work out the position angle of the current pixel
C
C Input :
C X, Y : Coordinates relative to the centre of the galaxy
C
C Output:
C ANG in degrees (between 0. and 360)
C***********************************************************************
	SUBROUTINE ANGLED(X,Y,ANG)
	REAL*4 X,Y,ANG
        REAL*4 PI
        DATA PI/3.1415927/
 
	RADIUS=SQRT(X*X+Y*Y)
	IF(RADIUS.EQ.0.) THEN
            ANG = 0.
        ELSE
	    COSINUS=X/RADIUS
C 0 < ANG < 180
	    ANG=ACOS(COSINUS)
            ANG=ANG*180./PI 
	    IF(Y.LT.0.) ANG=360.-ANG
        ENDIF

	RETURN
	END
C***********************************************************************
C Subroutine READ_PARAM
C
C Input:
C NAMEPAR : Name of the parameter file
C
C Output
C All the content of common blocks /PRF1_DATA/, /PRF1_PARAM/
C and /PRF1_OPTIONS
C
C ********* EXAMPLE OF INPUT PARAMETER FILE : ************************
C               (In "PROFILE1.DOC")
C
C INPUT FILE : (no longer used as input)
C *
C TYPE OF PROFILE : 1="NORMAL"  2="MEDIAN" PROFILE    (1) :
C *
C NAME OF THE OUTPUT PROFILE ? (no longer used as input)
C *
C OPTION : 1=ANGULAR SECTOR    2=COMPLETE ANNULI   3=SET OF 12 SECTORS (30 DEG)
C *
C ORIENTATION (TRIGONOMETRIC: OX=0., OY=90.) (DEGREES BETWEEN 0. AND 360.) :
C *
C AXIS RATIO B/A :
C *
C CENTER OF THE GALAXY XC, YC : (-1,-1 if galaxy centered)
C *,*
C FIRST INCREMENT IN RADIUS (IN ARCSEC), AND RATIO OF TWO SUCCESSIVE INCREMENTS
C *,*
C STARTING AND ENDING EQUIVALENT RADIUS (SQRT(A*B), IN ARCSEC.) :
C *,*
C LIMITING ANGLES FOR THE ANGULAR SECTOR (IF COMPLETE ANNULI TYPE 0.,0.) :
C *,*
C SKY LEVEL (ONLY FOR "PROFILE1.DAT") :
C *
C CONSTANT FOR THE MAGNITUDES (PER SQUARED ARCSEC.) :
C *
C SCALE IN ARCSEC. PER PIXEL :
C *
C NUMBER OF ITERATIONS, AND SIGMA REJECTION (3 is rather good) :
C *,*
C LEVEL OF THE POINTS TO REJECT  (PLAGAL: 0.,MOSAIC: 0.) :
C *
C DO YOU WANT TO PRINT THE RESULTS (IN "PROFILE1.DAT)  (N) ?
C *
C
C***********************************************************************
	SUBROUTINE READ_PARAM(NAMEPAR)
	PARAMETER (IDIM=600)
        REAL*4 AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX
        REAL*4 XINCREM,FACT,SCALE,SKY,SIGREJEC,BADVALUE,RVMAG
        REAL*4 RADMAXI
        INTEGER*4 NBINS,ITERMAX,NX,NY
	LOGICAL MASK_OUT,SECTOR,MEDIAN,MULT_SECT
	CHARACTER NAME1*40,NAMEPRO*40,NAMEPAR*40,COMMENTS1*160
	CHARACTER BUFFER*80,ANS*1
	
	COMMON /PRF1_DATA/NX,NY,NAME1,COMMENTS1
	COMMON /PRF1_OPTIONS/MASK_OUT,SECTOR,MEDIAN,MULT_SECT
	COMMON /PRF1_PARAM/AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX,
	1	XINCREM,FACT,SCALE,SKY,NBINS,SIGREJEC,BADVALUE,ITERMAX,RVMAG
 
10	FORMAT(A)
 
C Opening the parameter input file :
12	OPEN(UNIT=10,FILE=NAMEPAR,STATUS='OLD',
	1	ACCESS='SEQUENTIAL',ERR=997)
 
C NAME1 : Name of the input file :
	READ(10,10,ERR=999)BUFFER
C JLP94, READ(10,10,ERR=999)NAME1 replaced by:
	READ(10,10,ERR=999)BUFFER
 
C**** Input of the parameters for the profile ******
 
 
C IOP1=1 Mean profile
C IOP1=2 Median profile
	READ(10,10,ERR=999)BUFFER
	READ(10,*,ERR=999)IOP1
	MEDIAN=(IOP1.EQ.2)
	IF(MEDIAN)WRITE(3,893)
893	FORMAT(' MEDIAN PROFILE WITH COMPLETE ANNULI',/)
 
C Name of the output profile :'
	READ(10,10,ERR=999)BUFFER
C JLP94, READ(10,10,ERR=999)NAMEPRO replaced by:
	READ(10,10,ERR=999)BUFFER
 
C  IOP2=1 : Profile within an angular sector
C  IOP2=2 : Complete annuli
C  IOP2=3 : Multiple sectors
	READ(10,10,ERR=999)BUFFER
	READ(10,*,ERR=999)IOP2
	MULT_SECT=(IOP2.EQ.3)
	SECTOR=(IOP2.EQ.1)
	  IF(SECTOR)THEN
	     WRITE(3,890)
890	     FORMAT(' PROFILE WITHIN AN ANGULAR SECTOR',/)
	  ELSEIF(MULT_SECT)THEN
	     WRITE(3,891)
891	     FORMAT(' SET OF 12 PROFILES ',/)
C Set SECTOR true (for internal loop of profile computation)
	     SECTOR=.TRUE.
	  ELSE
	     WRITE(3,892)
892	     FORMAT(' PROFILE WITH COMPLETE ANNULI',/)
	  ENDIF
 
C  Position angle in degrees :
	READ(10,10,ERR=999)BUFFER
	READ(10,*,ERR=999)PHI
 
C  B/A ratio :
	READ(10,10,ERR=999)BUFFER
	READ(10,*,ERR=999)AXRATIO
 
C  Galaxy centre : X0,Y0
	READ(10,10,ERR=999)BUFFER
	READ(10,*,ERR=999)X0,Y0
C If X0=-1 Y0=-1, take the center of the frame as center for the profile:
        IF(X0.EQ.-1..AND.Y0.EQ.-1.)THEN
          X0 = FLOAT(NX/2 + 1)
          Y0 = FLOAT(NY/2 + 1)
          WRITE(6,871) X0,Y0 
871       FORMAT(' OK: I take the center as (X0=',F10.3,',Y0=',F10.3,')')
        ENDIF
 
C Increment for radmin and ratio of two successive increments
	READ(10,10,ERR=999)BUFFER
	READ(10,*,ERR=999)XINCREM,FACT
 
C Mean minimum and maximum radii (in arcseconds)
	READ(10,10,ERR=999)BUFFER
	READ(10,*,ERR=999)RADMIN1,RADMAX1
C Exit if RADMIN > RADMAX:
	IF(RADMIN1.GE.RADMAX1)THEN
	  PRINT 79
	  WRITE(3,*) 79
79	  FORMAT(' Fatal error: RADMIN >= RADMAX')
	  CLOSE(3)
	  CLOSE(10)
	  STOP
	ENDIF
 
C  ANGULAR LIMITS OF THE SECTOR : MIN,MAX  (BETWEEN 0. AND +360.)'
	READ(10,10,ERR=999)BUFFER
	READ(10,*,ERR=999)AMIN,AMAX
C Conversion to values between 0 and 360 degrees:
	IF(AMIN.LT.0.)AMIN=AMIN+360.
	IF(AMAX.LT.0.)AMAX=AMAX+360.
 
C  SKY LEVEL (USED FOR THE OUTPUT IN "PROFILE1.DAT", BUT NOT
C SUBTRACTED FROM THE PROFILE)
	READ(10,10,ERR=999)BUFFER
	READ(10,*,ERR=999)SKY
 
C  Zero for the magnitudes (per square arcsecond) :'
	READ(10,10,ERR=999)BUFFER
	READ(10,*,ERR=999)RVMAG
 
C  Scale (in arcsec/pixel)
	READ(10,10,ERR=999)BUFFER
	READ(10,*,ERR=999)SCALE
 
C Check consistency with RADMAX:
        RADMAXI=SQRT(FLOAT(NX*NX + NY*NY))
        RADMAXI=RADMAXI*SCALE
        IF(RADMAX1.GT.RADMAXI)THEN
           WRITE(6,872) RADMAXI,RADMAX1
           WRITE(3,872) RADMAXI,RADMAX1
872        FORMAT(' WARNING: Maximum radius here is ',G12.5,
     1            'arcsec (diagonal of the frame)',/,
     1            ' Hence I replace your input value of',G12.5,
     1            ' arcsec by this new value')
           RADMAX1=RADMAXI
        ENDIF
	WRITE(3,873)PHI,AXRATIO,X0,Y0,XINCREM,FACT,RADMIN1,RADMAX1
873	FORMAT(' ELLIPSE PARAMETERS:',/,
	1	5X,'POSITION ANGLE :',
	1	F7.3,5X,'B/A:',F7.3,/,5X,'CENTRE:     X0=',
	1	F7.3,5X,'Y0=',F7.3,/,4X,
	1	' FIRST INCREMENT (ARCSEC) :',F7.3,/,4X,
	1	' RATIO OF TWO SUCCESSIVE INCREMENTS :',F8.5,/,
	1	' PROFILE COMPUTED BETWEEN THE EQUIVALENT RADII :',
	1	'  RADMIN1=',F7.2,
	1	'   RADMAX1=',F7.2,' (ARCSEC)',/)
 
C Number of iterations and sigma rejection
	READ(10,10,ERR=999)BUFFER
	READ(10,*,ERR=999)ITERMAX,SIGREJEC
	 IF(SIGREJEC.LE.0)THEN
	  PRINT *,' FATAL ERROR : NEGATIVE OR NULL REJECTION RATE'
	  STOP
	 ENDIF
 
C Bad value to reject (as it can be assigned by some programs fixed
C values when the values have to be rejected)
	READ(10,10,ERR=999)BUFFER
	READ(10,*,ERR=999)BADVALUE
 
	IF(SECTOR)WRITE(3,875)AMIN,AMAX
875	FORMAT('  ANGULAR LIMITS OF THE SECTOR :',2X,F7.2,5X,F7.2,/)
 
	READ(10,10,ERR=999)BUFFER
	READ(10,10,ERR=999)ANS
C	WRITE(3,876)ANS
876	FORMAT(' DO YOU WANT TO PRINT THE PROFILE ? (N) : ',A)
C	PRINT_RESULTS=(ANS.EQ.'Y'.OR.ANS.EQ.'y')
 
	CLOSE (10)
	RETURN
 
C Error messages 
C ?? (Fatal errors since this program is designed to be
C used in Batch):
997	PRINT *,' Error opening the parameter input file'
        PRINT *,' Try again, enter the filename:'
        READ(5,10) NAMEPAR
        GOTO 12
	STOP
999	PRINT *,' ERROR WHILE READING THE PARAMETER INPUT FILE'
	STOP
998	PRINT *,' ERROR OPENING THE OUTPUT PROFILE'
	STOP
 
	END
C***********************************************************************
C Subroutine READ_PARAM_SIMPLEST
C
C Input:
C NAMEPAR : Name of the parameter file
C
C WARNING: SECTOR, PHI, AMIN, AMAX should be set 
C in the common blocks before calling this routine!
C
C Output
C All the content of common blocks /PRF1_DATA/, /PRF1_PARAM/
C and /PRF1_OPTIONS
C
C ********* EXAMPLE OF INPUT PARAMETER FILE : ************************
C               (In "PROFILE1.DOC")
C
C INPUT FILE : (no longer used as input)
C *
C TYPE OF PROFILE : 1="NORMAL"  2="MEDIAN" PROFILE    (1) :
C *
C NAME OF THE OUTPUT PROFILE ? (no longer used as input)
C *
C OPTION : 1=ANGULAR SECTOR    2=COMPLETE ANNULI   3=SET OF 12 SECTORS (30 DEG)
C *
C ORIENTATION (TRIGONOMETRIC: OX=0., OY=90.) (DEGREES BETWEEN 0. AND 360.) :
C *
C AXIS RATIO B/A :
C *
C CENTER OF THE GALAXY XC, YC : (-1,-1 if galaxy centered)
C *,*
C FIRST INCREMENT IN RADIUS (IN ARCSEC), AND RATIO OF TWO SUCCESSIVE INCREMENTS
C *,*
C STARTING AND ENDING EQUIVALENT RADIUS (SQRT(A*B), IN ARCSEC.) :
C *,*
C LIMITING ANGLES FOR THE ANGULAR SECTOR (IF COMPLETE ANNULI TYPE 0.,0.) :
C *,*
C SKY LEVEL (ONLY FOR "PROFILE1.DAT") :
C *
C CONSTANT FOR THE MAGNITUDES (PER SQUARED ARCSEC.) :
C *
C SCALE IN ARCSEC. PER PIXEL :
C *
C NUMBER OF ITERATIONS, AND SIGMA REJECTION (3 is rather good) :
C *,*
C LEVEL OF THE POINTS TO REJECT  (PLAGAL: 0.,MOSAIC: 0.) :
C *
C DO YOU WANT TO PRINT THE RESULTS (IN "PROFILE1.DAT)  (N) ?
C *
C
C***********************************************************************
	SUBROUTINE READ_PARAM_SIMPLEST(NAMEPAR)
	PARAMETER (IDIM=600)
        REAL*4 AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX
        REAL*4 XINCREM,FACT,SCALE,SKY,SIGREJEC,BADVALUE,RVMAG
        REAL*4 RADMAXI
        INTEGER*4 NBINS,ITERMAX,NX,NY
	LOGICAL MASK_OUT,SECTOR,MEDIAN,MULT_SECT
	CHARACTER NAME1*40,NAMEPRO*40,NAMEPAR*40,COMMENTS1*160
	CHARACTER BUFFER*80,ANS*1
	
	COMMON /PRF1_DATA/NX,NY,NAME1,COMMENTS1
	COMMON /PRF1_OPTIONS/MASK_OUT,SECTOR,MEDIAN,MULT_SECT
	COMMON /PRF1_PARAM/AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX,
	1	XINCREM,FACT,SCALE,SKY,NBINS,SIGREJEC,BADVALUE,ITERMAX,RVMAG
 
10	FORMAT(A)
 
C Opening the parameter input file :
12	OPEN(UNIT=10,FILE=NAMEPAR,STATUS='UNKNOWN',
	1	ACCESS='SEQUENTIAL',ERR=997)
 
C NAME1 : Name of the input file :
	WRITE(10,*,ERR=999)' INPUT FILE : (no longer used as input)'
	WRITE(10,*,ERR=999)'*'
 
C**** Input of the parameters for the profile ******
	WRITE(10,*,ERR=999)
     1  'TYPE OF PROFILE : 1="NORMAL"  2="MEDIAN" PROFILE    (1) :'
	WRITE(10,*,ERR=999)'1'
	MEDIAN=.FALSE.
 
C Name of the output profile :'
	WRITE(10,*,ERR=999)
     1  ' NAME OF THE OUTPUT PROFILE ? (no longer used as input)'
	WRITE(10,*,ERR=999)'*'
 
C  IOP2=1 : Profile within an angular sector
C  IOP2=2 : Complete annuli
C  IOP2=3 : Multiple sectors
	WRITE(10,*,ERR=999)
     1  'OPTION : 1=ANGULAR SECTOR    2=COMPLETE ANNULI'
     1  '   3=SET OF 12 SECTORS (30 DEG)'
C SECTOR should be set before calling this routine:
        IF(SECTOR)THEN
	  WRITE(10,*,ERR=999)'1'
        ELSE
	  WRITE(10,*,ERR=999)'2'
        ENDIF
	MULT_SECT=.FALSE.
 
C  Position angle in degrees :
	WRITE(10,*,ERR=999)
     1  ' ORIENTATION (TRIGONOMETRIC: OX=0., OY=90.)',
     1  ' (DEGREES BETWEEN 0. AND 360.):'
	WRITE(10,'(F5.1)',ERR=999)PHI
 
C  B/A ratio :
        AXRATIO=1.
	WRITE(10,*,ERR=999) ' AXIS RATIO B/A :'
	WRITE(10,'(F3.1)',ERR=999)AXRATIO
 
C  Galaxy centre : X0,Y0
	WRITE(10,*,ERR=999)
     1  ' CENTER OF THE GALAXY XC, YC : (-1,-1 if galaxy centered)'
C I take the center of the frame as center for the profile:
        X0 = FLOAT(NX/2 + 1)
        Y0 = FLOAT(NY/2 + 1)
	WRITE(10,871,ERR=999)X0,Y0
871     FORMAT(F7.1,',',F7.1)
 
C Increment for radmin and ratio of two successive increments
	WRITE(10,*,ERR=999)
     1  ' FIRST INCREMENT IN RADIUS (IN ARCSEC),',
     1  ' AND RATIO OF TWO SUCCESSIVE INCREMENTS'
        XINCREM=1.
        FACT=1.
	WRITE(10,872,ERR=999)XINCREM,FACT
872     FORMAT(F3.1,',',F3.1)
 
C Mean minimum and maximum radii (in arcseconds)
	WRITE(10,*,ERR=999)
     1  ' STARTING AND ENDING EQUIVALENT RADIUS (SQRT(A*B), IN ARCSEC.) :'
        RADMIN1=0.
C I take the radius of the maximum circle which fits in the image: 
        IF(NX.LE.NY)THEN
         RADMAX1=NX/2
        ELSE
         RADMAX1=NY/2
        ENDIF
	WRITE(10,875,ERR=999)RADMIN1,RADMAX1
875     FORMAT(F7.1,',',F7.1)
 
C  ANGULAR LIMITS OF THE SECTOR : MIN,MAX  (BETWEEN 0. AND +360.)'
	WRITE(10,*,ERR=999)
     1  ' LIMITING ANGLES FOR THE ANGULAR SECTOR',
     1  ' (IF COMPLETE ANNULI TYPE 0.,0.) :'
C Conversion to values between 0 and 360 degrees:
	IF(AMIN.LT.0.)AMIN=AMIN+360.
	IF(AMAX.LT.0.)AMAX=AMAX+360.
	WRITE(10,875,ERR=999)AMIN,AMAX
 
C  SKY LEVEL (USED FOR THE OUTPUT IN "PROFILE1.DAT", BUT NOT
C SUBTRACTED FROM THE PROFILE)
	WRITE(10,*,ERR=999)' SKY LEVEL (ONLY FOR "PROFILE1.DAT") :'
        SKY=0.
	WRITE(10,'(F3.1)',ERR=999)SKY
 
C  Zero for the magnitudes (per square arcsecond) :'
	WRITE(10,*,ERR=999)
     1  ' CONSTANT FOR THE MAGNITUDES (PER SQUARED ARCSEC.) :'
        RVMAG=25.
	WRITE(10,'(F4.1)',ERR=999)RVMAG
 
C  Scale (in arcsec/pixel)
	WRITE(10,*,ERR=999)
     1  ' SCALE IN ARCSEC. PER PIXEL :'
        SCALE=1.
	WRITE(10,'(F3.1)',ERR=999)SCALE
 
	WRITE(3,873)PHI,AXRATIO,X0,Y0,XINCREM,FACT,RADMIN1,RADMAX1
873	FORMAT(' ELLIPSE PARAMETERS:',/,
	1	5X,'POSITION ANGLE :',
	1	F7.3,5X,'B/A:',F7.3,/,5X,'CENTRE:     X0=',
	1	F7.3,5X,'Y0=',F7.3,/,4X,
	1	' FIRST INCREMENT (ARCSEC) :',F7.3,/,4X,
	1	' RATIO OF TWO SUCCESSIVE INCREMENTS :',F8.5,/,
	1	' PROFILE COMPUTED BETWEEN THE EQUIVALENT RADII :',
	1	'  RADMIN1=',F7.2,
	1	'   RADMAX1=',F7.2,' (ARCSEC)',/)
 
C Number of iterations and sigma rejection
	WRITE(10,*,ERR=999)
     1  ' NUMBER OF ITERATIONS, AND SIGMA REJECTION (3 is rather good) :'
        ITERMAX=5
        SIGREJEC=3
	WRITE(10,874,ERR=999)ITERMAX,SIGREJEC
874     FORMAT(I2,',',F4.2)
 
C Bad value to reject (as it can be assigned by some programs fixed
C values when the values have to be rejected)
	WRITE(10,*,ERR=999)
     1  ' LEVEL OF THE POINTS TO REJECT  (PLAGAL: 0.,MOSAIC: 0.) :'
        BADVALUE=-12345
	WRITE(10,*,ERR=999)'-12345'
 
	IF(SECTOR)WRITE(3,876)AMIN,AMAX
876	FORMAT('  ANGULAR LIMITS OF THE SECTOR :',2X,F7.2,5X,F7.2,/)
 
	WRITE(10,*,ERR=999)
     1  ' DO YOU WANT TO PRINT THE RESULTS (IN "PROFILE1.DAT)  (N) ?'
	WRITE(10,*,ERR=999)'Y'
C	PRINT_RESULTS=(ANS.EQ.'Y'.OR.ANS.EQ.'y')
 
	CLOSE (10)
	RETURN
 
C Error messages 
C ?? (Fatal errors since this program is designed to be
C used in Batch):
997	PRINT *,' Error opening the (temporary) parameter input file'
	STOP
999	PRINT *,' Error while writing to the (temporary) parameter file'
	STOP
998	PRINT *,' Error opening the output profile'
	STOP
 
	END
C ***************************************************************************
C Subroutine to output the results
C
C Output in unit 2 (Profile),   and unit 3 (PROFILE1.DAT)
C ***************************************************************************
	SUBROUTINE OUTPUT_RESULTS(NAMEPAR,NAMEPRO)
	PARAMETER (IDIM1=2000)
	REAL*8 MEAN1
	REAL*8 MEAN(IDIM1),RADFIRST(IDIM1),SUM1(IDIM1),SIGMA(IDIM1)
	INTEGER*4 NBFIRST(IDIM1),NBER(IDIM1)
	LOGICAL MASK_OUT,SECTOR,MEDIAN,MULT_SECT
	CHARACTER NAME1*40,NAMEPAR*40,NAMEPRO*40,COMMENTS1*160
	CHARACTER BUFFER*80
 
	COMMON /PRF1_DATA/NX,NY,NAME1,COMMENTS1
	COMMON /PRF1_OPTIONS/MASK_OUT,SECTOR,MEDIAN,MULT_SECT
	COMMON /PRF1_PARAM/AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX,
	1	XINCREM,FACT,SCALE,SKY,NBINS,SIGREJEC,BADVALUE,ITERMAX,RVMAG
	COMMON /PRF1_RESULTS/MEAN,SIGMA,NBER,RADFIRST,NBFIRST
 
10	FORMAT(A)
	IERR=0
992	IERR=IERR+1
	IF(IERR.GT.1)THEN
	 PRINT *,' ERROR OPENING THE OUTPUT PROFILE'
	 PRINT *,' ATTEMPT WITH ANOTHER NAME : PROF_01.PRO'
	 NAMEPRO='PROF_01.PRO'
	ENDIF
 
C Opening the output profile :
	WRITE(3,871)NAMEPRO
871	FORMAT(' OUTPUT PROFILE:',A,/)
C JLP97: I first try with "status=unknown", but it appended the file with IBM...
C	OPEN(2,FILE=NAMEPRO,STATUS='unknown',ERR=992)
	OPEN(2,FILE=NAMEPRO,STATUS='new',ERR=998)
 
C RTMAG: zero magnitude per pixel
	RTMAG=RVMAG+5.*ALOG10(SCALE)
 
	WRITE(3,874)NBINS,RVMAG,ITERMAX,SIGREJEC,SKY,SCALE
874	FORMAT(' NUMBER OF BINS :',I5,5X,
	1	' ZERO FOR THE MAGNITUDES : (PER SQ. ARCSEC)',F7.3,5X,
	1	/,' NBER OF ITERATIONS : ',I2,5X,'SIGMA REJECTION',F7.3,
	1	/,' SKY LEVEL : ',F7.3,
	1	' (SUBTRACTED TO COMPUTE THE MAGNITUDES)',/
	1	' SCALE : ',F7.3,'"/PIXEL',/,
	1	' NB: R=SQRT(A*B) IS THE EQUIV. RADIUS IN ARCSEC',/)
 
	SUM1(1)=FLOAT(NBFIRST(1))*MEAN(1)
	DO 600 KK=2,NBINS
	SUM1(KK)=SUM1(KK-1)+FLOAT(NBFIRST(KK))*(MEAN(KK)-SKY)
600	CONTINUE
 
	 WRITE(3,501)
501	 FORMAT(2X,'    R    ',1X,' R 1/4 ',2X,'   MEAN  ',
	1	'INITIAL NB',1X,'FINAL NB',
	1	' STD DEVIATION',' MEAN MAG',2X,'INT MAG')
 
	TMAG=100.
	
	NPTS=0
	DO 400 L=1,NBINS
	  IF(RADFIRST(L).EQ.0..OR.NBFIRST(L).EQ.0)GO TO 400
	  NPTS=NPTS+1
	  R4=RADFIRST(L)**0.25
	  VMAG=100.
	  MEAN1=MEAN(L)-SKY
	  IF(MEAN1.GT.0..AND.SUM1(L).GT.0.) THEN
	   VMAG=-2.5*DLOG10(MEAN1)+RVMAG
	   TMAG=-2.5*DLOG10(SUM1(L))+RTMAG
	  ENDIF
 
	IF(L.LT.20)
	1	WRITE(3,500)RADFIRST(L),R4,MEAN(L),NBFIRST(L),NBER(L),
	1	SIGMA(L),VMAG,TMAG
500	 FORMAT(1X,F9.2,1X,F7.3,2X,E12.3,2X,I6,2X,I6,2X,E12.3,
	1	2X,F7.2,2X,F7.2)
 
400	CONTINUE
 
 
C Re-opening the parameter input file :
	OPEN(UNIT=10,FILE=NAMEPAR,STATUS='OLD',
	1	ACCESS='SEQUENTIAL',ERR=997)
 
C Copying the parameters of the profile in the output profile :
C (Adding an extra space to avoid problems when printing it)
	DO I=1,32
	  READ(10,10,ERR=997)BUFFER
C On line 2, write the actual input image name: 
	  IF(I.EQ.2)THEN
	    BUFFER=' '
	    WRITE(BUFFER,*) NAME1 
	  ENDIF
C On line 6, write the actual output profile name: 
	  IF(I.EQ.6)THEN
	    BUFFER=' '
	    WRITE(BUFFER,*) NAMEPRO 
	  ENDIF
C On line 14, write the actual value of the center: 
	  IF(I.EQ.14)THEN
	    BUFFER=' '
	    WRITE(BUFFER,11) X0,Y0 
11          FORMAT(G10.3,G10.3)
	  ENDIF
C On line 18, write the actual value of the radial boundaries: 
	  IF(I.EQ.18)THEN
	    BUFFER=' '
	    WRITE(BUFFER,11) RADMIN1,RADMAX1
	  ENDIF
C On line 20, write the actual values of AMIN, AMAX which have been
C changed if option MULT_SECT
	  IF(I.EQ.20)THEN
	    BUFFER=' '
	    WRITE(BUFFER,11) AMIN,AMAX
	  ENDIF
	WRITE(2,303,ERR=998)BUFFER
303	  FORMAT(A)
	END DO
	CLOSE(10)
 
C Writes the profile values:
	WRITE(2,*,ERR=998)NPTS
	DO I=1,NBINS
C JLP94: old version  IF(RADFIRST(I).NE.0..AND.NBFIRST(I).NE.0)THEN
C Problem at origin, for radius=0.
C So I replace it by:
	  IF(NBFIRST(I).NE.0)THEN
	    WRITE(2,34,ERR=998)RADFIRST(I),MEAN(I),FLOAT(NBFIRST(I))
34	    FORMAT(2(G14.7,2X),G12.5)
	  ENDIF
	END DO
 
	CLOSE(2)
 
	RETURN
 
C Error messages (Fatal errors since this program is designed to be
C used in Batch):
997	PRINT *,'OUPUT_RESULTS/Fatal error',
     1          ' re-opening the parameter input file'
	STOP
998	PRINT *,'OUPUT_RESULTS/Fatal error',
     1          ' opening in the output profile (should be "new")'
	STOP
 
	END
 
C***************************************************************************
C Subroutine UTILITIES
C To compute coefficients used by all the subsequent routines
C***************************************************************************
	SUBROUTINE UTILITIES
	CHARACTER NAME1*40,COMMENTS1*160
        REAL*4 PI
        DATA PI/3.1415927/
 
	COMMON /PRF1_DATA/NX,NY,NAME1,COMMENTS1
	COMMON /PRF1_PARAM/AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX,
	1	XINCREM,FACT,SCALE,SKY,NBINS,SIGREJEC,BADVALUE,ITERMAX,RVMAG
	COMMON /PRF1_UTILITIES/AXISMIN,AXISMAX,BB1,DD1,UU,VV,WW,
	1	IXMIN,IXMAX,IYMIN,IYMAX
 
C Determination of the coefficients of the ellipses
C
	CO=COS(PHI*PI/180.)
	SI=SIN(PHI*PI/180.)
	UU=CO*CO+(SI/AXRATIO)**2
	VV=SI*SI+(CO/AXRATIO)**2
	WW=2.*SI*CO*(1.-1./(AXRATIO*AXRATIO))
 
C AXISMAX, AXISMIN : Upper and lower limit for the semi major axis (in arcsec)
	AXISMAX=RADMAX1/SQRT(AXRATIO)
	AXISMIN=RADMIN1/SQRT(AXRATIO)
 
C Definition of the actual limits in the working area
	IRMAX=INT(AXISMAX/SCALE)
 
	IXMIN=INT(X0)-IRMAX-1
	IXMAX=INT(X0)+IRMAX+1
	IXMIN=MAX(1,IXMIN)
	IXMAX=MIN(NX,IXMAX)
 
	IYMIN=INT(Y0)-IRMAX-1
	IYMAX=INT(Y0)+IRMAX+1
	IYMIN=MAX(1,IYMIN)
	IYMAX=MIN(NY,IYMAX)
 
C Computes the coefficients for finding the bin a given pixel belongs to :
C Coefficients for finding the index K :
	EPSILON=FACT-1.
	IF(EPSILON.EQ.0.)THEN
	 EPSILON=1.E-06
	 FACT=1.+EPSILON
	ENDIF
	BB1=(1./EPSILON)-0.5
	DD1=2./(EPSILON*XINCREM)
 
C Computes the number of bins for the profile :
	NBINS=1+INT(SQRT(BB1*BB1+DD1*(AXISMAX-AXISMIN))-BB1)
 
C Print the limits of the working area:
	WRITE(3,801)IXMIN,IXMAX,IYMIN,IYMAX
801	FORMAT(' IXMIN,IXMAX,IYMIN,IYMAX:',5I6)
 
	RETURN
	END
