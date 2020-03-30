C++**********************************************************
C	PROGRAM UVCOVERAGE
C**********************************************************
C Program to compute the UV coverage with an array of telescopes.
C Possibility of reading a parameter file, instead of interactive
C input: an example is given in "SOURCE:UVCOVERAGE.DOC"
C F77 Version
C
C JLP
C Version: 02-11-90
C--**********************************************************
	PROGRAM UVCOVERAGE
	REAL*8 XLONG,XLAT,LOWEST_ELEV,GST0H,DJULIAN
	REAL*8 YEAR,TIME,LAMBDA
	REAL*8 ALPHA,DELTA,RISING_TIME,SETTING_TIME
	REAL*8 BASE_LENGTH,BASE_AZIM,BASE_DELTA,BASE_HOUR
	INTEGER*4 IDAY,IMON
	INTEGER*4 LU_LOG,LU_DAT,LU_IN,LU_OUT
	CHARACTER ANS*1,PARAM_FILE*40
	COMMON/UVCDATA/XLONG,XLAT,LOWEST_ELEV,ALPHA,DELTA,
     1      DJULIAN,GST0H,RISING_TIME,SETTING_TIME,LAMBDA
	COMMON/UNITS/LU_LOG,LU_DAT,LU_IN,LU_OUT
 
10	FORMAT (A)
 
	CALL JLP_BEGIN
C File with the details and parameters:
	LU_LOG=14
	OPEN(LU_LOG,FILE='uvcoverage.log',STATUS='unknown')
 
C File with the output data to be plotted:
	LU_DAT=15
	OPEN(LU_DAT,FILE='uvcoverage.dat',STATUS='unknown')
 
	WRITE(LU_LOG,202)
	LU_OUT=6
	WRITE(LU_OUT,202)
202	FORMAT(' Program UVCOVERAGE',
     1      ' - Version of 02-11-90 -',/)
 
C Possibility of reading a parameter file, instead of interactive input:
	WRITE(LU_OUT,298)
298	FORMAT(' Name of the parameter file',
     1      '(cf. source/uvcoverage.doc),',/,
     1      ' (Type "Return" for interactive input) ?')
	LU_IN=5
	READ(LU_IN,10) PARAM_FILE
	IF(PARAM_FILE(1:1).NE.' ')THEN
	  LU_IN=1
	  LU_OUT=19
	  OPEN(LU_IN,FILE=PARAM_FILE,ACCESS='SEQUENTIAL',
     1      STATUS='OLD')
	ENDIF
 
C Input of latitude, longitude:
	CALL INPUT_LOCATION(XLAT,XLONG)
 
C Entering the lowest elevation (in degrees)
	CALL INPUT_COORD(
     1      ' Lowest elevation reachable by the telescope (DD,MM,SS)?',
     1      LOWEST_ELEV,'D')
 
C Wavelength (LAMBDA in nm):
296	WRITE(LU_OUT,297)
297	FORMAT(' Wavelength of the observations (nm)?')
	READ(LU_IN,*)LAMBDA
	IF(LAMBDA.LE.0.)GOTO 296
 
C Entering ALPHA (in hours)
	CALL INPUT_COORD(
     1      ' Right ascension of the object (HH,MM,SS)?',
     1      ALPHA,'H')
 
C Entering DELTA (in degrees)
	CALL INPUT_COORD(' Declination ? (DD,MM,SS)',DELTA,'D')
 
C Entering the date of observation
	WRITE(LU_OUT,203)
203	FORMAT(' Day of observation? (DD,MM,YYYY) (Ex: 01 06 1988)')
	READ(LU_IN,*) IDAY,IMON,YEAR
 
C Julian day at 0:00 (U.T.) (leave it inside the loop, to get the good GST0H)
	TIME=0.D0
	CALL JULIAN(YEAR,IMON,IDAY,TIME,DJULIAN,GST0H)
C Computing rising and setting time:
	CALL OBSERVING_TIME(ALPHA,DELTA,DJULIAN,GST0H,
     1      XLAT,XLONG,LOWEST_ELEV,RISING_TIME,SETTING_TIME)
 
C*************************************************************
C Output in the log file:
C*************************************************************
C Parameters of the observatory:
	CALL OUTPUT_COORD(XLAT,' Latitude: ','D')
	CALL OUTPUT_COORD(XLONG,' Longitude: ','H')
	CALL OUTPUT_COORD(LOWEST_ELEV,
     1      ' Lowest elevation reachable:','D')
C Coordinates of the star:
	CALL OUTPUT_COORD(ALPHA,' Right Ascension:','H')
	CALL OUTPUT_COORD(DELTA,' Declination:','D')
C Date, and Julian day:
	WRITE(LU_LOG,3003) IDAY,IMON,INT(YEAR),DJULIAN
3003	FORMAT(/,' Date of observation :',T40,I2,'/',I2,'/',I4,/,
     1      ' "Julian day" :',T35,F15.3)
C Output of GST0H:
	CALL OUTPUT_COORD(GST0H,' G.S.T. at 0 H (U.T.) :','H')
C Output of rising and setting time:
	CALL OUTPUT_COORD(RISING_TIME,' Rising time (U.T.):','H')
	CALL OUTPUT_COORD(SETTING_TIME,' Setting time (U.T.):','H')
 
C******************************************************************
C Loop starting here
C
C******************************************************************
1	CONTINUE
 
C Input of the parameters of the baseline:
	CALL INPUT_BASELINE(BASE_LENGTH,BASE_AZIM,
     1      BASE_DELTA,BASE_HOUR,XLAT)
 
C Output of these parameters
	WRITE(LU_OUT,302) BASE_LENGTH,BASE_AZIM,BASE_DELTA,BASE_HOUR
	WRITE(LU_LOG,302) BASE_LENGTH,BASE_AZIM,BASE_DELTA,BASE_HOUR
302	FORMAT(/,' Length, azimuth, delta and hour angle of',
     1      ' the baseline:',/,4(F12.4,3X))
 
C When interactive:
C	WRITE(LU_OUT,204)
204	FORMAT(' Observation time (U.T.) ? (HH,MM,SS)')
C	READ(LU_IN,*) IH1,IH2,H3
C	TIME=FLOAT(IH1)+FLOAT(IH2)/60.+H3/3600.
 
	CALL UVC_ONEBASE(BASE_LENGTH,BASE_AZIM,
     1      BASE_DELTA,BASE_HOUR)
 
C********************************************************
C Prompt for another baseline :
	WRITE(LU_OUT,205)
205	FORMAT(' Do you want another go with a new baseline? (Y)')
	READ(LU_IN,10) ANS
	IF (ANS.NE.'N'.AND.ANS.NE.'n') GO TO 1
	CLOSE(15)
 
C************************************************************
C Possibility of plotting the uv coverage:
	WRITE(LU_OUT,405)
405	FORMAT(' Do you want to plot the uv-coverage (Y)')
	READ(LU_IN,10) ANS
	IF (ANS.NE.'N'.AND.ANS.NE.'n')THEN
	   CALL PLOT_UVCOVER
	ENDIF
 
C************************************************************
C Possibility of outputing the dirty beam:
	WRITE(LU_OUT,406)
406	FORMAT(' Do you want to output an image of the uv-coverage (Y)',/,
     1      ' (to generate the dirty beam after Fourier transform')
	READ(LU_IN,10) ANS
	IF (ANS.NE.'N'.AND.ANS.NE.'n')THEN
	   CALL GENE_IMAGE
	ENDIF
 
C End
	WRITE(LU_OUT,207)
207	FORMAT(' Output in "uvcoverage.log" and "uvcoverage.dat"')
	CLOSE(LU_LOG)
	IF(PARAM_FILE(1:1).EQ.' ') CLOSE(5)
 	CALL JLP_END	
	STOP
	END
	
C************************************************************
C	SUBROUTINE PRECESS
C From "Numerical Ephemerides" ?
C Input : ALPHA in hour and DELTA in degrees
C DYEARS : (year of observation - year of the catalogue)
C************************************************************
	SUBROUTINE PRECESS(ALPHA,DELTA,DYEARS)
	REAL*8 ALPHA,DELTA,DYEARS
	REAL*8 WORK1,WORK2,PALPHA,PDELTA,PI
	INTEGER*4 LU_LOG,LU_DAT,LU_IN,LU_OUT
	COMMON/UNITS/LU_LOG,LU_DAT,LU_IN,LU_OUT
	DATA PI/3.14159265/
 
C PALPHA and PDELTA : correction per year
	PALPHA=(3.075D0+1.336D0*SIN(ALPHA*PI/12.D0)
     1      *TAN(DELTA*PI/180.D0))/3600.D0
	PDELTA=(20.04D0*COS(ALPHA*PI/12.D0))/3600.D0
 
	PALPHA=PALPHA*DYEARS
	PDELTA=PDELTA*DYEARS
	WORK1=PALPHA*60.
	WORK2=PDELTA*60.
	WRITE(LU_LOG,700)WORK1,WORK2
700	FORMAT(/,' Correction in alpha (min), delta (arcmin): ',/,
     1      T30,2(F12.3,2X),/)
 
	ALPHA=MOD(ALPHA+PALPHA,24.D0)
	IF(ALPHA.LT.0D0)ALPHA=ALPHA+24.D0
	DELTA=DELTA+PDELTA
 
	WRITE(LU_OUT,800)
	WRITE(LU_LOG,800)
800	FORMAT(' Coordinates corrected for precession:')
	CALL OUTPUT_COORD(ALPHA,' Right ascension :','H')
	CALL OUTPUT_COORD(DELTA,' Declination :','D')
 
	RETURN
	END
C**************************************************************
C Subroutine CONVERT_COORD
C
C Input:
C VALUE (in degrees or in hours)
C
C Output:
C NEGATIVE : true when negative value in input
C IAL1 degrees, IAL2 ', IAL3 ", (when O='D')
C IAL1 hourS, IAL2 min, IAL3 sec, (when O='H')
C
C**************************************************************
	SUBROUTINE CONVERT_COORD(VALUE,NEGATIVE,IAL1,IAL2,AL3,OPT)
	REAL*8 COORD,VALUE,AL2,AL3
	INTEGER*4 IAL1,IAL2
	LOGICAL NEGATIVE
	CHARACTER OPT*1
 
C Transfer of the input:
	COORD=VALUE
 
C------------------------------------------------------
C Translating the values between 0H and 24H or 0.deg and 360.deg
	NEGATIVE=.FALSE.
	IF(OPT.EQ.'H')THEN
	  COORD=MOD(COORD,24.D0)
	    IF(COORD.LT.0.D0) THEN
	      COORD=COORD+24.D0
	    ENDIF
	ELSE
	  IF(COORD.LT.0.D0)THEN
	    NEGATIVE=.TRUE.
	    COORD=-1.D0*COORD
	  ENDIF
	ENDIF
C------------------------------------------------------
C Converting the values in min, sec or ' "
 
	IAL1=INT(COORD)
	AL2=(COORD-FLOAT(IAL1))*60.D0
	IAL2=INT(AL2)
	AL3=(AL2-FLOAT(IAL2))*60.D0
 
C------------------------------------------------------
C When negative ' or negative ", we add 60 :
	IF(OPT.EQ.'D')THEN
	    IF(AL3.LT.0.)THEN
	      AL3=AL3+60.
	      IAL2=IAL2-1
	    ENDIF
	    IF(IAL2.LT.0)THEN
	      IAL2=IAL2+60
	      IAL1=IAL1-1
	    ENDIF
	ENDIF
C------------------------------------------------------
 
	RETURN
	END
 
C*********************************************************************
C Subroutine JULIAN to compute the Julian day of an observation:
C
C The Julian day begins at Greenwich mean noon (at 12 U.T.)
C
C The Gregorian calendar reform is taken into account here.
C Thus the day following 1582 October 4 is 1582 October 15.
C
C The B.C. years are counted astronomically. Thus the year
C before the year +1 is the year 0.
C
C Input:
C YEAR, IMON, IDAY, TIME : Year, month, day and time of the observation
C
C Output:
C DJULIAN: Julian day
C GST0H: Greenwich sidereal time at 0:00 (U.T.)
C*********************************************************************
	SUBROUTINE JULIAN(YEAR,IMON,IDAY,TIME,DJULIAN,GST0H)
	REAL*8 YEAR,TIME,DJULIAN,GST0H
	REAL*8 DAY1,YEAR1,DATE_OBS,DATE_REFORM,TT1
	INTEGER*4 IMON,IDAY,MONTH1,IA1,IB1
		
	DAY1=TIME/24.+FLOAT(IDAY)
C First the year after the 1st March ...
	IF(IMON.GT.2)THEN
	  YEAR1=YEAR
	  MONTH1=IMON
	ELSE
	  YEAR1=YEAR-1
	  MONTH1=IMON+12
	ENDIF
 
C Then check if after the Gregorian reform:
	DATE_OBS=YEAR+(INT(275*IMON/9)-2.*INT((IMON+9)/12)+IDAY-30.)/365.
	DATE_REFORM=1582.+289./365.
	IF(DATE_OBS.GE.DATE_REFORM)THEN
	  IA1=INT(YEAR1/100.)
	  IB1=2-IA1+INT(FLOAT(IA1)/4.)
	ELSE
	  IB1=0
	ENDIF
 
C Now final formula:
	DJULIAN=INT(365.25*YEAR1)+INT(30.6001*(MONTH1+1))
     1      +DAY1+1720994.5+IB1
 
C Greenwich Sidereal time at 0:00:00 (U.T.)
	TT1=(DJULIAN-2415020.0)/36525.
	GST0H=6.6460656 + 2400.051262*TT1
     1      + 0.00002581*TT1*TT1
	GST0H=MOD(GST0H,24.D0)
	IF(GST0H.LT.0)GST0H=GST0H+24.D0
 
	RETURN
	END
C****************************************************************
C Subroutine INPUT_LOCATION
C Prompt for the latitude and longitude of the observatory
C
C Output:
C XLAT (degrees) : latitude
C XLONG (hours) : longitude
C****************************************************************
	SUBROUTINE INPUT_LOCATION(XLAT,XLONG)
	REAL*8 XLAT,XLONG
	INTEGER*4 ILOC
	INTEGER*4 LU_LOG,LU_DAT,LU_IN,LU_OUT
	COMMON/UNITS/LU_LOG,LU_DAT,LU_IN,LU_OUT
 
	WRITE(LU_OUT,300)
300	FORMAT(' Location :',/,' 1: Pic du Midi',/,
     1      ' 2: Mount Stromlo :',/,
     1      ' 3: CFHT (Hawaii) :',/,
     1      ' 4: La Silla (ESO) :',/,
     1      ' 5: O.H.P. :',/,
     1      ' 6: Other',/,
     1      ' Enter the number of the location:  ',$)
	READ(LU_IN,*) ILOC
 
	IF(ILOC.EQ.1)THEN
C LW=-00 DEG 08' 4"   LAT=+42 DEG 56.2'
	  XLONG=0.00896296
	  XLAT=+42.9366667
 
	ELSEIF(ILOC.EQ.2)THEN
C LW=-149 DEG 00' 30" LAT=-35 DEG 19.2'
	  XLONG=-9.9338889
	  XLAT=-35.320
 
	ELSEIF(ILOC.EQ.3)THEN
C LW=+155 DEG 28' 18" LAT=+19 DEG 49.6'
	  XLONG=+10.364778
	  XLAT=+19.826667
 
	ELSEIF(ILOC.EQ.4)THEN
C LW=+70 DEG 43' 48"  LAT=-29 DEG 15.4'
	  XLONG=+4.7153333
	  XLAT=-29.256667
 
	ELSEIF(ILOC.EQ.5)THEN
C LW=-5 DEG 42' 48"   LAT=+43 DEG 55.6'
	  XLONG=-.3808889
	  XLAT=+43.926667
 
C************* Prompt here the coordinates of the observatory:
	ELSE
C Latitude (in degrees)
	  CALL INPUT_COORD(' Latitude ? (DD,MM,SS)',XLAT,'D')
C Longitude :
	  CALL INPUT_COORD(' Longitude ? (HH,MM,SS)',XLONG,'H')
	ENDIF
 
	RETURN
	END
 
C******************************************************************
C Subroutine LOCAL_COORD to compute the azimuth and elevation
C of a star, at a given time
C
C Input:
C ALPHA (in hours), DELTA (degrees): coordinates of the star
C GST0H : Greenwich Sidereal Time of the observation, at 0:00 (U.T.)
C TIME : time (U.T.) of the observation
C XLAT (degrees), XLONG (hours): coordinates of the observatory
C
C
C Output:
C HOUR_ANGLE: hour angle
C ELEV: elevation
C AZIM: azimuth
C**************************************************************
	SUBROUTINE LOCAL_COORD(ALPHA,DELTA,GST0H,TIME,XLAT,XLONG,
     1      HOUR_ANGLE,ELEV,AZIM)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 TIME,HOUR_ANGLE,ELEV,AZIM
	REAL*8 XLAT,XLONG,ALPHA,DELTA
	REAL*8 XLAT1,DELTA1,SIN_AZIM,COS_AZIM
	REAL*8 SIN_ELEV,GST0H,HOUR_ANGLED,SIDEREAL_TIME
 
C Calculation of the hour angle
	SIDEREAL_TIME=GST0H + TIME * 1.002737908 - XLONG
 	HOUR_ANGLE=SIDEREAL_TIME - ALPHA
	HOUR_ANGLE=MOD(HOUR_ANGLE,24.D0)
 
C For the southern hemisphere, we change the signs of DELTA and XLAT:
	IF(XLAT.LT.0.D0)THEN
	  XLAT1=-XLAT
	  DELTA1=-DELTA
	ELSE
	  XLAT1=XLAT
	  DELTA1=DELTA
	ENDIF
 
C Calculation of the elevation ELEV
C (HOUR_ANGLED is HOUR_ANGLE converted in degrees)
	HOUR_ANGLED=HOUR_ANGLE*180.D0/12.D0
	SIN_ELEV=SIND(DELTA1)*SIND(XLAT1)+COSD(DELTA1)*
     1      COSD(XLAT1)*COSD(HOUR_ANGLED)
	ELEV=ASIND(SIN_ELEV)
 
C Calculation of the azimuth:
	IF(COSD(ELEV).EQ.0.D0)THEN
	  AZIM=0.
	ELSE
	  SIN_AZIM=SIND(HOUR_ANGLED)*COSD(DELTA1)/COSD(ELEV)
	  COS_AZIM=(COSD(HOUR_ANGLED)*SIND(XLAT1)*COSD(DELTA1)
     1      -SIND(DELTA1)*COSD(XLAT1))/COSD(ELEV)
	  CALL INVERSE_ANGLE(COS_AZIM,AZIM)
	  IF(SIN_AZIM.LT.0.D0)AZIM=-1.D0*AZIM
	ENDIF
 
	RETURN
	END
C******************************************************************
C Subroutine OUTPUT_COORD
C to write on the log file the given coordinates
C
C Input:
C VALUE in hour (if OPT=H) or degrees (if OPT=D)
C
C******************************************************************
	SUBROUTINE OUTPUT_COORD(VALUE,STRING,OPT)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 VALUE,H2
	INTEGER*4 IH0,IH1
	LOGICAL NEGATIVE
	CHARACTER OPT*1,STRING*(*),CSIGN*1,COPT*3
	INTEGER*4 LU_LOG,LU_DAT,LU_IN,LU_OUT
	COMMON/UNITS/LU_LOG,LU_DAT,LU_IN,LU_OUT
 
C Conversion in degrees, arcmin, arcsec, or hours, min, sec.
	CALL CONVERT_COORD(VALUE,NEGATIVE,IH0,IH1,H2,OPT)
 
C Definition of the parameters for the output:
C Units:
	IF(OPT.EQ.'H'.OR.OPT.EQ.'h')THEN
	  COPT=' H '
	ELSE
	  COPT=' D '
	ENDIF
 
C Sign:
	IF(NEGATIVE)THEN
	  CSIGN='-'
	ELSE
	  CSIGN=' '
	ENDIF
 
C Output on the screen:
c	  WRITE(LU_OUT,802)STRING,CSIGN,IH0,COPT,IH1,H2
C Output on a file:
	  WRITE(LU_LOG,802)STRING,CSIGN,IH0,COPT,IH1,H2
802	  FORMAT(A,T30,A1,I4,A3,I2,' m ',
     1      F5.2,' s')
 
	RETURN
	END
 
C******************************************************************
C Subroutine INPUT_COORD
C
C Input:
C STRING
C
C Output:
C VALUE in hours (if OPT=H)
C       or in degrees (if OPT=D)
C
C******************************************************************
	SUBROUTINE INPUT_COORD(STRING,VALUE,OPT)
	REAL*8 AL3,VALUE
	INTEGER*4 IAL1,IAL2
	CHARACTER OPT*1,STRING*(*),BUFFER*80
	INTEGER*4 LU_LOG,LU_DAT,LU_IN,LU_OUT
	COMMON/UNITS/LU_LOG,LU_DAT,LU_IN,LU_OUT
 
10	FORMAT(A)
 
55	WRITE(LU_OUT,*)STRING
	READ(LU_IN,10,ERR=99) BUFFER
	READ(BUFFER,*,ERR=99)IAL1,IAL2,AL3
 
C Check if negative input:
	IF(BUFFER(1:1).EQ.'-'.OR.BUFFER(2:2).EQ.'-')THEN
	  VALUE=FLOAT(IAL1)-FLOAT(IAL2)/60.-AL3/3600.
	ELSE
	  VALUE=FLOAT(IAL1)+FLOAT(IAL2)/60.+AL3/3600.
	ENDIF
 
	RETURN
 
C Error message:
99	WRITE(LU_OUT,98)
98	FORMAT(' Error while reading the input, try again:')
	GO TO 55
	END
C******************************************************************
C Subroutine BASELINE
C to compute the length of the projected baseline of the telescopes,
C and the projected angle of this baseline on the plane of the
C sky relative to the North.
C
C Input:
C BASE_LENGTH (meters) : Length of the baseline as measured on the ground
C BASE_AZIM (degrees) : azimuth of the baseline
C BASE_HOUR (hours) : "hour angle" of the baseline
C BASE_DELTA : "declination" of the baseline
C ELEV (degrees), and AZIM (degrees) : elevation and azimuth of the star
C ALPHA,DELTA (degrees): right ascension and declination of the star
C
C Output:
C PROJ_LENGTH (meters)
C PROJ_ANGLE (degrees) : orientation positive for North-East-South
C			 (usual in astronomy)
C******************************************************************
	SUBROUTINE BASELINE(BASE_LENGTH,BASE_AZIM,BASE_HOUR,
     1      BASE_DELTA,ELEV,AZIM,
     1      HOUR_ANGLE,DELTA,XLAT,PROJ_LENGTH,PROJ_ANGLE)
	REAL*8 HOUR_ANGLE,ELEV,AZIM
	REAL*8 DELTA,XLAT,PROJ_LENGTH,PROJ_ANGLE
	REAL*8 BASE_LENGTH,BASE_AZIM,BASE_DELTA,BASE_HOUR
	REAL*8 COS_CCC,SIN_CCC,DHOUR,SIN_ANGLE,COS_ANGLE
 
C Compute the apparent length:
	COS_CCC=COSD(AZIM-BASE_AZIM)*COSD(ELEV)
	SIN_CCC=SQRT(1.D0-COS_CCC**2)
	PROJ_LENGTH=BASE_LENGTH*SIN_CCC
 
C Compute the angle of the projection of the baseline on the plane
C of the sky, along the direction of sight:
 
	IF(SIN_CCC.LT.1.E-10)THEN
 
C Solving the easy case when the sinus is null:
	  IF(XLAT.GE.0.D0)THEN
	    PROJ_ANGLE=180.D0
	  ELSE
	    PROJ_ANGLE=0.D0
	  ENDIF
 
C Then the general case:
	ELSE
	  DHOUR = (BASE_HOUR - HOUR_ANGLE)*15.
	  SIN_ANGLE = COSD(BASE_DELTA)*SIND(DHOUR)/SIN_CCC
	  COS_ANGLE = (SIND(BASE_DELTA)-COS_CCC*SIND(DELTA))/
     1      (SIN_CCC*COSD(DELTA))
	  CALL INVERSE_ANGLE(COS_ANGLE,PROJ_ANGLE)
	  IF(SIN_ANGLE.LT.0)PROJ_ANGLE=-1.D0*PROJ_ANGLE
	ENDIF
 
	RETURN
	END
C***********************************************************************
C Subroutine OBSERVING_TIME
C To compute the rising and setting time of a star
C
C Input:
C ALPHA (in hours), DELTA (degrees): coordinates of the star
C DJULIAN : Julian day at 0:00 (U.T.)
C GST0H : Greenwich Sidereal Time at 0:00 (U.T.)
C XLAT (degrees), XLONG (hours): coordinates of the observatory
C
C Output:
C RISING_TIME, SETTING_TIME : in hours, U.T. risng and setting time
C***********************************************************************
	SUBROUTINE OBSERVING_TIME(ALPHA,DELTA,DJULIAN,GST0H,
     1      XLAT,XLONG,LOWEST_ELEV,RISING_TIME,SETTING_TIME)
	REAL*8 XLAT,XLONG,ALPHA,DELTA,DJULIAN,GST0H,LOWEST_ELEV
	REAL*8 RISING_TIME,SETTING_TIME
	REAL*8 COS_HOUR,HOUR
	INTEGER*4 LU_LOG,LU_DAT,LU_IN,LU_OUT
	COMMON/UNITS/LU_LOG,LU_DAT,LU_IN,LU_OUT
 
C Computing the hour angle corresponding to the time of rise or set
C of a star:
	COS_HOUR= (SIND(LOWEST_ELEV) - SIND(XLAT)*SIND(DELTA))/
     1      (COSD(XLAT) * COSD(DELTA))
 
C Check if the star can be on the horizon:
	IF(COS_HOUR.LE.-1.D0)THEN
	  RISING_TIME=0.00
	  SETTING_TIME=24.-1.D-8
	  WRITE(LU_OUT,303)
	  WRITE(LU_LOG,303)
303	  FORMAT(/,' This star is always above the horizon')
	  RETURN
	ENDIF
 
	IF(COS_HOUR.GE.1.D0)THEN
	  RISING_TIME=0.00
	  SETTING_TIME=0.00
	  WRITE(LU_OUT,304)
	  WRITE(LU_LOG,304)
304	  FORMAT(/,' This star is always under the horizon')
	  RETURN
	ENDIF
 
C Computing the hour angle:
	CALL INVERSE_ANGLE(COS_HOUR,HOUR)
	HOUR=HOUR*24.D0/360.D0
	HOUR=MOD(HOUR,24.D0)
	IF(HOUR.LT.0.D0)HOUR=-1.D0*HOUR
	IF(HOUR.GT.12.D0)HOUR=24.D0-HOUR
 
C Greenwich sidereal time:
	RISING_TIME=MOD(ALPHA-HOUR+XLONG-GST0H,24.D0)
	IF(RISING_TIME.LT.0.D0)RISING_TIME=RISING_TIME+24.D0
	SETTING_TIME=MOD(ALPHA+HOUR+XLONG-GST0H,24.D0)
	IF(SETTING_TIME.LT.0.D0)SETTING_TIME=SETTING_TIME+24.D0
 
C Conversion to U.T. :
	RISING_TIME = RISING_TIME / 1.002737908D0
	SETTING_TIME = SETTING_TIME / 1.002737908D0
 
C Now we put it in good shape:
	RISING_TIME=MOD(RISING_TIME,24.D0)
	IF(RISING_TIME.LT.0.D0)RISING_TIME=RISING_TIME+24.D0
	SETTING_TIME=MOD(SETTING_TIME,24.D0)
	IF(SETTING_TIME.LT.0.D0)SETTING_TIME=SETTING_TIME+24.D0
 
	RETURN
	END
C***********************************************************
C Subroutine INVERSE_ANGLE
C To solve the problems when the cosinus is slightly larger than 1.:
C Tolerance=1 + 1.E-12
C
C***********************************************************
	SUBROUTINE INVERSE_ANGLE(COS_ANGLE,ANGLE)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 COS_ANGLE,ANGLE
	REAL*8 TOLERANCE,TOLERANCE2
	DATA TOLERANCE/1.000000000001/
	DATA TOLERANCE2/1.5/
	INTEGER*4 LU_LOG,LU_DAT,LU_IN,LU_OUT
	COMMON/UNITS/LU_LOG,LU_DAT,LU_IN,LU_OUT
 
C Fatal error:
	IF(COS_ANGLE.GT.TOLERANCE.OR.COS_ANGLE.LT.-TOLERANCE)THEN
	  WRITE(LU_LOG,200)
	  WRITE(LU_OUT,200)
200	  FORMAT(' Fatal error in INVERSE_ANGLE: |cosinus| > 1 !')
	  WRITE(LU_OUT,206) ABS(COS_ANGLE)-1.D0
	  WRITE(LU_LOG,206) ABS(COS_ANGLE)-1.D0
206	  FORMAT(' cos - 1.d0 =',E12.4,/)
	  ANGLE=ACOSD(COS_ANGLE)
	  STOP
	ENDIF
 
C Warnings:
	IF(COS_ANGLE.GT.1.D0.AND.COS_ANGLE.LT.TOLERANCE2)THEN
	  WRITE(LU_LOG,202) COS_ANGLE-1.D0
202	  FORMAT(' Warning: cosinus larger than 1:',/,
     1      ' cos - 1.d0 =',E12.4,/)
	  COS_ANGLE=1.D0
	ENDIF
 
	IF(COS_ANGLE.LT.-1.D0.AND.COS_ANGLE.GT.-TOLERANCE2)THEN
	  WRITE(LU_LOG,203) COS_ANGLE+1.D0
203	  FORMAT(' Warning: cosinus slightly smaller than -1:',/,
     1      ' cos + 1.d0 =',E12.4,/)
	  COS_ANGLE=-1.D0
	ENDIF
	
C Computation:
	ANGLE=ACOSD(COS_ANGLE)
	
	RETURN
	END
C*********************************************************************
C Subroutine INPUT_BASELINE
C to input the parameters of the baseline:
C
C Input:
C XLAT: latitude
C
C Output:
C BASE_LENGTH: Length of the baseline in meters
C BASE_AZIM (degrees): Azimuth of the baseline
C BASE_DELTA (degrees): "Declination" of the baseline
C BASE_HOUR (hours): "Hour angle" of the baseline
C*********************************************************************
	SUBROUTINE INPUT_BASELINE(BASE_LENGTH,BASE_AZIM,
     1      BASE_DELTA,BASE_HOUR,XLAT)
	REAL*8 BASE_LENGTH,BASE_AZIM,BASE_DELTA,BASE_HOUR,XLAT
	REAL*8 SIN_DELTA,COS_HOUR,SIN_HOUR,XLAT1
	INTEGER*4 LU_LOG,LU_DAT,LU_IN,LU_OUT
	COMMON/UNITS/LU_LOG,LU_DAT,LU_IN,LU_OUT
 
	WRITE(LU_OUT,301)
301	FORMAT(' Baseline: length (meters) and azimuth (degrees)')
	READ(LU_IN,*)BASE_LENGTH,BASE_AZIM
	
C Computing the parameters of the baseline:
 
C For the southern hemisphere, we change the sign of XLAT,
C and use BASE_DELTA with the "wrong" sign for the resolution of the spherical
C triangle:
	XLAT1=ABS(XLAT)
 
C Declination in degrees:
	SIN_DELTA=-1.D0*COSD(BASE_AZIM)*COSD(XLAT1)
	BASE_DELTA=ASIND(SIN_DELTA)
 
C and Hour angle in hours:
	COS_HOUR=-1.D0*TAND(XLAT1)*TAND(BASE_DELTA)
	SIN_HOUR=SIND(BASE_AZIM)/COSD(BASE_DELTA)
	CALL INVERSE_ANGLE(COS_HOUR,BASE_HOUR)
	IF(SIN_HOUR.LT.0)BASE_HOUR=-1.D0*BASE_HOUR
	BASE_HOUR=BASE_HOUR*24./360.
 
C For the southern hemisphere, we get the good sign for BASE_DELTA:
	IF(XLAT.LT.0.D0)BASE_DELTA=-1.*BASE_DELTA
 
	RETURN
	END
C****************************************************************
C Subroutine PLOT_UVCOVER
C
C****************************************************************
	SUBROUTINE PLOT_UVCOVER
	PARAMETER(NPOINTS=200,KCUR=60)
	REAL*4 XPLOT(NPOINTS,KCUR),YPLOT(NPOINTS,KCUR)
	INTEGER*4 NPTS(KCUR),K,NCUR
	CHARACTER NCHAR(KCUR)*4,PCOLOR(KCUR)*30
	CHARACTER TITLE*40,XLABEL*30,YLABEL*30,PLOTDEV*32
	INTEGER*4 LU_LOG,LU_DAT,LU_IN,LU_OUT
	COMMON/UNITS/LU_LOG,LU_DAT,LU_IN,LU_OUT
 
C Open the file with the data to be plotted:
	OPEN(LU_DAT,FILE='uvcoverage.dat',STATUS='OLD')
 
C Read the data	(Put an open circle to indicate the rising point)
	WRITE(LU_OUT,292)
292	FORMAT(' The first point of each curve (rising point)',
     1      ' will be circled')
	K=1
94	IF(K.GE.KCUR-3) GOTO 95
	  READ(LU_DAT,*,END=99) NPTS(K)
	  NPTS(K+1)=NPTS(K)
	  NCHAR(K)='L'
          PCOLOR(K)='Default'
	  NCHAR(K+1)='L'
          PCOLOR(K+1)='Default'
	  DO I=1,NPTS(K)
	    READ(LU_DAT,*) XPLOT(I,K),YPLOT(I,K)
C Adding the symmetric curve (since if (A,B) exists, (B,A) also exists)
	    XPLOT(I,K+1)=-1.*XPLOT(I,K)
	    YPLOT(I,K+1)=-1.*YPLOT(I,K)
	  END DO
	  NPTS(K+2)=1
	    NCHAR(K+2)='82'
            PCOLOR(K+2)='Default'
	    XPLOT(1,K+2)=XPLOT(1,K)
	    YPLOT(1,K+2)=YPLOT(1,K)
	  NPTS(K+3)=1
	    NCHAR(K+3)='82'
            PCOLOR(K+3)='Default'
	    XPLOT(1,K+3)=-1.*XPLOT(1,K)
	    YPLOT(1,K+3)=-1.*YPLOT(1,K)
	  K=K+4
	GOTO 94
 
C Error message if KCUR exceeded:
95	WRITE(LU_OUT,293) KCUR/4
293	FORMAT(' Warning: maximum number of curves exceeded (MAX=',
     1      I3,')')
 
C End of reading:
99	CLOSE(15)
	NCUR=K-1
	WRITE(LU_OUT,603) NCUR
603	FORMAT(I4,' CURVES ENTERED')
	
C Return if no curves:
	IF(NCUR.LE.0)THEN
	  WRITE(LU_OUT,*) ' No curves: return without plotting'
	  RETURN
	ENDIF
 
C Now plotting the data:
	WRITE(LU_OUT,606)
606	FORMAT(' Output graphic device: ',$)
	READ(LU_IN,10) PLOTDEV
10	FORMAT(A)
	XLABEL='  Megawavelength'
	YLABEL='  Megawavelength'
	WRITE(LU_OUT,607)
607	FORMAT(' Title: ',$)
	READ(LU_IN,10) TITLE
 
C Generate the curves:
	CALL NEWPLOT(XPLOT,YPLOT,NPTS,NPOINTS,NCUR,
     1      XLABEL,YLABEL,TITLE,NCHAR,PCOLOR,PLOTDEV,' ',' ')
 
	RETURN
	END
C****************************************************************
C Subroutine UVC_ONEBASE
C To compute the uv frequency coverage corresponding to one baseline
C
C Output in unit 15 (in BASELINE), and unit LU_LOG (logfile)
C****************************************************************
	SUBROUTINE UVC_ONEBASE(BASE_LENGTH,BASE_AZIM,
     1      BASE_DELTA,BASE_HOUR)
	REAL*8 XLONG,XLAT,LOWEST_ELEV,GST0H,DJULIAN
	REAL*8 TIME,HOUR_ANGLE,ELEV,AZIM,LAMBDA
	REAL*8 ALPHA,DELTA,RISING_TIME,SETTING_TIME
	REAL*8 PROJ_LENGTH,PROJ_ANGLE
	REAL*8 WORK0,WORK1,WORK2,TIME_STEP
	REAL*8 BASE_LENGTH,BASE_AZIM,BASE_DELTA,BASE_HOUR
	INTEGER*4 ITIME,NSTEP
	INTEGER*4 LU_LOG,LU_DAT,LU_IN,LU_OUT
	COMMON/UNITS/LU_LOG,LU_DAT,LU_IN,LU_OUT
	COMMON/UVCDATA/XLONG,XLAT,LOWEST_ELEV,ALPHA,DELTA,
     1      DJULIAN,GST0H,RISING_TIME,SETTING_TIME,LAMBDA
 
10	FORMAT (A)
12	FORMAT(' ')
 
 
C******************************************************************
C Loop from the rising time to the setting time:
 
C First check if it is possible:
	IF(SETTING_TIME.EQ.RISING_TIME)THEN
	   WRITE(LU_OUT,206)
	   WRITE(LU_LOG,206)
206	   FORMAT(' Exit: the star is not visible at this latitude')
	   STOP
	ENDIF
 
C Then compute the parameters of the loop:
	NSTEP=101
	IF(SETTING_TIME.LT.RISING_TIME)THEN
	   SETTING_TIME=SETTING_TIME+24.D0/1.002737908
	ENDIF
	TIME_STEP=(SETTING_TIME-RISING_TIME)/FLOAT(NSTEP-1)
	TIME=RISING_TIME
 
C Beginning of a new set of data for the output file:
	  WRITE(15,*) NSTEP
 
C Beginning the loop:
	DO 1000 ITIME=1,NSTEP
 
C Get the local coordinates of the star, for the given time and location:
	  CALL LOCAL_COORD(ALPHA,DELTA,GST0H,TIME,XLAT,XLONG,
     1      HOUR_ANGLE,ELEV,AZIM)
 
C Compute the apparent length of the baseline and its orientation
C relative to the north:
	  CALL BASELINE(BASE_LENGTH,BASE_AZIM,BASE_HOUR,
     1      BASE_DELTA,ELEV,AZIM,HOUR_ANGLE,DELTA,XLAT,
     1      PROJ_LENGTH,PROJ_ANGLE)
 
C Output data in the logfile:
C Northward will be along the Y axis, Westward along the X axis:
	  WORK0=PROJ_ANGLE-90.D0
C LAMBDA is in nm, we want to get Megawavelengths:
	  WORK1=1000.*PROJ_LENGTH*COSD(WORK0)/LAMBDA
	  WORK2=1000.*PROJ_LENGTH*SIND(WORK0)/LAMBDA
	  WRITE(LU_DAT,308) WORK1,WORK2
308	  FORMAT(F12.4,2X,F12.4)
 
C Output of data every 10 steps:
	  IF(MOD(ITIME,10).EQ.1)THEN
	    WRITE(LU_LOG,12)
	    CALL OUTPUT_COORD(TIME,' Time (U.T.):','H')
	    CALL OUTPUT_COORD(HOUR_ANGLE,' Hour angle :','H')
	    CALL OUTPUT_COORD(ELEV,' Elevation (degrees):','D')
	    CALL OUTPUT_COORD(AZIM,' Azimuth (degrees):','D')
c	    WRITE(LU_OUT,303) PROJ_LENGTH,PROJ_ANGLE
	    WRITE(LU_LOG,303) PROJ_LENGTH,PROJ_ANGLE
303	    FORMAT(' Projected length (m) and angle (deg) of',
     1      ' the baseline:',/,2(F12.4,3X))
	  ENDIF
 
C Get ready for the next step:
	  TIME=TIME+TIME_STEP
C Check if we change the date of observation:
	  IF(TIME.GT.24.D0)THEN
	    TIME=TIME-24.D0
	    GST0H=GST0H+24.*0.002737908
	    WRITE(LU_LOG,309)
309	    FORMAT(/,' Warning : we are now working on the following day!')
	  ENDIF
 
C End of the loop:
1000	CONTINUE
 
	RETURN
	END
C****************************************************************
C Subroutine GENE_IMAGE
C
C****************************************************************
	SUBROUTINE GENE_IMAGE
	PARAMETER(NX=256,NY=256)
	REAL*4 IMAGE(NX,NY),XX,YY
	REAL*4 XMAX,XMIN,YMAX,YMIN
	CHARACTER COMMENTS*80,NAME_IMA*40
	INTEGER*4 NPTS
	INTEGER*4 LU_LOG,LU_DAT,LU_IN,LU_OUT
	COMMON/UNITS/LU_LOG,LU_DAT,LU_IN,LU_OUT
 
C Open the file with the data to be plotted:
	OPEN(LU_DAT,FILE='uvcoverage.dat',STATUS='OLD')
 
C Read the data	(Put an open circle to indicate the rising point)
291	WRITE(LU_OUT,292)
292	FORMAT(' Enter the limits for the image (in Megawavelength):',
     1      ' XMIN,XMAX,YMIN,YMAX :')
	READ(LU_IN,*) XMIN,XMAX,YMIN,YMAX
	IF((XMAX.LE.XMIN).OR.(YMAX.LE.YMIN))GOTO 291
 
C Cleans the image:
	DO J=1,NY
	  DO I=1,NX
	    IMAGE(I,J)=0.
	  END DO
	END DO
 
C Reads the previous data file, and set to 1 the uv-points:
	DO K=1,10000
	  READ(LU_DAT,*,END=99) NPTS
	  DO I=1,NPTS
	    READ(LU_DAT,*,END=99) XX,YY
	    IX=NINT(NX*(XX-XMIN)/(XMAX-XMIN))
	    IY=NINT(NY*(YY-YMIN)/(YMAX-YMIN))
	    IF((IX.GE.1.AND.IX.LE.NX).AND.
     1      (IY.GE.1.AND.IY.LE.NY)) IMAGE(IX,IY)=1.
C Adding the symmetric point relative to the center
C (since if (A,B) exists, (B,A) also exists)
	    IX=NINT(NX*(0.-XX-XMIN)/(XMAX-XMIN))
	    IY=NINT(NY*(0.-YY-YMIN)/(YMAX-YMIN))
	    IF((IX.GE.1.AND.IX.LE.NX).AND.
     1      (IY.GE.1.AND.IY.LE.NY)) IMAGE(IX,IY)=1.
	  END DO
	END DO
 
C Error message if 1000 curves exceeded:
	WRITE(LU_OUT,293)
293	FORMAT(' Warning: maximum number of curves exceeded',
     1	  ' (MAX= 10000)')
 
C End of reading:
99	CLOSE(15)
 
C Writes the image:
	CALL JLP_INQUIFMT
	NAME_IMA=' '
	COMMENTS=' UVCOVERAGE'
	CALL JLP_WRITEIMAG(IMAGE,NX,NY,NX,NAME_IMA,COMMENTS)
 
	RETURN
	END
