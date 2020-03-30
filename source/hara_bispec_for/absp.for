C++**********************************************************
C Program ABSORPTION
C To compute atmospheric absorption
C and position of Risley prisms
C
C Corrected from a bug with negative coordinates (01-08-93)
C Version 07-08-93
C JLP
C--**********************************************************
	PROGRAM ABSORPTION
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 XLONG,XLAT,EQUIN
	REAL*8 AA,TIME,DYEARS,HOUR_ANGLE,ELEV,AZIM
	REAL*8 PI,DEGTORAD,ALPHA,DELTA,HOUR_ANGLER,SIN_BETA,BETA
        REAL*8 ATMPP,HYGRO,TEMPER,PW_SAT,FF
        REAL*8 LAMBDAC,DLAMBDA,CROSS_ANGLE,RESID_DISP
	INTEGER*4 IDD,MM
	CHARACTER ANS*1
	DATA PI/3.14159265/
        DEGTORAD=PI/180.00
 
10	FORMAT (A)
 
	OPEN(9,FILE='absorption.log',STATUS='unknown')
	WRITE(6,76)
76      FORMAT(' Program Absorption  - Version of 07-08-93 -')
 
C We also set the mean atmospheric conditions:
C 550 mm Hg 50% of hygrometry 0.C (Temp)
        ATMPP=561.0
        HYGRO=60.
        TEMPER=6.0

C Saturation water pressure:
        CALL GET_PW_SAT(PW_SAT,TEMPER)
        FF = HYGRO * PW_SAT / 100.
 
C Input of latitude and longitude :
C Here set it to 1 (TBL application)
	CALL INPUT_LOCATION(XLAT,XLONG,1)

C***** Write on a file the coordinates of the observatory:
	WRITE(6,3001) XLAT,XLONG
	WRITE(9,3001) XLAT,XLONG
3001	FORMAT(8x,'Latitude:',F12.6,
     1	4X,' Longitude:',F12.6)
 
C***** Wavelength and bandwidth: 
	WRITE(6,78)
78      FORMAT(' Central wavelength and bandwidth (nm) ?')
	READ(5,*) LAMBDAC,DLAMBDA 

	WRITE(6,3011) LAMBDAC,DLAMBDA
	WRITE(9,3011) LAMBDAC,DLAMBDA 
3011	FORMAT(8x,'Wavelength:',F12.6,
     1	4X,' Bandwidth:',F12.6)
 
C******************************************************************
C Entering the date of observation:
	PRINT *,' Day of observation: (DD,MM,YYYY) (Ex: 10 09 1993)'
	READ(5,*) IDD,MM,AA
 
C******************************************************************
C Entry point:
C******************************************************************
C Entering ALPHA (in hours)
22	CALL INPUT_COORD(
     1	' Right ascension of the object (HH,MM,SS)?',
     1	ALPHA,'H')
C	ALPHA=DMOD(ALPHA,24.D0)
 
C Entering DELTA (in degrees)
	CALL INPUT_COORD(' Declination ? (DD,MM,SS)',DELTA,'D')
 
C Entering equinox
	PRINT *,' Equinox ? (AAAA.AA) (enter 0 if no correction)'
	READ(5,*) EQUIN

C******************************************************************
C Entry point:
C******************************************************************
11	PRINT *,' Observation time (U.T.) ? (HH,MM,SS)'
	READ(5,*) IH1,IH2,H3
	TIME=FLOAT(IH1)+FLOAT(IH2)/60.+H3/3600.
 
	WRITE(9,3002)ALPHA,DELTA,EQUIN,IDD,MM,INT(AA),IH1,
     1	IH2,IH3
3002	FORMAT(' ALPHA :',F12.6,/,' DELTA :',F12.6,/,
     1	' EQUINOX :',F9.2,/,' DAY : ',I2,'-',I2,
     1	'-',I4,/,' TIME : ',I2,':',I2,':',I2)
 
C Correction for the precession
        IF(EQUIN.NE.0)THEN
	  DYEARS=AA-EQUIN
	  CALL PRECESS(ALPHA,DELTA,DYEARS)
        ENDIF
 
C Get the local coordinates of the star, for the given time and location:
	CALL LOCAL_COORD(AA,MM,IDD,TIME,XLAT,XLONG,ALPHA,DELTA,
     1	HOUR_ANGLE,ELEV,AZIM)
 
C Output of ELEV and AZIM:
	CALL OUTPUT_COORD(ELEV,' Elevation :','D')
	CALL OUTPUT_COORD(AZIM,' Azimuth :','D')
 
C Get the zenith distance:
	ZEN_DIST=90.-ELEV
	CALL OUTPUT_COORD(ZEN_DIST,' Zenith distance :','D')
 
C Get the air mass (sec z) :
	RHO=1.D0/COS(ZEN_DIST*DEGTORAD)
	PRINT 806,RHO
	WRITE(9,806)RHO
806	FORMAT(' Air mass : ',T30,F9.3,/)
 
C Get Beta angle, i.e., angle between the sides linking the star to the pole
C and the star to the zenith (usefull for correction of atmospheric dispersion) 
C (HOUR_ANGLER is HOUR_ANGLE converted to radians)
	HOUR_ANGLER=HOUR_ANGLE*PI/12.D0
	SIN_BETA=COS(XLAT*DEGTORAD)*SIN(HOUR_ANGLER)/SIN(ZEN_DIST*DEGTORAD)
	BETA=ASIN(SIN_BETA)/DEGTORAD
	PRINT 808,BETA,ZEN_DIST
	WRITE(9,808)BETA,ZEN_DIST
808	FORMAT(' Beta angle (for atm. disp.)',T30,F9.3,' (deg)',
     1    ' (North=0, East=90)',/,
     1    ' Zenith distance :',T30,F9.3,' (deg)')

        ITALK=1
        CALL GET_CROSSANGLE(LAMBDAC,DLAMBDA,CROSS_ANGLE,RESID_DISP,
     1        BETA,TEMPER,FF,ATMPP,ZEN_DIST,ICODE1,ICODE2,ITALK)
 
C********************************************************
C Prompts for another go :
	PRINT *,' Do you want to change the time only? (Y)'
	READ(5,10) ANS
	IF (ANS.NE.'N'.AND.ANS.NE.'n') GO TO 11
	PRINT *,' Do you want to change the coordinates and time? (Y)'
	READ(5,10) ANS
	IF (ANS.NE.'N'.AND.ANS.NE.'n') GO TO 22
	PRINT *,' Do you want to change the wavelength and time? (Y)'
	READ(5,10) ANS
	IF (ANS.NE.'N'.AND.ANS.NE.'n') THEN
C***** Wavelength and bandwidth: 
	  WRITE(6,3011) LAMBDAC,DLAMBDA
	  WRITE(9,3011) LAMBDAC,DLAMBDA 
	  READ(5,*) LAMBDAC,DLAMBDA 
          GO TO 11
        ENDIF
 
C End
	PRINT *,' Output in "absorption.log"'
	CLOSE(9)
	STOP
	END
C************************************************************
C	SUBROUTINE PRECESS
C From "Numerical Ephemerides" ?
C Input : ALPHA in hour and DELTA in degrees
C DYEARS : Difference of year of observation - year of the catalogue.
C************************************************************
	SUBROUTINE PRECESS(ALPHA,DELTA,DYEARS)
	REAL*8 PI,ALPHA,DELTA,DYEARS
	REAL*8 PALPHA,PDELTA,WORK1,WORK2
	DATA PI/3.14159265/
 
C PALPHA and PDELTA : correction per year
	PALPHA=(3.075D0+1.336D0*SIN(ALPHA*PI/12.D0)
     1	*DTAN(DELTA*PI/180.D0))/3600.D0
	PDELTA=(20.04D0*COS(ALPHA*PI/12.D0))/3600.D0
 
	PALPHA=PALPHA*DYEARS
	PDELTA=PDELTA*DYEARS
	WORK1=PALPHA*60.
	WORK2=PDELTA*60.
	PRINT 700,WORK1,WORK2
	WRITE(9,700)WORK1,WORK2
700	FORMAT(/,' Correction in alpha (min), delta (arcmin): ',/,
     1	T30,2(F12.3,2X),/)
 
	ALPHA=ALPHA+PALPHA
	DELTA=DELTA+PDELTA
 
	WRITE(6,800)
	WRITE(9,800)
800	FORMAT(' Coordinates corrected for precession:')
	CALL OUTPUT_COORD(ALPHA,' Right ascension :','H')
	CALL OUTPUT_COORD(DELTA,' Declination :','D')
 
	RETURN
	END
C*********************************************************************
C Subroutine JULIAN to compute the Julian day of an observation:
C
C The Julian day begins at Greenwich mean noon (at 12 U.T.)
C
C Here also the Gregorian calendar reform is taken into account.
C Thus the day following 1582 October 4 is 1582 October 15.
C
C The B.C. years are counted astronomically. Thus the year
C before the year +1 is the year 0.
C
C Input:
C AA, MM, IDD, TIME : year,month, day, time of the observation
C DJUL : Julian day
C*********************************************************************
	SUBROUTINE JULIAN(AA,MM,IDD,TIME,DJUL)
	IMPLICIT REAL*8(A-H,O-Z)
		
	DAY1=TIME/24.+FLOAT(IDD)
C First the year after the 1st March ...
	IF(MM.GT.2)THEN
	  YEAR1=AA
	  MONTH1=MM
	ELSE
	  YEAR1=AA-1
	  MONTH1=MM+12
	ENDIF
 
C Then check if after the Gregorian reform:
	DATE_OBS=AA+(INT(275*MM/9)-2.*INT((MM+9)/12)+IDD-30.)/365.
	DATE_REFORM=1582.+289./365.
	IF(DATE_OBS.GE.DATE_REFORM)THEN
	  IA1=INT(YEAR1/100.)
	  IB1=2-IA1+INT(FLOAT(IA1)/4.)
	ELSE
	  IB1=0
	ENDIF
 
C Now final formula:
	DJUL=INT(365.25*YEAR1)+INT(30.6001*(MONTH1+1))
     1	+DAY1+1720994.5+IB1
 
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
	SUBROUTINE INPUT_LOCATION(XLAT,XLONG,ILOC)
	REAL*8 XLAT,XLONG
	INTEGER*4 ILOC
 
        IF(ILOC.LE.0.OR.ILOC.GE.6)THEN
	   PRINT 300
300	   FORMAT(' Location :',/,' 1: Pic du Midi',/,
     1	' 2: Mount Stromlo :',/,
     1	' 3: CFHT (Hawaii) :',/,
     1	' 4: La Silla (ESO) :',/,
     1	' 5: O.H.P. :',/,
     1	' 6: Other',/,
     1	' Enter the number of the location:  ',$)
	   READ(5,*) ILOC
        ENDIF
 
	IF(ILOC.EQ.1)THEN
C LW=-00 DEG 08' 4"
	  XLONG=0.00896296
	  XLAT=+42.9366667
	ENDIF
 
	IF(ILOC.EQ.2)THEN
C LW=-149 DEG 00' 30" LAT=-35 DEG 19.2'
	  XLONG=-9.9338889
	  XLAT=-35.320
	ENDIF
 
	IF(ILOC.EQ.3)THEN
C LW=+155 DEG 28' 18" LAT=+19 DEG 49.6'
	  XLONG=+10.364778
	  XLAT=+19.826667
	ENDIF
 
	IF(ILOC.EQ.4)THEN
C LW=+70 DEG 43' 48"  LAT=-29 DEG 15.4'
	  XLONG=+4.7153333
	  XLAT=-29.256667
	ENDIF
 
	IF(ILOC.EQ.5)THEN
C LW=-5 DEG 42' 48"
	  XLONG=-.3808889
	  XLAT=+43.926667
	ENDIF
 
C************* Prompt here the coordinates of the observatory:
	IF(ILOC.EQ.6)THEN
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
C AA, MM, IDD, TIME : year,month, day, time of the observation
C XLAT (degrees), XLONG (hours): coordinates of the observatory
C ALPHA (in hours), DELTA (degrees): coordinates of the star
C
C Other parameters:
C DJUL : Julian day
C
C Output:
C HOUR_ANGLE: hour angle
C ELEV: elevation
C AZIM: azimuth
C**************************************************************
	SUBROUTINE LOCAL_COORD(AA,MM,IDD,TIME,
     1	XLAT,XLONG,ALPHA,DELTA,
     1	HOUR_ANGLE,ELEV,AZIM)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 AA,TIME,HOUR_ANGLE,ELEV,AZIM
	REAL*8 XLAT,XLONG,ALPHA,DELTA
	REAL*8 PI,DEGTORAD,SIN_ELEV,DJUL,HOUR_ANGLER
	INTEGER*4 IDD,MM
	DATA PI/3.14159265/
        DEGTORAD = PI/180.00
 
C Julian day of the observation:
	CALL JULIAN(AA,MM,IDD,TIME,DJUL)
	PRINT 3004,DJUL
	WRITE(9,3004) DJUL
3004	FORMAT(/,' "Julian day" :',T30,F15.3)
 
C Greenwich Sidereal time at 0:00:00 (U.T.)
	TT1=(DJUL-2415020.0)/36525.
	TSG0H=24.*(0.276919398 + 100.0021359*TT1
     1	+ 0.000001075*TT1*TT1)
 
C Conversion of TSG0H in hour, min, sec
	CALL OUTPUT_COORD(TSG0H,' G.S.T. at 0 H (U.T.) :','H')
 
C Calculation of the hour angle
 	HOUR_ANGLE=TSG0H+TIME*1.002737908-ALPHA-XLONG
	HOUR_ANGLE=DMOD(HOUR_ANGLE,24.D0)
	CALL OUTPUT_COORD(HOUR_ANGLE,' Hour angle :','H')
 
C Calculation of the elevation ELEV
C (HOUR_ANGLER is HOUR_ANGLE converted to radians)
	HOUR_ANGLER=HOUR_ANGLE*PI/12.D0
	SIN_ELEV=SIN(DELTA*DEGTORAD)*SIN(XLAT*DEGTORAD)+COS(DELTA*DEGTORAD)*
     1	COS(XLAT*DEGTORAD)*COS(HOUR_ANGLER)
	ELEV=ASIN(SIN_ELEV)/DEGTORAD
 
C Calculation of the azimuth:
	IF(COS(ELEV*DEGTORAD).EQ.0.D0)THEN
	  AZIM=0.
	ELSE
	  SIN_AZIM=SIN(HOUR_ANGLER)*COS(DELTA*DEGTORAD)/COS(ELEV*DEGTORAD)
	  COS_AZIM=(COS(HOUR_ANGLER)*SIN(XLAT*DEGTORAD)*COS(DELTA*DEGTORAD)
     1	-SIN(DELTA*DEGTORAD)*COS(XLAT*DEGTORAD))/COS(ELEV*DEGTORAD)
	  AZIM=ACOS(COS_AZIM)/DEGTORAD
	  IF(SIN_AZIM.LT.0.D0)AZIM=-1.D0*AZIM
	ENDIF
 
	RETURN
	END
C******************************************************************
C Subroutine OUTPUT_COORD
C
C Input:
C VALUE in hour (if OPT=H) or degrees (if OPT=D)
C
C******************************************************************
	SUBROUTINE OUTPUT_COORD(VALUE,STRING,OPT)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 VALUE,H2
	INTEGER*4 IH0,IH1
	CHARACTER OPT*1,STRING*(*)
 
	CALL CONVERT_COORD(VALUE,IH0,IH1,H2,OPT)
 
C Output on a file, and on the screen:
	IF(OPT.EQ.'H'.OR.OPT.EQ.'h')THEN
	  WRITE(6,802)STRING,IH0,IH1,H2
	  WRITE(9,802)STRING,IH0,IH1,H2
802	  FORMAT(A,T30,I4,' H ',I2,' M  ',
     1	F5.2,' S')
	ELSE
	  WRITE(6,803)STRING,IH0,IH1,H2
	  WRITE(9,803)STRING,IH0,IH1,H2
803	  FORMAT(A,T30,I4,' D ',I2,' M  ',
     1	F5.2,' S')
	ENDIF
 
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
C
C
C******************************************************************
	SUBROUTINE INPUT_COORD(STRING,VALUE,OPT)
	REAL*8 AL3,VALUE
	INTEGER*4 IAL1,IAL2
	CHARACTER OPT*1,STRING*(*),BUFFER*80
 
10	FORMAT(A)
 
55	WRITE(6,*)STRING
	READ(5,10,ERR=99) BUFFER
	READ(BUFFER,*,ERR=99)IAL1,IAL2,AL3
 
C Check if negative input:
	IF(BUFFER(1:1).EQ.'-'.OR.BUFFER(2:2).EQ.'-')THEN
	  VALUE=FLOAT(IAL1)-FLOAT(IAL2)/60.-AL3/3600.
	ELSE
	  VALUE=FLOAT(IAL1)+FLOAT(IAL2)/60.+AL3/3600.
	ENDIF
 
	RETURN
 
C Error message:
99	WRITE(6,98)
98	FORMAT(' Error while reading the input, try again:')
	GO TO 55
	END
C**************************************************************
C Subroutine CONVERT_COORD
C Input: COORD in degrees or in hour
C Output: IAL1 degrees IAL2 ' IAL3 " (when O='D')
C Output: IAL1 hour IAL2 mn IAL3 sec (when O='H')
C**************************************************************
	SUBROUTINE CONVERT_COORD(COORD,IAL1,IAL2,AL3,OPT)
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 COORD,AL2,AL3
	INTEGER*4 IAL1,IAL2
	CHARACTER OPT*1
 
C------------------------------------------------------
C Translating the values between 0H and 24H or 0.deg and 360.deg
	IF(OPT.EQ.'H')THEN
 
	COORD=MOD(COORD,24.D0)
	IF(COORD.LT.0.D0) THEN
	COORD=COORD+24.D0
	ENDIF
	ISIGN=1
 
	ELSE
 
	IF(COORD.LT.0.D0)THEN
	ISIGN=-1
C Warning: this can be dangerous (but restored before return):
	COORD=-1.D0*COORD
	ELSE
	ISIGN=1
	ENDIF
 
	ENDIF
C------------------------------------------------------
C Converting the values in mn, sec or ' "
 
	IAL1=INT(COORD)
	AL2=(COORD-FLOAT(IAL1))*60.D0
	IAL2=INT(AL2)
	AL3=(AL2-FLOAT(IAL2))*60.D0
 
C------------------------------------------------------
C When negative ' or negative ", we add 60 :
	IF(OPT.EQ.'D')THEN
	IAL1=IAL1*ISIGN
 
	IF(AL3.LT.0.)THEN
	PRINT *,' AL3:',AL3
	AL3=AL3+60.
	IAL2=IAL2-1
	ENDIF
 
	IF(IAL2.LT.0)THEN
	PRINT *,' IAL2:',IAL2
	IAL2=IAL2+60
	IAL1=IAL1-1
	ENDIF
 
	ENDIF
C------------------------------------------------------
 
C Restore previous data... 
	IF(ISIGN.EQ.-1) COORD=-1.D0*COORD

	RETURN
	END
