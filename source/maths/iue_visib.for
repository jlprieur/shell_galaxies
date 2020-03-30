C++--------------------------------------------------------------------
C IUE_VISIB
C  Computes the visibility of stars in different observatories,
C  and especially for IUE (Sun, Moon, Earth, power and hot constraints...)
C
C  The ephemeris accuracy is sufficient for the stated purpose.
C  Moon coordinates are only accurate to a few degrees, though.
C
C JSBD    RGO 1982
C C. Boisson: Revised by me for IUE,ESO and non-pre-set sites, 1983, 1988
C
C JLP
C Version: 28-04-89
C-----------------------------------------------------------------------
C  Initial declarations
      IMPLICIT INTEGER*4 (I-M)
      IMPLICIT DOUBLE PRECISION (D)
      CHARACTER*1 SITEANS
      CHARACTER*80 SITENAME
C
C
C  Declare HOUR_HEADER,SITE_PRESET_COORDINATES,OBJECT_ALT
C   and AZ arrays
      DIMENSION ITMHDR(12),PRESET(9,2),IAZ(12),IALT(12)
C   Declare SITE NAME ARRAY
      DIMENSION ISITE(5,10)
C  Declare MONTH NAME ARRAY
      DIMENSION IMNTH(3,12)
C  Declare OBJECT NAME ARRAY
      DIMENSION INAME(100,10)
C  Declare REAL_OBJECT_COORDINATE ARRAY
      DIMENSION ROBJC(100,6)
C  Declare TEMPORARY ARRAYS
      DIMENSION IT1(10),IT2(10)
C
C**********
C  LOAD SITE ARRAY
      DATA PRESET(1,1),PRESET(1,2)/210.93333,-31.277039/
      DATA PRESET(2,1),PRESET(2,2)/341.52342,-33.93403/
      DATA PRESET(3,1),PRESET(3,2)/359.66167,50.871667/
      DATA PRESET(4,1),PRESET(4,2)/155.47167,19.82829/
      DATA PRESET(5,1),PRESET(5,2)/17.8761,28.75944/
      DATA PRESET(6,1),PRESET(6,2)/112.5,37.5/
      DATA PRESET(7,1),PRESET(7,2)/70.7333,-29.25/
C
C
C  INSERT SUN AND MOON NAMES INTO NAME ARRAY
      DATA IT1/' ',' ','S','U','N',' ',' ',' ',' ',' '/
      DATA IT2/' ','M','O','O','N',' ',' ',' ',' ',' '/
         DO 189 IB=1,10
            INAME(1,IB)=IT1(IB)
            INAME(2,IB)=IT2(IB)
189      CONTINUE
C
C        Fill up SITENAME variable with blanks
C
      DO I=1,80
         SITENAME(I:I)=' '
      END DO
C
C   Open output file properly
C
      OPEN (UNIT=3,FILE='iuevis.log',TYPE='unknown')
C
C  SET ALTITUDE LIMIT FOR OBJECTS
      DALTLIM=10.0D0
C
C  ASK USER FOR SITE
      WRITE(6,1)
1     FORMAT(////,10X,' **  STARUP  ** ',//)
C
C      Patch to put in any site
C
      SITEANS='A'
94      IF(SITEANS .EQ. 'M' .OR. SITEANS .EQ. 'P') GOTO 95
         WRITE (6,*) ' '
         WRITE (6,10)
   10    FORMAT (' Menu of sites or personal choice? (M/P) ',$)
         READ (5,'(A)') SITEANS
         JLP_UPCASE(SITEANS,1)
        GOTO 94

95      IF (SITEANS .EQ. 'M') THEN
         WRITE(6,*)'  ENTER SITE CHOICE -'
         WRITE(6,*)'                      AAT           1'
         WRITE(6,*)'                      SAAO          2'
         WRITE(6,*)'                      RGO           3'
         WRITE(6,*)'                      UKIRT/CFHT    4'
         WRITE(6,*)'                      LPO           5'
         WRITE(6,*)'                      UTAH          6'
         WRITE(6,*)'                      ESO           7'
         WRITE(6,*)'                      IUE           8'
         WRITE(6,*)' '
         WRITE(6,*)'                      END           99'
         WRITE(6,*)' '
         WRITE(6,2)
   2     FORMAT(1X,'    SITE ?> ',$)
         READ(5,*)ISCHC
      ELSE IF (SITEANS .EQ. 'P') THEN
         WRITE (6,20)
   20    FORMAT (' Enter site name: ',$)
         READ (5,'(A)') SITENAME
C
C        Find end of site name
C
         ISNEND=80
96       IF(SITENAME(I:I) .NE. ' ')GOTO 97
            ISNEND=ISNEND-1
	 GOTO 96

97         WRITE (6,*) ' '
         WRITE (6,*) 'Enter site coordinates'
         WRITE (6,*) 'LONGITUDE (DD.MMSS)  LATITUDE (DD.MMSS)'
	 WRITE (6,*) '***ATTENTION***  WEST is positive  ***ATTENTION***'
         READ (5,*) RSITELONG, RSITELAT
         CALL DECINP (RSITELONG,IH10,IM10,IS10)
         CALL HMSTOH (IH10,IM10,FLOAT(IS10),DSLONG)
         CALL DECINP (RSITELAT,IH20,IM20,IS20)
         CALL HMSTOH (IH20,IM20,FLOAT(IS20),DSLAT)
C
C        Convert longitude to convention = angles between 0 and 360
C
         IF (DSLONG .LT. 0.0D0) THEN
            DSLONG=DSLONG+360.0D0
         END IF
         PRESET (9,1)=SNGL(DSLONG)
         PRESET (9,2)=SNGL(DSLAT)
         ISCHC=9
      END IF
C
C  SITE CHOICE IS NOW ISCHC - VERIFY
C   IF 99 , END
      IF(ISCHC.GE.99)GO TO 999
      IF(ISCHC.LE.0)GO TO 999
 
C  WRITE SITE TO OUTPUT FILE
      WRITE(3,500)
500   FORMAT(' **  OUTPUT FROM IUE_VISIB  ** ',/)
      IF(ISCHC.EQ.1)WRITE(3,*)'  SITE IS  -  AAT '
      IF(ISCHC.EQ.2)WRITE(3,*)'  SITE IS  -  SAA0'
      IF(ISCHC.EQ.3)WRITE(3,*)'  SITE IS  -  RGO '
      IF(ISCHC.EQ.4)WRITE(3,*)'  SITE IS  -  UKIRT/CFHT'
      IF(ISCHC.EQ.5)WRITE(3,*)'  SITE IS  -  LPO '
      IF(ISCHC.EQ.6)WRITE(3,*)'  SITE IS  -  UTAH'
      IF(ISCHC.EQ.7)WRITE(3,*)'  SITE IS  -  ESO'
      IF(ISCHC.EQ.8)WRITE(3,*)'  SITE IS  -  IUE'
      IF (ISCHC .EQ. 9) WRITE (3,11) SITENAME(1:ISNEND)
 11   FORMAT ('   SITE IS  - ',A)
      IF (ISCHC .EQ. 9) THEN
         WRITE (3,502)
502      FORMAT ('   LONGITUDE',10X,'LATITUDE')
         WRITE (3,503) IH10,ABS(IM10),ABS(IS10),IH20,ABS(IM20),
     :                 ABS(IS20)
503      FORMAT (1X,I4,'d ',I2,'m ',I2,'s ',5X,I3,'d ',I2,'m ',I2,'s ')
      END IF
      WRITE(3,501)
501   FORMAT(///)
C
C  ASK FOR START DATE FOR OBSERVING RUN
      WRITE(6,3)
3     FORMAT(//,' ENTER DATE FOR START OF RUN (DD MM YYYY) > ',$)
      READ(5,*)ID1,IM1,IY1
      WRITE(6,*)' '
C
C  ASK FOR END DATE FOR OBSERVING RUN
      WRITE(6,4)
4     FORMAT(//,' ENTER DATE FOR  END  OF RUN (DD MM YYYY) > ',$)
      READ(5,*)ID2,IM2,IY2
      WRITE(6,*)' '
C
C  ASK FOR REQUIRED INTERVAL IN DAYS BETWEEN OUTPUT
      WRITE(6,5)
5     FORMAT(//,' INTERVAL BETWEEN OUTPUT DATA IN DAYS (DD) > ',$)
      READ(5,*)INTRVL
C
C  ASK FOR LIST OF OBJECTS
      WRITE(6,*)' '
      WRITE(6,*)'  INPUT LIST OF REQUIRED OBJECTS > '
      WRITE(6,*)'     NAME (10 CHARS) <CR> '
      WRITE(6,*)'     RA (HH.MMSS) DEC (DD.MMSS) '
      WRITE(6,*)' '
      WRITE(6,*)'  USE A CONTROL/Z TO END INPUT '
      WRITE(6,*)' '
      DO 110  IOBJ=3,1000
C  1 and 2 are the SUN and the MOON
      WRITE(6,25)(IOBJ-2)
25      FORMAT(1X,' OBJECT ',I4,'  ',$)
      READ(5,33,END=299)(INAME(IOBJ,INM),INM=1,10)
33    FORMAT(10A1)
         READ(5,*,END=299)RAIN,RDECIN
         CALL DECINP(RAIN,IH1O,IM1O,IS1O)
         CALL HMSTOH(IH1O,IM1O,FLOAT(IS1O),DRAH)
         CALL DECINP(RDECIN,IH2O,IM2O,IS2O)
         CALL HMSTOH(IH2O,IM2O,FLOAT(IS2O),DRDEC)
         ROBJC(IOBJ,1)=SNGL(DRAH*15.0)
         ROBJC(IOBJ,2)=FLOAT(IH1O)
         ROBJC(IOBJ,3)=FLOAT(IM1O)
         ROBJC(IOBJ,4)=SNGL(DRDEC)
         ROBJC(IOBJ,5)=FLOAT(IH2O)
         ROBJC(IOBJ,6)=FLOAT(IM2O)
110   CONTINUE
299   CONTINUE
C
C  SET UP NUMBER OF OBJECTS
      INUMOB=IOBJ-1
C
C Calculate julian date at start
      CALL JDCONV(ID1,IM1,IY1,DAY1)
C
C Calculate julian date at end
      CALL JDCONV(ID2,IM2,IY2,DAY2)
C
C  LOOP THROUGH RANGE OF DATES
      IRANGE=DINT(DAY2-DAY1+1)
      DO 100 IDINC=1,IRANGE,INTRVL
C
C Calculate julian date for this day
         DAYNOW=DAY1+DBLE(FLOAT(IDINC-1))
C
C Calculate date from julian day
         CALL CONVJD(DAYNOW,IY9,IM9,ID9)
      WRITE(3,*)' '
         WRITE(3,39)
39       FORMAT(1X,62('#'))
C
C     WRITE OUT DATE
         WRITE(3,12)ID9,IM9,IY9
12       FORMAT(1X,'  DATE -  ',I3,1X,I3,1X,I5)
C
C Calculate sun's position
         CALL SUNPOS(DAYNOW,DASUN,DDSUN)
C
C Calculate moon's position
         CALL MONPOS(DAYNOW,DAMOON,DDMOON)
C
C Write out date, julian day, and day's sun and moon positions
         WRITE(3,13)DAYNOW
13         FORMAT(1X,'  JULIAN DATE - ',F11.2)
C
C Calculate GST at 0H UT
         DGST0=48.0D0+6.64606556D0+0.065709822*(DAYNOW-
     &         2415020.0D0)
         DGST0=DMOD(DGST0,24.0D0)
         GST0=SNGL(DGST0)
C
C Write out
         CALL HTOHMS(DGST0,IGSTH,IGSTM,GSTS)
         WRITE(3,18)IGSTH,IGSTM,GSTS
18       FORMAT(1X,'  GST AT 0H UT    ',2I3,1X,F4.1)
         DSUNRA=DASUN/15.0D0
         DSUNDEC=DDSUN
         ROBJC(1,1)=SNGL(DSUNRA*15.0)
         ROBJC(1,4)=SNGL(DSUNDEC)
         CALL HTOHMS(DSUNRA,IH,IM,S)
         CALL HTOHMS(DSUNDEC,ID,IDM,SD)
         WRITE(3,16)IH,IM,ID,IDM
         ROBJC(1,2)=FLOAT(IH)
         ROBJC(1,3)=FLOAT(IM)
         ROBJC(1,5)=FLOAT(ID)
         ROBJC(1,6)=FLOAT(IDM)
16       FORMAT(1X,' SUN - - RA> ',2I3,' DEC> ',2I3)
         DRMONRA=DAMOON/15.0D0
         DRMONDD=DDMOON
         ROBJC(2,1)=SNGL(DRMONRA*15.0)
         ROBJC(2,4)=SNGL(DRMONDD)
         CALL HTOHMS(DRMONRA,IH,IM,S)
         CALL HTOHMS(DRMONDD,ID,IDM,SD)
         ROBJC(2,2)=FLOAT(IH)
         ROBJC(2,3)=FLOAT(IM)
         ROBJC(2,5)=FLOAT(ID)
         ROBJC(2,6)=FLOAT(IDM)
         WRITE(3,17)IH,IM,ID,IDM
17       FORMAT(1X,' MOON- -   > ',2I3,'    > ',2I3)
C
C
C Patch from C. Boisson to give IUE BETA angles
C
         IF (ISCHC .EQ. 8) THEN
            DASUN=DBLE(ROBJC(1,1))
            DDSUN=DBLE(ROBJC(1,4))
            WRITE (3,*)' '
            DO IOB=3,INUMOB
               DAOBJ=DBLE(ROBJC(IOB,1))
               DDOBJ=DBLE(ROBJC(IOB,4))
               CALL GETBETA (DASUN,DDSUN,DAOBJ,DDOBJ,DBETA)
               BETA=SNGL(DBETA)
               IRAH=INT(ROBJC(IOB,2))
               IRAM=INT(ROBJC(IOB,3))
               IDECD=INT(ROBJC(IOB,5))
               IDECM=INT(ROBJC(IOB,6))
               IRAM=ABS(IRAM)
               IDECM=ABS(IDECM)
               IF (BETA .GT. 135.0) THEN
                  WRITE (3,1031) (INAME(IOB,INM),INM=1,10),IRAH,IRAM,
     :                            IDECD,IDECM,BETA
 1031             FORMAT (2X,10A1,3X,'RA=',2I3,' Dec=',I4,I3,'  Beta=',
     :                    F7.2,'  ***SUN CONSTRAINT***')
               ELSE IF ((BETA .LT. 119.0 .AND. BETA .GT. 111.0) .OR.
     :                  (BETA .LT.  27.0 .AND. BETA. GT.  23.0)) THEN
                  WRITE (3,1032) (INAME(IOB,INM),INM=1,10),IRAH,IRAM,
     :                            IDECD,IDECM,BETA
 1032             FORMAT (2X,10A1,3X,'RA=',2I3,' Dec=',I4,I3,'  Beta=',
     :                    F7.2,'  **POWER CONSTRAINT**')
               ELSE IF (BETA .GT. 55.0 .AND. BETA .LT. 95.0)  THEN
                  WRITE (3,1033) (INAME(IOB,INM),INM=1,10),IRAH,IRAM,
     :                            IDECD,IDECM,BETA
 1033             FORMAT (2X,10A1,3X,'RA=',2I3,' Dec=',I4,I3,'  Beta=',
     :                    F7.2,'  ***HOT CONSTRAINT***')
               ELSE IF (BETA .LT. 13.0) THEN
                  WRITE (3,1034) (INAME(IOB,INM),INM=1,10),IRAH,IRAM,
     :                            IDECD,IDECM,BETA
 1034             FORMAT (2X,10A1,3X,'RA=',2I3,' Dec=',I4,I3,'  Beta=',
     :                    F7.2,'  **ANTISUN CONSTRAINT**')
               ELSE
                  WRITE (3,1035) (INAME(IOB,INM),INM=1,10),IRAH,IRAM,
     :                            IDECD,IDECM,BETA
 1035             FORMAT (2X,10A1,3X,'RA=',2I3,' Dec=',I4,I3,'  Beta=',
     :                    F7.2)
               END IF
            END DO
         GO TO 100
         END IF
C
C        End of IUE patch
C
C
         WRITE(3,*)' '
         WRITE(3,*)'     * * * * * * * * * * * * * * * * * * * * * * '
         WRITE(3,*)'     * *  TABLE OF OBJECT''S  ALTITUDE  ..AND * * '
         WRITE(3,*)'     * *                     AZIMUTH         * * '
         WRITE(3,*)'     * * * * * * * * * * * * * * * * * * * * * * '
         WRITE(3,*)' '
C
C     WRITE HEADER FOR HOURS TABLE - LOCAL MEAN TIME
         WRITE(3,*)' '
         DO 101  I=1,12
            IH=60+2*(I-1)-2*INT(PRESET(ISCHC,1)/15.0)
            IH=MOD(IH,24)
            ITMHDR(I)=IH
101      CONTINUE
         WRITE(3,37)(ITMHDR(I),I=1,12)
37        FORMAT(4X,'  LMT      ',10x,12I4)
C     WRITE HEADER FOR HOURS TABLE
         WRITE(3,*)' '
         DO 1010  I=1,12
            IH=60+2*(I-1)-INT(PRESET(ISCHC,1)/15.0)
            IH=MOD(IH,24)
            ITMHDR(I)=IH
1010      CONTINUE
         WRITE(3,7)(ITMHDR(I),I=1,12)
7        FORMAT(4X,'  UT      ',10x,12I4)
C
C  CALCULATE AZIMUTH AND ALTITUDE FOR EACH OBJECT
      DO 102 II=1,INUMOB
C
C     CLEAR OBJECT'S ALTITUDE AND AZIMUTH ARRAY
         DO 104 IJ=1,12
            IALT(IJ)=0
            IAZ(IJ)=0
104      CONTINUE
C
C     GET RA AND DEC FOR THIS OBJECT FROM THE ARRAY
         RA=ROBJC(II,1)
         RDEC=ROBJC(II,4)
C
C     CALCULATE THE HOUR (UT)
         DO 105 IHR=1,12
            IH=60+2*(IHR-1)-INT(PRESET(ISCHC,1)/15.0)
            IH=MOD(IH,24)
C
C     CALCULATE LOCAL SIDEREAL TIME
            RLST=GST0+1.002738*FLOAT(IH)-PRESET(ISCHC,1)/15.0
            RLST=AMOD((RLST+48.0),24.0)
C
C     CALCULATE ALTITUDE
      DRLST=DBLE(RLST)
      DRA=DBLE(RA)
      DRDEC=DBLE(RDEC)
      DCOORD=DBLE(PRESET(ISCHC,2))
            CALL ALTUDE(DRLST,DRA,DRDEC,DCOORD,DALT)
C
C     CALCULATE AZIMUTH
            CALL AZMUTH(DRLST,DRA,DRDEC,DCOORD,DAZ,DALT)
C
C     IF OBJECT BELOW SPECIFIED ALTITUDE,SKIP TO NEXT HOUR
               IF((DALT.LT.DALTLIM).AND.(II.GE.3))GO TO 199
C
C     WRITE INTO ARRAY FOR OUTPUT
            IAZ(IHR)=NINT(DAZ)
            IALT(IHR)=NINT(DALT)
199      CONTINUE
105      CONTINUE
C
C  WRITE OUTPUT ARRAY
      WRITE(3,*)' '
      WRITE(3,31)(INAME(II,INM),INM=1,6),(IALT(IW),IW=1,12)
      IRAH=INT(ROBJC(II,2))
      IRAM=INT(ROBJC(II,3))
      IDECD=INT(ROBJC(II,5))
      IDECM=INT(ROBJC(II,6))
      WRITE(3,30)IRAH,ABS(IRAM),IDECD,ABS(IDECM),(IAZ(IW),IW=1,12)
31    FORMAT(4X,6A1,14X,12I4)
30    FORMAT(1X,2I3,I4,I3,10X,12I4)
102   CONTINUE
C
C
100   CONTINUE
C
C  STOP
999   CONTINUE
C
      CLOSE(3)
      WRITE(6,*) ' Output in iuevis.log'
      STOP
      END
 
C---------------------------------------------------------------------
C Subroutine HTOHMS
C---------------------------------------------------------------------
      SUBROUTINE HTOHMS(DH,IH,IM,S)
      IMPLICIT REAL*8 (D)
      IH=JIDINT(DH)
      IM=JIABS(JIDINT(DMOD(DH,1D0)*6D1))
      S=ABS(SNGL(DMOD((DH*3.6D3),6D1)))
      RETURN
      END
 
C---------------------------------------------------------------------
C Subroutine HMSTOH
C---------------------------------------------------------------------
      SUBROUTINE HMSTOH(IH,IM,S,D)
      IMPLICIT REAL*8 (D)
      D=DBLE(S)/3.6D3+DFLOAT(IH)+DFLOAT(IM)/6D1
      RETURN
      END
 
C---------------------------------------------------------------------
C Subroutine JDCONV
C Computes Julian day
C---------------------------------------------------------------------
      SUBROUTINE JDCONV(ID,IM,IY,D)
      IMPLICIT REAL*8 (D)
      IF(IM.GT.2)THEN
            DG=DFLOAT(IY)
            DF=DFLOAT(IM)+1D0
         ELSE
            DG=DFLOAT(IY)-1D0
            DF=DFLOAT(IM)+13D0
         END IF
      D=IDINT(365.25D0*DG)+IDINT(30.6D0*DF)+DFLOAT(ID)+1720981.5D0
      ITEST=IY*10000+IM*100+ID
      IF(ITEST.LT.19000228)D=D+1D0
      IF(ITEST.LT.18000228)D=D+2D0
      RETURN
      END
 
C---------------------------------------------------------------------
C Subroutine ALTUDE
C---------------------------------------------------------------------
      SUBROUTINE ALTUDE(DT,DA,DD,DL,DALT)
      IMPLICIT REAL*8 (D)
      DRDCON=180D0/3.1415926535D0
      DHA=(DT*15D0-DA)/DRDCON
      DDR=DD/DRDCON
      DLR=DL/DRDCON
      DSNA=DSIN(DDR)*DSIN(DLR)+DCOS(DDR)*DCOS(DLR)*DCOS(DHA)
      DALT=DASIN(DSNA)*DRDCON
      RETURN
      END
 
C---------------------------------------------------------------------
C Subroutine AZMUTH
C---------------------------------------------------------------------
      SUBROUTINE AZMUTH(DT,DA,DD,DL,DAZ,DALT)
      IMPLICIT REAL*8 (D)
      DRDCON=180D0/3.1415926535D0
      DALTR=DALT/DRDCON
      DLR=DL/DRDCON
      DDR=DD/DRDCON
      DHA=(DT*15D0-DA)/DRDCON
      DCSA=(DSIN(DDR)-DSIN(DLR)*DSIN(DALTR))/(DCOS(DLR)*DCOS(DALTR))
      IF(DSIN(DHA).GE.0D0)THEN
            DAZ=360D0-DACOS(DCSA)*DRDCON
         ELSE
            DAZ=DACOS(DCSA)*DRDCON
         END IF
      RETURN
      END
 
C---------------------------------------------------------------------
C Subroutine SUNPOS
C Calculate sun's position
C---------------------------------------------------------------------
      SUBROUTINE SUNPOS(DT,DA,DDEC)
      IMPLICIT REAL*8 (D)
      DRDCON=180D0/3.1415926535D0
      DD=DT-2415020D0
      DDD=DD/1D4
      DDD=DDD**2D0
      DM=358.475833+0.985600267*DD-1.12D-05*DDD
      DW=281.220844+0.0000470684*DD+3.39D-05*DDD
      DE=0.01675104-1.1444D-05*DDD
      CALL KEPLER(DM,DE,DEA)
      CALL TRUAN(DE,DEA,DV)
      DMAJAX=1D0
      CALL DIST(DMAJAX,DE,DV,DR)
      DLONG=DV+DW
      DLAT=0D0
      CALL ECTOEQ(DLONG,DLAT,DT,DA,DDEC)
      IF (DA .LT. 0.0D0) DA = DA + 360.0D0
      RETURN
      END
 
C---------------------------------------------------------------------
C Subroutine DIST
C-------------------------------------------------------------------
      SUBROUTINE DIST(DA,DE,DV,DR)
      IMPLICIT REAL*8 (D)
      DRDCON=180D0/3.1415926535D0
      DR=DA*(1D0-DE**2D0)/(1D0+DE*DCOS(DV/DRDCON))
      RETURN
      END
 
C---------------------------------------------------------------------
C Subroutine KEPLER
C---------------------------------------------------------------------
      SUBROUTINE KEPLER(DM,DE,DEA)
      IMPLICIT REAL*8 (D)
      DRDCON=180D0/3.1415926535D0
      DMR=DM/DRDCON
      DEAR=DMR
2001  DELTA=DEAR-DE*DSIN(DEAR)-DMR
      IF(DABS(DELTA).LT.1D-06)GO TO 2000
      DELE=DELTA/(1D0-DE*DCOS(DEAR))
      DEAR=DEAR-DELE
      GO TO 2001
2000  DEA=DEAR*DRDCON
      RETURN
      END
 
C---------------------------------------------------------------------
C Subroutine TRUAN
C---------------------------------------------------------------------
      SUBROUTINE TRUAN(DE,DEA,DV)
      IMPLICIT REAL*8 (D)
      DRDCON=180D0/3.1415926535D0
      DEAR=DEA/DRDCON
      DTV2=DSQRT((1D0+DE)/(1D0-DE))*DTAN(DEAR/2D0)
      DV=(2D0*DATAN(DTV2))*DRDCON+360D0
      DV=DMOD(DV,360D0)
      RETURN
      END
 
C---------------------------------------------------------------------
C Subroutine PTOXY
C---------------------------------------------------------------------
      SUBROUTINE PTOXY(DR,DQ,DX,DY)
      IMPLICIT REAL*8 (D)
      DRDCON=180D0/3.1415926535D0
      DX=DR*DCOS(DQ/DRDCON)
      DY=DR*DSIN(DQ/DRDCON)
      RETURN
      END
 
 
C---------------------------------------------------------------------
C Subroutine XYTOP
C---------------------------------------------------------------------
      SUBROUTINE XYTOP(DX,DY,DR,DQ)
      IMPLICIT REAL*8 (D)
      DRDCON=180D0/3.1415926535D0
      DR=DSQRT(DX*DX+DY*DY)
      DQ=DATAN(DY/DX)*DRDCON
      IF(DX.GE.0D0)GOTO 4000
      DQ=DQ+180D0
4000  CONTINUE
      RETURN
      END
 
C---------------------------------------------------------------------
C Subroutine ECTOEQ
C---------------------------------------------------------------------
      SUBROUTINE ECTOEQ(DLONG,DLAT,DD,DA,DDEC)
      IMPLICIT REAL*8 (D)
      DRDCON=180D0/3.1415926535D0
      DLONGR=DLONG/DRDCON
      DLATR=DLAT/DRDCON
      DX=DCOS(DLONGR)*DCOS(DLATR)
      DY=DSIN(DLONGR)*DCOS(DLATR)
      DZ=DSIN(DLATR)
      CALL XYTOP(DY,DZ,DR,DTHETA)
      DE=23.452294-0.0035626*(DD-2415020D0)/1D4
      DTHETA=DTHETA+DE
      CALL PTOXY(DR,DTHETA,DY,DZ)
      CALL XYTOP(DX,DY,DRHO,DA)
      CALL XYTOP(DRHO,DZ,DB,DDEC)
      RETURN
      END
 
C---------------------------------------------------------------------
C Subroutine MONPOS
C---------------------------------------------------------------------
      SUBROUTINE MONPOS(DD,DA,DDEC)
      IMPLICIT REAL*8 (D)
      DRDCON=180D0/3.1415926535D0
      DI=5.1453
      DT=DD-2415020D0
      DDD=(DT/1D4)**2D0
      DLSUN=279.696678+0.9856473354*DT+0.000303*DDD
      DLSUN=DMOD(DLSUN,360D0)
      DMASUN=358.475833+0.9856002670*DT-0.00015*DDD
      DMASUN=DMOD(DMASUN,360D0)
      DMLMON=270.434358+13.1763965268*DT-0.0011330*DDD
      DMLMON=DMOD(DMLMON,360D0)
      DMAMON=DMLMON-334.329556-0.1114040803*DT+0.0007739*DTD
      DMAMON=DMAMON+360000D0
      DMAMON=DMOD(DMAMON,360D0)
      DLONGQ=259.183275-0.0529539222*DT+0.002078*DDD
      DLONGQ=DMOD(DLONGQ,360D0)
      DEVEC=1.274*DSIN((DMLMON*2D0-2D0*DLSUN-DMAMON)/DRDCON)
      DANUL=0.18581*DSIN(DMASUN/DRDCON)
      DA3=0.37*DSIN(DMASUN/DRDCON)
      DMDASH=DMAMON+DEVEC-DANUL-DA3
      DEQCNT=6.289*DSIN(DMDASH/DRDCON)
      DLCOR=DMLMON+DEVEC-DANUL+DEQCNT
      DVAR=0.65831*DSIN(2D0*(DLCOR-DLSUN)/DRDCON)
      DTOL=DLCOR+DVAR
      DNDASH=DLONGQ-0.16*DSIN(DMASUN/DRDCON)
      DMIN=DTOL-DNDASH
      DR1=1D0
      CALL PTOXY(DR1,DMIN,DX,DY)
      DZ=0D0
      CALL XYTOP(DY,DZ,DR,DTH)
      DTH=DTH+DI
      CALL PTOXY(DR,DTH,DY,DZ)
      CALL XYTOP(DX,DY,DR1,DTA)
      DLONG=DTA+DNDASH
      CALL XYTOP(DR1,DZ,DR,DLAT)
      CALL ECTOEQ(DLONG,DLAT,DD,DA,DDEC)
      RETURN
      END
 
C---------------------------------------------------------------------
C Subroutine CONVJD
C Calculate date from julian day
C---------------------------------------------------------------------
      SUBROUTINE CONVJD(DAY,IY,IM,ID)
      IMPLICIT REAL*8 (D)
      DD=DAY-2415020D0
      IY=JIDINT(DD/365D0)+1901
      IM=JIDINT(DMOD((DD/365D0),1D0))+2
      ID=31
4444  CALL JDCONV(ID,IM,IY,DAYNEW)
      IDIFF=JIDINT(DAYNEW-DAY)
      IF(IDIFF.GE.31)THEN
            IM=IM-1
            IF(IM.LE.0)THEN
                  IY=IY-1
                  IM=12
                  ID=31
               END IF
            ID=31
            GO TO 4444
         END IF
      IF((IDIFF.LT.31).AND.(IDIFF.NE.0))THEN
            ID=ID-1
            GO TO 4444
         END IF
      IF(IDIFF.EQ.0)ID=ID
      RETURN
      END
C---------------------------------------------------------------------
C      Subroutine DECINP
C
C  IN:  D  ANGULAR MEASURE IN +DD.MMSS FORMAT
C
C  OUT: IH DECODED +DD
C       IM DECODED +MM
C       IS DECODED +SS
C
C---------------------------------------------------------------------
      SUBROUTINE DECINP(D,IH,IM,IS)
C  Declare TYPES,ETC
      IFLG=1
      IF(D.LT.0.0)IFLG=-1
      AD=ABS(D)
      IH=IFLG*INT(AD+0.1)
      IM=IFLG*INT(AMOD((AD*100.0+0.1),100.0)+0.1)
      IS=IFLG*INT(AMOD((AD*10000.0+0.1),100.0)+0.1)
      RETURN
      END
C---------------------------------------------------------------------
C Subroutine GETBETA
C        Calculate the BETA angle
C---------------------------------------------------------------------
      SUBROUTINE GETBETA(DASUN,DDSUN,DAOBJ,DDOBJ,DBETA)
      IMPLICIT DOUBLE PRECISION (A-Z)
C
C        Convert angles in degrees to radians for the arithmetic
C
      FACTOR=2.0D0*3.1415926D0/360.0D0
      DASUNRAD=DASUN*FACTOR
      DDSUNRAD=DDSUN*FACTOR
      DAOBJRAD=DAOBJ*FACTOR
      DDOBJRAD=DDOBJ*FACTOR
      DBETA=ACOS(-SIN(DDOBJRAD)*SIN(DDSUNRAD)-COS(DDOBJRAD)
     :      *COS(DDSUNRAD)*COS(DAOBJRAD-DASUNRAD))
C
C        Convert back to degrees
C
      DBETA=DBETA/FACTOR
      IF (DBETA .LT. 0.0D0) DBETA=DBETA+360.0D0
      RETURN
      END
