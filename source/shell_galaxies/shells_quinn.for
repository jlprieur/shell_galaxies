C++------------------------------------------------------------------
C
C       PROGRAM TO COMPUTE THE SHELL DIAGRAM FROM
C       THE PERIOD/RMAX DATA
C
C
C RADIUS (input)
C PERIOD (input)
C
C ZRS (output) :
C      ZRS(MM,1)=Radius of shell MM with r1/4 plus halo
C      ZRS(MM,2)=Radius of shell MM if keplerian only
C ZANUM (intermediate) :
C      ZANUM(MM,1)=MM-PHF+TAU-1
C      ZANUM(MM,2)=MM-PHF
C
C P. J. Quinn  Sept. 1987
C Revised JLP  28-01-88
C------------------------------------------------------------------
        IMPLICIT DOUBLE PRECISION (A-H,O-Y)
	REAL*8 PERIOD(1000),RADIUS(1000)
	REAL*4 ZANUM(1000,2),ZRS(1000,2)
C
C Read unit 9 (Parameters)
	PRINT *,' Reading FOR009.DAT  (parameters)'
	OPEN(9,FILE='FOR009.DAT',STATUS='OLD')
        READ(9,*)GM1,GAMMA1,CHI,GM2,GAMMA2
	CLOSE(9)
	
C Read unit 10 (period-apocentre table)
	PRINT *,' Reading FOR010.DAT  (period-apocentre table)'
	OPEN(10,FILE='FOR010.DAT',STATUS='OLD')
        READ(10,*,END=20)(PERIOD(KK),RADIUS(KK),KK=1,1000)
20	KK=KK-1
        PMIN=PERIOD(1)
        RMIN=RADIUS(1)
	CLOSE(10)
	
C Open unit 11 (output table with shell parameters)
	OPEN(11,FILE=' SHELLS.SQI',STATUS='NEW')
 303    FORMAT(15X,4F12.6)
        WRITE(11,209)GM1,GAMMA1,CHI,GM2,GAMMA2
 209    FORMAT(' HALO MASS (GM1) AND GAMMA1, CHI (R_CUT/GAMMA_HALO),'
     1	' ELLIPTICAL MASS (GM2) AND GAMMA2',/,
     1	15X,5F12.6,/,
     1	' SHELL #, RADIUS, RADIUS (IF ONLY KEPLERIAN),',
     1	' LOG10(RADIUS), LOG(M-PHI+TAU-1),SLOPE (LOG/LOG PLANE)',/)
 
C Prompt some information to the user:
        WRITE(6,222)
 222    FORMAT(1X,'SHELLS>> NUMBER OF SHELLS   : '$)
        READ(5,*)NSHELL
C Increase NSHELL by 1 (otherwise one shell is missing...)
        NSHELL=NSHELL+1
 
	MM=NSHELL
        WRITE(6,223)
 223    FORMAT(1X,'SHELLS>> PHASE FACTOR       : '$)
        READ(5,*)PHF
        WRITE(6,999)
 999    FORMAT(1x,'SHELLS>> Tau                : '$)
        READ(5,*)tau
 
C Main loop
	MM=NSHELL
94	IF(MM.LE.1) GOTO 95
 
C Looking for the right period (TIME) in the array PERIOD
	TIME=((FLOAT(NSHELL)-PHF+tau-1.)/
     1	(FLOAT(MM)-PHF+tau-1.))*PMIN
 
	DO I=1,KK
	  IF(TIME.GE.PERIOD(I).AND.TIME.LT.PERIOD(I+1))GOTO 10
	END DO
 
	PRINT *,' FATAL ERROR'
	PRINT *,' The table of the periods is not complete'
	STOP
 
C When the nearest period is found :
10	SLOPE=(RADIUS(I+1)-RADIUS(I))/(PERIOD(I+1)-PERIOD(I))
        RR=RADIUS(I)+SLOPE*(TIME-PERIOD(I))
        ZRS(MM,1)=RR
        AN=NSHELL
        AM=MM
        ZRS(MM,2)=rmin*((float(NSHELL)-phf+tau-1.)/
     &                   (float(mm)-phf+tau-1.))**(2./3.)
        ZANUM(MM,1)=MM-PHF+tau-1
        AZX=ALOG(ZRS(MM,1))
        ALZ=ALOG(ZANUM(MM,1))
        IF(MM.EQ.NSHELL)THEN
          SLOPE=0
        ELSE
          SLOPE=(ALOG(ZRS(MM+1,1))-AZX)/(ALOG(ZANUM(MM+1,1))-ALZ)
        END IF
	WRITE(11,107)MM,ZRS(MM,1),ZRS(MM,2),AZX,ALZ,SLOPE
 107    FORMAT(15X,I3,5F12.6)
        ZANUM(MM,2)=MM-PHF
        MM=MM-1
 
	GOTO 94
 
95	CLOSE(9)
	CLOSE(10)
	CLOSE(11)
	PRINT *,' Output in SHELLS.SQI'
	END
