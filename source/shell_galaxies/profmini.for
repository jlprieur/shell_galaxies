C++********************************************************************	
C Program PROFMINI
C
C Computes a minimum profiles from elementary sector profiles
C created by "PROFILE1" selecting the option "Multiple sectors".
C
C SYNTAX:
C       RUNS PROFMINI generic_name output_profile
C
C Example:
C       RUNS PROFMINI test test.pro
C
C Nota1: the generic name should not have any extension. This name is used
C        to build all the profile names created by PROFILE1
C        (test_01.pro, test_02.pro, ... test_12.pro in our example).
C
C JLP
C Version of 17-04-97
C--********************************************************************	
	PROGRAM PROFMINI
	PARAMETER (IDIM1=2000,IDIM2=12)
	REAL*4 RAD(IDIM1,IDIM2),PROF(IDIM1,IDIM2)
	REAL*4 RAD1(IDIM1),PROF1(IDIM1)
	INTEGER*4 NBER(IDIM1,IDIM2),NBER1(IDIM1),NPTS(IDIM2),
     1	KSECT1(IDIM1)
	CHARACTER NAMEPRO*40,NAMEMINI*40
 
10	FORMAT(A)
	CALL JLP_BEGIN
 
	OPEN(7,FILE='profmin.log',STATUS='unknown')
 
	PRINT 20
20	FORMAT(' Program PROFMINI  Version of 17-04-97',/,
     1	' Computes a minimum profiles from the files',/,
     1	' created by "PROFILE1" with multiple sectors option')
 
C Entering the generic name of the profiles
	PRINT 21
21	FORMAT(' Enter the generic name of the profiles ',
     1	'(without _01.pro, _02.pro, ...)')
	READ(5,10) NAMEPRO
	WRITE(7,*)'Generic name:',NAMEPRO
 
C Entering the number of sectors
C	PRINT *,' Enter the number of sectors: (<12) '
C	READ(5,*) NPRO
	NPRO=12
	WRITE(7,*)' Number of sectors:',NPRO
 
C Entering the name of the output profile
25	PRINT 23
23	FORMAT(' Enter the name of the output profile')
	READ(5,10) NAMEMINI
	WRITE(7,*)' Output profile:',NAMEMINI
 
C Open the output profile :
	OPEN(2,FILE=NAMEMINI,STATUS='unknown')
 
C Reads the profiles:
	NMAX=IDIM1
	CALL READ_PROFMINI(NAMEPRO,NMAX,NPRO,RAD,PROF,NBER,NPTS)
 
C Compute the minimum profile :
	CALL PROFMIN1(RAD,PROF,NBER,NPTS,NMAX,NPRO,
     1	PROF1,RAD1,NBER1,KSECT1,NPTS1)
	
	WRITE(2,*)NPTS1
	 DO I=1,NPTS1
	  WRITE(2,*)RAD1(I),PROF1(I),FLOAT(NBER1(I)),KSECT1(I)
	 END DO
 
	CLOSE(2)
	CLOSE(7)
 
	PRINT *,' Logfile in "profmini.log"'
	CALL JLP_END
	STOP
	END
C********************************************************************	
C Subroutine READ_PROFMINI
C
C Input :
C
C NAMEPRO*40      CHARACTER   Generic name of the NPRO profiles
C NMAX		  INTEGER     First dimension of the output arrays
C NPRO            INTEGER     Number of profiles to read
C
C Output:
C
C RAD(NMAX,NPRO)  REAL        Radii of the elementary sectors
C PROF(NMAX,NPRO) REAL        Profiles of the elementary sectors
C NBER(NMAX,NPRO) INTEGER     Number of points taken into account for
C			      computing the corresponding value of the profile.
C NPTS(NPRO)      INTEGER     Number of bins for each profile
C
C********************************************************************	
	SUBROUTINE READ_PROFMINI(NAMEPRO,NMAX,NPRO,RAD,
     1	PROF,NBER,NPTS)
 
	REAL*4 RAD(NMAX,NPRO),PROF(NMAX,NPRO)
	INTEGER*4 NBER(NMAX,NPRO),NPTS(NMAX)
	CHARACTER NAMEPRO*40,BUFFER*80
 
10	FORMAT(A)
 
C Check the input name :
	INDX=INDEX(NAMEPRO,' ')
	 IF(INDX.EQ.0)THEN
	  PRINT *,' ERROR : BAD NAME FOR THE PROFILE'
	  STOP
	 ENDIF
 
C Loop on the NPRO profiles :
	DO K=1,NPRO
 
C Increments the name of the file :
	  I1=K/10
	  I2=K-10*I1
	  WRITE(NAMEPRO(INDX:INDX+6),23)I1,I2
23	  FORMAT('_',I1,I1,'.pro')
 
C Open the corresponding file :
	  OPEN(1,FILE=NAMEPRO,STATUS='OLD')
 
C Copy the header of the first header on the output profile:
	  IF(K.EQ.1)THEN
	    DO J=1,32
	      READ(1,10) BUFFER
	      WRITE(2,10) BUFFER
	    END DO	
	  ELSE
	    DO J=1,32
	      READ(1,10) BUFFER
	    END DO	
	  ENDIF
 
C Read the profile
	READ(1,*)NPTS(K)
	 DO I=1,NPTS(K)
	   READ(1,*) RAD(I,K),PROF(I,K),XNUMB
	   NBER(I,K)=XNUMB
	 END DO
 
C Close the file :
	CLOSE(1)
	
C Go to another file :
	END DO
 
	RETURN
	END
 
C********************************************************************	
C Subroutine PROFMIN1
C
C Input :
C
C RAD(NMAX,NPRO)  REAL        Radii of the elementary sectors
C PROF(NMAX,NPRO) REAL        Profiles of the elementary sectors
C NBER(NMAX,NPRO) INTEGER     Number of points taken into account for
C			      computing the corresponding value of the profile.
C NPTS(NPRO)      INTEGER     Number of bins for each profile
C
C
C Output:
C
C PROF1(NPTS1)     REAL        Minimum profile
C RAD1(NPTS1)      REAL        Radii (arbitrarily taken equal to the
C			      radii of the longest profile)	
C NBER1(NPTS1)     INTEGER     Number of points in the chosen profile
C********************************************************************	
	SUBROUTINE PROFMIN1(RAD,PROF,NBER,NPTS,NMAX,NPRO,
     1	PROF1,RAD1,NBER1,KSECT1,NPTS1)
 
	REAL*4 RAD(NMAX,NPRO),PROF(NMAX,NPRO)
	REAL*4 RAD1(NMAX),PROF1(NMAX)
	REAL*8 XSUM
	INTEGER*4 NBER(NMAX,NPRO),NBER1(NMAX),NPTS(NPRO)
	INTEGER*4 KSECT1(NMAX),PROF_INDEX
 
C Looking for the longest profile:
	KMAX=1
	LMAX=NPTS(1)
	  DO K=1,NPRO
	    IF(NPTS(K).GT.LMAX)THEN
	      LMAX=NPTS(K)
	      KMAX=K
	    END IF
	  END DO
 
	PRINT *,' Longest sector, length :',KMAX,LMAX
 
C Main loop : for each radii, looking for the minimum
C with a constraint : the number of points must be larger
C than 100 at large radii (noise pb)
 
	NPTS1=0
 
	DO I=1,NPTS(KMAX)
	  NSUM1=0
	  NSECT1=0
	  XSUM=0.
 
76	  XMIN=320000.
 
C For each profile :
	    DO K=1,NPRO
 
C Look for the corresponding position :
	     IF(K.NE.KMAX)THEN
	       II=PROF_INDEX(RAD,NMAX,K,NPTS(K),RAD(I,KMAX))
	     ELSE
	       II=I
	     ENDIF
 
C Check if minimum (with the constraint on the number of values (>100)
C at large radii (> 80 arcsec) :
 
C The following condition avoid the problems of values out of the range
C of one of the sectors : II.NE.NPTS(K)
 
	   IF((II.NE.NPTS(K).AND.PROF(II,K).LT.XMIN)
     1	.AND.(NBER(II,K).GE.100.OR.RAD(II,K).LT.80.))THEN
	     XMIN=PROF(II,K)
	     NB1=NBER(II,K)
	     KSECT=K
	     IISECT=II
	   ENDIF
 
C Check on another sector:
	  END DO
 
C********************
C If minimum found during this loop :
	IF(XMIN.NE.320000.)THEN
 
C Neutralize this minimum (for possible further loops on this position)
	  PROF(IISECT,KSECT)=+320000.
	  NSUM1=NB1+NSUM1
	  NSECT1=NSECT1+1
	  XSUM=XMIN*FLOAT(NB1)+XSUM
 
	WRITE(7,*)I,IISECT,KSECT
 
C Second constraint on the number of points : for the inner parts,
C we take as many sectors as necessary to obtain 100 points:
C Look for another minimum :
C@	  IF(NSUM1.LT.100.AND.RAD(I,KMAX).LT.80.)GO TO 76
 
	  ENDIF
 
C*******
C If minimum found at least once ,
C write on the output profile :
	IF(NSECT1.GT.0)THEN
	  NPTS1=NPTS1+1
	  RAD1(NPTS1)=RAD(I,KMAX)
	  PROF1(NPTS1)=XSUM/FLOAT(NSUM1)
	  NBER1(NPTS1)=NSUM1
	  KSECT1(NPTS1)=KSECT
C	  PRINT *,NPTS1,RAD1(NPTS1),PROF1(NPTS1),NBER1(NPTS1),
C     1	KSECT1(NPTS1)
	ENDIF
 
C Go to another position :
	END DO	
 
	RETURN
	END
 
C**********************************************************************
C Integer function PROF_INDEX
C Returns the index I such as :
C      (RAD(I-1,K)+RAD(I,K))/2. =< VALUE < (RAD(I,K)+RAD(I+1,K))/2
C**********************************************************************
	INTEGER FUNCTION PROF_INDEX(RAD,NMAX,K,NPTS,VALUE)
	REAL*4 RAD(NMAX,1)
	I=1
	WORK=(RAD(1,K)+RAD(2,K))/2.

94	IF (WORK.GE.VALUE.OR.I.GE.NPTS) GOTO 95
	   I=I+1
	   WORK=(RAD(I-1,K)+RAD(I,K))/2.
        GOTO 94

95	IF(I.NE.NPTS)I=MAX(1,I-1)
	PROF_INDEX=I
	RETURN
	END
