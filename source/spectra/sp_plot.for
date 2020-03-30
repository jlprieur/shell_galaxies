C++***************************************************************
C SP_PLOT
C Program to plot spectra (from Parkes)
C
C Version of 17-10-90
C--**************************************************************
	PROGRAM SP_PLOT
	PARAMETER (IDIM=2000,NMAX=300)
	REAL*8 XX(IDIM),YY(IDIM),YY1(IDIM)
	REAL*8 XC(20),SDEV(20),ERR
	REAL*4 CVELOCITY,XLOW(20),XHIGH(20)
	REAL*4 X1(IDIM),Y1(IDIM),Y2(IDIM)
	REAL*4 XOUT,YOUT
	INTEGER*4 NPTS,NOUT,KK,NPTS1
	LOGICAL PLAN,QUIET
	CHARACTER INFRAM(NMAX)*60
	INTEGER*4 INOPROMPT
	CHARACTER LIMITS_USER*80,LIMITS_HARD*80,PLOTDEV2*32
	CHARACTER ANS*1,PLOTDEV*32,PCOLOR*30
	CHARACTER INFILE*40,OUTFILE*40,COMMENTS*80,BUFFER*80
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,NCHAR*4
 
C Common block for NEWPLOT:
	COMMON/NEWPLOT1/INOPROMPT,PLAN,LIMITS_USER,PLOTDEV2,
     1	LIMITS_HARD
 
C Common block with JLP_DIRECTORY:
	COMMON/JLP_DIRECT/INFRAM
 
	CALL JLP_BEGIN
10	FORMAT(A)
 
	PRINT 88
88	FORMAT(' Program SP_PLOT to plot spectra from Nancay')
 
	PRINT *,' Graphic device? (for direct plot)'
	READ(5,10) PLOTDEV
 
C Input file:
78	WRITE(6,*) ' Input ASCII file(s) ? ("suff*.ext"  accepted)'
	READ(5,10) INFILE
	QUIET=.TRUE.
	CALL JLP_DIRECTORY(INFILE,NFILES,QUIET)
	WRITE(6,79) NFILES
79	FORMAT(' Number of files found: ',I5)
	IF(NFILES.LE.0) GOTO 78
 
C Lower and upper limits (-0.15,0.35 for Parkes)
	 WRITE(6,*) ' Lower and upper limit (in Y axis)'
	 READ(5,*) F1,F2
 
C Main loop on the files:
	DO K=1,NFILES
 
	  OPEN(1,FILE=INFRAM(K),STATUS='OLD',ACCESS='SEQUENTIAL')
	  READ(1,10) BUFFER
	  READ(BUFFER,*) X1(1),Y1(1)
	  I=INDEX(BUFFER(27:80),' ')+26
	  COMMENTS=BUFFER(I:80)
	    DO I=2,IDIM
	     READ(1,*,END=56) X1(I),Y1(I)
	    END DO
56	  NPTS=I-1
	  CLOSE(1)
 
	  CVELOCITY=X1(128)
	  WRITE(6,35) COMMENTS(1:53),CVELOCITY
35	  FORMAT(' Comments: ',A53,/,' Central velocity:',G12.3)
 
C Graphic options:
	  PLAN='N'
	  INOPROMPT=1
	  PLOTDEV2='N'
	  LIMITS_HARD=' '
	  V1=MIN(X1(NPTS-15),X1(15))
	  V2=MAX(X1(NPTS-15),X1(15))
	  WRITE(LIMITS_USER,*)V1,V2,F1,F2
 
	  CHAR1='V helio (km/s)'
	  CHAR2='Flux (Jy)'
	  I=INDEX(INFRAM(K),']')+1
	  TITLE=INFRAM(K)(I:40)
	  NCHAR='L0'
          PCOLOR='Default'
62	  CALL NEWPLOT(X1,Y1,NPTS,IDIM,1,CHAR1,CHAR2,TITLE,
     1   	NCHAR,PCOLOR,PLOTDEV,INFRAM(K),COMMENTS)
 
C End of the loop on the files:
	END DO
 
 	CALL JLP_END
	STOP
	END
