C++***************************************************************
C Program CONTOUR1
C
C  To contour an image at a series of levels
C  Each contour is written to an output file as an array with 
C  coordinates X and Y, in pixels (Format compatible with ELPLOT).
C
C JLP
C Version of 25-06-89
C Version of 25-11-2010
C--***************************************************************
	PROGRAM CONTOUR1
	REAL*4 XXX,YYY
	REAL*4 XPLOT(1000),YPLOT(1000)
	CHARACTER ANS*1,CHAR1*30,CHAR2*30,TITLE*40,PLOTDEV*32
	CHARACTER*40 NAME,NAMEGRA
	CHARACTER COMMENTS*80,NCHAR*4,PCOLOR*30
	LOGICAL LONGESTCONT,SKELETON
	
C Linux 32 bits:
C       INTEGER*4 PNTR_IN
C Linux 64 bits: I switch to INTEGER*8:
        INTEGER*8 PNTR_IN
	INTEGER*4 MADRID(1)
	COMMON /VMR/MADRID
 
C Common blocks with CONTOUR_SET :
	COMMON/BLOCK9/ LMAX,NCON
	COMMON/BLOCKB1/XXX(6000),YYY(6000)
10	FORMAT(A)
 
C Inquire the format of the files :
	CALL JLP_INQUIFMT
 
C Enters the input file
	PRINT *,' DISPLAY DEVICE ? (TEKTRO, CIFER_T5, ARGS,...)'
	READ(5,10) PLOTDEV
 
	PRINT *,' NAME OF THE FILE :'
	READ(5,10) NAME
	CALL JLP_VM_READIMAG(PNTR_IN,NX,NY,NAME,COMMENTS)
 
C File with the details of the program execution
	OPEN(UNIT=9,FILE='FITELLI.DAT',
     1	ACCESS='SEQUENTIAL',STATUS='UNKNOWN')
	WRITE(9,63)NAME
63	FORMAT(2X,'NAME OF THE INPUT FILE :',A40,/)
 
C Graphic file :
C	NAMEGRA='contour1.gra'
C	OPEN(UNIT=12,FORM='UNFORMATTED',FILE=NAMEGRA,
C     1	ACCESS='SEQUENTIAL',STATUS='UNKNOWN')
 
C  First calculates the sky level of the image KTA in 4 edge zones :
	  PRINT *,' SKY LEVEL ?'
	  READ(5,*) SKY
C	  CALL AUTO_SKY(MADRID(PNTR_IN),NX,NY,NX,SKY,SIGMA)
 
C	WRITE(9,437) SKY,SIGMA
C437	FORMAT(' SKY LEVEL AND STD DEV:',2(F12.4,2X))
 
C****************** Contouring the image ********************
C Entering the surface brightness levels (in log10) :
44	PRINT 440
440	FORMAT(' ENTER MIN, MAX LEVELS AND  STEP (in log10) :')
	READ(5,*) RMIN,RMAX,STEP
 
	WRITE(9,441) STEP,RMIN,RMAX
441	FORMAT(/,2X,'NEW SERIES OF LEVELS:',5X,'STEP =',F8.3,
     1	' FROM',F8.3,' TO',F8.3,' [ IN LOG10(INTENSITY) ]',/)
 
C Main loop on the levels... (isophote CV (real), level CK)
C First level :
	CK=RMIN
 
C Calling GGROPE to find the contour KV
41	CV=SKY+10.**CK
	LMAX=0
	NCON=0
	LONGESTCONT=.FALSE.
	SKELETON=.FALSE.
	CALL GGROPE(MADRID(PNTR_IN),NX,NY,NX,OX,OY,CV,LONGESTCONT)
 
	WRITE(9,750) CK,CV,NCON,LMAX
	WRITE(6,750) CK,CV,NCON,LMAX
750	FORMAT(/,'******** THE LEVEL',F7.2,'(LOG10)  OR',
     1	F12.3,' (INT)     CONTAINS',I6,
     1	' CONTOURS',/,
     1	' LENGTH OF SELECTED CONTOUR',I8,' POINTS')
	
	IF(LMAX.LE.0)GOTO 800
C Displaying the contour :
	  CHAR1=' X '
	  CHAR2=' Y '
	  WRITE(TITLE,678) STEP,RMIN,RMAX
678	  FORMAT('CONTOURS:',3(F8.3,X))
          NCHAR='L0'
          PCOLOR='Default'
 	  CALL NEWPLOT(XXX,YYY,LMAX,LMAX,1,CHAR1,CHAR2,
     1	TITLE,NCHAR,PCOLOR,PLOTDEV,NAME,COMMENTS)
 
C Storing the contour (only a subsample if more than 800)
C Defining the subsample :
	 LMAXPLOT=LMAX
	 ICNT = 1

94	  IF(LMAXPLOT.LE.800)GOTO 95
	   ICNT = ICNT*2
	   LMAXPLOT = LMAXPLOT/2
          GOTO 94

95	  DO 101 I=1,LMAXPLOT
	   XPLOT(I)=XXX(ICNT*I)
	   YPLOT(I)=YYY(ICNT*I)
101	  CONTINUE
C
C Closes the isophote (if closed contour) :
	 XTEST=AMAX1(ABS(XPLOT(LMAXPLOT)-XPLOT(1)),
     1	ABS(YPLOT(LMAXPLOT)-YPLOT(1)))
	   IF(XTEST.LT.0.5)THEN
	    LMAXPLOT=LMAXPLOT+1
	    XPLOT(LMAXPLOT)=XPLOT(1)
	    YPLOT(LMAXPLOT)=YPLOT(1)
	   ENDIF
C The points are written to disc file unit 12 as a string
C	 WRITE(12) LMAXPLOT,(XPLOT(I),YPLOT(I),I=1,LMAXPLOT)
C Stores the parameters :
C	 WRITE(12) 0.,1.,1.,1.,1.
 
C Preparing the next step
800	CK=CK+STEP
	
	IF(CK.LE.RMAX) GO TO 41
 
	PRINT *,' TERMINATED. DO YOU WANT ANOTHER LEVEL ? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GO TO 44
 
	WRITE(9,935)
935	FORMAT(/,' JOB COMPLETED.',/,'1')
 
	CLOSE(9)
	CLOSE(12)
 
	STOP
	END
C------------------------------------------------------------------------
C	include '../jlpsub/auto_sky.for'
	include '../jlpsub/contour_set.for'
