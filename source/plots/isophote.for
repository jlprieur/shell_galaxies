C++***************************************************************
C	Program ISOPHOTE
C To draw isophotes
C
C	IMAGE(NX,NY) Input image to be drawn
C	 (REAL*4)
C	AUX(I) : INTENSITE DU NIVEAU I (REEL)
C	NIV(I) : INTENSITE DU NIVEAU I (ENTIER)
C	ID(5) : MAILLE ELEMENTAIRE POUR LE TRACE DES ISOPHOTES
C	JK(4) : MAILLE ELEM. DE TEST ( > OU < A L'ISOPHOTE)
C JLP Version of 05-06-91
C--****************************************************************
	PROGRAM ISOPHOTE
        PARAMETER (MAX_LEVELS=100)
	INTEGER*4 PNTR_IN,MADRID(1)
	REAL*4 NIV(MAX_LEVELS),XOUT,YOUT
	REAL*4 XCENTER,YCENTER,DIAMETER,SKY,SIGMA
        REAL*4 MIN1,MAX1,MEAN1,MEAN2,SIGMA1,SIGMA2
        REAL*4 NE_LENGTH,X_NE,Y_NE,NE_ANGLE
        REAL*4 START1,STEP1,BACK1,EPSILON1
	INTEGER*4 NLEV1,NOUT,NE_FLAG
        INTEGER*4 IXSTART,IXEND,IYSTART,IYEND
        INTEGER*4 IXMIN,IXMAX,IYMIN,IYMAX
	LOGICAL*4 INTERAC,DEFAULT_VALUES,DRAW_CIRCLE,DRAW_SCALE,NORTH_EAST
        INTEGER*4 TICKS_IN,BOX_NUMBERS
	CHARACTER INTERACDEV*32,PLOTDEV*32,HARDCOPYDEV*32
        CHARACTER BUFFER*80,BUFFER_LEVELS*80
	CHARACTER NAME*40,COMMENTS*80,ANS*1,SCALE_LABEL*40
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40
	COMMON/VMR/MADRID

C Output points :
	COMMON/STR_OUTPUT/XOUT(200),YOUT(200),NOUT


10	FORMAT(A)
	IOPT=0
        TICKS_IN = 1
        BOX_NUMBERS = 1
	DRAW_CIRCLE=.FALSE.
	DRAW_SCALE=.FALSE.
        NORTH_EAST=.FALSE.
	OPEN(1,FILE='isophote.log',STATUS='unknown',
     1	ACCESS='SEQUENTIAL')
 
C Inquire the format of the files:
	CALL JLP_BEGIN
	CALL JLP_INQUIFMT
 
        PRINT *,84
84	FORMAT(' Program ISOPHOTE Version 05-06-91')

	PRINT *,' Use defaults values (full image, step=1, etc...)? [Y]' 
	READ(5,10)ANS
	DEFAULT_VALUES=(ANS.NE.'N'.AND.ANS.NE.'n')

C Inquire the device :
	WRITE(6,32)
32      FORMAT(' Interactive graphic output device',
     1         '(&xterm, &tektronix, &square, &none ...)?')
	READ(5,10) INTERACDEV
C Shift of one to allow standard denomination
C like &tektronix, $TEKTRO, etc
	INTERACDEV=INTERACDEV(2:)
	INTERAC=(INTERACDEV(1:1).EQ.'x'.OR.INTERACDEV(1:1).EQ.'t')
 
C Input of the parameters for drawing the levels
	PRINT *,' Input image ?'
	READ(5,10) NAME
	CALL JLP_VM_READIMAG(PNTR_IN,NX,NY,NAME,COMMENTS)
	WRITE (1,104)NAME
104	FORMAT(/,2X,' Input file: ',A)
 
C Batch mode:
	IF(DEFAULT_VALUES)THEN
 	  IXSTART=1
	  IXEND=NX
	  ISTEPX=1
	  IYSTART=1
	  IYEND=NY
	  ISTEPY=1

        ELSE
C Boundaries :
          CALL INPUT_BOUNDARIES(IXSTART,IXEND,ISTEPX,IYSTART,IYEND,ISTEPY)
        ENDIF
 
C Input of the levels for the first time:
        CALL JLP_AUTO_SCALE(MADRID(PNTR_IN),NX,NY,NX,MIN1,MAX1,
     1  MEAN1,MEAN2,SIGMA1,SIGMA2,IXMIN,IYMIN,IXMAX,IYMAX)
        CALL AUTO_SKY(MADRID(PNTR_IN),NX,NY,NX,SKY,SIGMA)

C Computing automatic (log) scale:

C Epsilon is the smallest value for log computation:
        EPSILON1=SIGMA/1.E+9
C Default is log scale:
        BACK1=SKY-3.*SIGMA
C Minimum is mean (after 3-sigma rejection) plus 1.5 sigma:
        WORK=MEAN2+1.5*SIGMA2
C Conversion to log scale:
        WORK=MAX(EPSILON1,WORK-BACK1)
        START1=LOG10(WORK)
C Maximum is actual maximum value (but will not be reached): 
C We will stop at one level below...
        WORK=MAX(EPSILON1,MAX1-BACK1)
C Conversion to log scale:
        WORK=LOG10(WORK)
        NLEV1=10
        STEP1=(WORK-START1)/FLOAT(NLEV1-2)
        STEP1=MAX(STEP1,EPSILON1)
        WRITE(BUFFER_LEVELS,23) START1,NLEV1,STEP1,BACK1
23      FORMAT(1PG10.3,':',I4,':',1PG10.3,':',1PG10.3)
        WRITE(6,28)
28      FORMAT('Computing automatic (log) scale')
        CALL INPUT_LEVELS(NIV,NNIV,MAX_LEVELS,BUFFER_LEVELS,EPSILON1)	
 
501     IF(DEFAULT_VALUES.OR.INTERAC)THEN
	  CHAR1=' '
	  CHAR2=' '
	  TITLE(1:40)=NAME(1:14)//BUFFER(1:25)
	ELSE
600	 PRINT *,' Enter the title :'
	 TITLE=' '
	 READ(5,10) TITLE(2:40)
	 PRINT *,' Enter X label :'
	 CHAR1=' '
	 READ(5,10) CHAR1(2:30)
	 PRINT *,' Enter Y label :'
	 CHAR2=' '
	 READ(5,10) CHAR2(2:30)
	ENDIF

C Beginning:
500     CONTINUE	
        IF(IOPT.NE.3)THEN
           PLOTDEV=INTERACDEV
        ELSE
           PLOTDEV=HARDCOPYDEV
        ENDIF
C Debug
        CALL ISOCONTOURS(MADRID(PNTR_IN),NX,NY,NX,IXSTART,
     1    IXEND,ISTEPX,IYSTART,IYEND,ISTEPY,NIV,NNIV,
     1    CHAR1,CHAR2,TITLE,
     1	  PLOTDEV,INTERAC,TICKS_IN,BOX_NUMBERS,NAME,COMMENTS)
C Circle:
        IF(DRAW_CIRCLE)THEN
          WRITE(1,44) XCENTER,YCENTER,DIAMETER
44        FORMAT(' Circle at X,Y = ',2(G10.3,1X),
     1             ' Diameter: ',G10.3)
	  CALL STR_CIRCLE(XCENTER,YCENTER,DIAMETER)
	  CALL JLP_GFLUSH
        ENDIF
C Scale 
        IF(DRAW_SCALE)THEN
	  CALL STR_SCALE(X_SCALE,Y_SCALE,SCALE_LENGTH,SCALE_LABEL)
	  CALL JLP_GFLUSH
        ENDIF

C North East direction: 
        IF(NORTH_EAST)THEN
	  CALL STR_NORTH_EAST(X_NE,Y_NE,NE_LENGTH,NE_ANGLE,NE_FLAG)
	  CALL JLP_GFLUSH
        ENDIF

C Closes plot:
        CALL STR_PLOT(0.,0.,999)
C Print output points (cursor)
	IF(NOUT.GT.0)THEN
	  DO I=1,NOUT
	    CALL GETPIXEL(MADRID(PNTR_IN),NX,NY,NX,
     1      XOUT(I),YOUT(I),IX,IY,XVALUE) 
            IF(XVALUE.GT.0.)THEN
               WORK = LOG10(XVALUE)
            ELSE 
               WORK = -100.
            ENDIF
	    PRINT *,' Point #',I,IX,IY,' value: ',XVALUE,' log10: ',WORK
	  END DO
	ENDIF
C---------------------------------------------------------
C Entering next option
	PRINT 310
310	FORMAT(' Menu :',/,
     1	' 1: Change the levels',/,
     1	' 2: Change the limits for the display',/,
     1	' 3: Store the graph (plot.plt)',/,
     1	' 4: Change the image only',/,
     1	' 5: Change the title and labels',/,
     1	' 6: Draw a circle',/,
     1	' 7: Erase the circle',/,
     1	' 8: Draw a scale bar',/,
     1	' 9: Erase the scale bar',/,
     1	' 10: Toggle for axis numbers: ON/OFF',/,
     1	' 11: Toggle for axis ticks: IN/OUT',/,
     1	' 12: Draw North-East directions',/,
     1	' 13: Erase North_East directions',/,
     1	' 20: Exit',/,
     1	' Enter the option you want : ')
	READ(5,*,ERR=999) IOPT
C**********************************************************
C Change the levels:
	IF(IOPT.EQ.1) THEN
          CALL INPUT_LEVELS(NIV,NNIV,MAX_LEVELS,BUFFER_LEVELS,EPSILON1)	
C Then plots the graph with new levels:
          GOTO 500
        ENDIF
C**********************************************************
C Change boundaries :
	IF(IOPT.EQ.2) THEN 
          CALL INPUT_BOUNDARIES(IXSTART,IXEND,ISTEPX,IYSTART,IYEND,ISTEPY)
	  GOTO 500
        ENDIF
C**********************************************************
C Change output device :
	IF(IOPT.EQ.3) THEN
	  WRITE(6,33)
33        FORMAT(' Hardcopy graphic device',
     1            ' (&postscript, &landscape, &square ...)?')
	  READ(5,10) HARDCOPYDEV
C Shift of one to allow standard denomination like &tektronix, $TEKTRO, etc
	  HARDCOPYDEV=HARDCOPYDEV(2:)
	  INTERAC=(HARDCOPYDEV(1:1).EQ.'x'.OR.HARDCOPYDEV(1:1).EQ.'t')
	  GO TO 500 
	ENDIF
C**********************************************************
C Change input image:
	IF(IOPT.EQ.4)THEN
           PRINT *,' INPUT FILE ?'
	   READ(5,10) NAME
	   CALL JLP_VM_READIMAG(PNTR_IN,NX,NY,NAME,COMMENTS)
	   WRITE (1,104)NAME
          IF(DEFAULT_VALUES.OR.INTERAC) TITLE(1:40)=NAME(1:14)//BUFFER(1:25)
           GOTO 500
        ENDIF
C**********************************************************
	IF(IOPT.EQ.5)GO TO 600 
C**********************************************************
C Draw a circle:
	IF(IOPT.EQ.6)THEN
	  WRITE(6,*)' Xcenter, Ycenter, Diameter?'
	  READ(5,*) XCENTER,YCENTER,DIAMETER
	  DRAW_CIRCLE=.TRUE.
	  GOTO 500
	ENDIF
C**********************************************************
C Erase the circle:
	IF(IOPT.EQ.7)THEN
	  DRAW_CIRCLE=.FALSE.
	  GOTO 500
	ENDIF
C**********************************************************
C Draw a scale bar:
	IF(IOPT.EQ.8)THEN
	  WRITE(6,*)' The scale will be an horizontal bar' 
	  WRITE(6,*)' Length in pixel, X and Y start position (user coord.):'
	  READ(5,*) SCALE_LENGTH,X_SCALE,Y_SCALE
	  WRITE(6,*)' Label to write on top (centered):'
	  READ(5,10) SCALE_LABEL 
	  DRAW_SCALE=.TRUE.
	  GOTO 500
	ENDIF
C**********************************************************
C Erase the scale bar:
	IF(IOPT.EQ.9)THEN
	  DRAW_SCALE=.FALSE.
	  GOTO 500
	ENDIF
C**********************************************************
C Toggle to active or desactive axis numbers:
	IF(IOPT.EQ.10)THEN
          BOX_NUMBERS = 1 - BOX_NUMBERS
          IF(BOX_NUMBERS.EQ.1)THEN
            WRITE(6,*)'Axes numbered'
          ELSE
            WRITE(6,*)'Axes without numbers'
          ENDIF
	  GOTO 500
	ENDIF
C**********************************************************
C Toggle for ticks in or out:
	IF(IOPT.EQ.11)THEN
          TICKS_IN = 1 - TICKS_IN 
          IF(TICKS_IN.EQ.1)THEN
            WRITE(6,*)'Ticks in'
          ELSE
            WRITE(6,*)'Ticks out'
          ENDIF
	  GOTO 500
	ENDIF
 
C**********************************************************
C Draw North-East directions: 
	IF(IOPT.EQ.12)THEN
	  WRITE(6,*)' North-East directions: ' 
	  WRITE(6,*)' Bar length in pixel, X and Y start position (user coord.):'
	  READ(5,*) NE_LENGTH,X_NE,Y_NE
	  WRITE(6,21)
21        FORMAT(' Position angle for the North ',/,
     1    '(Convention is OX axis: 0., OY axis: 90., -OX: 180., -OY: 270.)',/,
     1    ' and direction flag (1 if East angle > North angle, -1 otherwise):')
	  READ(5,*) NE_ANGLE,NE_FLAG 
	  NORTH_EAST=.TRUE.
	  GOTO 500
	ENDIF
C**********************************************************
C Erase the scale bar:
	IF(IOPT.EQ.13)THEN
	  NORTH_EAST=.FALSE.
	  GOTO 500
	ENDIF
 
C
999	PRINT *,' Logfile in "isophote.log"'
	CLOSE(1)
	CALL JLP_END
	STOP
	END
C************************************************************
        SUBROUTINE GETPIXEL(ARRAY,NX,NY,IDIM,X,Y,IX,IY,XVALUE) 
	REAL*4 ARRAY(IDIM,*)
        REAL*4 X,Y,XVALUE
        INTEGER*4 NX,NY,IX,IY

	IX=NINT(X)
	IX=MIN(NX,IX)
	IX=MAX(1,IX)

	IY=NINT(Y)
	IY=MIN(NY,IY)
	IY=MAX(1,IY)

	XVALUE=ARRAY(IX,IY)

        RETURN
	END
C********************************************************************
C 
C EPSILON1 is the smallest value for log computation
C********************************************************************
        SUBROUTINE INPUT_LEVELS(NIV,NNIV,NMAX,OLD_BUFFER,EPSILON1)
        REAL*4 NIV(*),BACKGROUND,EPSILON1,WORK
        INTEGER*4 NNIV,NMAX
        INTEGER*4 I1,I2,K1,K2
        CHARACTER BUFFER*80,WORD*80,OLD_BUFFER*80

10      FORMAT(A)

        IF(OLD_BUFFER(1:2).NE.'  ')THEN
          PRINT *,' Current levels are: '
          WRITE(6,10) OLD_BUFFER 
        ENDIF
400     WRITE(6,401)
401	FORMAT(' Three possibilities: discrete levels, log or linear series',/,
     1   ' Enter the discrete levels separated by commas: (Ex: 1.E-3,2.E-2)'
     1   ,/,' Or start/nber_of_steps/step  (for linear series):',
     1   ' (Ex: 1.0/20/0.1)',/,
     1   ' Or start:nber_of_steps:step:sky_back (for log series):',
     1   ' (Ex: 1.0:10:0.1:48.7)  (Type "CR" to keep current levels)')
	READ(5,10) BUFFER 
        IF(BUFFER.EQ.'')BUFFER=OLD_BUFFER

C Decoding BUFFER now:
	K1=INDEX(BUFFER(1:80),'/')
	K2=INDEX(BUFFER(1:80),':')
C Linear series: 
        IF(K1.GT.1)THEN
          CALL INPUT3(NIV,NNIV,NMAX,BUFFER,'/',3,ISTATUS)
          IF(ISTATUS.NE.0) GOTO 400
          STEP=NIV(3)
          DO I=2,NNIV
            NIV(I)=NIV(1)+FLOAT(I-1)*STEP
            PRINT *,' I, NIV ',I,NIV(I)
          END DO
C Log series: 
        ELSEIF(K2.GT.1)THEN
          CALL INPUT3(NIV,NNIV,NMAX,BUFFER,':',4,ISTATUS)
          IF(ISTATUS.NE.0) GOTO 400
          STEP=NIV(3)
          BACKGROUND=NIV(4)
          DO I=2,NNIV
            NIV(I)=NIV(1)+FLOAT(I-1)*STEP
          END DO
C Transformation:
          DO I=1,NNIV
C            WORK=MAX(EPSILON1,NIV(I)-BACKGROUND)
            NIV(I)=BACKGROUND + 10**NIV(I)
            PRINT *,' I, NIV, LOG10(NIV-BACK)',I,NIV(I),LOG10(NIV(I)-BACKGROUND)
          END DO
C Discrete levels:
        ELSE
	 NNIV=0
	 I1=1
408	 I2=80 
	 K1=INDEX(BUFFER(I1:80),',')
	 IF(K1.GT.1) I2=I1-2+K1
C	 PRINT *,' I1, I2',I1,I2
	   IF(I1.LE.I2)THEN
	     WORD=BUFFER(I1:I2)
	     READ(WORD,*,ERR=413) NIV(NNIV+1)
	     NNIV=NNIV+1
	     WRITE(1,40) NNIV,NIV(NNIV) 
40           FORMAT(' Level #',I3,' value: ',G12.5)
C	     PRINT *,' NIV(I)',NIV(NNIV+1)
	     I1=I2+2
	     GOTO 408 
	   ENDIF
C End of cases discrete/linear
        ENDIF

C Save current buffer (to make change easier for the user):
413	 OLD_BUFFER=BUFFER 
         RETURN
         END
C****************************************************************** 
         SUBROUTINE INPUT_BOUNDARIES(IXSTART,IXEND,ISTEPX,IYSTART,IYEND,ISTEPY)
         INTEGER*4 IXSTART,IXEND,ISTEPX,IYSTART,IYEND,ISTEPY
         INTEGER*4 NX1,NY1

300      WRITE(6,3)	
3	  FORMAT(' Enter first pixel, last pixel and step (integers):')
	  READ(5,*) IXSTART,IXEND,ISTEPX
	  IF(IXSTART.GE.IXEND.OR.ISTEPX.LE.0) GO TO 300 
 
400	 WRITE(6,4)
4	  FORMAT(' Enter first, last line and step (integers):')
	  READ(5,*) IYSTART,IYEND,ISTEPY
	  IF(IYSTART.GE.IYEND.OR.ISTEPY.LE.0) GO TO 400 

	 WRITE(1,41) IXSTART,IXEND,ISTEPX,IYSTART,IYEND,ISTEPY
41       FORMAT(' Start, end, step in X: ',3(I5,1X),/,
     1   ' Start, end, step in Y: ',3(I5,1X))

         RETURN
         END
C************************************************************
C
C To input the parameters separated by '/' or '\' 
C 
C Output in NIV(1),NIV(2),NIV(3)
C************************************************************
        SUBROUTINE INPUT3(NIV,NNIV,NMAX,BUFFER,SYMBOL,NVAL,ISTATUS)
        REAL*4 NIV(*)
        INTEGER*4 NNIV,NMAX,NVAL,IVAL
        INTEGER*4 I1,I2,K1,K2
        CHARACTER BUFFER*80,WORD*80,SYMBOL*1

        ISTATUS=0
	IVAL=0
	I1=1
C Main loop:
308	 I2=80 
	 K1=INDEX(BUFFER(I1:80),SYMBOL)
	 IF(K1.GT.1) I2=I1-2+K1
C	 PRINT *,' I1, I2',I1,I2
	 IF(I1.LE.I2)THEN
	   WORD=BUFFER(I1:I2)
	   READ(WORD,*,ERR=309) NIV(IVAL+1)
	   IVAL=IVAL+1
	   WRITE(1,441) IVAL,NIV(IVAL) 
441         FORMAT(' INPUT3/Input #',I3,' value: ',G12.5)
	   I1=I2+2
	   GOTO 308 
	 ENDIF
C
C Should be:  NNIV=3 or 4 and NIV(1)=start, NIV(2)=nber of steps, NIV(3)=step
C and background (if log)
         IF(IVAL.NE.NVAL)THEN
309         PRINT *,' Syntax error: try again' 
            PRINT *,' NIV(1), step, number of levels ',NIV(1),NIV(3),NIV(2)
            IF(SYMBOL(1:1).EQ.':')WRITE(6,34) NIV(4) 
            ISTATUS=-1
            RETURN
         ENDIF
C
         NNIV=INT(NIV(2))
         PRINT *,' NIV(1), step, number of levels ',NIV(1),NIV(3),NIV(2)
         IF(NNIV.GT.NMAX)THEN
             NNIV=NMAX
             PRINT *,' INPUT3/Error, only ',NMAX,' levels allowed'
             PRINT *,' (whereas required number of levels is:',NNIV,')'
             ISTATUS=-1
         ENDIF

         IF(SYMBOL(1:1).EQ.':')WRITE(6,34) NIV(4) 
34       FORMAT(' Sky background to be subtracted to input',
     1           ' data (for display only):',G12.5)

         RETURN
         END
C***************************************************************** 
	include 'jlpsub:isophote_set.for'
	include 'jlpsub:str.for'
        include 'jlpsub:jlp_auto_scale.for'
        include 'jlpsub:auto_sky.for'
