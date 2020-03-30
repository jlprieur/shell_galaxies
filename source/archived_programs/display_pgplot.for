C++*****************************************************************
C DISPLAY_PGPLOT
C To display an image with two symbols
C uses PGPLOT package
C
C Version of 06-04-88
C
C---------------------------------------------------------------------
	PROGRAM DISPLAY_PGPLOT
 
	PARAMETER (KCUR=50,IDIM=600)
 
	REAL*4 XPLOT(IDIM),Z(2),IMAGE(IDIM,IDIM)
	REAL*4 SPG_SIZE(2)
	INTEGER*4 IPG_SYMB(2)
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40
	CHARACTER PLOTDEV*32,ANS*1
	CHARACTER FILENAME*40,COMMENTS*80
	CHARACTER NCHAR(2)*4,CTYPE*80
	LOGICAL JUNK,CONNECTED
 
C Comon block with "NEWPLOT"
	COMMON/PARAMETERS/OFFX,OFFY,AXLEN,AYLEN,XMIN,YMIN,
     1	XMAX,YMAX,DELTAX,DELTAY
	
10	FORMAT(A)
 
C----------------------------------------------------------
C Read the image :
 
	CALL JLP_INQUIFMT
        WRITE(6,*) 'Input file: '
        READ(5,10) FILENAME
	CALL JLP_READIMAG(IMAGE,NX,NY,IDIM,FILENAME,COMMENTS)
		
C----------------------------------------------------------
C Input of the parameters:
 
20	PRINT *,' PGPLOT OUTPUT DEVICE/TYPE (VERSATEC.BIT/VERSA ?)'
	READ(5,10) PLOTDEV
	IF(INDEX(PLOTDEV,'/').EQ.0)THEN
	  PRINT *,' ERROR: ENTER THE DEVICE/TYPE AGAIN'
	  GO TO 20
	ENDIF
	PRINT 21,PLOTDEV
21	FORMAT(' PGPLOT device/type : ',A)
 
	PRINT 22
22	FORMAT(' Note : a special symbol will be drawn if the value',/,
     1	' is smaller than Z1, and another if it is larger than Z2')
	PRINT *,' ENTER THE THRESHOLD VALUES Z1 AND Z2 :'
	READ(5,*) Z(1),Z(2)
 
	PRINT *,' SYMBOL FOR Z1 (82?)'
	READ(5,10) NCHAR(1)
C Decoding the symbol :
	  CALL DECODE_SYMBOL(NCHAR(1),CONNECTED,
     1	ISYMB,SIZE,LTYPE)
	  CALL PGPLOT_SYMBOL(ISYMB,IPG_SYMB(1),SIZE,SPG_SIZE(1))
 
	PRINT *,' SYMBOL FOR Z2 (92?)'
	READ(5,10) NCHAR(2)
C Decoding the symbol :
	  CALL DECODE_SYMBOL(NCHAR(2),CONNECTED,
     1	ISYMB,SIZE,LTYPE)
	  CALL PGPLOT_SYMBOL(ISYMB,IPG_SYMB(2),SIZE,SPG_SIZE(2))
 
	PRINT *,' TITLE ?'
	READ(5,10) CHAR3
	PRINT *,' X LABEL ?'
	READ(5,10) CHAR1
	PRINT *,' Y LABEL ?'
	READ(5,10) CHAR2
 
 
C----------------------------------------------------------
C Plotting now:
 
	CALL PGBEGIN(19,PLOTDEV,1,1)
 
C Set the thickness of the lines (do not affect the Tektronix plots)
	 CALL GRINQTYP(CTYPE,JUNK)
	 IF(CTYPE(1:3).EQ.'VER'.OR.CTYPE(1:3).EQ.'ver')THEN
	   CALL GRSETLW(2)
	   OFFX=1.8
	   OFFY=0.8
	   AXLEN=6.
	 ELSEIF(CTYPE(1:3).EQ.'TEK'.OR.CTYPE(1:3).EQ.'tek')THEN
	   CALL GRSETLW(1)
	   OFFX=1.2
	   OFFY=0.8
	   AXLEN=4.8
	 ELSE
	   CALL GRSETLW(2)
	   OFFX=1.2
	   OFFY=0.8
	   AXLEN=4.8
	 ENDIF
 
C Defines the limits of the window:
	AYLEN=AXLEN*FLOAT(NY)/FLOAT(NX)
	XMIN=1.
	XMAX=FLOAT(NX)
	YMIN=1.
	YMAX=FLOAT(NY)
	PRINT *,' X1,Y1,X2,Y2',XMIN,YMIN,XMAX,YMAX
 
C Set the size of the characters :
	CALL PGSETC(1.25)
 
C Selects the fonts :
C 1=Normal
C 2=Roman
C 3=Italic
C 4=Script
	 CALL GRSETFONT(2)
 
C Set the limits of the graph, in device coordinates :
	X2=OFFX+AXLEN
	Y2=OFFY+AYLEN
	CALL PGVSIZE(OFFX,X2,OFFY,Y2)
 
	CALL PGWINDOW(XMIN,XMAX,YMIN,YMAX)
	CALL PGBOX('BCNST',0.0,0,'BCNSTV',0.0,0)
C
C Arguments for PGMTEXT:
C   Axis (Bottom,Left,...)
C   Distance from the axis (in "hight of character" unit, 0.=centered)
C   Reference along the axis (0.5=middle)
C   Justification / reference (0.5=middle)
C   Label
	CALL PGMTEXT('B',2.3,0.5,0.5,CHAR1)
	CALL PGMTEXT('L',3.,0.5,0.5,CHAR2)
	CALL PGMTEXT('T',1.2,0.02,0.0,CHAR3)
 
C-----------------------------------------------
C Draw the points :
	DO J=1,NY
	  DO I=1,NX
 
	      IF(IMAGE(I,J).LE.Z(1))THEN
	        CALL PGSETC(SPG_SIZE(1))
	        CALL PGPOINT(1,FLOAT(I),FLOAT(J),IPG_SYMB(1))
	      ELSEIF(IMAGE(I,J).GE.Z(2))THEN
	        CALL PGSETC(SPG_SIZE(2))
	        CALL PGPOINT(1,FLOAT(I),FLOAT(J),IPG_SYMB(2))
	      ENDIF
 
	  END DO
	END DO
 
 
C Flushes the SGS buffer to send all graphics to versatec.dat:
	CALL PGEND
 
	END
 
C--------------------------------------------------------------------
C Remember :
C
C NEWPLOT  :
C
C Code of the symbols available : (followed by a number between 1 and 9
C to increase the size, example 46 = big crosses, 42=tiny crosses)
C
C 0 = Histogram
C 1 = Small dot
C 2 = White triangle
C 3 = Black triangle
C 4 = Cross +
C 5 = Cross X
C 6 = White square
C 7 = Black square
C 8 = White circle
C 9 = Black circle
C
C L Solid line
C--------------------------------------------------------------------
C PGPLOT  :
C
C L, L0, or L1 : full line
C L2 dashed line
C L3 dash-dot-dash-dot...
C L4 dotted
C L5 dash-dot-dot-dot
C
C Conventional PGLOT symbols :
C
C 0 open square
C 1 dot
C 2 cross +
C 3 star *
C 4 open circle
C 5 cross x
C 6 open square (identical to 0)
C 7 open triangle, base down
C 8 open circle with a cross + inside
C 9 open circle with a dot inside
C 10 open square squashed from outside
C 11 diamond (losange)
C 12 open star with 5 peaks
C 13 open square (identical to 0)
C 14 open cross + (like the Red Cross)
C 15 open Jewish cross
C 16 filled square
C 17 filled circle
C 18 filled star with 5 peaks
C 19 open square (identical to 0)
C 20 to 27 open circles with increasing size
C 28 arrow to the left
C 29 arrow to the right
C 30 arrow upwards
C 31 arrow downwards
C---------------------------------------------------------------------------
