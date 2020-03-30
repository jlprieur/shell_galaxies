C*****************************************************************
C Set of subroutines including :
C PG_PLOT
C
C Version of 26-11-87
C
C----------------------------------------------------------------------
C Subroutine PG_PLOT using PGPLOT package :
C Similar to MONGO_PLOT or STROMLO_PLOT
C
C---------------------------------------------------------------------
	SUBROUTINE PG_PLOT(X,Y,NPTS,NMAX,KCURVE,
     1	CHAR1,CHAR2,CHAR3,NCHAR,PLOTDEV,XOUT,YOUT,NOUT,PLAN)
 
	REAL*4 X(NMAX,*),Y(NMAX,*)
	REAL*4 XOUT(1),YOUT(1),CURPOS
	REAL*4 XWORK(2),YWORK(2)
	INTEGER*4 NPTS(*)
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40,DATE*24
	CHARACTER PLOTDEV*32,ANS*1
	CHARACTER NCHAR(*)*4,CHAROUT*1,CTYPE*80
	INTEGER*2 ICHAROUT
	EQUIVALENCE (CHAROUT,ICHAROUT)
	LOGICAL CONNECTED,HISTO,JUNK,PLAN
 
C Common block with MONGO routine PLCURSE
	COMMON/PL_CUR/NCPOS,CURPOS(2,100)
C Comon block with "NEWPLOT"
	COMMON/PARAMETERS/OFFX,OFFY,AXLEN,AYLEN,XMIN,YMIN,
     1	XMAX,YMAX,TDX,TDY
	
10	FORMAT(A)
	PRINT *,' X1,Y1,X2,Y2',XMIN,YMIN,XMAX,YMAX
 
	PRINT 21,PLOTDEV
21	FORMAT(' PGPLOT device/type : ',A)
 
C Starting a new graph or turning a page :
C	 IF(ISTARTED.EQ.0)THEN
	   CALL PGBEGIN(19,PLOTDEV,1,1)
C	   ISTARTED=1
C	   SAVE ISTARTED
C	 ELSE
C	   CALL PGADVANCE
C	 ENDIF
 
C Set the thickness of the lines (do not affect the Tektronix plots)
	 CALL GRINQTYP(CTYPE,JUNK)
	 IF(CTYPE(1:3).EQ.'VER'.OR.CTYPE(1:3).EQ.'ver')THEN
	   CALL GRSETLW(2)
	 ELSEIF(CTYPE(1:3).EQ.'TEK'.OR.CTYPE(1:3).EQ.'tek')THEN
	   CALL GRSETLW(1)
	 ELSE
	   CALL GRSETLW(2)
	 ENDIF
 
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
 
C Make a coordinate box in user coordinates :
C When JUST=0 same scale in X and Y :
C	IF(PLAN)THEN
C	  JUST=1
C	ELSE
C	  JUST=0
C	ENDIF
C	CALL PGENV(XMIN,XMAX,YMIN,YMAX,JUST,-2)
 
	CALL PGWINDOW(XMIN,XMAX,YMIN,YMAX)
	CALL PGBOX('BCNST',0.0,0,'BCNSTV',0.0,0)
C	CALL PGLABEL(CHAR1,CHAR2,CHAR3)
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
 
C Draw the curves :
	DO KCU=1,KCURVE
 
C	  DO I=1,NPTS(KCU)
C	   XPLOT(I)=X(I,KCU)
C	   YPLOT(I)=Y(I,KCU)
C	  END DO
 
C Decoding the symbol :
	  CALL DECODE_SYMBOL(NCHAR(KCU),CONNECTED,ISYMB,SIZE,LTYPE)
 
	  IF(CONNECTED)THEN
C Select the line style for subsequent plotting
	     IF(NCHAR(KCU)(2:2).GT.'0'.AND.
     1	NCHAR(KCU)(2:2).LE.'5')THEN
	       READ(NCHAR(KCU)(2:2),108) ISTYLE
108	       FORMAT(I)
	     ELSE
	       ISTYLE=1
	     ENDIF
	    CALL GRSETLS(ISTYLE)
 
C Drawing connected points :
	    CALL PGLINE(NPTS(KCU),X(1,KCU),Y(1,KCU))
 
C Histogram
	  ELSEIF(ISYMB.EQ.0)THEN
	    DO I=1,NPTS(KCU)
	     XWORK(1)=X(I,KCU)
	     XWORK(2)=X(I,KCU)
	     YWORK(1)=0
	     YWORK(2)=Y(I,KCU)
	     CALL PGLINE(2,XWORK,YWORK)
	    END DO
C Drawing a symbol at the location of each point :
	  ELSE
	    CALL PGPLOT_SYMBOL(ISYMB,IPG_SYMB,SIZE,SPG_SIZE)
	    CALL PGSETC(SPG_SIZE)
	    CALL PGPOINT(NPTS(KCU),X(1,KCU),Y(1,KCU),
     1	IPG_SYMB)
	  ENDIF
 
	END DO
 
 
C Check if TEKTRONIX was selected :
C If it was, possibility of getting positions with the cursor.
	 IK1=INDEX(PLOTDEV,'TEK')
	 IK2=INDEX(PLOTDEV,'tek')
	 IK3=MAX(IK1,IK2)
	IF(IK3.NE.0)THEN
	 I=1
94	   CALL PGCURSE(XOUT(I),YOUT(I),CHAROUT)
	   I=I+1
	   IF(ICHAROUT.NE.13) GOTO 94
	 NOUT=I-1
	 PRINT *,' NOUT =',NOUT
C@	 ICHAROUT=ICHAR(CHAROUT)
	ENDIF
 
C Flushes the SGS buffer to send all graphics to versatec.dat:
	CALL PGEND
 
	RETURN
	END
C*********************************************************************
C SUBROUTINE PGPLOT_SYMBOL(ISYMB,IPG_SYMB)
C To convert NEWPLOT code to PGPLOT code
C Input:
C  ISYMB : NEWPLOT code
C Output:
C  IPG_SYMB : PGPLOT code
C*********************************************************************
	SUBROUTINE PGPLOT_SYMBOL(ISYMB,IPG_SYMB,SIZE,SPG_SIZE)
	INTEGER*4 ISYMB,IPG_SYMB,ITRANSFER(9)
C TABLE OF SYMBOLS :
	DATA ITRANSFER/1,7,18,2,5,0,16,4,17/
 
	IF(ISYMB.GE.1.AND.ISYMB.LE.9)THEN
	  IPG_SYMB=ITRANSFER(ISYMB)
	ELSE
	  IPG_SYMB=ISYMB
	ENDIF
 
C Size of the symbol :
 
C Filled symbols are too small with PGPLOT compared to the others:
	    IF(ISYMB.EQ.7.OR.ISYMB.EQ.9)THEN
	      SIZE=2*SIZE
	    ENDIF
C Too large :
C	    SPG_SIZE=0.9+SIZE*10.
 
C Rather good:
	    SPG_SIZE=0.2+SIZE*6.
 
	RETURN
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
