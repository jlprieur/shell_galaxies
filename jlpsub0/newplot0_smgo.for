C++*****************************************************************
C Set of subroutines using Super Mongo package (Version 1990):
C To draw curves, error bars, etc. To be called by NEWPLOT...
C
C Contains:
C    SMGO_PLOT
C UNIX working version of 04-07-89
C
C JLP
C Version 15-07-94
C--*****************************************************************
C Subroutine SMGO_PLOT using Super Mongo package:
C With possibility of displaying error bars
C---------------------------------------------------------------------
	SUBROUTINE SMGO_PLOT(X,Y,ERRY,NPTS,NMAX,KCURVE,CHAR1,CHAR2,
     1	CHAR3,NCHAR,PLOTDEV,XOUT,YOUT,NOUT,ERROR_BARS)
	REAL*4 X(NMAX,*),Y(NMAX,*),ERRY(NMAX,*)
	REAL*4 XOUT(*),YOUT(*)
	REAL*4 EXPD,EXPD1,SCALE_FACT,ANGL
	REAL*4 OFFX,OFFY,AXLEN,AYLEN,XMIN,XMAX,YMIN,YMAX
	REAL*4 DELTAX,DELTAY	
	INTEGER*4 NPTS(*),NMAX,KCURVE,LTYP
	INTEGER*4 I1,I2,I3,I4
	CHARACTER CHAR1*20,CHAR2*20,CHAR3*40,DATE*24
	CHARACTER PLOTDEV*32
	CHARACTER*4 NCHAR(*)
	LOGICAL CONNECTED,HISTO,ERROR_BARS,FILE
C to delete later...
	LOGICAL FILE1		
 
C Comon block with "NEWPLOT"
	COMMON/PARAMETERS/OFFX,OFFY,AXLEN,AYLEN,XMIN,YMIN,
     1	XMAX,YMAX,DELTAX,DELTAY
	
	WRITE(6,*) ' X1,Y1,X2,Y2',XMIN,YMIN,XMAX,YMAX
 
C Scaling factor for the size of the symbols is 1 by default:
	SCALE_FACT=1.0
C********************************************************
	FILE=PLOTDEV(1:5).NE.'XTERM'
     1	.AND.PLOTDEV(1:5).NE.'xterm'
     1	.AND.PLOTDEV(1:5).NE.'SUNWI'
     1	.AND.PLOTDEV(1:5).NE.'sunwi'
	PRINT 21,PLOTDEV
21	FORMAT(' Graphic device (Smongo) :',A)
 
C********************************************************
C Assign the terminal:
	   CALL DEVICE(PLOTDEV)
	   CALL GRAPHICS
 
C********************************************************
C Now define the parameters for the plot:
 
C Set the thickness of the lines to 2 for laser prints only
	IF(FILE)THEN
	 CALL LWEIGHT(2)
	ELSE
	 CALL LWEIGHT(1)
	 CALL ERASE
	ENDIF
 
C Set the limits of the graph, in device coordinates :
	I1=NINT(OFFX)
	I3=NINT(OFFY)
	I2=NINT(OFFX+AXLEN)
	I4=NINT(OFFY+AYLEN)
	CALL LOCATION(I1,I2,I3,I4)
 
C In user coordinates : X1,Y1,X2,Y2 :
	CALL LIMITS(XMIN,XMAX,YMIN,YMAX)
 
C Make a coordinate box:
C and draws the numbers (with SCALE_FACT as expansion factor)
	CALL EXPAND(SCALE_FACT)
	CALL BOX(1,2,0,0)
 
C********************************************************
C Draw the curves :
	DO KCU=1,KCURVE
 
C LTYPE (Line type = dashed, solid,...)
	CALL DECODE_SYMBOL(NCHAR(KCU),CONNECTED,ISYMB,
     1	SIZE,LTYP)
C Check if "HISTOGRAM" has to be selected
	HISTO=(ISYMB.EQ.0).AND.(.NOT.CONNECTED)
 
	  IF(HISTO)THEN
C Drawing connected points in the histogram mode:
	   CALL LTYPE(0)
	   CALL HISTOGRAM(X(1,KCU),Y(1,KCU),NPTS(KCU))
 
	  ELSEIF(CONNECTED)THEN
C Drawing connected points :
	   CALL LTYPE(LTYP)
	   CALL CONN(X(1,KCU),Y(1,KCU),NPTS(KCU))
 
	  ELSE
C Drawing a symbol at the location of each point :
C First decode the symbol :
	    CALL MONGO_SYMBOL(ISYMB,SIZE,NSIDES,ISTYLE,ANGL,EXPD)
	    EXPD1=EXPD*SCALE_FACT
	    CALL EXPAND(EXPD1)
	    CALL ANGLE(ANGL)
	    CALL LTYPE(0)
C Drawing the chosen symbol at each point :
 	    CALL PTYPE(NSIDES,ISTYLE)
	    CALL POINTS(X(1,KCU),Y(1,KCU),NPTS(KCU))
C Drawing the error bars if needed at each point :
	   IF(ERROR_BARS)THEN
	     CALL ERRORBAR(X(1,KCU),Y(1,KCU),
     1	ERRY(1,KCU),NPTS(KCU),2)
	     CALL ERRORBAR(X(1,KCU),Y(1,KCU),
     1	ERRY(1,KCU),NPTS(KCU),4)
	   ENDIF
	 ENDIF
 
	END DO
 
C********************************************************
C Draw the labels:
C In user coordinates : X1,X2,Y1,Y2 :
	CALL LIMITS(0.,1.,0.,1.)
C User coordinates :
	CALL RELOCATE(0.05,1.05)
	CALL EXPAND(SCALE_FACT)
	CALL ANGLE(0.)
 	CALL LABEL(CHAR3)
	CALL XLABEL(CHAR1)
	CALL YLABEL(CHAR2)
 
C Write the date if laser print :
	IF(FILE)THEN
	  CALL JLP_DATE_TIME(DATE)
	  CALL LWEIGHT(1)
	  EXPD1=0.35*SCALE_FACT
	  CALL EXPAND(EXPD1)
	  CALL RELOCATE(0.8,1.2)
	  CALL LABEL(DATE)
	  CALL EXPAND(SCALE_FACT)
	ENDIF
 
C Check if an interactive terminal was selected :
C Possibility of getting positions with the cursor.
	FILE1=.TRUE.
	IF(.NOT.FILE1)THEN
	  PRINT 89
89	  FORMAT(' Cursor mode: to exit type "return", or enter',
     1	' a point outside of the frame')
C	  CALL CURSOR(0.5,0.5,0)
	  DO I=1,100
C	    CALL CURSOR(XREAD,YREAD,1)
C End if point outside of the frame
	    IF((XREAD.LE.0.).OR.(XREAD.GE.1.)) GOTO 88
	    IF((YREAD.LE.0.).OR.(YREAD.GE.1.)) GOTO 88
C Drawing the chosen symbol at each point :
	    EXPD1=1.5*SCALE_FACT
	    CALL EXPAND(EXPD1)
	    CALL ANGLE(0.)
	    CALL LTYPE(0)
	    CALL RELOCATE(XREAD,YREAD)
	    CALL PTYPE(4,1)
	    CALL POINTS(XREAD,YREAD,1)
 
	    XOUT(I)=XMIN+XREAD*(XMAX-XMIN)
	    YOUT(I)=YMIN+YREAD*(YMAX-YMIN)
C	    PRINT *,' XREAD,YREAD',XREAD,YREAD
C	    PRINT *,' XOUT,YOUT',XOUT(I),YOUT(I)
	  END DO
88	 NOUT=I-1
	ENDIF
 
C Flushes the SGS buffer to send all graphics to the screen or the laser:
	IF(FILE)THEN
	   CALL HARDCOPY
	ENDIF
C Returns to alphanumeric mode:
	CALL ALPHA
 
	RETURN
	END
C--------------------------------------------------------------------
        include 'jlpsub:mongo_symbol.for'
