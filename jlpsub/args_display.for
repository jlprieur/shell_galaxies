C++********************************************************************
C Set of subroutines to display curves X,Y on the ARGS
C Contains: ARGS_DISPLAY, ARGS_CROSSJLP, ARGS_CURSORJLP, ARGS_ERASEJLP
C
C From Paul Nulsen's subroutine pell(x0,y0,b,c,r2)
C To link : STARDISK:[STARLINK.LIB]ARGS/LIB/INCLUDE=(BDATA$DATA)
C and STARDISK:[STARLINK.PACK.ASPIC.LIB]ROEARGS/LIB
C Working version of July 16th 1987. 
C
C Version 15-07-94
C JLP
C--********************************************************************
	SUBROUTINE ARGS_DISPLAY(XPLOT,YPLOT,NPTS,
	1	NMAX,KCURVE,NCHAR,COLOUR,NX,NY)
C**********************************************************************
C Subroutine to display KCURVE curves X,Y on the ARGS
C Version of July 16th 1987. JLP.
C From Paul Nulsen's subroutine pell(x0,y0,b,c,r2)
C To link : STARDISK:[STARLINK.LIB]ARGS/LIB/INCLUDE=(BDATA$DATA)
C and STARDISK:[STARLINK.PACK.ASPIC.LIB]ROEARGS/LIB
C
C NB : The centre is shifted one unit towards the origin to allow for
C the origin at 0, 0 on the ARGS (Imposed by the ARGS itself)
C Warning : In this routine, the image (NX,NY) is supposed to be centred
C in 256.,256. by "ADISP" !
C
C   IPLANE:          AVAILABLE COLOURS:
C   11,13,14    	B,R,W  (Y is actually White here !)
C   10,12       	B,R,Y  (W is actually Yellow here !)
C**********************************************************************
	IMPLICIT REAL*8 (A-H,O-Z)
	REAL*4 XPLOT(NMAX,1),YPLOT(NMAX,1)
	INTEGER*4 NPTS(1)
	CHARACTER ANS*1,COL*1,COLOUR(1)*1
	CHARACTER NCHAR1*4,NCHAR(1)*4
 
10	FORMAT(A)
 
C Allocate the ARGS (if .true. reset the ARGS)
	call srinit(0,.false.,istat)
	if(istat.ne.0)then
	  print *,' Failed to get the ARGS, try later'
	  return
	endif
 
C   Enable writing to the args (?)
	call args_allwrt
 
C   This defines a buffer size for args data
	call args_flush(2)
 
C   These calls enable writing and display of the overlay planes
	call args_s1('ZWE1','FFFF'X)
	call args_s1('ZDI1','FFFF'X)
	call args_s1('ZDO1','FFFF'X)
 
C   Flush the buffer
	call srsend
 
C The actual plotting commands (Warning : do not change .5 in the
C following !)
 
	DO KCU=1,KCURVE
	  NCHAR1=NCHAR(KCU)
	  COL=COLOUR(KCU)
	  IF(COL(1:1).EQ.'B'.OR.COL(1:1).EQ.'b')IPLANE=10
	  IF(COL(1:1).EQ.'W'.OR.COL(1:1).EQ.'w')IPLANE=11
	  IF(COL(1:1).EQ.'R'.OR.COL(1:1).EQ.'r')IPLANE=13
	  IF(COL(1:1).EQ.'Y'.OR.COL(1:1).EQ.'y')IPLANE=12
C	  IF(COL(1:1).EQ.'C'.OR.COL(1:1).EQ.'c')IPLANE=12
C	  IF(COL(1:1).EQ.'M'.OR.COL(1:1).EQ.'m')IPLANE=11
C	  IF(COL(1:1).EQ.'G'.OR.COL(1:1).EQ.'g')IPLANE=14
C   Define the plane to be used
	  call args_ovwrt(iplane)
 
C Define the colour to be used
	  call args_ovcol(iplane,col)
	  call args_ovgen('W')
 
C (NX, NY is the size of the displayed image)
	IXSTART=255-(NX+1)/2
	IYSTART=255-(NY+1)/2
 
C Drawing a solid line :
	  IF(NCHAR1(1:1).LT.'0'.OR.NCHAR1(1:1).GT.'9')THEN
	    IX=IXSTART+NINT(XPLOT(1,KCU))	
	    IY=IYSTART+NINT(YPLOT(1,KCU))
	    CALL ARGS_S1('XMA',IX)
	    CALL ARGS_S1('YMA',IY)
	      DO I=2,NPTS(KCU)
	       IX=IXSTART+NINT(XPLOT(I,KCU))
	       IY=IYSTART+NINT(YPLOT(I,KCU))
	       CALL ARGS_S1('XMA',IX)
	       CALL ARGS_S1('YDA',IY)
	      END DO
	  ELSE
C Drawing crosses :
	    ISIZE=4
	    DO I=1,NPTS(KCU)
	       IX=IXSTART+NINT(XPLOT(I,KCU))
	       IY=IYSTART+NINT(YPLOT(I,KCU))
	       CALL ARGS_CROSSJLP(IX,IY,ISIZE)
	    END DO
	  ENDIF
	END DO
 
C   Flush the buffer
	call srsend
 
C   Clear the overlay plane if required
	PRINT *,' DO YOU WANT TO CLEAR THE OVERLAY PLANES ? (N)'
	READ(5,10) ANS
	 IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
	 DO IPL=8,15
	 CALL ARGS_OVCLR(IPL)
	 END DO
	 ENDIF
 
	RETURN
	END
C**********************************************************************
C Subroutine to draw a cross on the ARGS at a given position IX,IY
C**********************************************************************
	SUBROUTINE ARGS_CROSSJLP(IX,IY,ISIZE)
	IS2=ISIZE/2
	IX1=IX-IS2
	IY1=IY-IS2
	IX2=IX+IS2
	IY2=IY+IS2
	CALL ARGS_S1('XMA',IX1)
	CALL ARGS_S1('YMA',IY1)
	CALL ARGS_S1('XMA',IX2)
	CALL ARGS_S1('YDA',IY2)
	IX1=IX-IS2
	IY1=IY+IS2
	IX2=IX+IS2
	IY2=IY-IS2
	CALL ARGS_S1('XMA',IX1)
	CALL ARGS_S1('YMA',IY1)
	CALL ARGS_S1('XMA',IX2)
	CALL ARGS_S1('YDA',IY2)
	RETURN
	END
C*************************************************************
C Subroutine to get some positions IOUTX, IOUTY on the ARGS
C
C Input :
C NX, NY : Size of the displayed image
C Output:
C IOUTX(NPOINT), IOUTY(NPOINT), NPOINT
C*************************************************************
	SUBROUTINE ARGS_CURSORJLP(IOUTX,IOUTY,NPOINT,NX,NY)
	INTEGER*4 IOUTX(1),IOUTY(1)
	CHARACTER TEXT*72
 
C Allocate the ARGS :
	CALL SRINIT(0,.FALSE.,ISTAT)
	IF(ISTAT.NE.0)THEN
	PRINT *,' ARGS NOT AVAILABLE'
	RETURN
	ENDIF
 
C Enable only sytem cursor :
	CALL ARGS_CURS('+')
 
C Centre cursor :
	CALL ARGS_CURP(0,255,255)
 
C Set cursor colour :
	CALL ARGS_CURC('W')
 
C Load trackballer/cursor program into the ARGS
	CALL ARGS_TBCL(0)
 
C Switch on lamps
	CALL ARGS_LAMPS(1,0,0,1)
 
C   Define the plane to be used
	  call args_ovwrt(14)
 
C Define the colour to be used
	  call args_ovcol(14,'R')
	  call args_ovgen('W')
 
C******* Entering the points ******************
C Loop until button 4 is pressed :
 
C (NX, NY is the size of the displayed image)
	IXSTART=255-(NX+1)/2
	IYSTART=255-(NY+1)/2
	IB4=0
	NPOINT=0
94	IF (IB4.NE.0) GOTO 95
	CALL ARGS_TBCX(IX,IY,IB1,IB2,IB3,IB4)
 
C If button 1 pressed display X, Y
	 IF(IB1.NE.0)THEN
	  NPOINT=NPOINT+1
	  IOUTX(NPOINT)=IX-IXSTART
	  IOUTY(NPOINT)=IY-IYSTART
	  CALL ARGS_CROSSJLP(IX,IY,4)
	  PRINT *,' Index, Position X, Y :',
	1	NPOINT,IOUTX(NPOINT),IOUTY(NPOINT)
	 ENDIF
	 GOTO 94
	
C Switch off lamps
95	CALL ARGS_LAMPS(0,0,0,0)
 
C Disable cursor
	CALL ARGS_CURS('0')
	CALL SRSEND
 
C Exit
	RETURN
	END
 
C******************************************************************
C Subroutine to erase the overlay planes :
C******************************************************************
	SUBROUTINE ARGS_ERASEJLP
	
C Allocate the ARGS (if .true. reset the ARGS)
	call srinit(0,.false.,istat)
	if(istat.ne.0)then
	  print *,' Failed to get the ARGS '
	  return
	endif
 
	 DO IPL=8,15
	 CALL ARGS_OVCLR(IPL)
	 END DO
 
	RETURN
	END
