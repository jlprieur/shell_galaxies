C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Program   XYCURA1
C
C  CALLING SEQUENCE:-   XYCURA  [ILEVEL= MARKER= COLOUR=
C                                        IDENTIFY= ]
C
C JLP
C Version of 24-04-90
C
C  FUNCTION:-
C
C             Make a list of X,Y positions by using a cursor to define the
C             positions on an image displayed on the ARGS. The positions can
C             have identifiers given to them.
C
C             Also to display on such an image the X,Y positions of an
C             optional input list.
C             If this is done and the cursor is used to define more
C             positions, these are added to the input list instead of a
C             new list being made.
C             These positions can be displayed as crosses; as ticks
C             displaced alternately by (-2,0) and (2,0); as 1 pixel
C             spots.
C             Of the input list, only those positions inside the area of
C             the image displayed on the ARGS will be put up.
C
C             In this new version we stop entering points by pressing
C             twice on the red button.
C             Pressing once prints the position -1000.,-1000.
C
C  USE:-
C             First an image must be on the ARGS (by ICDISP,ADISP,etc (qv)).
C               You can clear any existing crosses displayed on the image,
C             and then choose wether to have an input X,Y list.
C             If you do, then you choose wether to display the input
C             positions as blue crosses and wether to display their numbers
C             in the list.
C               Then input the new positions by means of the cursor.
C             The positions are then marked with red crosses.
C             If you have chosen to add identifiers you enter them on the
C             keyboard after each use of the cursor. (The choice is made by
C             setting IDENTIFY=TRUE on runnung the program.)
C             To exit input a position at or beyond the bottom left hand
C             corner of the image.
C               The positions are then output to the X,Y list.
C
C----------------------------------------------------------------------
C  USER PARAMETERS:-
C
C         OVERCL          NO                  Flag for clearing the crosses
C                                             left  on  the  ARGS  from any
C                                             previous XYCURA  Choices  are
C                                             NO,YES
C
C         INPUT                               The optional input X,Y list
C                                             Return null if there is none.
C
C         FXOUT           YES                  Flag for putting  up  on  the
C                                             ARGS  the  crosses  from  any
C                                             input list. Choices are
C                                             YES,NO.
C
C         NUMINP          NO                  Flag for adding the number of
C                                             the cross of the input list.
C
C         OUTPUT                              The  Output  file  containing
C                                             the positions
C
C         IDENTITY                            If inputting star names, this
C                                             is the name  of  the star.
C
C
C  NORMALLY DEFAULTED PARAMETERS:-
C
C         ILEVEL          2                   Flag for  outputting  on  the
C                                             terminal  the  positions from
C                                             the cursor as you go along.The
C                                             default  (=2) makes it happen
C                                             automatically. Setting it  to
C                                             1   on  running  the  program
C                                             supresses it.
C
C         MARKER          CROSS               The type of mark made at the
C                                             positions of the input XYlist.
C                                             Choices are crosses; ticks at
C                                             (-2,0) and (2,0) alternately;
C                                             spots.
C                                             Choices are CROSS,ATICK,SPOT
C
C         COLOUR          B                   The colour of the cross put
C                                             on the input list positions.
C                                             Choices are;- White,Red,Blue
C                                             Yellow,Cyan,Magenta,Green.
C                                             (use only 1st letter)
C
C         IDENTIFY        FALSE               Flag for choosing  wether  to
C                                             put  in via keyboard the name
C                                             of  the  star   at   position
C                                             chosen  via  the  cursor.  To
C                                             have    this    option    put
C                                             IDENTIFY=TRUE    wh   running
C                                             XYCURA.  If  not  chosen  the
C                                             stars    are    labelled   as
C                                             sequential numbe (#n).
C
C  USE OF TRACKER-BALL BUTTONS
C
C       GREEN 1     Inputs present position of cursor as star position.
C
C       WHITE 2     Decreases magnification by times 2
C
C       WHITE 3     Increases magnification by times 2
C
C       RED   4     End of a set of positions
C                   Exit when pressed twice without pressing on GREEN 1.
C
C         A J Penny                RGO
C
C*****************************************************************************
*CALLS
*	THIS PACKAGE:
*		LBGONE
*	STARLINK:
*		RDKEYC
*WRITTEN BY
*	R.F. WARREN-SMITH
*ADAPTED BY
*     A.J. PENNY, JLP
*----------------------------------------------------------------------
	PROGRAM XYCURA1
	PARAMETER (IDIM1=2000)
	REAL*4 XA(IDIM1),YA(IDIM1)
	CHARACTER IDENTA(IDIM1)*20
	CHARACTER LISTIN*30,LISTOUT*30
	CHARACTER CVAL*1,TITLE*30,PRBUF*40
	CHARACTER TEXT*72,TCOL*1
	LOGICAL IDENT
 
10	FORMAT(A)
 
C
C OBTAIN INTERACTION LEVEL
C
      ILEVEL=2
C
C  Get input XYlist mark option
C
    1 KOPT = 1
      CALL GETCMD('MARKER','CROSS,ATICK,SPOT,HELP,?.',.TRUE.,KOPT,
     +            TEXT,KTEXT,ISTAT)
      IF (KOPT.GE.4) THEN
         PRINT *,'Input X,Y list mark types'
         PRINT *,'Choices are :-'
         PRINT *,'CROSS    Cross'
         PRINT *,'ATICK    Ticks at (-2:0) and (2:0) alternately'
         PRINT *,'SPOT     1 pixel spots'
         PRINT *,'MARKER',ISTAT)
         GO TO 1
      ENDIF
C
C  Get colour of input spots
C
    2 KCOL = 1
      CALL GETCMD('COLOUR','B,W,R,Y,C,M,G,HELP,?.',.TRUE.,KCOL,
     +            TEXT,KTEXT,ISTAT)
      IF (KCOL.GE.8) THEN
         PRINT *,'Colour to paint input list markers'
         PRINT *,'Choices are :-'
         PRINT *,'White,Red,Blue,Yellow,Cyan,Magenta,Green'
         PRINT *,'(type only the first letter'
         CALL CNPAR('COLOUR',ISTAT)
         GO TO 2
      ENDIF
      IF (KCOL.EQ.1) TCOL = 'B'
      IF (KCOL.EQ.2) TCOL = 'W'
      IF (KCOL.EQ.3) TCOL = 'R'
      IF (KCOL.EQ.4) TCOL = 'Y'
      IF (KCOL.EQ.5) TCOL = 'C'
      IF (KCOL.EQ.6) TCOL = 'M'
      IF (KCOL.EQ.7) TCOL = 'G'
C
C  Get wether to clear all overlay planes before writing
C
    3 KOVCL = 2
      CALL GETCMD('OVERCL','YES,NO,HELP,?.',.TRUE.,KOVCL,
     +            TEXT,KTEXT,ISTAT)
      IF (KOVCL.GE.3) THEN
         PRINT *,'Choices are :-'
         PRINT *,'YES      Clear overlay planes'
         PRINT *,'NO       Leave planes as they are'
         CALL CNPAR('OVERCL',ISTAT)
         GO TO 3
      ENDIF
C
C DETERMINE IF IDENTIFIERS ARE TO BE PROMPTED FOR
C
      IDENT=.FALSE.
      CALL RDKEYL('IDENTIFY',.TRUE.,1,IDENT,NVAL,ISTAT)
C
C OBTAIN AN OPTIONAL INPUT DATA FRAME
C
	PRINT *,' DO YOU WANT TO EXTEND AN EXISTING LIST ? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
C Reading the input list :
11	  PRINT *,' NAME OF THE INPUT LIST ?'
	  READ(5,10) LISTIN
	  OPEN(1,FILE=LISTIN,STATUS='OLD',ERR=11)
 
	   IF(IDENT)THEN
	     DO I=1,IDIM1
	      READ(1,*,END=12)XA(I),YA(I),IDENTA(I)
	     END DO
	   ELSE
	     DO I=1,IDIM1
	      READ(1,*,END=12)XA(I),YA(I)
	     END DO
	   ENDIF
 
12	  LSTLEN=I-1
	  PRINT *,LSTLEN,' POINTS RECEIVED'
	  CLOSE(1)
 
C
C DETERMINE IF INPUT LIST CROSSES TO BE PUT UP
C
         PRINT *,'DISPLAY INPUT LIST POSITIONS ?'
    4    KFXOUT = 1
         CALL GETCMD('FXOUT','YES,NO,HELP,?.',.TRUE.,KFXOUT,
     +               TEXT,KTEXT,ISTAT)
         IF (KFXOUT.GE.3) THEN
            PRINT *,'Choices are :-'
            PRINT *,'YES     Positions are displayed'
            PRINT *,'NO      Positions not displayed'
            CALL CNPAR('FXOUT',ISTAT)
            GO TO 4
         ENDIF
         IF (KFXOUT.EQ.1) THEN
            PRINT *,'NUMBER THEM ?'
            NUMINP = 2
            CALL GETCMD('NUMINP','NO,YES.',1,NUMINP,PRBUF,NVAL,
     +                  ISTAT)
         ENDIF
 
	ELSE
 
C LSTLEN=NO OF LIST RECORDS
          LSTLEN=0
 
      ENDIF
C
C  Do the input list (if any) display and get the new positions
C
	LENBEG = LSTLEN
	MAXLEN=IDIM1
	CALL XYINCA(IDENTA,XA,YA,MAXLEN,LSTLEN,IDENT,ILEVEL,
     1	IERR,KOVCL,KFXOUT,NUMINP,KOPT,TCOL)
C
C IF NO LIST OBTAINED, GIVE ERROR MESSAGE, OTHERWISE
C OBTAIN OUTPUT DATA FRAME
C
	IF(LSTLEN.LE.0.OR.LSTLEN.EQ.LENBEG) THEN
	    WRITE(6,*) 'FATAL ERROR: No list'
	    STOP
	ELSE
13	    PRINT *,' Output list ?'
	    READ(5,10) LISTOUT
	    OPEN(2,FILE=LISTOUT,STATUS='NEW',ERR=13)
C
C OUTPUT FRAME SUCCESSFULLY OBTAINED. COPY LISTS TO
C OUTPUT DATA FRAME
C
	    IF(IDENT)THEN
	      DO I=1,LSTLEN
	        WRITE(2,*),XA(I),YA(I),IDENTA(I)
	      END DO
	    ELSE
	      DO I=1,LSTLEN
	        WRITE(2,*),XA(I),YA(I)
	      END DO
	    ENDIF
	  CLOSE(2)
C
C TELL USER HOW MANY ENTRIES IN OUTPUT LIST
C
	  IF(ILEVEL.GE.2) THEN
            WRITE(PRBUF,104)LSTLEN
	    IF(LSTLEN.EQ.1) PRBUF(28:)='ENTRY'
  104       FORMAT('   OUTPUT LIST HAS',I6,' ENTRIES')
	    WRITE(6,100) PRBUF
100	    FORMAT(A)
	  ENDIF
 
	ENDIF
 
C FREE ALL DATA AREAS AND EXIT
 
99	CALL JLP_END
	STOP
	END
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R XYINCA *
C      *            *
C      **************
C
C
* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO INTERACTIVELY OBTAIN A SET OF X,Y POSITIONS AND ATTACHED
*	CHARACTER IDENTIFIERS FROM THE ARGS AND INSERT THEM IN
*	A LIST OF POSITIONS
*     ALSO, IF REQUIRED, TO PUT UP AS CROSSES ANY POSITIONS ALREADY
*     IN THE LIST
*
*METHOD
*       IF WANTED, GET ALL POSNS FROM INPUT LIST AND PUT ON ARGS
*       SCREEN USING S/R CROSS TAKEN FROM PRGM SLICE
*       OBTAIN X,Y POSITION FROM THE ARGS SCREEN USING ARGS_CURSJLP AND
*       IDENTIFIER (IF REQUIRED) FROM KEYBOARD USING STARLINK
*	PARAMETER 'IDENTITY'.
*	IF THE IDENTIFIER IS BLANK, CREATE ONE USING THE CURRENT
*	COUNT OF BLANK IDENTIFIERS ENTERED. IF THE IDENTIFIER IS IN THE
*	FORM #N, RESET THE BLANK COUNTER TO N. OTHERWISE USE THE
*	IDENTIFIER AS IT STANDS AND ADD IT TO THE LIST.
*
*ARGUMENTS
*	ID (IN/OUT)
*	CHARACTER (MAXLEN)*20
*		A LIST OF IDENTIFIERS
*	X,Y (IN/OUT)
*	REAL(MAXLEN)
*		LISTS OF X,Y POSITIONS
*	MAXLEN (IN)
*	INTEGER
*		THE MAXIMUM NUMBER OF ENTRIES WHICH CAN BE HELD IN THE
*		LISTS
*	LEN (IN/OUT)
*	INTEGER
*		ON ENTRY, GIVES THE NUMBER OF ENTRIES ALREADY IN THE
*		LISTS ID,X AND Y. ON EXIT, GIVES THE NUMBER OF ENTRIES
*		IN THE OUTPUT LISTS.
*       IDENT (IN)
*       LOGICAL
*               IF TRUE, IDENTIFIERS ARE PROMPTED FOR
*       ILEVEL (IN)
*       INTEGER
*               INTERACTION LEVEL: CONTROLS PRINTING OF POSITIONS ON
*               SCREEN AS THEY ARE OBTAINED
*	IERR (OUT)
*	INTEGER
*		ERROR FLAG: ZERO FOR SUCCESS
*		1: LEN .GT. MAXLEN ON ENTRY
*
*     KOVCL (IN)
*     INTEGER
*             FLAG FOR CLEARING OVERLAY PLANE ON ENTRY
*     KFXOUT (IN)
*     INTEGER
*             FLAG FOR PUTTING UP CROSSES FROM INPUT LIST (1=YES)
*     NUMINP
*     INTEGER
*             FLAG FOR NUMBERING INPUT LIST CROSSES (1=NO)
*     COMFAC (IN)
*     REAL
*             COMPRESSION FACTOR OF DISPLAY
*     DX     (IN)
*     REAL
*             DISPLAY X START COORDINATE
*     DY     (IN)
*     REAL
*             DISPLAY Y START COORDINATE
*
*     ISX     Int       X width of displayed image
*     ISY     Int       Y width of displayed image
*
*    Input
*     KOPT    Int       flag for XYlist input mark type 1=+,2=tick,3=spot
*     TCOL    Char*1    Colour to paint input markers
*
*STARLINK PARAMETERS
*       IDENTITY
*               CHARACTER IDENTIFIER FOR POSITIONS
*
*CALLS
*	EDRS PACKAGE:
*		LBGONE,GETCMD
*	STARLINK:
*		RDKEYC,CNPAR,CTOR,CTOI
*     ASPIC:
*             ARGS_CLS,ARGS_NUMIM,ARGS_OVCL
*     THIS FILE:
*             CROSS,ATICK,SPOT,ARGS_OVOPN
*
*NOTES
*	USES BYTE ARRAYS
*
*WRITTEN BY
*	R.F. WARREN-SMITH
*ADAPTED BY
*     A.J. PENNY
* ----------------------------------------------------------------------
      SUBROUTINE XYINCA(ID,X,Y,MAXLEN,LEN,IDENT,ILEVEL,
     1	ISTATUS,KOVCL,KFXOUT,NUMINP,KOPT,TCOL)
 
      CHARACTER IDBUF*20,INBUF*80,PRBUF*80,TCOL*1,TEXT*72,VALUE*80
      LOGICAL IDENT,FINISH
      REAL X(MAXLEN),Y(MAXLEN)
	CHARACTER ID(MAXLEN)*20
C
C CHECK ARGUMENTS
C
	IF(MAXLEN.LT.LEN) THEN
	 ISTATUS=1
	 RETURN
	ENDIF
 
	ISTATUS=0
C
C CHECK LENGTH OF LIST IS NOT -VE, INITIALLISE BLANK IDENTIFIER
C COUNT TO AFTER INPUT LIST
C
      LEN=MAX(0,LEN)
      NBLANK= LEN + 1
 
C
C  Allocate the ARGS
C
	ISTAT=0
	CALL SRINIT(0,.FALSE.,ISTAT)
	IF (ISTAT.NE.0) THEN
	 WRITE(6,*) ' FATAL ERROR: ARGS not available'
	 STOP
	ENDIF
 
C Check if an image has been displayed :
	CALL ARGS_NUMIM(IDMAX)
	  IF(IDMAX.EQ.0)THEN
	    WRITE(6,*) ' No image has been displayed'
	    ISTATUS=1
	    RETURN
	  ENDIF
	PRINT *,IDMAX,' IMAGE(S) DISPLAYED'
 
C Reading the data base :
	CALL ARGS_RDIM(IXPOS,IYPOS,ISX,ISY,I,I,ISTAT)
	 IF(ISTAT.EQ.0)THEN
	   PRINT *,' SIZE   SX, SY :',ISX,ISY
	 ELSE
	   ISX=320
	   ISY=512
	 ENDIF
	CALL ARGS_RDPAR('COMPRE',1,TEXT,NVALS,ISTAT)
	 IF(ISTAT.EQ.0)THEN
	   READ(TEXT,900)KXB,KXE,KYB,KYE,KCOMP
900	   FORMAT(5I10)
	   COMFAC=REAL(KCOMP)
	   DX=REAL(KXB)
	   DY=REAL(KYB)
	   PRINT *,' COMFAC, DX, DY ',COMFAC,DX,DY
	 ELSE
C Assuming that the image has not been compressed :
           COMFAC = 1.0
C and that the indices start at 1,1
           DX = 1.0
           DY = 1.0
	 ENDIF
C
C  Clear ARGS overlay planes if wanted
C
      IF (KOVCL.EQ.1) THEN
         DO L = 8,15
            CALL ARGS_CLS(L)
         END DO
      ENDIF
C
C WRITE CROSSES FROM INPUT LIST POSNS IF REQUIRED
C
      IF (KFXOUT.EQ.1.AND.LEN.GT.0) THEN
         IF (TCOL.EQ.'B') NPLANE = 8
         IF (TCOL.EQ.'W') NPLANE = 9
         IF (TCOL.EQ.'R') NPLANE = 10
         IF (TCOL.EQ.'Y') NPLANE = 11
         IF (TCOL.EQ.'C') NPLANE = 12
         IF (TCOL.EQ.'M') NPLANE = 13
         IF (TCOL.EQ.'G') NPLANE = 14
         CALL ARGS_OVOPN(NPLANE,TCOL)
         CALL ARGS_NUMIM(IDMAX)
         DO K = 1,LEN
            KX = INT(X(K))
            KY = INT(Y(K))
            XA = (REAL(KX)-DX)/COMFAC
            YA = (REAL(KY)-DY)/COMFAC
            IF (XA.GE.1.0.AND.YA.GE.1.0.AND.
     +          XA.LE.REAL(ISX).AND.YA.LE.REAL(ISY)) THEN
               NUM = K
               IF (NUMINP.EQ.1) NUM = -1
               KN = -1
               IF (MOD(K,2).EQ.0) KN = 1
	        IF (KOPT.EQ.2) THEN
	         CALL ATICK(IDMAX,XA,YA,KN,NUM,TCOL)
	        ELSEIF(KOPT.EQ.3) THEN
	         CALL SPOT(IDMAX,XA,YA,NUM,TCOL)
                ELSE
	         CALL CROSS(IDMAX,XA,YA,NUM,TCOL)
	        ENDIF
            ENDIF
         END DO
      CALL ARGS_OVCL(NPLANE,.FALSE.)
      ENDIF
C
C
C  Get positions from ARGS using cursor and marking positions
C LOOP WHILE FINISH HAS NOT BEEN FOUND/SET AND LIST HAS NOT OVERFLOWED
C ------------------------------------------------------------
C
      CALL ARGS_OVOPN(10,'R')
      CALL ARGS_NUMIM(IDMAX)
	FINISH=.FALSE.
   67 CONTINUE
      IF((.NOT.FINISH).AND.(LEN.LE.MAXLEN)) THEN
C
C CALL ARGS_CURSJLP TO GET COORDINATES FROM ARGS SCREEN
C AND DRAW A CROSS AT THAT POSITION
C
C          CALL ASP_PAN(IX,IY,XA,YA)
	CALL ARGS_CURSJLP(XA,YA,ISTAT)
C
	IF(ISTAT.EQ.0)THEN
          IF (KOPT.EQ.1.OR.KOPT.EQ.2) CALL CROSS(IDMAX,XA,YA,-1,'R')
          IF (KOPT.EQ.3) CALL SPOT(IDMAX,XA,YA,-1,'R')
          X(LEN+1) = COMFAC*XA + DX
          Y(LEN+1) = COMFAC*YA + DY
	ELSEIF(ISTAT.EQ.2)THEN
	  X(LEN+1)=-1000.
	  Y(LEN+1)=-1000.
	ENDIF
 
C TEST IF USER WANTS TO STOP
C
	  KLAST=MAX(1,LEN)
          FINISH=((X(KLAST).EQ.-1000.).AND.(Y(KLAST).EQ.-1000.)
     1	.AND.(X(LEN+1).EQ.-1000.).AND.(Y(LEN+1).EQ.-1000.))
C
C IF NOT, PRINT POSITION IF REQUIRED
C
          IF(.NOT.FINISH) THEN
            IF(ILEVEL.GE.2) THEN
              KN = LEN + 1
C              KXN = INT(X(KN)+0.01)
C              KYN = INT(Y(KN)+0.01)
C              WRITE(6,64) KN,KXN,KYN
C   64         FORMAT(' ',I5,5X,2I7)
	       PRINT *,KN,X(KN),Y(KN)
            ENDIF
C
C IF IDENTIFIER REQUIRED, OBTAIN FROM ENVIRONMENT
C
          INBUF=' '
            IF(IDENT) THEN
	      CALL RDKEYC('IDENTITY',.FALSE.,1,INBUF,NVAL,ISTAT)
              CALL CNPAR('IDENTITY',ISTAT)
            ENDIF
            IDBUF=INBUF
          ENDIF
 
	  IF(.NOT.FINISH)THEN
C
C TEST IF LIST OF INPUT HAS OVERFLOWED
C
              IF(LEN.GE.MAXLEN) THEN
		FINISH=.TRUE.
	      ELSE
C
C INCREMENT LIST LENGTH IF IT WILL NOT OVERFLOW
C
		LEN=LEN+1
C
C TREAT THE SPECIAL CASES OF BLANK IDENTIFIER OR '#N'
C ----------------------------------------------------
C
C REMOVE LEADING BLANKS FROM IDENTIFIER AND TEST IF ALL BLANK
C
		CALL LBGONE(IDBUF)
		IF(IDBUF.EQ.' ') THEN
C
C IF BLANK, GENERATE AN IDENTIFIER FROM THE BLANK COUNT IN THE FORM
C '#N' AND INCREMENT THE BLANK COUNT
C
		  WRITE(IDBUF,'(I20)')NBLANK
		  IDBUF(1:1)='#'
	          CALL LBGONE(IDBUF(2:))
		  NBLANK=NBLANK+1
C
C IF ID STARTS WITH # THEN SEE IF IT IS FOLLOWED BY AN INTEGER
C IF SO, RESET NBLANK AND PUT ID IN #N STANDARD FORM
C RESET NBLANK SO THAT SUBSEQUENT BLANK IDENTIFIERS ARE CONVERTED TO
C SEQUENTIALLY NUMBERED '#N' FORM
C
		ELSE IF(IDBUF(1:1).EQ.'#') THEN
		  CALL CTOI(IDBUF(2:),NB,ISTATB)
		  IF(ISTATB.EQ.0) THEN
		    NBLANK=NB+1
		    WRITE(IDBUF,'(I20)')NB
		    IDBUF(1:1)='#'
		    CALL LBGONE(IDBUF(2:))
		  ENDIF
		ENDIF
C
C PUT ID INTO IDENTIFIER LIST
C
		  ID(LEN)=IDBUF(1:20)
	      ENDIF
	    ENDIF
C
C IF LIST IS FULL, RETURN
C
	    IF(LEN.GE.MAXLEN) THEN
	      FINISH=.TRUE.
	    ENDIF
	    GO TO 67
	  ENDIF
      CALL ARGS_OVCL(10,.FALSE.)
C
C REPOSITION IMAGE AT CENTRE OF ARGS
C
	CALL ARCEMA(255,255,1,1,1)
 
	END
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE CROSS (ID,X,Y,NUM)
C
C DRAW A CROSS ON ARGS AT POSITION (X,Y) (USER UNITS) OF WIDTH 5
C PIXELS.
C ID = IDENT. NUMBER OF THE IMAGE (IF MORE THAN ONE ON THE SCREEN)
C  IF NUM IS GREATER THAN 0, THE CROSS IS NUMBERED AS NUM.
C
      INTEGER PX,PY,STATUS
      CHARACTER*72 TEXT
      REAL X,Y,UX,UY,DX,DY,XVAL(2),YVAL(2)
      CALL ARGS_UTOP (ID,X,Y,PX,PY,STATUS)
      CALL ARGS_PTOU (ID,PX+2,PY+2,UX,UY,STATUS)
      DX = UX - X
      DY = UY - Y
      XVAL(1) = X - DX
      YVAL(1) = Y
      XVAL(2) = X + DX
      YVAL(2) = Y
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
      XVAL(1) = X
      YVAL(1) = Y - DY
      XVAL(2) = X
      YVAL(2) = Y + DY
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
C
      IF (NUM.GT.0) THEN
         DO K = 1,72
            TEXT(K:K) = ' '
         ENDDO
         WRITE(TEXT,900)NUM
  900    FORMAT(I10)
         KSHIFT = 0
         DO K = 1,9
            IF(TEXT(K:K).EQ.' ') KSHIFT = K
         ENDDO
         DO K = 1,10
            J = K + KSHIFT
            TEXT(K:K) = TEXT(J:J)
         ENDDO
         CALL ARGS_UTOP(ID,X,Y,PX,PY,STATUS)
         CALL ARGS_PTOU(ID,PX+3,PY+3,UXA,UYA,STATUS)
         KX = UXA
         KY = UYA
C         CALL ARGTXT(TEXT,'B',0,KX,KY,0)
      ENDIF
C
      END
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE SPOT (ID,X,Y,NUM)
C
C DRAW A SPOT ON ARGS AT POSITION (X,Y) (USER UNITS) OF WIDTH 1
C PIXELS. (ID IS ID OF IMAGE)
C  IF NUM IS GREATER THAN 0, THE CROSS IS NUMBERED AS NUM.
C
      INTEGER PX,PY,STATUS
      CHARACTER*72 TEXT
      REAL X,Y,UX,UY,DX,DY,XVAL(2),YVAL(2)
      XVAL(1) = INT(X)
      YVAL(1) = INT(Y)
      XVAL(2) = INT(X)
      YVAL(2) = INT(Y)
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
C
      IF (NUM.GT.0) THEN
         DO K = 1,72
            TEXT(K:K) = ' '
         ENDDO
         WRITE(TEXT,900)NUM
  900    FORMAT(I10)
         KSHIFT = 0
         DO K = 1,9
            IF(TEXT(K:K).EQ.' ') KSHIFT = K
         ENDDO
         DO K = 1,10
            J = K + KSHIFT
            TEXT(K:K) = TEXT(J:J)
         ENDDO
         CALL ARGS_UTOP(ID,X,Y,PX,PY,STATUS)
         CALL ARGS_PTOU(ID,PX+3,PY+3,UXA,UYA,STATUS)
         KX = UXA
         KY = UYA
C         CALL ARGTXT(TEXT,'B',0,KX,KY,0)
      ENDIF
C
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE ATICK (ID,X,Y,KN,NUM)
C
C DRAW A TICK ON ARGS AT POSITION (X+KN*2,Y) (USER UNITS)
C  (ID IS ID OF IMAGE) AND NUMBERS IT IF NUM IS +VE.
C
      INTEGER PX,PY,STATUS
      CHARACTER*72 TEXT
      REAL X,Y,UX,UY,DX,DY,XVAL(2),YVAL(2)
      KX = KN*2
      KXA = KN*3
      KY = KN
      CALL ARGS_UTOP (ID,X,Y,PX,PY,STATUS)
      CALL ARGS_PTOU (ID,PX+KX,PY+KY,UX,UY,STATUS)
      DXA = UX - X
      DY = UY - Y
      CALL ARGS_PTOU(ID,PX+KXA,PY,UX,UY,STATUS)
      DXB = UX - X
      XVAL(1) = X + DXA
      YVAL(1) = Y
      XVAL(2) = X + DXB
      YVAL(2) = Y
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
      XVAL(1) = X + DXA
      YVAL(1) = Y
      XVAL(2) = X + DXA
      YVAL(2) = Y + DY
      CALL ARGS_POLYL (ID,2,XVAL,YVAL,STATUS)
C
C
      IF (NUM.GT.0) THEN
         DO K = 1,72
            TEXT(K:K) = ' '
         ENDDO
         WRITE(TEXT,900)NUM
  900    FORMAT(I10)
         KSHIFT = 0
         DO K = 1,9
            IF(TEXT(K:K).EQ.' ') KSHIFT = K
         ENDDO
         DO K = 1,10
            J = K + KSHIFT
            TEXT(K:K) = TEXT(J:J)
         ENDDO
         CALL ARGS_UTOP(ID,X,Y,PX,PY,STATUS)
         CALL ARGS_PTOU(ID,PX+3,PY+3,UXA,UYA,STATUS)
         KX = UXA
         KY = UYA
C         CALL ARGTXT(TEXT,'B',0,KX,KY,0)
      ENDIF
C
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE ARGS_OVOPN(N,COL)
*
*+ PERFORM ALL INITIALISATION TO ALLOW USE OF BIT PLANE 'N'
* FOR OVERLAYS. OVERLAYS WILL BE IN COLOUR 'COL' (SEE 'ARGS_DECOL')
 
      INTEGER N,MASKN
      CHARACTER COL
 
* RESET VSR
      CALL ARGS_VSRRST
 
* DISABLE OVERLAYS TEMPORARILY
      CALL ARGS_OVCG('0')
 
* ENABLE PLANE 'N' AND SET ZDI
      MASKN = IAND(N,'000F'X)
      CALL ARGS_FLUSH(9)
      CALL ARGS_S1('ZWEI',ISHFT('0001'X,MASKN))
      CALL ARGS_S1('ZDI1',ISHFT('0001'X,MASKN))
 
* ENABLE OVERLAYS FOR BIT PLANE 'N'
      CALL ARGS_OVCG('W')
      CALL ARGS_OVC(ISHFT('0001'X,IAND(N,'007'X)),COL)
 
      END
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R ARCEMA *
C      *            *
C      **************
C
C  ORIGINAL PROGRAM PTW MOD 28/7/81 WFL TO ACCESS ARGS DATABASE
C  MOD BY AJP TO S/R ARCEMA 82 SEP
C
C ------------------------------------------------------
      SUBROUTINE ARCEMA(KX,KY,KXF,KYF,KW)
 
      CHARACTER VALUE*80
 
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
 
*  IF WANTED, USE PRESENT MAGNIFICATION
 
      IF (KW.EQ.0) THEN
         CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,ISTAT)
         CALL ASP_DZTOI('ZXF',VALUE,KXF,ISTAT)
         CALL ASP_DZTOI('ZYF',VALUE,KYF,ISTAT)
      ENDIF
 
*  DO THE ZOOM
 
      CALL IZOOM(KX,KY,KXF,KYF)
      CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,JSTAT)
      CALL ASP_ITODZ('ZXC',KX,VALUE,JSTAT)
      CALL ASP_ITODZ('ZYC',KY,VALUE,JSTAT)
      CALL ASP_ITODZ('ZXF',KXF,VALUE,JSTAT)
      CALL ASP_ITODZ('ZYF',KYF,VALUE,JSTAT)
      CALL ARGS_WRPAR('DISPZOOM',VALUE,1,JSTAT)
 
      END
 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE IZOOM(IX,IY,IFX,IFY)
      INTEGER IX,IY,IFX,IFY
 
      CALL ARGS_FLUSH(4)
      CALL ARGS_PUT1('C000'X+IX)
      CALL ARGS_PUT1('A000'X+IY)
      CALL ARGS_PUT1('5001'X)
      CALL ARGS_PUT1(256*(IFY-1)+IFX-1)
      CALL SRSEND
      END
C*************************************************************
C Subroutine to get one position X, Y on the ARGS with the cursor
C
C UX,UY :    (REAL  OUT)
C         Position of the cursor in pixels (reference=image)
C
C ISTATUS :
C           1  = Failure
C           2  = Red button
C*************************************************************
	SUBROUTINE ARGS_CURSJLP(UX,UY,ISTATUS)
	INTEGER*4 IDMAX,ID,IX,IY
	INTEGER*2 IOFF(3),ION(3),IFLIP(3)
	CHARACTER VALUE*80
	DATA IOFF,ION,IFLIP/3*0,'0038'X,'0008'X,'0003'X,3*0/
	SAVE ICALL
 
	ICALL=ICALL+1
 
*  SET SYSTEM CURSOR COLOUR AND ALLOW OVERLAYS
 
	CALL ARGS_VSR(IOFF,ION,IFLIP)
 
C Switch on lamps
	CALL ARGS_LAMPS(1,1,1,1)
 
C Read the present value of the zoom factor :
	   CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,JSTAT)
	   CALL ASP_DZTOI('ZXC',VALUE,IX,JSTAT)
	   CALL ASP_DZTOI('ZYC',VALUE,IY,JSTAT)
	   CALL ASP_DZTOI('ZXF',VALUE,KXF,JSTAT)
	   CALL ASP_DZTOI('ZYF',VALUE,KYF,JSTAT)
	    IF(KXF.EQ.0.OR.KYF.EQ.0)THEN
	     PRINT *,' EMPTY DATA BASE'
	     IX=256
	     IY=256
	     KXF=1
	     KYF=1
	     CALL ASP_ITODZ('ZXC',IX,VALUE,JSTAT)
	     CALL ASP_ITODZ('ZYC',IY,VALUE,JSTAT)
	     CALL ASP_ITODZ('ZXF',KXF,VALUE,JSTAT)
	     CALL ASP_ITODZ('ZYF',KYF,VALUE,JSTAT)
	     CALL ARGS_WRPAR('DISPZOOM',VALUE,1,JSTAT)
	    ENDIF
 
C Enable only system cursor :
	CALL ARGS_CURS('+')
 
C Centre cursor (if first call) :
	IF(ICALL.EQ.1)CALL ARGS_CURP(0,255,255)
 
C Load trackballer/cursor program into the ARGS :
	CALL ARGS_TBCL(0)
 
C******* Entering the points ******************
C Loop until button 1 or 4 is pressed :
 
	IB1=0
	IB2=0
	IB3=0
	IB4=0
 
94	IF(IB4.NE.0.OR.IB1.NE.0) GOTO 95
	CALL ARGS_TBCX(IX,IY,IB1,IB2,IB3,IB4)
 
C If button 1 pressed display X, Y
	 IF(IB1.NE.0)THEN
C	  CALL ARGS_CROSSJLP(IX,IY,4)
 
C Conversion in real taking into account the zoom and the centre position  ??
	  CALL ARGS_NUMIM(IDMAX)
	  CALL ARGS_ATOU(IDMAX,IX,IY,UX,UY,JSTAT)
	 ENDIF
 
C Possibility of zooming :
	 IF(IB2.NE.0.OR.IB3.NE.0)THEN
C Recentring the present pixel in the middle of the screen
	  CALL IZOOM(IX,IY,KXF,KYF)
C Increase or decrease zooming factor :
	    IF(IB2.EQ.1)THEN
	     KXF=MIN(16,KXF*2)
	     KYF=KXF
	    ELSE
	     KXF=MAX(1,KXF/2)
	     KYF=KXF
	    ENDIF
C Zooming the image :
	   CALL IZOOM(IX,IY,KXF,KYF)
C Storing the present state of the ARGS in the data base :
	   CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,JSTAT)
	   CALL ASP_ITODZ('ZXC',IX,VALUE,JSTAT)
	   CALL ASP_ITODZ('ZYC',IY,VALUE,JSTAT)
	   CALL ASP_ITODZ('ZXF',KXF,VALUE,JSTAT)
	   CALL ASP_ITODZ('ZYF',KYF,VALUE,JSTAT)
	   CALL ARGS_WRPAR('DISPZOOM',VALUE,1,JSTAT)
	ENDIF
 
	GOTO 94
 
C If RED button pressed :
95 	IF(IB4.EQ.1)THEN
	 ISTATUS=2
	ELSE
	 ISTATUS=0
	ENDIF
	
C Switch off lamps
	CALL ARGS_LAMPS(0,0,0,0)
 
*  DISABLE CURSOR
	CALL ARGS_CURS('0')
	CALL ARGS_VSRRST
	CALL SRSEND
	CALL ARGS_DOPDB ('ARGS_DEVICE',ISTAT)
	CALL ARGS_ATOU (IDMAX,IX,IY,UX,UY,ISTAT)
	CALL ARGS_CLDB (ISTAT)
 
C Exit
	RETURN
	END
