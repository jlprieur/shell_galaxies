C++********************************************************************
C Program EFIT6 to fit ellipses to shells
C Possibility of entering shells one by one, or by a whole set of shells
C (when using XYCURA1).
C Fits ellipses with fixed or free centre (calling EFIT). The minimum
C number of points for the fit is 8.
C
C ! Warning : sometimes problems if values are too big in X and Y ...
C ! Divide by 100 or 1000 to get the correct result...
C
C SYNTAX: (use it in prompting mode for the full possibilities)
C  RUNS EFIT6 [Y/N: interactive?] xcenter,ycenter scale("/pix)
C       [code: 1=XYCURA 2=GRINNEL 4=Midas Table ...]
C             [FIxed or FRee center] position_list output_catalog
C
C Example:
C  RUNS EFIT6 N 156.2,234.4 0.492 1 FI N1210_SUM.LIS N1210_SUM_FIX.EFI
C
C
C You can set the graphic output device  to:
C &xterm   (X window) 
C &postscript (portrait postscript)
C &square  (postscript, square)
C &landscape  (landscape postscript)
C 
C The postcript files are named "pst.tmp".
C 
C The way to enter multiple shells is simply to repeat twice the last
C point (i.e. you press twice on the cursor to indicate the last point
C of a given shell).
C
C JLP
C Version of 20-07-99 
C--********************************************************************
C Warning: the command mode does not work when Midas tables are used...
C
	PROGRAM EFIT6
	PARAMETER (IDIM=1000,IDIM1=2000,MAXPOINTS=2000)
	IMPLICIT REAL*8(A-H,P-Z)
	REAL*4 X,Y,XTT(IDIM1),YTT(IDIM1)
	REAL*4 WORKX2(IDIM),WORKY2(IDIM)
	REAL*8 THEMIN,THEMAX,Z(5),PI,X0,Y0,SCALE
        REAL*4 MEAN_RADIUS,ERR_MEAN_RADIUS 
	REAL*4 PARAM(6),ERRPARAM(8)
        INTEGER*4 NPOINT,IFLG
	CHARACTER OUTNAME*40,BUFF*78,ANS*1,PLOTDEV*32,CHOICE*5
        CHARACTER IN_FILE*40,IN_COMMENTS*80
	LOGICAL*4 AUTO_GUESS,AUTO_IMPROVE,FOURIER,KAMIKAZE
	LOGICAL*4 FIXCENT,BATCH,TALK
 
	COMMON/FIT1/Z,NPOINT,IFLG
	COMMON/BLOCKA/PI,THEMIN,THEMAX
	COMMON/BLOCKB/X(MAXPOINTS),Y(MAXPOINTS)
	COMMON/BLOCKC/SCALE
	COMMON/FIXED_CENT/X0,Y0
C 3333 label:
        include 'jlpsub:jlp_graphic_help.for'
	PI=3.14159265358979D0
 
10	FORMAT(A)
	CALL JLP_BEGIN
 
	PRINT 111
111	FORMAT(' Program EFIT6,  Version of 20-07-99',/,
     1	' Ellipse fitting to the shells with',
     1	' a fixed/free centre',/)	
 
C Opening a scratch file containing the results of "ECALC"
	OPEN(11,FILE='ellipse.tmp',STATUS='unknown')
 
C Opening the files to check the output :
	OPEN(9,FILE='efit6.log',STATUS='unknown')
 
C Choosing between interactive or batch
	PRINT *,'Do you want to display the fit',
     1	' (i.E., "interactive" mode) ?(Y)'
	READ(5,10) ANS
	BATCH=(ANS.EQ.'N'.OR.ANS.EQ.'n')
	IF(BATCH)THEN
	  PRINT *,' "Batch" mode selected'
	ELSE
78        PRINT *,' Output graphic device : (? for help):'
          READ(5,10) PLOTDEV
          IF((PLOTDEV(1:1).EQ.'?').OR.(PLOTDEV(1:1).EQ.' '))THEN
            PRINT 3333
            GOTO 78
          ENDIF
        ENDIF 
 
	PRINT *,' Centre of the galaxy : x0, y0 ?'
	READ(5,*) X0,Y0
	WRITE(9,*) ' Centre :',X0,Y0
 
	PRINT *,' Scale in arcsec/pixel ?'
	READ(5,*) SCALE
	WRITE(9,*) ' Scale :',SCALE
 
C***************************
C Entering the positions :
C***************************
C	PRINT 112
112	FORMAT(5X,'Are you working with:',/,
     1	' 1. a set of shells from "XYCURA1"',/,
     1	' 2. a set of shells from "GRINNEL"',/,
c     1	' 3. a set of shells from "MIDAS" ',
c     1	'(Midas table)',/,
     1	' 4. a set of shells from "MIDAS" ',
     1	'(Table converted to ASCII with PRINT/TABLE)',/,
     1	' 5. an ASCII file (X,Y,xxxx) with "# comments line" ?')
C	READ(5,*) IOPT
C JLP96: I set the format to "5"
        IOPT=5
 
	PRINT 113
113	FORMAT(' FIXED or FREE centre for the fit ?  FI(XED)/FR(REE)')
	READ(5,10) CHOICE
	FIXCENT=(CHOICE(1:2).EQ.'FI'.OR.CHOICE(1:2).EQ.'fi')	
 
	PRINT *,' Enter the file with the positions :'
	 IF(IOPT.EQ.1)THEN
C STARLINK and XYCURA1, option 4 for the format :
	   CALL RREADFILE(XTT,YTT,NTT,WORKX2,WORKY2,I,IDIM1,
     1                    IN_FILE,IN_COMMENTS,4)
	 ELSEIF(IOPT.EQ.2)THEN
C GRINNEL (Toulouse) option 6 for the format :
	   CALL RREADFILE(XTT,YTT,NTT,WORKX2,WORKY2,I,IDIM1,
     1                    IN_FILE,IN_COMMENTS,6)
	 ELSEIF(IOPT.EQ.3)THEN
C MIDAS table from DE ANZA (ESO) option 7 for the format :
	   CALL RREADFILE(XTT,YTT,NTT,WORKX2,WORKY2,I,IDIM1,
     1                    IN_FILE,IN_COMMENTS,7)
	 ELSEIF(IOPT.EQ.4)THEN
C MIDAS table converted to ASCII with PRINT/TABLE :
	   CALL RREAD_ASCII_MIDAS(IN_FILE,IN_COMMENTS,XTT,YTT,NTT)
	 ELSEIF(IOPT.EQ.5)THEN
C Ascii file 
	   CALL RREAD_ASCII(IN_FILE,IN_COMMENTS,XTT,YTT,NTT)
	 ELSE
C Option 0 (=unknown) for the format :
	    CALL RREADFILE(XTT,YTT,NTT,WORKX2,WORKY2,I,IDIM1,
     1                     IN_FILE,IN_COMMENTS,0)
	 ENDIF
C 
         PRINT *,' Number of points: NTT=',NTT
 
C Output file compatible with SHELL_PROC
302	 PRINT *,' Name of the output file (NEW!) (convention is "name.EFI"...):'
	 READ(5,10) OUTNAME
	 OPEN(1,FILE=OUTNAME,STATUS='NEW',ERR=399)
 
C Loop if multiple shells :
	 ITT=0
	 ISHELL=1
 
180      IF(ITT.LT.NTT)THEN

C Loading the positions of the new shell in X(I), Y(I),
C until we find (-1000,-1000) (point outside of the frame for GRINNEL)
C or that we find a point which has been entered twice (DE ANZA, MIDAS):
	    ITT=ITT+1
	    X(1)=XTT(ITT)
	    Y(1)=YTT(ITT)
            DO I=2,IDIM1
	      ITT=ITT+1
	      X(I)=XTT(ITT)
	      Y(I)=YTT(ITT)
C Exit if "end of shell" indicated (by repeating the coordinates
C of one point:
	      IF((XTT(ITT).EQ.XTT(ITT-1).AND.YTT(ITT).EQ.YTT(ITT-1))
     1	.OR.ITT.GE.NTT) GOTO 97
	      IF((XTT(ITT).EQ.-1000.).AND.(YTT(ITT).EQ.-1000.))THEN
                 ITT=NTT
                 GOTO 97
              ENDIF
C Exit from the loop if end of list reached:
              IF(ITT.EQ.NTT)GOTO 97
            END DO

97	    NPOINT=I-1
 
C Check if the number of points is null or equal to 1 :
C (999=END)
	    IF(NPOINT.LE.1) THEN
	       PRINT *,' NPOINT < 1,    all shells done'
	       GO TO 999
	    ENDIF
 
C*******************
C Ellipse fit :
C*******************
 
C Calling EFIT to fit a portion of an ellipse :
	AUTO_IMPROVE=.FALSE.
	AUTO_GUESS=.TRUE.
	FOURIER=.FALSE.
	KAMIKAZE=.TRUE.
 
	IF(FIXCENT)THEN
C 3 parameters (fixed centre)
	  NPARAM=3
	ELSE
C Free centre :
	  NPARAM=5
	ENDIF
 
C Fitting the ellipse :
	  TALK=.TRUE.
	  CALL EFIT(NPOINT,NPARAM,AUTO_GUESS,AUTO_IMPROVE,
     1	FOURIER,KAMIKAZE,IFAIL,TALK)
 
C Displaying the fitted ellipse
	  IF(.NOT.BATCH) 
     1          CALL DISPLAY_FIT(PLOTDEV,IN_FILE,IN_COMMENTS)
 
C Tidy the results with ECALC and provide index KK of internal array ESTORE 
	 CALL ECALC(KK)
 
C*************************
C Output of the results :
C
C  Structure of the files :
C # SHELL_NAME : Shell name                             [CHARACTER*40]
C & COMMENTS : Comments on the shell                    [CHARACTER*80]
C > VALUES : Values X,Y                                 [2 REAL VALUES]*NPTS
C \ PARAMETERS : Z1(5,IDIM)                             [5 REAL VALUES]
C / ERRORS : ERRZ1(5,IDIM)                              [5 REAL VALUES]
C % ANGULAR LIMITS : THEMIN,THEMAX                      [2 REAL VALUES]
C**************************
 
C Storing the output in a file compatible with SHELL_PROC
	  IF(BATCH)THEN
	    WRITE(BUFF,301)ISHELL
301	    FORMAT(' SHELL',I2)
	    WRITE(1,303)BUFF
	    WRITE(1,304)OUTNAME
	  ELSE
C Prompt the user if "not batch"
	    PRINT *,' NAME OF THE SHELL ?'
	    READ(5,10) BUFF
	    WRITE(1,303)BUFF
303	    FORMAT('# ',A)
	    PRINT *,' COMMENTS ?'
	    READ(5,10) BUFF
	    WRITE(1,304)BUFF
304	    FORMAT('& ',A)
	  ENDIF
 
	  DO I=1,NPOINT
	   WRITE(1,305)X(I),Y(I)
	  END DO
305	  FORMAT('>',2(1X,F8.2))
 
C Check IFAIL : Store the last results if IFAIL=0 (evrything OK)
C or IFAIL=-3 (large errors)
	IF(IFAIL.EQ.0.OR.IFAIL.EQ.-3)THEN
 
C Reading the shell parameters:
C
C PARAM(1) : Major axis (arcsec)
C PARAM(2) : Ellipticity (10*(1-B/A))
C PARAM(3) : Theta (degrees)
C PARAM(4) : X centre
C PARAM(5) : Y centre
 
C Reading the last results (if IFAIL=0) :
C MAJOR_AXIS,MEAN_RADIUS,ELLIPTICITY,X0,Y0,THETA
          CALL CONVERT_EFIT(KK,PARAM(1),MEAN_RADIUS,PARAM(2),PARAM(4),
     1              PARAM(5),PARAM(3),ERRPARAM(1),ERR_MEAN_RADIUS,
     1              ERRPARAM(2),ERRPARAM(4),ERRPARAM(5),ERRPARAM(3))

C Decode the information:

C '\' and 5 ellipse parameters
C Z(1) : Major axis (arcseconds)
C Z(2) : Ellipticity (10*(1-b/a))
C Z(3) : Theta (degrees)
C Z(4),Z(5) : Position OX, OY of the centre of the galaxy
C             in the initial image (in pixels)
C PB with '\', so 
C306	    FORMAT('\',5(1X,E12.5))
	     WRITE(1,306)CHAR(92),(PARAM(K),K=1,5)
306	    FORMAT(A1,5(1X,E12.5))
C '/' and 5 ellipse error parameters
	     WRITE(1,307)(ERRPARAM(K),K=1,5)
307	    FORMAT('/',5(1X,E12.5))
 
C Store THEMIN and THEMAX
	    THEMIND=THEMIN*180./PI
	    THEMAXD=THEMAX*180./PI
	     WRITE(1,308)THEMIND,THEMAXD
308	    FORMAT('%',2(1X,E12.5))
	ENDIF
 
C Go and process next shell
	 ISHELL=ISHELL+1
         GOTO 180
        ENDIF 

C****** End ********
999	CLOSE(1)
	CLOSE(9)
	CLOSE(11)
	PRINT *,' '
	PRINT *,' Output in efit6.log (results).'
 
	CALL JLP_END
	STOP
399     PRINT *,' Error opening NEW file: check if such a file is not already there!"'
        PRINT *,' Enter new name'
        GOTO 302
	END
C**************************************************************************
C Subroutine DISPLAY_FIT
C To display the fitted ellipse and the original points
C
C Input :
C PLOTDEV : Plotting device
C X,Y : input points
C NPOINT : Number of original points X,Y
C Z(5)
C THEMIN, THEMAX in radians
C**************************************************************************
	SUBROUTINE DISPLAY_FIT(PLOTDEV,IN_FILE,IN_COMMENTS)
	PARAMETER (IDIM=1000,MAXPOINTS=2000)
	IMPLICIT REAL*8(A-H,P-Z)
	REAL*4 X,Y
	REAL*4 XPLOT(IDIM),YPLOT(IDIM)
	REAL*4 X2PLOT(IDIM,2),Y2PLOT(IDIM,2)
	REAL*8 THEMIND,THEMAXD,THEMIN,THEMAX,Z(5),PI,SCALE
	INTEGER*4 NPOINT,IFLG,NPTS(2)
	LOGICAL*4 PGPLOT
	CHARACTER NAME*40,BUFF*78,IN_FILE*60,IN_COMMENTS*80
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40
	CHARACTER PLOTDEV*32,COLOUR(2)*1,NCHAR(2)*4
 
	COMMON/FIT1/Z,NPOINT,IFLG
	COMMON/BLOCKA/PI,THEMIN,THEMAX
	COMMON/BLOCKB/X(MAXPOINTS),Y(MAXPOINTS)
 
10	FORMAT(A)
 
C Generating the portion of ellipse:
	  THEMIND=THEMIN*180./PI
	  THEMAXD=THEMAX*180./PI
	  CALL GENE_ELLIPSE(XPLOT,YPLOT,NPLOT,Z,THEMIND,THEMAXD,1)
	  CHAR3=' Ellipse fit'
	  CHAR1=' X '
	  CHAR2=' Y '
 
	PGPLOT=(PLOTDEV(1:1).EQ.'*')
 
C Selection of the symbols according to the output device :
	  IF(PLOTDEV(1:4).EQ.'ARGS'.OR.PLOTDEV(1:4).EQ.'args')THEN
	     PRINT *,' SIZE OF THE DISPLAYED IMAGE : NX,NY ?'
	     READ(5,*) NX,NY
	     PRINT *,' COLOUR (B, R, W, OR Y) FOR THE CROSSES ?'
	     READ(5,10) COLOUR(1)
	     PRINT *,' COLOUR (B, R, W, OR Y) FOR THE ELLIPSE ?'
	     READ(5,10) COLOUR(2)
	     NCHAR(1)='54'
	     NCHAR(2)='L'
	     NPTS(1)=NPOINT
	      DO I=1,NPTS(1)
	       X2PLOT(I,1)=X(I)
	       Y2PLOT(I,1)=Y(I)
	      END DO
	     NPTS(2)=NPLOT
	      DO I=1,NPTS(2)
	       X2PLOT(I,2)=XPLOT(I)
	       Y2PLOT(I,2)=YPLOT(I)
	      END DO
C	     CALL ARGS_DISPLAY(X2PLOT,Y2PLOT,NPTS,IDIM,2,
C     1	NCHAR,COLOUR,NX,NY)
 
C Crosses x with PGPLOT :
	  ELSEIF(PGPLOT)THEN
	     CALL DISPLAY2(X,Y,1,NPOINT,XPLOT,YPLOT,1,NPLOT,
     1	CHAR1,CHAR2,CHAR3,PLOTDEV,'5','L',IN_FILE,IN_COMMENTS)
 
	  ELSE
	     CALL DISPLAY2(X,Y,1,NPOINT,XPLOT,YPLOT,1,NPLOT,
     1	CHAR1,CHAR2,CHAR3,PLOTDEV,'54','L',IN_FILE,IN_COMMENTS)
	  ENDIF
	
	RETURN
	END
C**************************************************************************
C Subroutine RREAD_ASCII_MIDAS
C To read a MIDAS table converted to ASCII with PRINT/TABLE :
C
C**************************************************************************
	SUBROUTINE RREAD_ASCII_MIDAS(NAME,COMMENTS,XTT,YTT,NTT)
	REAL*4 XTT(*),YTT(*)
	INTEGER*4 NTT
	CHARACTER NAME*40,COMMENTS*80,BUFF*80
 
10	FORMAT(A)
8	PRINT *,' Name of the file?'
	READ(5,10) NAME
	OPEN(2,FILE=NAME,STATUS='OLD',ERR=8)
C Read header:
	READ(2,10) COMMENTS 
	DO I=1,2
	  READ(2,10) BUFF
	END DO
C Read X and Y:
	DO I=1,2000
          READ(2,*,END=999,ERR=999) IDUMMY,XTT(I),YTT(I)
	END DO
 
999	NTT=I-1
	CLOSE(2)
	RETURN
	END
C**************************************************************************
C Subroutine RREAD_ASCII
C To read an ASCII file X,Y,intensity
C
C**************************************************************************
	SUBROUTINE RREAD_ASCII(IN_FILE,IN_COMMENTS,XTT,YTT,NTT)
	REAL*4 XTT(*),YTT(*)
	INTEGER*4 NTT,K
	CHARACTER IN_FILE*40,IN_COMMENTS,BUFF*80
 
10	FORMAT(A)
8	PRINT *,' Name of the file?'
	READ(5,10) IN_FILE
	OPEN(2,FILE=IN_FILE,STATUS='OLD',ERR=8)
        IN_COMMENTS=' '
C Read X and Y:
        K=0
	DO I=1,2000
          READ(2,10,END=999,ERR=999) BUFF
C Allow for comments starting with #,/,\,&,%
C CHAR(92)=\
          IF(BUFF(1:1).NE.'#'.AND.BUFF(1:1).NE.'%'.AND.BUFF(1:1).NE.CHAR(92)
     1       .AND.BUFF(1:1).NE.'&'.AND.BUFF(1:1).NE.'/') THEN
              K = K + 1
              IF(BUFF(1:1).EQ.'>')THEN
                READ(BUFF(2:80),*,ERR=999) XTT(K),YTT(K)
              ELSE
                READ(BUFF(1:80),*,ERR=999) XTT(K),YTT(K)
              ENDIF
          ELSE
              WRITE(6,10) BUFF(1:80)
          ENDIF
	END DO
 
999     IF(K.GT.1)THEN
C Automatically double last coordinate if not done,
C to be able to recognize the end of the shell:
           IF((XTT(K-1).NE.XTT(K)).OR.(YTT(K-1).NE.YTT(K)))THEN
               K = K +1
               XTT(K) = XTT(K-1)
               YTT(K) = YTT(K-1)
           ENDIF
        ENDIF
	NTT=K
	CLOSE(2)
	RETURN
	END
C**************************************************************************
	include 'jlpsub:efit.for'
	include 'jlpsub:gene_ellipse.for'
c	include 'jlpsub:args_display.for'
