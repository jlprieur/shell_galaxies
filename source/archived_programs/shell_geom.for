C++************************************************************
C Program SHELL_GEOM to process shell geometrical parameters
C
C  Structure of the input files created by EFIT6 (and processed here) :
C # SHELL_NAME : Shell name				[CHARACTER*40]
C & COMMENTS : Comments on the shell			[CHARACTER*80]
C > VALUES : Values X,Y					[2 REAL VALUES]*NPTS
C \ PARAMETERS : Z1(5,IDIM)				[5 REAL VALUES]
C / ERRORS : ERRZ1(5,IDIM)				[5 REAL VALUES]
C % ANGULAR LIMITS : THEMIN,THEMAX			[2 REAL VALUES]
C
C Format for 2 real values : (X,2(X,E13.6))
C Example for Z(5) : ('\',5(X,E13.6))
C
C Z(1) : Major axis (arcseconds)
C Z(2) : Ellipticity (10*(1-b/a))
C Z(3) : Theta (degrees)
C Z(4),Z(5) : Position OX, OY of the centre of the galaxy
C             in the initial image (in pixels)
C
C JLP 
C Version of 28/07/99 
C--************************************************************
	PROGRAM SHELL_GEOM
	PARAMETER (IDIM=200,NMAX=200,NMAX1=1000)
	REAL*4 XX(NMAX,IDIM),YY(NMAX,IDIM),Z1(5,IDIM),ERRZ1(5,IDIM)
	REAL*4 THEMIN(IDIM),THEMAX(IDIM)
        REAL*4 OX1,OY1,SCALE
	INTEGER*4 NPTS(IDIM),NSHELL
	CHARACTER*40 SHELL_NAME(IDIM)
	CHARACTER*80 COMMENTS(IDIM)
	CHARACTER NAMECAT*40,ANS*1
	COMMON/SPR1/XX,YY,Z1,ERRZ1,THEMIN,THEMAX,SHELL_NAME,
     1	COMMENTS,NPTS,NAMECAT,NSHELL
 
10	FORMAT(A)
	CALL JLP_BEGIN
 
	NSHELL=0
	PRINT 11
11	FORMAT( ' Program SHELL_GEOM',/,
     1	' Version of 28/07/99',/)
88	PRINT 12
12	FORMAT(' MENU :',/,
     1	' 1. Reading/extending a file with shell positions',/,
     1	' 2. Creating such a file',/,
     1	' 3. Displaying the shells',/,
     1	' 4. Creating a plot in polar coordinates',/,
     1	' 5. Output for "LateX"',/,
     1	' 6. Correcting the orientation (North-East)',
     1  ' of shell positions',/,
     1	' 10. Exit',/,
     1	10X,' Enter your choice : ',$)	
	READ(5,*) IOPT
 
C********************************************************************
C Reading a file with shell positions and fit results :
C********************************************************************
	IF(IOPT.EQ.1)THEN
 
101	  PRINT *,' Input file? (Created by EFIT6, convention is name.EFI ...)'
	  READ(5,10) NAMECAT
	  OPEN(1,FILE=NAMECAT,STATUS='OLD',ERR=101)
 
C Input of the file :
	  CALL SPR_INPUTFILE(1)
 
	  PRINT *,' Position of the centre of the galaxy : ox, oy (pixels)?'
	  READ(5,*) OX1,OY1
	  PRINT *,' Scale (arcsec/pixel) ?'
	  READ(5,*) SCALE
 
	  PRINT 109,NSHELL
109	  FORMAT(' Number of shells :',I5)
	  PRINT *,' Do you want to add some more shells ?(N)'
	  READ(5,10) ANS
	  IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')CALL SPR_EXTEND
	  CLOSE(1)
	  GO TO 88
	ENDIF
 
C********************************************************************
C Option 2 : Creation of a file with shell positions and fit results :
C********************************************************************
	IF(IOPT.EQ.2)THEN
	  PRINT 201
201	  FORMAT(' Creation of a NEW FILE')
202	  PRINT *,' Name of the output file ? (convention is name.EFI)'
	  READ(5,10) NAMECAT
	  OPEN(1,FILE=NAMECAT,STATUS='NEW',ERR=202)
 
C Input of the shell positions and fit parameters :
	  CALL SPR_EXTEND
 
	  CLOSE(1)
	  GO TO 88
	ENDIF
 
C********************************************************************
C Option 3 : Displaying the shells
C********************************************************************
	IF(IOPT.EQ.3)THEN
          IF(NSHELL.GT.0)THEN
	    CALL SPR_DISPLAY(OX1,OY1,SCALE)
          ELSE
            PRINT *,'Error: no shell has been entered yet'
          ENDIF
	  GO TO 88
	ENDIF
 
C********************************************************************
C Option 4 : Producing a plot in polar coordinates
C********************************************************************
	IF(IOPT.EQ.4)THEN
          IF(NSHELL.GT.0)THEN
	    CALL SPR_POLAR(OX1,OY1,SCALE)
          ELSE
            PRINT *,'Error: no shell has been entered yet'
          ENDIF
	  GO TO 88
	ENDIF
 
C********************************************************************
C Option 5 : Output for LateX :
C********************************************************************
	IF(IOPT.EQ.5)THEN
          IF(NSHELL.GT.0)THEN
	    CALL SPR_LATEX
          ELSE
            PRINT *,'Error: no shell has been entered yet'
          ENDIF
	  GO TO 88
	ENDIF
 
C********************************************************************
C Read a file with shell positions and fit results
C and correct the orientation North-East 
C********************************************************************
	IF(IOPT.EQ.6)THEN
 
C Read the file if not already opened:
         IF(NSHELL.EQ.0)THEN
601	  PRINT *,' Input file? (Created by EFIT6, *.EFI)'
	  READ(5,10) NAMECAT
	  OPEN(1,FILE=NAMECAT,STATUS='OLD',ERR=601)
 
C Decode the file :
	  CALL SPR_INPUTFILE(1)

	  PRINT 609,NSHELL
609	  FORMAT(' Number of shells :',I5)
 
	  CLOSE(1)
         ENDIF
 
	 CALL SPR_CORRECT
	ENDIF
 
C*******************************************************************
 
	CALL JLP_END
	STOP
	END
 
C********************************************************************
C Subroutine DECODE_JLP
C
C
C********************************************************************
	SUBROUTINE DECODE_JLP(BUFF,VALUE,NVALUES)
	CHARACTER*80 BUFF
	CHARACTER*13 WORD
	REAL*4 VALUE(NVALUES)
	II1=3
	II2=II1+12
	 DO K=1,NVALUES
	  WORD=BUFF(II1:II2)
	  VALUE(K)=DECODE_E13D6(WORD,ISTAT)
	  II1=II1+14
	  II2=II1+12
	 END DO
	RETURN
	END
C********************************************************************
	REAL FUNCTION DECODE_E13D6(WORD,ISTAT)
	INTEGER*4 ISTAT
	CHARACTER WORD*13
	CHARACTER CWORK2*2,CWORK6*6
	ISTAT=0
 
C Reading the "mantisse"
	CWORK6=WORD(4:9)
	DO I=1,6
	  IF(CWORK6(I:I).LT.'0'.OR.CWORK6(I:I).GT.'9')THEN
	   PRINT *,' ERROR WHILE DECODING E13.6 FORMAT'
	   ISTAT=1
	   RETURN
	  ENDIF
	END DO
	READ(CWORK6,26) IWORK6
26	FORMAT(I6)
C Reading the sign :
	IF(WORD(1:1).EQ.'-')IWORK6=-1*IWORK6
 
C Reading the power of ten :
	CWORK2=WORD(12:13)
	DO I=1,2
	  IF(CWORK2(I:I).LT.'0'.OR.CWORK2(I:I).GT.'9')THEN
	   PRINT *,' ERROR WHILE DECODING E13.6 FORMAT'
	   ISTAT=2
	   RETURN
	  ENDIF
	END DO
	READ(CWORK2,22) IWORK2
22	FORMAT(I2)
C Reading the sign :
	IF(WORD(11:11).EQ.'-')IWORK2=-1*IWORK2
 
C Computing the actual value :
	DECODE_E13D6=FLOAT(IWORK6)*10.**(IWORK2-6)
	END
 
C**********************************************************************
C Subroutine SPR_INPUTFILE to input a file with the shell positions
C
C  Structure of the files :
C # SHELL_NAME : Shell name				[CHARACTER*40]
C & COMMENTS : Comments on the shell			[CHARACTER*80]
C > VALUES : Values X,Y					[2 REAL VALUES]*NPTS
C \ PARAMETERS : Z1(5,IDIM)				[5 REAL VALUES]
C / ERRORS : ERRZ1(5,IDIM)				[5 REAL VALUES]
C % ANGULAR LIMITS : THEMIN,THEMAX			[2 REAL VALUES]
C
C Format for 2 real values : (X,2(X,E13.6))
C Example of format for Z(5) : ('\',5(X,E13.6))
C************************************************************
	SUBROUTINE SPR_INPUTFILE(LUNIT)
	PARAMETER (IDIM=200,NMAX=200,NMAX1=1000)
	REAL*4 XX(NMAX,IDIM),YY(NMAX,IDIM),Z1(5,IDIM),ERRZ1(5,IDIM)
	REAL*4 THEMIN(IDIM),THEMAX(IDIM)
	INTEGER*4 NPTS(IDIM),NSHELL
	CHARACTER SHELL_NAME(IDIM)*40,COMMENTS(IDIM)*80
	CHARACTER BUFF*80,NAMECAT*40
	COMMON/SPR1/XX,YY,Z1,ERRZ1,THEMIN,THEMAX,SHELL_NAME,
     1	COMMENTS,NPTS,NAMECAT,NSHELL
 
	LU=LUNIT
 
10	FORMAT(A)
	JMAX=IDIM*NMAX
	IS=NSHELL
 
C Main loop :
	DO J=1,JMAX
	  READ(LU,10,END=999)BUFF
C Reading the shell name :
	   IF(BUFF(1:1).EQ.'#')THEN
	    IS=IS+1
	    SHELL_NAME(IS)=BUFF(3:42)
	    PRINT *,IS,' ',SHELL_NAME(IS)
	    NPTS(IS)=0
C Initialize THEMIN and THEMAX to zero:
            THEMIN(IS)=0
            THEMAX(IS)=0
	   ENDIF
C Reading the comments :
	   IF(BUFF(1:1).EQ.'&')THEN
	    COMMENTS(IS)=' '
	    COMMENTS(IS)=BUFF(3:68)
	    PRINT *,COMMENTS(IS)
	   ENDIF
C Reading the position points X,Y :
	   IF(BUFF(1:1).EQ.'>')THEN
	    NPTS(IS)=NPTS(IS)+1
	    READ(BUFF(2:80),*) XX(NPTS(IS),IS),YY(NPTS(IS),IS)
	   ENDIF
C Reading the ellipse parameters Z1(5,IS) :
C	   IF(BUFF(1:1).EQ.'\')THEN
	   IF(BUFF(1:1).EQ.CHAR(92))THEN
	    READ(BUFF(2:80),*) (Z1(K,IS),K=1,5)
	   ENDIF
C Reading the errors on these parameters ERRZ1(5,IS) :
	   IF(BUFF(1:1).EQ.'/')THEN
	    READ(BUFF(2:80),*) (ERRZ1(K,IS),K=1,5)
	   ENDIF
C Reading the angular limits THEMIN, THEMAX :
	   IF(BUFF(1:1).EQ.'%')THEN
	    READ(BUFF(2:80),*) THEMIN(IS),THEMAX(IS)
	   ENDIF
	END DO
 
999	NSHELL=IS
	RETURN
	END
 
C********************************************************************
C Subroutine SPR_EXTEND to create/extend a file
C with shell positions and fit results :
C
C  Structure of the files :
C # SHELL_NAME : Shell name				[CHARACTER*40]
C & COMMENTS : Comments on the shell			[CHARACTER*80]
C > VALUES : Values X,Y					[2 REAL VALUES]*NPTS
C \ PARAMETERS : Z1(5,IDIM)				[5 REAL VALUES]
C / ERRORS : ERRZ1(5,IDIM)				[5 REAL VALUES]
C % ANGULAR LIMITS : THEMIN,THEMAX			[2 REAL VALUES]
C
C Format for 2 real values : (X,2(X,E13.6))
C Example of format for Z(5) : ('\',5(X,E13.6))
C************************************************************
	SUBROUTINE SPR_EXTEND
	PARAMETER (IDIM=200,NMAX=200,NMAX1=1000)
	REAL*4 XX(NMAX,IDIM),YY(NMAX,IDIM),Z1(5,IDIM),ERRZ1(5,IDIM)
	REAL*4 THEMIN(IDIM),THEMAX(IDIM)
	REAL*4 X1(NMAX),Y1(NMAX),X2(NMAX),Y2(NMAX)
	INTEGER*4 NPTS(IDIM),NSHELL
	CHARACTER SHELL_NAME(IDIM)*40,COMMENTS(IDIM)*80
	CHARACTER BUFF*80,NAMECAT*40,NAME*40,ANS*1
        CHARACTER IN_FILE*40,IN_COMMENTS*80
	COMMON/SPR1/XX,YY,Z1,ERRZ1,THEMIN,THEMAX,SHELL_NAME,
     1	COMMENTS,NPTS,NAMECAT,NSHELL
 
10	FORMAT(A)
 
	PRINT *,' Do you want to input files created by efit6 ? (Y)'
	READ(5,10) ANS
 
 
	IF(ANS.NE.'n'.AND.ANS.NE.'N')THEN
	 PRINT *,' Number of files you want to input ?'
	 READ(5,*) NS
	 IS1=NSHELL+1
	 IS2=NSHELL+NS

	 DO IS=IS1,IS2
301	  PRINT *,' For shell #',IS,' : input file ? '
	  READ(5,10) NAME
	  OPEN(2,FILE=NAME,STATUS='OLD',ERR=301)
	  CALL SPR_INPUTFILE(2)
 	  CLOSE(2)
	 END DO
 
	ELSE
 
	 PRINT *,' Number of new shells you want to input ?'
	 READ(5,*) NS
	 IS1=NSHELL+1
	 IS2=NSHELL+NS

C*********************
C Manual input of the shell parameters :
	NSHELL=NSHELL+NS
	DO IS=IS1,IS2
C Name :
	PRINT 210
210	FORMAT(/,' Now enter the parameters for the new shell :',
     1	/,' Shell_name ?')
	READ(5,10) SHELL_NAME(IS)
C Comments :
	PRINT *,' Comments ?'
	READ(5,10) BUFF
	COMMENTS(IS)=BUFF
C Values :
	CALL RREADFILE(X1,Y1,NPT1,X2,Y2,NPT2,NMAX,
     1                 IN_FILE,IN_COMMENTS,0)
	NPTS(IS)=NPT1
	  DO K=1,NPT1
	   XX(K,IS)=X1(K)
	   YY(K,IS)=Y1(K)
	  END DO
C Fit parameters :
	   PRINT 211
211	   FORMAT(' Enter : major axis ("), ellipticity',
     1	' (10*(1-b/a)), theta (deg), ox, oy')
	   READ(5,*) (Z1(KK,IS),KK=1,5)
C Errors on these parameters :
	   PRINT 214
214	   FORMAT(' Enter the corresponding errors :')
	   READ(5,*) (ERRZ1(KK,IS),KK=1,5)
C Angular limits :
	   PRINT 215
215	   FORMAT(' Enter : themin (deg), themax (deg)')
	   READ(5,*) THEMIN(IS),THEMAX(IS)
 	END DO
 
	ENDIF
 
 
C Writing the shell parameters on unit 1:
	DO IS=IS1,IS2
	  WRITE(1,205)SHELL_NAME(IS)
205	  FORMAT('# ',A)
	  BUFF=COMMENTS(IS)
	  WRITE(1,206)BUFF(1:78)
206	  FORMAT('& ',A)
	   DO K=1,NPTS(IS)
	    WRITE(1,207)XX(K,IS),YY(K,IS)
	   END DO
207	   FORMAT('>',2(1X,F8.2))
	   WRITE(1,208) CHAR(92),(Z1(KK,IS),KK=1,5)
208	   FORMAT(A1,5(1X,E12.5))
C208	   FORMAT('\',5(1X,E12.5))
	   WRITE(1,213) (ERRZ1(KK,IS),KK=1,5)
213	   FORMAT('/',5(1X,E12.5))
	   WRITE(1,209)THEMIN(IS),THEMAX(IS)
209	   FORMAT('%',5(1X,E12.5))
	END DO
 
	RETURN
	END
 
C********************************************************************
C Subroutine SPR_CORRECT to correct the North-East orientation of a file
C with shell positions and fit results :
C
C  Structure of the files :
C # SHELL_NAME : Shell name				[CHARACTER*40]
C & COMMENTS : Comments on the shell			[CHARACTER*80]
C > VALUES : Values X,Y					[2 REAL VALUES]*NPTS
C \ PARAMETERS : Z1(5,IDIM)				[5 REAL VALUES]
C / ERRORS : ERRZ1(5,IDIM)				[5 REAL VALUES]
C % ANGULAR LIMITS : THEMIN,THEMAX			[2 REAL VALUES]
C
C Format for 2 real values : (X,2(X,E13.6))
C Example of format for Z(5) : ('\',5(X,E13.6))
C************************************************************
	SUBROUTINE SPR_CORRECT
	PARAMETER (IDIM=200,NMAX=200,NMAX1=1000)
	REAL*4 XX(NMAX,IDIM),YY(NMAX,IDIM),Z1(5,IDIM),ERRZ1(5,IDIM)
	REAL*4 THEMIN(IDIM),THEMAX(IDIM),W1
	REAL*4 X1(NMAX),Y1(NMAX),X2(NMAX),Y2(NMAX)
	INTEGER*4 NPTS(IDIM),NSHELL,NX
	CHARACTER SHELL_NAME(IDIM)*40,COMMENTS(IDIM)*80
	CHARACTER BUFF*80,NAMECAT*40,NAME*40,ANS*1
        CHARACTER OUT_FILE*40
	COMMON/SPR1/XX,YY,Z1,ERRZ1,THEMIN,THEMAX,SHELL_NAME,
     1	COMMENTS,NPTS,NAMECAT,NSHELL
 
10	FORMAT(A)
        
        WRITE(6,19)
19      FORMAT('I assume coordinates are within (0,0) to (nx-1,ny-1)') 

        WRITE(6,20)
20      FORMAT(' Version of July 1999: only X symmetry available',/,
     1         ' Enter width nx of input image:')
        READ(5,*) NX
	DO IS=1,NSHELL
	   DO K=1,NPTS(IS)
              XX(K,IS) = NX - XX(K,IS)
           END DO
C Theta:
          Z1(3,IS) = 180. - Z1(3,IS) 
C X center:
          Z1(4,IS) = NX - Z1(4,IS) 
C THEMIN, THEMAX:
          W1 = THEMIN(IS)
          THEMIN(IS) = 180. - THEMAX(IS)
          THEMAX(IS) = 180. - W1 
       END DO

22      WRITE(6,21)
21      FORMAT(' Enter new filename:')
        READ(5,10) OUT_FILE
        OPEN(1,NAME=OUT_FILE,STATUS='NEW',ERR=22)

C Writing the shell parameters on unit 1:
	DO IS=1,NSHELL
	  WRITE(1,205)SHELL_NAME(IS)
205	  FORMAT('# ',A)
	  BUFF=COMMENTS(IS)
	  WRITE(1,206)BUFF(1:78)
206	  FORMAT('& ',A)
	   DO K=1,NPTS(IS)
	    WRITE(1,207)XX(K,IS),YY(K,IS)
	   END DO
207	   FORMAT('>',2(1X,F8.2))
	   WRITE(1,208) CHAR(92),(Z1(KK,IS),KK=1,5)
208	   FORMAT(A1,5(1X,E12.5))
C208	   FORMAT('\',5(1X,E12.5))
	   WRITE(1,213) (ERRZ1(KK,IS),KK=1,5)
213	   FORMAT('/',5(1X,E12.5))
	   WRITE(1,209)THEMIN(IS),THEMAX(IS)
209	   FORMAT('%',5(1X,E12.5))
	END DO
        CLOSE(1)
 
	RETURN
	END
 
C********************************************************************
C Subroutine SPR_DISPLAY to display the shells
C********************************************************************
	SUBROUTINE SPR_DISPLAY(OX1,OY1,SCALE)
C Pb with "gene_ellipse" if NMAX1 is too small, leave it to 1000
	PARAMETER (IDIM=200,NMAX=200,NMAX1=1000)
	REAL*8 Z2(5),THEMI,THEMA
	REAL*4 XX(NMAX,IDIM),YY(NMAX,IDIM),Z1(5,IDIM),ERRZ1(5,IDIM)
	REAL*4 THEMIN(IDIM),THEMAX(IDIM)
	REAL*4 XXPLOT(NMAX1,IDIM),YYPLOT(NMAX1,IDIM)
	REAL*4 XPLOT(NMAX1),YPLOT(NMAX1),PI
        REAL*4 OX1,OY1,SCALE
	INTEGER*4 NPTS(IDIM),NPTSPLOT(NMAX1)
	CHARACTER SHELL_NAME(IDIM)*40,COMMENTS(IDIM)*80
	CHARACTER COLOUR1*1,COLOUR2*1,COLOUR3*1
	CHARACTER SYMB1*4,SYMB2*4,SYMB3*4
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40,ANS*1
	CHARACTER NAMECAT*40,PLOTDEV*32,COLOUR(NMAX1)*1
        CHARACTER NCHAR(NMAX1)*4,PCOLOR(NMAX1)*30
        LOGICAL PLOT_IN_PIXELS
 
C 3333 label:
        include 'jlpsub:jlp_graphic_help.for'

C Common block with NEWPLOT :
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
 
	COMMON/SPR1/XX,YY,Z1,ERRZ1,THEMIN,THEMAX,SHELL_NAME,
     1	COMMENTS,NPTS,NAMECAT,NSHELL
C	PI=3.14159265358979323846
        PI=ACOS(-1.)
 
10	FORMAT(A)
 
	PRINT *,' NSHELLS =',NSHELL
	CHAR3=NAMECAT
 
78      PRINT *,' Output graphic device : (? for help):'
        READ(5,10) PLOTDEV
        IF((PLOTDEV(1:1).EQ.'?').OR.(PLOTDEV(1:1).EQ.' '))THEN
          PRINT 3333
          GOTO 78
        ENDIF
        PRINT *,' Plot in pixel units? (Yes by default, or if No: in arcsec)'
        READ(5,10) ANS
        PLOT_IN_PIXELS=(ANS.NE.'N'.AND.ANS.NE.'n')
        IF(PLOT_IN_PIXELS)THEN
	  CHAR1=' X  (pixels)'
	  CHAR2=' Y  (pixels)'
        ELSE
	  CHAR1=' X  (arcsec)'
	  CHAR2=' Y  (arcsec)'
        ENDIF

C**********************
C Transfer of the shell positions to XXPLOT and YYPLOT :
        IF(PLOT_IN_PIXELS)THEN
	  DO IS=1,NSHELL
	  NPTSPLOT(IS)=NPTS(IS)
	    DO K=1,NPTS(IS)
	     XXPLOT(K,IS)=XX(K,IS)
	     YYPLOT(K,IS)=YY(K,IS)
	    END DO
	  END DO
        ELSE
C Center in (0.,0.), and scale in arcseconds
	  DO IS=1,NSHELL
	  NPTSPLOT(IS)=NPTS(IS)
	    DO K=1,NPTS(IS)
	     XXPLOT(K,IS)=(XX(K,IS)-OX1)*SCALE
	     YYPLOT(K,IS)=(YY(K,IS)-OY1)*SCALE
	    END DO
	  END DO
        ENDIF
 
C Crosses x for the points
	  SYMB1='51'
 
C Crosses + for the centers
	  SYMB3='43'
 
C Solid line for the fit
	  SYMB2='L0'
 
C**********************
C Generating the ellipses :
	DO IS=1,NSHELL
	  NCHAR(IS)=SYMB1
	  PCOLOR(IS)='Default'
	  NCHAR(IS+NSHELL)=SYMB2
	  PCOLOR(IS+NSHELL)='Default'
          IF(PLOT_IN_PIXELS)THEN
	    Z2(1)=Z1(1,IS)/SCALE
	    Z2(4)=Z1(4,IS)
	    Z2(5)=Z1(5,IS)
          ELSE
C Center in (0.,0.), and scale in arcseconds
	    Z2(1)=Z1(1,IS)
	    Z2(4)=(Z1(4,IS)-OX1)*SCALE
	    Z2(5)=(Z1(5,IS)-OY1)*SCALE
          ENDIF
	  Z2(2)=Z2(1)*(1-Z1(2,IS)/10.)
	  Z2(3)=Z1(3,IS)*PI/180.
	  THEMI=THEMIN(IS)
	  THEMA=THEMAX(IS)
	   IF((Z2(1).EQ.0..AND.Z2(2).EQ.0.)
     1        .OR.(THEMI.EQ.THEMA))THEN
	    NPLOT=0
	    PRINT *,' BAD ELLIPSE PARAMETERS FOR IS =',IS
            PRINT *,'Z2:',(Z2(K),K=1,5)
            PRINT *,'THEMIN, THEMAX:',THEMI,THEMA
            PRINT *,' I do not draw this fitted ellipse'
	   ELSE
            CALL GENE_ELLIPSE(XPLOT,YPLOT,NPLOT,Z2,THEMI,THEMA,1)
	   ENDIF
C Transfer to XXPLOT,YYPLOT
	  NPTSPLOT(IS+NSHELL)=NPLOT
	  DO K=1,NPLOT
	   XXPLOT(K,IS+NSHELL)=XPLOT(K)
	   YYPLOT(K,IS+NSHELL)=YPLOT(K)
	  END DO
	END DO
 
C Drawing the centre of the galaxy :
	KCURVE=2*NSHELL+1
	NPTSPLOT(KCURVE)=1
        IF(PLOT_IN_PIXELS)THEN
	  XXPLOT(1,KCURVE)=OX1
	  YYPLOT(1,KCURVE)=OY1
        ELSE
	  XXPLOT(1,KCURVE)=0.
	  YYPLOT(1,KCURVE)=0.
        ENDIF
 
	NCHAR(KCURVE)='49'
	PCOLOR(KCURVE)='Default'
 
C Drawing the ellipse centres :
	PRINT *,' Do you want to display the ellipse centres',
     1	' for each shell ? (y)'
	READ(5,10) ANS
	IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN
	  KCURVE=KCURVE+1
	  NCHAR(KCURVE)=SYMB3
	  PCOLOR(KCURVE)='Default'
	  KK=0
	  DO IS=1,NSHELL
	    IF(Z1(1,IS).NE.0..OR.Z1(2,IS).NE.0.)THEN
	     KK=KK+1
             IF(PLOT_IN_PIXELS)THEN
	       XXPLOT(KK,KCURVE)=Z1(4,IS)
	       YYPLOT(KK,KCURVE)=Z1(5,IS)
             ELSE
	       XXPLOT(KK,KCURVE)=Z1(4,IS)-OX1
	       YYPLOT(KK,KCURVE)=Z1(5,IS)-OY1
             ENDIF
	    ENDIF
	  END DO
	  NPTSPLOT(KCURVE)=KK
	ENDIF
 
C**********************
C Displaying the arrays XXPLOT and YYPLOT :
 
	IF(PLOTDEV.EQ.'args'.OR.PLOTDEV.EQ.'ARGS')THEN
 
C Displaying on the ARGS :
	  PRINT *,' SIZE OF THE DISPLAYED IMAGE NX,NY ?'
	  READ(5,*) NX,NY
	  PRINT *,' COLOUR FOR THE DATA POINTS (B,W,Y,R) ?'
	  READ(5,10) COLOUR1
	  PRINT *,' COLOUR FOR THE ELLIPSES (B,W,Y,R)?'
	  READ(5,10) COLOUR2
	  PRINT *,' COLOUR FOR THE CENTRES (B,W,Y,R)?'
	  READ(5,10) COLOUR3
	  IF(KCURVE.GT.1)COLOUR(KCURVE-1)=COLOUR3
	  COLOUR(KCURVE)=COLOUR3
	  DO I=1,NSHELL
	   COLOUR(I)=COLOUR1
	   COLOUR(I+NSHELL)=COLOUR2
	  END DO
C	  CALL ARGS_DISPLAY(XXPLOT,YYPLOT,NPTSPLOT,NMAX1,KCURVE,
C     1	NCHAR,COLOUR,NX,NY)
 
	ELSE
 
C Displaying on a graphic device :
	 CALL NEWPLOT(XXPLOT,YYPLOT,NPTSPLOT,NMAX1,KCURVE,
     1	CHAR1,CHAR2,CHAR3,NCHAR,PCOLOR,PLOTDEV,' ',' ')
	 IF(NOUT.NE.0)THEN
	 PRINT *,' ',NOUT,' POINTS RECEIVED :'
	 OPEN(3,FILE='shell_geom.dat',STATUS='unknown')
	 WRITE(3,*)NOUT
	   DO I=1,NOUT
	    PRINT *,I,XOUT(I),YOUT(I)
	    WRITE(3,*) XOUT(I),YOUT(I)
	   END DO
	 CLOSE(3)
	 PRINT *,' Output written in "shell_geom.dat"'
	 ENDIF
 
	ENDIF
 
	RETURN
	END
C********************************************************************
C Subroutine SPR_POLAR to create a plot in polar coordinates
C********************************************************************
	SUBROUTINE SPR_POLAR(OX1,OY1,SCALE)
	PARAMETER (IDIM=200,NMAX=200,NMAX1=1000)
	REAL*4 XX(NMAX,IDIM),YY(NMAX,IDIM),Z1(5,IDIM),ERRZ1(5,IDIM)
	REAL*4 THEMIN(IDIM),THEMAX(IDIM)
	REAL*4 XXPLOT(NMAX1,IDIM),YYPLOT(NMAX1,IDIM)
        REAL*4 OX1,OY1,SCALE
	INTEGER*4 NPTS(IDIM)
	CHARACTER SHELL_NAME(IDIM)*40,COMMENTS(IDIM)*80
	CHARACTER SYMB1*4,PCOLOR(NMAX1)*30
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40
	CHARACTER NAMECAT*40,PLOTDEV*32,NCHAR(NMAX1)*4
	COMMON/SPR1/XX,YY,Z1,ERRZ1,THEMIN,THEMAX,SHELL_NAME,
     1	COMMENTS,NPTS,NAMECAT,NSHELL
 
C Common block with NEWPLOT :
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
 
10	FORMAT(A)
 
	PRINT *,' NSHELLS =',NSHELL
	PRINT 86
86	FORMAT(' Now displaying in polar coordinates',/,
     1	' Possibility of output of a file directly',/,
     1	' compatible with SHELL_PLOT_QUINN',/,
     1	' by simply entering the mean value of the radius',
     1	' with the cursor',/)
	PRINT *,' OUTPUT GRAPHIC DEVICE : '
	READ(5,10) PLOTDEV
	PRINT *,' SYMBOL FOR THE DATA POINTS (L=LINE, 54=CROSSES) ?'
	READ(5,10) SYMB1
 
C**********************
C Computing the shell positions in XXPLOT and YYPLOT :
	DO IS=1,NSHELL
	  NCHAR(IS)=SYMB1
	  PCOLOR(IS)='Default'
	  DO K=1,NPTS(IS)
	   CALL POLAR_DEG(XX(K,IS),YY(K,IS),XXPLOT(K,IS),
     1	YYPLOT(K,IS),OX1,OY1)
	   XXPLOT(K,IS)=XXPLOT(K,IS)*SCALE
	  END DO
	END DO
 
C**********************
C Displaying the arrays XXPLOT and YYPLOT :
 
C Displaying on a graphic device :
	 CHAR1=' RADIUS (arcsec)'
	 CHAR2=' ANGLE (degrees)'
	 CHAR3=NAMECAT
	 CALL NEWPLOT(XXPLOT,YYPLOT,NPTS,NMAX1,NSHELL,
     1	CHAR1,CHAR2,CHAR3,NCHAR,PCOLOR,PLOTDEV,NAMECAT,' ')
 
C Storing the output in a file used by SHELLS_PLOT_QUINN to fit
C a dark halo :
	 IF(NOUT.NE.0)THEN
	 PRINT *,' ',NOUT,' POINTS RECEIVED :'
	 OPEN(3,FILE='spr_quinn.dat',STATUS='unknown')
	 WRITE (3,10)NAMECAT
	 WRITE(3,*)NOUT
	 WRITE(3,*)' #, RAD, RAD, LOGNEP(RAD), ANGLE'
	   DO I=NOUT,1,-1
	    PRINT *,I,XOUT(I),YOUT(I)
	    WRITE(3,*)I,XOUT(I),XOUT(I),ALOG(XOUT(I)),YOUT(I)
	   END DO
	 CLOSE(3)
	 PRINT *,' Output written in "spr_quinn.dat"'
	 ENDIF
 
	RETURN
	END
C********************************************************************
C Subroutine SPR_LATEX
C********************************************************************
	SUBROUTINE SPR_LATEX
	PARAMETER (IDIM=200,NMAX=200)
	REAL*4 XX(NMAX,IDIM),YY(NMAX,IDIM),Z1(5,IDIM),ERRZ1(5,IDIM)
	REAL*4 THEMIN(IDIM),THEMAX(IDIM),PI
	INTEGER*4 NPTS(IDIM)
	CHARACTER SHELL_NAME(IDIM)*40,COMMENTS(IDIM)*80
	CHARACTER NAMECAT*40,NAME1*40,NAME2*30,BUFF*80,CWORK*14
        CHARACTER BSL*1
	COMMON/SPR1/XX,YY,Z1,ERRZ1,THEMIN,THEMAX,SHELL_NAME,
     1	COMMENTS,NPTS,NAMECAT,NSHELL
        PI=ACOS(-1.)
 
10	FORMAT(A)
 
	PRINT *,' NSHELLS =',NSHELL
401	PRINT *,' OUTPUT FILE ?'
	READ(5,10) NAME1
C (On a VAX add 'CARRIAGECONTROL='LIST', to be compatible with the editor and 
C avoid trouble when printing it directly on PRINTRONIX, or
C transferring it to a microcomputer with ETHERNET)
	OPEN(4,FILE=NAME1,STATUS='NEW',ERR=401)
	PRINT *,' NAME OF THE GALAXY ?'
	READ(5,10) NAME2
 
C Problems with "\" with the SUN:
C402	FORMAT('\documentstyle{article}',/,
C     1	'\pagestyle{plain}',/,'\oddsidemargin 0.cm',/,
C     1	'\topmargin 0.cm',/,'\textwidth=18.cm',/,
C     1	'\textheight=26.cm',/,'\begin{document}',/,
C So I replace with:
        BSL=CHAR(92)
	WRITE(4,400) BSL,BSL,BSL,BSL,BSL,BSL,BSL,BSL
400	FORMAT(A1,'documentstyle{article}',/,
     1	A1,'pagestyle{plain}',/,A1,'oddsidemargin 0.cm',/,
     1	A1,'topmargin 0.cm',/,A1,'textwidth=18.cm',/,
     1	A1,'textheight=26.cm',/,A1,'begin{document}',/,
     1	A1,'large',/,/)
	WRITE(4,402)NAME2
402     FORMAT(' Geometrical parameters of the shells of : ',A,/)
 
	PRINT *,' COMMENTS ?'
	READ(5,10) BUFF
 
	WRITE(4,403)BSL,BUFF,BSL,BSL,BSL,BSL,BSL,BSL
403	FORMAT(A1,'vspace{5.mm}',/,/,A,/,/,A1,'normalsize',/,/,
     1	A1,'vspace{8.mm}',
     1	A1,'begin{tabular}{|l|r|r|r|r|r|r|r|r|} ',A1,'hline',/,
     1	'& & & & & & & & ',A1,A1)
	WRITE(4,408)BSL,BSL,BSL,BSL,BSL,BSL,BSL,
     1              BSL,BSL,BSL,BSL,BSL,BSL,BSL,BSL
408     FORMAT('Shell & Major & Radius & Ellipt. & $',A1,'Theta_{c}$ ',
     1	/,'& $',A1,'Theta_{med}$ & $',A1,'Delta ',A1,'Theta$ ',
     1	'& $X_{c}$ & $Y_{c}$ ',A1,A1,/,
     1	'number & axis & & & & & & & ',A1,A1,/,
     1	'(1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9) ',A1,A1,/,
     1	'& & & & & & & & ',A1,A1,' ',A1,'hline',/,
     1	'& & & & & & & & ',A1,A1)
 
	ERR_THEMAX=8.
	ERR_THEMIN=8.
 
	DO IS=1,NSHELL
	  IF(Z1(1,IS).NE.0..OR.Z1(2,IS).NE.0)THEN
	   CWORK=SHELL_NAME(IS)
	   THETA_MEDIUM=(THEMAX(IS)+THEMIN(IS))/2.
	   DELTA=THEMAX(IS)-THEMIN(IS)
	   XMINAXIS=Z1(1,IS)*(1.-Z1(2,IS)/10.)
	   ERR_MINAXIS=SQRT( (ERRZ1(1,IS)*(1.-Z1(2,IS)/10.))**2
     1	+ (Z1(1,IS)*ERRZ1(2,IS)/10.)**2 )
	   ERR_DELTA=SQRT( (ERR_THEMAX/2.)**2 + (ERR_THEMIN/2.)**2 )
	   RAD_MEDIUM=SQRT( ( Z1(1,IS)*COS((THETA_MEDIUM-Z1(3,IS))*PI/180.) )**2
     1	+ ( XMINAXIS*SIN((THETA_MEDIUM-Z1(3,IS))*PI/180.) )**2 )
C For ERR_RAD_MEDIUM : approximation I neglect the uncertainties on the
C position angles :
	   ERR_RADM=SQRT( ( ERRZ1(1,IS)*Z1(1,IS)
     1	*(COS((THETA_MEDIUM-Z1(3,IS))*PI/180.))**2
     1	/RAD_MEDIUM )**2
     1	+ ( ERR_MINAXIS*XMINAXIS
     1	*(SIN((THETA_MEDIUM-Z1(3,IS))*PI/180.))**2
     1	/RAD_MEDIUM )**2 )
	   WRITE(4,404)CWORK,Z1(1,IS),RAD_MEDIUM,Z1(2,IS),Z1(3,IS),
     1	THETA_MEDIUM,DELTA,Z1(4,IS),Z1(5,IS),BSL,BSL
404	   FORMAT(A14,' & ',F6.1,' & ',F6.1,
     1	4(' & ',F8.1),/,2(' & ',F8.1),A1,A1)
	   WRITE(4,405)BSL,ERRZ1(1,IS),BSL,ERR_RADM,BSL,ERRZ1(2,IS),
     1                 BSL,ERRZ1(3,IS),BSL,ERR_DELTA,BSL,ERRZ1(4,IS),
     1                 BSL,ERRZ1(5,IS),BSL,BSL,BSL,BSL
405	   FORMAT(2(' & $',A1,'pm$ ',F6.1),' & $',A1,'pm$ ',F5.2,' & $',
     1  A1,'pm$ ',F6.1,' & ',/,3(' & $',A1,'pm$ ',F6.1),A1,A1,
     1	/,'& & & & & & & & ',A1,A1)
	  ENDIF
	END DO
 
	WRITE(4,407) BSL,BSL,BSL
407	FORMAT(A1,'hline',/,A1,'end{tabular}',/,/,A1,'end{document}')
 
	CLOSE(4)
	RETURN
	END
C*************************************************************************
c	include 'jlpsub:args_display.for'
	include 'jlpsub:gene_ellipse.for'
	include 'jlpsub:polar_deg.for'
