C++------------------------------------------------------------------------
C General program to display X,Y lists with/without error bars,
C with some basic arithmetic computations.
C Designed for galaxy profiles and shells, but works with ascii X,Y lists,
C Midas, FITS spectra (by calling READFILE).
C
C Version of 18-12-2010
C JLP 
C--------------------------------------------------------------------------
	PROGRAM PLOT1
        IMPLICIT NONE
        INTEGER*4 IDIM,KCUR,MAXLAB 
	PARAMETER (IDIM=100000,KCUR=30,MAXLAB=50)
C Declarations for labels (same as in PLOT1):
        CHARACTER LABEL(MAXLAB)*30
        REAL*4 XLABEL(MAXLAB),YLABEL(MAXLAB),ANGLE_LABEL(MAXLAB)
        INTEGER*4 NLABELS,IXOFF_LABEL,IYOFF_LABEL
C
	REAL*4 XX,YY,ERRORX,ERRORY,CMAG,CTE,CMULT,EXPAND
	REAL*4 XPLOT,YPLOT,ERPLOTX,ERPLOTY
        REAL*4 COEFF,XOUT,YOUT
	REAL*4 XX1(IDIM),YY1(IDIM),XX2(IDIM),YY2(IDIM)
	INTEGER*4 NPTS,IOPT,IOPX,IOPY,IOUT_TO_FIRST
        INTEGER*4 ICURVE_ENTERED,ICURVE_DISPLAYED,KK,NOUT
        INTEGER*4 NPT1,NPT2,I,JLP_AXES
        INTEGER*4 XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10
	CHARACTER IN_FILE*40,IN_COMMENTS*80
	CHARACTER CHAR1*40,CHAR2*40,CHAR3*40
	CHARACTER NCHAR*4,PCOLOR*30,ANS*1,PLOTDEV*32
        CHARACTER LABEL_FNAME*40,BUFFER*80
	LOGICAL ERROR_PLOT,CHECK_INPUT
 
	COMMON/BLOCKA/XX(IDIM,KCUR),YY(IDIM,KCUR),ERRORX(IDIM,KCUR),
     1  ERRORY(IDIM,KCUR)
	COMMON/BLOCKB/XPLOT(IDIM,KCUR),YPLOT(IDIM,KCUR),
     1	ERPLOTX(IDIM,KCUR),ERPLOTY(IDIM,KCUR),ERROR_PLOT,CHECK_INPUT
	COMMON/BLOCKC/KK,NPTS(KCUR),NCHAR(KCUR),PCOLOR(KCUR),CMAG(KCUR),
     1  CTE(KCUR),COEFF(KCUR),CMULT(KCUR)
 
C Common block with NEWPLOT :
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
C Labels
        COMMON/STR_LABELS/NLABELS,XLABEL,YLABEL,
     1        ANGLE_LABEL,LABEL,IXOFF_LABEL,IYOFF_LABEL
 
C Contains format with label=3333
	include 'jlpsub:jlp_graphic_help.for'
 
10	FORMAT(A)
        ICURVE_ENTERED=0
        ICURVE_DISPLAYED=0
        NLABELS=0

	PRINT 88
88	FORMAT(5X,' Program PLOT1',
     1	'    Version of 18-07-2007',/)
C
	PRINT 89
89	FORMAT(' Do you want me to type the values of the input',
     1	' files ? (N)')
	READ(5,10) ANS
	CHECK_INPUT=.FALSE.
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')CHECK_INPUT=.TRUE.

C Image format (in case of option 8 in READFILE):
        CALL JLP_BEGIN
        CALL JLP_INQUIFMT
C
	KK=0
	ERROR_PLOT=.FALSE.
12	WRITE(6,130)
130     FORMAT(' Menu :',/,
     1	' 1. Input of vectors (X,Y)',/,
     1	' 2. Input of a list (X,ERROR_X,Y,ERRROR_Y) with error bars',/,
     1	' 3. Input of a list of labels (in a file)')
        IF(ICURVE_ENTERED.EQ.1) WRITE(6,131)
131      FORMAT(' 4. Displaying the lists as they are',/,
     1	' 5. Displaying the lists after some changes (log,...)')
        IF(ICURVE_DISPLAYED.EQ.1) WRITE(6,132)
132     FORMAT(' 6. Resetting the number of curves to 0',/,
     1	' 7. Saving a curve in a file',/,
     1	' 8. Arithmetic on the curves')
        WRITE(6,133)
133     FORMAT(' 10. Exit',/,
     1	5X,' Enter the option you want: ')
	READ(5,*) IOPT

C------------------------------------------------------------------
C Without error bars
	IF(IOPT.EQ.1)THEN
	CALL RREADFILE(XX1,YY1,NPT1,XX2,YY2,NPT2,IDIM,
     1                 IN_FILE,IN_COMMENTS,0)
C XX(I,KK) AND YY(I,KK) POINT NUMBER I OF THE LINE KK
C NPTS(KK) NUMBER OF POINTS OF THE LINE KK
 
C Input of the first list :
	  KK=KK+1
	  NPTS(KK)=NPT1
          CALL INPUT_NCHAR_PCOLOR(NCHAR(KK),PCOLOR(KK))
	    DO I=1,NPT1
	      XX(I,KK)=XX1(I)
	      YY(I,KK)=YY1(I)
	    END DO
	   IF(CHECK_INPUT)THEN
	    DO I=1,NPT1
	      PRINT *,I,XX(I,KK),YY(I,KK)
	    END DO
	   ENDIF
 
C Input of the second list :
	IF(NPT2.NE.0)THEN
	  KK=KK+1
	  NPTS(KK)=NPT2
          CALL INPUT_NCHAR_PCOLOR(NCHAR(KK),PCOLOR(KK))
	    DO I=1,NPT2
	      XX(I,KK)=XX2(I)
	      YY(I,KK)=YY2(I)
	    END DO
	   IF(CHECK_INPUT)THEN
	    DO I=1,NPT2
	      PRINT *,I,XX(I,KK),YY(I,KK)
	    END DO
	   ENDIF
	ENDIF
 
        ICURVE_ENTERED=1

	GO TO 12
	ENDIF
 
C------------------------------------------------------------------
C Input with error bars
	IF(IOPT.EQ.2)THEN
 
	ERROR_PLOT=.TRUE.
	KK=KK+1
 
C Reading XX1, YY1, XX2=Error_X, YY2=Error_Y 
	CALL RREADFILE_ERRORS(XX1,YY1,XX2,YY2,NPT1,IDIM,
     1                 IN_FILE,IN_COMMENTS,2)
C	X(I,KK) AND Y(I,KK) POINT NUMBER I OF THE LINE KK
C	NPTS(KK) NUMBER OF POINTS OF THE LINE K
 
	NPTS(KK)=NPT1
	 DO I=1,NPT1
	   XX(I,KK)=XX1(I)
	   YY(I,KK)=YY1(I)
	   ERRORX(I,KK)=XX2(I)
	   ERRORY(I,KK)=YY2(I)
	 END DO
 
	 IF(CHECK_INPUT)THEN
	   DO I=1,NPT1
	     PRINT 508,I,XX(I,KK),YY(I,KK),ERRORX(I,KK),ERRORY(I,KK)
	   END DO
	 ENDIF
508	FORMAT(' I=',I4,'X,Y,ERROR X,ERROR Y:',4(F12.3,1X))
 
        CALL INPUT_NCHAR_PCOLOR(NCHAR(KK),PCOLOR(KK))
 
        ICURVE_ENTERED=1
	GO TO 12
	ENDIF
C------------------------------------------------------------------
C Input to display labels 
	IF(IOPT.EQ.3)THEN
          WRITE(6,301)
301       FORMAT(' Format of input file:',
     1           ' col#1=Xstart col#2=Ystart col#3=angle col#4=label')
          WRITE(6,302)
302       FORMAT(' Offset in X and Y to be added to label coordinates',
     1           ' (between 0 and 32000): ')
          READ(5,*) IXOFF_LABEL, IYOFF_LABEL
          CALL READ_LABELS(XLABEL,YLABEL,ANGLE_LABEL,LABEL,
     1                         NLABELS,MAXLAB,LABEL_FNAME)
C These data will be internally processed in NEWPLOT...
 
	GO TO 12
	ENDIF
C------------------------------------------------------------------
C Displaying the curves XPLOT and YPLOT :
C
	IF(IOPT.EQ.4.OR.IOPT.EQ.5)THEN
 
C Getting the good coordinates:
	IF(IOPT.EQ.4)THEN
          IOPX = 1
          IOPY = 1
        ELSE
          IOPX = 0
          IOPY = 0
        ENDIF
	CALL COORDINATE(IOPX,IOPY)
 
14	PRINT *,' TITLE ?'
	READ(5,10) CHAR3
C JLP2007: I write a '0' at the end (for C interface)
        WRITE(CHAR3,19) CHAR3(1:38),CHAR(0)
19      FORMAT(A,A)
        WRITE(6,*)CHAR3,'(OK)'
	PRINT *,' X AXIS LABEL ?'
	READ(5,10) CHAR1
        WRITE(CHAR1,19) CHAR1(1:38),CHAR(0)
	PRINT *,' Y AXIS LABEL ?'
	READ(5,10) CHAR2
        WRITE(CHAR2,19) CHAR2(1:38),CHAR(0)
	PRINT *,'JLP_axes, XGRID, YGRID are wanted, X and Y is log10,',
     1          ' expand (0,0,0,0,0,1.2)?'
	READ(5,10) BUFFER
        READ(BUFFER,*,ERR=90) JLP_AXES, XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,
     1          Y_IS_LOG10,EXPAND
        GOTO 91
90      EXPAND = 1.2 
        READ(BUFFER,*) JLP_AXES,XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,
     1          Y_IS_LOG10
91      PRINT *,' OK: EXPAND=', EXPAND
        PRINT *,' OK: BUFFER=', BUFFER 

C Displaying with NEWPLOT :
28	PRINT *,' Output device: ( &xterm, &square, ? for help)'
	READ(5,10) PLOTDEV
	IF((PLOTDEV(1:1).EQ.'?').OR.(PLOTDEV(1:1).EQ.' '))THEN
	  PRINT 3333
	  GO TO 28
	ENDIF
 
C If option 5 has been called at least once the option is
C forced to "NEWPLOT1" (contained in  "jlpsub0/newplot0_set1.for")
C Call at the end NEWPLOT21 in jlplib/jlp_splot_idv/jlp_newplot.cpp
 50	IF(.NOT.ERROR_PLOT)THEN
	  CALL NEWPLOT1(XPLOT,YPLOT,NPTS,IDIM,KK,CHAR1,CHAR2,
     1      CHAR3,NCHAR,PCOLOR,PLOTDEV,IN_FILE,IN_COMMENTS,
     1      JLP_AXES,XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,
     1      Y_IS_LOG10,EXPAND)
	ELSE
          print *,' Error plot'
	  CALL NEWPLOT1_ERROR(XPLOT,YPLOT,ERPLOTX,ERPLOTY,NPTS,IDIM,KK,
     1	    CHAR1,CHAR2,CHAR3,NCHAR,PCOLOR,PLOTDEV,IN_FILE,IN_COMMENTS,
     1      JLP_AXES,XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,
     1      Y_IS_LOG10,EXPAND)
	ENDIF
 
	IF(NOUT.NE.0)THEN
	PRINT *,' ',NOUT,' points received'
	  DO I=1,NOUT
	  PRINT 100,I,XOUT(I),YOUT(I)
	  END DO
100	FORMAT(' POINT:',I5,' X=',F12.3,' Y=',F12.3)
	ENDIF
 
	PRINT *,' Do you want to change the parameters of',
     1	' the frame ?(N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GO TO 50
 
        ICURVE_DISPLAYED=1
	GO TO 12
	ENDIF
C------------------------------------------------------------------
	IF(IOPT.EQ.6)THEN
          KK=0
          ERROR_PLOT=.FALSE.
          GO TO 12
        ENDIF
C------------------------------------------------------------------
	IF(IOPT.EQ.7)THEN
	  CALL OUTPUT_CURVE
	  GO TO 12
	ENDIF
C------------------------------------------------------------------
	IF(IOPT.EQ.8)THEN
	  CALL PLOT1_ARITH(IOUT_TO_FIRST)
	  GO TO 12
	ENDIF
C------------------------------------------------------------------
9999    CALL JLP_END	
        STOP
	END
 
C********************************************************************
C Subroutine performing operations on X,Y for the display
C********************************************************************
	SUBROUTINE COORDINATE(IOPX,IOPY)
        IMPLICIT NONE
        INTEGER*4 IDIM,KCUR
	PARAMETER (IDIM=100000,KCUR=30)
	REAL*4 XX,YY,CMAG,CTE,CMULT,XPLOT,YPLOT,ERPLOTX,ERPLOTY
        REAL*4 ERRORX,ERRORY,COEFF,WORK
        INTEGER*4 IOPX,IOPY,I,K,KK,NPTS
	CHARACTER NCHAR*4,PCOLOR*30
	LOGICAL ERROR_PLOT,CHECK_INPUT
 
	COMMON/BLOCKA/XX(IDIM,KCUR),YY(IDIM,KCUR),ERRORX(IDIM,KCUR),
     1  ERRORY(IDIM,KCUR)
	COMMON/BLOCKB/XPLOT(IDIM,KCUR),YPLOT(IDIM,KCUR),
     1	ERPLOTX(IDIM,KCUR),ERPLOTY(IDIM,KCUR),ERROR_PLOT,CHECK_INPUT
	COMMON/BLOCKC/KK,NPTS(KCUR),NCHAR(KCUR),PCOLOR(KCUR),CMAG(KCUR),
     1  CTE(KCUR),COEFF(KCUR),CMULT(KCUR)
C----------------------------------------------------------------
C?
        IF(IOPX.LT.1.OR.IOPX.GT.4)THEN
	   PRINT *,' Do you want for the X axis :'
	   PRINT *,' 1.X, 2.LOG10(X-CTE1), 3.X**.25, 4.X**4 5.C1*(X-CTE1) ?'
	   READ(5,*)  IOPX
        ENDIF
C----------------------------------------------------------------
	IF(IOPX.EQ.1)THEN
	  DO K=1,KK
	    DO I=1,NPTS(K)
	      XPLOT(I,K)=XX(I,K)
	    END DO
	  END DO
	ENDIF
C----------------------------------------------------------------
	IF(IOPX.EQ.2)THEN
	   PRINT *,' Constant you want to subtract from the X values'
	   DO K=1,KK
	     PRINT 705,K
	     READ(5,*) CTE(K)
	   END DO
	DO K=1,KK
	  DO I=1,NPTS(K)
            XPLOT(I,K)=XX(I,K)-CTE(K)
	    IF(XPLOT(I,K).GT.0.)THEN
	      XPLOT(I,K)=ALOG10(XPLOT(I,K))
	    ELSE
	      XPLOT(I,K)=-10.
	    ENDIF
	  END DO
	END DO
	ENDIF
C----------------------------------------------------------------
	IF(IOPX.EQ.3)THEN
	  DO K=1,KK
	    DO I=1,NPTS(K)
	      IF(XX(I,K).GT.0.)THEN
	        XPLOT(I,K)=XX(I,K)**0.25
	      ELSE
	        XPLOT(I,K)=0.
	      ENDIF
	    END DO
	  END DO
	ENDIF
C----------------------------------------------------------------
	IF(IOPX.EQ.4)THEN
	  DO K=1,KK
	    DO I=1,NPTS(K)
	      XPLOT(I,K)=XX(I,K)**4
	    END DO
	  END DO
	ENDIF
C----------------------------------------------------------------
	IF(IOPX.EQ.5)THEN
	   PRINT *,' C1 and CTE values:    C1 * (X - CTE)'
	   DO K=1,KK
	     PRINT 705,K
	     READ(5,*) CMULT(K),CTE(K)
	   END DO
	DO K=1,KK
	  DO I=1,NPTS(K)
            XPLOT(I,K)=CMULT(K)*(XX(I,K)-CTE(K))
	  END DO
	END DO
	ENDIF
C*****************************************************************
C?
        IF(IOPY.LT.1.OR.IOPY.GT.5)THEN
	  PRINT 703
703	  FORMAT(' Do you want for the Y axis:',/,
     1	' 1. Y',/,' 2. CMULT * (Y-CTE)',/,
     1	' 3. LOG10(Y-CTE)',/,
     1	' 4. CMAG + COEFF*LOG10(Y-CTE)',/,
     1	' 5. CTE + 10**(-0.4*(Y-CMAG))',/,
     1	5X,' Enter the option you want: ')
	  READ(5,*) IOPY
         ENDIF
 
	IF(IOPY.NE.1.AND.IOPY.NE.2)THEN
	   PRINT *,' Constant you want to subtract from the Y values'
	   DO K=1,KK
	     PRINT 705,K
705	     FORMAT(' For curve #',I3,' :== ',$)
	     READ(5,*) CTE(K)
	   END DO
	ENDIF

	IF(IOPY.EQ.2)THEN
	   PRINT *,' CMULT and CTE values:'
	   DO K=1,KK
	     PRINT 706,K
706	     FORMAT(' For curve #',I3,' CMULT, CTE :== ',$)
	     READ(5,*) CMULT(K), CTE(K)
	   END DO
	ENDIF
 
	IF(IOPY.GE.4)THEN
	  PRINT *,' Enter CMAG, and COEFF (-2.5 ?):'
	  DO K=1,KK
	    PRINT 705,K
	    READ(5,*) CMAG(K),COEFF(K)
	  END DO	
	ENDIF
 
C----------------------------------------------------------------
C Option 1 : YPLOT=Y
C
	IF(IOPY.EQ.1)THEN
 
	DO K=1,KK
	DO I=1,NPTS(K)
	   YPLOT(I,K)=YY(I,K)
	END DO
	END DO
 
C Transfer of ERROR if option 5 has been called at least once.
	IF(ERROR_PLOT)THEN
	 DO K=1,KK
	  DO I=1,NPTS(K)
	   ERPLOTX(I,K)=ERRORX(I,K)
	   ERPLOTY(I,K)=ERRORY(I,K)
	  END DO
	 END DO
	ENDIF
 
	ENDIF
C----------------------------------------------------------------
C Option 2 : YPLOT=CMULT*(Y-CTE)
C
       IF(IOPY.EQ.2)THEN
        DO 546 K=1,KK
         DO 546 I=1,NPTS(K)
          YPLOT(I,K)=CMULT(K) * (YY(I,K)-CTE(K))
546      CONTINUE
 
C Transfer of ERROR if option 5 has been called at least once.
	IF(ERROR_PLOT)THEN
	  DO K=1,KK
	   DO I=1,NPTS(K)
	    ERPLOTX(I,K)=ERRORX(I,K)
	    ERPLOTY(I,K)=ERRORY(I,K)
	   END DO
	  END DO
	ENDIF
 
	ENDIF
	
C----------------------------------------------------------------
	IF(IOPY.EQ.3)THEN
	  DO K=1,KK
	    DO I=1,NPTS(K)
	      WORK=YY(I,K)-CTE(K)
	       IF(WORK.LE.0.)THEN
	        YPLOT(I,K)=-10.
	       ELSE
	        YPLOT(I,K)=ALOG10(WORK)
	       ENDIF
	    END DO
	  END DO
	ENDIF
C----------------------------------------------------------------
	IF(IOPY.EQ.4)THEN
 
	DO K=1,KK
	  DO I=1,NPTS(K)
	  WORK=YY(I,K)-CTE(K)
	    IF(WORK.LE.0.)THEN
	     YPLOT(I,K)=32.
	    ELSE
	     YPLOT(I,K)=CMAG(K)+COEFF(K)*ALOG10(WORK)
	    ENDIF
	  END DO
	END DO
 
	ENDIF
C----------------------------------------------------------------
	IF(IOPY.EQ.5)THEN
 
	DO K=1,KK
	 DO I=1,NPTS(K)
	    WORK=-0.4*(YY(I,K)-CMAG(K))
	   IF(WORK.LT.-10..OR.WORK.GT.10.)THEN
	    YPLOT(I,K)=0.
	   ELSE
	    YPLOT(I,K)=CTE(K)+10**WORK
	   ENDIF
	 END DO
	END DO
 
	ENDIF
C----------------------------------------------------------------
	RETURN
	END
C********************************************************************
C Subroutine performing arithmetic operations on the curves
C 
C IOUT_TO_FIRST: set to one if output curve has been transferred 
C to first curve
C********************************************************************
	SUBROUTINE PLOT1_ARITH(IOUT_TO_FIRST)
        IMPLICIT NONE
        INTEGER*4 IDIM,KCUR
	PARAMETER (IDIM=100000,KCUR=30)
	REAL*4 XX,YY,CMAG,CTE,CMULT,XPLOT,YPLOT,ERPLOTX,ERPLOTY
        REAL*4 ERRORX,ERRORY,COEFF,YMIN0
        INTEGER*4 IOUT_TO_FIRST,ISTATUS,NC1,NC2
        INTEGER*4 K,KK,NPTS,IOP6,I,NPT0
	CHARACTER NCHAR*4,PCOLOR*30
	LOGICAL ERROR_PLOT,CHECK_INPUT
 
	COMMON/BLOCKA/XX(IDIM,KCUR),YY(IDIM,KCUR),ERRORX(IDIM,KCUR),
     1  ERRORY(IDIM,KCUR)
	COMMON/BLOCKB/XPLOT(IDIM,KCUR),YPLOT(IDIM,KCUR),
     1	ERPLOTX(IDIM,KCUR),ERPLOTY(IDIM,KCUR),ERROR_PLOT,CHECK_INPUT
	COMMON/BLOCKC/KK,NPTS(KCUR),NCHAR(KCUR),PCOLOR(KCUR),CMAG(KCUR),
     1  CTE(KCUR),COEFF(KCUR),CMULT(KCUR)
 
C----------------------------------------------------------------
C IOUT_TO_FIRST: set to one if output curve has been transferred 
C to first curve
        IOUT_TO_FIRST=0
C
66	PRINT 67
67	FORMAT(' Menu ;',/,' WARNING : OPTIONS 1,2 OR 3',
     1	' ARE ONLY POSSIBLE ',
     1	' WHEN THE CURVES ARE SAMPLED IN THE SAME WAY (OPTION 4)',/,/,
     1	' 1. Subtraction of two curves and',
     1	' output in curve #1',/,
     1	' 2. Division of two curves and',
     1	' output in curve #1',/,
     1	' 3. Minimum of all the curves and',
     1	' output in curve #1',/,
     1	' 4. Resampling a curve',/,
     1	' 10. Return to the main menu',/,
     1	5X,' Enter the option you want: ')
	READ(5,*) IOP6

C----------------------------------------------------------------
C IOP6=1
C Difference of two curves :
C----------------------------------------------------------------
	IF(IOP6.EQ.1)THEN
	PRINT 68
68	FORMAT(' Enter the entry number of the',
     1	' 2 curves (output=first-second)')
	READ(5,*) NC1,NC2
	
	NPT0=MIN(NPTS(NC1),NPTS(NC2))
        NPTS(1)=NPT0
	DO I=1,NPT0
	 IF(XPLOT(I,NC1).EQ.XPLOT(I,NC2))THEN
	  XPLOT(I,1)=XPLOT(I,NC1)
	  YPLOT(I,1)=YPLOT(I,NC1)-YPLOT(I,NC2)
	 ELSE
	  PRINT *,' ERROR : NOT SAMPLED IN THE SAME WAY'
	  PRINT *,' I, X1, X2 :',I,XPLOT(I,NC1),XPLOT(I,NC2)
	  GO TO 66
	 ENDIF
	END DO
 
        IOUT_TO_FIRST=1
	ENDIF
 
C----------------------------------------------------------------
C IOP6=2
C Division of two curves :
C----------------------------------------------------------------
	IF(IOP6.EQ.2)THEN
	PRINT 69
69	FORMAT(' Enter the entry number of the',
     1	' 2 curves (output=first/second)')
	READ(5,*) NC1,NC2
	
	NPT0=MIN(NPTS(NC1),NPTS(NC2))
        NPTS(1)=NPT0
	DO I=1,NPT0
	 IF(XPLOT(I,NC1).EQ.XPLOT(I,NC2))THEN
	  XPLOT(I,1)=XPLOT(I,NC1)
	   IF(YPLOT(I,NC2).EQ.0)YPLOT(I,NC2)=0.1E-10
	  YPLOT(I,1)=YPLOT(I,NC1)/YPLOT(I,NC2)
	 ELSE
	  PRINT *,' Error: not sampled in the same way'
	  PRINT *,' I, X1, X2 :',I,XPLOT(I,NC1),XPLOT(I,NC2)
	  GO TO 66
	 ENDIF
	END DO
 
        IOUT_TO_FIRST=1
	ENDIF
 
C----------------------------------------------------------------
C IOP6=3
C Minimum of all curves :
C----------------------------------------------------------------
	IF(IOP6.EQ.3)THEN
	PRINT *,' Output: minimum of ',KK,' curves in curve #1'
 
C Looking for the number of points :	
	NPT0=NPTS(1)
	 DO K=2,KK
	  NPT0=MIN(NPTS(K),NPT0)
	 END DO
	NPTS(1)=NPT0
 
C Computing the minimum :
	DO I=2,NPT0
	  YMIN0=YPLOT(I,1)
	   DO K=1,KK
	    YMIN0=AMIN1(YPLOT(I,K),YMIN0)
	   END DO
	  YPLOT(I,1)=YMIN0
	END DO
 
        IOUT_TO_FIRST=1
	ENDIF
 
C----------------------------------------------------------------
C IOP6=4
C Resampling a curve
C----------------------------------------------------------------
	IF(IOP6.EQ.4)THEN
401	  PRINT 63
63 	  FORMAT('Resampling a curve in x ',/,
     1	'Enter the entry numbers of the',
     1	' 2 curves (curve to resample and reference):')
	  READ(5,*) NC1,NC2
	
	  PRINT *,' OK: Output in curve #',NC1

C Linear interpolation: 
          CALL RESAMPLE_CURVE(XX,YY,XPLOT,YPLOT,NPTS,IDIM,
     1                         NC1,NC2,ISTATUS)

	  GO TO 66
	ENDIF
 
C Return to main menu: 
        IF(IOUT_TO_FIRST.EQ.1)THEN
C Store output curve to XX,YY arrays:
           DO I=1,NPTS(1)
             XX(I,1)=XPLOT(I,1)
             YY(I,1)=YPLOT(I,1)
           ENDDO
C Set the number of curves to 1:
           KK=1 
        ENDIF
 
	RETURN
	END
C********************************************************************
C Subroutine to resample a curve
C Performs spline interpolation, by calling NAG library routines
C
C INPUT:
C XPLOT(*,NC1),YPLOT(*,NC1)
C XPLOT(*,NC2),YPLOT(*,NC2)
C NC1: index of curve to be resampled 
C NC2: index of reference curve
C
C OUTPUT:
C XPLOT(*,NC1),YPLOT(*,NC1)
C
C Not used any longer (and no tested either, in this version...)
C********************************************************************
        SUBROUTINE RESAMPLE_CURVE1(XX,YY,XPLOT,YPLOT,NPTS,IDIM,
     1                              NC1,NC2,ISTATUS)
        IMPLICIT NONE
        INTEGER*4 IDIM1
	PARAMETER (IDIM1=100000)
        INTEGER*4 NPTS(*),NC1,NC2,IDIM
        REAL*4 XPLOT(IDIM,*),YPLOT(IDIM,*),XX(IDIM,*),YY(IDIM,*)
	REAL*8 XABSCISS,RESULT
	REAL*8 X1(IDIM1),Y1(IDIM1),SPLINE1(IDIM1),K1(IDIM1)
        INTEGER*4 I,IFAIL,ISTATUS,ISAFE,IMAX

        ISTATUS=0
	ISAFE=0
	   DO I=1,NPTS(NC1)
	    X1(I)=XPLOT(I,NC1)
	    Y1(I)=YPLOT(I,NC1)
	   END DO
C1991	  CALL SPLINE_INTER(X1,Y1,NPTS(NC1),SPLINE1,NCAP71,K1)	
 
C Generation of the resampled curve :
	  CALL INDEX_MAX4(XPLOT(1,NC2),NPTS(NC2),
     1	XPLOT(NPTS(NC1),NC1),IMAX)
	  NPTS(NC1)=IMAX
	  DO I=1,NPTS(NC1)
	    X1(I)=XPLOT(I,NC2)
	    XABSCISS=XPLOT(I,NC1)
C Calling NAG routine E02BBF:
	    IFAIL=1
C1991	    CALL E02BBF(NCAP71,K1,SPLINE1,XABSCISS,RESULT,IFAIL)
	     IF(IFAIL.NE.0)THEN
	      PRINT 97,IFAIL,I
97	      FORMAT(' E02BBF : Failure in E02BBF, IFAIL=',I5,/,
     1	' AT POINT #',I5)
C If error we prompt for another try :
	      IF(ISAFE.EQ.0)THEN
	        PRINT 98,NC2,NC1,NC1,NC2
98		FORMAT(' RESAMPLE_CURVE1/Fatal error, try again with ',I3,1X,I3,
     1	' instead of ',I3,1X,I3)
	        ISAFE=1
                RETURN
	      ELSE
C If error again, we give up:
	        PRINT *,'RESAMPLE_CURVE1/Sorry, the problem was harder...'
                RETURN
	      ENDIF
	     ENDIF
	    Y1(I)=RESULT
	  END DO
 
C Store output curve to XPLOT,YPLOT arrays:
          DO I=1,NPTS(NC1)
            XPLOT(I,NC1)=X1(I)
            YPLOT(I,NC1)=Y1(I)
          ENDDO

        RETURN
        END
C********************************************************************
C Subroutine to resample a curve
C Performs linear interpolation
C
C INPUT:
C XPLOT(*,NC1),YPLOT(*,NC1)
C XPLOT(*,NC2),YPLOT(*,NC2)
C NC1: index of curve to be resampled 
C NC2: index of reference curve
C
C OUTPUT:
C XPLOT(*,NC1),YPLOT(*,NC1)
C
C********************************************************************
        SUBROUTINE RESAMPLE_CURVE(XX,YY,XPLOT,YPLOT,NPTS,IDIM,
     1                            NC1,NC2,ISTATUS)
        INTEGER*4 NPTS(*),NC1,NC2,IDIM,ISTATUS
        REAL*4 XPLOT(IDIM,*),YPLOT(IDIM,*),XX(IDIM,*),YY(IDIM,*)
        REAL*4 XLOC,DELTAX,DELTAY
        INTEGER*4 I,I1,I2

        DO I=1,NPTS(NC2)
          XLOC=XPLOT(I,NC2)
C Calling INDEX_MAX4(ARRAY,NPOINT,VALUE,INDEX) in newplot0_set2.for...
C (Search for the higher INDEX verifying ARRAY(INDEX).LE.VALUE)
          CALL INDEX_MAX4(XPLOT(1,NC1),NPTS(NC1),XLOC,I1)
          I2=MIN(NPTS(NC1),I1+1)
C Perform linear interpolation:
          DELTAY=YPLOT(I2,NC1)-YPLOT(I1,NC1)
          DELTAX=XPLOT(I2,NC1)-XPLOT(I1,NC1)
          IF(DELTAX.NE.0.)THEN
             YY(I,NC1)=YPLOT(I1,NC1)
     1                +(XLOC-XPLOT(I1,NC1))*DELTAY/DELTAX
          ELSE
             YY(I,NC1)=YPLOT(I1,NC1)
          ENDIF
C Debug:
          IF(I.GT.400.AND.I.LT.450)THEN
            PRINT *,'XPLOT(I,NC2),XPLOT(I1,NC1)'
            PRINT *,XPLOT(I,NC2),XPLOT(I1,NC1)
            PRINT *,'I,I1,I2,YY(I,NC1),YPLOT(I1,NC1)'
            PRINT *,I,I1,I2,YY(I,NC1),YPLOT(I1,NC1)
          ENDIF
C Transfer the new X value to output curve:
          XX(I,NC1)=XLOC
        END DO

C Transfer the new NPTS value to output curve:
        NPTS(NC1)=NPTS(NC2)

C Store output curve to XPLOT,YPLOT arrays:
          DO I=1,NPTS(NC1)
            XPLOT(I,NC1)=XX(I,NC1)
            YPLOT(I,NC1)=YY(I,NC1)
          ENDDO

	RETURN
	END
C********************************************************************
C Subroutine to output a curve from XPLOT,YPLOT
C********************************************************************
	SUBROUTINE OUTPUT_CURVE
	PARAMETER (IDIM=100000,KCUR=30)
	REAL*8 XXWORK,YYWORK
	REAL*4 XX,YY,CMAG,CTE,CMULT,XPLOT,YPLOT,ERPLOTX,ERPLOTY
        REAL*4 ERRORX,ERRORY
	CHARACTER NCHAR*4,PCOLOR*30
	CHARACTER NAME*30
	LOGICAL ERROR_PLOT,CHECK_INPUT
	COMMON/BLOCKA/XX(IDIM,KCUR),YY(IDIM,KCUR),ERRORX(IDIM,KCUR),
     1  ERRORY(IDIM,KCUR)
	COMMON/BLOCKB/XPLOT(IDIM,KCUR),YPLOT(IDIM,KCUR),
     1	ERPLOTX(IDIM,KCUR),ERPLOTY(IDIM,KCUR),ERROR_PLOT,CHECK_INPUT
	COMMON/BLOCKC/KK,NPTS(KCUR),NCHAR(KCUR),PCOLOR(KCUR),CMAG(KCUR),
     1  CTE(KCUR),COEFF(KCUR),CMULT(KCUR)
 
10	FORMAT(A)
 
	PRINT *,' NUMBER OF THE CURVE YOU WANT TO OUTPUT ?'
	READ(5,*) KK1
807	PRINT *,' NAME OF THE OUTPUT FILE ?'
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='NEW',ACCESS='SEQUENTIAL',
     1	ERR=807)
	WRITE(1,*)NPTS(KK1)
	 DO I=1,NPTS(KK1)
	  XXWORK=DBLE(XPLOT(I,KK1))
	  YYWORK=DBLE(YPLOT(I,KK1))
	  WRITE(1,*)XXWORK,YYWORK
	 END DO
	CLOSE(1)
 
	RETURN
	END
C-----------------------------------------------------------------
        SUBROUTINE READ_LABELS(XLABEL,YLABEL,ANGLE_LABEL,LABEL,
     1                         NLABELS,MAXLAB,LABEL_FNAME)
        REAL*4 XLABEL(*),YLABEL(*),ANGLE_LABEL(*)
        INTEGER*4 NLABELS,MAXLAB
        CHARACTER LABEL(MAXLAB)*30,BUFFER*100,BUFFER1*100
        CHARACTER LABEL_FNAME*(*)
10      FORMAT(A)
        WRITE(6,20)
20      FORMAT(' File with labels :')
        READ(5,10) LABEL_FNAME
        OPEN(3,FILE=LABEL_FNAME,STATUS='OLD')
        NLABELS=0
        DO I=1,MAXLAB
          READ(3,10,ERR=67,END=67) BUFFER 
          IF(BUFFER(1:1).NE.'#')THEN
            IMAX=100
            CALL JLP_FREEFORMAT_R(BUFFER,IMAX,XLABEL(I),ISTAT)
            CALL JLP_FREEFORMAT_R(BUFFER,IMAX,YLABEL(I),ISTAT)
            CALL JLP_FREEFORMAT_R(BUFFER,IMAX,ANGLE_LABEL(I),ISTAT)
            CALL JLP_FREEFORMAT_C(BUFFER,IMAX,LABEL(I),ISTAT)
C Debug:
            WRITE(6,*) XLABEL(I),YLABEL(I),ANGLE_LABEL(I),LABEL(I)
          ELSE
            WRITE(6,30) BUFFER 
30          FORMAT(' Comments: ',A)
          ENDIF
        END DO
67      NLABELS=I-1
        WRITE(6,40) NLABELS
40      FORMAT(' OK: ',I5,'labels read') 
        RETURN
        END
C-----------------------------------------------------------------
C To read free formatted real value
C
C Input: buffer like   "  34.5 65.4 67.3"
C Output: value=34.5 buffer: " 65.4 67.3"
C-----------------------------------------------------------------
        SUBROUTINE JLP_FREEFORMAT_R(BUFFER,IMAX,VALUE,ISTAT)
        CHARACTER BUFFER*(*)
        INTEGER*4 IMAX,ISTAT,I,J,I1

        ISTAT=0

C Look for the limits of the first blank area:
        DO I=1,IMAX
          IF(BUFFER(I:I).NE.' ')GOTO 20
        ENDDO

C Else error: empty buffer
        WRITE(6,12)
12      FORMAT(' JLP_FREEFORMAT_R/Error: empty buffer')
        ISTAT=-1
        RETURN

20      I1=I-1 
        IMAX=IMAX-I1
C Shift to the left:
        IF(I1.GT.0)THEN
          DO I=1,IMAX
            J=I1+I
            BUFFER(I:I)=BUFFER(J:J)
          END DO
        ENDIF

C Read data:
        READ(BUFFER,*) VALUE

C Look for next blank:
        DO I=1,IMAX
          IF(BUFFER(I:I).EQ.' ')GOTO 30
        ENDDO

30      I1=I-1 
        IMAX=IMAX-I1
C Shift to the left:
        IF(I1.GT.0)THEN
          DO I=1,IMAX
            J=I1+I
            BUFFER(I:I)=BUFFER(J:J)
          END DO
        ENDIF

        RETURN
        END
C-----------------------------------------------------------------
C To read character string 
C
C Input: buffer like   "    Bonjour this is an example" 
C Output: value="Bonjour this is an example"
C-----------------------------------------------------------------
        SUBROUTINE JLP_FREEFORMAT_C(BUFFER,IMAX,CVALUE,ISTAT)
        IMPLICIT NONE
        CHARACTER BUFFER*(*),CVALUE*(*)
        INTEGER*4 IMAX,ISTAT,I,J,I1

        ISTAT=0

C Look for the limits of the first blank area:
        DO I=1,IMAX
          IF(BUFFER(I:I).NE.' ')GOTO 20
        ENDDO

C Else error: empty buffer
        WRITE(6,12)
12      FORMAT(' JLP_FREEFORMAT_C/Error: empty buffer')
        CVALUE=' '
        ISTAT=-1
        RETURN

20      I1=I-1 
        IMAX=IMAX-I1
C Shift to the left:
        IF(I1.GT.0)THEN
          DO I=1,IMAX
            J=I1+I
            BUFFER(I:I)=BUFFER(J:J)
          END DO
        ENDIF

        CVALUE=BUFFER
        RETURN
        END
C**********************************************************************
C Prompt NCHAR and PCOLOR to the user
C
C OUTPUT:
C  NCHAR0: symbol type and size
C  PCOLOR0: color for the symbols or for the lines
C**********************************************************************
       SUBROUTINE INPUT_NCHAR_PCOLOR(NCHAR0, PCOLOR0)
       CHARACTER NCHAR0*4,PCOLOR0*30,BUFFER*80
       INTEGER I,ISAFE
10     FORMAT(A)
C Contains format with label=3334
	include 'jlpsub:jlp_graphic_help.for'
       
27     PRINT *,'TYPE OF CHARACTER WITH SIZE, AND COLOR (? for help)'
	READ(5,10) BUFFER 
	  IF((BUFFER.EQ.'?').OR.(BUFFER.EQ.' '))THEN
	    PRINT 3334
	    GO TO 27
          ELSE
            I=INDEX(BUFFER,',')
            IF(I.LE.1)THEN
              NCHAR0=BUFFER
              PCOLOR0='Default'
            ELSE
              NCHAR0=BUFFER(1:I-1)
              PCOLOR0=BUFFER(I+1:)
            ENDIF
	  ENDIF
        RETURN
        END
C-----------------------------------------------------------------
c	include 'jlpsub:gene_ellipse.for'
C Contains SPLINE_INTER :
c	include 'jlpsub:project.for'
