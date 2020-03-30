C++*****************************************************************
C Set of subroutines to draw multiple curves X1,Y1,X2,Y2,... 
C with possibility of error bars. 
C UNIX version.
C
C Only one package is possible :
C SPLOT (calling NEWPLOT21)
C
C Contains:
C NEWPLOT, NEWPLOT_ERROR, NEWPLOT0, WINDOW_LIMITS
C
C JLP
C Version of 10-01-2012
C--*****************************************************************
C 	SUBROUTINE NEWPLOT_ERROR(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,CHAR1,
C     1	  CHAR2,CHAR3,NCHAR,PCOLOR,PLOTDEV,FILENAME,COMMENTS,JLP_AXES,
C     1   XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10)
C INPUT:
C	X AND Y : arrays of the curves to display (REAL*4)
C	ERRX : errors in X to be displayed (REAL*4)
C	ERRY : errors in Y to be displayed (REAL*4)
C	NPTS : array with the number of points for each curve
C	NMAX : maxi. number of points (declaration of the arrays)
C	KCURVE : number of curves (declaration of the arrays)
C	CHAR1 : X AXIS title
C	CHAR2 : Y AXIS title (IF CHAR(30:30).EQ.'H' : MONGO HISTO ???)
C	CHAR3 : general title of the graph
C	NCHAR : array of the symbols for the different curves
C       PCOLOR : color to be used for drawing the curves
C	PLOTDEV : output plotting device
C       FILENAME,COMMENTS: just for caption in case of hardcopy
C
C OUTPUT:
C	XOUT(200), YOUT(200) : ARRAYS OF THE POINTS
C				            ENTERED WITH THE CURSOR
C       NOUT : Number of points entered with the cursor
C	(in common block STR_OUTPUT).
C
C OTHERS:
C       TDX, TDY: Scale to transform user coordinates to window
C                 coordinates (between 0 and 32000 with mongo... OTHERS:
C Code:
C  NEWPLOT:$:1,PGPLOT:*:2,MONGO_GKS:#:3,SMONGO:&:4,MONGO_87:%:5
C*****************************************************************
	SUBROUTINE NEWPLOT_ERROR(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,CHAR1,
     1   CHAR2,CHAR3,NCHAR,PCOLOR,PLOTDEV,FILENAME,COMMENTS)
        IMPLICIT NONE
        INTEGER*4 NMAX,KCURVE,JLP_AXES
        INTEGER*4 XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10
	REAL*4 X(NMAX,*),Y(NMAX,*),ERRX(NMAX,*),ERRY(NMAX,*),EXPAND
	CHARACTER PLOTDEV*40
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40
        CHARACTER FILENAME*60,COMMENTS*80,NCHAR(*)*4,PCOLOR(*)*30
	INTEGER*4 NPTS(*), ERROR_BARS
	  ERROR_BARS = 1
          JLP_AXES = 0
          XGRID_IS_WANTED = 0
          YGRID_IS_WANTED = 0
          X_IS_LOG10 = 0
          Y_IS_LOG10 = 0
          EXPAND = 1.2
	  CALL NEWPLOT0(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,CHAR1,
     1	 CHAR2,CHAR3,NCHAR,PCOLOR,PLOTDEV,ERROR_BARS,FILENAME,COMMENTS,
     1   JLP_AXES,XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10,
     1   EXPAND)
	RETURN
	END
C********************************************************************** 
C NEWPLOT1_ERROR:
C Same as NEWPLOT_ERROR, but with more input parameters:
C  XGRID_IS_WANTED,YGRID_IS_WANTED = 1 if underlying grid is wanted
C  X_IS_LOG10,Y_IS_LOG10 = 1 if axes are to be displayed in logarithmic form
C********************************************************************** 
	SUBROUTINE NEWPLOT1_ERROR(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,CHAR1,
     1   CHAR2,CHAR3,NCHAR,PCOLOR,PLOTDEV,FILENAME,COMMENTS,JLP_AXES,
     1   XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10,EXPAND)
        IMPLICIT NONE
        INTEGER*4 NMAX,KCURVE,JLP_AXES
        INTEGER*4 XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10
	REAL*4 X(NMAX,*),Y(NMAX,*),ERRX(NMAX,*),ERRY(NMAX,*)
        REAL*4 EXPAND
	CHARACTER PLOTDEV*40
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40
        CHARACTER FILENAME*60,COMMENTS*80,NCHAR(*)*4,PCOLOR(*)*30
	INTEGER*4 NPTS(*), ERROR_BARS
	  ERROR_BARS=1
	  CALL NEWPLOT0(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,CHAR1,
     1	 CHAR2,CHAR3,NCHAR,PCOLOR,PLOTDEV,ERROR_BARS,FILENAME,COMMENTS,
     1   JLP_AXES,XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10,
     1   EXPAND)
	RETURN
	END
 
C********************************************************************** 
C NEWPLOT: same as NEWPLOT_ERROR but without error bars:
C********************************************************************** 
	SUBROUTINE NEWPLOT(X,Y,NPTS,NMAX,KCURVE,CHAR1,CHAR2,
     1	 CHAR3,NCHAR,PCOLOR,PLOTDEV,FILENAME,COMMENTS)
        IMPLICIT NONE
        INTEGER*4 NMAX,KCURVE,JLP_AXES
        INTEGER*4 XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10
	REAL*4 X(NMAX,*),Y(NMAX,*),EXPAND
	CHARACTER PLOTDEV*40
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40,PCOLOR(*)*30
        CHARACTER FILENAME*60,COMMENTS*80,NCHAR(*)*4
	INTEGER*4 NPTS(*), ERROR_BARS
	REAL*4 ERRX(1),ERRY(1)
	  ERROR_BARS=0
          JLP_AXES=0
          XGRID_IS_WANTED = 0
          YGRID_IS_WANTED = 0
          X_IS_LOG10 = 0
          Y_IS_LOG10 = 0
          EXPAND = 1.2
	  CALL NEWPLOT0(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,CHAR1,
     1	 CHAR2,CHAR3,NCHAR,PCOLOR,PLOTDEV,ERROR_BARS,FILENAME,COMMENTS,
     1   JLP_AXES,XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10,
     1   EXPAND)
	RETURN
	END
C********************************************************************** 
C NEWPLOT1:
C Same as NEWPLOT, but with more input parameters:
C  JLP_AXES = 1 for new box (compatible with grid)
C  XGRID_IS_WANTED,YGRID_IS_WANTED = 1 if underlying grid is wanted
C  X_IS_LOG10,Y_IS_LOG10 = 1 if axes are to be displayed in logarithmic form
C********************************************************************** 
	SUBROUTINE NEWPLOT1(X,Y,NPTS,NMAX,KCURVE,CHAR1,CHAR2,
     1	 CHAR3,NCHAR,PCOLOR,PLOTDEV,FILENAME,COMMENTS,JLP_AXES,
     1   XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10,EXPAND)
        IMPLICIT NONE
        INTEGER*4 NMAX,KCURVE,JLP_AXES
        INTEGER*4 XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10
	REAL*4 X(NMAX,*),Y(NMAX,*),EXPAND
	CHARACTER PLOTDEV*40
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40,PCOLOR(*)*30
        CHARACTER FILENAME*60,COMMENTS*80,NCHAR(*)*4
	INTEGER*4 NPTS(*), ERROR_BARS
	REAL*4 ERRX(1),ERRY(1)
	  ERROR_BARS=0
	  CALL NEWPLOT0(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,CHAR1,
     1	 CHAR2,CHAR3,NCHAR,PCOLOR,PLOTDEV,ERROR_BARS,FILENAME,COMMENTS,
     1   JLP_AXES,XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10,
     1   EXPAND)
	RETURN
	END
 
C**********************************************************************
C Main routine:
C**********************************************************************
	SUBROUTINE NEWPLOT0(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,CHAR1,
     1   CHAR2,CHAR3,NCHAR,PCOLOR,PLOTDEV,ERROR_BARS,FILENAME,COMMENTS,
     1   JLP_AXES,XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10,
     1   EXPAND)
        IMPLICIT NONE
        INTEGER MAXLAB,NMAX,KCURVE
C Declarations for labels (same as in PLOT1):
        PARAMETER (MAXLAB=50)
        CHARACTER*30 LABEL(MAXLAB)
        REAL*4 XLABEL(MAXLAB),YLABEL(MAXLAB),ANGLE_LABEL(MAXLAB),EXPAND
        INTEGER*4 XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10
        INTEGER*4 JLP_AXES,NLABELS,AUTO_SCALE
C
	REAL*4 X(NMAX,*),Y(NMAX,*),ERRX(NMAX,*),ERRY(NMAX,*)
	REAL*4 XOUT,YOUT,OFFX,OFFY,AXLEN,AYLEN
	REAL*4 XMINUSER,XMAXUSER,YMINUSER,YMAXUSER
	REAL*4 TDX,TDY,WORK,XMIN,XMAX,YMIN,YMAX
	INTEGER*4 NPTS(*),IPLAN,ERROR_BARS,FULL_CAPTION
        INTEGER*4 IXOFF_LABEL,IYOFF_LABEL,NOUT,NOUT_MAX,IDEV
        INTEGER*4 Y_IS_REVERSED,TICKS_IN
	CHARACTER PLOTDEV*40,PLOTDEV1*40
	CHARACTER NCHAR(*)*4,PCOLOR(*)*30,CHAR4*40,ANSWER*80
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40,ANS*1
        CHARACTER FILENAME*60,COMMENTS*80
	LOGICAL FILE,PLAN,COPY
	COMMON/PARAMETERS/OFFX,OFFY,AXLEN,AYLEN,XMINUSER,YMINUSER,
     1	XMAXUSER,YMAXUSER,TDX,TDY,IDEV
C Output points :
	COMMON/STR_OUTPUT/XOUT(200),YOUT(200),NOUT
C Labels 
        COMMON/STR_LABELS/NLABELS,XLABEL,YLABEL,
     1        ANGLE_LABEL,LABEL,IXOFF_LABEL,IYOFF_LABEL
10	FORMAT(A)
 
C Graphic help:
	INCLUDE 'jlpsub:jlp_graphic_help.for'

C Possibility of NULL device:
	IF(PLOTDEV(1:4).EQ.'NULL'.OR.PLOTDEV(1:4).EQ.'null'
     1     .OR.PLOTDEV(2:5).EQ.'NULL'.OR.PLOTDEV(2:5).EQ.'null')THEN
	  PRINT 128
128	  FORMAT(' NEWPLOT0/ null device has been selected',/)
	  RETURN
	 ENDIF

C JLP2002: Filter to allow output for publications
C Remove undesirable caption
        IF(PLOTDEV(2:2).EQ.'L'.OR.PLOTDEV(2:2).EQ.'P'.OR.
     1     PLOTDEV(2:2).EQ.'S'.OR.PLOTDEV(2:2).EQ.'F')THEN
          FULL_CAPTION=0
        ELSE
          FULL_CAPTION=1
        ENDIF
 
C Protections:	
	IF(KCURVE.EQ.0)THEN
	  PRINT 120
120	  FORMAT(' NEWPLOT0/ FATAL ERROR: KCURVE = 0')
	  STOP
	ENDIF
	IF(NPTS(1).EQ.0)THEN
	  PRINT 121
121	  FORMAT(' NEWPLOT0/ FATAL ERROR: NPTS(1) = 0')
	  STOP
	ENDIF
	IF(NMAX.EQ.0)THEN
	  PRINT 122
122	  FORMAT(' NEWPLOT0/ FATAL ERROR: NMAX = 0')
	  STOP
	ENDIF
 
C Erasing the counter NOUT :
	NOUT=0
 
C Possibility of same scale in X and Y (usefull for displaying the shells
C in the plane of the sky)
	PRINT *,' Same scale in x and y? (N)'
	READ(5,10) ANS
	PLAN=(ANS.EQ.'y'.OR.ANS.EQ.'Y')
	IF(PLAN)THEN
	  IPLAN=1
	ELSE
	  IPLAN=0
	ENDIF
 
C Check which package has been selected, and
C set the physical limits for the plot (device-dependant)
	COPY=.FALSE.
C JLP2004: no longer used:
C	CALL WINDOW_LIMITS(PLOTDEV,ICODE,FILE,COPY)
 
C Compute the minimum and maximum :
C If JLP axes: takes only min/max (will compute exact boundaries in splot) 
        IF(JLP_AXES.EQ.1)THEN
          CALL MINMAX_SCALE(X,NMAX,KCURVE,NPTS,XMIN,XMAX)
          IF(ERROR_BARS.EQ.1)THEN
          CALL MINMAX_ERROR_SCALE(Y,ERRY,NMAX,KCURVE,NPTS,YMIN,YMAX)
          ELSE
          CALL MINMAX_SCALE(Y,NMAX,KCURVE,NPTS,YMIN,YMAX)
          ENDIF
C If SMongo axes: takes +/- 5 percents larger frame
        ELSE
          CALL NEWSCALE(X,NMAX,KCURVE,NPTS,XMIN,XMAX)
          IF(ERROR_BARS.EQ.1)THEN
            CALL NEWSCALE_ERROR(Y,ERRY,NMAX,KCURVE,NPTS,YMIN,YMAX)
          ELSE
            CALL NEWSCALE(Y,NMAX,KCURVE,NPTS,YMIN,YMAX)
          ENDIF
        ENDIF   
 
C Prompt for other values if the user wants to change :
	PRINT 114,XMIN,XMAX,YMIN,YMAX
114	FORMAT(' Proposed scaling values:',4(1PE12.5,1X))
 
	WRITE(6,205)
205     FORMAT(' New values you want? ',/,
     1	'  (Enter "OK" or press on RETURN key if they are O.K.)',/,
     1	'  (Enter "-Y" if you simply want an inversion',
     1  ' in the Y axis (used for magnitudes))')
	ANSWER=' '
	READ(5,10) ANSWER
	IF((ANSWER(1:3).NE.'   ').AND.(ANSWER(1:2).NE.'OK')
     1     .AND.(ANSWER(1:2).NE.'ok'))THEN
           IF(ANSWER(1:3).EQ.'-Y'.OR.ANSWER(1:2).EQ.'-y')THEN
             WORK=YMIN
             YMIN=YMAX
             YMAX=WORK 
           ELSE
	     READ(ANSWER,*) XMIN,XMAX,YMIN,YMAX
           ENDIF
        ENDIF
        Y_IS_REVERSED=0
	XMINUSER=XMIN
	YMINUSER=YMIN
	XMAXUSER=XMAX
	YMAXUSER=YMAX
 
C Selecting the package :
	PLOTDEV1=PLOTDEV(2:40)
 
C	IF(ICODE.EQ.1)THEN
C	    CALL STROMLO_PLOT(X,Y,ERRY,NPTS,NMAX,KCURVE,
C     1	CHAR1,CHAR2,CHAR3,NCHAR,PLOTDEV1,XOUT,YOUT,NOUT,
C     1	PLAN,ERROR_BARS)
 
C PGPLOT:
C	ELSEIF(ICODE.EQ.2)THEN
C	  CALL PG_PLOT(X,Y,NPTS,NMAX,KCURVE,CHAR1,CHAR2,
C     1	CHAR3,NCHAR,PLOTDEV1,XOUT,YOUT,NOUT,PLAN)
 
C MONGO_GKS:
C	ELSEIF(ICODE.EQ.3)THEN
C	  CALL MONGO_PLOT(X,Y,NPTS,NMAX,KCURVE,CHAR1,CHAR2,
C     1	CHAR3,NCHAR,PLOTDEV1,XOUT,YOUT,NOUT)
 
C Super Mongo:
C	IF(ICODE.EQ.4)THEN
C	  CALL SMGO_PLOT(X,Y,ERRY,NPTS,NMAX,KCURVE,CHAR1,CHAR2,
C     1	CHAR3,NCHAR,PLOTDEV1,XOUT,YOUT,NOUT,ERROR_BARS)
C	ENDIF

C New routine: 
C (Call JLP_DEVICE_CURVE)
	 CALL JLP_SPDEVICE_CURVE(PLOTDEV1,XMIN,XMAX,YMIN,YMAX,
     1                           IPLAN,CHAR3,IDEV)
         NOUT_MAX = 200
         Y_IS_REVERSED=0
         TICKS_IN = 0
         AUTO_SCALE = 0
C	 CALL NEWPLOT2(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,
C     1     XMIN,XMAX,YMIN,YMAX,IPLAN,Y_iS_REVERSED,CHAR1,CHAR2,
C     1	    CHAR3,NCHAR,PCOLOR,XOUT,YOUT,NOUT,NOUT_MAX,ERROR_BARS,
C     1     FILENAME,COMMENTS,FULL_CAPTION,EXPAND,TICKS_IN,IDEV)
C JLP2007: more possibilities with NEWPLOT21:
	 CALL NEWPLOT21(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,
     1      XMIN,XMAX,YMIN,YMAX,AUTO_SCALE,IPLAN,Y_IS_REVERSED,
     1      CHAR1,CHAR2,CHAR3,NCHAR,PCOLOR,XOUT,YOUT,NOUT,NOUT_MAX,
     1      ERROR_BARS,FILENAME,COMMENTS,FULL_CAPTION,JLP_AXES,
     1      XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10,
     1      EXPAND,TICKS_IN,IDEV)
         IF(NLABELS.GT.0)THEN 
            CALL JLP_PLOTLABELS(NLABELS,XLABEL,YLABEL,
     1        ANGLE_LABEL,LABEL,IXOFF_LABEL,IYOFF_LABEL,MAXLAB)
            CALL JLP_GFLUSH(IDEV)
         ENDIF
         CALL JLP_SPCLOSE(IDEV)
 
C*********************************************************************
C Possibility of output to the laser_printer or a graphic file
C*********************************************************************
 
C Check if a graphic metafile has not been previously selected :
	IF(FILE)RETURN
 
C Possibility of getting a copy with another package:
72	PRINT 71
71	FORMAT(' Do you want a copy on a laser printer?',/,
     1	' Answer with the device name for a copy,',
     1	' "N" or "n" for no copy, or "?" for help')
	READ(5,10) PLOTDEV1
	IF(PLOTDEV1.EQ.'?')THEN
	   PRINT 3333
	   GOTO 72
	ENDIF
	IF((PLOTDEV1.EQ.' ').OR.(PLOTDEV1(1:1).EQ.'Y')
     1	.OR.(PLOTDEV1(1:1).EQ.'y'))GOTO 72
	COPY=((PLOTDEV1(1:1).NE.'N').AND.(PLOTDEV1(1:1).NE.'n'))
 
	IF(COPY)THEN
C Check which package has been selected, and
C set the physical limits for the plot (device-dependant)
C
C JLP2004: no longer used:
C	  CALL WINDOW_LIMITS(PLOTDEV1,ICODE,FILE,COPY)
 
	  PRINT *,' Enter title ("return" to keep the present title)'
	  READ(5,10) CHAR4
	  IF(CHAR4.NE.' ')CHAR3=CHAR4
 

C Selecting the subroutine:
	  PLOTDEV1=PLOTDEV1(2:40)
C	  IF(ICODE.EQ.1)THEN
C STROMLO :::::
C	    CALL STROMLO_PLOT(X,Y,ERRY,NPTS,NMAX,KCURVE,CHAR1,CHAR2,
C     1	CHAR3,NCHAR,PLOTDEV1,XOUT,YOUT,NOUT,PLAN,ERROR_BARS)
C	    PRINT *,' OUTPUT IN "PLOT.PLT"'
 
C PGPLOT :::::
C	  ELSEIF(ICODE.EQ.2)THEN
C	    CALL PG_PLOT(X,Y,NPTS,NMAX,KCURVE,CHAR1,CHAR2,
C     1	CHAR3,NCHAR,PLOTDEV1,XOUT,YOUT,NOUT,PLAN)
 
C MONGO_GKS:
C	  ELSEIF(ICODE.EQ.3)THEN
C	    CALL MONGO_PLOT(X,Y,NPTS,NMAX,KCURVE,CHAR1,CHAR2,
C     1	CHAR3,NCHAR,PLOTDEV1,XOUT,YOUT,NOUT)
C	    PRINT *,' OUTPUT IN "CANON.DAT"'
 
C Super Mongo:
C	  IF(ICODE.EQ.4)THEN
C	    CALL SMGO_PLOT(X,Y,ERRY,NPTS,NMAX,KCURVE,CHAR1,CHAR2,
C     1	CHAR3,NCHAR,PLOTDEV1,XOUT,YOUT,NOUT,ERROR_BARS)
C	      PRINT *,' Output in "MGOxxxxxx.VEC"'
C	  ENDIF

C New routine: 
	 CALL JLP_SPDEVICE_CURVE(PLOTDEV1,XMIN,XMAX,YMIN,YMAX,
     1                           IPLAN,CHAR3,IDEV)
         NOUT_MAX = 200
C Before Jul. 2007:
C	 CALL NEWPLOT2(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,CHAR1,CHAR2,
C     1	CHAR3,NCHAR,PCOLOR,XOUT,YOUT,NOUT,NOUT_MAX,ERROR_BARS,
C     1  FILENAME,COMMENTS,FULL_CAPTION,IDEV)
C JLP2007: more possibilities with NEWPLOT21:
         TICKS_IN = 0
         AUTO_SCALE = 0
	 CALL NEWPLOT21(X,Y,ERRX,ERRY,NPTS,NMAX,KCURVE,
     1     XMIN,XMAX,YMIN,YMAX,AUTO_SCALE,IPLAN,Y_IS_REVERSED,
     1     CHAR1,CHAR2,CHAR3,NCHAR,PCOLOR,XOUT,YOUT,NOUT,NOUT_MAX,
     1     ERROR_BARS,FILENAME,COMMENTS,FULL_CAPTION,JLP_AXES,
     1     XGRID_IS_WANTED,YGRID_IS_WANTED,X_IS_LOG10,Y_IS_LOG10,
     1     EXPAND,TICKS_IN,IDEV)
         IF(NLABELS.GT.0)THEN 
            CALL JLP_PLOTLABELS(NLABELS,XLABEL,YLABEL,
     1        ANGLE_LABEL,LABEL,IXOFF_LABEL,IYOFF_LABEL,MAXLAB)
            CALL JLP_GFLUSH(IDEV)
         ENDIF
         CALL JLP_SPCLOSE(IDEV)
 
 
	ENDIF
 
	RETURN
	END
C------------------------------------------------------
C Subroutine WINDOW_LIMITS
C
C Input:
C COPY (logical): true if a copy is beeing asked
C
C Output:
C ICODE
C  (STROMLO:$:1,PGPLOT:*:2,MONGO_GKS:#:3,SMONGO:&:4,MONGO_87:%:5)
C FILE (logical): true if graphic output file has been selected
C OFFX, OFFY, AXLEN, AYLEN (offset values and axes lengths)
C------------------------------------------------------
	SUBROUTINE WINDOW_LIMITS(PLOTDEV,ICODE,FILE,COPY)
        IMPLICIT NONE
	REAL*4 OFFX,OFFY,AXLEN,AYLEN,XMINUSER,YMINUSER
	REAL*4 XMAXUSER,YMAXUSER,TDX,TDY
	INTEGER*4 ICODE,I,J,I1,I2,JC,ISTATUS,IDEV
	CHARACTER PLOTDEV*(*)
	CHARACTER BUFFER*80,STRING*4,EXEC_DIR*40,KERFILE*60
	LOGICAL FILE,COPY,FOUND
 
	COMMON/PARAMETERS/OFFX,OFFY,AXLEN,AYLEN,XMINUSER,YMINUSER,
     1	XMAXUSER,YMAXUSER,TDX,TDY,IDEV
 
C  NEWPLOT:$:1,PGPLOT:*:2,MONGO_GKS:#:3,SMONGO:&:4,MONGO_87:%:5
	FOUND=.FALSE.
	ICODE=0
	IF(PLOTDEV(1:1).EQ.'$')ICODE=1
	IF(PLOTDEV(1:1).EQ.'*')ICODE=2
	IF(PLOTDEV(1:1).EQ.'#')ICODE=3
	IF(PLOTDEV(1:1).EQ.'&')ICODE=4
	IF(PLOTDEV(1:1).EQ.'%')ICODE=5
 
C Default device: &xterm or &postscript:
99	IF(ICODE.EQ.0)THEN
           WRITE(6,60) PLOTDEV
60	     FORMAT(' WINDOW_LIMITS/Wrong specification: PLOTDEV=',A,
     1	/,8X,'(Check the file "$EXEC/graphic0.ker")')
	   IF(.NOT.COPY)THEN
	     PLOTDEV='&xterm'
	     ICODE=4
	   ELSE
	     PLOTDEV='&postscript'
	     ICODE=4
	   ENDIF
           WRITE(6,61) PLOTDEV
61	   FORMAT(' Device set to default: ',A)
	ENDIF
 
C Read graphic parameters in file "graphic0.ker"
        STRING='EXEC'
        CALL JLP_GETENV(STRING,4,EXEC_DIR,ISTATUS) 
        I1=INDEX(EXEC_DIR,' ')
        I2=INDEX(EXEC_DIR,CHAR(0))
        I = MIN(I1,I2)-1
        J = MAX(I1,I2)-1
        IF(I.LE.0) I=J
        IF(ISTATUS.EQ.0.AND.I.GT.0)THEN
           KERFILE=EXEC_DIR(1:I)//'/graphic0.ker'
        ELSE
           WRITE(6,47)
47         FORMAT(' WINDOW_LIMITS/Error decoding symbol EXEC,',
     1     ' will try in default directory')
           KERFILE='graphic0.ker'
        ENDIF
	OPEN(35,FILE=KERFILE,STATUS='OLD',ERR=999)
10	FORMAT(A)
	DO I=1,1000
	  READ(35,10,END=90) BUFFER
	  READ(BUFFER(1:1),'(I1)',ERR=90)JC
	  IF(JC.EQ.ICODE)THEN
	    READ(BUFFER(5:5),'(I1)')J
C The device name has been found:
	    IF(BUFFER(7:J+6).EQ.PLOTDEV(2:J+1))THEN
	       FILE=(BUFFER(3:3).EQ.'F')
	       READ(BUFFER(30:80),*) OFFX,OFFY,AXLEN,AYLEN
	       FOUND=.TRUE.
	       GOTO 90
	    ENDIF
	  ENDIF
	END DO
 
90	CLOSE(35)
	IF(.NOT.FOUND)THEN
	  ICODE=0
 	  GOTO 99
	ENDIF
 
	RETURN
999	WRITE(6,64) KERFILE
64	FORMAT(' WINDOW_LIMITS/Error: graphic0.ker not found in ',A)
C JLP94 Before: OFFX=3500 (but pb with labels as 1.45E-5, ...)
	OFFX=4500
	OFFY=3500
	AXLEN=28000
	AYLEN=28000
	RETURN	
	END
C----------------------------------------------------------------------
        SUBROUTINE JLP_PLOTLABELS(NLABELS,XLABEL,YLABEL,
     1        ANGLE_LABEL,LABEL,IXOFF_LABEL,IYOFF_LABEL,MAXLAB)
        IMPLICIT NONE
C
C Declarations for labels (same as in PLOT1):
        INTEGER*4 MAXLAB,NLABELS,IXOFF_LABEL,IYOFF_LABEL,IDEV
        CHARACTER*30 LABEL(MAXLAB)
        REAL*4 XLABEL(MAXLAB),YLABEL(MAXLAB),ANGLE_LABEL(MAXLAB)
C
C Internal definitions:
        REAL*4 LABEL_LENGTH
        INTEGER*4 IX,IY,I
C 
C Definitions (as in STR.FOR) for coordinate conversion:
        REAL*4 OFFX,OFFY,AXLEN,AYLEN,TDX,TDY
        REAL*4 XMINUSER,XMAXUSER,YMINUSER,YMAXUSER
        COMMON/PARAMETERS/OFFX,OFFY,AXLEN,AYLEN,XMINUSER,YMINUSER,
     1  XMAXUSER,YMAXUSER,TDX,TDY,IDEV


        DO I=1,NLABELS
C Conversion of user coordinates to local coordinates
          IX=(XLABEL(I)-XMINUSER)*TDX+OFFX
          IY=(YLABEL(I)-YMINUSER)*TDY+OFFY

C Shift of the offset entered by the user:
          IX=IX+IXOFF_LABEL
          IY=IY+IYOFF_LABEL

C JLP_SPLABEL(label,max_length,ix,iy,angle,expand,idrawit,length)
          CALL JLP_SPLABEL(LABEL(I),30,IX,IY,ANGLE_LABEL(I),
     1                     1.0,1,LABEL_LENGTH,IDEV)
        END DO

	RETURN	
	END
