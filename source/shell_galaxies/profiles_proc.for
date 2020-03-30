C++------------------------------------------------------------------------
C PROFILES_PROC
C To process galaxy profiles
C To compute the color index of a galaxy, or concatenate 2 profiles
C (long/short exposures)
C
C Only in interactive mode.
C
C JLP - Version of 24-10-88
C From "PLOT1"
C------------------------------------------------------------------------
	PROGRAM PROFILES_PROC
	PARAMETER (IDIM=2000,KCUR=10)
	REAL*4 X,Y,ERROR,CMAG,CTE
	REAL*4 XPLOT,YPLOT,ERPLOT
	REAL*4 XX1(IDIM),YY1(IDIM),XX2(IDIM),YY2(IDIM)
	INTEGER*4 NPTS
	CHARACTER OPTION1*50,ANS*1,IN_FILE*40,IN_COMMENTS*80
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40
	CHARACTER NCHAR*4,CHOICE*1,PLOTDEV*32
	LOGICAL ERROR_PLOT,CHECK_INPUT
 
	COMMON/BLOCKA/X(IDIM,KCUR),Y(IDIM,KCUR),ERROR(IDIM,KCUR)
	COMMON/BLOCKB/XPLOT(IDIM,KCUR),YPLOT(IDIM,KCUR),
     1	ERPLOT(IDIM,KCUR),ERROR_PLOT,CHECK_INPUT
	COMMON/BLOCKC/KK,NPTS(KCUR),NCHAR(KCUR),CMAG(KCUR),CTE(KCUR),
     1	COEFF(KCUR)
	COMMON/IOPT/IOPX,IOPY
 
C Common block with NEWPLOT :
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
 
10	FORMAT(A)
 
C When calling RREADFILE, two possibilities:
C IFMT=3 (profiles from PROFILE1) or IFMT=0 (prompt the user)
	PRINT *,' Are you working with profiles from "PROFILE1" ?(Y)'
	READ(5,10)ANS
	IF(ANS.EQ.'n'.OR.ANS.EQ.'N')THEN
	  IFMT=0
	ELSE
	  IFMT=3
	ENDIF
 
11	KK=0
	ERROR_PLOT=.FALSE.
12	PRINT 13
13	FORMAT(' MENU :',/,
     1	' 1. Input of profiles',/,
     1	' 2. Displaying the curves',/,
     1	' 3. Color indices',/,
     1	' 4. Resetting the number of curves to 0',/,
     1	' 5. Concatenation of two profiles (short/long exposure)',/,
     1	' 6. Output of a curve in a file',/,
     1	' 7. HELP (Color indices)',/,
     1	' 8. HELP (Concatenation of two profiles)',/,
     1	' 10. EXIT',/,
     1	' ENTER THE OPTION YOU WANT : ',$)
	READ(5,*) IOPT
	IF(IOPT.EQ.4)THEN
	  GO TO 11
C------------------------------------------------------------------
C Help: Color indices
C------------------------------------------------------------------
	ELSEIF(IOPT.EQ.7)THEN
	  PRINT 1100
1100	  FORMAT(' To compute a color index from two profiles',/,
     1	' - Enter the two profiles (option 1 twice)',/,
     1	' - Display the two profiles (magnitude versus r**0.25)',/,
     1	'   with the good sky values (options 2)',/,
     1	' - Then resample the profiles',/,
     1	' and divide one profile by the other (option 3)',/,
     1	' - Display the resulting curve',/,
     1	' with the good constants A + B*LOG(Y)',
     1	' and save it (option 6)')
	  GO TO 12
C------------------------------------------------------------------
C Help: Concatenation of profiles
C------------------------------------------------------------------
	ELSEIF(IOPT.EQ.8)THEN
	  PRINT 1200
1200	  FORMAT(' To concatenate two profiles',/,
     1	' - Enter the two profiles (option 1 twice)',/,
     1	' - Display the two profiles (magnitude versus r**0.25)',/,
     1	'   with the good sky values (options 2)',/,
     1	' - Get with the cursor the location of the junction',/,
     1	' - Select option 5 to obtain the synthetic profile,',/,
     1	' display it, and save it (option 6)')
	  GO TO 12
C------------------------------------------------------------------
	ELSEIF(IOPT.EQ.1)THEN
 
C Option IFMT=3 (profiles from PROFILE1) or IFMT=0 (prompt the user)
	  CALL RREADFILE(XX1,YY1,NPT1,XX2,YY2,NPT2,IDIM,
     1                   IN_FILE,IN_COMMENTS,IFMT)
C X(I,KK) AND Y(I,KK) POINT NUMBER I OF THE LINE KK
C NPTS(KK) NUMBER OF POINTS OF THE LINE KK
 
C Input of the profile :
	IF(NPT1.NE.0)THEN
	  KK=KK+1
	  NPTS(KK)=NPT1
	  PRINT *,'TYPE OF CHARACTER AND SIZE YOU WANT (ex L0 or L1)'
	  PRINT *,'L0=solid line L1=dotted line'
	  READ(5,10) NCHAR(KK)
          PCOLOR(KK)='Default'
	    DO I=1,NPT1
	      X(I,KK)=XX1(I)
	      Y(I,KK)=YY1(I)
	    END DO
	ENDIF
 
	GO TO 12
C------------------------------------------------------------------
C Displaying the curves XPLOT and YPLOT :
 
	ELSEIF(IOPT.EQ.2)THEN
300	  CALL COORDINATE
 
314	  PRINT *,' Now displaying the curve(s):'
	  PRINT *,'           TITLE ?'
	  READ(5,10) CHAR3
	  PRINT *,'    X AXIS LABEL ?'
	  READ(5,10) CHAR1
	  PRINT *,'    Y AXIS LABEL ?'
	  READ(5,10) CHAR2
 
	  PRINT *,' OUTPUT DEVICE : ($TEKTRO ?)'
	  READ(5,10) PLOTDEV
	IF(PLOTDEV.EQ.' ')PLOTDEV='$TEKTRO'
 
C Displaying with NEWPLOT :
50	  CALL NEWPLOT(XPLOT,YPLOT,NPTS,IDIM,KK,CHAR1,CHAR2,
     1	CHAR3,NCHAR,PCOLOR,PLOTDEV,IN_FILE,IN_COMMENTS)
 
	  IF(NOUT.NE.0)THEN
	   PRINT *,' ',NOUT,' POINTS RECEIVED'
	    DO I=1,NOUT
	      PRINT 100,I,XOUT(I),YOUT(I)
	    END DO
100	   FORMAT(' POINT:',I5,' X=',F12.3,' Y=',F12.3)
	  ENDIF
 
	  PRINT *,' DO YOU WANT TO CHANGE THE PARAMETERS OF',
     1	' THE FRAME ?(N)'
	  READ(5,10) ANS
	   IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GO TO 50
 
	  GO TO 12
C------------------------------------------------------------------
	ELSEIF(IOPT.EQ.3)THEN
	  CALL COLOR_INDICES
C If KK=2, this means the user wants to come back to the root menu:
	  IF(KK.EQ.2)GO TO 12
	  GO TO 300
C------------------------------------------------------------------
	ELSEIF(IOPT.EQ.5)THEN
	  CALL CONCATENATION
C If KK=2, this means the user wants to come back to the root menu:
	  IF(KK.EQ.2)GO TO 12
	  GO TO 314
C------------------------------------------------------------------
	ELSEIF(IOPT.EQ.6)THEN
	  CALL OUTPUT_CURVE
	  GO TO 12
	ENDIF
C------------------------------------------------------------------
9999	STOP
	END
 
C********************************************************************
C	SUBROUTINE PERFORMING OPERATIONS ON XX,YY FOR THE DISPLAY
C********************************************************************
	SUBROUTINE COORDINATE
	PARAMETER (IDIM=2000,KCUR=10)
	REAL*4 XX,YY,CMAG,CTE,XPLOT,YPLOT,ERPLOT,ERROR
	CHARACTER*4 NCHAR
	LOGICAL ERROR_PLOT,CHECK_INPUT
 
	COMMON/BLOCKA/XX(IDIM,KCUR),YY(IDIM,KCUR),ERROR(IDIM,KCUR)
	COMMON/BLOCKB/XPLOT(IDIM,KCUR),YPLOT(IDIM,KCUR),
     1	ERPLOT(IDIM,KCUR),ERROR_PLOT,CHECK_INPUT
	COMMON/BLOCKC/KK,NPTS(KCUR),NCHAR(KCUR),CMAG(KCUR),CTE(KCUR),
     1	COEFF(KCUR)
 
	COMMON/IOPT/IOPX,IOPY
 
C----------------------------------------------------------------
C?
	PRINT *,' DO YOU WANT FOR THE X AXIS :'
	PRINT *,'      1.X, 2.LOG10(X), or 3.X**.25  ?'
	READ(5,*)  IOPX
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
	 DO K=1,KK
	  DO I=1,NPTS(K)
	    IF(XX(I,K).GT.0.)THEN
	      XPLOT(I,K)=ALOG10(XX(I,K))
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
C*****************************************************************
C?
	PRINT 703
703	FORMAT(' DO YOU WANT FOR THE Y AXIS :',/,
     1	' 1. Y',/,' 2. (Y-CTE)',/,
     1	' 3. LOG10(Y-CTE)',/,
     1	' 4. CMAG - 2.5*LOG10(Y-CTE)  (magnitudes)',/,
     1	' 5. CMAG + COEFF*LOG10(Y)  (for color indices)',/,
     1	' ENTER THE OPTION YOU WANT : ')
	READ(5,*) IOPY
 
	IF(IOPY.NE.1.AND.IOPY.NE.5)THEN
	   PRINT *,' CONSTANT YOU WANT TO SUBTRACT TO THE CURVE'
	   DO K=1,KK
	     PRINT *,' NUMBER :',K,' ?'
	     READ(5,*) CTE(K)
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
	   ERPLOT(I,K)=ERROR(I,K)
	  END DO
	 END DO
	ENDIF
 
	ENDIF
C----------------------------------------------------------------
C Option 2 : YPLOT=Y-CTE
C
	IF(IOPY.EQ.2)THEN
	DO K=1,KK
	 DO I=1,NPTS(K)
	  YPLOT(I,K)=YY(I,K)-CTE(K)
	 END DO
	END DO
 
C Transfer of ERROR if option 5 has been called at least once.
	IF(ERROR_PLOT)THEN
	  DO K=1,KK
	   DO I=1,NPTS(K)
	    ERPLOT(I,K)=ERROR(I,K)
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
	        YPLOT(I,K)=-5.
	       ELSE
	        YPLOT(I,K)=ALOG10(WORK)
	       ENDIF
	    END DO
	  END DO
	ENDIF
C----------------------------------------------------------------
	IF(IOPY.EQ.4.OR.IOPY.EQ.5)THEN
 
C Prompt for the parameters
	  IF(IOPY.EQ.4)THEN
	    PRINT *,' Enter CMAG for each curve:'
	    DO K=1,KK
	      PRINT *,' NUMBER :',K,' ?'
	      READ(5,*) CMAG(K)
	      COEFF(K)=-2.5
	    END DO	
	  ELSE
C For IOPY=5, KK=1, so:
	    PRINT *,' Enter CMAG and COEFF for the color index:'
	    DO K=1,KK
	      READ(5,*) CMAG(K),COEFF(K)
	      CTE(K)=0.
	    END DO	
	  ENDIF
 
C Do the transformation:
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
	RETURN
	END
C********************************************************************
C Subroutine to compute color indices
C from two profiles
C********************************************************************
	SUBROUTINE COLOR_INDICES
	PARAMETER (IDIM=2000,KCUR=10)
	REAL*8 X1(IDIM),Y1(IDIM),SPLINE1(IDIM),K1(IDIM)
	REAL*8 XABSCISS,RESULT
	REAL*4 XX,YY,CMAG,CTE,XPLOT,YPLOT,ERPLOT,ERROR
	CHARACTER NCHAR*4
	LOGICAL ERROR_PLOT,CHECK_INPUT
 
	COMMON/BLOCKA/XX(IDIM,KCUR),YY(IDIM,KCUR),ERROR(IDIM,KCUR)
	COMMON/BLOCKB/XPLOT(IDIM,KCUR),YPLOT(IDIM,KCUR),
     1	ERPLOT(IDIM,KCUR),ERROR_PLOT,CHECK_INPUT
	COMMON/BLOCKC/KK,NPTS(KCUR),NCHAR(KCUR),CMAG(KCUR),CTE(KCUR),
     1	COEFF(KCUR)
	COMMON/IOPT/IOPX,IOPY
 
C----------------------------------------------------------------
C
66	PRINT 67
67	FORMAT(10X,' COLOR INDEX COMPUTATION',/,
     1	' 1. First resampling the curves',/,
     1	' and then division of two curves and',
     1	' output in curve #1 ',/,
     1	' 10. Return to the main menu',/,
     1	10X,' Enter your choice :  ',$)
	READ(5,*) IOP6
	IF(IOP6.NE.1)RETURN
 
C----------------------------------------------------------------
C Resampling a curve
C----------------------------------------------------------------
401	  PRINT 63
63 	  FORMAT(' RESAMPLING THE CURVES')
C For interpolation, we take the curve starting with the lowest value
C as reference:
	  IF(XPLOT(1,1).LE.XPLOT(1,2))THEN
	    NC1=1
	    NC2=2
	  ELSE
	    NC1=2
	    NC2=1
	  ENDIF
	
C Interpolation of the reference curve:
	   DO I=1,NPTS(NC1)
	    X1(I)=XPLOT(I,NC1)
	    Y1(I)=YPLOT(I,NC1)
	   END DO
	  CALL SPLINE_INTER(X1,Y1,NPTS(NC1),SPLINE1,NCAP71,K1)	
 
C Generation of the resampled curve :
	  CALL INDEX_MAX4(XPLOT(1,NC2),NPTS(NC2),
     1	XPLOT(NPTS(NC1),NC1),IMAX)
	  NPTS(NC1)=IMAX
	  DO I=1,NPTS(NC1)
	    XPLOT(I,NC1)=XPLOT(I,NC2)
	    XABSCISS=XPLOT(I,NC1)
C Calling NAG routine E02BBF:
	    IFAIL=1
	    CALL E02BBF(NCAP71,K1,SPLINE1,XABSCISS,RESULT,IFAIL)
	     IF(IFAIL.NE.0)THEN
	      PRINT *,' E02BBF : FAILURE IN E02BBF, IFAIL=',IFAIL
	     ENDIF
	    YPLOT(I,NC1)=RESULT
	  END DO
	PRINT *,' Resampling is done'
 
C----------------------------------------------------------------
C Division of two curves :
C----------------------------------------------------------------
	PRINT 69
69	FORMAT(' Now give the entry numbers of the',
     1	' 2 curves (OUTPUT=FIRST/SECOND)',/,
     1	' (for B-R, B/R...)')
	READ(5,*) NC1,NC2
	NPT0=MIN(NPTS(NC1),NPTS(NC2))
 
	IF(IOPX.EQ.3.AND.IOPY.EQ.4)THEN
	  PRINT 61
61	  FORMAT(/,' Division of the two curves in their original form',
     1	'  (Radius, Intensity-sky)',/)
	  DO I=1,NPT0
	   IF(XPLOT(I,NC1).EQ.XPLOT(I,NC2))THEN
	     XPLOT(I,1)=XPLOT(I,NC1)**4
	     YNUM=10**(0.4*(CMAG(NC1)-YPLOT(I,NC1)))
	     YDEN=10**(0.4*(CMAG(NC2)-YPLOT(I,NC2)))
	      IF(ABS(YDEN).LT.1.E-15)YDEN=1.E-15
	     YPLOT(I,1)=YNUM/YDEN
	   ELSE
	     PRINT *,' ERROR : NOT SAMPLED IN THE SAME WAY'
	     PRINT *,' I, X1, X2 :',I,XPLOT(I,NC1),XPLOT(I,NC2)
	     RETURN
	   ENDIF
 	  END DO
	ELSE
	  PRINT 62
62	  FORMAT(' Division of the two curves as they appeared',
     1	' in the graph')
	  DO I=1,NPT0
	   IF(XPLOT(I,NC1).EQ.XPLOT(I,NC2))THEN
	     XPLOT(I,1)=XPLOT(I,NC1)
	      IF(YPLOT(I,NC2).EQ.0)YPLOT(I,NC2)=0.1E-10
	     YPLOT(I,1)=YPLOT(I,NC1)/YPLOT(I,NC2)
	   ELSE
	     PRINT *,' ERROR : NOT SAMPLED IN THE SAME WAY'
	     PRINT *,' I, X1, X2 :',I,XPLOT(I,NC1),XPLOT(I,NC2)
	     RETURN
	    ENDIF
 	  END DO
	ENDIF
 
C----------------------------------------------------------------
C Setting the number of curves to 1, now:
	PRINT *,' Resetting the number of curves to 1, now'
	KK=1
	NPTS(1)=NPT0
C Copying the data to XX and YY
	DO I=1,NPTS(1)
	   XX(I,1)=XPLOT(I,1)
	   YY(I,1)=YPLOT(I,1)
	END DO
 
C Print a sample of data points:
	PRINT *,' Sample of the first values:'
	J=MIN(NPTS(1),10)
	DO I=1,J
	  PRINT *,' I, X, Y:',I,XX(I,1),YY(I,1)
	END DO
 	
	RETURN
	END
 
C********************************************************************
C Subroutine to concatenate two curves from XPLOT,YPLOT
C********************************************************************
	SUBROUTINE CONCATENATION
	PARAMETER (IDIM=2000,KCUR=10)
	REAL*8 XXWORK,YYWORK
	REAL*4 XX,YY,CMAG,CTE,XPLOT,YPLOT,ERPLOT,ERROR
	CHARACTER*4 NCHAR
	CHARACTER NAME*30,ANS*1
	LOGICAL ERROR_PLOT,CHECK_INPUT
 
	COMMON/BLOCKA/XX(IDIM,KCUR),YY(IDIM,KCUR),ERROR(IDIM,KCUR)
	COMMON/BLOCKB/XPLOT(IDIM,KCUR),YPLOT(IDIM,KCUR),
     1	ERPLOT(IDIM,KCUR),ERROR_PLOT,CHECK_INPUT
	COMMON/BLOCKC/KK,NPTS(KCUR),NCHAR(KCUR),CMAG(KCUR),CTE(KCUR),
     1	COEFF(KCUR)
	COMMON/IOPT/IOPX,IOPY
 
10	FORMAT(A)
C----------------------------------------------------------------
	PRINT *,' I hope you have gone through option 3 ...'
	PRINT *,' NUMBER OF THE TWO CURVES YOU WANT TO CONCATENATE :'
	READ(5,*) KK1,KK2
	PRINT *,' ABSCISSA OF THE JUNCTION (in the "plotting" units)'
	READ(5,*) XJUNCT
 
	I1=1
	DO I=1,NPTS(KK1)
	  IF(XJUNCT.GT.XPLOT(I,KK1))THEN
	   I1=I
	  ELSE
	   GO TO 820
	  ENDIF
	END DO	
 
820	I2=NPTS(KK2)
	DO I=NPTS(KK2),1,-1
	  IF(XJUNCT.LT.XPLOT(I,KK2))THEN
	   I2=I
	  ELSE
	   GO TO 830
	  ENDIF
	END DO	
 
830	PRINT 831,KK1,I1,XPLOT(I1,KK1),YPLOT(I1,KK1),
     1	KK2,I2,XPLOT(I2,KK2),YPLOT(I2,KK2)
831	FORMAT(' Junction index, X and Y values for curve #',
     1	I3,':',I4,2(2X,G12.4),/,
     1	' and for curve #',I3,':',I4,2(2X,G12.4))
 
	PRINT 833
833	FORMAT(' Do you want to shift the second curve',
     1	' to minimize the difference? (Y)')
	ACCEPT 10,ANS
	IF(ANS.EQ.'n'.OR.ANS.EQ.'N')THEN
	  VSHIFT=0.000
	ELSE
	  DELTA_Y=YPLOT(I1+1,KK1)-YPLOT(I1,KK1)
	  DELTA_X=XPLOT(I1+1,KK1)-XPLOT(I1,KK1)
	  YVALUE1=YPLOT(I1,KK1)+(XPLOT(I2,KK2)-XPLOT(I1,KK1))*
     1	DELTA_Y/DELTA_X
	  VSHIFT=YVALUE1-YPLOT(I2,KK2)
	  PRINT *,' Shift in Y of the second curve:',VSHIFT
	ENDIF
 
C Storing the result in curve #1
	KK=1
	 DO I=1,I1
	  XX(I,KK)=XPLOT(I,KK1)
	  YY(I,KK)=YPLOT(I,KK1)
	 END DO
	NUMKK=I1
	 DO I=I2,NPTS(KK2)
 	  NUMKK=NUMKK+1
	  XX(NUMKK,KK)=XPLOT(I,KK2)
	  YY(NUMKK,KK)=YPLOT(I,KK2)+VSHIFT
	 END DO
	NPTS(KK)=NUMKK
	PRINT 806,NUMKK
806	FORMAT(' The resulting curve has',I5,' points')
 
C Storing the resulting curve in #1:
C Saving it in XPLOT, YPLOT too:
	DO I=1,NPTS(KK)
	  XPLOT(I,KK)=XX(I,KK)
	  YPLOT(I,KK)=YY(I,KK)
	END DO
	
	PRINT *,' You can save this curve with option 6'
	RETURN
	END
C********************************************************************
C Subroutine to output a curve from XPLOT,YPLOT
C********************************************************************
	SUBROUTINE OUTPUT_CURVE
	PARAMETER (IDIM=2000,KCUR=10)
	REAL*8 XXWORK,YYWORK
	REAL*4 XX,YY,CMAG,CTE,XPLOT,YPLOT,ERPLOT,ERROR
	CHARACTER*4 NCHAR
	CHARACTER NAME*30
	LOGICAL ERROR_PLOT,CHECK_INPUT
	COMMON/BLOCKA/XX(IDIM,KCUR),YY(IDIM,KCUR),ERROR(IDIM,KCUR)
	COMMON/BLOCKB/XPLOT(IDIM,KCUR),YPLOT(IDIM,KCUR),
     1	ERPLOT(IDIM,KCUR),ERROR_PLOT,CHECK_INPUT
	COMMON/BLOCKC/KK,NPTS(KCUR),NCHAR(KCUR),CMAG(KCUR),CTE(KCUR),
     1	COEFF(KCUR)
	COMMON/IOPT/IOPX,IOPY
 
10	FORMAT(A)
 
C Number of the curve:
	IF(KK.NE.1)THEN
	  PRINT *,' NUMBER OF THE CURVE YOU WANT TO OUTPUT ?'
	  READ(5,*) KK1
	ELSE
	  KK1=1
	ENDIF
C Creating a new file:
807	PRINT *,' NAME OF THE OUTPUT FILE ?'
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='NEW',ACCESS='SEQUENTIAL',
     1	ERR=807)
 
	IF(IOPX.EQ.3.AND.IOPY.EQ.4)THEN
	  PRINT 61
61	  FORMAT(' Saving the curve in its "original" form',
     1	'  (Radius, Intensity-sky)')
	  DO I=1,NPTS(KK1)
	    XSAVE=XPLOT(I,KK1)**4
	    YSAVE=10**(0.4*(CMAG(KK1)-YPLOT(I,KK1)))
	    WRITE(1,*) XSAVE,YSAVE
	  END DO
	ELSE
	  PRINT 62
62	  FORMAT(' Storing the curve as it appeared',
     1	' in the graph')
	  DO I=1,NPTS(KK1)
	    WRITE(1,*)XPLOT(I,KK1),YPLOT(I,KK1)
	  END DO
	ENDIF
 
 
	CLOSE(1)
 
	RETURN
	END
C-----------------------------------------------------------------
