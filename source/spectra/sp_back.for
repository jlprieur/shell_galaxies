C++***************************************************************
C SP_BACK
C Program to remove the background from a spectrum.
C Fits a polynomial, and then subtract this fit
C Designed for Parkes data (256 channels)
C
C Version of 03-10-90
C--**************************************************************
	PROGRAM SP_BACK
	PARAMETER (IDIM=2000)
	REAL*8 XX(IDIM),YY(IDIM),YY1(IDIM)
	REAL*8 XC(20),SDEV(20),ERR
	REAL*4 CVELOCITY,XLOW(20),XHIGH(20)
	REAL*4 X1(IDIM),Y1(IDIM),Y2(IDIM)
	REAL*4 XOUT,YOUT
	INTEGER*4 NPTS,NOUT,KK,NPTS1
	LOGICAL PLAN
	INTEGER*4 INOPROMPT
	CHARACTER LIMITS_USER*80,LIMITS_HARD*80,PLOTDEV2*32
	CHARACTER ANS*1,PLOTDEV*32
	CHARACTER INFILE*40,OUTFILE*40,COMMENTS*80,BUFFER*80
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,NCHAR*4,PCOLOR*30
 
C Common block for cursor:
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
C Common block for NEWPLOT:
	COMMON/NEWPLOT1/INOPROMPT,PLAN,LIMITS_USER,PLOTDEV2,
     1	LIMITS_HARD
 
	CALL JLP_BEGIN
10	FORMAT(A)
 
	PRINT 88
88	FORMAT(' Program SP_BACK to subtract the background from a',
     1	' profile/spectrum',/,
     1	' Designed for Parkes data (256 channels)')
 
	PRINT *,' Graphic device? (for direct plot)'
	READ(5,10) PLOTDEV
 
C Reading the input file :
	WRITE(6,*) ' Input ASCII file ?'
	READ(5,10) INFILE
	OPEN(1,FILE=INFILE,STATUS='OLD',ACCESS='SEQUENTIAL')
	READ(1,10) BUFFER
	READ(BUFFER,*) X1(1),Y1(1)
	I=INDEX(BUFFER(27:80),' ')+26
	COMMENTS=BUFFER(I:80)
	  DO I=2,IDIM
	   READ(1,*,END=57) X1(I),Y1(I)
	  END DO
57	CLOSE(1)
	NPTS=I-1
 
	CVELOCITY=X1(128)
	WRITE(6,35) COMMENTS(1:53),CVELOCITY
35	FORMAT(' Comments: ',A53,/,' Central velocity:',G12.3)
 
C Graphic options:
	PLAN='N'
	INOPROMPT=1
	PLOTDEV2='N'
	LIMITS_HARD=' '
	V1=X1(241)
	V2=X1(15)
	F1=-0.5
	F2=1.
	WRITE(LIMITS_USER,*)V1,V2,F1,F2
 
	CHAR1='V helio (km/s)'
	CHAR2='Flux (Jy)'
	TITLE=COMMENTS(1:40)
	NCHAR='L0'
	PCOLOR='Default'
	WRITE(6,28)
28	FORMAT(' Displaying the curves (Use the cursor',
     1	' to set the limits of the zones selected for the fit)')
62	CALL NEWPLOT(X1,Y1,NPTS,IDIM,1,CHAR1,CHAR2,TITLE,
     1	NCHAR,PCOLOR,PLOTDEV,INFILE,COMMENTS)
	
	PRINT *,' Do you want to display the curve again? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GOTO 62
 
C*****************************************************************
C Check if an even number of points have been entered:
	NZONES=NOUT/2
	ITEST=NZONES*2
	IF(ITEST.EQ.NOUT.AND.ITEST.NE.0)THEN
	  WRITE(6,39) NZONES
39	  FORMAT(' Number of zones for the fit: ',I3)
	  DO K=1,NZONES
	    I=(2*K)-1
	    XLOW(K)=XOUT(I)
	    XHIGH(K)=XOUT(I+1)
	    WRITE(6,38) K,XLOW(K),XHIGH(K)
38	    FORMAT(' Zone number ',I3,' Limits: ',2(G12.3,X))
	  END DO
 
	ELSE
 
C Displaying the coordinates of the points drawn by the cursor, if any:
	  DO I=1,NOUT
	    PRINT *,' I, X, Y :',I,XOUT(I),YOUT(I)
	  END DO
 
52	  PRINT *,' Number of zones you want to select for the fit? (0=all)'
	  READ(5,*) NZONES
 
	  DO K=1,NZONES
	    PRINT *,' Lower and upper limits for zone number:',K,' ?'
	    READ(5,*) XLOW(K),XHIGH(K)
	  END DO
 
C If no points, takes everything (except the edges...)
	  NZONES=NZONES+1
	  XLOW(NZONES)=X1(245)
	  XHIGH(NZONES)=X1(15)
 
	ENDIF
 
C*****************************************************************
63	PRINT *,' Order of the polynomial?'
	READ(5,*) KK
 
C Conversion to double precision:
	DO I=1,NPTS
	  XX(I)=X1(I)
	  YY(I)=Y1(I)
	END DO
 
C Selection of the fit points:
	NPTS1=NPTS
	CALL SP_SELECT(XX,YY,XLOW,XHIGH,NPTS1,NZONES)
	IF(NPTS1.EQ.0)THEN
	  PRINT *,' Error: 0 point(s) have been selected !'
	  GOTO 52
	ENDIF
 
C Fitting a polynomial :
	  PRINT *,' Fitting the Polynomial now ...'
	  CALL POLYFIT(XX,YY,NPTS1,KK,XC,SDEV,ERR)
 
	  WRITE(6,25) ERR
25	  FORMAT(' Mean error rms :',G10.3)
	   DO K=1,KK+1
	     WRITE(6,26) K-1,XC(K),SDEV(K)
26	     FORMAT(' Order: ',I3,' Coeff: ',G10.3,' Error: ',G10.3)
	   END DO
	
C Conversion to double precision:
	DO I=1,NPTS
	  XX(I)=X1(I)
	  YY(I)=Y1(I)
	END DO
 
C Computing the model:	
	    DO I=1,NPTS
	      CALL CALPOLY(XX(I),YY1(I),XC,KK)
	    END DO
 
C Plotting the result:
	  DO I=1,NPTS
	   Y2(I)=Y1(I)-YY1(I)
	  END DO
 
C Limits for the plot:
	F1=-0.15
	F2=0.3
	WRITE(LIMITS_USER,*)V1,V2,F1,F2
 
	TITLE=COMMENTS(1:40)
	PLOTDEV2='N'
C	PLOTDEV2='*PGPLOT.PLT/VERSA'
	LIMITS_HARD=' '
	WRITE(6,29)
29	FORMAT(' Displaying the curves (you can use the cursor',
     1	' to get the coordinates to measure the noise...')
	CALL NEWPLOT(X1,Y2,NPTS,IDIM,1,CHAR1,CHAR2,TITLE,
     1	NCHAR,PCOLOR,PLOTDEV,INFILE,COMMENTS)
C Displaying the coordinates of the points drawn by the cursor, if any:
	DO I=1,NOUT
	  PRINT *,' I, X, Y :',I,XOUT(I),YOUT(I)
	END DO
 
	IF(NOUT.EQ.2)THEN
	  XLO=XOUT(1)
	  XHI=XOUT(2)
	ELSE
	  PRINT *,' Enter lower and upper limit for noise computation:'
	  READ(5,*) XLO,XHI
	ENDIF
 
	CALL SP_NOISE(X1,Y2,NPTS,XLO,XHI)
 
	PRINT *,' Do you want to change the order of the polynomial? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GOTO 63
 
	PRINT *,' Do you want to change the selection boundaries? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GOTO 62
 
C Storing the output in an ASCII file :
	WRITE(6,*) ' Output ASCII file ?'
	READ(5,10) OUTFILE
	OPEN(2,FILE=OUTFILE,STATUS='NEW',ACCESS='SEQUENTIAL')
	BUFFER=' '
	WRITE(BUFFER,*) X1(1),Y1(1)
	BUFFER(32:80)=COMMENTS(1:49)
	WRITE(2,10) BUFFER
	  DO I=2,NPTS
	   WRITE(2,*) X1(I),Y2(I)
	  END DO
	CLOSE(2)
 
 	CALL JLP_END
	STOP
	END
C----------------------------------------------------------------------
C SP_SELECT
C To select a zone between XLOW and XHIGH for the fit (compact the
C array in output and update NPTS)
C---------------------------------------------------------------------
	SUBROUTINE SP_SELECT(X1,Y1,XLOW,XHIGH,NPTS,NZONES)
	REAL*8 X1(*),Y1(*)
	REAL*4 XLOW(*),XHIGH(*)
	INTEGER*4 NPTS,NZONES
	LOGICAL GOOD
 
	IF(NZONES.LE.0)THEN
	  PRINT *,' SP_SELECT/ 0 zone(s) selected'
	  RETURN
	ENDIF
 
	II=0
	DO I=1,NPTS
	  GOOD=.FALSE.
	  DO K=1,NZONES
	   IF(X1(I).GE.XLOW(K).AND.X1(I).LE.XHIGH(K)) GOOD=.TRUE.
	  END DO
	  IF(GOOD)THEN
	   II=II+1
	   X1(II)=X1(I)
	   Y1(II)=Y1(I)
	  ENDIF
	END DO
 
	NPTS=II
	RETURN
	END
C----------------------------------------------------------------------
	SUBROUTINE SP_NOISE(X,Y,NPTS,XLOW,XHIGH)
	REAL*4 X(NPTS),Y(NPTS)
	REAL*8 SUM,SUMSQ
	REAL*4 XMEAN,SIGMA
 
	IF(XHIGH.LE.XLOW)THEN
	  PRINT *,' SP_NOISE/Wrong limits'
	  RETURN
	ENDIF
 
	SUM=0.
	SUMSQ=0.
	K=0
	DO I=1,NPTS
	  IF(X(I).GE.XLOW.AND.X(I).LE.XHIGH)THEN
	    K=K+1
	    SUM=SUM+Y(I)
	    SUMSQ=SUMSQ+Y(I)*Y(I)
	  ENDIF
	END DO
 
	IF(K.NE.0)THEN
	  XMEAN=SUM/FLOAT(K)
	  SIGMA=SQRT(SUMSQ/FLOAT(K)-XMEAN*XMEAN)
	  WRITE(6,23) XMEAN,SIGMA
23	  FORMAT(' Mean, sigma',2(G10.3))
	ENDIF
 
	RETURN
	END
C**********************************************************************
	include 'jlpsub:polyfit.for'
