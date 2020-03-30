C++***************************************************************
C spec_back 
C Program to remove the background from a spectrum.
C Fits a polynomial, and then subtract this fit
C Input spectrum: image file 
C       with flux in first line, wavelengths in second line
C
C Version of 26-05-94
C--**************************************************************
	PROGRAM SPEC_BACK
	CHARACTER IN_NAME*40,IN_COMMENTS*80
	CHARACTER OUT_NAME*40,OUT_COMMENTS*80,PLOTDEV*32
        INTEGER*4 ISTAT,IMODE
	INTEGER*4 MADRID(1),PNTR_1,PNTR_2,NX_1,NX_2,NY_1,NY_2
	COMMON /VMR/MADRID
 
	CALL JLP_BEGIN
10	FORMAT(A)
 
	WRITE(6,22)
22      FORMAT(' Program spec_back        JLP-Version of 23-05-94',/,
     1    ' to subtract the background from a spectrum')
 
	WRITE(6,23)
23      FORMAT(' Graphic device? (for direct plot)')
	READ(5,10) PLOTDEV

C Two options here:
	WRITE(6,24)
24      FORMAT(' Two modes are possible: ',/,
     1         ' 1. Input of individual points (X,Y) with the cursor',/,
     1         ' 2. Input of intervals (X1,X2), (X3,X4),...with the cursor',/, 
     1         ' Enter your choice : ')
        READ(5,*) IMODE
 
C Inquires about the format of the files :
	CALL JLP_INQUIFMT
 
C************************************************************
C Loading input image file
C************************************************************
	WRITE(6,*) ' Input spectrum (image file) ?'
	READ(5,10) IN_NAME
	CALL JLP_VM_READIMAG(PNTR_1,NX_1,NY_1,IN_NAME,IN_COMMENTS)

C Get memory space for output file:
        NX_2=NX_1
        NY_2=NY_1
        ISIZE=NX_2*NY_2*4
	CALL JLP_GETVM(PNTR_2,ISIZE)
 
	CALL BACK_SUBTRACT(MADRID(PNTR_1),MADRID(PNTR_2),NX_1,NY_1,
     1              NX_2,NY_2,PLOTDEV,IN_NAME,IN_COMMENTS,IMODE,ISTAT)

C**** Output file :
        IF(ISTAT.EQ.0)THEN
          WRITE(6,*) 'Output file: '
          READ(5,10) OUT_NAME
	  WRITE(OUT_COMMENTS,1003) IN_NAME(1:12)
1003      FORMAT('Background subracted from ',A)
 	  CALL JLP_WRITEIMAG(MADRID(PNTR_2),NX_2,NY_2,NX_2,
     1             OUT_NAME,OUT_COMMENTS)
        ENDIF

	CALL JLP_END
	STOP
	END
C************************************************************************
	SUBROUTINE BACK_SUBTRACT(IMAGE1,IMAGE2,NX_1,NY_1,
     1              NX_2,NY_2,PLOTDEV,IN_NAME,IN_COMMENTS,IMODE,ISTAT)
	PARAMETER (IDIM=2000)
        REAL*4 IMAGE1(NX_1,*),IMAGE2(NX_2,*)
	REAL*8 XX(IDIM),YY(IDIM)
	REAL*8 XC(20),SDEV(20),ERR,XX1,YY1
	REAL*4 X1(IDIM,2),Y1(IDIM,2)
	REAL*4 XOUT,YOUT
	INTEGER*4 NPTS(2),NOUT,KK,NPTS1
	INTEGER*4 ISTAT,IMODE
	CHARACTER ANS*1,PLOTDEV*32,IN_NAME*(*),IN_COMMENTS*(*)
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,NCHAR(2)*4,PCOLOR(2)*30

C Common block for cursor:
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT

C Status:
        ISTAT=0

10	FORMAT(A)

C Transfer of the spectrum to X,Y array
        IF(NX_1.GT.IDIM)THEN
          WRITE(6,23) IDIM
23        FORMAT('BACK_SUBTRACT/Fatal error, maximum size set to ',I5)
          ISTAT=-1
        ENDIF

C Loading arrays for display:
	NPTS(1)=NX_1
        DO I=1,NPTS(1)
         X1(I,1)=IMAGE1(I,2)
         Y1(I,1)=IMAGE1(I,1)
        ENDDO

C Conversion to double precision:
	DO I=1,NPTS(1)
	  XX(I)=IMAGE1(I,2)
	  YY(I)=IMAGE1(I,1)
	END DO
 
C Graphic output
	CHAR1='Wavelength'
	CHAR2='Flux'
	NCHAR(1)='L0'
        PCOLOR(1)='Default'
	WRITE(6,28)
28	FORMAT(' Displaying the input spectrum')
        IF(IMODE.EQ.1)THEN
	  WRITE(6,29)
29	  FORMAT('Use the cursor to input individual points (X,Y)',
     1           ' for background fit.')
	  TITLE='Enter the points for the fit'
        ELSE
	  WRITE(6,30)
30	  FORMAT('Use the cursor to select the boundaries X1,X2,X3,X4,...',
     1           ' for background fit.')
	  TITLE='Enter the zone boundaries '
        ENDIF
62	CALL NEWPLOT(X1,Y1,NPTS,IDIM,1,CHAR1,CHAR2,TITLE,
     1	NCHAR,PCOLOR,PLOTDEV,IN_NAME,IN_COMMENTS)
	
	PRINT *,' Do you want to display the curve again? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GOTO 62
 
C*****************************************************************
        IF(IMODE.EQ.1)THEN
          NPTS1=NOUT
          DO I=1,NPTS1 
             XX(I)=XOUT(I)
             YY(I)=YOUT(I)
          ENDDO
        ELSE
C Selection of points within XX,YY array:
C Input: XX,YY (NPTS points)
C Output: XX,YY (NPTS1 points)
	  CALL INPUT_ZONES(XX,YY,NPTS(1),NPTS1,XOUT,YOUT,NOUT)
        ENDIF

63	PRINT *,' Order of the polynomial?'
	READ(5,*) KK
 
C Fitting a polynomial :
	PRINT *,' Fitting the Polynomial now ...'
	CALL POLYFIT(XX,YY,NPTS1,KK,XC,SDEV,ERR)
 
	WRITE(6,25) ERR
25	FORMAT(' Mean fitting error rms :',G10.3)
	  DO K=1,KK+1
	    WRITE(6,26) K-1,XC(K),SDEV(K)
26	    FORMAT(' Order: ',I3,' Coeff: ',G10.3,' Error: ',G10.3)
	  END DO
	
C Plotting the original spectrum and the fit:
        NPTS(2)=NPTS(1)
	DO I=1,NPTS(2)
          X1(I,2)=X1(I,1)
          XX1=X1(I,2)
	  CALL CALPOLY(XX1,YY1,XC,KK)
	  Y1(I,2)=YY1
	END DO
 
	NCHAR(2)='L2'
        PCOLOR(2)='Default'
	TITLE='Spectrum and fitted polynomial'
51	WRITE(6,40)
40	FORMAT(' Displaying the fit and the original spectrum')
	CALL NEWPLOT(X1,Y1,NPTS,IDIM,2,CHAR1,CHAR2,TITLE,
     1	NCHAR,PCOLOR,PLOTDEV,IN_NAME,IN_COMMENTS)
	PRINT *,' Do you want to change the window parameters? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GOTO 51 

C Possibility of changing the order of the polynomial:
	PRINT *,' Do you want to change the order of the polynomial? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GOTO 63
 
C Plotting the flattened spectrum:
	DO I=1,NPTS(2)
          XX1=X1(I,2)
	  CALL CALPOLY(XX1,YY1,XC,KK)
	  Y1(I,1)=Y1(I,1)-YY1
	END DO
 
	TITLE=' '
53	WRITE(6,39)
39	FORMAT(' Displaying the flattened spectrum (Use the cursor',
     1	' to define the boundaries for noise estimation)')
	CALL NEWPLOT(X1,Y1,NPTS,IDIM,1,CHAR1,CHAR2,TITLE,
     1	NCHAR,PCOLOR,PLOTDEV,IN_NAME,IN_COMMENTS)
	PRINT *,' Do you want to change the window parameters? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GOTO 53 

C Displaying the coordinates of the points drawn by the cursor, if any:
	DO I=1,NOUT
	  PRINT *,' I, X, Y :',I,XOUT(I),YOUT(I)
	END DO
 
	PRINT *,' Do you want an estimation of the noise? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
	  IF(NOUT.EQ.2)THEN
	    XLO=XOUT(1)
	    XHI=XOUT(2)
	  ELSE
	    PRINT *,' Enter lower and upper limit for noise computation:'
	    READ(5,*) XLO,XHI
	  ENDIF
	  CALL SP_NOISE(X1,Y1,NPTS(1),XLO,XHI,IDIM)
        ENDIF
 
C New possibility of changing the order of the polynomial:
	PRINT *,' Do you want to change the order of the polynomial? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
C Restoring the original spectrum:
	  DO I=1,NPTS(1)
            XX1=X1(I,1)
	    CALL CALPOLY(XX1,YY1,XC,KK)
	    Y1(I,1)=Y1(I,1)+YY1
	  END DO
          GOTO 63
        ENDIF
 
	PRINT *,' Do you want to change the selection boundaries? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
C Restoring the original spectrum:
	  DO I=1,NPTS(1)
            XX1=X1(I,1)
	    CALL CALPOLY(XX1,YY1,XC,KK)
	    Y1(I,1)=Y1(I,1)+YY1
	  END DO
          GOTO 62
        ENDIF

C Transfer of the array to output image: 
	DO I=1,NPTS(1)
          IMAGE2(I,1)=Y1(I,1)
          IMAGE2(I,2)=X1(I,1)
	END DO
 
        RETURN
	END
C----------------------------------------------------------------------
C SP_SELECT_ZONES
C To select a zone between XLOW and XHIGH for the fit (compact the
C array in output and update NPTS)
C---------------------------------------------------------------------
	SUBROUTINE SP_SELECT_ZONES(XX,YY,XLOW,XHIGH,NPTS,NZONES)
	REAL*8 XX(*),YY(*)
	REAL*4 XLOW(*),XHIGH(*)
	INTEGER*4 NPTS,NZONES,II
	LOGICAL GOOD
 
	IF(NZONES.LE.0)THEN
	  PRINT *,' SP_SELECT/ 0 zone(s) selected'
	  RETURN
	ENDIF
 
	II=0
	DO I=1,NPTS
	  GOOD=.FALSE.
	  DO K=1,NZONES
	   IF(XX(I).GE.XLOW(K).AND.XX(I).LE.XHIGH(K)) GOOD=.TRUE.
	  END DO
	  IF(GOOD)THEN
	   II=II+1
	   XX(II)=XX(I)
	   YY(II)=YY(I)
	  ENDIF
	END DO
 
C Number of points to be fitted:
	NPTS=II

C If more than 500 points, takes one out of two:
35      IF(NPTS.GT.500)THEN
        WRITE(6,34) NPTS
34      FORMAT(' SP_SELECT/Warning:     npts =',I8,/,
     1        ' Maximum capacity is 500 points for the fit',/,
     1        ' Therefore I take a subsample of points (one out of two...)')
	  II=0
	  DO I=1,NPTS,2
	     II=II+1
	     XX(II)=XX(I)
	     YY(II)=YY(I)
	  END DO
C Number of points to be fitted:
          NPTS=II
          GOTO 35
        ENDIF

	RETURN
	END
C----------------------------------------------------------------------
	SUBROUTINE SP_NOISE(X,Y,NPTS,XLOW,XHIGH,IDIM)
	REAL*4 X(IDIM,*),Y(IDIM,*)
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
	  IF(X(I,1).GE.XLOW.AND.X(I,1).LE.XHIGH)THEN
	    K=K+1
	    SUM=SUM+Y(I,1)
	    SUMSQ=SUMSQ+Y(I,1)*Y(I,1)
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
C******************************************************************
C INPUT_ZONES
C To select a zone between XLOW and XHIGH for the fit (compact the
C array in output and reduce the number of points)
C Input: XX,YY (NPTS points)
C Output: XX,YY (NPTS1 points)
C******************************************************************
	SUBROUTINE INPUT_ZONES(XX,YY,NPTS,NPTS1,XOUT,YOUT,NOUT)
C Maximum number of zones: 40, therefore MAX_NZONES2=80
        PARAMETER (MAX_NZONES2=80)
        INTEGER*4 NZONES,NOUT,K,I,NPTS,NPTS1,ITEST
        REAL*8 XX(*),YY(*)
        REAL*4 XOUT(*),YOUT(*)
        REAL*4 XLOW(MAX_NZONES2),XHIGH(MAX_NZONES2)

C Check if an even number of points have been entered:
	NZONES=NOUT/2
        IF(NZONES.GT.(MAX_NZONES2/2))THEN
          WRITE(6,35) NZONES,MAX_NZONES2/2
35        FORMAT(' INPUT_ZONES/Error NZONES=',I5,
     1          'whereas only ',I4,' zones are allowed here')
        ENDIF
	ITEST=NZONES*2
	IF(ITEST.EQ.NOUT.AND.ITEST.NE.0)THEN
	  WRITE(6,39) NZONES
39	  FORMAT(' Number of zones for the fit: ',I3)
	  DO K=1,NZONES
	    I=(2*K)-1
	    XLOW(K)=XOUT(I)
	    XHIGH(K)=XOUT(I+1)
	    WRITE(6,38) K,XLOW(K),XHIGH(K)
38	    FORMAT(' Zone number ',I3,' Limits: ',2(G12.3,1X))
	  END DO
 
	ELSE
 
	  PRINT *,' Sorry, you have not entered an even number of points!' 

C Displaying the coordinates of the points drawn by the cursor, if any:
	  DO I=1,NOUT
	    PRINT *,' Cursor #I :',I,' X,Y = ',XOUT(I),YOUT(I)
	  END DO
 
52	  PRINT *,' Number of zones you want to select for the fit?'
	  READ(5,*) NZONES
 
	  DO K=1,NZONES
	    PRINT *,' Lower and upper limits for zone number:',K,' ?'
	    READ(5,*) XLOW(K),XHIGH(K)
	  END DO
 
	ENDIF
 
C Selection of the fit points:
	NPTS1=NPTS
	CALL SP_SELECT_ZONES(XX,YY,XLOW,XHIGH,NPTS1,NZONES)
	IF(NPTS1.EQ.0)THEN
	  WRITE(6,31)
31        FORMAT(' INPUT_ZONE/Error: 0 point(s) have been selected !')
          GOTO 52
	ENDIF

        RETURN
        END
C**********************************************************************
	include 'jlpsub:polyfit.for'
