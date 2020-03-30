C++****************************************************************
C PROGRAM FITOU
C
C
C JLP Version of 06-06-91
C--****************************************************************
	PROGRAM FITOU 
        PARAMETER (MAXPOINTS=2000)
	INTEGER*4 PNTR_IN,MADRID(1)
	REAL*4 NIV(100)
	REAL*8 Z,PI,THEMIN,THEMAX,SCALE
	REAL*4 XOUT,YOUT
	REAL*4 XOUT1,YOUT1
	LOGICAL AUTO_GUESS,AUTO_IMPROVE,FOURIER,KAMIKAZE
	LOGICAL*4 INTERAC,TALK
	REAL*4 WORK(1)
	INTEGER*4 ERROR_BARS
	CHARACTER LINE_TYPE*4
	CHARACTER PLOTDEV*32
	CHARACTER NAME*40,COMMENTS*80
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40
	COMMON/VMR/MADRID
 
C Output points :
	COMMON/STR_OUTPUT/XOUT1(200),YOUT1(200),NOUT

	COMMON/BLOCKA/PI,THEMIN,THEMAX
C XOUT and YOUT : input arrays for EFIT
	COMMON/BLOCKB/XOUT(MAXPOINTS),YOUT(MAXPOINTS)
	COMMON/BLOCKC/SCALE
C Output from EFIT and input for GENE_ELLIPSE
C Z(1): major axis
C Z(2): minor axis
C Z(3): Thetac (radians)
C Z(4): XC
C Z(5): YC
	COMMON/FIT1/Z(5),M,IFAIL
 
10	FORMAT(A)

	OPEN(9,FILE='fitou.log',STATUS='unknown',
     1	ACCESS='SEQUENTIAL')
	OPEN(11,FILE='fitou.tmp',STATUS='SCRATCH')
	
	IOPT=0
 
C Inquire the format of the files:
	CALL JLP_BEGIN
	CALL JLP_INQUIFMT
 
        PRINT *,84
84	FORMAT(' Program FITOU Version 05-06-91')

C Input the parameters for drawing the levels
	PRINT *,' Scale in arcsec per pixel ?'
	READ(5,*) SCALE
	PRINT 9000
9000	FORMAT(' Approximative center of the ellipses: X,Y ?')
	READ(5,*) Z(4),Z(5) 

C Inquire the device :
100	PRINT *,' Graphic output (xterm, tektro, postscript ...)?'
	READ(5,10) PLOTDEV
	INTERAC=(PLOTDEV(1:1).EQ.'x'.OR.PLOTDEV(1:1).EQ.'t')
	IF(IOPT.EQ.3) GO TO 500 
 
C Input of the parameters for drawing the levels
 
200	PRINT *,' INPUT FILE ?'
	READ(5,10) NAME
	CALL JLP_VM_READIMAG(PNTR_IN,NX,NY,NAME,COMMENTS)
	WRITE (1,104)NAME
104	FORMAT(/,2X,' INPUT FILE : ',A30,/)
 
C Boundaries :
300	PRINT 3
3	FORMAT(' Enter first, last pixel, and step:')
	READ(5,*) NX1,NX2,ISTEPX
	IF(NX1.GE.NX2.OR.ISTEPX.LE.0) GO TO 300 
 
8000	PRINT 6004
6004	FORMAT(' Enter first, last line, and step:')
	READ(5,*) NY1,NY2,ISTEPY
	IF(NY1.GE.NY2.OR.ISTEPY.LE.0) GO TO 8000
	IF(IOPT.EQ.2) GO TO 500 
 
C Input of the levels
400	PRINT 401
401	FORMAT(' Number of levels ? =')
	READ(5,*) NNIV
	PRINT 212
212	FORMAT(' Values of the levels :',
     1	'(type "RETURN" after each value)')
	 DO 213 INIV=1,NNIV
           READ(5,*) VV
           NIV(INIV)=VV
213	 CONTINUE
 
501	IF(INTERAC)THEN
	 TITLE(2:40)=NAME(1:39)
	 CHAR1=' '
	 CHAR2=' '
	ELSE
	 PRINT *,' Enter the title :'
	 TITLE=' '
	 READ(5,10) TITLE(2:40)
	 PRINT *,' Enter X label :'
	 CHAR1=' '
	 READ(5,10) CHAR1(2:30)
	 PRINT *,' Enter Y label :'
	 CHAR2=' '
	 READ(5,10) CHAR2(2:30)
	ENDIF

C Drawing contours, and getting XOUT1, YOUT1 for the fit: 
500	CALL ISOCONTOURS(MADRID(PNTR_IN),NX,NY,NX,NX1,
	1	NX2,ISTEPX,NY1,NY2,ISTEPY,NIV,NNIV,
	1	CHAR1,CHAR2,TITLE,
	1	PLOTDEV,INTERAC,NAME,COMMENTS)


C Transfer to XOUT, YOUT:
        DO I=1,NOUT
	  XOUT(I)=XOUT1(I)
	  YOUT(I)=YOUT1(I)
        END DO
C
C Fitting an ellipse to the entered points XOUT and YOUT
	AUTO_IMPROVE=.TRUE.
	AUTO_GUESS=.TRUE.
	FOURIER=.TRUE.
	KAMIKAZE=.TRUE.
	TALK=.FALSE.
	CALL EFIT(NOUT,5,AUTO_GUESS,AUTO_IMPROVE,FOURIER,
	1	KAMIKAZE,IFAIL,TALK)
	IF(IFAIL.NE.0) THEN
	  PRINT *,' EFIT/IFAIL =', IFAIL
C Closes plot:
          CALL STR_PLOT(0.,0.,999)
        ELSE
C Generating the ellipse	
	  CALL GENE_ELLIPSE(XOUT,YOUT,NPOINT,Z,0.,0.,0)
 
C Displaying the ellipse on the tektronix
	  ERROR_BARS=0
	  LINE_TYPE="L1"
	  CALL JLP_CURVE(XOUT,YOUT,WORK,NPOINT,LINE_TYPE,ERROR_BARS)
C Closes plot:
          CALL STR_PLOT(0.,0.,999)

        ENDIF


C---------------------------------------------------------
C Entering next option
	PRINT 310
310	FORMAT(' Menu :',/,
     1	' 1: Change the levels',/,
     1	' 2: Change the limits for the display',/,
     1	' 3: Store the graph (plot.plt)',/,
     1	' 4: Change the image and the window parameters',/,
     1	' 5: Change the image only',/,
     1	' 10: Exit',/,
     1	' Enter the option you want : ')
	READ(5,*) IOPT
	IF(IOPT.EQ.1) GO TO 400
	IF(IOPT.EQ.2) GO TO 300 
	IF(IOPT.EQ.3) THEN
	  GO TO 100 
	ENDIF
	IF(IOPT.EQ.4)GO TO 200 
	IF(IOPT.EQ.5)GO TO 200 
 
C End:
C ECALC is called at the end to tidy up the ellipse fit output
        CALL ECALC(KK)
	PRINT *,' Results in "fitou.log"'
	CLOSE(1)
	CLOSE(11)
	CALL JLP_END
	STOP
	END
C************************************************************
	include 'jlpsub:isophote_set.for'
	include 'jlpsub:efit.for'
	include 'jlpsub:gene_ellipse.for'
	include 'jlpsub:str.for'
