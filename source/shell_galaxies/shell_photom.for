C++--------------------------------------------------------------------
C Program SHELL_PHOTOM
C
C This program generates a graph with the portions of shells which
C have been measured, and creates a table with the shell colours
C and photometry.
C The computed errors are 1*sigma, if more than two measurements are available,
C otherwise they are set to ER0=0.15 (about 15%).
C
C Should be used after "SHELL" or "SHELL_COLOURS".
C SHELL creates a catalog of measurements which is needed in input here.
C It is better to get a set of measurements
C for each of the shells in the B, (V when available,) and R images,
C by fitting different baselines, or removing the galaxy in a different way.
C
C If you use "SHELL_COLOURS" instead of "SHELL", edit the catalogue created
C by this program and transform it to the "SHELL" format
C ("SHELL_COLOURS" has the advantage of displaying 2 profiles on the
C same graph, which can be better for fitting baselines...).
C
C Maximum number of measurements per shell and per colour: IMAX = 10
C (can be easily changed inside of the program)
C
C JLP
C Version of 05-06-97
C--------------------------------------------------------------------
	PROGRAM SHELL_PHOTOM
	PARAMETER (NMAX=200,IMAX=10)
	REAL*4 AMIN,AMAX,RADMIN,RADMAX
	REAL*4 POS_ANGLE,AXIS_RATIO,X0,Y0,AREA,LUMI
	INTEGER*4 NMEAS
	CHARACTER NAME1*60
	CHARACTER FILT*1,FILTER*1
	CHARACTER*10 SHELLNAME
 
	COMMON /SHINT_ARRAYS/SHELLNAME(NMAX),
     1	AMIN(NMAX),AMAX(NMAX),RADMIN(NMAX),RADMAX(NMAX),
     1	POS_ANGLE(NMAX),AXIS_RATIO(NMAX),X0(NMAX),Y0(NMAX),
     1	AREA(NMAX),LUMI(IMAX,NMAX),FILTER(NMAX),NMEAS(NMAX)
 
10	FORMAT(A)
 
C Open the log file :
	OPEN(10,FILE='shell_photom.log',STATUS='unknown')
 
C Read the catalogue :
	CALL SHINT_READ(NAME1,KSECT)
	PRINT *,' Scale in arcsec/pixel?'
	READ(5,*) SCALE
 
C Write the details in the log file :
	WRITE (10,11) NAME1,KSECT,SCALE
11	FORMAT(' Program SHELL_PHOTOM   Version of 05-06-97',/,
     1	' Input file : ',A,/,
     1	' Number of sectors : ',I5,/,
     1	' Scale in arcsec/pixel : ',G12.5)
 
C Display the main menu :
88	 PRINT 85
85	FORMAT(' Menu:',/,
     1	' 1. Displaying the areas which have been measured',/,
     1	' 2. Correcting the input measurements (exp. time, absorp.)',/,
     1	' 3. Computing the colours and output to a "LateX" file',/,
     1	' 10. Exit',/,
     1	'     Enter your choice:')
	  READ(5,*) IOPTION
 
C Selection according to the option :
C**** Option = 1 and 2  *******
	IF(IOPTION.EQ.1.OR.IOPTION.EQ.2)THEN
94	    PRINT *,' Filter: B, V OR  R   (E=Exit) (upper case!!)'
	    READ(5,10) FILT
C Loop while not "Exit":
	    IF(FILT.EQ.'E'.OR.FILT.EQ.'e') GOTO 88
 
C**** Option = 1 *******
	    IF (IOPTION.EQ.1)THEN
C Display the areas which have been measured :
	      CALL SHINT_GRAPH(FILT,KSECT,NAME1,SCALE)
	    ELSE
 
C**** Option = 2 *******
C Do the photometric corrections :
	      CALL SHINT_CORREC(FILT,KSECT)
	    ENDIF
	   GO TO 94
 
C**** Option = 3 *******
	ELSEIF (IOPTION.EQ.3)THEN
	   PRINT 87
87	   FORMAT (' MENU2:',/,
     1	' 1. 1-colour photometry',/,
     1	' 2. 2-colour B,R photometry',
     1	' (with the same number of measurements in B and R)',/,
     1	' 3. 3-colour B,V,R photometry',
     1	' (with the same number of measurements in B,V,R)',/,
     1	' 10. Return to the main menu')
	  READ(5,*)IOPT3
	  IF(IOPT3.EQ.1)THEN
	    CALL SHINT_1COLOUR(KSECT,NAME1,SCALE)
	  ELSEIF(IOPT3.EQ.2)THEN
	    CALL SHINT_2COLOURS(KSECT,NAME1,SCALE)
	  ELSEIF(IOPT3.EQ.3)THEN
	    CALL SHINT_3COLOURS(KSECT,NAME1,SCALE)
	  ENDIF
	  GOTO 88
	ENDIF
 
	CLOSE (10)
	PRINT *,' Logfile is "shell_photom.log"'
	STOP
	END
C--------------------------------------------------------------------
C Subroutine SHINT_GRAPH
C Generates a graph with the portions of shells which have been measured
C--------------------------------------------------------------------
	SUBROUTINE SHINT_GRAPH(FILT,KSECT,NAME1,SCALE)
	PARAMETER (IDIM1=1000,NMAX=200,IMAX=10)
	REAL*8 Z1(5),AAMIN,AAMAX
	REAL*4 XX(IDIM1,NMAX),YY(IDIM1,NMAX),AMIN,AMAX,RADMIN,RADMAX
	REAL*4 POS_ANGLE,AXIS_RATIO,X0,Y0,AREA,LUMI
	INTEGER*4 NPTS(IDIM1),NMEAS
	CHARACTER NCHAR(IDIM1)*4
	CHARACTER NAME1*60,FILTER*1,FILT*1
	CHARACTER PLOTDEV*32,CHAR1*30,CHAR2*30,TITLE*40
	CHARACTER SHELLNAME*10,COMMENTS*80
 
	COMMON /SHINT_ARRAYS/SHELLNAME(NMAX),
     1	AMIN(NMAX),AMAX(NMAX),RADMIN(NMAX),RADMAX(NMAX),
     1	POS_ANGLE(NMAX),AXIS_RATIO(NMAX),X0(NMAX),Y0(NMAX),
     1	AREA(NMAX),LUMI(IMAX,NMAX),FILTER(NMAX),NMEAS(NMAX)
	DATA PI/3.14159/
 
10	FORMAT(A)
 
C Now displaying the sectors :
	II=0
	DO K=1,KSECT
	
	   IF (FILTER(K).EQ.FILT)THEN
 
 	     Z1(1)=RADMIN(K)/SCALE
	     Z1(2)=Z1(1)*AXIS_RATIO(K)
	     Z1(3)=POS_ANGLE(K)*PI/180.
	     Z1(4)=X0(K)
	     Z1(5)=Y0(K)
	     II=II+1
	     NCHAR(II)='L'
             AAMIN=AMIN(K)
             AAMAX=AMAX(K)
	     CALL GENE_ELLIPSE(XX(1,II),YY(1,II),
     1	NPTS(II),Z1,AAMIN,AAMAX,1)
 
	     Z1(1)=RADMAX(K)/SCALE
	     Z1(2)=Z1(1)*AXIS_RATIO(K)
	     II=II+1
	     NCHAR(II)='L'
	     CALL GENE_ELLIPSE(XX(1,II),YY(1,II),
     1	NPTS(II),Z1,AAMIN,AAMAX,1)
	
	     II=II+1
	     NCHAR(II)='L'
	     XX(1,II)=XX(1,II-1)
	     YY(1,II)=YY(1,II-1)
	     XX(2,II)=XX(1,II-2)
	     YY(2,II)=YY(1,II-2)
	     NPTS(II)=2
 
	     II=II+1
	     NCHAR(II)='L'
	     XX(1,II)=XX(NPTS(II-2),II-2)
	     YY(1,II)=YY(NPTS(II-2),II-2)
	     XX(2,II)=XX(NPTS(II-3),II-3)
	     YY(2,II)=YY(NPTS(II-3),II-3)
	     NPTS(II)=2
 
C Drawing the center :
	     II=II+1
	     NCHAR(II)='44'
	     XX(1,II)=X0(K)
	     YY(1,II)=Y0(K)
	     NPTS(II)=1
 
	  ENDIF
 
	END DO
 
 
C Calling NEWPLOT :
	PRINT *,' Graphic device: &xterm, &square, ...'
	READ(5,10) PLOTDEV
	CHAR1=' X (pixels)'
	CHAR2=' Y (pixels)'
	TITLE=NAME1
	IIMAX=IDIM1
        COMMENTS=' '
	CALL NEWPLOT(XX,YY,NPTS,IIMAX,II,CHAR1,CHAR2,TITLE,NCHAR,
     1	PLOTDEV,NAME1,COMMENTS)
 
	RETURN
	END
C-----------------------------------------------------------------------
C Subroutine SHINT_1COLOUR to compute the shell colours
C
C RT: R Total,
C RS : R Surface
C-----------------------------------------------------------------------
	SUBROUTINE SHINT_1COLOUR(KSECT,NAME1,SCALE)
 
	PARAMETER (NMAX=200,IMAX=10)
	REAL*4 AMIN,AMAX,RADMIN,RADMAX,TOTAL_LUMI
	REAL*4 POS_ANGLE,AXIS_RATIO,X0,Y0,AREA,LUMI
	REAL*4 CTE(4),RC(NMAX),ERR_RT(NMAX),ERR_RS(NMAX)
	REAL*4 RT(NMAX),RS(NMAX),ERR_BC(NMAX),ERR_RC(NMAX)
	INTEGER*4 NMEAS
	CHARACTER NAME1*60,FILTER*1,FILT*1,NAME2*40
	CHARACTER SHELLNAME*10,BUFF*80,BSL*1
 
	COMMON /SHINT_ARRAYS/SHELLNAME(NMAX),
     1	AMIN(NMAX),AMAX(NMAX),RADMIN(NMAX),RADMAX(NMAX),
     1	POS_ANGLE(NMAX),AXIS_RATIO(NMAX),X0(NMAX),Y0(NMAX),
     1	AREA(NMAX),LUMI(IMAX,NMAX),FILTER(NMAX),NMEAS(NMAX)
 
10	FORMAT(A)
	PRINT *,' One-colour photometry  with only one colour measured'
     1          '(either R or B)'
 
C ASCII file:
5	PRINT *,' OUTPUT ASCII FILE ?'
	READ(5,10) NAME2
 	OPEN(2,FILE=NAME2,STATUS='NEW',ERR=5)
	WRITE(2,27)
27	FORMAT(' Amin',T7,' SMAmin',T15,' Rcounts',
     1	T28,'  Rt',T36,'  Rsurf',T46,'Shell',/,
     1	' Amax',T7,' SMAmax',T15,' err',T28,' err',
     1	T36,' ERR_Rs',T46,'name',/,/)
 
C (Carriage control = list , to be compatible with the editor and not
C have trouble for printing it directly on PRINTRONIX, or
C transferring it to a microcomputer with ETHERNET)
401	PRINT *,' OUTPUT LATEX FILE ?'
	READ(5,10) NAME2
	OPEN(7,FILE=NAME2,STATUS='NEW',ERR=401)
	PRINT *,' NAME OF THE GALAXY ?'
	READ(5,10) NAME2
 
        BSL = CHAR(92)
	WRITE(7,402)BSL,BSL,BSL,BSL,BSL,BSL,BSL,BSL,NAME2
402	FORMAT(A1,'documentstyle{article}',/,
     1	A1,'pagestyle{plain}',/,A1,'oddsidemargin 0.cm',/,
     1	A1,'topmargin 0.cm',/,A1,'textwidth=18.cm',/,
     1	A1,'textheight=26.cm',/,A1,'begin{document}',/,
     1	A1,'large',/,/,
     1	' Photometry of the shells of : ',A,/)
 
	PRINT *,' COMMENTS ?'
	READ(5,10) BUFF
 
	WRITE(7,403)BSL,BUFF,BSL,BSL,BSL,BSL,BSL,BSL,BSL,BSL,BSL,BSL,
     1              BSL,BSL,BSL,BSL,BSL,BSL,BSL
C Put "\normalsize" instead of "\large" if you want it smaller
403	FORMAT(A1,'vspace{5.mm}',/,/,A,/,/,A1,'large',/,/,
     1	A1,'vspace{8.mm}',
     1	A1,'begin{tabular}{|l|r|r|r|r|r|r|} ',A1,'hline',/,
     1	'& & & & & & ',A1,A1,/,
     1	'Shell & A$_{min}$ & A$_{max}$ & SMA$_{min}$ & SMA$_{max}$',
     1	/,'& mag$_\hbox{surf}$ & mag$_\hbox{T}$ ',A1,A1,/,
     1	'& & & & & & ',A1,A1,/,
     1	'(1) & (2) & (3) & (4) & (5) & (6) & (7) ',A1,A1,/,
     1	'& & & & & & ',A1,A1,' ',A1,'hline',/,
     1	'& & & & & & ',A1,A1)
 
C RT file
15	PRINT *,' Name of the magnitude output file (x,y for plots)?'
	READ(5,10) NAME2
 	OPEN(3,FILE=NAME2,STATUS='NEW',ERR=15)
	WRITE(3,18) NAME1 
18      FORMAT('# EQUI_RADIUS   Magni_T   Magni_T_ERROR   From: ',A)
 
C Entering the constants:
	PRINT 11
11	FORMAT(' Conversion to magnitudes :',/,
     1	' R = CTE(1) *(-2.5*ALOG10(R_COUNTS) + CTE(2)',/,
     1  'or B = CTE(1) *(-2.5*ALOG10(B_COUNTS) + CTE(2)',/,
     1	' Enter the 2 constants:')
	READ(5,*) (CTE(I),I=1,2)
 
C Main loop:
        TOTAL_LUMI=0.
	DO K=1,KSECT
 
C Initializing the counters :
	  SUM_RC=0.
	  SUMSQ_RC=0.
	  SUM_RT=0.
	  SUMSQ_RT=0.
	  SUM_RS=0.
	  SUMSQ_RS=0.
 
C Computing the values and their errors :
	   DO I=1,NMEAS(K)
 
C R counts:
	    VALUE=LUMI(I,K)
	    SUM_RC=VALUE+SUM_RC
	    SUMSQ_RC=VALUE**2+SUMSQ_RC
 
C R surface magnitude:
	    VALUE=-2.5*CTE(1)*ALOG10(LUMI(I,K)/AREA(K))
     1	+CTE(2)+5.*ALOG10(SCALE)
	    SUM_RS=VALUE+SUM_RS
	    SUMSQ_RS=VALUE**2+SUMSQ_RS
 
C R total magnitude:
	    VALUE=-2.5*CTE(1)*ALOG10(LUMI(I,K))+CTE(2)
	    SUM_RT=VALUE+SUM_RT
	    SUMSQ_RT=VALUE**2+SUMSQ_RT
 
	   END DO
 
	  W1=FLOAT(NMEAS(K))
	  RC(K)=SUM_RC/W1
	  RT(K)=SUM_RT/W1
 	  RS(K)=SUM_RS/W1
 
C For the errors I take 1*sigma:
	  IF(W1.GT.1.)THEN
	    ERR_RC(K)=SQRT((SUMSQ_RC - W1*(RC(K)**2))/(W1-1.))
	    ERR_RT(K)=SQRT((SUMSQ_RT - W1*(RT(K)**2))/(W1-1.))
	    ERR_RS(K)=SQRT((SUMSQ_RS - W1*(RS(K)**2))/(W1-1.))
	  ELSE
C Else, with only one measurement, I suppose an error of about 15%
C (which is 0.15 in magnitude, too)
	    ER0=0.15
	    ERR_RC(K)=ER0
	    ERR_RT(K)=ER0
	    ERR_RS(K)=ER0
	  ENDIF
 
C ASCII file:
	  WRITE(2,23) AMIN(K),RADMIN(K),RC(K),RT(K),RS(K),SHELLNAME(K)
23	  FORMAT(F5.1,2X,F6.1,2X,E11.4,2(2X,F6.2),2X,A)
	  WRITE(2,25) AMAX(K),RADMAX(K),ERR_RC(K),ERR_RT(K),ERR_RS(K)
25	  FORMAT(F5.1,2X,F6.1,2X,E11.4,2(2X,F6.2),/)
 
C Latex file:
	  WRITE(7,404) SHELLNAME(K)(1:10),AMIN(K),AMAX(K),RADMIN(K),
     1	RADMAX(K),RT(K),RS(K),BSL,BSL
404	   FORMAT(A14,2(' & ',F6.1),2(' & ',F6.2),/,
     1	2(' & ',F5.2),' ',A1,A1)
	   WRITE(7,405) BSL,ERR_RT(K),BSL,ERR_RS(K),BSL,BSL,BSL,BSL
405	   FORMAT('& & & &',2(' & $',A1,'pm$ ',F5.2),' ',A1,A1,
     1	/,'& & & & & & ',A1,A1)
C RT file:
C New (after 20-10-88): I put the equivalent radius of the upper limit:
	  EQUI_RADIUS=RADMAX(K)*SQRT(AXIS_RATIO(K))
	  WRITE(3,28) EQUI_RADIUS,RT(K),ERR_RT(K)
28	  FORMAT(3(G12.5,1X))
 
          TOTAL_LUMI=TOTAL_LUMI+RC(K)
	END DO
 
C Compute the total luminosity:
	VALUE=-2.5*CTE(1)*ALOG10(TOTAL_LUMI)+CTE(2)
        WRITE(2,406) VALUE,TOTAL_LUMI
        WRITE(7,406) VALUE,TOTAL_LUMI
406     FORMAT(/,' Total magnitude of the shells:',F12.2,
     1         /,' Total counts:',G12.5)

C Ascii file:
	CLOSE(2)
C RT file:
	CLOSE(3)
C Latex file:
	WRITE(7,407) BSL,BSL,BSL
407	FORMAT(A1,'hline',/,A1,'end{tabular}',/,/,A1,'end{document}')
	CLOSE(7)
 
	RETURN
	END
C-----------------------------------------------------------------------
C Subroutine SHINT_2COLOURS to compute the shell colours
C
C RT, BRT : R Total, (B-R)Total
C RS : R Surface
C-----------------------------------------------------------------------
	SUBROUTINE SHINT_2COLOURS(KSECT,NAME1,SCALE)
 
	PARAMETER (NMAX=200,IMAX=10)
	REAL*4 AMIN,AMAX,RADMIN,RADMAX
	REAL*4 POS_ANGLE,AXIS_RATIO,X0,Y0,AREA,LUMI
	REAL*4 CTE(4)
	REAL*4 BC(NMAX),RC(NMAX),BT(NMAX),BS(NMAX),BRT(NMAX)
	REAL*4 RT(NMAX),RS(NMAX),ERR_BC(NMAX),ERR_RC(NMAX)
	REAL*4 ERR_BT(NMAX),ERR_BS(NMAX),ERR_BRT(NMAX)
	REAL*4 ERR_RT(NMAX),ERR_RS(NMAX)
	INTEGER*4 NMEAS
	CHARACTER NAME1*60,FILTER*1,FILT*1,NAME2*40
	CHARACTER SHELLNAME*10,BUFF*80,BSL*1
 
	COMMON /SHINT_ARRAYS/SHELLNAME(NMAX),
     1	AMIN(NMAX),AMAX(NMAX),RADMIN(NMAX),RADMAX(NMAX),
     1	POS_ANGLE(NMAX),AXIS_RATIO(NMAX),X0(NMAX),Y0(NMAX),
     1	AREA(NMAX),LUMI(IMAX,NMAX),FILTER(NMAX),NMEAS(NMAX)
 
10	FORMAT(A)
 
C ASCII file:
5	PRINT *,' OUTPUT ASCII FILE ?'
	READ(5,10) NAME2
 	OPEN(2,FILE=NAME2,STATUS='NEW',ERR=5)
	WRITE(2,27)
27	FORMAT(' Amin',T7,' SMAmin',T15,' Bcounts',T28,' Rcounts',
     1	T41,'  Bt',T49,' (B-R)t',T57,'  Bsurf',T67,
     1	'Shell',/,
     1	' Amax',T7,' SMAmax',T15,' err',T28,' err',
     1	T41,' ERR_Bt',T49,' ERR_BRt',T57,' ERR_Bs',
     1	T67,'name',/,/)
 
C (Carriage control = list , to be compatible with the editor and not
C have trouble for printing it directly on PRINTRONIX, or
C transferring it to a microcomputer with ETHERNET)
401	PRINT *,' OUTPUT LATEX FILE ?'
	READ(5,10) NAME2
	OPEN(7,FILE=NAME2,STATUS='NEW',ERR=401)
	PRINT *,' NAME OF THE GALAXY ?'
	READ(5,10) NAME2
 
        BSL=CHAR(92)
	WRITE(7,402)BSL,BSL,BSL,BSL,BSL,BSL,BSL,BSL,NAME2
402	FORMAT(A1,'documentstyle{article}',/,
     1	A1,'pagestyle{plain}',/,A1,'oddsidemargin 0.cm',/,
     1	A1,'topmargin 0.cm',/,A1,'textwidth=18.cm',/,
     1	A1,'textheight=26.cm',/,A1,'begin{document}',/,
     1	A1,'large',/,/,
     1	' Photometry of the shells of : ',A,/)
 
	PRINT *,' COMMENTS ?'
	READ(5,10) BUFF
 
	WRITE(7,403)BSL,BUFF,BSL,BSL,BSL,BSL,BSL,BSL,BSL,BSL,
     1            BSL,BSL,BSL,BSL,BSL,BSL,BSL,BSL,BSL
C Put "\normalsize" instead of "\large" if you want it smaller
403	FORMAT(A1,'vspace{5.mm}',/,/,A,/,/,A1,'large',/,/,
     1	A1,'vspace{8.mm}',
     1	A1,'begin{tabular}{|l|r|r|r|r|r|r|r|} ',A1,'hline',/,
     1	'& & & & & & & ',A1,A1,/,
     1	'Shell & A$_{min}$ & A$_{max}$ & SMA$_{min}$ & SMA$_{max}$',
     1	/,'& B$_{total}$ & (B-R)$_{total}$ & B$_{surf}$ ',A1,A1,/,
     1	'& & & & & & & ',A1,A1/,
     1	'(1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) ',A1,A1,/,
     1	'& & & & & & & ',A1,A1,' ',A1,'hline',/,
     1	'& & & & & & & ',A1,A1)
 
C B-R file
15	PRINT *,' (B-R) OUTPUT FILE ?'
	READ(5,10) NAME2
 	OPEN(3,FILE=NAME2,STATUS='NEW',ERR=15)
	WRITE(3,18) NAME1 
18      FORMAT('# EQUI_RADIUS   B-R   B-R_ERROR   From: ',A)
 
C Entering the constants:
	PRINT 11
11	FORMAT(' Conversion to magnitudes : (we take B=R+(B-R))',/,
     1	' R = CTE(1) *(-2.5*ALOG10(R_COUNTS) + CTE(2)',/,
     1	' B-R = CTE(3) * (-2.5*ALOG10(B_COUNTS/R_COUNTS)) + CTE(4)',/,
     1	' Enter the 4 constants:')
	READ(5,*) (CTE(I),I=1,4)
 
C Main loop:
	DO K2=1,KSECT/2
 
C Looking for the indices of the red and blue measurements:
	  K=K2*2-1
	  DO K1=K,K+1
	    IF(FILTER(K1).EQ.'B')THEN
	      KBLUE=K1
	    ELSE
	      KRED=K1
	    ENDIF
	  END DO
	
C Test to check if it is really the same shell
	IF(SHELLNAME(KBLUE).NE.SHELLNAME(KRED))THEN
	  WRITE(6,188) SHELLNAME(K)
188	  FORMAT(' Warning: missing colour (?) for shell: ',A,/,
     1	' (Remember: measurements of the same shell should',
     1	' be grouped by 2: B,R, or R,B)')
          WRITE(6,189) SHELLNAME(KBLUE),SHELLNAME(KRED)
189       FORMAT(' Check that this is the same shell: (B and R) ',A,2X,A)
	ENDIF
 
C Initializing the counters :
	  SUM_BC=0.
	  SUMSQ_BC=0.
	  SUM_RC=0.
	  SUMSQ_RC=0.
	  SUM_RT=0.
	  SUMSQ_RT=0.
	  SUM_RS=0.
	  SUMSQ_RS=0.
	  SUM_BRT=0.
	  SUMSQ_BRT=0.
 
C Computing the values and their errors :
	   DO I=1,NMEAS(KRED)
 
C B counts:
	    VALUE=LUMI(I,KBLUE)
	    SUM_BC=VALUE+SUM_BC
	    SUMSQ_BC=VALUE**2+SUMSQ_BC
 
C R counts:
	    VALUE=LUMI(I,KRED)
	    SUM_RC=VALUE+SUM_RC
	    SUMSQ_RC=VALUE**2+SUMSQ_RC
 
C R surface magnitude:
	    VALUE=-2.5*CTE(1)*ALOG10(LUMI(I,KRED)/AREA(KRED))
     1	+CTE(2)+5.*ALOG10(SCALE)
	    SUM_RS=VALUE+SUM_RS
	    SUMSQ_RS=VALUE**2+SUMSQ_RS
 
C R total magnitude:
	    VALUE=-2.5*CTE(1)*ALOG10(LUMI(I,KRED))+CTE(2)
	    SUM_RT=VALUE+SUM_RT
	    SUMSQ_RT=VALUE**2+SUMSQ_RT
 
C B-R index:
	     IF(LUMI(I,KBLUE).EQ.0)THEN
	       PRINT 89,KBLUE
89	       FORMAT(' FATAL ERROR : Blue luminosity=0 for I=',I3)
	       STOP
	     ENDIF
	    RATIO=LUMI(I,KBLUE)/LUMI(I,KRED)
	    VALUE=-2.5*CTE(3)*ALOG10(RATIO)+CTE(4)
	    SUM_BRT=VALUE+SUM_BRT
	    SUMSQ_BRT=VALUE*VALUE+SUMSQ_BRT
 
	   END DO
 
	  W1=FLOAT(NMEAS(KRED))
	  BC(K)=SUM_BC/W1
	  RC(K)=SUM_RC/W1
	  RT(K)=SUM_RT/W1
 	  RS(K)=SUM_RS/W1
 	  BRT(K)=SUM_BRT/W1
 
C For the errors I take 1*sigma:
	  IF(W1.GT.1.)THEN
	    ERR_BC(K)=SQRT((SUMSQ_BC - W1*(BC(K)**2))/(W1-1.))
	    ERR_RC(K)=SQRT((SUMSQ_RC - W1*(RC(K)**2))/(W1-1.))
	    ERR_RT(K)=SQRT((SUMSQ_RT - W1*(RT(K)**2))/(W1-1.))
	    ERR_RS(K)=SQRT((SUMSQ_RS - W1*(RS(K)**2))/(W1-1.))
	    ERR_BRT(K)=SQRT((SUMSQ_BRT - W1*(BRT(K)**2))/(W1-1.))
	  ELSE
C Else, with only one measurement, I suppose an error of about 15%
C (which is 0.15 in magnitude, too)
	    ER0=0.15
	    ERR_BC(K)=ER0
	    ERR_RC(K)=ER0
	    ERR_RT(K)=ER0
	    ERR_RS(K)=ER0
	    ERR_BRT(K)=ER0
	  ENDIF
 
C B total magnitude: B= R+(B-R)
	BT(K)=RT(K)+BRT(K)
	ERR_BT(K)=ERR_RT(K)
	BS(K)=RS(K)+BRT(K)
	ERR_BS(K)=ERR_RS(K)
 
C ASCII file:
	  WRITE(2,23) AMIN(K),RADMIN(K),BC(K),RC(K),BT(K),
     1	BRT(K),BS(K),SHELLNAME(K)
23	  FORMAT(F5.1,2X,F6.1,2X,2(E11.4,2X),3(F6.2,2X),2X,A)
	  WRITE(2,25) AMAX(K),RADMAX(K),ERR_BC(K),ERR_RC(K),
     1	ERR_BT(K),ERR_BRT(K),ERR_BS(K)
25	  FORMAT(F5.1,2X,F6.1,2X,2(E11.4,2X),3(F6.2,2X),/)
 
C Latex file:
	  WRITE(7,404) SHELLNAME(K)(1:10),AMIN(K),AMAX(K),RADMIN(K),
     1	RADMAX(K),BT(K),BRT(K),BS(K),BSL,BSL
404	   FORMAT(A14,2(' & ',F6.1),2(' & ',F6.2),/,
     1	3(' & ',F5.2),' ',A1,A1)
	   WRITE(7,405) BSL,ERR_BT(K),BSL,ERR_BRT(K),ERR_BS(K),BSL,BSL,BSL,BSL
405	   FORMAT('& & & &',3(' & $',A1,'pm$ ',F5.2),' ',A1,A1
     1	/,'& & & & & & & ',A1,A1)
C B-R file:
C New (after 20-10-88): I put the equivalent radius of the upper limit:
	  EQUI_RADIUS=RADMAX(K)*SQRT(AXIS_RATIO(K))
	  WRITE(3,28) EQUI_RADIUS,BRT(K),ERR_BRT(K)
28	  FORMAT(3(G12.5,1X))
 
	END DO
 
C Ascii file:
	CLOSE(2)
C B-R file:
	CLOSE(3)
C Latex file:
	WRITE(7,407) BSL,BSL,BSL
407	FORMAT(A1,'hline',/,A1,'end{tabular}',/,/,A1,'end{document}')
	CLOSE(7)
 
	RETURN
	END
C-----------------------------------------------------------------------
C Subroutine SHINT_3COLOURS to compute the shell colours
C
C RT, BVT, VRT, BRT : R Total, (B-V)Total, (V-R)Total, (B-R)Total
C RS : R Surface
C-----------------------------------------------------------------------
	SUBROUTINE SHINT_3COLOURS(KSECT,NAME1,SCALE)
 
	PARAMETER (NMAX=200,IMAX=10)
	REAL*4 AMIN,AMAX,RADMIN,RADMAX
	REAL*4 POS_ANGLE,AXIS_RATIO,X0,Y0,AREA,LUMI
	REAL*4 CTE(8)
	REAL*4 BT(NMAX),BS(NMAX),BRT(NMAX),BVT(NMAX),VRT(NMAX)
	REAL*4 ERR_BT(NMAX),ERR_BS(NMAX),ERR_BRT(NMAX)
	REAL*4 ERR_BVT(NMAX),ERR_VRT(NMAX)
	INTEGER*4 NMEAS
	CHARACTER NAME1*60,FILTER*1,FILT*1,NAME2*40
	CHARACTER*10 SHELLNAME
 
	COMMON /SHINT_ARRAYS/SHELLNAME(NMAX),
     1	AMIN(NMAX),AMAX(NMAX),RADMIN(NMAX),RADMAX(NMAX),
     1	POS_ANGLE(NMAX),AXIS_RATIO(NMAX),X0(NMAX),Y0(NMAX),
     1	AREA(NMAX),LUMI(IMAX,NMAX),FILTER(NMAX),NMEAS(NMAX)
 
10	FORMAT(A)
5	PRINT *,' OUTPUT ASCII FILE ?'
	READ(5,10) NAME2
 
	OPEN(2,FILE=NAME2,STATUS='NEW',ERR=5)
	WRITE(2,27)
27	FORMAT('  AMIN',T12,'  AMAX',T24,'  RADMIN',T36,'  RADMAX',
     1	T48,'  B T',T60,'  B S',T72,'  (B - V) T',T84,
     1	'  (V - R) T',T96,'  (B - R) T',T108,
     1	'  SHELL NAME',/,
     1	T48,'  ERR_BT',T60,'  ERR_BS',T72,'  ERR_BVT',T84,
     1	'  ERR_VRT',T96,'  ERR_BRT',/,/)
 
15	PRINT *,' (B-V) OUTPUT FILE ?'
	READ(5,10) NAME2
	OPEN(3,FILE=NAME2,STATUS='NEW',ERR=15)
 
19	PRINT *,' (V-R) OUTPUT FILE ?'
	READ(5,10) NAME2
	OPEN(4,FILE=NAME2,STATUS='NEW',ERR=19)
 
21	PRINT *,' (B-R) OUTPUT FILE ?'
	READ(5,10) NAME2
	OPEN(5,FILE=NAME2,STATUS='NEW',ERR=21)
 
	PRINT 11
11	FORMAT(' REMEMBER :',/,
     1	' B = CTE(1) * (-2.5*ALOG10(B_COUNTS)) + CTE(2)',/,
     1	' B-V = CTE(3) * (-2.5*ALOG10(B_COUNTS/V_COUNTS)) + CTE(4)',/,
     1	' V-R = CTE(5) * (-2.5*ALOG10(V_COUNTS/R_COUNTS)) + CTE(6)',/,
     1	' B-R = CTE(7) * (-2.5*ALOG10(B_COUNTS/R_COUNTS)) + CTE(8)',/,
     1	' ENTER THE 8 CONSTANTS :')
	READ(5,*) (CTE(I),I=1,8)
 
	KSECT=(KSECT/3)*3
 
	DO K2=1,KSECT/3
 
C Looking for the indices of the blue, visible and red measurements:
	  K=(K2-1)*3+1
	  DO K1=K,K+2
	    IF(FILTER(K1).EQ.'B')THEN
	      KBLUE=K1
	    ELSEIF(FILTER(K1).EQ.'V')THEN
	      KVIS=K1
	    ELSE
	      KRED=K1
	    ENDIF
	  END DO
 
C Test to check if it is really the same shell
	IF((SHELLNAME(KBLUE).NE.SHELLNAME(KRED)).OR.
     1	(SHELLNAME(KVIS).NE.SHELLNAME(KRED)))THEN
	  PRINT 288,SHELLNAME(K)
288	  FORMAT(' Fatal error: missing colour for shell: ',A,/,
     1	' (Remember: measurements of the same shell should',
     1	' be grouped by 3: B,V,R, or different order)')
	  STOP
	ENDIF
 
C Initializing the counters :
	  SUM_BT=0.
	  SUMSQ_BT=0.
	  SUM_BS=0.
	  SUMSQ_BS=0.
	  SUM_BVT=0.
	  SUMSQ_BVT=0.
	  SUM_VRT=0.
	  SUMSQ_VRT=0.
	  SUM_BRT=0.
	  SUMSQ_BRT=0.
 
C Computing the values and their errors :
	   DO I=1,NMEAS(KRED)
	    VALUE=-2.5*CTE(1)*ALOG10(LUMI(I,KRED))+CTE(2)
	    SUM_BT=VALUE+SUM_BT
	    SUMSQ_BT=VALUE*VALUE+SUMSQ_BT
 
	    VALUE=-2.5*CTE(1)*ALOG10(LUMI(I,KRED)/AREA(KRED))
     1	+CTE(2)+5.*ALOG10(SCALE)
	    SUM_BS=VALUE+SUM_BS
	    SUMSQ_BS=VALUE*VALUE+SUMSQ_BS
 
	    RATIO=LUMI(I,KBLUE)/LUMI(I,KVIS)
	    VALUE=-2.5*CTE(3)*ALOG10(RATIO)+CTE(4)
	    SUM_BVT=VALUE+SUM_BVT
	    SUMSQ_BVT=VALUE*VALUE+SUMSQ_BVT
 
	    RATIO=LUMI(I,KVIS)/LUMI(I,KRED)
	    VALUE=-2.5*CTE(5)*ALOG10(RATIO)+CTE(6)
	    SUM_VRT=VALUE+SUM_VRT
	    SUMSQ_VRT=VALUE*VALUE+SUMSQ_VRT
 
	    RATIO=LUMI(I,KBLUE)/LUMI(I,KRED)
	    VALUE=-2.5*CTE(7)*ALOG10(RATIO)+CTE(8)
	    SUM_BRT=VALUE+SUM_BRT
	    SUMSQ_BRT=VALUE*VALUE+SUMSQ_BRT
 
	   END DO
 
	  W1=FLOAT(NMEAS(KRED))
	  BT(K)=SUM_BT/W1
	  BS(K)=SUM_BS/W1
	  BVT(K)=SUM_BVT/W1
	  VRT(K)=SUM_VRT/W1
	  BRT(K)=SUM_BRT/W1
 
C For the errors I take 1*sigma:
	  IF(W1.GT.1.)THEN
	    ERR_BT(K)=SQRT((SUMSQ_BT - W1*(BT(K)**2))/(W1-1.))
	    ERR_BS(K)=SQRT((SUMSQ_BS - W1*(BS(K)**2))/(W1-1.))
	    ERR_BVT(K)=SQRT((SUMSQ_BVT - W1*(BVT(K)**2))/(W1-1.))
	    ERR_VRT(K)=SQRT((SUMSQ_VRT - W1*(VRT(K)**2))/(W1-1.))
	    ERR_BRT(K)=SQRT((SUMSQ_BRT - W1*(BRT(K)**2))/(W1-1.))
	  ELSE
C Else, with only one measurement, I suppose an error of about 15%
C (which is 0.15 in magnitude, too)
	    ER0=0.15
	    ERR_BT(K)=ER0
	    ERR_BS(K)=ER0
	    ERR_BVT(K)=ER0
	    ERR_VRT(K)=ER0
	    ERR_BRT(K)=ER0
	  ENDIF
 
	  PRINT 23,AMIN(K),AMAX(K),RADMIN(K),RADMAX(K),BT(K),BS(K),
     1	BVT(K),VRT(K),BRT(K),SHELLNAME(K)(1:10)
	  WRITE(2,23) AMIN(K),AMAX(K),RADMIN(K),RADMAX(K),BT(K),BS(K),
     1	BVT(K),VRT(K),BRT(K),SHELLNAME(K)(1:10)
23	  FORMAT(9(G11.4,1X),A)
	  WRITE(2,25) ERR_BT(K),ERR_BS(K),ERR_BVT(K),
     1	ERR_VRT(K),ERR_BRT(K)
25	  FORMAT(T48,5(G11.4,1X),/)
C New (after 20-10-88): I put the equivalent radius of the upper limit:
	  EQUI_RADIUS=RADMAX(K)*SQRT(AXIS_RATIO(K))
	  WRITE(3,28) EQUI_RADIUS,BVT(K),ERR_BVT(K)
	  WRITE(4,28) EQUI_RADIUS,VRT(K),ERR_VRT(K)
	  WRITE(5,28) EQUI_RADIUS,BRT(K),ERR_BRT(K)
28	  FORMAT(3(G12.5,1X))
 
	END DO
 
C ASCII file:
	CLOSE(2)
C B-V:
	CLOSE(3)
C V-R:
	CLOSE(4)
C B-R:
	CLOSE(5)
 
	RETURN
	END
C-----------------------------------------------------------------------
C Subroutine SHINT_READ
C Reads a catalogue of measurements
C
C-----------------------------------------------------------------------
	SUBROUTINE SHINT_READ(NAME1,KSECT)
	PARAMETER (NMAX=200,IMAX=10)
	REAL*4 AMIN,AMAX,RADMIN,RADMAX
	REAL*4 POS_ANGLE,AXIS_RATIO,X0,Y0,AREA,LUMI
	INTEGER*4 NMEAS
	CHARACTER NAME1*60,BUFF*80
	CHARACTER FILT*1,FILTER*1
	CHARACTER*10 SHELLNAME
 
	COMMON /SHINT_ARRAYS/SHELLNAME(NMAX),
     1	AMIN(NMAX),AMAX(NMAX),RADMIN(NMAX),RADMAX(NMAX),
     1	POS_ANGLE(NMAX),AXIS_RATIO(NMAX),X0(NMAX),Y0(NMAX),
     1	AREA(NMAX),LUMI(IMAX,NMAX),FILTER(NMAX),NMEAS(NMAX)
 
10	FORMAT(A)
 
	KSECT=1
C NMEAS(KSECT) = Number of measurements (initialised to 0)
	NMEAS(KSECT)=0
 
C Open the input file (created by "SHELL")
11	PRINT 12
12      FORMAT(' Input catalogue (created by SHELL)',
     1        ' (If file created by SHELL_COLOURS, please edit it ',
     1        ' and put the profile name before the corresp. measur.)')
	READ(5,10) NAME1
 
	  OPEN(1,FILE=NAME1,STATUS='OLD',ERR=989)
 
	    DO I=1,10000
	     READ(1,10,END=105) BUFF

C Goes to upper case:
             CALL JLP_UPCASE(BUFF,80)
 
C Detects the "#" which separates the measurements in the same colour:
	     IF(BUFF(1:1).EQ.'#')THEN
C Increasing KSECT only if measurements exist for the field which has just
C been read:
	       IF(NMEAS(KSECT).NE.0)KSECT=KSECT+1
C NMEAS(KSECT) = Number of measurements (initialised to 0)
	       NMEAS(KSECT)=0
 
C Filter :
	     ELSEIF(BUFF(2:8).EQ.'PROFILE')THEN
C Conversion to upper case:
	       CALL JLP_UPCASE(BUFF,80)
	       IBLUE=MAX(INDEX(BUFF(20:80),'_B'),INDEX(BUFF(20:80),'B_'),
     1		INDEX(BUFF(20:80),'-B'))
	       IF(IBLUE.LE.0)IBLUE=200
	       IVIS=MAX(INDEX(BUFF(20:80),'_V'),INDEX(BUFF(20:80),'V_'),
     1		INDEX(BUFF(20:80),'-V'))
	       IF(IVIS.LE.0)IVIS=200
	       IRED=MAX(INDEX(BUFF(20:80),'_R'),INDEX(BUFF(20:80),'R_'),
     1		INDEX(BUFF(20:80),'-R'))
	       IF(IRED.LE.0)IRED=200
	       IFOUND=MIN(IBLUE,IVIS,IRED,100)
	
	         IF(IBLUE.EQ.IFOUND)THEN
	           FILTER(KSECT)='B'
	         ELSEIF(IVIS.EQ.IFOUND)THEN
	           FILTER(KSECT)='V'
	         ELSEIF(IRED.EQ.IFOUND)THEN
	           FILTER(KSECT)='R'
	         ELSE
	           PRINT 79,I,BUFF(1:80)
79		   FORMAT(' FILTER NOT IDENTIFIED, line #:',I3,/,
     1	   A,/,' Should be "-B","_B","B_","-R","_R","R_","-V","_V" or "V_"',
     1	   '  (or lower case)',/,' FATAL ERROR, exit')
	           STOP
	         ENDIF
 
C Name :
	       II1=MAX(INDEX(BUFF(20:80),'SH')-1,INDEX(BUFF(20:80),'-S0'),
     1	        INDEX(BUFF(20:80),'-S1'))
	       II1=MAX(II1,INDEX(BUFF(20:80),'_S0'),
     1	        INDEX(BUFF(20:30),'_S1'))
	         IF(II1.LE.0)THEN
	           PRINT 81,I,BUFF(1:80)
81		   FORMAT(' SHELL NAME NOT IDENTIFIED, line:# :',I3,/,
     1	   A,/,' Should be "-S01","-S10","S10A","SH2","SH10A",',
     1	   '"_S01","_S10"...(or lower case)',/,' FATAL ERROR, exit')
	           STOP
	         ELSE
	           II2=MAX(INDEX(BUFF(II1+20:80),'_'),
     1		INDEX(BUFF(II1+20:80),'.'))-2
	           SHELLNAME(KSECT)=BUFF(II1+20:II1+II2+20)
	         ENDIF
 
C Position angle :
	     ELSEIF(BUFF(2:15).EQ.'POSITION ANGLE')THEN
	       READ(BUFF(19:80),*)POS_ANGLE(KSECT)
 
C Axis ratio :
	     ELSEIF(BUFF(2:11).EQ.'AXIS RATIO')THEN
	       READ(BUFF(19:80),*)AXIS_RATIO(KSECT)
 
C Centre of the ellipses :
	     ELSEIF(BUFF(2:7).EQ.'CENTRE')THEN
	       READ(BUFF(19:80),*)X0(KSECT),Y0(KSECT)
 
C Limiting angles :
	     ELSEIF(BUFF(2:16).EQ.'LIMITING ANGLES')THEN
	       READ(BUFF(19:80),*)AMIN(KSECT),AMAX(KSECT)
 
C Limiting radii (semi-major axis actually) :
	     ELSEIF(BUFF(2:15).EQ.'LIMITING RADII')THEN
	       READ(BUFF(19:80),*)RADMIN(KSECT),RADMAX(KSECT)
 
C Luminosity and area :
	     ELSEIF(BUFF(2:19).EQ.'LUMINOSITIES, AREA')THEN
	     NMEAS(KSECT)=NMEAS(KSECT)+1
	        IF(IMES.GT.IMAX)THEN
	         PRINT *,' ERROR : MORE THAN',IMAX,' MEASUREMENTS FOR THE',
     1	       ' SAME AREA'
	         NMEAS(KSECT)=IMAX
	        ENDIF
	       READ(BUFF(20:80),*)
     1	WORK,LUMI(NMEAS(KSECT),KSECT),AREA(KSECT)
 
	     ENDIF
 
	    END DO
 
105	  CLOSE(1)
 
	RETURN
989	PRINT *,' ERROR : PLEASE ENTER THE NAME AGAIN :'
	GOTO 11
	END
C*****************************************************************
C Correction for atmospheric absorption :
C OUT=CCDMAG-AIRMASS*(ABSO(1,KCOLOR)
C	+ABSO(2,KCOLOR)*COLOR_INDEX
C Except for CARTER's correction (2 color B,R photometry)
C where there is also a correction B-R in B
C
C Colors:
C	B : 1
C	V : 2
C	R : 3
C******************************************************************
	SUBROUTINE SHINT_CORREC(FILT,KSECT)
	PARAMETER (NMAX=200,IMAX=10)
	REAL*4 AMIN,AMAX,RADMIN,RADMAX
	REAL*4 POS_ANGLE,AXIS_RATIO,X0,Y0,AREA,LUMI
	REAL*4 ABSO(2,4)
	INTEGER*4 NMEAS
	CHARACTER NAME1*60,BUFF*80
	CHARACTER FILT*1,FILTER*1
	CHARACTER*10 SHELLNAME
 
	COMMON /SHINT_ARRAYS/SHELLNAME(NMAX),
     1	AMIN(NMAX),AMAX(NMAX),RADMIN(NMAX),RADMAX(NMAX),
     1	POS_ANGLE(NMAX),AXIS_RATIO(NMAX),X0(NMAX),Y0(NMAX),
     1	AREA(NMAX),LUMI(IMAX,NMAX),FILTER(NMAX),NMEAS(NMAX)
 
507	PRINT 508
508	FORMAT(' WHICH CORRECTION DO YOU WANT ?',/,
     1	' 1: MIKE BESSEL''S',/,
     1	' 2: AAT "STANDARD"',/,
     1	' 3: DAVE CARTER''S',/,
     1	' 4: HELP ',/,
     1	' 10: EXIT',/,
     1	' ENTER THE NUMBER YOU WANT :',$)
	READ(5,*) IOPT5
 
C Possibility of a HELP on line :
	IF(IOPT5.EQ.4)THEN
	  PRINT 519
	  PRINT 509
	  PRINT 510
	  PRINT 511
	  GO TO 507
 
C Return directly to the main menu if error when choosing this option :
	ELSEIF(IOPT5.GT.4)THEN
	  RETURN
	ENDIF
 
C Enter the exposure time and air mass for the image which has been selected :
	PRINT *,' EXPOSURE TIME, AIRMASS ?'
	READ(5,*) EXPOTIME,AIRMASS
 
C Title for the log file :
	WRITE(10,519)
519	FORMAT(/,/,5(1H*),' CORRECTION FOR ATMOSPHERIC ABSOPTION',/)
 
C Mike Bessel's corrections :
	IF(IOPT5.EQ.1)THEN
	  ABSO(1,1)=+0.30
	  ABSO(2,1)=-0.04
	  ABSO(1,2)=+0.17
	  ABSO(2,2)=+0.0
	  ABSO(1,3)=+0.13
	  ABSO(2,3)=+0.0
	  ABSO(1,4)=+0.0
	  ABSO(2,4)=+0.0
	  PRINT 509
	  WRITE(10,509)
509	  FORMAT(' (CORRECTION "MIKE BESSEL")',/,
     1	' Binst = -2.5 log10(Counts/sec) ',
     1	'-(0.30 - 0.04*(B-V)cat)*SEC(Z)',/,
     1	' Vinst = -2.5 log10(Counts/sec) - 0.17*SEC(Z)',/,
     1	' Rinst = -2.5 log10(Counts/sec) - 0.13*SEC(Z)',/,
     1	' (No correction in I)')
 
C Input of the color term for the correction :
	  IF (FILT.EQ.'B')THEN
	    PRINT *,' MEAN VALUE OF (B-V) FOR THE IMAGE ?'
	    READ(5,*) COLOR_INDEX
	  ELSE
	    COLOR_INDEX=0
	  ENDIF
 
	ENDIF
 
C AAT corrections
	IF(IOPT5.EQ.2)THEN
	  ABSO(1,1)=+0.27
	  ABSO(2,1)=-0.01
	  ABSO(1,2)=+0.13
	  ABSO(2,2)=+0.04		!for B-V
	  ABSO(1,3)=+0.13
	  ABSO(2,3)=-0.05		!for V-R
	  ABSO(1,4)=+0.08
	  ABSO(2,4)=+0.01		!for R-I
	  PRINT 510
	  WRITE(10,510)
510	  FORMAT(' (CORRECTION AAT "STANDARD")',/,
     1	' Binst = -2.5 log10(Counts/sec) ',
     1	'-(0.27 - 0.01*(B-V)cat)*SEC(Z)',/,
     1	' Vinst = -2.5 log10(Counts/sec)',
     1	'-(0.13 + 0.04*(B-V)cat)*SEC(Z)',/,
     1	' Rinst = -2.5 log10(Counts/sec)',
     1	'-(0.13 - 0.05*(V-R)cat)*SEC(Z)',/,
     1	' Iinst = -2.5 log10(Counts/sec)',
     1	'-(0.08 + 0.01*(R-I)cat)*SEC(Z)')
 
C Input of the color term for the correction :
	  IF (FILT.EQ.'B'.OR.FILT.EQ.'V')THEN
	    PRINT *,' MEAN VALUE OF (B-V) FOR THE OBJECT ? (0.95?)'
	    READ(5,*) COLOR_INDEX
	  ELSEIF(FILT.EQ.'R')THEN
	    PRINT *,' MEAN VALUE OF (V-R) FOR THE OBJECT ? (0.65?)'
	    READ(5,*) COLOR_INDEX
	  ELSEIF(FILT.EQ.'I')THEN
	    PRINT *,' MEAN VALUE OF (R-I) FOR THE OBJECT ?'
	    READ(5,*) COLOR_INDEX
	  ELSE
	    COLOR_INDEX=0
	  ENDIF
 
	ENDIF
 
C Dave Carter's corrections
	IF(IOPT5.EQ.3)THEN
	  ABSO(1,1)=+0.30
	  ABSO(2,1)=-0.022	!for B-R
	  ABSO(1,2)=+0.17
	  ABSO(2,2)=+0.0
	  ABSO(1,3)=+0.13
	  ABSO(2,3)=+0.0
	  ABSO(1,4)=+0.0
	  ABSO(2,4)=+0.0
	  PRINT 511
	  WRITE(10,511)
511	  FORMAT(' (CORRECTION "DAVE CARTER")',/,
     1	' Binst = -2.5 log10(Counts/sec) ',
     1	'-(0.30 - 0.022*(B-R)cat)*SEC(Z)',/,
     1	' Vinst = -2.5 log10(Counts/sec) - 0.17*SEC(Z)',/,
     1	' Rinst = -2.5 log10(Counts/sec) - 0.13*SEC(Z)',/,
     1	' (No correction in I)')
 
C Input of the color term for the correction :
	  IF (FILT.EQ.'B')THEN
	    PRINT *,' MEAN VALUE OF (B-R) FOR THE OBJECT ?'
	    READ(5,*) COLOR_INDEX
	  ELSE
	    COLOR_INDEX=0
	  ENDIF
	ENDIF
 
C Selecting the good coefficients according to the filter :
	IF(FILT.EQ.'B')THEN
	  KCOLOR=1
	ELSEIF(FILT.EQ.'V')THEN
	  KCOLOR=2
	ELSEIF(FILT.EQ.'R')THEN
	  KCOLOR=3
	ENDIF
 
C Division by the exposure time and correction
	DO K=1,KSECT
	  IF(FILTER(K).EQ.FILT)THEN
	    DO I=1,NMEAS(K)
	      OUTMAG=-2.5*ALOG10(LUMI(I,K)/EXPOTIME)
     1	-AIRMASS*(ABSO(1,KCOLOR)+ABSO(2,KCOLOR)*COLOR_INDEX)
	      LUMI(I,K)=10**(-0.4*OUTMAG)
	    END DO
	  ENDIF
	END DO
 
	RETURN
	END
C------------------------------------------------------------------------
	include 'jlpsub:gene_ellipse.for'
	include 'jlpsub:jlp_upcase.for'
