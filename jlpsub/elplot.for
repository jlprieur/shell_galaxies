C-------------------------------------------------------------------------
C Subroutine ELPLOT
C
C To display the points and the fitted ellipses (FITELLI)
C
C Originally from Dave Carter
C Version July 17th 1987
C-------------------------------------------------------------------------
	SUBROUTINE ELPLOT(NAMEF)
	PARAMETER (IDIM=800,IDIM1=6000)
	PARAMETER (KCUR=100)
 	REAL*8 A,B,OX,OY,PHI
	REAL*4 X(IDIM1),Y(IDIM1)
 	REAL*4 X1(IDIM,KCUR),Y1(IDIM,KCUR)
	INTEGER*4 NPTS(KCUR)
	CHARACTER NAMEF*60,TITLE*40,CHAR1*30,CHAR2*30
	CHARACTER NCHAR(KCUR)*4,PCOLOR(KCUR)*30
	CHARACTER NYN*1,PLOTDEV*32,COMMENTS*80
	include 'jlpsub:jlp_graphic_help.for'
 
10	FORMAT(A)
	ISAFE=0
C
C Opens input file :
 
C	PRINT *,' NAME OF FILE TO PLOT??'
C	READ(5,10) NAMEF
	OPEN(11,FILE=NAMEF,FORM='UNFORMATTED',
	1	STATUS='OLD',ERR=99)
 
C Increment :
	THINC=3.14159/195
C------------------------------------------------------------
C Reading the file :
C-------------------------------------------------------------
	K=0
C--------
C Reading the contour and storing only a subsample of the contour :
 
200	READ(11,END=100,ERR=999) LMAX,(X(I),Y(I),I=1,LMAX)
C	PRINT *,' LMAX,X(1)Y(1)----10',LMAX,(X(I),Y(I),I=1,10)
 
C Defining the subsample :
	K=K+1
	ICNT = 1
94	  IF(LMAX.LT.800) GOTO 95
	  ICNT = ICNT*2
	  LMAX = LMAX/2
	  GOTO 94
95	  DO 101 I=1,LMAX
	  X1(I,K)=X(ICNT*I)
	  Y1(I,K)=Y(ICNT*I)
101	  CONTINUE

C Close the isophote:
	LMAX=LMAX+1
	X1(LMAX,K)=X1(1,K)
	Y1(LMAX,K)=Y1(1,K)
	NPTS(K)=LMAX
	NCHAR(K)='L0'
        PCOLOR(K)='Default'
 
C----
C Reading the ellipse parameters and generating an ellipse :
 
300	READ(11,END=100,ERR=1999) A,B,PHI,OX,OY
C	PRINT *,' A,B,PHI,OX,OY',A,B,PHI,OX,OY 
 
	K=K+1
	COSPHI=COS(PHI)
	SINPHI=SIN(PHI)
	THETA=-THINC
	  DO 20 I=1,402
	  THETA=THETA+THINC
	  XX1=A*COS(THETA)
	  YY1=B*SIN(THETA)
	  X1(I,K)=XX1*COSPHI-YY1*SINPHI+OX
	  Y1(I,K)=YY1*COSPHI+XX1*SINPHI+OY
20	  CONTINUE
 
	NPTS(K)=402
	NCHAR(K)='L1'
        PCOLOR(K)='Default'
 
C Suppression of the level if LMAX=0
	IF(LMAX.EQ.0)THEN 
	  K=K-2
	ENDIF
 
C Return to the beginning of the loop
	GO TO 200
 
C K : number of curves
100	NCURVE=K
C	  PRINT *,' NCURVE',NCURVE
	  IF(NCURVE.EQ.0)THEN
 	   PRINT *,' ELPLOT/ Fatal error: NCURVE=0 !'
	   RETURN
	  ENDIF
C----------------------------------------------------------
C Displaying the curves :
 
	CHAR1=' '
	CHAR2=' '
	TITLE=NAMEF
	CALL CORREC_TITLE(TITLE)
	NMAX=IDIM
78	PRINT *,' OUTPUT DEVICE : (? for help):'
	READ(5,10) PLOTDEV
	IF((PLOTDEV(1:1).EQ.'?').OR.(PLOTDEV(1:1).EQ.' '))THEN
	   PRINT 3333
	   GOTO 78
	ENDIF
        COMMENTS=' '
	CALL NEWPLOT(X1,Y1,NPTS,NMAX,NCURVE,CHAR1,CHAR2,TITLE,
	1	NCHAR,PCOLOR,PLOTDEV,NAMEF,COMMENTS)
	CLOSE(11)
	RETURN
99	PRINT *,' ELPLOT/ Error opening data file'
	RETURN
999	PRINT *,' ELPLOT/ Error reading data file (contour)'
	ISAFE=ISAFE+1
	IF(ISAFE.LT.10)GO TO 200
	CLOSE(11)
	RETURN
1999	PRINT *,' ELPLOT/ Error reading data file (parameters)'
	ISAFE=ISAFE+1
	IF(ISAFE.LT.10)GO TO 300
	CLOSE(11)
	RETURN
	END
C-----------------------------------------------------------------
	SUBROUTINE CORREC_TITLE(TITLE)
	CHARACTER*40 TITLE
	DO I=1,40
	IF(TITLE(I:I).EQ.'_')TITLE(I:I)='-'
	END DO
	RETURN
	END
