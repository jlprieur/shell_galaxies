	PROGRAM ZEROPOINT
C++*******************************************************************
C	Program ZEROPOINT to determine the zero point and an
C estimation of the level of the sky in C.C.D. files from
C photometric measurements in circular apertures.
C
C  Version September 29th 1986
C*********************************************************************
C  Input :profile created by BPROFILE (free-format with
C         number of points, and in each line:
C         radius(I),brightness(I),float(number(I) of points per contour)
C   and  :file created by "READ_CATALOG" reading "SHELL.CAT"
C	1: Log10(D(0) from RC2 in unit 0.1'
C	2: Log10(A :diameter of the aperture in unit 0.1')
C	3: Log10( A/D(0))
C	4: V band magnitude (except when B is attached: B)
C	5: B-V color index
C	6: U-B color index
C	7: * , if uncertainty (from the author himself)
C--*********************************************************************
C Description of the arrays :
C RADPRO : Equivalent radii (arcseconds) of the input profile
C PRO : Input profile (linear)
C
C RADPHOT : Radii (arcseconds) of the photometric measurements
C PHOTOM : Reference magnitudes
C
C MAGNI : Computed magnitude assuming the values of SKY and CMAG
C         (From the input profile).
C--*******************************************************************
	PARAMETER (IDIM=1000)
	PARAMETER (IDIM1=100)
	REAL*8 SUM(IDIM1),SURFACE(IDIM1)
	REAL*8 WWORK,WWORK1,WWORK2,SOM
	REAL*4 DELTAMAG(IDIM1),DELTASKY(IDIM1)
	REAL*4 RADPRO(IDIM),PRO(IDIM),NPRO(IDIM),
     1	MAGNI(IDIM),RADPHOT(IDIM1),PHOTOM(IDIM1)
	INTEGER*4 ITRACE(IDIM1),INDEX(IDIM1)
	CHARACTER ANS*1,NAME*25,COLOR*1,BUFF*80,IOP*4
10	FORMAT(A)
	OPEN(3,FILE='zeropoint.log',STATUS='unknown',
     1	ACCESS='SEQUENTIAL',ERR=4)
	PRINT 20
	WRITE(3,20)
20	FORMAT(' PROGRAM MAGNITUDE',4X,'VERSION OF 10-JUL-86')
 
C Reading the input profile ...
55	PRINT *,' NAME OF THE PROFILE '
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='OLD',ERR=55)
	READ(1,*)NPOINT
	DO I=1,NPOINT
	READ(1,*) WWORK,WWORK1,WWORK2
	RADPRO(I)=WWORK
	PRO(I)=WWORK1
	NPRO(I)=WWORK2
	END DO
 
	WRITE(3,98)NAME
98	FORMAT(/,' INPUT PROFILE  : ',A)
 
C----------------------------------------------------------
C Defining the upper limit for the integration used for
C the display of the curves...
	PRINT 56
56	FORMAT(/,' Maximum radius of the profile for',
     1	' the comparisons ? ',$)
	READ(5,*) RMAX
 
	CALL INDEX_MAX(RADPRO,NPOINT,RMAX,MAXI)
	PRINT *,' Index maxi. for the profile :',MAXI
	PRINT *,' '
C-----------------------------------------------------------
C Reading the photometric file ...
4	PRINT *,' Name of the photometric file'
	PRINT *,' (From READ_CATALOG, with only one galaxy) ?'
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='OLD',ACCESS='SEQUENTIAL',
     1	ERR=4)
 
	PRINT *,' COLOR (B OR V?) :'
	READ(5,10) COLOR
C	PRINT *,' AIR MASS, EXPOSURE TIME IN SECONDS ?'
C	READ(5,*) AIRMASS,TIME
C	CTIME=-2.5*ALOG10(TIME)
	TIME=1.
	CTIME=0.
	AIRMASS=1.
	WRITE(3,106)NAME,COLOR,AIRMASS,TIME
106	FORMAT(/,' INPUT PHOTOMETRIC FILE :',A,/,
     1	' COLOR :',A,5X,' AIR MASS :',F9.3,' EXPOSURE :',
     1	F9.3,'SEC')
 
C Reading and printing the header:
 
	READ(1,800)BUFF
	PRINT 800,BUFF
	WRITE(3,800)BUFF
800	FORMAT(/,A)
	READ(1,10)BUFF
	PRINT 10,BUFF
	WRITE(3,10)BUFF
	READ(1,10)BUFF
 
C The array ITRACE is reset to 0
C It will be filled with 1 if B-V is null in the current line
	DO I=1,IDIM1
	ITRACE(I)=0
	END DO
 
C Reading the photometric file
	BV=0.
 
	DO I=1,IDIM1
	READ(1,802,END=810)WORK1,WORK2,WORK3,
     1	WORK4,WORK5,WORK6
	PRINT 802,WORK1,WORK2,WORK3,WORK4,WORK5,WORK6
	WRITE(3,802)WORK1,WORK2,WORK3,WORK4,WORK5,WORK6
802	FORMAT(9X,6(F9.3,2X))
	BV=BV+WORK5
C Conversion into radii in arcseconds of the aperture
	RADPHOT(I)=(10.**(WORK2))*3.
	   IF(COLOR.EQ.'V')THEN
	   PHOTOM(I)=WORK4
	   ELSE
	   IF(WORK5.EQ.0.)ITRACE(I)=1
	   PHOTOM(I)=WORK4+WORK5
	   ENDIF
	END DO
 
810	CLOSE(1)
	NUMBER=I-1
C ITRAC contains the number of lines with B-V=0.00
	ITRAC=0
	   DO I=1,NUMBER
	   ITRAC=ITRAC+ITRACE(I)
	   END DO
	PRINT 97,I,ITRAC
	WRITE(3,97)I,ITRAC
97	FORMAT(2X,I4,' MEASUREMENTS',/,
     1	2X,'WITH ',I4,' NULL VALUES FOR B-V')
	BV=BV/(NUMBER-ITRAC)
	PRINT 95,BV,COLOR
	WRITE(3,95)BV,COLOR
95	FORMAT(' MEAN B-V :',F12.3,/,
     1	5X,'RADIUS',9X,A1,/)
 
C Generation of the B magnitudes, even if B-V was null
	IF(COLOR.EQ.'B')THEN
	   DO I=1,NUMBER
	   PHOTOM(I)=PHOTOM(I)+FLOAT(ITRACE(I))*BV
	   END DO
	ENDIF
 
C Printing of the integrated magnitudes
	DO I=1,NUMBER
	WRITE(3,96)RADPHOT(I),PHOTOM(I)
	PRINT 96,RADPHOT(I),PHOTOM(I)
96	FORMAT(2X,F9.3,2X,F9.3)
	END DO
C----------------------------------------------------------
C Looking for the last value that we can take into account
C for the comparison with the profile
	CALL INDEX_MAX(RADPHOT,NUMBER,RADPRO(MAXI),
     1	MAXIPHOT)
 
C----------------------------------------------------------
C Correction for atmospheric absorption
C according to Mike Bessel's values
C	IF(COLOR.EQ.'B')THEN
C	ABS0=0.3-0.04*BV
C	ENDIF
C	IF(COLOR.EQ.'V')THEN
C	ABS0=0.17
C	ENDIF
C	IF(COLOR.EQ.'R')THEN
C	ABS0=0.13
C	ENDIF
C	ABS=ABS0*AIRMASS
C	PRINT 128,ABS
C	WRITE(3,128)ABS
	ABS=0.
128	FORMAT(/,' ATMOSPHERIC ABSORPTION :',F12.3)
 
C-----------------------------------------------------------------
C Integration of the profile for each value of RADPHOT(I)
C assuming SKY=0. and generation of the arrays SUM and SURFACE
C INDEX allows the passage from PHOTOM indices to PRO indices
C SURFACE is the number of pixels inside of the aperture
	DO I=1,MAXIPHOT
	CALL INDEX_MAX(RADPRO,NPOINT,RADPHOT(I),MAX)
	INDEX(I)=MAX
	SUM(I)=0.
	SURFACE(I)=0.
	   DO J=1,INDEX(I)
	   SURFACE(I)=NPRO(J)+SURFACE(I)
	   SUM(I)=SUM(I)+PRO(J)*NPRO(J)
	   END DO
	END DO
 
C-----------------------------------------------------------
C Estimation of the first guess
C CMAG from the first value assuming SKY=0. at the first
C passage and then iterating with the previous value
C Converges very quickly.
	SKY=0.
	DO II=1,10
	SOM=SUM(1)-SKY*SURFACE(1)
	   IF(SOM.GT.0.)THEN
	   CMAG=PHOTOM(1)+2.5*DLOG10(SOM)+CTIME
	   ENDIF
C SKY from the last value assuming CMAG is correct
	WWORK=(PHOTOM(MAXIPHOT)-CMAG+CTIME)/-2.5
	WWORK=SUM(MAXIPHOT)-SURFACE(MAXIPHOT)*SKY
     1	-10.**WWORK
	SKY=SNGL(WWORK/SURFACE(MAXIPHOT))+SKY
	END DO
	PRINT *,' FIRST GUESS :'
 
100	PRINT 867,SKY,CMAG
	WRITE(3,867)SKY,CMAG
867	FORMAT(' PROPOSED VALUES FOR SKY AND CMAG :',/,
     1	2X,F12.2,2X,F9.3)
 
	PRINT 101
101	FORMAT(' SKY LEVEL, CMAG (ZERO POINT IN COUNTS/S) ?')
	READ(5,*) SKY,CMAG
	PRINT *,'DO YOU WANT AN AUTOMATIC VERSION ?'
	READ(5,10) ANS
 
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
	IOP='AUTO'
	ELSE
	IOP='MANU'
	ENDIF
 
C-----------------------------------------------------------------
C Iterations on the values of RADPHOT(I)
C
	INCRE=0
105	CONTINUE
 
	DO I=1,MAXIPHOT
	MAGNI(I)=32.
	DELTAMAG(I)=32.
	SOM=SUM(I)-SKY*SURFACE(I)
 
	WWORK=(PHOTOM(I)-CMAG+CTIME)/-2.5
	WWORK1=SOM-10.**WWORK
	DELTASKY(I)=SNGL(WWORK1/SURFACE(I))
 
	IF(SOM.GT.0.)THEN
	MAGNI(I)=-2.5*DLOG10(SOM)+CMAG-CTIME
	DELTAMAG(I)=PHOTOM(I)-MAGNI(I)
	ENDIF
 
	END DO
 
 
	DELTAS=0.
	DELTAM=0.
	DO I=1,MAXIPHOT
	DELTAS=DELTAS+DELTASKY(I)
	DELTAM=DELTAM+DELTAMAG(I)
	END DO
 
	DELTAS=DELTAS/MAXIPHOT
	DELTAM=DELTAM/MAXIPHOT
 
	IF(IOP.NE.'AUTO')THEN
	PRINT 301,CMAG,SKY,COLOR,COLOR
	WRITE(3,301)CMAG,SKY,COLOR,COLOR
301	FORMAT(/,' INTEGRATED MAGNITUDES FOR CMAG =',F9.3,
     1	' AND SKY =',F9.3,/,
     1	5X,'RADIUS',5X,A1,' PROFIL',3X,A1,' PHOTOM',
     1	3X,'DELTA MAG',2X,'DELTA SKY',/)
 
	DO I=1,MAXIPHOT
	PRINT 302,RADPHOT(I),MAGNI(I),PHOTOM(I),
     1	DELTAMAG(I),DELTASKY(I)
	WRITE(3,302)RADPHOT(I),MAGNI(I),PHOTOM(I),
     1	DELTAMAG(I),DELTASKY(I)
302	FORMAT(2X,5(F9.3,2X))
	END DO
 
	PRINT 303,DELTAS,DELTAM
	WRITE(3,303)DELTAS,DELTAM
	ENDIF
303	FORMAT(' MEAN DIFFERENCE IN :',/,
     1	' SKY LEVEL :',F9.3,5X,' MAG :',F9.3)
C------------------------------------------------------------------
	IF(ITEST.EQ.0)THEN
	CMAG=CMAG+DELTAM
	ITEST=1
	ELSE
	SKY=SKY-(DELTAS/2.)
	ITEST=0
	ENDIF
	INCRE=INCRE+1
	WORK1=DELTAS
	WORK2=DELTAM
	IF(WORK1.LT.0.)WORK1=-1.*WORK1
	IF(WORK2.LT.0.)WORK2=-1.*WORK2
	IF(WORK1.LE.0.001.AND.WORK2.LE.0.001)THEN
	PRINT 118,INCRE
	WRITE(3,118)INCRE
118	FORMAT(' THE PROCESS HAS CONVERGED AFTER ',
     1	I5,' ITERATIONS')
	GO TO 107
	ENDIF
	IF(IOP.EQ.'AUTO'.AND.INCRE.LE.300)GO TO 105
 
107	IF(INCRE.GE.299)THEN
	PRINT 119
	WRITE(3,119)
119	FORMAT(' FAILURE TO FIND A SOLUTION')
	ENDIF
 
	PRINT 301,CMAG,SKY,COLOR,COLOR
	WRITE(3,301)CMAG,SKY,COLOR,COLOR
 
	DO I=1,MAXIPHOT
	PRINT 302,RADPHOT(I),MAGNI(I),PHOTOM(I),
     1	DELTAMAG(I),DELTASKY(I)
	WRITE(3,302)RADPHOT(I),MAGNI(I),PHOTOM(I),
     1	DELTAMAG(I),DELTASKY(I)
	END DO
 
	PRINT 303,DELTAS,DELTAM
	WRITE(3,303)DELTAS,DELTAM
 
	PRINT *,' DO YOU WANT ANOTHER TRY ?(Y/N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GO TO 100
 
	PRINT *,' DO YOU WANT TO STORE THE RESULT IN A FILE ?'
	READ(5,10) ANS
 
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
5	PRINT *,'NAME OF THE FILE ?'
	READ(5,10) NAME
	OPEN(2,FILE=NAME,STATUS='NEW',ERR=5)
	WRITE(2,*)MAXI
 
	DO I=1,MAXIPHOT
	IF(SUM(I).GT.0.)THEN
	WRITE(2,*)RADPHOT(I),MAGNI(I)
	ENDIF
	END DO
 
	CLOSE(2)
	ENDIF
 
	PRINT *,' Output in "zeropoint.log"'
	CLOSE(3)
	STOP
	END
