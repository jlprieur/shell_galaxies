C++------------------------------------------------------------------
C Program CYLIN
C To project spherical coordinates (alpha, delta) on
C a cylinder limited at Delta_lim
C
C JLP
C Previous version of 05-07-88
C Version of 02/04/2007
C------------------------------------------------------------------
	PROGRAM CYLIN
	PARAMETER(IDIM=10000,IDIM1=10000,NMAGMAX=8)
	REAL*4 ALPHA(IDIM1),DELTA(IDIM1),XMAG(IDIM1)
	REAL*4 XX1(IDIM1),YY1(IDIM1),XMAG1(IDIM1)
	REAL*4 XX2(IDIM1),YY2(IDIM1),XMAG2(IDIM)
	REAL*4 XX3(IDIM1),YY3(IDIM1),XMAG3(IDIM)
	REAL*4 XX4(IDIM1),YY4(IDIM1),XMAG4(IDIM)
	REAL*4 XPLOT(IDIM,NMAGMAX),YPLOT(IDIM,NMAGMAX)
	INTEGER*4 NPTS(NMAGMAX)
	CHARACTER NCHAR(NMAGMAX)*4,SYMB(9)*4,PLOTDEV*32
        CHARACTER PCOLOR(NGAMAX)*30
	CHARACTER XLABEL*30,YLABEL*30,TITLE*40,ANS*1
	DATA SYMB/'1   ','41  ','42  ','43  ','44  ',
     1	'45  ','46  ','45  ','48  '/
	DATA PI/3.14159/
 
10	FORMAT(A)
 
C******************
C Reading the data:
	CALL READ_COORD(ALPHA,DELTA,XMAG,NSTAR)
 
C****************************
C Transforming the coordinates
C RAD: Radius of the cylinder:
	PRINT *,' RADIUS OF THE CYLINDER:'
	READ(5,*) RAD
	PRINT *,' ENTER DELTA_LIM: (DEGREES)'
	READ(5,*) DELTA_LIM
	DELTA_LIM=DELTA_LIM*PI/180.
 
C Possibility of symmetry:
	PRINT *,' SYMMETRY RELATIVE TO Y AXIS ?  (N)'
	READ(5,10) ANS
 
	IF((ANS.EQ.'Y').OR.(ANS.EQ.'y'))THEN
	   DO I=1,NSTAR
	     ALPHA(I)=2.*PI-ALPHA(I)
	   END DO
	ENDIF
 
 
C Changing the coordinates:
	CALL CHANGE_COORD(ALPHA,DELTA,XMAG,NSTAR,
     1	XX1,YY1,XMAG1,NSTAR1,XX2,YY2,XMAG2,NSTAR2,
     1	XX3,YY3,XMAG3,NSTAR3,RAD,DELTA_LIM)
C
	PRINT *,' NSTAR1, NSTAR2, NSTAR3',NSTAR1,NSTAR2,NSTAR3
 
C***********************
C Generating the curves:
	PRINT *,' LIMITING MAGNITUDE : (INTEGER < 7)'
	READ(5,*) NMAG
 
 
C Symbols: ('41' for NMAG)
	DO I=NMAG,1,-1
	  NCHAR(I)=SYMB(NMAG-I+1)
          PCOLOR(I)='Default'
	END DO
 
	PRINT *,' PLOT DEVICE ?'
	READ(5,10) PLOTDEV
	XLABEL=' '
	YLABEL=' '
	TITLE=' '
 
C First curves : cylinder
C Many pieces in X and Y : (otherwise, too small)
	NPARTSX=3
	NPARTSY=1
	PRINT *,' NUMBER OF PARTS IN X, AND Y ? (3,1)'
	READ(5,*) NPARTSX,NPARTSY
 
	HIGHT=2.*RAD*TAN(DELTA_LIM)
	XLENTH=2.*PI*RAD
 
	DO KY=1,NPARTSY
 
C Boundaries for the display:
	  YBOT=FLOAT(KY-1)*HIGHT/FLOAT(NPARTSY)-HIGHT/2.
	  YTOP=FLOAT(KY)*HIGHT/FLOAT(NPARTSY)-HIGHT/2
 
	  DO KX=1,NPARTSX
 
C Boundaries for the display:
	    XBOT=FLOAT(KX-1)*XLENTH/FLOAT(NPARTSX)
	    XTOP=FLOAT(KX)*XLENTH/FLOAT(NPARTSX)
 
	      NSTAR4=0
 
	      DO I=1,NSTAR1
	        IF((XX1(I).GE.XBOT).AND.(XX1(I).LE.XTOP)
     1	.AND.(YY1(I).GE.YBOT).AND.(YY1(I).LE.YTOP))THEN
	          NSTAR4=NSTAR4+1
	          XX4(NSTAR4)=XX1(I)
	          YY4(NSTAR4)=YY1(I)
	          XMAG4(NSTAR4)=XMAG1(I)
	        ENDIF
	      END DO
 
	    CALL GENE_CURVES(XX4,YY4,XMAG4,NSTAR4,XPLOT,YPLOT,
     1	IDIM,NMAG,NPTS)
 
	    PRINT *,' NPTS',(NPTS(J),J=1,NMAG)
 
C Output of the curve:
	    KCUR=NMAG
	    CALL NEWPLOT(XPLOT,YPLOT,NPTS,IDIM,KCUR,
     1	XLABEL,YLABEL,TITLE,NCHAR,PCOLOR,PLOTDEV,' ',' ')
 
	  END DO
	END DO
 
C*****************************
C Second curve : northern disc
	CALL GENE_CURVES(XX2,YY2,XMAG2,NSTAR2,XPLOT,YPLOT,
     1	IDIM,NMAG,NPTS)
 
	PRINT *,' NPTS',(NPTS(K),K=1,NMAG)
 
C Drawing a circle:
	KCUR=NMAG+1
	NPTS(KCUR)=MIN(200,IDIM)
	NCHAR(KCUR)='L   '
	DELTA_I=2*PI/FLOAT(NPTS(KCUR))
	DO I=1,NPTS(KCUR)
	  XPLOT(I,KCUR)=RAD*COS(I*DELTA_I)
	  YPLOT(I,KCUR)=RAD*SIN(I*DELTA_I)
	END DO
 
C Output of the second curve:
	CALL NEWPLOT(XPLOT,YPLOT,NPTS,IDIM,KCUR,
     1	XLABEL,YLABEL,TITLE,NCHAR,PLOTDEV,' ',' ')
 
C***************************
C Third curve : southern disc
	CALL GENE_CURVES(XX3,YY3,XMAG3,NSTAR3,XPLOT,YPLOT,
     1	IDIM,NMAG,NPTS)
 
	PRINT *,' NPTS',(NPTS(K),K=1,NMAG)
 
C Here, using the same circle as the second curve:
	KCUR=NMAG+1
 
C Output of the third curve:
	CALL NEWPLOT(XPLOT,YPLOT,NPTS,IDIM,KCUR,
     1	XLABEL,YLABEL,TITLE,NCHAR,PLOTDEV,' ',' ')
 
 
	END
C---------------------------------------------------------------
C Subroutine READ_COORD
C to read the coordinates from a direct access file:
C---------------------------------------------------------------
	SUBROUTINE READ_COORD(ALPHA,DELTA,XMAG,NSTAR)
	REAL*4 ALPHA(*),DELTA(*),XMAG(*)
	REAL*4 FSAO,ALPHAH,DELTAD
	INTEGER*2 NCT,IMV,IPG,ISP
	INTEGER*4 NSTAR
	CHARACTER*40 FILENAME
	DATA PI/3.14159/
 
10	FORMAT(A)
 
12	PRINT *,' Input file?'
	READ(5,10) FILENAME
	OPEN(1,FILE=FILENAME,STATUS='OLD',ACCESS='DIRECT',ERR=12)
 
C Reading the values:
C and conversion to radians
	PI12=PI/12.
	PI180=PI/180.
 
	DO I=1,30000
	  READ(1,REC=I,ERR=20) NCT,FSAO,ALPHAH,DELTAD,
     1	IMV,IPG,ISP
	  XMAG(I)=FLOAT(IMV)/100.
	  ALPHA(I)=PI12*ALPHAH
	  DELTA(I)=PI180*DELTAD
	END DO
	
C
	PRINT *,' The file has not been completly read'
 
20	NSTAR=I-1
	PRINT *,' ',NSTAR,' STARS'
	CLOSE(1)
 
	RETURN
	END
C---------------------------------------------------------------
C Subroutine GENE_CURVES
C
C Input:
C XX, YY
C XMAG
C NSTAR
C IDIM
C NMAG
C
C Output:
C XPLOT, YPLOT
C NPTS
C---------------------------------------------------------------
	SUBROUTINE GENE_CURVES(XX,YY,XMAG,NSTAR,XPLOT,YPLOT,
     1	IDIM,NMAG,NPTS)
	REAL*4 XX(*),YY(*),XMAG(*)
	REAL*4 XPLOT(IDIM,*),YPLOT(IDIM,*)
	INTEGER*4 NPTS(*)
 
C Erasing the counter:
	DO K=1,NMAG
	   NPTS(K)=0
	END DO
 
C Sorting the stars according to their magnitudes:
	DO I=1,NSTAR
	  K=MAX(INT(XMAG(I)),0)+1
	   IF(K.LE.NMAG)THEN
	     NPTS(K)=NPTS(K)+1
	     XPLOT(NPTS(K),K)=XX(I)
	     YPLOT(NPTS(K),K)=YY(I)
	   ENDIF
	END DO
 
	RETURN
	END
C-------------------------------------------------------------------
C Subroutine CHANGE_COORD
C Transforming the coordinates
C
C Warning: ALPHA and DELTA in radians !!!!!!!!!!!!!!!
C-------------------------------------------------------------------
	SUBROUTINE CHANGE_COORD(ALPHA,DELTA,XMAG,NSTAR,
     1	XX1,YY1,XMAG1,NSTAR1,XX2,YY2,XMAG2,NSTAR2,
     1	XX3,YY3,XMAG3,NSTAR3,RAD,DELTA_LIM)
	REAL*4 ALPHA(*),DELTA(*),XMAG(*)
	REAL*4 XX1(*),YY1(*),XMAG1(*),XX2(*),YY2(*),XMAG2(*)
	REAL*4 XX3(*),YY3(*),XMAG3(*)
	REAL*4 DELTA_LIM
	INTEGER*4 NSTAR,NSTAR1,NSTAR2
 
C RAD: Radius of the cylinder:
C RAD1: Radius of the sphere containing the bottom
C     and top edges of the cylinder
	RAD1=RAD/COS(DELTA_LIM)
 
C Erasing the counters:
	NSTAR1=0
	NSTAR2=0
	NSTAR3=0
 
C---------
C Main loop:
 
	DO I=1,NSTAR
 
C Under DELTA_LIM, simple projection on the equatorial plane:
C (Southern disc)
	  IF(DELTA(I).LT.-1.*DELTA_LIM)THEN
	    NSTAR3=NSTAR3+1
	    XX3(NSTAR3)= RAD1*COS(DELTA(I))*COS(ALPHA(I))
	    YY3(NSTAR3)= RAD1*COS(DELTA(I))*SIN(ALPHA(I))
	    XMAG3(NSTAR3)=XMAG(I)
 
C Over DELTA_LIM, simple projection on the equatorial plane:
C (Northern disc)
	  ELSEIF(DELTA(I).GT.DELTA_LIM)THEN
	    NSTAR2=NSTAR2+1
	    XX2(NSTAR2)= RAD1*COS(DELTA(I))*COS(ALPHA(I))
	    YY2(NSTAR2)= RAD1*COS(DELTA(I))*SIN(ALPHA(I))
	    XMAG2(NSTAR2)=XMAG(I)
 
C Else, cylindrical coordinates:
	  ELSE
	    NSTAR1=NSTAR1+1
	    XX1(NSTAR1)=RAD*ALPHA(I)
	    YY1(NSTAR1)=RAD*TAN(DELTA(I))
	    XMAG1(NSTAR1)=XMAG(I)
	  ENDIF
 
	END DO
	
	RETURN
	END
