	PROGRAM GRI_ELLI
C++----------------------------------------------------------------------
C Program GRI_ELLI
C From ARGPLOT.FOR
C    Version of 06-11-87
C
C   Plots a portion of an ellipse with the given parameters on the GRINNEL
C     N.B. The centre is shifted one unit towards the origin to allow for
C     the origin at 0, 0 on the args
C     X0, Y0   = the ellipse centre (in the data array)
C----------------------------------------------------------------------
	PARAMETER (PI=3.14159265358979323846D0)
	REAL*8 Z(5)
	REAL*4 XPLOT(1024),YPLOT(1024)
	CHARACTER ANS*1,COLOR*1
 
10	FORMAT(A)
C
	PRINT *,' PROGRAM GRI_ELLI'
C************************************************************
C  Creation of a portion of an ellipse
C************************************************************
C	PRINT *,' SCALE IN ARCSEC./PIXEL ?'
C	READ(5,*) SCALE
	SCALE=1.0
 
12	PRINT *,' CENTRE OX,OY ?'
	READ(5,*) Z(4),Z(5)
 
	PRINT *,' MAJOR AXIS (ARCSEC.),  AND ELLIPTICITY ?'
	READ(5,*) Z(1),ELL
	Z(1)=Z(1)/SCALE
	ECCEN=1.-ELL/10.
	Z(2)=Z(1)*ECCEN
 
	PRINT *,' THETA, THETA MIN AND THETA MAX (0. < DEGREES < 360.)'
	READ(5,*) THETAC,THEMI,THEMA
	Z(3)=THETAC*PI/180.
 
	IOPT=1
	CALL GENE_ELLIPSE(XPLOT,YPLOT,NPOINT,Z,THEMI,THEMA,IOPT)
 
C   The actual plotting command :
	PRINT *,' COLOR : (UPPER CASE !!!!)   R, G, B, OR W ?'
	READ(5,10) COLOR
 
C Initializing the GRINNEL :
	PRINT *,' CALLING GRINNEL_INIT'
	CALL GRINNEL_INIT
 
C Drawing the vector :
	PRINT *,' CALLING GRINNEL_VECTOR'
	CALL GRINNEL_VECTOR(XPLOT,YPLOT,NPOINT,COLOR)
 
C   Clear the overlay plane if required
C
	PRINT *,' DO YOU WANT TO CLEAR THE OVERLAY PLANE ? (N)'
	READ(5,10) ANS
	 IF(ANS.EQ.'Y'.OR.ANS.EQ.'y') THEN
	  CALL GRINNEL_CLEAR(COLOR)
	 ENDIF
 
	PRINT *,' DO YOU WANT ANOTHER TRY ?(N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y') GO TO 12
 
	  CALL GRSEND
	END
C----------------------------------------------------------------------------
	include 'jlpsub:gene_ellipse.for'
	include 'jlpsub:gri_set.for'
