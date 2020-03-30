C++*****************************************************************
C Program ARGS_ELLIPSE
C Program to display a portion of an ellipse on the ARGS
C JLP Version of 15-07-87
C--*****************************************************************
	PROGRAM ARGS_ELLIPSE
      implicit real*8 (a-h,o-z)
	PARAMETER (IDIM=2000)
	REAL*4 XPLOT(IDIM),YPLOT(IDIM)
	REAL*8 Z(5),Z1(5)
	CHARACTER ANS*1,COLOUR*1,NCHAR(2)*4
 
10	FORMAT(A)
 
	PRINT *,' PROGRAM ARGS_ELLIPSE'
	PRINT *,' FOR AN IMAGE CENTRED IN 256.,256. BY "ADISP"'
 
	PRINT *,' SIZE NX,NY OF THE DISPLAYED IMAGE ?'
	READ(5,*) NX,NY
 
	PRINT *,' SCALE IN ARCSEC./PIXEL ?'
	READ(5,*) SCALE
 
12	PRINT *,' CENTRE OX,OY ?'
	READ(5,*) OX,OY
	Z1(4)=OX
	Z1(5)=OY
	PRINT *,' MAJOR AXIS (ARCSEC.),  AND AXIS RATIO B/A ?'
	READ(5,*) Z(1),ECCEN
	Z1(1)=Z(1)/SCALE
	Z1(2)=Z1(1)*ECCEN
	PRINT *,' THETA, THETA MIN AND THETA MAX (DEGREES)'
	READ(5,*) THETA,THEMI,THEMA
	Z1(3)=THETA*PI/180.
 
C Generating the portion of ellipse :
	CALL GENE_ELLIPSE(XPLOT,YPLOT,NPLOT,Z1,THEMI,THEMA,1)
 
C Display a portion of an ellipse on the ARGS
	PRINT *,' COLOUR ? (B, R, Y, OR W)'
	READ(5,10) COLOUR
	PRINT *,' SYMBOL (L=LINE, 54=CROSSES) ?'
	READ(5,10) NCHAR(1)
	CALL ARGS_DISPLAY(XPLOT,YPLOT,NPLOT,NPLOT,
     1	1,NCHAR,COLOUR,NX,NY)
 
	PRINT *,' DO YOU WANT ANOTHER TRY ?'
	READ(5,10) ANS
	IF(ANS.EQ.'Y') GO TO 12
	END
C********************************************************************
	include 'jlpsub:gene_ellipse.for'
	include 'jlpsub:args_display.for'
