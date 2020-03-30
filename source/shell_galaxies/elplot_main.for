C++-------------------------------------------------------------------
C Program ELPLOT_MAIN
C To plot the ellipses and contours from FITELLI
C
C SYNTAX:
C To display a graphic file produced by FITELLI:
C    RUNS ELPLOT_MAIN 1 name.GRA graphic_device [Y/N : same scale in x and y?]
C            xmin,xmax,ymin,ymax 10
C
C Example:
C    RUNS ELPLOT_MAIN 1 test.gra %AGFA_L Y " " 10
C
C Nota1: use it in prompting mode to see all the options.
C Nota2: AUTO_SCALE is selected when a blank is entered for xmin,xmax,ymin,ymax
C For the list of graphic devices type RUNS GRAPHICS ??
C
C JLP
C Version  31/07/90
C-------------------------------------------------------------------
	PROGRAM ELPLOT_MAIN
	CHARACTER*60 NAMEIN,NAMEOUT
10	FORMAT(A)
	CALL JLP_BEGIN
 
80	PRINT 88
88	FORMAT(' MENU :',/,
     1	' 1. Plotting a graphic file (from FITELLI or EFIT) ',/,
     1	' 2. Correction of an old graphic file (from FITELLI) with IFAIL.NE.0',/,
     1	' 3. Correction of ellipse fit parameters (from EFIT)',/,
     1	' 10. Exit',/,
     1	' Enter the option you want: ',$)
	READ(5,*) IOPT
        IF(IOPT.LE.0.AND.IOPT.GE.4) GOTO 999
 
C-----------------------------------------------------------------------
C OPTION 1 : Creation of "canon.dat"
	IF(IOPT.EQ.1)THEN
	  PRINT *,' Name of the graphic input file? (*.gra)'
	  READ(5,10) NAMEIN
	  CALL ELPLOT(NAMEIN)
	  GO TO 999 
        ENDIF

C-----------------------------------------------------------------------
C OPTION 1 or 2 : Correction of old files:

301	  PRINT *,' Name of the graphic input file? (*.gra)'
	  READ(5,10) NAMEIN
	  OPEN(11,FILE=NAMEIN,STATUS='OLD',FORM='UNFORMATTED',
     1	  ERR=301)

302	  PRINT *,' Name of the graphic output file?'
	  READ(5,10) NAMEOUT
	  OPEN(12,FILE=NAMEOUT,STATUS='NEW',FORM='UNFORMATTED',
     1	  ERR=302)

C-----------------------------------------------------------------------
C OPTION 2 : Correction of a file from FITELLI:
	IF(IOPT.EQ.2)THEN
	  CALL CORREC_ELPLOT
        ELSE
C--------------------------------------------------------------------
C OPTION 3 : Changing ellipse parameters from EFIT:
	  CALL CORREC_PARAM
	ENDIF

	CLOSE(11)
	CLOSE(12)
 
999	CALL JLP_END
	STOP
	END
C-------------------------------------------------------------------------
C Subroutine CORREC_ELPLOT to generate artificial ellipse parameters
C to be compatible with ELPLOT ( This incompatibility was created
C by "FITELLI" when IFAIL was non equal to 0
C
	SUBROUTINE CORREC_ELPLOT
	PARAMETER (IDIM=6000)
 	REAL*8 A,B,OX,OY,PHI
	REAL*4 X(IDIM),Y(IDIM)
 
	ISAFE=0
 
	PRINT *,' NUMBER OF THE LEVEL WITH IFAIL.NE.0 ?'
	PRINT *,' (ENTER A HIGH VALUE IF YOU JUST WANT TO CHECK)'
	READ(5,*) KK1
C------------------------------------------------------------
C Reading the file :
C-------------------------------------------------------------
	KK=0
C Reading the contour :
200	READ(11,END=100,ERR=9999) LMAX,(X(I),Y(I),I=1,LMAX)
	KK=KK+1
	PRINT 301,KK,LMAX
301	FORMAT(' LEVEL NUMBER :',I4,' POINTS :',I5)
	WRITE(12) LMAX,(X(I),Y(I),I=1,LMAX)
 
C Reading the ellipse parameters except if KK=KK1
	  IF(KK.NE.KK1)THEN
	  READ(11,END=100,ERR=999) A,B,PHI,OX,OY
	  ELSE
	  PRINT 302
302	FORMAT(' ELLIPSE PARAMETERS : A,B,PHI,OX,OY ? ')
	  READ(5,*) A,B,PHI,OX,OY
	  ENDIF
 
	PRINT 303,A,B,PHI,OX,OY
	WRITE(12) A,B,PHI,OX,OY
303	FORMAT(' ELLIPSE PARAMETERS :',/,
     1	' A,B,PHI,OX,OY : ',5F12.4)
	GO TO 200
 
100	RETURN
999	PRINT *,' ERROR WHILE READING THE FILE (PARAMETERS)'
	RETURN
9999	PRINT *,' ERROR WHILE READING THE FILE (CONTOUR)'
	ISAFE=ISAFE+1
	IF(ISAFE.LT.10)GO TO 200
	RETURN
	END
C-------------------------------------------------------------------------
C Subroutine CORREC_PARAM to change ellipse parameters compatible with ELPLOT
C
	SUBROUTINE CORREC_PARAM
	PARAMETER (IDIM=6000)
 	REAL*8 A,B,OX,OY,PHI
	REAL*4 X(IDIM),Y(IDIM)
	INTEGER*4 NSUP(100)
 
	ISAFE=0
 
	PRINT *,' NUMBER OF THE LEVEL YOU WANT TO CORRECT ?'
	READ(5,*) KK1
C Possible suppression of levels
	DO I=1,100
	NSUP(I)=0
	END DO
	
	PRINT *,' HOW MANY LEVELS DO YOU WANT TO SUPPRESS (0 ?) ?'
	READ(5,*) ISUP
	IF(ISUP.NE.0)THEN
	DO I=1,ISUP
	PRINT 204,I
204	FORMAT(' ',I3,'. ENTRY NUMBER :',$)
	READ(5,*) IWORK
	NSUP(IWORK)=1
	END DO
	ENDIF
C------------------------------------------------------------
C Reading the file :
C-------------------------------------------------------------
	KK=0
C Reading the contour :
 
200	READ(11,END=100,ERR=9999) LMAX,(X(I),Y(I),I=1,LMAX)
	KK=KK+1
	PRINT 301,KK,LMAX
301	FORMAT(' LEVEL NUMBER :',I4,' POINTS :',I5)
	IF(NSUP(KK).EQ.0)THEN
	WRITE(12) LMAX,(X(I),Y(I),I=1,LMAX)
	ENDIF
 
C Reading the ellipse parameters in the file
	  READ(11,END=100,ERR=999) A,B,PHI,OX,OY
	  IF(KK.EQ.KK1)THEN
	  PRINT 302
302	FORMAT(' ELLIPSE PARAMETERS : A,B,PHI,OX,OY ? ')
	  READ(5,*) A,B,PHI,OX,OY
	  ENDIF
 
	PRINT 303,A,B,PHI,OX,OY
	IF(NSUP(KK).EQ.0)THEN
	WRITE(12) A,B,PHI,OX,OY
	ENDIF
303	FORMAT(' ELLIPSE PARAMETERS :',/,
     1	' A,B,PHI,OX,OY : ',5F12.4)
	GO TO 200
 
100	RETURN
999	PRINT *,' ERROR WHILE READING THE FILE (PARAMETERS)'
	RETURN
9999	PRINT *,' ERROR WHILE READING THE FILE (CONTOUR)'
	ISAFE=ISAFE+1
	IF(ISAFE.LT.10)GO TO 200
	RETURN
	END
C-------------------------------------------------------------------
	include 'jlpsub:elplot.for'
