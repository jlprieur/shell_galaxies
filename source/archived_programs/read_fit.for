C++----------------------------------------------------------------------
C Program READ_FIT
C To read output files from "FITELLI" and extend the shell catalog
C of ellipse fits. This catalog is a Latex file, and can be directly
C output on a laser via Latex.
C
C Example of syntax (for seeing the whole possibilities,
C       use it first in prompting mode):
C  RUNS READ_FIT 1 OLD_ELLIPSE_FIT.CAT 2 N1210_RL.FIT 4 NEW_ELLIPSE_FIT.CAT
C
C JLP
C Version of 23/07/90
C----------------------------------------------------------------------
C Array PARAM(I,J,K) with  i= level index,  j = galaxy index
C
C Key for index K :
C 1 = MAJAX
C 2 = MEANRAD
C 3 = ENUMBER
C 4 = XCENT
C 5 = YCENT
C 6 = THETA
C
C NMEASUR : Number of measurements
C NAMEGAL : Name of the galaxy
C
	PROGRAM READ_FIT
	REAL*4 PARAM,ERRPARAM
	INTEGER*4 NMEASUR
	CHARACTER NAMEGAL*40
	CHARACTER ANS*1,NAME*40
 
	COMMON /RDFIT_DATA/KK,NAMEGAL(1000),NMEASUR(1000),
     1	PARAM(1000,100,6),ERRPARAM(1000,100,6)
 
10	FORMAT(A)
	CALL JLP_BEGIN
 
	KK=0
80	PRINT 88
88	FORMAT(' MENU :',/,
     1	' 1. INPUT OF A CATALOGUE OF PARAMETERS',/,
     1	' 2. INPUT OF A PARAMETER FILE',/,
     1	' 3. OUTPUT OF THE NAMES OF THE GALAXIES',/,
     1	' 4. OUTPUT OF THE CURRENT VERSION OF THE'
     1	' CATALOGUE AND EXIT',/,
     1	' 5. GENERATION OF CURVES',/,
     1	' 10. EXIT',/,
     1	' ENTER THE OPTION YOU WANT : ',$)
	READ(5,*) IOPT
C---------------------------------------------------------------------
C OPTION 1 : Input of a catalogue
C
	IF(IOPT.EQ.1)THEN
	  CALL READ_CATALOG
	  GO TO 80
	ENDIF
 
C---------------------------------------------------------------------
C OPTION 2 : Input of a parameter file
C
	IF(IOPT.EQ.2)THEN
	 CALL READ_PARAMFILE
	 GO TO 80
	ENDIF
 
C---------------------------------------------------------------------
C OPTION 3 : Output of the names of the galaxies
C
	IF(IOPT.EQ.3)THEN
	 CALL OUTPUT_NAMES
	 GO TO 80
	ENDIF
 
C---------------------------------------------------------------------
C OPTION 4 : Output of the current version of the catalogue
C            and exit
C
	IF(IOPT.EQ.4)THEN
	 CALL OUTPUT_CATALOG
	ENDIF
 
C---------------------------------------------------------------------
C OPTION 5 : Generation of curves
C
	IF(IOPT.EQ.5)THEN
	 CALL DISPLAY_CURVES
	 GO TO 80
	ENDIF
 
C End of the programme :
	CALL JLP_END
	STOP
	END
C**********************************************************************
C Subroutine to remove "_" in a word (for word-processor output)
C**********************************************************************
	SUBROUTINE CORREC_NAMEGAL(NAME)
	CHARACTER NAME*40
	DO I=1,40
	IF(NAME(I:I).EQ.'_')NAME(I:I)='-'
	END DO
	RETURN
	END
C*******************************************************************
C Subroutine to sort an array according to its first 8 values
C*******************************************************************
	SUBROUTINE CLASS_WORDS(TEST,INDEX,NPOINT)
	PARAMETER (IDIM=1000)
	CHARACTER*40 TEST(IDIM)
	CHARACTER*40 CWORK
	CHARACTER*8 TEST1(IDIM)
	INTEGER*4 INDEX(IDIM)	
C
C Initialization of INDEX
C
	DO J=1,NPOINT
	  CWORK=TEST(J)
	  TEST1(J)=CWORK(1:8)
	  INDEX(J)=J
	END DO
C
C Method of the "bubble"
C
 
600	INVERS=0
 
	DO J=1,NPOINT-1
	  I1=INDEX(J)
	  I2=INDEX(J+1)
	  IF(TEST1(I1).GT.TEST1(I2))THEN
	    INDEX(J+1)=I1
	    INDEX(J)=I2
	    INVERS=INVERS+1
	  ENDIF	
	END DO
 
	IF(INVERS.NE.0) GO TO 600
 
	RETURN
	END
C---------------------------------------------------------------------
C Subroutine READ_CATALOG
C OPTION 1 : Input of a catalogue
C
C (Structure of a catalogue :
C  Number of objects
C  Name, number of levels and ellipse parameters for each object
C  as in a single file)
C---------------------------------------------------------------------
	SUBROUTINE READ_CATALOG
	REAL*4 PARAM,ERRPARAM
	INTEGER*4 NMEASUR
	CHARACTER NAMEGAL*40
	CHARACTER NAME*40,BUFF*80
	COMMON /RDFIT_DATA/KK,NAMEGAL(1000),NMEASUR(1000),
     1	PARAM(1000,100,6),ERRPARAM(1000,100,6)
 
10	FORMAT(A)
 
103	PRINT *,' NAME OF THE CATALOGUE ?'
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='OLD',
     1	ACCESS='SEQUENTIAL',ERR=103)
 
	NOBJECT=0
C Skipping the first 3 lines ("begin document")
	DO II=1,3
	  READ(1,10,END=999)BUFF
	END DO
 
C Reading the file :
	READ(1,104)NOBJECT
	PRINT 104,NOBJECT
104	FORMAT(/,'%vspace {3.mm}',/,' Number of galaxies : ',I4)
 
C loop on the objects:
	DO J=1,NOBJECT
	 KK=KK+1
	 READ(1,105)NAMEGAL(KK)
105	 FORMAT(/,'%eject',/,' Name : ',A)
	 PRINT 106,NAMEGAL(KK)
106	 FORMAT(' NAME :',A)
	 READ(1,108)NMEASUR(KK)
108	 FORMAT(/,'%vspace {3.mm}',/,' Number of levels :',I4)
 
C Skipping " begin tabular" :
	 READ(1,407) 
407	 FORMAT(/,'%vspace{3.mm}',/,
     1	'%begin {tabular}{llllll}',/,
     1	' M.axis & Mean rad & Ellip. & X cent &',
     1	' Y cent & Theta %%',/,
     1	' & & & & & %%')
 
C Loop on the levels :
	   DO I=1,NMEASUR(KK)
	    READ(1,110)(PARAM(I,KK,JJ),JJ=1,6)
	    READ(1,110)(ERRPARAM(I,KK,JJ),JJ=1,6)
110	    FORMAT(5(F9.3,' &'),F9.3)
	   END DO
C "End tabular"
	  READ(1,409)
409	  FORMAT('%end {tabular}')
	END DO
 
999	CLOSE(1)
	IF(NOBJECT.EQ.0)THEN
	  PRINT *,' Catalogue empty'
	ENDIF
	RETURN
	END
C---------------------------------------------------------------------
C Subroutine READ_PARAMFILE
C OPTION 2 : Input of a parameter file
C
C (Structure of a parameter file :
C  Name of the object
C  Number of levels
C  Ellipse parameters and error on these parameters for each level)
C---------------------------------------------------------------------
	SUBROUTINE READ_PARAMFILE
	REAL*4 PARAM,ERRPARAM
	INTEGER*4 NMEASUR
	CHARACTER NAMEGAL*40
	CHARACTER ANS*1,NAME*40
	COMMON /RDFIT_DATA/KK,NAMEGAL(1000),NMEASUR(1000),
     1	PARAM(1000,100,6),ERRPARAM(1000,100,6)
 
10	FORMAT(A)
 
203	PRINT *,' NAME OF THE FILE ?'
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='OLD',ACCESS='SEQUENTIAL',ERR=203)
 
	KK=KK+1
	READ(1,10)NAMEGAL(KK)
	PRINT 205,NAMEGAL(KK)
205	FORMAT(' Name : ',A)
	READ(1,*)NMEASUR(KK)
	PRINT 208,NMEASUR(KK)
208	FORMAT(' Number of levels :',I4)
	  DO I=1,NMEASUR(KK)
	    READ(1,*)(PARAM(I,KK,JJ),JJ=1,6)
	    READ(1,*)(ERRPARAM(I,KK,JJ),JJ=1,6)
	  END DO
 
	CLOSE(1)
	RETURN
	END
C---------------------------------------------------------------------
C Subroutine OUTPUT_NAMES
C OPTION 3 : Output of the names of the galaxies
C---------------------------------------------------------------------
	SUBROUTINE OUTPUT_NAMES
	REAL*4 PARAM,ERRPARAM
	INTEGER*4 NMEASUR
	CHARACTER NAMEGAL*40
	CHARACTER NAME*40,BSL*1
	COMMON /RDFIT_DATA/KK,NAMEGAL(1000),NMEASUR(1000),
     1	PARAM(1000,100,6),ERRPARAM(1000,100,6)
 
10	FORMAT(A)
 
	NOBJECT=KK
303	PRINT *,' NAME OF THE OUTPUT FILE ?'
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='unknown',ACCESS='SEQUENTIAL',ERR=303)
 
C Back slash (Pb, Unix, Vax, etc...)
	BSL = CHAR(92)
	WRITE(1,306) BSL,BSL,BSL
306	FORMAT(A,'documentstyle {article}',/,
     1	A,'begin {document}',/,
     1	A,'large')
	PRINT 304,BSL,NOBJECT,BSL
	WRITE(1,304)BSL,NOBJECT,BSL
304	FORMAT(/,A,'vspace {3.mm}',/,' Number of galaxies : ',I4,
     1	/,A,'vspace {8.mm}')
 
	DO J=1,NOBJECT
	  WRITE(1,305)BSL,J,NAMEGAL(J)
305	  FORMAT(/,A,'vspace {3.mm}',/,I4,' Name : ',A)
	  WRITE(1,308)NMEASUR(J)
308	  FORMAT(/,' Number of levels :',I4)
	END DO
 
	WRITE(1,309) BSL
309	FORMAT(A,'end {document}')
 
	CLOSE(1)
	RETURN
	END
C---------------------------------------------------------------------
C Subroutine OUTPUT_CATALOG
C OPTION 4 : Output of the current version of the catalogue
C
C (Structure of a catalogue:
C  Number of objects
C  Name, number of levels and ellipse parameters for each object)
C---------------------------------------------------------------------
	SUBROUTINE OUTPUT_CATALOG
	PARAMETER (IDIM=1000)
	REAL*4 PARAM,ERRPARAM
	INTEGER*4 INDEX(IDIM),NMEASUR
	CHARACTER NAMEGAL*40,CWORK*40
	CHARACTER NAME*40,BSL*1
	COMMON /RDFIT_DATA/KK,NAMEGAL(1000),NMEASUR(1000),
     1	PARAM(1000,100,6),ERRPARAM(1000,100,6)
 
10	FORMAT(A)
 
	NOBJECT=KK
403	PRINT *,' NAME OF THE OUTPUT CATALOGUE ?'
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='unknown',
     1	ACCESS='SEQUENTIAL',ERR=403)
 
C Classification of the catalogue :
	CALL CLASS_WORDS(NAMEGAL,INDEX,NOBJECT)
 
C Back slash (Pb, Unix, Vax, etc...)
	BSL = CHAR(92)
	WRITE(1,406) BSL,BSL,BSL
406	FORMAT(A,'documentstyle {article}',/,
     1	A,'begin {document}',/,
     1	A,'large')
	PRINT 104,BSL,NOBJECT
	WRITE(1,104)BSL,NOBJECT
104	FORMAT(/,A,'vspace {3.mm}',/,' Number of galaxies : ',I4)
 
	DO JJ=1,NOBJECT
	  J=INDEX(JJ)
C Replaces "_" by "-" in NAMEGAL(J) (for the word-processor "LATEX") :
	  CWORK=NAMEGAL(J)
	  CALL CORREC_NAMEGAL(CWORK)
	  WRITE(1,405)BSL,CWORK
405	  FORMAT(/,A,'eject',/,' Name : ',A)
	  WRITE(1,408)BSL,NMEASUR(J)
408	  FORMAT(/,A,'vspace {3.mm}',/,' Number of levels :',I4)
	  WRITE(1,407)BSL,BSL,BSL,BSL,BSL,BSL
407	  FORMAT(/,A,'vspace{3.mm}',/,
     1	A,'begin {tabular}{llllll}',/,
     1	' M.axis & Mean rad & Ellip. & X cent &',
     1	' Y cent & Theta ',A,A,/,
     1	' & & & & & ',A,A)
	    DO I=1,NMEASUR(J)
	     WRITE(1,410) (PARAM(I,J,K),K=1,6),BSL,BSL
	     WRITE(1,410) (ERRPARAM(I,J,K),K=1,6),BSL,BSL
410	     FORMAT(5(F9.3,' &'),F9.3,' ',A,A)
	    END DO
	  WRITE(1,409) BSL
409	  FORMAT(A,'end {tabular}')
	END DO
 
	WRITE(1,411) BSL
411	FORMAT(/,A,'end {document}')
	CLOSE(1)
 
	RETURN
	END
 
C---------------------------------------------------------------------
C Subroutine DISPLAY_CURVES
C OPTION 5 : Generation of curves
C
C---------------------------------------------------------------------
	SUBROUTINE DISPLAY_CURVES
	PARAMETER (IDIM=1000)
	REAL*4 PARAM,ERRPARAM
	REAL*4 XPLOT(IDIM,60),YPLOT(IDIM,60)
	REAL*4 ERRPLOTY(IDIM,60),ERRPLOTX(IDIM,60)
	REAL*8 Z(5),THEMIN,THEMAX
	REAL*4 WORKX(IDIM),WORKY(IDIM)
	INTEGER*4 NMEASUR,NUMB(IDIM),NPTS(IDIM)
	CHARACTER NAMEGAL*40,FILEOUT*40
	CHARACTER NCHAR(60)*4,PCOLOR(60)*30,SYMBOL(6)*20
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,PLOTDEV*32
	CHARACTER ANS*1
	LOGICAL ERROR_BAR
 
	COMMON /RDFIT_DATA/KK,NAMEGAL(1000),NMEASUR(1000),
     1	PARAM(1000,100,6),ERRPARAM(1000,100,6)
 
	DATA SYMBOL/'MAJOR AXIS','MEAN RADIUS','ELLIPTICITY',
     1	'X CENTRE','Y CENTRE','PHI'/
 
10	FORMAT(A)
 
	KP=0
	ERROR_BAR=.FALSE.
	IELLIPSE=0
	NOBJECT=KK
	PRINT 104,BSL,NOBJECT
104	FORMAT(/,A,'vspace {3.mm}',/,' Number of galaxies : ',I4)
500	PRINT 509
509	FORMAT(' Menu :',/,
     1	' 1. Ellipse parameters',/,
     1	' 2. Special curves (with error bars...)',/,
     1	' 3. Drawing shells',/,
     1	' 4. Displaying the curves',/,
     1	' 10. Return to the main menu',/,
     1	' Enter the number you want : ')
	READ(5,*) IOP
	IF(IOP.GT.4)RETURN
 
C*****************************************************************
C------------- IOP=1 : Ellipse parameters-----
	IF(IOP.EQ.1)THEN
 
	IELLIPSE=1
 
C Input of the indices :
	PRINT 502
502	FORMAT(' Number of sets of measurements you want to',
     1	' display ?')
	READ(5,*) NUMBER
	PRINT 503
503	FORMAT(' Enter entry number and symbol for each set :')
	PRINT 506
506	FORMAT(' L=line 4XX="+" 1X=point 2X=white tri. 3X=black tri.',
     1	/,'4X="+" 5X="X" 6X=white sq. 7X=black sq.',
     1	' 8X=white cir. 9X=black cir.',/,
     1	' (X is the size in 0.05 inch)')
	DO K=1,NUMBER
	  PRINT 504,K
504	  FORMAT(' SET NUMBER ',I4,' := ',$)
C (Nota : KP= previous value for the number of curves)
	  READ(5,518) NUMB(KP+K),NCHAR(KP+K)
          PCOLOR(KP+K) = 'Default'
518	  FORMAT(I4,A)
	  PRINT *,NAMEGAL(NUMB(KP+K))
	END DO
 
C Choice of the coordinates :
	PRINT 501
501	FORMAT(' REMEMBER : ',/,
     1	' 1=MAJAXIS    2=MEANRADIUS   3=ELLI ',
     1	' 4=XCENT   5=YCENT   6=PHI ',/,
     1	' ENTER THE NUMBERS YOU WANT FOR X AND Y AXIS: ',$)
	READ(5,*) KXX,KYY
 
	PRINT 519
519	FORMAT(' ENTER THE CONSTANTS BY WHICH YOU WANT TO',
     1	' MULTIPLY X AND Y VALUES: ')
	READ(5,*) CTX,CTY
 
C Filling the arrays XPLOT and YPLOT :
	DO K=1,NUMBER
	 J=NUMB(KP+K)
	 NPTS(KP+K)=NMEASUR(J)
	  DO I=1,NMEASUR(J)
	   XPLOT(I,KP+K)=CTX*PARAM(I,J,KXX)
	   YPLOT(I,KP+K)=CTY*PARAM(I,J,KYY)
	   ERRPLOTY(I,KP+K)=0.
	  END DO
	END DO
 
C (KP : Number of curves)
	KP=NUMBER+KP
	GO TO 500
	ENDIF
 
C*****************************************************************
C--------------IOP=2 : Special curves -------------------
 
C Possibility of displaying shell parameters :
540	IF(IOP.EQ.2)THEN
C (KP : Number of curves)
	 KP=KP+1
	 PRINT 506
	 PRINT *,' NUMBER OF THE INPUT ARRAY AND SYMBOL ?'
	 READ(5,518) NUMB(KP),NCHAR(KP)
         PCOLOR(KP)='Default'
	 JS=NUMB(KP)
	 NPTS(KP)=NMEASUR(JS)
	 PRINT *,NAMEGAL(JS)
	 PRINT 521
521	 FORMAT(' NUMBER OF THE COLUMNS FOR X AND Y AXIS : ',$)
	 READ(5,*) KXSHELL,KYSHELL
 
	 PRINT 520
520	 FORMAT(' ENTER THE CONSTANTS BY WHICH YOU WANT TO',
     1	' MULTIPLY X AND Y VALUES: ')
	 READ(5,*) CTX,CTY
 
C Storing the paramaters in the KP th curve :
	   DO I=1,NMEASUR(JS)
	    XPLOT(I,KP)=CTX*PARAM(I,JS,KXSHELL)
	    YPLOT(I,KP)=CTY*PARAM(I,JS,KYSHELL)
	   END DO
 
C Inluding error bars :
	 PRINT 522
522	 FORMAT(' DO YOU WANT TO DISPLAY ERROR BARS ? (Y)')
	 READ(5,10) ANS
	  IF(ANS.NE.'N'.AND.ANS.NE.'n')ERROR_BAR=.TRUE.
 
	 PRINT 523
523	 FORMAT(' NUMBER OF THE COLUMN FOR Y ERROR : ',$)
	 READ(5,*) KERROR
	   DO I=1,NMEASUR(JS)
	    ERRPLOTY(I,KP)=CTY*PARAM(I,JS,KERROR)
	   END DO
 
	GO TO 500
	ENDIF
 
C*****************************************************************
C--------------IOP=3 : Shells -------------------
 
C Possibility of drawing shells :
	IF(IOP.EQ.3)THEN
	PRINT *,' NUMBER OF THE INPUT ARRAY?'
	READ(5,*) JS
 
	NSHELL=NMEASUR(JS)
C (KP : Number of curves) :
	KP=NSHELL
 
	PRINT *,NAMEGAL(JS)
	PRINT 531
531	FORMAT(' NUMBER OF THE COLUMNS FOR M.AXIS, THETA0, ',
     1	'THETA MIN, THETA MAX AND B/A :')
	READ(5,*) KAXIS,KTHETA,KMIN,KMAX,KRATIO
 
	PRINT *,'  (Rotation of +90 degrees for Theta,',
     1	' Theta min, Theta max)'
 
C Main loop :
	DO I=1,NSHELL
 
C Major axis :
	Z(1)=PARAM(I,JS,KAXIS)
 
C Minor axis :
	XRATIO=PARAM(I,JS,KRATIO)
	Z(2)=Z(1)*XRATIO
 
C Nota : 90. degrees are added to all the angles to rotate the drawing
C Theta0 (transformation in radians):
	Z(3)=3.14159*(PARAM(I,JS,KTHETA)+90.)/180.
 
C Centre of the ellipses:
	Z(4)=0.D0
	Z(5)=0.D0
 
C Rotation of 90 degrees for Theta, Theta min and Theta max :
	THEMIN=PARAM(I,JS,KMIN)+90.
	THEMAX=PARAM(I,JS,KMAX)+90.
 
C IOPELLI=1 : Part of an ellipse
C NPSHELL is an output parameter from ELLIPSE
	IOPELLI=1
	CALL GENE_ELLIPSE(WORKX,WORKY,NPSHELL,Z,THEMIN,
     1	THEMAX,IOPELLI)
 
C Storing the parameters in the I th curve :
	  NPTS(I)=NPSHELL
	  NCHAR(I)='L'
          PCOLOR(I)='Default'
	  DO J=1,NPSHELL
	   XPLOT(J,I)=WORKX(J)
	   YPLOT(J,I)=WORKY(J)
	  END DO
 
	END DO
 
	GO TO 500
	ENDIF
 
C*****************************************************************
C--------------IOP=4 : Displaying the curves -------------------
 
	IF(IOP.EQ.4)THEN
	 IWINDOW=0
 
C Return if no curve :
	 IF(KP.EQ.0)THEN
	  PRINT *,' NO CURVE HAS BEEN ENTERED ...'
	  GO TO 500
	 ENDIF
	 DO I=1,KP
	  PRINT 550,I,NCHAR(I),PCOLOR(I)
550	  FORMAT(' CURVE: ',I3,'  SYMBOL: ',A,' COLOR:', A)
	 END DO
 
C Displaying the arrays :
	PRINT *,' DEVICE : TEKTRO, CIFER_T5, CANON, ARGS1 ?'
	READ(5,10) PLOTDEV
 
C Loading  the axis labels :
	  IF(IELLIPSE.EQ.1)THEN
	   CHAR1=SYMBOL(KXX)
	   CHAR2=SYMBOL(KYY)
	  ELSE
	   PRINT *,' ENTER X LABEL :'
	   READ(5,10) CHAR1
	   PRINT *,' ENTER Y LABEL :'
	   READ(5,10) CHAR2
	  ENDIF
 
	PRINT *,' ENTER TITLE :'
	READ(5,10) TITLE
	NMAX=IDIM
	  IF(.NOT.ERROR_BAR)THEN
	    CALL NEWPLOT(XPLOT,YPLOT,NPTS,NMAX,KP,CHAR1,
     1	CHAR2,TITLE,NCHAR,PCOLOR,PLOTDEV,' ',' ')
	  ELSE
	    CALL NEWPLOT_ERROR(XPLOT,YPLOT,ERRPLOTX,ERRPLOTY,NPTS,NMAX,
     1	KP,CHAR1,CHAR2,TITLE,NCHAR,PCOLOR,PLOTDEV,' ',' ')
	  ENDIF
 
C Possibility of storing the curves in a file :
	PRINT *,' DO YOU WANT TO OUTPUT THESE CURVES IN A FILE ? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'y'.OR.ANS.EQ.'Y')THEN
11	  PRINT *,' NAME OF THE OUTPUT FILE ?'
	  READ(5,10) FILEOUT
	  OPEN(3,FILE=FILEOUT,STATUS='unknown',
     1	ERR=11)
	  DO K=1,KP
	    WRITE(3,*) ' SHELL #',K
	      DO I=1,NPTS(K),5
	        WRITE(3,*)NINT(100.*XPLOT(I,K)),
     1	NINT(100.*YPLOT(I,K)),0.
	      END DO
	    WRITE(3,*) -1000,-1000,0.
	  END DO
	 CLOSE (3)
	ENDIF
 
	GO TO 500
	ENDIF
 
	RETURN
	END
C---------------------------------------------------------------------	
	include 'jlpsub:gene_ellipse.for'
