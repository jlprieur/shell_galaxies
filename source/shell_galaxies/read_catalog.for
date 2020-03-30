	PROGRAM READ_CATALOG
C++************************************************************
C	PROGRAM READ_CATALOG
C To read the catalogue of shell galaxies
C
C I, IDENT : identification
C F, FIELD : number of the field in the ESO/SRC survey
C W, WEST  : distance of the object from the east edge of the plate
C N, NORTH : distance of the object from the south edge of the plate
C R, RA    : right ascension
C D, DEC   : declination
C
C E, 1, ENV   : environment (I: isolated, C: cluster, G: small group
C               (less than 10))
C C, 2, COMM  : comments on the shells and extenal features surrounding the
C	        galaxy.
C S, 3, SHAPE : description of the galaxy
C A, 4, AA    : other denomination (NGC or IC, or other)
C B, 5, BPHOT : photometry in circular apertures with 6 parameters
C               log(D25),log(aperture in 0.1'),log(D25)-log(apert.), V, B-V, U-B
C               each parameter is separated by a "tab" (Ctrl I)
C O, 6, OBS   : observations of this object.
C H, 7, HBIBLI: bibliography.
C
C PHOTOM(6,IDIM) : array of the photometric measurements
C
C JLP Version of 14-02-90
C--************************************************************
	PARAMETER (IDIM=200,NLINES=30)
	REAL*4 PHOTOM(6,IDIM)
	INTEGER*4 INDEX1,IDESCR
	CHARACTER*80 DESCR
	CHARACTER BUFF*90,CHAR*80,CHAR3*3,ANS*1,WORD*20,NAME*30
	CHARACTER TEST(IDIM)*3,OUTPUT(IDIM)*120,AAGAL*80
	CHARACTER*80 IDENT,FIELD,WEST,NORTH,RA,DEC
 
	COMMON /RD_CATAL1/ INDEX1(IDIM),IDENT(IDIM),FIELD(IDIM),
     1	WEST(IDIM),NORTH(IDIM),RA(IDIM),DEC(IDIM),
     1	DESCR(IDIM,NLINES,7),IDESCR(IDIM,7)
 
10	FORMAT(A)
 
	OPEN(2,FILE='read_catalog.log',STATUS='unknown',
     1	ACCESS='SEQUENTIAL')
	PRINT 20
	WRITE(2,20)
20	FORMAT(' Program "READ_CATALOG",  Version of 14-02-90')
 
C Input the catalogue :
	CALL RD_INPUTCAT(NGALAXY)
 
C********************************************************************
C Menu :
C********************************************************************
 
88	PRINT 87
87	FORMAT(/,' Menu :',/,
     1	' 1: Galaxies in increasing right ascension',/,
     1	' 2: Galaxies in increasing field number',/,
     1	' 3: Extraction of a subset of this catalog',
     1	' (with a keyword)',/,
     1	' 4: Creation of a file with photometric measurements',/,
     1	'    in circular apertures',/,
     1	' 5: Creation of a correlation table (or TeX output)',/,
     1	' 6: Histogram',/,
     1	' 7: Diagram with two parameters',/,
     1	' 10: Exit',/,
     1	' Enter the number of the option you want:')
	READ(5,*) IOPT
 
	IF(IOPT.EQ.10)GO TO 8888
 
C------------------------------------------------------------------------
	IF(IOPT.EQ.1)THEN
	   NINDEX1=NGALAXY
	    DO I=1,NINDEX1
	      INDEX1(I)=I
	    END DO
C Writing the selection on unit 2
	   CALL RD_WRITE(NINDEX1)
 
C________________________________________________________
	ELSEIF(IOPT.EQ.2)THEN
	   DO I=1,NGALAXY
	     CHAR=FIELD(I)
	     TEST(I)=CHAR(1:3)
	   END DO
	  NINDEX1=NGALAXY
	  CALL CLASS_CHAR(TEST,INDEX1,NINDEX1)
C Writing the selection on unit 2
	  CALL RD_WRITE(NINDEX1)
 
C*******************************************************************
C Option 3 : Extracting a selection from the catalogue with a key-word
C*******************************************************************
	ELSEIF(IOPT.EQ.3)THEN
300	  PRINT 301
301	  FORMAT(' In which array do you want to search ?',/,
     1	' 1: Environment (E)',/,
     1	' 2: Comments (C)',/,
     1	' 3: Description of the galaxy (S)',/,
     1	' 4: Identification (NGC, IC, ARP or ESO number) (A)',/,
     1	' 5: Observations (O)',/,
     1	' 6: Bibliography (H)',/,
     1	' Enter the NUMBER you want :',$)
	  READ(5,*,ERR=300) ICHOICE
	  PRINT 302
302	  FORMAT(' Enter the keyword:')
	  READ(5,10) WORD
	  LWORD=INDEX(WORD,'  ')-1
	  PRINT *,' LENGTH :',LWORD
 
C Looking for the key-word:
	    PRINT *,' Option chosen: ',ICHOICE
	    PRINT *,' DESCR(1,1,3)',DESCR(1,1,3)
	    CALL SEARCH(WORD,LWORD,%DESCR(DESCR(1,1,ICHOICE)),
     1	IDESCR(1,ICHOICE),INDEX1,NINDEX1,NGALAXY)
 
C Output of the result :
 
	PRINT 967,WORD,LWORD,NINDEX1
	WRITE(2,967)WORD,LWORD,NINDEX1
967	FORMAT(/,' Output of a subset of the catalogue',/
     1	' KEY-WORD: ',A20,' (WITH THE FIRST',I4,' LETTERS)',/,
     1	5X,' NUMBER OF GALAXIES FOUND :',I5,/,/)
 
	  IF(NINDEX1.EQ.0)THEN
	    PRINT *,' The word is not in this field'
	  ELSE
C Writing the selection on unit 2
	    CALL RD_WRITE(NINDEX1)
	  ENDIF
 
C------------------------------------------------------------------
C Creation of a catalogue with photometric measurements :
C------------------------------------------------------------------
	ELSEIF(IOPT.EQ.4)THEN
	  PRINT *,' WHICH GALAXIES DO YOU WANT TO OUTPUT :'
	  PRINT *,' Identification (NGCxxx, ESOxxxx,...) OR "ALL" ?'
	  READ(5,10) WORD
 
	  IF(WORD(1:3).EQ.'ALL')THEN
	    ISTART=1
	    IEND=NGALAXY
	  ELSE
	    LWORD=8
	    CALL SEARCH1(WORD,LWORD,IDENT,INDEX1,NINDEX1,NGALAXY)
	      IF(NINDEX1.EQ.0)THEN
	        PRINT *,' There is not any galaxy with this name'
	        GO TO 88
	      ENDIF
	        IF(NINDEX1.EQ.1)PRINT *,'O.K.: one galaxy has been found'
	    PRINT *,DESCR(INDEX1(1),1,4)
	    ISTART=INDEX1(1)
	    IEND=ISTART
 
	  ENDIF
 
408	  PRINT *,' OUTPUT FILE ?'
	  READ(5,10) NAME
	  OPEN(4,FILE=NAME,STATUS='NEW',
     1	ACCESS='SEQUENTIAL',ERR=408)
 
	    DO 401 IGAL=ISTART,IEND
	       IF(IDESCR(IGAL,5).EQ.0)GO TO 401
	      WRITE(4,403)IDENT(IGAL)(1:8),DESCR(IGAL,1,4)
403	      FORMAT(/,3X,' IDENTIFICATION : ',A,
     1	6X,A)
	      WRITE(4,404)
404	      FORMAT(11X,'LOG(D25)',3X,'LOG(APER)',2X,
     1	'LOG(D/A)',7X,'V',9X,'B-V',8X,'U-B',/)
 
	      DO IK=1,IDESCR(IGAL,5)
	        BUFF=DESCR(IGAL,IK,5)
	        CALL READVALUES(BUFF,PHOTOM,IK)
	      END DO
 
	      DO IK=1,IDESCR(IGAL,5)
	        WRITE(4,405)(PHOTOM(KK,IK),KK=1,6)
	      END DO
 
401	    CONTINUE
 
405	  FORMAT(9X,6(F9.3,2X))
	  CLOSE(4)
	  GO TO 88
 
C*******************************************************************
C Option 5 : Creation of a correlation table
C*******************************************************************
	ELSEIF(IOPT.EQ.5)THEN
	  CALL RD_TABLE(NGALAXY)
 
C Return to the main menu :
	  GO TO 88
C*******************************************************************
C Option 6 : Histogram
C*******************************************************************
	ELSEIF(IOPT.EQ.6)THEN
	  CALL RD_HISTO(NGALAXY)
 
C Return to the main menu :
	  GO TO 88
C*******************************************************************
C Option 7 : Diagram with two parameters
C*******************************************************************
	ELSEIF(IOPT.EQ.7)THEN
	  CALL RD_DIAGRAM(NGALAXY)
 
C Return to the main menu :
	  GO TO 88
 
	ELSE
	  GO TO 88
	ENDIF
 
8888	PRINT *,' Output in "read_catalog.log"'
 
	CLOSE(2)
	STOP
	END
 
C******************************************************************
C	SUBROUTINE READVALUES(BUFF,PHOTOM,ILINE)
C To read the values of aperture photometry measurements
C and store the result in PHOTOM(6,IDIM)
C*******************************************************************
	SUBROUTINE READVALUES(BUFF,PHOTOM,ILINE)
	PARAMETER (IDIM=200)
	REAL*4 PHOTOM(6,IDIM),VALUE
	INTEGER*4 LIMIT(80)
	CHARACTER BUFF*80,WORD*80,TAB*1
	DATA TAB/'09'X/
10	FORMAT(A)
 
C LIMIT(IK) is the location of the IK th TAB.
	IK=1
	LIMIT(IK)=0
 
	DO I=1,80
	  IF(BUFF(I:I).EQ.TAB)THEN
	    IK=IK+1
	    LIMIT(IK)=I
	  ENDIF
	END DO
 
	NWORDS=MIN0(IK,6)
C Now the last limit:
	LIMIT(NWORDS+1)=80
 
C Sometimes  I put a * after the last value
C and the work is already done in that case
C Otherwise:
C	IF(IK.LT.NWORDS)THEN
C	DO I=LIMIT(NWORDS+1),80
C	  IF(BUFF(I:I).EQ.' ')THEN
C	    LIMIT(NWORDS+1)=I
C	    GO TO 200
C	  ENDIF
C	END DO
C	PRINT *,' I CANNOT FIND THE END OF THE LINE ...'
C200	CONTINUE
C	ENDIF
 
C Loop on all the words
	DO I=1,NWORDS
 
C Then we decode the words which lay between 2 limits
	  J1=LIMIT(I)+1
	  J2=LIMIT(I+1)-1
C	  ILENTH=J2-J1+1
C	  WORD(1:ILENTH)=BUFF(J1:J2)
C	  CALL ASCII_REAL(WORD,ILENTH,VALUE)
C	  PHOTOM(I,ILINE)=VALUE
	  READ(BUFF(J1:J2),*)PHOTOM(I,ILINE)
	END DO
 
	RETURN
	END
C-----------------------------------------------------------------------
C Subroutine RD_INPUTCAT to input the catalogue
C
C-----------------------------------------------------------------------
	SUBROUTINE RD_INPUTCAT(NGALAXY)
	PARAMETER (IDIM=200,NLINES=30)
	INTEGER*4 INDEX1,IDESCR
	CHARACTER*80 DESCR
	CHARACTER BUFF*90,CHAR*80,CHAR3*3,ANS*1,WORD*20,NAME*30
	CHARACTER*80 IDENT,FIELD,WEST,NORTH,RA,DEC
	CHARACTER CODE(7)*1
	DATA CODE/'E','C','S','A','B','O','H'/
 
	COMMON /RD_CATAL1/ INDEX1(IDIM),IDENT(IDIM),FIELD(IDIM),
     1	WEST(IDIM),NORTH(IDIM),RA(IDIM),DEC(IDIM),
     1	DESCR(IDIM,NLINES,7),IDESCR(IDIM,7)
 
10	FORMAT(A)
 
	PRINT *,' NAME OF THE CATALOGUE ?'
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='OLD',ACCESS='SEQUENTIAL')
	JMAX=IDIM*30
 
C********************************************************************
C Reading the catalogue :
C********************************************************************
	I=0
	DO J=1,JMAX
 
	  READ(1,10,END=9999)BUFF
C Ident
	  IF(BUFF(1:1).EQ.'I')THEN
	    I=I+1
	    IDENT(I)=BUFF(3:82)
C Field number
	  ELSEIF(BUFF(1:1).EQ.'F')THEN
	    FIELD(I)=BUFF(3:82)
C Declination
	  ELSEIF(BUFF(1:1).EQ.'D')THEN
	    DEC(I)=BUFF(3:82)
C Right Ascension
	  ELSEIF(BUFF(1:1).EQ.'R')THEN
	    RA(I)=BUFF(3:82)
C North position on the plate
	  ELSEIF(BUFF(1:1).EQ.'N')THEN
	    NORTH(I)=BUFF(3:82)
C West position
	  ELSEIF(BUFF(1:1).EQ.'W')THEN
	    WEST(I)=BUFF(3:82)
	  ENDIF
 
C Description:
	  DO K=1,7
	    IF(BUFF(1:1).EQ.CODE(K))THEN
	      IDESCR(I,K)=IDESCR(I,K)+1
	      IK=IDESCR(I,K)
	      DESCR(I,IK,K)=BUFF(3:82)
	    ENDIF
	  END DO
 
	END DO
 
9999	PRINT *,'END OF FILE'
	CLOSE(1)
	NGALAXY=I
 
	PRINT 80,NGALAXY
	WRITE(2,80)NGALAXY
80	FORMAT(/,2X,' TOTAL NUMBER OF GALAXIES IN THE CATALOGUE : ',
     1	I5,/)
 
	RETURN
	END
C--------------------------------------------------------------------------
C
C Printing all the objects which have their indices in the array "INDEX1"
C
C--------------------------------------------------------------------------
	SUBROUTINE RD_WRITE(NINDEX1)
	PARAMETER (IDIM=200,NLINES=30)
	INTEGER*4 INDEX1,IDESCR
	CHARACTER*80 DESCR
	CHARACTER*1 OUT(7)
	CHARACTER BUFF*90,CHAR*80,CHAR3*3,ANS*1,WORD*20,NAME*30
	CHARACTER*80 IDENT,FIELD,WEST,NORTH,RA,DEC
 
	COMMON /RD_CATAL1/ INDEX1(IDIM),IDENT(IDIM),FIELD(IDIM),
     1	WEST(IDIM),NORTH(IDIM),RA(IDIM),DEC(IDIM),
     1	DESCR(IDIM,NLINES,7),IDESCR(IDIM,7)
 
10	FORMAT(A)
 
	PRINT 1001
1001	FORMAT(' WHICH ARRAYS DO YOU WANT TO OUTPUT',/,
     1	' - ENVIRONMENT ? (N)',$)
	READ(5,10) OUT(1)
	PRINT 1002
1002	FORMAT(' - COMMENTS ?(N)',$)
	READ(5,10) OUT(2)
	PRINT 1003
1003	FORMAT(' - DESCRIPTION OF THE GALAXY ?(N)',$)
	READ(5,10) OUT(3)
	PRINT 1004
1004	FORMAT(' - NGC OR IC NUMBER ?(N)',$)
	READ(5,10) OUT(4)
	PRINT 1005
1005	FORMAT(' - PHOTOMETRIC MEASUREMENTS ?(N)',$)
	READ(5,10) OUT(5)
	PRINT 1006
1006	FORMAT(' - OBSERVATIONS ?(N)',$)
	READ(5,10) OUT(6)
	PRINT 1007
1007	FORMAT(' - BIBLIOGRAPHY ?(N)',$)
	READ(5,10) OUT(7)
 
C**************************************************************
C Now writing in the file...
 
	WRITE(2,1020)
1020	FORMAT(2X,' IDENT  ',3X,'FIELD',2X,' W ',3X,' N ',3X,
     1	'RIGHT ASCENSION',3X,'  DECLINATION  ')	
 
	DO J=1,NINDEX1
	  I=INDEX1(J)
	  WRITE(2,1202) IDENT(I)(1:8),FIELD(I)(1:3),
     1	WEST(I)(1:3),NORTH(I)(1:3),RA(I)(1:16),DEC(I)(1:16)
1202	FORMAT(/,2X,A8,3X,A3,4X,A3,3X,A3,3X,A15,3X,A15)
 
	  DO K=1,7
	    IF((OUT(K).EQ.'Y'.OR.OUT(K).EQ.'y')
     1	.AND.IDESCR(I,K).NE.0.AND.K.NE.5)THEN
	       DO IK=1,IDESCR(I,K)
	         WRITE(2,1203) DESCR(I,IK,K)(1:68)
1203	         FORMAT(12X,A68)
	       END DO
	    ENDIF
	  END DO
 
	  IF((OUT(5).EQ.'Y'.OR.OUT(5).EQ.'y')
     1	.AND.IDESCR(I,5).NE.0)THEN
	    WRITE(2,1204)
1204	    FORMAT(/,13X,'LOG(D25)',X,'LOG(APER)',X,
     1	'LOG(D/A)',3X,'V',6X,'B-V',5X,'U-B',/)
 
	    DO IK=1,IDESCR(I,5)
	      WRITE(2,1205) DESCR(I,IK,5)
1205	      FORMAT(14X,A80)
	    END DO
 
            WRITE(2,1201)
1201 	    FORMAT(' ')
	  ENDIF
 
	END DO
 
 
	RETURN
	END
C*******************************************************************
C Creation of a correlation table
C*******************************************************************
	SUBROUTINE RD_TABLE(NGALAXY)
	PARAMETER (IDIM=200,NLINES=30)
	REAL*4 PHOTOM(6,IDIM),VALUES_OUT(IDIM)
	INTEGER*4 INDEX1,IDESCR
	LOGICAL OUT_COORD
	CHARACTER*80 DESCR
	CHARACTER BUFF*90,CHAR*80,CHAR3*3,ANS*1,WORD*20,NAME*30
	CHARACTER OUTPUT(IDIM)*120,AAGAL*80
	CHARACTER*80 IDENT,FIELD,WEST,NORTH,RA,DEC
	CHARACTER COORD(2)*26
 
	COMMON /RD_CATAL1/ INDEX1(IDIM),IDENT(IDIM),FIELD(IDIM),
     1	WEST(IDIM),NORTH(IDIM),RA(IDIM),DEC(IDIM),
     1	DESCR(IDIM,NLINES,7),IDESCR(IDIM,7)
 
10	FORMAT(A)
 
	  IBEGIN=1
 
C Clearing out OUTPUT :
	   DO IGAL=1,NGALAXY
	    OUTPUT(IGAL)='&&   '
	   END DO
 
C Output of a TEX file :
508	  PRINT *,' Output TeX file ?'
	  READ(5,10) NAME
	  OPEN(9,FILE=NAME,STATUS='NEW',
     1	ACCESS='SEQUENTIAL',ERR=508)
 
C Header of the TeX file:
	  WRITE(9,562) NGALAXY
562	     FORMAT('\magnification=\magstep1',/,
     1	'\hsize=16.5truecm',/,
     1	'\vsize=24.truecm',/,
     1	'\voffset=-0.5truecm',/,
     1	'\hoffset=-0.5truecm',/,/,
     1	' Number of galaxies: ',I5,/)
 
	  PRINT 555
555	  FORMAT(' Do you want also to output the coordinates? (N)')
	  READ(5,10),ANS
	  OUT_COORD=((ANS.EQ.'Y').OR.(ANS.EQ.'y'))
 
	  PRINT 511
511	  FORMAT(' Number of keywords ? ',/,
     1	'(Enter 0 if you want to output only names and coordinates)')
	  READ(5,*) NKWORDS
	  NCOL=NKWORDS+2
	  IF(OUT_COORD)NCOL=NCOL+2
 
C**********************************************************************
C Loop on all the keywords to generate OUTPUT
	DO IKWORD=1,NKWORDS
 
500	  PRINT 501
501	  FORMAT(' In which array do you want to search ?',/,
     1	' 1: Environment (E)',/,
     1	' 2: Comments (C)',/,
     1	' 3: Description of the galaxy (S)',/,
     1	' 4: Identification (NGC, IC, ARP or ESO number) (A)',/,
     1	' 5: Observations (O)',/,
     1	' 6: Bibliography (H)',/,
     1	' Enter the NUMBER you want :',$)
	  READ(5,*,ERR=500) ICHOICE
	  PRINT 502
502	  FORMAT(' Which key-word are you looking for ?')
	  READ(5,10) WORD
 
	  LWORD=INDEX(WORD,'  ')-1
	  PRINT *,' LENGTH :',LWORD
 
	  PRINT 517
517	  FORMAT('     Two possibilities :',/,
     1	' 1. Correlation table filled',
     1	' with crosses when the item is found',/,
     1	' 2. Table with the values of the given parameter',/,
     1	'     Enter your choice :')
	  READ(5,*) ICHO1
 
C Writing crosses when the item is found in OUTPUT :
	  IF(ICHO1.EQ.1)THEN
	     CALL RD_CORREL1(WORD,LWORD,%DESCR(DESCR(1,1,ICHOICE)),
     1	IDESCR(1,ICHOICE),OUTPUT,NUMBER,IBEGIN,NGALAXY)
	     IBEGIN=IBEGIN+5
	  ELSE
C Writing the values in OUTPUT :
	     CALL RD_CORREL2(WORD,LWORD,%DESCR(DESCR(1,1,ICHOICE)),
     1	IDESCR(1,ICHOICE),OUTPUT,
     1	VALUES_OUT,NUMBER,IBEGIN,NGALAXY)
	     IBEGIN=IBEGIN+10
	  ENDIF
 
C Writing the keyword in the output TEX file :
	  PRINT 567,WORD,NUMBER
	  WRITE(2,567)WORD,NUMBER
567	  FORMAT(/,' Keyword: ',A20,
     1	5X,' NUMBER OF OBJECTS SELECTED :',I5,/,/)
C Writing in the begining of the output file:
	  WRITE(9,565)WORD,NUMBER
565	  FORMAT(' Keyword: ',A20,
     1	2X,' Objects selected:',I5,/)
 
	ENDDO
C**********************************************************************
C Writing on the TeX file now:
 
C Beginning of the table:
	     WRITE(9,566)
566	     FORMAT('\vbox{\tabskip=0pt\offinterlineskip',/,
     1	'\hrule',/,'\halign to \hsize{\tabskip=\centering',/,
     1	<NCOL>('\vrule#&\strut#\hfil&',/),
     1	'\vrule#\tabskip=0pt\cr',/,
     1	'height2pt%',/,<NCOL>('&\omit&',/),'\cr',/,
     1	'& Ident. \hfil&',/,<NCOL-1>('&---&',/),'\cr',/,
     1	'height2pt%',/,<NCOL>('&\omit&',/),'\cr',/,
     1	'\noalign{\hrule}',/,
     1	'height2pt%',/,<NCOL>('&\omit&',/),'\cr',/)
 
	     IEND=MIN(IBEGIN,115)
 
	     DO IGAL=1,NGALAXY
 
C Looking for the identification in NGC numbers :
	       AAGAL=' '
	       IMAX=INDEX(DESCR(IGAL,1,4),';')-1
	        IF(IMAX.GE.1)THEN
		  AAGAL=DESCR(IGAL,1,4)(1:IMAX)
	        ELSE
	          AAGAL='$\ldots$'
	        ENDIF
 
C Writing the final "Carriage return" for each record:
	       WRITE(OUTPUT(IGAL)(IEND:120),568)
568	       FORMAT(' &\cr')
 
C Cutting the lines in less than 80 characters
	        IF(OUTPUT(1)(56:57).EQ.'&&')THEN
	           IEND1=55
	           IEND2=IEND+5-IEND1
	        ELSE
	           IEND1=50
	           IEND2=IEND+5-IEND1
	        ENDIF
C Writing on one line if less than 80 characters, or two lines otherwise.
	        IF(IEND2.LE.0)THEN
	          IF(OUT_COORD)THEN
		    CALL RDC_DECODE_COORD(RA(IGAL),DEC(IGAL),COORD)
	            WRITE(9,549) IDENT(IGAL),AAGAL,COORD(1),COORD(2),
     1	    OUTPUT(IGAL)
549	            FORMAT('&',A8,'&&',A11,'&&',A26,'&&',A26,/,A<IEND+5>)
	          ELSE
	            WRITE(9,569) IDENT(IGAL),AAGAL,OUTPUT(IGAL)
569	            FORMAT('&',A8,'&&',A11,A<IEND+5>)
	          ENDIF
	        ELSE
	          IF(OUT_COORD)THEN
		    CALL RDC_DECODE_COORD(RA(IGAL),DEC(IGAL),COORD)
	            WRITE(9,540) IDENT(IGAL),AAGAL,COORD(1),COORD(2),
     1	    OUTPUT(IGAL),OUTPUT(IGAL)(IEND1+1:IEND+5)
540	            FORMAT('&',A8,'&&',A11,'&&',A26,'&&',A26,/,
     1	    A<IEND1>,/,A)
	          ELSE
	            WRITE(9,570) IDENT(IGAL),AAGAL,
     1	    OUTPUT(IGAL),OUTPUT(IGAL)(IEND1+1:IEND+5)
570	            FORMAT('&',A8,'&&',A11,A<IEND1>,/,A)
	          ENDIF
	        ENDIF
 
	     IF(IGAL.EQ.NGALAXY)THEN
	       WRITE(9,572)
572	       FORMAT('height2pt%',/,<NCOL>('&\omit&',/),'\cr}',
     1	/,'\hrule}',/,'\vfill\eject',/,'\bye')
	     ELSEIF(MOD(IGAL,40).EQ.0)THEN
	        WRITE(9,571)
571	        FORMAT('height2pt%',/,<NCOL>('&\omit&',/),'\cr}',
     1	/,'\hrule}',/,'\vfill\eject')
	        WRITE(9,566)
	     ENDIF
 
	     END DO
 
	   CLOSE(9)
 
	RETURN
	END
C*******************************************************************
C Histogram
C*******************************************************************
	SUBROUTINE RD_HISTO(NGALAXY)
	PARAMETER (IDIM=200,NLINES=30)
	REAL*4 PHOTOM(6,IDIM)
	REAL*4 VALUES_IN(IDIM),XOUT(IDIM),YOUT(IDIM)
	INTEGER*4 INDEX1,IDESCR
	CHARACTER*80 DESCR
	CHARACTER BUFF*90,CHAR*80,CHAR3*3,ANS*1,WORD*20,NAME*30
	CHARACTER OUTPUT(IDIM)*120,AAGAL*80
	CHARACTER*80 IDENT,FIELD,WEST,NORTH,RA,DEC
	CHARACTER NCHAR*4,CHAR1*30,CHAR2*30,TITLE*40,PLOTDEV*32
        CHARACTER PCOLOR*30
 
	COMMON /RD_CATAL1/ INDEX1(IDIM),IDENT(IDIM),FIELD(IDIM),
     1	WEST(IDIM),NORTH(IDIM),RA(IDIM),DEC(IDIM),
     1	DESCR(IDIM,NLINES,7),IDESCR(IDIM,7)
 
10	FORMAT(A)
 
500	  PRINT 501
501	  FORMAT(' In which array do you want to search ?',/,
     1	' 1: Environment (E)',/,
     1	' 2: Comments (C)',/,
     1	' 3: Description of the galaxy (S)',/,
     1	' 4: Identification (NGC, IC, ARP or ESO number) (A)',/,
     1	' 5: Observations (O)',/,
     1	' 6: Bibliography (H)',/,
     1	' Enter the NUMBER you want :',$)
	  READ(5,*) ICHOICE
	  PRINT 502
502	  FORMAT(' WHICH KEY-WORD ARE YOU LOOKING FOR ?')
	  READ(5,10) WORD
 
	  LWORD=INDEX(WORD,'  ')-1
	  PRINT *,' LENGTH :',LWORD
 
C Writing the values in OUTPUT :
	IBEGIN=1
	CALL RD_CORREL2(WORD,LWORD,%DESCR(DESCR(1,1,ICHOICE)),
     1	IDESCR(1,ICHOICE),OUTPUT,
     1	VALUES_IN,NPTS_IN,IBEGIN,NGALAXY)
 
C Computing Min and Max :
	PRINT *,' ',NPTS_IN,' VALUES FOUND'
	XMIN=VALUES_IN(1)
	XMAX=VALUES_IN(1)
	DO I=2,NPTS_IN
	  XMIN=AMIN1(XMIN,VALUES_IN(I))
	  XMAX=AMAX1(XMAX,VALUES_IN(I))
	END DO
	PRINT *,' MIN AND MAX :',XMIN,XMAX
 
C Generating the histogram:
	PRINT *,' MIN, MAX, BIN WIDTH ?'
	READ(5,*) VMIN,VMAX,BIN_WIDTH
	CALL RD_HISTO2(VALUES_IN,NPTS_IN,XOUT,YOUT,NPTS_OUT,
     1	VMIN,VMAX,BIN_WIDTH)
 
C Output of the curve:
	NCHAR='L'
        PCOLOR='Default'
	KCUR=1
	CHAR2=' N'
	PRINT *,' X LABEL ?'
	READ(5,10) CHAR1
	PRINT *,' TITLE ?'
	READ(5,10) TITLE
	PRINT *,' PLOT DEVICE ?'
	READ(5,10) PLOTDEV
	CALL NEWPLOT(XOUT,YOUT,NPTS_OUT,IDIM,KCUR,CHAR1,CHAR2,
     1	TITLE,NCHAR,PCOLOR,PLOTDEV,' ',' ')
 
	RETURN
	END
C----------------------------------------------------------------------
C Subroutine RD_HISTO2
C Creates an histogram VALUES_OUT from a series of input values VALUES_IN
C----------------------------------------------------------------------
	SUBROUTINE RD_HISTO2(VALUES_IN,NPTS_IN,XOUT,YOUT,
     1	NPTS_OUT,VMIN,VMAX,BIN_WIDTH)
	REAL*4 VALUES_IN(*),XOUT(*),YOUT(*)
 
C Computing NPTS_OUT:
	NPTS_OUT=2*(NINT((VMAX-VMIN)/BIN_WIDTH)+1)
	  IF(NPTS_OUT.EQ.0.OR.NPTS_OUT.GT.200)THEN
	    PRINT *,' ERROR : BIN_WIDTH IS WRONG'
	    NPTS_OUT=1
	    RETURN
	  ENDIF
 
	DO J=1,NPTS_OUT/2
C	  XOUT(J)=VMIN+(FLOAT(J)+0.5)*BIN_WIDTH
	  XOUT(2*J-1)=VMIN+FLOAT(J-1)*BIN_WIDTH
	  XOUT(2*J)=VMIN+FLOAT(J)*BIN_WIDTH
	  YOUT(2*J-1)=0.
	  YOUT(2*J)=0.
	END DO
 
	DO I=1,NPTS_IN
	  WORK=AMIN1(VALUES_IN(I),VMAX)
	  WORK=AMAX1(VALUES_IN(I),VMIN)
	  J=NINT((WORK-VMIN)/BIN_WIDTH)+1
	  YOUT(2*J)=YOUT(2*J)+1.
	  YOUT(2*J-1)=YOUT(2*J-1)+1.
	END DO
 
	RETURN
	END
C*******************************************************************
C Diagram
C Draws a diagram with two parameters
C*******************************************************************
	SUBROUTINE RD_DIAGRAM(NGALAXY)
	PARAMETER (IDIM=200,NLINES=30)
	REAL*4 PHOTOM(6,IDIM),VOUT(IDIM)
	REAL*4 XPLOT(IDIM),YPLOT(IDIM)
	INTEGER*4 INDEX1,IDESCR
	CHARACTER*80 DESCR
	CHARACTER BUFF*90,CHAR*80,CHAR3*3,ANS*1,WORD*20,NAME*30
	CHARACTER OUTPUT(IDIM)*120,AAGAL*80
	CHARACTER*80 IDENT,FIELD,WEST,NORTH,RA,DEC
	CHARACTER NCHAR*4,CHAR1*30,CHAR2*30,TITLE*40,PLOTDEV*32
        CHARACTER PCOLOR*30
 
	COMMON /RD_CATAL1/ INDEX1(IDIM),IDENT(IDIM),FIELD(IDIM),
     1	WEST(IDIM),NORTH(IDIM),RA(IDIM),DEC(IDIM),
     1	DESCR(IDIM,NLINES,7),IDESCR(IDIM,7)
 
10	FORMAT(A)
 
509	  PRINT *,' OUTPUT MONGO FILE ?'
	  READ(5,10) NAME
	  OPEN(10,FILE=NAME,STATUS='NEW',
     1	ACCESS='SEQUENTIAL',ERR=509)
 
	  PRINT 489
489	  FORMAT(/,'    ENTER THE PARAMETER YOU WANT IN X :')
	DO I=1,2
	  IBEGIN=1+(I-1)*19
500	  PRINT 501
501	  FORMAT(/,' In which array do you want to search ?',/,
     1	' 1: ENVIRONMENT (E)',/,
     1	' 2: COMMENTS (C)',/,
     1	' 3: DESCRIPTION OF THE GALAXY (S)',/,
     1	' 4: NGC OR IC NUMBER (A)',/,
     1	' 6: OBSERVATIONS (O)',/,
     1	' 7: BIBLIOGRAPHY (H)',/,
     1	' 8: Right ascension ',/,
     1	' 9: Declination ',/,
     1	' Enter the NUMBER you want :')
	  READ(5,*,ERR=500) ICHOICE
	
C Coordinates:
	IF(ICHOICE.EQ.8)THEN
	     NOUT=NGALAXY
	     DO J=1,NOUT
		CALL DECODE_RA(RA(J),XRA)
	        WRITE(OUTPUT(J)(IBEGIN:),29) XRA
29	FORMAT('&& ',G15.7,2X)
	     END DO
	ELSEIF(ICHOICE.EQ.9)THEN
	     NOUT=NGALAXY
	     DO J=1,NOUT
		CALL DECODE_DEC(DEC(J),XDEC)
	        WRITE(OUTPUT(J)(IBEGIN:),29) XDEC
	     END DO
	ELSE
	  PRINT 502
502	  FORMAT(' WHICH KEY-WORD ARE YOU LOOKING FOR ?')
	  READ(5,10) WORD
	  LWORD=INDEX(WORD,'  ')-1
	  PRINT *,' LENGTH :',LWORD
C Writing the values in OUTPUT:
	  CALL RD_CORREL2(WORD,LWORD,%DESCR(DESCR(1,1,ICHOICE)),
     1	IDESCR(1,ICHOICE),OUTPUT,
     1	VOUT,NOUT,IBEGIN,NGALAXY)
 
	ENDIF
 
	PRINT *,' '
	PRINT *,' ',NOUT,' VALUES FOUND'
 
C Looking for the Y values :
	  PRINT *,' '
	  PRINT *,' ENTER THE PARAMETER YOU WANT IN Y :'
	  PRINT *,' '
	END DO
 
C Reading the points:
	NPTS=0
	DO I=1,NGALAXY
	   READ(OUTPUT(I)(3:19),*,ERR=11) X1
	   READ(OUTPUT(I)(22:40),*,ERR=11) Y1
	   NPTS=NPTS+1
	   XPLOT(NPTS)=X1
	   YPLOT(NPTS)=Y1
11	END DO
 
	PRINT *,' NUMBER OF POINTS FOR THE DIAGRAM :',NPTS
 
C Output in Mongo file:
	CALL WR_MONGO(XPLOT,YPLOT,NPTS)
 
C Output of the curve:
	KCUR=1
	PRINT *,' PLOT DEVICE ? (NONE if graph is not wanted)'
	READ(5,10) PLOTDEV
	IF((PLOTDEV(1:2).EQ.'NO').OR.(PLOTDEV(1:2).EQ.'no')) RETURN
	PRINT *,' X LABEL ?'
	READ(5,10) CHAR1
	PRINT *,' Y LABEL ?'
	READ(5,10) CHAR2
	PRINT *,' TITLE ?'
	READ(5,10) TITLE
	PRINT *,' SYMBOL (42?)'
	READ(5,10) NCHAR
        PCOLOR='Default'
	CALL NEWPLOT(XPLOT,YPLOT,NPTS,IDIM,KCUR,CHAR1,CHAR2,
     1	TITLE,NCHAR,PCOLOR,PLOTDEV,' ',' ')
 
	RETURN
	END
C----------------------------------------------------------------------
C Output in Mongo file:
	SUBROUTINE WR_MONGO(XPLOT,YPLOT,NPTS)
	REAL*4 XPLOT(NPTS),YPLOT(NPTS)
	INTEGER IFMT,NPTS
 
281	PRINT 280
280	FORMAT(' Format for the output  (MONGO FILE):',/,
     1	' 1: (2(X,G14.7))',/,' 2: (2(X,I10))',/,
     1	' 3: (X,I10,X,G14.7)',/,' 4: (X,G14.7,X,I10)',/,
     1	' Enter the NUMBER of the format you want :')
	READ(5,*,ERR=281) IFMT
 
	DO I=1,NPTS
	   X1=XPLOT(I)
	   Y1=YPLOT(I)
	IF(IFMT.EQ.1)THEN
	   PRINT 291,X1,Y1
	   WRITE(10,291) X1,Y1
291	FORMAT(2(X,G14.7))
	ELSEIF(IFMT.EQ.2)THEN
	   PRINT 292,INT(X1),INT(Y1)
	   WRITE(10,292) INT(X1),INT(Y1)
292	FORMAT(2(X,I10))
	ELSEIF(IFMT.EQ.3)THEN
	   PRINT 293,INT(X1),Y1
	   WRITE(10,293) INT(X1),Y1
293	FORMAT(X,I10,X,G14.7)
	ELSEIF(IFMT.EQ.4)THEN
	   PRINT 294,X1,INT(Y1)
	   WRITE(10,294) X1,INT(Y1)
294	FORMAT(X,G14.7,X,I10)
	ENDIF
	END DO
C Closing Mongo file:
	   CLOSE(10)
	RETURN
	END
C--------------------------------------------------------------
	SUBROUTINE DECODE_DEC(DEC,XDEC)
	CHARACTER DEC*80
	REAL*4 XDEC
	INTEGER IDEG,IMIN,ISEC
	I=INDEX(DEC,'DEG')
C	J=INDEX(DEC,''')
	J=I+6
	K=INDEX(DEC,'"')
	READ(DEC(1:I-1),*) IDEG
	READ(DEC(I+3:J-1),*) IMIN
	READ(DEC(J+1:K-1),*) ISEC
	IF(INDEX(DEC(1:4),'-').NE.0)THEN
	  XDEC=IDEG-FLOAT(IMIN)/60.-FLOAT(ISEC)/3600.
	ELSE
	  XDEC=IDEG+FLOAT(IMIN)/60.+FLOAT(ISEC)/3600.
	ENDIF
	RETURN
	END
C------------------------------------------------------------------
	SUBROUTINE DECODE_RA(RA,XRA)
	CHARACTER RA*80
	REAL*4 XRA
	INTEGER IHOU,IMIN,ISEC
	I=INDEX(RA,'H')
	J=INDEX(RA,'MN')
	K=INDEX(RA,'SEC')
	READ(RA(1:I-1),*) IHOU
	READ(RA(I+1:J-1),*) IMIN
	READ(RA(J+2:K-1),*) ISEC
	XRA=IHOU+FLOAT(IMIN)/60.+FLOAT(ISEC)/3600.
C Conversion in degrees to compare with ESO coordinates:
	XRA=XRA*15.
	RETURN
	END
C--------------------------------------------------------------------
C For TeX output:
	SUBROUTINE RDC_DECODE_COORD(RA1,DEC1,COORD)
	CHARACTER RA1*80,DEC1*80,COORD(2)*26
C RA:
	I=INDEX(RA1,'H')
	J=INDEX(RA1,'MN')
	K=INDEX(RA1,'SEC')
	READ(RA1(1:I-1),*) IHOU
	READ(RA1(I+1:J-1),*) IMIN
	READ(RA1(J+2:K-1),*) SEC
	WRITE(COORD(1),21)IHOU,IMIN,SEC
21	FORMAT(I2,'$^H$ ',I2,'$^{m}$ ',F4.1,'$^{s}$')
 
C DEC:
	I=INDEX(DEC1,'DEG')
C	J=INDEX(DEC1,''')
	J=I+6
	K=INDEX(DEC1,'"')
	READ(DEC1(1:I-1),*) IDEG
	READ(DEC1(I+3:J-1),*) IMIN
	READ(DEC1(J+1:K-1),*) SEC
	
	IDEG=JIABS(IDEG)
	IF(INDEX(DEC1(1:4),'-').NE.0)THEN
	  WRITE(COORD(2),22)IDEG,IMIN,SEC
22	  FORMAT('$-$',I3,'$^o$ ',I2,''' ',F4.1,'" ')
	ELSE
	  WRITE(COORD(2),23)IDEG,IMIN,SEC
23	  FORMAT('$+$',I3,'$^o$ ',I2,''' ',F4.1,'" ')
	ENDIF
 
	RETURN
	END
C--------------------------------------------------------------------
	include 'jlpsub:search_set.for'
	include 'jlpsub:sort_set.for'
