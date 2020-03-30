C++-----------------------------------------------------------------------
C Program to work with filters
C Catalogue in FILTER.CAT (indexed file)
C JLP Version 18th May 1987
C-----------------------------------------------------------------------
	PROGRAM FILTER_INDEX
	PARAMETER (IDIM=200)
	REAL*8 XX(IDIM),YY(IDIM)
	CHARACTER BUFFER*80,NAMECAT*25
 
	STRUCTURE /FILTER_JLP1_STRUCT/
	  INTEGER*4 ORDER_NUMBER		!POSITION 1:4  KEY 0
	  CHARACTER*20 NAME 			!POSITION 5:24  KEY 1
	  CHARACTER*25 FILE_NAME 		!POSITION 25:49  KEY 2
	  CHARACTER*80 DESCRIPTION		!POSITION 50:132
	END STRUCTURE
 
	RECORD /FILTER_JLP1_STRUCT/ FILTER
 
	ITOTAL=0
10	FORMAT(A)
	
	PRINT *,' ENTER THE NAME OF THE CATALOGUE :'
	READ(5,10) NAMECAT
	OPEN(1,FILE=NAMECAT,ACCESS='KEYED',STATUS='UNKNOWN',
     1	KEY=(1:4:INTEGER,5:24,25:49),
     1	RECORDTYPE='VARIABLE',ORGANIZATION='INDEXED')
 
 
88	PRINT 89
89	FORMAT(' MENU :',/,
     1	' 1. INPUT OF NEW FILES',/,
     1	' 2. CORRECTION OF INFORMATION',/,
     1	' 3. LOOKING FOR SOME INFORMATION',/,
     1	' 4. OUTPUT THE FULL LIST OF THE CATALOGUE',/,
     1	' 10. EXIT')
	READ(5,*) IOP
 
C-------------------------------------------------------------------------
C Option 1
C------------------------------------------------------------------------
	IF (IOP.EQ.1)THEN
	PRINT *,' NUMBER OF FILTERS YOU WANT TO ENTER ?'
	READ(5,*) NUMB
 
	DO I=1,NUMB
	PRINT *,' NAME OF THE FILTER ?'
	READ(5,10) FILTER.NAME
105	PRINT *,' NAME OF THE FILE ?'
	READ(5,10) FILTER.FILE_NAME
 
	OPEN(2,FILE=FILTER.FILE_NAME,ERR=105,
     1	STATUS='OLD')
	READ(2,*)NPOINT
	PRINT *,' NPOINT =',NPOINT
	  DO KK=1,NPOINT
	  READ(2,*) XX(KK),YY(KK)
	  END DO
	READ(2,10)FILTER.DESCRIPTION
	PRINT *,FILTER.DESCRIPTION
 
	PRINT *,' DESCRIPTION ?'
	READ(5,10) FILTER.DESCRIPTION
 
C Guessing the TOTAL number of records :
107	FILTER.ORDER_NUMBER=ITOTAL+1
	ITOTAL=ITOTAL+1
	WRITE(1,ERR=106) FILTER
	END DO
 
	GO TO 88
106	ITOTAL=ITOTAL+1
	GO TO 107
	ENDIF
 
C----------------------------------------------------------------------
C Option 2
C----------------------------------------------------------------------
 
	IF(IOP.EQ.2)THEN
	PRINT *,' WHICH NUMBER DO YOU WANT TO MODIFY ?'
	READ(5,*) NUMB
	READ(1,KEY=NUMB,KEYID=0,ERR=997) FILTER
	PRINT 201,FILTER.ORDER_NUMBER,
     1	FILTER.NAME,FILTER.FILE_NAME,
     1	FILTER.DESCRIPTION
201	FORMAT('  ORDER NUMBER : ',I6,
     1	'   NAME : ',A,'  FILE_NAME : ',A,/,
     1	'  DESCRIPTION : ',A)
	PRINT 202
202	FORMAT(' CODE : 1. NAME  2. FILE NAME  3. DESCRIPTION',/,
     1	' WHICH NUMBER DO YOU WANT ?')
	READ(5,*) ICODE
	PRINT *,' ENTER NEW ITEM :'
	IF(ICODE.EQ.1)READ(5,10) FILTER.NAME
	IF(ICODE.EQ.2)READ(5,10) FILTER.FILE_NAME
	IF(ICODE.EQ.3)READ(5,10) FILTER.DESCRIPTION
	REWRITE(1,ERR=998) FILTER
	GO TO 88
	ENDIF
 
C----------------------------------------------------------------------
C Option 3
C----------------------------------------------------------------------
 
	IF(IOP.EQ.3)THEN
 
	PRINT *,' WHICH NUMBER DO YOU WANT TO CHECK ?'
	READ(5,*) NUMB
	READ(1,KEY=NUMB,KEYID=0,ERR=997) FILTER
	PRINT 201,FILTER.ORDER_NUMBER,
     1	FILTER.NAME,FILTER.FILE_NAME,
     1	FILTER.DESCRIPTION
 
	GO TO 88
	ENDIF
C----------------------------------------------------------------------
C Option 4
C----------------------------------------------------------------------
 
	IF(IOP.EQ.4)THEN
 
	OPEN(3,FILE='filter_index.log',STATUS='unknown')
	ITOTAL=0
 
	DO II=1,1000
	READ(1,KEY=II,KEYID=0,ERR=403) FILTER
	ITOTAL=ITOTAL+1
	PRINT 201,FILTER.ORDER_NUMBER,
     1	FILTER.NAME,FILTER.FILE_NAME,
     1	FILTER.DESCRIPTION
	WRITE(3,201) FILTER.ORDER_NUMBER,
     1	FILTER.NAME,FILTER.FILE_NAME,
     1	FILTER.DESCRIPTION
	END DO
 
403	CLOSE(3)
	PRINT *,' Output in "filter_index.log"'
	GO TO 88
	ENDIF
 
C End of the program
	CLOSE(1)
	STOP
C-------------------------------------------------------------------
C Error messages :
998	PRINT *,' Problems writing in the catalogue'
	STOP
997	PRINT *,' Problems while reading in the catalogue'
	STOP
	END
 
 
