C++***************************************************************
C Program to change some instructions in the programs
C
C JLP Version of 10/12/90
C--***************************************************************
	PROGRAM PRECOMPILER
	INTEGER*4 LENGTH_OLD,LENGTH_NEW
	LOGICAL PURGE
	CHARACTER GENERIC_NAME*30,OUTPUT(500)*60
	CHARACTER OLD_STRING*80,NEW_STRING*80,ANS*1
	COMMON /JLP_DIRECT/OUTPUT
 
10	FORMAT(A)
 
	PRINT 88
88	FORMAT(' Precompiler Version 17-12-90',/,
     1	' Automatic purge in Unix machines...')
 
C Possibility of purging the files:
	WRITE(6,*) ' Do you want automatic purge of the files ? (N)'
	READ(5,10) ANS
	PURGE=(ANS.EQ.'y'.OR.ANS.EQ.'Y')
 
	WRITE(6,11)
11	FORMAT(' Character string you are looking for ? (end with @@)')
	OLD_STRING=' '
	READ(5,10) OLD_STRING
	LENGTH_OLD=MAX(INDEX(OLD_STRING,'@@')-1,1)
 
	WRITE(6,12)
12	FORMAT(' New character string you want to substitute ?',
     1	'(end with @@)')
	NEW_STRING=' '
	READ(5,10) NEW_STRING
	LENGTH_NEW=MAX(INDEX(NEW_STRING,'@@')-1,1)
 
	WRITE(6,8)
8	FORMAT(' INPUT PROGRAMS : (*.* accepted) ',$)
	READ(5,10)GENERIC_NAME
 
C QUIET is set to true (no output of the names on the terminal)
	CALL JLP_DIRECTORY(GENERIC_NAME,NUMBER,1)
	WRITE(6,*)' ',NUMBER,' files have been found'
 
	DO I=1,NUMBER
	  WRITE(6,14) OUTPUT(I)
14	  FORMAT(' Modifying the file : ',A)
	  CALL MODIFY_FILE(OUTPUT(I),OLD_STRING,LENGTH_OLD,
     1	NEW_STRING,LENGTH_NEW,PURGE)
	END DO
 
	STOP
	END
C*****************************************************************
C Subroutine REPLACE
C To replace one string by another
C*****************************************************************
	SUBROUTINE REPLACE(BUFFER_IN,OLD_STRING,LENGTH_OLD,
     1	NEW_STRING,LENGTH_NEW,BUFFER_OUT,LENGTH_OUT)
	INTEGER*4 LENGTH_OLD,LENGTH_NEW,LOCATION
	CHARACTER*(*) BUFFER_IN,BUFFER_OUT,OLD_STRING,NEW_STRING
	CHARACTER*70 BLANK
	BLANK=' '
 
10	FORMAT(A)
	BUFFER_OUT=' '
C Look for the old string:
	LOCATION=INDEX(BUFFER_IN,OLD_STRING(:LENGTH_OLD))-1
 
	IF(LOCATION.GT.0)THEN
 
C General case when the old string has been found:
	  BUFFER_OUT=BUFFER_IN(:LOCATION)//NEW_STRING(:LENGTH_NEW)
     1	//BUFFER_IN(LOCATION+LENGTH_OLD+1:)
	  WRITE(6,21) BUFFER_IN(:72),BUFFER_OUT(:72)
21	  FORMAT(' Old:',A,/,' New:',A)
 
C Case when the old string is just in the beginning of the line:
	ELSEIF(LOCATION.EQ.0)THEN
	  BUFFER_OUT=NEW_STRING(:LENGTH_NEW)
     1	//BUFFER_IN(LOCATION+LENGTH_OLD+1:)
	  WRITE(6,21) BUFFER_IN(:72),BUFFER_OUT(:72)
 
C When not found:
	ELSE
	  BUFFER_OUT=BUFFER_IN
 
	ENDIF
 
	LENGTH_OUT=MAX(INDEX(BUFFER_OUT,BLANK)-1,1)
 
	RETURN
	END
C*************************************************************************
C Subroutine MODIFY_NAME
C
C*************************************************************************
	SUBROUTINE MODIFY_FILE(NAME,OLD_STRING,LENGTH_OLD,
     1	NEW_STRING,LENGTH_NEW,PURGE)
	INTEGER*4 LENGTH_OLD,LENGTH_NEW
	LOGICAL PURGE
	CHARACTER BUFFER_IN*150,BUFFER_OUT*150
	CHARACTER OLD_STRING*(*),NEW_STRING*(*),NAME*(*)
 
10	FORMAT(A)
 
C Input
	IF(PURGE)THEN
	  OPEN(1,FILE=NAME,STATUS='OLD')
	ELSE
	  OPEN(1,FILE=NAME,STATUS='OLD')
	ENDIF
 
C Output (check if UNIX or VAX):
	I=INDEX(NAME,';')
	IF(I.GT.1) THEN
	  NAME=NAME(:I)//'                 '
	  OPEN(2,FILE=NAME,STATUS='NEW')
C For UNIX:
	ELSE
	  OPEN(2,FILE=NAME,STATUS='UNKNOWN')
	ENDIF
 
C Reading the input file and writing on the new one:
	  DO I=1,10000
	    BUFFER_IN=' '
	    READ(1,10,END=99)BUFFER_IN
	    CALL REPLACE(BUFFER_IN,OLD_STRING,
     1	LENGTH_OLD,NEW_STRING,LENGTH_NEW,BUFFER_OUT,LENGTH_OUT)
	    WRITE(2,10)BUFFER_OUT(:LENGTH_OUT)
	  END DO
	  WRITE(6,*) ' SORRY I STOP AT THE 10000TH LINE'
 
99	  CLOSE(2)
	IF(PURGE)THEN
	  CLOSE(1,DISPOSE='DELETE')
	ELSE
	  CLOSE(1)
	ENDIF
 
	RETURN
	END
