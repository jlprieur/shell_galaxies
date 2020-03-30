C++-----------------------------------------------------------------
C Set of routines SET_SEARCH.FOR
C To look for a word in a character array.
C Used by HELP_JLP.
C
C Contains SEARCH, SEARCH1, CLASS, ASCII_REAL, RD_CORREL1, RD_CORREL2
C
C JLP
C Version of 16-01-90
C-------------------------------------------------------------------
C Subroutine SEARCH to look for a word in a 2-dimensional
C array of characters*80.
C Input
C WORD*(1:LWORD) : Word to look for
C ARRAY(200,30) : Array where to look for the word
C
C Output
C INDEX1(200) : Index of the items where the word has been found.
C*******************************************************************
	SUBROUTINE SEARCH(WORD,LWORD,ARRAY,NARRAY,INDEX1,NINDEX1,
	1	NGALAXY)
	PARAMETER (IDIM=200)
	CHARACTER*80 ARRAY(IDIM,*)
	INTEGER*4 NARRAY(IDIM),INDEX1(IDIM)
	CHARACTER WORD*(*),BUFF*80
 
C Loop on all the galaxies:
 
	NINDEX1=0
 
	DO 800 IGAL=1,NGALAXY
	  NLINE=NARRAY(IGAL)
	    IF(NLINE.EQ.0) GO TO 800
C NLINE: number of lines for the galaxy number I	
 
	DO 700 ILINE=1,NLINE
 
C Search for the word in a line
	  BUFF=ARRAY(IGAL,ILINE)
	  K=INDEX(BUFF,WORD(1:LWORD))
 
C	  PRINT *,' IGAL,ILINE,K',IGAL,ILINE,K
 
C When the word is found (K > 0) we put the number of
C the current galaxy in the array INDEX1, incrementing
C the number of points in INDEX1 and then we go to the next galaxy
	    IF(K.NE.0)THEN
	      NINDEX1=NINDEX1+1
	      INDEX1(NINDEX1)=IGAL
	      GO TO 800
	    ENDIF
 
700	CONTINUE
 
800	CONTINUE
 
	RETURN
	END
 
C*******************************************************************
C	Subroutine to look for a word in a 2-dimensional
C array of characters*80 and write crosses in the output array when the
C item is found.
C
C Input
C WORD*(1:LWORD) : Word to look for
C ARRAY(200,30) : Array where to look for the word
C IBEGIN: Index where to start writing on the output array.
C
C Output
C NUMBER : number of detections (crosses)
C OUTPUT(200)*120 : Output with the results
C*******************************************************************
	SUBROUTINE RD_CORREL1(WORD,LWORD,ARRAY,NARRAY,OUTPUT,NUMBER,
	1	IBEGIN,NGALAXY)
	PARAMETER (IDIM=200)
	CHARACTER*80 ARRAY(IDIM,*)
	INTEGER*4 NARRAY(IDIM)
	LOGICAL FOUND
	CHARACTER OUTPUT(200)*120
	CHARACTER WORD*(*),BUFF*80
 
10	FORMAT(A)
 
C Loop on all the galaxies:
 
	NUMBER=0
 
	DO 800 IGAL=1,NGALAXY
	  FOUND=.FALSE.
 
C NLINE: number of lines for the galaxy number I	
	  NLINE=NARRAY(IGAL)
 
	  IF(NLINE.GT.0)THEN
 
	    DO ILINE=1,NLINE
 
C Search for the word in a line
	      BUFF=ARRAY(IGAL,ILINE)
	      K=INDEX(BUFF,WORD(1:LWORD))
 
C When the word is found :
	      IF(K.NE.0)THEN
	        FOUND=.TRUE.
	      ENDIF
 
	    END DO
 
	 ENDIF
 
C Writing the result :
	  IF(FOUND)THEN
	    NUMBER=NUMBER+1
	    WRITE(OUTPUT(IGAL)(IBEGIN:120),11)
11	    FORMAT('&& *')
	  ELSE
	    WRITE(OUTPUT(IGAL)(IBEGIN:120),12)
12	    FORMAT('&&  ')
	  ENDIF
 
800	CONTINUE
 
	RETURN
	END
C*******************************************************************
C	Subroutine to look for a word in a 2-dimensional
C array of characters*80 and write the corresponding value in the output array
C
C Input
C WORD*(1:LWORD) : Word to look for
C ARRAY(200,30) : Array where to look for the word
C IBEGIN: Index where to start writing on the output array.
C
C Output
C VALUES : array with the values (if numbers)
C NUMBER : number of values written in the output file
C OUTPUT(200)*120 : Output with the results
C*******************************************************************
	SUBROUTINE RD_CORREL2(WORD,LWORD,ARRAY,NARRAY,OUTPUT,
	1	VALUES,NUMBER,IBEGIN,NGALAXY)
	PARAMETER (IDIM=200)
	REAL*4 VALUES(*)
	INTEGER*4 NARRAY(*)
	CHARACTER*80 ARRAY(IDIM,*)
	CHARACTER OUTPUT(*)*120,WORD*(*),BUFF*80
	LOGICAL FOUND
 
10	FORMAT(A)
 
C Loop on all the galaxies:
	NUMBER=0
 
	DO 800 IGAL=1,NGALAXY
 
C NLINE: number of lines for the galaxy number I	
	  NLINE=NARRAY(IGAL)
	  FOUND=.FALSE.
 
	  IF(NLINE.GT.0)THEN
 
	    DO ILINE=1,NLINE
 
C Search for the word in a line
	      BUFF=ARRAY(IGAL,ILINE)
	      K=INDEX(BUFF,WORD(1:LWORD))
 
C When the word is found :
	      IF(K.NE.0)THEN
	        K1=MIN(79,K+LWORD)
	        K2=MAX(K1,INDEX(BUFF(K1:80),';')+K1-2)
	        K3=MAX(K1,INDEX(BUFF(K1:80),'#')+K1-2)
	        IF(K3.NE.K1)K2=MIN(K2,K3)
	        WRITE(OUTPUT(IGAL)(IBEGIN:120),13) BUFF(K1:K2)
13	        FORMAT('&&',A<K2-K1+1>)
	        READ(BUFF(K1:K2),*,ERR=28)VALUES(NUMBER)
29	        FOUND=.TRUE.
	      ENDIF
 
	    END DO
 
	 ENDIF
 
C If the key word has not been found:
	    IF(.NOT.FOUND)THEN
	        WRITE(OUTPUT(IGAL)(IBEGIN:120),14)
14	        FORMAT('&&$\ldots$')
	    ELSE
C Please leave this here, otherwise, too many counts...
	        NUMBER=NUMBER+1
	    ENDIF
 
800	CONTINUE
 
	RETURN
 
C---------------------------------------------------------------
C When there is an error, try to correct it:
28	PRINT *,' ERROR READING THE VALUE FOR OBJECT #',IGAL
	PRINT *,'  BUFF(K1:K2)',BUFF(K1:K2)
 
C Second attempt with a correction:
94	 IF((BUFF(K2:K2).GE.'0'.AND.BUFF(K2:K2).LE.'9')
	1	.OR.(K2-K1).LT.1) GOTO 95
	   K2=K2-1
	 GOTO 94
 
95	 PRINT *,'  BUFF(K1:K2)',BUFF(K1:K2)
	 READ(BUFF(K1:K2),*,ERR=30)VALUES(NUMBER)
	 PRINT *,' The correction has been successful:',
	 1	' CONGRATULATIONS!'
	GO TO 29
 
C If the correction has failed :
30	VALUES(NUMBER)=-32000.
	PRINT *,' ERROR AGAIN: I GIVE UP WITH OBJECT #',IGAL
	GO TO 29
C-----------------------------------------------------------------
	END
 
C*******************************************************************
C	Subroutine to look for a word, WORD1(1:LWORD),
C	in a monodimensional array of characters ARRAY.
C*******************************************************************
	SUBROUTINE SEARCH1(WORD,LWORD,ARRAY,INDEX1,NINDEX1,NGALAXY)
	INTEGER*4 INDEX1(*)
	CHARACTER*(*) ARRAY(*)
	CHARACTER WORD*(*)
 
C Loop on all the galaxies:
 
	NINDEX1=0
 
	DO 800 IGAL=1,NGALAXY
 
	  K=INDEX(ARRAY(IGAL),WORD(1:LWORD))
 
C When the word is found (K > 0) we put the number of
C the current galaxy in the array INDEX1, incrementing
C the number of points in INDEX1 and then we go to the next galaxy
 
	  IF(K.GT.0)THEN
	    NINDEX1=NINDEX1+1
	    INDEX1(NINDEX1)=IGAL
	  ENDIF
 
800	CONTINUE
 
900	RETURN
	END
 
C*******************************************************************
C	Subroutine to sort an array according to its first value
C*******************************************************************
	SUBROUTINE CLASS(ARRAY,INDEX1,NPOINT)
	CHARACTER*(*) ARRAY(*)
	INTEGER*4 INDEX1(*)	
C
C Initialization of INDEX1
C
	DO J=1,NPOINT
	  INDEX1(J)=J
	END DO
C
C Method of the "bubble"
C
 
600	INVERS=0
 
	DO J=1,NPOINT-1
	  I1=INDEX1(J)
	  I2=INDEX1(J+1)
	  IF(ARRAY(I1).GT.ARRAY(I2))THEN
	    INDEX1(J+1)=I1
	    INDEX1(J)=I2
	    INVERS=INVERS+1
	  ENDIF	
	END DO
 
	IF(INVERS.NE.0) GO TO 600
 
	RETURN
	END
C**********************************************************
C	SUBROUTINE ASCII_REAL(WORD,ILENTH,VALUE)
C
C Subroutine to read a word and return the value
C**********************************************************
	SUBROUTINE ASCII_REAL(WORD,ILENTH,VALUE)
	CHARACTER WORD*80,ANS*1
	REAL*4 VALUE
	INTEGER*4 IWORK(80)
10	FORMAT(A)
 
	ISTART=1
	VALUE=0.
	SIGN=1.
 
	IF(WORD(1:1).EQ.'+')THEN
	  SIGN=1.
	  ISTART=2
	ENDIF
 
	IF(WORD(1:1).EQ.'-')THEN
	  SIGN=-1.
	  ISTART=2
	ENDIF
	
	IDOT=0
C IDOT : location of the dot
 
	DO 22 I=ISTART,ILENTH
 
	IF(WORD(I:I).EQ.'.')THEN
	  IDOT=I
	  GO TO 22
	ENDIF
C We can't decode the letters or other symbols
 
	IF(WORD(I:I).LT.'0'.OR.WORD(I:I).GT.'9')THEN
	  IWORK(I)=0
	ELSE
	  DECODE(1,100,WORD(I:I))IWORK(I)
100	  FORMAT(I1)
	ENDIF
 
22	CONTINUE
 
C Now we have in IWORK the series of integer 090656 and
C the location of the dot if any.
	IF(IDOT.EQ.0)THEN
 
	    DO I=ISTART,ILENTH
	      EXP=FLOAT(ILENTH-I)
	      VALUE=VALUE+FLOAT(IWORK(I))*10**EXP
	    END DO
 
	ELSE
 
	    DO I=ISTART,IDOT-1
	      EXP=FLOAT(IDOT-I-1)
	      VALUE=VALUE+FLOAT(IWORK(I))*10**EXP
	    END DO
 
	    DO I=IDOT+1,ILENTH
	      EXP=FLOAT(IDOT-I)
	      VALUE=VALUE+FLOAT(IWORK(I))*10**EXP
	    END DO
 
	ENDIF
 
	VALUE=VALUE*SIGN
	RETURN
	END
