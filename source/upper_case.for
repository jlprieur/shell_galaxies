C++
*    ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
*    s                                                            s
*    s                       UPPER_CASE.FOR                       s
*    s                                                            s
*    ssssssssssssssssssssssssssssssssssssssssssssssssssssssssssssss
*
*    Purpose :   Translate the program from lowercase to uppercase
*                (for the Fortran-77 compatibility)
*
*         in :   infile.for  ( the orignal program)
*        out :   outfile.for (= 'P'+INFILE)
*
*     Author :   Yanling FANG   ESO -- Garching
*    Language:   Fortran 77
*    Version :   1.1 10-Nov-1988
*	
*       note :   Only translate the command part, not including the
*                comment and character to be written
*
C--
 
 
*
	IMPLICIT NONE
	INTEGER LENGTH,IPOSI
 
	CHARACTER*40 INFILE
	CHARACTER*80 LINE,SCHAR
 
*    Define read & write units, and file's channel to be opened
	INTEGER  IREAD,IWRIT,IIN,IOUT
        DATA     IREAD,IWRIT,IIN,IOUT,LINE / 5,6,10,20,' ' /
 
*    Open input& output  files
	WRITE(IWRIT,'(A)')
 
100	CONTINUE
	WRITE(IWRIT,'(A,$)') ' Name of the input file : '
	READ(IREAD,'(A)')INFILE
 
	OPEN ( UNIT=IIN,FILE=INFILE,ERR=999,STATUS='OLD')
	GOTO 150
999	WRITE(IWRIT,'(A)')' Wrong name, have another try !'
	GOTO 100
 
150	OPEN ( UNIT=IOUT,FILE='P'//INFILE,
     :          STATUS='NEW')
 
	WRITE(IWRIT,'(A)')' Working now ... '
 
*    Read the input file and transfer it
200	CONTINUE
 
	READ(IIN,400,ERR=350,END=300) LINE
 
        IF (.NOT.(LINE(1:1).EQ.'*'.OR.
     :            LINE(1:1).EQ.'C'.OR.
     :            LINE(1:1).EQ.'c'.OR.
     :            LINE(1:1).EQ.'!') ) CALL UP(LINE)
 
*    To remove the dummy blanks in the end of line
         LENGTH   = LEN(LINE)
         DO 250 I = LENGTH,1,-1
           SCHAR = LINE(I:I)
           IPOSI = I
           IF (SCHAR.NE.' ') GOTO 260
250      CONTINUE
 
*    To write the proper line in the output file
260     CONTINUE
	WRITE(IOUT,'(A)') LINE(:IPOSI)
 
	GOTO 200
 
300     CONTINUE
*    end transfer
 
        GOTO 360
 
350     WRITE(IWRIT,'(A)') ' Error while reading the program'
 
360     CONTINUE
 
400	FORMAT(A)
 
	WRITE(IWRIT,'(A)')
 
	CLOSE(UNIT=IIN)
	CLOSE(UNIT=IOUT)
 
	END
 
*----------------------------------------------------------------
 
 
      SUBROUTINE UP(C)
 
*+
*  purpose: change the lowercase into uppercase
*  note: 1. We don't change that the characters inside '...', this
*           is done by ICOUNT;
*        2. IRECORD is considered for the case '' when writing the
*           like :  WRITE(IWRIT,'('' simple way of writing '')')
*+
 
      CHARACTER*(*) C
      INTEGER J,ICOUNT,IRECORD
 
      IRECORD = 0
      ICOUNT = 1
 
      DO 40 J = 1,LEN(C)
 
         IF (C(J:J).EQ.'''') THEN
*        when  '  comes one by another, ICOUNT is not incremented
            IF ( J.NE.(IRECORD-1) ) ICOUNT = ICOUNT + 1
            IRECORD = J
         ENDIF
 
         GOTO (10,40,30), ICOUNT
 
*     Normal letter (not " ' ")
10       CONTINUE
         IF(C(J:J).GE.'a'.AND.C(J:J).LE.'z') THEN
             C(J:J) = CHAR(ICHAR(C(J:J)) + 'A' - 'a')
         ENDIF
         GOTO 40
 
*     Meet ' second time
30       CONTINUE
         ICOUNT = 1
 
40    CONTINUE
 
      END
 
