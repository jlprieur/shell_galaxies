C++*************************************************************
*    LOWER_CASE.FOR 
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
*    From UPPER_CASE.FOR...
*    But here, please note that here all the lines are converted.
C--*************************************************************
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
 
150	OPEN ( UNIT=IOUT,FILE='l_'//INFILE,
     :          STATUS='NEW')
 
	WRITE(IWRIT,'(A)')' Working now ... '
 
*    Read the input file and transfer it
200	CONTINUE
 
	READ(IIN,400,ERR=350,END=300) LINE
 
        CALL DOWN(LINE)
 
	WRITE(IOUT,'(A)') LINE
 
	GOTO 200
 
300     CONTINUE
*    end transfer
 
* Just to skip error message
        GOTO 360
 
350     WRITE(IWRIT,'(A)') ' Error while reading the program'
 
360     CONTINUE
 
400	FORMAT(A)
 
	WRITE(IWRIT,'(A)')
 
	CLOSE(UNIT=IIN)
	CLOSE(UNIT=IOUT)
 
	END
 
*----------------------------------------------------------------
      SUBROUTINE DOWN(C)
*+
*  purpose: change the lowercase into uppercase
*+
      CHARACTER*(*) C
      INTEGER J
 
      IRECORD = 0
      ICOUNT = 1
 
      DO 40 J = 1,LEN(C)
         IF(C(J:J).GE.'A'.AND.C(J:J).LE.'Z') THEN
             C(J:J) = CHAR(ICHAR(C(J:J)) + ICHAR('a') - ICHAR('A'))
         ENDIF
40    CONTINUE
 
      END
 
