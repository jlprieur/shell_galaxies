C++******************************************************************
C Program HELP_SYNTAX
C To read the help header of a program (limited with C++ and C--):
C This program is mainly used with "RUNS name_of_the_file ?"
C and "RUNS name_of_the_doc ??"
C
C SYNTAX:
C         RUNS HELP_SYNTAX name_of_the_program
C or      RUNS name_of_the_program ?
C or      RUNS name_of_the_documentation ??
C
C Examples:
C         RUNS FITELLI ?
C         RUNS GRAPHICS ??
C
C JLP
C Version 23-07-90
C--******************************************************************
	PROGRAM HELP_SYNTAX
	CHARACTER BUFFER*40,NAME*50
	INTEGER*4 ISTATUS
 
	CALL JLP_BEGIN
 
C This program is mainly used with "RUNS name_of_the_program ?"
C So we do not display the prompting line in that case:
	CALL JLP_GET_SYMBOL('JLP_PROMPT',BUFFER,ISTATUS)
	IF(ISTATUS.NE.0..OR.
     1	(BUFFER(1:1).NE.'N'.AND.BUFFER(1:1).NE.'n'))THEN	
	  WRITE(6,*) ' Program for which you want documentation ?'
	ENDIF
	  READ(5,10) BUFFER
10	FORMAT(A)
 
C	BUFFER(40:40)='.'
C	I=MIN(INDEX(BUFFER(1:40),' '),INDEX(BUFFER(1:40),'.'))-1
C	NAME='source:'//BUFFER(1:I)//'.for'
	NAME=BUFFER
 
	OPEN(4,FILE=NAME,ACCESS='SEQUENTIAL',
     1	STATUS='OLD',ERR=99)
 	CALL READ_STX
	CLOSE(4)
999	CALL JLP_END
	STOP
99	WRITE(6,*) ' Sorry: unable to open ',NAME
	GOTO 999
	END
C----------------------------------------------------------
C Subroutine READ_STX
C Prints everything between the first line starting with C++ or *++
C and the first starting with C-- or *--
C----------------------------------------------------------
	SUBROUTINE READ_STX
	CHARACTER BUF1*80
	LOGICAL PRINT_DOC
	DO J=1,1000
	  READ(4,101,END=99) BUF1
101	  FORMAT(A80)
C Start printing when C++ is found)
	  IF(BUF1(2:3).EQ.'++') PRINT_DOC=.TRUE.
C Print the line:
	  IF(PRINT_DOC) WRITE(6,101) BUF1
C Stop if comments are finished (when C-- has not been found)
	  IF(BUF1(1:1).EQ.'c') BUF1(1:1)='C'
	  IF(PRINT_DOC.AND.((BUF1(1:1).NE.'C')
     1	.AND.(BUF1(1:1).NE.'*'))) GOTO 99
C Stop when C-- has been found
	  IF(BUF1(2:3).EQ.'--') GOTO 99
	END DO
99	RETURN
	END
