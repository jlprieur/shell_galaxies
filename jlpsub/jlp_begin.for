C++--------------------------------------------------------
C Subroutine JLP_BEGIN
C To check if the input parameters have to be found
C in the file "JLP_LU5.TMP", or read from LU=5 (Fortran default for input
C from a terminal)
C and if the output has to be written
C in the file "JLP_LU6.TMP", or directly on LU=6 (Fortran default for output
C from a terminal)
C Version of 30-11-2001
C----------------------------------------------------------
	SUBROUTINE JLP_BEGIN
	INTEGER*4 JLP_LU5,ISTATUS
	CHARACTER BUFFER*40,SYMBOL*40
C 	COMMON /JLP_LU56/JLP_LU5,JLP_LU6

C Input:
	JLP_LU5=0
C Read the symbol JLP_PROMPT
        BUFFER=' '
        SYMBOL='JLP_PROMPT'
	IL=10
	CALL JLP_GETENV(SYMBOL,IL,BUFFER,ISTATUS)
C        WRITE(6,*) 'JLP_BEGIN/Buffer: ',BUFFER
C Then check the error flag:
	IF(ISTATUS.EQ.0)THEN
C Reads the buffer:
C	     WRITE(6,*)' BUFFER:',BUFFER(1:1)
	   IF((BUFFER(1:1).EQ.'N').OR.(BUFFER(1:1).EQ.'n'))THEN
C	     WRITE(6,*)' JLP_BEGIN/Opening jlp_lu5.tmp'
	     OPEN(5,FILE='jlp_lu5.tmp',STATUS='OLD',ERR=98)
	     JLP_LU5=5
C            WRITE(6,*)' JLP_BEGIN/Direct input of parameters from jlp_lu5.tmp'
	   ENDIF
	ENDIF

C Output:
98	JLP_LU6=0
C Read the symbol JLP_QUIET
        BUFFER=' '
        SYMBOL='JLP_QUIET'
	IL=9
	CALL JLP_GETENV(SYMBOL,IL,BUFFER,ISTATUS)
C Then check the error flag:
	IF(ISTATUS.EQ.0)THEN
C Reads the buffer:
	   IF((BUFFER(1:1).EQ.'Y').OR.(BUFFER(1:1).EQ.'y'))THEN
C	     WRITE(6,*)' Redirection of messages to jlp_lu6.tmp'
	     OPEN(6,FILE='jlp_lu6.tmp',STATUS='UNKNOWN',ERR=99)
	     JLP_LU6=6
	   ENDIF
	ENDIF

99	RETURN
	END
