C++**********************************************************
C Set of subroutines to emulate VAX/VMS extensions on UNIX system.
C Interface with C routines.
C
C Contains JLP_GETVM, JLP_FREEVM, JLP_DO_COMMAND,
C    JLP_INIT_TIMER, JLP_SHOW_TIMER, JLP_DATE_TIME, JLP_GET_SYMBOL,
C    JLP_SET_SYMBOL,
C    JLP_DIRECTORY, JLP_CLI_PRESENT, JLP_CLI_GET_VALUE
C
C JLP
C Version 64 bits of 15-11-2007
C--*********************************************************
C----------------------------------------------------------
C Subroutine JLP_GETVM ---> pointer of real*4 ARRAY(NX,NY)
C To get virtual memory space with MADRID common block
C----------------------------------------------------------
        SUBROUTINE JLP_GETVM(PNTR_ARRAY,MEMSIZE)
	LOGICAL STATUS
C Linux 64 bits: I switch to INTEGER*8:
        INTEGER*8 PNTR_ARRAY,POINTER,MEMSIZE
C Should keep MADRID as integer*4 to be able to compute the address from the index!
        INTEGER*4 MADRID(1)
        COMMON /VMR/MADRID
 
C Getting memory space for the array "IMAGE":
C MEMSIZE=4*NX*NY if real*4 array
C Calling C jlp_gvm routine:
        call jlp_gvm(POINTER,MEMSIZE)
C Before jan99:
C       PNTR_ARRAY=(POINTER-JLP_LOC(MADRID(1)))/4 + 1
        CALL JLP_LOC(MADRID(1),PNTR_ARRAY)

        PNTR_ARRAY=(POINTER-PNTR_ARRAY)/4 + 1
 
	RETURN
	END
C----------------------------------------------------------
C Subroutine JLP_FREEVM to free the memory space
C previously allocated with MADRID common block
C----------------------------------------------------------
	SUBROUTINE JLP_FREEVM(PNTR_ARRAY,MEMSIZE)
C Linux 64 bits: I switch to INTEGER*8:
        INTEGER*8 PNTR_ARRAY,POINTER,MEMSIZE
        INTEGER*4 MADRID(1)
        COMMON /VMR/MADRID
 
        CALL JLP_LOC(MADRID(PNTR_ARRAY),POINTER)
C Calling C jlp_fvm routine:
        call jlp_fvm(POINTER)
 
        RETURN
        END
C----------------------------------------------------------
C To replace cli$present
C----------------------------------------------------------
        LOGICAL FUNCTION JLP_CLI_PRESENT(STRING)
        CHARACTER STRING*(*),BUFFER*80,STRING1*80
        JLP_CLI_PRESENT=.FALSE.
 
        OPEN(29,FILE='jlp_command.tmp',STATUS='OLD',ERR=999)
 
10	FORMAT(A)
 
	STRING1=' '
	STRING1=STRING
	KMAX=MAX(1,INDEX(STRING1,' ')-1)
C@	 PRINT *,' STRING1 :',STRING1(1:KMAX)
C@	 PRINT *,' KMAX :',KMAX
	 DO I=1,80
	   BUFFER=' '
	   READ(UNIT=29,ERR=98,END=98,FMT=10) BUFFER(1:I)
C@	   PRINT *,' BUFFER:',BUFFER(1:I)
	   CLOSE(29)
	   OPEN(29,FILE='jlp_command.tmp',STATUS='OLD',ERR=999)
	   DO K=1,I-KMAX-1
	     IF((BUFFER(K:K).EQ.'/')
     1	.AND.(BUFFER(K+1:K+KMAX).EQ.STRING1(1:KMAX))) THEN
	       JLP_CLI_PRESENT=.TRUE.
C@	       PRINT *,' OK FOUND :'
	       GOTO 98
	     ENDIF
	   END DO
	 END DO
 
98	CLOSE(29)
 
999	RETURN
        END
C----------------------------------------------------------
C To replace cli$get_value
C----------------------------------------------------------
        INTEGER FUNCTION JLP_CLI_GET_VALUE(STRING,VALUE)
	CHARACTER STRING*(*),VALUE*(*),BUFFER*81,STRING1*80
	JLP_CLI_GET_VALUE=1
 
	OPEN(29,FILE='jlp_command.tmp',STATUS='OLD',ERR=999)
 
10	FORMAT(A)
 
	STRING1=' '
	STRING1=STRING
	KMAX=MAX(1,INDEX(STRING1,' ')-1)
C@	PRINT *,' STRING1 :',STRING1(1:KMAX)
C@	PRINT *,' KMAX :',KMAX
	 DO I=1,80
	   BUFFER=' '
	   READ(UNIT=29,ERR=98,END=98,FMT=10) BUFFER(1:I)
C@	   PRINT *,' BUFFER:',BUFFER(1:I)
	   CLOSE(29)
	   OPEN(29,FILE='jlp_command.tmp',STATUS='OLD',ERR=999)
	 END DO
 
C Now look for the value:
98	KMAX=KMAX+1
	STRING1(KMAX:KMAX)='='
	  DO K=1,I-KMAX-1
	     IF((BUFFER(K:K).EQ.'/')
     1	.AND.(BUFFER(K+1:K+KMAX).EQ.STRING1(1:KMAX))) THEN
	       JLP_CLI_GET_VALUE=0
C@	       PRINT *,' OK FOUND :'
	       I1=K+INDEX(BUFFER(K+1:80),'/')
	       IF(I1.LE.K+KMAX+1) I1=K+INDEX(BUFFER(K+1:81),' ')
	       VALUE=' '
	       VALUE=BUFFER(K+KMAX+1:I1-1)
C@	       PRINT *,' VALUE :',VALUE
	       GOTO 98
	     ENDIF
	 END DO
 
	CLOSE(29)
 
999	RETURN
        END
C----------------------------------------------------------
C To replace lib$do_command
C----------------------------------------------------------
        SUBROUTINE JLP_DO_COMMAND(STRING)
        CHARACTER STRING*(*)
        RETURN
        END
C----------------------------------------------------------
C To replace lib$init_timer
C----------------------------------------------------------
        SUBROUTINE JLP_INIT_TIMER
        RETURN
        END
C----------------------------------------------------------
C To replace lib$show_timer
C----------------------------------------------------------
        SUBROUTINE JLP_SHOW_TIMER
        RETURN
        END
C----------------------------------------------------------
C To replace lib$date_time
C----------------------------------------------------------
        SUBROUTINE JLP_DATE_TIME(STRING)
        CHARACTER STRING*(*)
        INTEGER ISTATUS
        CALL JLP_CTIME(STRING,ISTATUS)
        RETURN
        END
C----------------------------------------------------------
C To replace lib$get_symbol
C----------------------------------------------------------
        SUBROUTINE JLP_GET_SYMBOL(SYMBOL,VALUE,ISTATUS)
	CHARACTER VALUE*(*),SYMBOL*(*),MYSYMBOL*30
	INTEGER ISTATUS,LENGTH1
	MYSYMBOL=SYMBOL
        LENGTH1=30
	CALL JLP_GETENV(MYSYMBOL,LENGTH1,VALUE,ISTATUS)
        RETURN
	END
C----------------------------------------------------------
C To replace lib$set_symbol
C----------------------------------------------------------
        SUBROUTINE JLP_SET_SYMBOL(SYMBOL,VALUE,ISTATUS)
	CHARACTER VALUE*(*),SYMBOL*(*)
	CHARACTER MYVALUE*80,MYSYMBOL*30
	INTEGER ISTATUS,LENGTH1,LENGTH2
        LENGTH1=30
        LENGTH2=80
	MYSYMBOL=SYMBOL
        MYVALUE=VALUE
	CALL JLP_SETENV(MYSYMBOL,LENGTH1,MYVALUE,LENGTH2,ISTATUS)
        RETURN
	END
C----------------------------------------------------------
C JLP_DIRECTORY
C----------------------------------------------------------
        SUBROUTINE JLP_DIRECTORY(INPUT,NUMBER,QUIET)
	CHARACTER INPUT*(*),OUTPUT(100)*60
C Nota: when we put buffer*10000, we get an error on the SUN sytem...
C SUN authorises 6000 characters
C and IBM authorises only 500 characters !!!!!!
C	CHARACTER BUFFER*6000,MYINPUT*80
	CHARACTER BUFFER*500,MYINPUT*80
	INTEGER NUMBER
	INTEGER QUIET,LENGTH1
	COMMON /JLP_DIRECT/OUTPUT
        LENGTH1=80
	MYINPUT=INPUT
	CALL JLP_DIR(MYINPUT,LENGTH1,BUFFER,NUMBER,QUIET,ISTAT)
	  K=1
	  DO I=1,NUMBER
	   OUTPUT(I)=BUFFER(K:K+59)
	   K=K+60
	  ENDDO
	IF(QUIET.EQ.0)THEN
	  DO I=1,NUMBER
           DO KK=1,20
            IF(ICHAR(OUTPUT(I)(KK:KK)).LT.32.OR.
     1         ICHAR(OUTPUT(I)(KK:KK)).GT.126)OUTPUT(I)(KK:KK)=' '
           END DO
	   WRITE(6,40) I,OUTPUT(I)(1:20)
40	   FORMAT(' File#',I3,' ',A20)
	  END DO
	ENDIF
        RETURN
        END
