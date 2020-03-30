C++******************************************************************
C Program TEST_VAX1
C To test the modules in JLP_VAX1.FOR
C (Vax emulator)
C
C JLP
C Version 20-03-90
C--******************************************************************
	PROGRAM TEST_VAX1
	CHARACTER BUFFER*40,BUFF_DIR*60,NAME*40,SYMBOL*40
	INTEGER*4 PNTR_ARRAY,ISTATUS
	INTEGER*4 MADRID(1)
	COMMON /VMR/MADRID
 
	PRINT *,' CALLING INIT_TIMER'
	CALL JLP_INIT_TIMER
 
        IF(I.NE.2341)GOTO 345
 
	PRINT *,' CALLING GETVM'
	CALL JLP_GETVM(PNTR_ARRAY,400)
	PRINT *,' CALLING TEST1'
	CALL TEST1(MADRID(PNTR_ARRAY),10,10)
	PRINT *,' CALLING FREEVM'
	CALL JLP_FREEVM(PNTR_ARRAY,400)
 
        SYMBOL='TT1'
	CALL JLP_GET_SYMBOL(SYMBOL,BUFFER,ISTATUS)
	PRINT *,' CALL GET_SYMBOL(TT1):',ISTATUS,BUFFER
	CALL JLP_SET_SYMBOL('TT1','4567.089,890.8,98.7',ISTATUS)
	PRINT *,' CALL SET_SYMBOL(TT1):',ISTATUS
	CALL JLP_GET_SYMBOL('TT1',BUFFER,ISTATUS)
	PRINT *,' CALL GET_SYMBOL(TT1):',ISTATUS,BUFFER
C	CALL JLP_DELETE_SYMBOL('TT1',ISTATUS)
C	PRINT *,' CALL DELETE_SYMBOL(TT1):',ISTATUS
	CALL JLP_GET_SYMBOL('TT1',BUFFER,ISTATUS)
	PRINT *,' CALL GET_SYMBOL(TT1):',ISTATUS,BUFFER
	CALL JLP_SET_SYMBOL('TT1',' ADJKDSAKHJ',ISTATUS)
	PRINT *,' CALL SET_SYMBOL(TT1):',ISTATUS
	CALL JLP_GET_SYMBOL('TT1',BUFFER,ISTATUS)
	PRINT *,' CALL GET_SYMBOL(TT1):',ISTATUS,BUFFER
 
	CALL JLP_GET_SYMBOL('JLP_FORMAT',10,BUFFER,ISTATUS)
	PRINT *,' CALL GET_SYMBOL(JLP_FORMAT):',ISTATUS,' ',BUFFER
 
345	CALL JLP_DATE_TIME(BUFFER)
	PRINT *,' CALL DATE_TIME:',BUFFER
 
	PRINT *,' CALLING SHOW_TIMER'
	CALL JLP_SHOW_TIMER
 
	BUFF_DIR='*.dat'
	PRINT *,' Calling jlp_directory(*.dat)'
	CALL JLP_DIRECTORY(BUFF_DIR,I,0)
	PRINT *,' Number of files found:',I,BUFF_DIR
 
	BUFF_DIR='*e*.*'
	PRINT *,' Calling jlp_directory(*e*.*)'
	CALL JLP_DIRECTORY(BUFF_DIR,I,0)
	PRINT *,' Number of files found:',I,BUFF_DIR
 
C	PRINT *,' CALLING JLP_RANDNAME'
C	CALL JLP_RANDNAME(NAME)

C Last command, because, exit afterwards:
	PRINT *,' Calling do_command ($DIRECTORY)'
	CALL JLP_DO_COMMAND('$DIRECTORY')
 
	STOP
	END
C----------------------------------------------------------
C Subroutine TEST1
C----------------------------------------------------------
	SUBROUTINE TEST1(ARRAY,NX,NY)
	REAL*4 ARRAY(NX,NY)
	DO J=1,10
	  DO I=1,10
	    ARRAY(I,J)=1.01
	  END DO
	END DO
	PRINT *,' ARRAY(10,10)',ARRAY(10,10)
	RETURN
	END
C---------------------------------------------------------
C	include 'jlpsub:jlp_vax1.for'