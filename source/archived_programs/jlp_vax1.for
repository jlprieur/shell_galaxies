C++**********************************************************
C Set of subroutines calling specific Vax routines
C Same as jlp0_vax1.for but VAX/VMS version
C
C Contains JLP_GETVM, JLP_FREEVM, JLP_GET_SYMBOL,
C	   JLP_SET_SYMBOL, JLP_DELETE_SYMBOL,JLP_DATE_TIME,
C          JLP_INIT_TIMER, JLP_SHOW_TIMER, JLP_DO_COMMAND, JLP_DIRECTORY
C	   JLP_CLI_PRESENT,JLP_CLI_GET_VALUE
C Non Vax specific:
C          JLP_RANDOM_NAME
C
C JLP
C Version of 18-03-90
C--*********************************************************
C----------------------------------------------------------
C Subroutine JLP_GETVM ---> pointer of ARRAY(NX,NY)
C To get virtual memory space
C----------------------------------------------------------
	SUBROUTINE JLP_GETVM(PNTR_ARRAY,MEMSIZE)
	INTEGER*4 PNTR_ARRAY,MEMSIZE
	LOGICAL STATUS
 
C Getting memory space for the array "IMAGE":
C MEMSIZE=4*NX*NY if real*4 array
C MEMSIZE=2*NX*NY if integer*2 image
 
C Calling VAX system routine:
	STATUS=LIB$GET_VM(MEMSIZE,PNTR_ARRAY)
 
C Exit if no space available:
	 IF(.NOT.STATUS) THEN
	   PRINT 21
21	   FORMAT(' FATAL ERROR IN JLP_GETVM',
	1	' NO DYNAMICAL MEMORY SPACE AVAILABLE')
	   STOP
	 ENDIF
 
	RETURN
	END
C----------------------------------------------------------
C Subroutine JLP_FREEVM to free the memory space
C previously allocated
C----------------------------------------------------------
	SUBROUTINE JLP_FREEVM(PNTR_ARRAY,MEMSIZE)
	INTEGER*4 PNTR_ARRAY,MEMSIZE
 
C Freeing the allocated memory:
	CALL LIB$FREE_VM(MEMSIZE,PNTR_ARRAY)
 
	RETURN
	END
 
C----------------------------------------------------------
C Subroutine JLP_GET_SYMBOL
C----------------------------------------------------------
	SUBROUTINE JLP_GET_SYMBOL(NAME,CONTENT,ISTATUS)
	INTEGER*4 ISTATUS
 	CHARACTER NAME*(*),CONTENT*(*)
	LOGICAL STATUS
 
	STATUS=LIB$GET_SYMBOL(NAME,CONTENT)
	ISTATUS=0
	IF(.NOT.STATUS)ISTATUS=1
 
	RETURN
	END
C----------------------------------------------------------
C Subroutine JLP_SET_SYMBOL
C----------------------------------------------------------
	SUBROUTINE JLP_SET_SYMBOL(NAME,CONTENT,ISTATUS)
	INTEGER*4 ISTATUS
 	CHARACTER NAME*(*),CONTENT*(*)
	LOGICAL STATUS
 
	STATUS=LIB$SET_SYMBOL(NAME,CONTENT)
	ISTATUS=0
	IF(.NOT.STATUS)ISTATUS=1
 
	RETURN
	END
C----------------------------------------------------------
C Subroutine JLP_DELETE_SYMBOL
C----------------------------------------------------------
	SUBROUTINE JLP_DELETE_SYMBOL(NAME,ISTATUS)
	INTEGER*4 ISTATUS
 	CHARACTER NAME*(*)
	LOGICAL STATUS
 
	STATUS=LIB$DELETE_SYMBOL(NAME)
	ISTATUS=0
	IF(.NOT.STATUS)ISTATUS=1
 
	RETURN
	END
C----------------------------------------------------------
C Subroutine JLP_DATE_TIME
C----------------------------------------------------------
	SUBROUTINE JLP_DATE_TIME(STRING)
 	CHARACTER STRING*(*)
 
	CALL LIB$DATE_TIME(STRING)
 
	RETURN
	END
C----------------------------------------------------------
C Subroutine JLP_INIT_TIMER
C----------------------------------------------------------
	SUBROUTINE JLP_INIT_TIMER
 
	CALL LIB$INIT_TIMER
 
	RETURN
	END
C----------------------------------------------------------
C Subroutine JLP_SHOW_TIMER
C----------------------------------------------------------
	SUBROUTINE JLP_SHOW_TIMER
 
	CALL LIB$SHOW_TIMER
 
	RETURN
	END
C----------------------------------------------------------
C Subroutine JLP_DO_COMMAND
C (Warning: in Vax context, this routine contains a "STOP"...)
C----------------------------------------------------------
	SUBROUTINE JLP_DO_COMMAND(STRING)
 	CHARACTER STRING*(*)
 
	CALL LIB$DO_COMMAND(STRING)
 
	RETURN
	END
C------------------------------------------------------------------------
C Subroutine JLP_DIRECTORY to look for the files which correspond to
C the generic name INPUT (for example 012*.bdf)
C When only one file is found, the input name is set to the name of this file.
C
C Input:
C INPUT name of the files to look for
C QUIET flag, 1=true when the names are not displayed on the terminal
C
C Output:
C NUMBER number of files found
C INPUT if only one file has been found
C in common block, also:
C OUTPUT list of all the files which have been found
C------------------------------------------------------------------------
	SUBROUTINE JLP_DIRECTORY(INPUT,NUMBER,QUIET)
	PARAMETER (IDIM2=500)
	INTEGER*4 QUIET
	CHARACTER*(*) INPUT
C !! Leave it at 60, please...
	CHARACTER*60 RESULT,OUTPUT(IDIM2)
	CHARACTER*120 OUTPUT1
	INTEGER*4 CONTEXT,NUMBER,STATUS
	DATA LIB$_FOUND/65537/
	COMMON /JLP_DIRECT/OUTPUT
 
	NUMBER=0
	CONTEXT=0
	STATUS=LIB$_FOUND
 
94	IF (STATUS.NE.LIB$_FOUND) GOTO 95
C Look for the file name:
	  STATUS=LIB$FIND_FILE
	1	(%DESCR(INPUT),%DESCR(RESULT),%REF(CONTEXT))
C	1(%DESCR(INPUT),%DESCR(RESULT),%REF(CONTEXT),%DESCR('TOTO.*;0'))
	  IF(STATUS.EQ.LIB$_FOUND) THEN
C When QUIET, the names are not displayed on the terminal:
	    IF(QUIET.EQ.0)WRITE(6,*) ' ',RESULT(1:60)
	    NUMBER=NUMBER+1
	    OUTPUT(NUMBER)=RESULT(1:60)
	  ENDIF
	GOTO 94
 
C End of the session:
95	CALL LIB$FIND_FILE_END(%REF(CONTEXT))
 
C When only one file has been found, the input name is modified, and
C set to the name of this file:
	IF(NUMBER.EQ.1)THEN
C Generate the output name:
	    IF(INDEX(INPUT,']').NE.0)THEN
	       OUTPUT1=INPUT(:INDEX(INPUT,']'))
	1	//OUTPUT(1)(INDEX(OUTPUT(1),']')+1:)
	    ELSE
	       OUTPUT1=OUTPUT(1)(INDEX(OUTPUT(1),']')+1:)
	    ENDIF
	    IF(INDEX(INPUT,';').EQ.0)OUTPUT1=OUTPUT1(:INDEX(OUTPUT1,';')-1)
	    INPUT=OUTPUT1(1:LEN(INPUT))
	ENDIF
	
	RETURN
	END
C****************************************************************************
C To replace CLI$GET_VALUE
C****************************************************************************
	INTEGER*4 FUNCTION JLP_CLI_GET_VALUE(STRING,VALUE)
	INTEGER*4 CLI$GET_VALUE,I
	CHARACTER STRING*(*),VALUE*(*)
	I = CLI$GET_VALUE(STRING,VALUE)
	JLP_CLI_GET_VALUE = 1-I
	RETURN
	END
C****************************************************************************
C To replace CLI$PRESENT
C****************************************************************************
	LOGICAL*4 FUNCTION JLP_CLI_PRESENT(STRING)
	LOGICAL*4 CLI$PRESENT,I
	CHARACTER STRING*(*)
	I = CLI$PRESENT(STRING)
	JLP_CLI_PRESENT = I
	RETURN
	END

C---------------------------------------------------------
C JLP_RANDNAME
C Finds a random name for the metafile with the time,
C and set the symbol JLP_METAFILE to that new name:
C---------------------------------------------------------
	subroutine jlp_randname(name)
	  integer*4 istatus
	  character date*40,name*(*)
C Use the date which always has the time encoded like 17:34:56 (Unix and Vax):
	  call jlp_date_time(DATE)
	  i=index(DATE,':')-2
	  name=' '
	  name='meta'//DATE(i:i+1)//DATE(i+3:i+4)//DATE(i+6:i+7)//'.tmp'
	  call jlp_set_symbol('JLP_METAFILE',name,istatus)
	  return
	end
