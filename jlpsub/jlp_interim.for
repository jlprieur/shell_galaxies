C++-----------------------------------------------------------
C Set of subroutines to access BDF files.
C JLP version of INTERIM routines
C
C Contains :
C JLP_RDIMAG, JLP_ACCFRM, JLP_GETPPI, JLP_WRIMAG, JLP_WRFRM
C
C JLP
C Version of 11-07-91
C
C-------------------------------------------------------------
	SUBROUTINE JLP_RDIMAG(NAME,FORMAT,MAXDIM,DIMENS,
	1	ACTDIM,PNTR,FILENAME,STATUS)
C++
C     RDIMAG - Read Image Data
C
C     This routine is used to read a frame of type 'IMAGE' into
C     memory and return a pointer to it. The actual number of
C     dimensions (axes) is returned in ACTDIM and the size of each
C     dimension in the elements of the array DIMENS. The
C     pointer returned will be passed as %val(PNTR) to a lower
C     level routine which can read image data from the array
C     argument.
C
C     A frame that has been accessed by RDIMAG must be released,
C     after processing, by a call to FRDATA.
C
C     This routine is used to map an existing bulk data frame into
C     memory and return an address pointer to it. If the format of
C     the data is not that requested by the user, then automatic
C     conversion will occur. If the user wishes to be informed that
C     a conversion has taken place, he can specify the format code
C     as its corresponding negative value and the status return will
C     be set accordingly.
C
C
C     (NOTE that access violation will occur if any attempt is made
C     to modify frame data that is marked for input; the memory
C     space allocated to the frame data is read-only protected).
C
C     Input arguments:
C     ---------------
C     NAME:    CHARACTER expression:	Parameter name of data frame
C     FORMAT:  INTEGER expression:	Data format required
C     MAXDIM:  INTEGER expression:      Maximum number of dimensions
C					required.
C
C     Output arguments:
C     ----------------
C     DIMENS:  INTEGER array:		Array to hold dimension sizes
C     ACTDIM:  INTEGER variable:	Actual number of dimensions
C     PNTR:    INTEGER variable:	Pointer to data in memory
C     STATUS:  INTEGER variable:	Status return value
C
C
C     D.PEARCE  29/JUL/80  VERSION #2
C--
C
	IMPLICIT INTEGER(A-Z)
C
	CHARACTER*(*) FILENAME,NAME
	INTEGER*4     FORMAT,MAXDIM,DIMENS(*),ACTDIM,PNTR,STATUS
C
	CHARACTER*8   TYPE
	CHARACTER*20  VALUE
	CHARACTER*6   AXIS
	DATA AXIS/ 'NAXISn' /
C
	INCLUDE 'CORE:INTERIM(ERRPAR)'
C
C .....access bulk data frame :
	CALL JLP_ACCFRM(NAME,ENTRY,FILENAME,IERR)
	IF (IERR.NE.0)THEN
	 STATUS=1
	 RETURN
	ENDIF
C
C .....read frame if accessible
	CALL STL_RDFRM(ENTRY,FORMAT,TYPE,SIZE,PNTR,IERR1)
	IF (IERR1.NE.0) THEN
	 PRINT *,' JLP_RDIMAG/STL_RDFRM : Error when reading the frame'
	 STATUS=1
	 RETURN
	ENDIF
C
	IF (TYPE.NE.'IMAGE') THEN
	 PRINT *,' JLP_RDIMAG : Type code invalid  (not "IMAGE")'
	 STATUS=1
	 RETURN
	ENDIF
C
C Get the size of the array :
	CALL RDDSCR(NAME,'NAXIS',1,VALUE,N,IERR)
	IF (IERR.NE.0) THEN
	 PRINT *,' JLP_WRIMAG/RDDSCR : Frame-type invalid (not "IMAGE")'
	 ISTATUS=1
	 RETURN
	ENDIF
 
	CALL CTOI(VALUE,ACTDIM,IND)
C
	DO D=1,MAXDIM
	 DIMENS(D)=1
	END DO
 
	DO D=1,ACTDIM
	 CALL ITOC(D,AXIS(6:6),IND)
	 CALL RDDSCR(NAME,AXIS,1,VALUE,N,STAT)
	  IF (STAT.NE.0) THEN
	   PRINT *,' JLP_WRIMAG/RDDSCR : Frame-type invalid',
	1	' (not "IMAGE")'
	   ISTATUS=1
	   RETURN
	  ENDIF
	 CALL CTOI(VALUE,DIMENS(D),IND)
	END DO
C
	RETURN
	END
C----------------------------------------------------------------------------
	SUBROUTINE JLP_ACCFRM(NAME,ENTRY,FILENAME,STATUS)
C++
C   From STL_ACCFRM - Access Frame
C
C     This routine will search the Parameter Control Tables for
C     an existing frame parameter entry of the same name. If one
C     does not exist, then it is created and the bulk data file
C     is physically opened. This operation returns the I/O channel
C     assigned to the file, the FILES-11 file identification and
C     the device name on which the volume is mounted.
C
C     CALL JLP_ACCFRM(NAME,ENTRY,FILENAME,STATUS)
C
C     Input parameter:
C     ---------------
C     NAME:    CHARACTER expression:	Parameter name of data frame
C
C     Output parameters:
C     -----------------
C     ENTRY:   INTEGER variable:	PCT entry number
C     STATUS:  INTEGER variable:	Status return value
C
C
C     D.PEARCE  10/JUL/80  VERSION #2
C--
C
	IMPLICIT INTEGER(A-Z)
C
	INTEGER*4 ENTRY,MAXVAL,ACTVAL,STATUS
	LOGICAL DFAULT,ALREADY_IN,CONNECT_FILE,USERSUP
	CHARACTER*(*) NAME,FILENAME
C
	CHARACTER*64  BDFNAM
	CHARACTER*80  VALUES(32)
C
	INCLUDE 'CORE:INTERIM(PCTCOM)'
	INCLUDE 'CORE:INTERIM(ERRPAR)'
	
	STATUS=0
 
C  .....search for existing entry in PCT
	CALL STL_FINDPE(NAME,ENTRY)
C
C  .....if entry does not exist, then create one
	IF (ENTRY.EQ.0)CALL STL_CREPE(NAME,ENTRY)
C
C  .....check if frame is null
	IF (PCT_STATE(ENTRY).EQ.PCT_NULL) THEN
	 PRINT *,' JLP_ACCFRM : Frame null'
	 STATUS=1
	 RETURN
	ENDIF
C
C  .....check if frame entry is already active
	IF (PCT_STATE(ENTRY).EQ.PCT_ACTIVE) THEN
	 PRINT *,' JLP_ACCFRM : Frame already active'
	 STATUS=0
	 RETURN
	ENDIF
C
C  .....Get Program Parameter Information from the environment
C Warning : PROVAL called by GETPPI needs type-entry .
	IF(NAME(1:2).EQ.'IN')PCT_TYPE(ENTRY)='FRAME(R)'
	IF(NAME(1:3).EQ.'OUT')PCT_TYPE(ENTRY)='FRAME(W)'
 
C Warning : BDF file ;1 are only accepted :
	BDFNAM=' '
	IVERSION=INDEX(FILENAME,';')
	IF(IVERSION.EQ.0)THEN
	  BDFNAM=FILENAME
	ELSE
	  PRINT *,' Warning, only BDF files ;1  in INTERIM',
	1	' environment !'
	  I=IVERSION-1
	  BDFNAM(1:I)=FILENAME(1:I)
	ENDIF
 
	DFAULT=.TRUE.
	ALREADY_IN=.TRUE.
	CONNECT_FILE=.FALSE.
	MAXVAL=1
	CALL JLP_GETPPI(ENTRY,DFAULT,MAXVAL,BDFNAM,ACTVAL,
	1	ALREADY_IN,CONNECT_FILE,USERSUP,IERR1)
	IF (IERR1.NE.0)THEN
	 PRINT *,' JLP_ACCFRM/JLP_GETPPI : Failure to get',
	1	' information on PCT parameters'
	 STATUS=1
	 RETURN
	ENDIF
 
	IF(USERSUP)THEN
	  PRINT *,' JLP_ACCFRM : Filename already in the command line'
	  FILENAME=BDFNAM
	ENDIF
	  PRINT *,' FILENAME = ',BDFNAM
C
C  .....check if bulk data frame type in the connection file
	IF (PCT_TYPE(ENTRY)(1:5).NE.'FRAME')THEN
	 PRINT *,' JLP_ACCFRM : Error in the connection file : '
	 PRINT *,' ',NAME,' must be a frame-type entry'
	 PRINT *,' ',NAME,' must start with "IN" or "OUT"'
	 STATUS=1
	 RETURN
	ENDIF
C
C  .....open bulk data file
	CALL STL_OPNBDF(BDFNAM,PCT_IOCHAN(ENTRY),PCT_FILEID(1,ENTRY),
	1	PCT_DEVICE(ENTRY),PCT_RDONLY(ENTRY),IERR2)
	IF (.NOT.IERR2)THEN
	 PRINT *,' JLP_ACCFRM/STL_OPNBDF : Error opening BDF file'
	 STATUS=1
	ENDIF
C@	CALL STL_SSERR(' FATAL ERROR IN STL_OPNBDF',IERR2)
C
	RETURN
	END
C----------------------------------------------------------------------------
	SUBROUTINE JLP_GETPPI(ENTRY,DFAULT,MAXVAL,CARRAY,ACTVAL,
	1	ALREADY_IN,CONNECT_FILE,USERSUP,STATUS)
C++
C     GETPPI - Get Program Parameter Information.
C
C     This routine serves as an interim interface to the command
C     process. It is used to obtain program parameter information
C     from the user environment.
C
C     An entry is assumed to have been reserved in the Parameter
C     Control Tables (PCT) for the specified parameter; the input
C     argument 'ENTRY' points to this.
C
C     If the entry has null values, (STATE=NULL), then the status
C     is set accordingly and no further operations are performed.
C
C     If the entry is already active, (STATE=ACTIVE), then the values
C     previously stored are returned to the program.
C
C     If the entry has been cancelled, (STATE=CANCEL), then new values
C     are obtained directly from the user.
C
C     If the entry is in its 'ground' state, (STATE=GROUND), then there
C     are three methods by which the parameter values can be obtained:
C
C       (1) The user command line.
C       (2) Connection File defaults.
C       (3) User response to prompts.
C	(4) If ALREADY_IN, direct input, no prompt when (STATE=GROUND)
C
C     The user may have specified the parameter on the command
C     line thus:  ....COORDS=1,1,512,512.... Any parameters
C     supplied in this fashion, will have been written to a buffer
C     by the command process in the form:-
C
C       /name=[value,value...]/name=[value,value...]/.....
C
C     The specified parameter name is searched for in this buffer and,
C     if found, any associated values are assumed to be those required
C     by the applications program.
C
C     The Connection File is then searched, to confirm the entry is
C     valid and to determine the parameter type.
C
C     If the user has not supplied the parameter value(s) on the
C     command line, then the corresponding Connection File entry
C     will be checked for any default value(s) which, if they exist,
C     will be returned to the applications program.
C
C     If no defaults exist, then the user will be prompted for the
C     parameters value(s), which he enters in the form:
C
C        [value][,value][,value].....[,value]
C
C     The user can just hit <return> after the prompt, if he does not
C     wish to supply any value(s) at all. The user can specify a 'null'
C     default value in the connection file by two consecutive slashes.
C
C
C     CALL STL_GETPPI(ENTRY,DFAULT,MAXVAL,CARRAY,ACTVAL,STATUS)
C
C     Input parameters:
C     ----------------
C     CARRAY:  CHARACTER array:  (If already in)
C     ENTRY:   INTEGER expression:
C     DFAULT:  LOGICAL expression:
C     ALREADY_IN:  LOGICAL expression:    Do not prompt the user, the value
C					  is already in.
C     CONNECT_FILE:  LOGICAL expression:    To accept or not default values
C		 			    from the connection file.
C     MAXVAL:  INTEGER expression:
C
C     Output parameters:
C     -----------------
C     CARRAY:  CHARACTER array: (if not.already_in or if user-supplied)
C     ACTVAL:  INTEGER variable:
C     STATUS:  INTEGER variable:      Status return.
C     USERSUP:  LOGICAL expression:    True when the value is found in
C				      the command line (USER-SUPPLIED)
C
C
C     D.PEARCE  14/SEP/80  VERSION #2
C--
	IMPLICIT INTEGER(A-Z)
C
	CHARACTER*(*) CARRAY(*)
	INTEGER ENTRY,MAXVAL,ACTVAL,STATUS
	LOGICAL DFAULT,ALREADY_IN,CONNECT_FILE,USERSUP
C
	CHARACTER*80  BUFFER,PARM(3),VALUES(32)
C
	INCLUDE 'CORE:INTERIM(PCTCOM)'
	INCLUDE 'CORE:INTERIM(ERRPAR)'
 
C Initialization :
	BUFFER=' '
	ACTVAL=0
 
C**********************************************************************
C (PCT_NULL=4)
C .....check for NULL state
	IF (PCT_STATE(ENTRY).EQ.PCT_NULL) THEN
	 STATUS=ERR_PARNUL
	 PRINT *,' JLP_GETPPI : No entry created for this parameter'
	 RETURN
	ENDIF
 
C**********************************************************************
C Parameter already active
C .....check for ACTIVE state: if active, do not go further.
	IF (PCT_STATE(ENTRY).EQ.PCT_ACTIVE) THEN
	 STATUS=0
	 CALL STL_RDPV(PCT_PVADDR(1,ENTRY),MAXVAL,CARRAY,ACTVAL)
	 RETURN
	ENDIF
 
C**********************************************************************
C CANCEL STATE : Changing the value of a parameter
C**********************************************************************
C .....check for CANCEL state
	IF (PCT_STATE(ENTRY).EQ.PCT_CANCEL) THEN
C
C .....get values from direct user-input if not already in
	 IF(ALREADY_IN)THEN
	 ACTVAL=1
	 VALUES(1)=CARRAY(1)
	 ELSE
	 CALL STL_USRINP(CARRAY,MAXVAL,DFAULT,PCT_NAME(ENTRY),
	1	VALUES,ACTVAL)
	 ENDIF
C
C .....process raw values and prepare different things for image arrays.
	 CALL JLP_PROVAL(ENTRY,VALUES,MAXVAL,ACTVAL,CARRAY,STATUS)
	 RETURN
C
	ENDIF
 
C**********************************************************************
C GROUND STATE : Setting the value of a new parameter
C**********************************************************************
C.....check for GROUND state (PCT_GROUND=1)
	IF (PCT_STATE(ENTRY).EQ.PCT_GROUND) THEN
C
C********* First priority : Command Line
C .....Search User Command Buffer for specified name
	 CALL STL_SERUCB(PCT_NAME(ENTRY),BUFFER,ISTAT)
C
C .....set user-supplied flag accordingly
	 USERSUP=(ISTAT.EQ.0)
C
C  .....extract and translate values on command line
	 IF (USERSUP) THEN
	    CALL GEN_EXTRSS(BUFFER,'=',PARM,2,N)
	    CALL STL_TRNVAL(PARM(2))
	    CALL GEN_EXTRSS(PARM(2),',',VALUES,32,ACTVAL)
 	 ENDIF
 
C********* Second priority : Connection file (Getting extra info too)
C  .....Search Program Connection File for specified name
	 IF(CONNECT_FILE)THEN
 
	 CALL STL_SERPCF(PCT_NAME(ENTRY),BUFFER,IERR)
C .....check status
	 IF (IERR.NE.0)THEN
	  PRINT *,' JLP_GETPPI/STL_SERPCF : Error,',
	1	' this entry name is not in the connection file'
	  STATUS=1
	  RETURN
	 ENDIF
 
C .....extract parameter name, type and values list
	 CALL GEN_EXTRSS(BUFFER,'/',PARM,3,N)
 
C .....set parameter type in Parameter Control Tables
C (For instance "FRAME(R)"
	 PCT_TYPE(ENTRY)=PARM(2)
 
C .....if no user-supplied parameters on command line,
C .....look for the default values
	 IF (.NOT.USERSUP) THEN
C .....translate and extract any connection-file defaults
	  CALL STL_TRNVAL(PARM(3))
	  CALL GEN_EXTRSS(PARM(3),',',VALUES,32,ACTVAL)
	 ENDIF
 
	ENDIF
 
C*********** Direct prompting *********
C .....if there aren't any in the command line and
C .....in the connection file, prompt user (unless null default in con.file)
	 IF (.NOT.USERSUP.AND.ACTVAL.EQ.0
	1	.AND.INDEX(BUFFER,'//').EQ.0) THEN
	   IF(ALREADY_IN)THEN
	     ACTVAL=1
	     VALUES(1)=CARRAY(1)
	   ELSE
	     CALL STL_USRINP(CARRAY,MAXVAL,DFAULT,PCT_NAME(ENTRY),
	1	VALUES,ACTVAL)
	   ENDIF
 
	 ENDIF
C
C .....process raw values and prepare different things for image arrays.
	 CALL JLP_PROVAL(ENTRY,VALUES,MAXVAL,ACTVAL,CARRAY,IERR)
	 IF(IERR.NE.0)THEN
	 PRINT *,' JLP_GETPPI/JLP_PROVAL : Error while',
	1	' checking the consistency of the input values'
	 STATUS=3
	 ENDIF
C
	ENDIF
C
	RETURN
	END
C----------------------------------------------------------------------------
	SUBROUTINE JLP_WRIMAG(NAME,FORMAT,DIMENS,NDIM,PNTR,
	1	FILENAME,STATUS)
C++
C     WRIMAG - Write Image Data
C
C     This routine is used to map memory, intended for image
C     output, onto a bulk data frame. The pointer returned will
C     be passed as %val(PNTR) to a lower level routine which
C     can write image data into the array argument.
C
C     A frame that has been accessed by WRIMAG must be released,
C     after processsing, by a call to FRDATA.
C
C     CALL JLP_WRIMAG(NAME,FORMAT,DIMENS,NDIM,PNTR,FILENAME,STATUS)
C
C     Input arguments:
C     ---------------
C     NAME:    CHARACTER expression:	Parameter name of data frame
C     FORMAT:  INTEGER expression:      Data format
C     DIMENS:  INTEGER array:		Array holding dimension sizes
C     NDIM:    INTEGER expression:   	Number of dimensions
C
C     Output arguments:
C     ----------------
C     PNTR:    INTEGER variable:	Pointer to data in memory
C     STATUS:  INTEGER variable:	Status return value
C
C
C     D.PEARCE  30/JUL/80  VERSION #2
C--
C
	IMPLICIT INTEGER(A-Z)
C
	CHARACTER*(*) NAME,FILENAME
	INTEGER*4     FORMAT,DIMENS(*),NDIM,PNTR,STATUS
C
	CHARACTER*20  VALUE
	CHARACTER*6   AXIS
	DATA AXIS/ 'NAXISn' /
C
	INCLUDE 'CORE:INTERIM(ERRPAR)'
C
C .....access bulk data frame
	CALL JLP_ACCFRM(NAME,ENTRY,FILENAME,IERR)
	IF(IERR.NE.0)THEN
	 STATUS=1
	 PRINT *,' JLP_WRIMAG/JLP_ACCFRM : Failure to access BDF file'
	 RETURN
	ENDIF
C
C     This routine is used to map a new bulk data frame into memory
C     and return an address pointer to it.
C .....write frame if accessible
	SIZE=1
	 DO D=1,NDIM
	  SIZE=SIZE*DIMENS(D)
	 ENDDO
	CALL STL_WRFRM(ENTRY,FORMAT,'IMAGE',SIZE,PNTR,STATUS)
	 IF (STATUS.NE.0)THEN
	  PRINT *,' JLP_WRIMAG/STL_WRFRM : Error writing the BDF frame'
	 RETURN
	ENDIF
C
	CALL ITOC(NDIM,VALUE,IND)
	CALL ADDSCR(NAME,'NAXIS',VALUE,1,IND)
C
	 DO D=1,NDIM
	   CALL ITOC(D,AXIS(6:6),IND)
	   CALL ITOC(DIMENS(D),VALUE,IND)
	   CALL ADDSCR(NAME,AXIS,VALUE,1,IND)
	 END DO
 
	RETURN
	END
C----------------------------------------------------------------------------
      SUBROUTINE JLP_CHKBDF(TYPE,BDFNAM,STATUS)
C++
C     CHKBDF - Check Bulk Data Frame
C
C     This routine is used to check the existence of a bulk data file.
C     The connection-file entry is syntax checked and the access mode
C     is determined, (Read or Write). The bulk data filename is then
C     translated to its equivalent VMS file specification. If the file
C     is for input then it is simply opened to validate its existence.
C     If it is for output, then it is created after deletion of any
C     existing file. The Frame Control Block and first Local Descriptor
C     Block are set up and then written to the new file.
C
C
C     CALL JLP_CHKBDF(TYPE,BDFNAM,STATUS)
C
C     Input argument:
C     ---------------
C     TYPE:    CHARACTER expression:	Connection-file entry type
C     BDFNAM:  CHARACTER variable:	Bulk Data File Name. (This
C					parameter is modified)
C
C     Output arguments:
C     -----------------
C     BDFNAM:  CHARACTER variable:	Translated logical name
C     STATUS:  INTEGER variable:	Status return value
C
C
C     D.PEARCE  21/JUL/80  VERSION #2
C--
C
      IMPLICIT      INTEGER(A-Z)
C
      CHARACTER*(*) TYPE,BDFNAM
      INTEGER*4     STATUS
C
      CHARACTER*64  EQVNAM
      INTEGER*2     FILEID(3)
      CHARACTER*8   DEVICE
      CHARACTER*3   MODE
      LOGICAL*4     RDONLY
C
C
      INCLUDE 'CORE:INTERIM(FCBCOM)'
      INCLUDE 'CORE:INTERIM(LDBCOM)'
      INCLUDE 'CORE:INTERIM(ERRPAR)'
C
C
      STATUS=ERR_NORMAL
C
C .....determine access mode
      MODE=TYPE(6:8)
C
C .....validate access-mode
	IF (MODE.NE.'(R)'.AND.MODE.NE.'(W)')THEN
	 PRINT *,' JLP_CHKBDF : Bad connection-file entry '
	ENDIF
C
C .....translate bulk data frame filename
	CALL STL_TRNBDF(BDFNAM)
C
C .....if file is for input, check it already exists
      IF (MODE.EQ.'(R)') THEN
	 CALL STL_OPNBDF(BDFNAM,IOCHAN,FILEID,DEVICE,RDONLY,IOSTAT)
	 IF (.NOT.IOSTAT) STATUS=ERR_FRMNAC
C
C .....else, if output, create a new one and add control blocks
      ELSE
C
C .....delete frame file if it already exists
         CALL STL_DELBDF(BDFNAM,IOSTAT)
	 IF (.NOT.IOSTAT) THEN
	   STATUS=ERR_FRMNAC
	   PRINT *,' JLP_CHKBDF/STL_DELBDF : Error deleting BDF file'
	   RETURN
	 ENDIF
C
C .....set allocation quantity and create frame file
         ALQ=2
         CALL STL_CREBDF(BDFNAM,ALQ,NVB,IOSTAT)
         IF (.NOT.IOSTAT) THEN
	   PRINT *,' JLP_CHKBDF/STL_CREBDF : Error creating BDF file'
	   STATUS=ERR_FRMNAC
	   RETURN
         ENDIF
C
C .....open bulk data frame file for control block initialisation
         CALL STL_OPNBDF(BDFNAM,IOCHAN,FILEID,DEVICE,RDONLY,IOSTAT)
C
C .....FCB initialisation
         FCB_IMPVER=2
         FCB_BDTYPE='        '
         FCB_NDVAL=0
         CALL SYS$ASCTIM(,FCB_CREATE,,)
         FCB_ACCESS=FCB_CREATE
         FCB_MODIFY=FCB_CREATE
         FCB_INCARN=0
         FCB_PTRLDB=2
         FCB_ENDLDB(1)=FCB_PTRLDB
         FCB_ENDLDB(2)=1
         FCB_LEXBDF=2
         FCB_PEXBDF=NVB
         FCB_NEXT=0
C
C .....LDB initialisation
         LDB_BLKNUM=FCB_PTRLDB
         DO N=1,LDB_NDSCRW
	   LDB_DWORD(N)=0
         ENDDO
         LDB_NEXT=0
C
C .....write control blocks to BDF
         CALL STL_WVB(IOCHAN,FCB,1,IOSTAT)
         CALL STL_WVB(IOCHAN,LDB,LDB_BLKNUM,IOSTAT)
C
      ENDIF
C
C .....close Bulk Data File
	IOSTAT=SYS$DASSGN(%val(IOCHAN))
C
	RETURN
      END
C*************************************************************************
      SUBROUTINE JLP_PROVAL(ENTRY,VALUES,MAXVAL,ACTVAL,CARRAY,STATUS)
C++
C     PROVAL - Process Parameter Values
C
C     This routine is used to process program parameters obtained from
C     the user environment. If there are no values supplied, (ACTVAL=0),
C     then a null status is set and no further operations are performed,
C     otherwise, the following occurs:
C
C     (1) If the parameter is a bulk data frame type, then certain oper-
C         -ations need to be performed to prepare the file for subse-
C         -quent processing.
C     (2) A page of memory is dynamically allocated to the PCT entry.
C     (3) The values are then stored in the dynamically allocated memory
C         area.
C     (4) The requested number of values, (MIN(ACTVAL,MAXVAL)), are then
C         copied to the calling program's buffer.
C     (5) The PCT state is set to ACTIVE.
C
C     CALL STL_PROVAL(ENTRY,VALUES,MAXVAL,ACTVAL,CARRAY,STATUS)
C
C     Input arguments:
C     ---------------
C     ENTRY:   INTEGER expression:	PCT entry number
C     VALUES:  CHARACTER array:		Array holding 'raw' values
C     MAXVAL:  INTEGER expression:	Maximum number of values
C     ACTVAL:  INTEGER expression:	Actual number of values
C
C     Output arguments:
C     ----------------
C     CARRAY:  CHARACTER array:		Array to hold 'processed' values
C     STATUS:  INTEGER variable:	Status return value
C
C
C     D.PEARCE  15/SEP/80  VERSION #2
C--
C
      IMPLICIT      INTEGER(A-Z)
C
      INTEGER*4     ENTRY,MAXVAL,ACTVAL,STATUS
      CHARACTER*(*) VALUES(*),CARRAY(*)
C
      INCLUDE 'CORE:INTERIM(PCTCOM)'
      INCLUDE 'CORE:INTERIM(ERRPAR)'
C
C
C     .....set defaults
      STATUS=ERR_NORMAL
      PCT_STATE(ENTRY)=PCT_ACTIVE
C
C     .....check for null values
      IF (ACTVAL.EQ.0) THEN
	 STATUS=ERR_PARNUL
	 PRINT *,' JLP_PROVAL : Actual number of values = 0 !'
	 PCT_STATE(ENTRY)=PCT_NULL
    	 RETURN
      ENDIF
C
C     .....set PCT state
      PCT_STATE(ENTRY)=PCT_ACTIVE
C
C     .....if parameter is frame-type, check bulk data file
      IF (PCT_TYPE(ENTRY)(1:5).EQ.'FRAME') THEN
         CALL JLP_CHKBDF(PCT_TYPE(ENTRY),VALUES(1),STATUS)
         IF (STATUS.NE.ERR_NORMAL) THEN
	 PRINT *,' JLP_PROVAL/JLP_CHCKBDF : Error while checking',
	1	' BDF file'
	 ENDIF
      ENDIF
C
C .....get 512 bytes of virtual memory for values
      CALL LIB$GET_VM(512,PCT_PVADDR(1,ENTRY))
      PCT_PVADDR(2,ENTRY)=PCT_PVADDR(1,ENTRY)+511
C
C .....Write Parameter Values to virtual memory
      CALL STL_WRPV(PCT_PVADDR(1,ENTRY),VALUES,ACTVAL)
C
C .....copy requested number of values to user buffer
      NVAL=MIN(MAXVAL,ACTVAL)
      IF (NVAL.GT.0) THEN
	 DO N=1,NVAL
	    CARRAY(N)=VALUES(N)
	 ENDDO
      ENDIF
C
C .....set status return
	STATUS=0
C
	RETURN
	END
