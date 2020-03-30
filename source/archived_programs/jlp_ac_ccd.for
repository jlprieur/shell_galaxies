C++**********************************************************
C Set of subroutines to access CCD direct access files (Toulouse format)
C
C Contains JLP_RDCCD, JLP_RDICCD, JLP_WRCCD, JLP_WRICCD,
C 	JLP_VM_RDCCD, JLP_VM_RDICCD, JLP_VM_RDCCD1, JLP_VM_RDICCD1
C
C JLP
C Version of 23-03-90
        PROGRAM TEST_READCCD
        PARAMETER(IDIM=600)
        REAL*4 IMAGE1(IDIM,IDIM)
        CHARACTER FILENAME*60,COMMENTS*80,IDENT*80

        WRITE(6,*) ' Enter image name:'
        READ(5,10) FILENAME
10      FORMAT(A)
        CALL JLP_RDCCD(IMAGE1,NX,NY,IDIM,FILENAME,
	1       COMMENTS,ISTAT)
        END
C--*********************************************************
C Subroutine to read real CCD direct access format files
C*********************************************************
        SUBROUTINE JLP_RDCCD(IMAGE1,NX,NY,IDIM,FILENAME,
	1       COMMENTS,ISTAT)
        REAL*4 IMAGE1(IDIM,*)
        INTEGER*2 NPL,NL
        CHARACTER FILENAME*(*),COMMENTS*(*),IDENT*80
 
        ISTAT=0
        OPEN(19,FILE=FILENAME,STATUS='OLD',ACCESS='DIRECT',
	1       ERR=15)
 
C Warning : Integer*2 for the header !!!!
        READ(19'1)IDENT,NPL,NL
        NX=NPL
        NY=NL
        PRINT 30,IDENT,NX,NY
30      FORMAT(' IDENTIFICATION : ',A,/,
	1       ' NX =',I5,' NY =',I5,/)
 
C Input :
        DO J=1,NY
        READ(19'J+1) (IMAGE1(I,J),I=1,NX)
        END DO
 
C Remember : the first character is 'R' or 'I'
        PRINT *,' IDENT(1:1) :    ',IDENT(1:1)
        COMMENTS=' '
        COMMENTS(1:79)=IDENT(2:80)
 
        CLOSE(19)
        RETURN
 
C ISTAT non null if error when reading the file
15      ISTAT=1
        RETURN
        END
C*********************************************************
C Subroutine to read integer CCD direct access format files
C*********************************************************
        SUBROUTINE JLP_RDICCD(IMAGE1,NX,NY,IDIM,FILENAME,
	1       COMMENTS,ISTAT)
        REAL*4 IMAGE1(IDIM,*)
        INTEGER*2 IBUFF(1200),NPL,NL
        CHARACTER FILENAME*(*),COMMENTS*(*),IDENT*80
10      FORMAT(A)
        ISTAT=0
 
        OPEN(19,FILE=FILENAME,STATUS='OLD',ACCESS='DIRECT',
	1       ERR=15)
 
C Warning : Integer*2 for the header !!!!
        READ(19'1)IDENT,NPL,NL
        NX=NPL
        NY=NL
C Remember : the first character is 'R' or 'I'
        PRINT *,' IDENT(1:1) :    ',IDENT(1:1)
        PRINT *,' NX,NY',NX,NY
        COMMENTS=' '
        COMMENTS(1:79)=IDENT(2:80)
 
C Input :
        DO J=1,NY
        READ(19'J+1,ERR=15) (IBUFF(I),I=1,NX)
         DO I=1,NX
          IMAGE1(I,J)=FLOATI(IBUFF(I))
         END DO
        END DO
 
 
        CLOSE(19)
        RETURN
 
C ISTAT non null if error when reading the file
15      ISTAT=1
        RETURN
 
        END
 
C*********************************************************
C Subroutine to write CCD direct access format files
C*********************************************************
        SUBROUTINE JLP_WRCCD(IMAGE1,NX,NY,IDIM,FILENAME,
	1       COMMENTS,ISTAT)
        REAL*4 IMAGE1(IDIM,*)
        INTEGER*4 NX,NY,IDIM,ISTAT
        INTEGER*2 NL,NPL
        CHARACTER FILENAME*(*),IDENT*80,COMMENTS*(*)
        ISTAT=0
 
        IDENT(1:2)='R '
        IDENT(3:80)=COMMENTS(1:78)
        INISIZE=NX*NY/128
        OPEN(19,FILE=FILENAME,STATUS='NEW',ACCESS='DIRECT',
	1       RECL=NX,INITIALSIZE=INISIZE,ERR=15)
 
C Warning : integer*2 for the header !!!!
        NPL=NX
        NL=NY
 
C Errors accessing this file in Mireille's programs
C can be caused by her convention of declaring IDENT*80 :
        IF(NX.LT.80)THEN
          PRINT *,' WARNING : NX < 80 ',
	1       ' Problems may occur when reading these files...'
          NMAX=MIN(NX,80)
        ELSE
          NMAX=80
        ENDIF
 
        WRITE(19'1)IDENT(1:NMAX),NPL,NL
          DO J=1,NY
          WRITE(19'J+1) (IMAGE1(I,J),I=1,NX)
          END DO
        CLOSE(19)
        RETURN
 
C ISTAT non null if error when writing the file
15      ISTAT=1
        RETURN
        END
 
C*********************************************************
C Subroutine to write integer CCD direct access format files
C*********************************************************
        SUBROUTINE JLP_WRICCD(IMAGE1,NX,NY,IDIM,FILENAME,
	1       COMMENTS,ISTAT)
        REAL*4 IMAGE1(IDIM,*)
        INTEGER*2 IBUFF(1200),NPL,NL
        CHARACTER FILENAME*(*),IDENT*80,COMMENTS*(*)
        ISTAT=0
 
 
        INISIZE=NX*NY/256
C Warning : NREC in 4-byte units !!!!!
        NREC=(NX+1)/2
        OPEN(19,FILE=FILENAME,STATUS='NEW',ACCESS='DIRECT',
	1       RECL=NREC,INITIALSIZE=INISIZE,ERR=15)
 
C Header :
        IDENT(1:2)='I '
        IDENT(3:80)=COMMENTS(1:78)
 
C Warning : Integer*2 for the header !!!!
        NPL=NX
        NL=NY
        IF(NREC.LT.80)THEN
          PRINT *,' WARNING : NREC < 80 ',
	1       ' Problems may occur when reading these files...'
          NMAX=MIN(NREC,80)
        ELSE
          NMAX=80
        ENDIF
 
        WRITE(19'1)IDENT(1:NMAX),NPL,NL
 
C Conversion in integer*2 before output :
        DO J=1,NY
          DO I=1,NX
           WORK=IMAGE1(I,J)
           IF(WORK.LT.-32000.)WORK=-32000.
           IF(WORK.GT.32000.)WORK=32000.
           IBUFF(I)=NINT(WORK)
          END DO
        WRITE(19'J+1) (IBUFF(I),I=1,NX)
        END DO
 
        CLOSE(19)
        RETURN
 
C ISTAT non null if error when writing the file
15      ISTAT=1
        RETURN
        END
C*********************************************************
C Subroutine JLP_VM_RDCCD ---> pointer of IMAGE(NX,NY)
C To read real CCD direct access format files
C----------------------------------------------------------
        SUBROUTINE JLP_VM_RDCCD(PNTR_IMAGE,NX,NY,FILENAME,
	1       COMMENTS,ISTAT)
        INTEGER*4 NX,NY,ISTAT
	INTEGER*4 PNTR_IMAGE,MADRID(1),ISIZE
        INTEGER*2 NPL,NL
        CHARACTER*(*) FILENAME,COMMENTS
        CHARACTER IDENT*80
	COMMON /VMR/MADRID
 
10      FORMAT(A)
        ISTAT=0
 
        OPEN(1,FILE=FILENAME,STATUS='OLD',ACCESS='DIRECT',
	1       ERR=99)
 
C Warning : Integer*2 for the header !!!!
        READ(1'1)IDENT,NPL,NL
        NX=NPL
        NY=NL
        COMMENTS=IDENT
 
C Getting memory space for the array "IMAGE":
	ISIZE=4*NX*NY
        CALL JLP_GETVM(PNTR_IMAGE,ISIZE)
 
C Copying the input file to the array "IMAGE"
        CALL JLP_VM_RDCCD1(MADRID(PNTR_IMAGE),NX,NY)
 
        CLOSE(1)
        RETURN
 
C Return if error:
99      ISTAT=2
        RETURN
 
        END
C---------------------------------------------------------------
        SUBROUTINE JLP_VM_RDCCD1(A,NX,NY)
        REAL*4 A(NX,NY)
C Input :
        DO J=1,NY
        READ(1'J+1) (A(I,J),I=1,NX)
        END DO
 
        RETURN
        END
C*********************************************************
C Subroutine to read integer CCD direct access format files
C*********************************************************
        SUBROUTINE JLP_VM_RDICCD(PNTR_IMAGE,NX,NY,FILENAME,
	1       COMMENTS,ISTAT)
        INTEGER*4 NX,NY,ISTAT
        INTEGER*2 NPL,NL
	INTEGER*4 MADRID(1),PNTR_IMAGE,ISIZE
        CHARACTER*(*) FILENAME,COMMENTS
        CHARACTER IDENT*80
	COMMON /VMR/MADRID
 
10      FORMAT(A)
        ISTAT=0
 
        OPEN(1,FILE=FILENAME,STATUS='OLD',ACCESS='DIRECT',
	1       ERR=99)
 
C Warning : Integer*2 for the header !!!!
        READ(1'1)IDENT,NPL,NL
        NX=NPL
        NY=NL
        COMMENTS=IDENT
 
C Getting memory space for the array "IMAGE":
	ISIZE=4*NX*NY
        CALL JLP_GETVM(PNTR_IMAGE,ISIZE)
 
C JLP 93: Debug:
        PRINT *,'NX,NY,ISIZE',NX,NY,ISIZE

C Copying the input file to the array "IMAGE"
        CALL JLP_VM_RDICCD1(MADRID(PNTR_IMAGE),NX,NY)
 
        CLOSE(1)
        RETURN
 
C Return if error
99      ISTAT=2
        RETURN
        END
C---------------------------------------------------------------
        SUBROUTINE JLP_VM_RDICCD1(A,NX,NY)
        INTEGER*2 IWORK(1024)
        REAL*4 A(NX,NY)
 
        DO J=1,NY
        READ(1'J+1) (IWORK(I),I=1,NX)
         DO I=1,NX
           A(I,J)=FLOAT(IWORK(I))
         END DO
        END DO
 
        RETURN
        END
