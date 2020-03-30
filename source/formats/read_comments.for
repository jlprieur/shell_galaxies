C++***************************************************************
C Program to read comments of a 2D image file 
C Wild card is allowed (works with the whole directory)
C
C Syntax:
C       RUNS READ_COMMENTS infile
C Example:
C       RUNS WRITE_COMMENTS "modsq*.mt"
C
C When using a star * in the command line, please note that 
C the double quote " " is necessary with IBM...
C
C JLP
C Version of 27-05-97
C--***************************************************************
	PROGRAM READ_COMMENTS 
	INTEGER PNTR_IMAGE,NFILES,MADRID(1)
	INTEGER NX,NY,IFMT_IN,IFMT_OUT,QUIET
	CHARACTER NAME*60,COMMENTS*80
        CHARACTER INFRAM(100)*60
	COMMON /VMR/MADRID
C Common block with JLP_DIRECTORY:
        COMMON/JLP_DIRECT/INFRAM

10	FORMAT(A)
 
C To get the possibility of command line
	CALL JLP_BEGIN
 
C First displays program name:
        WRITE(6,45)
45      FORMAT(' Program READ_COMMENTS Version 27-05-97',/,
     1  ' (Please note that you have to use double quotes like "test*.bdf"'
     1  ,/,' when using command mode on IBM)')

C Inquire the format (input/output):
	CALL JLP_INQUIFMT
 
C Prompts the file name:
        WRITE(6,46)
46      FORMAT(' Enter filename (wild cards like *.mt allowed)')
        READ(5,10) NAME
        QUIET=1
        CALL JLP_DIRECTORY(NAME,NFILES,QUIET)
        IF(NFILES.GT.1) WRITE(6,101) NFILES
101     FORMAT(' OK: ',I5,' file(s) found',/) 

C Main loop: 
        DO I=1,NFILES
          CALL JLP_VM_READIMAG(PNTR_IMAGE,NX,NY,INFRAM(I),COMMENTS)
 
C Displays comments (not necessary since JLP_VM_READIMAG displays this
C information automatically...):
C          WRITE(6,102) INFRAM(I)(1:40),COMMENTS(1:80)
C102	  FORMAT('File:',A40,5X,' Comments: ',A80)

C Frees memory:
          ISIZE=NX*NY*4
          CALL JLP_FREEVM(PNTR_IMAGE,ISIZE)
        END DO

	CALL JLP_END
	STOP
	END
C-----------------------------------------------------------------------
