C++***************************************************************
C Program to change the format of an image file:
C      integer, or real CCD files, integer or real BDF files,
C      MIDAS, or CDCA
C
C Syntax:
C       RUNS CONVERT_FORMAT FTM_IN,FTM_OUT IN OUT
C Exemple:
C       RUNS CONVERT_FORMAT 2,5 TEST1.STAR TEST2
C
C Code for the format (FMT_IN or FMT_OUT):
C   1 = Integer*2 Starlink BDF
C   2 = Real*4 Starlink BDF
C   3 = Integer*2 CCD Toulouse (Fortran direct access)
C   4 = Real*4 CCD Toulouse (Fortran direct access)
C   5 = Real*4 Midas BDF
C   6 = Integer*2 CDCA (Fortran direct access, fixed length reccords)
C   7 = Integer*2 FITS 
C   8 = Real*4 FITS 
C   9 = Karim's format
C
C JLP
C Version of 23-07-90
C--***************************************************************
	PROGRAM CONVERT_FORMAT
	INTEGER*4 PNTR_IMAGE,MADRID(1)
	INTEGER NX,NY,IFMT_IN,IFMT_OUT
	CHARACTER SYMBOL*20,BUFFER*40,NAME*40,COMMENTS*80
	COMMON /VMR/MADRID
 
C To get the possibility of command line
C	CALL JLP_BEGIN
C 
        WRITE (6,45)
45	FORMAT(' Program CONVERT_FORMAT, Version 21-10-92')

C Inquire the format (input/output) :
	CALL JLP_FORMAT(0,0)

C Input :
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
10      FORMAT(A)
	  CALL JLP_VM_READIMAG(PNTR_IMAGE,NX,NY,NAME,COMMENTS)
 
C Output :
        WRITE(6,*) 'Output file: '
        READ(5,10) NAME
        PRINT *,' Comments: ',COMMENTS
	  CALL JLP_WRITEIMAG(MADRID(PNTR_IMAGE),NX,NY,NX,
     1	NAME,COMMENTS)
 
	CALL JLP_END
	STOP
	END
C
