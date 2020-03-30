C++***************************************************************
C Program to compute the square of each pixel value of an image:
C
C Syntax:
C       RUNS SQUARE_IMAGE IN OUT
C Exemple:
C       RUNS SQUARE_IMAGE TEST SQRT_TEST 
C
C JLP
C Version of 08-01-93
C--***************************************************************
	PROGRAM SQUARE_IMAGE 
	INTEGER*4 PNTR_IMAGE,MADRID(1)
	INTEGER NX,NY,IFMT_IN,IFMT_OUT
	CHARACTER SYMBOL*20,BUFFER*40,NAME*40,COMMENTS*80
	COMMON /VMR/MADRID
 
C To get the possibility of command line
	CALL JLP_BEGIN
C 
        WRITE (6,45)
45	FORMAT(' Program SQUARE_IMAGE, Version 08-01-93')

C Inquire the format (input/output) :
	CALL JLP_INQUIFMT

C Input :
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
	CALL JLP_VM_READIMAG(PNTR_IMAGE,NX,NY,NAME,COMMENTS)
 
C Compute square root:
	CALL SQUARE_IMA(MADRID(PNTR_IMAGE),MADRID(PNTR_IMAGE),NX,NY,NX)

C Output :
        WRITE(6,*) 'Output file: '
        READ(5,10) NAME
        PRINT *,' Comments: ',COMMENTS
	CALL JLP_WRITEIMAG(MADRID(PNTR_IMAGE),NX,NY,NX,
     1	NAME,COMMENTS)
 
	CALL JLP_END
	STOP
	END
C***************************************************************
C
C***************************************************************
	SUBROUTINE SQUARE_IMA(IN,OUT,NX,NY,IDIM)
        INTEGER*4 NX,NY,IDIM,IX,IY
	REAL*4 IN(IDIM,*),OUT(IDIM,*),WORK

C Main loop:
        DO IY=1,NY
          DO IX=1,NX
             OUT(IX,IY)=IN(IX,IY)*IN(IX,IY)
          END DO
        END DO

	RETURN
	END
