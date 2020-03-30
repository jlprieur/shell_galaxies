C++*******************************************************
C Program to compute the 90 degrees rotated version of an image
C
C Syntax: RUNS MIRROR INPUT OUTPUT
C
C JLP
C Version of 23-03-90
C--*******************************************************
	PROGRAM ROTATE90 
	INTEGER*4 ISIZE,MADRID(1)
	INTEGER*4 PNTR_IN,PNTR_OUT,NX1,NY1,NX2,NY2
	CHARACTER FILENAME*40,COMMENTS*80
	COMMON /VMR/MADRID

	CALL JLP_BEGIN
	
	WRITE(6,20)
20	FORMAT(' 90degrees rotated version of an image')
	
C Format of the files:
	CALL JLP_INQUIFMT
 
C Input of the image:
        WRITE(6,*) 'Input file: '
        READ(5,10) FILENAME
10      FORMAT(A)
	CALL JLP_VM_READIMAG(PNTR_IN,NX1,NY1,FILENAME,COMMENTS)

C Allocating dynamical memory space:
	ISIZE=NX1*NY1*4
	CALL JLP_GETVM(PNTR_OUT,ISIZE)
 
C Calling the compression subroutine :
	CALL ROTATE_90D(MADRID(PNTR_IN),MADRID(PNTR_OUT),NX1,NY1)
 
C Output of the file :
        WRITE(6,*) 'Output file: '
        READ(5,10) FILENAME
	CALL JLP_WRITEIMAG(MADRID(PNTR_OUT),NY1,NX1,NY1,
     1	FILENAME,COMMENTS)
 
	CALL JLP_END
	END
C---------------------------------------------------------------
C Subroutine ROTATE_90D 
C--------------------------------------------------------------
	SUBROUTINE ROTATE_90D(INPUT,OUTPUT,NX,NY)
	REAL*4 INPUT(NX,NY),OUTPUT(NY,NX)
        INTEGER I,J,I1,J1
 
	DO J=1,NY
	  DO I=1,NX
	    OUTPUT(NY-J,I)=INPUT(I,J)
	  END DO
	END DO
 
	RETURN
	END
