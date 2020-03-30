C++*******************************************************
C Program to dilate an image by a factor 2
C with dynamical allocation of memory
C (opposite of COMPRESS)
C
C Syntax: RUNS DILATE FMT_IN,FMT_OUT INPUT OUTPUT
C
C JLP
C Version of 13-06-93
C--*******************************************************
	PROGRAM DILATE 
	INTEGER*4 ISIZE,MADRID(1)
	INTEGER*4 PNTR_IN,PNTR_OUT,NX1,NY1,NX2,NY2
	CHARACTER FILENAME*40,COMMENTS*80
	COMMON /VMR/MADRID

	CALL JLP_BEGIN
	
	WRITE(6,20)
20	FORMAT(' Dilatation of an image by a factor of 2')
	
C Format of the files:
	CALL JLP_INQUIFMT
 
C Input of the image:
        WRITE(6,*) 'Input file: '
        READ(5,10) FILENAME
	CALL JLP_VM_READIMAG(PNTR_IN,NX1,NY1,FILENAME,COMMENTS)

C Allocating dynamical memory space:
	NX2=NX1*2
	NY2=NY1*2
	ISIZE=NX2*NY2*4
	CALL JLP_GETVM(PNTR_OUT,ISIZE)
 
C Calling the dilatation subroutine :
	CALL DILATE_FILE(MADRID(PNTR_IN),MADRID(PNTR_OUT),
     1	NX1,NY1,NX2,NY2)
 
C Output of the file :
        WRITE(6,*) 'Output file: '
        READ(5,10) FILENAME
	CALL JLP_WRITEIMAG(MADRID(PNTR_OUT),NX2,NY2,NX2,
     1	FILENAME,COMMENTS)
 
	CALL JLP_END
	END
C---------------------------------------------------------------
C Subroutine DILATE_FILE
C--------------------------------------------------------------
	SUBROUTINE DILATE_FILE(INPUT,OUTPUT,NX1,NY1,NX2,NY2)
	REAL*4 INPUT(NX1,NY1),OUTPUT(NX2,NY2)
 
	DO J2=1,NY2
	  DO I2=1,NX2
	    I1=I2/2+1
	    J1=J2/2+1
	    OUTPUT(I2,J2)=INPUT(I1,J1)
	  END DO
	END DO
 
	RETURN
	END
