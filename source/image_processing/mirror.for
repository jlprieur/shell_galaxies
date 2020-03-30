C++*******************************************************
C Program to compute the mirror version X/Y of an image
C
C Syntax: RUNS MIRROR INPUT OUTPUT
C
C JLP
C Version of 23-03-90
C--*******************************************************
	PROGRAM MIRROR 
	INTEGER*4 ISIZE,MADRID(1)
	INTEGER*4 PNTR_IN,PNTR_OUT,TEST,NX1,NY1,NX2,NY2,IOPT
	CHARACTER FILENAME*40,COMMENTS*80
	COMMON /VMR/MADRID

	CALL JLP_BEGIN
	
	WRITE(6,20)
20	FORMAT(' 1. X/Y mirror version of an image',/,
     1         ' 2. X mirror',/,
     1         ' 3. Y mirror',/,
     1         ' 4. Rotation of 180 degrees',/,
     1         ' 10. Exit',/,
     1         ' Enter your choice: ')
        READ(5,21) IOPT
21      FORMAT(I2)

C Exit if invalid option:
        IF(IOPT.LE.0.OR.IOPT.GT.4) THEN 
          WRITE(6,*)' OK, exit without any processing'
          CALL JLP_END
          STOP
        ENDIF
	
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
 
C Calling the XY mirror subroutine :
        IF(IOPT.EQ.1)THEN
	  CALL XY_MIRROR(MADRID(PNTR_IN),MADRID(PNTR_OUT),NX1,NY1)
          NX2=NY1
          NY2=NX1
        ELSEIF(IOPT.EQ.2)THEN
	  CALL X_MIRROR(MADRID(PNTR_IN),MADRID(PNTR_OUT),NX1,NY1)
          NX2=NX1
          NY2=NY1
        ELSEIF(IOPT.EQ.3)THEN
	  CALL Y_MIRROR(MADRID(PNTR_IN),MADRID(PNTR_OUT),NX1,NY1)
          NX2=NX1
          NY2=NY1
C Rotation of 180 degrees:
        ELSEIF(IOPT.EQ.4)THEN
	  ISIZE=NX1*NY1*4
          CALL JLP_GETVM(TEST,ISIZE)
	  CALL X_MIRROR(MADRID(PNTR_IN),MADRID(TEST),NX1,NY1)
	  CALL Y_MIRROR(MADRID(TEST),MADRID(PNTR_OUT),NX1,NY1)
          CALL JLP_FREEVM(TEST,ISIZE)
          NX2=NX1
          NY2=NY1
        ENDIF
 
C Output of the file :
        WRITE(6,*) 'Output file: '
        READ(5,10) FILENAME
	CALL JLP_WRITEIMAG(MADRID(PNTR_OUT),NX2,NY2,NX2,
     1	FILENAME,COMMENTS)
 
	CALL JLP_END
	END
C---------------------------------------------------------------
C Subroutine XY_MIRROR
C--------------------------------------------------------------
	SUBROUTINE XY_MIRROR(INPUT,OUTPUT,NX1,NY1)
	REAL*4 INPUT(NX1,NY1),OUTPUT(NY1,NX1)
        INTEGER I,J
 
	DO J=1,NY1
	  DO I=1,NX1
	    OUTPUT(J,I)=INPUT(I,J)
	  END DO
	END DO
 
	RETURN
	END
C---------------------------------------------------------------
C Subroutine X_MIRROR
C--------------------------------------------------------------
	SUBROUTINE X_MIRROR(INPUT,OUTPUT,NX1,NY1)
	REAL*4 INPUT(NX1,NY1),OUTPUT(NX1,NY1)
        INTEGER I,J
 
	DO J=1,NY1
	  DO I=1,NX1
	    OUTPUT(I,J)=INPUT(NX1-I+1,J)
	  END DO
	END DO
 
	RETURN
	END
C---------------------------------------------------------------
C Subroutine Y_MIRROR
C--------------------------------------------------------------
	SUBROUTINE Y_MIRROR(INPUT,OUTPUT,NX1,NY1)
	REAL*4 INPUT(NX1,NY1),OUTPUT(NX1,NY1)
        INTEGER I,J
 
	DO J=1,NY1
	  DO I=1,NX1
	    OUTPUT(I,J)=INPUT(I,NY1-J+1)
	  END DO
	END DO
 
	RETURN
	END
