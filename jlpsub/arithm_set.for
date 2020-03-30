C++------------------------------------------------------------------
C Set of subroutines to perform arithmetic operations
C Contains :
C  ADD_FILE, SUB_FILE, C_MULT, DIV_FILE, MUL_FILE
C
C JLP  
C Version of 26-01-94
C--------------------------------------------------------------------
C ADD_FILE
C Subroutine to add 2 files
C------------------------------------------------------------------
	SUBROUTINE ADD_FILE(INPUT1,INPUT2,OUTPUT,NX,NY,IDIM)
	REAL*4 INPUT1(IDIM,*),INPUT2(IDIM,*),OUTPUT(IDIM,*)
        INTEGER*4 I,J,NX,NY
 
	DO J=1,NY
	  DO I=1,NX
	    OUTPUT(I,J)=INPUT1(I,J)+INPUT2(I,J)
	  END DO
	END DO
 
	RETURN
	END
 
C------------------------------------------------------------------
C SUB_FILE
C Subroutine to subtract 2 files
C------------------------------------------------------------------
	SUBROUTINE SUB_FILE(INPUT1,INPUT2,OUTPUT,NX,NY,IDIM)
	REAL*4 INPUT1(IDIM,*),INPUT2(IDIM,*),OUTPUT(IDIM,*)
        INTEGER*4 I,J,NX,NY
 
	DO J=1,NY
	  DO I=1,NX
	    OUTPUT(I,J)=INPUT1(I,J)-INPUT2(I,J)
	  END DO
	END DO
 
	RETURN
	END
 
C------------------------------------------------------------------
C C_MUL
C Subroutine to multiply an image with a constant
C------------------------------------------------------------------
	SUBROUTINE C_MUL(INPUT,OUTPUT,NX,NY,IDIM,CT1,CT2)
	REAL*4 INPUT(IDIM,*),OUTPUT(IDIM,*)
        INTEGER*4 I,J,NX,NY
 
	DO J=1,NY
	  DO I=1,NX
	    OUTPUT(I,J)=(INPUT(I,J)-CT1)*CT2
	  END DO
	END DO
 
	RETURN
	END
 
C------------------------------------------------------------------
C MUL_FILE
C Subroutine to multiply 2 files
C------------------------------------------------------------------
	SUBROUTINE MUL_FILE(INPUT1,INPUT2,OUTPUT,NX,NY,IDIM,SKY1,SKY2)
	REAL*4 INPUT1(IDIM,*),INPUT2(IDIM,*),OUTPUT(IDIM,*)
        REAL*4 SKY1,SKY2
        INTEGER*4 I,J,NX,NY
 
	DO J=1,NY
	  DO I=1,NX
	    OUTPUT(I,J)=(INPUT1(I,J)-SKY1)*(INPUT2(I,J)-SKY2)
	  END DO
	END DO
 
	RETURN
	END
C------------------------------------------------------------------
C DIV_FILE
C Subroutine to divide 2 files
C------------------------------------------------------------------
	SUBROUTINE DIV_FILE(INPUT1,INPUT2,OUTPUT,
	1	NX,NY,IDIM,SKY1,SKY2)
	REAL*4 INPUT1(IDIM,*),INPUT2(IDIM,*),OUTPUT(IDIM,*)
        REAL*4 SKY1,SKY2,WORK2
        INTEGER*4 I,J,NX,NY,INULL
	INULL=0
 
	DO J=1,NY
	  DO I=1,NX
	   WORK2=(INPUT2(I,J)-SKY2)
	     IF(WORK2.NE.0.)THEN
	       OUTPUT(I,J)=(INPUT1(I,J)-SKY1)/WORK2
	     ELSE
	       OUTPUT(I,J)=0.
	       INULL=INULL+1
	     ENDIF
	  END DO
	END DO
 
	WRITE(6,*) ' DIV_FILE/Number of zeros in file2 :',INULL
 
	RETURN
	END
