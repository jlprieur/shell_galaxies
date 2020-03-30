      PROGRAM TEST
C+++
C.IDENTIFICATION
C     PROGRAM TEST
C
C.PURPOSE
C     TEST THE EXTRACTION OF FORTRAN EXTENSIONS FROM A SOURCE FILE
C
C---
      PARAMETER    (MAX_ARRAY=100)
C
      IMPLICIT NONE
      INTEGER  I,N
      REAL     ARRAY(MAX_ARRAY,MAX_ARRAY)
      REAL     THIS_IS_REAL,RESULT

      print *,' This is a test of lower case statement'
C
C     TEST ENDDO
C
      DO I = 1,MAX_ARRAY
	 DO N = 1,MAX_ARRAY
            IF (I.EQ.N) GOTO 200
	    ARRAY(I,N) = I*K
  200    ENDDO
      ENDDO
C
C     INITIATE VARIABLES
C
      RESULT = 0.0E0                    ! RESET RESULT
C
C     NEXT TEST
C
      DO I = 1,MAX_ARRAY
	 DO 100, N = 1,MAX_ARRAY
            THIS_IS_REAL = RESULT + ARRAY(I,N)
  100    CONTINUE
      ENDDO
C
C     CALL SUBROUTINE
C
      CALL DUMMTEST(RESULT)
C
C     END TEST
C
      STOP
      END
C
C
      SUBROUTINE DUMMTEST(X)
      IMPLICIT NONE
      REAL X
C
      X = X * X
C
      RETURN
      END