C++------------------------------------------------------------------------
C ARITHM_COLUMNS
C General program to perform arithmethic operations on X,Y lists 
C with/without error bars,
C
C Reads galaxy profiles, ascii X,Y lists, Midas, FITS spectra, etc 
C (by calling READFILE).
C
C JLP 
C Version of 24-03-2010
C--------------------------------------------------------------------------
        PROGRAM ARITHM_COLUMNS
        IMPLICIT NONE
        INTEGER*4 IDIM
        PARAMETER (IDIM=20000)
C
	REAL*4 XX(IDIM),Y1(IDIM),Y2(IDIM),ZZ(IDIM)
        REAL*4 ERRORX(IDIM),ERRORY(IDIM),ERRORZ(IDIM)
        REAL*4 AA,BB,CC
        INTEGER*4 NPT1,NPT2,NPTS,I,IOPT
        CHARACTER IN_FILE*40,IN_COMMENTS*80,ANS*1
	LOGICAL ERROR_BARS,CHECK_INPUT
 
10	FORMAT(A)
        NPTS=0

	PRINT 88
88	FORMAT(5X,' Program ARITHM_COLUMNS',/,
     1    'Version of 24-11-2004',/)
C
	PRINT 89
89	FORMAT(' Do you want me to type the values of the input',
     1	' files ? (N)')
	READ(5,10) ANS
	CHECK_INPUT=.FALSE.
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')CHECK_INPUT=.TRUE.

C Image format (in case of option 8 in READFILE):
        CALL JLP_BEGIN
        CALL JLP_INQUIFMT
C
	ERROR_BARS=.FALSE.
12	WRITE(6,130)
130     FORMAT(' Menu :',/,
     1	' 1. Input of a vector (X,Y1) or (X,Y1,Y2)',/,
     1	' 2. Input of a vector (X,Y,ERRROR_Y) with error bars',/,
     1	5X,' Enter the option you want: ')
	READ(5,*) IOPT

C------------------------------------------------------------------
C Without error bars
	IF(IOPT.EQ.1)THEN
	CALL RREADFILE(XX,Y1,NPT1,XX,Y2,NPT2,IDIM,
     1                 IN_FILE,IN_COMMENTS,0)
	IF((NPT2.NE.0).AND.(NPT1.NE.NPT2))THEN
          WRITE(6,*)' Fatal error: not the same X list'
          STOP
	ENDIF
	IF(CHECK_INPUT)THEN
          IF(NPT2.NE.0)THEN
	    DO I=1,NPT1
	      PRINT *,I,XX(I),Y1(I),Y2(I)
	    END DO
          ELSE
	    DO I=1,NPT1
	      PRINT *,I,XX(I),Y1(I)
	    END DO
          ENDIF
	ENDIF
 
	ERROR_BARS=.FALSE.
        NPTS=NPT1

C------------------------------------------------------------------
C Input with error bars
	ELSEIF(IOPT.EQ.2)THEN
 
	ERROR_BARS=.TRUE.
 
C Reading XX, Y1, Error_X, Error_Y 
	CALL RREADFILE_ERRORS(XX,Y1,ERRORX,ERRORY,NPT1,IDIM,
     1                 IN_FILE,IN_COMMENTS,2)
 
	 IF(CHECK_INPUT)THEN
	   DO I=1,NPT1
	     PRINT 508,I,XX(I),Y1(I),ERRORX(I),ERRORY(I)
	   END DO
	 ENDIF
508	FORMAT(' I=',I4,'X,Y,ERROR X,ERROR Y:',4(F12.3,1X))
 
	ERROR_BARS=.TRUE.
        NPT2=0
        NPTS=NPT1
	ENDIF
C------------------------------------------------------------------
        IF(NPTS.EQ.0)THEN
          WRITE(6,*) 'No curves: Exit'
          STOP
        ENDIF

C------------------------------------------------------------------
        IF(NPT2.NE.0)THEN
	WRITE(6,131)
131     FORMAT(' Menu :',/,
     1  ' 1. X1 := X1 + A and Y1 := Y1 + B and  Y2 := Y2 + C',/,
     1  '                         (sortie: X1, Y1, Y2)',/,
     1	' 2. Z = A * (Y2/Y1) + B  (sortie: X1,Z)',/,
     1	' 3. Z = A * (Y1/Y2) + B  (sortie: X1,Z)',/,
     1	5X,' Enter the option you want: ')
	READ(5,*) IOPT

C------------------------------------------------------------------
	IF(IOPT.EQ.1)THEN
          WRITE(6,*)' Enter A, B, C:'
          READ(5,*) AA,BB,CC
          CALL ARITH_XY1Y2(XX,Y1,Y2,NPTS,AA,BB,CC)
	ELSE IF(IOPT.EQ.2)THEN
          WRITE(6,*)' Enter A, B:'
          READ(5,*) AA,BB
          CALL ARITH1(Y2,Y1,ZZ,NPTS,AA,BB)
        ELSEIF(IOPT.EQ.3)THEN
          WRITE(6,*)' Enter A, B:'
          READ(5,*) AA,BB
          CALL ARITH1(Y1,Y2,ZZ,NPTS,AA,BB)
        ENDIF

        ELSE
 	  WRITE(6,132)
132       FORMAT(' Menu :',/,
     1	' 2. Z = A * Y1 + B  (sortie: X1, Z)',/,
     1	5X,' Enter the option you want: ')
	  READ(5,*) IOPT
C------------------------------------------------------------------
	IF(IOPT.EQ.2)THEN
          WRITE(6,*)' Enter A, B: '
          READ(5,*) AA,BB
          CALL ARITH2(Y1,ZZ,ERRORY,ERRORZ,ERROR_BARS,NPTS,
     1                AA,BB)
        ENDIF


        ENDIF
C------------------------------------------------------------------
        IF(IOPT.EQ.1)THEN
          CALL OUTPUT_CURVE_XYY(XX,Y1,Y2,NPTS)
        ELSEIF(IOPT.EQ.2.OR.IOPT.EQ.3) THEN
          CALL OUTPUT_CURVE(XX,ZZ,ERRORX,ERRORZ,ERROR_BARS,NPTS)
        ENDIF
C------------------------------------------------------------------
        CALL JLP_END
        STOP
	END
C********************************************************************
C Subroutine performing arithmetic operations on the curves
C********************************************************************
        SUBROUTINE ARITH_XY1Y2(XX,Y1,Y2,NPTS,AA,BB,CC)
        IMPLICIT NONE
	REAL*4 XX(*),Y1(*),Y2(*)
        REAL*4 AA,BB,CC
        INTEGER*4 I,NPTS
 
        DO I=1,NPTS
           XX(I)=XX(I)+AA
           Y1(I)=Y1(I)+BB
           Y2(I)=Y2(I)+CC
        END DO

        RETURN
        END
C********************************************************************
C Subroutine performing arithmetic operations on the curves
C without error_bars 
C********************************************************************
	SUBROUTINE ARITH1(Y1,Y2,ZZ,NPTS,AA,BB)
        IMPLICIT NONE
	REAL*4 Y1(*),Y2(*),ZZ(*)
        REAL*4 AA,BB
        INTEGER*4 I,NPTS
 
	DO I=1,NPTS
          IF(Y2(I).NE.0)THEN
	    ZZ(I)=AA*Y1(I)/Y2(I)+BB
          ELSE
            ZZ(I)=1.e+9
          ENDIF
	END DO

	RETURN
	END
C********************************************************************
C Subroutine performing arithmetic operations on the curves
C with error_bars 
C********************************************************************
	SUBROUTINE ARITH2(Y1,ZZ,ERRORY,ERRORZ,ERROR_BARS,
     1                    NPTS,AA,BB)
        IMPLICIT NONE
	REAL*4 Y1(*),ZZ(*),ERRORY(*),ERRORZ(*)
        REAL*4 AA,BB
        INTEGER*4 I,NPTS
	LOGICAL ERROR_BARS
 
C
	DO I=1,NPTS
	  ZZ(I)=AA*Y1(I)+BB
	END DO

C Errors:
        IF(ERROR_BARS)THEN
	  DO I=1,NPTS
	    ERRORZ(I)=AA*ERRORY(I)
	  END DO
        ENDIF
 
	RETURN
	END

C********************************************************************
C Subroutine to output a curve from XPLOT,YPLOT
C********************************************************************
	SUBROUTINE OUTPUT_CURVE(XX,ZZ,ERRORX,ERRORZ,ERROR_BARS,NPTS)
        IMPLICIT NONE
	REAL*4 XX(*),ZZ(*),ERRORX(*),ERRORZ(*)
        INTEGER I,NPTS
	CHARACTER NAME*60
	LOGICAL ERROR_BARS
 
10	FORMAT(A)
 
807	PRINT *,' NAME OF THE OUTPUT FILE ?'
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
         IF(ERROR_BARS)THEN
	   DO I=1,NPTS
	     WRITE(1,*)XX(I),ZZ(I),ERRORX(I),ERRORZ(I)
	   END DO
         ELSE
	   DO I=1,NPTS
	     WRITE(1,*)XX(I),ZZ(I)
	   END DO
         ENDIF
	CLOSE(1)
 
	RETURN
	END

C********************************************************************
C Subroutine to output a curve from XPLOT,YPLOT
C********************************************************************
        SUBROUTINE OUTPUT_CURVE_XYY(XX,Y1,Y2,NPTS)
        IMPLICIT NONE
	REAL*4 XX(*),Y1(*),Y2(*)
        INTEGER I,NPTS
        CHARACTER NAME*60
 
10      FORMAT(A)
 
897     PRINT *,' NAME OF THE OUTPUT FILE ?'
        READ(5,10) NAME
        OPEN(1,FILE=NAME,STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
           DO I=1,NPTS
               WRITE(1,*)XX(I),Y1(I),Y2(I)
           END DO
        CLOSE(1)
 
        RETURN
        END
