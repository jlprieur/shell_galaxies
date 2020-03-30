C+++********************************************************************
C Set of subroutines to read 1-D files
C Called by readfile1.for (split into two to avoid compilation problems)
C 
C JLP
C Version of 22-03-2012
C+++********************************************************************
C+++********************************************************************
C Subroutine RREADFILE(X1,Y1,NPTS1,X2,Y2,NPTS2,IDIM,IOPTION)
C Possibility of entering the option
C REAL output arrays
C Format of the input files:
C 1. 2 columns X, Y
C 2. 3 columns X, Y1, Y2
C 3. PROFILE1
C 4. NPOINTS, followed by 2 columns X, Y
C 5. 2 curves NPOINTS,XY
C 6. LaTeX table 
C 7. MIDAS Table
C 8. Image file (Midas, FITS, etc) 
C---********************************************************************
	SUBROUTINE RREADFILE(X1,Y1,NPTS1,X2,Y2,NPTS2,IDIM,
     1                       NAME,COMMENTS,IOPTION)
	REAL*4 X1(*),Y1(*),X2(*),Y2(*)
        INTEGER*4 NPTS1,NPTS2,IDIM,IOPTION,ISIZE
C Problem with Linux 64 bits, so I switch to INTEGER*8
        INTEGER*8 PNTR_XX1,PNTR_YY1,PNTR_XX2,PNTR_YY2
C Linux 32 bits: switch to INTEGER*4
C        INTEGER*4 PNTR_XX1,PNTR_YY1,PNTR_XX2,PNTR_YY2
C Keep integer*4 in any case:
        INTEGER*4 MADRID(1)
        CHARACTER NAME*(*),COMMENTS*(*)
        COMMON /VMR/MADRID

        ISIZE=IDIM*8
        CALL JLP_GETVM(PNTR_XX1,ISIZE)
        CALL JLP_GETVM(PNTR_YY1,ISIZE)
        CALL JLP_GETVM(PNTR_XX2,ISIZE)
        CALL JLP_GETVM(PNTR_YY2,ISIZE)
	CALL DREADFILE(MADRID(PNTR_XX1),MADRID(PNTR_YY1),NPTS1,
     1         MADRID(PNTR_XX2),MADRID(PNTR_YY2),NPTS2,IDIM,
     1         NAME,COMMENTS,IOPTION)

C Transfer:
        CALL READFILE_TO_SINGLE(X1,MADRID(PNTR_XX1),NPTS1)
        CALL READFILE_TO_SINGLE(Y1,MADRID(PNTR_YY1),NPTS1)
        CALL READFILE_TO_SINGLE(X2,MADRID(PNTR_XX2),NPTS2)
        CALL READFILE_TO_SINGLE(Y2,MADRID(PNTR_YY2),NPTS2)

        CALL JLP_FREEVM(PNTR_XX1,ISIZE)
        CALL JLP_FREEVM(PNTR_YY1,ISIZE)
        CALL JLP_FREEVM(PNTR_XX2,ISIZE)
        CALL JLP_FREEVM(PNTR_YY2,ISIZE)

        RETURN
	END
C+++********************************************************************
C Subroutine RREADFILE_ERRORS(X1,Y1,NPTS1,X2,Y2,NPTS2,IDIM,IOPTION)
C Possibility of entering the option
C REAL output arrays
C Format of the input files:
C 1. 2 columns X, Y
C 2. 3 columns X, Y1, Y2
C 3. PROFILE1
C 4. NPOINTS, followed by 2 columns X, Y
C 5. 2 curves NPOINTS,XY
C 6. LaTeX table
C 7. MIDAS
C---********************************************************************
	SUBROUTINE RREADFILE_ERRORS(X1,Y1,X2,Y2,NPTS1,IDIM,
     1                       NAME,COMMENTS,IOPTION)
	REAL*4 X1(*),Y1(*),X2(*),Y2(*)
        INTEGER*4 NPTS1,IDIM,IOPTION,ISIZE
C Problem with Linux 64 bits, so I switch to INTEGER*8
        INTEGER*8 PNTR_XX1,PNTR_YY1,PNTR_XX2,PNTR_YY2
C Linux 32 bits: switch to INTEGER*4
C        INTEGER*4 PNTR_XX1,PNTR_YY1,PNTR_XX2,PNTR_YY2
C Keep integer*4 in any case:
        INTEGER*4 MADRID(1)
        CHARACTER NAME*(*),COMMENTS*(*)
        COMMON /VMR/MADRID

        ISIZE=IDIM*8
        CALL JLP_GETVM(PNTR_XX1,ISIZE)
        CALL JLP_GETVM(PNTR_YY1,ISIZE)
        CALL JLP_GETVM(PNTR_XX2,ISIZE)
        CALL JLP_GETVM(PNTR_YY2,ISIZE)
	CALL DREADFILE_ERRORS(MADRID(PNTR_XX1),MADRID(PNTR_YY1),
     1         MADRID(PNTR_XX2),MADRID(PNTR_YY2),NPTS1,IDIM,
     1         NAME,COMMENTS,IOPTION)

C Transfer:
        CALL READFILE_TO_SINGLE(X1,MADRID(PNTR_XX1),NPTS1)
        CALL READFILE_TO_SINGLE(Y1,MADRID(PNTR_YY1),NPTS1)
        CALL READFILE_TO_SINGLE(X2,MADRID(PNTR_XX2),NPTS1)
        CALL READFILE_TO_SINGLE(Y2,MADRID(PNTR_YY2),NPTS1)

        CALL JLP_FREEVM(PNTR_XX1,ISIZE)
        CALL JLP_FREEVM(PNTR_YY1,ISIZE)
        CALL JLP_FREEVM(PNTR_XX2,ISIZE)
        CALL JLP_FREEVM(PNTR_YY2,ISIZE)

        RETURN
	END
C*****************************************************************
C Conversion to double precision:
C*****************************************************************
        SUBROUTINE READFILE_FROM_IMAGE(X1,Y1,NPTS1,IMAGE,NX,NY)
        INTEGER*4 I,NX,NY,NPTS1
        REAL*8 X1(*),Y1(*)
        REAL*4 IMAGE(NX,NY)

        NPTS1=NX

C Flux in first line
	DO I=1,NPTS1
	 Y1(I)=IMAGE(I,1)
	ENDDO

C Wavelength in 2nd line
        IF(NY.GT.1)THEN
          WRITE(6,20)
20        FORMAT('READFILE_FROM_IMAGE: line #1 in Y1, line #2 in X1 ?')
          DO I=1,NPTS1
	    X1(I)=IMAGE(I,2)
	  ENDDO
        ELSE
          WRITE(6,21)
21        FORMAT('READFILE_FROM_IMAGE: line #1 in Y1, pixel index in X1 ?')
          DO I=1,NPTS1
	    X1(I)=FLOAT(I)
	  ENDDO
        ENDIF

        RETURN
        END
C----------------------------------------------------------------------
C   To read column number IX from an ASCII file
C INPUT:
C    LU = Logical unit
C    IX = Number of the column
C    L1,L2: first and last lines to read
C
C OUTPUT:
C    X = Array of read values
C    NPTS = Number of values returned
C----------------------------------------------------------------------
        SUBROUTINE READ_ASCII(LU,IX,X,NPTS,IDIM,L1,L2,ITALK)
	INTEGER LU,IX,NPTS,L1,L2,ITALK
	REAL*8 X(*),WORK(30)
        CHARACTER BUFFER*120

10	FORMAT(A)
	NPTS=0
	DO 100 I=1,IDIM
	   BUFFER=' '
	   READ(LU,10,ERR=100,END=101) BUFFER 
C Test on first/last lines allowed for recording the data:
           IF(I.GE.L1.AND.I.LE.L2)THEN
C Here to avoid problems when empty line on SUN...
C and to allow for comments lines (starting with #)
	   IF(BUFFER(1:20).NE.'                    '.AND.
     1        BUFFER(1:1).NE.'#')THEN
	     READ(BUFFER,*,ERR=100) (WORK(K),K=1,IX) 
	     NPTS=NPTS+1
             X(NPTS)=WORK(IX)
C	    WRITE(6,*) 'I=',I,'X=',X(NPTS)
           ELSE
             IF(BUFFER(1:1).EQ.'#'.AND.ITALK.EQ.1)THEN
               WRITE(6,23) BUFFER(2:)
23             FORMAT('Comments: ',A)
             ENDIF
	   ENDIF
C EOF Test on L1,L2:
          ENDIF
          IF(I.GE.L2)GOTO 101
100     CONTINUE	
C Stop if file is too big: 
           WRITE(6,28) IDIM
28         FORMAT(' READ_ASCII/Fatal error: maximum size is ',I5)
           STOP
C Just for the label:
101     CONTINUE	
C        WRITE(6,*) ' NPTS=',NPTS
	RETURN
	END
