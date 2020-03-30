C+++********************************************************************
C Set of subroutines to read 1-D files
C Contains:
C  DREADFILE, RREADFILE, DREADFILE_ERRORS, RREADFILE_ERRORS
C 
C JLP
C Version of 22-03-2012
C+++********************************************************************
C Subroutine DREADFILE(X1,Y1,NPTS1,X2,Y2,NPTS2,IDIM,IOPTION)
C Possibility of entering the option
C DOUBLE PRECISION output arrays
C Format of the input files:
C 1. 2 columns X, Y
C 2. 3 columns X, Y1, Y2
C 3. PROFILE1
C 4. NPOINTS, followed by 2 columns X, Y
C 5. 2 curves NPOINTS,XY
C 6. LaTeX table 
C 7. MIDAS Table
C 8. Image file (Midas, FITS, etc) 
C
C---********************************************************************
	SUBROUTINE DREADFILE(X1,Y1,NPTS1,X2,Y2,NPTS2,IDIM,
     1                       NAME,COMMENTS,IOPTION)
        IMPLICIT NONE
        INTEGER*4 NPTS,NPTS0,NPTS1,NPTS2,IDIM,IOPTION,NX,NY
C Problem with Linux 64 bits, so I switch to INTEGER*8
        INTEGER*8 PNTR_IN
C Linux 32 bits: switch to INTEGER*4
C       INTEGER*4 PNTR_IN
C Keep integer*4 in any case:
        INTEGER*4 MADRID(1)
        INTEGER*4 IOPT,I,IX,IY1,IY2,ITALK,L1,L2
	REAL*8 X1(*),Y1(*),X2(*),Y2(*)
	CHARACTER NAME*(*),COMMENTS*(*),BUFFER*80
        COMMON /VMR/MADRID
10	FORMAT(A)
 
	IOPT=IOPTION
C Initialization of first and last lines, for READ_ASCII:
        L1=0
        L2=IDIM+1000
	IF(IOPT.LE.0.OR.IOPT.GT.8)THEN
	WRITE(6,111) IDIM
111	FORMAT(/,8X,'DREADFILE (Max size is ',I5,' points)',
	1	' Possible formats for the input :',/,
	1	1X,' 1: columns "X,Y1,Y2,..."',/,
	1	1X,' 3: Profile created by "PROFILE1"',/,
	1	1X,' 4: NPOINTS, then 2 columns X,Y',/,
	1	1X,' 5: 2 curves X,Y stored successively',
	1	1X,' with NPOINTS as the first row for each',/,
	1	1X,' 6: LaTeX table',/,
	1	1X,' 7: MIDAS table (first 2 columns)',/,
	1	1X,' 8: Image file (2 lines only) Midas, FITS, etc.',/,
	1	10X,'Enter your choice :== ',$)
	  READ(5,*) IOPT
	ENDIF
 
	IF(IOPT.LT.7)THEN
3	  WRITE(6,*) ' Name of the file?'
	  READ(5,10) NAME
	  OPEN(UNIT=17,FILE=NAME,STATUS='OLD',ERR=3)
	ENDIF
C----------------------------------------------------------------------
C   ASCII file with columns :
C   X,Y1,Y2,...
C----------------------------------------------------------------------
	IF(IOPT.EQ.1)THEN
	  WRITE(6,62)
62	  FORMAT(' Enter column numbers for X, Y1 and Y2',
     1     ' (enter 0 for Y2 if only two columns are needed)')
          WRITE(6,61) 
61        FORMAT('NEW: you can also precise the first and last line',/,
     1          'Example: 1,2,0:44,66 to select columns 1 and 2',
     1          ' from 44th to 66th line') 
          READ(5,10) BUFFER
	  READ(BUFFER,*)IX,IY1,IY2
          I=INDEX(BUFFER,':')
          IF(I.GT.1)THEN
	   READ(BUFFER(I+1:),*)L1,L2
           WRITE(6,*)' COLUMNS:',IX,IY1,IY2,' LINES: ',L1,L2
          ENDIF
C X column:
          ITALK=1
	  CALL READ_ASCII(17,IX,X1,NPTS0,IDIM,L1,L2,ITALK)
C Y1 column:
	  CLOSE(17)
	  OPEN(UNIT=17,FILE=NAME,STATUS='OLD')
          ITALK=0
	  CALL READ_ASCII(17,IY1,Y1,NPTS1,IDIM,L1,L2,ITALK)
	  IF(NPTS0.NE.NPTS1) WRITE(6,104) NPTS0,NPTS1
104	  FORMAT('DREADFILE/ERROR: ror X: NPTS0 = ',I5,' for y: NPTS1 =',I5)
C Y2 column:
	  IF(IY2.LE.0)THEN
	    NPTS2=0
	  ELSE
	    CLOSE(17)
	    OPEN(UNIT=17,FILE=NAME,STATUS='OLD')
            ITALK=0
	    CALL READ_ASCII(17,IY2,Y2,NPTS2,IDIM,L1,L2,ITALK)
	    IF(NPTS1.NE.NPTS2) WRITE(6,105) NPTS1,NPTS2
105	    FORMAT('DREADFILE/ERROR: NPTS1 = ',I5,' NPTS2 =',I5)
C Now copy X1 to X2:
	    DO I=1,NPTS2
	      X2(I)=X1(I)
	    ENDDO
	  ENDIF

	  IF(NPTS1.EQ.0) WRITE(6,106)
106	  FORMAT('DREADFILE/ERROR: NPTS1=0 !')
	ENDIF
C----------------------------------------------------------------------
C File created by PROFILE1 with 32 lines of comments in the beginning
C and then three columns Radius, Mean, Number(real*4)
C----------------------------------------------------------------------
	IF(IOPT.EQ.3)THEN
	  DO I=1,32
	    READ(17,10,ERR=999)BUFFER
  	  END DO
C Reads the number of points...
	  READ(17,10,ERR=999)BUFFER
	   DO I=1,IDIM
	     READ(17,*,END=201) X1(I),Y1(I),Y2(I)
	     X2(I)=X1(I)
	   END DO
C Stop if file is too big: 
          WRITE(6,28) IDIM
28        FORMAT('DREADFILE/Fatal error: maximum size is ',I5)
          STOP
C
201	  NPTS1=I-1
C	  NPTS2=NPTS1
	  NPTS2=0
	  IF(NPTS1.EQ.0) WRITE(6,205)
205	  FORMAT(' ERROR: NPTS1=0, NPTS2=0 !')
	ENDIF
C----------------------------------------------------------------------
C   NPTS1
C   X1,Y1
C----------------------------------------------------------------------
	IF(IOPT.EQ.4)THEN
C Allowing 10 lines of comments
          DO I=1,10
	    READ(17,10,ERR=999)BUFFER
            IF((BUFFER(1:1).NE. '%').AND.(BUFFER(1:1).NE.'#'))GOTO 1056
          ENDDO
1056	    READ(BUFFER,*,ERR=999)NPTS
	    DO I=1,NPTS
	      READ(17,*,END=501)X1(I),Y1(I)
	      NPTS1=I
	    END DO
	  IF(NPTS1.NE.NPTS)THEN
	    WRITE(6,405)
405	    FORMAT(' Warning: less points than expected ',
	1	'(are you sure of the format?)')
	  ENDIF
          NPTS2=0
 
	ENDIF
C----------------------------------------------------------------------
c Two curves stored successively :
C   NPTS1
C   X1,Y1
C   NPTS2
C   X2,Y2
C----------------------------------------------------------------------
	IF (IOPT.EQ.5)THEN
C Allowing 10 lines of comments
          DO I=1,10
	    READ(17,10,ERR=999)BUFFER
            IF((BUFFER(1:1).NE. '%').AND.(BUFFER(1:1).NE.'#'))GOTO 2056
          ENDDO

C First curve:
2056	  READ(BUFFER,*,ERR=999)NPTS
	    DO I=1,NPTS
	      READ(17,*,END=501)X1(I),Y1(I)
	      NPTS1=I
	    END DO
	  IF(NPTS1.NE.NPTS)THEN
	    WRITE(6,505)
505	    FORMAT(' Warning: less points than expected ',
     1            '(are you sure of the format?)')
	  ENDIF
 
C Second curve:
	  READ(17,*,ERR=999)NPTS
	    DO I=1,NPTS
	      READ(17,*,END=501)X2(I),Y2(I)
	      NPTS2=I
	    END DO
	  IF(NPTS2.NE.NPTS)THEN
	    WRITE(6,505)
	  ENDIF
 
	ENDIF
C---------------------------------------------------------------------
C LaTeX table in a file
C Not available yet
C---------------------------------------------------------------------
501	IF (IOPT.EQ.6) THEN
C	  WRITE(6,63)
C63	  FORMAT(' Enter column numbers for X, Y1 and Y2',
C      1     ' (enter 0 for Y2 if only two columns are needed)')
C	  READ(5,*)IX,IY1,IY2
C          PRINT *,' OKKKK IX=',IX,' IY1=',IY1,' IY2=',IY2

C INPUT:
C   IX, IY1: column numbers for X and Y1
C   IDIM: maximum number of points
C
C OUTPUT:
C   X1, Y1: data
C   NPTS1: number of points
C
            NPTS1=0
            WRITE(6,*)'This option is not available yet'
            WRITE(6,*)'Please run ""latex_to_ascii"" instead '
C           CALL JLP_RDLATEX_TABLE(IX,IY1,X1,Y1,NPTS1,IDIM,NAME)
C Y2 column:
	  IF(IY2.LE.0)THEN
	    NPTS2=0
	  ELSE
	    NPTS2=0
C            CALL JLP_RDLATEX_TABLE(IX,IY2,X2,Y2,NPTS2,IDIM,NAME)
          ENDIF
	ENDIF
C---------------------------------------------------------------------
C MIDAS table (first two columns only)
C or MIDAS spectrum (with START and STEP for wavelengths...)
C---------------------------------------------------------------------
	IF (IOPT.EQ.7) THEN
 
	  WRITE(6,*) ' Sorry MIDAS Table option not available here...'
C JLP_RDTABLE and JLP_RDSPEC return REAL*4:
C	  IF(IOPT.EQ.7)THEN
C	    CALL JLP_RDTABLE(X3,Y3,NPTS1)
C	  ELSE
C	    CALL JLP_RDSPEC(X3,Y3,NPTS1)
C	  ENDIF
C	  DO I=1,NPTS1
C	    X1(I)=X3(I)
C	    Y1(I)=Y3(I)
C	  END DO
	  NPTS2=0
	ENDIF
C---------------------------------------------------------------------
C Image file (two lines only)
C---------------------------------------------------------------------
C JLP_RDTABLE and JLP_RDSPEC return REAL*4:
	IF (IOPT.EQ.8) THEN
          NAME=' '
          CALL JLP_VM_READIMAG(PNTR_IN,NX,NY,NAME,COMMENTS)
C Transfer to double precision arrays:
          CALL READFILE_FROM_IMAGE(X1,Y1,NPTS1,MADRID(PNTR_IN),NX,NY)
          NPTS2=0
	ENDIF
C---------------------------------------------------------------------
	WRITE(6,*) ' NPOINTS1, NPOINTS2 =',NPTS1,NPTS2
 
	IF(IOPT.LT.7)CLOSE(17)

	RETURN
 
C Fatal error:
999	WRITE(6,*) ' Fatal error: abnormal end of file during read'
	STOP
	END
C+++********************************************************************
C Subroutine DREADFILE_ERRORS(X1,Y1,NPTS1,X2,Y2,NPTS2,IDIM,IOPTION)
C Possibility of entering the option
C DOUBLE PRECISION output arrays
C Only one option for the format of the input files:
C  columns X, Y1, Y2
C
C---********************************************************************
	SUBROUTINE DREADFILE_ERRORS(X1,Y1,X2,Y2,NPTS1,IDIM,
     1                       NAME,COMMENTS,IOPTION)
        INTEGER*4 NPTS1,IDIM,IOPTION,ITALK,IX,IY1,IX2,IY2
        INTEGER*4 L1,L2
	REAL*8 X1(*),Y1(*),X2(*),Y2(*)
	CHARACTER NAME*(*),COMMENTS*(*),BUFFER*80
10	FORMAT(A)
 
3	WRITE(6,*) ' Name of the file?'
	READ(5,10) NAME
	OPEN(UNIT=17,FILE=NAME,STATUS='OLD',ERR=3)
C----------------------------------------------------------------------
C   ASCII file with columns :
C   X,Y1,Y2,...
C----------------------------------------------------------------------
	WRITE(6,63)
63	FORMAT(' Enter column numbers for X, Y, ERROR_X and ERROR_Y')
        WRITE(6,61)
61      FORMAT('NEW: you can also precise the first and last line',/,
     1        'Example: 1,2,4,5:44,66 to select columns 1,2,4,5',
     1        ' from 44th to 66th line')
        READ(5,10) BUFFER
        READ(BUFFER,*)IX,IY1,IX2,IY2
        I=INDEX(BUFFER,':')
        L1=0
        L2=IDIM+1000
        IF(I.GT.1)THEN
         READ(BUFFER(I+1:),*)L1,L2
         WRITE(6,*)' COLUMNS:',IX,IY1,IY2,' LINES: ',L1,L2
        ENDIF
        ITALK=0
C X column:
	IF(IX.GT.0)THEN
	CALL READ_ASCII(17,IX,X1,NPTS0,IDIM,L1,L2,ITALK)
        ENDIF
C Y1 column:
	  IF(IY1.GT.0)THEN
	    CLOSE(17)
	    OPEN(UNIT=17,FILE=NAME,STATUS='OLD')
	    CALL READ_ASCII(17,IY1,Y1,NPTS1,IDIM,L1,L2,ITALK)
	    IF(NPTS0.NE.NPTS1) WRITE(6,104) NPTS0,NPTS1
104	    FORMAT('DREADFILE/ERROR: NPTS0 = ',I5,' NPTS1 =',I5)
          ENDIF
C X2 column:
	  IF(IX2.GT.0)THEN
	    CLOSE(17)
	    OPEN(UNIT=17,FILE=NAME,STATUS='OLD')
	    CALL READ_ASCII(17,IX2,X2,NPTS1,IDIM,L1,L2,ITALK)
	    IF(NPTS0.NE.NPTS1) WRITE(6,104) NPTS0,NPTS1
	  ENDIF

C Y2 column:
	  IF(IY2.GT.0)THEN
	    CLOSE(17)
	    OPEN(UNIT=17,FILE=NAME,STATUS='OLD')
	    CALL READ_ASCII(17,IY2,Y2,NPTS1,IDIM,L1,L2,ITALK)
	    IF(NPTS0.NE.NPTS1) WRITE(6,104) NPTS0,NPTS1
	  ENDIF

	IF(NPTS1.EQ.0)THEN 
          WRITE(6,106)
106	  FORMAT('DREADFILE/ERROR: NPTS1=0 !')
        ELSE
	  WRITE(6,*) ' NPOINTS1 =',NPTS1
	ENDIF
C---------------------------------------------------------------------
 
	CLOSE(17)
	RETURN
	END
C+++********************************************************************
C Set of subroutines to read 1-D files
C Called by readfile1.for (split into two to avoid compilation problems)
C 
C JLP
C Version of 22-03-2012
C+++********************************************************************
C*****************************************************************
C Conversion to single precision:
C*****************************************************************
        SUBROUTINE READFILE_TO_SINGLE(X,XX,NPTS)
        REAL*4 X(*)
        REAL*8 XX(*)
        INTEGER*4 I,NPTS

	DO I=1,NPTS
	 X(I)=SNGL(XX(I))
	ENDDO

        RETURN
        END
