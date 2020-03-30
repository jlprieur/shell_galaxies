C++--------------------------------------------------------------------
C Program to smooth images with possibility of unsharp masking.
C JLP 
C Version of 12-11-91 
C--------------------------------------------------------------------
	PROGRAM SMOOTH1
        INTEGER GSIZE
        PARAMETER(GSIZE=31)
	REAL*4 GRID(GSIZE,GSIZE)
	INTEGER*4 PNTR_IN,PNTR_OUT,ISIZE
	INTEGER*4 MADRID(1)
	CHARACTER NAME*40,NAME_OUT*40,COMMENTS*80
        CHARACTER NSMOOTH*30,ANS*1,BUFFER*40
	LOGICAL UNSHARP
	COMMON /VMR/MADRID
 
10	FORMAT(A)
	PRINT 5
5	FORMAT(' Program smooth1 to smooth 2-D images'
     1	,/,' Possibility of unsharp masking',/,' Version 12-11-91')
 
        CALL JLP_BEGIN
C Inquires about the format of the files :
	CALL JLP_INQUIFMT
 
C Read the input image :
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
	CALL JLP_VM_READIMAG(PNTR_IN,NX,NY,NAME,COMMENTS)
 
	PRINT 70
70	FORMAT(' MENU : ',/,
     1	' 1. INPUT OF THE VALUES FOR THE GRID',
     1	' AND STORAGE IN A FILE',/,
     1	' 2. GAUSSIAN GRID',/,
     1	' 3. "TOP HAT" ',/,
     1	' 4. GRID FROM A FILE ',/,
     1	' ENTER THE OPTION : ',$)
	READ(5,*) IOPT
 
	PRINT *,' SIZE OF SMOOTHING GRID (ODD INTEGER IN RANGE 1-',GSIZE,') '
	READ(5,*) NSM
 
C-------------------------------------------------------------
C IOPT=1 Input of the values and storage in a file :
	IF(IOPT.EQ.1)THEN
        WRITE(BUFFER,11) NSM
11      FORMAT(' Width of spec. grid:',I4)
	IOP=4
	SUM=0.
	DO J=1,NSM
	 PRINT 101,J
101	 FORMAT(' LINE :',I3)
	   DO I=1,NSM
	    PRINT 102,I
102	    FORMAT('  ',I3,' := ',$)
	    READ(5,*) GRID(I,J)
	    SUM=SUM+GRID(I,J)
	   END DO
	END DO
 
	IF(SUM.NE.1.)THEN
	PRINT 106
106	FORMAT(' WARNING : THE GRID IS NOT NORMALIZED',/,
     1	' DO YOU WANT ME TO NORMALIZE IT ? (Y)')
	READ(5,10) ANS
	  IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN
	   DO I=1,NSM
	    DO J=1,NSM
	     GRID(I,J)=GRID(I,J)/SUM
	    END DO
	   END DO
	   PRINT *,' GRID NORMALIZED'
	  ENDIF
	ENDIF
 
105	PRINT *,' NAME OF THE OUTPUT FILE (GRID) ?'
	READ(5,10) NSMOOTH
	OPEN(1,FILE=NSMOOTH,STATUS='NEW',ERR=105)
	WRITE(1,*) ((GRID(I,J),I=1,NSM),J=1,NSM)
	CLOSE(1)
	ENDIF
 
C-------------------------------------------------------------
C IOPT=2  Gaussian grid:
	IF(IOPT.EQ.2)THEN
        WRITE(BUFFER,12) NSM
12      FORMAT(' Width of Gauss grid:',I4)
	  IOP=2
	  PRINT 203
203	  FORMAT(' SIGMA (IN PIXELS) : ',$)
	  READ(5,*) SIGMA
	ENDIF
 
C-------------------------------------------------------------
C IOPT=3  "Top Hat" :
	IF(IOPT.EQ.3)THEN
          IOP=3
          WRITE(BUFFER,13) NSM
13        FORMAT(' Width of Top Hat grid:',I4)
        ENDIF
 
C-------------------------------------------------------------
C IOPT=4 Input of a file :
	IF(IOPT.EQ.4)THEN
	  IOP=4
          WRITE(BUFFER,14) NSM
14        FORMAT(' Width of file grid:',I4)
405	  PRINT *,' Name of precalculated smoothing grid'
	  READ(5,10) NSMOOTH
 
	  OPEN(1,FILE=NSMOOTH,STATUS='OLD',ERR=405)
	  READ(1,*) ((GRID(I,J),I=1,NSM),J=1,NSM)
	  CLOSE(1)
 
	ENDIF
 
C--------------------------------------------------------------
C Prints the grid if IOP=4
	IF(IOP.EQ.4)THEN
	  PRINT 801
801	  FORMAT(' SMOOTHING GRID :')
	  DO J=1,NSM
	    DO I=1,NSM
	     PRINT *,I,J,GRID(I,J)
	    END DO
	  END DO
	ENDIF
C-------------------------------------------------------------
C Unsharp masking :
	PRINT *,' DO YOU WANT UNSHARP MASKING ? (N)'
	READ(5,10) ANS
	UNSHARP=(ANS.EQ.'Y'.OR.ANS.EQ.'y')
	 IF(UNSHARP)THEN
	  PRINT *,' FRACTION OF THE SMOOTHED IMAGE YOU WANT TO REMOVE ?'
	  READ(5,*) FRACT
	 ENDIF
 
 
C Output file:
	PRINT *,' OUTPUT FILE:'
	READ(5,10) NAME_OUT
	ISIZE=NX*NY*4
	CALL JLP_GETVM(PNTR_OUT,ISIZE)
 
C Calls SMOOTH2 :
C   IOP=2 Gauss
C   IOP=3 Top hat
C   IOP=4 Input grid
 
	CALL SMOOTH2(MADRID(PNTR_IN),MADRID(PNTR_OUT),
     1	NX,NY,NX,IOP,GRID,NSM,SIGMA,UNSHARP,FRACT)
 
C Output :
	IN=INDEX(NAME,' ')
	  IF(UNSHARP)THEN
	   WRITE(COMMENTS,902)NAME(1:IN),BUFFER(1:30)
902	   FORMAT(' Unsharp mask from :',A,A)
	  ELSE
	   WRITE(COMMENTS,903)NAME(1:IN),BUFFER(1:30)
903	   FORMAT('Smoothed image of:',A,A)
	  ENDIF
	CALL JLP_WRITEIMAG(MADRID(PNTR_OUT),NX,NY,NX,
     1	NAME_OUT,COMMENTS)

	CALL JLP_END
	STOP
	END
C------------------------------------------------------------------------
	include 'jlpsub:smooth2.for'
