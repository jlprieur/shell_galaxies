C++***************************************************************
C SP_SCALE
C Program to create new X coordinates of a spectrum (special application
C for Parkes reduction...)
C
C Assumes that the central velocity is at pixel 128 (spectrum of 256 values)
C and set up a linear scale. For Parkes, vstep=-8.279 (f=1415.448 MHz)
C
C Syntax:
C      RUNS SP_SCALE file_in vstep file_out
C
C Version of 18-09-90
C--**************************************************************
	PROGRAM SP_SCALE
	PARAMETER (IDIM=2000,NMAX=300)
	REAL*4 X1(IDIM),Y1(IDIM)
	REAL*4 VELO,VLIGHT,CVELOCITY,VSTEP
	INTEGER*4 NPTS
	INTEGER*4 QUIET
	CHARACTER OUTFRAME*60,OUTFRAM(NMAX)*60,INFRAM(NMAX)*60
	CHARACTER ANS*1,INFILE*40,OUTFILE*40,COMMENTS*80,BUFFER*80
C Common block with JLP_DIRECTORY:
	COMMON/JLP_DIRECT/INFRAM
 
	CALL JLP_BEGIN
10	FORMAT(A)
 
	PRINT 88
88	FORMAT(' Program SP_BACK to rescale the Velocity axis of a',
     1	' profile/spectrum (Optical velocity mode)')
	NPTS=256
 
C Input file:
78	WRITE(6,*) ' Input ASCII file(s) ? ("suff*.ext"  accepted)'
	READ(5,10) INFILE
	QUIET=1
	CALL JLP_DIRECTORY(INFILE,NFILES,QUIET)
	WRITE(6,79) NFILES
79	FORMAT(' Number of files found: ',I5)
	IF(NFILES.LE.0) GOTO 78
 
	WRITE(6,*) ' Output ASCII file(s) ? ("*.ext"  accepted)'
	READ(5,10) OUTFILE
 
C Generate OUTFRAM from INFRAM, following OUTFILE
	CALL JLP_NFRAME1(OUTFILE,OUTFRAM,INFRAM,NFILES)
 
	WRITE(6,24)
24	FORMAT(' Step per pixel? (linear scale):')
	READ(5,*) VSTEP
 
C Main loop on the files:
	DO K=1,NFILES
 
	OPEN(1,FILE=INFRAM(K),STATUS='OLD',ACCESS='SEQUENTIAL')
	READ(1,10) BUFFER
	READ(BUFFER,*) X1(1),Y1(1)
	COMMENTS=BUFFER(27:80)
	  DO I=2,NPTS
	   READ(1,*) X1(I),Y1(I)
	  END DO
	CLOSE(1)
 
	CVELOCITY=X1(128)
	WRITE(6,28) COMMENTS(1:53),CVELOCITY
28	FORMAT(' Comments: ',A53,/,' Central velocity:',G12.3)
 
C Creating a new X axis (VLIGHT= Light velocity):
	VLIGHT=300000.0
	DO I=1,NPTS
	  VELO=CVELOCITY+VSTEP*FLOAT(I-128)
	  X1(I)=VELO+(VELO-CVELOCITY)*(VELO+CVELOCITY)/VLIGHT
	END DO
 
C Storing the output in an ASCII file :
	OPEN(2,FILE=OUTFRAM(K),STATUS='NEW',ACCESS='SEQUENTIAL')
	BUFFER=' '
	WRITE(BUFFER,*) X1(1),Y1(1)
	BUFFER(31:80)=COMMENTS(1:50)
	WRITE(2,10) BUFFER
	  DO I=2,NPTS
	   WRITE(2,*) X1(I),Y1(I)
	  END DO
	CLOSE(2)
 
C End of the loop on the files:
	END DO
 
 	CALL JLP_END
	STOP
	END
