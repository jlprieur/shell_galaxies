C++**********************************************************************
C SP_FLUX
C  To process radio HI spectra from PARKES (Observations from Nov. 1989)
C  Computes the flux above the background (after SP_BACK)
C  Interactive mode or batch mode:
C
C Batch:
C    RUNS SP_FLUX in_file N min,max
C
C JLP
C Version of 15-10-90
C--**********************************************************************
	PROGRAM SP_FLUX
	PARAMETER (IDIM=512)
	REAL*4 X1(IDIM),Y1(IDIM),XOUT,YOUT
	REAL*4 CVELOCITY,SUM
	INTEGER*4 NPTS,NOUT
	CHARACTER ANS*1,INFILE*40,COMMENTS*80,BUFFER*80,PCOLOR*30
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,NCHAR*4,PLOTDEV*32
C Common block for cursor:
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
 
10	FORMAT(A)
 
	CALL JLP_BEGIN
 
	WRITE(6,63)
63	FORMAT(' Program SP_FLUX Version 15-10-90',/,
     1	' To compute the flux above the background',/,
     1	' (For Parkes spectra)')
 
	NOUT=0
 
C Input file
	WRITE(6,*) ' Input ASCII file?'
	READ(5,10) INFILE
 
	OPEN(1,FILE=INFILE,STATUS='OLD',ACCESS='SEQUENTIAL')
	READ(1,10) BUFFER
	READ(BUFFER,*) X1(1),Y1(1)
	COMMENTS=BUFFER(27:80)
	  DO I=2,IDIM
	   READ(1,*,END=57) X1(I),Y1(I)
	  END DO
57	CLOSE(1)
	NPTS=I-1
 
	CVELOCITY=X1(128)
	WRITE(6,28) COMMENTS(1:53),CVELOCITY
28	FORMAT(' Comments: ',A53,/,' Central velocity:',G12.3)
 
C Prompt for interactive or batch mode:
	WRITE(6,*)' Interactive mode (with cursor)? [Y]'
	READ(5,10) ANS
 
	IF(ANS.NE.'n'.AND.ANS.NE.'N')THEN
	  WRITE(6,*)' Graphic device ?'
	  READ(5,10) PLOTDEV
	  CHAR1='V helio (km/s)'
	  CHAR2='Flux (Jy)'
	  TITLE=' '
	  NCHAR='L0'
	  PCOLOR='Default'
	  WRITE(6,*) ' Enter the limits with the cursor:'
62        CALL NEWPLOT(X1,Y1,NPTS,IDIM,1,CHAR1,CHAR2,TITLE,
     1	NCHAR,PCOLOR,PLOTDEV,IN_FILE,COMMENTS)
	ENDIF
 
C Non-interactive mode:
	  IF(NOUT.NE.2)THEN
	    WRITE(6,*) ' Lower and upper limit:'
	    READ(5,*) XOUT(1),XOUT(2)
	  ENDIF
 
C Sum:
	 CALL SP_SUM(X1,Y1,NPTS,XOUT(1),XOUT(2),SUM)
	 WRITE(6,*) ' Sum: ',SUM
 
	CALL JLP_END
	STOP
	END
C*******************************************************************
C SP_SUM
C To get the flux within two boundaries:
C*******************************************************************
	SUBROUTINE SP_SUM(X1,Y1,NPTS,XMIN,XMAX,SUM)
	REAL*4 X1(*),Y1(*)
	INTEGER*4 NPTS,IMIN,IMAX
	REAL*4 XMIN,XMAX,SUM
 
C Check that min is lower than max:
	IF(XMIN.GT.XMAX)THEN
	  WORK=XMAX
	  XMAX=XMIN
	  XMIN=WORK
	ENDIF
 
C Check if decreasing or increasing:
	IF(X1(1).LT.X1(NPTS))THEN
 
C Looking for the minimum:
	  DO I=1,NPTS
	    IF(X1(I).GE.XMIN)GOTO 22
	  END DO
22	  IMIN=I
 
C Looking for the maximum:
	  DO I=NPTS,1,-1
	   IF(X1(I).LE.XMAX)GOTO 24
	  END DO
24	  IMAX=I
 
	ELSE
 
C Looking for the maximum (pixel):
	  DO I=NPTS,1,-1
	    IF(X1(I).GE.XMIN)GOTO 32
	  END DO
32	  IMAX=I
 
C Looking for the minimum (pixel):
	  DO I=1,NPTS
	   IF(X1(I).LE.XMAX)GOTO 34
	  END DO
34	  IMIN=I
 
	ENDIF
 
C Check if error:
	SUM=0.
	IF(IMIN.GE.IMAX)THEN
	  WRITE(6,*)' Error SP_SUM/No pixel has been selected!'
	  RETURN
	ELSE
	  WRITE(6,*)' SP_SUM/ Absol. values (from to)',XMIN,XMAX
	  WRITE(6,*)' SP_SUM/ Pixels (from to)',IMIN,IMAX
	ENDIF
 
C Sum:
	DO I=IMIN,IMAX
	  SUM=SUM+Y1(I)
	END DO
 
	RETURN
	END
