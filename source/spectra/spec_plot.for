C++***************************************************************
C spec_plot
C Program to plot a 1-D spectrum.
C 
C Input spectrum: image file 
C       with flux in first line, wavelengths in second line
C
C Version of 17-12-98
C--**************************************************************
	PROGRAM SPEC_BACK
	CHARACTER IN_NAME*40,IN_COMMENTS*80
	CHARACTER PLOTDEV*32
        INTEGER*4 ISTAT,IMODE
	INTEGER*4 MADRID(1),PNTR_1,NX_1,NY_1
	COMMON /VMR/MADRID
 
	CALL JLP_BEGIN
10	FORMAT(A)
 
	WRITE(6,22)
22      FORMAT(' Program plot_back        JLP-Version of 17-12-98',/,
     1  ' Input spectrum: image file flux in line #1, wavelength in line #2')
 
C Inquires about the format of the files :
	CALL JLP_INQUIFMT
 
	WRITE(6,23)
23      FORMAT(' Graphic output device?')
	READ(5,10) PLOTDEV

C************************************************************
C Loading input image file
C************************************************************
	WRITE(6,*) ' Input spectrum (image file) ?'
	READ(5,10) IN_NAME
	CALL JLP_VM_READIMAG(PNTR_1,NX_1,NY_1,IN_NAME,IN_COMMENTS)

	CALL PLT_SPECTRUM(MADRID(PNTR_1),NX_1,NY_1,PLOTDEV,
     1        IN_NAME,IN_COMMENTS)

	CALL JLP_END
	STOP
	END
C************************************************************************
C To plot a spectrum
C INPUT:
C   IMAGE1(NX_1,NY_1)
C   PLOTDEV: name of plotting device
C   IN_NAME, IN_COMMENTS: name and comments of image to be written
C                         on the caption of the plot (if hardcopy) 
C************************************************************************
	SUBROUTINE PLT_SPECTRUM(IMAGE1,NX_1,NY_1,PLOTDEV,
     1         IN_NAME,IN_COMMENTS)
	PARAMETER (IDIM=2000)
        REAL*4 IMAGE1(NX_1,*)
	REAL*4 X1(IDIM),Y1(IDIM)
	REAL*4 XOUT,YOUT
	INTEGER*4 NPTS,NOUT,FILE_OPENED
	CHARACTER ANS*1,PLOTDEV*32,LOGFILE*40,IN_NAME*40,IN_COMMENTS*80
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,NCHAR*4

C Common block for cursor:
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT

10	FORMAT(A)

C Flag set to one when logfile has been opened
        FILE_OPENED=0

C Transfer of the spectrum to X,Y array
        IF(NX_1.GT.IDIM)THEN
          WRITE(6,23) IDIM
23        FORMAT('PLT_SPECTRUM/Fatal error, maximum size set to ',I5)
          ISTAT=-1
        ENDIF

C Loading arrays for display:
        NPTS=NX_1
        DO I=1,NPTS
         X1(I)=IMAGE1(I,2)
         Y1(I)=IMAGE1(I,1)
        ENDDO

C Graphic output
	CHAR1='Wavelength'
	CHAR2='Flux'
        TITLE=IN_NAME
	NCHAR='L0'
	WRITE(6,28)
28	FORMAT(' Displaying the input spectrum')

62	CALL NEWPLOT(X1,Y1,NPTS,IDIM,1,CHAR1,CHAR2,TITLE,
     1	NCHAR,PLOTDEV,IN_NAME,IN_COMMENTS)
	
        IF(NOUT.GT.0)THEN
C Possibility of storing pixel values in a file:
          IF(FILE_OPENED.NE.1)THEN
	    PRINT *,' Do you want to output the cursor values to a file? (Y)'
	    READ(5,10) ANS
	    IF(ANS.NE.'N'.AND.ANS.NE.'n') THEN
              FILE_OPENED=1
	      PRINT *,' Name of output file (if old file: appended)' 
	      READ(5,10) LOGFILE
              OPEN(2,FILE=LOGFILE,STATUS='UNKNOWN')
            ENDIF
          ENDIF

C Output to logfile:
          IF(FILE_OPENED.EQ.1)THEN
            WRITE(2,35) XOUT(1),YOUT(1),IN_NAME
35          FORMAT(1PG11.4,1X,1PG11.4,1X,A)
	    DO I=2,NOUT
C              WRITE(6,34) I,XOUT(I),YOUT(I)
C34            FORMAT(' Point #',I5,' X =',G12.5,' Y=',G12.5)
              WRITE(2,36) XOUT(I),YOUT(I)
36            FORMAT(1PG11.4,1X,1PG11.4)
            END DO
          ENDIF
        ENDIF
 
	PRINT *,' Do you want to display the curve again? (Y)'
	READ(5,10) ANS
	IF(ANS.NE.'N'.AND.ANS.NE.'n')GOTO 62
 
C Close logfile:
        IF(FILE_OPENED.EQ.1) CLOSE(2)

        RETURN
	END
