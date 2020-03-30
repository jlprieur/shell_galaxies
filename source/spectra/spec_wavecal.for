C++***************************************************************
C spec_wavecal
C Program to perform wavelength calibration of a spectrum.
C Fits a polynomial to the dispersion relation, 
C and then compute the wavelengths from the pixel values.
C Input spectrum: image file 
C       with flux in line #1, wavelengths/pixels in line #2
C
C Version of 21-12-98 
C--**************************************************************
	PROGRAM SPEC_WAVECAL
        PARAMETER(MAX_NWAVE0=200,MAX_NLINES=100)
	CHARACTER IN_NAME*40,IN_COMMENTS*80,WAVE0_NAME(MAX_NWAVE0)*20
	CHARACTER OUT_NAME*40,OUT_COMMENTS*80
	CHARACTER CALIB_FILE*40,PLOTDEV*32,ANS*1
        INTEGER*4 ISTAT,NWAVE0,NLINES
	INTEGER*4 MADRID(1),PNTR_1,NX,NY
	INTEGER*4 PNTR_2,KK,IEMISSION
	REAL*8 XC(20),SDEV(20)
        REAL*4 WAVE0(MAX_NWAVE0)
        REAL*8 XX(MAX_NLINES),YY(MAX_NLINES)
	COMMON /VMR/MADRID
 
	CALL JLP_BEGIN
10	FORMAT(A)
 
	WRITE(6,22)
22      FORMAT(' Program spec_wavecal        JLP-Version of 21-12-98',/,
     1    ' to perform wavelength calibration of a spectrum')
        OPEN(UNIT=1,FILE='spec_wavecal.log',STATUS='UNKNOWN')
        WRITE(1,22)
 
C Inquires about the format of the files :
	CALL JLP_INQUIFMT
 
	WRITE(6,23)
23      FORMAT(' Graphic device? (for interactive plot)')
	READ(5,10) PLOTDEV

	WRITE(6,24)
24      FORMAT(' Input ASCII catalog with calibration lines?  (Format is',/,
     1      ' wavelength in column.#1, intensity in #2, name in #3)')
	READ(5,10) CALIB_FILE 

C NWAVE0: in input, maximum size of WAVE0, in output, actual size of WAVE0 
        NWAVE0=MAX_NWAVE0
        CALL READ_CALIB(CALIB_FILE,WAVE0,WAVE0_NAME,NWAVE0,ISTAT)
        IF(ISTAT.NE.0)THEN
           WRITE(6,35) CALIB_FILE
35         FORMAT(' Fatal error in READ_CALIB opening/reading calibration',
     1         ' file:',A)
           GOTO 999
        ENDIF

C************************************************************
C Loading input image file
C************************************************************
	WRITE(6,*) ' Input spectrum (image file) ?'
	READ(5,10) IN_NAME
	CALL JLP_VM_READIMAG(PNTR_1,NX,NY,IN_NAME,IN_COMMENTS)

C Get memory space for output file:
        ISIZE=NX*NY*4
	CALL JLP_GETVM(PNTR_2,ISIZE)
 
        WRITE(6,30)
30	FORMAT(' Emission lines ? [Y]') 
        READ(5,10) ANS
        IEMISSION=1
	IF(ANS.NE.'N'.AND.ANS.NE.'n')IEMISSION=0

        WRITE(1,125) IN_NAME,CALIB_FILE
125      FORMAT(' Input spectrum with lines: ',A,/,
     1         ' Calibration file (from catalog): ',A)
        IF(IEMISSION.EQ.1)THEN
          WRITE(1,126)
126       FORMAT(' Calibration with emission lines')
        ELSE
          WRITE(1,127)
127       FORMAT(' Calibration with absorption lines')
        ENDIF

C************************************************************
C 1st step: plot the spectrum and identify the lines
C************************************************************
C Input: 
C WAVE0(NWAVE0) array with calibration wavelengths
C MADRID(PNTR_1) input spectrum with emission/absorption lines
C Output: approximative X coordinates in XX array
C         calibration wavelengths in YY array
	CALL IDENTIFY_LINES(MADRID(PNTR_1),NX,
     1         WAVE0,WAVE0_NAME,NWAVE0,XX,YY,NLINES,PLOTDEV,ISTAT)

C************************************************************
C 2nd step: look for central wavelength of NLINES lines 
C************************************************************
C Input: approximative X coordinates in XX array
C Output: precise X coordinates in XX array
        CALL SEARCH_LINES(MADRID(PNTR_1),NX,XX,NLINES,IEMISSION)

C************************************************************
C 3rd step: fit a polynomial to the wavelengh/pixel dispersion relation
C************************************************************
	CALL FIT_DISPERSION(XX,YY,NLINES,XC,SDEV,KK,PLOTDEV,ISTAT)

C************************************************************
C 4th step: apply this polynomial to compute the wavelengh values 
C of the input spectrum.
C************************************************************
	CALL COMPUTE_WAVE(MADRID(PNTR_1),MADRID(PNTR_2),NX,
     1              XC,KK,ISTAT)

C**** Output file :
        IF(ISTAT.EQ.0)THEN
          WRITE(6,*) 'Output file (wavelength-calibrated spectrum): '
          READ(5,10) OUT_NAME
	  WRITE(OUT_COMMENTS,1003) IN_NAME(1:12)
1003      FORMAT('Wavelength_calibrated version of ',A)
 	  CALL JLP_WRITEIMAG(MADRID(PNTR_2),NX,NY,NX,
     1             OUT_NAME,OUT_COMMENTS)
        ENDIF
        WRITE(1,129)OUT_NAME
129     FORMAT(' Output calibrated file: ',A)

C End:
999	CALL JLP_END
        CLOSE(1)
	STOP
	END
C************************************************************************
	SUBROUTINE IDENTIFY_LINES(IMAGE1,NX,
     1         WAVE0,WAVE0_NAME,NWAVE0,XX,YY,NLINES,PLOTDEV,ISTAT)
	PARAMETER (IDIM=2000)
        REAL*4 IMAGE1(NX,*)
	REAL*8 XX(*),YY(*),YY1
	REAL*4 X1(IDIM,2),Y1(IDIM,2),XOUT,YOUT,WAVE0(*)
	INTEGER*4 NPTS(2),NOUT,ISTAT,NWAVE0,I,J,J1,J2
	CHARACTER ANS*1,PLOTDEV*32,WAVE0_NAME(*)*20,PCOLOR(2)*30
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,NCHAR(2)*4

C Common block for cursor:
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT

C Status:
        ISTAT=0

10	FORMAT(A)

C Transfer of the spectrum to X,Y array
        IF(NX.GT.IDIM)THEN
          WRITE(6,23) IDIM
23        FORMAT('BACK_SUBTRACT/Fatal error, nx max =',I5)
          ISTAT=-1
          STOP
        ENDIF

	NPTS(1)=NX
        DO I=1,NPTS(1)
C Loading arrays for display:
          X1(I,1)=IMAGE1(I,2)
          Y1(I,1)=IMAGE1(I,1)
	END DO
 
27	WRITE(6,28)
28	FORMAT(' Displaying the input spectrum')
	WRITE(6,29)
29	FORMAT('Use the cursor to select the lines',
     1           ' (approximately)')
C Graphic output
	CHAR1='Wavelength'
	CHAR2='Flux'
	NCHAR(1)='L0'
        PCOLOR(1)='Default'
	TITLE='Select the lines (approximately)'
62	CALL NEWPLOT(X1,Y1,NPTS,IDIM,1,CHAR1,CHAR2,TITLE,
     1	NCHAR,PCOLOR,PLOTDEV,' ',' ')
	
	PRINT *,' Do you want to display the curve again? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GOTO 62
 
        IF(NOUT.EQ.0)THEN
          WRITE(6,32) NOUT
32        FORMAT(' IDENTIFY_LINES/Error: no points entered')
          GOTO 27
        ENDIF

C Input of XX, YY arrays:
        NLINES=0
        DO I=1,NOUT
          J=NINT(XOUT(I))
34        WRITE(6,33) I,J,IMAGE1(J,1) 
33        FORMAT(' Line #',I3,' IX = ',I4,' Intensity:',1PG10.3,/,
     1           ' Enter wavelength (0 for help, -1 to reject):')
          READ(5,*)YY1
          IF(YY1.EQ.0.)THEN
            WRITE(6,35) NWAVE0
35          FORMAT(' Displaying a subsample of the input catalog',
     1             ' of calibration lines',/,
     1             I5,' lines available. Enter first and last line number:') 
            READ(5,*)J1,J2
            J1=MAX(1,J1)
            J2=MIN(NWAVE0,J2)
            DO J=J1,J2
              WRITE(6,36) J,WAVE0(J),WAVE0_NAME(J) 
              WRITE(1,36) J,WAVE0(J),WAVE0_NAME(J) 
36            FORMAT(' Line #',I4,' Wavelength :',1PG12.5,' Name: ',A20)
            ENDDO
            GOTO 34 
          ELSE
            IF(YY1.NE.-1.)THEN
              NLINES=NLINES+1
              YY(NLINES)=YY1
              XX(NLINES)=XOUT(I)
            ENDIF
          ENDIF
        END DO

	WRITE(6,38)
38      FORMAT(' Is it OK ? (Y) ',/,
     1         '(Otherwise, possibility of displaying the spectrum again)')
	READ(5,10) ANS
	IF(ANS.EQ.'N'.OR.ANS.EQ.'n')GOTO 27 
 
        RETURN
	END
C************************************************************************
C Fit the dispersion relation
C************************************************************************
	SUBROUTINE FIT_DISPERSION(XX,YY,NLINES,XC,SDEV,KK,PLOTDEV,ISTAT)
	PARAMETER (IDIM=2000)
	REAL*8 XX(*),YY(*),XX1,YY1
	REAL*8 XC(*),SDEV(*),ERR
	REAL*4 X1(IDIM,2),Y1(IDIM,2),DELTA,STEP
	REAL*4 XOUT,YOUT
	INTEGER*4 NPTS(2),NOUT,KK,NLINES
	INTEGER*4 ISTAT
	CHARACTER ANS*1,PLOTDEV*32,PCOLOR(2)*30
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,NCHAR(2)*4

C Common block for cursor:
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT

C Status:
        ISTAT=0

10	FORMAT(A)

C Loading arrays for display:
	NPTS(1)=NLINES
        DO I=1,NPTS(1)
         X1(I,1)=XX(I)
         Y1(I,1)=YY(I)
        ENDDO

C Graphic output
        WRITE(6,22)
22      FORMAT(' Displaying input data point for dispersion relation fit')
	CHAR1='Pixel'
	CHAR2='Wavelength'
	NCHAR(1)='550'
        PCOLOR(1)='Default'

	TITLE=' Dispersion input data points'
33	CALL NEWPLOT(X1,Y1,NPTS,IDIM,1,CHAR1,CHAR2,TITLE,
     1	NCHAR,PCOLOR,PLOTDEV,' ',' ')
	PRINT *,' Do you want to display the curve again? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GOTO 33
 
63	PRINT *,' Order of the polynomial for the dispersion relation fit?'
	READ(5,*) KK
 
C*****************************************************************
C Fitting a polynomial :
	PRINT *,' Fitting the polynomial now ...'
	CALL POLYFIT(XX,YY,NLINES,KK,XC,SDEV,ERR)
 
	WRITE(6,25) ERR
	WRITE(1,25) ERR
25	FORMAT(' Mean fitting error rms :',G10.3)
	  DO K=1,KK+1
	    WRITE(6,26) K-1,XC(K),SDEV(K)
	    WRITE(1,26) K-1,XC(K),SDEV(K)
26	    FORMAT(' Order: ',I3,' Coeff: ',G10.3,' Error: ',G12.5)
	  END DO
	
C Plotting the input data points and the fit:
        NPTS(2)=200
        DELTA=1.2*(XX(NLINES)-XX(1))
        STEP=DELTA/NPTS(2)
	DO I=1,NPTS(2)
          X1(I,2)=XX(1)-0.1*DELTA+FLOAT(I)*STEP
          XX1=X1(I,2)
	  CALL CALPOLY(XX1,YY1,XC,KK)
	  Y1(I,2)=YY1
	END DO
 
	NCHAR(2)='L2'
        PCOLOR(2)='Default'
	TITLE='Dispersion relation'
51	WRITE(6,40)
40	FORMAT(' Displaying the fit and the original data points')
	CALL NEWPLOT(X1,Y1,NPTS,IDIM,2,CHAR1,CHAR2,TITLE,
     1	NCHAR,PCOLOR,PLOTDEV,' ',' ')
	PRINT *,' Do you want to change the window parameters? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GOTO 51 

	PRINT *,' Do you want to change the order of the polynomial? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GOTO 63
 
        RETURN
	END
C************************************************************************ 
C Compute the wavelengths using dispersion relation
C and transfer input spectrum to output 
C************************************************************************ 
	SUBROUTINE COMPUTE_WAVE(IMAGE1,IMAGE2,NX,
     1              XC,KK,ISTAT)
        REAL*4 IMAGE1(NX,*),IMAGE2(NX,*)
	REAL*8 XX1,YY1,XC(*)
	INTEGER*4 NX,KK

        ISTAT=0

	DO I=1,NX
C Computing the wavelengths
          XX1=IMAGE1(I,2)
	  CALL CALPOLY(XX1,YY1,XC,KK)
          IMAGE2(I,2)=YY1
C Transfer of the array to output image: 
          IMAGE2(I,1)=IMAGE1(I,1)
	END DO

        RETURN
	END
C**********************************************************************
C Routine to read calibration ASCII file
C 
C NWAVE0: in input, maximum size of WAVE0, in output, actual size of WAVE0 
C**********************************************************************
        SUBROUTINE READ_CALIB(CALIB_FILE,WAVE0,WAVE0_NAME,NWAVE0,ISTAT)
        REAL*4 WAVE0(*)
        INTEGER*4 NWAVE0,ISTAT
        CHARACTER CALIB_FILE*40,WAVE0_NAME(*)*20
C
C Opening input calibration file:
        OPEN(2,FILE=CALIB_FILE,STATUS='OLD',ERR=99)
        DO I=1,NWAVE0
          READ(2,*,END=21) WAVE0(I),XINTENSITY,WAVE0_NAME(I)
C          READ(2,20,END=21) WAVE0(I),XINTENSITY,WAVE0_NAME(I)
C20        FORMAT(G12.5,1X,G12.5,1X,A20)
        END DO
21      NWAVE0=I-1
        CLOSE(2)
        ISTAT=0
        RETURN
C
C Error case:
99      WRITE(6,34)CALIB_FILE
34      FORMAT(' Error opening input catalog :',A)
        ISTAT=-1
        RETURN
        END
C*****************************************************************
C Look for central wavelength of NLINES lines 
C Input: approximative X coordinates in XX array
C Output: precise X coordinates in XX array
C************************************************************************ 
        SUBROUTINE SEARCH_LINES(IMAGE1,NX,XX,NLINES,IEMISSION)
        REAL*4 IMAGE1(NX,*),INTENS_MAX
        REAL*8 XX(*),XX1,SUM
        INTEGER*4 NLINES,IEMISSION,NLINES1,IX(3)

C IERROR set to NX/256 (i.e. 4 pixels if NX=1024)
        IERROR=NX/256
        IERROR=MAX(2,IERROR)

C Save previous value of NLINES:
        NLINES1=NLINES
        NLINES=0

C Loop on all the lines:
        DO ILINE=1,NLINES1
          IX(1)=NINT(XX(ILINE))
          IX(2)=MAX(1,IX(1)-IERROR)
          IX(3)=MIN(NX,IX(1)+IERROR)
C Look for nearest minimum/maximum: 
           INTENS_MAX=IMAGE1(IX(1),1)
C Debug message:
           WRITE(6,28) ILINE,IX(1),INTENS_MAX
           WRITE(1,28) ILINE,IX(1),INTENS_MAX
28         FORMAT(' Line #',I4,' Approximation: IX=',I5,
     1            ' Intensity:',G12.5)
C Emission or absorption line:
           IF(IEMISSION.EQ.1)THEN
             DO I=IX(1),IX(2)
               IF(INTENS_MAX.LT.IMAGE1(I,1))THEN
                 IX(1)=I
                 INTENS_MAX=IMAGE1(I,1)
               ENDIF
             END DO
           ELSE
             DO I=IX(1),IX(2)
               IF(INTENS_MAX.GT.IMAGE1(I,1))THEN
                 IX(1)=I
                 INTENS_MAX=IMAGE1(I,1)
               ENDIF
             END DO
           ENDIF
C Debug message:
           WRITE(6,29) ILINE,IX(1),INTENS_MAX
           WRITE(1,29) ILINE,IX(1),INTENS_MAX
29         FORMAT(' Line #',I4,' Nearest extremum: IX=',I5,
     1            ' Intensity:',G12.5)
C
C Error if maximum on the edge:
           IF(IX(1).EQ.IX(3).OR.IX(1).EQ.IX(2))THEN
             WRITE(6,25) ILINE
             WRITE(1,25) ILINE
25           FORMAT('SEARCH_LINES/Error with line #',I4,
     1              ' (Extremum on the edge...)')
           ELSE
C Then look for barycenter (with 3 values) 
             IX(2)=MAX(1,IX(1)-1)
             IX(3)=MIN(NX,IX(1)+1)
             XX1=0.
             SUM=0.
             DO I=1,3
               XX1=XX1+IX(I)*IMAGE1(IX(I),1)
               SUM=SUM+IMAGE1(IX(I),1)
             END DO
             IF(SUM.NE.0)THEN
               NLINES=NLINES+1
               XX(NLINES)=XX1/SUM
C Debug message:
               WRITE(6,30) NLINES,XX(NLINES)
               WRITE(1,30) NLINES,XX(NLINES)
30             FORMAT(' Final XX,YY line index #',I4,
     1            ' After interpolation: X=',G13.5)
             ENDIF
           ENDIF
        END DO

C Check that more than 0 lines have been found:
        IF(NLINES.EQ.0)THEN
          IF(IEMISSION.EQ.1)THEN
            WRITE(6,31)
            WRITE(1,31)
31          FORMAT(' Fatal error when searching emission lines')
          ELSE
            WRITE(6,32)
            WRITE(1,32)
32          FORMAT(' Fatal error when searching absorption lines')
          ENDIF
          STOP
        ENDIF

        RETURN
        END
C************************************************************************ 
	include 'jlpsub:polyfit.for'
