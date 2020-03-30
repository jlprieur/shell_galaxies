C++***************************************************************
C spec_copycal
C Program to copy wavelength calibration 
C from a calibrated spectrum to another one.
C (Should be used after spec_wavecal...)
C
C Input spectrum: image file with flux in line#1, pixels in line #2
C Output spectrum: image file with flux in line#1, wavelengths in line #2
C
C Version of 07-06-94
C--**************************************************************
	PROGRAM SPEC_COPYCAL
	CHARACTER IN_NAME*40,IN_COMMENTS*80
	CHARACTER CAL_NAME*40,CAL_COMMENTS*80
	CHARACTER OUT_NAME*40,OUT_COMMENTS*80
	INTEGER*4 MADRID(1),PNTR_1,PNTR_2,PNTR_3
        INTEGER*4 NX_1,NX_2,NY_1,NY_2
	COMMON /VMR/MADRID
 
	CALL JLP_BEGIN
10	FORMAT(A)
 
	WRITE(6,22)
22      FORMAT(' Program spec_copycal        JLP-Version of 07-06-94',/,
     1    ' to copy wavelength calibration from a calibrated spectrum',/,
     1    ' to another one')
 
C Inquires about the format of the files :
	CALL JLP_INQUIFMT
 
C************************************************************
C Loading input spectra 
C************************************************************
	WRITE(6,*) ' Input wavelength calibrated spectrum (image file) ?'
	READ(5,10) CAL_NAME
	CALL JLP_VM_READIMAG(PNTR_1,NX_1,NY_1,CAL_NAME,CAL_COMMENTS)

	WRITE(6,*) ' Input spectrum to be wavel. calibrated (image file) ?'
	READ(5,10) IN_NAME
	CALL JLP_VM_READIMAG(PNTR_2,NX_2,NY_2,IN_NAME,IN_COMMENTS)
        IF(NX_1.NE.NX_2.OR.NY_1.NE.NY_2)THEN
          WRITE(6,32) NX_1,NY_1,NX_2,NY_2
32        FORMAT(' Fatal error: input files do no match.',/,
     1           ' NX1, NY1 =',2(I5,1X),' whereas NX2, NY2 =',2(I5,1X))
          GOTO 999
        ENDIF

C Get memory space for output file:
        ISIZE=NX_1*NY_1*4
	CALL JLP_GETVM(PNTR_3,ISIZE)
 
C************************************************************
C Copy wavelengths from file1 to file3
C    and flux from file2 to file3 
C************************************************************
	CALL COMPUTE_WAVE(MADRID(PNTR_1),MADRID(PNTR_2),
     1        MADRID(PNTR_3),NX_1)

C**** Output file :
          WRITE(6,*) 'Output file: '
          READ(5,10) OUT_NAME
	  WRITE(OUT_COMMENTS,1003) IN_NAME(1:20),CAL_NAME(1:20)
1003      FORMAT('Wave_cal of ',A,' with ',A)
 	  CALL JLP_WRITEIMAG(MADRID(PNTR_3),NX_1,NY_1,NX_1,
     1             OUT_NAME,OUT_COMMENTS)

C End:
999	CALL JLP_END
	STOP
	END
C************************************************************************
C Compute the wavelengths using dispersion relation
C and transfer input spectrum (IMAGE2) to output (IMAGE3) 
C************************************************************************ 
	SUBROUTINE COMPUTE_WAVE(CAL_IMAGE,IMAGE2,IMAGE3,NX)
        INTEGER*4 I,NX
        REAL*4 CAL_IMAGE(NX,*),IMAGE2(NX,*),IMAGE3(NX,*)

	DO I=1,NX
C Transfer the wavelength to output image: 
          IMAGE3(I,2)=CAL_IMAGE(I,2)
C Transfer the intensity to output image: 
          IMAGE3(I,1)=IMAGE2(I,1)
	END DO

        RETURN
	END
C************************************************************************ 
