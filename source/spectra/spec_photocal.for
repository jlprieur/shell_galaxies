C++***************************************************************
C spec_photocal
C Program to perform photometric calibration 
C
C Input spectrum: file with flux in line#1, wavelengths in line #2
C Photometric standard spectrum: file with flux in line#1, wavel. in line#2
C Output spectrum: file with overall efficiency in line#1, wavel. in line #2
C
C Version of 07-06-94
C--**************************************************************
	PROGRAM SPEC_PHOTOCAL
	CHARACTER IN_NAME*40,IN_COMMENTS*80
	CHARACTER CAL_NAME*40,CAL_COMMENTS*80
	CHARACTER OUT_NAME*40,OUT_COMMENTS*80
	INTEGER*4 MADRID(1),PNTR_1,PNTR_2,PNTR_3
        INTEGER*4 NX_1,NX_2,NY_1,NY_2,ISTAT
	COMMON /VMR/MADRID
 
	CALL JLP_BEGIN
10	FORMAT(A)
 
	WRITE(6,22)
22      FORMAT(' Program spec_photocal        JLP-Version of 07-06-94',/,
     1      ' to divide an image spectrum by the absolute flux of the star')
 
C Inquires about the format of the files :
	CALL JLP_INQUIFMT
 
C************************************************************
C Loading input spectra 
C************************************************************
	WRITE(6,*) ' Input reference spectrum with absolute flux of the star ?'
	READ(5,10) CAL_NAME
	CALL JLP_VM_READIMAG(PNTR_1,NX_1,NY_1,CAL_NAME,CAL_COMMENTS)

	WRITE(6,*) ' Input (wavelength calibrated) spectrum ?'
	READ(5,10) IN_NAME
	CALL JLP_VM_READIMAG(PNTR_2,NX_2,NY_2,IN_NAME,IN_COMMENTS)

C Get memory space for output file:
        ISIZE=NX_2*NY_2*4
	CALL JLP_GETVM(PNTR_3,ISIZE)
 
C************************************************************
C First resample input reference spectrum
C Output in MADRID(PNTR_3)
C************************************************************
	CALL PHOTOCAL_RESAMPLE(MADRID(PNTR_1),NX_1,MADRID(PNTR_2),
     1        NX_2,MADRID(PNTR_3),ISTAT)

C************************************************************
C Then divide data spectrum by reference
C MADRID(PNTR_3) = MADRID(PNTR_2) / MADRID(PNTR_3)
C************************************************************
	CALL COMPUTE_EFFICIENCY(MADRID(PNTR_2),MADRID(PNTR_3),
     1        NX_2,ISTAT)

C**** Output file :
        IF(ISTAT.EQ.0)THEN
          WRITE(6,*) 'Output file: '
          READ(5,10) OUT_NAME
	  WRITE(OUT_COMMENTS,1003) IN_NAME(1:20),CAL_NAME(1:20)
1003      FORMAT('Overall effic. with ',A,' and ',A)
 	  CALL JLP_WRITEIMAG(MADRID(PNTR_3),NX_2,NY_2,NX_2,
     1             OUT_NAME,OUT_COMMENTS)
        ENDIF

C End:
999	CALL JLP_END
	STOP
	END
C************************************************************************
C Compute the overall efficiency by dividing 
C resampled data spectrum by reference spectrum
C IMAGE3 = IMAGE2 / IMAGE3
C************************************************************************ 
	SUBROUTINE COMPUTE_EFFICIENCY(IMAGE2,IMAGE3,NX,ISTAT)
        INTEGER*4 I,NX,ISTAT
        REAL*4 IMAGE2(NX,*),IMAGE3(NX,*)

        ISTAT=0
	DO I=1,NX
C Compute efficiency: 
          IF(IMAGE3(I,1).GT.0.)THEN
             IMAGE3(I,1)=IMAGE2(I,1)/IMAGE3(I,1)
          ELSE
             IMAGE3(I,1)=0.
          ENDIF
	END DO

        RETURN
	END
C************************************************************************ 
C Resample photometric reference spectrum
C Perform linear interpolation.
C (Remember: X is in line#2 Y in line#2)
C
C INPUT:
C   IMAGE1(NX1,*) reference spectrum (to be resampled in X)
C   IMAGE2(NX2,*) used to determine the sampling (X stored in line#2)
C OUTPUT:
C   IMAGE3(NX3,*) flux of reference spectrum (IMAGE1)
C                   with same X sampling as IMAGE2 
C************************************************************************
	SUBROUTINE PHOTOCAL_RESAMPLE(IMAGE1,NX1,IMAGE2,NX2,IMAGE3,ISTAT)
        INTEGER*4 I,NX1,NX2,ISTAT,I1
        REAL*4 IMAGE1(NX1,*),IMAGE2(NX2,*),IMAGE3(NX2,*)
        REAL*4 XLOC,DELTAX,DELTAY

        ISTAT=0
	DO I=1,NX2
          XLOC=IMAGE2(I,2)
C Calling INDEX_MAX4(ARRAY,NPOINT,VALUE,INDEX) in newplot0_set2.for...
C (Search for the higher INDEX verifying ARRAY(INDEX).LE.VALUE)
          CALL INDEX_MAX4(IMAGE1(1,2),NX1,XLOC,I1)
          I2=MIN(NX1,I1+1)
C Perform linear interpolation:
          DELTAY=IMAGE1(I2,1)-IMAGE1(I1,1)
          DELTAX=IMAGE1(I2,2)-IMAGE1(I1,2)
          IF(DELTAX.NE.0.)THEN
             IMAGE3(I,1)=IMAGE1(I1,1)
     1                +(XLOC-IMAGE1(I1,2))*DELTAY/DELTAX
          ELSE
             IMAGE3(I,1)=IMAGE1(I1,1)
          ENDIF
C Transfer the wavelength to output image: 
          IMAGE3(I,2)=XLOC
	END DO

        RETURN
	END
