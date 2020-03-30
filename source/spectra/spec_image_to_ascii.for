C++***************************************************************
C spec_image_to_ascii
C Program to convert an image file to an ascii file
C
C Input spectrum: image file with flux in line#1, wavelengths in line #2
C Output spectrum: ascii file with wavelength in column #1,  in line #2
C
C JLP
C Version of 08-06-94
C--**************************************************************
	PROGRAM SPEC_ASCII_TO_IMAGE
	CHARACTER IN_NAME*40,IN_COMMENTS*80
	CHARACTER OUT_NAME*40,OUT_COMMENTS*80
	INTEGER*4 MADRID(1),PNTR_IN
        INTEGER*4 LU,NX,NY,ISTAT
	COMMON /VMR/MADRID
 
	CALL JLP_BEGIN
10	FORMAT(A)
 
	WRITE(6,22)
22      FORMAT(' Program spec_ascii_to_image   JLP-Version of 08-06-94',/,
     1    ' to convert a spectrum image file to an ASCII file')
 
C Inquires about the format of the files :
	CALL JLP_INQUIFMT
 
C************************************************************
C Loading input spectrum 
C************************************************************
        WRITE(6,26)
26      FORMAT(' Input file name (image file) :') 
        READ(5,10) IN_NAME
        CALL JLP_VM_READIMAG(PNTR_IN,NX,NY,IN_NAME,IN_COMMENTS)

C Opens output ASCII file:
        LU=1
        WRITE(6,28)
28      FORMAT(' Output file name (ASCII file) :') 
        READ(5,10) OUT_NAME
        OPEN(LU,FILE=OUT_NAME,STATUS='NEW')

	WRITE(OUT_COMMENTS,1003) IN_NAME(1:20)
1003    FORMAT('ASCII version of ',A)
C
C Writes wavelengths and flux from input file to output file 
	CALL COPY_TO_FILE(LU,MADRID(PNTR_IN),NX,NY,
     1        OUT_NAME,OUT_COMMENTS,ISTAT)

        CLOSE(LU)

C End:
999	CALL JLP_END
	STOP
	END
C************************************************************************
C Writes to output ascii file
C Wavelength in col#1
C Flux in col#2
C************************************************************************ 
	SUBROUTINE COPY_TO_FILE(LU,IMAGE,NX,NY,
     1                          OUT_NAME,OUT_COMMENTS,ISTAT)
        INTEGER*4 I,LU,NX,NY,ISTAT
        REAL*4 IMAGE(NX,*)
        CHARACTER OUT_NAME*(*),OUT_COMMENTS*(*)

C Comments:
        WRITE(LU,32) OUT_COMMENTS
32      FORMAT('#',A)

C Data:
        IF(NY.GT.1)THEN
          WRITE(6,33)
33        FORMAT(' OK: writing line#1 (flux) in col#2 and line#2 in col#1')
	  DO I=1,NX
             WRITE(LU,34) IMAGE(I,2),IMAGE(I,1)
34           FORMAT(2(1PG13.6,1X))
	  END DO
C If second line not present writes pixel data:
        ELSE
          WRITE(6,35)
35        FORMAT(' OK: writing line#1 (flux) in col#2 ',
     1           'and pixel position in col#1')
	  DO I=1,NX
             WRITE(LU,34) FLOAT(I),IMAGE(I,1)
	  END DO
        ENDIF

        RETURN
	END
C************************************************************************ 
