C++***************************************************************
C spec_arithm
C Program to process a spectrum or an ascii file
C
C Input file: image or ascii file with  a maximum of 3 lines or colums
C Output file: in the same format as input. 
C
C Version of 24-06-94
C--**************************************************************
	PROGRAM SPEC_ARITHM
        PARAMETER (IDIM=10000)
	CHARACTER IN_NAME*40,IN_COMMENTS*80
	CHARACTER OUT_NAME*40,OUT_COMMENTS*80
        REAL*4 X1(IDIM),Y1(IDIM),X2(1),Y2(1)
	INTEGER*4 MADRID(1),PNTR_OUT
        INTEGER*4 NPTS1,NPTS2,NX_OUT,NY_OUT,ISTAT
	COMMON /VMR/MADRID
 
	CALL JLP_BEGIN
10	FORMAT(A)
 
	WRITE(6,22)
22      FORMAT(' Program spec_ascii_to_image   JLP-Version of 07-06-94',/,
     1    ' to convert an ASCII file to an image file')
 
C Inquires about the format of the files :
	CALL JLP_INQUIFMT
 
C************************************************************
C Loading input spectrum 
C************************************************************
	WRITE(6,*) ' Entering input spectrum (ASCII file) :'
        IOPTION=1
        CALL RREADFILE(X1,Y1,NPTS1,X1,Y2,NPTS2,IDIM,
     1                IN_NAME,IN_COMMENTS,IOPTION)
        IF(NPTS1.EQ.0)THEN
           WRITE(6,34)
34         FORMAT(' Fatal error: Number of points = 0 !')
           GOTO 999
        ENDIF

C Get memory space for output file:
        NX_OUT=NPTS1
        NY_OUT=2
        ISIZE=NX_OUT*NY_OUT*4
	CALL JLP_GETVM(PNTR_OUT,ISIZE)
 
C************************************************************
C Copy wavelengths and flux from input file to output file 
C************************************************************
	CALL COPY_TO_IMAGE(MADRID(PNTR_OUT),NX_OUT,NY_OUT,
     1        X1,Y1,ISTAT)

C**** Output file :
        IF(ISTAT.EQ.0)THEN
          WRITE(6,*) 'Output file: '
          READ(5,10) OUT_NAME
	  WRITE(OUT_COMMENTS,1003) IN_NAME(1:20)
1003      FORMAT('Copy of ',A)
 	  CALL JLP_WRITEIMAG(MADRID(PNTR_OUT),NX_OUT,NY_OUT,NX_OUT,
     1             OUT_NAME,OUT_COMMENTS)
        ENDIF

C End:
999	CALL JLP_END
	STOP
	END
C************************************************************************
C Compute the wavelengths using dispersion relation
C and transfer input spectrum (IMAGE2) to output (IMAGE3) 
C************************************************************************ 
	SUBROUTINE COPY_TO_IMAGE(IMAGE_OUT,NX_OUT,NY_OUT,
     1        X1,Y1,ISTAT)
        INTEGER*4 I,NX_OUT,NY_OUT,ISTAT
        REAL*4 X1(*),Y1(*),IMAGE_OUT(NX_OUT,*)

	DO I=1,NX_OUT
C Transfer of the wavelengths to output image: 
          IMAGE_OUT(I,2)=X1(I)
C Transfer of the intensity to output image: 
          IMAGE_OUT(I,1)=Y1(I)
	END DO

        RETURN
	END
C************************************************************************ 
