C++***************************************************************
C Program to recentre the FFT of a given image
C Moves the zero from the origin to the center of the image
C or inversely.
C Keep the same size as original image.
C
C Syntax:
C        RECEN_FFT INPUT OUTPUT
C
C JLP
C Version of 18-03-90
C--***************************************************************
	PROGRAM RECEN_FFT
	PARAMETER(IDIM=512)
	REAL*4 INPUT(IDIM,IDIM),OUTPUT(IDIM,IDIM)
	INTEGER*4 NX,NY
	CHARACTER NAME*40,COMMENTS*80
 
	CALL JLP_BEGIN
 
	WRITE(6,87)
87	FORMAT(' Program RECEN_FFT to recentre FFT transforms',/,
     1	' Maximum size: 512x512')
 
C Inquire the format (input/output) :
	CALL JLP_INQUIFMT
 
C Input :
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
10      FORMAT(A)
	CALL JLP_READIMAG(INPUT,NX,NY,IDIM,NAME,COMMENTS)
 
	CALL RECENTRE(INPUT,OUTPUT,NX,NY,IDIM)
 
C Output :
        WRITE(6,*) 'Output file: '
        READ(5,10) NAME
	CALL JLP_WRITEIMAG(OUTPUT,NX,NY,IDIM,NAME,COMMENTS)
 
	CALL JLP_END
	STOP
	END
C*****************************************************************
	SUBROUTINE RECENTRE(INPUT,OUTPUT,NX,NY,IDIM)
	REAL*4 INPUT(IDIM,IDIM),OUTPUT(IDIM,IDIM)
	INTEGER*4 NX,NY,NNX,NNY
 
	NNX=2*(NX/2)
	NNY=2*(NY/2)
	IF((NX.NE.NNX).OR.(NY.NE.NNY))THEN
	  PRINT *,' ERROR: NOT EVEN NUMBER OF PIXELS IN X AND/OR Y'
	  STOP
	ENDIF
 
	DO J=1,NY/2
	  DO I=1,NX/2
	    OUTPUT(I+NX/2,J+NY/2)=INPUT(I,J)
	    OUTPUT(I,J)=INPUT(I+NX/2,J+NY/2)
	    OUTPUT(I,J+NY/2)=INPUT(I+NX/2,J)
	    OUTPUT(I+NX/2,J)=INPUT(I,J+NY/2)
	  END DO
	END DO
	
	RETURN
	END
