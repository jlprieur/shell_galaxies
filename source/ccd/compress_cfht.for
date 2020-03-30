C++*******************************************************
C Program to compress CFHT images by a factor 2
C Initial size : 680x1024 or 680x1056
C Final size : 340x512
C Version of November 3rd 1987
C--*******************************************************
	PROGRAM COMPRESS_CFHT
	PARAMETER (IDIM=1100)
	REAL*4 IMAGE(IDIM,IDIM)
	CHARACTER NAME*40,COMMENTS*80
 
	PRINT 20
20	FORMAT(' COMPRESSION OF AN IMAGE BY A FACTOR 2')
 
C Inquire the format of the file :
	CALL JLP_INQUIFMT
 
C Input of the file :
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
	CALL JLP_READIMAG(IMAGE,NPL1,NL1,IDIM,NAME,COMMENTS)
 
C Main loop :
	NPL2=NPL1/2
	NL2=NL1/2
	DO J2=1,NL2
	  DO I2=1,NPL2
	    I1=(2*I2)-1
	    J1=(2*J2)-1
	    IMAGE(I2,J2)=0.25*(IMAGE(I1,J1)+IMAGE(I1,J1+1)+
     1	IMAGE(I1+1,J1+1)+IMAGE(I1+1,J1))
	  END DO
	END DO
 
C	PRINT *,' PREVIOUS NPL,NL :',NPL2,NL2
 
C Suppression of the last columns
	IF(NL2.GT.512) NL2=512
	PRINT *,' NEW NPL,NL :',NPL2,NL2
 
C Output of the file :
        WRITE(6,*) 'Output file: '
        READ(5,10) NAME
	CALL JLP_WRITEIMAG(IMAGE,NPL2,NL2,IDIM,NAME,COMMENTS)
 
        CALL JLP_END
	END
