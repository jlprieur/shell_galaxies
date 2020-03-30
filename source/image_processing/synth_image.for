C++***************************************************************
C Program to synthesize an image:
C
C JLP Version of 15/09/88
C--***************************************************************
	PROGRAM SYNTH_IMAGE
	PARAMETER (IDIM=600)
	REAL*4 IMAGE(IDIM,IDIM)
	CHARACTER NAME*40,COMMENTS*80
 
C Inquire the format (input/output) :
	CALL JLP_INQUIFMT
 
	PRINT *,' OUTPUT SIZE : NX, NY ?'
	READ(5,*) NX,NY
 
	PRINT *,' IMAGE SET TO 1.'
	DO J=1,NY
	  DO I=1,NX
	    IMAGE(I,J)=1.
	  END DO
	END DO
	
	PRINT *,' TO EXIT, TYPE 0,0,0'
	PRINT *,' GIVE THE POSITION AND VALUE: I,J,VAL'
	READ(5,*) I,J,VAL
 
94	IF(I.EE.0.OR.J.EQ.0) GOTO 95
	  IMAGE(I,J)=VAL
	  PRINT *,' GIVE THE POSITION AND VALUE: I,J,VAL'
	  READ(5,*) I,J,VAL
	GOTO 94
 
C Output :
95      WRITE(6,*) 'Output file: '
        READ(5,10) NAME
	CALL JLP_WRITEIMAG(IMAGE,NX,NY,
     1	IDIM,NAME,COMMENTS)
 
	END
