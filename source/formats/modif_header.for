C*********************************************************
C Program MODIF_HEADER
C to read integer CCD direct access format files
C and change the header
C JLP Version of 01-05-87
C*********************************************************
	PROGRAM MODIF_HEADER
	PARAMETER (IDIM=600)
	INTEGER*2 IMAGE2(600,600),NPL,NL
	CHARACTER NAME*40,IDENT*80
10	FORMAT(A)
 
15	PRINT 20
20	FORMAT(' NAME OF THE INPUT FILE ?')
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='OLD',ACCESS='DIRECT',
     1	ERR=15)
 
C Warning : Integer*2 for the header !!!!
	READ(1'1)IDENT,NPL,NL
	PRINT 30,IDENT,NPL,NL
30	FORMAT(' IDENTIFICATION : ',/,A,/,
     1	' NX =',I4,' NY =',I4,/)	
 
	PRINT *,' ENTER NEW NX AND NY :'
	READ(5,*) NPL,NL
 
	PRINT *,' ENTER NEW IDENTIFICATION :'
	READ(5,10) IDENT(1:80)
 
C Output :
	WRITE(1'1)IDENT,NPL,NL
 
	CLOSE(1)
	END
