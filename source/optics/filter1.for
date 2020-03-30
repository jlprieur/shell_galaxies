C++------------------------------------------------------------
C To read filter files :
C JLP Version of 01-04-87
C------------------------------------------------------------
	PROGRAM FILTER1
	REAL*8 XX(200),YY(200)
	REAL*4 MES(200)
	CHARACTER NAME*30,IDENT*80
10	FORMAT(A)
 
C Input :
5	PRINT *,' INPUT FILE ?'
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='OLD',ERR=5)
 
C Reading the input :
	NUMBER=141
	DO K=1,NUMBER
	READ(1,*)MES(K)
	END DO
	
	READ(1,10)IDENT
 
C Output :
	OPEN(2,FILE=NAME,STATUS='NEW',ERR=5)
	WRITE(2,*)NUMBER
	DO I=1,NUMBER
	XX(I)=300.+FLOAT(I-1)*5.
	YY(I)=MES(I)
	WRITE(2,*)XX(I),YY(I)
	END DO
 
	WRITE(2,10)IDENT
 
	CLOSE(1)
	CLOSE(2)
	STOP
	END
