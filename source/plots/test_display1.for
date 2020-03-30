C++*******************************************************************
C TEST_DISPLAY1.FOR
C Short plotting program to test graphic packages
C
C JLP
C Version 29-07-90
C--*******************************************************************
	PARAMETER(idim=200)
	REAL*4 XX(idim),YY(idim)
	INTEGER*4 NPTS
	CHARACTER CHAR1*(20),CHAR2*(20),TITLE*(40),PLOTDEV*(40)
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
10	FORMAT(A)
 
	DO I=1,20
	 XX(i)=i
	 YY(i)=i**2
	END DO
 
	NSTART=1
	NEND=20
	CHAR1='test'
	CHAR2='test'
	TITLE=' OK?'
	PRINT *,' Title?'
	READ(5,10) TITLE
	PRINT *,' Plotdevice ($TEKTRO ?)'
	READ(5,10) PLOTDEV
 
	CALL DISPLAY1(XX,YY,NSTART,NEND,CHAR1,CHAR2,TITLE,PLOTDEV)
 
	PRINT *,' NPOINTS=',NOUT
	DO I=1,NOUT
	  PRINT *,' I,X,Y',I,XOUT(I),YOUT(I)
	END DO
 
	STOP
	END
