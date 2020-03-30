C++********************************************************************
C Program WRITEFILE
C To generate a sequential file interactively
C JLP Version of 06-08-86
C--********************************************************************
	PROGRAM WRITEFILE
	PARAMETER (IDIM=1000)
	REAL*8 X(IDIM),Y1(IDIM),Y2(IDIM)
	CHARACTER NAME*30
10	FORMAT(A)
	PRINT *,' PROGRAM WRITEFILE VERSION OF 06-08-86'
5	PRINT *,' Name of the output file ?'
	READ(5,10) NAME
	OPEN(UNIT=1,FILE=NAME,STATUS='NEW',ERR=5)
	PRINT 100
100	FORMAT(' MENU :',/,' 1: FORMAT NPOINT, X,Y',/,
     1	' 2: FORMAT NPOINT, X, Y1, Y2',/,
     1	' 10: EXIT',/,
     1	' ENTER THE OPTION YOU WANT : ',$)
	READ(5,*) IOPT
C----------------------------------------------------------
	IF(IOPT.EQ.1)THEN
	PRINT *,' ENTER X, Y (TO STOP TYPE -1000.,0.)'
 
	DO 150 J=1,IDIM
	PRINT *,' X,Y = ?'
	READ(5,*) X(J),Y1(J)
	IF(X(J).EQ.-1000.)GOTO 180
150	CONTINUE
 
180	NPOINT=J-1
	PRINT *,' ',NPOINT,'POINTS'
	WRITE(1,*)NPOINT
 
	DO 101 I=1,NPOINT
	WRITE(1,*) X(I),Y1(I)
101	CONTINUE
 
	ENDIF
 
C----------------------------------------------------------
	IF(IOPT.EQ.2)THEN
	PRINT *,' ENTER X,Y1,Y2 (TO STOP TYPE -1000.,0.,0.)'
 
	DO 250 J=1,IDIM
	PRINT *,' X, Y1, Y2= ?'
	READ(5,*) X(J),Y1(J),Y2(J)
	IF(X(J).EQ.-1000.)GOTO 280
250	CONTINUE
 
280	NPOINT=J-1
	PRINT *,' ',NPOINT,'POINTS'
	WRITE(1,*)NPOINT
 
	DO 201 I=1,NPOINT
	WRITE(1,*) X(I),Y1(I),Y2(I)
201	CONTINUE
 
	ENDIF
C------------------------------------------------------------------
 
	CLOSE(1)
	STOP
	END
