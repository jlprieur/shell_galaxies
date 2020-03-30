C*******************************************************************
C Subroutine DISPLAY2 to display 2 curves X1,Y1 and X2,Y2	
C in real*4, with selected symbols NCHAR1 and NCHAR2.
C
C
C Input :
C  X1, Y1
C  N1START, N1END
C  X2, Y2
C  N2START, N2END
C  CHAR1,CHAR2,TITLE
C  PLOTDEV
C  NCHAR1, NCHAR2
C
C JLP
C Version of 21/05/97
C*******************************************************************
	SUBROUTINE DISPLAY2(X1,Y1,N1START,N1END,X2,Y2,N2START,N2END,
     1                    CHAR1,CHAR2,TITLE,PLOTDEV,NCHAR1,NCHAR2,
     1                    IN_FILE,IN_COMMENTS)
	PARAMETER (IDIM=6000)
	REAL*4 X1(1),Y1(1),X2(1),Y2(1)
	REAL*4 X(IDIM,2),Y(IDIM,2)
	INTEGER*4 NPTS(2)
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,PLOTDEV*32
        CHARACTER IN_FILE*60,IN_COMMENTS*80
	CHARACTER NCHAR(2)*4,PCOLOR(2)*30
	CHARACTER*(*) NCHAR1,NCHAR2
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
 

	NPTS(1)=N1END-N1START+1
	NPTS(2)=N2END-N2START+1
 
C Transferring the data:
	DO I=N1START,N1END
	  II=I-N1START+1
	  X(II,1)=X1(I)
	  Y(II,1)=Y1(I)
	END DO
 
	DO I=N2START,N2END
	  II=I-N2START+1
	  X(II,2)=X2(I)
	  Y(II,2)=Y2(I)
	END DO
 
	NCHAR(1)=NCHAR1
        PCOLOR(1)='Default'
	NCHAR(2)=NCHAR2
        PCOLOR(2)='Default'
 
C Drawing the curve
	KK=2		! Number of curves
 
	CALL NEWPLOT(X,Y,NPTS,IDIM,KK,CHAR1,CHAR2,TITLE,
     1             NCHAR,PCOLOR,PLOTDEV,IN_FILE,IN_COMMENTS)
 
	RETURN
	END
