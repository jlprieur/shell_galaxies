C-------------------------------------------------------------------
C Set of routines SORT_SET.FOR
C To sort integer, real and character 1-D arrays
C OPTION: 'A' for ascending, 'D' for descending order
C
C Version of 16-06-91
C Contains SORT_REAL4, SORT_REAL8, SORT_INT4, SORT_CHAR 
C-------------------------------------------------------------------
C*******************************************************************
C Subroutine to sort int*4 arrays XX, YY according to the
C XX value.
C OPTION: 'A' for ascending, 'D' for descending order
C*******************************************************************
	SUBROUTINE SORT_INT4(XX,YY,NPOINT,OPTION)
	PARAMETER (IDIM=2000)
	INTEGER*4 XX(*),YY(*),X1(IDIM),Y1(IDIM)
	INTEGER*4 INDEX(IDIM)	
	CHARACTER OPTION*1
	ISAFE=0
C
C Initialization of INDEX
C
	DO J=1,NPOINT
	  INDEX(J)=J
	END DO
C
C Method of the "bubble"
C
 
600	INVERS=0
 
	DO J=1,NPOINT-1
	  I1=INDEX(J)
	  I2=INDEX(J+1)
	  IF(XX(I1).GT.XX(I2))THEN
	    INDEX(J+1)=I1
	    INDEX(J)=I2
	    INVERS=INVERS+1
	  ENDIF	
	END DO
	ISAFE=ISAFE+1
 
	IF(ISAFE.GT.+1.000E+06)THEN
	PRINT *,' I GIVE UP : TOO MANY LOOPS !!!'
	RETURN
	ENDIF
 
	IF(INVERS.NE.0) GO TO 600

C Permutation of the values in the arrays XX and YY
	DO I=1,NPOINT
	  KK=INDEX(I)
	  X1(I)=XX(KK)
	  Y1(I)=YY(KK)
	END DO
 
C Ascending or descending order: 
	IF(OPTION(1:1).NE.'A'.AND.OPTION(1:1).NE.'a')THEN
	  DO I=1,NPOINT
	    II=NPOINT-I+1
	    XX(I)=X1(II)
	    YY(I)=Y1(II)
	  END DO
	ELSE
	  DO I=1,NPOINT
	    XX(I)=X1(I)
	    YY(I)=Y1(I)
	  END DO
	ENDIF
	PRINT *,' PERMUTATION TERMINATED.'
 
	RETURN
	END
C*******************************************************************
C Subroutine to sort real*4 arrays XX, YY according to the
C XX value.
C*******************************************************************
	SUBROUTINE SORT_REAL4(XX,YY,NPOINT,OPTION)
	PARAMETER (IDIM=2000)
	REAL*4 XX(*),YY(*),X1(IDIM),Y1(IDIM)
	INTEGER*4 INDEX(IDIM)	
	CHARACTER OPTION*1
	ISAFE=0
C
C Initialization of INDEX
C
	DO J=1,NPOINT
	INDEX(J)=J
	END DO
C
C Method of the "bubble"
C
 
600	INVERS=0
 
	DO J=1,NPOINT-1
	  I1=INDEX(J)
	  I2=INDEX(J+1)
	  IF(XX(I1).GT.XX(I2))THEN
	    INDEX(J+1)=I1
	    INDEX(J)=I2
	    INVERS=INVERS+1
	  ENDIF	
	END DO
	ISAFE=ISAFE+1
 
	IF(ISAFE.GT.+1.000E+06)THEN
	PRINT *,' I GIVE UP : TOO MANY LOOPS !!!'
	RETURN
	ENDIF
 
	IF(INVERS.NE.0) GO TO 600

C Permutation of the values in the arrays XX and YY
	DO I=1,NPOINT
	  KK=INDEX(I)
	  X1(I)=XX(KK)
	  Y1(I)=YY(KK)
	END DO
 
C Ascending or descending order: 
	IF(OPTION(1:1).NE.'A'.AND.OPTION(1:1).NE.'a')THEN
	  DO I=1,NPOINT
	    II=NPOINT-I+1
	    XX(I)=X1(II)
	    YY(I)=Y1(II)
	  END DO
	ELSE
	  DO I=1,NPOINT
	    XX(I)=X1(I)
	    YY(I)=Y1(I)
	  END DO
	ENDIF
	PRINT *,' PERMUTATION TERMINATED.'
 
	RETURN
	END
C*******************************************************************
C Subroutine to sort real*8 arrays XX, YY according to the
C XX value.
C*******************************************************************
	SUBROUTINE SORT_REAL8(XX,YY,NPOINT,OPTION)
	PARAMETER (IDIM=2000)
	REAL*8 XX(*),YY(*),X1(IDIM),Y1(IDIM)
	INTEGER*4 INDEX(IDIM)	
	CHARACTER OPTION*1
	ISAFE=0
C
C Initialization of INDEX
C
	DO J=1,NPOINT
	INDEX(J)=J
	END DO
C
C Method of the "bubble"
C
 
600	INVERS=0
 
	DO J=1,NPOINT-1
	  I1=INDEX(J)
	  I2=INDEX(J+1)
	  IF(XX(I1).GT.XX(I2))THEN
	    INDEX(J+1)=I1
	    INDEX(J)=I2
	    INVERS=INVERS+1
	  ENDIF	
	END DO
	ISAFE=ISAFE+1
 
	IF(ISAFE.GT.+1.000E+06)THEN
	PRINT *,' I GIVE UP : TOO MANY LOOPS !!!'
	RETURN
	ENDIF
 
	IF(INVERS.NE.0) GO TO 600

C Permutation of the values in the arrays XX and YY
	DO I=1,NPOINT
	  KK=INDEX(I)
	  X1(I)=XX(KK)
	  Y1(I)=YY(KK)
	END DO
 
C Ascending or descending order: 
	IF(OPTION(1:1).NE.'A'.AND.OPTION(1:1).NE.'a')THEN
	  DO I=1,NPOINT
	    II=NPOINT-I+1
	    XX(I)=X1(II)
	    YY(I)=Y1(II)
	  END DO
	ELSE
	  DO I=1,NPOINT
	    XX(I)=X1(I)
	    YY(I)=Y1(I)
	  END DO
	ENDIF
	PRINT *,' PERMUTATION TERMINATED.'
 
	RETURN
	END
C*******************************************************************
C Subroutine to sort an array in increasing order: 
C Character version (first character only)
C*******************************************************************
	SUBROUTINE SORT_CHAR(ARRAY,INDEX1,NPOINT)
	CHARACTER*(*) ARRAY(*)
	INTEGER*4 INDEX1(*)
C
C Initialization of INDEX1
C
	DO J=1,NPOINT
	  INDEX1(J)=J
	END DO
C
C Method of the "bubble"
C
 
600	INVERS=0
 
	DO J=1,NPOINT-1
	  I1=INDEX1(J)
	  I2=INDEX1(J+1)
	  IF(ARRAY(I1).GT.ARRAY(I2))THEN
	    INDEX1(J+1)=I1
	    INDEX1(J)=I2
	    INVERS=INVERS+1
	  ENDIF	
	END DO
 
	IF(INVERS.NE.0) GO TO 600
 
	RETURN
	END
