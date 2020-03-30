C++********************************************************************
C Program MIDAS_TAB
C Test program
C JLP Version of 01-10-88
C--********************************************************************
	PROGRAM MIDAS_TAB
	PARAMETER (IDIM=500)
	REAL XX(IDIM),YY(IDIM)
	INTEGER NPTS
 
	CALL JLP_RDTABLE(XX,YY,NPTS)
 
	END
C**********************************************************************
C Subroutine JLP_RDTABLE
C**********************************************************************
	SUBROUTINE JLP_RDTABLE(XX,YY,NROW)
	REAL XX(*),YY(*)
	INTEGER ACTVALS,ISTAT,ERRORS(2)
	INTEGER NCOL,NROW,NSC1
	LOGICAL*4 NULL
	CHARACTER TABLE*40,TEXT*80
	DATA ERRORS/0,100/
 
C Midas environment:
	CALL PROLOG_ST('JLP_RDTABLE')
 
C Error handling:
	CALL ERRCNTRL_ST('GET',1,2,2)
 
C Get the table name
	CALL GETKEY_ST('P1','C',1,40,ACTVALS,TABLE,ISTAT)
	IF(ISTAT.NE.0) THEN
5	   CALL PROMPT_ST('Input table ?','P1','C',1,40,
     1	ACTVALS,TABLE,ISTAT)
	   IF(ISTAT.NE.0) GO TO 5
	ENDIF
 
C Read the table:
	CALL TBL_READ(TABLE,ISTAT)
	IF(ISTAT.NE.0) GO TO 1000
	CALL TBL_RDINFO(TABLE,NCOL,NROW,NSC1,ISTAT)
	IF(ISTAT.NE.0) GO TO 1000
 
C Displays the table description:
	WRITE(TEXT,77) NCOL,NROW,NSC1
77	FORMAT(' NCOL=',I5,'  NROW=',I5,'  NSC1=',I5)
	CALL PUTTEXT_ST(TEXT,ISTAT)
	IF(ISTAT.NE.0) GO TO 1000
 
C Read the first two columns:
	DO J=1,NROW
	  CALL TBL_RDELEM(TABLE,'R',J,1,XX(J),NULL,ISTAT)
	  IF(ISTAT.NE.0) GO TO 1000
	  CALL TBL_RDELEM(TABLE,'R',J,2,YY(J),NULL,ISTAT)
	  IF(ISTAT.NE.0) GO TO 1000
	  WRITE(TEXT,78) J,XX(J),YY(J)
78	  FORMAT(' J,X,Y',X,I5,2X,F12.6,2X,F12.6)
	  CALL PUTTEXT_ST(TEXT,ISTAT)
	  IF(ISTAT.NE.0) GO TO 1000
	END DO
 
C End:
999	CALL EPILOG_ST
	RETURN
 
C When error:
1000	ERRORS(1)=ISTAT
	CALL PUTKEY_ST('PROGSTAT','I',ERRORS,1,2,ISTAT)
	GO TO 999
	END
