C++*****************************************************************
C Program magni_int
C to compute integrated magnitudes of the shells
C
C JLP
C Version 30-03-89
C--*****************************************************************
	PROGRAM MAGNI_INT
	REAL MAGNI,XINT
	PRINT 20
20	FORMAT(' Program MAGNI_INT, to compute total',
     1	' magnitude of the shells',/,
     1	' Enter the integrated magnitude of the',
     1	' shells,    (one by one, and enter 0 to exit) : ')
 
	XINT=0.
 
300	PRINT 40
40	FORMAT (' ==: ',$)
	READ(5,*) MAGNI
	IF(MAGNI.NE.0.)THEN
	  XINT = 10**(-0.4*MAGNI) + XINT
	  GO TO 300
	ENDIF
 
	MAGNI=-2.5*LOG10(XINT)
	PRINT *,' ==> Total magnitude: ',MAGNI
	STOP
	END
