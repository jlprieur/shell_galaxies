      SUBROUTINE JLP_UPCASE(C,N)
*+
*  purpose: change the lowercase into uppercase
*+
 
      CHARACTER C*(*)
 
        DO 40 J=1,N
         IF((C(J:J).GE.'a').AND.(C(J:J).LE.'z'))
     1     C(J:J) = CHAR(ICHAR(C(J:J))+ICHAR('A')-ICHAR('a'))
40      CONTINUE
 
	RETURN
      END
