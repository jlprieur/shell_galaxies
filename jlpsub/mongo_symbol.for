C--------------------------------------------------------------------
C Called by MONGO to decode the symbols as in NEWPLOT :
C Input : ISYMB, SIZE
C Output : NSIDES, ISTYLE, ANGLE, EXPAND
C ISTYLE : 0=OPEN, 1=SKELETAL (VERTICES CONNECTED TO THE CENTRE)
C	   2=STARRED, 3=FILLED IN
C--------------------------------------------------------------------
	SUBROUTINE MONGO_SYMBOL(ISYMB,SIZE,NSIDES,ISTYLE,ANGLE,EXPAND)
 
	EXPAND=SIZE/0.15
	ANGLE=0.
 
C Small dot :
	IF(ISYMB.EQ.1)THEN
	NSIDES=0
	ISTYLE=0
	ENDIF
 
C White triangle :
	IF(ISYMB.EQ.2)THEN
	NSIDES=3
	ISTYLE=0
	ENDIF
 
C Black triangle :
	IF(ISYMB.EQ.3)THEN
	NSIDES=3
	ISTYLE=3
	ENDIF
 
C Cross + :
	IF(ISYMB.EQ.4)THEN
	NSIDES=4
	ISTYLE=1
	ANGLE=45.
	ENDIF
 
C Cross X :
	IF(ISYMB.EQ.5)THEN
	NSIDES=4
	ISTYLE=1
	ENDIF
 
C White square :
	IF(ISYMB.EQ.6)THEN
	NSIDES=4
	ISTYLE=0
	ENDIF
 
C Black square :
	IF(ISYMB.EQ.7)THEN
	NSIDES=4
	ISTYLE=3
	ENDIF
 
C White circle :
	IF(ISYMB.EQ.8)THEN
	NSIDES=20
	ISTYLE=0
	ENDIF
 
C Black circle :
	IF(ISYMB.EQ.9)THEN
	NSIDES=20
	ISTYLE=3
	ENDIF
 
	RETURN
	END
