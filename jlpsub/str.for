C++************************************************************
C Initially emulator of STROMLO graphic package
C But completely changed since!
C
C Contains: STR_PLOT, STR_CIRCLE
C
C Works in user coordinates
C
C JLP
C Version 24-11-2006
C--************************************************************
C************************************************************
C STR_PLOT
C
C INPUT:
C  X, Y: User coordinates
C  IPEN = 9999 : End of file
C       = 999  : End of plot 
C       = 2    : Plot down 
C       = 3    : Plot up 
C************************************************************
	SUBROUTINE STR_PLOT(X,Y,IPEN,IDV1)
        IMPLICIT NONE
	REAL*4 X,Y
        INTEGER*4 IX,IY,IPEN,IDV1

        CALL CONV_USER_TO_MGO(X,Y,IX,IY,IDV1)
        IF (IPEN.EQ.2)THEN
	  CALL JLP_DRAW(IX,IY,IDV1)
	ELSEIF (IPEN.EQ.3)THEN
	  CALL JLP_RELOC(IX,IY,IDV1)
	ELSEIF (IPEN.GE.999)THEN
	  CALL JLP_SPCLOSE(IDV1)
	ELSEIF (IPEN.GE.9999)THEN
	  CALL JLP_SPCLOSE(IDV1)
	ENDIF
 
	RETURN
	END
C***************************************************************
C Circle (user coordinates)
C Warning: convention is that (0,0) is the lower left corner.
C INPUT:
C  X, Y: User coordinates
C  DIAM: Diameter in user coordinates
C***************************************************************
	SUBROUTINE STR_CIRCLE(X,Y,DIAM,IDV1)
        IMPLICIT NONE
	REAL*4 X,Y,DIAM
        INTEGER*4 ISIZE,ISYMBOL,IDV1

C Internally multiplied by 20 in jlp_symbol (window coordinates)
C Correction is OK now (JLP OCTOBER 1993):
C	SIZE=DIAM/20.
C No longer necessary with SYMBOL2... (JLP2006)

	ISYMBOL=8
C With ISYMBOL = 8, draw an open circle with a diameter of 20 * SIZE
	CALL JLP_SYMBOL2(X,Y,DIAM,ISYMBOL,IDV1)

	RETURN
	END
