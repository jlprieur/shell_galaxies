C++************************************************************************
C Subroutine BELL
C To generate a "bell" sound
C
C JLP Version 01-01-87
C--************************************************************************
	SUBROUTINE BELL
	LOGICAL*1 BELL1
	 BELL1='007'O
	 PRINT 1,BELL1
1	 FORMAT(1X,A)
	RETURN
	END
