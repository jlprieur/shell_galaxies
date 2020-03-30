C++ *********************************************************************
C Contains:
C  POLAR_DEG
C  DOUBLE_POLAR_DEG
C  DOUBLE_LOGPOLAR_DEG
C 
C
C-- *********************************************************************
	SUBROUTINE POLAR_DEG(X,Y,RAD,ANGLE,OX,OY)
C *********************************************************************
C  Finds position angle given X, Y, and centre.
C  Position angle in degrees measured from positive X axis through
C  positive Y axis
C
C *********************************************************************
	REAL*4 X,Y,RAD,ANGLE,OX,OY,A,B,PI
        PI=ACOS(-1.)
 
	A=X-OX
	B=Y-OY
	RAD=SQRT(A*A+B*B)
 
	IF(RAD.LT.0.01) THEN
	 WRITE(6,100)
100	 FORMAT(' ANGLE CALLED AT CENTRE POINT')
	 ANGLE=0.0D0
	 RETURN
	ENDIF
 
	IF(B.GT.0.)THEN
	  ANGLE=ACOS(A/RAD)*180./PI
	ELSEIF(B.LT.0)THEN
	  ANGLE=ACOS(-A/RAD)*180./PI+180.
	ELSEIF(B.EQ.0)THEN
	  IF(A.GT.0)ANGLE=0.0D0
	  IF(A.LT.0)ANGLE=180.
	ENDIF
 
	RETURN
	END
C*********************************************************************
	SUBROUTINE DOUBLE_POLAR_DEG(X,Y,RAD,ANGLE,OX,OY)
C*********************************************************************
C Same as POLAR_DEG but in double precision
C
C Finds position angle given X, Y, and centre.
C Position angle in degrees measured from positive X axis through
C positive Y axis
C*********************************************************************
	REAL*8 X,Y,RAD,ANGLE,OX,OY,A,B,PI
        PI=ACOS(-1.)
 
	A=X-OX
	B=Y-OY
	RAD=SQRT(A*A+B*B)
 
	IF(RAD.LT.0.01) THEN
	 WRITE(6,100)
100	 FORMAT(' ANGLE CALLED AT CENTRE POINT')
	 ANGLE=0.0D0
	 RETURN
	ENDIF
 
	IF(B.GT.0.)THEN
	  ANGLE=ACOS(A/RAD)*180./PI
	ELSEIF(B.LT.0)THEN
	  ANGLE=ACOS(-A/RAD)*180./PI+180.
	ELSEIF(B.EQ.0)THEN
	  IF(A.GT.0)ANGLE=0.0D0
	  IF(A.LT.0)ANGLE=180.
	ENDIF
 
	RETURN
	END
C*********************************************************************
	SUBROUTINE DOUBLE_LOGPOLAR_DEG(X,Y,LOGRAD,ANGLE,OX,OY)
C*********************************************************************
C Same as DOUBLE_POLAR_DEG but log for x 
C
C Finds position angle given X, Y, and centre.
C Position angle in degrees measured from positive X axis through
C positive Y axis
C*********************************************************************
	REAL*8 X,Y,RAD,LOGRAD,ANGLE,OX,OY,A,B,PI
        PI=ACOS(-1.)
 
	A=X-OX
	B=Y-OY
	RAD=SQRT(A*A+B*B)
        IF(RAD.EQ.0.)THEN
           LOGRAD = -1000.
        ELSE
           LOGRAD=LOG(RAD)
        ENDIF
 
	IF(RAD.LT.0.01) THEN
	 WRITE(6,100)
100	 FORMAT(' ANGLE CALLED AT CENTRE POINT')
	 ANGLE=0.0D0
	 RETURN
	ENDIF
 
	IF(B.GT.0.)THEN
	  ANGLE=ACOS(A/RAD)*180./PI
	ELSEIF(B.LT.0)THEN
	  ANGLE=ACOS(-A/RAD)*180./PI+180.
	ELSEIF(B.EQ.0)THEN
	  IF(A.GT.0)ANGLE=0.0D0
	  IF(A.LT.0)ANGLE=180.
	ENDIF
 
	RETURN
	END

