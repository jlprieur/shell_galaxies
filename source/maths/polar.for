C++*************************************************
C Program POLAR to compute the polar coordinates (angle and separation)
C from the Cartesian coordinates of two points
C--*************************************************
        PROGRAM POLAR 
        REAL*4 X1,Y1,X2,Y2,RAD,ANGLE,ANGLE2,A,B,PI
        PI=ACOS(-1.)

        WRITE(6,20)
20      FORMAT('Program POLAR to compute the polar coordinates',
     1         ' (angle and separation)',/,
     1         'from the Cartesian coordinates of two points')
        WRITE(6,21)
21      FORMAT(' Enter the X, Y coordinates of the first point (origin):')
        READ(5,*) X1,Y1
        WRITE(6,22)
22      FORMAT(' Enter the X, Y coordinates of the second point:')
        READ(5,*) X2,Y2
        A=X2-X1
        B=Y2-Y1
        RAD=SQRT(A*A+B*B)

        IF(RAD.LT.1.E-9) THEN
         WRITE(6,100)
100      FORMAT(' Radius is too small')
         ANGLE=0.0
        ENDIF

        IF(B.GT.0.)THEN
          ANGLE=ACOS(A/RAD)*180./PI
        ELSEIF(B.LT.0)THEN
          ANGLE=ACOS(-A/RAD)*180./PI+180.
        ELSEIF(B.EQ.0)THEN
          IF(A.GT.0)ANGLE=0.0D0
          IF(A.LT.0)ANGLE=180.
        ENDIF

        IF(ANGLE.LT.0.)ANGLE = ANGLE+360.
        WRITE(6,30) X1,Y1,X2,Y2,RAD,ANGLE
30      FORMAT(' X1,Y1= ',2(G12.5,1X),3X,' X2,Y2= ',2(G12.5,1X),/,
     1         ' Radius: ',G12.5,'   Angle: ',G12.5,' (degrees)')

C ANGLE is between 0 and 360 degrees,
C ANGLE2 is between -90 and 90 degrees modulus 180 degrees (for speckle)
        ANGLE2=ANGLE
        IF(ANGLE2.GT.180.)ANGLE2 = ANGLE2 - 180.
        IF(ANGLE2.GT.90.)ANGLE2 = ANGLE2 - 180.
        WRITE(6,32) ANGLE2
32      FORMAT(' For speckle: angle2=',G12.5)
        STOP
        END

