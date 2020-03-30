C++*************************************************
C Program POLAR_DIST to compute distance
C of two points given by their polar coordinates (angle and separation)
C from the Cartesian coordinates of two points
C--*************************************************
        PROGRAM POLAR_DIST 
        REAL*4 R1,A1,R2,A2,DIST,A,B,PI
        PI=ACOS(-1.)

        WRITE(6,20)
20      FORMAT('Program POLAR_DIST to compute the distance',
     1         ' of two points given by their polar coordinates')
        WRITE(6,21)
21      FORMAT(' Enter the rho,theta(deg) coordinates of the first point (origin):')
        READ(5,*) R1,A1
        A1=A1*PI/180.
        WRITE(6,22)
22      FORMAT(' Enter the rho,theta(deg) coordinates of the second point:')
        READ(5,*) R2,A2
        A2=A2*PI/180.
        A=R1*COS(A1)-R2*COS(A2)
        B=R1*SIN(A1)-R2*SIN(A2)
        DIST=SQRT(A*A+B*B)

        WRITE(6,30) DIST 
30      FORMAT(' Distance: ',G12.5)

        STOP
        END

