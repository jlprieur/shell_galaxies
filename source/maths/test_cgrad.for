C*******************************************************************
C TEST_CGRAD
C To test conjugate gradient routines (contained in "conju_grad.c") 
C Calls JLP_CGRAD
C
C We examine the problem: psi = aa phi
C We suppose that the solution is: phi = (x=1;  y=2)
C    1x + 2y = 5.1
C    4x + 5y = 14.5
C   -1x + 2y = 2.9
C   -2x + 1y = 0.1
C
C Version 01-01-1993 
C JLP
C*******************************************************************
        program test_cgrad
	double precision aa(2,4),phi(2),psi(4)
	double precision cte 
        integer ifail, nx, ny, i ,j
        nx = 2
        ny = 4

C Vector psi:
        psi(1)=5.1
        psi(2)=14.5
        psi(3)=2.9
        psi(4)=0.1

C Matrix
        aa(1,1)=1.
        aa(2,1)=2.
        aa(1,2)=4.
        aa(2,2)=5.
        aa(1,3)=-1.
        aa(2,3)=2.
        aa(1,4)=-2.
        aa(2,4)=1.

C First guess for phi:
        print *,' Enter initial guess (one value only)'
        read(5,*)cte
        do i=1,2
          phi(i)=CTE
        enddo

C       
         call jlp_cgrad(aa,psi,phi,nx,ny,ifail)
         print *,' ifail=',ifail

        stop
        end
