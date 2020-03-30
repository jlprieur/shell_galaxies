C*************************************************************
C Subroutine CHEBYFIT
C Fits a Chebyscheff polynomial to arrays XX and YY
C
C XX real*8 array, in any order (!)
C KK : Order of the polynomial (less than 19)
C ICLASS=1 if array in ascending order and need to be sorted
C	      =0 otherwise
C************************************************************
	SUBROUTINE CHEBYFIT(XX,YY,NPTS,KK,ICLASS)
	IMPLICIT REAL*8 (A-H,O-Z)
	PARAMETER (IDIM=2000)
	REAL*8 XX(NPTS),YY(NPTS)
	REAL*8 WORK1(3,IDIM),WORK2(2,20)
	REAL*8 W(IDIM),CHEBY(20),SIGMA(20),ACHEBY(20,20)
C This common block is internal to these two subroutines
	COMMON /CHEBYFIT1/CHEBY,XMIN,XMAX,DELTA
C
C	Generating the array of the weights
C
	DO I=1,NPTS
	W(I)=1.D0
	END DO
C Sort the array XX in ascending order (if ICLASS=0)
	IF(ICLASS.EQ.0)THEN
	  CALL SORT_REAL8(XX,YY,NPTS,'A')
	ENDIF
C
C Generates the coefficients of the Chebyscheff polynomial
C
C	M=NPTS : number of points to fit
C	XX=RAD : array X
C	YY=PROF1 : array Y
C	W(M) : array of the weights
C	WORK1 and WORK2 : arrays for E02ADF to work
C	ACHEBY(nrows,kplus1) : coeff. of the Chebyscheff polynomial
C	SIGMA(kplus1) : rms of the different fits of poly. of degree K
C	IFAIL : error flag
C
	M=NPTS
	KPLUS1=KK+1
	NROWS=20
	IFAIL=1
	CALL E02ADF(M,KPLUS1,NROWS,XX,YY,W,WORK1,WORK2,
	1	ACHEBY,SIGMA,IFAIL)
	IF(IFAIL.NE.0)PRINT 56,IFAIL
56	FORMAT(' WARNING : ERROR IN E02ADF    IFAIL=',I4)
	XMIN=XX(1)
	XMAX=XX(M)
	DELTA=XMAX-XMIN
C	WRITE (6,99) SIGMA(KK+1)
99	FORMAT(' RMS FOR THE FIT WITH POLYNOMIALS = ',F9.4)
C
C	Generating the array of the Chebyscheff polynomial
C
	DO IPLUS1=1,KPLUS1
	CHEBY(IPLUS1)=ACHEBY(KPLUS1,IPLUS1)
C	PRINT 100,IPLUS1,CHEBY(IPLUS1)
	END DO
100	FORMAT(' CHEBY ',I2,' : ',F10.3)
	RETURN
	END
C***************************************************************
C	Subroutine CALCHEBY
C	Calculates the image Y of X by a CHEBYSCHEFF polynomial
C***************************************************************
	SUBROUTINE CALCHEBY(X,Y,KK)
	IMPLICIT REAL*8 (A-H,O-Z)
	REAL*8 CHEBY(20)
	REAL*8 X,Y,XMIN,XMAX
C This common block is internal to these two subroutines
	COMMON /CHEBYFIT1/CHEBY,XMIN,XMAX,DELTA
C
C	Calculates the value of XCAP corresponding to X
C	(for Chebyscheff polynomials, XCAP comprised between -1 and +1
C
	XCAP=((X-XMIN)-(XMAX-X))/DELTA
	IPLUS1=KK+1
	IFAIL=1
C	IPLUS1 : degree + 1 of the polynomial
C	CHEBY(20) : array of the coeff. of the Chebyscheff function
C	IFAIL : error flag
C
	CALL E02AEF(IPLUS1,CHEBY,XCAP,Y,IFAIL)
	IF(IFAIL.NE.0)PRINT 56,IFAIL
56	FORMAT(' WARNING : ERROR IN E02AEF    IFAIL=',I4)
	RETURN
	END
C******************************************************************
	include 'jlpsub:sort_set.for'
