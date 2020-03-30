C********************************************************************
C  EFIT fits ellipses to contours then analyses 		    *
C  the deviations of the contours from the fitted ellipses, the     *
C  way it does this is to Fourier analyse the residuals as a        *
C  function of position angle. The purpose of this is to look for   *
C  both deviations which might be artefacts, due to a poor fit of   *
C  the ellipse or to a superimposed star image, and for deviations  *
C  which might be real, caused by weak disc-bulge structure or by   *
C  box-shaped isophotes.                                            *
C
C  Call NAG routine E04HFF with second derivatives                  *
C
C  From Dave Carter
C  Possibility of free or fixed centre according to the value of NPARAM1
C
C Z(1) Major axis (pixels)
C Z(2) Minor axis (pixels)
C Z(3) Theta (radians)
C Z(4) OX (pixels)
C Z(5) OY (pixels)
C
C THEMIN, THEMAX : limiting angles in radians
C
C IFLAG ERROR FLAG (POSITIVE IF NAG ERROR CODE, NEGATIVE IF MY OWN CODE)
C CODE:
C -1 TOO FEW POINTS
C -2 TOO MANY POINTS
C -3 LARGE ERRORS
C -4 STORAGE SPACE EXHAUSTED
C
C General output in unit 6 (interactive) and unit 9 (journal).
C Output of Fourier components in unit 4 ("fitelli.fou")
C
C JLP
C Version of 05/06/92
C********************************************************************
	SUBROUTINE EFIT(NPOINT1,NPARAM1,AUTO_GUESS,AUTO_IMPROVE,
	1	FOURIER,KAMIKAZE,IFLAG,TALK)
	IMPLICIT REAL*8(A-H,P-Z)
        PARAMETER (MAXPOINTS=2000)
	REAL*8 Z(5),PI,THEMIN,THEMAX,FSUMSQ,SDEV(5),SCALE
	REAL*4 X,Y,ESTORE,FDATA,RRR,AAAA,AA
	REAL*8 FVECC(MAXPOINTS),AJAC(MAXPOINTS,5),WORK(10*MAXPOINTS)
        REAL*8 TMIN,TMAX,RMIN,RMAX,THETA
	INTEGER*4 IFLG,IWORK(10),NEE,NPARAM,NPOINT
	LOGICAL*4 FOURIER,AUTO_GUESS,AUTO_IMPROVE,KAMIKAZE,TALK
 
	COMMON/FIT1/Z,NPOINT,IFLG
	COMMON/BLOCKA/PI,THEMIN,THEMAX
	COMMON/BLOCKB/X(MAXPOINTS),Y(MAXPOINTS)
	COMMON/BLOCKC/SCALE
	COMMON/EPRM/SDEV
	COMMON/EDATA/ESTORE(200,10),FDATA(200,10),NEE
	COMMON/FOU1/RRR,AAAA,AA(4,2)
	COMMON/CHECKEFIT1/IILSFUN2,IILSHES2
 
	EXTERNAL LSFUN2,LSHES2
 
	PI=3.14159265358979323846D0

	IFLAG=0
	NPOINT=NPOINT1
	M=NPOINT
	NPARAM=NPARAM1
	ISTEP=1
	Z3OLD=0.0
	WRITE(9,1000) NPOINT
	IF(TALK) WRITE(6,1000) NPOINT
1000	FORMAT(/,2X,' EFIT CALLED :',2X,I8,' POINTS RECEIVED')
C
C  Doesn't try to fit the contour if there are less than 8 points
C
1111	IF(NPOINT.LT.8) THEN
	 WRITE(9,1020)
	 IF(TALK) WRITE(6,1020)
1020	 FORMAT(' TRY AGAIN WITH MORE POINTS ')
	 IFLAG=-1
	 RETURN
	ENDIF
 
C  Doesn't try to fit the contour if there are more than MAXPOINTS points
	IF(NPOINT.GE.MAXPOINTS) THEN
	 WRITE(9,1030)
	 IF(TALK) WRITE(6,1030)
1030	 FORMAT(' TOO MANY POINTS FOR ELLFIT')
	 IFLAG=-2
	 RETURN
	ENDIF
 
C  Sets up initial guess at fit on the first step :
	IF(ISTEP.LE.1.AND.AUTO_GUESS)THEN
	 IF(TALK) WRITE(6,1031)
	 WRITE(9,1031)
1031	 FORMAT(' EFIT/FIRST ATTEMPT :')
	 CALL INPARS(NPARAM)
	ENDIF

C  Calls NAG routine to minimise the residuals from
C  the fitted ellipse.
66	LIW=10
C LW>90+13*NPOINT when NPARAM=5
	LW=35000
	IILSFUN2=0
	IILSHES2=0
	IFAIL=1
	CALL E04HFF(NPOINT,NPARAM,Z,FSUMSQ,IWORK,LIW,WORK,LW,IFAIL)
	WRITE(9,1041)IILSFUN2,IILSHES2
1041	FORMAT(' ITERATIONS FOR E04HFF : IILSFUN2 =',I5,
     1          ' IILSHES2 =',I5)
 
C  Write to units 6 & 9 if it fails to find minimum
	IF(IFAIL.NE.0) THEN
	  WRITE(9,1040) IFAIL
	  IF(TALK) THEN
             WRITE(6,1040) IFAIL
1040	     FORMAT(' FAILURE IN E04HFF, IFAIL =',I4)
	     WRITE(9,1041)IILSFUN2,IILSHES2
C IFAIL=2: wrong starting point (more than 50 iterations)
             IF(IFAIL.EQ.2)THEN
                WRITE(6,1042) 
1042	        FORMAT(' Wrong initial guess: try with another center...')
             ENDIF
          ENDIF
	  IFLAG=IFAIL
	ENDIF
 
C Compute new values of THEMIN,THEMAX, if center has changed (free center):
        IF(NPARAM.EQ.5)THEN
	  CALL THETA_MIN_MAX(TMIN,TMAX,RMIN,RMAX,THETA,Z(4),Z(5)) 
          THEMIN=TMIN
          THEMAX=TMAX
        ENDIF
C JLP99
        WRITE(6,*)' JLP99/THEMIN,THEMAX',THEMIN,THEMAX

C Return if serious problem :
	IF((IFAIL.LT.5.AND.IFAIL.GT.8).AND.(.NOT.KAMIKAZE))RETURN
 
C  Work out errors in the fit by inverting the Hessian matrix :
	CALL ERPARM(FSUMSQ,MFLAG,NPARAM,TALK)
 
	IF(MFLAG.NE.0)THEN
	  WRITE(9,1050)
	  IF(TALK) WRITE(6,1050)
1050	  FORMAT(' FAILURE TO FIND A SENSIBLE SOLUTION')
	  IFLAG=-3
C JLP99: I go on in that case.
C	  IF(.NOT.KAMIKAZE)RETURN
	ENDIF
 
C Display the results with "monitor" routine:
	CALL MONIT(NPARAM,FVECC,FSUMSQ,TALK)
 
C Possibility of removing bad points such as small stars if
C AUTO_IMPROVE=.TRUE.
	 IF(ISTEP.LT.30.AND.AUTO_IMPROVE)THEN	
	    ISTEP=ISTEP+1
C Look at the variation of the position angle, and decide whether
C to do a new iteration:
	    ZDELTA=DABS(Z3OLD-Z(3))
	    Z3OLD=Z(3)
	    IF(ZDELTA.GT.0.002)THEN
C Compute the residuals FVECC with the solution :
	       LJC=MAXPOINTS
	       CALL LSFUN2(NPOINT,NPARAM,Z,FVECC,AJAC,LJC)
C Another call of E0 after elimination of points over 1.6-sigma from
C the ellipse:
	       RMS=DSQRT(FSUMSQ/FLOAT(NPOINT-NPARAM))
	       VTEST=1.6*RMS
C Eliminates only the points with positive residuals FVECC
C since most of the time the pollution comes from stars in the outskirts
	       II=0
	       DO I=1,NPOINT
	         IF(FVECC(I).LT.VTEST)THEN
	            II=II+1
	            X(II)=X(I)
	            Y(II)=Y(I)
	         ENDIF
	       END DO
 
	  IF(TALK) WRITE (6,857)
	  WRITE (9,857)
857	  FORMAT(' REMOVING POINTS WITH RESIDUALS OVER 1.6 SIGMA')
	  NPOINT=II
	  IF(TALK) WRITE(6,856) ISTEP,NPOINT
	  WRITE(9,856) ISTEP,NPOINT
856	  FORMAT(2X,I2,'th CALL OF E04HFF WITH :',I5,' POINTS')
	  GO TO 1111
	ENDIF
 
	ENDIF
 
C  Stores final results for later output (with ECALC)
 
C Return if there is more than 200 curves (!)
1300	NEE=NEE+1
	IF(NEE.GT.200) THEN
	 WRITE(9,1333)
	 IF(TALK) WRITE(6,1333)
1333	 FORMAT(' ESTORE STORAGE EXHAUSTED')
	 IFLAG=-4
	 RETURN
	ENDIF
 
	ESTORE(NEE,1)=Z(1)
	ESTORE(NEE,2)=Z(2)
	ESTORE(NEE,3)=Z(3)
	ESTORE(NEE,4)=Z(4)
	ESTORE(NEE,5)=Z(5)
	ESTORE(NEE,6)=SDEV(1)
	ESTORE(NEE,7)=SDEV(2)
	ESTORE(NEE,8)=SDEV(3)
	ESTORE(NEE,9)=SDEV(4)
	ESTORE(NEE,10)=SDEV(5)
 
C  Call the subroutine which Fourier analyses the residuals
C  of the contour from the fitted ellipse
C  and store the Fourier output for later output (with FFCALC)
	IF(FOURIER)THEN
	  CALL RECSER
	  FDATA(NEE,1)=RRR
	  FDATA(NEE,2)=AAAA
	  FDATA(NEE,3)=AA(1,1)
	  FDATA(NEE,4)=AA(1,2)
	  FDATA(NEE,5)=AA(2,1)
	  FDATA(NEE,6)=AA(2,2)
	  FDATA(NEE,7)=AA(3,1)
	  FDATA(NEE,8)=AA(3,2)
	  FDATA(NEE,9)=AA(4,1)
	  FDATA(NEE,10)=AA(4,2)
	ENDIF
 
	RETURN
	END
C**********************************************************
C SUBROUTINE LSFUN2(NPOINT,NPARAM,Z,FVECC,FJACC,LJC)
C This routine is called by E04HFF to work out all of the
C residuals and the Jacobian matrix
C
C**********************************************************
	SUBROUTINE LSFUN2(NPOINT,NPARAM,Z,FVECC,FJACC,LJC)
	IMPLICIT REAL*8(A-H,P-Z)
        PARAMETER (MAXPOINTS=2000)
	REAL*8 PI,THEMIN,THEMAX,X0,Y0
	REAL*4 X,Y
	REAL*8 Z(5),FVECC(NPOINT),FJACC(LJC,NPARAM)
	REAL*8 AJAC(MAXPOINTS,5)
	COMMON/BLOCKA/PI,THEMIN,THEMAX
	COMMON/BLOCKB/X(MAXPOINTS),Y(MAXPOINTS)
	COMMON/CHECKEFIT1/IILSFUN2,IILSHES2
	COMMON/FIXED_CENT/X0,Y0
	IILSFUN2=IILSFUN2+1
 
	TCS=DCOS(Z(3))
	TSN=DSIN(Z(3))
	TCS2=TCS*TCS
	TSN2=TSN*TSN
	Z1=Z(1)
	Z2=Z(2)
C To avoid division by 0 :
	IF(DABS(Z1).LT.1.0D-10)Z1=1.0D-10
	IF(DABS(Z2).LT.1.0D-10)Z2=1.0D-10
	DENA=Z1*Z1
	DENB=Z2*Z2
 
C  Residuals in array FVECC :
	DO 30 I=1,NPOINT
	TDX=X(I)-Z(4)
	TDY=Y(I)-Z(5)
	TDX2=TDX*TDX
	TDY2=TDY*TDY
	  F1=((TDX2*TCS2)+(TDY2*TSN2)+
	1	2.0*(TDX*TDY*TCS*TSN))/DENA
	  F2=((TDX2*TSN2)+(TDY2*TCS2)-
	1	2.0*(TDX*TDY*TCS*TSN))/DENB
	  FVECC(I)=F1+F2-1.0D0
 
C  Jacobian matrix in two dimensional array AJAC. This bit is
C  right, don't change it!
	  AJAC(I,1)=-2.0*F1/Z1
	  AJAC(I,2)=-2.0*F2/Z2
	  AJAC(I,4)=(-2.0*TDX*TCS2-2.0*TDY*TCS*TSN)/DENA+
	1	(-2.0*TDX*TSN2+2.0*TDY*TCS*TSN)/DENB
	  AJAC(I,5)=(-2.0*TDY*TSN2-2.0*TDX*TCS*TSN)/DENA+
	1	(-2.0*TDY*TCS2+2.0*TDX*TCS*TSN)/DENB
	  AJAC(I,3)=(2.0*(TDY2-TDX2)*TCS*TSN+
	1	2.0*TDX*TDY*(TCS2-TSN2))/DENA+
	1	(2.0*(TDX2-TDY2)*TCS*TSN-
	1	2.0*TDX*TDY*(TCS2-TSN2))/DENB
30	CONTINUE
 
	DO I=1,NPOINT
	 DO K=1,NPARAM
	  FJACC(I,K)=AJAC(I,K)
	 END DO
	END DO
 
	END
C**********************************************************
C SUBROUTINE LSHES2(NPOINT,NPARAM,FVECC,Z,HESS,LHES)
C  This routine is called by E04HFF to work out
C the elements of the symmetric Hessian matrix (Second derivatives)
C Here FVECC is an input array which must not be changed.
C**********************************************************
	SUBROUTINE LSHES2(NPOINT,NPARAM,FVECC,Z,HESS,LHES)
        PARAMETER (MAXPOINTS=2000)
 	IMPLICIT REAL*8(A-H,P-Z)
	REAL*8 X0,Y0,PI,THEMIN,THEMAX
	REAL*4 X,Y
	REAL*8 FHESS(15)
	REAL*8 Z(5),FVECC(NPOINT),HESS(LHES)
	COMMON/BLOCKA/PI,THEMIN,THEMAX
	COMMON/BLOCKB/X(MAXPOINTS),Y(MAXPOINTS)
	COMMON/CHECKEFIT1/IILSFUN2,IILSHES2
	COMMON/FIXED_CENT/X0,Y0
	IILSHES2=IILSHES2+1
 
	TCS=DCOS(Z(3))
	TSN=DSIN(Z(3))
	TCS2=TCS*TCS
	TSN2=TSN*TSN
	Z1=Z(1)
	Z2=Z(2)
 
C To avoid division by 0 :
	IF(DABS(Z1).LT.1.0D-10)Z1=1.0D-10
	IF(DABS(Z2).LT.1.0D-10)Z2=1.0D-10
 
	DENA=Z1*Z1
	DENB=Z2*Z2
	Z13=Z1*Z1*Z1
	Z23=Z2*Z2*Z2
 
	NMAXHESS=((NPARAM+1)*NPARAM)/2
C Initialise the array HESS
	DO K=1,NMAXHESS
	HESS(K)=0.0
	END DO
 
C  Residuals in array FVECC :
	DO 30 I=1,NPOINT
	TDX=X(I)-Z(4)
	TDY=Y(I)-Z(5)
	TDX2=TDX*TDX
	TDY2=TDY*TDY
	  F1=((TDX2*TCS2)+(TDY2*TSN2)+
	1	2.0*(TDX*TDY*TCS*TSN))/DENA
	  F2=((TDX2*TSN2)+(TDY2*TCS2)-
	1	2.0*(TDX*TDY*TCS*TSN))/DENB
 
C  Hessian matrix in one dimensional array FHESS.
C 1,1
	FHESS(1)=6.0*F1/DENA
C 2,1
	FHESS(2)=0.
C 2,2
	FHESS(3)=6.0*F2/DENB
C 3,1
	FHESS(4)=(2.0*(TDY2-TDX2)*TCS*TSN+
	1	2.0*TDX*TDY*(TCS2-TSN2))*(-2.0/Z13)
C 3,2
	FHESS(5)=(2.0*(TDX2-TDY2)*TCS*TSN-
	1	2.0*TDX*TDY*(TCS2-TSN2))*(-2.0/Z23)
C 3,3
	FHESS(6)=(2.0*TSN2*(TDX2-TDY2)-
	1	2.0*TCS2*(TDX2-TDY2)-
	1	8.0*TSN*TCS*TDX*TDY)/DENA+
	1	(2.0*TCS2*(TDX2-TDY2)-
	1	2.0*TSN2*(TDX2-TDY2)+
	1	8.0*TSN*TCS*TDX*TDY)/DENB
C 4,1
	FHESS(7)=(-2.0*TDX*TCS2-2.0*TDY*TCS*TSN)*(-2.0/Z13)
C 4,2
	FHESS(8)=(-2.0*TDX*TSN2+2.0*TDY*TCS*TSN)*(-2.0/Z23)
C 4,3
	FHESS(9)=(4.0*TCS*TSN*TDX-2.0*TDY*(TCS2-TSN2))/DENA+
	1	(-4.0*TCS*TSN*TDX+2.0*TDY*(TCS2-TSN2))/DENB
C 4,4
	FHESS(10)=2.0*TCS2/DENA+2.0*TSN2/DENB
C 5,1
	FHESS(11)=(-2.0*TDY*TSN2-2.0*TDX*TCS*TSN)*(-2.0/Z13)
C 5,2
	FHESS(12)=(-2.0*TDY*TCS2+2.0*TDX*TCS*TSN)*(-2.0/Z23)
C 5,3
	FHESS(13)=(-4.0*TCS*TSN*TDY-2.0*TDX*(TCS2-TSN2))/DENA+
	1	(4.0*TCS*TSN*TDY+2.0*TDX*(TCS2-TSN2))/DENB
C 5,4
	FHESS(14)=2.0*TCS*TSN/DENA-2.0*TCS*TSN/DENB
C 5,5
	FHESS(15)=2.0*TSN2/DENA+2.0*TCS2/DENB
 
	DO K=1,NMAXHESS
 	 HESS(K)=HESS(K)+FVECC(I)*FHESS(K)
	END DO
 
30	CONTINUE
 
	END
C****************************************************************
C SUBROUTINE MONIT(NPARAM,FVECC,FSUMSQ,TALK)
C
C  This routine can be called by EFIT to output how the
C  fit is going.
C
C****************************************************************
	SUBROUTINE MONIT(NPARAM,FVECC,FSUMSQ,TALK)
	IMPLICIT REAL*8(A-H,P-Z)
	REAL*8 Z(5),FVECC(NPOINT),SDEV(5),SXC,SYC
	REAL*8 PI,THEMIN,THEMAX,FSUMSQ,SCALE
        INTEGER*4 NPOINT,IFLG
	LOGICAL*4 TALK
	COMMON/FIT1/Z,NPOINT,IFLG
	COMMON/EPRM/SDEV
	COMMON/BLOCKA/PI,THEMIN,THEMAX
	COMMON/BLOCKC/SCALE
 
	RMS=DSQRT(FSUMSQ/FLOAT(NPOINT-NPARAM))
	WRITE(9,302) FSUMSQ,RMS
302	FORMAT(/,'  SUM OF SQUARES =',E12.5,'  RMS =',E12.5)
 
C Permutation of Z(1) and Z(2) when Z(1) larger than Z(2) :
	IF(Z(1).LT.Z(2))THEN
	 IF(TALK) WRITE (6,1080)
	 WRITE (9,1080)
1080	 FORMAT(' Z(1) LARGER THAN Z(2):',
	1	' PERMUTATION OF Z(1) AND Z(2) AND THETA=THETA+PI/2')
	 XZ1=Z(1)
	 XZ2=Z(2)
	 XDEV1=SDEV(1)
	 XDEV2=SDEV(2)
	 Z(1)=XZ2
	 Z(2)=XZ1
	 SDEV(1)=XDEV2
	 SDEV(2)=XDEV1
	 Z(3)=Z(3)+PI/2.0
	ENDIF
 
C----------------------------------------------------------------
C Output of the results :
	WRITE(9,101)
	IF(TALK) WRITE(6,101)
101	FORMAT(/,4X,'MAJOR AXIS',4X,'MEAN RADIUS',4X,'E NUMBER',6X,
	1	'XCENT',8X,'YCENT',7X,'THETA')
	WRITE(9,1088)
	IF(TALK) WRITE(6,1088)
1088	FORMAT(5X,'(ARCSEC)',57X,'(DEGREES)')
 
	XMAJAX=Z(1)
	XMINAX=Z(2)
	THETA=Z(3)
	SMJ=SDEV(1)
	SMN=SDEV(2)
	STH=SDEV(3)*180./PI
	IF(STH.GT.360.)STH=360.
	SXC=SDEV(4)
	SYC=SDEV(5)
 
	RAD=SQRT(XMAJAX*XMINAX)
	ECC=10.*(XMAJAX-XMINAX)/XMAJAX
	PCMAJ=SMJ/XMAJAX
	PCMIN=SMN/XMINAX
	PCRAD=ABS(PCMAJ)+ABS(PCMIN)
	SRAD=0.5*RAD*PCRAD
	SECC=ECC*PCRAD
 
C Conversion in arcseconds (Major axis and its error, Radius and its error) :
	XMAJAX=XMAJAX*SCALE
	SMAJAX=SMJ*SCALE
	RAD=RAD*SCALE
	SRAD=SRAD*SCALE
 
C Conversion in degrees (Theta and its error)
999	IF(THETA.LE.PI) GO TO 998
	THETA=THETA-PI
	GO TO 999
998	IF(THETA.GT.0.) GO TO 997
	THETA=THETA+PI
	GO TO 998
997	THETA=THETA*180./PI
 
C Output of the values :
	IF(TALK) WRITE(6,102) XMAJAX,RAD,ECC,Z(4),Z(5),THETA
	IF(TALK) WRITE(6,103) SMAJAX,SRAD,SECC,SXC,SYC,STH
	WRITE(9,102) XMAJAX,RAD,ECC,Z(4),Z(5),THETA
	WRITE(9,103) SMAJAX,SRAD,SECC,SXC,SYC,STH
102	FORMAT(2X,6(F12.4,1X))
103	FORMAT(2X,6(2X,'+/-',F8.4))
 
	RETURN
	END
C*******************************************************
	SUBROUTINE ERPARM(FSUMSQ,MFLAG,NPARAM,TALK)
C
C  This routine works out the formal errors on the fit by
C  inverting the Hessian matrix.
C
	IMPLICIT REAL*8(A-H,P-Z)
        PARAMETER (MAXPOINTS=2000)
	REAL*8 A(6,5),B(5,5),Z(5),WORK(5),FSUMSQ,SDEV(5)
	REAL*8 AJAC(MAXPOINTS,5),FVECC(MAXPOINTS)
	LOGICAL*4 TALK
	INTEGER*4 NPOINT,IFLG
	COMMON/FIT1/Z,NPOINT,IFLG
	COMMON/EPRM/SDEV
	MFLAG=0
 
C Compute the matrix AJAC :
	LJC=MAXPOINTS
	CALL LSFUN2(NPOINT,NPARAM,Z,FVECC,AJAC,LJC)
 
	DO 1 I=1,NPARAM
	 DO 1 J=1,NPARAM
	 SUM=0.0
	  DO K=1,NPOINT
 	  SUM=SUM+AJAC(K,I)*AJAC(K,J)
	  END DO
	 A(I,J)=SUM
1	CONTINUE
 
	IA=6
	IB=5
	IFAIL=1
C  Matrix inversion
	CALL F01ABF(A,IA,NPARAM,B,IB,WORK,IFAIL)
	 IF(IFAIL.NE.0)THEN
	 IF(TALK) WRITE(6,100) IFAIL
	 WRITE(9,100) IFAIL
100	 FORMAT(/,' WARNING : FAILURE WHILE INVERTING THE MATRIX'
     1	       ' TO WORK OUT THE ERRORS',/,
     1       ' ERROR IN F01ABF, IFAIL =',I5,/,
     1        '(ifail=2 means: ill-conditionned matrix)')
	 MFLAG=1
	 RETURN
	 ENDIF
 
	C=FSUMSQ/(NPOINT-NPARAM)
	DO I=1,NPARAM
	 DO J=1,I
	 B(I,J)=B(I,J)*C
	 END DO
	END DO
 
	DO I=1,5
	SDEV(I)=DSQRT(DABS(B(I,I)))
	END DO
 
	IF((SDEV(1).GT.10.).OR.(SDEV(2).GT.10.)
	1	.OR.(SDEV(4).GT.10.).OR.(SDEV(5).GT.10.))THEN
	 IF(TALK) WRITE(6,202)
	 WRITE(9,202)
202	FORMAT(/,' WARNING !! THE ERRORS ARE TOO HIGH TO BE '
	1	'REASONABLE ...')
	MFLAG=2
	ENDIF
 
C	WRITE(9,201)
C201	FORMAT(' ***********COVARIANCE MATRIX***********')
C	DO 4 I=1,NPARAM
C	WRITE(9,200) (B(I,J),J=1,I)
C4	CONTINUE
C200	FORMAT(' ',5E14.7)
 
	RETURN
	END
C*******************************************************************
	SUBROUTINE ECALC(KK)
C
C  This tidies up the ellipse fit output and writes it to unit 9 and 11.
C Unit 11 is used by FITELLI to generate catalogues of parameters
	REAL*4 MAJAX,MINAX,RAD,ECC,THETA
        REAL*4 SMAJAX,SRAD,SECC,SXC,SYC,STH
	REAL*8 SCALE,Z(5)
        INTEGER*4 KK,NPOINT,IFLG
	COMMON/EDATA/ ESTORE(200,10),FDATA(200,10),NEE
	COMMON/BLOCKC/SCALE
	COMMON/FIT1/Z,NPOINT,IFLG
	PI=3.14159265358979323846
	WRITE(9,100)
100	FORMAT('1',/,8X,
	1	'************* ELLIPSE FIT OUTPUT ************',/)
	WRITE(9,101)
101	FORMAT(/,4X,'MAJOR AXIS',4X,'MEAN RADIUS',4X,'E NUMBER',6X,
	1	'XCENT',8X,'YCENT',7X,'THETA')
	WRITE(9,1088)
1088	FORMAT(5X,'(ARCSEC)',57X,'(DEGREES)')
 
C NEE : Number of levels
	WRITE(11,*)NEE
 
	DO 1 I=1,NEE
          CALL CONVERT_EFIT(I,MAJAX,RAD,ECC,Z(4),Z(5),THETA,
     1                       SMAJAX,SRAD,SECC,SXC,SYC,STH)

C Output of the values :
	  WRITE(9,102) MAJAX,RAD,ECC,Z(4),Z(5),THETA
	  WRITE(9,103) SMAJAX,SRAD,SECC,SXC,SYC,STH
	  WRITE(11,102) MAJAX,RAD,ECC,Z(4),Z(5),THETA
	  WRITE(11,102) SMAJAX,SRAD,SECC,SXC,SYC,STH
1	CONTINUE
102	FORMAT(2X,6(F12.4,1X))
103	FORMAT(2X,6(2X,'+/-',F8.4))

C Last index of array ESTORE (needed by EFIT6.FOR)
        KK=NEE
        RETURN
	END
C************************************************************
	SUBROUTINE RECSER
C
C  This routine Fourier analyses the residuals of the contour
C  from the fitted ellipse.
C  Cf David Carter's thesis
C
	IMPLICIT REAL*8 (Z)
        PARAMETER (MAXPOINTS=2000)
	REAL*8 Z(5)
	INTEGER*4 NPOINT,IFLG
	COMMON/FIT1/Z,NPOINT,IFLG
	COMMON/FOU1/RRR,A,AA(4,2)
	COMMON/BLOCKB/X(MAXPOINTS),Y(MAXPOINTS)
	REAL*4 R(MAXPOINTS+1),TH(MAXPOINTS+1),DELTH(MAXPOINTS+1)
	REAL*4 AVTH(MAXPOINTS+1),AVR(MAXPOINTS+1)

	PI=3.14159265358979323846
	AAA=Z(1)
	BBB=Z(2)
	THETA=Z(3)
	TCS=COS(THETA)
	TSN=SIN(THETA)
	RRR=SQRT(AAA*BBB)
C
C First transform the coordinates of the points
C to a coordinate system in which the fitted
C ellipse is a circle of unit radius.
C
C Calculate radius of the contour as a function of azimuthal
C angle.
C
	DO 2 I=1,NPOINT
C Reset the origin to the center of the ellipse:
          XXX=X(I)-Z(4)
	  YYY=Y(I)-Z(5)
C Rotation of theta (ie new origin for the phases is the major axis):
	  XSTAR=XXX*TCS+YYY*TSN
	  YSTAR=YYY*TCS-XXX*TSN
C Normalisation to major and minor axis (ie ellipse becomes a circle):
	  XSTAR=XSTAR/AAA
	  YSTAR=YSTAR/BBB
C Here are the coordinates of the current point in
C the new system (polar coordinates)
	  R(I)=SQRT(XSTAR*XSTAR+YSTAR*YSTAR)
	  TH(I)=ANGLE0(XSTAR,YSTAR,R(I))
2	CONTINUE

	R(NPOINT+1)=R(1)
	TH(NPOINT+1)=TH(1)
	A=0.
C
C  Notice that the contour can go either way round, i.e. the
C  average DELTH can be positive or negative
C
	DO 3 I=1,NPOINT

C Check the difference in phase between two consecutive points
	DELTH(I)=TH(I+1)-TH(I)
	IF(DELTH(I).LT.-PI)THEN
	  ISWCH=1
	ELSEIF(DELTH(I).GT.PI)THEN
	  ISWCH=2
        ELSE
	  ISWCH=0
	ENDIF

C Now computes the average of the phases:
	AVTH(I)=(TH(I+1)+TH(I))/2.

C The following is simply to get the right value for the
C phases (and get rid of the dependence modulo 2*PI)
	IF(ISWCH.EQ.0) GO TO 8
	IF(ISWCH.EQ.2) GO TO 11
	DELTH(I)=DELTH(I)+2.*PI
12	IF(AVTH(I).GE.PI) GO TO 9
	IF(AVTH(I).LT.-PI) AVTH(I)=AVTH(I)+PI
	GO TO 10
11	DELTH(I)=DELTH(I)-2.*PI
	GO TO 12
9	AVTH(I)=AVTH(I)-PI
10	CONTINUE
8	AVR(I)=(R(I+1)+R(I))/2.
	A=A+DELTH(I)*AVR(I)
3       CONTINUE

	A=A/(2.*PI)
C
C  The sum for the Fourier series is formed in the arrray AA,
C  which goes from 1 to 4; Fourier components up to 4th order
C  are calculated.
C
	DO 4 J=1,2
	DO 4 K=1,4
	  AA(K,J)=0.0
4       CONTINUE

	DO 5 K=1,4
	DO 5 I=1,NPOINT
	  AA(K,1)=AA(K,1)+DELTH(I)*AVR(I)*SIN(K*AVTH(I))
	  AA(K,2)=AA(K,2)+DELTH(I)*AVR(I)*COS(K*AVTH(I))
5       CONTINUE

	DO 6 K=1,4
	DO 6 J=1,2
 	  AA(K,J)=AA(K,J)/PI
6       CONTINUE
C
C  Now if the zero order component (A) is negative (it should be
C  about +1) then all the others will be of the wrong sign too.
C  So reverse them.
C
	IF(A.LT.0.0) THEN
	  DO  K=1,4
	    DO  J=1,2
	      AA(K,J)=-AA(K,J)
	    END DO
	  END DO
	  A=-A
	END IF

	RETURN
	END
C-------------------------------------------------------------------------
	SUBROUTINE FFCALC
C
C  This routine tidies up and outputs the Fourier output.
C
	REAL*8 SCALE
	COMMON/EDATA/ ESTORE(200,10),FDATA(200,10),NEE
	COMMON/BLOCKC/SCALE
C Opens new file for Fourier output:
        OPEN(4,FILE='fitelli.fou',STATUS='UNKNOWN')
	WRITE(4,200)
200	FORMAT(/,' *******************  FOURIER COMPONENT OUTPUT  *******'
     :,'*********')
	WRITE(4,201)
201	FORMAT(/,' RADIUS ZERO ORDER  FIRST ORDER  SECOND ORDER'
     :,'   THIRD ORDER     FOURTH ORDER',/,' (ARCSEC)',6X,
     :4('MAGNITUDE PHASE '))
C
C  First write the fourier components out as amplitudes and phases.
C
	DO 1 I=1,NEE
	 RRR=FDATA(I,1)
	 A0=FDATA(I,2)
	 AA1=FDATA(I,3)
	 BB1=FDATA(I,4)
	 AA2=FDATA(I,5)
	 BB2=FDATA(I,6)
	 AA3=FDATA(I,7)
	 BB3=FDATA(I,8)
	 AA4=FDATA(I,9)
	 BB4=FDATA(I,10)
	 AMAG1=SQRT(AA1**2+BB1**2)
	 AMAG2=SQRT(AA2**2+BB2**2)
	 AMAG3=SQRT(AA3**2+BB3**2)
	 AMAG4=SQRT(AA4**2+BB4**2)
	 APH1=0.
	 APH2=0.
	 APH3=0.
	 APH4=0.
	 IF(BB1.NE.0)APH1=ATAN(AA1/BB1)
	 IF(BB2.NE.0)APH2=ATAN(AA2/BB2)
	 IF(BB3.NE.0)APH3=ATAN(AA3/BB3)
	 IF(BB4.NE.0)APH4=ATAN(AA4/BB4)
	
	 RRR=RRR*SCALE
	 WRITE(4,202) RRR,A0,AMAG1,APH1,AMAG2,
     1      APH2,AMAG3,APH3,AMAG4,APH4
202	FORMAT(' ',F7.2,1X,F5.3,9(1X,F7.4))
1	CONTINUE
C
	WRITE(4,203)
203	FORMAT(/,' RADIUS ZERO ORDER FIRST ORDER  SECOND ORDER'
     :,'    THIRD ORDER     FOURTH ORDER',/,' (ARCSEC)',6X,
     :4(' SINE   COSINE  '))
C
C  Then write the data out as sine and cosine components.
C
	DO 11 I=1,NEE
  	 RRR=FDATA(I,1)
	 A0=FDATA(I,2)
	 AA1=FDATA(I,3)
	 BB1=FDATA(I,4)
	 AA2=FDATA(I,5)
	 BB2=FDATA(I,6)
	 AA3=FDATA(I,7)
	 BB3=FDATA(I,8)
	 AA4=FDATA(I,9)
	 BB4=FDATA(I,10)
	 RRR=RRR*SCALE
	 WRITE(4,202) RRR,A0,AA1,BB1,AA2,BB2,AA3,BB3,AA4,BB4
11	CONTINUE

	CLOSE(4)
	RETURN
	END
C----------------------------------------------------------------------
	FUNCTION ANGLE0(X,Y,R)
C  Works out position angle given X,Y (when the centre is at the
C  origin !!). Position angles measured from the positive X axis through
C  the positive Y axis. (Radians)
C  (to compute Fourier components)
	REAL*4 ANGLE0,PI,X,Y,R
	PI=3.14159265358979323846
	IF(Y)6,2,1
1	ANGLE0=ACOS(X/R)
	RETURN
2	IF(X)5,3,4
3	WRITE(9,100)
	ANGLE0=0.0
	RETURN
4	ANGLE0=0.0
	RETURN
5	ANGLE0=PI
	RETURN
6	ANGLE0=ACOS(-X/R)+PI
	RETURN
100	FORMAT(' ANGLE0 CALLED AT CENTRE POINT')
	END
C******************************************************************
C SUBROUTINE INPARS(NPARAM)
C Set up initial parameters for first guess at the fit
C
C*******************************************************************
	SUBROUTINE INPARS(NPARAM)
        PARAMETER (MAXPOINTS=2000)
        REAL*8 Z(5),PI,THEMIN,THEMAX,X0,Y0
	REAL*8 TMIN,TMAX,RMIN,RMAX,THETA
        INTEGER*4 NPOINT,IFLG
	COMMON/FIT1/Z,NPOINT,IFLG
	COMMON/BLOCKA/ PI,THEMIN,THEMAX
	COMMON/FIXED_CENT/X0,Y0
 
C Take the center of the galaxy as the first guess for the
C position of the centre :
        Z(4)=X0
        Z(5)=Y0
 
	CALL THETA_MIN_MAX(TMIN,TMAX,RMIN,RMAX,THETA,Z(4),Z(5)) 

C  Initial parameters in array Z :
	Z(1)=RMAX
	Z(2)=RMIN
	Z(3)=THETA
        THEMIN=TMIN
        THEMAX=TMAX
	WRITE(9,1000) (Z(I),I=1,5)
1000    FORMAT(/,' FIRST GUESS : MAJAX (PIXELS), MINAX (PIXELS)',
	1	' THETA (RADIANS), OX, OY :',
	1	/,2X,5(E12.5,2X))
 
	RETURN
	END
 
C******************************************************************
C SUBROUTINE THETA_MIN_MAX 
C Compute the limiting angles TMIN,TMAX in radians, 
C given a center (XC,YC) in pixel coordinates
C
C THETA is the first guess for the position angle 
C       (estimated with the maximum radius)
C
C*******************************************************************
	SUBROUTINE THETA_MIN_MAX(TMIN,TMAX,RMIN,RMAX,THETA,XC,YC) 
        PARAMETER (MAXPOINTS=2000)
	REAL*8 Z(5),PI,TMIN,TMAX,XC,YC,RMIN,RMAX,THETA
        REAL*4 X1,Y1,R,DELTATHETA
	REAL*4 X,Y,TMIN1,TMAX1,TMIN2,TMAX2,THETA1,THETA2,TMID,TMAD
	INTEGER*4 NPOINT,IFLG
	COMMON/FIT1/Z,NPOINT,IFLG
	COMMON/BLOCKB/X(MAXPOINTS),Y(MAXPOINTS)
 
	PI=3.14159265358979323846D0
	RMIN=1.D10
	RMAX=0.D0
	TMIN1=2.1*PI
	TMIN2=TMIN1
	TMAX1=-2.1*PI
	TMAX2=TMAX1
 
C  Find the point on the contour which is furthest from the guess
C  at the centre.
	DO 2 I=1,NPOINT
	X1=X(I)-XC
	Y1=Y(I)-YC
	R=SQRT(X1**2+Y1**2)
	THETA1=ANGLE0(X1,Y1,R)
	TMIN1=AMIN1(THETA1,TMIN1)
	TMAX1=AMAX1(THETA1,TMAX1)
 
	THETA2=THETA1-PI
	IF(THETA2.LT.0.)THETA2=THETA2+2*PI
 
	TMIN2=AMIN1(THETA2,TMIN2)
	TMAX2=AMAX1(THETA2,TMAX2)

	IF(RMIN.GT.R)RMIN=R
	  IF(R.GT.RMAX) THEN
	    RMAX=R
	    THETA=THETA1
	  ENDIF
2	CONTINUE
 
C Test if  DELTATHETA is larger than 340 degrees :
	DELTATHETA=TMAX1-TMIN1
	IF(DELTATHETA.GT.5.93)THEN
	  TMIN=TMIN2-PI
	  TMAX=TMAX2-PI
	ELSE
	  TMIN=TMIN1
	  TMAX=TMAX1
	ENDIF
 
	TMID=TMIN*180./PI
	TMAD=TMAX*180./PI
	WRITE(9,1001)TMID,TMAD
1001	FORMAT('THETA MIN, THETA MAX :',
     1          2X,F8.3,2X,F8.3)
 
	RETURN
	END
C**********************************************************************
C
C ESTORE(K,1) : Major axis (pixels)
C ESTORE(K,2) : Minor axis (pixels)
C ESTORE(K,3) : Theta (radians)
C ESTORE(K,4) : XC (pixels)
C ESTORE(K,5) : YC (pixels)
C ESTORE(K,6) : Error of major axis (pixels)
C ESTORE(K,7) : Error of minor axis (pixels)
C ESTORE(K,8) : Error of theta (radians)
C ESTORE(K,9) : Error of XC (pixels)
C ESTORE(K,10) : Error of YC (pixels)
C**********************************************************************
        SUBROUTINE CONVERT_EFIT(K,MAJAX,RAD,ECC,XC,YC,THETA,
     1                       SMAJAX,SRAD,SECC,SXC,SYC,STH)
        REAL*4 ESTORE,FDATA,PI
        REAL*4 MAJAX,MINAX,RAD,ECC,XC,YC,THETA
        REAL*4 SMAJAX,SRAD,SECC,SXC,SYC,STH
        REAL*4 PCMAJ,PCMIN,PCRAD
        REAL*8 SCALE
        INTEGER*4 NEE
	COMMON/EDATA/ ESTORE(200,10),FDATA(200,10),NEE
	COMMON/BLOCKC/SCALE

	PI=3.14159265358979323846D0
	  MAJAX=ESTORE(K,1)
	  MINAX=ESTORE(K,2)
	  THETA=ESTORE(K,3)
	  XC=ESTORE(K,4)
	  YC=ESTORE(K,5)
	  SMJ=ESTORE(K,6)
	  SMN=ESTORE(K,7)
	  STH=ESTORE(K,8)*180./PI
	  SXC=ESTORE(K,9)
	  SYC=ESTORE(K,10)

	  RAD=SQRT(MAJAX*MINAX)
	  ECC=10.*(MAJAX-MINAX)/MAJAX
	  PCMAJ=SMJ/MAJAX
	  PCMIN=SMN/MINAX
	  PCRAD=ABS(PCMAJ)+ABS(PCMIN)
	  SRAD=0.5*RAD*PCRAD
	  SECC=ECC*PCRAD
 
C Conversion to arcseconds (Major axis and its error, Radius and its error) :
          MAJAX=MAJAX*SCALE
          SMAJAX=SMJ*SCALE
          RAD=RAD*SCALE
          SRAD=SRAD*SCALE

C Conversion to degrees (Theta and its error)
999         IF(THETA.LE.PI) GO TO 998
          THETA=THETA-PI
          GO TO 999
998         IF(THETA.GT.0.) GO TO 997
          THETA=THETA+PI
          GO TO 998
997       THETA=THETA*180./PI

         RETURN
         END
