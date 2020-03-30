C++--------------------------------------------------------------------------
C  PROGRAM TO COMPUTE THE PERIOD/ENERGY/RMAX RELATION FOR A GIVEN POTENTIAL
C
C  Initially called "DVHALO"
C
C  P.J.QUINN FEB.82
C
C--------------------------------------------------------------------------
	PROGRAM DVHALO
        IMPLICIT DOUBLE PRECISION (A-H,O-Y)
        DIMENSION ZEN(500,2),ZPERIOD(500,2),ZRMAX(500,2),NPTS(2)
        DIMENSION WN(500),X(500)
        DIMENSION ZRESID(500)
C
        TOL = 0.001
 
        WRITE(6,1)
   1    FORMAT('0')
        WRITE(6,2)
   2    FORMAT(1X,'PE>> SUPPLY GM1  AND GAMMA1             : '$)
        READ(5,*)GM1,GAMMA1
        WRITE(6,330)
 330    FORMAT(1X,'PE>> SUPPLY HALO CUTOFF  : RCUT         : '$)
        READ(5,*)RCUT
        CHI=RCUT/GAMMA1
C        WRITE(6,100)
C 100    FORMAT(1X,'PE>> SUPPLY GM2 AND GAMMA2              : '$)
C        READ(5,*)GM2,GAMMA2
	GM2=1.
	GAMMA2=1.
 331    WRITE(9,*)GM1,GAMMA1,CHI,GM2,GAMMA2
        RZ=0.
        CALL POT4(RZ,GM1,GAMMA1,CHI,GM2,GAMMA2,PZ)
        WRITE(6,98)PZ
  98    FORMAT(1X,'PE>> CENTRAL POTENTIAL VALUE IS      : ',F12.6)
 
        WRITE(6,300)
 300    FORMAT(1X,'PE> MIN AND MAX RADII VALUES OF INTEREST',
     1	' (IN Re UNITS): '$)
        READ(5,*)RIN,ROUT
        CALL POT4(RIN,GM1,GAMMA1,CHI,GM2,GAMMA2,EMIN)
        CALL POT4(ROUT,GM1,GAMMA1,CHI,GM2,GAMMA2,EMAX)
        WRITE(6,301)EMIN,EMAX
 301    FORMAT(1X,'PE> POTENTIAL AT RMIN, RMAX  :',F12.6,X,F12.6)
 
C       WRITE(6,3)
C  3    FORMAT(1X,'PE>> SUPPLY ENERGY RANGE (MIN TO MAX) :'$)
C        READ(5,*)EMIN,EMAX
C        WRITE(6,4)
C   4    FORMAT(1X,'PE>>        # ENERGY INCREMENTS      : '$)
C        READ(5,*)NE
	NE=100
        DELE=(EMAX-EMIN)/(FLOAT(NE)+0.1)
C        WRITE(6,6)
C   6    FORMAT(1X,'PE>>        # SUMMATION TERMS        : '$)
C        READ(5,*)N
	N=20
        CALL POLYNM(N,X)
        print *,'>>Legendre fn. and quad. parameters set <<'
        M=2*N
        CALL WEIGHTS(M,X,WN)
        print *,'>>Weights set <<'
        E=EMIN
        DO 10 I =1,NE
        if(mod(i,10).eq.0) write(6,*) i
        ZEN(I,1)=E
        ZEN(I,2)=E
        CALL FINDR4(GM1,GAMMA1,CHI,GM2,GAMMA2,E,TOL,R)
        ZPERIOD(I,2)=3.14159*(10+DSQRT(R*R+100))**1.5
        ZPERIOD(I,2)=ZPERIOD(I,2)/(2.*DSQRT(2.D+0))
        ZRMAX(I,1)=R
        ZRMAX(I,2)=R
        ANS=0.
        DO 20 K=1,N
          XX=(1.-X(K)*X(K))
          XXX=XX*R
          CALL POT4(XXX,GM1,GAMMA1,CHI,GM2,GAMMA2,P)
          FUN=DSQRT((1.-XX)/(E-P))
          ANS=ANS+FUN*2.*WN(K)
  20    CONTINUE
        ZPERIOD(I,1)=R*1.4142*ANS
        ZRESID(I)=ZPERIOD(I,1)-ZPERIOD(I,2)
        E=E+DELE
  10    continue
 103    DO 30 L=1,NE
        WRITE(10,*)ZPERIOD(L,1),ZRMAX(L,1)
  30    continue
        END
C-------------------------------------------------------------------
C
C SUBROUTINE POT4 TO GENERATE POTENTIAL VALUES
C
C-------------------------------------------------------------------
        SUBROUTINE POT4(R,GM1,GAMMA1,CHI,GM2,GAMMA2,P)
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        IF(R.EQ.0.)THEN
          P=-0.5*(GM1/(GAMMA1*(CHI-DATAN(CHI))))*DLOG(1.+CHI*CHI)
          P=P-2.62175
          RETURN
        END IF
        RX=R/GAMMA1
        IF(RX.GT.CHI)THEN
          P=-GM1/R
        ELSE
          T1=0.5*DLOG((1.+R*R/(GAMMA1*GAMMA1))/(1.+CHI*CHI))
          T2=-1.+(1./RX)*DATAN(RX)
          P=(GM1/(GAMMA1*(CHI-DATAN(CHI))))*(T1+T2)
        END IF
          IF(R.GE..1) GO TO 10
        PS=-10.49*(R*DLOG10(R)+.166*R)-2.62175
        P=P+PS
        RETURN
 10       IF(R.GE.1.) GO TO 20
        PS=1.978441+.953731*DLOG10(R)*(1.-.42*DLOG10(R)-
     &     .26*(DLOG10(R))**2)-2.62175
        P=P+PS
        RETURN
 
 20     R14=R**.25
        PS=-1./R+161.68466*R**(5./8.)*DEXP(-7.66924944*R14)*(1.+
     &     1.3259771/R14+2.2593429/(R14**2)+.14729883/(R14**3)
     &     -9.6032102E-3/R+1.8782563E-3/(R*R14)-6.1226858E-4/(R*R14*R14)
     &     )
        P=P+PS
        RETURN
        END
C--------------------------------------------------------------------------
C
C       SUBROUTINE TO GENERATE THE POTENTIAL
C       DERIVATIVE VALUES.
C
C--------------------------------------------------------------------------
        SUBROUTINE DPOTENTIAL4(R,GM1,GAMMA1,CHI,GM2,GAMMA2,DP)
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        IF(R.EQ.0.)THEN
          DP=0.
          RETURN
        END IF
        RX=R/GAMMA1
        IF(RX.LE.CHI)THEN
          T1=(RX/GAMMA1)*(1./(1.+RX*RX))
          T2=-(GAMMA1/(R*R))*DATAN(RX)
          T3=(1./R)*(1./(1.+RX*RX))
          DP=(GM1/(GAMMA1*(CHI-DATAN(CHI))))*(T1+T2+T3)
        ELSE
          DP=GM1/(R*R)
        END IF
          IF(R.GE.1.E-3) GO TO 10
        R14=R**.25
        DP=DP+621.1*R14*(1.-9.4*R14+46.4*R14**2-102.*R14**3)
        RETURN
 10       IF(R.GE..1) GO TO 20
        DP=DP-10.49*(.6+DLOG10(R))
        RETURN
 20       IF(R.GE.1.) GO TO 30
        DP=DP+.4209595*(1.-.84*DLOG10(R)-.78*(DLOG10(R))**2)/R
        RETURN
 30     R14=R**.25
        DP=DP+(1.-310.*R**(11./8.)*DEXP(-7.66924944*R14)*(2.
     &     +R14+R14**2))/(R**2)
        RETURN
        END
C--------------------------------------------------------------------------
C
C       A SUBROUTINE TO COMPUTE THE TURNING RADII OF
C       A GIVEN POTENTIAL BY NEWTONS METHOD
C
C--------------------------------------------------------------------------
        SUBROUTINE FINDR4(GM1,GAMMA1,CHI,GM2,GAMMA2,E,
     1	TOL,R)
C
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c       RI=-(GM1+GM2)/(2.*E)
        RI=0.
        CALL POT4(RI,GM1,GAMMA1,CHI,GM2,GAMMA2,P)
        AA=E-P
        ASIGN=AA/DABS(AA)
  99    RI=RI+0.1*(GAMMA1+gamma2)
        CALL POT4(RI,GM1,GAMMA1,CHI,GM2,GAMMA2,P)
        BB=E-P
        if(bb.eq.0.) bsign=asign
 
        if(bb.ne.0.) BSIGN=BB/DABS(BB)
        IF(ASIGN.NE.BSIGN)GOTO 101
        ASIGN=BSIGN
        GOTO 99
 101    RI=RI-0.1*(GAMMA1+gamma2)/2.
   10   CALL POT4(RI,GM1,GAMMA1,CHI,GM2,GAMMA2,P)
C       CALL DPOTENTIAL(RI,GM,GAMMA,DP)
C       CALL DPOTENTIAL3(RI,GM,GAMMA,CHI,DP)
        CALL DPOTENTIAL4(RI,GM1,GAMMA1,CHI,GM2,GAMMA2,DP)
        DP=-DP
        RI2=RI-(E-P)/DP
C       WRITE(6,*)RI,RI2
        DEL=ABS(RI-RI2)
        IF(DEL.LE.TOL)GOTO 20
        RI=RI2
        GOTO 10
 20     R=RI2
        RETURN
        END
C--------------------------------------------------------------------------
C
C       SUBROUTINE TO GENERATE GAUSSIAN WEIGHTS.
C
C--------------------------------------------------------------------------
        SUBROUTINE WEIGHTS(N,X,WN)
C
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION WN(1),X(1)
C
        DIMENSION P(1000),DP(1000)
C
        FF=FLOAT(N)/2.
        FAC=FF-N/2
        IF(FAC.NE.0.)THEN
          NN=INT(FF)+1
        ELSE
          NN=INT(FF)
        END IF
        DO 50 M=1,NN
          XX=X(M)
          P(1)=1
          P(2)=XX
          DO 10 I=3,N+1
            T1=(2*FLOAT(I-2)+1)*XX*P(I-1)
            T2=FLOAT(I-2)*P(I-2)
	    P(I)=(T1-T2)/FLOAT(I-1)
10	  CONTINUE
C       WRITE(6,*)(P(K),K=1,N+1)
          DP(1)=0
          DO 20 K=2,N+1
            T3=-FLOAT(K-1)*XX*P(K)
            T4=FLOAT(K-1)*P(K-1)
            DP(K)=(1./(1-XX*XX))*(T3+T4)
20	  CONTINUE
C         WRITE(6,*)(DP(K),K=1,N+1)
          WN(M)=2./((1-XX*XX)*DP(N+1)*DP(N+1))
  50    CONTINUE
        RETURN
        END
 
C-----------------------------------------------------------------------
C Subroutine POLYNM
C----------------------------------------------------------------------
        SUBROUTINE POLYNM(NP,XI)
C       *** COMPUTES LEGENDRE FUNCTIONS, AND QUADRATURE PARAMETERS.
        DOUBLE PRECISION X,XJ,PL,DPL,P(257),DP(257),XI(1)
C       DIMENSION XI(1)
        N=NP+NP
        PIN=1.5707963/(N+N+1)
        PL=1D0
        P(1)=1D0
        DP(1)=0D0
        DO 40 MJ=1,NP
        M=NP-MJ+1
        THE=(4*M-1)*PIN
        Y=THE+1.0/(8*N*N*TAN(THE))
        X=COS(Y)
 35     P(2)=X
        DP(2)=1D0-X*X
        IF (N.LE.1) GO TO 20
        DO 30 J=2,N
        XJ=J
        P(J+1)=((J+J-1)*X*P(J)-(J-1)*P(J-1))/XJ
 30     DP(J+1)=XJ*(P(J)-X*P(J+1))
 20     PL=P(N+1)
        DPL=DP(N+1)/(1D0-X*X)
        X=X-PL/DPL
        IF (DABS(PL).GE.1D-9) GO TO 35
        XI(MJ)=X
 40     CONTINUE
        RETURN
        END
