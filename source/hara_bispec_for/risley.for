C++***************************************************************
C Program to compute the residual dispersion
C of Risley prisms (to correct atmospheric dispersion) 
C Used for computing the angle of Risley prisms for the TBL speckle camera.
C
C JLP
C Version of 10-03-91
C--***************************************************************
	PROGRAM RISLEY 
	PARAMETER (IDIM=1024)
	REAL*4 LAMBDA(IDIM),AD(IDIM)
	REAL*4 INDEX1(IDIM),INDEX2(IDIM),RESIDUAL(IDIM)
        REAL*4 PP,FF,TT,ZD,BETA1,BETA2,CLAMBDA,DLAMBDA
	CHARACTER OUTFILE*40,BUFFER*90

	WRITE(6,35)
35	FORMAT(' Program RISLEY to compute Risley prisms',
     1  ' for the T2M speckle camera',/) 

	WRITE(6,36)
36	FORMAT(' Enter P (mm Hg, atm. pressure), F (mm, water vapor),',
     1	/,' T (degree C, temperature), Zenith angle (degrees) :')
	READ(5,*) PP,FF,TT,ZD
 
	WRITE(6,39)
39	FORMAT(' Enter roof angle of F4 and SK10 prisms: (degrees)') 
	READ(5,*) BETA1,BETA2 
	BETA1=BETA1*3.1415926/180.
	BETA2=BETA2*3.1415926/180.
 
	WRITE(6,40)
40	FORMAT(' Enter central lambda and delta lambda (nm):') 
	READ(5,*) CLAMBDA,DLAMBDA
	CLAMBDA=CLAMBDA/1000.
	DLAMBDA=DLAMBDA/1000.
 
	WRITE(6,48)
48	FORMAT(' Enter crossing angle between the two sets of',
     1      ' prisms: (degrees)') 
	READ(5,*) CROSS_ANGLE 
	CANGLE=(CROSS_ANGLE/2.)*3.1415926/180.

C With 66 points, the step is 10 nm:
	NPTS=66
	STEP=(1-0.35)/FLOAT(NPTS-1)
C First Lambda in microns (for the formulae..)
	DO I=1,NPTS
	  LAMBDA(I)=0.35+STEP*FLOAT(I-1)
        END DO
        CALL INDEX_MAX4(LAMBDA,NPTS,CLAMBDA,KCENT)
	WORK1=CLAMBDA-DLAMBDA/2.
        CALL INDEX_MAX4(LAMBDA,NPTS,WORK1,K1)
	WORK2=CLAMBDA+DLAMBDA/2.
        CALL INDEX_MAX4(LAMBDA,NPTS,WORK2,K2)
        PRINT *,' K',KCENT,K1,K2

	WRITE(6,37)
37	FORMAT(' Output file:')
	READ(5,10) OUTFILE
10	FORMAT(A)

C Computing the indices of the prisms:
	CALL PRISM_INDEX(INDEX1,INDEX2,LAMBDA,NPTS,IDIM) 
        IF(I.EQ.1234)THEN
	  OPEN(3,FILE='f4_sk10.dat',STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
          DO I=1,NPTS
	    WRITE(3,65) LAMBDA(I),INDEX1(I),INDEX2(I)
65	    FORMAT(3(F8.6,2X))
          ENDDO
          CLOSE(3) 
          PRINT *,' Output of the index file f4_sk10.dat (f4 and sk10)'
        ENDIF

C Computing the atmospheric dispersion in arcseconds:
	CALL ATM_REFRAC1(PP,FF,TT,ZD,LAMBDA,AD,NPTS,IDIM)

C Translating atm. dispersion:
	WORK1=AD(KCENT)
	DO I=1,NPTS
	  AD(I)=AD(I)-WORK1
        END DO

C Computing residual dispersion:
        RADTOSEC=180.*3600./3.1415926
	WORK3=2.*COS(CANGLE)*RADTOSEC/200.
        DO I=1,NPTS
	  WORK1=(INDEX1(I)-1.)*SIN(BETA1)
	  WORK2=(INDEX2(I)-1.)*SIN(BETA2)
C For the T2M speckle camera, the magnification factor is 200:
	  RESIDUAL(I)=AD(I)-(WORK1-WORK2)*WORK3
C	  IF(I.LT.20)THEN
C	    PRINT *,AD(I),WORK1,WORK2,RESIDUAL(I)
C	  ENDIF
        ENDDO 

C Computing mean deviation and residual dispersion:
	WORK1=SQRT(WORK1)*(180./3.1415926)/200.
        WORK2=RESIDUAL(K2)-RESIDUAL(K1)
	WRITE(6,49) RESIDUAL(K2),WORK2
49	FORMAT(' Mean deviation of the beam at',
	1    ' central wavelength (degrees) ',
	1    G12.5,/,' Residual dispersion (arcsec) ',F10.5) 

C Conversion to nm:
	WORK1=RESIDUAL(KCENT)
	DO I=1,NPTS
	  LAMBDA(I)=LAMBDA(I)*1000.
	  RESIDUAL(I)=RESIDUAL(I)-WORK1
        END DO

C Output of the data:
	OPEN(1,FILE=OUTFILE,STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
	WRITE(6,45)
45	FORMAT(' Output of the full dispersion (atmos + prism)',
     1    ' in arcsec',/,' lambda, resid, dispers., F4, SK10')
	WRITE(BUFFER,41) LAMBDA(1),RESIDUAL(1),AD(1),INDEX1(1),INDEX2(1),
	1      PP,FF,TT,ZD
41	FORMAT(F8.2,4(1X,F7.4),5X,F8.2,1X,3(F5.1,1X))
	WRITE(1,10) BUFFER
	DO I=2,NPTS
	  WRITE(1,42) LAMBDA(I),RESIDUAL(I),AD(I),INDEX1(I),INDEX2(I)
42	  FORMAT(F8.2,4(1X,F7.4))
	END DO
	CLOSE(1)

	STOP
	END
C****************************************************************
C SK10 and F4
C****************************************************************
	SUBROUTINE PRISM_INDEX(INDEX1,INDEX2,LAMBDA,NPTS,IDIM) 
	REAL*4 F4(6),SK10(6)
	REAL*4 LAMBDA(IDIM),INDEX1(IDIM),INDEX2(IDIM)
	DATA F4/2.54469,-8.5925665E-03,2.2583116E-02,
     1  7.3789910E-04,-9.5060668E-06,3.82577E-08/
	DATA SK10/2.588171,-9.3042171E-03,1.6075769E-02,
     1  2.2083748E-04,3.5467529E-06,2.6143582E-07/
 
	DO I=1,NPTS
	  Y2=LAMBDA(I)*LAMBDA(I)
	  YY=1./Y2
C Index:
	  INDEX1(I)=F4(1)+F4(2)*Y2+F4(3)*YY+F4(4)*YY*YY
     1  +F4(5)*YY*YY*YY+F4(6)*YY*YY*YY*YY
	  INDEX1(I)=SQRT(INDEX1(I))
	  INDEX2(I)=SK10(1)+SK10(2)*Y2+SK10(3)*YY+SK10(4)*YY*YY
     1  +SK10(5)*YY*YY*YY+SK10(6)*YY*YY*YY*YY
	  INDEX2(I)=SQRT(INDEX2(I))

	END DO
 
	RETURN
	END
C***************************************************************
C Routine to compute atmospheric refraction versus wavelength
C From Simon, W. 1966, A.J. 71,190
C--***************************************************************
	SUBROUTINE ATM_REFRAC1(PP,FF,TT,ZD,LAMBDA,AD,NPTS,IDIM)
	REAL*4 CC(15),LAMBDA(IDIM),AD(IDIM)
        REAL*4 PP,FF,TT,ZD,ZD_RAD,PI
        REAL*4 ZZ,Z2,Z3,Z4,SS,SQ,QQ,YY,Y2
	DATA CC/3.45020E-03,-3.34591E-04,-1.60149E-05,-1.54316,
     1	2.27095E-01,3.14759E-03,2.87409E-03,-2.92730E-04,
     1	-1.56673E-05,1.41299E-03,-2.22307E-04,-1.66610E-06,
     1	-3.34814E-05,5.33352E-06,3.52107E-08/
	DATA QQ/0.740568/
 
	WRITE(6,35)
35	FORMAT(' Computing atmospheric refraction',/,
     1	' From Simon, W. 1966, A.J. 71,190, h0=2811m, p0=547mm, T0=6deg')
C	PRINT *,' PP,FF,TT,ZD',PP,FF,TT,ZD
 
        PI = 3.14159265
        ZD_RAD = ZD*PI/180.0
	ZZ=TAN(ZD_RAD)
	Z2=ZZ*ZZ
	Z3=Z2*ZZ
	Z4=Z2*Z2
 
	SS=PP-0.148238*FF+(1.049-0.0157*TT)*PP*PP*1.E-06
	SS=SS/(720.883*(1.+0.003661*TT))
	SQ=SS/QQ
 
	DO I=1,NPTS
	  YY=1./(LAMBDA(I)*LAMBDA(I))
	  Y2=YY*YY
C Atmospheric dispersion in arcsec
C (From Simon, W. 1966, A.J. 71,190)
	  AD(I)=CC(1)+CC(2)*YY+CC(3)*Y2+CC(4)*ZZ+CC(5)*ZZ*YY
     1	+CC(6)*ZZ*Y2+CC(7)*Z2+CC(8)*Z2*YY+CC(9)*Z2*Y2
     1	+CC(10)*Z3+CC(11)*Z3*YY+CC(12)*Z3*Y2+CC(13)*Z4
     1	+CC(14)*Z4*YY+CC(15)*Z4*Y2
	  AD(I)=AD(I)*SQ
	END DO
 
	RETURN
	END
C***************************************************************
C Routine to compute atmospheric refraction versus wavelength
C From Simon, W. 1966, A.J. 71,190
C--***************************************************************
	SUBROUTINE ATM_REFRAC2(PP,FF,TT,ZD,LAMBDA,AD,NPTS,IDIM)
	REAL*4 CC(15),LAMBDA(IDIM),AD(IDIM)
        REAL*4 PP,FF,TT,ZD,ZD_RAD,PI
        REAL*4 ZZ,Z2,Z3,Z4,SS,SQ,QQ,YY,Y2
	DATA CC/3.45020E-03,-3.34591E-04,-1.60149E-05,-1.54316,
     1	2.27095E-01,3.14759E-03,2.87409E-03,-2.92730E-04,
     1	-1.56673E-05,1.41299E-03,-2.22307E-04,-1.66610E-06,
     1	-3.34814E-05,5.33352E-06,3.52107E-08/
	DATA QQ/0.740568/
 
	WRITE(6,35)
35	FORMAT(' Computing atmospheric refraction',/,
     1	' From Simon, W. 1966, A.J. 71,190, h0=2811m, p0=547mm, T0=6deg')
C	PRINT *,' PP,FF,TT,ZD',PP,FF,TT,ZD
 
        PI = 3.14159265
        ZD_RAD = ZD*PI/180.0
	ZZ=TAN(ZD_RAD)
	Z2=ZZ*ZZ
	Z3=Z2*ZZ
	Z4=Z2*Z2
 
	SS=PP-0.148238*FF+(1.049-0.0157*TT)*PP*PP*1.E-06
	SS=SS/(720.883*(1.+0.003661*TT))
	SQ=SS/QQ
 
	DO I=1,NPTS
	  YY=1./(LAMBDA(I)*LAMBDA(I))
	  Y2=YY*YY
C Atmospheric dispersion in arcsec
C (From Simon, W. 1966, A.J. 71,190)
	  AD(I)=CC(1)+CC(2)*YY+CC(3)*Y2+CC(4)*ZZ+CC(5)*ZZ*YY
     1	+CC(6)*ZZ*Y2+CC(7)*Z2+CC(8)*Z2*YY+CC(9)*Z2*Y2
     1	+CC(10)*Z3+CC(11)*Z3*YY+CC(12)*Z3*Y2+CC(13)*Z4
     1	+CC(14)*Z4*YY+CC(15)*Z4*Y2
	  AD(I)=AD(I)*SQ
	END DO
 
	RETURN
	END
