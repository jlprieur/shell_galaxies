C++***************************************************************
C Program to compute atmospheric refraction versus wavelength
C From Simon, W. 1966, A.J. 71,190
C
C
C JLP
C Version of 10-09-91
C--***************************************************************
	PROGRAM ATM_REFRAC
	PARAMETER (IDIM=1024)
	REAL*4 CC(15),QQ,LAMBDA(IDIM),AD(IDIM)
	CHARACTER OUTFILE*40,BUFFER*80
	DATA CC/3.45020E-03,-3.34591E-04,-1.60149E-05,-1.54316,
     1	2.27095E-01,3.14759E-03,2.87409E-03,-2.92730E-04,
     1	-1.56673E-05,1.41299E-03,-2.22307E-04,-1.66610E-06,
     1	-3.34814E-05,5.33352E-06,3.52107E-08/
	DATA QQ/0.740568/
 
	WRITE(6,35)
35	FORMAT(' Program ATM_REFRAC to compute atmospheric'
     1	' refraction',/,
     1	' From Simon, W. 1966, A.J. 71,190')
 
	WRITE(6,36)
36	FORMAT(' Enter P (mm Hg, atm. pressure), F (mm, water vapor),',
     1	/,' T (degree C, temperature), Zenith angle (degrees) :')
	READ(5,*) PP,FF,TT,ZD
 
	WRITE(6,37)
37	FORMAT(' Output file:')
	READ(5,10) OUTFILE
10	FORMAT(A)
 
	ZZ=TAND(ZD)
	Z2=ZZ*ZZ
	Z3=Z2*ZZ
	Z4=Z2*Z2
 
	SS=PP-0.148238*FF+(1.049-0.0157*TT)*PP*PP*1.E-06
	SS=SS/(720.883*(1.+0.003661*TT))
	SQ=SS/QQ
 
	NPTS=66
	STEP=(1-0.35)/FLOAT(NPTS-1)
	DO I=1,NPTS
C First Lambda in microns (for the formulae..)
	  LAMBDA(I)=0.35+STEP*FLOAT(I-1)
	  YY=1./(LAMBDA(I)*LAMBDA(I))
	  Y2=YY*YY
C Atmospheric dispersion in arcsec
C (From Simon, W. 1966, A.J. 71,190)
	  AD(I)=CC(1)+CC(2)*YY+CC(3)*Y2+CC(4)*ZZ+CC(5)*ZZ*YY
     1	+CC(6)*ZZ*Y2+CC(7)*Z2+CC(8)*Z2*YY+CC(9)*Z2*Y2
     1	+CC(10)*Z3+CC(11)*Z3*YY+CC(12)*Z3*Y2+CC(13)*Z4
     1	+CC(14)*Z4*YY+CC(15)*Z4*Y2
	  AD(I)=AD(I)*SQ
C Conversion to nm:
	  LAMBDA(I)=LAMBDA(I)*1000.
	END DO
 
C Output of the data:
	OPEN(1,FILE=OUTFILE,STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
	WRITE(BUFFER,41) LAMBDA(1),AD(1),PP,FF,TT,ZD
41	FORMAT(F10.4,X,G13.6,5X,4(F10.4,X))
	WRITE(1,10) BUFFER
	DO I=2,NPTS
	  WRITE(1,42) LAMBDA(I),AD(I)
42	  FORMAT(F10.4,X,G13.6)
	END DO
	CLOSE(1)
 
	STOP
	END
