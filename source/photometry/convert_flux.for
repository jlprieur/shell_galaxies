C++**********************************************************************
C CONVERT_FLUX
C Convert magnitudes to F_lambda (erg/s/cm2/A)
C
C JLP
C Version 29-06-90
C++**********************************************************************
	PROGRAM CONVERT_FLUX
	DOUBLE PRECISION XLAMBDA,XMAGNI,FLAMBDA,CLIGHT
	CLIGHT= 2.9979E+18
 
	OPEN(1,FILE='convert_flux.log',STATUS='unknown')
 
	PRINT 80
80	FORMAT(' CONVERT_FLUX Version of 15-03-89',/,
     1	' 1. Conversion Magnitude to Flambda',
     1	' (Stone et Baldwin 1983, MNRAS 204, 347)',/)
	WRITE(1,80)
	 WRITE(1,69)
69	 FORMAT('  LAMBDA (A)       MAGNITUDE          FLUX   (erg/s/cm2/A)')
 
C Starting the loop:
 
66	PRINT *,' Lambda (A) and magnitude : (0,0 to exit)'
	READ(5,*) XLAMBDA,XMAGNI
	IF((XLAMBDA.NE.0).AND.(XMAGNI.NE.0)) THEN
	 FLAMBDA= CLIGHT * (10**(-0.4*(XMAGNI+48.595))) /(XLAMBDA**2)
	 WRITE(6,70) XLAMBDA,XMAGNI,FLAMBDA
	 WRITE(1,70) XLAMBDA,XMAGNI,FLAMBDA
70	 FORMAT(2X,F12.4,2X,F12.4,2X,G14.6)	
	 GOTO 66
	ENDIF
 
	CLOSE(1)
        PRINT *,' Output in "convert_flux.log"'
	END
