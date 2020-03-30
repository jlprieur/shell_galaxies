C++***************************************************************
C Program spec_extract 
C To extract a spectrum from a 2D image
C Linear interpolation for slice extraction
C Extraction of a spectrum:
C  - from a 2D image containing only one spectrum if rotation is needed
C  - or containing more spectra if horizontal case(please run "resample1" before)')
C
C JLP
C Version of 17-12-98
C--**************************************************************
	PROGRAM SPEC_EXTRACT 
	CHARACTER REP*1,IN_NAME*40,IN_COMMENTS*80
	CHARACTER OUT_NAME*40,OUT_COMMENTS*80
	INTEGER*4 MADRID(1),PNTR_1,PNTR_2
	INTEGER*4 IX1,IX2,IY1,IY2,NX_2,NY_2 
	INTEGER*4 NX_1,NY_1
	COMMON /VMR/MADRID
 
10	FORMAT(A)
 
	CALL JLP_BEGIN
 
	WRITE(6,22)
22      FORMAT(' Program spec_extract  Version of 17-12-98',/,
     1         ' Extraction from a 2D image containing only one spectrum',
     1         ' if rotation is applied',/,
     1         ' or more spectra if horizontal (please run "resample1" before)')
        WRITE(6,*) ' JLP99: Inversion of wavelengths for speckle data'
 
C Inquires about the format of the files :
	CALL JLP_INQUIFMT
 
C************************************************************
C Loading input image file
C************************************************************
        WRITE(6,*) 'Input file: '
        READ(5,10) IN_NAME
	CALL JLP_VM_READIMAG(PNTR_1,NX_1,NY_1,IN_NAME,IN_COMMENTS)

        WRITE(6,23)
23      FORMAT(' Enter the limits of the subwindow: (IX1,IY1,IX2,IY2)',/,
     1         ' (Lower left corner IX1,IY1 coordinates',
     1         ' and Upper Right corner IX2,IY2 coordinates)')
        READ(5,*) IX1,IY1,IX2,IY2

21      WRITE(6,24)
24      FORMAT(' Enter the projection angle ',
     1         '(angle of the spectrum lines relative to OX): ',/,
     1         '(between -90. and +90.; 0.0 if horizontal, 90.0 if vertical): ')
        READ(5,*) P_ANGLE
        IF(P_ANGLE.LT.-90.OR.P_ANGLE.GT.90.)GOTO 21

C Getting virtual memory: output image is NX_2 by NY_2 pixels
        NX_2=1+(IX2-IX1+1)*COS(P_ANGLE*3.14159/180.)
     1        +(IY2-IY1+1)*SIN(P_ANGLE*3.14159/180.)
C Two lines: the first is the spectral flux, the second will be the wavelengths
        NY_2=2
         
        ISIZE=NX_2*NY_2*4
	CALL JLP_GETVM(PNTR_2,ISIZE)
 
	CALL XTRACT(MADRID(PNTR_1),MADRID(PNTR_2),NX_1,NY_1,NX_2,
     1              IX1,IX2,IY1,IY2,P_ANGLE,ISTAT)
 
C**** Output file :
        IF(ISTAT.EQ.0)THEN
          WRITE(6,*) 'Output file: '
          READ(5,10) OUT_NAME
	  WRITE(OUT_COMMENTS,1003) IN_NAME(1:12),IX1,IY1,IX2,IY2,P_ANGLE
1003      FORMAT('From ',A,'X1,Y1,X2,Y2,ANGLE:',4(1X,I5),1X,1PG10.3)
 	  CALL JLP_WRITEIMAG(MADRID(PNTR_2),NX_2,NY_2,NX_2,
     1             OUT_NAME,OUT_COMMENTS)
        ENDIF

	CALL JLP_END
	STOP
	END
C**************************************************************
C Subroutine XTRACT 
C
C**************************************************************
	SUBROUTINE XTRACT(IMAGE1,IMAGE2,NX_1,NY_1,NX_2,
     1              IX1,IX2,IY1,IY2,P_ANGLE,ISTAT)
	REAL*4 IMAGE1(NX_1,*),IMAGE2(NX_2,*)
        REAL*4 P_ANGLE,STEP2,X2,ALPHA,SC_PRO,COSP,SINP
        INTEGER*4 IX1,IX2,IY1,IY2,ISTAT,II,JJ,I2,J2

C Status:
        ISTAT=0
 
C Initialization of IMAGE2:
	DO I=1,NX_2
          IMAGE2(I,1)=0
        END DO

C Scalar product with vector (cos(P_ANGLE), sin(P_ANGLE))
        COSP=COS(P_ANGLE*3.14159/180.)
        SINP=SIN(P_ANGLE*3.14159/180.)
	DO J=IY1,IY2
	  DO I=IX1,IX2
            SC_PRO=FLOAT(I-IX1+1)*COSP + FLOAT(J-IY1+1)*SINP
            I2=INT(SC_PRO)
            ALPHA=SC_PRO-I2
	    IMAGE2(I2,1)=IMAGE2(I2,1)+(1.-ALPHA)*IMAGE1(I,J)
	    IMAGE2(I2+1,1)=IMAGE2(I2+1,1)+ALPHA*IMAGE1(I,J)
          END DO
        END DO

	DO I=1,NX_2
C Since the wavelengths are not known at this stage, 
C we fill the second line with pixel values:
C JLP99: Inversion for speckle data:
          IMAGE2(I,2)=FLOAT(NX_2-I+1)
        END DO
 
	RETURN
	END
