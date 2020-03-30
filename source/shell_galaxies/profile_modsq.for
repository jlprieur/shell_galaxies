C++***************************************************************
C    Program PROFILE_MODSQ
C
C Simpler version of PROFILE_MODSQ, specially designed for
C computing profiles of angular sectors of a centered "modsq" (=power spectrum)
C
C This programme computes a profile along circular annuli.
C Discards stars and defects over n-sigma (n is chosen by the user)
C Possibility of computation within angular sectors or on complete annuli.
C Input parameters in a file (See PROFILE1.DOC for documentation
C Output profile stored with the input parameter file.
C Output checking file in "profile_plain.log"
C
C Unit 2  : Profile (Output)
C Unit 3  : profile_plain.log (Logfile)
C Unit 10 : Parameter input file
C
C JLP2000:
C Differences with PROFILE_PLAIN:
C Does not compute a mask
C
C Syntax:
C  runs profile_modsq input_image 0.,0. output_profile 
C or
C  runs profile_modsq input_image phi_center,delta_phi output_profile 
C
C JLP
C Version of 24-02-00
C--***************************************************************
	PROGRAM PROFILE_MODSQ
	LOGICAL MASK_OUT,SECTOR,MEDIAN,MULT_SECT
        INTEGER NX,NY,NBINS,ITERMAX,IXMIN,IXMAX,IYMIN,IYMAX
        REAL AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX
        REAL XINCREM,FACT,SCALE,SKY,SIGREJEC,BADVALUE,RVMAG
        REAL AXISMIN,AXISMAX,BB1,DD1,UU,VV,WW,DELTA_PHI
	CHARACTER NAME1*40,NAMEPRO*40,NAMEPAR*40,COMMENTS1*160
	CHARACTER MASK_NAME*40,MASK_COM*160
	CHARACTER ANS*1
	INTEGER*4 MADRID(1),PNTR_ARRAY,PNTR_MASK
	COMMON /VMR/MADRID
10	FORMAT(A)
 
	COMMON /PRF1_DATA/NX,NY,NAME1,COMMENTS1
	COMMON /PRF1_OPTIONS/MASK_OUT,SECTOR,MEDIAN,MULT_SECT
	COMMON /PRF1_PARAM/AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX,
     1	XINCREM,FACT,SCALE,SKY,NBINS,SIGREJEC,BADVALUE,ITERMAX,RVMAG
	COMMON /PRF1_UTILITIES/AXISMIN,AXISMAX,BB1,DD1,UU,VV,WW,
     1	IXMIN,IXMAX,IYMIN,IYMAX
 
	CALL JLP_BEGIN
 
C Writing the title :
	WRITE(6,867)
867	FORMAT(' PROGRAM PROFILE_MODSQ TO COMPUTE PROFILES OF GALAXIES',
     1	' - VERSION OF 23/06/88 -',/)
 
C Output file with all the important details
	OPEN(3,FILE='profile_plain.log',STATUS='unknown',
     1	ACCESS='SEQUENTIAL',ERR=999)
	WRITE(3,867)
 
C Prompting the format for the files :
	CALL JLP_INQUIFMT
 
	MASK_OUT=.FALSE.
 
C Input image 
        PRINT *,' Input image ?'
        READ(5,10) NAME1

C**** Input of the image to process *****************
        CALL JLP_VM_READIMAG(PNTR_ARRAY,NX,NY,NAME1,COMMENTS1)
        WRITE(3,868)NAME1,NX,NY
868     FORMAT(' INPUT FILE : ',A,/,' NX, NY :',I4,2X,I4)

        PRINT *,' Theta_center, Delta_Theta (0,0 if complete annuli) ?'
        READ(5,*)PHI,DELTA_PHI
        IF(PHI.EQ.0..AND.DELTA_PHI.EQ.0)THEN
          PRINT *,'OK complete annuli'
          SECTOR=.FALSE.
          PHI=0.
          AMIN=0.
          AMAX=0.
        ELSE
          SECTOR=.TRUE.
          AMIN=PHI-DELTA_PHI/2.
          AMAX=PHI+DELTA_PHI/2.
          PRINT *,'OK profile on sectors'
        ENDIF

C Temprorary file with the input parameters
        NAMEPAR='profile_modsq.par'

C Input of the simplest set of parameters:
C WARNING: SECTOR, PHI, AMIN, AMAX should be set
C in the common blocks before calling this routine!
        CALL READ_PARAM_SIMPLEST(NAMEPAR)

C Ouput profile:
        PRINT *,' Output profile name ?'
        READ(5,10) NAMEPRO

C*********** General procedure ********
C Computes different coefficients which are used during all the following ;
	CALL UTILITIES
 
C Computes the profile
	CALL PROF1(MADRID(PNTR_ARRAY),NX)
 
C Output the results :
	CALL OUTPUT_RESULTS(NAMEPAR,NAMEPRO)
 
	CALL JLP_END
	PRINT *,' Files created: "profile_plain.log" and "profile_modsq.par"'
	STOP
999	PRINT *,' ERROR OPENING "profile_plain.log"'
	STOP
	END
C ******************************************************************
C Subroutine PROF1
C Computes the profile discarding the
C bad points (BADVALUE) and the stars and defects over n-sigma.
C Now, no longer used. But could be useful later. So I keep it.
C
C Meaning of the different arrays for the main loop on ITER (iterations):
C
C RADFIRST : Mean equivalent radius in arcseconds for the first iteration
C NBFIRST  : pixel number in the first step (ITER=1)
C
C SIGMA    : sigma computed in the previous iteration (ITER_1)
C MEAN     : mean computed in the previous iteration (ITER-1)
C
C NBER     : pixel number beeing computed in this iteration
C MAJAXIS  : Semi-major axis in arcseconds beeing computed in this iteration
C SUM      : sum being computed in the current iteration
C SUMSQ    : sum of squares being computed in the current iteration
C
C Output:
C MEAN, SIGMA : for the last iteration
C
C*******************************************************************
	SUBROUTINE PROF1(ARRAY,IDIM)
	PARAMETER (IDIM1=2000)
	REAL*4 ARRAY(IDIM,*)
	REAL*8 SIG(IDIM1),MEAN(IDIM1),RADFIRST(IDIM1)
	REAL*8 SUM(IDIM1),SUMSQ(IDIM1),MAJAXIS(IDIM1),SIGMA(IDIM1)
	INTEGER*4 NBER(IDIM1),NBFIRST(IDIM1)
	LOGICAL MASK_OUT,SECTOR,MEDIAN,MULT_SECT,NORMAL
	CHARACTER NAME1*40,COMMENTS1*160
 
	COMMON /PRF1_DATA/NX,NY,NAME1,COMMENTS1
	COMMON /PRF1_OPTIONS/MASK_OUT,SECTOR,MEDIAN,
     1	MULT_SECT
	COMMON /PRF1_PARAM/AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX,
     1	XINCREM,FACT,SCALE,SKY,NBINS,SIGREJEC,BADVALUE,ITERMAX,RVMAG
	COMMON /PRF1_RESULTS/MEAN,SIGMA,NBER,RADFIRST,NBFIRST
	COMMON /PRF1_UTILITIES/AXISMIN,AXISMAX,BB1,DD1,UU,VV,WW,
     1	IXMIN,IXMAX,IYMIN,IYMAX
 
C Problem when boundaries around zero, so:
        NORMAL=AMIN.LE.AMAX 

C Initialization of the arrays (SIGMA and MEAN : of the previous iteration)
	DO K=1,NBINS
	 SIGMA(K)=10000000.
	 MEAN(K)=0.D0
	END DO
 
C **********************************************************************
C Main loop :  (index=ITER) , to discard deviant points (over n-sigma)
C The image is scanned and when a value is discarded, it is set
C to BADVALUE, to avoid examining it on the next iterations.
C **********************************************************************
 
	DO 200 ITER=1,ITERMAX
 
C Initialise the following arrays for the next step :
C SUMSQ : sum of squares of the current step
C NBER : pixel number of the current step
C SUM : sum of the current step
 
	 DO 201 K=1,NBINS
	   NBER(K)=0
	   SUMSQ(K)=0D0
	   SUM(K)=0D0
201	 CONTINUE
 
	 DO 1 IY=IYMIN,IYMAX
	 Y1=FLOAT(IY)-Y0
	  DO 2 IX=IXMIN,IXMAX
 
C Discard the points equal to BADVALUE
	  IF(ARRAY(IX,IY).EQ.BADVALUE.AND.ITER.GT.1)GO TO 2
 
C Check if the pixel lies within the angular limits of the sector :
	  X1=FLOAT(IX)-X0
	    IF (SECTOR) THEN
	     CALL ANGLED(X1,Y1,APOINT)
             IF(NORMAL)THEN
	       IF(APOINT.LT.AMIN.OR.APOINT.GT.AMAX)THEN
	         ARRAY(IX,IY)=BADVALUE
	         GO TO 2
	       ENDIF
             ELSE
	       IF(APOINT.GT.AMAX.AND.APOINT.GT.AMIN)THEN
	         ARRAY(IX,IY)=BADVALUE
	         GO TO 2
	       ENDIF
	     ENDIF
	    ENDIF
 
C F2: Semi major axis of the ellipse containing the current pixel (in arcsec)
	  F2=SCALE*SQRT(UU*X1*X1+VV*Y1*Y1+WW*X1*Y1)
 
C KBIN : Index of the profile bin corresponding to the current pixel
C The law is such that the increment is increased of EPSILON
C at each step (Then the ratio of two successive increments
C is roughly FACT=1. + EPSILON since EPSILON is assumed to be small.
	  KBIN=1+INT(SQRT(BB1*BB1+DD1*(F2-AXISMIN))-BB1)
 
	    IF(KBIN.GE.1.AND.KBIN.LE.NBINS) THEN
	     SIG3=SIGREJEC*SIGMA(KBIN)
	     WORK=ABS(ARRAY(IX,IY)-MEAN(KBIN))
 
C Discard the points over (SIGREJEC*SIGMA)
	      IF(WORK.LE.SIG3)THEN
	        SUM(KBIN)=SUM(KBIN)+ARRAY(IX,IY)
	        SUMSQ(KBIN)=SUMSQ(KBIN)+ARRAY(IX,IY)*ARRAY(IX,IY)
	        NBER(KBIN)=NBER(KBIN)+1
	        MAJAXIS(KBIN)=MAJAXIS(KBIN)+F2
	        GO TO 2
	      ENDIF
	    ENDIF
	  ARRAY(IX,IY)=BADVALUE
2	  CONTINUE
1	 CONTINUE
 
 
C Reduce the value of NBINS if the arrays are too large.
C This accelerates the next iterations.
94	IF(NBER(NBINS).GT.2.OR.NBINS.LT.10) GOTO 95
	  NBINS=NBINS-1
	GOTO 94
 
C  Preparation for the next step :
95	 DO 300 KL=1,NBINS
	  XNUMB=FLOAT(NBER(KL))
	   IF(XNUMB.EQ.0)THEN
	     MEAN(KL)=0.
	     SIGMA(KL)=10000000.
	   ELSEIF(XNUMB.LE.2)THEN
	     MEAN(KL)=SUM(KL)/XNUMB
	     SIGMA(KL)=10000000.
	   ELSE
	     MEAN(KL)=SUM(KL)/XNUMB
	     SIGMA(KL)=DSQRT((SUMSQ(KL)/XNUMB)-MEAN(KL)*MEAN(KL))
	   ENDIF
300	 CONTINUE
 
C If First iteration, store the values of NBER and MAJAXIS :
	 IF(ITER.EQ.1)THEN
	   DO KL=1,NBINS
	     NBFIRST(KL)=NBER(KL)
	     IF(NBFIRST(KL).GT.0.)
     1	RADFIRST(KL)=SQRT(AXRATIO)*MAJAXIS(KL)/FLOAT(NBER(KL))
	   END DO
	 ENDIF
 
200	CONTINUE
 
	RETURN
	END
C-----------------------------------------------------------------------
	include 'jlpsub:profile_set.for'
