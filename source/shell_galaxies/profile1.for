C++***************************************************************
C    Program PROFILE1
C
C This program computes a profile along elliptical annuli.
C Discards stars and defects over n-sigma (n is chosen by the user)
C Possibility of computation within angular sectors or on complete annuli.
C Input parameters in a file (See PROFILE1.DOC for documentation
C Output profile stored with the input parameter file.
C
C Multiple sectors (i.e. a series of 12 sectors of 30 degrees) can
C be selected in the parameter file (option 3).
C
C Syntax:
C       RUNS PROFILE1 [Y/N: mask?] [[mask_name]] parameter_file
C
C Examples:
C       RUNS PROFILE1 N in_image parameter_file out_profile
C       RUNS PROFILE1 Y test_mask in_image parameter_file out_profile
C
C JLP
C Version of 02-02-94
C--***************************************************************
C Unit 2  : Profile (Output)
C Unit 3  : profile1.log (Logfile)
C Unit 10 : Parameter input file
C*****************************************************************
	PROGRAM PROFILE1
	PARAMETER (IDIM1=2000)
        REAL*4 R1,R2,R3,R4,RADMAX
C Linux 32 bits: 
C	INTEGER*4 PNTR_ARRAY,IPROF2D,PNTR_MASK
C Linux 64 bits: I switch to INTEGER*8:
	INTEGER*8 PNTR_ARRAY,IPROF2D,PNTR_MASK
C
	INTEGER*4 MADRID(1)
	LOGICAL MASK_OUT,SECTOR,MEDIAN,MULT_SECT
	CHARACTER NAME1*40,NAMEPRO*40,NAMEPAR*40,COMMENTS1*160
	CHARACTER MASK_NAME*40,MASK_COM*160
	CHARACTER ANS*1
 
	COMMON /VMR/MADRID
	COMMON /PRF1_DATA/NX,NY,NAME1,COMMENTS1
	COMMON /PRF1_OPTIONS/MASK_OUT,SECTOR,MEDIAN,MULT_SECT
	COMMON /PRF1_PARAM/AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX,
     1	XINCREM,FACT,SCALE,SKY,NBINS,SIGREJEC,BADVALUE,ITERMAX,RVMAG
	COMMON /PRF1_UTILITIES/AXISMIN,AXISMAX,BB1,DD1,UU,VV,WW,
     1	IXMIN,IXMAX,IYMIN,IYMAX
 
10	FORMAT(A)
	CALL JLP_BEGIN
C Writing the title :
	WRITE(6,867)
867	FORMAT(' Program PROFILE1 to compute profiles of galaxies',
     1	' Version of 02/02/94',/)
 
C Output file with all the important details
	OPEN(3,FILE='profile1.log',STATUS='unknown',
     1	ACCESS='SEQUENTIAL',ERR=999)
	WRITE(3,867)
 
C Prompting the format for the files :
	CALL JLP_INQUIFMT
 
	PRINT 15
15	FORMAT(' Do you want to output a mask with the pixels which',
     1	/,' will be selected when computing the profile (N)')
	READ(5,10) ANS
	MASK_OUT=(ANS.EQ.'Y'.OR.ANS.EQ.'y')
	IF(MASK_OUT)THEN
	 PRINT *,' Enter the name for the output mask:'
	 READ(5,10) MASK_NAME
	ENDIF
 
C Input image file
	PRINT *,' Input image file ?'
	READ(5,10) NAME1
 
C**** Input of the image to process *****************
	CALL JLP_VM_READIMAG(PNTR_ARRAY,NX,NY,NAME1,COMMENTS1)
	WRITE(3,868)NAME1,NX,NY
868	FORMAT(' INPUT FILE : ',A,/,' NX, NY :',I4,2X,I4)
 
C Input file with the input parameters
	PRINT *,' Input file with the parameters for the profile ?'
	READ(5,10) NAMEPAR
 
C Read the parameter file :
	CALL READ_PARAM(NAMEPAR)

C Ouput profile:
	PRINT *,' Output profile name ?'
	READ(5,10) NAMEPRO
 
C********** Special case : multiple sectors  **********
C Loop on the different sectors if MULT_SECT is true :
	IF(MULT_SECT)THEN
         PRINT *,' OK: multiple sectors'
	 ISECTOR=1
	 AMIN=0.
	 AMAX=30.
C Incrementing the names of the profiles automatically :
	 I=INDEX(NAMEPRO,' ')
	 INDX=INDEX(NAMEPRO,'.')
	 IF(INDX.EQ.0)INDX=I
 
C Starting the loop :
554	 I1=ISECTOR/10
	 I2=ISECTOR-10*I1
	 WRITE(NAMEPRO(INDX:INDX+6),873)I1,I2
873	 FORMAT('_',I1,I1,'.pro')
	ENDIF
 
C*********** General procedure ********
C Computes different coefficients which are used during all the following ;
	CALL UTILITIES
 
C Exit if too many bins
	IF(NBINS.GT.IDIM1)THEN
	  PRINT *,' Fatal error : too many bins !'
	  PRINT *,' NBINS =',NBINS
	  PRINT *,' Whereas   MAX Number =',IDIM1
	  STOP
	ENDIF
 
C An estimation of the maximum number of pixels in the annuli is given
C by (circle_length*(width+2)) where width is in pixels.
C (+1) is due to the "thickness" of each pixel.
C
C Four angles of the frame:
        R1=(FLOAT(NX)-X0)**2+(FLOAT(NY)-Y0)**2
        R2=(FLOAT(NX)-X0)**2+(FLOAT(NY)-Y0)**2
        R3=(FLOAT(NX)-X0)**2+(FLOAT(NY)-Y0)**2
        R4=(FLOAT(NX)-X0)**2+(FLOAT(NY)-Y0)**2
        RADMAX=MAX(R1,R2)
        RADMAX=MAX(RADMAX,R3)
        RADMAX=MAX(RADMAX,R4)
C Conversion to arcsec:
        RADMAX=SCALE*SQRT(RADMAX)
C Take minimum value of RADMAX1 (max wanted by the user) and maximum
C value taking into account the size of the image:
        RADMAX=MIN(RADMAX,RADMAX1)
C Then compute maximum width:
        XINC_MAX=XINCREM
        R1=RADMIN1
        DO I=1,NBINS
C R2: old value, R1: new value of radius
         R2=R1
         XINC_MAX=XINC_MAX*FACT
         R1=R1+XINC_MAX
         IF(R1.GT.RADMAX)GOTO 34
        ENDDO
C Computing equivalent radius betwwen R2 and RADMAX:
34      R3=(R2**2 + RADMAX**2)/2.
        R3=SQRT(R3)
C 2 PI -> 6.30
        R3=6.30*(R3/SCALE)*(1.+XINC_MAX/SCALE)
        NMAX=NINT(R3)
        WRITE(6,38) RADMAX,R3,XINC_MAX,NMAX,NBINS
38      FORMAT(' Estimated maximum radius: ',G12.5,' arcsec'/,
     1         ' Equivalent radius of last annulus: ',G12.5,' arcsec'/,
     1         ' Maximum annulus width (last annulus): ',G12.5,' arcsec',/,
     1         ' Maximum number of pixels in an annulus: ',I8,/,
     1         ' Number of annuli (NBINS): ',I5)
 
C Allocating memory space for the working array PROF2D :
	N_VMSIZE=NBINS*4*NMAX
	CALL JLP_GETVM(IPROF2D,N_VMSIZE)
 
C Loading the array in PROF2D
	CALL GENE_PROF2D(MADRID(PNTR_ARRAY),NX,MADRID(IPROF2D),NMAX)
 
C Computing the profile :
	IF(MEDIAN)THEN
C Set ratio to 0.5 (since looking for the middle value of the histogram...)
	 RATIO=0.50
	 CALL MEDIAN_PROF2D(MADRID(PNTR_ARRAY),NX,
     1	MADRID(IPROF2D),NMAX,RATIO)
	ELSE
	 CALL MEAN_PROF2D(MADRID(PNTR_ARRAY),NX,
     1	MADRID(IPROF2D),NMAX)
	ENDIF
 
C Output the results :
	CALL OUTPUT_RESULTS(NAMEPAR,NAMEPRO)
 
C********** Special case : multiple sectors  **********
C Loop on the different sectors if MULT_SECT is true :
	IF(MULT_SECT.AND.AMAX.LT.360.)THEN
	 ISECTOR=ISECTOR+1
	 AMIN=AMIN+30.
	 AMAX=AMAX+30.
C Freeing memory space:
	 CALL JLP_FREEVM(IPROF2D,N_VMSIZE)
	 GO TO 554
	ENDIF
 
C ****** Only the last one if multiple sectors ******
C Creation and output of the mask *******************
	IF(MASK_OUT)THEN
 
C Allocating memory space :
	  ISIZE=NX*NY*4
	  CALL JLP_GETVM(PNTR_MASK,ISIZE)
 
	  IF(MEDIAN)THEN
	   CALL MASK_MINI(MADRID(PNTR_MASK),MADRID(PNTR_ARRAY),NX)
	  ELSE
	   CALL MASK_MEAN(MADRID(PNTR_MASK),MADRID(PNTR_ARRAY),NX)
	  ENDIF
	 PRINT *,' Output of the mask :'
	 MASK_COM=' '
	 I=INDEX(NAMEPRO,'  ')
	 WRITE(MASK_COM,101)NAMEPRO(1:I-1)
101	 FORMAT(' Mask with ',A,'//')
	 PRINT *,' COMMENTS FOR THE MASK :',MASK_COM
	 CALL JLP_WRITEIMAG(MADRID(PNTR_MASK),NX,NY,NX,
     1	MASK_NAME,MASK_COM)
C Freeing memory space :
	  CALL JLP_FREEVM(PNTR_MASK,ISIZE)
	ENDIF
 
	CALL JLP_END
	STOP
999	PRINT *,' Fatal error opening "profile1.log"'
	CALL JLP_END
	STOP
	END
C ******************************************************************
C Subroutine GENE_PROF2D
C
C To generate a 2-D array PROF2D(NMAX,NBINS) by scanning the input
C image ARRAY once.
C
C
C Output :
C
C PROF2D : Array of the values of the input image classified according
C	   to the major axis of the ellipses which they belong to.
C RADFIRST : Mean equivalent radius in arcseconds for the first iteration
C NBFIRST  : pixel number in each annulus
C
C*******************************************************************
	SUBROUTINE GENE_PROF2D(ARRAY,IDIM,PROF2D,NMAX)
	PARAMETER (IDIM1=2000)
	REAL*4 PROF2D(NMAX,NBINS),ARRAY(IDIM,*)
	REAL*8 MEAN(IDIM1),RADFIRST(IDIM1),
     1	SUM_MAJAX(IDIM1),SIGMA(IDIM1)
	INTEGER*4 NBER(IDIM1),NBFIRST(IDIM1)
	LOGICAL MASK_OUT,SECTOR,MEDIAN,MULT_SECT,NORMAL
	CHARACTER NAME1*40,COMMENTS1*160
 
	COMMON /PRF1_DATA/NX,NY,NAME1,COMMENTS1
	COMMON /PRF1_OPTIONS/MASK_OUT,SECTOR,MEDIAN,MULT_SECT
	COMMON /PRF1_PARAM/AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX,
     1	XINCREM,FACT,SCALE,SKY,NBINS,SIGREJEC,BADVALUE,ITERMAX,RVMAG
	COMMON /PRF1_RESULTS/MEAN,SIGMA,NBER,RADFIRST,NBFIRST
	COMMON /PRF1_UTILITIES/AXISMIN,AXISMAX,BB1,DD1,UU,VV,WW,
     1	IXMIN,IXMAX,IYMIN,IYMAX
 
C Logical name:
	NORMAL=(AMIN.LE.AMAX)
 
C **********************************************************************
C Determination of the radius for each annulus	
C The image is scanned once
C **********************************************************************
	 DO K=1,NBINS
	   NBFIRST(K)=0
	   SUM_MAJAX(K)=0.D0
	 END DO
 
	 DO 3 IY=IYMIN,IYMAX
	 Y1=FLOAT(IY)-Y0
 
	  DO 2 IX=IXMIN,IXMAX
	  X1=FLOAT(IX)-X0
 
C Check if the pixel lies within the angular limits of the sector :
	    IF (SECTOR) THEN
	     CALL ANGLED(X1,Y1,APOINT)
	     IF(NORMAL)THEN
	       IF((APOINT.LT.AMIN).OR.(APOINT.GT.AMAX)) GO TO 2
	     ELSE
	       IF((APOINT.LT.AMIN).AND.(APOINT.GT.AMAX)) GO TO 2
	     ENDIF
	    ENDIF
 
C F2: Semi major axis of the ellipse containing the current pixel (in arcsec)
	  F2=SCALE*SQRT(UU*X1*X1+VV*Y1*Y1+WW*X1*Y1)
C JLP94: before .LE. now .LT.
	  IF(F2.LT.AXISMIN)GO TO 2
 
C KBIN : Index of the profile bin corresponding to the current pixel
	  KBIN=1+INT(SQRT(BB1*BB1+DD1*(F2-AXISMIN))-BB1)
	    IF(KBIN.GE.1.AND.KBIN.LE.NBINS) THEN
	     NBFIRST(KBIN) = NBFIRST(KBIN)+1
             IF(NBFIRST(KBIN).LE.0)THEN 
               PRINT *,' PB, NBFIRST (KBIN) =',NBFIRST(KBIN),' KBIN=',KBIN
             ENDIF
	     SUM_MAJAX(KBIN)=SUM_MAJAX(KBIN)+F2
	     PROF2D(NBFIRST(KBIN),KBIN)=ARRAY(IX,IY)
	    ENDIF
 
2	  CONTINUE 
 
3        CONTINUE	
 
C Reduce the value of NBINS if the array is too large.
94	IF(NBFIRST(NBINS).NE.0.OR.NBINS.LT.2)GOTO 95
	NBINS=NBINS-1
	GOTO 94
 
C Computing the mean equivalent radius (sqrt(a*b)) for each annulus :
95	  DO KL=1,NBINS
	   IF(NBFIRST(KL).GT.0.)
     1	RADFIRST(KL)=SQRT(AXRATIO)*SUM_MAJAX(KL)/FLOAT(NBFIRST(KL))
	  END DO
 
	RETURN
	END
C ******************************************************************
C Subroutine MEAN_PROF2D
C Computes a mean profile discarding the
C bad points (BADVALUE) and the stars and defects over n-sigma.
C
C Meaning of the different arrays for the main loop on ITER (iterations):
C
C SIGMA    : sigma computed in the previous iteration (ITER_1)
C MEAN     : mean computed in the previous iteration (ITER-1)
C NBER     : pixel number beeing computed in this iteration
C
C and variables :
C
C SUM      : sum being computed in the current iteration
C SUMSQ    : sum of squares being computed in the current iteration
C
C Output:
C MEAN, SIGMA, NBER : of the last iteration
C
C*******************************************************************
	SUBROUTINE MEAN_PROF2D(ARRAY,IDIM,PROF2D,NMAX)
	PARAMETER (IDIM1=2000)
	REAL*4 PROF2D(NMAX,NBINS),ARRAY(IDIM,*)
	REAL*8 MEAN(IDIM1),RADFIRST(IDIM1),SIGMA(IDIM1)
	REAL*8 SUM,SUMSQ,WORK
	INTEGER*4 NBER(IDIM1),NBFIRST(IDIM1)
 
	COMMON /PRF1_PARAM/AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX,
     1	XINCREM,FACT,SCALE,SKY,NBINS,SIGREJEC,BADVALUE,ITERMAX,RVMAG
	COMMON /PRF1_RESULTS/MEAN,SIGMA,NBER,RADFIRST,NBFIRST
 
 
C **********************************************************************
C Main loop on the radius index KBIN :
C **********************************************************************
 
	DO K=1,NBINS
 
C Initialization:
	 MEAN(K)=0.
	 SIGMA(K)=10000000.
	 NBER(K)=0
 
C **********************************************************************
C The line (K) is scanned ITERMAX times, and stars and defects over n-sigma
C are automatically discarded.
C **********************************************************************
 
	DO ITER=1,ITERMAX
	 SUM=0.D0
	 SUMSQ=0.D0
	 NB=0
	 SIG3=SIGREJEC*SIGMA(K)
 
	   DO I=1,NBFIRST(K)
	    WORK=ABS(PROF2D(I,K)-MEAN(K))
C Discard the points equal to BADVALUE or over n-sigma :
C JLP94: do not discard any point if the bin contains less than 8 pixels
	     IF((NBFIRST(K).LE.8)
     1          .OR.(PROF2D(I,K).NE.BADVALUE.AND.WORK.LT.SIG3))THEN
	      NB=NB+1
	      SUMSQ=SUMSQ+PROF2D(I,K)*PROF2D(I,K)
	      SUM=SUM+PROF2D(I,K)
	     ENDIF
	   END DO
 
C Compute the mean and standard deviation for each annulus,
C and store the result in the arrays MEAN and SIGMA :
	  IF(NB.NE.0)THEN
	   NBER(K)=NB
	   MEAN(K)=SUM/FLOAT(NB)
	  IF(NB.GT.3)
     1	SIGMA(K)=DSQRT((SUMSQ/FLOAT(NB))-MEAN(K)*MEAN(K))
	  ENDIF
 
C Go to another iteration
	 END DO
 
C Go to another bin
	END DO
 
	RETURN
	END
C ******************************************************************
C Subroutine MASK_MINI
C
C Create a mask where all the values inferior to MEAN(K) are set to 1.,
C and the other ones to 0., for MINI profiles.
C
C Input :
C MEAN : median value from MEDIAN_PROF2D
C
C*******************************************************************
	SUBROUTINE MASK_MINI(MASK1,ARRAY,IDIM)
	PARAMETER (IDIM1=2000)
	REAL*8 MEAN(IDIM1),RADFIRST(IDIM1),SIGMA(IDIM1)
	REAL*4 MASK1(IDIM,*),ARRAY(IDIM,*)
	INTEGER*4 NBER(IDIM1),NBFIRST(IDIM1)
	LOGICAL MASK_OUT,SECTOR,MEDIAN,MULT_SECT,NORMAL
	CHARACTER NAME1*40,COMMENTS1*160
 
	COMMON /PRF1_DATA/NX,NY,NAME1,COMMENTS1
	COMMON /PRF1_OPTIONS/MASK_OUT,SECTOR,MEDIAN,MULT_SECT
	COMMON /PRF1_PARAM/AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX,
     1	XINCREM,FACT,SCALE,SKY,NBINS,SIGREJEC,BADVALUE,ITERMAX,RVMAG
	COMMON /PRF1_RESULTS/MEAN,SIGMA,NBER,RADFIRST,NBFIRST
	COMMON /PRF1_UTILITIES/AXISMIN,AXISMAX,BB1,DD1,UU,VV,WW,
     1	IXMIN,IXMAX,IYMIN,IYMAX
 
C Logical name:
	NORMAL=(AMIN.LE.AMAX)
 
C Erasing the array:
        DO IY=1,NY
	  DO IX=1,NX
           MASK1(IX,IY)=0.
	  ENDDO
	ENDDO

C**********************************************************************
C Main loop :
C Scan only the working area since VAX initializes the arrays to 0
C when they are created :
 
	DO IY=IYMIN,IYMAX
	Y1=FLOAT(IY)-Y0
 
	DO IX=IXMIN,IXMAX
	  X1=FLOAT(IX)-X0
 
	  IF(ARRAY(IX,IY).EQ.BADVALUE) GO TO 2
 
C Check if the pixel lies within the angular limits of the sector :
	    IF (SECTOR) THEN
	     CALL ANGLED(X1,Y1,APOINT)
	     IF(NORMAL)THEN
	       IF((APOINT.LT.AMIN).OR.(APOINT.GT.AMAX)) GO TO 2
	     ELSE
	       IF((APOINT.LT.AMIN).AND.(APOINT.GT.AMAX)) GO TO 2
	     ENDIF
	    ENDIF
 
C F2: Semi major axis of the ellipse containing the current pixel (in arcsec)
	  F2=SCALE*SQRT(UU*X1*X1+VV*Y1*Y1+WW*X1*Y1)
 
C KBIN : Index of the profile bin corresponding to the current pixel
	  KBIN=1+INT(SQRT(BB1*BB1+DD1*(F2-AXISMIN))-BB1)
 
	    IF(KBIN.GE.1.AND.KBIN.LE.NBINS) THEN
 
C Set to 1. the points selected by MINI_PROF2D
	      IF(ARRAY(IX,IY).LE.MEAN(KBIN))MASK1(IX,IY)=1.
	    ENDIF
 
2	 END DO
 
	END DO
 
	RETURN
	END
C ******************************************************************
C Subroutine MEDIAN_PROF2D
C
C To compute the median value in each annuli (when RATIO is set to 0.5)
C
C Arrays :
C NBFIRST  : pixel number in each annulus
C NBER     : number of pixels kept in each annulus
C
C*******************************************************************
	SUBROUTINE MEDIAN_PROF2D(ARRAY,IDIM,PROF2D,NMAX,RATIO)
	PARAMETER (IDIM1=2000)
	REAL*4 PROF2D(NMAX,*),ARRAY(IDIM,*)
	REAL*8 MEAN(IDIM1),RADFIRST(IDIM1),SIGMA(IDIM1)
	REAL*8 SUM0
	INTEGER*4 NBER(IDIM1),NBFIRST(IDIM1),IHISTO(200)
 
	COMMON /PRF1_PARAM/AXRATIO,PHI,X0,Y0,RADMIN1,RADMAX1,AMIN,AMAX,
     1	XINCREM,FACT,SCALE,SKY,NBINS,SIGREJEC,BADVALUE,ITERMAX,RVMAG
	COMMON /PRF1_RESULTS/MEAN,SIGMA,NBER,RADFIRST,NBFIRST
 
C **********************************************************************
C Main loop on the radius index K :
C **********************************************************************
 
	DO K=1,NBINS
 
C Initialization:
	 XMIN0=320000.
	 XMAX0=-320000.
 
C Looks for the minimum value :
	   DO I=1,NBFIRST(K)
	     IF(PROF2D(I,K).NE.BADVALUE)THEN
                XMIN0=MIN(PROF2D(I,K),XMIN0)
                XMAX0=MAX(PROF2D(I,K),XMAX0)
	     ENDIF
	   END DO
 
C Return if only one value :
	IF(XMAX0.EQ.XMIN0)THEN
	MEAN(K)=XMIN0
	GO TO 2
	ENDIF
 
C Set up the increment for the histogram :
	NHISBINS=100
	FACTHIS=FLOAT(NHISBINS)/(XMAX0-XMIN0)
 
C Resets the histogram (IHISTO) :
	DO II=1,NHISBINS+2
	IHISTO(II)=0
	END DO
 
C Computes the histogram :
	NPTSHIS=0
	   DO I=1,NBFIRST(K)
	     IF(PROF2D(I,K).NE.BADVALUE)THEN
	      NPTSHIS=NPTSHIS+1
	      II=NINT((PROF2D(I,K)-XMIN0)*FACTHIS)+1
	      IHISTO(II)=IHISTO(II)+1
	     ENDIF
	   END DO
	NBER(K)=NPTSHIS
 
C Works out the median 
C (i.e. looking for the middle column of the histogram (when RATIO=0.5)) 
	XNPTMAX=FLOAT(NBER(K))*RATIO
	SUM0=FLOAT(IHISTO(1))
	II=1
96	IF(SUM0.GE.XNPTMAX.OR.II.GE.NHISBINS) GOTO 97
	 II=II+1
	 SUM0=SUM0+FLOAT(IHISTO(II))
	GOTO 96
 
C Stores the result (median) in MEAN (although it is not the mean in that case)
97	MEAN(K)=XMIN0+FLOAT(II-1)/FACTHIS
 
2	END DO
 
	RETURN
	END
C-----------------------------------------------------------------
	include 'jlpsub:profile_set.for'
