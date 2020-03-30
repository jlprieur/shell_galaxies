C++**********************************************************************
C Program FFIELD to compute a mean flat field discarding the stars and defects.
C JLP
C Version of 10-10-92 
C--*************************************************************************
	PROGRAM FFIELD
	INTEGER*4 PNTR_IM(30),PNTR_OUTPUT,PNTR_OFFSET
	REAL*4 MEAN(30)
        REAL*4 TOLER,CRIT,CSF
	CHARACTER RF*1,FILENAME*40,COMMENTS*80
        INTEGER*4 IOPT,NPL,NL,NX,NY,NXOUT,NYOUT
        INTEGER*4 NPLD,NLD
	INTEGER*4 MADRID(1)
	COMMON /VMR/MADRID
 
	COMMON/PARAM/NPL,NL,NPL1,NPL2,NL1,NL2,NPLD,NPLF,
     1	NLD,NLF,MEAN,NXOUT,NYOUT
 
10	FORMAT(A)
 
	OPEN(9,FILE='ffield.log',STATUS='unknown')
        WRITE(6,87)
        WRITE(9,87)
87      FORMAT(' Program FFIELD   Version 10-10-92',/,
     1    ' Two options: 1=Most_likely 2=Mean_with_rejection',/,
     1    ' Enter the option you want:')
        READ(5,*) IOPT
 
C Option for the file formats :
	CALL JLP_BEGIN
	CALL JLP_INQUIFMT
 
C Input of the PLU files
	PRINT *,' Number of flat fields : (max=30)'
	READ(5,*) NFILE
	
	PRINT *,' Enter the flat fields :'
	 DO K=1,NFILE
	  NPL=NX
	  NL=NY
          WRITE(6,*) 'Input file: '
          READ(5,10) FILENAME
	  CALL JLP_VM_READIMAG(PNTR_IM(K),NX,NY,
     1	FILENAME,COMMENTS)
 
C Check if same size or not:
	   IF(K.GE.2.AND.((NPL.NE.NX).OR.(NL.NE.NY)))THEN
	     PRINT *,' FATAL ERROR: THE INPUT IMAGES',
     1	' HAVE NOT THE SAME SIZE'
	     STOP
	   ENDIF
	 END DO
 
C Entering the limits for the work
	PRINT 1015
1015	FORMAT(' Working area on the flat fields :',
     1  ' (Will determine output size)',/, 
     1	' Starting and ending pixels : (0,0=ALL)  ',$)
	READ(5,*) NPL1,NPL2
	 IF(NPL1.EQ.0) NPL1=1
	 IF(NPL2.EQ.0) NPL2=NPL
	PRINT 1016
1016	FORMAT(' Starting and ending line : (0,0=ALL)  ',$)
	READ(5,*) NL1,NL2
	 IF(NL1.EQ.0) NL1=1
	 IF(NL2.EQ.0) NL2=NL
	WRITE(9,200) NPL1,NPL2,NL1,NL2
200	FORMAT(' Working area : NPL1,NPL2:',2I5,'   NL1,NL2:',2I5,/)
 
C Two options for the offset : file or constant
	RF='C'
	PRINT *,' Do you want to subtract a bias frame or a ',
     1	'constant : F/C ? (C)'
	READ(5,10) RF
 
	IF(RF.EQ.'F'.OR.RF.EQ.'f') THEN
6001	  PRINT *,' Filename of the offset image:'
	  CALL JLP_VM_READIMAG(PNTR_OFFSET,NPL,NL,FILENAME,COMMENTS)
	ELSE
	  PRINT *,' Bias value:'
	  READ(5,*) CSF
	  WRITE(9,202) CSF
202	  FORMAT(' Bias value: ',F7.2)
	  MEMS_OFFSET=4*NPL*NL
	  CALL JLP_GETVM(PNTR_OFFSET,MEMS_OFFSET)
 
C Generating a constant offset image:
	  CALL GENE_OFFSET(MADRID(PNTR_OFFSET),CSF)
	ENDIF
 
C Entering the limits for the normalisation
	PRINT 4000
4000	FORMAT(/,' Definition of the normalization area'
     1	' for each flat field',/)
	PRINT *,' Starting and ending pixels : '
	READ(5,*) NPLD,NPLF
	 IF(NPLD.EQ.0) NPLD=1
	 IF(NPLF.EQ.0) NPLF=NPL
	PRINT *,' Starting and ending lines : '
	READ(5,*) NLD,NLF
	 IF(NLD.EQ.0) NLD=1
	 IF(NLF.EQ.0) NLF=NL
	WRITE(9,203) NPLD,NPLF,NLD,NLF
203	FORMAT(' Area used to compute the mean for each flat field:',
     1	/,' NPLD,NPLF:',2I5,'    NLD,NLF:',2I5,/)
C	NLD=NLD+1
C	NLF=NLF+1
 
C Getting memory space for the output :
	NXOUT=NPL2-NPL1+1
	NYOUT=NL2-NL1+1
	MEMS_OUTPUT=4*NXOUT*NYOUT
	CALL JLP_GETVM(PNTR_OUTPUT,MEMS_OUTPUT)
 
C Begining of the operations:
	PRINT *,'********* Starting the calculations *********'
	PRINT *,' Details in : "ffield.log"'
	PRINT *,' NFILE = ',NFILE
 
C Calculates the mean in the normalisation area for all the images.
	DO K=1,NFILE
	  CALL MEAN_AREA(MADRID(PNTR_IM(K)),MADRID(PNTR_OFFSET),K)
	END DO
 
C Then determining the final value with all the input images :
        IF(IOPT.EQ.1)THEN
	  PRINT *,' Enter relative tolerance : (0.01 ?) :'
	  READ(5,*) TOLER
        ELSE
	  PRINT *,' Enter criterium for rejection (in sigma units) :'
	  READ(5,*) CRIT
        ENDIF
	CALL PLUMOY1(PNTR_IM,MADRID(PNTR_OFFSET),
     1	MADRID(PNTR_OUTPUT),NFILE,CRIT,TOLER,IOPT)
 
C Output file :
	PRINT *,' Output mean flat field'
        READ(5,10) FILENAME
	COMMENTS=' '
	CALL JLP_WRITEIMAG(MADRID(PNTR_OUTPUT),NXOUT,NYOUT,NXOUT,
     1	FILENAME,COMMENTS)
 
	CLOSE(9)
 
	CALL JLP_END
	STOP
	END
 
C*********************************************************************
C Subroutine MEAN_AREA to calculate the mean in the area
C of normalisation : NPLD, NPLF for the pixels
C and NLD, NLF for the lines
C
C Input:
C  K: image number
C  IMAGE(NPL,NL): image
C
C Output :
C MEAN(K) : Mean value of the normalisation area for the file I
C
C*********************************************************************
	SUBROUTINE MEAN_AREA(IMAGE,OFFSET,K)
	REAL*4 MEAN(30),IMAGE(NPL,NL),OFFSET(NPL,NL)
	REAL*8 SUM,SUMSQ
	COMMON/PARAM/NPL,NL,NPL1,NPL2,NL1,NL2,NPLD,NPLF,
     1	NLD,NLF,MEAN,NXOUT,NYOUT
 
C Computing the intermediary sums :
	  SUM=0.D0
	  SUMSQ=0.D0
	  NUMBER=0
	   DO IY=NLD,NLF
	      DO IX=NPLD,NPLF
	        NUMBER=NUMBER+1
	        SUM=SUM+IMAGE(IX,IY)
     1	           -OFFSET(IX,IY)
	        SUMSQ=SUMSQ+
     1	(IMAGE(IX,IY)-OFFSET(IX,IY))**2
	      END DO
	   END DO
 
C Computing the mean :
	  MEAN(K)=SUM/FLOAT(NUMBER)
	  STANDEV=SUMSQ/FLOAT(NUMBER)-MEAN(K)*MEAN(K)
          STANDEV=SQRT(STANDEV)
 
C Output of the mean and standard deviation:
	  PRINT 3,K,SUM,MEAN(K),STANDEV
	  WRITE(9,3) K,SUM,MEAN(K),STANDEV
3	  FORMAT(' FILE : ',I2,3X,' SUM = ',E11.4,' MEAN :',E11.4,
     1	' STDEV :',E11.4)
 
 
	RETURN
	END
 
C**********************************************************************
C Subroutine PLUMOY1
C Calling MOST_LIKELY for each pixel of the image
C
C Input :
C	PNTR_IM= array with the pointers of the input flat field images
C	CRIT= Criterium for rejecting the bad points (in sigma units)
C Output :
C	OUTPUT= Output flat field
C**********************************************************************
	SUBROUTINE PLUMOY1(PNTR_IM,OFFSET,OUTPUT,NFILE,
     1    CRIT,TOLER,IOPT)
	REAL*4 MEAN(30)
	REAL*4 OUTPUT(NXOUT,NYOUT),OFFSET(NPL,NL)
	REAL*4 SIGMA,VALMOY,VX(30),TOLER,CRIT
	INTEGER*4 PNTR_IM(30)
	INTEGER*4 MADRID(1)
	COMMON /VMR/MADRID
 
	COMMON/PARAM/NPL,NL,NPL1,NPL2,NL1,NL2,NPLD,NPLF,
     1	NLD,NLF,MEAN,NXOUT,NYOUT
 
C Option: 1= most likely
C         2= mean with CRIT*sigma rejection
C
	BAD_POINTS=0.
 
C Loop on the lines
	DO 66 IY=NL1,NL2
 
C Calculates the final values for the current line
	 DO 6 IX=NPL1,NPL2
 
C First computes sigma and mean value for the current pixel
C with all the images :
	  DO K=1,NFILE
	    VX(K)=(GET_PIXEL(MADRID(PNTR_IM(K)),IX,IY)
     1	-OFFSET(IX,IY))/MEAN(K)
	  END DO
 
         IF(IOPT.EQ.1)THEN
C Status=1 if failure, 0, otherwise.
	  CALL MOST_LIKELY(NFILE,VX,VALMOY,SIGMA,IX,IY,TOLER,ISTATUS)
         ELSE 
C Compute VALMOY and standard deviation with array VGG and NGG values:
          CALL MEAN_SIGMA(VX,NFILE,VALMOY,SIGMA)
	  NTOT=0
	  SOM=0.
C Limits for validity:
	  DELM=VALMOY-CRIT*SIGMA
	  DELP=VALMOY+CRIT*SIGMA
 
C Then discards the values over sigma :
	 DO 7 K=1,NFILE
C Increasing the counter if valid:
	  IF(VX(K).GE.DELM.AND.VX(K).LE.DELP) THEN
	    NTOT=NTOT+1
	    SOM=SOM+VX(K)
	  ENDIF
7	 CONTINUE
 
C XTEST counts the number of "non valid" points
           STATUS=1
	   IF(NTOT.NE.0) THEN
	    VALMOY=SOM/FLOAT(NTOT)
            STATUS=0
	   ENDIF
         ENDIF
C Now fills the array:
	 IX1=IX-NPL1+1
	 IY1=IY-NL1+1
	 OUTPUT(IX1,IY1)=VALMOY
C Status=1 if failure, 0, otherwise.
	 BAD_POINTS=BAD_POINTS+STATUS
6	CONTINUE
 
66      CONTINUE
 
C Output the number of "non valid" points :
	IF(BAD_POINTS.NE.0..AND.IOPT.EQ.1)THEN
	  PRINT 70,BAD_POINTS
	  WRITE(9,70) BAD_POINTS
70	  FORMAT(/,' PLUMOY1/WARNING: ',E12.5,' pixels computed',/,
     1      '    without rejection because of too big scatter')
	ELSEIF(BAD_POINTS.NE.0..AND.IOPT.NE.1)THEN
	  PRINT 71,BAD_POINTS
	  WRITE(9,71) BAD_POINTS
71	  FORMAT(/,' PLUMOY1/WARNING: ',E12.5,' pixels computed',/,
     1      '    without rejection because of too big sigma')
	ENDIF
 
	RETURN
	END
C*********************************************************************
C Subroutine MOST_LIKELY 
C Central part of the program. This subroutine finds the value to
C keep at a given pixel.
C VX : array with the intensity values of the current pixel, from
C the different images. It is modified in this subroutine.
C VGG : work array to store the values of VX different from MIn and MAX
C VDD : work array to store sucessive MIN and MAX from array VX 
C*********************************************************************
	SUBROUTINE MOST_LIKELY(NFILE,VX,VALMOY,SIGMA,IX,IY,TOLER,
     1            ISTATUS)
	REAL*4 VX(30),VDD(30),VGG(30),DIFV(3)
        REAL*4 TOLER,SIGMA,VALMOY,DIF
        INTEGER*4 ISTATUS,NDEC,NGOOD_FILES
 
        ISTATUS=0
	NDEC=0.
	NGOOD_FILES=NFILE
C Number of elements in VGG:
	NGG=0

C*******************************************************************
C Loop as long as MAX-MIN greater than TOLER and as long as there are
C different files
5	VMIN=VX(1)
	VMAX=VX(1)
C I. Looking for min. and max.
	DO 1 K=1,NGOOD_FILES
	  IF(VX(K).GT.VMAX) THEN
	    KMAX=K
	    VMAX=VX(K)
	  ENDIF
	  IF(VX(K).LT.VMIN) THEN
	    KMIN=K
	    VMIN=VX(K)
	  ENDIF
1	CONTINUE
 
C*******************************************************************
C II. Checking the difference between the current MIN and MAX:
	DIF=VMAX-VMIN
C If small difference, compute Mean and Variance and exit,
	IF(DIF.LE.TOLER) THEN
	  DO 2 K=1,NGOOD_FILES
	    NGG=NGG+1
	    VGG(NGG)=VX(K)
2         CONTINUE
C Compute VALMOY and standard deviation with the current array VGG:
          CALL MEAN_SIGMA(VGG,NGG,VALMOY,SIGMA)
 
C If all is right at the first step (i.e. NDEC=0), returns.
C Otherwise, compute better estimate of the mean, by adding
C successive MIN and MAX if close enough: 
	 IF(NDEC.NE.0)THEN
	  DO 10 KD=1,NDEC
	    DIFD=ABS(VDD(KD)-VALMOY)
C Add VDD terms (i.e. MIN and MAX) if DIF < TOLER 
	    IF(DIFD.LE.TOLER)THEN
	     NGG=NGG+1
	     VGG(NGG)=VDD(KD)
            ENDIF
10	  CONTINUE
C OK: Compute VALMOY and standard deviation with array VGG and NGG values:
          CALL MEAN_SIGMA(VGG,NGG,VALMOY,SIGMA)
	 ENDIF
         RETURN
 
C Else (if DIF > 0.01), eliminates VMIN and VMAX in the array VX
C and stores these values in the array VDD
        ELSE
	 NDEC=NDEC+1
	 VDD(NDEC)=VMIN
	 NDEC=NDEC+1
	 VDD(NDEC)=VMAX
	 I=0
	 DO 4 K=1,NGOOD_FILES
	  IF((K.NE.KMAX).AND.(K.NE.KMIN))THEN
	    I=I+1
	    VX(I)=VX(K)
          ENDIF
4	 CONTINUE
	 NGOOD_FILES=I
 
C Loop while it remains more than one file:
         IF(NGOOD_FILES.GT.1) GO TO 5
        ENDIF

C**********************************************************************
C Case when there is only one file left, at an iteration larger than the first: 
C we have to go one step earlier and give up the tolerance constraint: 
	IF(NGOOD_FILES.EQ.1.AND.NDEC.GT.2) THEN
	  VMIN2=VDD(NDEC-1)
	  VMAX2=VDD(NDEC)
C Loop with the MIN:
	  NGA=0
	  DO 40 KD=1,NDEC-2,2
	    DIF11=ABS(VMIN2-VDD(KD))
	    IF(DIF11.LE.0.01)THEN
	      NGA=NGA+1
	      VGG(NGA)=VDD(KD)
            ENDIF
40	  CONTINUE
C NGA.NE.0: i.e. we've found a compromise with the MIN.
	  IF(NGA.NE.0)THEN
            NGG=NGA
	    NGG=NGG+1
	    VGG(NGG)=VMIN2
C Compute MOYJA and standard deviation with array VGG and NGG values:
            CALL MEAN_SIGMA(VGG,NGG,MOYJA,SIGMA)
	    DIFFE=ABS(MOYJA-VX(1))
C Add the first term if close enough:
	    IF(DIFFE.LE.TOLER)THEN
	      NGG=NGG+1
	      VGG(NGG)=VX(1)
            ENDIF
C OK: we've found a compromise with the MIN. 
C Compute VALMOY and standard deviation with array VGG and NGG values:
            CALL MEAN_SIGMA(VGG,NGG,VALMOY,SIGMA)
            RETURN
           ENDIF
C If compromise not found with MIN (i.e. NGA=0), try with MAX:
	  DO 41 KD=2,NDEC-1,2
	    DIF22=ABS(VMAX2-VDD(KD))
	    IF(DIF22.LE.TOLER)THEN
	      NGA=NGA+1
	      VGG(NGA)=VDD(KD)
            ENDIF
41	  CONTINUE
C
	  IF(NGA.NE.0)THEN
            NGG=NGA
	    NGG=NGG+1
	    VGG(NGG)=VMAX2
C Compute MOYJA and standard deviation with array VGG and NGG values:
            CALL MEAN_SIGMA(VGG,NGG,MOYJA,SIGMA)
	    DIFFE=ABS(MOYJA-VX(1))
C Add the first term if close enough:
	    IF(DIFFE.LE.TOLER)THEN
	      NGG=NGG+1
	      VGG(NGG)=VX(1)
            ENDIF
C OK: we've found a compromise with the MAX. 
C Compute VALMOY and standard deviation with array VGG and NGG values:
            CALL MEAN_SIGMA(VGG,NGG,VALMOY,SIGMA)
            RETURN
          ENDIF
C If failure again (NGA=0), try with both: MIN and MAX:
	  DO 52 KD=1,NDEC
	    DIFFA=ABS(VX(1)-VDD(KD))
C Check if value is close enough from the series of MIN / MAX
	    IF(DIFFA.LE.TOLER)THEN
	      NGA=NGA+1
	      VGG(NGA)=VDD(KD)
            ENDIF
52	  CONTINUE
C If at least one point is close enough, add first term, compute mean and exit:
	  IF(NGA.NE.0)THEN
	    NGA=NGA+1
	    VGG(NGA)=VX(1)
	    NGG=NGA
C OK: Compute VALMOY and standard deviation with array VGG and NGG values:
            CALL MEAN_SIGMA(VGG,NGG,VALMOY,SIGMA)
            RETURN
          ELSE
C Else, complete failure, send the two (most likely) values 
C and give up tolerance constraint:
	    VMIN1=VMIN2
	    VMAX1=VMAX2
	    GO TO 33
          ENDIF
	ENDIF
 
C Case when there is only one file left, on the first step
	IF(NGOOD_FILES.EQ.1.AND.NDEC.EQ.2) THEN
	   NGA=0
 
	   DO 35 KD=1,2
	     DI1=ABS(VDD(KD)-VX(1))
	     IF(DI1.LE.TOLER)THEN
	       NGA=NGA+1
	       VGG(NGA)=VDD(KD)
             ENDIF
35	   CONTINUE
 
	   IF(NGA.NE.0)THEN
	     NGA=NGA+1
	     VGG(NGA)=VX(1)
	     NGG=NGA
C OK: Compute VALMOY and standard deviation with array VGG and NGG values:
            CALL MEAN_SIGMA(VGG,NGG,VALMOY,SIGMA)
            RETURN
           ENDIF
C MAX-VX(1):
	   DIFV(2)=ABS(VDD(2)-VX(1))
C MIN-VX(1):
	   DIFV(3)=ABS(VDD(1)-VX(1))
C If MAX-VX(1) less than MIN-VX(1), define new interval with closest values: 
	   IF(DIFV(2).LT.DIFV(3)) THEN
	     VMIN1=VDD(2)
	     VMAX1=VX(1)
C If VX(1)-MIN less than all the others:
           ELSE
	     VMIN1=VDD(1)
	     VMAX1=VX(1)
	   ENDIF
C Display failure message since tolerance constraints could not be held.
	  GO TO 33
	ENDIF
 
C Case when there are no more files left.: we have to go one step
C earlier and give up the tolerance constraint: 
	IF(NGOOD_FILES.EQ.0) THEN
	  VMIN1=VDD(NDEC-1)
	  VMAX1=VDD(NDEC)
	  NMI=0
C Loop with the MIN:
	   DO 30 KD=1,NDEC-2,2
	     DIF1=ABS(VMIN1-VDD(KD))
	     IF(DIF1.LE.0.01)THEN
	       NMI=NMI+1
	       VGG(NMI)=VDD(KD)
             ENDIF
30	   CONTINUE
	   IF(NMI.NE.0)THEN
	     NMI=NMI+1
	     VGG(NMI)=VMIN1
	     NGG=NMI
C OK: we've found a compromise with the MIN. 
C Compute VALMOY and standard deviation with array VGG and NGG values:
            CALL MEAN_SIGMA(VGG,NGG,VALMOY,SIGMA)
            RETURN
           ENDIF
C If compromise not found with MIN (i.e. NMI=0), try with MAX:
	   DO 31 KD=2,NDEC-1,2
	     DIF2=ABS(VMAX1-VDD(KD))
	     IF(DIF2.LE.0.01)THEN
	       NMI=NMI+1
	       VGG(NMI)=VDD(KD)
             ENDIF
31	   CONTINUE
	   IF(NMI.NE.0)THEN
	      NMI=NMI+1
	      VGG(NMI)=VMAX1
	      NGG=NMI
C OK: we've found a compromise with the MAX. 
C Compute VALMOY and standard deviation with array VGG and NGG values:
              CALL MEAN_SIGMA(VGG,NGG,VALMOY,SIGMA)
            RETURN
            ELSE
C Else, complete failure, give up tolerance constraint:
            GO TO 33
            ENDIF
	ENDIF
 
C Case with only two values (MIN and MAX):
33	  VGG(1)=VMIN1
	  VGG(2)=VMAX1
	  NGG=2
	  DIFDER=VMAX1-VMIN1
  	   IF(DIFDER.GT.(4.*TOLER)) THEN
	     WRITE(9,38) DIFDER,IX,IY
38	     FORMAT(' Large scatter :',F12.4,', for IX=',I4,
     1	' IY=',I4,' (mean computed with only two values)')
	   ENDIF
C OK: Compute VALMOY and standard deviation with array VGG and NGG values:
          CALL MEAN_SIGMA(VGG,NGG,VALMOY,SIGMA)
          STATUS=1
        RETURN
        END
C-------------------------------------------------------------------
C Subroutine MEAN_SIGMA
C To compute Mean and standard deviation of array VGG(NGG):
C-------------------------------------------------------------------
        SUBROUTINE MEAN_SIGMA(VGG,NGG,VALMOY,SIGMA)
        REAL*4 VGG(*),VALMOY,SIGMA,SIG2
        INTEGER*4 K,NGG
C
C Error handling:
        IF(NGG.EQ.0)THEN
           VALMOY=0.
           SIGMA=0.
           PRINT *,' MEAN_SIGMA/ ERROR: wrong call with NGG=0' 
           RETURN
        ENDIF
C
C Now working correctly:
	VALMOY=0.
	 DO 20 K=1,NGG
	   VALMOY=VALMOY+VGG(K)
20	 CONTINUE
	 VALMOY=VALMOY/FLOAT(NGG)
C Calculates Sigma**2 with the actual mean VALMOY
99	 SIG2=0.
	 DO 30 K=1,NGG
	  SIG2=SIG2+(VALMOY-VGG(K))*(VALMOY-VGG(K))
30       CONTINUE
	 SIGMA=SQRT(SIG2/FLOAT(NGG))
 
	RETURN
	END
 
C---------------------------------------------------------------------
C Subroutine GENE_OFFSET to generate a constant offset
C------------------------------------------------------------------------
	SUBROUTINE GENE_OFFSET(IMAGE,CSF)
	REAL*4 MEAN(30)
	REAL*4 IMAGE(NPL,NL)
	REAL*4 CSF
	COMMON/PARAM/NPL,NL,NPL1,NPL2,NL1,NL2,NPLD,NPLF,
     1	NLD,NLF,MEAN,NXOUT,NYOUT
 
	  DO IY=NL1,NL2
	    DO IX=NPL1,NPL2
	     IMAGE(IX,IY)=CSF
	    END DO
	  END DO
	RETURN
	END
 
C------------------------------------------------------
C Function GET_PIXEL
C
C------------------------------------------------------
	REAL FUNCTION GET_PIXEL(IMAGE,IX,IY)
	REAL*4 MEAN(30)
	REAL*4 IMAGE(NPL,NL)
 
	COMMON/PARAM/NPL,NL,NPL1,NPL2,NL1,NL2,NPLD,NPLF,
     1	NLD,NLF,MEAN,NXOUT,NYOUT
 
	GET_PIXEL=IMAGE(IX,IY)
 
	RETURN
	END
 
C-----------------------------------------------------------
C	include 'jlpsub:jlp_vm_bdf.for'
