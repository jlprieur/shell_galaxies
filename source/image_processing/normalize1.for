C++******************************************************************
C NORMALIZE1
C
C To normalize one image to a similar image, determining the
C scale and zero difference between the two images by plotting
C the intensities in the two images against each other.
C
C JLP
C Version 10-04-90
C--******************************************************************
	PROGRAM NORMALIZE1
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*method
*       obtain input images and extract descriptor items. call pchist
*       to implement an auto-scaling function to determine a range of
*       data values to plot. obtain necessary parameters from the
*       environment. call nmplot to normalise the two images.
*
*starlink parameters
*       aimage
*              input image to normalize to
*       bimage
*              input image to be normalized
*       ilevel
*              interaction level...controls printing of results
*       pcrange
*              lower and upper histogram points (in percent) defining
*              the default (auto-scaled) data range used in image a
*       arange
*              lower and upper range of data values in image a, used
*              to override the autoscaling
*       nbin
*              number of bins to use in binning the scatter plot prior
*              to fitting a straight line
*       niter
*              number of iterations to reject bad data values
*       nsigma
*              number of standard deviations at which bad data is
*              rejected
*       minpix
*              minimum number of good pixels required in a bin before
*              it contributes to the fitted line
*       device
*              graphics device on which to plot the fitted line
*       e
*              output parameter giving the slope in the expression
*              b=e*a+c
*       c      output parameter giving the constant c in the
*              above expression
*
*calls
*              pchist,nmplot
*
*written by
*       r.f. warren-smith
*-----------------------------------------------------------------------
*
* NBINS for the histogram:
	PARAMETER(NBINS=10000)
	CHARACTER CVAL*1,PLOTDEV*32,NAMEA*40,NAMEB*40
	CHARACTER COMMENTSA*80,COMMENTSB*80,OPTIONS*200
	REAL*4 MMM,CCC,NSIGMA,PCRANGE(2),ARANGE(2)
	REAL*4 INVALA,INVALB
	INTEGER*4 MADRID(1)
	INTEGER*4 IHIST(NBINS),IPA,IPB,IP1,IP2,IP3,IP4,IP5
	COMMON /VMR/MADRID
 
	CALL JLP_BEGIN
	CALL JLP_INQUIFMT
 
	WRITE(6,27)
27	FORMAT(' Program NORMALIZE1,  Version 10-04-90',/,
     1	' To compute the linear transformation between the'
     1	' intensities of two images')
 
* Obtain user interaction level
	ILEVEL=2
*
* Obtain first input image
	WRITE(6,*) ' First image:'
	NAMEA=' '
	CALL JLP_VM_READIMAG(IPA,NPIXA,NLINEA,NAMEA,
     1	COMMENTSA)
 
*
* Obtain second input image
	WRITE(6,*) ' Second image:'
	NAMEB=' '
	CALL JLP_VM_READIMAG(IPB,NPIXB,NLINEB,NAMEB,
     1	COMMENTSB)
	INVALA=-32000.
	INVALB=INVALA
 
* call pchist to auto scale, giving the default data range to use
* for image a
*
	CALL PCMINMAX(MADRID(IPA),NPIXA,NLINEA,INVALA,XMIN,XMAX)
	WRITE(6,55) XMIN,XMAX
55	FORMAT(' Min, max for image 1:',2(G12.5,X))
	IF (XMIN.EQ.XMAX) THEN
	  WRITE(6,*) ' Fatal error: same value for the whole image 1'
	  GO TO 99
	ENDIF
 
* Obtain percentage histogram range for scaling the binning of
* image a
* (Between 0. and 1.)
	PCRANGE(1)=0.05
	PCRANGE(2)=0.95
	WRITE(6,56) PCRANGE(1),PCRANGE(2)
56	FORMAT(' We take the histogram between ',F5.2,' and',F5.2,
     1	' %')
 
	CALL PCHIST1(MADRID(IPA),NPIXA,NLINEA,INVALA,PCRANGE,ARANGE,
     1	IHIST,NBINS,XMIN,XMAX,IERR)
 
*
* if the image has no valid pixels, abort with error message
	IF (IERR.NE.0) THEN
	  WRITE(6,*) ' Fatal error: no valid pixels'
	  GO TO 99
	ENDIF
 
	WRITE(6,57) ARANGE(1),ARANGE(2)
57	FORMAT(' Which correspond to ',G12.5,' and',G12.5,
     1	' in real values')
*
* obtain number of bins for binning the scatter plot
	NBIN=50
*
* obtain the number of data rejection iterations
	NITER=2
*
* obtain rejection threshold
	NSIGMA=3.0
*
* obtain the minimum number of pixels required per bin
	MINPIX=5
*
* obtain plotting device
	WRITE(6,24)
24	FORMAT(' Output graphic device ?')
	READ(5,10) PLOTDEV
10	FORMAT(A)
 
*
* Obtain workspace
	I4=NBIN*4
	I8=NBIN*8
	CALL JLP_GETVM(IP1,I4)
	CALL JLP_GETVM(IP2,I4)
	CALL JLP_GETVM(IP3,I4)
	CALL JLP_GETVM(IP4,I8)
	CALL JLP_GETVM(IP5,I4)
*
* Call nmplot to perform normalization of image b to image a
*
	CALL NMPLOT(MADRID(IPA),NPIXA,NLINEA,INVALA,
     1	MADRID(IPB),NPIXB,NLINEB,INVALB,ILEVEL,ARANGE(1),
     1	ARANGE(2),NBIN,NITER,NSIGMA,MINPIX,PLOTDEV,MMM,CCC,IERRN,
     1	MADRID(IP1),MADRID(IP2),MADRID(IP3),MADRID(IP4),MADRID(IP5),
     1	NAMEA,NAMEB)
 
	IF (IERRN.NE.0) THEN
* if no fit was obtained, quit with error message
	  WRITE(6,22) IERRN
22	  FORMAT(' Fatal error in NMPLOT: fit has failed, IERRN= ',I3)
	ELSE
* Write fit parameters to environment:
* b=e*a+c
	  WRITE(6,23) MMM,CCC
23	  FORMAT(' Ratio image2/image1 RATIO21 =',G12.5,/,
     1	' Offset 2 - 1, OFF21 =',G12.5,/,
     1	' That is : image2_normalized = (image2-OFF21)/RATIO21 '/,
     1	' OR        image2 = image1 * RATIO21 + OFF21')
	ENDIF
 
99	CALL JLP_END
	STOP
	END
 
C*********************************************************************
C PCMINMAX
C Computes min/max:
C*********************************************************************
	SUBROUTINE PCMINMAX(IA,NPIX,NLINES,INVAL,XMIN,XMAX)
	REAL*4 IA(NPIX*NLINES),XMIN,XMAX,INVAL,WORK
	NN=NPIX*NLINES
	XMIN=IA(1)
	XMAX=IA(1)
	  DO 5 I=1,NN
	    WORK=IA(I)
	    IF(WORK.NE.INVAL)THEN
	      XMIN=MIN(XMIN,WORK)
	      XMAX=MAX(XMAX,WORK)
	    ENDIF
5	  CONTINUE
 
	RETURN
	END
 
	SUBROUTINE PCHIST1(IA,NPIX,NLINES,INVAL,PCRANGE,ARANGE,
     1	IHIST,NBINS,XMIN,XMAX,IERR)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*purpose
*       to find the integer value in an image corresponding to a
*       specific fraction of the image histogram from 0.0 to 1.0
*
*method
*       form a histogram of the image integers and scan up or down to
*       find the appropriate point
*
*arguments
*       ia (in)
*       real*4(npix*nlines)
*               the input image
*       npix,nlines (in)
*       integer
*               dimensions of ia
*       inval (in)
*       real
*               invalid pixel flag for ia
*       pcrange (in)
*       real(2)
*               array of fractional positions in the histogram in the
*               range 0.0 to 1.0
*       ihist (workspace)
*       integer(nbins)
*               used to hold the image histogram. contains histogram
*               on exit
*
*written by
*       r.f. warren-smith
*-----------------------------------------------------------------------
*
	REAL*4 IA(NPIX*NLINES),INVAL
	INTEGER*4 IHIST(NBINS)
	REAL*4 PCRANGE(2),XMIN,XMAX,ARANGE(2)
 
*
* initiallise histogram
*
	IERR=0
 
	DO 1 I=1,NBINS
	  IHIST(I)=0
1	CONTINUE
 
	STEP=(XMAX-XMIN)/FLOAT(NBINS-1)
*
* form a histogram of all the valid pixels
*
	NPTS=0
	NN=NPIX*NLINES
	DO 2 I=1,NN
         IF(IA(I).NE.INVAL) THEN
	   WORK=(IA(I)-XMIN)/STEP
           INTGR=NINT(WORK)+1
           IHIST(INTGR)=IHIST(INTGR)+1
           NPTS=NPTS+1
         ENDIF
2	CONTINUE
 
*
* if there are no valid pixels, exit with error flag set
      IF(NPTS.EQ.0) THEN
         IERR=1
         GO TO 99
      ENDIF
 
* Consider each percentage histogram point
*
      DO 4 I=1,2
 
* Calculate the number of data points corresponding to this point
* counting up or down depending on which side of the median
*
         IF(PCRANGE(I).LE.0.5) THEN
            LIMIT=NINT(NPTS*PCRANGE(I))
            ISTART=1
            IEND=NBINS
            IDIRN=1
         ELSE
            LIMIT=NINT(NPTS*(1.0-PCRANGE(I)))
            ISTART=NBINS
            IEND=1
            IDIRN=-1
         ENDIF
 
 
         IF(LIMIT.EQ.0) LIMIT=1
*
* Count through histogram to find this point
*
         N=0
 
         DO 3 J=ISTART,IEND,IDIRN
            N=N+IHIST(J)
            IF(N.GE.LIMIT) GO TO 8
3        CONTINUE
 
8        ARANGE(I)=J*STEP+XMIN
4     CONTINUE
 
99    RETURN
 
      END
 
	SUBROUTINE NMPLOT(IA,NPIXA,NLINEA,INVALA,IB,NPIXB,
     1	NLINEB,INVALB,ILEVEL,AMIN,AMAX,NBIN,NITER,NSIGMA,
     1	MINPIX,PLOTDEV,MMM,CCC,IERR,NSUM,ASUM,BSUM,B2SUM,VARLIM,
     1	NAMEA,NAMEB)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*Purpose
*       to normalize one image to another similar image by plotting
*       the intensities in each image against each other
*
*Method
*       Intensities which are valid in each input image and lie within
*       the data range to be used in image A are binned according to
*       the intensity in image A. A mean and standard deviation for the
*       B intensities are found for each bin. A straight line is fitted
*       to this binned data to determine the slope and intercept, from
*       which the B image may be normalized to the A image. Iterations
*       are performed to reject bad data by repeating the binning and
*       line fitting procedure, rejecting pixels whose B intensities
*       deviate by more than a specified number of standard deviations
*       from the line fitted in the previous iteration.
*
*Arguments
*       ia (in)
*       integer*2(npixa,nlinea)
*              first input image to be normalized to
*       npixa,nlinea (in)
*       integer
*              dimensions of image ia
*       invala (in)
*       real
*              invalid pixel flag for ia
*       ib (in)
*       integer*2(npixb,nlineb)
*              second input image...the one to be normalized
*       npixb,nlineb (in)
*       integer
*              dimensions of ib
*       invalb (in)
*       real
*              invalid pixel flag for ib
*       ilevel (in)
*       integer
*              interaction level...controls printing of results
*       amin,amax (in)
*       real
*              the range of data values to use in image a
*       nbin (in)
*       integer
*              the number of bins to use for the data
*       niter (in)
*       integer
*              number of data rejection iterations to use
*       NSIGMA (in)
*       real
*              rejection threshold in standard deviations for aberrant
*              data values
*       minpix (in)
*       integer
*              min. number of data values required before a bin is used
*       mmm (out)
*       real
*              slope of line fitted in the expression b=m*a+c
*       ccc (out)
*       real
*              constant c in the above expression
*       ierr (out)
*       integer
*              error flag: zero for success
*       nsum (work)
*       integer(nbin)
*              workspace
*       asum,bsum (work)
*       real(nbin)
*              workspace
*       b2sum (work)
*       double precision(nbin)
*              workspace
*       varlim (work)
*       real(nbin)
*              workspace
*
*notes
*       uses integer*2 arrays and subroutine names with more than
*       six characters
*
*written by
*       r.f. warren-smith
*-----------------------------------------------------------------------
*
	REAL*4 IA(NPIXA,NLINEA),IB(NPIXB,NLINEB)
	INTEGER*4 NSUM(NBIN)
	CHARACTER PLOTDEV*32,CHAR1*30,CHAR2*30,TITLE*40,NCHAR*4
	CHARACTER NAMEA*40,NAMEB*40,PCOLOR*30
	REAL*4 MMM,CCC,VARLIM(NBIN),ASUM(NBIN),BSUM(NBIN),RESID
	REAL*8 XSUM,X2SUM,YSUM,XYSUM,WTSUM,B2SUM(NBIN),X,Y,WT,
     1	ATOP,ABOT
	REAL*4 INVALA,INVALB
 
	IERR=0
 
*
* Set minimum expected variance for a bin
*
      Q0=0.5
      MMM=1.0
      CCC=0.0
 
*
* Set constants to convert intensity into bins
*
      AAMIN=MIN(AMIN,AMAX)
      AAMAX=MAX(AMIN,AMAX)
      A0=1.5-AAMIN*(NBIN-1)/MAX(1.0E-20,AAMAX-AAMIN)
      A1=(NBIN-1)/MAX(1.0E-20,AAMAX-AAMIN)
 
*
* initiallise the variance threshold for each bin
*
 
      DO 1 I=1,NBIN
         VARLIM(I)=1.0E20
1     CONTINUE
 
 
*
* perform niter iterations
*
      ITER=1
 
37    IF(ITER.LE.NITER)THEN
 
*
* initiallise the bins
*
 
         DO 2 I=1,NBIN
            NSUM(I)=0
            ASUM(I)=0.0
            BSUM(I)=0.0
            B2SUM(I)=0.0D0
2        CONTINUE
 
 
*
* scan the area common to both images
*
 
         DO 99 J=1,MIN(NLINEA,NLINEB)
 
            DO 98 I=1,MIN(NPIXA,NPIXB)
 
*
* use pixels which are ok on both images
*
 
               IF(IA(I,J).NE.INVALA.AND.IB(I,J).NE.INVALB) THEN
 
*
* check if the a intensity is in the required data range
*
                  A=IA(I,J)
                  IF(A.GE.AAMIN.AND.A.LE.AAMAX) THEN
 
*
* calculate the bin number and form sums for this bin
*
                     IBIN=NINT(A0+A1*A)
                     B=IB(I,J)
 		     RESID=(B-A*MMM-CCC)**2
                     IF(RESID.LE.VARLIM(IBIN))THEN
                        NSUM(IBIN)=NSUM(IBIN)+1
                        BSUM(IBIN)=BSUM(IBIN)+B
                        B2SUM(IBIN)=B2SUM(IBIN)+B*B
                        ASUM(IBIN)=ASUM(IBIN)+A
                     ENDIF
 
                  ENDIF
 
               ENDIF
 
98          CONTINUE
 
99       CONTINUE
 
*
* count the total number of pixels used
*
         NPIX=0
 
         DO 3 I=1,NBIN
            NPIX=NPIX+NSUM(I)
3        CONTINUE
 
	WRITE(6,47) NPIX
47	FORMAT(' Total number of pixels used:',I7)
 
* calculate the max. weight to be applied to any bin (this is the number
* of points per bin if the points are uniformly distributed)
*
         WTMAX=REAL(NPIX)/REAL(NBIN)
 
* initiallise sums for the straight line fit
*
         XSUM=0.0D0
         X2SUM=0.0D0
         YSUM=0.0D0
         XYSUM=0.0D0
         WTSUM=0.0D0
         ABOT=1.0D20
         ATOP=-1.0D20
 
*
* scan those bins with at least the minimum number of pixels in
*
 
         DO 4 I=1,NBIN
 
            IF(NSUM(I).GE.MINPIX) THEN
 
*
* form the weight for this bin
*
               WT=MIN(WTMAX,REAL(NSUM(I)))
 
*
* set the variance threshold in this bin for the next iteration
*
               VARLIM(I)=(NSIGMA**2)*(B2SUM(I)-(BSUM(I)**2)/NSUM(I))
     :          /NSUM(I)
               VARLIM(I)=MAX(VARLIM(I),Q0*(NSIGMA**2))
 
*
* Form the mean data values in each bin
*
               X=ASUM(I)/NSUM(I)
               Y=BSUM(I)/NSUM(I)
 
*
* Form weighted sums for fit
*
               XSUM=XSUM+X*WT
               X2SUM=X2SUM+X*X*WT
               YSUM=YSUM+Y*WT
               XYSUM=XYSUM+Y*X*WT
               WTSUM=WTSUM+WT
 
*
* Note the range of values used
*
               ATOP=MAX(ATOP,X)
               ABOT=MIN(ABOT,X)
 
            ELSE
 
*
* Set minimum variance for bins with too few points
*
               VARLIM(I)=(NSIGMA**2)*Q0
            ENDIF
 
4        CONTINUE
 
*
* If normal equations are singular, abort with ierr=1
*
         DET=WTSUM*X2SUM-XSUM*XSUM
 
         IF(DET.EQ.0.0) THEN
            IERR=1
	    WRITE(6,*) ' Null DET in NMPLOT'
            GO TO 77
         ENDIF
 
*
* form the straight line parameters (B=MMM*A+CCC)
*
         MMM=(WTSUM*XYSUM-XSUM*YSUM)
         CCC=(X2SUM*YSUM-XSUM*XYSUM)
         MMM=MMM/DET
         CCC=CCC/DET
 
*
* if required, print the results of this iteration
*
 
         IF(ILEVEL.GE.2) THEN
            WRITE(6,14)ITER,NPIX,ABOT,ATOP
14          FORMAT('   Iteration ',I3,' used ',I10,' pixels from A=',
     1	G13.6,' to ',G13.6)
            WRITE(6,15)MMM,CCC
15          FORMAT('   Fit gives:   B=(',G13.6,')*A + (',G13.6,')')
         ENDIF
 
 
*
* on the final iteration, plot the fit if required
*
 
         IF(ITER.EQ.NITER) THEN
 
* remove bins with no data in and form the values to plot
*
            NDATA=0
 
            DO 16 I=1,NBIN
 
               IF(NSUM(I).GE.MINPIX) THEN
                  NDATA=NDATA+1
                  ASUM(NDATA)=ASUM(I)/NSUM(I)
                  BSUM(NDATA)=BSUM(I)/NSUM(I)
                  VARLIM(NDATA)=SQRT(MAX(0.0D0,(B2SUM(I)-(BSUM(I)**2)
     :             *NSUM(I))/NSUM(I)))
               ENDIF
 
16          CONTINUE
 
*
* Plot graph
*
	    NCHAR='43'
            PCOLOR='Default'
	    WRITE(CHAR1,42) NAMEA(1:12)
	    WRITE(CHAR2,42) NAMEB(1:12)
42	    FORMAT('Int. of',A12)
	    TITLE=' Normalization plot'
            ASUM(2)=ASUM(NDATA)
            BSUM(1)=MMM*ASUM(1)+CCC
            BSUM(2)=MMM*ASUM(2)+CCC
C            CALL HIGR_DRLINE(ASUM,BSUM,2)
	    CALL NEWPLOT(ASUM,BSUM,NDATA,NBIN,1,CHAR1,CHAR2,
     1	TITLE,NCHAR,PCOLOR,PLOTDEV,' ',' ')
 
         ENDIF
 
 
*
* Count iterations and return for next
*
         ITER=ITER+1
         GO TO 37
 
      ENDIF
 
77    RETURN
      END
