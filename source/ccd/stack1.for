C++************************************************************************
C STACK1 :
C To combine mutually aligned and normalised images into a
C single output image
C Methods of combining the images: mean, median, mode, minimum, maximum.
C
C Calls EDRS routines (included here): LINSTK, COMBIN, WMODE, NTHMIN
C
C JLP
C Version of 05-04-94
C--************************************************************************
	PROGRAM STACK1
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*Method
*       obtain each input image and extract the required descriptor
*       items. obtain an output image. treat each line in turn...
*       copy one line from each image into workspace using linstk, then
*       combine them into a single output line using combin. when all
*       lines have been treated, update the output descriptor items
*
*
*Starlink parameters
*       method
*               the method of combining the images
*		mean, median, mode, minimum, maximum
*       errors
*               estimates of the image errors..used in weighting the
*               images
*       minimage
*               minimum number of good images required at any point
*       comments
*               comments to replace the comments of 'input1' in the output
*               image
*
* from "stack" (r.f. warren-smith)
*-----------------------------------------------------------------------
* Set maximum number of images which can be stacked
*
	PARAMETER (MAXIM=20)
	INTEGER*4 MADRID(1),IPLST,IPOUT
	CHARACTER CVAL*1,COMMENTS*80,IMGCHR*8,NAMEOUT*40
	CHARACTER NAMEIN(MAXIM)*40
*
* Dimension arrays to hold details of each image
*
	REAL WEIGHT(MAXIM),WORK(MAXIM,2)
	INTEGER INVAL(MAXIM),NPIX(MAXIM),NLINES(MAXIM),IP(MAXIM)
	COMMON /VMR/MADRID
 
*
* Initiallise size of output image
*
	NPOUT=1
	NLOUT=1
 
*
* Count through each possible input image
*
	CALL JLP_BEGIN
	CALL JLP_INQUIFMT
 
	WRITE(6,*) ' Enter the input images  (to stop: "return")'
	WRITE(6,*) ' (Maximum number: 5 or sometimes 6 images 1024x1024 on "ts" ...)' 
 
	DO 12 IM=1,MAXIM
 
* Obtain the input image
*
	WRITE(6,*) ' Enter next input image (to stop: "return"): '
	NAMEIN(IM)=' '
	READ(5,10) NAMEIN(IM)
10	FORMAT(A)
	IF(NAMEIN(IM)(1:2).NE.'  ')THEN
	  CALL JLP_VM_READIMAG(IP(IM),NPIX(IM),NLINES(IM),
     1	NAMEIN(IM),COMMENTS)
*
* If no image was obtained, and im less than 1 abort,
* otherwise end the input loop.
*
	ELSE
          IF(IM.LE.2) THEN
	    WRITE(6,*) ' Fatal error: only one image!'
            GO TO 99
          ELSE
            GO TO 101
          ENDIF
 
	ENDIF
 
*
* If image was obtained successfully, set output image size to the
* largest image so far
*
	  NPOUT=MAX(NPOUT,NPIX(IM))
	  NLOUT=MAX(NLOUT,NLINES(IM))
*
* Store the necessary descriptor information from the image
*
	  INVAL(IM)=-32000
c            CALL GTDSCR(IMGCHR,'inval','integer',INVAL(IM),RVAL,CVAL
c     :       ,IERR)
 
12    CONTINUE
 
 
*
* Set number of images being used and obtain workspace
*
101   IM=IM-1
	WRITE(6,*) IM,' images to stack'
	I=NPOUT*IM*4
	CALL JLP_GETVM(IPLST,I)
*
* Obtain output image frame
*
	I=NPOUT*NLOUT*4
	CALL JLP_GETVM(IPOUT,I)
*
* Output image obtained successfully...obtain required method
* of combining images
*
	 WRITE(6,50)
50	 FORMAT(' Possible methods : 1=MEAN, 2=MEDIAN, 3=MODE,',
     1	' 4=MINIMUM, 5=MAXIMUM',/,
     1	' Enter the required method of combining the images :')
	 READ(5,*) IMETH
 
*
         WRITE(6,*) ' Output image to store the result :'
         READ(5,10) NAMEOUT

* Obtain minimum number of valid values required at each pixel
*
C	 WRITE(5,58)
C58	 FORMAT(' Minimum number of valid values (i.e. .NE.-32000) required at',
C     1	' each pixel')
C	 READ(5,*) NMIN
C JLP94:
         NMIN=MAX(3,IM-2)
 
*
* set default weights for images
 
         DO 61 I=1,IM
            WEIGHT(I)=1.0
61       CONTINUE
 
*
* if the method requires weights to be assigned to each image,
* (1=mean, 3=mode)
* obtain error estimates for each image
 
         IF((IMETH.EQ.1).OR.(IMETH.EQ.3)) THEN
64	 WRITE(6,57) IM
57	 FORMAT(' Enter error estimates for each image',
     1	' (Used to assign a weight to each image),(',I3,' values needed)')
	    READ(5,*) (WEIGHT(I),I=1,IM)
 
*
* Convert errors to weights
*
            DO 62 I=1,IM
 
               IF(WEIGHT(I).LT.1.0E-09.OR.WEIGHT(I).GT.1.0E09) THEN
*
* If any error estimate was invalid, give message and return
* for new entry
*
	          WRITE(6,*) ' Bad value'
                  GO TO 64
               ELSE
                  WEIGHT(I)=(1.0/WEIGHT(I))**2
               ENDIF
 
62          CONTINUE
 
         ENDIF
 
*
* Obtain descriptor items for output image
*
	 INVALB=-32000
 
* COUNT THROUGH THE OUTPUT IMAGE LINES
 
         DO 500 J=1,NLOUT
 
* COPY THE CORRESPONDING INPUT LINE OF EACH IMAGE INTO THE WORKSPACE
* USING LINSTK
 
            DO 498 IMAGE=1,IM
               CALL LINSTK(MADRID(IP(IMAGE)),NPIX(IMAGE),NLINES(IMAGE),
     1	INVAL(IMAGE),J,MADRID(IPLST),NPOUT,IMAGE)
498         CONTINUE
 
*
* CALL COMBIN TO COMBINE THE INPUT LINES INTO A SINGLE OUTPUT LINE
* USING THE METHOD SPECIFIED IN IMETH
*
	  CALL COMBIN(MADRID(IPLST),NPOUT,IM,WEIGHT,IMETH,
     1	INVAL,NMIN,INVALB,J,MADRID(IPOUT),WORK)
500      CONTINUE
 
 
	J=MIN(4,IM)
	WRITE(COMMENTS,11)IM,(NAMEIN(I)(1:12),I=1,J)
11	FORMAT(' Stack of',I2,' images: ',4(A,1X),'...')
	CALL JLP_WRITEIMAG(MADRID(IPOUT),NPOUT,NLOUT,NPOUT,
     1	NAMEOUT,COMMENTS)
99	CALL JLP_END
 
	STOP
	END
 
C@*********************************************************************
C LINSTK  (Called by STACK)
c@*********************************************************************
      SUBROUTINE LINSTK(IA,NPIXA,NLINEA,INVALA,LINEA,IB,NPIXB,NIMAGE)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*purpose
*       to copy a specified line from an image into a stack of image
*       lines taken from elsewhere
*
*method
*       copy the specified line to a specified stack location. pad the
*       line with invalid pixels to make up any extra length required.
*       replace it with invalid pixels if the line specified lies
*       outside the input image
*
*arguments (in=input, ou=output)
*       ia (in)             real*4(npixa,nlinea)     the input image
*
*       npixa,nlinea (in)   integer                  dimensions of ia
*
*       invala (in)         integer                  invalid pixel flag for ia
*
*       linea (in)          integer      specifies the line to be copied from ia
*
*       ib (in/out)         real*4(npixb,*)
*				 the stack of image lines, each of length npixb
*       npixb (in)          integer                  the length of lines in ib
*
*       nimage (in)         integer
*                            the location in the stack ib into which the line is
*                            to be copied
*-----------------------------------------------------------------------
*
*
	REAL*4 IA(NPIXA,NLINEA),IB(NPIXB,*)
 
*
* if the line required lies outside the input image, fill the
* corresponding workspace with invalid pixel values
*
 
      IF(LINEA.GT.NLINEA) THEN
 
         DO 1 I=1,NPIXB
            IB(I,NIMAGE)=INVALA
1        CONTINUE
 
      ELSE
*
* otherwise copy the required line to the workspace
*
         DO 2 I=1,NPIXB
*
* if the workspace has longer lines than the image, pad out with
* invalid pixel values
*
            IF(I.LE.NPIXA) THEN
               IB(I,NIMAGE)=IA(I,LINEA)
            ELSE
               IB(I,NIMAGE)=INVALA
            ENDIF
 
2        CONTINUE
 
      ENDIF
 
      RETURN
      END
 
C@*********************************************************************
C COMBIN (Called by STACK)
C@*********************************************************************
      SUBROUTINE COMBIN(IA,NPIX,NIMAGE,WEIGHT,METH,INVAL
     : ,NMIN,INVALB,LINEB,IB,WORK)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*purpose
*       to combine a number of lines from different images into a
*       single output line
*
*method
*       work along the line, combining the values from each input image
*       using either a weighted mean, the median, the mode, the maximum
*       or the minimum, taking due account of invalid pixels in each image.
*
*arguments
*       ia (in)
*       real*4(npix,nimage)
*               the input stack of image lines
*       npix (in)
*       integer
*               the number of pixels per line
*       nimage (in)
*       integer
*               the number of image lines in the stack ia
*       weight (in)
*       real(nimage)
*               weights to be used with each image if using mean or
*               mode
*       meth (in)
*       integer
*               method to be used in the range 1 to 5
*       inval (in)
*       integer
*               invalid pixel flags for input image lines
*       nmin (in)
*       integer
*               minimum number of images required at each pixel before
*               a valid output value is generated
*       invalb (in)
*       integer
*               invalid pixel flag for output image
*       lineb (in)
*       integer
*               the line in the output image ib into which the result
*               is to be put
*       ib (in/out)
*       real*4(npix,*)
*               the output image into which the resultant line is
*               inserted
*       work (workspace)
*       real(nimage,2)
*               used to store intermediate values in the computations
*
*calls
*               nthmin,wmode
*
*-----------------------------------------------------------------------
*
*
      REAL*4 IA(NPIX,NIMAGE),IB(NPIX,*)
      REAL WEIGHT(NIMAGE),WORK(NIMAGE,2)
      INTEGER INVAL(NIMAGE)
 
*
* set probability of a pixel being corrupt (used in the routine wmode)
* 0.01 is a safe value for electronographs
*
      PARAMETER (PBAD=0.01)
 
*
* set accuracy criterion and limit the method to the range 1-5
*
      TOLL=0.5
      METHOD=MIN(MAX(1,METH),5)
 
*
* count through each pixel in the line
*
 
      DO 100 I=1,NPIX
 
*
* initiallise counter for good images at each pixel location
*
         NGOOD=0
 
*
* if the mean is required:
* ------------------------
*
 
         IF(METHOD.EQ.1) THEN
            SUM1=0.0
            SUM2=0.0
 
*
* form weighted mean of valid values
*
 
            DO 901 J=1,NIMAGE
 
               IF(IA(I,J).NE.INVAL(J)) THEN
                  NGOOD=NGOOD+1
                  SUM1=SUM1+WEIGHT(J)
                  SUM2=SUM2+WEIGHT(J)*IA(I,J)
               ENDIF
 
901         CONTINUE
 
 
*
* if insufficient images were good, abort and set output invalid
*
 
            IF(NGOOD.GE.NMIN) THEN
               B=SUM2/SUM1
 
            ELSE
               GO TO 999
 
            ENDIF
 
 
*
* if the median is required:
* --------------------------
*
 
         ELSE IF(METHOD.EQ.2) THEN
 
*
* extract a list of the valid values
*
 
            DO 902 J=1,NIMAGE
 
               IF(IA(I,J).NE.INVAL(J)) THEN
                  NGOOD=NGOOD+1
                  WORK(NGOOD,1)=IA(I,J)
               ENDIF
 
902         CONTINUE
 
 
*
* if sufficient points are present, call nthmin to find the median
* value
*
 
            IF(NGOOD.GE.NMIN) THEN
               NMED=NGOOD/2+1
               CALL NTHMIN(WORK(1,1),NGOOD,NMED,WORK(1,2),IERR)
 
*
* median lies between two values if the number of values is even
*
 
               IF(MOD(NGOOD,2).EQ.0) THEN
                  B=(WORK(1,2)+WORK(2,2))*0.5
 
               ELSE
                  B=WORK(1,2)
               ENDIF
 
 
            ELSE
               GO TO 999
 
            ENDIF
 
 
*
* if the mode is required:
* ------------------------
*
 
         ELSE IF(METHOD.EQ.3) THEN
 
*
* form a list of the valid values and their weights
*
 
            DO 903 J=1,NIMAGE
 
               IF(IA(I,J).NE.INVAL(J)) THEN
                  NGOOD=NGOOD+1
                  WORK(NGOOD,1)=IA(I,J)
                  WORK(NGOOD,2)=WEIGHT(J)
               ENDIF
 
903         CONTINUE
 
 
*
* if sufficient values are valid, call wmode to find the maximum
* likelihood value (output in real b)
*
 
            IF(NGOOD.GE.NMIN) THEN
               CALL WMODE(WORK(1,1),WORK(1,2),NGOOD,PBAD,3,TOLL,B,
     1	SIGMA)
 
            ELSE
               GO TO 999
 
            ENDIF
 
 
*
* if the minimum is required:
* ---------------------------
*
 
         ELSE IF(METHOD.EQ.4) THEN
            B=1.0E20
 
*
* find the minimum of the valid values
*
 
            DO 904 J=1,NIMAGE
 
               IF(IA(I,J).NE.INVAL(J)) THEN
                  NGOOD=NGOOD+1
                  B=MIN(B,IA(I,J))
               ENDIF
 
904         CONTINUE
 
 
            IF(NGOOD.LT.NMIN) GO TO 999
 
*
* if the maximum is required:
* ---------------------------
*
 
         ELSE IF(METHOD.EQ.5) THEN
            B=-1.0E20
 
*
* find the maximum of the valid values
*
 
            DO 905 J=1,NIMAGE
 
               IF(IA(I,J).NE.INVAL(J)) THEN
                  NGOOD=NGOOD+1
                  B=MAX(B,IA(I,J))
               ENDIF
 
905         CONTINUE
 
 
            IF(NGOOD.LT.NMIN) GO TO 999
         ENDIF
 
 
*
* the output value is now known...
*
	 IB(I,LINEB)=B
 
         GO TO 100
 
*
* pixels with insufficient valid input images are here set invalid
* on output:
*
999      IB(I,LINEB)=INVALB
100   CONTINUE
 
      RETURN
 
      END
c@*********************************************************************
c wmode
c@*********************************************************************
      SUBROUTINE WMODE(X,W,NX,PBAD,NITER,TOLL,XMODE,SIGMA)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*purpose
*       to find the most likely mean value for a set of data points
*       with different normally distributed errors and a constant
*       probability of being corrupt.
*
*method
*       this routine uses the statistical model used by mode to
*       minimise the effect of corrupt data, but includes different
*       weights for each data point, to allow for different intrinsic
*       errors. see mode for a fuller description of the algorithm.
*
*arguments
*       x (in)
*       real(nx)
*               an array of data values
*       w (in)
*       real(nx)
*               an array of weights for each data point in x. the
*               weights are inversely proportional to the square of
*               the relative errors on each data point.
*       nx (in)
*       integer
*               the number of data points
*       pbad (in)
*       real
*               an estimate of the probability that any one data point
*               is corrupt. (this value not very critical)
*       niter (in)
*       integer
*               the maximum number of iterations to be performed
*       toll (in)
*       real
*               the absolute accuracy required in the mean value.
*               iterations cease when two successive iterations agree
*               to within this value.
*       xmode (out)
*       real
*               the estimate of the uncorrupted mean value
*       sigma (out)
*       real
*               an estimate of the uncorrupted normalised standard
*               deviation of the data points. an estimate of the
*               standard deviation of any one data point is:
*                   sigma/sqrt(w) , where w is its weight.
*
*written by
*       r.f. warren-smith
*-----------------------------------------------------------------------
*
*
      REAL X(NX),W(NX)
      DOUBLE PRECISION SUM1,SUM2,SUM3
 
*
* Form sums for finding the mean and standard deviation of the
* data
*
      SUM1=0.0D0
      SUM2=0.0D0
      SUM3=0.0D0
 
      DO 1 I=1,NX
         DATA=W(I)
         SUM1=SUM1+DATA
         DATA=DATA*X(I)
         SUM2=SUM2+DATA
         DATA=DATA*X(I)
         SUM3=SUM3+DATA
1     CONTINUE
 
 
*
* Calculate the weighted mean and variance
*
      XMODE=SUM2/SUM1
      SIG2=(SUM3-(SUM2*SUM2)/SUM1)/NX
      SIG2=MAX(0.0,SIG2)
 
*
* Having formed the initial estimates, the main iteration loop
* starts here
*
 
      DO 3 ITER=1,NITER
 
*
* Calculate the standard deviation and normalisation constants
*
         SIG2=MAX(1.0E-20,SIG2)
         SIG1=SQRT(SIG2)
         W1=1.0/SIG1
         W2=0.5/SIG2
         PBNORM=0.707107*PBAD
 
*
* Set limit at which points are negligible at 10 stand. devs.
*
         DEVLIM=100.0*SIG2
 
*
* Initiallise sums for finding new estimates
*
         SUMA=0.0
         SUMB=0.0
         SUMC=0.0
         SUMD=0.0
 
*
* Count through data points
*
 
         DO 2 I=1,NX
 
*
* Find deviation from mode, normalised to that expected from
* the weights supplied
*
            DX=X(I)-XMODE
            DX2=DX*DX
            DEV2=DX2*W(I)
 
*
* If point is not negligible, calculate the fractional probability
* that it is good
*
 
            IF(DEV2.LE.DEVLIM) THEN
               EX=EXP(-W2*DEV2)
               PROB=EX/(EX+PBNORM)
               SUMA=SUMA+PROB
               SUMB=SUMB+PROB*W(I)
               SUMC=SUMC+DX*PROB*W(I)
               SUMD=SUMD+DEV2*PROB
            ENDIF
 
2        CONTINUE
 
 
*
* Calculate the new estimates
*
         SUMA=MAX(SUMA,1.0E-20)
         SUMB=MAX(SUMB,1.0E-20)
         DXMODE=SUMC/SUMB
         XMODE=XMODE+DXMODE
         SIG2=SUMD/SUMA
 
*
* If required accuracy has been achieved, exit iteration loop
*
 
         IF(ABS(DXMODE).LE.TOLL) THEN
            GO TO 7
 
         ENDIF
 
3     CONTINUE
 
7     SIGMA=SQRT(SIG2)
      RETURN
 
      END
 
c@*********************************************************************
c nthmin
c@*********************************************************************
      SUBROUTINE NTHMIN(X,NX,N,STAK,IERR)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*Purpose
*       to find the n'th smallest number in a set of data values
*
*Method
*       maintain a stack of the smallest values. compare each data
*       point with the top of stack.. if smaller, insert it in the
*       stack at an appropriate level to maintain non-increasing
*       stack order. top of stack is lost when data is inserted.
*
*Arguments
*       x (in)
*       real(nx)
*               array of data values
*       nx (in)
*       integer
*               number of data points
*       n (in)
*       integer
*               specifies the n in 'nth smallest data value'
*       stak (out)
*       real(n)
*               stack of n smallest values.. n'th smallest in stak(1)
*       ierr (out)
*       integer
*               error flag: zero for success
*
*Written by
*       r.f. warren-smith
*-----------------------------------------------------------------------
*
*
      REAL X(NX),STAK(N)
      PARAMETER (EXTREM=1.0E20)
 
*
* Check argument validity
*
 
      IF(N.GT.NX) THEN
         IERR=1
 
      ELSE
         IERR=0
 
*
* Initiallise the stack of smallest values
*
 
         DO 1 I=1,N
            STAK(I)=EXTREM
1        CONTINUE
 
 
*
* Compare each data value with the top of stack
*
 
         DO 6 J=1,NX
 
            IF(X(J).LT.STAK(1)) THEN
 
*
* If less than top of stack, it belongs in the stack.. search down
* the stack, moving contents up.
*
 
               DO 2 LOCN=2,N
 
                  IF(X(J).LT.STAK(LOCN)) THEN
 
                     STAK(LOCN-1)=STAK(LOCN)
 
                   ELSE
* When correct level is found, insert data in stack
*
                     STAK(LOCN-1)=X(J)
                     GO TO 61
 
                  ENDIF
 
2              CONTINUE
 
 
*
* Arrive here if data point belongs on bottom of stack
*
               STAK(N)=X(J)
61             CONTINUE
            ENDIF
 
6        CONTINUE
 
      ENDIF
 
      RETURN
      END
