C++**********************************************************************
C SP_SHIFT
C  To process radio HI spectra from PARKES (Observations from Nov. 1989)
C  Shift and add a series of spectra (with weight)
C
C JLP
C Version of 05-10-90
C--**********************************************************************
	PROGRAM SP_SHIFT
	PARAMETER (IDIM=512)
	REAL*4 X1(IDIM),Y1(IDIM),Y2(IDIM),VELO(IDIM),SPECT(IDIM)
	REAL*4 VLIGHT,CVELOCITY,VSTEP,VAL
	INTEGER*4 NPTS
	LOGICAL QUIET,SMOOTH
	CHARACTER ANS*1,INFILE*40,OUTFILE*40,COMMENTS*80,BUFFER*80
	CHARACTER TITLE*40,PLOTDEV*32,ANSWER*1
 
10	FORMAT(A)
 
	CALL JLP_BEGIN
 
	WRITE(6,63)
63	FORMAT(' Program SP_SHIFT Version 07-09-90',/,
     1	' To shift and add spectra from Parkes')
 
	NPTS=256
 
C Number of files:
	WRITE(6,*) ' Number of spectra ? (>=1)'
	READ(5,*) NFILES
 
	WRITE(6,*) ' Hanning smoothing (3rd order)? [Y]'
	READ(5,10) ANS
	SMOOTH=(ANS.NE.'N'.AND.ANS.NE.'n')
 
C Input file (First one is the reference...)
78	WRITE(6,*) ' Input ASCII file (The first one is the reference...)?'
	READ(5,10) INFILE
 
	OPEN(1,FILE=INFILE,STATUS='OLD',ACCESS='SEQUENTIAL')
	READ(1,10) BUFFER
	READ(BUFFER,*) X1(1),Y1(1)
	COMMENTS=BUFFER(27:80)
	  DO I=2,NPTS
	   READ(1,*) X1(I),Y1(I)
	  END DO
	CLOSE(1)
 
	CVELOCITY=X1(128)
	WRITE(6,28) COMMENTS(1:53),CVELOCITY
28	FORMAT(' Comments: ',A53,/,' Central velocity:',G12.3)
 
* Weighting factor:
	WRITE(6,*) ' Weight for the sum (1.?)'
	READ(5,*) WEIGHT
 
* Transfer:
	DO I=1,NPTS
	  VELO(I)=X1(I)
	  SPECT(I)=Y1(I)*WEIGHT
	END DO
 
* Main loop:
	DO K=1,NFILES-1
 
C Input file (First one is the reference...)
	  WRITE(6,*) ' Input ASCII file (spectrum to shift)?'
	  READ(5,10) INFILE
 
	  OPEN(1,FILE=INFILE,STATUS='OLD',ACCESS='SEQUENTIAL')
	  READ(1,10) BUFFER
	  READ(BUFFER,*) X1(1),Y1(1)
	    DO I=2,NPTS
	      READ(1,*) X1(I),Y1(I)
	    END DO
	  CLOSE(1)
	  WRITE(6,28) BUFFER(27:80),X1(128)
 
* Weighting factor:
	  WRITE(6,*) ' Weight for the sum (1.?)'
	  READ(5,*) WEIGHT
 
	  SHIFT=(X1(128)-CVELOCITY)/(X1(127)-X1(128))
	  WRITE(6,*) ' Shifting value (pixels):',SHIFT
	  CALL SHIFTSP(Y1,Y2,SHIFT,NPTS)
 
* Transfer:
	  DO I=1,NPTS
	    SPECT(I)=SPECT(I)+Y2(I)*WEIGHT
	  END DO
 
	END DO
 
* Hanning smoothing:
	DO I=2,NPTS-1
	  VAL=SPECT(I-1)+2.*SPECT(I)+SPECT(I+1)
	  SPECT(I)=VAL/4.
	END DO
 
* Write the output spectrum as an ASCII file:
* with some comments on the first line, after the data:
	PRINT *,' Output ASCII file (spectrum) ?'
	READ(5,10) OUTFILE
	OPEN(2,FILE=OUTFILE,STATUS='NEW',ACCESS='SEQUENTIAL')
	WRITE(2,66) VELO(1),SPECT(1),COMMENTS(1:18),CVELOCITY
66	FORMAT(2(X,G14.7),' ',A,' CVEL:',F9.2)
	DO I=2,NPTS
	  WRITE(2,*) VELO(I),SPECT(I)
	END DO
	CLOSE(2)
 
	CALL JLP_END
	STOP
	END
C@**********************************************************************
C REBINX
C@**********************************************************************
 
	subroutine rebinx(input1,npixa,invala,invalb,minx,maxx,
     1	c,scale,mode,output1,npixb,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*purpose
*       to resample a spectrum at positions given by a linear
*       transformation of the output pixel positions
*
*method
*       scan the output image, transforming the pixel positions.
*       interpolate in the input image to determine the image value
*       at these points. the routine uses nearest-neighbour or linear
*       interpolation between the 4 nearest pixels
*
*arguments
*       input1 (in)
*       real*4(idim)     (npixa)
*               the input spectrum
*       npixa (in)
*       integer
*               the dimensions of input
*       invala (in)
*       integer
*               invalid pixel flag for input
*       invalb (in)
*       integer
*               invalid pixel flag for output1
*       minx,maxx (in)
*       integer
*               range of output pixels to be replaced
*       c (in)
*       real(6)
*               coefficients giving the transformation from output
*               positions to input positions
*       scale (out)
*       real
*               scale factor which is needed if total image intensity
*               is to be conserved
*       mode (in)
*       integer
*               type of interpolation
*               1: nearest neighbour
*               2: linear
*               3: constant noise
*       output1 (in/out)
*       real*4(idim)     (npixb)
*               output image
*       npixb (in)
*       integer
*               dimensions of output1
*       ierr (out)
*       integer
*               error flag: zero for success
*
*written by
*       r.f. warren-smith
*-----------------------------------------------------------------------
*
	PARAMETER (IDIM=600)
	REAL*4 INPUT1(IDIM),OUTPUT1(IDIM)
	INTEGER XMIN,XMAX,X,XCEN
	REAL C(2),WX(-1:+1)
 
*
* SET UP WEIGHT ARRAYS FOR CONSTANT NOISE INTERPOLATION
*
	DATA WX(0)/1.0/
 
*
* CHECK ARGUMENT VALIDITY
*
 
	IF(MINX.GT.MAXX) THEN
	   IERR=1
 
	ELSE
	   IERR=0
 
*
* Restrict max and min x limits to lie in output image
*
	   XMIN=MIN(MAX(1,MINX),NPIXB)
	   XMAX=MIN(MAX(1,MAXX),NPIXB)
 
*
* Restrict interpolation method to be 1 to 3
*
	   METHOD=MIN(MAX(1,MODE),3)
 
*
* Scale factor to conserve counts is (abs. value of determinant)
*
	   SCALE=ABS(C(2))
 
*
* Scan the selected area of the output image (location x) and
* calculate the transformed position (xdash) in the input
* image
*
	      XREF=C(1)
 
	      DO 11 X=XMIN,XMAX
	 	  XDASH=XREF+C(2)*X
*
* find nearest pixel location
*
	         XCEN=NINT(XDASH)
 
*
* if nearest pixel lies outside input image, output pixel is invalid
* otherwise continue with interpolation
*
 
	         IF((XCEN.LT.1).OR.(XCEN.GT.NPIXA)) THEN
	            OUTPUT1(X)=INVALB
 
	         ELSE
 
*
* for nearest-neighbour interpolation, output pixel=nearest pixel,
* -----------------------------------
* or is invalid if input pixel is invalid
*
 
           IF(METHOD.EQ.1) THEN
 
               IF(INPUT1(XCEN).EQ.INVALA) THEN
                  OUTPUT1(X)=INVALB
 
               ELSE
                  OUTPUT1(X)=INPUT1(XCEN)
               ENDIF
 
 
*
* for linear interpolation, output pixel is invalid if nearest input
* ------------------------
* pixel is invalid. otherwise continue with interpolation
*
 
        ELSE IF(METHOD.EQ.2) THEN
 
           IF(INPUT1(XCEN).EQ.INVALA) THEN
              OUTPUT1(X)=INVALB
 
           ELSE
 
*
* find shift from next lowest pixel,line location
*
              I=XDASH
              DX=XDASH-I
*
* initiallise sums for forming weighted mean
*
              SUM=0.0
              WTSUM=0.0
*
* form weighted mean of adjacent 4 pixels, checking that each lies
* within the input image and is not invalid
*
                IF(I.GE.1) THEN
 
                   IF(INPUT1(I).NE.INVALA) THEN
*
* weight is calculated from the x shift from integer pixel locations
*
                      WT=(1.0-DX)*(1.0-DY)
                      SUM=SUM+INPUT1(I)*WT
                      WTSUM=WTSUM+WT
                   ENDIF
                ENDIF
 
 
               IF(I+1.LE.NPIXA) THEN
 
                   IF(INPUT1(I+1).NE.INVALA) THEN
                        WT=DX*(1.0-DY)
                        SUM=SUM+INPUT1(I+1)*WT
                        WTSUM=WTSUM+WT
                   ENDIF
              ENDIF
 
 
*
* assign weighted mean to output pixel (wtsum cannot be zero, since
* at least 1 input pixel must be valid)
*
              OUTPUT1(X)=SUM/WTSUM
        ENDIF
 
*
* for constant noise interpolation (output noise independent of
* --------------------------------
* resampling phase)
*
 
        ELSE IF(METHOD.EQ.3) THEN
 
*
* if nearest pixel is invalid, so is output pixel. otherwise continue
* with interpolation
*
 
            IF(INPUT1(XCEN).EQ.INVALA) THEN
                 OUTPUT1(X)=INVALB
 
            ELSE
 
*
* calculate the shift from the nearest pixel,line position
*
                DX=XDASH-XCEN
 
*
* calculate the x and y weight arrays (dependent on the phase dx,dy)
*
                R1=DX*DX+0.25
                WX(-1)=R1-DX
                WX(1)=R1+DX
 
*
* now scan the 9 nearest pixels, forming a weighted sum of all the
* valid ones
*
                SUM=0.0
                WTSUM=0.0
 
*
* check we are still in the image
*
                DO 21 ISHIFT=-1,1
                    II=XCEN+ISHIFT
                    IF(II.GE.1.AND.II.LE.NPIXA) THEN
*
* include the pixel if it is valid
*
                         IF(INPUT1(II).NE.INVALA) THEN
                               WT=WX(ISHIFT)
                               SUM=SUM+INPUT1(II)*WT
                               WTSUM=WTSUM+WT
                         ENDIF
                    ENDIF
 
21             CONTINUE
 
*
* assign the interpolated value to the output pixel
*
                OUTPUT1(X)=SUM/WTSUM
              ENDIF
 
            ENDIF
          ENDIF
 
11	CONTINUE
 
12	CONTINUE
 
	ENDIF
 
	RETURN
 
	END
 
C**********************************************************************
C Subroutine SHIFTSP
C To shift one spectrum of a given quantity
C (Interface with REBIN)
C**********************************************************************
	SUBROUTINE SHIFTSP(INPUT,OUTPUT,SHIFT,NPTS)
	REAL*4 INPUT(*),OUTPUT(*)
	REAL*4 SHIFT,C(2)
	INTEGER*4 NPTS,METHOD
	LOGICAL CONSRV
 
*
* Input image sucessfully obtained...
* obtain position transformation coefficients
*
*   Xnew  is  C * Xold
 
	C(1)=SHIFT
	C(2)=1.
*
* Obtain type of interpolation required (2=linear)
*
	 METHOD=2
*
* Determine if total data count is to be conserved
*
	 CONSRV=.FALSE.
*
* Obtain size of output image, using input size as default
*
	 NPIX=NPTS
	 NPOUT=NPIX
*
* Defines the parameters used by EDRSLIB (integer files)
	INVALA=-100000
	INVALB=0
	SCALE=1.0
	ZERO=0.0
*
* Call rebin to resample the input image over the entire area
* covered by the output image
*
	CALL REBINX(INPUT,NPIX,INVALA,INVALB,1,NPOUT,
     1	C,SCALE,METHOD,OUTPUT,NPOUT,IERR)
 
	RETURN
	END
