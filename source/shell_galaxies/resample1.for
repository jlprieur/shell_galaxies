C++**********************************************************************
C RESAMPLE1
C  From "RESAMPLE" (EDRS, R.F. Warren-Smith)
C  Resamples an image at points defined by transforming the
C  pixel coordinates of the output image. 
C  For interpolation 3 options are available: 
C     -- nearest-neighbour 
C     -- linear interpolation between the 4 nearest pixels, 
C     -- constant noise interpolation (between the 9 nearest pixels).
C
C  First gets the input, output image filenames and transform coefficients.
C  Calls rebin to resample the image by the required method.
C
C SYNTAX:
C     RUNS RESAMPLE1 in_image coeff_file out_image
C Example:
C     RUNS RESAMPLE1 name name.XYF name_SHIFT
C
C Nota1: the coefficient file is simply an ASCII file with 6 values.
C        This file is generally produced by XYFIT1.
C
C new_coordinates= matrix old_coordinates:
C    input(x_new) = input(C1 + C2 x_old + C3 y_old)
C         (y_new)        (C4 + C5 x_old + C6 y_old)
C
C JLP
C Version of 17-12-98
C--**********************************************************************
	PROGRAM RESAMPLE1
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*
*STARLINK PARAMETERS
*       INPUT
*               THE INPUT IMAGE
*       METHOD
*               SPECIFIES THE RESAMPLING INTERPOLATION METHOD
*       CONSERVE
*               (LOGICAL) IF SET TRUE, THE IMAGE IS RESCALED TO
*               CONSERVE TOTAL DATA SUM
*       NPIXOUT
*               NUMBER OF PIXELS PER LINE IN THE OUTPUT IMAGE
*       NLINEOUT
*               NUMBER OF LINES IN THE OUTPUT IMAGE
*       OUTPUT
*               OUTPUT IMAGE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*
* Calls EDRS routine (included here): REBIN
*-----------------------------------------------------------------------
*
        REAL ANGLE,COEF1(6),WX,WY
	INTEGER*4 NAXIS(2),METHOD,ISIZE
        INTEGER*4 NX_IN,NY_IN,NX_OUT,NY_OUT
	INTEGER*4 PNTR_IN,PNTR_OUT,MADRID(1)
	CHARACTER IMAGE_IN*40,IMAGE_OUT*40,NAME*40,COMMENTS*80
        CHARACTER BUFFER*80
        COMMON/VMR/MADRID
 
10	FORMAT(A)
 
	CALL JLP_BEGIN
 
* Inquire the format of the images :
	CALL JLP_INQUIFMT
        WRITE(6,11)
11      FORMAT('Resample1, version 17/12/98',/,
     1         ' new_coordinates= matrix * old_coordinates: '/,
     1         ' image(x_new, y_new) = image(C1 + C2 x_old + C3 y_old,',/,
     1                                     ' C4 + C5 x_old + C6 y_old)')

	  DO K=1,6
           COEF1(K)=0.
          END DO
          COEF1(2)=1.
          COEF1(6)=1.
*
* Obtain input image
*
	PRINT *,' Input image ?'
	READ(5,10) IMAGE_IN
	CALL JLP_VM_READIMAG(PNTR_IN,NX_IN,NY_IN,IMAGE_IN,COMMENTS)
 
* Obtain position transformation coefficients
*
16        WRITE(6,12)
12      FORMAT(' Menu: ',/,
     1         ' 1,x   : Rotation of xx degrees (0=horizontal, 90=vertical)',/,
     1         ' 2     : Symmetry left-right in X (look at MIRROR.FOR)',/,
     1         ' 3     : Symmetry top-bottom in Y (look at MIRROR.FOR)',/,
     1         ' 4,x,y : Shift of x,y',/,
     1         ' 5,filename     : File with 6 coefficients',/,
     1         ' Enter your choice: ',$)
        READ(5,10) BUFFER
        IF(BUFFER(1:1).EQ.'1')THEN
          READ(BUFFER(3:80),*) ANGLE
          ANGLE = ANGLE*3.14159/180.
          COEF1(2)=COS(ANGLE)
          COEF1(3)=SIN(ANGLE)
          COEF1(5)=-SIN(ANGLE)
          COEF1(6)=COS(ANGLE)
C Keep center in the middle:
          WX = COEF1(2)*NX_IN/2. + COEF1(3)*NY_IN/2.
          WY = COEF1(5)*NX_IN/2. + COEF1(6)*NY_IN/2.
          COEF1(1)=NX_IN/2.-WX
          COEF1(4)=NY_IN/2.-WY
        ELSE IF(BUFFER(1:1).EQ.'2')THEN
          COEF1(1)=NX_IN
          COEF1(2)=-1.
        ELSE IF(BUFFER(1:1).EQ.'3')THEN
          COEF1(4)=NY_IN
          COEF1(6)=-1.
        ELSE IF(BUFFER(1:1).EQ.'4')THEN
          READ(BUFFER(3:80),*)WX,WY
          COEF1(1)=-WX
          COEF1(4)=-WY
        ELSE IF(BUFFER(1:1).EQ.'5')THEN
	  OPEN(1,FILE=NAME,STATUS='OLD',ERR=16)
	  READ(1,*)(COEF1(K),K=1,6)
	  CLOSE(1)
        ELSE
          GOTO 16
        ENDIF
C Output just to check:
	WRITE(6,35)(COEF1(K),K=1,6)
35      FORMAT(' OK: coefficients are:',/,2(2X,3(1X,G10.3),/))
 
*
* Obtain type of interpolation required (2=linear)
* and determine if total data count is to be conserved
*
         WRITE(6,32)
32       FORMAT(' Interpolation method:   1=nearest   2=linear  3=constant noise ',/,
     1     ' and total count conserved:   0=NO      1=YES    (Example: 2,0) ?')
	 READ(5,*) METHOD,ICONSERV
         WRITE(6,*) ' METHOD :=',METHOD,' (2=LINEAR)'
*
* Obtain size of output image, using input size as default
*
	 NX_OUT=NX_IN
	 NY_OUT=NY_IN
	 WRITE(6,*) ' Size of the output image',NX_OUT,NY_OUT
 
* Obtain output image
*
	WRITE(6,*) ' Output image ?'
	READ(5,10) IMAGE_OUT

* Get memory space for output image:
       ISIZE=NX_OUT*NY_OUT*4
       CALL JLP_GETVM(PNTR_OUT,ISIZE) 
*
* Defines the parameters used by EDRSLIB (integer files)
	INVALA=-100000
	INVALB=0.

* By default flux scale is set to unity
	SCALE=1.0
*
* Call REBIN to resample the input image over the entire area
* covered by the output image
*
	CALL REBIN(MADRID(PNTR_IN),NX_IN,NY_IN,INVALA,INVALB,1,NX_OUT,1,
     1	NY_OUT,COEF1,SCALE,METHOD,MADRID(PNTR_OUT),NX_OUT,NY_OUT,IERR)
 
* If conservation of total flux, rescale the intensities:
        IF(ICONSERV.EQ.1.AND.IERR.EQ.0)THEN
          WRITE(6,*) ' Rescaling the image intensity'
          NPIX_TOTAL=NX_OUT*NY_OUT
          CALL CMULT_IMAGE(MADRID(PNTR_OUT),MADRID(PNTR_OUT),SCALE,NPIX_TOTAL)
        ENDIF

        IF(IERR.NE.0)THEN
          WRITE(6,33) IERR
33        FORMAT(' Fatal error in REBIN, error flag=',I4)
        ELSE
* Write the output image :
	  IMAX=MAX(INDEX(IMAGE_IN,'  '),1)
	  IMAX=MIN(10,IMAX)
	  WRITE(COMMENTS,25)IMAGE_IN(1:IMAX),(COEF1(IK),IK=1,6)
25	  FORMAT(A,'resampled ',6(G9.2))
	  CALL JLP_WRITEIMAG(MADRID(PNTR_OUT),NX_OUT,NY_OUT,NX_OUT,
     1                    IMAGE_OUT,COMMENTS)
        ENDIF
 
	CALL JLP_END
	STOP
	END
C@**********************************************************************
C REBIN
C@**********************************************************************
	subroutine rebin(input1,npixa,nlinea,invala,invalb,minx,maxx,miny,
     1	maxy,coef1,scale,mode,output1,npixb,nlineb,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To resample an image at positions given by a linear
*       transformation of the output pixel positions
*
*METHOD
*       Scan the output image, transforming the pixel positions.
*       Interpolate in the input image to determine the image value
*       at these points. The routine uses nearest-neighbour or linear
*       interpolation between the 4 nearest pixels
*
*ARGUMENTS
*       INPUT1 (IN)
*       REAL*4(NPIXA,NLINEA)
*               The input image
*       NPIXA,NLINEA (IN)
*       INTEGER
*               The dimensions of input
*       INVALA (IN)
*       INTEGER
*               Invalid pixel flag for input
*       INVALB (IN)
*       INTEGER
*               Invalid pixel flag for output1
*       MINX,MAXX,MINY,MAXY (IN)
*       INTEGER
*               Range of output pixels to be replaced
*       COEF1(IN)
*       REAL(6)
*               Coefficients giving the transformation from output
*               positions to input positions
*       SCALE (OUT)
*       REAL
*               Scale factor which is needed if total image intensity
*               is to be conserved
*       MODE (IN)
*       INTEGER
*               Type of interpolation
*               1: Nearest neighbour
*               2: Linear
*               3: Constant noise
*       OUTPUT1 (IN/OUT)
*       REAL*4(NPIXB,NLINEB)
*               Output image
*       NPIXB,NLINEB (IN)
*       INTEGER
*               Dimensions of output1
*       IERR (OUT)
*       INTEGER
*               Error flag: zero for success
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
	real*4 input1(npixa,*),output1(npixb,*)
	integer xmin,xmax,ymin,ymax,x,y,xcen,ycen
	real coef1(6),wx(-1:+1),wy(-1:+1)
 
* Set up weight arrays for constant noise interpolation
*
	data wx(0)/1.0/,wy(0)/1.0/
 
* Check argument validity
*
	if(minx.gt.maxx) then
	   ierr=1
	else if(miny.gt.maxy) then
	   ierr=2
	else
	   ierr=0
*
* Restrict max and min x,y limits to lie in output image
*
	   xmin=min(max(1,minx),npixb)
	   xmax=min(max(1,maxx),npixb)
	   ymin=min(max(1,miny),nlineb)
	   ymax=min(max(1,maxy),nlineb)
 
*
* Restrict interpolation method to be 1 to 3
*
	   method=min(max(1,mode),3)
 
* Scale factor to conserve counts is (abs. value of determinant)
*
	   scale=abs(coef1(2)*coef1(6)-coef1(3)*coef1(5))
 
* debug:
           write(6,34) xmin,xmax,ymin,ymax,scale
34         format(' xmin, xmax, ymin, ymax :',4I5,' scale factor:',G10.3)
           write(6,35) (coef1(i),i=1,6) 
35         format(2(2X,3(1X,G10.3),/))
           if(scale.eq.0.) then
              ierr=-3
              write(6,36)
36            format('REBIN/Fatal error, wrong transformation, null determinant!')
              return
           endif

* Scan the selected area of the output image (location x,y) and
* calculate the transformed position (xdash,ydash) in the input
* image
*
	   do 12 y=ymin,ymax
	      xref=coef1(1)+coef1(3)*y
	      yref=coef1(4)+coef1(6)*y
 
	      do 11 x=xmin,xmax
	 	  xdash=xref+coef1(2)*x
	 	  ydash=yref+coef1(5)*x
 
*
* Find nearest pixel location
*
	         xcen=nint(xdash)
	         ycen=nint(ydash)
 
* If nearest pixel lies outside input image, output pixel is invalid
* otherwise continue with interpolation
*
 
	         if((xcen.lt.1).or.(xcen.gt.npixa).or.(ycen.lt.1).or.
     1	            (ycen.gt.nlinea)) then
	            output1(x,y)=invalb
	         else
 
* ---------------------------------------------------------------------
* 1. FOR NEAREST-NEIGHBOUR INTERPOLATION, output pixel=nearest pixel,
* or is invalid if input pixel is invalid
* ---------------------------------------------------------------------
 
	            if(method.eq.1) then
 
	               if(input1(xcen,ycen).eq.invala) then
	                  output1(x,y)=invalb
 
	               else
 	                 output1(x,y)=input1(xcen,ycen)
	               endif
 
 
* ---------------------------------------------------------------------
* 2. FOR LINEAR INTERPOLATION, output pixel is invalid if nearest input
* pixel is invalid. Otherwise continue with interpolation
* ---------------------------------------------------------------------
 
	            else if(method.eq.2) then
 
	               if(input1(xcen,ycen).eq.invala) then
 	                 output1(x,y)=invalb
 
	               else
 
*
* Find shift from next lowest pixel,line location
*
	                  i=xdash
	                  j=ydash
	                  dx=xdash-i
	                  dy=ydash-j
 
*
* Initiallise sums for forming weighted mean
*
	                  sum=0.0
	                  wtsum=0.0
 
*
* Form weighted mean of adjacent 4 pixels, checking that each lies
* within the input image and is not invalid
*
 
	                  if(j.ge.1) then
 
	                     if(i.ge.1) then
 
	                        if(input1(i,j).ne.invala) then
 
*
* Weight is calculated from the X,Y shift from integer pixel locations
*
	                           wt=(1.0-dx)*(1.0-dy)
	                           sum=sum+input1(i,j)*wt
	                           wtsum=wtsum+wt
	                        endif
 
	                     endif
 
 
	                     if(i+1.le.npixa) then
 
	                        if(input1(i+1,j).ne.invala) then
	                           wt=dx*(1.0-dy)
	                           sum=sum+input1(i+1,j)*wt
	                           wtsum=wtsum+wt
	                        endif
 
	                     endif
 
	                  endif
 
 
	                  if(j+1.le.nlinea) then
 
	                     if(i.ge.1) then
 
	                        if(input1(i,j+1).ne.invala) then
	                           wt=(1.0-dx)*dy
	                           sum=sum+input1(i,j+1)*wt
	                           wtsum=wtsum+wt
	                        endif
 
	                     endif
 
 
	                     if(i+1.le.npixa) then
 
                              if(input1(i+1,j+1).ne.invala) then
                                 wt=dx*dy
                                 sum=sum+input1(i+1,j+1)*wt
                                 wtsum=wtsum+wt
                              endif
 
                           endif
 
                        endif
 
*
* Assign weighted mean to output pixel (WTSUM cannot be zero, since
* at least 1 input pixel must be valid)
*
                        output1(x,y)=sum/wtsum
                     endif
 
* ---------------------------------------------------------------------
* 3. FOR CONSTANT NOISE INTERPOLATION (output noise independent of
* resampling phase)
* ---------------------------------------------------------------------
 
                  else if(method.eq.3) then
 
*
* If nearest pixel is invalid, so is output pixel. otherwise continue
* with interpolation
*
 
                     if(input1(xcen,ycen).eq.invala) then
                        output1(x,y)=invalb
 
                     else
 
*
* Calculate the shift from the nearest pixel,line position
*
                        dx=xdash-xcen
                        dy=ydash-ycen
 
*
* Calculate the X and Y weight arrays (dependent on the phase DX,DY)
*
                        r1=dx*dx+0.25
                        r2=dy*dy+0.25
                        wx(-1)=r1-dx
                        wx(1)=r1+dx
                        wy(-1)=r2-dy
                        wy(1)=r2+dy
 
*
* Now scan the 9 nearest pixels, forming a weighted sum of all the
* valid ones
*
                        sum=0.0
                        wtsum=0.0
 
                        do 22 jshift=-1,1
                           jj=ycen+jshift
 
*
* Check we are still in the image
*
 
                           if(jj.ge.1.and.jj.le.nlinea) then
 
                              do 21 ishift=-1,1
                                 ii=xcen+ishift
 
                                 if(ii.ge.1.and.ii.le.npixa) then
 
*
* Include the pixel if it is valid
*
 
                                    if(input1(ii,jj).ne.invala) then
                                       wt=wx(ishift)*wy(jshift)
                                       sum=sum+input1(ii,jj)*wt
                                       wtsum=wtsum+wt
                                    endif
 
                                 endif
 
21                            continue
 
                           endif
 
22                      continue
 
 
*
* Assign the interpolated value to the output pixel
*
                        output1(x,y)=sum/wtsum
                     endif
 
                  endif
 
               endif
 
11	    continue
 
12	 continue
 
	endif
 
	return
 
	end
C******************************************************************
C Multiplication of the whole image by a constant:
        SUBROUTINE CMULT_IMAGE(OUT,IN,SCALE,NPIX_TOTAL)
        REAL*4 OUT(*), IN(*)
        REAL*4 SCALE
        DO I=1,NPIX_TOTAL
          OUT(I)=IN(I)*SCALE
        END DO
        RETURN
        END
