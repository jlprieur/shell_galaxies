C++**********************************************************************
C Program STARFIT1
C Determines the parameters of a model star profile by fitting
C a gaussian profile to star images (FWHM~2.35484*sigma)
C It also displays the fit.
C
C position_list: list of x,y,diameter (as for input in "PATCH1")
C
C Syntax:
C     STARFIT1 image position_list box_size graphic_device
C              [Y/N: same scale in x and y?] xmin,xmax,ymin,ymax
C Or (if only one star is there): 
C     STARFIT1 image ix,iy box_size graphic_device
C              [Y/N: same scale in x and y?] xmin,xmax,ymin,ymax
C
C Example:
C     STARFIT1 test test.pat 17 %apple2 N OK 
C
C Nota1: position_list is a list of x,y,diameter (as for input in "PATCH1")
C Nota2: size of the box (in pixels) around the stars for the fit
C Nota3: by entering blanks for xmin,xmax,ymin,ymax, the auto_scale mode is
C        selected for the graph.
C
C Creates an output list with the objects that have been fitted and
C initial diameters (compatible with PATCH1): file is "starfit1.dat"
C JLP93: Possibility of direct input of coordinates if only one object
C is to be fitted.
C
C JLP
C Version of 10/07/96
C--**********************************************************************
C Uses EDRS library: CLNSTA, ELLIPS, GAUFIT, MODE, PRFFIT, RADPRF, WMODE
C Included here: SPARAM, STARIM,RADPRF
C
	PROGRAM STARFIT1
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* Obtain input image containing stars. obtain list of star
*       positions.
*       obtain parameters controlling the fitting, then call
*       sparam to perform the fitting and return the profile parameters
*       write the results to the environment.
*
*STARLINK PARAMETERS
*       INPUT
*               THE INPUT IMAGE
*       ISIZE
*               LENGTH OF THE SEARCH SQUARE SIDE TO BE USED IN LOCATING
*               THE STARS AND FINDING THEIR ELLIPTICITY
*       RANGE
*               RADIUS IN UNITS OF THE STAR 'SIGMA' OUT TO WHICH THE
*               RADIAL STAR PROFILE IS TO BE FITTED
*       SEEING
*               OUTPUT PARAMETER: THE FWHM ACROSS THE STARS' MINOR AXIS
*       AXISR
*               OUTPUT PARAMETER: THE AXIS RATIO OF THE STAR IMAGES
*       THETA
*               OUTPUT PARAMETER: THE INCLINATION OF THE MAJOR AXIS TO
*               THE X AXIS IN DEGREES (X THROUGH Y POSITIVE)
*       GAMMA
*               OUTPUT PARAMETER: THE EXPONENT IN THE RADIAL PROFILE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*----------------------------------------------------------------------
	PARAMETER (IDIM1=1000)
	REAL*4 XA(IDIM1),YA(IDIM1),SIG(IDIM1,5)
	REAL*4 XA1(IDIM1),YA1(IDIM1)
        real*4 toll,tolfit,sky_level
        LOGICAL AUTO_SKY,NO_DISPLAY
C Old Nag: INTEGER*4 IWORK1(IDIM1),IWORK2(IDIM1)
	INTEGER*4 INDEX1(IDIM1),IDIAM1(IDIM1)
	INTEGER*4 PNTR_INPUT,MADRID(1)
	CHARACTER FILENAME*40,LISTIN*40,LISTOUT*40,COMMENTS*80
        character ans*1,plotdev*32
	COMMON /VMR/MADRID 
 
10	FORMAT(A)
	CALL JLP_BEGIN
	OPEN(3,FILE='starfit1.log',STATUS='unknown',ERR=998)
 
	PRINT 79
	WRITE(3,79)
79	FORMAT(' Program STARFIT1  Version of 10/07/96',/,
     1	' Fits the following function to star profiles:',/,
     1	'    Y = Yo * exp(-0.5*((radius/sigma)**gamma))',/,/,
     1	' Then:  FWHM=2.0*sigma*(1.38629**(1.0/gamma))',/,
     1	' (i.e. FWHM=2.35*sigma, when gaussian with gamma=2.0)')
 
* OBTAIN INPUT IMAGE FRAME
*
	CALL JLP_INQUIFMT
        WRITE(6,*) 'Input file: '
        READ(5,10) FILENAME
	CALL JLP_VM_READIMAG(PNTR_INPUT,NPIX,NLINES,
     1	FILENAME,COMMENTS)
	WRITE(3,109)FILENAME
109	FORMAT(' INPUT IMAGE :',A)
*
* OBTAIN LIST OF INITIAL STAR POSITIONS
*
 
17	PRINT *,' Name of the star position list (same as for "PATCH")'
        PRINT *,' (Enter x,y  if only one star has to be fitted)' 
	READ(5,10) LISTIN
        I=INDEX(LISTIN,',')
        PRINT *,'Index=',I
        IF(I.LT.2)THEN
	  OPEN(1,FILE=LISTIN,STATUS='OLD',ERR=17)
	    DO I=1,IDIM1
	      READ(1,*,END=18)XA1(I),YA1(I),IDIAM1(I)
	    END DO
18	  CLOSE(1)
	  NPTS1=I-1
	  PRINT *,NPTS1,' OBJECTS RECEIVED'
        ELSE
          READ(LISTIN,*)XA1(1),YA1(1)
          IDIAM1(1)=10.
          NPTS1=1
        ENDIF
 
C	PRINT *,' Maximum number of objects you want to fit'
C	READ(5,*) NSTAR
	NSTAR=30
	NSTAR=MIN(NPTS1,NSTAR)
 
C37	PRINT *,' Name of output list with the selected stars'
C	READ(5,10) LISTOUT
	LISTOUT='starfit1.dat'
	OPEN(2,FILE=LISTOUT,STATUS='unknown')
 
* Obtain search area size (isize) and the range of radii to be
* used in the profile fit (range,in units of sigma)
*
*	ISIZE=19
* JLP 93
*       tolfit=0.0005, toll=0.001
300	write(6,21)
21      format(' Window width for the fit of each star (pixels)',/,
     1         ' and fit tolerance for gaussian and profile (50.,0.5,0.1 ?):')
	READ(5,*) isize,toll,tolfit
	RANGE=4.0
 
	PRINT *,' Automatic sky determination and defaults removal? (Y)' 
	READ(5,10) ANS 
          AUTO_SKY=(ANS.NE.'n'.AND.ANS.NE.'N')
        IF(.NOT.AUTO_SKY)THEN
	  PRINT *,' Sky level:'
          READ(5,*) SKY_LEVEL 
        ENDIF

C Plot mean profile if more than one star, otherwise not very good...
        NO_DISPLAY=.FALSE.
        IF(NSTAR.GT.1.OR.(NSTAR.EQ.1.AND..NOT.NO_DISPLAY))THEN
	   PRINT *,' Graphic device ($FILE)?'
	   READ(5,10) PLOTDEV
	   IF(PLOTDEV(1:5).EQ.'     ') PLOTDEV='$FILE'
        ENDIF
 
C Write in "starfit1.log"
	WRITE(3,110)LISTIN,NPTS1,NSTAR,LISTOUT,ISIZE
110	FORMAT(' INPUT LIST :',A,/,' ',I5,' OBJECTS RECEIVED',/,
     1	' ',I5,' OBJECTS TO FIT',/,
     1	' OUTPUT LIST :',A,/,' Window size:',I5)
 
C Original index:
	   DO I=1,NPTS1
	     INDEX1(I)=I
	   END DO

C Sorting in descending order, according to the patch size:
        IF(NPTS1.GT.1)THEN
	   CALL SORT_INT4(IDIAM1,INDEX1,NPTS1,'D')
        ENDIF
 
 
C Sorting the arrays XA and YA :
	 DO I=1,NSTAR
	   XA(I)=XA1(INDEX1(I))
	   YA(I)=YA1(INDEX1(I))
	 END DO
 
* CALL SPARAM TO FIND THE MEAN STAR PROFILE PARAMETERS
*
	call sparam(madrid(pntr_input),npix,nlines,npix,isize,range,
     1	xa,ya,idiam1,nstar,axisr,theta,fwhm,gamma,
     1	sig,filename,plotdev,ierr,toll,tolfit,auto_sky,sky_level,no_display)
 
* If ierr is not zero, no fit has been obtained... give error message
*
	IF(IERR.EQ.0)THEN
	  PRINT *,' Starfit1/Successful fit'
	ELSEIF(IERR.EQ.1)THEN
	  PRINT *,' Starfit1/Failure: no stars have been found in this image'
          PRINT *,' Try with higher tolerance values (1.,1. for instance...)'
	ELSEIF(IERR.EQ.2)THEN
	  write(6,23)
23        format(' Starfit1/Failure: Gaussian fit failed on the mean profile',
     1           /,' (computed with all the individually fitted stars)')
	ENDIF

* JLP 93
        write(6,22)
22      format(' Do you want another try with other parameter values? (N)')
        read(5,10)ans
        if(ans.eq.'Y'.or.ans.eq.'y')goto 300

99	CALL JLP_END
	CLOSE(2)
	CLOSE(3)
	WRITE(6,*)' Logfile in: "starfit1.log"'
	STOP
998	WRITE(6,*)' Fatal error opening "starfit1.log"'
	CALL JLP_END
	STOP
	END
C**********************************************************************
C SPARAM
C**********************************************************************
	subroutine sparam(image,npix,nlines,idim,isize,range,x,y,
     1	idiam,nstar,axisr,theta,fwhm,gamma,sig,filename,plotdev,
     1  ierr,toll,tolfit,auto_sky,sky_level,no_display)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND A SET OF PARAMETERS DESCRIBING A MODEL STAR IMAGE
*       FITTED TO A SET OF STAR IMAGES AND TO DISPLAY THE RESULTS.
*
*METHOD
*       CALL STARIM TO DETERMINE THE ELLIPTICITY OF THE STAR IMAGES
*       PRINT HEADINGS AND A TABLE OF THE ELLIPSE PARAMETERS FOR
*       EACH STAR USED. PRINT THE MEAN ELLIPSE PARAMETERS.
*       CALL RADPRF TO DETERMINE THE MODEL RADIAL STAR PROFILE AND TO
*       PRINT THE RESULTS.
*
*ARGUMENTS
*       IMAGE(NPIX,NLINES) (input)
*       REAL*4(IDIM,*)
*               THE INPUT IMAGE CONTAINING THE STARS TO BE FITTED
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF IMAGE
*       ISIZE (IN)
*       INTEGER
*               LENGTH OF THE SEARCH SQUARE SIDE USED IN FINDING STARS
*               AND CALCULATING THEIR ELLIPTICITY
*       RANGE (IN)
*       REAL
*               THE RADIUS IN UNITS OF THE STAR 'SIGMA' OUT TO WHICH
*               THE RADIAL PROFILE IS FITTED
*       X,Y (IN)
*       REAL(NSTAR)
*               THE APPROXIMATE POSITIONS OF THE STARS TO BE FITTED
*       NSTAR (IN)
*       INTEGER
*               THE NUMBER OF STARS TO BE USED
*       AXISR (OUT)
*       REAL
*               THE AXIS RATIO OF THE STAR IMAGES
*       THETA (OUT)
*       REAL
*               THE INCLINATION OF THE MAJOR AXIS OF THE STAR IMAGES TO
*               THE X DIRECTION IN RADIANS (X THROUGH Y POSITIVE)
*       FWHM (OUT)
*       REAL
*               THE FULL WIDTH AT HALF MAXIMUM OF THE STAR IMAGES IN
*               THE MINOR AXIS DIRECTION
*       GAMMA (OUT)
*       REAL
*               THE EXPONENT IN THE RADIAL STAR PROFILE
*       SIG (WORKSPACE)
*       REAL(NSTAR,5)
*               INTERMEDIATE STORAGE FOR THE WIDTHS OF THE STAR MARGINAL
*               PROFILES
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*     STARIM,ELLIPS
*
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
	PARAMETER (IDIM1=1000)
	real*4 image(idim,*)
	real*4 x(nstar),y(nstar),sig(idim1,5),prf(4),wt(idim1)
        real*4 toll,tolfit,sky_level
        logical auto_sky,no_display
	integer*4 idiam(nstar)
	character filename*(*),plotdev*(*)
 
*
* CALL STARIM TO DETERMINE THE MEAN ELLIPTICITY OF THE STAR IMAGES
*
	ierr=0
	call starim(image,npix,nlines,idim,x,y,nstar,
     1	isize,sig0,axisr,theta,ngood,sig,filename,toll,auto_sky,sky_level)
 
        print *,' ',ngood,' star(s) successively fitted'

*
* If no stars could be found to determine the ellipticity, set ierr=1
* and abort
*
 
        if(ngood.le.0) then
         ierr=1
         go to 99
        endif
 
c Transfer sig(*,5) to wt:
	do i=1,nstar
	  wt(i)=sig(i,5)
	end do
 
* Print headings
*
       write(3,11)
11     format(4x,' X COORD.',5x,' Y COORD.',4x,
     : 'FWHM SEEING',5x,'AXIS RATIO',5x,'ANGLE (DEG)')
       write(3,12)
12     format(5x,' --------',5x,' --------',4x,
     : '-----------',5x,'----------',5x,'-----------')
* JLP 93: displays also to screen when the number of star is small:
       if(nstar.le.10) then
         write(6,11)
         write(6,12)
       endif
 
**************************************************************************
*
* CONSIDER EACH STAR
*
**************************************************************************
 
       do 101 i=1,nstar
 
*
* IF THE WIDTHS OF THE MARGINAL PROFILES WERE FOUND, CALL ELLIPS
* TO FIND THE PARAMETERS SPECIFYING THE STAR SHAPE
* Remember : SIG(i,5) is positive when the star has been taken
* into account
*
          if(sig(i,5).gt.1.0e-10) then
             prf(1)=sig(i,1)
             prf(2)=sig(i,2)
             prf(3)=sig(i,3)
             prf(4)=sig(i,4)
             call ellips(prf,stasig,staaxs,stathe)
 
*
* CALCULATE THE FWHM SEEING DISK, ASSUMING A GAUSSIAN PROFILE
* AND PRINT THE STAR PARAMETERS
*
             seeing=stasig*2.35482
             angle=stathe*57.29578
             write(3,13)x(i),y(i),seeing,staaxs,angle
13           format(' ',2(ss,g13.6,1x),1x,ss,g10.3,6x,ss,g10.3,
     1	6x,ss,g10.3)
* JLP 93: displays also to screen when the number of star is small:
             if(nstar.le.10) then
                write(6,13)x(i),y(i),seeing,staaxs,angle
             endif
 
 
C Store the good objects in the output list :
	     WRITE(2,*)X(I),Y(I),IDIAM(I)
 
          else
 
* IF THE MARGINAL PROFILE WIDTHS WERE NOT FOUND, PRINT DETAILS
*
             write(3,14)x(i),y(i)
14           format(' ',2(ss,g13.6,1x),3x,'cannot fit this star')
          endif
 
101     continue
 
**************************************************************************
*
* NOW PRINT THE NUMBER OF STARS FOUND OK
*
**************************************************************************
 
       write(3,15)ngood
15     format(/,3x,i10,' star(s) fitted successfully')
 
*
* PRINT THE MEAN RESULTS (if more than one star, otherwise not very good)
*
       if(ngood.le.1.and.no_display)return
*
       write(3,16)axisr
16     format('   Mean axis ratio=',ss,g11.4)
       write(3,17)theta*57.29578
17     format(/,'   Mean inclination of major axis=',ss,g11.4,
     1	' degrees',/)
 
*
* CALL RADPRF TO DETERMINE THE FORM OF THE RADIAL PROFILE
*
	call radprf(image,npix,nlines,idim,sig0,axisr,theta,range
     1	,x,y,nstar,wt,fwhm,gamma,filename,plotdev,ierrf,tolfit)
 
* IF THE RADIAL PROFILE COULD NOT BE FOUND, SET IERR=2 AND ABORT
 
        if(ierrf.ne.0) ierr=2
99    return
 
      end
 
C@***********************************************************************
C  Subroutine STARIM
C@***********************************************************************
      subroutine starim(image,npix,nlines,idim,x0,y0,nstar,isize,sig0,
     1	axisr,theta,ngood,sig,filename,toll,auto_sky,sky_level)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE MEAN AXIS RATIO, SEEING DISK SIZE AND INCLINATION
*       OF A SET OF ELLIPTICAL STAR IMAGES.
*
*METHOD
*       FOR EACH STAR, FORM MARGINAL PROFILES IN X AND Y USING DATA
*       IN A SQUARE SEARCH AREA. ALSO FORM MARGINAL PROFILES IN
*       DIRECTIONS P AND Q, INCLINED AT 45 DEGREES TO X AND Y, USING
*       DATA IN A SEARCH SQUARE INCLINED AT 45 DEGREES. CALL CLNSTA
*       TO REMOVE BACKGROUNDS, NEIGHBOURING STARS, DIRT, ETC. FROM
*       THESE PROFILES, THEN CALL GAUFIT TO FIT A GAUSSIAN TO EACH,
*       TO DETERMINE THE CENTRE, WIDTH AND AMPLITUDE. CALCULATE A MEAN
*       CENTRE FOR EACH STAR. COMBINE THE 4 WIDTH ESTIMATES FROM EACH
*       STAR USING WMODE TO GIVE 4 MEAN WIDTHS, THEN CALL ELLIPS TO
*       CALCULATE THE MEAN STAR IMAGE ELLIPSE PARAMETERS.
*
*ARGUMENTS
*       IMAGE(NPIX,NLINES) (IN)
*       REAL*4(IDIM,*)
*               THE INPUT IMAGE CONTAINING THE STAR IMAGES
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF IMAGE
*       X0,Y0 (IN/OUT)
*       REAL(nstar)
*               INITIAL APPROXIMATE POSITIONS FOR EACH STAR IMAGE
*               IF THE STAR IS FOUND, AN ACCURATE POSITION IS RETURNED
*       nstar (IN)
*       INTEGER
*               THE NUMBER OF STAR IMAGES TO BE USED
*       ISIZE (IN)
*       INTEGER
*               THE LENGTH OF THE SIDE OF THE SEARCH SQUARE
*       SIG0 (OUT)
*       REAL
*               THE MEAN STAR 'SIGMA'
*       AXISR (OUT)
*       REAL
*               THE MEAN STAR AXIS RATIO
*       THETA (OUT)
*       REAL
*               THE MEAN INCLINATION OF THE STAR MAJOR AXES TO THE
*               X DIRECTION IN RADIANS (X THROUGH Y POSITIVE)
*       NGOOD (OUT)
*       INTEGER
*               THE NUMBER OF STARS SUCCESSFULLY FOUND AND USED IN THE
*               FIT
*       SIG (OUT)
*       REAL(nstar,5)
*               SIG(I,1), SIG(I,2), SIG(I,3) AND SIG(I,4) RETURN THE
*               GAUSSIAN WIDTHS OF STAR I IN DIRECTIONS INCLINED AT
*               0, 45, 90 AND 135 DEGREES TO THE X AXIS (X THROUGH Y
*               POSITIVE). SIG(I,5) RETURNS THE SUM OF THE AMPLITUDES
*               OF THE 4 PROFILES OF STAR I (I.E. IT IS PROPORTIONAL
*               TO THE AMPLITUDE OF STAR I). IF A STAR WAS NOT FOUND,
*               ALL ITS ENTRIES IN SIG ARE ZERO.
*
*CALLS
*               CLNSTA,GAUFIT,WMODE,ELLIPS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
*
* SET MAXIMUM RADIUS FROM CENTRE FOR FORMING THE MARGINAL PROFILES
* AND SET MAXP TO SQRT(2.0) TIMES THIS TO ALLOW BINNING IN THE 45
* DEGREE DIRECTION ALSO
*
      parameter (maxx=50,maxp=1.41421*maxx+1.0,maxsiz=2*maxx+1)
 
*
* DIMENSION ARRAYS
*
	PARAMETER (IDIM1=1000)
        real*4 image(idim,*)
        real*4 toll,sky_level
        logical auto_sky
	integer nx(-maxx:maxx),ny(-maxx:maxx),np(-maxp:maxp)
        integer nq(-maxp:maxp),istar,bini,binj,binp,binq
	real x0(nstar),y0(nstar),sig(idim1,5),xsum(-maxx:maxx)
        real ysum(-maxx:maxx),psum(-maxp:maxp),qsum(-maxp:maxp),sigma(4)
	parameter (niter=3,itmode=10,ngauit=15)
	character filename*(*)
 
*
* DETERMINE THE SIZE OF THE SEARCH AREA AS ODD AND NOT EXCEEDING
* MAXSIZ, THEN DETERMINE THE CORRESPONDING NUMBER OF BINS IN THE
* 45 DEGREE DIRECTIONS
*
       ix=min(isize,maxsiz)
       idx=max(1,ix/2)
       idp=nint(1.41421*idx)
 
*
* CONSIDER EACH STAR POSITION IN TURN, COUNTING SUCCESSES IN NGOOD
*
      ngood=0
      do 800 istar=1,nstar
         x=x0(istar)
         y=y0(istar)
 
*
* PERFORM NITER ITERATIONS, EACH TIME CENTERING THE SEARCH AREA ON
* THE PREVIOUS ESTIMATE OF THE STAR CENTRE
*
         do 87 iter=1,niter
            i0=nint(min(max(-1.0e8,x),1.0e8))
            j0=nint(min(max(-1.0e8,y),1.0e8))
 
*
* INITIALLISE ARRAYS FOR FORMING THE MARGINAL PROFILES IN THE
* X AND Y DIRECTIONS AND AT 45 DEGREES (P AND Q)
*
            do 41 i=-idx,idx
               xsum(i)=0.0
               ysum(i)=0.0
               nx(i)=0
               ny(i)=0
41          continue
 
            do 42 i=-idp,idp
               psum(i)=0.0
               qsum(i)=0.0
               np(i)=0
               nq(i)=0
42          continue
 
*
* NOW FORM THE MARGINAL PROFILES, SCANNING A LARGE ENOUGH IMAGE
* AREA TO ACCOMMODATE THE SEARCH SQUARE TURNED THROUGH 45 DEGREES
 
            do 81 binj=-idp,idp
               j=j0+binj
 
* CHECK THAT WE ARE STILL INSIDE THE IMAGE
               if(j.ge.1.and.j.le.nlines) then
 
                  do 80 bini=-idp,idp
                     i=i0+bini
 
                     if(i.ge.1.and.i.le.npix) then
* FIND THE P,Q COORDINATES
* x+y=cte for p, i.e. straight lines at 45 degrees downwards
                       binp=bini+binj
* x-y=cte for p, i.e. straight lines at 45 degrees upwards
                       binq=binj-bini
 
* IF THE PIXEL LIES IN THE NORMAL SEARCH SQUARE, ADD IT TO THE
* X AND Y MARGINALS
 
                    if(abs(bini).le.idx.and.abs(binj).le.idx)then
                       xsum(bini)=xsum(bini)+image(i,j)
                       ysum(binj)=ysum(binj)+image(i,j)
                       nx(bini)=nx(bini)+1
                       ny(binj)=ny(binj)+1
                    endif
 
*
* IF THE PIXEL LIES IN THE 45 DEGREE SQUARE, ADD IT TO THE P AND Q
* MARGINALS
                    if(abs(binp).le.idp.and.abs(binq).le.idp)then
                        psum(binp)=psum(binp)+image(i,j)
                        qsum(binq)=qsum(binq)+image(i,j)
                        np(binp)=np(binp)+1
                        nq(binq)=nq(binq)+1
                    endif
 
                  endif
 
80              continue
 
             endif
 
81        continue
 
*
* EVALUATE THE X AND Y MARGINALS
*
 
          do 31 i=-idx,idx
 
             if(nx(i).gt.0) then
               xsum(i)=xsum(i)/nx(i)
             else
               xsum(i)=2.0e20
             endif
 
             if(ny(i).gt.0) then
                ysum(i)=ysum(i)/ny(i)
             else
               ysum(i)=2.0e20
             endif
 
31          continue
 
*
* EVALUATE THE P AND Q MARGINALS
*
            do 32 i=-idp,idp
 
               if(np(i).gt.0) then
                  psum(i)=psum(i)/np(i)
               else
                  psum(i)=2.0e20
               endif
 
               if(nq(i).gt.0) then
                  qsum(i)=qsum(i)/nq(i)
               else
                  qsum(i)=2.0e20
               endif
 
32          continue
 
* Debug:
            open(9,file="sum1.dat",status="new")
            do i=-idx,idx
              write(9,*)i,xsum(i),ysum(i),psum(i),qsum(i)
            enddo
            close(9)
*
* CALL CLNSTA TO REMOVE THE BACKGROUND AND NEIGHBOURING STARS, DIRT
* ETC. FROM EACH PROFILE
* (xcenter=0.0)
*
              italk=1
              call clnsta(xsum(-idx),-idx,idx,0.0,
     1                    auto_sky,sky_level,italk)
              call clnsta(ysum(-idx),-idx,idx,0.0,
     1                    auto_sky,sky_level,italk)
              call clnsta(psum(-idp),-idp,idp,0.0,
     1                    auto_sky,sky_level,italk)
              call clnsta(qsum(-idp),-idp,idp,0.0,
     1                    auto_sky,sky_level,italk)
 
* Debug:
            open(9,file="sum2.dat",status="new")
            do i=-idx,idx
              write(9,*)i,xsum(i),ysum(i),psum(i),qsum(i)
            enddo
            close(9)
*
* CALL GAUFIT TO FIT A GAUSSIAN TO EACH PROFILE
*
            call gaufit(xsum(-idx),-idx,idx,ngauit,toll,ax,xcen,sigx
     :       ,back,ierrx)
            call gaufit(ysum(-idx),-idx,idx,ngauit,toll,ay,ycen,sigy
     :       ,back,ierry)
            call gaufit(psum(-idp),-idp,idp,ngauit,toll,ap,pcen,sigp
     :       ,back,ierrp)
            call gaufit(qsum(-idp),-idp,idp,ngauit,toll,aq,qcen,sigq
     :       ,back,ierrq)
*
* Correct for different bin spacing in the p,q directions
*
            sigp=0.7071068*sigp
            sigq=0.7071068*sigq
*
* IF NO FATAL ERRORS WERE ENCOUNTERED, CALCULATE A MEAN CENTRE
* POSITION FROM THE CENTRES OF EACH PROFILE. OTHERWISE EXIT
* FROM THE ITERATION LOOP WHICH FINDS THE CENTRE
            ierrft=max(ierrx,ierry,ierrp,ierrq)
 
            if(ierrft.eq.0) then
               xnew=i0+(xcen+0.5*(pcen-qcen))*0.5
               ynew=j0+(ycen+0.5*(pcen+qcen))*0.5
 
* CALCULATE SHIFT OF CENTRE THIS ITERATION... IF IT SATISFIES THE
* ACCURACY CRITERION, EXIT FROM THE CENTRE-FINDING ITERATION LOOP
*
               shift=sqrt((x-xnew)**2+(y-ynew)**2)
               x=xnew
               y=ynew
 
* JLP93: when toll is big, exit before niter=3 iterations,...
*               if(shift.le.toll) go to 601
 
            else
* JLP 93: displays also to screen when the number of star is small:
               if(nstar.le.10) then
                 write(6,28) istar,ierrx,ierry,ierrp,ierrq
28               format(' starim/Error when fitting individual profile #',
     1                I5,/,' ierrx, ierry, ierrp, ierrq =',4(1X,I2))
                 write(6,29) istar,x,y,sigx,sigy,sigp,sigq,
     1           sigx*2.355,sigy*2.355,sigp*2.355,sigq*2.355 
               endif
               go to 601
            endif
 
87       continue
 
*
* If the centre was found successfully, record the widths of each
* profile and the star centre and form a weight from
* the star amplitude
*
 
601      if(ierrft.eq.0) then
 
* JLP: Here I added a test to discard points when the axis ratio
* is too large (23-09-88):
	    axis1=sigx/sigy
	    axis2=sigy/sigx
	    axis3=sigp/sigq
	    axis3=sigq/sigp
	    axisr_max=amax1(axis1,axis2,axis3,axis4)
            AXIS_RATIO_MAX=1.6
	    if(axisr_max.le.axis_ratio_max)then
               	ngood=ngood+1
	        x0(istar)=x
	        y0(istar)=y
	        sig(istar,1)=sigx
	        sig(istar,2)=sigp
	        sig(istar,3)=sigy
	        sig(istar,4)=sigq
	        sig(istar,5)=ap+aq+ax+ay
	     else
	        sig(istar,1)=0.0
	        sig(istar,2)=0.0
	        sig(istar,3)=0.0
	        sig(istar,4)=0.0
	        sig(istar,5)=0.0
	    endif
 
*
* If the centre was not found successfully, record the weight as zero
*
	 else
	    sig(istar,1)=0.0
	    sig(istar,2)=0.0
	    sig(istar,3)=0.0
	    sig(istar,4)=0.0
	    sig(istar,5)=0.0
         endif
 
* JLP 93: displays also to screen when the number of star is small:
               if(nstar.le.10) then
                 write(6,29) istar,x,y,sigx,sigy,sigp,sigq,
     1           sigx*2.355,sigy*2.355,sigp*2.355,sigq*2.355 
29               format(' starim/Gaussian fit, star #',
     1                 I5,6X,'x_cent, y_cent =',2(1X,F8.2),
     1                /,' sigx, sigy, siqp, sigq =',4(1X,G11.4),
     1                /,' fwhmx, fwhmy, fwhmp, fwhmq =',4(1X,G11.4))
               endif
800   continue
 
*
* If at least one star was successful, call WMODE to find a mean
* width for each profile direction
*
 
      if(ngood.ge.1) then
         do 34 nsig=1,4
            call wmode(sig(1,nsig),sig(1,5),ngood,0.01,itmode,toll,
     :      sigma(nsig),err)
34       continue
*         print *,' sigma x, p, y, q out of wmode:',(sigma(i),i=1,4)
* Call ellips to find the parameters of an elliptical star image
* from the marginal widths
         call ellips(sigma,sig0,axisr,theta)
      endif
 
      return
 
      end
C********************************************************************
C RADPRF
C********************************************************************
      subroutine radprf(image,npix,nlines,idim,sig0,axisr,theta
     : ,range,x,y,nstar,wt,fwhm,gamma,filename,plotdev,ierr,tolfit)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIT A RADIAL PROFILE TO A SET OF STAR IMAGES
*
*METHOD
*       FOR EACH STAR, PUT THE DATA INTO BINS REPRESENTING ISOPHOTAL
*       ZONES , ALLOWING FOR IMAGE ELLIPTICITY. FORM A LINKED LIST
*       OF ALL THE PIXELS IN EACH ZONE, THEN PROCESS THE CONTENTS OF
*       EACH ZONE USING MODE TO REJECT ERRONEOUS DATA. FIT A GAUSSIAN
*       TO EACH BINNED STAR AND NORMALISE THE DATA TO UNIT CENTRAL
*       AMPLITUDE. PUT THE NORMALISED DATA INTO A SET OF BINS FILLED
*       WITH DATA FOR ALL STARS. CALL PRFFIT TO FIT A RADIAL PROFILE TO
*       THIS COMBINED DATA. PRINT THE RESULTS.
*       IF REQUIRED, PLOT THE MEAN PROFILE AND THE FITTED FUNCTION
*
*ARGUMENTS
*       IMAGE(NPIX,NLINES) (IN)
*       REAL*4(IDIM,*)
*               THE INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               DIMENSIONS OF IMAGE
*       SIG0 (IN)
*       REAL
*               STAR 'SIGMA' ACROSS THE MINOR AXIS.
*       AXISR (IN)
*       REAL
*               THE STAR AXIS RATIO
*       THETA (IN)
*       REAL
*               THE INCLINATION OF THE STAR MAJOR AXIS TO THE X AXIS
*               IN RADIANS (X THROUGH Y POSITIVE)
*       RANGE (IN)
*       REAL
*               THE NUMBER OF STAR 'SIGMA'S OUT TO WHICH THE RADIAL
*               PROFILE IS FITTED
*       X,Y (IN)
*       REAL(nstar)
*               THE ACCURATE STAR CENTRE POSITIONS
*       nstar (IN)
*       INTEGER
*               THE NUMBER OF STARS
*       WT (IN)
*       REAL(nstar)
*               POSITIVE FOR STARS TO BE USED, ZERO FOR THOSE TO BE
*               OMITTED
*       FWHM (OUT)
*       REAL
*               AN OUTPUT ESTIMATE OF THE FULL WIDTH AT HALF MAXIMUM
*               ACROSS THE MINOR AXIS OF A STAR IMAGE
*       GAMMA (OUT)
*       REAL
*               STAR RADIAL PROFILE PARAMETER
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*               MODE,PRFFIT
*-----------------------------------------------------------------------
*
* SET MAXIMUM RADIUS TO BE CONSIDERED FOR THE RADIAL PROFILE
*
* SET RESOLUTION (NO. OF BINS PER PIXEL SPACING) FOR BINNING THE RADIAL
* PROFILES OF EACH STAR (RESOL1) AND THE MEAN PROFILE (RESOL2)
*
        parameter (maxrad=50,maxpts=(2*maxrad+1)**2,resol1=2.5)
        parameter (resol2=10.0,nbin1=maxrad*resol1+1)
        parameter (nbin2=maxrad*resol2+1)
 
* SET NUMBER OF RADIAL BINS REQUIRED
	parameter (pbad=0.01,niter=10,nitfit=20)
	parameter (nmax=nbin2+1)
*
* DIMENSION ARRAYS...NOTE IMAGE IS TREATED AS A 1 DIMENSIONAL ARRAY
*
        real image(*)
        real x(nstar),y(nstar),wt(nstar),data(maxpts),profil(0:nbin2)
        real profwt(0:nbin2),profr(0:nbin2),r(0:nbin1),dmode(0:nbin1)
        real tolfit
        integer lstart(0:nbin1),nxtadr(maxpts),iadr(maxpts),npts(0:nbin1)
        integer bin,istar,bin2
        double precision sumw,sumf,sumf2,sumdf,sumd
        character filename*(*)
	REAL*4 XPLOT(nmax,2),YPLOT(nmax,2)
	INTEGER*4 NPTSPLOT(2)
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40,NCHAR(2)*4
        CHARACTER PCOLOR(2)*30,PLOTDEV*(*)
 
*
* CALCULATE CONSTANTS FOR RELATING RADIAL DISTANCE IN AN ELLIPTICAL
* STAR IMAGE TO EFFECTIVE DISTANCE ALONG THE MINOR AXIS
*
      ierr=0
      s=sin(theta)
      c=cos(theta)
      raxisr=(1.0/axisr)**2
      consta=s*s+c*c*raxisr
      constb=c*c+s*s*raxisr
      constc=2.0*c*s*(raxisr-1.0)
 
*
* CALCULATE SCALE FACTORS FOR CONVERTING RADIAL DISTANCE TO BINS
*
      rsig=1.0/sig0
      rscale=resol1*2.0/min(2.0,sig0)
      rscl2=resol2*2.0/min(2.0,sig0)
 
*
* FIND THE RADIUS LIMIT IN THE MINOR AXIS DIRECTION IMPOSED BY
* THE FITTING LIMIT 'RANGE'
*
      rlimit=range*sig0
 
*
* FIND THE SIZE OF SQUARE TO BE SCANNED AROUND EACH STAR TO
* ACCOMMODATE THE RADIAL FITTING RANGE
*
      ishift=min(max(1,nint(rlimit*axisr)),maxrad)
 
*
* FIND THE NUMBER OF BINS TO BE USED IN BINNING THE RADIAL PROFILES
* OF EACH STAR AND OF THE MEAN PROFILE
*
      mxbin1=min(nbin1,int(rlimit*rscale))
      mxbin2=min(nbin2,int(rlimit*rscl2))
 
*
* INITIALLISE THE ARRAYS TO HOLD THE MEAN RADIAL PROFILE
*
 
      do 41 bin=0,nbin2
         profr(bin)=0.0
         profil(bin)=0.0
         profwt(bin)=0.0
41    continue
 
*
* CONSIDER EACH STAR WHICH HAS POSITIVE WEIGHT
*
 
      do 991 istar=1,nstar
 
         if(wt(istar).gt.1.0e-10) then
 
*
* FIND THE EDGES OF THE SEARCH SQUARE CENTRED ON THE STAR
*
            i0=nint(min(max(-1.0e8,x(istar)),1.0e8))
            j0=nint(min(max(-1.0e8,y(istar)),1.0e8))
            imin=max(1,i0-ishift)
            imax=min(npix,i0+ishift)
            jmin=max(1,j0-ishift)
            jmax=min(nlines,j0+ishift)
 
*
* INITIALLISE ARRAYS TO POINT TO THE START OF A LINKED LIST OF ALL
* THE PIXELS IN A GIVEN RADIAL BIN
* AND TO FORM MEAN RADII FOR EACH BIN
*
 
            do 37 bin=0,mxbin1
               r(bin)=0.0
               npts(bin)=0
               lstart(bin)=0
37          continue
 
*
* SCAN THE SQUARE AROUND THE STAR, CALCULATING THE X AND Y
* DISPLACEMENTS FROM THE CENTRE
*
            index1=0
            dy=jmin-y(istar)-1.0
            dx0=imin-x(istar)-1.0
 
            do 97 j=jmin,jmax
C@               imlocn=(j-1)*npix+imin-1
               imlocn=(j-1)*idim+imin-1
               dy=dy+1.0
               dy2=dy*dy
               dx=dx0
 
               do 96 i=imin,imax
                  imlocn=imlocn+1
                  dx=dx+1.0
*
* CALCULATE THE EFFECTIVE RADIUS
                  dx2=dx*dx
                  dxy=dx*dy
                  rdash=sqrt(consta*dx2+constb*dy2+constc*dxy)
 
* FIND THE RADIAL BIN
                  bin=rdash*rscale
 
                  if(bin.le.mxbin1.and.rdash.le.rlimit) then
 
* FORM SUMS FOR THE EFFECTIVE MEAN RADIUS
*
                     npts(bin)=npts(bin)+1
                     r(bin)=r(bin)+rdash
 
* FORM A LINKED LIST OF ALL THE PIXELS IN THIS RADIAL BIN
*
                     index1=index1+1
                     iadr(index1)=imlocn
                     nxtadr(index1)=lstart(bin)
                     lstart(bin)=index1
                  endif
96             continue
 
97          continue
 
 
*
* INITIALLISE SUMS FOR FORMING A LEAST SQUARES GAUSSIAN FIT TO THE
* STAR RADIAL PROFILE AND BACKGROUND
*
            sumw=0.0d0
            sumf=0.0d0
            sumf2=0.0d0
            sumdf=0.0d0
            sumd=0.0d0
 
*
* CONSIDER EACH BIN WITH ONE OR MORE POINTS IN IT
*
 
            do 100 bin=0,mxbin1
 
               if(lstart(bin).gt.0) then
 
*
* EXTRACT THE PIXELS IN THIS BIN FROM THE LINKED LIST AND STORE
* IN ARRAY 'DATA'
*
                  icount=0
                  index1=lstart(bin)
 
144               if(index1.gt.0) then
                     icount=icount+1
                     data(icount)=image(iadr(index1))
                     index1=nxtadr(index1)
                     go to 144
 
                  endif
 
*
* CALL MODE TO FIND THE MOST LIKELY VALUE IN THE BIN
*
                  call mode(data,icount,pbad,niter,0.1,dmode(bin),dsig)
 
*
* FORM THE MEAN EFFECTIVE RADIUS FOR THE BIN
*
                  r(bin)=r(bin)/npts(bin)
 
*
* FORM SUMS FOR FITTING A GAUSSIAN PROFILE
* USING THE NUMBER OF DATA POINTS AS A WEIGHT
*
                  funct=exp(-0.5*(r(bin)*rsig)**2)
                  fw=funct*npts(bin)
                  sumw=sumw+npts(bin)
                  sumf=sumf+fw
                  sumf2=sumf2+(funct*fw)
                  sumdf=sumdf+dmode(bin)*fw
                  sumd=sumd+dmode(bin)*npts(bin)
               endif
 
100         continue
 
*
* SOLVE THE NORMAL EQUATIONS FOR THE GAUSSIAN FIT
*
            det=sumf2*sumw-sumf*sumf
 
            if(det.ne.0.0) then
               amp=(sumw*sumdf-sumd*sumf)/det
               back=(sumf2*sumd-sumdf*sumf)/det
 
*
* INSERT THE STAR PROFILE INTO THE MEAN PROFILE BINS
*
 
               do 141 bin=0,mxbin1
 
                  if(lstart(bin).gt.0) then
                     bin2=r(bin)*rscl2
 
                     if(bin2.le.mxbin2) then
 
*
* USE THE NO. OF POINTS AND THE STAR AMPLITUDE AS A WEIGHT
*
                        profil(bin2)=profil(bin2)+(dmode(bin)-back)
     :                   *npts(bin)
                        profr(bin2)=profr(bin2)+r(bin)*amp*npts(bin)
                        profwt(bin2)=profwt(bin2)+amp*npts(bin)
                     endif
 
                  endif
 
141            continue
 
            endif
 
         endif
 
991   continue
 
 
*
* CALCULATE THE MEAN RADIAL PROFILE AND ASSOCIATED RADII FROM THE
* BINNED DATA
*
 
      do 167 bin=0,mxbin2
 
         if(profwt(bin).gt.0.0) then
            profil(bin)=profil(bin)/profwt(bin)
            profr(bin)=profr(bin)/profwt(bin)
         endif
 
167   continue
 
*
* SET INITIAL ESTIMATES OF PROFILE PARAMETERS AND CALL PRFFIT TO
* OBTAIN A FULL LEAST SQUARES FIT TO THE PROFILE
*
      amp=1.0
      back=0.0
      gamma=2.0
      sigma=sig0
      call prffit(profil,profwt,profr,mxbin2+1,nitfit,tolfit,amp,back
     : ,sigma,gamma,ierrf)
 
*
* IF FIT WAS NOT SUCCESSFUL, SET IERR=2
*
 
      if(ierrf.ne.0) then
         ierr=2
*
* OTHERWISE PRINT RESULTS
*
      else
*
* CALCULATE FULL WIDTH HALF MAXIMUM SEEING
*
          fwhm=2.0*sigma*(1.38629**(1.0/gamma))
          write(3,201)sigma,fwhm,gamma,back
201        format(' Values for the averaged profile (with all stars)',/,
     1            '   Sigma seeing=',ss,g11.4,/,
     1            '   Equivalent FWHM =',ss,g11.4,/,
     1            '   Gamma=',ss,g11.4,
     1            '   Estimated background',ss,g11.4)
* JLP 93: displays also to screen:
          write(6,201)sigma,fwhm,gamma,back
 
* Plot results, if graphics is required
* Compress the data arrays to remove empty bins:
*
            ndata=1
            do 604 bin=0,mxbin2
               if(profwt(bin).gt.0.0) then
                  yplot(ndata,1)=profil(bin)-back
                  xplot(ndata,1)=profr(bin)
                  ndata=ndata+1
               endif
604         continue
	    if(ndata.le.1)then
	       print *,' radprf/Error: all data bins empty (i.e. zero-weighted)!'
	       return
	    endif
	    nptsplot(1)=ndata-1
	    NCHAR(1)='82'
            PCOLOR(1)='Default'

*
* Calculate the fitted profile over the data range
*
            rmax=xplot(nptsplot(1),1)
	    nptsplot(2)=mxbin2+1
	    NCHAR(2)='L'
            PCOLOR(2)='Default'
            do 605 ip=1,nptsplot(2)
               radius=(rmax*(ip-1))/mxbin2
               xplot(ip,2)=radius
               yplot(ip,2)=amp*exp(-0.5*((radius/sigma)**gamma))
605         continue
 
	kcurve=2
	char1='Radius (Pixels)'
	char2='Intensity'
	write(char3,25) filename(1:12),fwhm,gamma,axisr
25	format(a12,' FWHM:',f5.1,' Gam:',f3.1,' a/b:',f3.1)
	print *,' Graphic output of the mean star profile :'
	call newplot(xplot,yplot,nptsplot,nmax,kcurve,
     1	char1,char2,char3,nchar,pcolor,plotdev,filename,' ')
 
      endif
 
99    return
      end
*************************************************************************
* Here I added the constraint that the axis ratio should not be too large
* (small galaxies for example)
	include 'jlpsub:starfit_set.for'
	include 'jlpsub:sort_set.for'
