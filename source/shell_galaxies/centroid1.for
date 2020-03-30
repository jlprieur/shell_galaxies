C++*********************************************************************
C CENTROID1
C Determines the centers of star-like images given an initial estimate
C of their positions
C
C SYNTAX:
C     RUNS CENTROID1 in_image approx_list out_list
C
C Example:
C     RUNS CENTROID1 test.BDF test.PAT test.CEN
C
C Nota: the input and output files containing the lists are ASCII files
C with 3 columns: x, y, idiameter
C
C JLP
C Version of 18/07/90
C--**********************************************************************
	PROGRAM CENTROID1
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*       obtain input image and list of positions and extract required
*       descriptor items. obtain workspace and extract positions and
*       identifiers from list. obtain required parameters from environ-
*       ment and call loclst to find centroids. obtain output dataset
*       and add output positions to it. form output descriptor.
*
*STARLINK PARAMETERS
*       ILEVEL
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       INPUT
*               INPUT IMAGE
*       INPUT
*               INPUT LIST OF INITIAL POSITIONS
*       ISIZE
*               SIZE OF SEARCH SQUARE FOR FORMING CENTROID
*       ISIGN
*               INDICATES IF STAR-LIKE FEATURES ARE POSITIVE OR
*               NEGATIVE
*       MAXSHIFT
*               MAXIMUM SHIFT FROM INITIAL POSITION
*       MAXITER
*               MAXIMUM NUMBER OF CENTROIDING ITERATIONS
*       TOLERENC
*               ACCURACY TO WHICH CENTROIDS ARE REQUIRED
*       OUTPUT
*               OUTPUT LIST OF POSITIONS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
C
C Uses EDRS library: NTHMIN, LOCLST, LOCATE, NTHMIN (included here)
*-----------------------------------------------------------------------
	PARAMETER (IDIM1=1000)
	INTEGER*4 NAXIS(2)
	REAL*4 XAPPRO(IDIM1),YAPPRO(IDIM1),XCENT(IDIM1),YCENT(IDIM1)
	INTEGER*4 IDIAM_IN(IDIM1),IDIAM_OUT(IDIM1)
	character idsign*8
	CHARACTER IMAGE_IN*40,NAMEOUT*40,NAMEIN*40
	INTEGER*4 MADRID(1)
	COMMON /VMR/MADRID
10	FORMAT(A)
 
	CALL JLP_BEGIN
	CALL JLP_INQUIFMT
*
* OBTAIN INTERACTION LEVEL
	ilevel=2
*
* OBTAIN INPUT IMAGE FRAME
*
	PRINT *,' INPUT IMAGE :'
	READ(5,10) IMAGE_IN
	CALL JLP_VM_READIMAG(IPIMG,NPIX,NLINES,IMAGE_IN,COMMENTS)
 
*
* IMAGE OBTAINED SUCCESSFULLY... GET INVALID FLAG FROM DESCRIPTOR
*
	   inval=-100000
*
* OBTAIN INPUT XY LIST OF INITIAL SEARCH POSITIONS
*
	PRINT *,' Input list of star positions ?'
7	READ(5,10) NAMEIN
	OPEN(1,FILE=NAMEIN,STATUS='OLD',ERR=7)
 
C Reading the input list :
	I=0
87	 IF(I.GE.IDIM1) GOTO 88
	  READ(1,*,END=9)WORK1,WORK2,IDIAM
	  IF(IDIAM.GE.5)THEN
	    I=I+1
	    XAPPRO(I)=WORK1
	    YAPPRO(I)=WORK2
	    IDIAM_IN(I)=IDIAM
	  ENDIF
         GOTO 87
88	 CONTINUE
9	NPTSIN=I
	CLOSE(1)
	PRINT *,NPTSIN,' POINTS RECEIVED'
	 IF(NPTSIN.EQ.0)THEN
	  STOP
	 ENDIF
*
* Obtain output list for storing the results :
*
8	PRINT *,' Output list of star positions ?'
	READ(5,10) NAMEOUT
	OPEN(1,FILE=NAMEOUT,STATUS='NEW',ERR=8)
 
* Obtain search area size (isize),sign of image features (isign),
* max shift from initial position (shftmx), max no. of centroiding
* iterations (maxit) and location accuracy (toll) from environment
*
	ISIZE=15
	WRITE(6,*) ' Size of search area : (in pixels, odd number)'
	READ(5,*) ISIZE
 
C Positive features only :
	ISIGN=1
 
C Take the default values: shift max, max iterations, tolerence:
	SHFTMX=ISIZE*1.5
	MAXIT=3
	TOLL=0.05
*
* Call LOCLST to find the image centroids
*
	CALL LOCLST(XAPPRO,YAPPRO,IDIAM_IN,NPTSIN,MADRID(IPIMG),
     1	NPIX,NLINES,INVAL,ISIZE,ISIGN,SHFTMX,MAXIT,TOLL,
     1	ILEVEL,XCENT,YCENT,IDIAM_OUT,NPTSOUT,IERR)
 
*
* If there are no output locations, give message and abort
*
 
	IF(NPTSOUT.LE.0) THEN
	  PRINT *,' No stars found'
	ELSE
	  DO I=1,NPTSOUT
	    WRITE(1,*)XCENT(I),YCENT(I),IDIAM_OUT(I)
	  END DO
	ENDIF
 
 99	CLOSE(1)
	CALL JLP_END
	STOP
	END
C@**********************************************************************
C LOCLST
C@**********************************************************************
	subroutine loclst(xa,ya,idiam_in,nin,image,npix,nlines,
     1	inval,isize,isign,shftmx,maxit,toll,ilevel,
     1	xb,yb,idiam_out,nout,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE CENTROIDS OF A SET OF IMAGE FEATURES AND PRINT
*       THE RESULTS
*
*METHOD
*       PRINT TITLE FOR TABLE OF RESULTS, CALL LOCATE FOR EACH FEATURE
*       TO FIND THE CENTROIDS. COPY SUCCESSFUL LOCATIONS TO OUTPUT
*       POSITION AND IDENTIFIER LIST. PRINT RESULTS.
*
*ARGUMENTS
*       XA,YA (IN)
*       REAL(NIN)
*               APPROXIMATE FEATURE POSITIONS
*       NIN (IN)
*       INTEGER
*               NUMBER OF INPUT POSITIONS
*       IMAGE (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               DIMENSIONS OF IMAGE
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IMAGE
*       ISIZE (IN)
*       INTEGER
*               SIZE OF SEARCH AREA SIDE
*       ISIGN (IN)
*       INTEGER
*               POSITIVE IF IMAGE FEATURES ARE POSITIVE, OTHERWISE
*               NEGATIVE
*       SHFTMX (IN)
*       REAL
*               MAX SHIFT ALLOWED FROM STARTING POSITIONS
*       MAXIT (IN)
*       INTEGER
*               MAX NUMBER OF CENTROIDING ITERATIONS
*       TOLL (IN)
*       REAL
*               ACCURACY REQUIRED IN THE CENTROID POSITION
*       ILEVEL (IN)
*       INTEGER
*               FLAG TO CONTROL PRINTING OF RESULTS
*       XB,YB (OUT)
*       REAL(NIN)
*               OUTPUT CENTROID ESTIMATES. THOSE NOT FOUND ARE OMITTED
*       NOUT (OUT)
*       INTEGER
*               THE NUMBER OF SUCCESSFULLY FOUND LOCATIONS IN X,Y
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       THIS PACKAGE:
*               LOCATE
*
*NOTES
*       USES REAL*4 AND BYTE ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real*4 image(npix,nlines)
	real xa(nin),ya(nin),xb(nin),yb(nin)
	integer*4 idiam_in(*),idiam_out(*)
      character prbuf*80,errmsg(2)*24
      data errmsg/'  MAXIMUM SHIFT EXCEEDED','  NO IMAGE FEATURE FOUND'
     :/
 
*
* IF ILEVEL IS ABOVE 1, PRINT TITLES
*
 
      if(ilevel.gt.1) then
         write(6,14)
14       format(/,t27,'POSITION SEARCHED',t57,'CENTROID FOUND')
         write(6,17)
17       format(t27,'-----------------',t57,'--------------')
         write(6,15)
15       format(' ',t24,2(' X COORD.',5x,' Y COORD.',5x))
         write(6,16)
16       format('     ----------',t24,2(' --------',5x,' --------',5x))
      endif
 
 
*
* SCAN THROUGH LIST, CALLING LOCATE TO FIND EACH CENTROID IN TURN
*
      nout=0
 
      do 7 i=1,nin
         call locate(xa(i),ya(i),image,npix,nlines,inval,isize,isign
     :    ,shftmx,maxit,toll,xb(nout+1),yb(nout+1),ierr)
 
*
* IF A FATAL ERROR IS FOUND, RETURN
*
 
         if(ierr.ge.3) go to 99
 
*
         if(ierr.eq.0) then
            nout=nout+1
	    idiam_out(nout)=idiam_in(i)
 
*
* IF ILEVEL IS ABOVE 1, PRINT THE SUCCESSFUL LOCATION
*
 
            if(ilevel.gt.1) then
 
               write(6,10)xa(i),ya(i),xb(nout),yb(nout)
10             format(' ',t23,4(ss,g13.6,1x))
            endif
 
 
         else
 
*
* IF THE CENTROID WAS NOT FOUND, THEN IF ILEVEL IS ABOVE 2, PRINT
* DETAILS
*
 
            if(ilevel.gt.2) then
 
               write(6,11)xa(i),ya(i),errmsg(ierr)
11             format(' ',t23,2(ss,g13.6,1x),a24)
            endif
 
         endif
 
7     continue
 
 
*
* IF A LIST HAS BEEN PRINTED, ADD SPACING AT THE END
*
 
      if(ilevel.ge.2) then
         print *,' '
         print *,' '
      endif
 
99    return
 
      end
 
C@***********************************************************************
C LOCATE
C@***********************************************************************
      subroutine locate(x0,y0,image,npix,nlines,inval,isize,isign,
     :shftmx,maxit,toll,x,y,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO LOCATE THE CENTROIDS OF STAR LIKE IMAGE FEATURES
*
*METHOD
*       FORM MARGINAL PROFILES WITHIN A SEARCH SQUARE. SUBTRACT A
*       BACKGROUND ESTIMATE FROM EACH PROFILE AND FIND THE PROFILE
*       CENTROIDS. REPEAT FOR A SET NUMBER OF ITERATIONS , OR UNTIL
*       THE REQUIRED ACCURACY IS REACHED.
*
*ARGUMENTS
*       X0,Y0 (IN)
*       REAL
*               THE INITIAL ESTIMATE OF THE FEATURE POSITION
*       IMAGE (IN)
*       REAL*4(NPIX,NLINES)
*               THE IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF IMAGE
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IMAGE
*       ISIZE (IN)
*       INTEGER
*               SIZE OF THE SEARCH SQUARE SIDE
*       ISIGN (IN)
*       INTEGER
*               POSITIVE IF THE IMAGE FEATURES ARE POSITIVE, NEGATIVE
*               IF THEY ARE NEGATIVE
*       SHFTMX (IN)
*       REAL
*               MAX SHIFT ALLOWED FROM INITIAL POSITION X0,Y0
*       MAXIT (IN)
*       INTEGER
*               NUMBER OF CENTROIDING ITERATIONS
*       TOLL (IN)
*       REAL
*               ACCURACY REQUIRED IN THE CENTROID POSITION. ITERATIONS
*               STOP WHEN THIS ACCURACY HAS BEEN MET
*       X,Y (OUT)
*       REAL
*               CENTROID POSITION FOUND. IF IERR IS NOT ZERO, X AND Y
*               RETURN THE VALUES OF X0,Y0
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG, 0:SUCCESS
*                           1:NO DATA IN SEARCH AREA
*                           2:SHFTMX EXCEEDED
*
*CALLS
*       THIS PACKAGE:
*               NTHMIN
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real*4 image(npix,nlines)
 
*
* SET MAXIMUM SIZE OF SEARCH AREA
*
      parameter (maxsiz=51)
      real xav(maxsiz),yav(maxsiz),stak(maxsiz/4+2)
      integer nxav(maxsiz),nyav(maxsiz)
 
*
* SET SQUARE CENTRE TO STARTING POSITION AND CHECK ARGUMENT VALIDITY
*
      x=x0
      y=y0
      ierr=0
 
      if(npix.lt.1) then
         ierr=3
 
      else if(nlines.lt.1) then
         ierr=4
 
      else
 
*
* RESTRICT SEARCH SQUARE SIZE TO LIE FROM 3 TO MAXSIZ
*
         nsamp=min(max(3,isize),maxsiz)
         ihalf=nsamp/2
 
*
* MAKE NUMBER OF ITERATIONS AT LEAST 1
*
         niter=max(1,maxit)
 
*
* START COUNTING ITERATIONS
* -------------------------
*
         iter=0
63       iter=iter+1
 
*
* FIND STARTING EDGE OF SEARCH SQUARE
*
         istart=nint(min(max(-1.0e6,x),1.0e6))-(ihalf+1)
         jstart=nint(min(max(-1.0e6,y),1.0e6))-(ihalf+1)
 
*
* REMEMBER STARTING POSITION THIS ITERATION
*
         xlast=x
         ylast=y
 
*
* INITIALLISE ARRAYS FOR FORMING MARGINAL PROFILES
*
 
         do 10 nbin=1,nsamp
            xav(nbin)=0.0
            yav(nbin)=0.0
            nxav(nbin)=0
            nyav(nbin)=0
10       continue
 
 
*
* SCAN SEARCH AREA, FORMING X AND Y PROFILES FROM ALL VALID PIXELS
*
 
         do 30 j=1,nsamp
            jposn=jstart+j
 
            if((jposn.ge.1).and.(jposn.le.nlines)) then
 
               do 20 i=1,nsamp
                  iposn=istart+i
 
                  if((iposn.ge.1).and.(iposn.le.npix)) then
 
                     if(image(iposn,jposn).ne.inval) then
                        xav(i)=xav(i)+image(iposn,jposn)
                        yav(j)=yav(j)+image(iposn,jposn)
                        nxav(i)=nxav(i)+1
                        nyav(j)=nyav(j)+1
                     endif
 
                  endif
 
20             continue
 
            endif
 
30       continue
 
 
*
* EVALUATE THOSE X PROFILE BINS WHICH CONTAIN AT LEAST 1 VALID PIXEL,
* INVERT THE RESULTS IF ISIGN.LT.0
*
 
         do 80 nbin=1,nsamp
 
            if(nxav(nbin).eq.0) then
               xav(nbin)=1.0e10
 
            else
               xav(nbin)=xav(nbin)/nxav(nbin)
 
               if(isign.lt.0) then
                  xav(nbin)=-xav(nbin)
               endif
 
            endif
 
 
*
* REPEAT FOR THE Y PROFILE
*
 
            if(nyav(nbin).eq.0) then
               yav(nbin)=1.0e10
 
            else
               yav(nbin)=yav(nbin)/nyav(nbin)
 
               if(isign.lt.0) then
                  yav(nbin)=-yav(nbin)
               endif
 
            endif
 
80       continue
 
 
*
* CALL NTHMIN TO FIND THE LOWER QUARTILE POINT IN EACH PROFILE AS A
* BACKGROUND ESTIMATE
*
         nth=max(nsamp/4,2)
         call nthmin(xav,nsamp,nth,stak,ierr)
         zlevx=stak(1)
         call nthmin(yav,nsamp,nth,stak,ierr)
         zlevy=stak(1)
 
*
* INITIALLISE SUMS FOR FORMING CENTROIDS
*
         xnumer=0.0
         xdenom=0.0
         ynumer=0.0
         ydenom=0.0
         xposn=istart
         yposn=jstart
 
*
* SCAN THE PROFILES, USING ALL DATA ABOVE THE BACKGROUND TO FORM
* SUMS FOR THE CENTROIDS
*
 
         do 110 nbin=1,nsamp
            xposn=xposn+1.0
            yposn=yposn+1.0
 
            if(nxav(nbin).ne.0) then
               data=max(xav(nbin)-zlevx,0.0)
               xnumer=xnumer+xposn*data
               xdenom=xdenom+data
            endif
 
 
            if(nyav(nbin).ne.0) then
               data=max(yav(nbin)-zlevy,0.0)
               ynumer=ynumer+yposn*data
               ydenom=ydenom+data
            endif
 
110      continue
 
 
*
* IF EITHER PROFILE CONTAINED NO DATA, ABORT WITH IERR=2
*
 
         if((xdenom.lt.1.0e-10).and.(ydenom.lt.1.0e-10)) then
            ierr=2
            x=x0
            y=y0
 
         else
 
            if(xdenom.lt.1.0e-10)then
               x=xlast
               y=ynumer/ydenom
 
            else if(ydenom.lt.1.0e-10)then
               x=xnumer/xdenom
               y=ylast
 
            else
 
*
* OTHERWISE FORM THE X AND Y CENTROIDS AND FIND THE SHIFT FROM
* THE INITIAL POSITION
*
               x=xnumer/xdenom
               y=ynumer/ydenom
            endif
 
            shift=sqrt((x-x0)**2+(y-y0)**2)
 
*
* IF MAX SHIFT IS EXCEEDED, ABORT WITH IERR=1
*
 
            if(shift.gt.shftmx) then
               ierr=1
               x=x0
               y=y0
 
            else
 
*
* OTHERWISE FIND THE POSITION SHIFT THIS ITERATION
*
               psncng=sqrt((x-xlast)**2+(y-ylast)**2)
 
*
* IF REQUIRED ACCURACY HAS BEEN MET, EXIT. OTHERWISE, IF ITERATIONS
* REMAIN, GO ROUND ITERATION LOOP AGAIN
*
 
               if(psncng.gt.toll) then
 
                  if(iter.lt.niter) go to 63
               endif
 
            endif
 
         endif
 
      endif
 
      return
      end
C***************************************************************************
      subroutine nthmin(x,nx,n,stak,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE N'TH SMALLEST NUMBER IN A SET OF DATA VALUES
*
*METHOD
*       MAINTAIN A STACK OF THE SMALLEST VALUES. COMPARE EACH DATA
*       POINT WITH THE TOP OF STACK.. IF SMALLER, INSERT IT IN THE
*       STACK AT AN APPROPRIATE LEVEL TO MAINTAIN NON-INCREASING
*       STACK ORDER. TOP OF STACK IS LOST WHEN DATA IS INSERTED.
*
*ARGUMENTS
*       X (IN)
*       REAL(NX)
*               ARRAY OF DATA VALUES
*       NX (IN)
*       INTEGER
*               NUMBER OF DATA POINTS
*       N (IN)
*       INTEGER
*               SPECIFIES THE N IN 'NTH SMALLEST DATA VALUE'
*       STAK (OUT)
*       REAL(N)
*               STACK OF N SMALLEST VALUES.. N'TH SMALLEST IN STAK(1)
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real x(nx),stak(n)
      parameter (extrem=1.0e20)
 
*
* CHECK ARGUMENT VALIDITY
*
 
      if(n.gt.nx) then
         ierr=1
 
      else
         ierr=0
 
*
* INITIALLISE THE STACK OF SMALLEST VALUES
*
 
         do 1 i=1,n
            stak(i)=extrem
1        continue
 
 
*
* COMPARE EACH DATA VALUE WITH THE TOP OF STACK
*
 
         do 6 j=1,nx
 
            if(x(j).lt.stak(1)) then
 
*
* IF LESS THAN TOP OF STACK, IT BELONGS IN THE STACK.. SEARCH DOWN
* THE STACK, MOVING CONTENTS UP.
*
 
               do 2 locn=2,n
 
                  if(x(j).lt.stak(locn)) then
                     stak(locn-1)=stak(locn)
 
                  else
 
*
* WHEN CORRECT LEVEL IS FOUND, INSERT DATA IN STACK
*
                     stak(locn-1)=x(j)
                     go to 61
 
                  endif
 
2              continue
 
 
*
* ARRIVE HERE IF DATA POINT BELONGS ON BOTTOM OF STACK
*
               stak(n)=x(j)
61             continue
            endif
 
6        continue
 
      endif
 
      return
 
      end
C***********************************************************************
