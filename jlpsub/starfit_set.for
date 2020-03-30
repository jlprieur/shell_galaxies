C++*********************************************************************
C STARFIT_SET
C To fit a star model (ellipses and gaussian profile)
C Extracted from EDRSLIB
C Routines called by STARFIT1, 
C or needed for a secondary call (NTHMIN called by CLNSTA)
C
C Contains 
C    CLNSTA, GAUFIT, ELLIPS, WMODE, PRFFIT, MODE, NTHMIN
C JLP
C Version 02-11-93
C--*********************************************************************
      subroutine clnsta(x,imin,imax,xcen,auto_sky,sky_level,italk)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO 'CLEAN' A MARGINAL STAR PROFILE, REMOVING NEIGHBOURING
*       STARS AND BLEMISHES
*
*METHOD
*       Subtract lower quartile data point as a background estimate
*       Work out from centre of star, preventing each data point from
*       exceeding the maximum of the two previous data values. This
*       ensures that the data does not increase with increasing
*       distance from the centre.
*
*ARGUMENTS
*       X (IN/OUT)
*       REAL(IMIN:IMAX)
*               DATA ARRAY..INVALID VALUES ARE SET ABOVE 1.0E20
*       IMIN,IMAX (IN)
*       INTEGER
*               FIRST AND LAST COORDINATES OF DATA ARRAY.. ALSO
*               DEFINE THE DIMENSION OF X
*       XCEN (IN)
*       REAL
*               ESTIMATE OF POSITION OF STAR CENTRE
*
*CALLS
*       THIS PACKAGE:
*               NTHMIN
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      parameter (nmin=2)
      real x(imin:imax),stak(25),smin(nmin)
      real sky_level
      logical auto_sky
      integer italk

* If manual input of sky level and if no "cleaning is wanted":
       if(.not.auto_sky)then
         do i=imin,imax
            if(x(i).lt.1.0e20) then
              x(i)=x(i)-sky_level
              if(x(i).lt.0.)x(i)=0.
            else
              x(i)=0.
            endif
         end do 
         return
       endif

*
* CALCULATE THE NUMBER OF DATA POINTS AND FIND THE LOWER QUARTILE
* POINT AS A BACKGROUND ESTIMATE
*
          nx=imax-imin+1
          nquart=min(max(1,nint(nx*0.25)),25)
* Finds the nquart'th minimum value of x array and stores it in stak(1):
          call nthmin(x(imin),nx,nquart,stak,ierr)

       if(italk.eq.1) write(6,21) stak(1)
21     format('clnsta/Estimated background: ',G12.5)
 
*
* SUBTRACT THE BACKGROUND FROM ALL THE VALID DATA POINTS, FINDING
* THE MAXIMUM VALUE
*
 

      smax=-2.0e20
 
      do 17 i=imin,imax
 
* Previously, empty bins have been set to 1.e21...

         if(x(i).lt.1.0e20) then
            x(i)=x(i)-stak(1)
            smax=max(smax,x(i))
         endif
 
17    continue
 
 
*
* WORK OUT FROM THE CENTRE OF THE STAR IMAGE TOWARDS EACH END
* OF THE DATA ARRAY IN TURN
*
      i0=nint(xcen)
 
      do 61 idirn=-1,1,2
 
         if(idirn.lt.0) then
            iend=imin
         else
            iend=imax
         endif
 
 
*
* INITIALLISE THE STORE OF THE LAST NMIN DATA VALUES
*
 
         do 81 j=1,nmin
            smin(j)=smax
81       continue
 
         minloc=1
 
*
* WORK THROUGH THE DATA ARRAY, AT EACH POINT CALCULATING AN UPPER
* DATA LIMIT AS THE MAXIMUM OF THE LAST NMIN VALUES
*
 
         do 60 i=i0,iend,idirn
            uplim=smin(1)
 
            do 82 j=2,nmin
               uplim=max(uplim,smin(j))
82          continue
 
 
*
* LIMIT THE DATA TO THE CURRENT UPPER LIMIT
*
            x(i)=min(uplim,x(i))
 
*
* MINLOC CYCLES FROM 1 TO NMIN AND POINTS TO THE STORE FOR
* THIS VALUE IN THE LIST OF THE LAST NMIN VALUES
*
            minloc=mod(minloc,nmin)+1
 
            if(x(i).ge.0.0) then
               smin(minloc)=x(i)
 
            else
 
*
* DON'T USE NEGATIVE VALUES...REPLACE WITH THE CURRENT UPPER LIMIT
*
               smin(minloc)=uplim
            endif
 
60       continue
 
61    continue
 
      return
 
      end

      subroutine gaufit(data,imin,imax,niter,toll,amp,x0,sigma,back,
     : ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIT A GAUSSIAN TO A 1 DIMENSIONAL ARRAY OF DATA
*
*METHOD
*       FIND INITIAL ESTIMATES OF THE GAUSSIAN AMPLITUDE, MEAN AND
*       WIDTH BY CENTROIDING AND LINEAR LEAST-SQUARES, USING THE
*       LOWER QUARTILE DATA POINT AS AN INITIAL BACKGROUND ESTIMATE.
*       REFINE THE ESTIMATES BY REPEATEDLY SOLVING THE LINEARISED NORMAL
*       EQUATIONS FOR A LEAST-SQUARES FIT.
*
*ARGUMENTS
*       DATA (IN)
*       REAL(IMIN:IMAX)
*               THE DATA ARRAY
*       IMIN,IMAX (IN)
*       INTEGER
*               THE COORDINATES OF THE FIRST AND LAST DATA ELEMENTS
*               THESE ALSO DEFINE THE DIMENSION OF 'DATA'
*       NITER (IN)
*       INTEGER
*               MAXIMUM NUMBER OF REFINING ITERATIONS
*       TOLL (IN)
*       REAL
*               ACCURACY CRITERION: ABSOLUTE FOR THE CENTRE AND
*               RELATIVE FOR THE AMPLITUDE AND WIDTH
*               ACCURACY OF BACK IS ASSESSED AS A FRACTION OF THE
*               GAUSSIAN AMPLITUDE
*       AMP (OUT)
*       REAL
*               THE GAUSSIAN AMPLITUDE
*       X0 (OUT)
*       REAL
*               THE GAUSSIAN CENTRE
*       SIGMA (OUT)
*       REAL
*               THE 'SIGMA' FOR THE GAUSSIAN
*       BACK (OUT)
*       REAL
*               THE BACKGROUND SIGNAL LEVEL
*       IERR (OUT)
*       INTEGER
*               ERROR INDICATOR: ZERO FOR SUCCESS
*               1: ACCURACY CRITERION NOT MET
*               2: ALL DATA HAS THE SAME VALUE
*               3: NORMAL EQUATIONS ARE SINGULAR
*
*CALLS
*       THIS PACKAGE:
*               NTHMIN
*       NAG LIBRARY:
*               F04ATF
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real data(imin:imax),x(4),c(4),stak(25)
      double precision sum0,sum1,sum2,sum3,a(4,4),r(4),dx(4)
      double precision aa(4,4),wks1(4),wks2(4)
C PB with the Sun precompiler...
C      parameter (varmin=0.3**2)
C       parameter (varmin=0.09)
       real varmin
       varmin=0.09
 
*
* INITIALLISE SUMS FOR FORMING INITIAL ESTIMATE OF THE DATA MEAN
* AND STANDARD DEVIATION
*
      ierr=1
      points=imax-imin+1
      sum1=0.0d0
      sum2=0.0d0
      sum3=0.0d0
 
*
* FIND THE LOWER QUARTILE DATA POINT (OR THE 25'TH SMALLEST IF THERE
* ARE TOO MANY) TO USE AS AN INITIAL BACKGROUND ESTIMATE
*
      nquart=min(max(1,nint(points*0.25)),25)
      call nthmin(data(imin),nint(points),nquart,stak,ierrqt)
      x(4)=stak(1)
 
*
* FORM SUMS FOR MEAN AND STANDARD DEVIATION
*
 
      do 1 i=imin,imax
 
*
* DO NOT COUNT NEGATIVE DATA
*
         d=max(0.0,data(i)-x(4))
         sum1=sum1+d
         d=d*i
         sum2=sum2+d
         d=d*i
         sum3=sum3+d
1     continue
 
 
*
* IF THERE ARE NO POSITIVE DATA POINTS, ABORT WITH IERR=2
*
 
      if(sum1.le.0.0d0) then
         print 86
86       format('gaufit/error: no positive data points')
         ierr=2
         go to 99
      endif
 
*
* FORM MEAN AND STANDARD DEVIATION
*
      x(2)=sum2/sum1
      x(3)=sqrt(max(dble(varmin),(sum3-sum2*x(2))/sum1))
 
*
* INITIALLISE SUMS FOR FORMING AN INITIAL ESTIMATE OF THE GAUSSIAN
* AMPLITUDE
*
      sum0=0.0d0
      sum1=0.0d0
      sum2=0.0d0
      sum3=0.0d0
 
*
* CONSIDER THOSE DATA POINTS WITHIN 5 STANDARD DEVIATIONS OF THE MEAN
*
      rvar=0.5/(x(3)**2)
      devmax=(5.0*x(3))**2
 
      do 2 i=imin,imax
         dev=i-x(2)
         dev2=dev*dev
 
         if(dev2.le.devmax) then
 
*
* FORM SUMS
*
            ex=exp(-rvar*dev2)
            d=max(0.0,data(i)-x(4))
            sum0=sum0+ex*ex
            sum1=sum1+d*ex
            sum2=sum2+d*dev*ex
            sum3=sum3+d*dev2*ex
         endif
 
2     continue
 
*
* IF THERE WERE NO SATISFACTORY DATA POINTS, ABORT WITH IERR=2
*
      if(sum0.le.0.0d0.or.sum1.le.0.0d0) then
         print 84
84       format(' gaufit/error: no satisfactory data points')
         ierr=2
         go to 99
      endif
 
 
*
* FORM ESTIMATES OF THE GAUSSIAN HEIGHT AND MEAN
*
      x(1)=sum1/sum0
      x(2)=x(2)+sum2/sum1
 
*
* ESTIMATE THE VARIANCE, THEN MAKE AN APPROXIMATE CORRECTION
* FOR THE FACT THAT THE DATA POINTS WERE WEIGHTED WITH A GAUSSIAN
* USING THE OLD VALUE FOR THE VARIANCE
*
      var=(sum3-(sum2*sum2)/sum1)/sum1
      oldvar=x(3)*x(3)
      var=(oldvar*var)/max(oldvar-var,1.0e-10)
      x(3)=sqrt(max(varmin,var))
 
*
* START THE ITERATION LOOP TO REFINE THE ESTIMATES OF THE GAUSSIAN
* PARAMETERS
*
 
      do 88 iter=1,niter
 
*
* INITIALLISE ARRAYS FOR FORMING THE LINEARISED NORMAL EQUATIONS
*
         do i=1,4
          r(i)=0.0d0
            do j=1,4
             a(i,j)=0.0d0
            end do 
         end do
 
*
* CONSIDER DATA POINTS WITHIN 5 STANDARD DEVIATIONS OF THE MEAN
*
         rvar1=1.0/x(3)
         rvar2=rvar1*rvar1
         rvar=0.5*rvar2
         devmax=(5.0*x(3))**2
 
         do 44 i=imin,imax
            dev=i-x(2)
            dev2=dev*dev
 
            if(dev2.le.devmax) then
 
*
* FORM THE QUANTITIES REQUIRED IN THE NORMAL EQUATIONS
*
               ex=exp(-rvar*dev2)
               c(1)=ex
               c(2)=ex*x(1)*dev*rvar2
               c(3)=c(2)*dev*rvar1
               c(4)=1.0
 
*
* DELTA IS THE DEVIATION OF THE FIT FROM THE DATA
*
               delta=x(1)*ex+x(4)-data(i)
 
*
* FORM SUMS FOR NORMAL EQUATIONS
*
               r(1)=r(1)+delta*c(1)
               r(2)=r(2)+delta*c(2)
               r(3)=r(3)+delta*c(3)
               r(4)=r(4)+delta*c(4)
               a(1,1)=a(1,1)+c(1)*c(1)
               a(2,1)=a(2,1)+c(2)*c(1)
               a(3,1)=a(3,1)+c(3)*c(1)
               a(4,1)=a(4,1)+c(4)*c(1)
               a(2,2)=a(2,2)+c(2)*c(2)
               a(3,2)=a(3,2)+c(3)*c(2)
               a(4,2)=a(4,2)+c(4)*c(2)
               a(3,3)=a(3,3)+c(3)*c(3)
               a(4,3)=a(4,3)+c(4)*c(3)
               a(4,4)=a(4,4)+c(4)*c(4)
            endif
 
44       continue
 
*
* FORM THE COMPLETE SYMMETRIC MATRIX OF NORMAL COEFFICIENTS
*
         a(1,2)=a(2,1)
         a(1,3)=a(3,1)
         a(2,3)=a(3,2)
         a(1,4)=a(4,1)
         a(2,4)=a(4,2)
         a(3,4)=a(4,3)
 
*
* Call NAG routine F04ATF to solve the linearised normal equations
* 
* a dx = r
* aa, wks1 and wks2 are used as working spaces
*
        ifail=1
        call F04ATF(a,4,r,4,dx,aa,4,wks1,wks2,ifail)

C JLP 93:
C JLP98: error! since CGRAD doesnot require A*A but only A!!!!
C         npar = 4
C         call JLP_CGRAD(a,r,dx,npar,npar,ifail)
C Use JLP_INVMAT instead!
C Inversion of a matrix with pivotal elements (A*B=C):
C        IFAIL=0
C        CALL JLP_INVMAT(A,B,C,NN,NNMAX,IFAIL)

 
         if(ifail.ne.0) then
            print 87,ifail
87          format(' gaufit/Error calling f04atf, ifail = ',I5)
C87          format(' gaufit/Error calling JLP_CGRAD, ifail = ',I5)
            ierr=3
            go to 99
         endif
 
 
*
* APPLY THE CORRECTIONS TO THE GAUSSIAN PARAMETERS WITH DAMPING
* FACTORS FOR LARGE AMPLITUDE CHANGES
*
         x(1)=x(1)-dx(1)/(1.0+2.0*abs(dx(1)/max(x(1),1.0e-20)))
         x(2)=x(2)-dx(2)/(1.0+2.0*abs(dx(2)/points))
         x(3)=x(3)-dx(3)/(1.0+2.0*abs(dx(3)/max(x(3),1.0e-20)))
         x(4)=x(4)-dx(4)/(1.0+2.0*abs(dx(4)/max(x(1),1.0e-20)))
 
*
* IF THE ACCURACY CRITERION IS MET, EXIT FROM THE ITERATION LOOP
*
 
         if((abs(dx(1)).le.toll*x(1)).and.(abs(dx(2)).le.toll).and.
     :   (abs(dx(3)).le.toll*x(3)).and.(abs(dx(4)).le.toll*x(1))) then
            ierr=0
            amp=x(1)
            x0=x(2)
            sigma=x(3)
            back=x(4)
            go to 99
 
         endif
 
88    continue
 
99    return
 
      end
 
 
 
      subroutine ellips(sig,sig0,axisr,theta)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO CALCULATE THE AXIS RATIO, INCLINATION AND MINOR AXIS WIDTH
*       OF A STAR IMAGE, GIVEN THE GAUSSIAN WIDTHS OF MARGINAL PROFILES
*       AT 45 DEGREE INTERVALS
*
*METHOD
*       THE RECIPROCAL OF THE SQUARE OF THE WIDTH VARIES APPROXIMATELY
*       LIKE THE LENGTH OF AN ELLIPSE DIAMETER AS THE ANGLE OF
*       PROJECTION VARIES. THE ROUTINE CALCULATES THE REQUIRED
*       PARAMETERS ASSUMING THIS RELATION HOLDS, THEN ITERATES,
*       CALCULATING THE EXPECTED DEVIATION FROM THIS LAW AND SUBTRACTING
*       IT FROM THE DATA BEFORE CALCULATING A NEW ESTIMATE. THE SOLUTION
*       OF THE ELLIPSE EQUATIONS IS ANALOGOUS TO USING THE STOKES
*       PARAMETERS OF LINEAR POLARIZATION TO FIND THE ELLIPSE PARAMETERS
*       THE ROUTINE ASSUMES A GAUSSIAN PROFILE FOR THE STAR
*
*ARGUMENTS
*       SIG (IN)
*       REAL(4)
*               THE GAUSSIAN WIDTHS OF THE MARGINAL PROFILES OF THE STAR
*               IN DIRECTIONS AT 0,45,90 AND 135 DEGREES TO THE X AXIS
*       SIG0 (OUT)
*       REAL
*               THE WIDTH OF THE MINOR AXIS OF THE ELLIPSE
*       AXISR (OUT)
*       REAL
*               THE AXIS RATIO OF THE ELLIPSE.
*       THETA (OUT)
*       REAL
*               THE INCLINATION OF THE MAJOR AXIS TO THE X AXIS IN
*               RADIANS. (X THROUGH Y POSITIVE)
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      parameter (niter=10,tolls=1.0e-4,tolla=1.0e-4,tollt=1.0e-2)
      parameter (ang=0.0174533,t45=45.0*ang,t90=90.0*ang,t135=135.0
     : *ang)
      real sig(4),rsig2(4),t(4)
      double precision d(4),delq,delu,amp1,amp2
 
*
* WORK WITH THE RECIPROCALS OF THE WIDTHS SQUARED... THESE VARY
* APPROXIMATELY ELLIPTICALLY WITH INCLINATION ANGLE
*
 
      do 1 i=1,4
         rsig2(i)=(1.0/sig(i))**2
1     continue
 
 
*
* SET INITIAL ESTIMATES OF ELLIPSE PARAMETERS
*
      sig0=1.0
      axisr=1.0
      theta=0.0
 
*
* PERFORM ITERATIONS TO FIND THE ACCURATE PARAMETERS
*
 
      do 13 iter=1,niter
         rsig02=(1.0/sig0)**2
         axisr2=axisr**2
         t(1)=theta
         t(2)=theta-t45
         t(3)=theta-t90
         t(4)=theta-t135
 
*
* MAKE A CORRECTION TO THE DATA VALUES WHICH IS THE AMOUNT BY WHICH
* THEY WOULD DEVIATE FROM A PURE ELLIPTICAL VARIATION GIVEN THE
* CURRENT ELLIPSE PARAMETERS
*
 
         do 12 i=1,4
            c=cos(t(i))
            s=sin(t(i))
            d(i)=rsig02*((c*s*(axisr2-1.0))**2)/(axisr2*((axisr2*c*c)
     :       +(s*s)))
            d(i)=d(i)+rsig2(i)
12       continue
 
 
*
* NOW FIND THE ELLIPSE PARAMETERS ASSUMING THE DATA VARIES ELLIPTICALLY
*
         delq=d(3)-d(1)
         delu=d(4)-d(2)
         amp1=0.5d0*sqrt(delq**2+delu**2)
         amp2=0.25d0*(d(1)+d(2)+d(3)+d(4))
         amp1=min(amp1,0.9999d0*amp2)
 
         if(delq.eq.0.0d0.and.delu.eq.0.0d0) then
            thetan=0.0
 
         else
            thetan=0.5d0*atan2(delu,delq)
         endif
 
         rsig02=amp1+amp2
         sig0n=sqrt(1.0/max(rsig02,1.0e-10))
         axisrn=sqrt((amp1+amp2)/(amp2-amp1))
 
*
* CALCULATE THE CHANGES TO THE PARAMETERS
*
         dsig0=abs(sig0n-sig0)
         daxisr=abs(axisrn-axisr)
         dtheta=abs(thetan-theta)
         sig0=sig0n
         axisr=axisrn
         theta=thetan
 
*
* IF THE ACCURACY CRITERION IS MET, EXIT FROM THE ITERATION LOOP
*
 
         if(dsig0.le.tolls.and.daxisr.le.tolla.and.dtheta.le.tollt)then
            go to 99
 
         endif
 
13    continue
 
99    return
 
      end
 
 
 
      subroutine wmode(x,w,nx,pbad,niter,toll,xmode,sigma)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE MOST LIKELY MEAN VALUE FOR A SET OF DATA POINTS
*       WITH DIFFERENT NORMALLY DISTRIBUTED ERRORS AND A CONSTANT
*       PROBABILITY OF BEING CORRUPT.
*
*METHOD
*       THIS ROUTINE USES THE STATISTICAL MODEL USED BY MODE TO
*       MINIMISE THE EFFECT OF CORRUPT DATA, BUT INCLUDES DIFFERENT
*       WEIGHTS FOR EACH DATA POINT, TO ALLOW FOR DIFFERENT INTRINSIC
*       ERRORS. SEE MODE FOR A FULLER DESCRIPTION OF THE ALGORITHM.
*
*ARGUMENTS
*       X (IN)
*       REAL(NX)
*               AN ARRAY OF DATA VALUES
*       W (IN)
*       REAL(NX)
*               AN ARRAY OF WEIGHTS FOR EACH DATA POINT IN X. THE
*               WEIGHTS ARE INVERSELY PROPORTIONAL TO THE SQUARE OF
*               THE RELATIVE ERRORS ON EACH DATA POINT.
*       NX (IN)
*       INTEGER
*               THE NUMBER OF DATA POINTS
*       PBAD (IN)
*       REAL
*               AN ESTIMATE OF THE PROBABILITY THAT ANY ONE DATA POINT
*               IS CORRUPT. (THIS VALUE NOT VERY CRITICAL)
*       NITER (IN)
*       INTEGER
*               THE MAXIMUM NUMBER OF ITERATIONS TO BE PERFORMED
*       TOLL (IN)
*       REAL
*               THE ABSOLUTE ACCURACY REQUIRED IN THE MEAN VALUE.
*               ITERATIONS CEASE WHEN TWO SUCCESSIVE ITERATIONS AGREE
*               TO WITHIN THIS VALUE.
*       XMODE (OUT)
*       REAL
*               THE ESTIMATE OF THE UNCORRUPTED MEAN VALUE
*       SIGMA (OUT)
*       REAL
*               AN ESTIMATE OF THE UNCORRUPTED NORMALISED STANDARD
*               DEVIATION OF THE DATA POINTS. AN ESTIMATE OF THE
*               STANDARD DEVIATION OF ANY ONE DATA POINT IS:
*                   SIGMA/SQRT(W) , WHERE W IS ITS WEIGHT.
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real x(nx),w(nx)
      double precision sum1,sum2,sum3
 
*
* FORM SUMS FOR FINDING THE MEAN AND STANDARD DEVIATION OF THE
* DATA
*
      sum1=0.0d0
      sum2=0.0d0
      sum3=0.0d0
 
      do 1 i=1,nx
         data=w(i)
         sum1=sum1+data
         data=data*x(i)
         sum2=sum2+data
         data=data*x(i)
         sum3=sum3+data
1     continue
 
 
*
* CALCULATE THE WEIGHTED MEAN AND VARIANCE
*
      xmode=sum2/sum1
      sig2=(sum3-(sum2*sum2)/sum1)/nx
      sig2=max(0.0,sig2)
 
*
* HAVING FORMED THE INITIAL ESTIMATES, THE MAIN ITERATION LOOP
* STARTS HERE
*
 
      do 3 iter=1,niter
 
*
* CALCULATE THE STANDARD DEVIATION AND NORMALISATION CONSTANTS
*
         sig2=max(1.0e-20,sig2)
         sig1=sqrt(sig2)
         w1=1.0/sig1
         w2=0.5/sig2
         pbnorm=0.707107*pbad
 
*
* SET LIMIT AT WHICH POINTS ARE NEGLIGIBLE AT 10 STAND. DEVS.
*
         devlim=100.0*sig2
 
*
* INITIALLISE SUMS FOR FINDING NEW ESTIMATES
*
         suma=0.0
         sumb=0.0
         sumc=0.0
         sumd=0.0
 
*
* COUNT THROUGH DATA POINTS
*
 
         do 2 i=1,nx
 
*
* FIND DEVIATION FROM MODE, NORMALISED TO THAT EXPECTED FROM
* THE WEIGHTS SUPPLIED
*
            dx=x(i)-xmode
            dx2=dx*dx
            dev2=dx2*w(i)
 
*
* IF POINT IS NOT NEGLIGIBLE, CALCULATE THE FRACTIONAL PROBABILITY
* THAT IT IS GOOD
*
 
            if(dev2.le.devlim) then
               ex=exp(-w2*dev2)
               prob=ex/(ex+pbnorm)
               suma=suma+prob
               sumb=sumb+prob*w(i)
               sumc=sumc+dx*prob*w(i)
               sumd=sumd+dev2*prob
            endif
 
2        continue
 
 
*
* CALCULATE THE NEW ESTIMATES
*
         suma=max(suma,1.0e-20)
         sumb=max(sumb,1.0e-20)
         dxmode=sumc/sumb
         xmode=xmode+dxmode
         sig2=sumd/suma
 
*
* IF REQUIRED ACCURACY HAS BEEN ACHIEVED, EXIT ITERATION LOOP
*
 
         if(abs(dxmode).le.toll) then
            go to 7
 
         endif
 
3     continue
 
7     sigma=sqrt(sig2)
      return
 
      end
 

      subroutine prffit(p,pw,pr,np,niter,toll,amp,back,sigma,gamma,
     :ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIT A LEAST-SQUARES RADIAL STAR PROFILE TO APPROPRIATELY
*       BINNED DATA
*
*METHOD
*       THE ROUTINE REFINES INITIAL ESTIMATES OF THE STAR AMPLITUDE,
*       THE BACKGROUND, THE 'SIGMA' AND THE RADIAL EXPONENT G WHICH
*       ARE GIVEN ON ENTRY. THE ROUTINE REPEATEDLY FORMS AND SOLVES
*       THE LINEARISED NORMAL EQUATIONS FOR A LEAST-SQUARES FIT.
*
*ARGUMENTS
*       P (IN)
*       REAL(NP)
*               AN ARRAY OF DATA VALUES AT VARYING DISTANCES FROM THE
*               STAR CENTRE
*       PW (IN)
*       REAL(NP)
*               AN ARRAY OF WEIGHTS (INVERSELY PROPORTIONAL TO THE
*               SQUARE OF THE PROBABLE ERROR) ASSOCIATED WITH THE DATA
*               VALUES IN P
*       PR (IN)
*       REAL(NP)
*               AN ARRAY OF RADII ASSOCIATED WITH THE DATA VALUES IN P
*       NP (IN)
*       INTEGER
*               THE DIMENSION OF P,PW,PR
*       NITER (IN)
*       INTEGER
*               THE MAXIMUM NUMBER OF REFINING ITERATIONS
*       TOLL (IN)
*       REAL
*               THE FRACTIONAL ACCURACY REQUIRED IN THE RESULT
*               ACCURACY IS ASSESSED AS A FRACTION OF THE PARAMETER
*               VALUE FOR AMP,SIGMA AND GAMMA. THE ACCURACY OF BACK
*               IS ASSESSED AS A FRACTION OF AMP. ITERATIONS CEASE
*               WHEN TWO SUCCESSIVE ITERATIONS GIVE RESULTS AGREEING
*               WITHIN THE RELATIVE ACCURACY TOLL.
*       AMP (IN/OUT)
*       REAL
*               THE STAR AMPLITUDE
*       BACK (IN/OUT)
*       REAL
*               THE BACKGROUND SIGNAL
*       SIGMA (IN/OUT)
*       REAL
*               THE STAR 'SIGMA'
*       GAMMA (IN/OUT)
*       REAL
*               THE EXPONENT IN THE RADIAL PROFILE FUNCTION
*               THE FITTED FUNCTION IS:
*                   AMP*EXP(-0.5*(RADIUS/SIGMA)**GAMMA)+BACK
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*                           1: ACCURACY CRITERION NOT MET
*
*CALLS
*       NAG LIBRARY:
*               F04ATF
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      parameter (npar=4)
      real p(np),pw(np),pr(np),x(npar)
      double precision r(npar),a(npar,npar),c(npar),dx(npar)
      double precision aa(npar,npar),wks1(npar),wks2(npar)
 
*
* COPY INITIAL PARAMETER ESTIMATES TO ARRAY X
*
      ierr=1
      x(1)=back
      x(2)=amp
      x(3)=sigma
      x(4)=gamma
 
*
* BEGIN THE ITERATION LOOP:
*
 
      do 88 iter=1,niter
 
*
* INITIALLISE ARRAYS FOR FORMING THE LINEARISED NORMAL EQUATIONS
*
 
         do 6 i=1,npar
            r(i)=0.0d0
 
            do 16 j=1,npar
               a(i,j)=0.0d0
16          continue
 
6        continue
 
 
*
* CONSIDER EACH DATA POINT WITH POSITIVE WEIGHT
*
         rsig=1.0/x(3)
 
         do 44 i=1,np
 
            if(pw(i).gt.1.0e-10) then
 
*
* FORM THE REQUIRED PARAMETERS FOR THE NORMAL EQUATIONS
*
               alpha=(pr(i)*rsig)**x(4)
               ex=exp(-0.5*alpha)
               beta=x(2)*ex*alpha*0.5
* Four coefficients: 
               c(1)=1.0d0
               c(2)=ex
               c(3)=beta*rsig*x(4)
               c(4)=-beta*log(max(1.0e-20,pr(i))*rsig)
 
*
* FORM DEVIATION OF CURRENT PROFILE FROM DATA POINT
*
               delta=(x(1)+x(2)*ex-p(i))*pw(i)
 
*
* FORM SUMS FOR NORMAL EQUATIONS
*
 
               do 174 j=1,npar
                  r(j)=r(j)+delta*c(j)
 
                  do 173 k=1,j
                     a(k,j)=a(k,j)+c(k)*c(j)*pw(i)
173               continue
 
174            continue
 
            endif
 
44       continue
 
 
*
* FORM OTHER HALF OF SYMMETRIC MATRIX OF COEFFICIENTS
*
 
         do 164 j=1,npar-1
 
            do 163 i=j+1,npar
               a(i,j)=a(j,i)
163         continue
 
164      continue
 
 
*
* CALL NAG ROUTINE F04ATF TO SOLVE THE NORMAL EQUATIONS
*
* a dx = r
* aa, wks1 and wks2 are used as working spaces
         ifail=1
         call f04atf(a,npar,r,npar,dx,aa,npar,wks1,wks2,ifail)
 
C JLP 93:
C         call JLP_CGRAD(a,r,dx,npar,npar,ifail)
C JLP98: error! since CGRAD doesnot require A*A but only A!!!!
C Use JLP_INVMAT instead!
*
* IF THE EQUATIONS WERE SINGULAR, ABORT WITH IERR=2
*
         if(ifail.ne.0) then
            print 87,ifail
87          format(' prffit/Error calling f04atf, ifail = ',I5)
C87          format(' prffit/Error calling JLP_CGRAD, ifail = ',I5)
            ierr=2
            go to 99
         endif
 
*
* APPLY THE RESULTANT CORRECTIONS TO THE PROFILE PARAMETERS
* WITH DAMPING FACTORS FOR LARGE AMPLITUDE CHANGES
*
         x(1)=x(1)-dx(1)
         x(2)=x(2)-dx(2)/(1.0+2.0*abs(dx(2)/max(1.0e-20,x(2))))
         x(3)=x(3)-dx(3)/(1.0+2.0*abs(dx(3)/max(1.0e-20,x(3))))
         x(4)=x(4)-dx(4)/(1.0+2.0*abs(dx(4)/max(1.0e-20,x(4))))
 
*
* FORM RELATIVE CHANGES IN EACH PARAMETER
*
         e1=abs(dx(1)/max(1.0e-20,x(2)))
         e2=abs(dx(2)/max(1.0e-20,x(2)))
         e3=abs(dx(3)/max(1.0e-20,x(3)))
         e4=abs(dx(4)/max(1.0e-20,x(4)))
 
*
* IF RELATIVE ACCURACY CRITERION IS MET, EXIT FROM ITERATION LOOP
*
 
         if(max(e1,e2,e3,e4).le.toll) then
            back=x(1)
            amp=x(2)
            sigma=x(3)
            gamma=x(4)
            ierr=0
            go to 99
 
         endif
 
88    continue
 
99    return
 
      end

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
      subroutine mode(x,nx,pbad,niter,toll,xmode,sigma)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO ESTIMATE THE MEAN OF A NUMBER OF NORMALLY DISTRIBUTED DATA
*       VALUES, SOME OF WHICH MAY BE CORRUPT.
*
*METHOD
*       THE ROUTINE IS BASED ON MAXIMISING THE LIKELIHOOD FUNCTION
*       FOR A STATISTICAL MODEL IN WHICH ANY OF THE DATA POINTS HAS
*       A CONSTANT PROBABILITY OF BEING CORRUPT. A WEIGHTED MEAN IS
*       FORMED WITH WEIGHTS CHOSEN ACCORDING TO THE DEVIATION OF EACH
*       DATA POINT FROM THE CURRENT ESTIMATE OF THE MEAN. THE WEIGHTS
*       ARE DERIVED FROM THE RELATIVE PROBABILITIES OF BEING VALID
*       OR CORRUPT. A SEQUENCE OF THESE ITERATIONS CONVERGES TO A
*       STATIONARY POINT IN THE LIKELIHOOD FUNCTION. THE ROUTINE
*       APPROXIMATES TO A K-SIGMA CLIPPING ALGORITHM FOR A LARGE NUMBER
*       OF DATA POINTS AND TO A MODE-ESTIMATING ALGORITHM FOR FEWER
*       DATA POINTS.
*
*ARGUMENTS
*       X (IN)
*       REAL(NX)
*               AN ARRAY OF DATA VALUES
*       NX (IN)
*       INTEGER
*               THE NUMBER OF DATA VALUES
*       PBAD (IN)
*       REAL
*               AN ESTIMATE OF THE PROBABILITY THAT ANY ONE DATA POINT
*               WILL BE CORRUPT (THIS VALUE IS NOT CRITICAL)
*       NITER (IN)
*       INTEGER
*               THE MAXIMUM NUMBER OF ITERATIONS REQUIRED
*       TOLL (IN)
*       REAL
*               THE ABSOLUTE ACCURACY REQUIRED IN THE ESTIMATE OF THE
*               MEAN. ITERATIONS CEASE WHEN TWO SUCCESSIVE ESTIMATES
*               DIFFER BY LESS THAN THIS AMOUNT
*       XMODE (OUT)
*       REAL
*               THE ESTIMATE OF THE UNCORRUPTED MEAN
*       SIGMA (OUT)
*       REAL
*               AN ESTIMATE OF THE UNCORRUPTED STANDARD DEVIATION
*               OF THE DATA POINTS
*
*CALLS
*       NONE
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real x(nx)
      double precision sum1,sum2,sum3
 
*
* FORM INITIAL ESTIMATE OF MEAN AND SIGMA
*
      sum1=0.0d0
      sum2=0.0d0
      sum3=0.0d0
 
      do 1 i=1,nx
         sum1=sum1+1.0d0
         sum2=sum2+x(i)
         sum3=sum3+x(i)*x(i)
1     continue
 
      xmode=sum2/sum1
      sig2=(sum3-(sum2*sum2)/sum1)/sum1
      sig2=max(sig2,0.0)
 
*
* NOW START THE ITERATION LOOP
*
      pbnorm=pbad*0.707107
 
      do 3 iter=1,niter
         sig2=max(sig2,1.0e-20,1.0e-12*xmode**2)
         w2=0.5/sig2
 
*
* INITIALLISE SUMS FOR FORMING NEW ESTIMATE
*
         suma=0.0
         sumb=0.0
         sumc=0.0
         devlim=100.0*sig2
 
*
* SCAN THROUGH THE DATA POINTS, FORMING WEIGHTED SUMS TO CALCULATE
* NEW MEAN AND VARIANCE
*
 
         do 2 i=1,nx
            dev=x(i)-xmode
            dev2=dev*dev
 
*
* IGNORE POINTS MORE THAN 10 SIGMA FROM MODE
*
 
            if(dev2.le.devlim) then
 
*
* WEIGHTS DEPEND ON THE FRACTIONAL PROBABILITY OF BEING GOOD DATA
*
               ex=exp(-w2*dev2)
               prob=ex/(pbnorm+ex)
               suma=suma+prob
               sumb=sumb+dev*prob
               sumc=sumc+dev2*prob
            endif
 
2        continue
 
 
*
* FORM THE NEW ESTIMATES
*
         suma=max(suma,1.0e-20)
         dxmode=sumb/suma
         xmode=xmode+dxmode
         sig2=sumc/suma
 
*
* IF THE REQUIRED ACCURACY HAS BEEN MET, RETURN
*
         if(abs(dxmode).le.toll) go to 7
3     continue
 
7     sigma=sqrt(sig2)
 
      end
