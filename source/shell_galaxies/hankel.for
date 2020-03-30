C++********************************************************************
C Program HANKEL
C To smooth a radial profile using an arbitrary PSF. Uses
C Hankel transforms of the PSF and the surface brightness profile.
C From Timothy Bruce (Stromlo)
C Version of 22-09-86
C--********************************************************************
	PROGRAM HANKEL
	implicit real*8 (a-h,o-z)
 
	parameter (dim=200)
 
	dimension grd(dim), rpsf(dim), rsb(dim), psf(dim),
     $           hpsf(10*dim), sb(dim), hsba(10*dim),hsb(10*dim),
     $           sba(dim), xkk(10*dim)
 
	real*4 sbalog(dim), sblog(dim), rlog(dim), hpsfpl(10*dim),
     $         hsbapl(10*dim), hsbpl(10*dim), xkpl(10*dim)
 
	character ans*1
        open(unit=30,name='xsubrapp.dat',status='new')
	pi=3.14159265358979323846
	twopi = 2.0 * pi
c--------------------------------------------------------------------------
 
	print *, 'First evaluate the Hankel transforms of the PSF and the'
	print *, 'Surface Brightness'
 
 
	print *, 'Read in PSF from table'
 
        open(unit=10,name='normipc.dat',status='old')
        do 1 i=1,200
        read(10,*,end=111)rpsf(i),psf(i)
        write(6,*)i,rpsf(i),psf(i)
        npsf=i
1       continue
c    	call filin (rpsf, psf, npsf )
 
111     print *, 'Read in radial surface brightness profile from table'
 
        open(unit=11,name='tnlolo.dat',status='old')
        do 2 j=1,200
        read(11,*,end=112)rsb(j),sb(j)
        write(6,*)j,rsb(j),sb(j)
        nsb=j
2       continue
c	call filin ( rsb, sb, nsb )
 
112	print *, 'Is surface brightness data in log-log form?  (y/n)'
	accept 10, ans
10	format (a1)
	if (ans.eq.'n'.or.ans.eq.'N') goto 50
	print *, 'Converting surface brightness to linear form.'
	do 40 i = 1,nsb
	rsb(i) = 10.0**rsb(i)
	sb(i)  = 10.0**sb(i)
40	continue
50	continue
	
	print *, 'Largest increment in radius of surface brightness profile.'
	accept *, delr
	rmax = rsb(nsb)
 
c	Increment and Maximum value of radial wave number.
 
	delk = 1.0 /( 2.0 * rmax )
	xkmax = 2.0 / delr
	kmax = xkmax/delk + 4
 
c	Evaluate Hankel transforms of PSF and Surface Brightness.
 
	ifail = 0
 
	do 100  k = 1, kmax
 
	xk = (k-1) * delk
	xkpl(k) = xk
		do 90 i = 1, nsb
 
		r = rpsf(i)
		arg = xk * r

C Calling NAG routine S17AEF 
		grd(i) = r * psf(i) * s17aef (arg, ifail)
 
90		continue

C Calling NAG routine D01GAF:
		call d01gaf (rpsf, grd, npsf, res, er, ifail)
		hpsf(k) = res
		hpsfpl(k) = res
100 	continue
 
	do 200 k = 1, kmax
	xk = (k-1)*delk
		do 190 i=1,nsb
		r = rsb(i)
		arg = xk * r
C Calling NAG routine S17AEF 
		grd(i) = r * sb(i) * s17aef (arg, ifail)
190 	continue

C Calling NAG routine D01GAF:
        print *,'calling do1gaf(surf br)'
	call d01gaf (rsb, grd, nsb, res, er, ifail)
	hsb(k) = res
	hsbpl(k) = res
200	continue
 
	do 300 k = 1, kmax
	hsba(k) = twopi * hsb(k) * hpsf(k)
	hpsfpl(k) = hpsf(k)
300	continue
 
	call curve (kmax, xkpl, hpsfpl )
	call curve (kmax, xkpl, hsbpl )
	call gratek ( 'k', 'Hankel Transform', 'PSF and Surface Brightness',
     $	                   's,tickin')
 
	print *, 'Inverse Hankel Transform to determine Apparent Surface'
	print *, 'Brightness.'
 
	do 400 i = 1, nsb
		r = rsb(i)
		do 390 k = 1, kmax
			xk = (k-1) * delk
			xkk(k) = xk
			arg = xk * r
			grd(k) = xk * hsba(k) * s17aef (arg, ifail)
390			continue

C Calling NAG routine D01GAF:
                print *,'do1gaf(xkk,grd,kmax)'
		call d01gaf ( xkk, grd, kmax, res, er, ifail)
		sba(i) = res
		sbalog(i) = dlog10 ( sba(i) )
		sblog(i)  = dlog10 ( sb(i) )
       		if (rsb(i).gt.0.000001) then
 		rlog(i)   = dlog10 ( rsb(i) )
                 write(6,*)rlog(i),sbalog(i),sblog(i)
                 write(30,*)rlog(i),sbalog(i),sblog(i)
		else
		rlog(i) = -4.0
		end if
 
400	continue
 
	call curve ( nsb, rlog, sblog )
	call curve ( nsb, rlog, sbalog )
 
	call gratek ( 'log r', 'log SB',
     $                'Real and Apparent Surface Brightness',
     $			's,tickin' )
 
	end
 
