C++**********************************************************************
C Program OPTIC
C To compute optic characteristics (geometrical optics with thin lenses)
C
C JLP
C Version 02-12-92
C--**********************************************************************
	program optic
        integer ioption,status
	real*4 focal_length, pp, pprime, distance, magni
        real*4 aa,bb,cc
C
C Choice of the option:
	print 67
67	format(' Program optic      ---- Version 02-12-92 ---')
	print 68
68	format(' Menu: ',/,
     1     ' 1: Compute image plane position P'' (knowing',
     1         ' P and focal length F)',/,
     1     ' 2: Compute P and P'' (knowing',
     1         ' working distance and focal length F)',/,
     1     ' 10: Exit',/,
     1     ' Enter your choice:')
        read(5,*,err=99,end=99) ioption

C*************************************************************************
C Option 1:  Compute magnification and P' knowing P and focal length
C*************************************************************************
        if(ioption.eq.1)then
	  print 41
41	  format(' Enter P and focal length F:')
	  read(5,*) pp,focal_length
          print 42 
42        format(/,' Basic relation is: ',
     1       '     1/P + 1/P'' = 1/F ')
          pprime = 1./(1./focal_length - 1./pp)
          magni = pprime/pp
          print 43,pp,pprime,magni
43        format(/,' P =',1pg12.5,' P'' =',1pg12.5,
     1            ' Magnification (P''/P)=',1pg11.4)
C*************************************************************************
C Option 2:  Compute magnification, P and P' knowing working distance
C            and focal length
C*************************************************************************
C Formulae:
C              1/pp + 1/pprime = 1/focal_length
C Thus:     focal_length = pp*pprime / (pp + pprime)
C
C But:             pp + pprime = distance
C Thus:     pprime = distance - pp
C           focal_length = (distance - pp) * pp / distance
C           pp*pp - pp * distance + focal_length * distance = 0
C*************************************************************************
        elseif(ioption.eq.2)then
          print 69
69        format(' Enter working distance D (between the two image planes)',
     1           'and focal length F:')
          read(5,*) distance,focal_length
            print 70
70          format(/,' Starting relations are :  ',/,
     1       '        1/P + 1/P'' = 1/F ',/,
     1       '        P + P'' = D',/, 
     1       ' We then infer that P is such that: (actually two solutions P and P'')',/, 
     1       '        P*P - P*D +  F*D = 0')
            aa = 1.
            bb = -1.*distance
            cc = focal_length * distance
	    call second_order(aa,bb,cc,pprime,pp,status)
            if(status.ne.0)then
              print 71
71            format(/,' Error: no solution to this problem'
     1               ' Reduce focal length or increase working distance') 
            else
              magni = pprime/pp
              print 72,pp,pprime,magni
72            format(/,' P =',1pg12.5,' P'' =',1pg12.5,
     1               ' Magnification (P''/P)=',1pg11.4)
            endif

C*************************************************************************
C Formulae:
C              1/pp + 1/pprime = 1/focal_length
C Thus:     focal_length = pp*pprime / (pp + pprime)
C
C But:             pp + pprime = distance
C Thus:     pprime = distance - pp
C           focal_length = (distance - pp) * pp / distance
C           pp*pp - pp * distance + focal_length * distance = 0
C*************************************************************************
         endif

99       stop
         end
C*************************************************************************
C Solve second order equation
C aa x2 + bb x + cc = 0
C Delta = bb*bb - 4*a*c
C*************************************************************************
	subroutine second_order(aa,bb,cc,s1,s2,status)
        integer status
	real*4 aa, bb, cc, s1, s2, delta

        status = 0

C Compute "determinant"
	delta = bb*bb - 4*aa*cc

	if(delta.lt.0)then
	  status = -1
        elseif(delta.eq.0)then
          s1 = - bb /(2*aa)
          s2 = s1 
        else
          s1 = (- bb + sqrt(delta))/(2*aa)
          s2 = (- bb - sqrt(delta))/(2*aa)
        endif

	return
        end
