C************************************************************
C Reads data FITS file from A.J. Pickles of 4771 points from
C 1150 A to 10620 A
C
C File with spectral types and photometric data: "pickles_types_data.txt" 
C FITS data file: "pickles_library.fits"
C
C The last line (#132 is filled with wavelengths)
C
C Reading part taken from a program written by J.-F. Leborgne (version 
C of 29/06/99)
C
C JLP
C Version of 28/08/2006
C************************************************************
      program spect_pickles
      implicit none
      integer NPTS,NSPEC
      parameter (NPTS=4771,NSPEC=132)
      real*4 flux(NPTS,NSPEC),wavel(NPTS)
      real*4 oIII_filter(NPTS)
      real*4 b_filter(NPTS),v_filter(NPTS),r_filter(NPTS)
      real*4 rl_filter(NPTS),ir_filter(NPTS),ccd_sens(NPTS)
      character*6 jstar(NSPEC)
      character*60 filename
      integer jst,pnst,idim,iopt,jst1,jst2
      idim=NPTS
C*************************************************
C Read data file (pickles_library.fits)
      call jlp_read_pickles(wavel,flux,idim)

C Read the names (in pickles_types_data.txt):
      call jlp_read_types(jstar)

C Create PISCO filters
      call create_pisco_filters(wavel,idim,ccd_sens,b_filter,
     1                 v_filter,r_filter,rl_filter,ir_filter,
     1                 oIII_filter)

      write(6,11)
11    format(' Menu: ',/,
     1       '1. Extract a spectrum',/,
     1       '2. Compute B, V, R, indices of the whole catalog',/,
     1       '3. Binary simulation',/,
     1       '4. PISCO scale calibration',/,
     1       '10. Exit')
      read(5,*) iopt
C*************************************************
C Debug: select a specific spectrum
      if(iopt.eq.1)then
       write(6,12) 
12     format(' Number of spectrum (in Table 2, from 1 to 131):')
       read(5,*) jst1
       call extract_one_spectrum(wavel,flux,jstar,idim,jst1)

C*************************************************
C Compute all B,V,R indices
      else if(iopt.eq.2)then
        call color_indices_to_file(wavel,flux,jstar,idim,
     1        b_filter,v_filter,r_filter,rl_filter,ir_filter,ccd_sens)

C**************************************************************
C Binary simulation
      else if(iopt.eq.3)then
        write(6,*)' Binary simulation'
        write(6,13)
13      format('Binary simulation',/,
     1         ' Enter two star numbers (in Table 2, from 1 to 131)')
        read(5,*) jst1,jst2
        call jlp_composite(jst1,jst2,jstar,wavel,flux,idim,
     1        b_filter,v_filter,r_filter,rl_filter,ir_filter,ccd_sens)
C**************************************************************
C PISCO scale calibration 
C (with slit mask in Merate, 2006)
      else if(iopt.eq.4)then
       write(6,*)' Deneb A2I  (#120)'
C       write(6,14) 
14     format(' Target spectrum number (in Table 2, from 1 to 131):')
C       read(5,*) jst1
        jst1 = 120
       call pisco_scale_calib(jst1,wavel,flux,idim,
     1        b_filter,v_filter,r_filter,rl_filter,ir_filter,
     1        oIII_filter,ccd_sens)
      
      endif
      stop
      end
C************************************************************
C Simulate a composite spectrum star
C by adding two spectra
C 
C************************************************************
      subroutine jlp_composite(jst1,jst2,jstar,wavel,flux,idim,
     1        b_filter,v_filter,r_filter,rl_filter,ir_filter,ccd_sens)
      implicit none
      integer i,idim,jst1,jst2,pnst1,pnst2
      real*4 flux(idim,*),wavel(*)
      real*4 b_filter(idim),v_filter(idim),r_filter(idim)
      real*4 rl_filter(idim),ir_filter(idim),ccd_sens(idim)
      real*4 bb_sum1,bb_sum2,vv_sum1,vv_sum2,rr_sum1,rr_sum2,b_v1
      real*4 rl_sum1,rl_sum2,ir_sum1,ir_sum2
      real*4 vv_johnp1,vv_johnp2
      real*4 absol_v1,absol_v2,dm_absol,w1,w2,w3
      character filename*60, answer*80
      character*6 jstar(*)

C Look for library indices (pnst1, pnst2) 
      call find_library_index(jst1,pnst1)
      call find_library_index(jst2,pnst2)

      call pickles_v_absol(jst1,jstar,absol_v1)
      call pickles_v_absol(jst2,jstar,absol_v2)
      write(6,*)' Keep these M_v values?[CR] or give new values:'
      read(5,10) answer 
10    format(a)
      if(answer.ne.' ')then
       read(answer,*) absol_v1,absol_v2 
      endif
      dm_absol=absol_v2-absol_v1
      write(6,*)' Delta_m=',dm_absol
      call pickles_color_indices(wavel,flux,idim,pnst1,
     1                         bb_sum1,vv_johnp1,rr_sum1)
      call pickles_color_indices(wavel,flux,idim,pnst2,
     1                         bb_sum2,vv_johnp2,rr_sum2)
      write(6,*)' b_sum1',bb_sum1,'b_sum2',bb_sum2
      bb_sum1 = bb_sum1/vv_johnp1
      bb_sum2 = bb_sum2/vv_johnp2
      rr_sum1 = rr_sum1/vv_johnp1
      rr_sum2 = rr_sum2/vv_johnp2
      write(6,40)dm_absol-2.5*alog10(bb_sum2/bb_sum1),dm_absol,
     1           dm_absol-2.5*alog10(rr_sum2/rr_sum1)
40    format('Pickles''Johnson: b2-b1 = ',f5.2,' v2-v1= ',f5.2,
     1       ' r2-r1 (uncal.)=',f5.2)
      call pisco_color_indices(wavel,flux,idim,pnst1,
     1        b_filter,v_filter,r_filter,rl_filter,ir_filter,ccd_sens,
     1        bb_sum1,vv_sum1,rr_sum1,rl_sum1,ir_sum1)
      call pisco_color_indices(wavel,flux,idim,pnst2,
     1        b_filter,v_filter,r_filter,rl_filter,ir_filter,ccd_sens,
     1        bb_sum2,vv_sum2,rr_sum2,rl_sum2,ir_sum2)
C Normalization to vv_john, since I use M_v (absolute V magnitude)
      bb_sum1 = bb_sum1/vv_johnp1
      bb_sum2 = bb_sum2/vv_johnp2
      vv_sum1 = vv_sum1/vv_johnp1
      vv_sum2 = vv_sum2/vv_johnp2
      rr_sum1 = rr_sum1/vv_johnp1
      rr_sum2 = rr_sum2/vv_johnp2
      rl_sum1 = rl_sum1/vv_johnp1
      rl_sum2 = rl_sum2/vv_johnp2
      ir_sum1 = ir_sum1/vv_johnp1
      ir_sum2 = ir_sum2/vv_johnp2
      write(6,38)dm_absol-2.5*alog10(bb_sum2/bb_sum1),
     1           dm_absol-2.5*alog10(vv_sum2/vv_sum1),
     1           dm_absol-2.5*alog10(rr_sum2/rr_sum1),
     1           dm_absol-2.5*alog10(rl_sum2/rl_sum1),
     1           dm_absol-2.5*alog10(ir_sum2/ir_sum1)
38    format('Pisco: b2-b1 = ',f5.2,' v2-v1= ',f5.2,
     1 ' r2-r1= ',f5.2,' rl2-rl1= ',f5.2,' ir2-ir1= ',f5.2)
      call johnson_color_indices(wavel,flux,idim,pnst1,
     1                         bb_sum1,vv_sum1,rr_sum1)
      call johnson_color_indices(wavel,flux,idim,pnst2,
     1                         bb_sum2,vv_sum2,rr_sum2)
      bb_sum1 = bb_sum1/vv_sum1
      bb_sum2 = bb_sum2/vv_sum2
      write(6,39)dm_absol-2.5*alog10(bb_sum2/bb_sum1),dm_absol
39    format('Jaschek''s Johnson: b2-b1 = ',f5.2,' v2-v1= ',f5.2)
C**********************************************
      write(6,*) ' Output composite spectrum: '
      read(5,10)filename
      open(9,name=filename,status='unknown')
      do i=1,idim
        w1=(flux(i,pnst1)/vv_sum1)*10.**(-0.4*absol_v1)
        w2=(flux(i,pnst2)/vv_sum2)*10.**(-0.4*absol_v2)
        w3=w1+w2
C Store spectrum in column #132
        flux(i,132)=w3
        write(9,15) wavel(i),w1,w2,w3 
15      format(f6.1,1x,3(e12.5,1x))
      end do
      close(9)
C***********************************************
C Calibration:
      pnst1=132
      call johnson_color_indices(wavel,flux,idim,pnst1,
     1                         bb_sum1,vv_sum1,rr_sum1)
C Constant=0.80 (so that B-V=0 for A0V)
      b_v1=-2.5*alog10(bb_sum1/vv_sum1)+0.80
      write(6,*)' B-V (Jasheck''s Johnson) =',b_v1
      call pickles_color_indices(wavel,flux,idim,pnst1,
     1                         bb_sum1,vv_sum1,rr_sum1)
C Constant=0.70 (so that B-V=0 for A0V)
      b_v1=-2.5*alog10(bb_sum1/vv_sum1)+0.70
      write(6,*)' B-V (Pickles''s Johnson) =',b_v1
      return
      end
C************************************************************
C Read FITS data file: 'pickles_library.fits'
C Containing all spectra.
C************************************************************
      subroutine jlp_read_pickles(wavel,flux,idim)
      implicit none
      integer i,nx,ny,madrid(1),idim
      real*4 flux(idim,*),wavel(idim)
      character filename*60,comments*80
      common /vmr/madrid

      filename='pickles_library.fits'

      call jlp_begin
      call jlp_inquifmt
      call jlp_readimag(flux,nx,ny,idim,filename,comments)
C The last line is filled with wavelengths:
        do i=1,nx
          wavel(i) = flux(i,ny)
        end do
      write(6,*)' File pickles_library.fits successfully read'
      write(6,*)' nx=',nx,' ny=',ny
      return
      end
C************************************************************
C Read Bolometric magnitude in "pickles_types_data.txt" 
C************************************************************
      subroutine pickles_v_absol(jst,jstar,absol_v)
      implicit none
      real*4 m_bolo,bolo_correc,absol_v 
      integer jst,is_the_same,i
      character buffer*150,star_name*6
      character*6 jstar(*)
      open(11,name='pickles_types_data.txt',status='old')
C First 5 lines of comments
      do i=1,5
        read(11,10)buffer
10      format(a)
      end do
      do i=1,131
        read(11,10,end=14)buffer
        read(buffer,12) star_name,m_bolo,bolo_correc
12      format(t24,a6,t93,f5.2,t98,f5.2)
        if(jstar(jst).eq.star_name)then
          is_the_same=1
          goto 14
        endif
      end do
14    close(11)
      if(is_the_same.eq.1)then
        absol_v=m_bolo-bolo_correc
        write(6,29)jst,jstar(jst),star_name,m_bolo,
     1             bolo_correc,absol_v 
29      format(i3,' ',a6,' ',a6,' m_bol=',f6.2,' BC=',f6.2,
     1         ' M_v=',f6.2)
      else
        absol_v=-100
        write(6,*)jstar(jst),' Not found'
      endif
      return
      end
C************************************************************
C Compute PISCO filters 
C 
C OUTPUT:
C ccd_sens: CCD sensitivity
C b_filter, v_filter, r_filter, rl_filter, ir_filter: transmission curves
C************************************************************
      subroutine create_pisco_filters(wavel,idim,ccd_sens,b_filter,
     1                       v_filter,r_filter,rl_filter,ir_filter,
     1                       oIII_filter)
      implicit none
      integer idim,pnst,i,save_filters
      real*4 wavel(idim)
      real*4 max_trans,bandw,centralw,bw,lc
      real*4 oIII_filter(idim)
      real*4 b_filter(idim),v_filter(idim),r_filter(idim)
      real*4 rl_filter(idim),ir_filter(idim),ccd_sens(idim)
      character filename*60

C ICCD sensitivity:
      call compute_iccd_sens(ccd_sens,wavel,idim)

C B filter (Oriel 447/47...):
      max_trans=0.74
      centralw=4470.
      bandw=470.
      call compute_gauss(max_trans,bandw,centralw,wavel,b_filter,idim)

C V filter (Oriel 530/57...):
      max_trans=0.67
      centralw=5300.
      bandw=570.
      call compute_gauss(max_trans,bandw,centralw,wavel,v_filter,idim)

C R filter (Oriel 644/70...):
      max_trans=0.61
      centralw=6440.
      bandw=700.
      call compute_gauss(max_trans,bandw,centralw,wavel,r_filter,idim)

C RL filter (Oriel 743/69...):
      max_trans=0.68
      centralw=7430.
      bandw=690.
      call compute_gauss(max_trans,bandw,centralw,wavel,rl_filter,idim)

C IR filter (Oriel 855/74...):
      max_trans=0.66
      centralw=8550.
      bandw=740.
      call compute_gauss(max_trans,bandw,centralw,wavel,ir_filter,idim)

C OIII filter 
      max_trans=0.63
      centralw=5010.
      bandw=110.
      call compute_gauss(max_trans,bandw,centralw,wavel,oIII_filter,idim)

C Save to output files:
      save_filters=1
      if(save_filters.eq.1)then
      write(6,21) 
21    format('Name of Pisco B,V,R filters and ICCD',
     1       ' sensitivity output file : ')
      read(5,22) filename 
22    format(A)
      open(9,name=filename,status='unknown')
      write(9,13)
13    format('# wavel, (Pisco) B* CCD, V*CCD, R*CCD, RL*CCD,',
     1        ' IR*CCD, CCD sensitivity, OIII*CCD')
      write(6,*)' New filter characteristics: (Central and bwidth)'
      call new_filterchar(wavel,b_filter,ccd_sens,lc,bw,idim)
      write(6,14)'B   ',nint(lc),nint(bw)
      write(9,14)'B   ',nint(lc),nint(bw)
14    format('# Filter ',A,' Cent. wavel:',I5,' Bandwidth:',I4)
      call new_filterchar(wavel,v_filter,ccd_sens,lc,bw,idim)
      write(6,14)'V   ',nint(lc),nint(bw)
      write(9,14)'V   ',nint(lc),nint(bw)
      call new_filterchar(wavel,r_filter,ccd_sens,lc,bw,idim)
      write(6,14)'R   ',nint(lc),nint(bw)
      write(9,14)'R   ',nint(lc),nint(bw)
      call new_filterchar(wavel,rl_filter,ccd_sens,lc,bw,idim)
      write(6,14)'RL  ',nint(lc),nint(bw)
      write(9,14)'RL  ',nint(lc),nint(bw)
      call new_filterchar(wavel,ir_filter,ccd_sens,lc,bw,idim)
      write(6,14)'IR  ',nint(lc),nint(bw)
      write(9,14)'IR  ',nint(lc),nint(bw)
      call new_filterchar(wavel,oIII_filter,ccd_sens,lc,bw,idim)
      write(6,14)'OIII',nint(lc),nint(bw)
      write(9,14)'OIII',nint(lc),nint(bw)
      do i=1,idim
       write(9,16) wavel(i),b_filter(i)*ccd_sens(i),
     1    v_filter(i)*ccd_sens(i),r_filter(i)*ccd_sens(i),
     1    rl_filter(i)*ccd_sens(i),ir_filter(i)*ccd_sens(i),ccd_sens(i),
     1    oIII_filter(i)*ccd_sens(i)
16     format(f12.5,7(1x,e12.5))
      end do
      close(9)
      endif

      end
C************************************************************
C Compute PISCO color indices of a star 
C
C INPUT:
C pnst: index of the star in flux array
C flux(*,pnst): irradiance distribution of the star 
C walel(*): array filled with wavelengths
C ccd_sens: CCD sensitivity
C b_filter, v_filter, r_filter, rl_filter, ir_filter: transmission curves
C                                                     of PISCO filters
C
C OUTPUT:
C bb_sum, vv_sum, rr_sum, rl_sum, ir_sum: color indices of the star 
C************************************************************
      subroutine pisco_color_indices(wavel,flux,idim,pnst,
     1        b_filter,v_filter,r_filter,rl_filter,ir_filter,ccd_sens,
     1        bb_sum,vv_sum,rr_sum,rl_sum,ir_sum)
      implicit none
      integer idim,pnst,i
      real*4 flux(idim,*),wavel(idim)
      real*4 bb_sum,vv_sum,rr_sum,rl_sum,ir_sum
      real*4 max_trans,bandw,centralw,bw,lc
      real*4 b_filter(idim),v_filter(idim),r_filter(idim)
      real*4 rl_filter(idim),ir_filter(idim),ccd_sens(idim)
      real*8 b_sum, v_sum, r_sum, l_sum, i_sum

C B filter (Oriel 447/47...):
      b_sum = 0.
      do i=1,idim
         b_sum = b_sum + b_filter(i) * flux(i,pnst) * ccd_sens(i) 
      end do

C V filter (Oriel 530/57...):
      v_sum = 0.
      do i=1,idim
         v_sum = v_sum + v_filter(i) * flux(i,pnst) * ccd_sens(i) 
      end do

C R filter (Oriel 644/70...):
      r_sum = 0.
      do i=1,idim
         r_sum = r_sum + r_filter(i) * flux(i,pnst) * ccd_sens(i) 
      end do

C RL filter (Oriel 743/69...):
      l_sum = 0.
      do i=1,idim
         l_sum = l_sum + rl_filter(i) * flux(i,pnst) * ccd_sens(i) 
      end do

C IR filter (Oriel 855/74...):
      i_sum = 0.
      do i=1,idim
         i_sum = i_sum + ir_filter(i) * flux(i,pnst) * ccd_sens(i) 
      end do

C Conversion to single precision:
      bb_sum = b_sum 
      vv_sum = v_sum 
      rr_sum = r_sum 
      rl_sum = l_sum
      ir_sum = i_sum

      return
      end
C************************************************************
C Compute ICCD sensitivity from file "iccd.dat" 
C************************************************************
      subroutine compute_iccd_sens(ccd_sens,wavel,idim)
      implicit none
      integer idim,i,iw,i_low
      real*4 ccd_sens(idim),wavel(idim)
      real*4 ww,w_low,lower,higher
C First fill array with zeroes outside of the intervall [3800,9200]
C and with -1 inside:
      do i=1,idim
      ccd_sens(i)=0.
      if(wavel(i).gt.3900.and.wavel(i).lt.9200) ccd_sens(i)=-1
      end do
C Read data from file:
      open(11,name="iccd.dat",status='old')
      do i=1,idim
        read(11,*,end=331) iw,ww
        i_low = (float(iw) - 1150.) / 5. + 1
        ccd_sens(i_low)=ww
      end do
331   close(11)
C      write(6,*)' File iccd.dat read i=',i 
C Now proceed to interpolation:
      do i=1,idim
           if(ccd_sens(i).eq.-1.)then
              w_low = int(wavel(i)/100.) * 100.
              i_low = (w_low - 1150.) / 5. + 1
              lower=ccd_sens(i_low)
              higher=ccd_sens(i_low+20)
              ccd_sens(i)=lower + (higher - lower)*float(i-i_low)/20.
           endif
      enddo
      return
      end
C************************************************************
C Compute Johnson's color indices 
C************************************************************
      subroutine johnson_color_indices(wavel,flux,idim,pnst,
     1                                 bb_sum,vv_sum,rr_sum)
      implicit none
      integer idim,pnst,i
      real*4 flux(idim,*),wavel(idim)
      real*4 bb_sum,vv_sum,rr_sum
      real*4 max_trans,bandw,centralw,bj_filter(idim),vj_filter(idim)
      real*8 b_sum, v_sum
      character filename*60
C B filter (Johnson's)
      call compute_b_johnson(wavel,bj_filter,idim)
      b_sum = 0.
      do i=1,idim
         b_sum = b_sum + bj_filter(i) * flux(i,pnst) 
C         if(wavel(i).lt.4000)write(6,*) 
C     1          'bj_filter=',bj_filter(i),'flux',flux(i,pnst),'b_sum =',b_sum
      end do
C V filter (Johnson's)
      call compute_v_johnson(wavel,vj_filter,idim)
      v_sum = 0.
      do i=1,idim
         v_sum = v_sum + vj_filter(i) * flux(i,pnst) 
      end do
      bb_sum = b_sum 
      vv_sum = v_sum 
C Debug:
      b_sum=100
      if(b_sum.ne.-100)goto 333
      write(6,21) 
21    format(' Name of B, V filter output file : ')
      read(5,22) filename 
22    format(A)
      open(9,name=filename,status='unknown')
      do i=1,idim
       write(9,16) wavel(i),bj_filter(i),vj_filter(i)
16     format(f12.5,e12.5,1X,e12.5)
      end do
      close(9)
333   return
      end
C************************************************************
C Compute Johnson's color indices with Pickles's table#7 
C************************************************************
      subroutine pickles_color_indices(wavel,flux,idim,pnst,
     1                                 bb_sum,vv_sum,rr_sum)
      implicit none
      integer idim,pnst,i
      real*4 flux(idim,*),wavel(idim)
      real*4 bb_sum,vv_sum,rr_sum
      real*4 max_trans,bandw,centralw
      real*4 bp_filter(idim),vp_filter(idim),rp_filter(idim)
      real*8 b_sum,v_sum,r_sum
      character filename*60
C B filter (Johnson's, as quoted by Pickles)
      call compute_b_pickles(wavel,bp_filter,idim)
      b_sum = 0.
      do i=1,idim
         b_sum = b_sum + bp_filter(i) * flux(i,pnst) 
C         if(wavel(i).lt.4000)write(6,*) 
C     1          'bp_filter=',bp_filter(i),'flux',flux(i,pnst),'b_sum =',b_sum
      end do
C V filter (Johnson's, as quoted by Pickles)
      call compute_v_pickles(wavel,vp_filter,idim)
      v_sum = 0.
      do i=1,idim
         v_sum = v_sum + vp_filter(i) * flux(i,pnst) 
      end do
C R filter (Johnson's, as quoted by Pickles)
      call compute_r_pickles(wavel,rp_filter,idim)
      r_sum = 0.
      do i=1,idim
         r_sum = r_sum + rp_filter(i) * flux(i,pnst) 
      end do
      bb_sum = b_sum 
      vv_sum = v_sum 
      rr_sum = r_sum 
C Debug:
      b_sum=100
      if(b_sum.ne.-100)goto 333
      write(6,21) 
21    format(' Name of B, V, R Johnson (Pickles) filter output file : ')
      read(5,22) filename 
22    format(A)
      open(9,name=filename,status='unknown')
      do i=1,idim
       write(9,16) wavel(i),bp_filter(i),vp_filter(i),rp_filter(i)
16     format(f12.5,e12.5,3(1x,e12.5))
      end do
      close(9)
333   return
      end
C************************************************************
C Compute B (V and R) filter(s) 
C************************************************************
      subroutine compute_gauss(max_trans,bandw,centralw,wavel,
     1                         b_filter,idim)
      implicit none
      integer idim,i
      real*4 max_trans,bandw,centralw,wavel(idim),b_filter(idim)
      real*4 work,c1
C When x is half bandwith:  exp- x^2/sigma^2 = 0.5
C Hence                    - x^2/sigma^2 = ln 0.5 = - ln 2
C And                      x^2 = ln 2 * sigma^2 = 0.6931 * sigma^2
C and                      (bandw/2)^2 = ln 2 * sigma^2
C c1 = bandw**2 / 2.773 
      c1 = bandw * bandw / ( 4. * log(2.))
      do i=1,idim
        work = - (wavel(i) - centralw) * (wavel(i) - centralw) / c1
        b_filter(i) = max_trans * exp(work)
      end do
      return
      end
C*********************************************************************
C B3 Filter
C From Table#7 of Pickles'paper
C*********************************************************************
      subroutine compute_b_pickles(wavel,bp_filter,idim)
      implicit none
      integer i_low,i_high,idim,i
      real*4 wavel(idim),bp_filter(idim)
      real*4 w_low,lower,higher,t1,t2,t3
      integer iw1,iw2,iw3
      character buffer*120

      open(11,name='pickles_filters.txt',status='old')
C First four lines with comments:
      do i=1,4
      read(11,10) buffer
10    format(a)
      end do
C First go, load reduced table:
      do i=1,idim
       if(wavel(i).gt.3600.and.wavel(i).lt.5550)then
         bp_filter(i)=-1.
       else 
         bp_filter(i)=0.
       endif
      end do
C Then 66 lines with data (B3 filter):
      do i=1,66
       read(11,10)buffer
       read(buffer,15) iw1,t1,iw2,t2,iw3,t3
15     format(i4,t6,f5.3,t12,i4,t17,f5.3,t23,i4,t28,f5.3)
       i_low = (float(iw3) - 1150.) / 5. + 1
       bp_filter(i_low)=t3
      end do
      close(11)
      do i=1,idim
           if(bp_filter(i).eq.-1.)then
              w_low = int(wavel(i)/50.) * 50.
              i_low = (w_low - 1150.) / 5. + 1
              lower=bp_filter(i_low)
              higher=bp_filter(i_low+10)
              bp_filter(i)=lower + (higher - lower)*float(i-i_low)/10.
           endif
      enddo
      return
      end
C*********************************************************************
C V Filter
C From Table#7 of Pickles'paper
C*********************************************************************
      subroutine compute_v_pickles(wavel,vp_filter,idim)
      implicit none
      integer i_low,i_high,idim,i
      real*4 wavel(idim),vp_filter(idim)
      real*4 w_low,lower,higher,t1,t2,t3,t4
      integer iw1,iw2,iw3,iw4
      character buffer*120

      open(11,name='pickles_filters.txt',status='old')
C First four lines with comments:
      do i=1,4
        read(11,10) buffer
10      format(a)
      end do
C First go, load reduced table:
      do i=1,idim
       if(wavel(i).gt.4750.and.wavel(i).lt.7400)then
         vp_filter(i)=-1.
       else 
         vp_filter(i)=0.
       endif
      end do
C Then 66 lines with data (B3 filter):
      do i=1,66
       read(11,10)buffer
       read(buffer,15) iw1,t1,iw2,t2,iw3,t3,iw4,t4
15     format(i4,t6,f5.3,t12,i4,t17,f5.3,t23,i4,t28,f5.3,t34,i4,t39,f5.3)
       i_low = (float(iw4) - 1150.) / 5. + 1
       vp_filter(i_low)=t4
C       write(6,*)' w4',iw4,' wavel()',wavel(i_low),' t4=',t4
      end do
      close(11)
      do i=1,idim
           if(vp_filter(i).eq.-1.)then
              w_low = int(wavel(i)/50.) * 50.
              i_low = (w_low - 1150.) / 5. + 1
              lower=vp_filter(i_low)
              higher=vp_filter(i_low+10)
              vp_filter(i)=lower + (higher - lower)*float(i-i_low)/10.
           endif
      enddo
      return
      end
C*********************************************************************
C R Filter
C From Table#7 of Pickles'paper
C*********************************************************************
      subroutine compute_r_pickles(wavel,rp_filter,idim)
      implicit none
      integer i_low,i_high,idim
      real*4 wavel(idim),rp_filter(idim)
      real*4 w_low,lower,higher,t1,t2,t3,t4,t5
      integer iw1,iw2,iw3,iw4,iw5,i
      character buffer*120

      open(11,name='pickles_filters.txt',status='old')
C First four lines with comments:
      do i=1,4
        read(11,10) buffer
10      format(a)
      end do
C First go, load reduced table:
      do i=1,idim
       if(wavel(i).gt.5500.and.wavel(i).lt.8750)then
         rp_filter(i)=-1.
       else 
         rp_filter(i)=0.
       endif
      end do
C Then 66 lines with data (R filter):
      do i=1,66
       read(11,10)buffer
       read(buffer,15) iw1,t1,iw2,t2,iw3,t3,iw4,t4,iw5,t5
15     format(i4,t6,f5.3,t12,i4,t17,f5.3,t23,i4,t28,f5.3,t34,i4,
     1        t39,f5.3,t45,i4,t50,f5.3)
       i_low = (float(iw5) - 1150.) / 5. + 1
       rp_filter(i_low)=t5
C       write(6,*)' w5',iw5,' wavel()',wavel(i_low),' t5=',t5
      end do
      close(11)
      do i=1,idim
           if(rp_filter(i).eq.-1.)then
              w_low = int(wavel(i)/50.) * 50.
              i_low = (w_low - 1150.) / 5. + 1
              lower=rp_filter(i_low)
              higher=rp_filter(i_low+10)
              rp_filter(i)=lower + (higher - lower)*float(i-i_low)/10.
           endif
      enddo
      return
      end
C*********************************************************************
C From The Classification of stars (Carlos and Mercedes Jaschek, 1987,
C Cambridge Univ. Press)
C*********************************************************************
      subroutine compute_b_johnson(wavel,bj_filter,idim)
      implicit none
      integer i_low,i_high,idim,i
      real*4 wavel(idim),bj_filter(idim)
      real*4 w_low,lower,higher
C First go, load reduced table:
      do i=1,idim
       if(wavel(i).gt.3500.and.wavel(i).lt.5600)then
         bj_filter(i)=-1.
       else 
         bj_filter(i)=0.
       endif
      end do
      do i=1,idim
       if(wavel(i).eq.3500) bj_filter(i)=0.000
       if(wavel(i).eq.3600) bj_filter(i)=0.006
       if(wavel(i).eq.3700) bj_filter(i)=0.080
       if(wavel(i).eq.3800) bj_filter(i)=0.337
       if(wavel(i).eq.3900) bj_filter(i)=1.425
       if(wavel(i).eq.4000) bj_filter(i)=2.253
       if(wavel(i).eq.4100) bj_filter(i)=2.806
       if(wavel(i).eq.4200) bj_filter(i)=2.950
       if(wavel(i).eq.4300) bj_filter(i)=3.000
       if(wavel(i).eq.4400) bj_filter(i)=2.937
       if(wavel(i).eq.4500) bj_filter(i)=2.780
       if(wavel(i).eq.4600) bj_filter(i)=2.520
       if(wavel(i).eq.4700) bj_filter(i)=2.230
       if(wavel(i).eq.4800) bj_filter(i)=1.881
       if(wavel(i).eq.4900) bj_filter(i)=1.550
       if(wavel(i).eq.5000) bj_filter(i)=1.275
       if(wavel(i).eq.5100) bj_filter(i)=0.975
       if(wavel(i).eq.5200) bj_filter(i)=0.695
       if(wavel(i).eq.5300) bj_filter(i)=0.430
       if(wavel(i).eq.5400) bj_filter(i)=0.210
       if(wavel(i).eq.5500) bj_filter(i)=0.055
       if(wavel(i).eq.5600) bj_filter(i)=0.000
      end do
      do i=1,idim
           if(bj_filter(i).eq.-1.)then
              w_low = int(wavel(i)/100.) * 100.
              i_low = (w_low - 1150.) / 5. + 1
              lower=bj_filter(i_low)
              higher=bj_filter(i_low+20)
              bj_filter(i)=lower + (higher - lower)*float(i-i_low)/20.
           endif
      enddo
      return
      end
C*********************************************************************
C From The Classification of stars (Carlos and Mercedes Jaschek, 1987,
C Cambridge Univ. Press)
C*********************************************************************
      subroutine compute_v_johnson(wavel,vj_filter,idim)
      implicit none
      integer idim,i_low,i_high,i
      real*4 wavel(idim),vj_filter(idim)
      real*4 w_low,lower,higher
C First go, load reduced table:
      do i=1,idim
       if(wavel(i).gt.4700.and.wavel(i).lt.7300)then
         vj_filter(i)=-1.
       else 
         vj_filter(i)=0.
       endif
      end do
      do i=1,idim
       if(wavel(i).eq.4700) vj_filter(i)=0.000
       if(wavel(i).eq.4800) vj_filter(i)=0.020
       if(wavel(i).eq.4900) vj_filter(i)=0.175
       if(wavel(i).eq.5000) vj_filter(i)=0.900
       if(wavel(i).eq.5100) vj_filter(i)=1.880
       if(wavel(i).eq.5200) vj_filter(i)=2.512
       if(wavel(i).eq.5300) vj_filter(i)=2.850
       if(wavel(i).eq.5400) vj_filter(i)=2.820
       if(wavel(i).eq.5500) vj_filter(i)=2.625
       if(wavel(i).eq.5600) vj_filter(i)=2.370
       if(wavel(i).eq.5700) vj_filter(i)=2.050
       if(wavel(i).eq.5800) vj_filter(i)=1.720
       if(wavel(i).eq.5900) vj_filter(i)=1.413
       if(wavel(i).eq.6000) vj_filter(i)=1.068
       if(wavel(i).eq.6100) vj_filter(i)=0.795
       if(wavel(i).eq.6200) vj_filter(i)=0.567
       if(wavel(i).eq.6300) vj_filter(i)=0.387
       if(wavel(i).eq.6400) vj_filter(i)=0.250
       if(wavel(i).eq.6500) vj_filter(i)=0.160
       if(wavel(i).eq.6600) vj_filter(i)=0.110
       if(wavel(i).eq.6700) vj_filter(i)=0.081
       if(wavel(i).eq.6800) vj_filter(i)=0.061
       if(wavel(i).eq.6900) vj_filter(i)=0.045
       if(wavel(i).eq.7000) vj_filter(i)=0.028
       if(wavel(i).eq.7100) vj_filter(i)=0.017
       if(wavel(i).eq.7200) vj_filter(i)=0.007
       if(wavel(i).eq.7300) vj_filter(i)=0.000
      end do
      do i=1,idim
           if(vj_filter(i).eq.-1.)then
              w_low = int(wavel(i)/100.) * 100.
              i_low = (w_low - 1150.) / 5. + 1
              lower=vj_filter(i_low)
              higher=vj_filter(i_low+20)
              vj_filter(i)=lower + (higher - lower)*float(i-i_low)/20.
           endif
      enddo
      return
      end
C************************************************************
C Read spectral types from file "pickles_types_data.txt" 
C************************************************************
      subroutine jlp_read_types(jstar)
      implicit none
      integer i
      character filename*60, buffer*120
      character*6 jstar(*)

      open(11,name='pickles_types_data.txt',status='old')
C First 5 lines of comments
      do i=1,5
        read(11,10)buffer
10      format(a)
      end do
      do i=1,131
        read(11,10,end=14)buffer
        read(buffer,12) jstar(i) 
12      format(t24,a6)
      end do
14    close(11)

      return
      end

C********************************************************
C JLP2002: pickles_types.jlp with "pnst, spectral type"
C********************************************************
      subroutine find_library_index(jst,pnst)
      implicit none
      character name*8,filename*20
      integer i,j,jst,pnst
      filename='pickles_types.jlp'
      open(9,file=filename,status='old')
      pnst=0
      do i=1,131
        read(9,*) j,name 
        if(j.eq.jst)then
         pnst=i
         goto 15
        endif
      enddo
12    format(I3,' ',A8)
15    close(9)
      if(pnst.eq.0)then
        write(6,*)'find_library_index/Fatal error: entry not found'
     1            ' in ""pickles_types.jlp""'
        stop
      else
        write(6,28) jst,name,pnst
28      format('In pickles_types.jlp: jst=',I5,' name=',A8,
     1         ' pnst=',I3)
      endif
      return
      end
C******************************************************
C To compute color indices of the whole catalogue
C******************************************************
      subroutine color_indices_to_file(wavel,flux,jstar,idim,
     1        b_filter,v_filter,r_filter,rl_filter,ir_filter,ccd_sens)
      implicit none
      integer jst,pnst,idim
      real*4 flux(idim,*),wavel(*)
      real*4 b_filter(idim),v_filter(idim),r_filter(idim)
      real*4 rl_filter(idim),ir_filter(idim),ccd_sens(idim)
      real*4 bb_sum,vv_sum,rr_sum,rl_sum,ir_sum
      real*4 absol_v,b_v_john,b_v_pisco,b_v_pickles
      character*6 jstar(*)
      character filename*20

      filename='spect_b_v.dat'
      open(9,name=filename,status='unknown')
      write(9,*) ' Name  B-V(Johnson) B-V(Pisco) B-V(Pickles) M_v'
      do jst=1,129
        call find_library_index(jst,pnst)
        call pisco_color_indices(wavel,flux,idim,pnst,
     1        b_filter,v_filter,r_filter,rl_filter,ir_filter,ccd_sens,
     1        bb_sum,vv_sum,rr_sum,rl_sum,ir_sum)
        b_v_pisco = -2.5 * alog10(bb_sum/vv_sum)
        call johnson_color_indices(wavel,flux,idim,pnst,
     1                           bb_sum,vv_sum,rr_sum)
C Constant=0.80 (so that B-V=0 for A0V)
        b_v_john = -2.5 * alog10(bb_sum/vv_sum) + 0.80
        call pickles_color_indices(wavel,flux,idim,pnst,
     1                           bb_sum,vv_sum,rr_sum)
C Constant=0.70 (so that B-V=0 for A0V)
        b_v_pickles = -2.5 * alog10(bb_sum/vv_sum)+0.70
        call pickles_v_absol(jst,jstar,absol_v)
        write(9,36) jst,jstar(jst),b_v_john,b_v_pisco,b_v_pickles,
     1              absol_v
36      format(i3,1x,a6,4(3x,f5.2))
      end do
      close(9)
      return
      end
C******************************************************
C To extract a spectrum 
C******************************************************
      subroutine extract_one_spectrum(wavel,flux,jstar,idim,jst)
      implicit none
      integer i,idim,jst,pnst
      real*4 flux(idim,*),wavel(*)
      character*6 jstar(*)
      character filename*20

C Look for library index (pnst) 
      call find_library_index(jst,pnst)
      write(6,28) jst,jstar(jst),pnst
28    format(' Name of #',I5,' ',A6,' Library index: ',I3)
C*************************************************
C Transfer to output file:
      write(6,21) 
21    format(' Name of output file : ')
      read(5,22) filename 
22    format(A)
      open(9,name=filename,status='unknown')
      do i=1,idim
       write(9,16) wavel(i),flux(i,pnst)
16     format(f12.5,e12.5)
      end do
      close(9)
      return
      end
C******************************************************************
C Compute new filter characteristics, after multiplication
C with ICCD response
C******************************************************************
      subroutine new_filterchar(wavel,filter,ccd_sens,lcent,bwidth,idim)
      implicit none
      integer idim
      real*4 wavel(idim),filter(idim),ccd_sens(idim),lcent,bwidth
      real*8 sum,sumsq,sumw,ww,sigma2,maxi
      integer i,imaxi
      sum = 0.
      sumsq = 0.
      sumw = 0.
      imaxi=1
      maxi=0.
      do i=1,idim
        ww = ccd_sens(i)*filter(i)
        if(ww.gt.maxi)then
          maxi = ww
          imaxi = i
        endif
        sum = sum + wavel(i)*ww
        sumsq = sumsq + wavel(i)*wavel(i)*ww
        sumw = sumw + ww 
      end do
      write(6,*) ' maxi=',maxi,' imaxi=',imaxi,
     1           ' at lambda=',wavel(imaxi)
      lcent = sum/sumw
C Gaussian with statistics:
C When x is half bandwith:  exp- x^2/ (2 sigma^2) = 0.5
C Hence                    - x^2/(2 sigma^2) = ln 0.5 = - ln 2
C And                      x^2 = ln 2 * 2 * sigma^2
C and                      (bandw/2)^2 = ln 2 * 2 * sigma^2
C BW^2 = 8 ln 2 * sigma^2 = 2 * 2.773 sigma^2 

      sigma2 = (sumsq/sumw) - lcent*lcent
      bwidth = sqrt(2. * 2.773 * sigma2)
      return
      end
C************************************************************
C Routine used for PISCO scale calibration 
C Computes the effective wavelength of PISCO filters
C taking into account the star energy distribution 
C 
C OUTPUT:
C b_lc, v_lc, r_lc, rl_lc, ir_lc, oIII_lc : central wavelengths
C                                          
C************************************************************
      subroutine pisco_filters_and_star(jst,wavel,flux,idim,
     1      b_filter,v_filter,r_filter,rl_filter,ir_filter,
     1      oIII_filter,ccd_sens,b_lc,v_lc,r_lc,rl_lc,ir_lc,oIII_lc,
     1      b_lc_err,v_lc_err,r_lc_err,rl_lc_err,ir_lc_err,oIII_lc_err)
      implicit none
      integer i,idim,jst,pnst
      real*4 flux(idim,*),wavel(*)
      real*4 oIII_filter(idim)
      real*4 b_filter(idim),v_filter(idim),r_filter(idim)
      real*4 rl_filter(idim),ir_filter(idim),ccd_sens(idim)
      real*4 filter(idim),lc,bw
      real*4 b_lc,v_lc,r_lc,rl_lc,ir_lc,oIII_lc
      real*4 b_lc_err,v_lc_err,r_lc_err,rl_lc_err,ir_lc_err,oIII_lc_err
      character filename*60, answer*80

C Look for library index (pnst) 
      call find_library_index(jst,pnst)

C*************************************************
C Save to output file:
      write(6,21) 
21    format(' Name of output file: ')
      read(5,22) filename 
22    format(A)
      open(9,name=filename,status='unknown')
      write(6,*)' New PISCO filter characteristics: (Central and bwidth)'
      write(6,*)' when taking into account the star energy distribution'

C      write(6,*)' DDDDDDDEBUG: FLUX SET TO ONE!'
C      do i=1,idim
C        flux(i,pnst) = 1.0 
C      enddo

C Blue
      do i=1,idim
        filter(i) = b_filter(i) * flux(i,pnst) 
      enddo
      call new_filterchar(wavel,filter,ccd_sens,lc,bw,idim)
      write(6,14)'B   ',nint(lc),nint(bw)
      write(9,14)'B   ',nint(lc),nint(bw)
14    format('# Filter ',A,' Cent. wavel:',I5,' Bandwidth:',I4)
      b_lc = lc
C Estimated error on central lambda (in angstroms):
      b_lc_err = 20. 

C Visible
      do i=1,idim
        filter(i) = v_filter(i) * flux(i,pnst) 
      enddo
      call new_filterchar(wavel,filter,ccd_sens,lc,bw,idim)
      write(6,14)'V   ',nint(lc),nint(bw)
      write(9,14)'V   ',nint(lc),nint(bw)
      v_lc = lc
C Estimated error on central lambda (in angstroms):
      v_lc_err = 20. 

C Red 
      do i=1,idim
        filter(i) = r_filter(i) * flux(i,pnst) 
      enddo
      call new_filterchar(wavel,filter,ccd_sens,lc,bw,idim)
      write(6,14)'R   ',nint(lc),nint(bw)
      write(9,14)'R   ',nint(lc),nint(bw)
      r_lc = lc
C Estimated error on central lambda (in angstroms):
      r_lc_err = 20. 

C RL Far red 
      do i=1,idim
        filter(i) = rl_filter(i) * flux(i,pnst) 
      enddo
      call new_filterchar(wavel,filter,ccd_sens,lc,bw,idim)
      write(6,14)'RL  ',nint(lc),nint(bw)
      write(9,14)'RL  ',nint(lc),nint(bw)
      rl_lc = lc
C Estimated error on central lambda (in angstroms):
      rl_lc_err = 20. 

C IR Infrared
      do i=1,idim
        filter(i) = ir_filter(i) * flux(i,pnst) 
      enddo
      call new_filterchar(wavel,filter,ccd_sens,lc,bw,idim)
      write(6,14)'IR  ',nint(lc),nint(bw)
      write(9,14)'IR  ',nint(lc),nint(bw)
      ir_lc = lc
C Estimated error on central lambda (in angstroms):
      ir_lc_err = 20. 

C OIII 
      do i=1,idim
        filter(i) = oIII_filter(i) * flux(i,pnst) 
      enddo
      call new_filterchar(wavel,filter,ccd_sens,lc,bw,idim)
      write(6,14)'OIII',nint(lc),nint(bw)
      write(9,14)'OIII',nint(lc),nint(bw)
      oIII_lc = lc
C Estimated error on central lambda (in angstroms):
      oIII_lc_err = 5.

      close(9)
      return
      end
C************************************************************
C Routine used for PISCO scale calibration
C Computes the scale for 10 and 20 mm eyepieces
C from observations with the slit mask in Merate, 
C taking into account the star energy distribution.
C
C************************************************************
      subroutine pisco_scale_calib(jst,wavel,flux,idim,
     1        b_filter,v_filter,r_filter,rl_filter,ir_filter,
     1        oIII_filter,ccd_sens)
      implicit none
      integer i,idim,jst
      real*4 flux(idim,*),wavel(*)
      real*4 oIII_filter(idim)
      real*4 b_filter(idim),v_filter(idim),r_filter(idim)
      real*4 rl_filter(idim),ir_filter(idim),ccd_sens(idim)
      real*4 b_lc,v_lc,r_lc,rl_lc,ir_lc,oIII_lc
      real*4 b_lc_err,v_lc_err,r_lc_err,rl_lc_err,ir_lc_err,oIII_lc_err
      real*4 scale_10(4),Dscale_10(4),scale_20(4),Dscale_20(4)
      real*4 mean_scale_10,Dmean_scale_10,mean_scale_20,Dmean_scale_20
      real*4 sigma_mean_10,sigma_mean_20
      real*4 rho_10(4),Drho_10(4),rho_20(4),drho_20(4)
      real*4 lc_10(4),Dlc_10(4),lc_20(4),Dlc_20(4)
      real*8 sumweights,sumsquares
      character filename*60, answer*80
      character*4 filter_10(4),filter_20(4)

C Compute the central wavelengths b_lc, v_lc, ...
C of PISCO B, V, ... filters,
C taking the star energy distribution into account:
       call pisco_filters_and_star(jst,wavel,flux,idim,
     1      b_filter,v_filter,r_filter,rl_filter,ir_filter,
     1      oIII_filter,ccd_sens,b_lc,v_lc,r_lc,rl_lc,ir_lc,oIII_lc,
     1      b_lc_err,v_lc_err,r_lc_err,rl_lc_err,ir_lc_err,oIII_lc_err)

C Load measured values (from observations made in november 2005
C that were processed in december 2005): 
       filter_10(1) = 'B   '
       rho_10(1) = 33.08
       Drho_10(1) = 0.12 
       lc_10(1) = b_lc
       Dlc_10(1) = b_lc_err

       filter_10(2) = 'V   '
       rho_10(2) = 39.02
       Drho_10(2) = 0.14 
       lc_10(2) = v_lc
       Dlc_10(2) = v_lc_err

       filter_10(3) = 'R   '
       rho_10(3) = 46.76 
       Drho_10(3) = 0.28 
       lc_10(3) = r_lc
       Dlc_10(3) = r_lc_err

       filter_10(4) = 'OIII'
       rho_10(4) = 36.60
       Drho_10(4) = 0.21 
       lc_10(4) = oIII_lc
       Dlc_10(4) = oIII_lc_err

       filter_20(1) = 'B   '
       rho_20(1) = 14.11
       Drho_20(1) = 0.1
       lc_20(1) = b_lc
       Dlc_20(1) = b_lc_err

       filter_20(2) = 'V   '
       rho_20(2) = 16.61 
       Drho_20(2) = 0.05
       lc_20(2) = v_lc
       Dlc_20(2) = v_lc_err

       filter_20(3) = 'R   '
       rho_20(3) = 19.99
       Drho_20(3) = 0.14
       lc_20(3) = r_lc
       Dlc_20(3) = r_lc_err

       filter_20(4) = 'OIII'
       rho_20(4) = 15.59 
       Drho_20(4) = 0.03
       lc_20(4) = oIII_lc
       Dlc_20(4) = oIII_lc_err

       do i=1,4
         call compute_pisco_scale(rho_10(i), Drho_10(i), lc_10(i), 
     1                       Dlc_10(i), scale_10(i), Dscale_10(i))
         write(6,120) 10,filter_10(i),scale_10(i),Dscale_10(i) 
       end do

       do i=1,4
         call compute_pisco_scale(rho_20(i), Drho_20(i), lc_20(i), 
     1                       Dlc_20(i), scale_20(i), Dscale_20(i))
         write(6,120) 20,filter_20(i),scale_20(i),Dscale_20(i) 
120    format('Eyepiece: ',i2,' Filter: ',a,' Scale: ',f7.5,
     1  ' +/- ',f7.5,' arsec/pixel') 
       end do

C Weighted mean for the 10 mm eyepiece:
       sumweights = 0.
       sumsquares = 0.
       mean_scale_10 = 0.
       Dmean_scale_10 = 0.
       do i=1,4
         sumweights = sumweights + 1. / Dscale_10(i)
         mean_scale_10 = mean_scale_10 + scale_10(i) / Dscale_10(i)
         Dmean_scale_10 = Dmean_scale_10 + Dscale_10(i)
         sumsquares = sumsquares 
     1      + scale_10(i) * scale_10(i) / Dscale_10(i)
       end do
       mean_scale_10 = mean_scale_10 / sumweights
       sumsquares = sumsquares / sumweights
       sigma_mean_10 = sqrt(sumsquares - mean_scale_10 * mean_scale_10)
C Dmean_scale_10 is the quadratic mean of all sigma
C It is the "average" value of sigma
       Dmean_scale_10 = sqrt(Dmean_scale_10 / sumweights)

C Weighted mean for the 20 mm eyepiece:
       sumweights = 0.
       mean_scale_20 = 0.
       Dmean_scale_20 = 0.
       sumsquares = 0.
       do i=1,4
         sumweights = sumweights + 1. / Dscale_20(i)
         mean_scale_20 = mean_scale_20 + scale_20(i) / Dscale_20(i)
         Dmean_scale_20 = Dmean_scale_20 + Dscale_20(i)
         sumsquares = sumsquares 
     1      + (scale_20(i) * scale_20(i)) / Dscale_20(i)
       end do
       mean_scale_20 = mean_scale_20 / sumweights
       sumsquares = sumsquares / sumweights
       sigma_mean_20 = sqrt(sumsquares - mean_scale_20 * mean_scale_20)
       Dmean_scale_20 = sqrt(Dmean_scale_20 / sumweights)

C Error is: sigma/sqrt(n-1)
       write(6,121) 10, mean_scale_10, Dmean_scale_10/sqrt(3.),
     1              sigma_mean_10
       write(6,121) 20, mean_scale_20, Dmean_scale_20/sqrt(3.),
     1              sigma_mean_20
121    format('Mean scale for ',i2,'-mm eyepiece: ',f7.5,
     1  ' +/-',f7.5,' arsec/pixel  (sigma_mean=',f7.5,')') 

      return
      end

C************************************************************************
C Compute the scale of PISCO from measurement of the distance between
C adjacent patches in the autocorrelation image obtained when observing
C a single star with the Zeiss telescope and its pupil mask.
C
C INPUT:
C  rho: distance in pixels between adjacent patches in the autocorrelation
C       image
C  error_rho: estimate of rho error 
C  lambda: central lambda in angstroms
C  error_lambda: error on central lambda in angstroms
C
C OUTPUT:
C  scale: value of the scale (in arcsec/pixel)
C  err_scale: estimate of scale error (in arcsec/pixel)
C
C************************************************************************
      subroutine compute_pisco_scale(rho, error_rho, lambda, 
     1               error_lambda, scale, err_scale)
      real*4 rho, error_rho,scale, err_scale
      real*4 lambda, error_lambda, aa, rad_to_arcsec
      real*4 relative_error
      rad_to_arcsec = 180. * 60. * 60. / 3.141592654

C aa: distance between two slits (in mm)
      aa = 87.96
      error_aa = 0.02

        scale = (lambda * 1.e-10 / (aa * 1.e-3)) 
     1           * rad_to_arcsec / rho
        relative_error = sqrt((error_rho / rho)**2 
     1            + (error_lambda /lambda)**2 + (error_aa / aa)**2)
        err_scale = scale * relative_error
      return
      end
