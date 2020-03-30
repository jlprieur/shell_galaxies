C++------------------------------------------------------------------
C Program to compute the gradient map of an image
C Works in all directions
C From Bill Sparks
C
C JLP
C Version of 25-04-90
C--------------------------------------------------------------------
	PROGRAM QUICKGRAD2
	INTEGER*4 DIM1(2),DIM3(2)
	INTEGER*4 IPT1,IPT3,MADRID(1)
	CHARACTER NAME*40,COMMENTS*80
	COMMON /VMR/MADRID
 
	CALL JLP_BEGIN
	CALL JLP_INQUIFMT
 
	WRITE(6,23)
23	FORMAT(' Program QUICKGRAD2',/,
     1	' To compute the gradient of the log')
C
	WRITE(6,*) ' Input image:'
        READ(5,10) NAME
	CALL JLP_VM_READIMAG(IPT1,DIM1(1),DIM1(2),NAME,COMMENTS)
	DIM3(1)=DIM1(1)
	DIM3(2)=DIM1(2)
 
C Getting memory space for the output:
	I=DIM3(1)*DIM3(2)*4
	CALL JLP_GETVM(IPT3,I)
 
	PRINT *,' ENTER THE SKY LEVEL AND THE NOISE :'
	READ(5,*) BACK,RMS
 
	CALL QUICKGRAD_SUB(MADRID(IPT1),DIM1,MADRID(IPT3),DIM3,BACK,RMS)
 
	WRITE(6,*) ' Output gradient map:'
        READ(5,10) NAME
	CALL JLP_WRITEIMAG(MADRID(IPT3),DIM3(1),DIM3(2),
     1	DIM1(1),NAME,COMMENTS)
 
80	CALL JLP_END
	STOP
	END
C------------------------------------------------------------------
C Subroutine QUICKGRAD_SUB which actually computes the gradient map
 
      SUBROUTINE QUICKGRAD_SUB(IM,IDIM,GRADIENT,IDGR,BACK,RMS)
      integer idim(2),idgr(2),idker(2)
      real im(idim(1),idim(2)),gradient(idgr(1),idgr(2)),s1(6),s2(6),
     1 f(3)
      character cnum*1
c
c Filter row - assumed antisymmetric
c
      data f/1,1,1/
c
c Subroutine to generate a gradient map by convolution with a kernel
c and quadratic addition of the 2 components.
c Output is log of image with offset Out=Kernel*(Log(Image-Offset))
c
      offset=ABS(back-3.0*rms)
      nfilter=3
      fsum=0
      do i=1,nfilter
CCCCC        call itoc(i,cnum,ist)
CCCCC        call promptr('Top row of filter f'//cnum,'F',f(i))
        fsum=fsum+f(i)
      enddo
c
      do j=1,idim(2)
        write(6,101)j
101	format(1h ,'Doing row:',i4)
        if(j.eq.1.or.j.eq.idim(2))then
          do i=1,idim(1)
            gradient(i,j)=0.0
          enddo
          go to 20
        endif
        do i=1,idim(1)
           if(i.eq.1.or.i.eq.idim(1))then
             gradient(i,j)=0.0
             go to 20
           endif
c Set values below offset to back
c
           s1(1)=im(i-1,j+1)-offset
           s1(2)=im(i,j+1)-offset
           s1(3)=im(i+1,j+1)-offset
           s1(4)=im(i-1,j-1)-offset
           s1(5)=im(i,j-1)-offset
           s1(6)=im(i+1,j-1)-offset
           s2(1)=im(i-1,j+1)-offset
           s2(2)=im(i-1,j)-offset
           s2(3)=im(i-1,j-1)-offset
           s2(4)=im(i+1,j+1)-offset
           s2(5)=im(i+1,j)-offset
           s2(6)=im(i+1,j-1)-offset
           do k=1,6
             if(s1(k).le.0.0)s1(k)=back-offset
             if(s2(k).le.0.0)s2(k)=back-offset
             s1(k)=alog10(s1(k))
             s2(k)=alog10(s2(k))
           enddo
           sum1=(f(1)*s1(1)+f(2)*s1(2)+f(3)*s1(3)-
     1           f(1)*s1(4)-f(2)*s1(5)-f(3)*s1(6))/fsum
           sum2=(f(1)*s2(1)+f(2)*s2(2)+f(3)*s2(3)-
     1           f(1)*s2(4)-f(2)*s2(5)-f(3)*s2(6))/fsum
           sum=sum1*sum1+sum2*sum2
           gradient(i,j)=sqrt(sum)
   20   enddo
      enddo
c
80	return
	end
