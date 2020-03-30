C++*****************************************************************
C Program to test Vax emulator on UNIX machines
C
C JLP
C Version 25-07-91
C--*****************************************************************
	program test_vax
	integer*4 pntr,memsize
	integer istatus
	character*60 name,value
	character*60 output(200)
	logical*4 prest
	integer*4 madrid(1), nfiles, quiet, istatus
	common /vmr/madrid
	
	print *,' Program to test Vax emulator'
 
C Check if toto is present on the command line:
	prest=jlp_cli_present('toto')
	if(prest)then
	   print *,' /toto present in command line'
	   i=jlp_cli_get_value('toto',value)
	   print *,' i:',i
	   print *,' value:',value
	else
	   print *,' /toto absent in command line'
	endif
 
	memsize=1600*4
C	call jlp_gvm(pntr,memsize)
	call jlp_getvm(pntr,memsize)
	call test1(madrid(pntr),40)
C	call jlp_fvm(pntr)
	call jlp_freevm(pntr,memsize)
	memsize=10000*4
C	call jlp_gvm(pntr,memsize)
	call jlp_getvm(pntr,memsize)
	call test1(madrid(pntr),100)
C	call jlp_fvm(pntr)
	call jlp_freevm(pntr,memsize)
C
	name='MIDVERS'
	call jlp_getenv(name,7,value,istatus)
	print *,' Symbol and value :',name,value
	print *,' Status:',istatus
 
	call jlp_getenv('MIDVERS',7,value,istatus)
	print *,' MIDVERS, value :',name,value
	print *,' Status:',istatus

	call jlp_date_time(value)
	print *,' Date, time:',value
	print*,' Now calling directory (*.tmp) '
	name='*.tmp'
        quiet = 0
	call jlp_dir(name,60,output,nfiles,quiet,istatus)
	print *,' istatus= ',istatus
	print *,' nfiles=',nfiles
	if(istatus.eq.0)then
	do i=1,nfiles
	  print *,' file #',i,' is ',output(i)
	end do
	endif
 
	stop
	end
	subroutine test1(image,n)
	real*4 image(n,n)
	print *,'image(5,5)',image(5,5)
	do i=1,n
	  do j=1,n
	     image(j,i)=2
	     end do
	     end do
	print *,'image(5,5)',image(5,5)
	return
	end
C@	include 'jlp0_vax1.f'
