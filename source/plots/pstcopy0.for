      program PSTCOPY0
C++************************************************************************
C Program PSTCOPY0
C To copy an image on a postscript laserprinter
C
C From ILASER_2V7:
C Michael C. B. Ashley / Mount Stromlo Observatory / 06-May-86
C
C Initially not portable, calling Vax routines, and linked with Vax macros
C (IMG_PATCH_BYTES, and IMG_SCALE_IMAGE).
C
C 25-03-90: Hexa encoding and sequential formatted file
C           (before: unformatted and one byte encoding, but doesn't work well
C           with UNIX)
C 12-09-89: Modified to accept Midas format, and JLP_READIMAG.
C 18-03-90: Replacement of some VAX routines (towards portability...),
C           AND suppression of macro routines (!!!!!)
C
C Example of simple command (everything displayed between MIN and MAX)
C   RUNS PSTCOPY0 FILE
C
C Another one (with relative MIN and MAX, after 3 sigma rejection)
C   RUNS PSTCOPY0 FILE /AUTOSCALE/ID='Example 293843'
C
C More elaborated:
C   RUNS PSTCOPY0 FILE /WHITE=2.0/BLACK=6.2/XCENTRE=128/NX=30/YMIN=10
C        /NY=32/ID='TEST'/DEBUG/NOAXES
C
C JLP
C Version 18-05-91
C--*************************************************************************
*
* A program to plot images on the LaserWriter.
*
* The program is designed to be used as a foreign command at DCL level. So
* before using it you should do '$set command pstcopy', using the associated
* command definition file PSTCOPY.CLD.
*
* The format of the command is as follows:
*
* $ PSTCOPY FILESPEC
*        /XMIN        - the first x pixel to display
*                       Default: 1
*        /XCENTRE     - the central x pixel to display
*        /XMAX        - the last x pixel to display
*                       Default: the highest x pixel
*        /NX          - the number of x pixels to display
*                       Note: at most two of the above four qualifiers can
*                             be specified simultaneously
*        /YMIN        - the first x pixel to display
*                       Default: 1
*        /YCENTRE     - the central y pixel to display
*        /YMAX        - the last x pixel to display
*                       Default: the highest y pixel
*        /NY          - the number of x pixels to display
*                       Note: at most two of the above four qualifiers can
*                             be specified simultaneously
*        /ASPECT      - specifies the shape of the output pixels: ASPECT is
*                          the length in the y direction divided by the
*                          length in the x direction
*                       Default: 1.0
*        /BITS        - the number of bits per pixel to use in the output image
*                       Default: 8. Allowable values: 1, 2, 4, 8, or 16.
*                       At the moment this is not implemented.
*        /BLACK       - the data value which will be mapped to black
*                       Default: 255.0
*        /WHITE       - the data value which will be mapped to white
*                       Default: 0.0
*        /ID          - a text string which will be written in a large font
*                          below the output image
*                       Default: FILESPEC
*        /DESCRIPTION - an additional text string which will be written in
*                          a smaller font below /ID
*                       Default: ' '; see below.
*        /[NO]AXES    - whether or not you want axes displayed. You might like
*                       to turn the axes off to speed up the printing
*                       process. Note that if you specify /NOAXES you will
*                       still get a border around the image. To turn off the
*                       border you should use /NOBORDER.
*                       Default: /AXES
*        /[NO]BORDER  - whether you want a border drawn around the image.
*                       If you specify /AXES you will always get a border.
*                       Default: /BORDER
*        /[NO]INFO    - whether you want the short paragraph of information
*                       included at the bottom of the image. If you specify
*                       /NOINFO then you just get the ID and the DESCRIPTION.
*                       Default: /INFO
*        /[NO]DEBUG   - whether you want debugging and timing information
*                       to be displayed when running PSTCOPY.
*                       Default: /NODEBUG
*
*
* Reservations:
*
* (1) /BITS isn't completely implemented yet. Do not use it.
*
      implicit none
 
	integer*4 i,j,k,STATUS
	real*4    r,s
 
	integer*4 NDIM,DIMS(2),REAL_ARRAY,WORK_ARRAY
	integer*4 XMIN,XMAX,YMIN,YMAX,XCENTRE,YCENTRE,NUM_BITS
	real*4    WHITE,BLACK,ASPECT
	logical*4 AXES
	character IMAGE_ID*(80),IMAGE_DESCRIPTION*(132),COMMENTS*80
	character FILESPEC*(255),VMSFILESPEC*(255),STRING*(255)
	character ENUM*(40),TYPE_OF_FILE*(40)
 
	integer*4 X_DIM,Y_DIM,NX,NY
	logical*4 HAVE_WHITE,HAVE_BLACK,HAVE_XMAX,HAVE_YMAX
	logical*4 BORDER,DEBUG,INFO,AUTO_SCALE
	real*4    ADD,MULT
 
	integer*4 TIO_TRUNC_R
	integer*4 jlp_cli_get_value
	logical*4 jlp_cli_present
	INTEGER*4 MADRID(1)
	COMMON /VMR/MADRID
 
C To allow for direct input:
	call jlp_begin
 
C Initialize a timer for measuring program performance.
	call jlp_init_timer
 
C Start getting the command qualifiers.
 
	     write(6,*) ' Input image :'
	     read(5,10) FILESPEC
10	     format(a)
 
	HAVE_XMAX=.false.
	XMIN=1
      if (jlp_cli_present ('XMIN')) then
          i = jlp_cli_get_value ('XMIN',STRING)
          read (STRING,*,err=997) XMIN
          if (jlp_cli_present ('XMAX')) then
              i = jlp_cli_get_value ('XMAX',STRING)
              read (STRING,*,err=997) XMAX
	      HAVE_XMAX=.true.
          else if (jlp_cli_present ('XCENTRE')) then
              i = jlp_cli_get_value ('XCENTRE',STRING)
              read (STRING,*,err=997) XCENTRE
              XMAX = XMIN+2*(XCENTRE-XMIN)-1
	      HAVE_XMAX=.true.
          else if (jlp_cli_present ('NX')) then
              i = jlp_cli_get_value ('NX',STRING)
              read (STRING,*,err=997) NX
              XMAX = XMIN+NX-1
	      HAVE_XMAX=.true.
          end if
      else if (jlp_cli_present ('XMAX')) then
          i = jlp_cli_get_value ('XMAX',STRING)
          read (STRING,*,err=997) XMAX
	  HAVE_XMAX=.true.
          if (jlp_cli_present ('XCENTRE')) then
              i = jlp_cli_get_value ('XCENTRE',STRING)
              read (STRING,*,err=997) XCENTRE
              XMIN = XMAX-2*(XMAX-XCENTRE)+1
          else if (jlp_cli_present ('NX')) then
              i = jlp_cli_get_value ('NX',STRING)
              read (STRING,*,err=997) NX
              XMIN = XMAX-NX+1
          end if
      else if (jlp_cli_present ('XCENTRE')) then
          i = jlp_cli_get_value ('XCENTRE',STRING)
          read (STRING,*,err=997) XCENTRE
          if (jlp_cli_present ('NX')) then
              i = jlp_cli_get_value ('NX',STRING)
              read (STRING,*,err=997) NX
              XMIN = XCENTRE-NX/2
              XMAX = XMIN+NX-1
	      HAVE_XMAX=.true.
          end if
      else if (jlp_cli_present ('NX')) then
          i = jlp_cli_get_value ('NX',STRING)
          read (STRING,*,err=997) NX
          XMAX = XMIN+NX-1
	  HAVE_XMAX=.true.
      end if
 
	HAVE_YMAX=.false.
	YMIN=1
      if (jlp_cli_present ('YMIN')) then
          i = jlp_cli_get_value ('YMIN',STRING)
          read (STRING,*,err=997) YMIN
          if (jlp_cli_present ('YMAX')) then
              i = jlp_cli_get_value ('YMAX',STRING)
              read (STRING,*,err=997) YMAX
	      HAVE_YMAX=.true.
          else if (jlp_cli_present ('YCENTRE')) then
              i = jlp_cli_get_value ('YCENTRE',STRING)
              read (STRING,*,err=997) YCENTRE
              YMAX = YMIN+2*(YCENTRE-YMIN)-1
	      HAVE_YMAX=.true.
          else if (jlp_cli_present ('NY')) then
              i = jlp_cli_get_value ('NY',STRING)
              read (STRING,*,err=997) NY
              YMAX = YMIN+NY-1
	      HAVE_YMAX=.true.
          end if
      else if (jlp_cli_present ('YMAX')) then
          i = jlp_cli_get_value ('YMAX',STRING)
          read (STRING,*,err=997) YMAX
          HAVE_YMAX = .false.
          if (jlp_cli_present ('YCENTRE')) then
              i = jlp_cli_get_value ('YCENTRE',STRING)
              read (STRING,*,err=997) YCENTRE
              YMIN = YMAX-2*(YMAX-YCENTRE)+1
          else if (jlp_cli_present ('NY')) then
              i = jlp_cli_get_value ('NY',STRING)
              read (STRING,*,err=997) NY
              YMIN = YMAX-NY+1
          end if
      else if (jlp_cli_present ('YCENTRE')) then
          i = jlp_cli_get_value ('YCENTRE',STRING)
          read (STRING,*,err=997) YCENTRE
          if (jlp_cli_present ('NY')) then
              i = jlp_cli_get_value ('NY',STRING)
              read (STRING,*,err=997) NY
              YMIN = YCENTRE-NY/2
              YMAX = YMIN+NY-1
	      HAVE_YMAX=.true.
          end if
      else if (jlp_cli_present ('NY')) then
          i = jlp_cli_get_value ('NY',STRING)
          read (STRING,*,err=997) NY
          YMAX = YMIN+NY-1
	  HAVE_YMAX=.true.
      end if
 
      AXES    = .not.jlp_cli_present ('NOAXES')
      AUTO_SCALE = jlp_cli_present ('AUTOSCALE')
      DEBUG   = jlp_cli_present ('DEBUG')
      INFO    = .not.jlp_cli_present ('NOINFO')
      BORDER  = jlp_cli_present ('BORDER')
 
      if (jlp_cli_present ('ASPECT')) then
          i = jlp_cli_get_value ('ASPECT',STRING)
          read (STRING,*,err=997) ASPECT
      else
          ASPECT = 1
      end if
 
      if (jlp_cli_present ('BITS')) then
          i = jlp_cli_get_value ('BITS',STRING)
          read (STRING,*,err=997) NUM_BITS
          if (NUM_BITS.ne.1.and.NUM_BITS.ne.2.and.NUM_BITS.ne.4.and.
     &        NUM_BITS.ne.8) then
              WRITE(6,*) '%PSTCOPY-F-ILLBITS - illegal value for /BITS'
              goto 999
          end if
      else
          NUM_BITS = 8
      end if
 
      HAVE_BLACK = jlp_cli_present ('BLACK')
      if (HAVE_BLACK) then
          i = jlp_cli_get_value ('BLACK',STRING)
          read (STRING,*,err=997) BLACK
      end if
 
      HAVE_WHITE = jlp_cli_present ('WHITE')
      if (HAVE_WHITE) then
          i = jlp_cli_get_value ('WHITE',STRING)
          read (STRING,*,err=997) WHITE
      end if
 
      if (jlp_cli_present ('ID')) then
          i = jlp_cli_get_value ('ID',IMAGE_ID)
      else
              IMAGE_ID = FILESPEC
      end if
 
C Check for the /DESCRIPTION qualifier, and if it is not present go and look
C for the file IMAGE_DESCRIPTION_FILE.
 
      if (jlp_cli_present ('DESCRIPTION')) then
          i = jlp_cli_get_value ('DESCRIPTION',IMAGE_DESCRIPTION)
          IMAGE_DESCRIPTION = IMAGE_DESCRIPTION
     &                            (2:TIO_TRUNC_R(IMAGE_DESCRIPTION)-1)
      else
         IMAGE_DESCRIPTION = ' '
      end if
 
      if (DEBUG) then
          WRITE(6,*) 'finished reading the qualifiers'
          call jlp_show_timer
      end if
 
C We have now finished reading the qualifiers.
 
C Reads the input image:
          call JLP_GET_IMAGE(FILESPEC,REAL_ARRAY,
     1	X_DIM,Y_DIM,VMSFILESPEC,COMMENTS)
 
C Set up the window limits:
	if (.not.HAVE_XMAX) XMAX=X_DIM
	XMAX=MIN(XMAX,X_DIM)
	XMIN=MAX(XMIN,1)
 
	if (.not.HAVE_YMAX) YMAX=Y_DIM
 	YMAX=MIN(YMAX,Y_DIM)
	YMIN=MAX(YMIN,1)
 
	NX=XMAX-XMIN+1
	NY=YMAX-YMIN+1
 
          if (DEBUG) then
              WRITE(6,*) ' Finished obtaining the image'
	      print *,' xmin,xmax,ymin,ymax:',xmin,xmax,ymin,ymax
	      print *,' xdim,ydim:',x_dim,y_dim
              call jlp_show_timer
          end if
 
	
	  if(AUTO_SCALE)then
	     print *,' OK, auto_scale'
	     call AUTOSCALE1(MADRID(REAL_ARRAY),X_DIM,Y_DIM,
     1	XMIN,YMIN,NX,NY,r,s,status)
	    if(status.eq.0)then
	      WHITE=r
	      BLACK=s
	      HAVE_WHITE=.TRUE.
	      HAVE_BLACK=.TRUE.
	    endif
	  endif
 
          if (.not.HAVE_WHITE.or..not.HAVE_BLACK) then
              call GET_LIMITS (MADRID(REAL_ARRAY),
     &                       X_DIM,Y_DIM,XMIN,YMIN,NX,NY,r,s)
	       if (.not.HAVE_WHITE) WHITE = r
	       if (.not.HAVE_BLACK) BLACK = s
          end if
 
	if (DEBUG) print *,' White, Black:',r,s
 
	  I=X_DIM*Y_DIM*4
          call jlp_getvm(WORK_ARRAY,I)
 
	CALL SCALE_IMAGE(MADRID(REAL_ARRAY),MADRID(WORK_ARRAY),
     1	X_DIM,Y_DIM,XMIN,XMAX,YMIN,YMAX,WHITE,BLACK)
 
          if (DEBUG) then
              WRITE(6,*) ' Finished scaling the image'
              call jlp_show_timer
          end if
 
C Now we create a file containing the info to be sent to the LaserWriter.
 
      call IMG_LASER_IMAGE (MADRID(WORK_ARRAY),X_DIM,Y_DIM,
     1	XMIN,YMIN,NX,NY,NUM_BITS,ASPECT,AXES,BORDER,
     1	WHITE,BLACK,IMAGE_ID,IMAGE_DESCRIPTION,
     1	VMSFILESPEC,INFO,COMMENTS,STATUS)
 
      if (DEBUG) then
          WRITE(6,*) ' Finished IMG_LASER_IMAGE'
          call jlp_show_timer
      end if
 
	if (STATUS.NE.0)THEN
	  WRITE(6,*) ' Fatal error in IMG_LASER_IMAGE'
	  goto 999
	endif
 
C This command sends file to the LaserWriter. If you don't want this
C to happen at run time, simply define `PRINT' to be `JUNK' using, e.g.,
C `$ PRINT=="JUNK"'.
	call jlp_do_command(
     1	'$print/queue=ps501/delete/notify pstcopy.tmp')
 
	goto 999
 
997   WRITE(6,*) ' Error reading an argument'
 
999	call jlp_end
	stop
	end
 
C********************************************************************
      subroutine GET_LIMITS (REAL_ARRAY,X_DIM,Y_DIM,XMIN,YMIN,
     &                       NX,NY,LOW,HIGH)
 
      implicit none
 
      integer*4 X_DIM,Y_DIM,XMIN,YMIN,NX,NY
      real*4    REAL_ARRAY(X_DIM,Y_DIM),LOW,HIGH
 
      integer*4 i,j
      real*4    r
 
      LOW  = +1.0E30
      HIGH = -1.0E30
 
      do j = YMIN,YMIN+NY-1
          do i = XMIN,XMIN+NX-1
              r = REAL_ARRAY(i,j)
              if (r.lt.LOW)  LOW  = r
              if (r.gt.HIGH) HIGH = r
          end do
      end do
      return
      end
 
C********************************************************************
      subroutine SCALE_IMAGE (REAL_ARRAY,WORK_ARRAY,
     1	X_DIM,Y_DIM,XMIN,XMAX,YMIN,YMAX,WHITE,BLACK)
 
      implicit none
 
      integer*4 X_DIM,Y_DIM,XMIN,XMAX,YMIN,YMAX
      real*4    REAL_ARRAY(X_DIM,Y_DIM)
      integer*4 i,j,k,WORK_ARRAY(*),WORK
	REAL*4 WHITE,BLACK,MUL,ADD
 
      MUL = 94.99/abs(BLACK-WHITE)
      ADD = 32.0-MUL*min(WHITE,BLACK)
 
C Encoding between 32 and 127 (for not interfering
C with control ASCII characters)
	k=1
      do j = YMIN,YMAX
          do i = XMIN,XMAX
              WORK = NINT(REAL_ARRAY(i,j)*MUL+ADD)
	      WORK = MAX(WORK,32)
	      WORK_ARRAY(k) = MIN(WORK,127)
	      k=k+1
          end do
      end do
      return
      end
 
C********************************************************************
      subroutine IMG_LASER_IMAGE (IMAGE,X_DIM,Y_DIM,XMIN,YMIN,NX,NY,
     &                             NUM_BITS,ASPECT,AXES,BORDER,
     &                             WHITE,BLACK,
     &                             IMAGE_ID,IMAGE_DESCRIPTION,
     &                             VMSFILESPEC,INFO,COMMENTS,STATUS)
 
C Version 1.1 / 29-Oct-1986 / Michael C. B. Ashley / Mt Stromlo Observatory
C Version 1.2 / 05-Nov-1986 / Changed to 1024 byte records to avoid a problem
C                             with the DEC print symbiont which sometimes
C                             crashes with 4096 byte records.
C Version 1.3 / 08-Sep-1987 / Added INFO argument.
C Version 1.4 / 23-Sep-1987 / Added BORDER argument.
 
      implicit  none
 
	integer*4 NX,NY,I2,IMAX
	integer*4 IMAGE(NX*NY)
	real*4    ASPECT,WHITE,BLACK
	logical*4 AXES,BORDER,INFO
	integer*4 X_DIM,Y_DIM,XMIN,YMIN,NUM_BITS,STATUS
	character IMAGE_ID*(*),IMAGE_DESCRIPTION*(*),VMSFILESPEC*(*)
	CHARACTER COMMENTS*(*),OUTPUT_NAME*40
 
      common /IMG_ILASER_1/BIG_BUFFER
      character BIG_BUFFER*(80)
 
      common /IMG_ILASER_2/BB_POINTER,IPSTFILE
      integer*4 BB_POINTER,IPSTFILE,ISTATUS
 
      character BUFFER*(120),DATE*(23),USERNAME*(40)
      integer*4 BUFFER_LENGTH,XLASER,YLASER,TOPLEVEL,i,j,k
      real*4    WIDTH,HEIGHT,r,DX,DY,XORIGIN,YORIGIN
 
      integer*4 TIO_TRUNC_R
 
C@      integer*4 lib$getjpi
 
      real*4     PAGE_WIDTH,PAGE_HEIGHT,LINE_WIDTH
      integer*4  FONT_SIZE,TICK_SIZE
 
      parameter (PAGE_WIDTH  = 72*167/25.4,
     &           PAGE_HEIGHT = 72*216/25.4,
     &           LINE_WIDTH  = 0.02,
     &           FONT_SIZE   = 10,
     &           TICK_SIZE   = 3)
 
      STATUS = 0
      BB_POINTER=1
 
      if (XMIN.lt.1.or.XMIN+NX-1.gt.X_DIM.or.NX.lt.1.or.
     &    YMIN.lt.1.or.YMIN+NY-1.gt.Y_DIM.or.NY.lt.1.or.
     &    (NUM_BITS.ne.1.and.NUM_BITS.ne.2.and.NUM_BITS.ne.4.and.
     &     NUM_BITS.ne.8).or.abs(ASPECT).lt.1.0E-9.or.
     &     WHITE.eq.BLACK) then
	  PRINT *,' Error with the parameters XMIN, X_DIM, NUM_BITS,...'
          STATUS = 1
          goto 999
      end if
 
	IPSTFILE=3
	OUTPUT_NAME='pstcopy.tmp'
	ISTATUS=0
C For UNIX compatibility:
	  open (unit=IPSTFILE,file=OUTPUT_NAME,status='new',
     1	access='sequential',err=1)
C	  IMODE=1
C	  CALL JLP_OSDOPEN(OUTPUT_NAME,40,IMODE,IPSTFILE,ISTATUS)
	  IF(ISTATUS.EQ.-1)THEN
	    WRITE(6,*) ' Fatal error opening postscript file pstcopy.tmp'
	    STOP
	  ENDIF
 
      goto 2
1	STATUS=3
	  print *,' Fatal error when opening pstcopy.tmp file'
      goto 998
 
2     call jlp_date_time(DATE)
 
C@ i = lib$getjpi (jpi$_username,,,,USERNAME,j)
	USERNAME='PRIEUR'
 
      r = min (PAGE_WIDTH/NX,PAGE_HEIGHT/(ASPECT*NY))
      WIDTH    = r * NX
      HEIGHT   = r * NY * ASPECT
      TOPLEVEL = 2**NUM_BITS-1
      XLASER   = 86
      YLASER   = 17*FONT_SIZE
      XORIGIN  = (PAGE_WIDTH-WIDTH)/2
      YORIGIN  = (PAGE_HEIGHT-HEIGHT)/2
 
112	format(A)
	  write(IPSTFILE,100)
     1	char(4),(NX*8)/NUM_BITS,NX,NY,NUM_BITS,NX,NY
100	FORMAT('%!PS-Adobe-',/,A1,/,'/line',I5,' string def ',/,
     1	'/dispimage {',3I5,' [',I5,' 0 0',I5,' 0 0]')
 
C Hexadecimal encoding:
	  write(IPSTFILE,115)
115	  FORMAT(' {currentfile line readhexstring pop} image ',
     1	'drawaxes showpage} def')
 
	write(IPSTFILE,106)
     1	FONT_SIZE*2,XLASER,YLASER-FONT_SIZE*2
106	format ('/Times-Roman findfont',I5,
     1	' scalefont setfont ',2I5,' translate ')
 
	write(IPSTFILE,101)
     1	-FONT_SIZE*4,IMAGE_ID(:min(len(IMAGE_ID),20)),
     1	FONT_SIZE,-FONT_SIZE*6,
     1	IMAGE_DESCRIPTION(:min(len(IMAGE_DESCRIPTION),30))
101	format (' 0',I5,' moveto (',A,') show',/,
     1	' /Times-Roman findfont',I5,' scalefont setfont',/,
     1	' 0',I5,' moveto (',A,') show ')
 
	YLASER = -FONT_SIZE*8
 
      if (INFO) then
        if (XMIN.eq.1.and.NX.eq.X_DIM.and.
     &     YMIN.eq.1.and.NY.eq.Y_DIM) then
	  write(BUFFER,109)YLASER,X_DIM,Y_DIM
109	format('0',I5,
     1	' moveto (The entire image is displayed:',
     1	I5,' by',I5,' pixels.) show')
	  call UTL_SQUEEZE_SPACES (BUFFER,BUFFER_LENGTH)
	  call UTL_REMOVE_NONSIG_ZEROES (BUFFER,BUFFER_LENGTH)
	  IMAX=MIN(80,BUFFER_LENGTH)
	  write(IPSTFILE,112) BUFFER(:IMAX)
          YLASER = YLASER - FONT_SIZE
        else
	  write(BUFFER,110)YLASER,X_DIM,Y_DIM
110	  format('0',I5,
     1	' moveto (The original image was',I5,' by',I5,
     1	' pixels.) show')
	  call UTL_SQUEEZE_SPACES (BUFFER,BUFFER_LENGTH)
	  call UTL_REMOVE_NONSIG_ZEROES (BUFFER,BUFFER_LENGTH)
	  IMAX=MIN(80,BUFFER_LENGTH)
	  write(IPSTFILE,112) BUFFER(:IMAX)
          YLASER = YLASER - FONT_SIZE
          write (BUFFER,*) '0',YLASER,
     &         ' moveto (This is a',NX,' by',NY,
     &         ' pixel subset, with lower left corner at pixel (',
     &         XMIN,',',YMIN,').) show '
	  call UTL_SQUEEZE_SPACES (BUFFER,BUFFER_LENGTH)
	  call UTL_REMOVE_NONSIG_ZEROES (BUFFER,BUFFER_LENGTH)
	  IMAX=MIN(80,BUFFER_LENGTH)
	  write(IPSTFILE,112) BUFFER(:IMAX)
          YLASER = YLASER - FONT_SIZE
        end if
 
        if (ASPECT.ne.1.0) then
          write (BUFFER,*) ' 0',YLASER,
     &         ' moveto (The pixels were enlarged by a factor of',
     &         ASPECT,' in the y direction.) show '
	  call UTL_SQUEEZE_SPACES (BUFFER,BUFFER_LENGTH)
	  call UTL_REMOVE_NONSIG_ZEROES (BUFFER,BUFFER_LENGTH)
	  IMAX=MIN(80,BUFFER_LENGTH)
	  write(IPSTFILE,112) BUFFER(:IMAX)
          YLASER = YLASER - FONT_SIZE
        end if
 
        write (BUFFER,200) YLASER,BLACK,WHITE
200     format ('0',I5,' moveto (Black corresponds to',G10.3,
     &        ', and white to ',G10.3,') show')
	  call UTL_SQUEEZE_SPACES (BUFFER,BUFFER_LENGTH)
	  call UTL_REMOVE_NONSIG_ZEROES (BUFFER,BUFFER_LENGTH)
	  IMAX=MIN(80,BUFFER_LENGTH)
	  write(IPSTFILE,112) BUFFER(:IMAX)
        YLASER = YLASER - FONT_SIZE
 
          write (BUFFER,*) '0',YLASER,
     &      ' moveto (The image file was ',VMSFILESPEC(1:30),') show'
	  call UTL_SQUEEZE_SPACES (BUFFER,BUFFER_LENGTH)
	  call UTL_REMOVE_NONSIG_ZEROES (BUFFER,BUFFER_LENGTH)
	  IMAX=MIN(80,BUFFER_LENGTH)
	  write(IPSTFILE,112) BUFFER(:IMAX)
 
          YLASER = YLASER - FONT_SIZE
          write (BUFFER,*) '0',YLASER,
     &      ' moveto (Comments: ',COMMENTS(:40),') show'
	  call UTL_SQUEEZE_SPACES (BUFFER,BUFFER_LENGTH)
	  call UTL_REMOVE_NONSIG_ZEROES (BUFFER,BUFFER_LENGTH)
	  IMAX=MIN(80,BUFFER_LENGTH)
	  write(IPSTFILE,112) BUFFER(:IMAX)
 
C Complements of comments:
          YLASER = YLASER - FONT_SIZE
          write (BUFFER,*) '0',YLASER,
     &      ' moveto (',COMMENTS(40:80),') show'
	  call UTL_SQUEEZE_SPACES (BUFFER,BUFFER_LENGTH)
	  call UTL_REMOVE_NONSIG_ZEROES (BUFFER,BUFFER_LENGTH)
	  IMAX=MIN(80,BUFFER_LENGTH)
	  write(IPSTFILE,112) BUFFER(:IMAX)
 
        YLASER = YLASER - FONT_SIZE
        write (BUFFER,*) '0',YLASER,' moveto (Printed on ',
     1	DATE(:20),' for ',USERNAME(:14),') show'
	  call UTL_SQUEEZE_SPACES (BUFFER,BUFFER_LENGTH)
	  call UTL_REMOVE_NONSIG_ZEROES (BUFFER,BUFFER_LENGTH)
	  IMAX=MIN(80,BUFFER_LENGTH)
	  write(IPSTFILE,112) BUFFER(:IMAX)
      end if
 
      DX = TICK_SIZE*NX/WIDTH
      DY = TICK_SIZE*NY/HEIGHT
 
      if (AXES) then
	  write(IPSTFILE,304)
304	format(
     &      '/drawaxes { {} settransfer  /inci {/i i 1 add def',/,
     &      'i 100 eq {/i 0 def currentpoint stroke moveto} if} def ',/,
     &      '/setupticks {/i 0 def ',/,
     &      '/ticks 10 {3 dy mul dy dy dy dy 2 dy mul dy dy dy dy} ',/,
     &      'repeat 100 array astore def n 50 ge ',/,
     &      '{ticks 0 5 dy mul put} if ticks 50 4 dy mul put} def ',/,
     &      '/horizaxis {newpath setupticks n y moveto 0 y lineto ',/,
     &      'n 1 add {/r ticks i get def inci 0 r rlineto ',/,
     &      '1 r neg rmoveto} repeat stroke} def ',/,
     &      '/vertaxis  {newpath setupticks x n moveto x 0 lineto ',/,
     &      'n 1 add {/r ticks i get def inci r 0 rlineto ',/,
     &      'r neg 1 rmoveto} repeat stroke} def')
 
	  write(IPSTFILE,300)
     1	1.0/NX,1.0/NY,LINE_WIDTH,NX,-DY,NY,NY,-DX,NX
300       format (2G12.5,' scale',G12.5,' setlinewidth /n ',I5,/,
     &      ' def /y 0 def /dy',G12.5,' def  horizaxis /y',I5,/,
     &      ' def /dy dy neg def horizaxis /n ',I5,' def /x 0 def /dy',/,
     &      G12.5,' def vertaxis /x',I5,' def /dy dy neg def vertaxis} def')
 
      elseif (BORDER) then
 	  write(IPSTFILE,400) LINE_WIDTH/max(NX,NY)
400       format (
     &      '/drawaxes { {} settransfer',G12.5,' setlinewidth newpath ',/,
     &      '0 0 moveto 0 1 lineto 1 1 lineto 1 0 lineto 0 0 lineto ',/,
     &      'stroke } def')
      else
	  write(IPSTFILE,*) '/drawaxes { } def'
      end if
 
C Set up the transfer function so that 32 and 127 map to black and white (or
C vice versa).
 
      if (BLACK.gt.WHITE) then
         write(BUFFER,*,err=997)
     1	'{1.336842 exch 2.68421 mul sub} settransfer'
      else
         write(BUFFER,*,err=997)
     1	'{0.1254902 sub 2.68421 mul} settransfer'
      end if
	  write(IPSTFILE,112) BUFFER(:80)
 
	  write (BUFFER,*,err=997) XORIGIN,YORIGIN,' translate',
     &  WIDTH,HEIGHT,' scale'
	  call UTL_SQUEEZE_SPACES (BUFFER,BUFFER_LENGTH)
	  call UTL_REMOVE_NONSIG_ZEROES (BUFFER,BUFFER_LENGTH)
	  IMAX=MIN(80,BUFFER_LENGTH)
	  write(IPSTFILE,112) BUFFER(:IMAX)
	BUFFER=' '
	BUFFER(70:80)= ' dispimage '
	write(IPSTFILE,112) BUFFER(:80)
 
C Hexadecimal encoding:
	  IMAX=NX*NY/40
	  IMAX=IMAX*40
	  DO I=1,IMAX,40
	    CALL JLP_HCODE(IMAGE(i),BUFFER,40)
	    write(IPSTFILE,112) BUFFER(:80)
	  END DO
	  I2=NX*NY-IMAX
	  IF (I2.NE.0)then
	    CALL JLP_HCODE(IMAGE(i),BUFFER,i2)
	    write(IPSTFILE,112) BUFFER(:80)
	  ENDIF
	  write(IPSTFILE,112) CHAR(4)
	
      goto 998
997   STATUS = 4
	  print *,' Fatal error when writing the postscript file'
C998   CALL JLP_OSDCLOSE(IPSTFILE,ISTATUS)
998   CLOSE(IPSTFILE)
	IF(ISTATUS.EQ.-1)THEN
	  PRINT *,' Error closing the postscript file'
	ENDIF
999   return
      end
 
C********************************************************************
      subroutine WRITE_LASER (STRING)
      implicit none
      character STRING*(*)
 
      integer*4 i
 
      common /IMG_ILASER_1/BIG_BUFFER
      character BIG_BUFFER*(80)
 
      common /IMG_ILASER_2/BB_POINTER,IPSTFILE
      integer*4 BB_POINTER,IPSTFILE
 
      integer*4 TIO_TRUNC_R
 
      i = TIO_TRUNC_R (STRING)
 
      if (BB_POINTER+i+1.gt.80) then
          BIG_BUFFER(BB_POINTER:) = ' '
          write(IPSTFILE,*) BIG_BUFFER
          BB_POINTER=1
      end if
 
      BIG_BUFFER(BB_POINTER:BB_POINTER+i-1) = STRING(:i)
      BIG_BUFFER(BB_POINTER+i:BB_POINTER+i) = ' '
      BB_POINTER = BB_POINTER+i+1
 
      return
      end
 
C********************************************************************
      subroutine WRITE_DISPIMAGE
 
      implicit none
 
      common /IMG_ILASER_1/BIG_BUFFER
      character BIG_BUFFER*(80)
 
      common /IMG_ILASER_2/BB_POINTER,IPSTFILE
      integer*4 BB_POINTER,IPSTFILE
 
      if (BB_POINTER.gt.1014) then
          BIG_BUFFER(BB_POINTER:) = ' '
          write(IPSTFILE,*) BIG_BUFFER
          BB_POINTER=1
      end if
 
      BIG_BUFFER(BB_POINTER:) = ' '
      BIG_BUFFER(70:80) = ' dispimage '
      write(IPSTFILE,*) BIG_BUFFER
 
      return
      end
C**********************************************************************
      subroutine JLP_GET_IMAGE(FILESPEC,
     &       IMAGE,X_DIM,Y_DIM,VMSFILESPEC,COMMENTS)
      character FILESPEC*(255),VMSFILESPEC*(255),COMMENTS*(80)
      integer*4 IMAGE,X_DIM,Y_DIM
 
      integer*4 i,istatus
C@ integer*4 lib$find_file
C@ include '($RMSDEF)'
 
	CALL JLP_INQUIFMT
 
	CALL JLP_VM_READIMAG(IMAGE,X_DIM,Y_DIM,FILESPEC,COMMENTS)
 
C Finds the whole name of the file (directory and so on)
  	  VMSFILESPEC=FILESPEC
	
	RETURN
	END
 
C**********************************************************************
*##############################################
      function TIO_TRUNC_R(CHR)
 
C              >I4         <C
 
C Copyright (c) 1983 M. Ashley
 
C Returns the position of the last non-blank character in CHR. If CHR is all
C blank, then returns a 1.
 
      implicit none
 
      integer*4 TIO_TRUNC_R
 
      character CHR*(*)
 
      integer i
 
      i=len(CHR)
94    if (CHR(i:i).ne.' '.or.i.le.1) goto 95
         i=i-1
      goto 94
95      TIO_TRUNC_R=i
      return
      end
 
*###############################################################
      subroutine  UTL_REMOVE_NONSIG_ZEROES (STRING,LENGTH)
      implicit none
      character STRING*(*)
      integer*4 LENGTH
      character C*1
      integer*4 i,j,START_NONSIG
      logical*4 HAVE_POINT,HAVE_ZERO
 
      HAVE_POINT = .false.
      HAVE_ZERO  = .false.
      i = 1
      do j = 1, LENGTH
          C = STRING(j:j)
          if (C.eq.'.') then
              HAVE_POINT = .true.
          else
              if (HAVE_POINT) then
                  if (C.lt.'0'.or.C.gt.'9') then
                      HAVE_POINT = .false.
                      if (HAVE_ZERO) then
                          HAVE_ZERO = .false.
                          i = START_NONSIG
                      end if
                  else if (C.eq.'0') then
                      if (.not.HAVE_ZERO) then
                          HAVE_ZERO = .true.
                          if (STRING(j-1:j-1).eq.'.') then
                              START_NONSIG = i+1
                          else
                              START_NONSIG = i
                          end if
                      end if
                  else
                      HAVE_ZERO = .false.
                  end if
              end if
          end if
          STRING(i:i) = C
          i = i+1
      end do
 
      LENGTH = max(1,i-1)
 
      i = index(STRING(:LENGTH),'E+00')
      if (i.ne.0) then
          STRING(i:LENGTH-4) = STRING(i+4:LENGTH)
          LENGTH = LENGTH-4
          i = index(STRING(:LENGTH),'E+00')
          if (i.ne.0) then
              STRING(i:LENGTH-4) = STRING(i+4:LENGTH)
              LENGTH = LENGTH-4
          end if
      end if
 
      return
      end
 
*###########################################################
      subroutine UTL_SQUEEZE_SPACES (STRING,LENGTH)
 
      implicit  none
      character STRING*(*)
      integer*4 LENGTH
 
      logical*4 FIRST_SPACE,HERE_COME_SOME_SPACES
      integer*4 i,j,k
 
      FIRST_SPACE = .true.
      HERE_COME_SOME_SPACES = .false.
      i = 1
      do k = 1, len(STRING)
          if (STRING(k:k).ne.' ') goto 1
      end do
      k = len(STRING)
1     do j = k, len(STRING)
          HERE_COME_SOME_SPACES = STRING(j:j).eq.' '
          if (.not.HERE_COME_SOME_SPACES) then
              FIRST_SPACE = .true.
              STRING(i:i) = STRING(j:j)
              i = i+1
          else if (FIRST_SPACE) then
              FIRST_SPACE = .false.
              STRING(i:i) = ' '
              i = i+1
          end if
      end do
      LENGTH = max(1,i-1)
      return
      end
C++***************************************************************
C Subroutine to compute some statistical parameters
C of an image, to be used as scaling values for further display
C
C JLP
C Version of 18-03-90
C--***************************************************************
	SUBROUTINE AUTOSCALE1(IMAGE,X_DIM,Y_DIM,XMIN,YMIN,
     1	NX,NY,XLOW,XHIGH,STATUS)
	INTEGER*4 X_DIM,Y_DIM,XMIN,YMIN,NX,NY,STATUS
	INTEGER*4 IX,IY
	REAL*4 IMAGE(X_DIM,Y_DIM),MIN1,MAX1
	REAL*4 MEAN1,SIGMA1,MEAN2,SIGMA2,XLOW,XHIGH,XNN,WORK
	REAL*8 SUM,SUMSQ
 
	STATUS=0
C First iteration:
	SUM=0.	
	SUMSQ=0.
	XNN=0.
	MIN1=IMAGE(XMIN,YMIN)
	MAX1=MIN1
	
	DO IY=YMIN,YMIN+NY-1
	  DO IX=XMIN,XMIN+NX-1
	    WORK=IMAGE(IX,IY)
	    SUM=SUM+WORK
	    SUMSQ=SUMSQ+WORK*WORK
	    XNN=XNN+1.
	    MIN1=MIN(WORK,MIN1)
	    MAX1=MAX(WORK,MAX1)
	  END DO
	END DO
	MEAN1=SUM/XNN
	WORK=SUMSQ/XNN-MEAN1**2
	IF(WORK.LE.0.)THEN
	   SIGMA1=MEAN1/10000.
	ELSE
	   SIGMA1=SQRT(WORK)
	ENDIF
	
C Second iteration:
	SUM=0.	
	SUMSQ=0.
	XLOW=MEAN1-3*SIGMA1
	XHIGH=MEAN1+3*SIGMA1
	XNN=0.
 
	DO IY=YMIN,YMIN+NY-1
	  DO IX=XMIN,XMIN+NX-1
	    WORK=IMAGE(IX,IY)
	    IF((WORK.LE.XHIGH).AND.(WORK.GE.XLOW))THEN
	      SUM=SUM+WORK
	      SUMSQ=SUMSQ+WORK*WORK
	      XNN=XNN+1.
	    ENDIF
	  END DO
	END DO
 
	IF(XNN.EQ.0)THEN
	  PRINT *,' FAILURE IN AUTOSCALE:',
     1	' No points between Mean +/- 3*Sigma !!'
	  STATUS=2
	  RETURN
	ENDIF
 
	MEAN2=SUM/XNN
	WORK=SUMSQ/XNN-MEAN2**2
	IF(WORK.LE.0.)THEN
	   SIGMA2=MEAN2/10000.
	ELSE
	   SIGMA2=SQRT(WORK)
	ENDIF
 
	XLOW=MEAN2-3*SIGMA2
	XLOW=MAX(XLOW,MIN1)
  	XHIGH=MEAN2+3*SIGMA2
	XHIGH=MIN(XHIGH,MAX1)
 
	RETURN
	END
C---------------------------------------------------------
C Hexadecimal encoding:
C---------------------------------------------------------
	SUBROUTINE JLP_HCODE(IMAGE,BUFFER,N)
	CHARACTER BUFFER(2*N)*1
	CHARACTER HCODE(16)*1
	INTEGER*4 I0,I1,I2
	INTEGER*4 IMAGE(N)
	DATA HCODE/'0','1','2','3','4','5','6','7','8','9',
     1	'A','B','C','D','E','F'/
	K=0
	DO I=1,N
	  I0=IMAGE(I)
	  I2=I0/16
	  K=K+1
	  BUFFER(K)=HCODE(I2+1)
	  I1=I0-I2*16
	  K=K+1
	  BUFFER(K)=HCODE(I1+1)
	END DO
	RETURN
	END
C---------------------------------------------------------
C Encoding in one byte:
C---------------------------------------------------------
	SUBROUTINE JLP_BCODE(IMAGE,BUFFER,N)
	INTEGER*4 N
	CHARACTER BUFFER(N)*1
	INTEGER*4 IMAGE(N)
	DO I=1,N
	  BUFFER(I)=CHAR(IMAGE(I))
	END DO
	RETURN
	END
