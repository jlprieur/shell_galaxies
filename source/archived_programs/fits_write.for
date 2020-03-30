C++*************************************************************************
C Program FITS_WRITE
C To write FITS files on the disk or on tape.
C Allowed format: integers with 16 or 32 bits/pixels
C Length of blocks is the same as logical records (2880),
C i.e blocking factor is 1.
C Wild card is allowed for input/output files.
C
C SYNTAX:
C (for disk)
C   RUNS FITS_WRITE [D for disk] bits/pix input_image output_fits_file
C or (tape)
C   RUNS FITS_WRITE [T for tape] bits/pix input_image device
C              first_output_sequence_number(if overwrite, or -1 if end_of_tape)
C
C Examples:
C   RUNS FITS_WRITE D 32 test.bdf test.mt
C   RUNS FITS_WRITE T 32 test.bdf MTA1: -1
C Or with wild cards:
C   RUNS FITS_WRITE D 32 t*.bdf *.mt
C   RUNS FITS_WRITE T 32 test*.bdf MTA1: -1
C
C JLP
C Version 03-11-93
C
C PLEASE FOLLOW LINES STARTING WITH "JLP93"...
C--*************************************************************************
C From ECRIFITS and AUWRITEFITS16_DISK (M. Cailloux)
C***************************************************************************
        PROGRAM FITS_WRITE
	PARAMETER (NBYTE=2880,NBYTE2=1440)
        REAL*4 BSCALE,BZERO,X0,Y0,DX,DY
	LOGICAL*1 IDATA(NBYTE)
        CHARACTER INFRAME*60,OUTFRAME*60,COMMENTS*80,OBJECT*20,ANS*1
        INTEGER*4 NAXIS,NX,NY,NBITS
 	INTEGER*4 ITAPE,IFID,FIRST_CALL,NPOSI
C JLP93 
	CHARACTER MT_DEVICE*20,INFRAM(100)*60,OUTFRAM(100)*60
	INTEGER*4 QUIET

	INTEGER*4 MADRID(1),PNT_IN,IN_DESCR,OUT_DESCR
	CHARACTER CDESCR*500
	COMMON /VMR/MADRID
	COMMON/FITS11/ITAPE,IFID,MT_DEVICE
	COMMON/FITS12/IDATA
C
        COMMON/JLP_DESCRIPTORS/CDESCR,IN_DESCR,OUT_DESCR
C Common block with JLP_DIRECTORY:
	COMMON/JLP_DIRECT/INFRAM

10      FORMAT(A)
        CALL JLP_BEGIN
        CALL JLP_INQUIFMT
C
        WRITE(6,11)
11      FORMAT(' Program FITS_WRITE Version 04-11-93 20:10',/,
     1  ' Current version only allows a maximum number of 15 files...',/,
C	1	' SUN Version (swap)')
	1	' VAX and DEC3100 Version (swap)')
 
C************************************************************************
C Selecting the options
C************************************************************************
 
C Disk or tape:
	WRITE(6,*) ' Write file on disk (D) or on tape (T) ?'
	READ(5,10) ANS
	ITAPE=0
	IF(ANS.EQ.'T'.OR.ANS.EQ.'t')ITAPE=1
 
C Reads NBYTE:
	WRITE(6,*) ' Number of bits/pix (16 or 32 ?)'
	READ(5,*) NBITS
C Check if 16 or 32 bits:
	IF(NBITS.NE.16.AND.NBITS.NE.32)THEN
	  PRINT *,' Sorry, only 16 and 32 bits/pixel are possible here'
	  STOP
	ENDIF
 
C Input file:
        WRITE(6,*) ' Input file:',
     1	' (if more than one, put wild card like: test*.bdf)'
        READ(5,10) INFRAME
 
	IF(ITAPE.EQ.0)THEN
C Output file:
          WRITE(6,*) ' Output FITS file:',
     1	' (if more than one, put wild card like: *.fits)'
          READ(5,10) OUTFRAME
	ELSE
	  PRINT 25
25	  FORMAT(' Input magnetic tape device : (MUB0:, MTA1:,...)')
	  READ(5,10) MT_DEVICE
	  PRINT *,' Sequence number where you want to start to write',
     1	' (-1=end of tape)'
	  READ(5,*) NFILE
C Nota: NFILE will be updated when the end of tape is found
	ENDIF
 
C************************************************************************
C Preparing the main loop:
C************************************************************************
 
C Looking at the list of files:
C JLP93
	QUIET=1
	CALL JLP_DIRECTORY(INFRAME,NFILES,QUIET)
	IF(NFILES.EQ.0)THEN
	  PRINT *,' Sorry no files have been found: ',INFRAME
	  STOP
	ENDIF
 
C Generating the output list of file names:
	IF(NFILES.GT.1.OR.INDEX(OUTFRAME,'*').GT.0)THEN
	   IF(ITAPE.EQ.0) THEN
	      CALL JLP_NFRAME1(OUTFRAME,OUTFRAM,INFRAM,NFILES)
C Just to check (for the user):
	      DO IFIL=1,NFILES
	        PRINT *,' IN: ',INFRAM(IFIL)
	        PRINT *,'   OUT: ',OUTFRAM(IFIL)
	      ENDDO
	    ELSE
C Just to check (for the user):
	      DO IFIL=1,NFILES
	        PRINT *,' IN: ',INFRAM(IFIL)
	      ENDDO
	    ENDIF
	ELSE
C When only one frame, without wild card:
	   IF(ITAPE.EQ.0) THEN
	     OUTFRAM(1)=OUTFRAME
	   ENDIF
	ENDIF
 
 
	X0=0.
	Y0=0.
	DX=1.
	DY=1.
        BSCALE=1.
        BZERO=0.
 
	IF(ITAPE.EQ.0)THEN
          IFID=7
	ELSE
 	  NPOSI=0
	ENDIF
	FIRST_CALL=1
 
C************************************************************************
C Main loop: writing the files
C************************************************************************
 
	DO 245 IFIL=1,NFILES
 
	IF(ITAPE.EQ.0)THEN
	  PRINT 67,INFRAM(IFIL),OUTFRAM(IFIL)
67	FORMAT(/,' Now writing file :',A,/,'     to: ',A)
	ELSE
	  IF(NFILE.LE.0)THEN
	  PRINT 68, INFRAM(IFIL)
68	FORMAT(/,' Now writing file :',A,' to end of tape')
	  ELSE
	  PRINT 69, INFRAM(IFIL),NFILE
69	FORMAT(/,' Now writing file :',A,' to sequence number #',I3)
	  ENDIF
	ENDIF
 
C Reading the input file:
	IN_DESCR=1
	CALL JLP_VM_READIMAG(PNT_IN,NX,NY,INFRAM(IFIL),COMMENTS)
C Removing the directory if present:
	I=MAX(1,INDEX(INFRAM(IFIL),']')+1)
	WRITE(OBJECT,111) INFRAM(IFIL)(I:I+17)
111	FORMAT(1H',A18,1H')
        CALL MINMAX_FITS(MADRID(PNT_IN),NX,NY,NX,BZERO,BSCALE,NBITS)
	PRINT *,' BZERO, BSCALE:',BZERO,BSCALE
 
C Open output file, or go to the right position on the tape:
	CALL FITS_WROPEN(OUTFRAM(IFIL),NFILE,FIRST_CALL,NPOSI,ISTATUS)
	FIRST_CALL=0
	IF(STATUS.NE.0)GOTO 246
 
C Writing the header:
        CALL FITS_WRHEADER(NBITS,OBJECT,NX,NY,DX,DY,X0,Y0,
     1	BSCALE,BZERO,ISTATUS)
	IF(STATUS.NE.0)GOTO 246
 
C Now writing the data:
	CALL FITS_WRDATA(MADRID(PNT_IN),NX,NY,NX,NBITS,BSCALE,
     1	BZERO,ISTATUS)
	IF(STATUS.NE.0)GOTO 246
 
C Close the output file on the disk, or write an EOF on the tape
	CALL FITS_WRCLOSE

C Free memory space of input file:
        ISIZE=NX*NY*4
        CALL JLP_FREEVM(PNT_IN,ISIZE)

245	CONTINUE
 
C Rewind the tape or close the last file:
	CALL FITS_WREND
 
        CALL JLP_END
        STOP
C********************************************************
C In case of error (generally when more than 15 files):
246     PRINT *,' Fatal error opening output fits file'
        PRINT *,'         (maximum should be 15 files...)'
	CALL FITS_WREND
        CALL JLP_END
        STOP
        END
C*************************************************************************
C FITS_WRHEADER
C Subroutine to write the header of a FITS formatted file
C This header occupies the first block.
C*************************************************************************
        SUBROUTINE FITS_WRHEADER(NBITS,OBJECT,NX,NY,DX,DY,X0,Y0,
     1	BSCALE,BZERO,ISTATUS)
	PARAMETER (NBYTE=2880)
        INTEGER*4 USE(36),ISTATUS,IN_DESCR,OUT_DESCR
        LOGICAL*1 IDATA(NBYTE)
        CHARACTER IMAGE(36)*80, BLANK*10, IDENT*80, COMMENT*10,text*60
        CHARACTER FIELD(36)*20, END*20, EQUAL*2, SLASH*2
        CHARACTER  KEYWORD(36)*8, CDATA*(NBYTE)
        CHARACTER*133 STRING,OBJECT*20,DAT*9
        CHARACTER OUTNAME*60,COMMENTS*80,CDESCR*500
        INTEGER*4 NAXIS,NX,NY,NBITS
        REAL*4 BSCALE,BZERO,X0,Y0,DX,DY
        EQUIVALENCE (IDATA(1),CDATA(1:1))
	COMMON/FITS12/IDATA
 
	COMMON/JLP_DESCRIPTORS/CDESCR,IN_DESCR,OUT_DESCR

        DATA EQUAL, SLASH /'= ','/ '/
        DATA BLANK /'          '/
        DATA COMMENT /'COMMENT   '/
        DATA END/'END '/
        DATA SPACE /' '/, QUOTE /''''/
        DATA KEYWORD/'SIMPLE  ','BITPIX  ','NAXIS   ','NAXIS1  ',
     1	'NAXIS2  ','NAXIS3  ','CRVAL1  ','CRVAL2  ','CRVAL3  ',
	2 	'CDELT1  ','CDELT2  ','CDELT3  ','BSCALE  ','BZERO   ',
	3	'OBJECT  ','DATE    ','DATE-OBS','ORIGIN  ','INSTRUME',
	4	'TELESCOP','OBSERVER','JLPDSC  ',13*'        ','END     '/
 
C  Standard FITS keywords:
C
C       NUMBER:        KEYWORD:
C
C       1               SIMPLE
C       2               BITPIX
C       3               NAXIS
C       4               NAXIS1
C       5               NAXIS2
C       6               NAXIS3
C       7               CRVAL1
C       8               CRVAL2
C       9               CRVAL3
C      10               CDELT1
C      11               CDELT2
C      12               CDELT3
C      13               BZERO
C      14               BSCALE
C      15               OBJECT
C      16               DATE
C      17               DATE-OBServation
C      18               ORIGIN
C      22               JLPDSC
C
	ISTATUS=0
C
C Resetting the lines to blanks
C (36 lines is the max if you want everything
C in only the first logical record)
        DO I=1, 36
           USE(I)=0
        END DO
        DO I=1, 2871, 10
           CDATA(I:I+9)=BLANK
        END DO
C
C
1000    FORMAT(I20)
2000    FORMAT(G20.10)
3000    FORMAT (A20)
          WRITE (FIELD(1), 3000) 'T'
          USE(1)=1
          WRITE (FIELD(2), 1000) NBITS
          USE(2)=1
C If only one line, NAXIS1 is set to 1
	NAXIS=2
	IF(NY.LE.1)NAXIS=1
          WRITE (FIELD(3), 1000) NAXIS
          USE(3)=1
          NAXIS1=NX
          WRITE (FIELD(4), 1000) NAXIS1
          USE(4)=1
          NAXIS2=NY
	IF(NY.GE.1) THEN
	  WRITE (FIELD(5), 1000) NAXIS2
          USE(5)=1
        ENDIF
          CRVAL1=X0
          WRITE (FIELD(7), 2000) CRVAL1
          USE(7)=1
          CRVAL2=Y0
          WRITE (FIELD(8), 2000) CRVAL2
          USE(8)=1
          CDELT1=DX
          WRITE (FIELD(10), 2000) CDELT1
          USE(10)=1
          CDELT2=DY
          WRITE (FIELD(11), 2000) CDELT2
          USE(11)=1
          WRITE (FIELD(15), 3000) OBJECT
          USE(15)=1
C LA DATE
C          CALL DATE(DAT)
C          WRITE (FIELD(16), 3000) DAT
          WRITE (FIELD(16), 115) '01-JAN-2000'
115	  FORMAT(1H',A11,1H')

          USE(16)=1
C BSCALE ET BZERO
          WRITE (FIELD(13), 2000) BSCALE
          USE(13)=1
          WRITE (FIELD(14), 2000) BZERO
          USE(14)=1
C ET LA CARTE " END "
          USE(36)=1
C
C CONSTRUCTION DES IMAGES DE CARTES
        NCARTE=0
        DO 110 ICLE=1,36
C Possibility of saving JLP descriptors:
          IF (ICLE.EQ.22.AND.IN_DESCR.EQ.1) THEN
            IC1=1
            DO I=1,8
              IC2=MIN(500,IC1+63)
              IMAGE(NCARTE)=KEYWORD(ICLE)//EQUAL
     1     //CDESCR(IC1:IC2)//SLASH
	      IC1=IC2+1
	      NCARTE=NCARTE+1
	    ENDDO
	  ENDIF
C Check if keyword has been selected: 
          IF (USE(ICLE).EQ.1) THEN
             NCARTE=NCARTE+1
	     IMAGE(NCARTE)=KEYWORD(ICLE)//EQUAL//FIELD(ICLE)//
     1	     BLANK//SLASH
	   ENDIF
C End card:
             IF (ICLE.EQ.36) IMAGE(NCARTE)=END
110      CONTINUE
 
C
C Writing the header now:
        DO 700 I=1, 36
         IFIRST=(I-1)*80 + 1
         ILAST = IFIRST + 79
         CDATA(IFIRST:ILAST) = IMAGE(I)
700     CONTINUE
 
C Writing the header on the output file/tape:
        CALL FITS_WRREC(ISTATUS)
	IF(ISTATUS.NE.0)THEN
	  PRINT *,' FITS_RDHEADER: error writing the header'
	  ISTATUS=1
	  RETURN
	ENDIF
 
	RETURN
	END
C*****************************************************************
C Computes BZERO and BSCALE according to the number of bits
C for the encoding
C*****************************************************************
        SUBROUTINE MINMAX_FITS(ARRAY,NX,NY,IDIM,BZERO,
     1	BSCALE,NBITS)
        REAL*4 ARRAY(IDIM,*)
	REAL*4 BZERO,BSCALE,XMIN,XMAX
        INTEGER*4 NX,NY,NBITS
 
	XMIN=ARRAY(1,1)
	XMAX=ARRAY(1,1)
        DO J=1,NY
         DO I=1,NX
          XMIN=MIN(ARRAY(I,J),XMIN)
          XMAX=MAX(ARRAY(I,J),XMAX)
         ENDDO
        ENDDO
 
        BZERO=XMIN
 
C Depending on NBITS
C If coding in 16 bits (VMAX= 2**15-1 = 32767
	VMAX=-1.+2.**(NBITS-1)
C With 32 bits I put -100 instead of -1 to avoid overflow pb with "NINT" ...
C Coding: IA=NINT((VALUE-BZERO)/BSCALE)
	IF(NBITS.EQ.32) VMAX=VMAX-100.
        BSCALE=(XMAX-XMIN)/VMAX
C If XMAX = XMIN bscale has no meaning...
	IF(BSCALE.EQ.0.) THEN
	  BSCALE = 1.
	  PRINT *,' MINMAX_FITS /Warning: all values are equal to',XMIN
	ENDIF
 
        RETURN
        END
C*************************************************************************
C FITS_WRDATA
C Subroutine to write the data on a FITS formatted file
C*************************************************************************
	SUBROUTINE FITS_WRDATA(ARRAY,NX,NY,IDIM,NBITS,BSCALE,
     1	BZERO,ISTATUS)
	PARAMETER (NBYTE=2880,NBYTE2=1440)
        LOGICAL*1 IDATA(NBYTE)
        INTEGER*2 MES(NBYTE2)
	REAL*4 ARRAY(IDIM,*),BSCALE,BZERO
        INTEGER*4 NX,NY,NBITS,ISTATUS
        EQUIVALENCE (IDATA(1),MES(1))
	COMMON/FITS12/IDATA
	
	ISTATUS=0
 
C Index for current pixel of the image:
	II=0
	IIMAX=NX*NY
	IBLOCKMAX=(NX*NY*NBITS/8+NBYTE-1)/NBYTE
 
	DO 112 IBLOCK=1,IBLOCKMAX
 
C Reset the buffer array to 0 before loading the next block:
          DO 1600 J=1,NBYTE2
            MES(J)=0
1600      CONTINUE
  	
C Computing the position of the last value to be written in that block:
	IMAX=NBYTE*8/NBITS
	ITEST=IIMAX-II
	IMAX=MIN(IMAX,ITEST)
 
C Coding the values up to this limit:
        NVAC=0
	IF(NBITS.EQ.16)THEN
	  DO I=1,IMAX
	    II=II+1
	    IY=II/NX
	    IX=II-IY*NX
	    IY=IY+1
	    CALL CODE_16BITS(ARRAY(IX,IY),MES,NVAC,BSCALE,BZERO)
	  END DO
C Swap the bytes (to correct Vax inversion...)
C NECESSARY for VAX and DEC3100:
          CALL SWAP1(MES,NBYTE2,MES)
	ELSE
	  DO I=1,IMAX
	    II=II+1
	    IY=II/NX
	    IX=II-IY*NX
	    IY=IY+1
	    CALL CODE_32BITS(ARRAY(IX,IY),MES,NVAC,BSCALE,BZERO)
	  END DO
C Swap the bytes (to correct Vax inversion...)
C Only SWAP1 is necessary for VAX and DEC3100: 
          CALL SWAP1(MES,NBYTE2,MES)
C Only SWAP2 is necessary for SUN, IBM, HP... 
C          CALL SWAP2(MES,NBYTE2,MES)
	ENDIF
 
 
C Writing the next record (IDATA):
        CALL FITS_WRREC(ISTATUS)
	IF(ISTATUS.NE.0)THEN
	  PRINT *,' FITS_WRREC: error writing the data'
	  ISTATUS=1
	  RETURN
	ENDIF
 
112	CONTINUE
 
999	RETURN
	END
C*************************************************************************
C Subroutine to code 32bit data
C*************************************************************************
	SUBROUTINE CODE_32BITS(VALUE,MES,NVAC,BSCALE,BZERO)
	REAL*4 VALUE,BSCALE,BZERO
	REAL*4 X1,X2
	INTEGER*2 MES(*),IAA(2)
	INTEGER*4 NVAC,IA
	EQUIVALENCE(IA,IAA(1))
 
C Decode:
c            X1=FLOAT(IA2)
c              IF(X1.LT.0.) X1=X1+65536.
c            X2=FLOAT(IA1)*65536.+X1
c            VALUE=X2*BSCALE+BZERO
 
C Code:
            IA=NINT((VALUE-BZERO)/BSCALE)
 
C High weight (IAA(2) on the Vax...)
            NVAC=NVAC+1
            MES(NVAC)=IAA(2)
 
C Low weight (IAA(1) on the Vax...)
            NVAC=NVAC+1
            MES(NVAC)=IAA(1)
 
	RETURN
	END
C*************************************************************************
C Subroutine to code 16bit data
C*************************************************************************
	SUBROUTINE CODE_16BITS(VALUE,MES,NVAC,BSCALE,BZERO)
	REAL*4 VALUE,BSCALE,BZERO
	REAL*4 X1
	INTEGER*2 MES(*)
	INTEGER*4 NVAC,IA1
 
        X1=(VALUE-BZERO)/BSCALE
        X1=MIN(X1,32767.)
        X1=MAX(X1,-32767.)
C Read 2 bytes:
	NVAC=NVAC+1
        MES(NVAC)=NINT(X1)
 
	RETURN
	END
C*****************************************************************************
C SWAP1
C Swap the two bytes of integer*2 values.
C
C IN   : Input array
C OUT  : Output array
C NCAR : Number of elements to swap
c************************************************************
        SUBROUTINE SWAP1(IN, NCAR, OUT)
        INTEGER*2 IN(*), OUT(*), AUXI2
	INTEGER*4 NCAR
        LOGICAL*1 LOW, AUXL1(2)
        EQUIVALENCE (AUXI2, AUXL1(1))
 
        DO I=1, NCAR
          AUXI2=IN(I)
          LOW  =AUXL1(1)
C Swap
          AUXL1(1)=AUXL1(2)
          AUXL1(2)=LOW
          OUT(I)  =AUXI2
        END DO
 
	RETURN
        END
C*****************************************************************************
C SWAP2
C Swap the two couple of bytes of integer*4 values.
C
C IN   : Input array
C OUT  : Output array
C NCAR : Number of elements to swap
c************************************************************
        SUBROUTINE SWAP2(IN, NCAR, OUT)
        INTEGER*2 IN(*), OUT(*), AUXI2
	INTEGER*4 NCAR
 
        DO I=1, NCAR,2
          AUXI2=IN(I)
C Swap
	  OUT(I) = IN(I+1)
          OUT(I+1)  =AUXI2
        END DO
 
	RETURN
        END
C******************************************************************
C FITS_WRREC
C To write a logical record of 2880 bytes
C******************************************************************
	SUBROUTINE FITS_WRREC(ISTATUS)
	PARAMETER (NBYTE=2880,NBYTE2=1440)
	LOGICAL*1 IDATA(NBYTE)
	INTEGER*4 ITAPE,IFID,ISTATUS
	INTEGER*2 IERROR,ISKIP,NCAR
	CHARACTER MT_DEVICE*20
	COMMON/FITS11/ITAPE,IFID,MT_DEVICE
	COMMON/FITS12/IDATA
	ISTATUS=0
	
	IF(ITAPE.EQ.0)THEN
	    CALL JLP_OSDWRITE(IFID,IDATA,NBYTE,ISTAT)
	    IF(ISTAT.EQ.-1)GOTO 99
	ELSE
	  NCAR=NBYTE
	  CALL MTACCESS(IFID,4,IDATA,NCAR,ISKIP,IERROR,MT_DEVICE)
	  IF (IERROR.NE.0) THEN
	    PRINT *,' FITS_WRREC: error when writing the data'
	    STOP
  	  END IF
	ENDIF
	
	RETURN
99	PRINT *,' FITS_WRREC: error when writing a record to file'
	ISTATUS=1
	RETURN
	END
 
C******************************************************************
C FITS_WRCLOSE
C To close the file on the disk, or write an EOF on the tape
C******************************************************************
	SUBROUTINE FITS_WRCLOSE
	PARAMETER (NBYTE=2880,NBYTE2=1440)
        LOGICAL*1 IDATA(NBYTE)
        CHARACTER MT_DEVICE*20
	INTEGER*4 ITAPE,IFID,NPOSI
	INTEGER*2 IERROR,ISKIP,NCAR
	COMMON/FITS11/ITAPE,IFID,MT_DEVICE
	COMMON/FITS12/IDATA
 
	IF(ITAPE.EQ.0)THEN
	   CALL JLP_OSDCLOSE(IFID,ISTAT)
	ELSE
C Writing EOF:
	  NCAR=NBYTE
	  CALL MTACCESS(IFID,6,IDATA,NCAR,ISKIP,IERROR,MT_DEVICE)
	  IF (IERROR.NE.0) THEN
	    PRINT *,' FITS_WRCLOSE: error when writing EOF'
	    STOP
  	  END IF
	ENDIF
 
	RETURN
	END
C******************************************************************
C FITS_WROPEN
C To open the file on the disk, or go to the location on the tape
C******************************************************************
	SUBROUTINE FITS_WROPEN(OUTFRAME,NFILE,FIRST_CALL,
     1	NPOSI,ISTATUS)
	PARAMETER (NBYTE=2880,NBYTE2=1440)
        LOGICAL*1 IDATA(NBYTE)
        CHARACTER OUTFRAME*60,MT_DEVICE*20
	INTEGER*4 ITAPE,IFID,FIRST_CALL,NPOSI,IMODE
	INTEGER*2 IERROR,ISKIP,NCAR
	COMMON/FITS11/ITAPE,IFID,MT_DEVICE
	COMMON/FITS12/IDATA
	ISTATUS=0
 
	IF(ITAPE.EQ.0)THEN
	   IMODE=1
	   CALL JLP_OSDOPEN(OUTFRAME,60,IMODE,IFID,ISTAT)
	   IF(ISTAT.EQ.-1)GOTO 99
 
	ELSE
 
	 IF(FIRST_CALL.EQ.1)THEN
C Getting the unit number IFID from the system when mounting the tape:
	  NCAR=NBYTE
	  CALL MTACCESS(IFID,5,IDATA,NCAR,ISKIP,IERROR,MT_DEVICE)
	  IF (IERROR.NE.0) THEN
	    PRINT *,' FITS_WROPEN: fatal error mounting the tape'
	    STOP
  	  END IF
C Rewinding the tape:
	  CALL MTACCESS(IFID,1,IDATA,NCAR,ISKIP,IERROR,MT_DEVICE)
	  IF (IERROR.NE.0) THEN
	    PRINT *,' FITS_WROPEN: fatal error rewinding the tape'
	    STOP
  	  END IF
	  NPOSI=1
	 ENDIF
 
	  IF(NFILE.GE.1)THEN
	    PRINT *,' FITS_WROPEN: going to write in position #',NFILE
	    ISKIP=NFILE-NPOSI
	    IF(ISKIP.GT.0)THEN
	      NCAR=NBYTE
	      CALL MTACCESS(IFID,3,IDATA,NCAR,ISKIP,IERROR,MT_DEVICE)
	      IF (IERROR.NE.0) THEN
	        PRINT *,' FITS_WROPEN: error when skipping to this position'
	        STOP
  	      END IF
	     NPOSI=NFILE
	    ENDIF
	  ELSE
C Going to the right position (skipping all the files already on the tape):
	    PRINT *,' FITS_WROPEN: going to end of tape'
	   DO I=1,1000000
	     ISKIP=1
	     NCAR=NBYTE
	     CALL MTACCESS(IFID,3,IDATA,NCAR,ISKIP,IERROR,MT_DEVICE)
	     IF (NCAR.EQ.0) GOTO 1200
	     IF (IERROR.NE.0) THEN
	        PRINT *,' FITS_WROPEN: error when going to end of tape'
	        STOP
  	     END IF
	   END DO
1200	   NPOSI=I
	   NFILE=NPOSI
	   PRINT 63,NFILE-1
63	   FORMAT(' FITS_WROPEN: This tape contains :',I3,' files')
 
	  ENDIF
 
	ENDIF
	
	RETURN
99	PRINT *,' FITS_WROPEN: unable to open file: ',OUTFRAME(1:30)
	ISTATUS=1
	RETURN
	END
C******************************************************************
C FITS_WREND
C To close the last file on the disk, or rewind the tape
C******************************************************************
	SUBROUTINE FITS_WREND
	PARAMETER (NBYTE=2880,NBYTE2=1440)
        LOGICAL*1 IDATA(NBYTE)
        CHARACTER MT_DEVICE*20
	INTEGER*4 ITAPE,IFID,FIRST_CALL,NPOSI,NBYT_BLOCK
	INTEGER*2 IERROR,ISKIP,NCAR
	COMMON/FITS11/ITAPE,IFID,MT_DEVICE
	COMMON/FITS12/IDATA
 
	IF(ITAPE.EQ.0)THEN
	   CALL JLP_OSDCLOSE(IFID,ISTAT)
	ELSE
C Rewinding the tape
	  NCAR=NBYTE
	  CALL MTACCESS(IFID,1,IDATA,NCAR,ISKIP,IERROR,MT_DEVICE)
	  IF (IERROR.NE.0) THEN
	    PRINT *,' FITS_RDCLOSE: fatal error when rewinding the tape'
            CALL JLP_END
	    STOP
  	  END IF
	ENDIF
 
	RETURN
	END
C*************************************************************************
C	include 'jlpsub:mtaccess.for'
C*************************************************************************
C CETTE ROUTINE TRAVAILLE SUR LES BANDES MAGNETIQUES EN
C MODE PHYSIQUE.
C OPERATIONS:
C		IFUNC=1 --> REMBOBINAGE
C		IFUNC=2 --> LECTURE D'UN ENREGISTREMENT
C		IFUNC=3 --> SKIP DE "ISKIP" FICHIERS (TAPE MARK)
C		IFUNC=4 --> ECRITURE D'UN ENREGISTREMENT
C		IFUNC=5 --> ASSIGNATION D'UN LUN A UN "LOGICAL"
C		IFUNC=6 --> ECRITURE FIN DE FICHIER (TAPE MARK)
C*************************************************************************
C Debug
C JLP93:
C------------------------------------------------------------------------
C Subroutine JLP_DIRECTORY to look for the files which correspond to
C the generic name INPUT (for example 012*.bdf)
C When only one file is found, the input name is set to the name of this file.
C
C Input:
C INPUT name of the files to look for
C QUIET flag, 1=true when the names are not displayed on the terminal
C
C Output:
C NUMBER number of files found
C INPUT if only one file has been found
C in common block, also:
C OUTPUT list of all the files which have been found
C------------------------------------------------------------------------
	SUBROUTINE JLP_DIRECTORY(INPUT,NUMBER,QUIET)
	PARAMETER (IDIM2=500)
	INTEGER*4 QUIET
	CHARACTER*(*) INPUT
C !! Leave it at 60, please...
	CHARACTER*60 RESULT,OUTPUT(IDIM2)
	CHARACTER*120 OUTPUT1
	INTEGER*4 CONTEXT,NUMBER,STATUS
	DATA LIB$_FOUND/65537/
	COMMON /JLP_DIRECT/OUTPUT
 
	NUMBER=0
	CONTEXT=0
	STATUS=LIB$_FOUND
 
94	IF (STATUS.NE.LIB$_FOUND) GOTO 95
C Look for the file name:
	  STATUS=LIB$FIND_FILE
	1	(%DESCR(INPUT),%DESCR(RESULT),%REF(CONTEXT))
C	1(%DESCR(INPUT),%DESCR(RESULT),%REF(CONTEXT),%DESCR('TOTO.*;0'))
	  IF(STATUS.EQ.LIB$_FOUND) THEN
C When QUIET, the names are not displayed on the terminal:
	    IF(QUIET.EQ.0)WRITE(6,*) ' ',RESULT(1:60)
	    NUMBER=NUMBER+1
	    OUTPUT(NUMBER)=RESULT(1:60)
	  ENDIF
	GOTO 94
 
C End of the session:
95	CALL LIB$FIND_FILE_END(%REF(CONTEXT))
 
C When only one file has been found, the input name is modified, and
C set to the name of this file:
	IF(NUMBER.EQ.1)THEN
C Generate the output name:
	    IF(INDEX(INPUT,']').NE.0)THEN
	       OUTPUT1=INPUT(:INDEX(INPUT,']'))
	1	//OUTPUT(1)(INDEX(OUTPUT(1),']')+1:)
	    ELSE
	       OUTPUT1=OUTPUT(1)(INDEX(OUTPUT(1),']')+1:)
	    ENDIF
	    IF(INDEX(INPUT,';').EQ.0)OUTPUT1=OUTPUT1(:INDEX(OUTPUT1,';')-1)
	    INPUT=OUTPUT1(1:LEN(INPUT))
	ENDIF
	
	RETURN
	END
