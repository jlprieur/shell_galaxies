C++*************************************************************************
C Program FITS_READ
C
C To read FITS files on disk or on tape.
C Allowed format: integers with 16 or 32 bits/pixels.
C When the disk file is not standard and that the length of the blocks is
C smaller than the length of the logical records (2880 bytes), the program
C makes an automatic determination of the block size.
C For tapes blocking factor option is allowed.
C (200 files is the maximum allowed by this program).
C
C Wild cards are allowed for the input/output files. But in that case
C the input files should be homogenous (i.e. same block size).
C
C Lists are allowed for the sequence numbers (but they should be
C sorted in increasing order). In that case use a wild card in the output name.
C
C
C SYNTAX:
c (for disk)
C   RUNS FITS_READ [D for disk] input_fits_file output_image [Y/N: talkative?]
C
C or (tape)
C   RUNS FITS_READ [T for tape] block_fact device sequence_number output_image
C           [Y/N: talkative?]
C
C Examples:
C   RUNS FITS_READ D test.mt test.bdf N
C   RUNS FITS_READ D *.mt *.bdf N
C   RUNS FITS_READ T 1 MTA0: 11 test.bdf N
C   RUNS FITS_READ T 1 MTA0: 1,2,5-11,30 test*.mt N
C     (last example: output will be test001.mt, test002.mt, test005.mt, etc)
C
C JLP
C Version 27-11-90
C--*************************************************************************
C From LECFITS and LECFITDISK_2880 (M. Cailloux)
C***************************************************************************
        PROGRAM FITS_READ
	PARAMETER (NBYTE=2880,NBYTE1=28800)
        LOGICAL*1 IDATA(NBYTE),BUFFER(NBYTE1)
	INTEGER*4 QUIET
        REAL*4 BSCALE,BZERO,X0,Y0,DX,DY
	INTEGER*4 ITAPE,INDEX_BUF,NFILES,NFIL(200),IBLOCKING
        INTEGER*4 NX,NY,NZ,NBITS,ITALK,FIRST_CALL,NPOSI,NBYT_BLOCK
	INTEGER*4 IFID,ISIZE,PNT_OUT
	CHARACTER MT_DEVICE*20
        CHARACTER OUTFRAME*60,COMMENTS*80,INFRAME*60,OBJECT*30,ANS*1
	CHARACTER OUTFRAM(200)*60,INFRAM(100)*60,OUTFRM*60,ANSWER*80
	CHARACTER EXTEN*2
	INTEGER*4 MADRID(1)
	COMMON /VMR/MADRID
	COMMON/FITS11/ITAPE,IFID,MT_DEVICE,INDEX_BUF,NBYT_BLOCK
	COMMON/FITS12/IDATA,BUFFER
C Common block with JLP_DIRECTORY:
	COMMON/JLP_DIRECT/INFRAM
 
10      FORMAT(A)
        CALL JLP_BEGIN
        CALL JLP_INQUIFMT
C
        WRITE(6,11)
11      FORMAT(' Program FITS_READ Version 27-11-90',
C     1	' UNIX Version (no swap)')
     1	' VAX and DEC3100 Version (swap)')
 
C************************************************************************
C Input of the options from the user:
C************************************************************************
	WRITE(6,*) ' File on disk (D) or on tape (T) ?'
	READ(5,10) ANS
	ITAPE=0
	IF(ANS.EQ.'T'.OR.ANS.EQ.'t')ITAPE=1
 
	IF(ITAPE.EQ.0)THEN
          WRITE(6,*) ' Input FITS file:',
     1	' (if more than one, put wild card "*.mt")'
          READ(5,10) INFRAME
C	  QUIET=1
	  QUIET=0
	  CALL JLP_DIRECTORY(INFRAME,NFILES,QUIET)
 
	ELSE
 
	  PRINT 25
25	  FORMAT(' Input magnetic tape device : (MUA0:, MTA1:,...)')
	  READ(5,10) MT_DEVICE
39	  PRINT *,' Blocking factor? (usual is 1, max allowed here is 10)'
	  READ(5,*) IBLOCKING
	  IF(IBLOCKING.LT.0.OR.IBLOCKING.GT.10)GOTO 39
	  NBYT_BLOCK=IBLOCKING*2880
	  PRINT *,' Sequence number(s) of the file(s) you want to read'
	  PRINT *,'      Example:   1,2,5-11,35 '
	  READ(5,10) ANSWER
C Decode the answer and computes the total number of files (NFILES)
	  CALL FITS_NFILES(ANSWER,NFIL,NFILES)
 
	ENDIF
 
C Name of the output file:
        WRITE(6,*) ' Output file: ',
     1	'(if more than one, put wild card "suff*.bdf")'
        READ(5,10) OUTFRAME
 
C Reading the header:
	WRITE(6,*) ' Do you want to see all the details of the header?[Y]'
	READ(5,10) ANS
	ITALK=1
	IF((ANS.EQ.'N').OR.(ANS.EQ.'n')) ITALK=0
 
C************************************************************************
C Preparing the main loop:
C************************************************************************
 
C Generating the output names:
	IF(NFILES.GT.1.OR.INDEX(OUTFRAME,'*').GE.1) THEN
	  IF(ITAPE.EQ.0)THEN
	    CALL JLP_NFRAME1(OUTFRAME,OUTFRAM,INFRAM,NFILES)
	  ELSE
	    CALL JLP_NFRAME2(OUTFRAME,OUTFRAM,NFIL,NFILES)
	  ENDIF
	ELSE
	  OUTFRAM(1)=OUTFRAME
	ENDIF
 
	DO I=1,NFILES
	  PRINT *,' OUTFRAM (I) :== ',OUTFRAM(I)
	END DO
 
C Check if standard format and returns block size:
	IF(ITAPE.EQ.0) CALL FITS_BLSIZE(INFRAM(1))
 
C NPOSI, position in the tape (file position)
	NPOSI=0
C
	FIRST_CALL=1
 
C************************************************************************
C Loop on all the files:
C************************************************************************
 
	DO 245 IFIL=1,NFILES
 
C Message to the user:
	IF(ITAPE.EQ.0)THEN
	  PRINT 46,INFRAM(IFIL)
46	  FORMAT(/,' Now reading file: ',A)
	ELSE
	  PRINT 47,NFIL(IFIL)
47	  FORMAT(/,' Now reading file # ',I3)
	ENDIF
 
C Open the new file, or go to the right position on the tape:
	CALL FITS_RDOPEN(INFRAM(IFIL),NFIL(IFIL),FIRST_CALL,
     1	NPOSI,ISTATUS)
	FIRST_CALL=0
	IF(ISTATUS.NE.0)GOTO 245
 
C Reads the header:
        CALL FITS_RDHEADER(NBITS,OBJECT,NX,NY,NZ,DX,DY,X0,Y0,
     1	BSCALE,BZERO,ITALK,ISTATUS)
 
C Output of some parameters for the user:
206     WRITE(6,133) NBITS,OBJECT,NX,NY,NZ,DX,DY,X0,Y0,BSCALE,BZERO
133      FORMAT(/,'-------------------------------------------------',/,
	1       ' Nber of bits/pixel:',I5,/,
	1       ' Object:',A30,/,
	1       ' NX, NY, NZ:',I5,X,I5,X,I5,/,
	1       ' DX, DY:',G12.5,X,G12.5,/,
	1       ' X0, Y0:',G12.5,X,G12.5,/,
	1       ' BSCALE, BZERO:',G12.5,X,G12.5,/,
	1       ' -------------------------------------------------')
	IF(ISTATUS.NE.0)GOTO 245
 
C Test NZ:
	IF(NZ.GT.99)
     1	PRINT *,' Warning: only 99 image planes allowed here (NZ)'
 
C If the overall size is 0:
	IF(NX.EQ.0)THEN
	  PRINT *,' No primary data matrix'
	  GOTO 245
	ENDIF
 
C Check if new format with real values:
	IF(NBITS.EQ.-32.OR.NBITS.EQ.-64)THEN
	  PRINT *,' Sorry, real*4 or real*8 not available yet'
	  GOTO 245
	ENDIF
 
C Check if 16 or 32 bits:
	IF(NBITS.NE.16.AND.NBITS.NE.32)THEN
	  PRINT *,' Sorry, only 16 and 32 bits/pixel are possible here'
	  GOTO 245
	ENDIF
 
C****************************************************************************
C Getting virtual memory for OUT:
	  ISIZE=NX*NY*4
	  CALL JLP_GETVM(PNT_OUT,ISIZE)
 
C Loop on the number of images:
	DO KZ=1,NZ
 
C Now reading the data:
	  CALL FITS_RDDATA(MADRID(PNT_OUT),NX,NY,NX,NBITS,
     1	BSCALE,BZERO,ISTATUS)
 
C Leave this as a comment since it is better to save what can be saved:
C	  IF(ISTATUS.NE.0)GOTO 245
 
C**********************************************************************
C Output file:
200       WRITE(COMMENTS,10) OBJECT
	  OUTFRM=OUTFRAM(IFIL)
	  IF(NZ.GT.1)THEN
	    K1=KZ/10
	    K2=KZ-K1*10
	    WRITE(EXTEN,38)K1,K2
38	    FORMAT(I1,I1)
	    OUTFRM=OUTFRAM(IFIL)
	    OUTFRM(60:60)=' '
	    I1=INDEX(OUTFRM,'.')
	    IF(I1.LE.1)I1=INDEX(OUTFRM,' ')
	    OUTFRM=OUTFRM(1:I1-1)//EXTEN//OUTFRM(I1:58)
	    PRINT *,' OUTFRM:',OUTFRM
	  ENDIF
 
	  CALL JLP_WRITEIMAG(MADRID(PNT_OUT),NX,NY,NX,OUTFRM,
     1	COMMENTS)
	END DO
	CALL JLP_FREEVM(PNT_OUT,ISIZE)
 
245	CONTINUE
 
 
C Rewinding back the tape or closing the last file:
	CALL FITS_RDEND
 
        CALL JLP_END
        STOP
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
 
        DO I=1,NCAR
          AUXI2=IN(I)
          LOW=AUXL1(1)
C Swap
          AUXL1(1)=AUXL1(2)
          AUXL1(2)=LOW
          OUT(I)=AUXI2
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
 
        DO I=1,NCAR,2
          AUXI2=IN(I)
	  OUT(I)=IN(I+1)
          OUT(I+1) =AUXI2
        END DO
 
	RETURN
        END
C*************************************************************************
C FITS_RDHEADER
C Subroutine to read the header of a FITS formatted file
C*************************************************************************
        SUBROUTINE FITS_RDHEADER(NBITS,OBJECT,NX,NY,NZ,DX,DY,X0,Y0,
     1	BSCALE,BZERO,ITALK,ISTATUS)
	PARAMETER (NBYTE=2880,NBYTE1=28800)
        LOGICAL*1 IDATA(NBYTE),BUFFER(NBYTE1)
	INTEGER*4 IEXTEND,IBLOCKED,LAST_REC
	INTEGER*4 IN_DESCR,OUT_DESCR,IC2,IC1
        CHARACTER WORD*8,OBJECT*30
        INTEGER*4 NAXIS,NX,NY,NZ,NBITS,ITALK,ISTATUS
        REAL*4 X0,Y0
        CHARACTER STRING*81,CDESCR*500
	COMMON/FITS12/IDATA,BUFFER
	COMMON/JLP_DESCRIPTORS/CDESCR,IN_DESCR,OUT_DESCR
 
10      FORMAT(A)
	ISTATUS=0
 
	IEXTEND=0
	IBLOCKED=0
C Initialisation:
        BZERO=0.
        BSCALE=1.
        X0=0.
        Y0=0.
        DX=1.
        DY=1.
	NX=0
	NY=1
	NZ=1
C Indices for JLP descriptors:
	IC1=1
	IC2=1
 
C Main loop:
	LAST_REC=0
	DO 18 IREC=1,10000000
 
C Reading the next logical record (IDATA)
	  CALL FITS_RDREC(LAST_REC,ISTATUS)
	  IF(ISTATUS.NE.0) RETURN
 
C Reading all the lines from this record (max 36 since 36x80=2880):
	JMAX=36
        DO 17 J=1,JMAX
 
C Pointer to the first, last elements:
        IDEB=(J-1)*80+1
        IFIN=IDEB+79
 
C Displays the string:
          WRITE(STRING,203,ERR=204) (IDATA(K),K=IDEB,IFIN)
203       FORMAT(X,80A)
          IF(ITALK.EQ.1) WRITE(6,*) STRING(1:80)
 
C Reads the keyword:
          WORD=STRING(2:7)
 
C First line: is that compatible with FITS or not?
        IF(J.EQ.1.AND.IREC.EQ.1) THEN
          IF(WORD(1:6).NE.'SIMPLE') THEN
	   PRINT *,' FITS_RDHEADER: This file is not written in FITS format'
	   ISTATUS=1
	   RETURN
	  ENDIF
        ENDIF
 
C Exit from this loop when END has been found:
          IF(WORD(1:3).EQ.'END') GO TO 999
C
C IDEB1 is the position of the first digit after "="
	IDEB1=11
C IDEB2 is the position of the last digit before "/"
	IDEB2=INDEX(STRING(IDEB1:80),'/')+IDEB1-2
        IDEB2=MAX(IDEB1,IDEB2)
C Debug:
C	PRINT *,' IDEB1, IDEB2',IDEB1,IDEB2
C	PRINT *,' STRING(IDEB2+1)',STRING(IDEB2+1:IDEB2+1)
C	PRINT *,' STRING TO READ:','>',STRING(IDEB1:IDEB2),'<'
C
	IF(WORD(1:6).EQ.'BITPIX') READ(STRING(IDEB1:IDEB2),*) NBITS
C Axis:
        IF(WORD(1:6).EQ.'NAXIS1') THEN
	  READ(STRING(IDEB1:IDEB2),*) NX
        ELSEIF(WORD(1:6).EQ.'NAXIS2') THEN
	  READ(STRING(IDEB1:IDEB2),*) NY
        ELSEIF(WORD(1:6).EQ.'NAXIS3') THEN
	  READ(STRING(IDEB1:IDEB2),*,ERR=205) NZ
          GOTO 209
205	  PRINT *,' Syntax error with NAXIS3'
          NZ=1
209	  CONTINUE
        ELSEIF(WORD(1:5).EQ.'NAXIS')THEN
	  READ(STRING(IDEB1:IDEB2),*) NAXIS
          IF(NAXIS.GT.3) THEN
	   PRINT *,' NAXIS = ',NAXIS
	   PRINT *,' Sorry, this program does not allow NAXIS > 3 '
	   ISTATUS=1
	   RETURN
	  ENDIF
	ENDIF
C
        IF(WORD(1:6).EQ.'OBJECT') READ(STRING(IDEB1:IDEB2),'(A)') OBJECT
        IF(WORD(1:6).EQ.'BSCALE') READ(STRING(IDEB1:IDEB2),*) BSCALE
        IF(WORD(1:5).EQ.'BZERO')  READ(STRING(IDEB1:IDEB2),*) BZERO
        IF(WORD(1:6).EQ.'CRVAL1') READ(STRING(IDEB1:IDEB2),*) X0
        IF(WORD(1:6).EQ.'CDELT1') READ(STRING(IDEB1:IDEB2),*) DX
        IF(WORD(1:6).EQ.'CRVAL2') READ(STRING(IDEB1:IDEB2),*) Y0
        IF(WORD(1:6).EQ.'CDELT2') READ(STRING(IDEB1:IDEB2),*) DY
        IF((WORD(1:6).EQ.'EXTEND').AND.(STRING(30:30).EQ.'T')) IEXTEND=1
C JLP91
C There is always a blank after the "="
C ie: JLPDSC = Now I start jjklfh lfhljhf ljh lkjhl/
        IF(WORD(1:6).EQ.'JLPDSC') THEN
	   OUT_DESCR=1
	   IC2=IC1+IDEB2-IDEB1-1
	   IF(IC2.LT.500)THEN
	     READ(STRING(IDEB1+1:IDEB2),10) CDESCR(IC1:IC2) 
	   ENDIF
	   IC1=IC2+1
        ENDIF

C
C Goes to the next line
17      CONTINUE
C Goes to the next record
18      CONTINUE
 
999	RETURN
204     PRINT *,' Error reading Header'
        WRITE(6,ERR=204) (IDATA(K),K=IDEB,IFIN)
        RETURN
	END
C*************************************************************************
C FITS_RDDATA
C Subroutine to read the data of a FITS formatted file
C*************************************************************************
	SUBROUTINE FITS_RDDATA(OUT,NX,NY,IDIM,NBITS,BSCALE,
     1	BZERO,ISTATUS)
	PARAMETER (NBYTE=2880,NBYTE1=28800,NBYTE2=1440)
        LOGICAL*1 IDATA(NBYTE),BUFFER(NBYTE1)
        INTEGER*2 MES(NBYTE2)
	REAL*4 OUT(IDIM,*),BSCALE,BZERO
        INTEGER*4 NX,NY,NBITS
        EQUIVALENCE (IDATA(1),MES(1))
	COMMON/FITS12/IDATA,BUFFER
 
10      FORMAT(A)
	ISTATUS=0
 
C Index for current pixel of the image:
	II=0
	IIMAX=NX*NY
	NRECORDS=(NX*NY*NBITS/8+2880-1)/2880
C Reset the flag for last record (to stop reading too far in the file)
	LAST_REC=0
 
	DO 112 IREC=1,NRECORDS
	  IF(IREC.EQ.NRECORDS)LAST_REC=1
C Reading the next record (IDATA):
	  CALL FITS_RDREC(LAST_REC,ISTATUS)
C Leave this commented to save as much as possible:
C	  IF(ISTATUS.NE.0) RETURN
 
C Computing the position of the last value to be read in that record:
	  IMAX=2880*8/NBITS
	  ITEST=IIMAX-II
	  IMAX=MIN(IMAX,ITEST)
 
C Decoding the values up to this limit:
          NVAC=0
	  IF(NBITS.EQ.16)THEN
C Swap the bytes (to correct Vax inversion...: 
C NECESSARY FOR VAX, AND DEC3100: 
            CALL SWAP1(MES,1440,MES)
 	
	    DO I=1,IMAX
	      II=II+1
	      IY=1+(II-1)/NX
	      IX=II-(IY-1)*NX
	      CALL DECODE_16BITS(OUT(IX,IY),MES,NVAC,BSCALE,BZERO)
	    END DO
	  ELSE
C Swap the bytes (to correct Vax inversion...: 
C NECESSARY FOR VAX, AND DEC3100: 
            CALL SWAP1(MES,1440,MES)
C NOTA:  NO CALL OF SWAP2, EVEN FOR SUN, HP, IBM,...
 	
	    DO I=1,IMAX
	      II=II+1
	      IY=1+(II-1)/NX
	      IX=II-(IY-1)*NX
	      CALL DECODE_32BITS(OUT(IX,IY),MES,NVAC,BSCALE,BZERO)
	    END DO
	  ENDIF
 
112	CONTINUE
 
999	RETURN
	END
C*************************************************************************
C Subroutine to decode 32bit data
C*************************************************************************
	SUBROUTINE DECODE_32BITS(VALUE,MES,NVAC,BSCALE,BZERO)
	REAL*4 VALUE,BSCALE,BZERO
	REAL*4 X1,X2
	INTEGER*2 MES(*)
	INTEGER*4 NVAC,IA1,IA2
 
C Read 4 bytes:
          NVAC=NVAC+1
          IA1=MES(NVAC)
          NVAC=NVAC+1
          IA2=MES(NVAC)
C Decode:
          X1=FLOAT(IA2)
            IF(X1.LT.0.) X1=X1+65536.
          X2=FLOAT(IA1)*65536.+X1
          VALUE=X2*BSCALE+BZERO
 
	RETURN
	END
C*************************************************************************
C Subroutine to decode 16bit data
C*************************************************************************
	SUBROUTINE DECODE_16BITS(VALUE,MES,NVAC,BSCALE,BZERO)
	REAL*4 VALUE,BSCALE,BZERO
	REAL*4 X1
	INTEGER*2 MES(*)
	INTEGER*4 NVAC
 
C Read 2 bytes:
          NVAC=NVAC+1
          X1=FLOAT(MES(NVAC))
C Decode:
          VALUE=X1*BSCALE+BZERO
 
	RETURN
	END
C******************************************************************
C FITS_RDREC
C To read a logical record of 2880 bytes
C******************************************************************
	SUBROUTINE FITS_RDREC(LAST_REC,ISTATUS)
	PARAMETER (NBYTE=2880,NBYTE1=28800)
        LOGICAL*1 IDATA(NBYTE),BUFFER(NBYTE1)
	INTEGER*4 ITAPE,IFID,NBYT_BLOCK,LAST_REC,ISTATUS
	INTEGER*2 IERROR,ISKIP
	CHARACTER MT_DEVICE*20
	COMMON/FITS11/ITAPE,IFID,MT_DEVICE,INDEX_BUF,NBYT_BLOCK
	COMMON/FITS12/IDATA,BUFFER
	ISTATUS=0
	
	IF(ITAPE.EQ.0)THEN
	 CALL FITS_RDRECF(LAST_REC,ISTATUS)
	ELSE
	 CALL FITS_RDRECT(LAST_REC,ISTATUS)
	ENDIF
	
	RETURN
	END
 
C******************************************************************
C FITS_RDOPEN
C To open the file on the disk, or go to the location on the tape
C******************************************************************
	SUBROUTINE FITS_RDOPEN(INFRAME,NFILE,FIRST_CALL,
     1	NPOSI,ISTATUS)
	PARAMETER (NBYTE=2880,NBYTE1=28800)
        LOGICAL*1 IDATA(NBYTE),BUFFER(NBYTE1)
        CHARACTER INFRAME*60,MT_DEVICE*20
	INTEGER*4 ITAPE,FIRST_CALL,NPOSI,INDEX_BUF,NBYT_BLOCK
	INTEGER*4 NFILE,IMODE,IFID,ISTAT
	INTEGER*2 IERROR,ISKIP,NCAR
	COMMON/FITS11/ITAPE,IFID,MT_DEVICE,INDEX_BUF,NBYT_BLOCK
	COMMON/FITS12/IDATA,BUFFER
 
	ISTATUS=0
 
	IF(ITAPE.EQ.0)THEN
	  IF(FIRST_CALL.NE.1) CALL JLP_OSDCLOSE(IFID,ISTAT)
 
C Reset INDEX_BUF, last value not copied from the buffer
	  INDEX_BUF=2880+1
 
C Open the file:
C in read mode:
	     IMODE=0
	     LENGTH=60
	     CALL JLP_OSDOPEN(INFRAME,LENGTH,IMODE,IFID,ISTAT)
	     IF (ISTAT.EQ.-1) GOTO 99
 
	ELSE
 
	 IF(FIRST_CALL.EQ.1)THEN
C Getting the unit number IFID from the system when mounting the tape:
	   NCAR=NBYT_BLOCK
	   CALL MTACCESS(IFID,5,IDATA,NCAR,ISKIP,IERROR,MT_DEVICE)
	   IF (IERROR.NE.0) THEN
	     PRINT *,' FITS_RDOPEN: fatal error mounting the tape'
	     STOP
  	   END IF
C Rewinding the tape:
	   CALL MTACCESS(IFID,1,IDATA,NCAR,ISKIP,IERROR,MT_DEVICE)
	   IF (IERROR.NE.0) THEN
	     PRINT *,' FITS_RDOPEN: fatal error rewinding the tape'
	     STOP
  	   END IF
	   NPOSI=1
	 ENDIF
 
C Going to the right position:
	  ISKIP=NFILE-NPOSI
	  IF(ISKIP.GT.0)THEN
	    CALL MTACCESS(IFID,3,IDATA,NCAR,ISKIP,IERROR,MT_DEVICE)
	    IF (IERROR.NE.0) THEN
	      PRINT *,' FITS_RDOPEN: fatal error when skipping',
     1	' to this file'
              CALL JLP_END
	      STOP
  	    END IF
	    NPOSI=NFILE
	  ENDIF
 
C Reset INDEX_BUF, last value not copied from the buffer
	INDEX_BUF=NBYT_BLOCK+1
	ENDIF
	
	RETURN
99	PRINT *,' FITS_RDOPEN: Unable to open:',INFRAME
	ISTATUS=1
	RETURN
	END
C******************************************************************
C FITS_RDEND
C To close the last file on the disk, or rewind the tape
C******************************************************************
	SUBROUTINE FITS_RDEND
	PARAMETER (NBYTE=2880,NBYTE1=28800)
        LOGICAL*1 IDATA(NBYTE),BUFFER(NBYTE1)
        CHARACTER MT_DEVICE*20
	INTEGER*4 IFID,ISTAT
	INTEGER*4 ITAPE,NPOSI,INDEX_BUF,NBYT_BLOCK
	INTEGER*2 IERROR,ISKIP,NCAR
	COMMON/FITS11/ITAPE,IFID,MT_DEVICE,INDEX_BUF,NBYT_BLOCK
	COMMON/FITS12/IDATA,BUFFER
 
	IF(ITAPE.EQ.0)THEN
	   CALL JLP_OSDCLOSE(IFID,ISTAT)
	ELSE
C Rewinding the tape
	  NCAR=NBYT_BLOCK
	  CALL MTACCESS(IFID,1,IDATA,NCAR,ISKIP,IERROR,MT_DEVICE)
	  IF (IERROR.NE.0) THEN
	    PRINT *,' FITS_RDCLOSE: fatal error when rewinding the tape'
            CALL JLP_END
	    STOP
  	  END IF
	ENDIF
 
	RETURN
	END
C******************************************************************
C FITS_BLSIZE
C To check if the file on the disk is standard (2880 bytes/block)
C or find its size
C******************************************************************
	SUBROUTINE FITS_BLSIZE(INFRAME)
	PARAMETER (NBYTE=2880,NBYTE1=28800)
        LOGICAL*1 IDATA(NBYTE),BUFFER(NBYTE1)
        CHARACTER INFRAME*60,MT_DEVICE*20
	INTEGER*4 IMODE,ISTAT,INDEX_BUF,ITAPE,IFID,NBYT_BLOCK
	COMMON/FITS11/ITAPE,IFID,MT_DEVICE,INDEX_BUF,NBYT_BLOCK
	COMMON/FITS12/IDATA,BUFFER
 
C Open the file:
	   IMODE=0
	   LENGTH=60
	   CALL JLP_OSDOPEN(INFRAME,LENGTH,IMODE,IFID,ISTAT)
 
C Look for the size of the blocks (use dichotomy if not standard):
	   NBYT_BLOCK=2880
	   CALL JLP_OSDREAD(IFID,IDATA,NBYT_BLOCK,ISTAT)
	   IF(ISTAT.EQ.-1)GOTO 79
	   PRINT *,' RDOPEN : the file looks standard (2880 bytes/block)'
	   GOTO 666
79	   PRINT 67
67	   FORMAT(' FITS_RDOPEN: the file is not standard',
     1	' (2880 bytes/block)',/,
     1	' Automatic determination of the size:')
	   NN1=1
	   NN3=2880
 
93	   IF ((NN3-NN1).LE.1) GOTO 94
C	     PRINT *,' NN1,2,3',NN1,NN2,NN3
	     CALL JLP_OSDCLOSE(IFID,ISTAT)
	     IMODE=0
	     LENGTH=60
	     CALL JLP_OSDOPEN(INFRAME,LENGTH,IMODE,IFID,ISTAT)
 
C Test on NN2:
	     NN2=(NN3+NN1)/2
	     I2=0
	     CALL JLP_OSDREAD(IFID,IDATA,NN2,ISTAT)
	     IF(ISTAT.EQ.-1)GOTO 802
	     I2=1
 
802	   IF(I2.EQ.0)THEN
	    NN3=NN2
	   ELSE
	    NN1=NN2
	   ENDIF
 
           GOTO 93
94	   CONTINUE
 
C Successful exit from the loop:
	   NBYT_BLOCK=NN1
666	   PRINT *,' NBYT_BLOCK=',NBYT_BLOCK
 
	   CALL JLP_OSDCLOSE(IFID,ISTAT)
 
	RETURN
	END
C******************************************************************
C FITS_RDRECF
C To read a logical record from a file
C******************************************************************
	SUBROUTINE FITS_RDRECF(LAST_REC,ISTATUS)
	PARAMETER (NBYTE=2880,NBYTE1=28800)
        LOGICAL*1 IDATA(NBYTE),BUFFER(NBYTE1)
	INTEGER*4 INDEX_BUF,NBYT_BLOCK,IFID,ITAPE,LAST_REC
        CHARACTER MT_DEVICE*20
	COMMON/FITS11/ITAPE,IFID,MT_DEVICE,INDEX_BUF,NBYT_BLOCK
	COMMON/FITS12/IDATA,BUFFER
 
	ISTATUS=0
 
C Index for IDATA:
	II=0
 
C Empty the buffer:
	DO I=INDEX_BUF,NBYT_BLOCK
	   II=II+1
	   IDATA(II)=BUFFER(I)
	END DO
 
C Get as many blocks (minus one) as needed to fill the array:
	NBLOCK=(2880-II)/NBYT_BLOCK
	DO IB=1,NBLOCK
	  CALL JLP_OSDREAD(IFID,BUFFER,NBYT_BLOCK,ISTAT)
	  IF(ISTAT.EQ.-1)GOTO 99
C Copy the buffer:
	  DO I=1,NBYT_BLOCK
	     II=II+1
	     IDATA(II)=BUFFER(I)
	  END DO
	END DO
 
C Read the last one:
	KMAX=NBYT_BLOCK
	IMAX=2880-II
	IF(LAST_REC.EQ.1) KMAX=IMAX
C The last call should be with IMAX=0 for standard files (2880)
	IF(IMAX.GT.0)THEN
	  CALL JLP_OSDREAD(IFID,BUFFER,KMAX,ISTAT)
	  IF(ISTAT.EQ.-1)GOTO 99
C Copy the buffer:
	  DO I=1,IMAX
	     II=II+1
	     IDATA(II)=BUFFER(I)
	  END DO
 
C Set the index of the buffer
C (position of the first character which has not been copied)
	INDEX_BUF=IMAX+1
 
	ELSE
	INDEX_BUF=NBYT_BLOCK+1
 
	ENDIF
 
 
	RETURN
98	PRINT *,' FITS_RDRECF: EOF before reading all the data'
	ISTATUS=1
	RETURN
99	PRINT *,' FITS_RDRECF: Error when reading the file'
	ISTATUS=1
	RETURN
	END
C******************************************************************
C FITS_RDRECT
C To read a logical record from a tape
C******************************************************************
	SUBROUTINE FITS_RDRECT(LAST_REC,ISTATUS)
	PARAMETER (NBYTE=2880,NBYTE1=28800)
        LOGICAL*1 IDATA(NBYTE),BUFFER(NBYTE1)
	INTEGER*4 INDEX_BUF,NBYT_BLOCK,ITAPE,IFID,LAST_REC
	INTEGER*2 IERROR,ISKIP,NCAR
        CHARACTER MT_DEVICE*20
	COMMON/FITS11/ITAPE,IFID,MT_DEVICE,INDEX_BUF,NBYT_BLOCK
	COMMON/FITS12/IDATA,BUFFER
 
	ISTATUS=0
 
C Index for IDATA:
	II=0
 
C Empty the buffer:
	IMAX=MIN(NBYT_BLOCK,INDEX_BUF+2879)
	DO I=INDEX_BUF,IMAX
	   II=II+1
	   IDATA(II)=BUFFER(I)
	END DO
	ILEFT=2880-II
 
C Read the tape :
	IF(ILEFT.GT.0)THEN
	    INDEX_BUF=INDEX_BUF-NBYT_BLOCK
	    NCAR=NBYT_BLOCK
	    CALL MTACCESS(IFID,2,BUFFER,NCAR,ISKIP,IERROR,MT_DEVICE)
	    IF (IERROR.LT.0.OR.IERROR.GT.1) THEN
	      PRINT *,' FITS_RDREC: error when reading the data'
	      PRINT *,' IERROR=',IERROR
	      ISTATUS=1
	      RETURN
	    ELSEIF(IERROR.EQ.1) THEN
	      PRINT *,' FITS_RDREC: EOF encountered...'
  	    END IF
C
	    IF (NCAR.LE.0) THEN
	      PRINT *,' FITS_RDREC: unexpected end of data'
	      ISTATUS=1
	      RETURN
  	    END IF
	ENDIF
 
C Copies the relevant part of BUFFER to IDATA:
	  DO I=1,ILEFT
	   II=II+1
	   IDATA(II)=BUFFER(I)
	  ENDDO
	  INDEX_BUF=INDEX_BUF+2880
 
	RETURN
	END
C*************************************************************************
C Subroutine to decode the list of files
C like 1,2,3-4,11-20,32
C*************************************************************************
	SUBROUTINE FITS_NFILES(ANSWER,NFIL,NFILES)
	CHARACTER ANSWER*80
	INTEGER*4 NFIL(*),NFILES
 
	II=0
 
	ILOW=0
	ANSWER(80:80)=','
 
87	IF (ILOW.GE.79) GOTO 89
	  IHIGH=ILOW+INDEX(ANSWER(ILOW+1:),',')
	  IDASH=ILOW+INDEX(ANSWER(ILOW+1:IHIGH),'-')
	  IF(IDASH.GT.ILOW+1) THEN
	     PRINT *,' Buff:',ANSWER(ILOW+1:IDASH-1)
	     PRINT *,' Buff:',ANSWER(IDASH+1:IHIGH-1)
	     READ(ANSWER(ILOW+1:IDASH-1),*) NMIN
	     READ(ANSWER(IDASH+1:IHIGH-1),*) NMAX
	     PRINT *,' NMIN,NMAX',NMIN,NMAX
	       DO K=NMIN,NMAX
	         II=II+1
	         NFIL(II)=K
	       END DO
	  ELSE
	     II=II+1
	     PRINT *,' Buff:',ANSWER(ILOW+1:IHIGH-1)
	     READ(ANSWER(ILOW+1:IHIGH-1),*) NFIL(II)
	  ENDIF
	  ILOW=IHIGH
	GOTO 87
89	CONTINUE
	
999	NFILES=II
 
	IF(NFILES.EQ.0) THEN
	  PRINT *,' Wrong format for the input sequence list'
	  STOP
	ENDIF
 
	RETURN
	END
C*************************************************************************
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
C	include 'jlpsub:jlp0_vax1.for'
