C++*************************************************************************
C Program FITS_CORRECT
C
C To correct FITS format files when not fully to the standard (From Pic du
C Midi, for example)
C
C Reads FITS files on disk or on tape.
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
C   RUNS FITS_CORRECT input_fits_file output_image [Y/N: talkative?]
C
C
C Examples:
C   RUNS FITS_CORRECT test.mt test.bdf N
C   RUNS FITS_CORRECT *.mt *.bdf N
C
C JLP
C Version 27-11-91
C--*************************************************************************
C From LECFITS and LECFITDISK_2880 (M. Cailloux)
C***************************************************************************
        PROGRAM FITS_CORRECT
	PARAMETER (NBYTE=2880,NBYTE1=28800)
        LOGICAL*1 IDATA(NBYTE),BUFFER(NBYTE1)
	INTEGER*4 QUIET
	INTEGER*4 INDEX_BUF,NFILES,NFIL(200)
        INTEGER*4 NX,NY,NBITS,ITALK,NBYT_BLOCK
	INTEGER*4 IFID,IFID_OUT
        CHARACTER OUTFRAME*60,INFRAME*60,OBJECT*30,ANS*1
	CHARACTER OUTFRAM(200)*60,INFRAM(100)*60,OUTFRM*60
	INTEGER*4 MADRID(1)
	COMMON /VMR/MADRID
	COMMON/FITS11/IFID,INDEX_BUF,NBYT_BLOCK
	COMMON/FITS12/IDATA,BUFFER
C Common block with JLP_DIRECTORY:
	COMMON/JLP_DIRECT/INFRAM
 
10      FORMAT(A)
        CALL JLP_BEGIN
        CALL JLP_INQUIFMT
C
        WRITE(6,11)
11      FORMAT(' Program FITS_CORRECT Version 27-11-91',
C     1	' UNIX Version (no swap)')
     1	' VAX and DEC3100 Version (swap)')
 
C************************************************************************
C Input of the options from the user:
C************************************************************************
          WRITE(6,*) ' Input FITS file:',
     1	' (if more than one, put wild card "*.mt")'
          READ(5,10) INFRAME
C	  QUIET=1
	  QUIET=0
	  CALL JLP_DIRECTORY(INFRAME,NFILES,QUIET)
 
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
	    CALL JLP_NFRAME1(OUTFRAME,OUTFRAM,INFRAM,NFILES)
	ELSE
	  OUTFRAM(1)=OUTFRAME
	ENDIF
 
	DO I=1,NFILES
	  PRINT *,' OUTFRAM (I) :== ',OUTFRAM(I)
	END DO
 
C Check if standard format and returns block size:
	CALL FITS_BLSIZE(INFRAM(1))
 
C************************************************************************
C Loop on all the files:
C************************************************************************
 
	DO 245 IFIL=1,NFILES
 
C Message to the user:
	  PRINT 46,INFRAM(IFIL)
46	  FORMAT(/,' Now reading file: ',A)
 
C Open the old FITS file:
	CALL FITS_RDOPEN(INFRAM(IFIL),NFIL(IFIL),ISTATUS)
	IF(ISTATUS.NE.0)GOTO 245
 
C Open the new FITS file:
           IMODE=1
	   OUTFRM=OUTFRAM(IFIL)
           LENGTH=60
           CALL JLP_OSDOPEN(OUTFRM,LENGTH,IMODE,IFID_OUT,ISTAT)
           IF(ISTAT.NE.0)THEN
             PRINT *,' Fatal error opening output FITS file'
             STOP
           ENDIF

C Reads the header:
        CALL FITS_RDHEADER(NBITS,OBJECT,NX,NY,IFID_OUT,ITALK,ISTATUS)
 
	CALL FITS_CPDATA(IFID_OUT,NX,NY,NBITS,ISTATUS)

C Closing files:
        CALL JLP_OSDCLOSE(IFID,ISTAT)
        IF(ISTAT.NE.0)THEN
           PRINT *,' Error closing input FITS file'
        ENDIF
        CALL JLP_OSDCLOSE(IFID_OUT,ISTAT)
        IF(ISTAT.NE.0)THEN
           PRINT *,' Error closing output FITS file'
        ENDIF


245	CONTINUE
 
 
        CALL JLP_END
        STOP
        END
C*************************************************************************
C FITS_RDHEADER
C Subroutine to read the header of a FITS formatted file
C*************************************************************************
        SUBROUTINE FITS_RDHEADER(NBITS,OBJECT,NX,NY,IFID_OUT,
     1	ITALK,ISTATUS)
	PARAMETER (NBYTE=2880,NBYTE1=28800)
        LOGICAL*1 IDATA(NBYTE),BUFFER(NBYTE1),IDATA1(NBYTE)
	INTEGER*4 LAST_REC
        CHARACTER WORD*8,OBJECT*30
        INTEGER*4 NAXIS,NX,NY,NZ,NBITS,ITALK,ISTATUS
        REAL*4 X0,Y0
        LOGICAL*1 DUMMY(80)
        CHARACTER STRING*81
        CHARACTER CDUMMY*80
	COMMON/FITS12/IDATA,BUFFER
        EQUIVALENCE (CDUMMY(1:1),DUMMY(1))
 
10      FORMAT(A)
	ISTATUS=0
        WRITE(CDUMMY,10) 'COMMENT = ''                          ''/'
        DO K=1,NBYTE
         IDATA1(K)=' '
        END DO
 
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
 
C Main loop:
	LAST_REC=0
	DO 18 IREC=1,10000000
 
C Reading the next logical record of 2880 bytes (IDATA)
	  CALL FITS_RDRECF(LAST_REC,ISTATUS)
	  IF(ISTATUS.NE.0) RETURN
 
C Reading all the lines from this record (max 36 since 36x80=2880):
	JMAX=36
        DO 17 J=1,JMAX
 
C Pointer to the first, last elements:
        IDEB=(J-1)*80+1
        IFIN=IDEB+79
        IFLAG=0
 
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
          IF(WORD(1:3).EQ.'END')THEN
C Copy 'END' to IDATA1:
          I=(J-1)*80
          DO K=IDEB,IFIN
             I=I+1
             IDATA1(I)=IDATA(K)
          END DO
          GOTO 999
          ENDIF
C
C IDEB1 is the position of the first digit after "="
	IDEB1=11
C IDEB2 is the position of the last digit before "/"
	IDEB2=MIN(INDEX(STRING(IDEB1:80),'/')+IDEB1-2,80)
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
205       IFLAG =-1 
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

C Check if / is present (and only once):
       I=INDEX(STRING,'/')
       IF(I.LT.2)IFLAG=-1
C If / is present twice check if ' is there (and before):
       I2=INDEX(STRING(I+1:81),'/')
       I1=INDEX(STRING,'''')
       IF(I2.GT.0.AND.I1.LT.2)IFLAG=-1

C Case NAXIS3 = / or wrong format:
        IF(IFLAG.NE.0)THEN
	  PRINT *,' Syntax error in that statement: automatic correction'
          I=1
          DO K=IDEB,IFIN
           IDATA(K)=DUMMY(I)
           I=I+1
          END DO
          IDEB2=80
        ENDIF

C Copy to IDATA1:
        I=(J-1)*80
         DO K=IDEB,IDEB+IDEB2-1
           I=I+1
           IDATA1(I)=IDATA(K)
         END DO

C Goes to the next line
17      CONTINUE

C Clean from zeroes:
        DO K=1,NBYTE
           IF(IDATA1(K).EQ.0) IDATA1(K)=32
        ENDDO
C Write to corrected file:
        NBYTES_OUT=2880
        CALL JLP_OSDWRITE(IFID_OUT,IDATA1,NBYTES_OUT,ISTAT)
          IF (ISTAT.LE.0) THEN
            PRINT *,' FITS_RDHEADER: error when writing the data'
          END IF
        DO K=1,NBYTE
         IDATA1(K)=' '
        END DO

C Goes to the next record
18      CONTINUE
 
C Clean from zeroes:
999     DO K=1,NBYTE
           IF(IDATA1(K).EQ.0) IDATA1(K)=32
        ENDDO
C Write to corrected file (last block):
        NBYTES_OUT=2880
        CALL JLP_OSDWRITE(IFID_OUT,IDATA1,NBYTES_OUT,ISTAT)
          IF (ISTAT.LE.0) THEN
            PRINT *,' FITS_RDHEADER: error when writing the data'
            PRINT *,' ISTAT',ISTAT
          END IF
        RETURN
204     PRINT *,' FITS_RDHEADER/ Error reading Header'
        WRITE(6,ERR=204) (IDATA(K),K=IDEB,IFIN)
        STOP
	END
C*************************************************************************
C FITS_CPDATA
C Subroutine to copy the data from old to new FITS formatted file
C*************************************************************************
	SUBROUTINE FITS_CPDATA(IFID_OUT,NX,NY,NBITS,ISTATUS)
	PARAMETER (NBYTE=2880,NBYTE1=28800,NBYTE2=1440)
        LOGICAL*1 IDATA(NBYTE),BUFFER(NBYTE1)
        INTEGER*2 MES(NBYTE2)
        INTEGER*4 NX,NY,NBITS
        EQUIVALENCE (IDATA(1),MES(1))
	COMMON/FITS12/IDATA,BUFFER
 
10      FORMAT(A)
	ISTATUS=0
 
C Index for current pixel of the image:
	II=0
	IIMAX=NX*NY
	NRECORDS=(NX*NY*NBITS/8+2880-1)/2880
        PRINT *,' NRECORDS, NBITS, NX,NY',NRECORDS,NBITS,NX,NY
C Reset the flag for last record (to stop reading too far in the file)
	LAST_REC=0
 
	DO 112 IREC=1,NRECORDS
	  IF(IREC.EQ.NRECORDS)LAST_REC=1
C Reading the next record (IDATA):
	  CALL FITS_RDRECF(LAST_REC,ISTATUS)
C Leave this commented to save as much as possible:
	  IF(ISTATUS.NE.0) RETURN
 
C Computing the position of the last value to be read in that record:
	  IMAX=2880*8/NBITS
	  ITEST=IIMAX-II
	  IMAX=MIN(IMAX,ITEST)
 
C Write corrected file:
        NBYTES_OUT=IMAX*(NBITS/8)
        CALL JLP_OSDWRITE(IFID_OUT,IDATA,NBYTES_OUT,ISTAT)
          IF (ISTAT.LE.0) THEN
            PRINT *,' FITS_CPDATA: error when writing the data'
            PRINT *,' ISTAT',ISTAT
          END IF

112	CONTINUE
 
999	RETURN
        END
C******************************************************************
C FITS_RDOPEN
C To open the file on the disk
C******************************************************************
	SUBROUTINE FITS_RDOPEN(INFRAME,NFILE,ISTATUS)
	PARAMETER (NBYTE=2880,NBYTE1=28800)
        LOGICAL*1 IDATA(NBYTE),BUFFER(NBYTE1)
        CHARACTER INFRAME*60
	INTEGER*4 INDEX_BUF,NBYT_BLOCK
	INTEGER*4 NFILE,IMODE,IFID,ISTAT
	COMMON/FITS11/IFID,INDEX_BUF,NBYT_BLOCK
	COMMON/FITS12/IDATA,BUFFER
 
	ISTATUS=0
 
 
C Reset INDEX_BUF, last value not copied from the buffer
	  INDEX_BUF=2880+1
 
C Open the file:
C in read mode:
	     IMODE=0
	     LENGTH=60
	     CALL JLP_OSDOPEN(INFRAME,LENGTH,IMODE,IFID,ISTAT)
	     IF (ISTAT.EQ.-1) GOTO 99
 
 
	RETURN
99	PRINT *,' FITS_RDOPEN: Unable to open:',INFRAME
	ISTATUS=1
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
        CHARACTER INFRAME*60
	INTEGER*4 IMODE,ISTAT,INDEX_BUF,IFID,NBYT_BLOCK
	COMMON/FITS11/IFID,INDEX_BUF,NBYT_BLOCK
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
	INTEGER*4 INDEX_BUF,NBYT_BLOCK,IFID,LAST_REC
	COMMON/FITS11/IFID,INDEX_BUF,NBYT_BLOCK
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
99	PRINT *,' FITS_RDRECF: Error when reading the file'
	ISTATUS=1
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
