C++---------------------------------------------------------------------
C Set of routines to read and write images in CDCA format
C
C Contains : JLP_RDCDCA, JLP_CDCALINE, JLP_OPENCDCA
C JLP_WRCDCA, JLPI_WRITECDCA, JLP_WRHEADER_CDCA
C JLP_VM_RDCDCA
C
C JLP
C Version 25-03-90
C-----------------------------------------------------------------------
C Subroutine JLP_RDCDCA
C-----------------------------------------------------------------------
	SUBROUTINE JLP_RDCDCA(IMAGE,NX,NY,IDIM,FILENAME,
	1	COMMENTS,ISTAT)
	REAL*4 IMAGE(IDIM,*)
	INTEGER*4 LU,IBLOCK
	CHARACTER FILENAME*(*),COMMENTS*(*)
 
C Open the file
	LU=17
	ISTAT=0
	CALL JLP_OPENCDCA(FILENAME,COMMENTS,LU,NX,NY,ISTAT)
	IF(ISTAT.NE.0)THEN
	  PRINT *,' JLP_READCDCA/JLP_OPENCDCA : ERROR OPENING THE FILE'
	  RETURN
	ENDIF
 
C Reading the file :
C ARRAY does not work!!!
C        CALL JLP_CDCARRAY(IMAGE,NX,NY,IDIM,LU)
	DO IY=1,NY
	  CALL JLP_CDCALINE(IMAGE,IDIM,IY,NX,1,NX,LU,IBLOCK)
	END DO
 
	CLOSE(LU)
	RETURN
	END
CDOC ...................................................................
C    JLP_CDCALINE(IMAGE, IDIM, IY, NX, IX1, IX2, LU, IBLOCK)
C
C    Application LECTURE D'UNE LIGNE D'UN FICHIER IMAGE
C
C    Auteur
C
C    Arguments
C	IY    I2  NUMERO DE LA LIGNE A LIRE
C	NX   I2  NOMBRE DE POINTS DANS LA LIGNE
C	IX1  I2  INDICE DU PREMIER POINT A LIRE
C	IX2  I2  INDICE DU DERNIER POINT A LIRE
C	LU    I2  NUMERO D'UNITE LOGIQUE DU FICHIER
C IBLOCK  (Output) NUMERO DU BLOC ACCEDE
C
C Nota: CDCA structure is defined as:
C 1st block (of NVABLO bytes): header
C then NLILI blocks for the first line, the last block is completed with zeroes.
C ....
C then NLILI blocks for the last line, the last block is completed with zeroes.
C---------------------------------------------------------------------
	SUBROUTINE JLP_CDCALINE(IMAGE,IDIM,IY,NX,IX1,IX2,
	1      LU,IBLOCK)
 
	REAL*4 IMAGE(IDIM,*)
C LA DIMENSION DE "IA" DOIT ETRE AU MOINS EGALE A NVABLO
	INTEGER*2 IA(512)
	INTEGER*4 IY,NX,IX1,IX2,LU,NVABLO
	
	COMMON /CDCA/ NVABLO
 
 
C NLILI = Number of blocks per line:
	NLILI=(NX-1)/NVABLO+1
C Numer of the first block to read:
	IBLOCK=(IY-1)*NLILI+(IX1-1)/NVABLO+2
	IX=1
C Number of pixels to be read:
	NXO=IX2-IX1+1
C KX is the index of the first value to be read in the IA array
	KX=IX1
 
1	READ(LU'IBLOCK,ERR=100) (IA(J),J=1,NVABLO)
	IBLOCK=IBLOCK+1
        DO J = KX,NVABLO
	  IMAGE(IX,IY)=FLOAT(IA(J))
	  IX=IX+1
C Normal exit: line filled
	  IF (IX.GT.NXO) THEN
	    IX = IX -1
C	    PRINT *,' Normal exit, KX=',KX,'IX =',IX
C	    PRINT *,'IA(1,10)',(IA(K),K=1,10)
C	    PRINT *,'IMAGE(1:10,IY)',(IMAGE(K,IY),K=1,10)
	    RETURN
	  ENDIF
	END DO
C KX is set to 1 for the next block 
	KX=1
	GO TO 1
 
100	CONTINUE
	PRINT *,'JLP_CDCALINE/Error, line',IY,' not filled!'
	RETURN
	END
C----------------------------------------------------------------------
C    JLP_CDCARRAY (IMAGE,IDIM,NX,NY,LU)
C
C    Arguments
C	NX   NOMBRE DE POINTS DANS LA LIGNE
C	LU   NUMERO D'UNITE LOGIQUE DU FICHIER
C
C Doesn't work for CDCA, since it assumes a compacted version
C without zeroes at the end of each line
C---------------------------------------------------------------------
	SUBROUTINE JLP_CDCARRAY (IMAGE,NX,NY,IDIM,LU)
 
	REAL*4 IMAGE(IDIM,IDIM)
	INTEGER*2 IA(512)
	INTEGER*4 NX,NY,LU,NVABLO
	
	COMMON /CDCA/ NVABLO
 
C LA DIMENSION DE "IA" DOIT ETRE AU MOINS EGALE A NVABLO
	NLILI=(NX-1)/NVABLO+1
	IBLOCK=(IY-1)*NLILI
	KX=1
	KY=1
	IBLOCK=2
 
1	READ (LU'IBLOCK,ERR=100) (IA(III),III=1,NVABLO)
	IBLOCK=IBLOCK+1
	DO K = 1,NVABLO
         IMAGE(KX,KY)=FLOAT(IA(K))
	 KX=KX+1
	 IF(KX.GT.NX)THEN
	  KX=KX-NX
	  KY=KY+1
	  IF(KY.GT.NY)GOTO 100
	 ENDIF
	END DO
	GO TO 1
 
100	CONTINUE
	IF(KY.LE.NY)THEN
          PRINT *,' JLP_CDCARRAY/Fatal error: Unexpected end of file'
	  PRINT *,' Number of lines read:',KY
	  STOP
	ENDIF
	RETURN
	END
C----------------------------------------------------------------------
C Subroutine JLP_OPENCDCA	
C
C Arguments
C	NX,NY Size of the image
C	IDX, IDY  PAS ENTRE DEUX POINTS (ARBITRAIRE OU MICRONS)
C	XSTART,YSTART : Starting point for the PDS scan
C----------------------------------------------------------------------
	SUBROUTINE JLP_OPENCDCA(FILENAME,COMMENTS,LU,NX,NY,IFLAG)
 
	REAL*4 BSCALE,BOFFSET,FATUM(20),XSTART,YSTART
	INTEGER*2 NL,NPL,IDX,IDY,BLA(10),TYPE
	INTEGER*2 NATUM(6),NLL,NPLL,NLH,NPLH
	INTEGER*4 NX,NY,LU
	CHARACTER IDENT*80,FILENAME*(*),COMMENTS*(*),BUFFER*1024
	CHARACTER CHECK_256*512,CHECK_128*256,NAMEDESCR*20
	INTEGER*4 IN_DESCR,OUT_DESCR,NVABLO
	CHARACTER CDESCR*500
	COMMON/CDCA/NVABLO
        COMMON/JLP_DESCRIPTORS/CDESCR,IN_DESCR,OUT_DESCR
	COMMON/JLP_ACCESSFMT/IFMT_IN,IFMT_OUT,NFILE_IN,NFILE_OUT

C
	CLOSE(LU)
	OPEN(LU,FILE=FILENAME,STATUS='OLD',ACCESS='DIRECT',
     1      ERR=150)
 
C
C STRUCTURE CDCA
C   Pour determiner la longueur d'un bloc, on essaie de lire la variable
C   CHECK_256, si il n'y a pas d'erreur c'est du CDCA 256, sinon c'est du 64
	READ(LU,REC=1,ERR=200) CHECK_256
 
C********
C  CDCA 256
	NVABLO=256
	PRINT *,' STRUCTURE CDCA 256'
	READ(LU'1,ERR=150)IDENT,NPLL,NLL,IDX,IDY,X,Y,TYPE,
     1   BSCALE,BOFFSET,BLA,NPLH,NLH,
     1   (NATUM(I),I=1,6),(FATUM(J),J=1,20)
C NPLL   : poids faible de NPL
C NLL    : poids faible de NL
C NPLH   : poids fort de NPL
C NLH    : poids fort de NL
C Bscale : facteur d'echelle pour les densites
C Boffset: zero de l'echelle
C BLA    : utilisation generale
C NATUM  : photometrie de surface de galaxies
C FATUM  : photometrie de surface de galaxies
c
C   calcul de NX, NY
	NX=32767*NPLH+NPLL
	NY=32767*NLH+NLL
	COMMENTS=' '
	COMMENTS=IDENT

C Descriptors:
	IF(IN_DESCR.EQ.1)THEN
C First writes @ to indicate the end of the descriptors (Necessary!!!) :
	 CDESCR(1:1)='@'
	 WRITE(BUFFER,'(I5)') IDX
	 NAMEDESCR='IDX'
	 ILENGTH=6
	 CALL JLP_WDESCR(NAMEDESCR,BUFFER,ILENGTH,ISTATUS)
	 WRITE(BUFFER,'(I5)') IDY
	 NAMEDESCR='IDY'
	 ILENGTH=6
	 CALL JLP_WDESCR(NAMEDESCR,BUFFER,ILENGTH,ISTATUS)
	 WRITE(BUFFER,'(20(X,E15.8))') (FATUM(J),J=1,20)
	 NAMEDESCR='FATUM'
	 ILENGTH=321
	 CALL JLP_WDESCR(NAMEDESCR,BUFFER,ILENGTH,ISTATUS)
	 WRITE(BUFFER,'(6(X,I5))') (NATUM(J),J=1,6)
	 NAMEDESCR='NATUM'
	 ILENGTH=37
	 CALL JLP_WDESCR(NAMEDESCR,BUFFER,ILENGTH,ISTATUS)
	ENDIF
 

	IFLAG=0
	RETURN
 
C********
C  CDCA 64
	
C Pour determiner la longueur d'un bloc
C We try to read CHECK_64
200	READ(LU,REC=1,ERR=150) CHECK_64
	NVABLO=64
	PRINT *,' STRUCTURE CDCA 64'
	READ(LU'1,ERR=150) IDENT,NPL,NL,IDX,IDY,X,Y
	NX=NPL
	NY=NL
	COMMENTS=' '
	COMMENTS=IDENT
	IFLAG=0
	RETURN
 
C Error accessing the file
150	IFLAG=1
	RETURN
	END
 
C-------------------------------------------------------------------
C Subroutine JLPI_WRITECDCA
C
C To write a CDCA file from an integer*2 file
C
C-------------------------------------------------------------------
	SUBROUTINE JLPI_WRITECDCA(INPUT,NX,NY,IDIM,
	1    FILENAME,COMMENTS)
	PARAMETER (NVABLO=256)
	REAL*4 BSCALE
	INTEGER*2 INPUT(IDIM,IDIM)
	INTEGER*2 IV(NVABLO)
	CHARACTER*(*) FILENAME,COMMENTS
 
	IREC=NVABLO/2
	OPEN(19,FILE=FILENAME,RECL=IREC,ACCESS='DIRECT',
	1      STATUS='NEW',ERR=999)
 
C Writing the CDCA header:
	BSCALE=1.
	CALL JLP_WRHEADER_CDCA(19,COMMENTS,BSCALE,NX,NY,IFLAG)
	IF(IFLAG.NE.0)THEN
	   PRINT *,' JLPI_WRITECDCA/Error writing the header'
	   GO TO 999
	ENDIF
 
C ID is the counter of the reccord for the output file:
	ID=2
 
C Loop on the lines  (Integer file in output : always with CDCA !!!)
	  DO 11 IY=1,NY
	    IX=1
50	    KPL=1
60	    IV(KPL)=INPUT(IX,IY)
	    KPL=KPL+1
	    IX=IX+1
C At the end of each line we write on the file, even if
C IV is not full :
	    IF(IX.GT.NX) GO TO 70
	    IF(KPL.LE.NVABLO) GO TO 60
C When the bloc is full we write on the file :
	    WRITE(19'ID) (IV(KQ),KQ=1,NVABLO)
	    ID=ID+1
C Initialization of IV for the next step :
	    DO IH=1,NVABLO
	      IV(IH)=0
	    END DO
	  GO TO 50
C Writing the end of the line :
70	  WRITE(19'ID) (IV(KQ),KQ=1,NVABLO)
	  ID=ID+1
 
11	  CONTINUE
 
	CLOSE(19)
	ISTAT=0
	RETURN
 
999	ISTAT=1
	RETURN
	END
 
C-------------------------------------------------------------------
C Subroutine JLP_WRCDCA
C
C To write a CDCA file from a real*4 file
C
C-------------------------------------------------------------------
	SUBROUTINE JLP_WRCDCA(INPUT,NX,NY,IDIM,
	1     FILENAME,COMMENTS,ISTAT)
	PARAMETER (NVABLO=256)
	REAL*4 INPUT(IDIM,IDIM)
	INTEGER*2 IV(NVABLO)
	CHARACTER*(*) FILENAME,COMMENTS
 
	IREC=NVABLO/2
	OPEN(19,FILE=FILENAME,RECL=IREC,ACCESS='DIRECT',
	1     STATUS='NEW',ERR=999)
 
C Scaling factor to output real files :
	PRINT *,' CONSTANT BY WHICH YOU WANT TO MULTIPLY THE',
	1    ' INPUT FILE : (BSCALE)'
	READ(5,*) BSCALE 

C Writing the CDCA header:
	CALL JLP_WRHEADER_CDCA(19,COMMENTS,BSCALE,NX,NY,IFLAG)
	IF(IFLAG.NE.0)THEN
	   PRINT *,' JLP_WRCDCA/Error writing the header'
	   GO TO 999
	ENDIF
 
C ID is the counter of the record for the output file:
	ID=2
 
	  DO 11 IY=1,NY
	    IX=1
250	    KPL=1
260	    WORK1=BSCALE*INPUT(IX,IY)
	     IF(WORK1.LT.-32000.)WORK1=-32000.
	     IF(WORK1.GT.32000.)WORK1=32000.
	    IV(KPL)=NINT(WORK1)
	    KPL=KPL+1
	    IX=IX+1
C At the end of each line we write on the file, even if
C IV is not full :
	    IF(IX.GT.NX) GO TO 270
	    IF(KPL.LE.NVABLO) GO TO 260
C When the bloc is full we write on the file :
	    WRITE(19'ID) (IV(KQ),KQ=1,NVABLO)
	    ID=ID+1
C Initialization of IV for the next step :
	     DO IH=1,NVABLO
	       IV(IH)=0
	     END DO
	    GO TO 250
C Writing the end of the line :
270	    WRITE(19'ID) (IV(KQ),KQ=1,NVABLO)
	    ID=ID+1
 
11	  CONTINUE
 
	CLOSE(19)
	ISTAT=0
	RETURN
 
999	ISTAT=1
	RETURN
	END
 
C----------------------------------------------------------------------
C Subroutine JLP_WRHEADER_CDCA	
C
C Arguments
C	NX,NY Size of the image
C
C
C	IDX, IDY  PAS ENTRE DEUX POINTS (ARBITRAIRE OU MICRONS)
C	XSTART,YSTART : Starting point for the PDS scan
C----------------------------------------------------------------------
	SUBROUTINE JLP_WRHEADER_CDCA(LU,COMMENTS,BSCALE,NX,NY,IFLAG)
	PARAMETER (NVABLO=256)
	REAL*4 BSCALE,BOFFSET,FATUM(20),XSTART,YSTART
	INTEGER*2 IDX,IDY,BLA(10)
	INTEGER*2 NATUM(6),NLL,NPLL,NLH,NPLH
	INTEGER*4 NY,NX,LU,ILENGTH,ISTATUS
	CHARACTER IDENT*80,TYPE*2,COMMENTS*(*),BUFFER*1024
	INTEGER*4 IN_DESCR,OUT_DESCR
	CHARACTER CDESCR*500
        COMMON/JLP_DESCRIPTORS/CDESCR,IN_DESCR,OUT_DESCR
 
C Transfer:
	IDENT=COMMENTS
 
C Conversion to integer*2
	NPLL=NX
	NLL=NY
	NPLH=0
	NLH=0
 
C Offset
	BOFFSET=0.
 
C Step in X and Y :
	IDX=1
	IDY=1
 
C Starting points
	XSTART=0.
	YSTART=0.
 
C NATUM
	NATUM(1)=NX
	NATUM(2)=NY
	NATUM(3)=IDX
	NATUM(4)=IDY
 
C FATUM
	FATUM(1)=XSTART
	FATUM(2)=YSTART
	FATUM(15)=1.		! (SCALE IN "/MICRONS)
 
C Writing the CDCA header:
	TYPE='DE'

C Descriptors:
	IF(OUT_DESCR.EQ.1)THEN
	 CALL JLP_RDESCR('IDX',BUFFER,ILENGTH,ISTATUS)
	 PRINT *,'BUFFER',BUFFER(1:80)
	 PRINT *,'ILENGTH',ILENGTH
	 PRINT *,'ISTATUS',ISTATUS
	 IF(ISTATUS.EQ.0) READ(BUFFER,*,ERR=100) IDX
100	 CALL JLP_RDESCR('IDY',BUFFER,ILENGTH,ISTATUS)
	 IF(ISTATUS.EQ.0) READ(BUFFER,*,ERR=200) IDY
200	 CALL JLP_RDESCR('FATUM',BUFFER,ILENGTH,ISTATUS)
	 IF(ISTATUS.EQ.0) READ(BUFFER,*,ERR=300) (FATUM(J),J=1,20)
300	 CALL JLP_RDESCR('NATUM',BUFFER,ILENGTH,ISTATUS)
	 IF(ISTATUS.EQ.0) READ(BUFFER,*,ERR=400) (NATUM(J),J=1,6)
400	ENDIF
 
C NPLL   : poids faible de NX
C NLL    : poids faible de NY
C NPLH   : poids fort de NX
C NLH    : poids fort de NY
C Bscale : facteur d'echelle pour les densites
C Boffset: zero de l'echelle
C BLA    : utilisation generale
C NATUM  : photometrie de surface de galaxies
C FATUM  : photometrie de surface de galaxies
C
C TYPE     : Type of the file ('DE' for an image)
C   calcul de NX, NY
C	NX=32767*NPLH+NPLL
C	NY=32767*NLH+NLL
 
	WRITE(LU'1,ERR=999)IDENT,NPLL,NLL,IDX,IDY,
	1     XSTART,YSTART,TYPE,BSCALE,BOFFSET,BLA,NPLH,NLH,
	1     (NATUM(I),1,6),(FATUM(J),J=1,20)
 
C No errors:
	IFLAG=0
	RETURN
 
C Error :
999	IFLAG=1
	RETURN
	END
C-----------------------------------------------------------------------
C Subroutine JLP_VM_RDCDCA
C-----------------------------------------------------------------------
	SUBROUTINE JLP_VM_RDCDCA(PNTR_IMAGE,NX,NY,FILENAME,
	1	COMMENTS,ISTAT)
	INTEGER*4 PNTR_IMAGE,NX,NY
	CHARACTER FILENAME*(*),COMMENTS*(*)
	INTEGER*4 MADRID(1),ISIZE
	COMMON /VMR/MADRID
 
C Open the file
	LU=17
	ISTAT=0
	CALL JLP_OPENCDCA(FILENAME,COMMENTS,LU,NX,NY,ISTAT)
	IF(ISTAT.NE.0)THEN
	  PRINT *,' JLP_READCDCA/JLP_OPENCDCA : ERROR OPENING THE FILE'
	  RETURN
	ENDIF
 
C Get dynamical memory:
	ISIZE=NX*NY*4
	CALL JLP_GETVM(PNTR_IMAGE,ISIZE)
 
C Reading the file :
C ARRAY does not work!!!
C        CALL JLP_CDCARRAY(MADRID(PNTR_IMAGE),NX,NY,NX,LU)
	DO IY=1,NY
	  CALL JLP_CDCALINE(MADRID(PNTR_IMAGE),NX,IY,NX,1,
     1        NX,LU,IBLOCK)
	END DO
 
	CLOSE(LU)
	RETURN
	END
