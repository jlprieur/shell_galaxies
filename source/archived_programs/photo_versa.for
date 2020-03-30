C++************************************************************************
C Program PHOTO_VERSA
C
C From "PHOTO_LASER"
C To display an image on the Versatec either in greyscale or with contours
C Uses PGPLOT package
C
C JLP  Version of 16-11-87
C--************************************************************************
	PROGRAM PHOTO_VERSA
	PARAMETER (IDIM=600)
	REAL*4 INPUT(IDIM,IDIM),CONT(20),OUTPUT(IDIM,IDIM)
	INTEGER*4 ICODE(20)
	LOGICAL CONTOUR
	CHARACTER TITLE*40,NAME*40,COMMENTS*80,ANS*1
	CHARACTER OPTION*10,PLOTDEV*32
10	FORMAT(A)
	PRINT 7
7	FORMAT(' Program PHOTO_VERSA   Version of 04-11-87')
 
C Reset the CPU timer :
	CALL LIB$INIT_TIMER
 
*   Get image
	CALL JLP_INQUIFMT
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
	CALL JLP_READIMAG(INPUT,NX,NY,IDIM,NAME,COMMENTS)
*
	PRINT *,' ENTER OPTION : CONTOURS or GREYSCALE ?'
	READ(5,10) OPTION
	CONTOUR=(OPTION(1:1).EQ.'c'.OR.OPTION(1:1).EQ.'C')
 
C Compression of the input image :
	PRINT *,' COMPRESSION FACTOR OF THE IMAGE ? (Power of 2)'
	READ(5,*) IFACT
 
*  Get thresholds and number of levels to be used :
	IF(CONTOUR)THEN
	  PRINT *,' ENTER THE NUMBER OF CONTOURS YOU WANT TO DRAW < 20'
	  READ(5,*) NCONT
C ISTYLE 1=solid  2=dash 3=dash dot 4=dot 5=dash dot dot dot
	  PRINT 22
22	  FORMAT(' REMEMBER :',
     1	' 1=SOLID 2=DASH 3=DASH-DOT 4=DOT 5=THICK SOLID')
	  DO I=1,NCONT
	    PRINT 20,I
20	    FORMAT(' LEVEL AND SYMBOL FOR CONTOUR  # ',I2,' : ',$)
	    READ(5,*) CONT(I),ICODE(I)
	  END DO
	  XMIN=CONT(1)
	  XMAX=CONT(NCONT)
	ELSE
	  PRINT *,' LOWER AND UPPER THRESHOLDS :'
	  READ(5,*) XMIN,XMAX
	ENDIF
 
* Get title
	PRINT *,' TITLE ?'
	READ(5,10) TITLE
 
C Compression of the image :
	CALL PHVS_COMPRESS(INPUT,NX,NY,IDIM,OUTPUT,IFACT)
 
21	PRINT *,' PLOTDEVICE ?'
	READ(5,10) PLOTDEV
 
C Generating the file "CANON.DAT" :
	CALL PHVS_FOTO4(OUTPUT,NX,NY,XMIN,XMAX,TITLE,
     1	CONT,NCONT,ICODE,CONTOUR,PLOTDEV,ISTAT)
 
	PRINT *,' DO YOU WANT TO OUTPUT THE GRAPH TO ANOTHER DEVICE ?(N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GO TO 21
 
C Check the CPU time used :
	CALL LIB$SHOW_TIMER
 
	END
 
C******************************************************************
	SUBROUTINE PHVS_FOTO4(IMAGE,NX,NY,XMIN,XMAX,
     1	TITLE,CONT,NCONT,ICODE,CONTOUR,PLOTDEV,ISTATS)
*+
*         This subroutine creates a Versatec bitmap of a grey-scale plot
*         or contours of a real array . The array need not be
*         square. On completion a VERSATEC.BIT file is produced which
*         needs to be printed:
*         $ PGVER VERSATEC.BIT
*
*:        Given (argument):
*
*         IMAGE  r - array containing image to be plotted, dimension
*                    NX by NY. There is a maximum to the dimensions
*                    of the array, usually 512*512, which is defined by
*                    a parameter statement.
*         NX   i - number of columns in image array (>1)
*         NY   i - number of rows in image array (>1)
*         XMAX r - value in array corresponding to white on the plot
*         XMIN r - value in array corresponding to black on the plot
*         TITLE  c - title for the plot
*
*         Returned (argument):
*
*         ISTATS i - status return: 0 - success; 1 - NX or NY < 2;
*                    2 - image too big.
*
* Other variables :
*	  TRANS r - transformation matrix from the world coordinates
*	            to the pixel coordinates (or vice versa)
*
*;        Graphics: PGPLOT
*
*-
	CHARACTER*(*) TITLE
	CHARACTER PLOTDEV*32,CTYPE*80
	CHARACTER CAPTION*60,DATE*24
	REAL*4 IMAGE(NX,NY)
	REAL*4 TRANS(6),CONT(20),CONT1(20)
	INTEGER*4 ICODE(20),PGBEGIN
	LOGICAL CONTOUR
	DATA TRANS/0.,1.,0.,0.,0.,1./
 
	ISTATS=0
*
*        Open workstation
*
C	PLOTDEV='VERSATEC.BIT/VERSATEC'
	PRINT 21,PLOTDEV
21	FORMAT(' PGPLOT device/type : ',A)
 
C Selecting the device :
	   ISTAT=PGBEGIN(19,PLOTDEV,1,1)
	   IF ( ISTAT .NE. 1 ) THEN
	     PRINT *,' Error accessing the device'
	     CALL EXIT
	   ENDIF
 
C Get the type of the selected device :
	 CALL GRINQTYP(CTYPE,JUNK)
	 PRINT *,' CTYPE :',CTYPE
 
C Set the intensity of the lines to the maximum (3)
C (do not affect the Tektronix plots)
	   CALL GRSETLI(3)
 
* Set world co-ordinates to pixels
*
	   CALL PGENV(0.5,REAL(NX)-0.5,0.5,REAL(NY)-0.5,1,-1)
 
*
* Draw the map using PGPLOT "CELL ARRAY" :
*
 
	IF(CONTOUR)THEN
	  DO I=1,NCONT
 
C PGPLOT :
C ISTYLE 1=solid  2=dash 3=dash dot 4=dot 5=thick solid
	   IF(ICODE(I).LE.4)THEN
	     IWIDTH=1
	     ISTYLE=ICODE(I)
	   ELSEIF(ICODE(I).EQ.5)THEN
	     IWIDTH=3
	     ISTYLE=1
	   ELSE
	     IWIDTH=1
	     ISTYLE=1
	   ENDIF
 
C	   CALL PGCONT(IMAGE,NX,NY,1,NX,1,NY,CONT,
C     1	NCONT,TRANS)
	   CONT1(1)=CONT(I)
 
C Problem with the buffer of PGCONT and ISTYLE : we cut into small pieces
	   NXPIECES=NX/30+1
	   NYPIECES=NY/30+1
	   IY1=1
	   DO KY=1,NYPIECES
	     IX1=1
	     IY2=MIN(KY*30,NY)
	     DO KX=1,NXPIECES
	       IX2=MIN(KX*30,NX)
	       CALL GRSETLS(ISTYLE)
	       CALL GRSETLW(IWIDTH)
	         CALL PGCONT(IMAGE,NX,NY,IX1,IX2,IY1,IY2,
     1	CONT1,1,TRANS)
	       IX1=MAX(IX2-2,1)
	      END DO
	      IY1=MAX(IY2-2,1)
	    END DO
 
	  END DO
	ELSE
	   CALL PGGRAY(IMAGE,NX,NY,1,NX,1,NY,XMIN,
     1	XMAX,TRANS)
	ENDIF
*
* Draw title
*
* Size :
	   CALL PGSETC(1.)
* Width :
	   CALL GRSETLW(3)
C 1=Normal
C 2=Roman
C 3=Italic
C 4=Script
* Font : (2=Roman)
	   CALL GRSETFONT(2)
*
	   CALL PGMTEXT('T',1.0,0.1,0.0,TITLE)
 
C Draw a caption when Versatec is selected :
	 IF(CTYPE(1:4).EQ.'VERS'.OR.CTYPE(1:4).EQ.'vers')THEN
* Caption : threshold
* 3=Italic
	   CALL GRSETLW(2)
	   CALL GRSETFONT(3)
	   CALL PGSETC(0.5)
	   WRITE(CAPTION,108)XMIN,XMAX
108	   FORMAT(' Thresholds :',G12.5,X,G12.5)
	   CALL PGMTEXT('B',1.0,1.0,0.0,CAPTION)
 
* And date/time :
	   CALL LIB$DATE_TIME(DATE)
	   CALL PGMTEXT('B',2.0,1.0,0.0,DATE)
	ENDIF
	
	   CALL PGEND
 
	RETURN
	END
 
C---------------------------------------------------------------------
C Subroutine PHVS_COMPRESS
C To compress an image by a factor of IFACT
C Arguments :
C IMAGE (IDIM,IDIM) input
C OUTPUT output
C NX,NY input/output
C IFACT input
C---------------------------------------------------------------------
	SUBROUTINE PHVS_COMPRESS(IMAGE,NX,NY,IDIM,OUTPUT,IFACT)
	REAL*4 IMAGE(IDIM,IDIM),OUTPUT(IDIM*IDIM)
 
* Compression of the image :
	 NY1=NY
	 NX1=NX
	 IFACT2=IFACT
94	 IF(IFACT2.LE.1)GOTO 95
	   NY1=NY1/2
	   NX1=NX1/2
	   DO J1=1,NY1
	      J=2*J1-1
	     DO I1=1,NX1
	       I=2*I1-1
	       IMAGE(I1,J1)=0.25*(IMAGE(I,J)+IMAGE(I,J+1)+
     1	IMAGE(I+1,J)+IMAGE(I+1,J+1))
	     END DO
	   END DO	
	   IFACT2=IFACT2/2
	 GOTO 94
 
95	NX=NX1
	NY=NY1
 
C Copy the image to the output :
	K=0
	DO J=1,NY
	  DO I=1,NX
	   K=K+1
	   OUTPUT(K)=IMAGE(I,J)
	  END DO
	END DO
 
	RETURN
	END
