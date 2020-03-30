C++---------------------------------------------------------------------
C	Program GRINNEL
C
C From GRINNEL and PATCH
C
c	programme de visualisation sur la grinnel de cliches ccd
c	fichiers entiers ou reels
C
C JLP
C Version of 04-11-90
C
C GRINNEL routines :
C		ECRITURE MEMOIRE:GRMWLW
C		CHARGEMENT DES LUT :GRMWR
C		CONFIGURE DES ENTREES IFM LUT :GRMIN
C		CONFIGURE DES ENTREES IVD :GRVIN
C
C---------------------------------------------------------------------
	PROGRAM GRINNEL
C
	PARAMETER (IDIM=1024)
	INTEGER*2 ENT,F1,F2,I2X,I2Y,KLM
	INTEGER*2 IDW,IDH,IZCLAM,ICHA,ICH0,ICH1,ICH2
	INTEGER*2 IBACK,IX_TITLE,IY_TITLE,IXSTEP,IYSTEP,ISIZE
	INTEGER*2 LUT1(1024),IBUFF1(512),ITITLE(40)
	INTEGER*4 NPL,NL,IXSTART,IYSTART
	REAL*4 IMAGE(IDIM,IDIM)
	REAL*4 XND1,XND2,XND3,VV1,VV2,VV3
	REAL*4 XMIN1,XMIN2,XMIN3
	REAL*4 XMAX1,XMAX2,XMAX3
	CHARACTER FILENAME*40,FILENAME_OUT*40,COMMENTS*80,TITLE*40
	CHARACTER ANS*1,IDENT*80,ANS_PLAN*1,CBUFF*80
	LOGICAL INPUT_FILE,INPUT_LEVEL,SQUARE_PIXEL,DRAW_CROSS
	COMMON/PARAM_GRI/IDW,IDH,IZCLAM,ICHA,ICH0,ICH1,ICH2,
     1	XMIN1,XMAX1,XMIN2,XMAX2,XMIN3,XMAX3,IXSTART,IYSTART
	EQUIVALENCE (ITITLE(1),TITLE(1:1))
 
10	FORMAT(A)
	INPUT_FILE=.FALSE.
	INPUT_LEVEL=.FALSE.
 
	PRINT *,' PROGRAMME DE VISUALISATION SUR GRINNEL'
	PRINT *,' FORMAT MAX 512*512'
	PRINT *,' Version of April 6th 1987'
 
	CALL GRINNEL_INIT
 
C	BUFLEN=32000		! lenth of buffer (imperative ...in common)
 
C Initialization of COMMON GRINNEL
c	CALL GRSINI
 
C KLM=0 : Zooming factor
	KLM=0
	CALL GRZFC(0,KLM)
C Synchro
	CALL GRSBFD
 
C****************************************************************
C	PARAMETRES DE CHARGEMENT DE L'IMAGE
c	IXSTART1=0
c	IYSTART1=0
c	ILEN=512
c	IDW=0
c	IDH=0
C Binary masks : 001  010 100  (1,2,4)
c	ICH0=1
c	ICH1=2
c	ICH2=4
c	ICHA=7
C Zero clamp
c	IZCLAM=1
	IBACK=0
 
	OPEN(3,FILE='GRINNEL.DAT',STATUS='NEW')
 
C Inquire the format of the input/output files :
	CALL JLP_INQUIFMT
 
C--------------------------------------------------------------------
80	IF(SQUARE_PIXEL)THEN
	  PRINT 82
	ELSE
	  PRINT 81
	ENDIF
 
C Complete menu
81	FORMAT(' Menu :'/,
     1	' 0. Erasing the screen',/,
     1	' 1. Loading an image',/,
     1	' 2. Loading a colour table (Look up table)',/,
     1	' 3. Assigning an image plane to each color',/,
     1	' 4. Zooming',/,
     1	' 5. Cursor',/,
     1	' 6. Loading the same image with different thresholds',/,
     1	' 7. Correction of bad columns',/,
     1	' 8. "Patch"',/,
     1	' 9. Inspecting the image',/,
     1	' 11. Grinnel test',/,
     1	' 12. Reset',/,
     1	' 13. Writing a title on the overlay planes',/,
     1	' 14. Erasing an overlay plane',/,
     1	' 15. Parameters of the present display',/,	
C     1	' 16. New version of the titles',/,
     1	' 10. Exit',/,
     1	' Enter your choice : ',$)
 
C Menu available when corrected geometry has been selected :
82	FORMAT(' Menu :'/,
     1	' 0. Erasing the screen',/,
     1	' 1. Loading an image',/,
     1	' 2. Loading a colour table (Look up table)',/,
     1	' 3. Assigning an image plane to each color',/,
     1	' 4. Zooming',/,
c     1	' 5. Cursor',/,
     1	' 6. Loading the same image with different thresholds',/,
c     1	' 7. Correction of bad columns',/,
c     1	' 8. "Patch"',/,
c     1	' 9. Inspecting the image',/,
     1	' 11. Grinnel test',/,
     1	' 12. Reset',/,
     1	' 13. Writing a title on the overlay planes',/,
     1	' 14. Erasing an overlay plane',/,
     1	' 15. Parameters of the present display',/,	
C     1	' 16. New version of the titles',/,
     1	' 10. Exit',/,
     1	' Enter your choice : ',$)
	READ(5,*) IOP
 
C---------------------------------------------------------
C Erasing the screen :
	IF(IOP.EQ.0)THEN
	   CALL GRFER(4095,4095,IBACK)
	   CALL GRSBFD
	   GO TO 80
	ELSEIF(IOP.EQ.1)THEN
	  GO TO 180
	ELSEIF(IOP.EQ.2)THEN
	  GO TO 280
	ELSEIF(IOP.EQ.3)THEN
	  GO TO 380
	ELSEIF(IOP.EQ.4)THEN
	  GO TO 480
	ELSEIF(IOP.EQ.5)THEN
	  GO TO 580
	ELSEIF(IOP.EQ.6)THEN
	  GO TO 680
	ELSEIF(IOP.EQ.7)THEN
	  GO TO 880
	ELSEIF(IOP.EQ.8)THEN
	  GO TO 880
	ELSEIF(IOP.EQ.9)THEN
	  GO TO 980
	ELSEIF(IOP.EQ.10)THEN
	  GO TO 999
C---------------------------------------------------------
C Grinnel test :
	ELSEIF(IOP.EQ.11)THEN
	PRINT *,' TEST NUMBER ?'
	READ(5,*) ITEST
	   CALL GRTINT(ITEST)
	   CALL GRSBFD
	   GO TO 80
C---------------------------------------------------------
C Reset :
	ELSEIF(IOP.EQ.12)THEN
	   CALL GRSRST
	   CALL GRSBFD
	   GO TO 80
C---------------------------------------------------------
C Writing a title :
	ELSEIF(IOP.EQ.13)THEN
	   PRINT 131,FILENAME
131	   FORMAT(' Remember : filename =',A)
	   PRINT *,' ENTER THE TITLE ("RETURN" TO WRITE THE FILENAME)'
	   READ(5,10) TITLE
	   IF(TITLE(1:5).EQ.'     ')TITLE=FILENAME
C ISIZE :  0=normal  1=double width
134	   PRINT 132
132	   FORMAT(' Standard position x,y, step ix,iy',
     1	' and size (0 or 1): 300,480,16,0,1',/,
     1	' ENTER THE POSITION YOU WANT ("RETURN" IF 300,480,16,0,1)')
	   READ(5,10) CBUFF
	   READ(CBUFF,133,ERR=134) IX_TITLE,IY_TITLE,IXSTEP,IYSTEP,ISIZE
133	   FORMAT(5I5)
	     IF(IX_TITLE.EQ.0.AND.IY_TITLE.EQ.0)THEN
	       IX_TITLE=300
	       IY_TITLE=480
	       IXSTEP=16
	       IYSTEP=0
	       ISIZE=1
	     ENDIF
	   PRINT *,' OVERLAY PLANE : 1,2,3 or 4 ?'
	   READ(5,*) IOVPLANE
	   ICAN=2**(7+IOVPLANE)
	   ISUB=ICAN
 
C Writing the title : (40 characters max)
	   CALL GRFCD(ICAN,ISUB,0,IZCLAMP,ITITLE,IX_TITLE,IY_TITLE,
     1	IXSTEP,IYSTEP,40,ISIZE,ISIZE,0)
	   CALL GRSBFD
	   GO TO 80
C---------------------------------------------------------
C Erasing the overlay planes :
	ELSEIF(IOP.EQ.14)THEN
	   PRINT *,' OVERLAY PLANE TO ERASE : 1,2,3 or 4 ?'
	   READ(5,*) IOVPLANE
	   ICAN=2**(7+IOVPLANE)
	   ISUB=ICAN
	   CALL GRFER(ICAN,ISUB,IBACK)
	   CALL GRSBFD
	   GO TO 80
C---------------------------------------------------------
C Parameters of the display :
	ELSEIF(IOP.EQ.15)THEN
	   PRINT 151,FILENAME,COMMENTS,XMIN1,XMAX1,
     1	XMIN2,XMAX2,XMIN3,XMAX3,JJ1,JJ2,JJ3,
     1	IOP4,VIOP4,NIOP4
151	   FORMAT(' IMAGE :',A,/,' COMMENTS:',A,/,
     1	' THRESHOLDS :',6(G12.5,2X),/,
     1	' IMAGE PLANES ASSOCIATED WITH R,G,B :',3(I2,2X),/,
     1	' OPTIONS OF THE LOOK UP TABLE :',I4,2X,
     1	G12.5,2X,I4)
	  GO TO 80
C New Version of the titles:
C	ELSEIF(IOP.EQ.16)THEN
C	   CALL INCLUDE
C	   GO TO 80
	ELSE
	  GO TO 80
 	ENDIF
 
C======================================================================
C
C======================================================================
C Opening a new file :
180	PRINT 181
181	FORMAT(' INPUT OF AN IMAGE :')
	READ(5,10) FILENAME
	CALL JLP_READIMAG(IMAGE,NPL,NL,IDIM,FILENAME,COMMENTS)
	INPUT_FILE=.TRUE.
	WRITE(3,185)FILENAME
185	FORMAT(' INPUT IMAGE : ',A)
	WRITE(3,186)COMMENTS
186	FORMAT(' COMMENTS : ',A)
 
C Possibility of a geometric correction (compression with a factor of 0.75 in X)
	PRINT 189
189	FORMAT(/,' WARNING : SOME OPTIONS ARE NOT AVAILABLE',
     1	' WHEN "CORRECTED GEOMETRY" IS SELECTED.',/,
     1	' DO YOU WANT AN IMAGE WITH CORRECTED GEOMETRY ?(N)')
	READ(5,10) ANS
	SQUARE_PIXEL=(ANS.EQ.'Y'.OR.ANS.EQ.'y')
	IF(SQUARE_PIXEL)
     1	CALL GRISQ_COMPRESS(IMAGE,NPL,NL,IMAGE,NPL,NL,IDIM)
 
C IXSTART, IYSTART Position of the first pixel to be displayed :
	IXSTART=1
	IYSTART=1
C PB while reading with the cursor when IL.NE.1 ...
 	IF(NL.GT.512.OR.NPL.GT.512)THEN
	  PRINT *,' MORE THAN 512x512 !'
	  PRINT *,' Enter the coordinates X,Y of the first pixel:'
	  READ(5,*) IXSTART,IYSTART
	ENDIF
 
C Reading a sample of the image :
	NPL1=MIN0(NPL-IXSTART+1,512)
	NL1=MIN0(NL-IYSTART+1,512)
 
	I1=MAX(1,NPL1/4)
	I2=MAX(1,NPL1/2)
	I3=MAX(1,NPL1*3/4)
	J1=MAX(1,NL1/4)
	J2=MAX(1,NL1/2)
	J3=MAX(1,NL1*3/4)
	PRINT *,' SAMPLE OF VALUES :'
	PRINT 1013,IMAGE(I1,J1),IMAGE(I2,J1),IMAGE(I3,J1)
	PRINT 1013,IMAGE(I1,J2),IMAGE(I2,J2),IMAGE(I3,J2)
	PRINT 1013,IMAGE(I1,J3),IMAGE(I2,J3),IMAGE(I3,J3)
1013	FORMAT(3X,3(1PE12.4,2X))
 
C
C	ENTREE DES 6 SEUILS
C**************************************************************
C
680	PRINT *,' STOCKAGE SUR 3 PLANS ? (Y)'
	READ(5,10) ANS_PLAN
 
	IF(ANS_PLAN.NE.'N'.AND.ANS_PLAN.NE.'n')THEN
	  PRINT *,' L''IMAGE VA ETRE STOQUEE SUR 3 PLANS-IMAGES'
	  PRINT *,XMIN1,XMAX1,XMIN2,XMAX2,XMIN3,XMAX3
31	  PRINT *,' ENTREZ LES 6 SEUILS CORRESPONDANTS :',
     1	' MIN1,MAX1,MIN2,MAX2,MIN3,MAX3 (REAL)'
	  READ(5,10) CBUFF
	  READ(CBUFF,30,ERR=31) XXMIN1,XXMAX1,XXMIN2,
     1	XXMAX2,XXMIN3,XXMAX3
30	  FORMAT(6G12.5)
	    IF(XXMIN1.NE.0..OR.XXMAX1.NE.0)THEN
	      XMIN1=XXMIN1
	      XMAX1=XXMAX1
	      XMIN2=XXMIN2
	      XMAX2=XXMAX2
	      XMIN3=XXMIN3
	      XMAX3=XXMAX3
	    ENDIF
	  INPUT_LEVEL=.TRUE.
 
C If bad values, prompt the user again :
	   IF((XMAX1.EQ.XMIN1).OR.
     1	(XMAX2.EQ.XMIN2).OR.
     1	(XMAX3.EQ.XMIN3))THEN
	     PRINT *,' BAD VALUES, ENTER NEW VALUES:'
	     GO TO 31
	   ENDIF	
 
	XND1=255./(XMAX1-XMIN1)
	XND2=255./(XMAX2-XMIN2)
	XND3=255./(XMAX3-XMIN3)
 
	ELSE
	  PRINT *,' PLAN IMAGE = 0'
	  PRINT *,' MIN, MAX ? (REAL)'
	  READ(5,*) XMIN1,XMAX1
	  XND1=255./(XMAX1-XMIN1)
	ENDIF
 
	PRINT *,' THRESHOLDS :',XMIN1,XMAX1,XMIN2,XMAX2,XMIN3,XMAX3
	ICH2=4
	PRINT *,' XND1,XIND2,XIND3 :',XND1,XND2,XND3
	PRINT *,' TRACE EN COURS...'
 
C Reading the input file and displaying the image line per line :
	DO 600 IY=IYSTART,NL1+IYSTART-1
 
C**************************************************************
C Loading the image on the screen
C**************************************************************
 
C Loading the current line on the 3 planes :
 
C ILEN = lenth
	ILEN=NPL1
C Starts at line 0
	IYNOW=IY-IYSTART
 
C Loading the buffer with the first limits :
 
	DO M=IXSTART,NPL1+IXSTART-1
	  VV1=(IMAGE(M,IY)-XMIN1)*XND1
	  VV1=MAX(1.,VV1)
	  VV1=MIN(255.,VV1)
 	  IBUFF1(M-IXSTART+1)=IIFIX(VV1)
	END DO
 
C Plane 0:
	CALL GRWLW(IBUFF1,IXSTART1,IYNOW,ILEN,IDW,IDH,ICH0,IZCLAM)
	CALL GRSBFD
 
C Loading the buffers 2 and 3 with the second and third limits :
 
	IF(ANS_PLAN.NE.'N'.AND.ANS_PLAN.NE.'n')THEN
 
	DO M=IXSTART,NPL1+IXSTART-1
	  VV1=(IMAGE(M,IY)-XMIN2)*XND2
	  VV1=MAX(1.,VV1)
	  VV1=MIN(255.,VV1)
 	  IBUFF1(M-IXSTART+1)=IIFIX(VV1)
	END DO
 
C Plane 1:
	CALL GRWLW(IBUFF1,IXSTART1,IYNOW,ILEN,IDW,IDH,ICH1,IZCLAM)
	CALL GRSBFD
 
 	DO M=IXSTART,NPL1+IXSTART-1
	  VV1=(IMAGE(M,IY)-XMIN3)*XND3
	  VV1=MAX(1.,VV1)
	  VV1=MIN(255.,VV1)
 	  IBUFF1(M-IXSTART+1)=IIFIX(VV1)
	END DO
 
C Plane 2:
	CALL GRWLW(IBUFF1,IXSTART1,IYNOW,ILEN,IDW,IDH,ICH2,IZCLAM)
	CALL GRSBFD
 
	ENDIF
 
C End of the current line :
600	CONTINUE
 
	CLOSE(1)
	GO TO 80
 
C*********************************************************************
C Loading the Look-up tables
280	CALL LUT_1(LUT1,IOP4,VIOP4,NIOP4)
 
	GO TO 80
C-------------------------------------------------------------------
 
C Colors with the video-drivers :
380	PRINT *,' ENTER THE NUMBERS OF THE IMAGE PLANES (0, 1, OU 2)'
	PRINT *,' ASSOCIATED WITH THE THREE COLORS : RED, GREEN, BLUE'
	READ(5,*) JJ1,JJ2,JJ3
	CALL GRVIN(0,0,JJ1,JJ2,JJ3)
	CALL GRSBFD
	GO TO 80
 
C--------------------------------------------------------------------
C ZOOM :
 
c zoom :
480	PRINT *,' ZOOM FACTOR (0,1,2 or 3) ?'
	READ(5,*) KLM
 
C Enable the zoom and pan on:
	CALL GRZON(0,1)
	CALL GRSBFD
 
C Reading the position :
	CALL GRZCR(0,I2X,I2Y,ENT,F1,F2)
	CALL GRSBFD
	PRINT *,' GRZCR : X,Y',I2X,I2Y
 
C Zoom : (KLM)
	CALL GRZFC(0,KLM)
	CALL GRSBFD
	CALL GRZCL(0,I2X,I2Y)
	CALL GRSBFD
 
C
	CALL GRZCO(0,1)		!CURSOR ON
	CALL GRSBFD
 
	GO TO 80
 
C-----------------------------------------------------------------------
C Cursor function :
580	IF(.NOT.INPUT_FILE)THEN
 
C Opening the file corresponding to the displayed image :
	  PRINT *,' PLEASE ENTER THE FILE CORRESPONDING TO THE DISPLAY'
	  READ(5,10) FILENAME
	  CALL JLP_READIMAG(IMAGE,NPL,NL,IDIM,FILENAME,COMMENTS)
	  WRITE(3,185)FILENAME
	  WRITE(3,186)COMMENTS
	  INPUT_FILE=.TRUE.
C IXSTART, IYSTART Position of the first pixel to be displayed :
	IXSTART=1
	IYSTART=1
C PB while reading with the cursor when IL.NE.1 ...
 	IF(NL.GT.512.OR.NPL.GT.512)THEN
	  PRINT *,' MORE THAN 512x512 !'
	  PRINT *,' Enter the coordinates X,Y of the first pixel:'
	  READ(5,*) IXSTART,IYSTART
	ENDIF
 
	  NPL1=MIN0(NPL-IXSTART+1,512)
	  NL1=MIN0(NL-IYSTART+1,512)
	ENDIF
 
C Enable the zoom and pan on: (cursor fixed at the center of the screen)
	CALL GRZON(0,1)
	CALL GRSBFD
 
C Displaying the cursor :
	CALL GRZCO(0,1)
	CALL GRSBFD
 
	WRITE(3,70)XMIN1,XMAX1,XMIN2,XMAX2,
     1	XMIN3,XMAX3
70	FORMAT(/,' SEUILS DES IMAGES 1, 2 ET 3:',/,
     1	3(2(X,G12.3),X))
	WRITE(3,69)JJ1,JJ2,JJ3,IOP4,VIOP4,NIOP4
69	FORMAT(2X,'JJ1, JJ2, JJ3 :',3(2X,I2),/,
     1	'  OPTION LUT CHOISIE:',2X,I5,2X,G12.4,2X,I5,/)
 
	WRITE(3,72)
72	FORMAT(/,2X,'ABSCISSE',4X,'ORDONNEE',4X,'INTENSITE',/)
 
	PRINT *,' DO YOU WANT A CROSS AT THE LOCATION OF THE CURSOR ?(N)'
	READ(5,10) ANS
	DRAW_CROSS=(ANS.EQ.'y'.OR.ANS.EQ.'Y')
 
	PRINT *,' TAPER "RETURN" POUR CHAQUE POINTE, S POUR SORTIR'
17	PRINT 71
71	FORMAT('  ?',$)
	READ(5,10) ANS
 
C Exit :
	   IF(ANS.EQ.'S')THEN
	   CLOSE (1)
	   GO TO 80
	   ENDIF
 
C Reading the position of the cursor on plane 0 :
15	CALL GRZCR(0,I2X,I2Y,ENT,F1,F2)
	CALL GRSBFD
 
C Testing "ENTER" of the "CURSOR CONTROL UNIT"
C	IF(ENT.NE.1) GO TO 15
 
C Adding 1 to this position since the display starts at (0,0)
	I2X=I2X+1
	I2Y=I2Y+1
 
C Test to be check if the point is on the image :
	  IF(I2X.LT.1.OR.I2X.GT.NPL1
     1	.OR.I2Y.LT.1.OR.I2Y.GT.NL1)THEN
	    PRINT 77
77	    FORMAT(' POINT OUTSIDE OF THE IMAGE ')
	    WRITE(3,79)
79	    FORMAT(2X,'-1000',8X,'-1000',6X,'       0.000')
	    GO TO 17
	  ENDIF
 
C Reading the value of the point :
	IX=I2X+IXSTART-1
	IY=I2Y+IYSTART-1
	PRINT 73,I2X,I2Y,IMAGE(IX,IY)
	WRITE(3,73)I2X,I2Y,IMAGE(IX,IY)
73	FORMAT(2X,I5,8X,I5,6X,F12.3)
 
C Drawing a cross at the position of the cursor :
	IF(DRAW_CROSS)THEN
C Integer*4 in input for GRINNEL_CROSS...
	  IIIX=I2X
	  IIIY=I2Y
 	  CALL GRINNEL_CROSS(IIIX,IIIY,5,'W')
	ENDIF
 
	GO TO 17
C--------------------------------------------------------------------
C "Patch" :
 
C Input of the parameters if not yet entered :
880	IF(.NOT.INPUT_LEVEL)THEN
	  PRINT *,' INPUT OF THE PARAMETERS FOR THE CURRENT IMAGE :'
	  PRINT *,' ENTER :',
     1	' MIN1,MAX1,MIN2,MAX2,MIN3,MAX3 (REAL)'
	  READ(5,*) XMIN1,XMAX1,XMIN2,XMAX2,XMIN3,XMAX3
	  INPUT_LEVEL=.TRUE.
	ENDIF
 
C Input of the file if not yet entered :
	IF(.NOT.INPUT_FILE)THEN
	  READ(5,10) FILENAME
	  CALL JLP_READIMAG(IMAGE,NPL,NL,IDIM,FILENAME,COMMENTS)
	  INPUT_FILE=.TRUE.
	  WRITE(3,185)FILENAME
	  WRITE(3,186)COMMENTS
C IXSTART, IYSTART Position of the first pixel to be displayed :
	IXSTART=1
	IYSTART=1
C PB while reading with the cursor when IL.NE.1 ...
 	IF(NL.GT.512.OR.NPL.GT.512)THEN
	  PRINT *,' MORE THAN 512x512 !'
	  PRINT *,' Enter the coordinates X,Y of the first pixel:'
	  READ(5,*) IXSTART,IYSTART
	ENDIF
	  NPL1=MIN0(NPL-IXSTART+1,512)
	  NL1=MIN0(NL-IYSTART+1,512)
	ENDIF
 
	IF(IOP.EQ.8)THEN
C Calling Patch :
	   CALL PATCH2(IMAGE,NPL,NL,IXSTART,IYSTART,IDIM)
	ELSE
C Correction of bad columns :
	   PRINT *,' CORRECTION OF  1.SINGLE  2.DOUBLE  3.TRIPLE',
     1	'   BAD COLUMN(S) ?'
	   READ(5,*) IBC
	   PRINT *,' ENTER THE NUMBER OF THE FIRST BAD COLUMN :'
	   READ(5,*) IBADC
 
	   IF(IBC.EQ.1)THEN
	      CALL BADCOLUMN1(IMAGE,NPL,NL,IBADC)
	   ELSEIF(IBC.EQ.2)THEN
	      CALL BADCOLUMN2(IMAGE,NPL,NL,IBADC)
	   ELSEIF(IBC.EQ.3)THEN
	      CALL BADCOLUMN3(IMAGE,NPL,NL,IBADC)
	   ENDIF
 
C Displaying the columns on the Grinnel :
	   I1=IBADC
	   I2=IBADC+2
	   DO J=IYSTART,NL+IYSTART
	     CALL GRINNEL_ROW(IMAGE,NPL,NL,I1,I2,J)
	   END DO
	ENDIF
	
 
C Output :
        WRITE(6,*) 'Output file: '
        READ(5,10) FILENAME_OUT
	CALL JLP_WRITEIMAG(IMAGE,NPL,NL,IDIM,FILENAME_OUT,COMMENTS)
	
	GO TO 80
 
C--------------------------------------------------------------------
C Inspecting the image :
 
980	CALL INSPECT_IMAGE(IMAGE,NPL,NL,IDIM,FILENAME,COMMENTS)
 
	GO TO 80
 
C-----------------------------------------------------------------
C End :
999	CALL GRSEND
	CLOSE(3)
1000	FORMAT(1X,'COORD :',2I4,' VALEUR LUE :',3I6)
	PRINT *,'LES VALEURS DES POINTES SONT EN GRINNEL.DAT'
	STOP
 
1001	PRINT *,' ERROR WHILE READING THE INPUT FILE'
	STOP
 
	END
C----------------------------------------------------------------------	
	include 'jlpsub:gri_set.for'
	include 'jlpsub:badcolumn.for'
	include 'jlpsub:inspect_image.for'
	include 'jlpsub:patch_set.for'
