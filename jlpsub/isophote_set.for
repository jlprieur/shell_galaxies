C---------------------------------------------------------------------
C Set of subroutines to draw contours with SPLOT graphic package.
C
C Contains :
C ISOCONTOUR, TRAC, TRAC1, LIRE11
C
C JLP 
C 04-04-2004
C---------------------------------------------------------------------
        SUBROUTINE ISOCONTOURS(IMAGE,NX,NY,IDIM,NX1,
     1         NX2,ISTEPX,NY1,NY2,ISTEPY,NIV,NNIV,
     1         CHAR1,CHAR2,TITLE,
     1         PLOTDEV,INTERAC,TICKS_IN,BOX_NUMBERS,IN_FILE,IN_COMMENTS)
C
	PARAMETER (IDIM1=1100)
	REAL*4 IMAGE(IDIM,*)
	REAL*4 XOUT(200),YOUT(200),NIV(100)
	REAL*4 ID1(IDIM1),ID2(IDIM1),ID(5)
	REAL*4 TDX,TDY
	INTEGER*4 JK(4),IN_FRAME,NNIV,IDV1
	INTEGER*4 NXD,NYD,ISTEPX,ISTEPY
	INTEGER*4 NX1,NX2,NY1,NY2,ITEX_FLAG,IHARDCOPY_DEV
        CHARACTER IN_FILE*60,IN_COMMENTS*80
	LOGICAL*4 INTERAC
	INTEGER*4 TICKS_IN,BOX_NUMBERS
	CHARACTER PLOTDEV*32
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40
        REAL*4 OFFX,OFFY,AXLEN,AYLEN,TDX,TDY

C Output points :
	COMMON/STR_OUTPUT/XOUT,YOUT,NOUT

C NXD: Number of pixels to draw, per line
	NXD=(NX2-NX1)/ISTEPX+1

C NYD : Number of lines to draw
	NYD=(NY2-NY1)/ISTEPY+1

C Frame and scale:
	IPLAN=1
	I=NX2-NX1
	J=NY2-NY1

C Select the device (ITEX_FLAG=1 for nice fonts, 0 otherwise)
	ITEX_FLAG=1
        TITLE="Isophote plot"
	CALL JLP_SPDEVICE_IMAGE(PLOTDEV,I,J,GAMMA1,TITLE,IDV1)

C First buffer:
	CALL LIRE11(IMAGE,NY1,NX1,NX2,ISTEPX,ID1,IDIM)
 
C
C Loop on the lines
C
	DO 25 IY=1,NYD-1
	IZY=IY
	NYI=NY1+IZY*ISTEPY
	CALL LIRE11(IMAGE,NYI,NX1,NX2,ISTEPX,ID2,IDIM)
C
C Loop on the points
C
	DO 20 IX=1,NXD-1
	IZX=IX
	ID(1)=ID1(IZX)
	ID(2)=ID1(IZX+1)
	ID(4)=ID2(IZX)
	ID(3)=ID2(IZX+1)
	ID(5)=ID(1)
	DMI=AMIN1(ID(1),ID(2),ID(3),ID(4))
	DMA=AMAX1(ID(1),ID(2),ID(3),ID(4))
C****************************************************************
C	BOUCLE SUR LES NIVEAUX ET TRACE DES ISOPHOTES DANS
C	LA CASE ELEMENTAIRE ID(5)
C	
C	DDT : current level
C
C****************************************************************
	   DO 22 INIV=1,NNIV
	   DDT=NIV(INIV)
	   IF(DDT.GT.DMA.OR.DDT.LT.DMI) GO TO 22
 
	     DO 30 K=1,4
	     JK(K)=0
	     P=(DDT-ID(K))*(DDT-ID(K+1))
	     IF(P.LE.0.) JK(K)=1
C	JK(K)=1 I.E. ISOPHOTE ENTRE K ET K+1
C	JK(K)=2 I.E. ISOPHOTE SUR K ET SUR K+1
	     IF(P.EQ.0..AND.ID(K).EQ.ID(K+1)) JK(K)=2
30	     CONTINUE
 
	   MIJ=MIN(JK(1),JK(2),JK(3),JK(4))
C
C Case when the isophote goes through the grid
C between the points
C   (MIJ=0) MIJ.NE.1 substituted by MIJ.EQ.0
C
	   IF(MIJ.EQ.0) THEN
	     DO 31 K=1,4
	     IF(JK(K).EQ.1) K2=K
31	     CONTINUE
           CALL TRAC(K2,DDT,ID,IZX,IZY,ISTEPX,ISTEPY,NX1,NY1,0,IDV1)
	   K3=K2-1
	   IF(ID(K2).EQ.DDT) K3=K2-2
	     DO 32 K=1,K3
	     IF(JK(K).EQ.1) K2=K
32	     CONTINUE
	   CALL TRAC(K2,DDT,ID,IZX,IZY,ISTEPX,ISTEPY,NX1,NY1,1,IDV1)
	   GO TO 22
	   ENDIF
 
	   MAJ=MAX(JK(1),JK(2),JK(3),JK(4))
C
C Case when all the isophote circles two points of the grid
C
	   IF(MAJ.NE.2) THEN
	   DK=(ID(1)-DDT)*(ID(3)-ID(4)-ID(2)+ID(1))-
     1	(ID(1)-ID(2))*(ID(1)-ID(4))
	   K=0
	      IF(DK.LT.0.) K=1
	   CALL TRAC(1,DDT,ID,IZX,IZY,ISTEPX,ISTEPY,NX1,NY1,0,IDV1)
	   CALL TRAC(2+2*K,DDT,ID,IZX,IZY,ISTEPX,ISTEPY,NX1,NY1,1,IDV1)
	   CALL TRAC(3-K,DDT,ID,IZX,IZY,ISTEPX,ISTEPY,NX1,NY1,0,IDV1)
	   CALL TRAC(4-K,DDT,ID,IZX,IZY,ISTEPX,ISTEPY,NX1,NY1,1,IDV1)
	   GO TO 22
	   ENDIF
C
C Case when the maximum is 2 ,i.e. two successive values are
C identical in the grid, and equal the level.
C
	   K2=0
 
	     DO 330 K=1,4
	     P=(ID(K+1)-DDT)*(ID(K)-DDT)
	     IF(P.LT.0.) K2=K
330	     CONTINUE
 
C
C Here all the values of the grid are identical
C
	   IF(K2.EQ.0) THEN
	     K2=5
	     DO 335 K=2,4
	     IF(ID(K).NE.DDT) K2=K
335	     CONTINUE
 
	     DO 336 L=1,2
	     K3=K2-1
	     K4=K3/2
	     X0=MOD(K4,2)
	     Y0=(K3-1)/2
	     TX=(FLOAT(IZX-1)+X0)*ISTEPX+NX1
	     TY=(FLOAT(IZY-1)+Y0)*ISTEPY+NY1
	     IPEN=4-L
	     CALL STR_PLOT(TX,TY,IPEN,IDV1)
	     K2=MOD(K2,4)+2
336	     CONTINUE
 
	     GO TO 22
	     ENDIF
 
C
C Here K2 .ne. 0.
C
	   CALL TRAC(K2,DDT,ID,IZX,IZY,ISTEPX,ISTEPY,NX1,NY1,0,IDV1)
	   K4=K2+1
	   K3=MOD(K4,4)+1
	   U=(DDT-ID(K2))/(ID(K2+1)-ID(K2))
	   K1=K3/2
	   IK1=MOD(K1,2)
	   IK2=MOD(K3,2)
 
	   IF(IK2) 331,331,332
332	   X0=U+IK1-2.*U*IK1
	   X0=1.-X0
	   Y0=IK1
	   GO TO 333
331	   X0=IK1
	   Y0=-U-IK1+2*U*IK1+1.
	   Y0=1.-Y0
 
333	 CONTINUE
C Just an instruction for the label
 
	   TX=(FLOAT(IZX-1)+X0)*ISTEPX+NX1
	   TY=(FLOAT(IZY-1)+Y0)*ISTEPY+NY1
	   CALL STR_PLOT(TX,TY,2,IDV1)
 
	     DO 334 L=1,2
	     K0=K3/2
	     K0=MOD(K0,2)
	     X0=K0
	     Y0=(K3-1)/2
	     TX=(FLOAT(IZX-1)+X0)*ISTEPX+NX1
	     TY=(FLOAT(IZY-1)+Y0)*ISTEPY+NY1
	     IPEN=4-L
	     CALL STR_PLOT(TX,TY,IPEN,IDV1)
	     K3=MOD(K3,4)+1
334	CONTINUE
 
	   GO TO 22
 
C End of the loop on the levels
C
22	CONTINUE
 
C End of the loop on the points
C
20	CONTINUE
 
	DO IPL=1,NXD
	ID1(IPL)=ID2(IPL)
	END DO
C End of the loop on the lines
C
25	CONTINUE
 
 
C***************************************************
C Drawing the frame ....
	CALL JLP_SPBOX(CHAR1,CHAR2,TITLE,TICKS_IN,BOX_NUMBERS,
     1                 ITEX_FLAG,IHARDCOPY_DEV,IN_FILE,
     1                 IN_COMMENTS,IDV1) 
 
        CALL JLP_GFLUSH(IDV1)
 
C-------------------------------------------------------------
C If INTERAC (tektro) possibility of entering points
 
C Entering points as long as IN_FRAME = 1 (inside the frame)
	IF(INTERAC)THEN
301	JJ=0
	CALL JLP_WHERE(XNOW,YNOW,IN_FRAME,IBUTTON,DRAW_CROSS,IDV1)
 
94	JJ=JJ+1
 
	XOUT(JJ)=XNOW
	YOUT(JJ)=YNOW
C JLP_WHERE is written in C and returns an index starting at 0,0:
	INTX=NINT(XNOW)+1
	INTY=NINT(YNOW)+1
	  IF(INTX.LE.0.OR.INTX.GT.NX
     1	.OR.INTY.LE.0.OR.INTY.GT.NY.OR.IN_FRAME.NE.1)THEN
C	    WRITE(6,*)' Point outside of the image,',
C     1      ' IN_FRAME=',IN_FRAME
	  ELSE
	    CALL JLP_SYMBOL1(XNOW,YNOW,5,4)
            CALL JLP_GFLUSH
C	    WRITE(6,1004)JJ,XOUT(JJ),YOUT(JJ),IMAGE(INTX,INTY)
	    WRITE(1,1004)JJ,XOUT(JJ),YOUT(JJ),IMAGE(INTX,INTY)
1004	    FORMAT(' Point #',I3,' X=',F7.1,
     1	'  Y=',F7.1,' VALUE =',E12.4)
	   CALL JLP_WHERE(XNOW,YNOW,IN_FRAME,IBUTTON,DRAW_CROSS,IDV1)
           GOTO 94
	  ENDIF
 
C End of the input
 
95	NOUT=JJ-1
	WRITE(1,1003)NOUT
1003	FORMAT(/,' Number of points:',I4,/)
	
	ENDIF
 
        RETURN
	END
C **************************************************************
      SUBROUTINE TRAC(K2,DDT,ID,IZX,IZY,ISTEPX,ISTEPY,NX1,NY1,L,IDV1)
C
C Subroutine TRAC from CDCA software 
C
C SOUS PROGRAMME TRAC.FTN
C
C TRACE D'UN TRAIT DANS BDLISE
C       K2 :between 1 and 4
C	DDT : current level
C	ID(5) : current grid
C	IZX, IZY : Coordinates of the first point of the grid
C	ISTEPX, ISTEPY : Step in X and Y
C	L : either 0 or 1
C
      REAL*4 ID(*)
      INTEGER*4 ISTEPX,ISTEPY,NX1,NY1
 
        D=ID(K2+1)-ID(K2)
        IF(D.EQ.0)THEN
          U=1
	ELSE
          U=(DDT-ID(K2))/D
        ENDIF

        K1=K2/2
        IK1=MOD(K1,2)
        IK2=MOD(K2,2)
        IF(IK2.LE.0)THEN
          X0=IK1
          Y0=-U-IK1+2*U*IK1+1
	ELSE
          X0=U+IK1-2*U*IK1
          Y0=IK1
	ENDIF
	IPEN=3-L
C ITX=(FLOAT(IZX-1)+X0)*ISTEPX
C ITY=(FLOAT(IZY-1)+Y0)*ISTEPY
C CALL FTPLOT(L,ITX,ITY)
	XSTART=(X0+FLOAT(IZX-1))*ISTEPX+NX1
	YSTART=(Y0+FLOAT(IZY-1))*ISTEPY+NY1
	CALL STR_PLOT(XSTART,YSTART,IPEN,IDV1)
 
	RETURN
	END
C***********************************************************
C Personal routine to replace TRAC when I was at Stromlo
C	SUBROUTINE TRAC1(K2,DDT,ID,IZX,IZY,ISTEPX,ISTEPY,ISTAT)
C       K2 :between 1 and 4
C	DDT : current level
C	ID(5) : current grid
C	IZX, IZY : Coordinates of the first point of the grid
C	ISTEPX, ISTEPY : Scale for drawing in x and y
C	ISTAT : either 0 or 1
C************************************************************
	SUBROUTINE TRAC1(K2,DDT,ID,IZX,IZY,ISTEPX,ISTEPY,ISTAT,IDV1)
	REAL*4 ID(5)
        INTEGER*4 ISTEPX,ISTEPY,IDV1
 
	ZX=FLOAT(IZX)
	ZY=FLOAT(IZY)
 
	IF(K2.EQ.1)THEN
	ZX=ZX+0.5
	ENDIF
 
	IF(K2.EQ.2)THEN
	ZX=ZX+1.
	ZY=ZY+0.5
	ENDIF
 
	IF(K2.EQ.3)THEN
	ZX=ZX+0.5
	ZY=ZY+1.
	ENDIF
 
	IF(K2.EQ.4)THEN
	ZX=ZX
	ZY=ZY+0.5
	ENDIF
 
	XSTART=ZX*ISTEPX
	YSTART=ZY*ISTEPY
	IPEN=3-ISTAT
	CALL STR_PLOT(XSTART,YSTART,IPEN,IDV1)
	RETURN
	END
C***********************************************************
	SUBROUTINE LIRE11(IMAGE,IY,IX1,IX2,IPX,MES,IDIM)
	REAL*4 MES(*),IMAGE(IDIM,*)

	NPOINT=0
	DO I=IX1,IX2,IPX
	  NPOINT=NPOINT+1
	  MES(NPOINT)=IMAGE(I,IY)
	END DO

	RETURN
	END
