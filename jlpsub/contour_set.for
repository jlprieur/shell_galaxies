C*************************************************************
C Set of subroutines to find contours in an integer image
C Originally from John Godwin (Oxford University) but
C modified quite a lot...
C Version of July 27th 1987. JLP.
C
C Possibility of selecting the longest contour if LONGESTCONT = .TRUE.
C Contains :
C   GGROPE, GGROPE1, SAVAGE, BBBONE, ZAPATA, BLOCK DATA BD
C
C Warning !! because of the block data, this set should not be included
C in a library without trouble...
C
C*************************************************************
C	SUBROUTINES GGROPE, GGROPE1
C Subroutine to determine the starting points for the contour
C (KV) in an integer image
C
C There is a subtle use of even/odd numbers when a pixel
C is found to belong to a contour that I still do not fully understand.
C But it works very well.
C*************************************************************
	SUBROUTINE GGROPE(INPUT,NX,NY,IDIM1,OX,OY,CV1,LONGESTCONT)
C---------------------------------------------------------
	REAL*4 INPUT(IDIM1,*)
	INTEGER*4 ISIZE,PNTR_IMAGE,MADRID(1)
	LOGICAL LONGESTCONT
	COMMON /VMR/MADRID
 
C Getting memory space for the conversion to an integer image:
	ISIZE=NX*NY*4
	CALL JLP_GETVM(PNTR_IMAGE,ISIZE)
 
C Calling the main routine:
	CALL GGROPE1(INPUT,NX,NY,IDIM1,MADRID(PNTR_IMAGE),
	1	OX,OY,CV1,LONGESTCONT)
	CALL JLP_FREEVM(PNTR_IMAGE,ISIZE)
	RETURN
	END
C----------------------------------------------------------
C Main routine:
C----------------------------------------------------------
	SUBROUTINE GGROPE1(INPUT,NX,NY,IDIM1,IMAGE,
	1	OX,OY,CV1,LONGESTCONT)
	REAL*4 INPUT(IDIM1,*)
	INTEGER*4 IMAGE(NX,NY)
	LOGICAL LONGESTCONT,SKELETON
 
	COMMON/BLOCK1/IX,IY,IDX,IDY,XL,YL,XH,YH,AAR,XX1,YY1
	COMMON/ZORN/LX,LY,CV,ISS,INX(8),INY(8),IPT(3,3),KV,MJ,NB
	COMMON/BLOCK9/ LMAX,NCON
	COMMON/BLOCKB1/ XXX(6000),YYY(6000)
	COMMON/CONTOUR1/XCENTRE(1000),YCENTRE(1000),
	1	RADIUS(1000),RADMIN,NCONT1,SKELETON
	ISS=0.
 
C Conversion to an integer image
C Getting only even numbers from the rows 2 to NY-1
	FM=1000.
	 DO J=2,NY-1
	   DO I=1,NX
	    IMAGE(I,J)=2*NINT(FM*INPUT(I,J)/2.)
	   END DO
	 END DO
 
C Loading the level to be found in the common block ZORN
	CV=CV1*FM
	KV=NINT(CV)
 
C Trying to find a starting point in the first row...
	DO 110 I=1,NX-1
	  IF(IMAGE(I,1).GE.KV) GO TO 110
	  IF(IMAGE(I+1,1).LT.KV) GO TO 110
C If KV is between IMAGE(I,1) and IMAGE(I+1,1) then
C starting point = IX+1,IY
C and initial direction  -1,0
	  IX=I+1
	  IY=1
	  IDX=-1
	  IDY=0
	  CALL SAVAGE(NX,NY,NX,IMAGE,OX,OY,LONGESTCONT)
110	CONTINUE
 
C Trying to find a starting point in the last column...
	DO 20 I=1,NY-1
	IF(IMAGE(NX,I).GE.KV) GO TO 20
	IF(IMAGE(NX,I+1).LT.KV) GO TO 20
	 IX=NX
	 IY=I+1
	 IDX=0
	 IDY=-1
	CALL SAVAGE(NX,NY,NX,IMAGE,OX,OY,LONGESTCONT)
20	CONTINUE
 
C Trying to find a starting point in the last row...
	DO 30 I=1,NX-1
	K=NX+1-I
	IF(IMAGE(K,NY).GE.KV) GO TO 30
	IF(IMAGE(K-1,NY).LT.KV) GO TO 30
	 IX=K-1
	 IY=NY
	 IDX=1
	 IDY=0
	CALL SAVAGE(NX,NY,NX,IMAGE,OX,OY,LONGESTCONT)
30	CONTINUE
 
C Trying to find a starting point in the first column...
	DO 40 I=1,NY-1
	K=NY-I+1
	IF(IMAGE(1,K).GE.KV) GO TO 40
	IF(IMAGE(1,K-1).LT.KV) GO TO 40
	 IX=1
	 IY=K-1
	 IDX=0
	 IDY=1
	CALL SAVAGE(NX,NY,NX,IMAGE,OX,OY,LONGESTCONT)
40	CONTINUE
 
C Then working on the whole image :
	ISS=1
	DO 10 J=2,NY-1
	  DO 10 I=1,NX-1
C Look for a starting point on an even value (i.e. not altered by "SAVAGE")
	IF((IMAGE(I,J).LT.KV.AND.IMAGE(I+1,J).GE.KV)
	1	.AND.MOD(IMAGE(NX+1-I,J),2).EQ.0) THEN
	     IX=I+1
	     IY=J
	     IDX=-1
	     IDY=0
	    CALL SAVAGE(NX,NY,NX,IMAGE,OX,OY,LONGESTCONT)
	  ENDIF
10	CONTINUE
 
	END
 
C*************************************************************
C Subroutine SAVAGE to folow the contour starting at IX,IY
C with the initial direction (?) IDX,IDY
C**************************************************************
	SUBROUTINE SAVAGE(NX,NY,IDIM,IMAGE,OX,OY,LONGESTCONT)
	LOGICAL LONGESTCONT,SKELETON
	INTEGER*4 IMAGE(IDIM,*)
	INTEGER*4 JT
	COMMON/BLOCK1/IX,IY,IDX,IDY,XL,YL,XH,YH,AAR,XX1,YY1
	COMMON/ZORN/LX,LY,CV,ISS,INX(8),INY(8),IPT(3,3),KV,NJ,NB
	COMMON/BLOCK3/IT,IG,N,X(6000),Y(6000),KL
	COMMON/BLOCKB1/XXX(6000),YYY(6000)
	COMMON/BLOCK9/ LMAX,NCON
	COMMON/CONTOUR1/XCENTRE(1000),YCENTRE(1000),
	1	RADIUS(1000),RADMIN,NCONT1,SKELETON
	DATA T/.001/
C JLP91 JT=0
	JT=0
	N=0
	IG=N
	ST=IG
	KL=1
	IX0=IX
	IY0=IY
	IS=IPT(IDX+2,IDY+2)
	IS0=IS
	KG=4*KV
5	CALL BBBONE(NX,NY,IDIM,IMAGE)
	IF(IT+JT.LT.2) GO TO 49
	M=N-1
	DCP=X(M)
	X(M)=X(N)
	X(N)=DCP
	DCP=Y(M)
	Y(M)=Y(N)
	Y(N)=DCP
49	IS=IS+1
	JT=IT
9	IF(IS.GT.8) IS=IS-8
	IDX=INX(IS)
	IDY=INY(IS)
	IX2=IX+IDX
	IY2=IY+IDY
	IF(IX2.GT.NX) GO TO 50
	IF(IY2.GT.NY) GO TO 74
	IF(MIN(IX2,IY2).EQ.0) GO TO 74
	IF(IX.NE.IX0) GO TO 12
	IF(IY.NE.IY0) GO TO 12
	IF(IS.EQ.IS0) GO TO 73
12	KKMM=IMAGE(IX2,IY2)
	IF(KV.GT.KKMM) GO TO 5
C Check if the value is even or odd :
	IF(MOD(IS,2).EQ.1) GO TO 6
	KKMM=IMAGE(IX,IY)
	KKMM=KKMM+IMAGE(IX2,IY)
	KKMM=KKMM+IMAGE(IX,IY2)
	KKMM=KKMM+IMAGE(IX2,IY2)
	IF(KKMM.LT.KG) GO TO 5
	IF(INX(IS-1).EQ.0) GO TO 215
	IX=IX+IDX
	IDX=-IDX
	IG=1
	CALL BBBONE(NX,NY,IDIM,IMAGE)
	IX=IX+IDX
	GO TO 6
215   IY=IY+IDY
	IDY=-IDY
	IG=1
	CALL BBBONE(NX,NY,IDIM,IMAGE)
	IY=IY+IDY
6	 IF(IX.LE.1) GO TO 100
	IF(IMAGE(IX-1,IY).GE.KV) GO TO 100
	M=NX-IX+2
C Loading an odd value in this position :
	IMAGE(M,IY)=2*(IMAGE(M,IY)/2)+1
100   IS=IS+5
	IX=IX2
	IY=IY2
	GO TO 9
50	IF(IMAGE(IX-1,IY).GE.KV) GO TO 73
C Loading an odd value in this position :
	IMAGE(2,IY)=2*(IMAGE(2,IY)/2)+1
74	IF(ISS.EQ.1)RETURN
73	IF(N.LT.3.AND.KL.EQ.1)RETURN
	NCON=NCON+1
 
C Return if the contour is too long:
	IF(N.GT.6000) THEN
	  WRITE(9,1090)
	  WRITE(6,1090)
1090	  FORMAT(' TOO LONG A CONTOUR FOUND')
	  LMAX=6001
	  RETURN
	ENDIF
C
C  Here we try to decide whether the contour found is really the
C  one we want. The criteria are a bit arbitrary, but seem to
C  work in all cases I have come across.
C
C N is the number of points of the current contour
C We compute the center of gravity of this contour:
	  X00=0.
	  Y00=0.
	   DO I=1,N
	     X00=X00+X(I)
	     Y00=Y00+Y(I)
	   END DO
	  X00=X00/FLOAT(N)
	  Y00=Y00/FLOAT(N)
C
C  If it is the contour we want (at least temporarily),
C  we fill up the contour store XXX and YYY with
C  the X and Y points of the current contour.
C
	IF(LONGESTCONT)THEN
	 X01=ABS(X00-OX)
	 Y01=ABS(Y00-OY)
C Old value:
C	 RTOL=FLOAT(N)/6.
	 RTOL=MAX(MAX(NX,NY)/50,5)
	  IF((Y01.LE.RTOL).AND.(X01.LE.RTOL).AND.(N.GT.LMAX))THEN
	   LMAX=N
	    DO I=1,N
	     XXX(I)=X(I)
	     YYY(I)=Y(I)
	    END DO
	  ENDIF
	ENDIF
 
C Compute the position of the centre and the maximum radius of the contour :
C When the minimum radius is null (only one point), the radius is
C arbitrarily set to 0.25 (multiplied by 4 in REMSTAR it will give 1 for
C the diameter of the patch to be removed by "PATCH").
	IF(SKELETON)THEN
	RADMINI2=RADMIN*RADMIN
	RADMAX1=0.0625
	 DO I=1,N
	  WORK=(X(I)-X00)**2 + (Y(I)-Y00)**2
	  WORK=AMAX1(WORK,0.0625)
	  RADMAX1=AMAX1(WORK,RADMAX1)
	 END DO
	 IF(RADMAX1.GE.RADMINI2)THEN
	  NCONT1=NCONT1+1
	  RADIUS(NCONT1)=SQRT(RADMAX1)
	  XCENTRE(NCONT1)=X00
	  YCENTRE(NCONT1)=Y00
	 ENDIF
	ENDIF
 
C Initialization for the next step ?
	CALL ZAPATA(N,N)
	RETURN
	END
C******************************************************************
C Subroutine BBBONE to provide the arrays X and Y of the contour
C******************************************************************
	SUBROUTINE BBBONE(NX,NY,IDIM,IMAGE)
	INTEGER*4 IMAGE(IDIM,*)
	COMMON/BLOCK1/IX,IY,IDX,IDY,XL,YL,XH,YH,AAR,XX1,YY1
	COMMON/ZORN/LX,LY,CV,ISS,INX(8),INY(8),IPT(3,3),KV,NJ,NB
	COMMON/BLOCK3/IT,IG,N,X(6000),Y(6000),KL
 
	IF(N.GE.NB) THEN
	   IF(KL.GE.30) THEN
	     WRITE(9,43)
	     WRITE(6,43)
43	     FORMAT(' FATAL ERROR IN BBBONE : CONTOUR TOO LONG,'
	1	' MEMORY FULL')
	     STOP
 	   ENDIF
 	  M=N-1
C Initialization for the next step ?
46	  CALL ZAPATA(M-1,M)
	  X(1)=X(M)
	  X(2)=X(N)
	  Y(1)=Y(M)
	  Y(2)=Y(N)
	  KL=KL+1
	  N=2
	ENDIF
 
C The next point has been found :
	N=N+1
	IT=0
	I=IMAGE(IX,IY)
C Linear interpolation to find where the contour actually goes :
	  IF(IDX.EQ.0)THEN
	    X(N)=FLOAT(IX)
	    Y(N)=FLOAT(IDY*(I-KV))/FLOAT(I-IMAGE(IX,IY+IDY))+FLOAT(IY)
	    RETURN
	  ENDIF
	  IF(IDY.EQ.0)THEN
	    Y(N)=FLOAT(IY)
	    X(N)=FLOAT(IDX*(I-KV))/FLOAT(I-IMAGE(IX+IDX,IY))+FLOAT(IX)
	    RETURN
	  ENDIF
	IX2=IX+IDX
	IY2=IY+IDY
	DCP=FLOAT(I+IMAGE(IX2,IY)+IMAGE(IX,IY2)+IMAGE(IX2,IY2))/4.
	 IF(IG.NE.1.AND.DCP.GT.CV)THEN
	   IT=1
	   CR=DCP-FLOAT(IMAGE(IX2,IY2))
	 ELSE
	   IG=0
	   CR=FLOAT(I)-DCP
	 ENDIF
 
	 IF(ABS(CR).GE.0.1)THEN
	  DCP=.5*(1.+(DCP-CV)/CR)
	  X(N)=FLOAT(IDX)*DCP+FLOAT(IX)
	  Y(N)=FLOAT(IDY)*DCP+FLOAT(IY)
	 ELSE
	  N=N-1
	 ENDIF
	
	RETURN
	END
C****************************************************************
C Initialization ??
	SUBROUTINE ZAPATA(M,I)
	COMMON/BLOCK1/IX,IY,IDX,IDY,XL,YL,XH,YH,AAR,XX1,YY1
	COMMON/ZORN/LX,LY,CV,ISS,INX(8),INY(8),IPT(3,3),KV,NJ,NB
	COMMON/BLOCK3/IT,IG,N,X(6000),Y(6000),KL
	IF(KL.NE.1) GO TO 10
	XX1=X(1)
	YY1=Y(1)
	XL=1000.
	YL=XL
	XH=0.
	YH=XH
	AAR=YH
10	CONTINUE
	IPT(2,2)=IPT(2,2)+I
	END
C******************************************************************
C BLOCK DATA BD
C Initialize the common block ZORN
C (Only once, when starting the main program).
C******************************************************************
      BLOCK DATA BD
      COMMON/ZORN/LX,LY,CV,ISS,INX(8),INY(8),IPT(3,3),KV,NJ,NB
      DATA INX/-1,-1,0,1,1,1,0,-1/,INY/0,1,1,1,0,-1,-1,-1/
      DATA IPT/8,7,6,1,0,5,2,3,4/
      DATA NJ/6000/,NB/6000/
      END
