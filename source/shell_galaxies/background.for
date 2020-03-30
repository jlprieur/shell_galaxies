C++--------------------------------------------------------------------
C Program BACKGROUND
C to fit a polynomial to the background of an image.
C Options :
C -  2-D Polynomial fitting on a subsample of points (excluding 2 boxes)
C -  Compression (with local histograms) and extension with pseudo-cubic
C    splines (From A. Bijaoui)
C
C JLP Version of 09-05-94
C--------------------------------------------------------------------
	PROGRAM BACKGROUND
	PARAMETER (NCHEBY=100)
	PARAMETER (IDIM1=6000,IDIM_GRID=200)
	REAL*4 GRID(IDIM_GRID,IDIM_GRID)
        REAL*8 XMIN0,XMAX0,YMIN0,YMAX0
        REAL*8 CHEBY(NCHEBY)
	REAL*8 XX(IDIM1),YY(IDIM1),ZZ(IDIM1)
	CHARACTER IN_NAME*60,NAME*60,COMMENTS*80
        INTEGER*4 KKX1,KKY1
	INTEGER*4 NPOINT(IDIM1)
	LOGICAL HOLE
	CHARACTER ANS*1
        INTEGER PNTR_IN,PNTR_OUT,MADRID(1)
        COMMON/VMR/MADRID
	COMMON/SURFIT/CHEBY,KKX1,KKY1,XMIN0,XMAX0,YMIN0,YMAX0
 
10	FORMAT(A)
	PRINT 5
5	FORMAT(' Program BACKGROUND to compute the background of an'
     1	' image',/)
	
	CALL JLP_BEGIN
C Inquire the format :
	CALL JLP_INQUIFMT
 
80	PRINT 88
88	FORMAT(' MENU :',/,
     1	' 1. Surface fitting with a 2-D polynomial',/,
     1	' 2. Spline interpolation on a grid (for patched images)',/,
C     1	' 3. Surface fitting with cubic splines',/,
     1	' 10. Exit',/,
     1	' Enter your choice : ',$)
	READ(5,*) IOPT
 
C	PRINT 70
C70	FORMAT(' SECOND MENU : ',/,
C     1	' 1. Subtraction of the background',/,
C     1	' 2. Division by the background',/,
C     1	' Enter the option :',$)
C	READ(5,*) IOPT1
	IOPT1=1
	PRINT 71
71	FORMAT('OK: subtraction of the background')
 
C Input of the image :
	WRITE(6,*) 'Input image: '
        READ(5,10) IN_NAME
	CALL JLP_VM_READIMAG(PNTR_IN,NX,NY,IN_NAME,COMMENTS)
        IDIM=NX
 
C Memory allocation for output image:
        ISIZE=NX*NY*4
        CALL JLP_GETVM(PNTR_OUT,ISIZE)

C--------------------------------------------------------------------
C Option 1 : 2-D polynomial fit
C---------------------------------------------------------------------
 
	IF(IOPT.EQ.1)THEN
C KKX and KKY :
	PRINT *,' Order of the polynomial in X and Y :'
	READ(5,*) KKX,KKY
 
	PRINT *,' Size of the cells in X and Y ?'
	READ(5,*) IXSIZE,IYSIZE
 
	PRINT 104
104	FORMAT(' Generating the grid',/)
	HOLE=.TRUE.
	CALL GENE_GRID(MADRID(PNTR_IN),NX,NY,IDIM,GRID,NXCELL,NYCELL,
     1	IDIM_GRID,IXSIZE,IYSIZE,HOLE)
	
	PRINT 105
105	FORMAT(' Loading the arrays',/)
 
	CALL PREPARE_E02CAF(GRID,NXCELL,NYCELL,IDIM_GRID,IXSIZE,
     1	IYSIZE,XX,YY,ZZ,NPTX,NPTY,NPOINT,KKX)
 
C Output of the grid :
	PRINT *,' Output of the grid :'
        READ(5,10) NAME
	 CALL JLP_WRITEIMAG(GRID,NXCELL,NYCELL,IDIM_GRID,NAME,COMMENTS)
 
	PRINT 106
106	FORMAT(' Fitting a 2-D polynomial ',/)
	CALL SURFACEFIT_POLY(XX,YY,ZZ,NPTX,NPTY,NPOINT,KKX,KKY)
 
	PRINT *,' Subtracting the background'
	CALL FLATTEN_1(MADRID(PNTR_IN),MADRID(PNTR_OUT),NX,NY,IDIM,IOPT1)
	PRINT *,' Background subtracted'
 
	ENDIF
 
C--------------------------------------------------------------------
C OPTION 2
C Two steps :
C  - Compression of the patched image with sky determination from the
C     shape of local histograms.
C  - Pseudo-spline extension of this grid
C
C---------------------------------------------------------------------
 
	IF(IOPT.EQ.2)THEN
 
	PRINT 204
204	FORMAT(' Generating the grid with local histograms',/)
	PRINT *,' Compression factor of the image ?'
	READ(5,*) IFACT
 
	PRINT *,' Lower and upper thresholds for the histogram ?'
	READ(5,*) ZMIN,ZMAX
	PRINT *,' Noise estimate ?'
	READ(5,*) ZNOISE
 
C NTERM = Number of terms for the fit of the histograms
C (NTERM > 30 for a good fit according to A. Bijaoui
	NTERM=34
	CALL COMPRES_HISTO(MADRID(PNTR_IN),NX,NY,IDIM,GRID,NXCELL,
     1	NYCELL,IDIM_GRID,IFACT,NTERM,ZMIN,ZMAX,ZNOISE)
 
	PRINT 211
211	FORMAT(/,' Transfer of the knots of the grid ',
     1	' (Necessary for the next step)')
	CALL TRANSFER_KNOTS(GRID,NXCELL,NYCELL,IDIM_GRID)
 
	PRINT 206
206	FORMAT(/,' Extension of the grid and subtraction ',/)
 
	CALL EXTEN_SPLINE1(GRID,NXCELL,NYCELL,IDIM_GRID,IFACT,
     1                     MADRID(PNTR_OUT),IDIM)
 
	NX1=NXCELL*IFACT
	NY1=NYCELL*IFACT
 
	PRINT 207
207	FORMAT(' Do you want to subtract the background ',/,
     1	' from the original image (not patched) ? (Y)')
	READ(5,10) ANS
 
	IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN
C Entering the image :
         WRITE(6,*) 'Input file: '
         READ(5,10) IN_NAME
	 CALL JLP_VM_READIMAG(PNTR_IN,NX,NY,IN_NAME,COMMENTS)
         IDIM=NX
	ENDIF
 
C Subtracting the background :
        CALL SUBTRACT(MADRID(PNTR_IN),MADRID(PNTR_OUT),NX1,NY1,IDIM)
 
	ENDIF
 
C--------------------------------------------------------------------
C OPTION 3 : 2-D fit of cubic splines
C Does not work : E02ZAF is limited at PX=12 and PY=12 !!!!!
C---------------------------------------------------------------------
 
	IF(IOPT.EQ.3)THEN
 
	PRINT 304
304	FORMAT(' I. Generating the grid',/)
	PRINT *,'     Size of the cells in X and Y ?'
	READ(5,*) IXSIZE,IYSIZE
 
	HOLE=.TRUE.
	CALL GENE_GRID(MADRID(MADRID_IN),NX,NY,IDIM,GRID,NXCELL,NYCELL,
     1	IDIM_GRID,IXSIZE,IYSIZE,HOLE)
 
C--------------------
C Filling the holes of the grid :
	PRINT 305
305	FORMAT(/,' II. Filling the holes of the grid ',
     1	'(polynomial fitting)',/)
 
C KKX and KKY :
	PRINT *,'    Order of the polynomial in X and Y :'
	READ(5,*) KKX,KKY
 
	PRINT 306
306	FORMAT(' II. a) Loading the arrays',/)
 
	CALL PREPARE_E02CAF(GRID,NXCELL,NYCELL,IDIM_GRID,IXSIZE,
     1	IYSIZE,XX,YY,ZZ,NPTX,NPTY,NPOINT,KKX)
 
	PRINT 307
307	FORMAT(' II. b) Fitting a 2-D polynomial ',/)
	CALL SURFACEFIT_POLY(XX,YY,ZZ,NPTX,NPTY,NPOINT,KKX,KKY)
 
	PRINT 308
308	FORMAT(' II. c) Filling the holes, now',/)
	CALL FILL_3(GRID,NXCELL,NYCELL,IDIM_GRID,IXSIZE,IYSIZE)
 
C Output of the grid :
C	PRINT *,' Output of the grid :'
C        READ(5,10) NAME
C	 CALL JLP_WRITEIMAG(GRID,NXCELL,NYCELL,IDIM_GRID,IDIM2,NAME,COMMENTS)
 
C-------------------
C Fitting cubic splines :
	PRINT 309
309	FORMAT(' II. d) Fitting cubic splines ',/)
	CALL SURFACEFIT_SPLINE(GRID,NXCELL,NYCELL,IDIM_GRID,IXSIZE,IYSIZE)
 
C	PRINT 310
C310	FORMAT(/,' III. Subtraction : calling "FLATTEN_3',/)
C	CALL FLATTEN_3(GRID,NXCELL,NYCELL,IDIM_GRID,
C     1                 MADRID(PNTR_IN),MADRID(PNTR_OUT),
C     1	NX,NY,IDIM,IXSIZE,IYSIZE,IOPT1)
 
	ENDIF
 
C----------------------------------------------------------------------
C Output
C----------------------------------------------------------------------
         WRITE(6,37)
37       FORMAT(' Output image:')
         READ(5,10) NAME
         WRITE(COMMENTS,33) IN_NAME(1:20)
33       FORMAT('Background subtr. from: ',A20)
	 CALL JLP_WRITEIMAG(MADRID(PNTR_OUT),NX,NY,IDIM,NAME,COMMENTS)
 
        CALL JLP_END
	STOP
	END
C*************************************************************
C	Subroutine MEAN_SIGMA to compute the mean and sigma
C	of an area defined by IMIN, IMAX JMIN and JMAX
C
C IFLAG : Detection of 0 (Outside of the mask)
C*************************************************************
	SUBROUTINE MEAN_SIGMA(IMAGE,IMIN,IMAX,JMIN,
     1	JMAX,XMEAN,SIGMA,IDIM,IFLAG)
	REAL*4 IMAGE(IDIM,*)
	REAL*8 TOTSUM,TOT2,XMEAN1,WORK
 
	IFLAG=1
C First passage to calculate the mean
	IT=0
        NSUM=0
        TOTSUM=0.0D0
        TOT2=TOTSUM
 
        DO 18 J=JMIN,JMAX
        DO 18 I=IMIN,IMAX
        AIJ=IMAGE(I,J)
	  IF(AIJ.EQ.0.0)THEN
	  IFLAG=0
	  RETURN
	  ENDIF
	NSUM=NSUM+1
        TOTSUM=TOTSUM+AIJ
        TOT2=TOT2+AIJ**2
18      CONTINUE
 
C TOTSUM : first mean
	TOTSUM=TOTSUM/DFLOAT(NSUM)
 
C TOT2 : first standard deviation
        TOT2=(TOT2/DFLOAT(NSUM))-TOTSUM**2
        TOT2=DSQRT(TOT2)
 
30	XMEAN1=TOTSUM
 
C ZUL: upper level
	ZUL=TOTSUM+3.*TOT2
 
C ZLL: low level
        ZLL=TOTSUM-3.*TOT2
 
C Second passage to calculate the standard deviation (discarding
C some points)
        NSUM=0
        TOTSUM=0.0D0
        TOT2=TOTSUM
 
        DO 19 J=JMIN,JMAX
        DO 19 I=IMIN,IMAX
        AIJ=IMAGE(I,J)
        IF((AIJ.NE.0.0).OR.(AIJ.LE.ZUL).OR.(AIJ.GE.ZLL))THEN
        NSUM=NSUM+1
        TOTSUM=TOTSUM+AIJ
        TOT2=TOT2+AIJ**2
	ENDIF
19      CONTINUE
 
C TOTSUM : second mean (without extreme values)
        TOTSUM=TOTSUM/DFLOAT(NSUM)
 
C TOT2 : second standard deviation (in intensity)
        TOT2=(TOT2/DFLOAT(NSUM))-TOTSUM**2
        TOT2=DSQRT(TOT2)
	   IF(XMEAN1.NE.0)THEN
	   WORK=DABS((TOTSUM-XMEAN1)/XMEAN1)
	   ELSE
	   WORK=DABS(TOTSUM-XMEAN1)
	   ENDIF
	
C Loop until the mean is stable ...
	IF(WORK.GT.0.1E-09)THEN
	IT=IT+1
	PRINT *,' IT =',IT
	GO TO 30
	ENDIF
 
	XMEAN=TOTSUM
	SIGMA=TOT2
 
	RETURN
	END
C-------------------------------------------------------------
C Subroutine to fit a 2-D polynomial :
C Input arrays :
C XX all points on Y=Y(1) followed by
C    all points on Y=Y(2) followed by ...
C (Size of XX = NPTX)
C
C NPOINT(K)= Number of points XX in line K
C
C YY values of the lines Y=Y(K) in strictly increasing order.
C (Size of YY = NPTY)
C
C ZZ in the same order as XX
C (Size of ZZ = NPTX)
C
C NCHEBY > (KKX+1)*(KKY+1)   (KKX and KKY : orders of the polynomials)
C NWORK > NPTX + 2 * MAX(NPTX,NPTY) + 2*NPTY*(KKX+2) + 5*(1+MAX(KKX,KKY))
C--------------------------------------------------------------
	SUBROUTINE SURFACEFIT_POLY(XX,YY,ZZ,NPTX,NPTY,NPOINT,
     1	KKX,KKY)
	PARAMETER (NCHEBY=100)
	PARAMETER (IDIM1=6000)
	REAL*8 XX(NPTX),YY(NPTY),ZZ(NPTX)
	REAL*8 CHEBY(NCHEBY),W(IDIM1),XMIN(IDIM1),XMAX(IDIM1)
	REAL*8 NUX(IDIM1),NUY(IDIM1),WORK(IDIM1)
        REAL*8 XMIN0,XMAX0,YMIN0,YMAX0
	INTEGER*4 NPOINT(NPTY)
        INTEGER*4 KKX1,KKY1
 
	COMMON/SURFIT/CHEBY,KKX1,KKY1,XMIN0,XMAX0,YMIN0,YMAX0
C Initiallization of polynomial order in common block:
	KKX1=KKX
	KKY1=KKY
 
C Lower and upper limits :
	XMIN0=XX(1)
	XMAX0=XX(NPTX)
	  DO I=1,NPTX
	  IF(XX(I).LT.XMIN0)XMIN0=XX(I)
	  IF(XX(I).GT.XMAX0)XMAX0=XX(I)
	  END DO
	YMIN0=YY(1)
	YMAX0=YY(NPTY)
	  DO J=1,NPTY
	  IF(YY(J).LT.YMIN0)YMIN0=YY(J)
	  IF(YY(J).GT.YMAX0)YMAX0=YY(J)
	  END DO
	WRITE(6,39) XMIN0,XMAX0,YMIN0,YMAX0
39      FORMAT('xmin,xmax,ymin,ymax',4(1X,F9.2))
        IF(XMIN0.EQ.XMAX0.OR.YMIN0.EQ.YMAX0)THEN
          WRITE(6,8)
8         FORMAT('Fatal error: null dynamical range in X or Y')
          CALL JLP_END
          STOP
        ENDIF
 
C Sets the weights to 1.
	DO I=1,NPTX
	 W(I)=1.D0
	END DO
 
C Defines the min and max values in each line
	DO I=1,NPTX
	  XMIN(I)=XMIN0
	  XMAX(I)=XMAX0
	END DO
 
C No constraints on the fit :
	INUXP1=1
	INUYP1=1
 
C Calling NAG routine E02CAF to fit a polynomial in X, and in Y
	IFAIL=1
	NWORK=IDIM1
	PRINT *,' NPTX,NPTY,KKX,KKY',NPTX,NPTY,KKX,KKY
 
	CALL E02CAF(NPOINT,NPTY,KKX,KKY,XX,YY,ZZ,W,NPTX,
     1	CHEBY,NCHEBY,XMIN,XMAX,
     1	NUX,INUXP1,NUY,INUYP1,WORK,NWORK,IFAIL)
 
	IF(IFAIL.NE.0)THEN
	  PRINT 30,IFAIL
30	  FORMAT(' SURFACEFIT_POLY/Error in E02CAF   IFAIL=',I4)
	ENDIF
 
	RETURN
	END
C******************************************************************
C Subroutine to generate the array GRID for further interpolation
C GRID(NXCELL,NYCELL) : output array
C IXSIZE : lenth of the cells in X
C IYSIZE : lenth of the cells in Y
C
C If HOLE=.true. possibility of excluding some areas
C-------------------------------------------------------------------
	SUBROUTINE GENE_GRID(INPUT,NX,NY,IDIM,GRID,NXCELL,NYCELL,
     1	IDIM_GRID,IXSIZE,IYSIZE,HOLE)
	REAL*4 INPUT(IDIM,*),GRID(IDIM_GRID,*)
	LOGICAL HOLE	
 
	NXCELL=(NX/IXSIZE)
	NYCELL=(NY/IYSIZE)
c	HALFXSIZE=IXSIZE/2
c	HALFYSIZE=IYSIZE/2
	PRINT *,' SIZE OF "GRID" : NXCELL, NYCELL',NXCELL,NYCELL
 
C If HOLE=.true. possibility of excluding some areas
	IF(HOLE)THEN
 
	PRINT *,' Exclusion of 2 rectangular areas :'
	PRINT *,' Enter the limits IXMIN,IXMAX,IYMIN,IYMAX for box 1 '
	READ(5,*) IXMIN1,IXMAX1,IYMIN1,IYMAX1
	PRINT *,' Enter the limits IXMIN,IXMAX,IYMIN,IYMAX for box 2 '
	READ(5,*) IXMIN2,IXMAX2,IYMIN2,IYMAX2
 
	ENDIF
 
	
	DO KY=1,NYCELL
 
	JMIN=(KY-1)*IYSIZE +1
	JMAX=KY*IYSIZE
C	YY(NPTY)=FLOAT(JMIN)+HALFYSIZE
 
	DO KX=1,NXCELL
 
	IMIN=(KX-1)*IXSIZE +1
	IMAX=KX*IXSIZE
 
C Removing pixels inside of the exclusion boxes :
	IF(HOLE)THEN
 
C Box 1 :
	IF(((IMIN.GT.IXMIN1.AND.IMIN.LT.IXMAX1).OR.
     1	(IMAX.GT.IXMIN1.AND.IMAX.LT.IXMAX1)).AND.
     1	((JMIN.GT.IYMIN1.AND.JMIN.LT.IYMAX1).OR.
     1	(JMAX.GT.IYMIN1.AND.JMAX.LT.IYMAX1)))THEN
	GRID(KX,KY)=-1.
	GO TO 20
	ENDIF
 
C Box 2:
	IF(((IMIN.GT.IXMIN2.AND.IMIN.LT.IXMAX2).OR.
     1	(IMAX.GT.IXMIN2.AND.IMAX.LT.IXMAX2)).AND.
     1	((JMIN.GT.IYMIN2.AND.JMIN.LT.IYMAX2).OR.
     1	(JMAX.GT.IYMIN2.AND.JMAX.LT.IYMAX2)))THEN
	GRID(KX,KY)=-1.
	GO TO 20
	ENDIF
	
	ENDIF
	
C Computing the mean :
	CALL MEAN_SIGMA(INPUT,IMIN,IMAX,JMIN,JMAX,
     1	XMEAN,SIGMA,IDIM,IFLAG)
C	PRINT *,' KX,IMIN,IMAX,JMIN,JMAX,XMEAN',
C     1            KX,IMIN,IMAX,JMIN,JMAX,XMEAN
 
C Possibility of multiplying by a mask (0 when outside of the frame) :
C Therefore discarding all these values :
	IF(IFLAG.NE.0)THEN
C	XX(NPTX)=FLOAT(IMIN)+HALFXSIZE
	GRID(KX,KY)=XMEAN
	ENDIF
 
20	END DO
 
	END DO
 
	RETURN
	END
C-------------------------------------------------------------
C Subroutine to flatten the input image with option 1
C  i.e., surface fitting with a 2-D polynomial
C--------------------------------------------------------------
	SUBROUTINE FLATTEN_1(INPUT,OUTPUT,NX,NY,IDIM,IOPT)
	PARAMETER (NCHEBY=100)
C IDIM1: Max size of the lines:
	PARAMETER (IDIM1=6000)
	REAL*4 INPUT(IDIM,*),OUTPUT(IDIM,*)
	REAL*8 CHEBY(NCHEBY),W(IDIM1),WORK(IDIM1)
	REAL*8 XX(IDIM1),ZZ(IDIM1),YY
        REAL*8 XMIN0,XMAX0,YMIN0,YMAX0
        INTEGER*4 NX,NY,KKX1,KKY1
 
	COMMON/SURFIT/CHEBY,KKX,KKY,XMIN0,XMAX0,YMIN0,YMAX0
 
	IMIN=NINT(XMIN0)+1
	IMAX=NINT(XMAX0)-1
	JMIN=NINT(YMIN0)+1
	JMAX=NINT(YMAX0)-1
	PRINT *,'FLATTEN_1/imin,imax,jmin,jmax',IMIN,IMAX,JMIN,JMAX

C JLP97: I erase the output array:
        DO J=1,NY
         DO I=1,NX
          OUTPUT(I,J)=0.
         END DO
        END DO
 
	DO J=JMIN,JMAX
	JJ=J-JMIN+1
	YY=FLOAT(J)
 
	DO I=IMIN,IMAX
	II=I-IMIN+1
	XX(II)=FLOAT(I)
	END DO
 
C Calling NAG routine E02CBF (to compute the value of the background for the
C current line)
	IFAIL=1
	NWORK=IDIM1
	MFIRST=1
	MLAST=IMAX-IMIN+1
 
	CALL E02CBF(MFIRST,MLAST,KKX,KKY,XX,XMIN0,XMAX0,
     1	YY,YMIN0,YMAX0,ZZ,CHEBY,NCHEBY,WORK,NWORK,IFAIL)
 
	IF(IFAIL.NE.0)THEN
	PRINT 30,IFAIL
30	FORMAT(' FLATTEN_1/Error in E02CBF (Computing back. value) IFAIL=',I4)
C If IFAIL=2: Y is not in [YMIN,YMAX[ intervall...
	  IF(IFAIL.EQ.2)THEN
             PRINT *,'IFAIL=2:   YMIN,YY,YMAX',YMIN0,YY,YMAX0
          ENDIF
        ELSE
C        PRINT 32,J
32      FORMAT('Line #',I4,' of the grid: successful fit')
	ENDIF
 
	IF(IOPT.EQ.1)THEN
	  DO I=IMIN,IMAX
	   II=I-IMIN+1
	   IF(INPUT(I,J).NE.0.)OUTPUT(I,J)=INPUT(I,J)-ZZ(II)
	  END DO
	ELSE
	  DO I=IMIN,IMAX
	    II=I-IMIN+1
	    IF(ZZ(II).EQ.0.)THEN
	     PRINT *,' WARNING ZZ(II)=0. !!'
	     OUTPUT(I,J)=0.
	    ELSE
	     OUTPUT(I,J)=INPUT(I,J)/ZZ(II)
	    ENDIF
	  END DO
	ENDIF
 
	END DO
 
	RETURN
	END
C******************************************************************
C Subroutine to generate the arrays XX YY and ZZ for the fit with E02CAF
C from a grid
C XX,YY : Coordinates of the centre
C ZZ : Mean intensity of the cell
C-------------------------------------------------------------------
	SUBROUTINE PREPARE_E02CAF(GRID,NXCELL,NYCELL,IDIM_GRID,IXSIZE,
     1	IYSIZE,XX,YY,ZZ,NPTX,NPTY,NPOINT,KKX)
	PARAMETER (IDIM1=6000)
	REAL*4 GRID(IDIM_GRID,*)
	REAL*8 XX(IDIM1),YY(IDIM1),ZZ(IDIM1)
	INTEGER*4 NPOINT(IDIM1)
	
 
	HALFXSIZE=FLOAT(IXSIZE)/2.
	HALFYSIZE=FLOAT(IYSIZE)/2.
 
	NPTX=0
	NPTY=0
 
	DO KY=1,NYCELL
 
C NPTY : "Index" for the current "line" of cells.
	NPTY=NPTY+1
	NPOINT(NPTY)=0
	JMIN=(KY-1)*IYSIZE +1
	JMAX=KY*IYSIZE
	YY(NPTY)=FLOAT(JMIN)+HALFYSIZE
 
	DO KX=1,NXCELL
	
	IMIN=(KX-1)*IXSIZE +1
	IMAX=KX*IXSIZE
 
C Therefore discarding bad values (0. and -1.) :
	IF(GRID(KX,KY).NE.0..AND.GRID(KX,KY).NE.-1.)THEN
	NPTX=NPTX+1
	NPOINT(NPTY)=NPOINT(NPTY)+1
	XX(NPTX)=FLOAT(IMIN)+HALFXSIZE
	ZZ(NPTX)=GRID(KX,KY)
	ENDIF
 
	END DO
 
C If less than KKX+1 points on the current line, we forget the current line
C for the fit. (For NAG routine)
	IF(NPOINT(NPTY).LT.KKX+1)THEN
	NPTX=NPTX-NPOINT(NPTY)
	NPTY=NPTY-1
	ENDIF
 
	END DO
 
	RETURN
	END
C-------------------------------------------------------------
C Subroutine to flatten the input image with option 1
C--------------------------------------------------------------
	SUBROUTINE FILL_3(GRID,NXCELL,NYCELL,IDIM_GRID,IXSIZE,IYSIZE)
	PARAMETER (IDIM2=200)
	PARAMETER (IDIM1=6000)
	REAL*4 GRID(IDIM_GRID,*)
	REAL*8 CHEBY(IDIM2),W(IDIM1),WORK(IDIM1)
	REAL*8 XX(IDIM1),ZZ(IDIM1),YY
        REAL*8 XMIN0,XMAX0,YMIN0,YMAX0
        INTEGER*4 KKX1,KKY1
 
	COMMON/SURFIT/CHEBY,KKX,KKY,XMIN0,XMAX0,YMIN0,YMAX0
 
C Main loop on the lines :
	DO J=1,NYCELL
	YY=IYSIZE*FLOAT(J)
 
	DO I=1,NXCELL
 
C When GRID(I,J)=-1 it is sustituted by the polynomial approximation :
	   IF(GRID(I,J).EQ.-1)THEN
	   XX(1)=IXSIZE*FLOAT(I)
 
C Calling NAG routine E02CBF (to compute the value of the background for the
C current point)
	   IFAIL=1
	   NWORK=IDIM1
	   MFIRST=1
	   MLAST=1
	   NCHEBY=IDIM2
 
	   CALL E02CBF(MFIRST,MLAST,KKX,KKY,XX,XMIN0,XMAX0,
     1	YY,YMIN0,YMAX0,ZZ,CHEBY,NCHEBY,WORK,NWORK,IFAIL)
 
	   IF(IFAIL.NE.0)THEN
	   PRINT 30,IFAIL
30	   FORMAT(' FILL_3/Error in E02CBF (computing back. value) IFAIL=',I4)
	   PRINT 31,XX(1),YY
31	   FORMAT(' Pb at point : X,Y :',F7.1,2X,F7.1)
	   ZZ(1)=0.
	   ENDIF
 
C Substitution :
	   GRID(I,J)=ZZ(1)
 
	   ENDIF
 
	END DO
	END DO
 
	RETURN
	END
C-------------------------------------------------------------
C Subroutine to fit 2-D cubic splines :
C Input array : GRID(NXCELL,NYCELL)
C
C--------------------------------------------------------------
	SUBROUTINE SURFACEFIT_SPLINE(GRID,NXCELL,NYCELL,
     1	IDIM_GRID,IXSIZE,IYSIZE)
	PARAMETER (IDIM3=2000,IDIM2=200)
	PARAMETER (IDIM1=6000)
	REAL*4 GRID(IDIM_GRID,*)
C	REAL*8 XX(IDIM1),YY(IDIM1),ZZ(IDIM1)
C	REAL*8 CHEBY(IDIM2),WEIGHT(IDIM1),LAMDA(IDIM3),MU(IDIM3)
C	REAL*8 WORK(IDIM1),DL(IDIM1),C(IDIM1)
C	REAL*8 EPS
	REAL*4 XX(IDIM1),YY(IDIM1),ZZ(IDIM1)
	REAL*4 CHEBY(IDIM2),WEIGHT(IDIM1),LAMDA(IDIM3),MU(IDIM3)
	REAL*4 WORK(IDIM1),DL(IDIM1),C(IDIM1)
	INTEGER*4 POINT(IDIM1),IADDRES(IDIM1)
        INTEGER PX, PY
 
C Loading the arrays XX and YY :
	MM=0
	DO J=1,NYCELL
	YWORK1=IYSIZE*FLOAT(J)
	  DO I=1,NXCELL
	  MM=MM+1
	  XX(MM)=IXSIZE*FLOAT(I)
	  YY(MM)=YWORK1
	  ZZ(MM)=GRID(I,J)
	  END DO
	END DO
 
C Sets the weights to 1.
	DO KK=1,MM
	WEIGHT(KK)=1.D0
	END DO
 
C Defines the interior knots in each line (starting at k=5)
	INCREX=4
34	PX=0
	DO I=2,NXCELL-1,INCREX
	PX=PX+1
	LAMDA(PX+4)=FLOAT(IXSIZE*I)
	END DO
	  IF(PX.LT.3)THEN
	  INCREX=MAX(INCREX-1,0)
	  PRINT *,' PX =', PX,' WARNING : NOT ENOUGH KNOTS IN X (!)'
	  GO TO 34
	  ENDIF
C Sets PX to PX+8 which is imposed by NAG E02ZAF and E02DAF :
	PX=PX+8
 
C Defines the interior knots in each column (starting at k=5)
	INCREY=4
35	PY=0
	DO J=2,NYCELL-1,INCREY
	PY=PY+1
	MU(PY+4)=FLOAT(IYSIZE*J)
	END DO
	  IF(PY.LT.3)THEN
	  INCREY=MAX(INCREY-1,0)
	  PRINT *,' PY =', PY,' WARNING : NOT ENOUGH KNOTS IN Y (!)'
	  GO TO 35
	  ENDIF
C Sets PY to PY+8 which is imposed by NAG E02ZAF and E02DAF :
	PY=PY+8
 
	PRINT *,' MM,PX,PY : ',MM,PX,PY
 
C Sorting the arrays to improve the efficiency of the algorithm
C for E02DAF (NAG): (Generating the array POINT of the indices)
C	IFAIL=1
 
	NADDRES=(PX-7)*(PY-7)
	NPOINT=MM+NADDRES
	PRINT *,' PX,PY,MM,XX(10),YY(10)',PX,PY,MM,XX(10),YY(10)
	PRINT *,' NPOINT,NADDRES',NPOINT,NADDRES
C	DO I=1,PX
CC	PRINT *,' LAMDA, I',LAMDA(I),I
C	END DO
C	DO I=1,PY
CC	PRINT *,' MU, I',MU(I),I
C	END DO
 
C Sorting the arrays with E02ZAF (NAG) 
	CALL E02ZAF(PX,PY,LAMDA,MU,MM,XX,YY,
     1	POINT,NPOINT,IADDRES,NADDRES,IFAIL)
C	CALL NEW_E02ZAF(PX,PY,LAMDA,MU,MM,XX,YY,
C     1	POINT,NPOINT,IADDRES,NADDRES,IFAIL)
	  IF(IFAIL.NE.0)THEN
	  PRINT 30,IFAIL
30	  FORMAT(' SURFACEFIT_SPLINE/Error in E02ZAF, IFAIL=',I4)
	  ENDIF
	PRINT *,' POINT(1),POINT(MM+1)',POINT(1),POINT(MM+1)
	PRINT *,' POINT(2),POINT(MM+2)',POINT(2),POINT(MM+2)
	PRINT *,' POINT(3),POINT(MM+3)',POINT(3),POINT(MM+3)
 
C Calling NAG routine E02DAF to fit 2-D cubic splines :
	IFAIL=1
	NWORK=IDIM1
C   Threshold to define 0 for the inversion of the matrix :
	EPS=1.D-08
C
	NC=(PX-4)*(PY-4)
 
	CALL E02DAF(MM,PX,PY,XX,YY,ZZ,WEIGHT,LAMDA,
     1	MU,POINT,NPOINT,DL,C,NC,WORK,NWORK,
     1	EPS,SIGMA,IRANK,IFAIL)
 
	IF(IFAIL.NE.0)THEN
	PRINT 31,IFAIL
31	FORMAT(' SURFACEFIT_SPLINE/Error in E02DAF   IFAIL=',I4)
	ENDIF
 
	PRINT 38,NC,IRANK,SIGMA
38	FORMAT(' Number of knots : ',I6,/,
     1	' Rank of the system :',I6,/,
     1	' Sigma for the fit :',E10.3)
 
	RETURN
	END
C---------------------------------------------------------------------------
	SUBROUTINE NEW_E02ZAF(PX,PY,LAMDA,MU,MM,XX,YY,
     1	POINT,NPOINT,IADDRES,NADDRES,IFAIL)
	REAL*8 XX(*),YY(*)
	REAL*8 MU(*),LAMDA(*)
	INTEGER*4 IADDRES(*),POINT(*)
        INTEGER*4 PPX,PPY,PX,PY,MM,NPOINT,NADDRES,IFAIL
 
	PPX=PX-8
	PPY=PY-8
 
C Present address for the panel IPANEL  (NADDRES=(PX-7)*(PX-7) because of
C the upper edges ...
	DO IPANEL=1,NADDRES
	IADDRES(IPANEL)=MM+IPANEL
	END DO
 
C--------------------------------------
C Loop on the points :
	DO 11 I=1,MM
 
C Classification in column :
	DO IX=1,PPX
	ILAM=IX+4
	WORKX=XX(I)-LAMDA(ILAM)
	  IF(WORKX.LT.0.)GO TO 12
	END DO
 
C Last column :
	IX=PPX+1
C???	GO TO 12
 
11	CONTINUE
 
	RETURN
C---------------------------------------------
C Classification in  row :
 
C Loop on MU :
12	DO 13 IY=1,PPY
	IMU=IY+4
	WORKY=YY(I)-MU(IMU)
	   IF(WORKY.LT.0.)THEN
	   IPANEL=(IX-1)*PPY+IY
	   POINT(IADDRES(IPANEL))=I
	   IADDRES(IPANEL)=I
           RETURN
	   ENDIF
13	CONTINUE
 
C Last row :
	IPANEL=(IX-1)*PPY+(PPY+1)
	POINT(IADDRES(IPANEL))=I
	IADDRES(IPANEL)=I
        RETURN
 
	END
C-----------------------------------------------------------------------
C Subroutine EXTEN_SPLINE1
C From AMTENB A.BIJAOUI 27 FEVRIER 1979
C      EXTENSION A L'AIDE DES B-SPLINES
C Version of April 6th 1987
C
C Input :
C GRID(NPL1,NL1)
C IFACT = Extension factor  (integer < 128 )
C
C Output :
C OUTPUT(NPL,NL)
C-----------------------------------------------------------------------
	SUBROUTINE EXTEN_SPLINE1(GRID,NPL1,NL1,IDIM_GRID,IFACT,OUTPUT,IDIM)
	PARAMETER(IDIM3=2000)
	REAL*4 GRID(IDIM_GRID,*),OUTPUT(IDIM,*)
 	REAL*4 AMES(4,IDIM3),PSI(128,4),DENS(IDIM3)
 
	NPL=NPL1*IFACT
	NL=NL1*IFACT
	PRINT *,' NPL,NL,IFACT',NPL,NL,IFACT
 
C Loading the array PSI for the extension
	DO K=1,IFACT
	  DO L=1,4
	  X=FLOAT(K-1)/FLOAT(IFACT)
	  PSI(K,L)=PHI(X,L)
	  END DO
	END DO	
 
C Reads line 1
	IY=1
 	CALL EXTEN(PSI,IFACT,GRID,IDIM_GRID,IY,NPL1,DENS,NPL)
 
C Loading AMES(1,*) and AMES(2,*):
 	DO IPL=1,NPL
 	AMES(1,IPL)=DENS(IPL)
	AMES(2,IPL)=DENS(IPL)
	END DO
 
C Reads line 2
	IY=2
 	CALL EXTEN(PSI,IFACT,GRID,IDIM_GRID,IY,NPL1,DENS,NPL)
 
C Loading AMES(3,*):
 	DO IPL=1,NPL
	AMES(3,IPL)=DENS(IPL)
	END DO
 
 	DO 25 IL=1,NL1-1
 	IL1=IL+2
 	IF(IL1.GT.NL1) GO TO 26
C Reads line IL1
	IY=IL1
 	CALL EXTEN(PSI,IFACT,GRID,IDIM_GRID,IY,NPL1,DENS,NPL)
 
C Loading AMES(4,*):
 	DO IPL=1,NPL
	AMES(4,IPL)=DENS(IPL)
	END DO
 
26	DO 28 K=1,IFACT
 	IL2=(IL-1)*IFACT+K
 
 	DO 29 IPL=1,NPL
 	S=0.
	  DO L=1,4
	  S=S+AMES(L,IPL)*PSI(K,L)
	  END DO
	OUTPUT(IPL,IL2)=S
29	CONTINUE
 
28	CONTINUE
 
C Rotation on the lines :
	   DO IPL=1,NPL
	   AMES(1,IPL)=AMES(2,IPL)
	   AMES(2,IPL)=AMES(3,IPL)
	   AMES(3,IPL)=AMES(4,IPL)
	   END DO
 
25	CONTINUE
 
	RETURN
 	END
C-----------------------------------------------------------------------
C Function PHI
C-----------------------------------------------------------------------
	REAL FUNCTION PHI(X,L)
 	GO TO (1,2,3,4),L
 1	PHI=((1.-X)**3)/6.
 	GO TO 5
 2	PHI=0.5*(X**3)-(X**2)+2./3.
 	GO TO 5
 3	PHI=-0.5*(X**3)+0.5*(X**2)+0.5*X+1./6.
 	GO TO 5
 4	PHI=(X**3)/6.
 5	RETURN
 	END
C--------------------------------------------------------------------------
C Subroutine EXTEN : Extension along a line.
C Input :
C GRID(NPL1,NL1)
C IY : Line index
C
C Output :
C DENS(NPL)		!NPL=NPL1*IFACT
C-----------------------------------------------------------------------
	SUBROUTINE EXTEN(PSI,IFACT,GRID,IDIM_GRID,IY,NPL1,DENS,NPL)
	REAL*4 GRID(IDIM_GRID,*)
 	REAL*4 DENS(1),PSI(128,4)
 	INTEGER*4 IL(4)
 
 	DO 1 IP=1,NPL1-1
 	   DO L=1,4
 	   IL(L)=IP+L-2
 	   IF(IL(L).LT.1) IL(L)=1
 	   IF(IL(L).GT.NPL1) IL(L)=NPL1
	   END DO
 
	DO 3 K=1,IFACT
 	IPL=K+(IP-1)*IFACT
 	S=0.
 	   DO L=1,4
 	   S= S + PSI(K,L)*GRID(IL(L),IY)
	   END DO
	DENS(IPL)=S
3	CONTINUE
 
1	CONTINUE
 
 	RETURN
 	END
C----------------------------------------------------------------------
C Subroutine COMPRES_HISTO
C
C From "AMCFC.FOR" A.BIJAOUI 25 JUIN 1980
C Generates a compressed map of the background
C
C Input :
C IFACT = Compression factor
C NTERM = Number of points taken into account to fit
C 	  the histogram (at least 30)
C ZMIN,ZMAX = Lower and upper limits for the background determination
C
C Output :
C
C----------------------------------------------------------------------
	SUBROUTINE COMPRES_HISTO(INPUT,NPL,NL,IDIM,GRID,NXCELL,
     1	NYCELL,IDIM_GRID,IFACT,NTERM,ZMIN,ZMAX,ZNOISE)
	REAL*4 INPUT(IDIM,*),GRID(IDIM_GRID,*)
 	INTEGER*4 ISTO(1024)
 
 	NXCELL=(NPL-1)/IFACT+1
 	NYCELL=(NL-1)/IFACT+1
C Unit for one step in the histogram :
	WORK1=(ZMAX-ZMIN)/800.
	WORK2=ZNOISE/4.
	ZSCALE=AMAX1(WORK1,WORK2)
	PRINT *,' STEP FOR THE HISTOGRAM : ',ZSCALE
 
C Main loop on the lines :
 	DO 1 IBL=1,NYCELL
 	NL1=1+(IBL-1)*IFACT
 	NL2=MIN(IBL*IFACT,NL)
 
C Loop on the columns :
 	DO 100 IBP=1,NXCELL
 	NP1=1+(IBP-1)*IFACT
 	NP2=MIN(IBP*IFACT,NPL)
 
C Erasing the histogram :
 	 DO K=1,1024
	 ISTO(K)=0
	 END DO
 
C Loading the histogram (values from 10 to 800 in "Zcale" units)
 	DO 3 IL=NL1,NL2
 	DO 3 IPL=NP1,NP2
	WORK=(INPUT(IPL,IL)-ZMIN)/ZSCALE
 	K=NINT(WORK)+1
 	IF(K.LE.10.OR.K.GT.800) GO TO 3
 	ISTO(K)=ISTO(K)+1
 3	CONTINUE
 
C Looking for the maximum ISTOMAX and KMAX (Peak in the distribution)
 	KMAX=11
 	ISTOMAX=ISTO(11)
 
 	DO 4 K=12,800
 	IF(ISTO(K).LE.ISTOMAX) GO TO 4
 	KMAX=K
 	ISTOMAX=ISTO(K)
4	CONTINUE
 
C	PRINT *,' @ KMAX ,ISTOMAX :',KMAX,ISTOMAX
 
C Preparing the data for the fit of the histogram :
 	NPL1=KMAX-NTERM
 	IF(NPL1.LE.0) NPL1=1
 	NPL2=KMAX+NTERM
 	IF(NPL2.GT.800) NPL2=800
 	NPL3=NPL2-NPL1+1
 	S0=0.
 	S1=0.
 	S2=0.
 	S3=0.
 
C Shifting the values of (-NPL1) :
	 DO IPL=NPL1,NPL2
	 ISTO(IPL-NPL1+1)=ISTO(IPL)
	 END DO
 
C Computing the sum of the centred momentum : S0, S1, S2, S3
	DO 5 IPL=1,NPL3
 	  X=ISTO(IPL)
 	  PL=IPL
 	  S0=S0+X
 	  X=X*PL
 	  S1=S1+X
 	  X=X*PL
 	  S2=S2+X
 	  X=X*PL
	  S3=S3+X
5	CONTINUE
 
C Computing the value of the background (approximation with the shape of the
C peak)
	  IF(S0.EQ.0.)THEN
	   GRID(IBP,IBL)=0.
	  ELSE
	   AM=S1/S0
 	   SIG=S2/S0-AM**2
 	   CURT=S3/S0-3.*AM*S2/S0+2.*(AM**3)
 	   IF(CURT.LE.0.) CURT=0.
 	   AX=(CURT/2.)**0.33333333
 	   FC=AM-AX+FLOAT(NPL1)-2.
 	   GRID(IBP,IBL)=(FC*ZSCALE)+ZMIN
	  ENDIF
 
100	CONTINUE
 
1	CONTINUE
 
	RETURN
 	END
C------------------------------------------------------------------------
C Subroutine TRANSFER_KNOTS
C To transfer the values of a grid from the centre of the cells to the top
C of the cells (Necessary for AMTENB or EXTEN_SPLINE)
C From AMEXP1,	A. Bijaoui 26th Feb 1979
C
C Version 8th April 1987
C
C Input :
C GRIDIN(NPL,NL)
C NPL,NL
C
C Output :
C NPL=(NPL+1)
C NL=(NL+1)
C GRIDIN(NPL,NL)
C-------------------------------------------------------------------------
	SUBROUTINE TRANSFER_KNOTS(GRIDIN,NPL,NL,IDIM_GRID)
	PARAMETER (IDIM2=200)
	REAL*4 GRIDIN(IDIM_GRID,*),GRIDOUT(IDIM2,IDIM2)
	REAL*4 MES1(IDIM2),MES2(IDIM2),MES3(IDIM2),MES4(IDIM2)
 
 	NPL1=NPL+1
 	NL1=NL+1
 
 	A1=-1./16.
 	A2=9./16.
 	A3=A2
 	A4=A1
 
C Loop on the pixels of the first line
C (to fill the arrays MES1, MES2, MES3)
	DO 1 IPL=1,NPL1
C Boundaries :
	IPL0=IPL-2
 	IPL0=MAX(1,IPL0)
 	IPL1=IPL-1
 	IPL1=MAX(1,IPL1)
 	IPL2=IPL
 	IPL2=MIN(NPL,IPL2)
 	IPL3=IPL+1
 	IPL3=MIN(NPL,IPL3)
 
C Computing the top value from the centre values in the current line
 	X=A1*GRIDIN(IPL0,1)+A2*GRIDIN(IPL1,1)
     1	+A3*GRIDIN(IPL2,1)+A4*GRIDIN(IPL3,1)
 	MES1(IPL)=X
 	MES2(IPL)=MES1(IPL)
 	MES3(IPL)=MES2(IPL)
 1	CONTINUE
 
C Main loop on the lines :
 	DO 2 IL=1,NL1
 	IL1=IL+1
 	IF(IL1.GT.NL) GO TO 3
 
 	DO 4 IPL=1,NPL1
C Boundaries :
	IPL0=IPL-2
 	IPL0=MAX(1,IPL0)
 	IPL1=IPL-1
 	IPL1=MAX(1,IPL1)
 	IPL2=IPL
 	IPL2=MIN(NPL,IPL2)
 	IPL3=IPL+1
 	IPL3=MIN(NPL,IPL3)
 
C Computing the top value from the centre values in the current line
 	X=A1*GRIDIN(IPL0,IL1)+A2*GRIDIN(IPL1,IL1)
     1	+A3*GRIDIN(IPL2,IL1)+A4*GRIDIN(IPL3,IL1)
	MES4(IPL)=X
4	CONTINUE
 
C Computing output :
3	DO 5 IPL=1,NPL1
 	X=A1*MES1(IPL)+A2*MES2(IPL)+A3*MES3(IPL)+A4*MES4(IPL)
	GRIDOUT(IPL,IL)=X
5	CONTINUE
 
C Rotation on the lines :
 	DO 6 IPL=1,NPL1
 	MES1(IPL)=MES2(IPL)
 	MES2(IPL)=MES3(IPL)
 	MES3(IPL)=MES4(IPL)
6	CONTINUE
 
2	CONTINUE
 
C Transfer back to the input array :
 	NPL=NPL+1
 	NL=NL+1
 
	DO J=1,NL
	DO I=1,NPL
	GRIDIN(I,J)=GRIDOUT(I,J)
	END DO
	END DO
 
	RETURN
 	END
C******************************************************************
C Subtraction of the background
C******************************************************************
        SUBROUTINE SUBTRACT(INPUT,OUTPUT,NX1,NY1,IDIM)
        REAL*4 INPUT(IDIM,*),OUTPUT(IDIM,*)
        DO J=1,NY1
          DO I=1,NX1
           OUTPUT(I,J)=INPUT(I,J)-OUTPUT(I,J)
          END DO
        END DO
        RETURN
        END
 
