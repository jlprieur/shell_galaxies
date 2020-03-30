C++***************************************************************
C Subroutine to compute some statistical parameters
C of an image, to be used as scaling values for further display
C
C JLP
C Version of 18-03-90
C--***************************************************************
	SUBROUTINE JLP_AUTO_SCALE(IMAGE,NX,NY,IDIM,MIN1,MAX1,
	1	MEAN1,MEAN2,SIGMA1,SIGMA2,IXMI,IYMI,IXMA,IYMA)
	INTEGER*4 NX,NY,IDIM
	INTEGER*4 IXMI,IYMI,IXMA,IYMA
	REAL*4 IMAGE(IDIM,*)
	REAL*4 MEAN1,SIGMA1,MEAN2,SIGMA2,MIN1,MAX1
	REAL*4 XLOW,XHIGH,XNN
	REAL*8 SUM,SUMSQ
 
C debug:
C        PRINT *,' image(1,1) =',image(1,1)

C First iteration:
	MIN1=IMAGE(1,1)
	MAX1=IMAGE(1,1)
	IXMI=1
	IYMI=1
	IXMA=1
	IYMA=1
	SUM=0.	
	SUMSQ=0.
	
	DO IY=1,NY
	  DO IX=1,NX
	    WORK=IMAGE(IX,IY)
	    IF(WORK.LT.MIN1)THEN
	      IXMI=IX
	      IYMI=IY
	      MIN1=WORK
	    ENDIF
	    IF(WORK.GT.MAX1)THEN
	      IXMA=IX
	      IYMA=IY
	      MAX1=WORK
	    ENDIF
	    SUM=SUM+WORK
	    SUMSQ=SUMSQ+WORK*WORK
	  END DO
	END DO
	MEAN1=SUM/(NX*NY)
	SIGMA1=SQRT(SUMSQ/(NX*NY)-MEAN1**2)
	
C Second iteration:
	SUM=0.	
	SUMSQ=0.
	XLOW=MEAN1-3*SIGMA1
	XHIGH=MEAN1+3*SIGMA1
	XNN=0.
 
	DO IY=1,NY
	  DO IX=1,NX
	    WORK=IMAGE(IX,IY)
	    IF((WORK.LE.XHIGH).AND.(WORK.GE.XLOW))THEN
	      SUM=SUM+WORK
	      SUMSQ=SUMSQ+WORK*WORK
	      XNN=XNN+1.
	    ENDIF
	  END DO
	END DO
 
	IF(XNN.NE.0)THEN
	  MEAN2=SUM/XNN
	  SIGMA2=SQRT(SUMSQ/XNN-MEAN2**2)
	ELSE
	  PRINT *,' No points between Mean1 +/- 3*Sigma1 !!!!'
	  MEAN2=MEAN1
	  SIGMA2=SIGMA1
	ENDIF
 
	RETURN
	END
