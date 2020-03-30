C++--------------------------------------------------------------------
C CIRCPHOT_SET
C To get the photometry of stars in circular apertures
C and visualize it (splot library) 
C
C Contains:
C  CIRCPHOT
C  
C JLP  Version of 06-08-2013
C--------------------------------------------------------------------
	SUBROUTINE CIRCPHOT(ARRAY,NX,NY,IDIM,IMAGE2,
     1     NX2,NY2,IDIM2,NAMEIN,NCOLORS,
     1     ITT_IS_LINEAR,LOWER_ITT,UPPER_ITT,GAMMA_D,GAMMA1,IDV1)
	REAL*4 ARRAY(IDIM,*)
	REAL*8 SUM_STAR,SUMSQ_SKY,SUM_SKY
	REAL*4 NSKY,NSTAR,DIAM1,DIAM2,TOTAL_INTENSITY,MEAN_STAR
        REAL LOWER_ITT,UPPER_ITT
        INTEGER*4 DIST2,DISTX2,DISTY2,RAD12,RAD22,DRAW_CROSS
	INTEGER*4 IMAGE2(IDIM2,*)
	INTEGER NX,NY,NX2,NY2,NCOLORS,NCIRC,GAMMA_D,GAMMA1
        INTEGER IXP,IYP,IN_FRAME,IRAD1,IRAD2,IOPTION,IDV1,ITT_IS_LINEAR
        LOGICAL NO_CURSOR
	CHARACTER NAMEIN*40,NAMEOUT*40,STAR_NAME*14
	CHARACTER ANS*1
 
10	FORMAT(A)
 
        NO_CURSOR=.FALSE.
 
C New options (JLP99):
        WRITE(6,108)
108     FORMAT(' Input of the diameters:',/,
     1         ' 1. Fixed values for all measurements',/,
     1         ' 2. Manual input for each measurement',/,
     1         ' 3. Interactive choice for each measurement',/,
     1         '10. Return to the main menu')
        READ(5,*) IOPTION 
        IF(IOPTION.GT.3.OR.IOPTION.LT.1)RETURN

        IF(IOPTION.EQ.1.OR.IOPTION.EQ.2)THEN
          WRITE(6,109)
109       FORMAT(' Interactive determination of the position? [y]?')
          READ(5,10) ANS 
          IF(ANS.EQ.'n'.OR.ANS.EQ.'N')NO_CURSOR=.TRUE.
        ENDIF

15	PRINT *,' Output file with the results ?'
	READ(5,10) NAMEOUT
	OPEN(2,FILE=NAMEOUT,STATUS='UNKNOWN',ERR=15)
	WRITE(2,106)NAMEIN
106	FORMAT(' INPUT IMAGE :',A)
	WRITE(2,107)
107	FORMAT('Name',T10,'Intensity',T22,
     1  ' Error',T33,'Xcen',T38,'Ycen',T42,
     1  'Star diam',T51,'Sky diam',T59,' Sky int.',T70,'Sigma',2X,'Mean star')

C The diameters :
        IF(IOPTION.EQ.1)THEN
 	   PRINT *,' Enter the diameters in pixels for the star',
     1   ' and the sky : (same value=no sky subtraction)'
	   READ(5,*) DIAM1,DIAM2
        ENDIF
 
        DRAW_CROSS=0
C Main loop
        DO 201 I=1,1000
         IF(IOPTION.NE.3)THEN
           IF(NO_CURSOR)THEN
             PRINT *,' X, Y position: (-1,-1 to exit)'
             READ(5,*)XP,YP
           ELSE
C Reading the position of the cursor:
             CALL JLP_WHERE(XP,YP,IN_FRAME,IBUTTON,DRAW_CROSS,IDV1)
           ENDIF
         ELSE
C Entering the centre :
            WRITE(6,88)
88          FORMAT(' Enter the center and the two circles with the cursor :',
     1   '(click outside of the image to exit)')
C JLP96:
           NCIRC = 2
           CALL JLP_GET_2CIRCLES(XP,YP,DIAM1,DIAM2,NCIRC,IDV1)
         ENDIF

C JLP_GET_2CIRCLES is written in C and returns an index starting at 0,0:
C (JLP99: JLP_WHERE also)
        IXP=INT(XP)+1
        IYP=INT(YP)+1

C Check if the point is in the image :
        IF(IXP.LT.1.OR.IXP.GT.NX
     1   .OR.IYP.LT.1.OR.IYP.GT.NY)THEN
          PRINT 77,IXP,IYP
77        FORMAT(' CIRCPHOT/Exit: Point outside of the image X, Y:',2I5)
          GO TO 99 
	  ENDIF

C Then the diameters :
        IF(IOPTION.EQ.2)THEN
 	   PRINT *,' ENTER THE DIAMETERS IN PIXELS FOR THE STAR',
     1   ' AND THE SKY :'
	   READ(5,*) DIAM1,DIAM2
        ENDIF

C In case of error, JLP_GET_2CIRCLES returns negative diameters:
C Please check this ONLY AFTER having checked IXP,IYP !
        IF(DIAM1.LE.0..AND.DIAM2.LE.0.) GOTO 201
        PRINT 71,IXP,IYP,DIAM1,DIAM2
71      FORMAT(' IX_center,IY_center,DIAM1,DIAM2',2(I5,1X),2(G12.5,1X))
 
C Displaying the circles: 
	XP=FLOAT(IXP)
	YP=FLOAT(IYP)
        CALL STR_CIRCLE(XP,YP,DIAM1,IDV1)
        CALL STR_CIRCLE(XP,YP,DIAM2,IDV1)
	CALL JLP_GFLUSH(IDV1)
 
C Computing the intensity in 3 iterations to estimate the sky with
c a 2-sigma rejection :
	SIGMA=1.E+12
	SKY=0.
 
        IRAD1=NINT(DIAM1/2.)
        IRAD2=NINT(DIAM2/2.)
        RAD12=IRAD1*IRAD1
        RAD22=IRAD2*IRAD2

        DO 3 IK=1,3
 
          SUM_SKY=0.D0
          SUMSQ_SKY=0.D0
	  SUM_STAR=0.D0
	  NSKY=0.
	  NSTAR=0.
 
          DO 2 IY=IYP-IRAD1,IYP+IRAD1
	    DISTY2=(IY-IYP)*(IY-IYP)
            IF(IY.GE.1.AND.IY.LE.NY)THEN 
            DO 1 IX=IXP-IRAD1,IXP+IRAD1
	       DISTX2=(IX-IXP)*(IX-IXP)
	       DIST2=DISTX2+DISTY2
               IF(IX.GE.1.AND.IX.LE.NX)THEN 
                IF(DIST2.LE.RAD12)THEN
	          NSTAR=NSTAR+1.
	          SUM_STAR=SUM_STAR+ARRAY(IX,IY)
                ELSEIF(DIST2.LE.RAD22)THEN
	          TEST=ABS((ARRAY(IX,IY)-SKY)/2.)
                  IF(TEST.LT.SIGMA)THEN
	              NSKY=NSKY+1.
	              SUM_SKY=SUM_SKY+ARRAY(IX,IY)
	              SUMSQ_SKY=SUMSQ_SKY+ARRAY(IX,IY)*ARRAY(IX,IY)
                   ENDIF
                ENDIF
               ENDIF
1             CONTINUE
            ENDIF
2       CONTINUE

        IF(NSKY.EQ.0.)THEN
          PRINT *,' CIRCPHOT/Error: no points for sky determination'
	  SKY=0.
	  SIGMA=0.
        ELSE
	  SKY=SUM_SKY/NSKY
	  SIGMA=SQRT(SUMSQ_SKY/NSKY-SKY*SKY)
        ENDIF
	
3       CONTINUE
 
        IF(NSTAR.EQ.0.)THEN
          PRINT *,' CIRCPHOT/Error: no points for star determination'
        ELSE
          PRINT *,' NSTAR, NSKY',NSTAR,NSKY
        ENDIF
 
C Computing the results :
	TOTAL_INTENSITY=(SUM_STAR-SKY*NSTAR)
        MEAN_STAR=SUM_STAR/NSTAR
	SIGMA_STAR=SIGMA*NSTAR
	SIGMA_SKY=SIGMA
 
	PRINT *,' Name of the star ?'
	READ(5,10) STAR_NAME

	PRINT 107
C JLP99: Pb here with tabulation ... 
	WRITE(6,1101)STAR_NAME,TOTAL_INTENSITY,SIGMA_STAR,
     1	IXP,IYP,DIAM1,DIAM2,SKY,SIGMA_SKY,MEAN_STAR
1101	FORMAT(A,1X,G11.4,1X,G10.3,1X,I4,1X,I4,1X,F7.2,1X,
     1	F7.2,1X,G10.3,1X,G9.2,1X,G9.2)
	WRITE(2,101) STAR_NAME,TOTAL_INTENSITY,SIGMA_STAR,
     1	IXP,IYP,DIAM1,DIAM2,SKY,SIGMA_SKY,MEAN_STAR
101	FORMAT(A,T10,G11.4,T22,G10.3,T33,I4,T38,I4,T43,F7.2,T51,
     1	F7.2,T59,G10.3,T70,G9.2,1X,G9.2)
 
C End of main loop
201     CONTINUE
 
99	CLOSE(2)
        RETURN
	END
