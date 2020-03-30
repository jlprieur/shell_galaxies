C++****************************************************************************
C Program "FLATT2M"
C To correct images from bad flat-fielding
C
C JLP
C Version of 20-06-90
C--***************************************************************************
	PROGRAM FLATT2M
	PARAMETER (IDIM=600)
	PARAMETER (NN=4)
	REAL*4 IMAGE1(IDIM,IDIM),IMAGE2(IDIM,IDIM),
     1	IMAGE3(IDIM,IDIM),IMAGE4(IDIM,IDIM)
	REAL*4 XMEAN9(NN),XMEAN8(NN)
	REAL*8 SUM,SUM9(NN),SUM8(NN)
	INTEGER*4 NBER8(NN),IXSTART8(NN),IXEND8(NN)
	INTEGER*4 NBER9(NN),IXSTART9(NN),IXEND9(NN)
	INTEGER*4 IYSTART9(NN),IYEND9(NN)
	CHARACTER ANS*1,NAME*40,COMMENTS*80
 
	COMMON/PARAM/NPL,NL
 
10	FORMAT(A)
	OPEN(1,FILE='flatt2m.log',STATUS='unknown')
	WRITE(1,*)' PROGRAM FLATT2M'
 
C Inquire the format :
	CALL JLP_INQUIFMT
 
88	PRINT 100
100	FORMAT(' MENU :',/,
     1	' 1. INPUT OF A MODEL OF FLAT FIELD',/,
     1	' 2. INPUT OF AN IMAGE TO PROCESS',/,
     1	' 3. INPUT OF A MODEL FOR THE SKY MODEL',/,
     1	' 4. SKY LEVEL OF AN AREA OF THE IMAGE AND THE SKY MODEL',/,
     1	' 5. DEFINITION OF 4 AREAS FOR FLAT FIELD SHIFT',/,
     1	' 6. INPUT OF SHIFTING VALUES AND CALCULATION',
     1	' OF THE MEAN IN THE 4 AREAS',/,
     1	' 7. CORRECTION OF AN IMAGE WITH A FLAT FIELD SHIFTED',/,
     1	' 8. CORRECTION OF AN IMAGE BY SUBTRACTING THE SKY MODEL',/,
     1	' 10. EXIT',/,
     1	' ENTER THE OPTION YOU WANT : ',$)
	READ(5,*) IOPT
 
C-----------------------------------------------------------------------
C OPTION 1
C**** Input of the BDF files *****************
	IF(IOPT.EQ.1)THEN
	PRINT *,' FLAT FIELD (NORMALIZED TO 1.) :'
        READ(5,10) NAME
	  CALL JLP_READIMAG(IMAGE1,NPL1,NL1,IDIM,NAME,COMMENTS)
	GO TO 88
	ENDIF
 
C-----------------------------------------------------------------------
C OPTION 2
	IF(IOPT.EQ.2)THEN
	PRINT *,' IMAGE YOU WANT TO PROCESS :'
        READ(5,10) NAME
	  CALL JLP_READIMAG(IMAGE2,NPL,NL,IDIM,NAME,COMMENTS)
	GO TO 88
	ENDIF
 
C----------------------------------------------------------------------
C OPTION 3
	IF(IOPT.EQ.3)THEN
	PRINT *,' MODEL FOR THE SKY :'
        READ(5,10) NAME
	  CALL JLP_READIMAG(IMAGE4,NPL,NL,IDIM,NAME,COMMENTS)
	GO TO 88
	ENDIF
 
C-------------------------------------------------------------------
C OPTION 4
C Calculates the sky level of the image and the model for the sky
	IF(IOPT.EQ.4)THEN
400	PRINT *,' AREA YOU WANT FOR THE SKY LEVEL :'
	PRINT *,' ENTER XSTART,XEND,YSTART,YEND (INTEGERS)'
	READ(5,*) IXSTART,IXEND,IYSTART,IYEND
	WRITE(1,*)IXSTART,IXEND,IYSTART,IYEND
 
	CALL SUM_AREA(IMAGE2,IXSTART,IXEND,IYSTART,IYEND,
     1	SUM,SKY2,NBER)
 
	PRINT 401
401	FORMAT(' IMAGE :')
	PRINT 402,NBER,SKY2
	WRITE(1,402)NBER,SKY2
402	FORMAT(' POINTS :',I10,' SKY LEVEL:',F15.4)
 
	CALL SUM_AREA(IMAGE4,IXSTART,IXEND,IYSTART,IYEND,
     1	SUM,SKY4,NBER)
 
	PRINT 403
403	FORMAT(' SKY MODEL :')
	PRINT 402,NBER,SKY4
	WRITE(1,402)NBER,SKY4
 
	GO TO 88
	ENDIF
 
C------------------------------------------------------------------------
C OPTION 5
C Areas for the fit :
	IF(IOPT.EQ.5)THEN
500	PRINT *,' AREAS YOU WANT TO CHECK FOR THE FIT :'
	DO I=1,NN
	PRINT *,' AREA NUMBER :',I
	PRINT *,' ENTER XSTART,XEND,YSTART,YEND (INTEGERS)'
	READ(5,*) IXSTART9(I),IXEND9(I),IYSTART9(I),IYEND9(I)
	WRITE(1,*)IXSTART9(I),IXEND9(I),IYSTART9(I),IYEND9(I)
	END DO
 
C Calculating the mean for each area in the image :
	PRINT 501
	WRITE(1,501)
501	FORMAT(' IMAGE :')
	DO I=1,NN
	CALL SUM_AREA(IMAGE2,IXSTART9(I),IXEND9(I),
     1	IYSTART9(I),IYEND9(I),SUM8(I),
     1	XMEAN8(I),NBER8(I))
	PRINT 502,I,NBER8(I),SUM8(I),XMEAN8(I)
	WRITE(1,502)I,NBER8(I),SUM8(I),XMEAN8(I)
502	FORMAT(/,' AREA NUMBER :',I3,/,' POINTS:',I10,
     1	' SUM:',E12.4,' MEAN:',E12.4)
	END DO
 
	GO TO 88
	ENDIF
 
C-----------------------------------------------------------------------
C OPTION 6
C Entering the shifting values
	IF(IOPT.EQ.6)THEN
600	PRINT *,' SHIFT IN X AND Y (REALS) :'
	READ(5,*) DDX,DDY
	WRITE(1,*)DDX,DDY
 
	PRINT *,' VALUE FOR THE SKY LEVEL ?'
	READ(5,*) SKY2
 
	PRINT 601
	WRITE(1,601)
601	FORMAT(' FLAT FIELD :')
	DO I=1,NN
	CALL DIVSHIFT_SUM(IMAGE1,IXSTART9(I),IXEND9(I),
     1	IYSTART9(I),IYEND9(I),
     1	DDX,DDY,SUM,XMEAN,NBER9(I))
	SUM9(I)=SUM*SKY2
	XMEAN9(I)=XMEAN*SKY2
 
	PRINT 602,I,NBER9(I),SUM9(I),XMEAN9(I)
	WRITE(1,602)I,NBER9(I),SUM9(I),XMEAN9(I)
602	FORMAT(/,' AREA NUMBER :',I3,/,' POINTS:',I10,
     1	' SUM:',E12.4,' MEAN:',E12.4)
 
	END DO
 
	GO TO 88
	ENDIF
	
C-------------------------------------------------------------------
C OPTION 7
C Correction of the image :
	IF(IOPT.EQ.7)THEN
 
700	PRINT *,' SHIFT IN X AND Y (REALS) :'
	READ(5,*) DDX,DDY
	WRITE(1,*)DDX,DDY
 
	PRINT *,' VALUE FOR THE SKY LEVEL ?'
	READ(5,*) SKY2
 
	CALL CORREC_IMAGE(IMAGE1,IMAGE2,IMAGE3,DDX,DDY,
     1	SKY2)
 
	PRINT *,' OUTPUT FILE:'
        READ(5,10) NAME
	  COMMENTS=' '
	  CALL JLP_WRITEIMAG(IMAGE3,NPL,NL,IDIM,NAME,COMMENTS)
 
	GO TO 88
	ENDIF
 
	
C-------------------------------------------------------------------
C OPTION 8
C Correction of the image, subtracting a model of sky:
	IF(IOPT.EQ.8)THEN
 
800	PRINT *,' ENTER THE SKY LEVELS FOR THE IMAGE,'
	PRINT *,' AND THE MODEL : SKY2, SKY4 ?'
	READ(5,*) SKY2,SKY4
	WRITE(1,*)SKY2,SKY4
	COEFF=SKY2/SKY4
 
	CALL SUB_SKY(IMAGE4,IMAGE2,IMAGE3,COEFF)
 
	PRINT *,' OUTPUT FILE:'
        READ(5,10) NAME
	  COMMENTS=' '
	  CALL JLP_WRITEIMAG(IMAGE3,NPL,NL,IDIM,NAME,COMMENTS)
 
	GO TO 88
	ENDIF
 
	CLOSE(1)
        PRINT *,' Output in "flatt2m.log"'
        STOP
	END
C-------------------------------------------------------------------
 
C*******************************************************************
C Subroutine SUM_AREA
C To get the mean value of  an area of an image
C*******************************************************************
	SUBROUTINE SUM_AREA(IMAGE1,IXSTART,IXEND,IYSTART,IYEND,
     1	SUM,XMEAN,NBER)
 
	PARAMETER (IDIM=600)
	REAL*4 IMAGE1(IDIM,IDIM)
	REAL*8 SUM
 
C Starting the main loop
	SUM=0.
	NBER=0
 
	DO 1 J=IYSTART+1,IYEND
	DO 2 I=IXSTART+1,IXEND
	NBER=NBER+1
	SUM=SUM+IMAGE1(I,J)
2	CONTINUE
1	CONTINUE
 
	IF(NBER.NE.0)THEN
	XMEAN=SUM/FLOAT(NBER)
	ELSE
	XMEAN=0.
	ENDIF
 
	RETURN
	END
 
C*******************************************************************
C Subroutine DIVSHIFT_SUM
C To get the mean value of the ratio (plu shifted)/plu
C*******************************************************************
	SUBROUTINE DIVSHIFT_SUM(IMAGE1,IXSTART,IXEND,IYSTART,IYEND,
     1	DDX,DDY,SUM,XMEAN,NBER)
 
	PARAMETER (IDIM=600)
	REAL*4 IMAGE1(IDIM,IDIM)
	REAL*8 SUM
	COMMON/PARAM/NPL,NL
 
C Getting the parameters for the shift :
	IDDX=IFIX(DDX)
	TDX=DDX-FLOAT(IDDX)
	IF(TDX.LT.0.)THEN
	TDX=TDX+1.
	IDDX=IDDX-1
	ENDIF
 
	IDDY=IFIX(DDY)
	TDY=DDY-FLOAT(IDDY)
	IF(TDY.LT.0.)THEN
	TDY=TDY+1.
	IDDY=IDDY-1
	ENDIF
 
	PRINT 400,IDDX,TDX,IDDY,TDY
400	FORMAT(' IDDX =',I4,' TDX=',F7.3,' IDDY=',I4,
     1	' TDY=',F7.3)
 
C Starting the main loop
	SUM=0.
	NBER=0
 
	DO 1 J=IYSTART+1,IYEND
	JD=J-IDDY
	IF(JD.LT.1.OR.JD.GT.NL) GO TO 1
 
	DO 2 I=IXSTART+1,IXEND
	ID=I-IDDX
	IF(ID.LT.1.OR.ID.GT.NPL) GO TO 2
	VALD=IMAGE1(ID,JD-1)*(1.-TDX)*TDY+IMAGE1(ID-1,JD-1)*TDX*TDY
     1	+IMAGE1(ID,JD)*(1.-TDX)*(1.-TDY)
     1	+IMAGE1(ID-1,JD)*TDX*(1.-TDY)
	VALC=IMAGE1(I,J)
	
	IF(VALC.NE.0.)THEN
	NBER=NBER+1
	SUM=SUM+VALD/VALC
	ENDIF
 
2	CONTINUE
1	CONTINUE
 
	IF(NBER.NE.0)THEN
	XMEAN=SUM/FLOAT(NBER)
	ELSE
	XMEAN=0.
	ENDIF
 
	RETURN
	END
 
C*******************************************************************
C Subroutine CORREC_IMAGE
C IMAGE1 : flat field model
C IMAGE2 : image to process
C IMAGE3 : output image
C COEFF : sky level of the input image
C*******************************************************************
	SUBROUTINE CORREC_IMAGE(IMAGE1,IMAGE2,IMAGE3,DDX,DDY,
     1	COEFF)
 
	PARAMETER (IDIM=600)
	REAL*4 IMAGE1(IDIM,IDIM),IMAGE2(IDIM,IDIM),
     1	IMAGE3(IDIM,IDIM)
	COMMON/PARAM/NPL,NL
 
C Getting the parameters for the shift :
	IDDX=IFIX(DDX)
	TDX=DDX-FLOAT(IDDX)
	IF(TDX.LT.0.)THEN
	TDX=TDX+1.
	IDDX=IDDX-1
	ENDIF
 
	IDDY=IFIX(DDY)
	TDY=DDY-FLOAT(IDDY)
	IF(TDY.LT.0.)THEN
	TDY=TDY+1.
	IDDY=IDDY-1
	ENDIF
 
	PRINT 400,IDDX,TDX,IDDY,TDY
400	FORMAT(/,' IDDX =',I4,' TDX=',F7.3,' IDDY=',I4,
     1	' TDY=',F7.3)
 
C Starting the main loop
 
	DO 1 J=2,NL
	JD=J-IDDY
	IF(JD.LT.1.OR.JD.GT.NL) GO TO 1
 
	DO 2 I=2,NPL
	ID=I-IDDX
	IF(ID.LT.1.OR.ID.GT.NPL) GO TO 2
	VALD=IMAGE1(ID,JD-1)*(1.-TDX)*TDY+IMAGE1(ID-1,JD-1)*TDX*TDY
     1	+IMAGE1(ID,JD)*(1.-TDX)*(1.-TDY)
     1	+IMAGE1(ID-1,JD)*TDX*(1.-TDY)
	VALC=IMAGE1(I,J)
	
	IF(VALC.NE.0.)THEN
	IMAGE3(I,J)=IMAGE2(I,J)-COEFF*VALD/VALC
	ELSE
	IMAGE3(I,J)=IMAGE2(I,J)
	ENDIF
 
2	CONTINUE
1	CONTINUE
 
	RETURN
	END
 
C*******************************************************************
C Subroutine SUB_SKY
C IMAGE4 : sky model
C IMAGE2 : image to process
C IMAGE3 : output image
C*******************************************************************
	SUBROUTINE SUB_SKY(IMAGE4,IMAGE2,IMAGE3,COEFF)
 
	PARAMETER (IDIM=600)
	REAL*4 IMAGE4(IDIM,IDIM),IMAGE2(IDIM,IDIM),
     1	IMAGE3(IDIM,IDIM)
	COMMON/PARAM/NPL,NL
 
	DO J=1,NL
	DO I=1,NPL
	IMAGE3(I,J)=IMAGE2(I,J)-COEFF*IMAGE4(I,J)
	END DO
	END DO
 
	RETURN
	END
 
