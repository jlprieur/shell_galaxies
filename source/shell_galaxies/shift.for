C++****************************************************************************
C Program SHIFT
C To shift an image of a some fractions of pixels
C and create of a larger image.
C JLP Version of 06-12-90
C--***************************************************************************
	PROGRAM SHIFT
	INTEGER*4 MADRID(1),PNTR_IN1,PNTR_OUT
	CHARACTER FILENAME*40,COMMENTS*80
	COMMON/VMR/MADRID
 
	PRINT 60
60	FORMAT(' Program to shift an image of some fractions'
     1	' of pixels',/,' and create a larger image',
     1	' (Dynamical allocation of memory)')
 
10	FORMAT(A)
 
	CALL JLP_BEGIN
	CALL JLP_INQUIFMT
 
C**** Input of the image *****************
        WRITE(6,*) 'Input file: '
        READ(5,10) FILENAME
	CALL JLP_VM_READIMAG(PNTR_IN1,NX1,NY1,FILENAME,COMMENTS)
 
	PRINT *,' OUTPUT = (INPUT - SKY ) * CTE'
	PRINT *,' Enter SKY value and constant CTE' 
	READ(5,*) SKY1,CTE1
	PRINT *,' SHIFT IN X AND Y :'
	READ(5,*) DDX1,DDY1
	PRINT *,' SIZE OF THE OUTPUT IMAGE (NX, NY)  (0,0 if same size)?'
	READ(5,*) NX2,NY2
        IF(NX2.EQ.0.OR.NY2.EQ.0)THEN
          NX2=NX1
          NY2=NY1
        ENDIF
 
C Allocating dynamical memory space :
	MEMS_OUT=4*NX2*NY2
	CALL JLP_GETVM(PNTR_OUT,MEMS_OUT)
 
	CALL SHIFT1(MADRID(PNTR_IN1),NX1,NY1,MADRID(PNTR_OUT),NX2,NY2,
     1	DDX1,DDY1,SKY1,CTE1)
 
C Output of the image :
        WRITE(6,*) 'Output file: (image 1 shifted)'
        READ(5,10) FILENAME
	COMMENTS=' '
	CALL JLP_WRITEIMAG(MADRID(PNTR_OUT),NX2,NY2,NX2,
     1	FILENAME,COMMENTS)
 
	CALL JLP_END
	STOP
	END
 
C--------------------------------------------------------------------
C Subroutine SHIFT1 to shift IMAGE1 with a shift of DDX and DDY
C The output is stored in IMAGE2 of a large size NPL2, NL2.
C-------------------------------------------------------------------
	SUBROUTINE SHIFT1(IMAGE1,NPL1,NL1,IMAGE2,NPL2,NL2,
     1	DDX,DDY,SKY,CTE)
	REAL*4 IMAGE1(NPL1,NL1),IMAGE2(NPL2,NL2)
 
C--------------------------------------------------------------------
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
 
C-------------------------------------------------------------------------
C Main loop
 
	DO 1 J=1,NL2
	JD=J-IDDY
 
	IF(JD.LT.2.OR.JD.GT.NL1)THEN
	DO I=1,NPL2
	IMAGE2(I,J)=0.
	END DO
 
	ELSE
 
	DO I=1,NPL2
	ID=I-IDDX
	  IF(ID.LT.2.OR.ID.GT.NPL1) THEN
	  IMAGE2(I,J)=0.
	  ELSE
 	  WORK=IMAGE1(ID,JD-1)*(1.-TDX)*TDY
     1	+IMAGE1(ID-1,JD-1)*TDX*TDY
     1	+IMAGE1(ID,JD)*(1.-TDX)*(1.-TDY)
     1	+IMAGE1(ID-1,JD)*TDX*(1.-TDY)
	  IMAGE2(I,J)=(WORK-SKY)*CTE
	  ENDIF
	END DO
 
	ENDIF
 
1	CONTINUE
 
	RETURN
	END
