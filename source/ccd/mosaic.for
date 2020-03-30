C++****************************************************************************
C Programm "MOSAIC"
C Version of January 6th 1987
C--***************************************************************************
	PROGRAM MOSAIC
	INTEGER*4 MADRID(1),PNTR_IN1,PNTR_IN2,PNTR_OUT
	CHARACTER NAME*40,COMMENTS*80
	COMMON /VMR/MADRID
 
10	FORMAT(A)
 
	PRINT 60
60	FORMAT(' Programm MOSAIC to get a mosaic from different',
     1	' CCD images',/,
     1	' (First shift the images with "SHIFT")')
 
	CALL JLP_INQUIFMT
 
C**** Input of the images *****************
	PRINT *,' IMAGE 1'
	READ(5,10) NAME
	CALL JLP_VM_READIMAG(PNTR_IN1,NX1,NY1,NAME,COMMENTS)
	PRINT *,' IMAGE 2'
	READ(5,10) NAME
	CALL JLP_READIMAG(PNTR_IN2,NX2,NY2,NAME,COMMENTS)
	NX=MIN0(NX1,NX2)
	NY=MIN0(NY1,NY2)
 
	PRINT *,' Sky values for images 1 and 2 :'
	READ(5,*) SKY1,SKY2
 
	PRINT *,' Ratio of the levels : image 1 over 2 :'
	READ(5,*) C12
 
C  Get dynamical memory space:
	ISIZE=NX*NY*4
	CALL JLP_GETVM(PNTR_OUT,ISIZE)
 
C Main part:
	CALL MOSAIC1(MADRID(PNTR_IN1),NX1,MADRID(PNTR_IN2),NX2,
     1	MADRID(PNTR_OUT),NX,NY,SKY1,SKY2,C12)
 
	PRINT *,' OUTPUT FILE:'
	READ(5,10) NAME
	CALL JLP_WRITEIMAG(MADRID(PNTR_OUT),NX,NY,NX,NAME,COMMENTS)
 
	END
C-----------------------------------------------------------------------
C Main loop :
C-----------------------------------------------------------------------
	SUBROUTINE MOSAIC1(IMAGE1,NX1,IMAGE2,NX2,IMAGE3,NX,NY,
     1	SKY1,SKY2,C12)
	REAL*4 IMAGE1(NX1,*),IMAGE2(NX2,*),IMAGE3(NX,*)
	REAL*4 SKY1,SKY2,C12
 
	DO J=1,NY
 
	 DO I=1,NX
 
	   IMAGE3(I,J)=IMAGE1(I,J)+IMAGE2(I,J)
 
C---- Adding the normalized portion of the missing image in the edges :
 
	   IF(IMAGE1(I,J).NE.0..OR.IMAGE2(I,J).NE.0.)THEN
 
	     IF(IMAGE1(I,J).EQ.0.)THEN
	      IMAGE3(I,J)=IMAGE3(I,J)+(IMAGE2(I,J)-SKY2)*C12+SKY1
	     ENDIF
 
	     IF(IMAGE2(I,J).EQ.0.)THEN
	      IMAGE3(I,J)=IMAGE3(I,J)+(IMAGE1(I,J)-SKY1)/C12+SKY2
	     ENDIF
	   ENDIF
	
	  END DO
 
	END DO
 
	RETURN
	END
