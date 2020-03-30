C++------------------------------------------------------------------
C GRADIENT1
C Program to compute the gradient map of an image
C From QUICKGRAD2 (Bill Sparks)
C
C Syntax:
C   RUNS GRADIENT1 in_image out_image [Y/N: manual entry of sky/noise ?]
C               [[sky_value,noise]]
C Examples:
C   RUNS GRADIENT1 test test_grad1 N
C   RUNS GRADIENT1 test test_grad1 Y 1230.4,23.4
C
C JLP
C Version of 25-06-94
C--------------------------------------------------------------------
	PROGRAM GRADIENT1
	INTEGER*4 PNTR_IN,PNTR_OUT,PNTR_WORK
	INTEGER*4 MADRID(1)
	CHARACTER NAMEIN*40,NAMEOUT*40,COMMENTS*80,ANS*1
	COMMON /VMR/MADRID
10	FORMAT(A)
 
C To allow for direct input of parameters in the command line:
	CALL JLP_BEGIN
 
	WRITE(6,81)
81	FORMAT(' Program GRADIENT1   Version 25-06-94',/,
     1	' To compute a log-gradient map')
 
C Inquire about the format of the files :
	CALL JLP_INQUIFMT
 
C Read the input image :
	PRINT *,' Input image ?'
	READ(5,10) NAMEIN
	CALL JLP_VM_READIMAG(PNTR_IN,NX,NY,NAMEIN,COMMENTS)
 
	PRINT *,' Output image ?'
	READ(5,10) NAMEOUT
	CALL JLP_GETVM(PNTR_OUT,NX*NY*4)
	CALL JLP_GETVM(PNTR_WORK,NX*NY*4)
 
C Entering the level of the sky and the noise of the image :
	PRINT *,' Do you know the sky level and',
     1	' the noise of the image (N)?'
	READ(5,10) ANS
	  IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
	    PRINT *,' Enter the sky level and the noise :'
	    READ(5,*) SKY,SIGMA
	  ELSE
C Else computing it directly from the image :
	    CALL AUTO_SKY(MADRID(PNTR_IN),NX,NY,NX,SKY,SIGMA)
	    PRINT *,' SKY, SIGMA :',SKY,SIGMA
	  ENDIF
 
C Compute the gradient map :
	CALL GRADIENT1_SUB(MADRID(PNTR_IN),MADRID(PNTR_OUT),
     1	MADRID(PNTR_WORK),NX,NY,NX,SKY,SIGMA)
 
C Write the output in a file :
	IN=INDEX(NAMEIN,'  ')-1
	WRITE(COMMENTS,35)NAMEIN(1:IN)
35	FORMAT(' Gradient map of: ',A,' //')
	CALL JLP_WRITEIMAG(MADRID(PNTR_OUT),NX,NY,NX,NAMEOUT,COMMENTS)
 
	CALL JLP_END
	STOP
	END
C------------------------------------------------------------------
C Subroutine GRADIENT1_SUB which computes the gradient map
C
C------------------------------------------------------------------
	SUBROUTINE GRADIENT1_SUB(IM,GRADIENT,WORK,NX,NY,IDIM,
     1	BACK,RMS)
	REAL*4 IM(IDIM,*),GRADIENT(IDIM,*),WORK(IDIM,*),C1(5)
	CHARACTER CNUM*1
c
c Filter row - assumed antisymmetric
c
	DATA C1/0.25,0.50,1,0.50,0.25/
c
c Subroutine to generate a gradient map by convolution with a kernel
c and quadratic addition of the 2 components.
c Output is log of image with offset Out=Kernel*(Log(Image-Offset))
c
	OFFSET=BACK-3.0*RMS
 
C Taking the logarithm of the whole image :
	BACKLOG=ALOG10(BACK-OFFSET)
	DO J=1,NY
	 DO I=1,NX
	 WORK1=IM(I,J)-OFFSET
	   IF(WORK1.GT.0)THEN
 	    WORK(I,J)=ALOG10(WORK1)
	   ELSE
c Set values below offset to back
	    WORK(I,J)=BACKLOG
	   ENDIF
	 END DO
	END DO
 
C Now scanning the image :
 
	do j=3,NY-2
 
	  do i=3,NX-2
 
	     SUM1=0.
	     DO I1=I-2,I+2
	      K=I1-I+3
	      SUM1=C1(K)*(WORK(I1,J-1)-WORK(I1,J+1)
     1	+0.50*(WORK(I1,J-2)-WORK(I1,J+2)))+SUM1
	     END DO
 
	     SUM2=0.
	     DO J1=J-2,J+2
	      K=J1-J+3
	      SUM2=C1(K)*(WORK(I-1,J1)-WORK(I+1,J1)
     1	+0.50*(WORK(I-2,J1)-WORK(I+2,J1)))+SUM2
	     END DO
	
	      GRADIENT(I,J)=SQRT(SUM1*SUM1+SUM2*SUM2)
20	end do
	end do
c
80	return
	end
C***********************************************************************
	include 'jlpsub:auto_sky.for'
