C++***********************************************************************
C Program RADIAL_TANGENTIAL
C Gradient of an image in the radial direction,
C or smoothing filter in the tangential direction
C
C SYNTAX:
C     RUNS RADIAL_TANGENTIAL in_image xcenter,ycenter option
C         [Y/N: manual entry of the sky/noise?] [[sky,noise]] out_image
C
C Examples:
C     RUNS RADIAL_TANGENTIAL test 128.3,245.6 3 N test_grad2
C     RUNS RADIAL_TANGENTIAL test 128.3,245.6 1 Y 234.3,12.3 test_smooth
C
C Nota1: Options availables are:
C        1 = Smooth the image tangentially
C        2 = Radial gradient map
C        3 = Radial gradient map of the logarithm
C
C JLP
C Version of 19-07-90
C--***********************************************************************
	PROGRAM RADIAL_TANGENTIAL
	PARAMETER (IDIM=600)
	REAL*4 INPUT(IDIM,IDIM),OUTPUT(IDIM,IDIM)
	CHARACTER NAME*40,COMMENTS*80
10	FORMAT(A)
 
	CALL JLP_BEGIN
	PRINT 5
5	FORMAT(' Program "RADIAL_TANGENTIAL"  Version of 29-12-87',/,
     1	' Gradient in the radial direction',/,
     1	' or smoothing filter in the tangential direction',/)
 
C Inquires about the format of the files:
	CALL JLP_INQUIFMT
 
C Read the input image:
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
	CALL JLP_READIMAG(INPUT,NX,NY,IDIM,NAME,COMMENTS)
	LNAME=MAX(INDEX(NAME,'  '),5)	
 
	PRINT *,' ENTER THE POSITION OF THE CENTRE : X0, Y0 (PIXELS, REAL)'
	READ(5,*) X0,Y0
 
C Options:
	PRINT 21
21	FORMAT(' TWO OPTIONS :',/,
     1	' 1. SMOOTH THE IMAGE TANGENTIALLY',/,
     1	' 2. RADIAL GRADIENT MAP',/,
     1	' 3. RADIAL GRADIENT MAP OF THE LOGARITHM',/,
     1	' ENTER YOUR CHOICE : ')
	READ(5,*) ICHOICE
 
C Smooths the image :
	IF(ICHOICE.EQ.1)THEN
	  CALL SMT_TANGENT1(INPUT,OUTPUT,NX,NY,IDIM,X0,Y0)
	  WRITE(COMMENTS,31) NAME(1:LNAME)
31	  FORMAT(X,A,' TANGENTIALLY SMOOTHED ')
 
	ELSEIF(ICHOICE.EQ.2)THEN
C Computes a radial gradient map:
 	  CALL SMT_GRADIENT1(INPUT,OUTPUT,NX,NY,IDIM,X0,Y0)
	  WRITE(COMMENTS,41) NAME(1:LNAME)
41	  FORMAT(' RADIAL GRADIENT MAP OF ',A)
 
	ELSE
C Entering the level of the sky and the noise of the image :
	  PRINT *,' DO YOU KNOW THE SKY LEVEL AND',
     1	' THE NOISE OF THE IMAGE (Y)?'
	  READ(5,10) ANS
	  IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN
	    PRINT *,' ENTER THE SKY LEVEL AND THE NOISE :'
	    READ(5,*) SKY,SIGMA
	  ELSE
C Else computing it directly from the image :
	    CALL AUTO_SKY(INPUT,NX,NY,IDIM,SKY,SIGMA)
	    PRINT *,' SKY, SIGMA :',SKY,SIGMA
	  ENDIF
 
C Getting the logarithm of the input image :
	  CALL SMT_LOGARITHM(INPUT,NX,NY,IDIM,SKY,SIGMA)
 
C Then computes a radial gradient map:
 	  CALL SMT_GRADIENT1(INPUT,OUTPUT,NX,NY,IDIM,X0,Y0)
	  LNAME=MIN(LNAME,20)
	  WRITE(COMMENTS,51) NAME(1:LNAME)
51	  FORMAT(' RADIAL GRADIENT MAP OF THE LOGARITHM OF ',A)
	ENDIF
 
C Output the result :
        WRITE(6,*) 'Output file: '
        READ(5,10) NAME
	CALL JLP_WRITEIMAG(OUTPUT,NX,NY,IDIM,NAME,COMMENTS)
 
	CALL JLP_END
	STOP
	END
C***********************************************************************
C Radial gradient with a smoothing effect tangentially
C
C Defines 24 directions in the image :
C Orientation :
C           10    9    8    7    6    5    4
C           11   1,1  2,1  3,1  4,1  5,1   3
C           12   1,2  2,2  3,2  4,2  5,2   2
C            1   1,3  2,3  3,3  4,3  5,3   1
C            2   1,4  2,4  3,4  4,4  5,4   12
C            3   1,5  2,5  3,5  5,5  5,5   11
C            4    5    6    7    8    9    10
C***********************************************************************
	SUBROUTINE SMT_GRADIENT1(IN,OUT,NX,NY,IDIM,X0,Y0)
	REAL*4 IN(IDIM,IDIM),OUT(IDIM,IDIM)
	REAL*4 C(5)
	DATA C/0.40,0.60,0.00,-0.60,-0.40/
 
C Main loop :
	DO J=3,NY-2
	Y1=FLOAT(J)-Y0
	  DO I=3,NX-2
	   X1=FLOAT(I)-X0
 
C Determining which sector the pixel belongs to:
	    CALL ATAND1(X1,Y1,ANG)
	    ISECT=INT((ANG+97.5)/15)-5
	    IF(ISECT.LT.1)ISECT=ISECT+12
	    IF(ISECT.GT.12)ISECT=ISECT-12
 
C ISECT=1
	    IF(ISECT.EQ.1)THEN
	     OUT(I,J)=C(1)*(IN(I-2,J-1)+2.*IN(I-2,J)+IN(I-2,J+1))
     1	+C(2)*(IN(I-1,J-1)+2.*IN(I-1,J)+IN(I-1,J+1))
     1	+C(4)*(IN(I+1,J-1)+2.*IN(I+1,J)+IN(I+1,J+1))
     1	+C(5)*(IN(I+2,J-1)+2.*IN(I+2,J)+IN(I+2,J+1))
C ISECT=2
	    ELSEIF(ISECT.EQ.2)THEN
	     OUT(I,J)=C(1)*(IN(I-2,J-2)+2.*IN(I-2,J-1)+IN(I-2,J))
     1	+C(2)*(IN(I-1,J-1)+2.*IN(I-1,J)+IN(I-1,J+1))
     1	+C(4)*(IN(I+1,J-1)+2.*IN(I+1,J)+IN(I+1,J+1))
     1	+C(5)*(IN(I+2,J)+2.*IN(I+2,J+1)+IN(I+2,J+2))
C ISECT=3
	    ELSEIF(ISECT.EQ.3)THEN
	     OUT(I,J)=C(1)*(IN(I-2,J-2)+2.*IN(I-2,J-1)+IN(I-2,J))
     1	+C(2)*(IN(I-1,J-2)+2.*IN(I-1,J-1)+IN(I-1,J))
     1	+C(4)*(IN(I+1,J)+2.*IN(I+1,J+1)+IN(I+1,J+2))
     1	+C(5)*(IN(I+2,J)+2.*IN(I+2,J+1)+IN(I+2,J+2))
C ISECT=4
	    ELSEIF(ISECT.EQ.4)THEN
	     OUT(I,J)=C(1)*(IN(I-2,J-1)+2.*IN(I-2,J-2)+IN(I-1,J-2))
     1	+C(2)*(IN(I-2,J)+2.*IN(I-1,J-1)+IN(I,J-2))
     1	+C(4)*(IN(I,J+2)+2.*IN(I+1,J+1)+IN(I+2,J))
     1	+C(5)*(IN(I+1,J+2)+2.*IN(I+2,J+2)+IN(I+2,J+1))
C ISECT=5
	    ELSEIF(ISECT.EQ.5)THEN
	     OUT(I,J)=C(1)*(IN(I-2,J-2)+2.*IN(I-1,J-2)+IN(I,J-2))
     1	+C(2)*(IN(I-2,J-1)+2.*IN(I-1,J-1)+IN(I,J-1))
     1	+C(4)*(IN(I,J+1)+2.*IN(I+1,J+1)+IN(I+2,J+1))
     1	+C(5)*(IN(I,J+2)+2.*IN(I+1,J+2)+IN(I+2,J+2))
C ISECT=6
	    ELSEIF(ISECT.EQ.6)THEN
	     OUT(I,J)=C(1)*(IN(I-2,J-2)+2.*IN(I-1,J-2)+IN(I,J-2))
     1	+C(2)*(IN(I-1,J-1)+2.*IN(I,J-1)+IN(I+1,J-1))
     1	+C(4)*(IN(I-1,J+1)+2.*IN(I,J+1)+IN(I+1,J+1))
     1	+C(5)*(IN(I,J+2)+2.*IN(I+1,J+2)+IN(I+2,J+2))
C ISECT=7
	    ELSEIF(ISECT.EQ.7)THEN
	     OUT(I,J)=C(1)*(IN(I-1,J-2)+2.*IN(I,J-2)+IN(I+1,J-2))
     1	+C(2)*(IN(I-1,J-1)+2.*IN(I,J-1)+IN(I+1,J-1))
     1	+C(4)*(IN(I-1,J+1)+2.*IN(I,J+1)+IN(I+1,J+1))
     1	+C(5)*(IN(I-1,J+2)+2.*IN(I,J+2)+IN(I+1,J+2))
C ISECT=8
	    ELSEIF(ISECT.EQ.8)THEN
	     OUT(I,J)=C(1)*(IN(I,J-2)+2.*IN(I+1,J-2)+IN(I+2,J-2))
     1	+C(2)*(IN(I-1,J-1)+2.*IN(I,J-1)+IN(I+1,J-1))
     1	+C(4)*(IN(I-1,J+1)+2.*IN(I,J+1)+IN(I+1,J+1))
     1	+C(5)*(IN(I-2,J+2)+2.*IN(I-1,J+2)+IN(I,J+2))
C ISECT=9
	    ELSEIF(ISECT.EQ.9)THEN
	     OUT(I,J)=C(1)*(IN(I,J-2)+2.*IN(I+1,J-2)+IN(I+2,J-2))
     1	+C(2)*(IN(I,J-1)+2.*IN(I+1,J-1)+IN(I+2,J-1))
     1	+C(4)*(IN(I-2,J+1)+2.*IN(I-1,J+1)+IN(I,J+1))
     1	+C(5)*(IN(I-2,J+2)+2.*IN(I-1,J+2)+IN(I,J+2))
C ISECT=10
	    ELSEIF(ISECT.EQ.10)THEN
	     OUT(I,J)=C(1)*(IN(I+1,J-2)+2.*IN(I+2,J-2)+IN(I+2,J-1))
     1	+C(2)*(IN(I,J-2)+2.*IN(I+1,J-1)+IN(I+2,J))
     1	+C(4)*(IN(I-2,J)+2.*IN(I-1,J+1)+IN(I,J+2))
     1	+C(5)*(IN(I-1,J+2)+2.*IN(I-2,J+2)+IN(I-2,J+1))
C ISECT=11
	    ELSEIF(ISECT.EQ.11)THEN
	     OUT(I,J)=C(1)*(IN(I+2,J-2)+2.*IN(I+2,J-1)+IN(I+2,J))
     1	+C(2)*(IN(I+1,J-2)+2.*IN(I+1,J-1)+IN(I+1,J))
     1	+C(4)*(IN(I-1,J)+2.*IN(I-1,J+1)+IN(I-1,J+2))
     1	+C(5)*(IN(I-2,J)+2.*IN(I-2,J+1)+IN(I-2,J+2))
C ISECT=12
	    ELSEIF(ISECT.EQ.12)THEN
	     OUT(I,J)=C(1)*(IN(I+2,J-2)+2.*IN(I+2,J-1)+IN(I+2,J))
     1	+C(2)*(IN(I+1,J-1)+2.*IN(I+1,J)+IN(I+1,J+1))
     1	+C(4)*(IN(I-1,J-1)+2.*IN(I-1,J)+IN(I-1,J+1))
     1	+C(5)*(IN(I-2,J)+2.*IN(I-2,J+1)+IN(I-2,J+2))
	    ENDIF
 
C To obtain a symmetric image relative to the centre :
	   IF(Y1.LT.0.AND.ISECT.NE.1)OUT(I,J)=-1.*OUT(I,J)
	   IF(X1.LT.0.AND.ISECT.EQ.1)OUT(I,J)=-1.*OUT(I,J)
 
C Relative gradient :
	   XMEAN=(IN(I-2,J-2)+IN(I-2,J-1)+IN(I-2,J)+IN(I-2,J+1)
     1	+IN(I-2,J+2)+IN(I-1,J-2)+IN(I-1,J-1)+IN(I-1,J)
     1	+IN(I-1,J+1)+IN(I-1,J+2)+IN(I,J-2)+IN(I,J-1)
     1	+IN(I,J)+IN(I,J+1)+IN(I,J+2)+IN(I+1,J-2)
     1	+IN(I+1,J-1)+IN(I+1,J)+IN(I+1,J+1)+IN(I+1,J+2)
     1	+IN(I+2,J-2)+IN(I+2,J-1)+IN(I+2,J)+IN(I+2,J+1)
     1	+IN(I+2,J+2))/25.
	   OUT(I,J)=OUT(I,J)/XMEAN
 
	 END DO
	END DO
 
	RETURN
	END
C***********************************************************************
C Radial gradient without any smoothing effect tangentially
C
C Defines 24 directions in the image :
C Orientation :
C           10    9    8    7    6    5    4
C           11   1,1  2,1  3,1  4,1  5,1   3
C           12   1,2  2,2  3,2  4,2  5,2   2
C            1   1,3  2,3  3,3  4,3  5,3   1
C            2   1,4  2,4  3,4  4,4  5,4   12
C            3   1,5  2,5  3,5  5,5  5,5   11
C            4    5    6    7    8    9    10
C***********************************************************************
	SUBROUTINE SMT_GRADIENT2(IN,OUT,NX,NY,IDIM,X0,Y0)
	REAL*4 IN(IDIM,IDIM),OUT(IDIM,IDIM)
	REAL*4 C(5)
	DATA C/0.40,0.60,0.00,-0.60,-0.40/
 
C Main loop :
	DO J=3,NY-2
	Y1=FLOAT(J)-Y0
	  DO I=3,NX-2
	   X1=FLOAT(I)-X0
 
C Determining which sector the pixel belongs to:
	    CALL ATAND1(X1,Y1,ANG)
	    ISECT=INT((ANG+97.5)/15)-5
	    IF(ISECT.LT.1)ISECT=ISECT+12
	    IF(ISECT.GT.12)ISECT=ISECT-12
 
C ISECT=1
	    IF(ISECT.EQ.1)THEN
	     OUT(I,J)=C(1)*IN(I-2,J)+C(2)*IN(I-1,J)
     1	+C(4)*IN(I+1,J)+C(5)*IN(I+2,J)
C ISECT=2
	    ELSEIF(ISECT.EQ.2)THEN
	     OUT(I,J)=C(1)*IN(I-2,J-1)+C(2)*IN(I-1,J)
     1	+C(4)*IN(I+1,J)+C(5)*IN(I+2,J+1)
C ISECT=3
	    ELSEIF(ISECT.EQ.3)THEN
	     OUT(I,J)=C(1)*IN(I-2,J-1)+C(2)*IN(I-1,J-1)
     1	+C(4)*IN(I+1,J+1)+C(5)*IN(I+2,J+1)
C ISECT=4
	    ELSEIF(ISECT.EQ.4)THEN
	     OUT(I,J)=C(1)*IN(I-2,J-2)+C(2)*IN(I-1,J-1)
     1	+C(4)*IN(I+1,J+1)+C(5)*IN(I+2,J+2)
C ISECT=5
	    ELSEIF(ISECT.EQ.5)THEN
	     OUT(I,J)=C(1)*IN(I-1,J-2)+C(2)*IN(I-1,J-1)
     1	+C(4)*IN(I+1,J+1)+C(5)*IN(I+1,J+2)
C ISECT=6
	    ELSEIF(ISECT.EQ.6)THEN
	     OUT(I,J)=C(1)*IN(I-1,J-2)+C(2)*IN(I,J-1)
     1	+C(4)*IN(I,J+1)+C(5)*IN(I+1,J+2)
C ISECT=7
	    ELSEIF(ISECT.EQ.7)THEN
	     OUT(I,J)=C(1)*IN(I,J-2)+C(2)*IN(I,J-1)
     1	+C(4)*IN(I,J+1)+C(5)*IN(I,J+2)
C ISECT=8
	    ELSEIF(ISECT.EQ.8)THEN
	     OUT(I,J)=C(1)*IN(I+1,J-2)+C(2)*IN(I,J-1)
     1	+C(4)*IN(I,J+1)+C(5)*IN(I-1,J+2)
C ISECT=9
	    ELSEIF(ISECT.EQ.9)THEN
	     OUT(I,J)=C(1)*IN(I+1,J-2)+C(2)*IN(I+1,J-1)
     1	+C(4)*IN(I-1,J+1)+C(5)*IN(I-1,J+2)
C ISECT=10
	    ELSEIF(ISECT.EQ.10)THEN
	     OUT(I,J)=C(1)*IN(I+2,J-2)+C(2)*IN(I+1,J-1)
     1	+C(4)*IN(I-1,J+1)+C(5)*IN(I-2,J+2)
C ISECT=11
	    ELSEIF(ISECT.EQ.11)THEN
	     OUT(I,J)=C(1)*IN(I+2,J-1)+C(2)*IN(I+1,J-1)
     1	+C(4)*IN(I-1,J+1)+C(5)*IN(I-2,J+1)
C ISECT=12
	    ELSEIF(ISECT.EQ.12)THEN
	     OUT(I,J)=C(1)*IN(I+2,J-1)+C(2)*IN(I+1,J)
     1	+C(4)*IN(I-1,J)+C(5)*IN(I-2,J+1)
	    ENDIF
 
C To obtain a symmetric image relative to the centre :
	   IF(Y1.LT.0.AND.ISECT.NE.1)OUT(I,J)=-1.*OUT(I,J)
	   IF(X1.LT.0.AND.ISECT.EQ.1)OUT(I,J)=-1.*OUT(I,J)
 
C Relative gradient :
	   XMEAN=(IN(I-1,J-2)+IN(I-1,J-1)+IN(I-1,J)+IN(I-1,J+1)
     1	+IN(I-1,J+2)+IN(I,J-2)+IN(I,J-1)+IN(I,J)+IN(I,J+1)
     1	+IN(I,J+2)+IN(I+1,J-2)+IN(I+1,J-1)+IN(I+1,J)
     1	+IN(I+1,J+1)+IN(I+1,J+2))/16.
	   OUT(I,J)=OUT(I,J)/XMEAN
 
	 END DO
	END DO
 
	RETURN
	END
 
C***********************************************************************
C Smooths tangentially.
C Defines 24 directions in the image :
C Orientation :
C            4    3    2    1   12   11   10
C            5   1,1  2,1  3,1  4,1  5,1   9
C            6   1,2  2,2  3,2  4,2  5,2   8
C            7   1,3  2,3  3,3  4,3  5,3   7
C            8   1,4  2,4  3,4  4,4  5,4   6
C            9   1,5  2,5  3,5  5,5  5,5   5
C            10   11   12   1    2    3    4
C***********************************************************************
	SUBROUTINE SMT_TANGENT1(IN,OUT,NX,NY,IDIM,X0,Y0)
	REAL*4 IN(IDIM,IDIM),OUT(IDIM,IDIM)
	REAL*4 C(5)
	DATA C/0.05,0.10,0.70,0.10,0.05/
 
C Main loop :
	DO J=3,NY-2
	Y1=FLOAT(J)-Y0
	  DO I=3,NX-2
	   X1=FLOAT(I)-X0
	    CALL ATAND1(X1,Y1,ANG)
	    ISECT=1+INT((ANG+97.5)/15)
	    IF(ISECT.EQ.13)ISECT=1
C ISECT=1
	    IF(ISECT.EQ.1)THEN
	     OUT(I,J)=(C(1)*IN(I-2,J)+C(2)*IN(I-1,J)+C(3)*IN(I,J)
     1	+C(4)*IN(I+1,J)+C(5)*IN(I+2,J))/2.
C ISECT=2
	    ELSEIF(ISECT.EQ.2)THEN
	     OUT(I,J)=(C(1)*IN(I-2,J-1)+C(2)*IN(I-1,J)+C(3)*IN(I,J)
     1	+C(4)*IN(I+1,J)+C(5)*IN(I+2,J+1))/2.
C ISECT=3
	    ELSEIF(ISECT.EQ.3)THEN
	     OUT(I,J)=(C(1)*IN(I-2,J-1)+C(2)*IN(I-1,J-1)+C(3)*IN(I,J)
     1	+C(4)*IN(I+1,J+1)+C(5)*IN(I+2,J+1))/2.
C ISECT=4
	    ELSEIF(ISECT.EQ.4)THEN
	     OUT(I,J)=(C(1)*IN(I-2,J-2)+C(2)*IN(I-1,J-1)+C(3)*IN(I,J)
     1	+C(4)*IN(I+1,J+1)+C(5)*IN(I+2,J+2))/2.
C ISECT=5
	    ELSEIF(ISECT.EQ.5)THEN
	     OUT(I,J)=(C(1)*IN(I-1,J-2)+C(2)*IN(I-1,J-1)+C(3)*IN(I,J)
     1	+C(4)*IN(I+1,J+1)+C(5)*IN(I+1,J+2))/2.
C ISECT=6
	    ELSEIF(ISECT.EQ.6)THEN
	     OUT(I,J)=(C(1)*IN(I-1,J-2)+C(2)*IN(I,J-1)+C(3)*IN(I,J)
     1	+C(4)*IN(I,J+1)+C(5)*IN(I+1,J+2))/2.
C ISECT=7
	    ELSEIF(ISECT.EQ.7)THEN
	     OUT(I,J)=(C(1)*IN(I,J-2)+C(2)*IN(I,J-1)+C(3)*IN(I,J)
     1	+C(4)*IN(I,J+1)+C(5)*IN(I,J+2))/2.
C ISECT=8
	    ELSEIF(ISECT.EQ.8)THEN
	     OUT(I,J)=(C(1)*IN(I+1,J-2)+C(2)*IN(I,J-1)+C(3)*IN(I,J)
     1	+C(4)*IN(I,J+1)+C(5)*IN(I-1,J+2))/2.
C ISECT=9
	    ELSEIF(ISECT.EQ.9)THEN
	     OUT(I,J)=(C(1)*IN(I+1,J-2)+C(2)*IN(I+1,J-1)+C(3)*IN(I,J)
     1	+C(4)*IN(I-1,J+1)+C(5)*IN(I-1,J+2))/2.
C ISECT=10
	    ELSEIF(ISECT.EQ.10)THEN
	     OUT(I,J)=(C(1)*IN(I+2,J-2)+C(2)*IN(I+1,J-1)+C(3)*IN(I,J)
     1	+C(4)*IN(I-1,J+1)+C(5)*IN(I-2,J+2))/2.
C ISECT=11
	    ELSEIF(ISECT.EQ.11)THEN
	     OUT(I,J)=(C(1)*IN(I+2,J-1)+C(2)*IN(I+1,J-1)+C(3)*IN(I,J)
     1	+C(4)*IN(I-1,J+1)+C(5)*IN(I-2,J+1))/2.
C ISECT=12
	    ELSEIF(ISECT.EQ.12)THEN
	     OUT(I,J)=(C(1)*IN(I+2,J-1)+C(2)*IN(I+1,J)+C(3)*IN(I,J)
     1	+C(4)*IN(I-1,J)+C(5)*IN(I-2,J+1))/2.
	    ENDIF
	 END DO
	END DO
 
	RETURN
	END
 
C***********************************************************************
C	SUBROUTINE ATAND1
C To work out the position angle of the current pixel
C
C Input :
C X, Y : Coordinates relative to the centre of the galaxy
C
C Output:
C ANG in degrees between -90. and +90)
C***********************************************************************
	SUBROUTINE ATAND1(X,Y,ANG)
	REAL*4 X,Y,ANG
 
	IF(X.EQ.0.) X=0.00001
	    TANG=Y/X
	    ANG=ATAND(TANG)
	RETURN
	END
C------------------------------------------------------------------------
C Get the logarithm of the input image
C
C------------------------------------------------------------------------
	SUBROUTINE SMT_LOGARITHM(IMAGE,NX,NY,IDIM,SKY,SIGMA)
	REAL*4 IMAGE(IDIM,IDIM)
 
	BACKGROUND=SKY-3.*SIGMA
 
	DO IY=1,NY
	  DO IX=1,NX
	   WORK=AMAX1(IMAGE(IX,IY)-BACKGROUND,0.0001)
	   IMAGE(IX,IY)=10.*ALOG10(WORK)
	  END DO
	END DO
 
	RETURN
	END
C***********************************************************************
	include 'jlpsub:auto_sky.for'
