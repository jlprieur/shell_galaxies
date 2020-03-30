C++*******************************************************************
C
C MEDIAN1
C
C To compute the median filtered version of the input image
C Uses NAG routine M01ANF
C
C Input parameters:
C  INPUT: input image
C  OUTPUT: output image
C  IBOX: Size of the box for which the median is comuted,
C        This should be an odd, positive integer greater than 3
C
C Syntax :   MEDIAN1 IN_FMT,OUT_FMT INPUT OUTPUT IBOX
C Example:   MEDIAN1 5,5 TEST TEST_MEDIAN 5
C
C JLP
C  Version of 25/03/90
C--**************************************************************************
	PROGRAM MEDIAN1
	INTEGER*4 PNTR_IN,PNTR_OUT
	CHARACTER NAMEIN*40,NAMEOUT*40,COMMENTS*80
	INTEGER*4 MADRID(1)
	COMMON /VMR/MADRID
 
10	FORMAT(A)
 
C Possibility of direct input of the parameters in the command line:
	CALL JLP_BEGIN
 
C Inquire the format of the files :
	CALL JLP_INQUIFMT
 
C Read the input image :
	NAMEIN=' '
	CALL JLP_VM_READIMAG(PNTR_IN,NX,NY,NAMEIN,COMMENTS)
 
C Obtain a name for the output :
	PRINT *,' OUTPUT IMAGE ?'
	READ(5,10) NAMEOUT
 
C Get dynamical memory for the output:
	I=NX*NY*4
	CALL JLP_GETVM(PNTR_OUT,I)
C
C NOW PICK UP THE BOX SIZE
C
	PRINT *,' Box size? (odd number > 3)'
	READ(5,*) IBOX
	IBOX=2*(IBOX/2)+1
	IF(IBOX.LT.3) THEN
	 PRINT *,' Box too small, I take Box size = 3'
	 IBOX=3
	ENDIF
	NB=NX/IBOX
	MB=NY/IBOX
	IF (NB.LE.3.OR.MB.LE.3) THEN
	 PRINT *,' FATAL ERROR : Box too large'
	 GOTO 999
	ENDIF
C
C HAVING GOT THE PARAMETERS , DO THE WORK
C
	CALL MEDIAN(MADRID(PNTR_IN),MADRID(PNTR_OUT),NX,NY,NX,IBOX)
 
C Ouput the results :
	LEN=INDEX(NAMEIN,'  ')-1
	WRITE(COMMENTS,11) NAMEIN(1:LEN),IBOX
11	FORMAT(' Median of: ',A,' Box:',I5)
	CALL JLP_WRITEIMAG(MADRID(PNTR_OUT),NX,NY,NX,NAMEOUT,COMMENTS)
 
999	CALL JLP_END
	STOP
	END
C**************************************************************************
      SUBROUTINE MEDIAN(IN,OUT,N,M,IDIM,IBOX)
C
C      IN IS THE INPUT ARRAY ; OUT THE OUTPUT ARRAY BOTH OF SIZE N BY M
C
C      THE ROUTINE APPLIES A MEDIAN FILTER TO THE INPUT ARRAY , YIELDING
C      THE OUTPUT ARRAY.
C      IBOX DEFINES THE SIZE OF THE BOX OVER WHICH THE MEDIAN IS COMPUTED
C
	REAL*4 IN(IDIM,*),OUT(IDIM,*),MEDN
C
C      SORT IS USED TO SORT THE PIXELS IN EACH BOX FOR THE MEDIAN
C
      DOUBLE PRECISION SORT(2500)
C
C      BOXSQ IS THE NUMBER OF PIXELS IN THE BOX (REAL) IBOXSC IS THE
C      INTEGER VERSION.
C
      IBOXSQ=IBOX*IBOX
      BOXSQ=IBOXSQ
C
C      FIRST ZEROIZE THE OUTPUT ARRAY
C
      DO 20 J=1,M
         DO 10 I=1,N
           OUT(I,J)=0
   10    CONTINUE
   20 CONTINUE
C
C      NOW FIGURE WHICH PIXELS CAN HAVE A BOX OF THIS SIZE ROUND THEM
C
      ISIDE=(IBOX-1)/2
      IS=1+ISIDE
      IE=N-ISIDE
      JS=1+ISIDE
      JE=M-ISIDE
      LOOP=0
C
C      NOW LOOP THROUGH ALL POSSIBLE PIXELS
C
      DO 500 J=JS,JE
         JHS=J-ISIDE
         JHE=J+ISIDE
         DO 400 I=IS,IE
            IHS=I-ISIDE
            IHE=I+ISIDE
C
C      SET THE PIXEL VALUES INTO A 1D ARRAY FOR SORTING
C
            K=1
            DO 200 JH=JHS,JHE
               DO 180 IH=IHS,IHE
                  SORT(K)=IN(IH,JH)
                  K=K+1
  180          CONTINUE
  200       CONTINUE
C
C Now call NAG M01ANF routine to do the sort
C
            IFAIL=0
            CALL M01ANF(SORT,1,IBOXSQ,IFAIL)
C
C and test that it is ok
C
            IF (IFAIL.NE.0) GO TO 400
C
C Now pick central value
C
            MEDN=SORT((IBOXSQ+1)/2)
C
C and store it in the output array
C
            OUT(I,J)=MEDN
  400    CONTINUE
  500 CONTINUE
C
C and return to calling segment
C
      RETURN
      END
