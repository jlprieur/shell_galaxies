C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C PATCH1
C
C  It allows the user to replace several circular  patches  in
C  an image with a fitted noisy piece of synthetic data. It is
C  derived from a program, by W.D. Pence (University of Sussex).
C
C  It may be used to remove large defects, bright galaxies  or
C  any other localised unwanted pixels. The result can be very
C  convincing. The input file with the positions and diameters
C  should be of the same format as the one created by REMSTAR (x,y,diam).
C
C Syntax:
C     RUNS PATCH1 input_image output_image [Y/N manual input of the noise?]
C          [[sigma_noise]] input_position_file
C Example:
C    RUNS PATCH1 test test_patched N test.pat
C    RUNS PATCH1 test test_patched Y 12.4 test.pat
C
C JLP
C Version 19-07-90
C--------------------------------------------------------------------------
C  Do not worry if you have arithmetic underflow, just compile it
C  with FORTRAN/CHECK=NOUNDERFLOW
C********************************************************************
	PROGRAM PATCH1
	PARAMETER (NDIMER=200)
	CHARACTER FILE_IN*40,FILE_OUT*40,NAME*40,ANS*1
	CHARACTER COMMENTS*80
	REAL*4 XS,YS,ERR1(NDIMER)
	INTEGER*4 IS,JS,MADRID(1)
	COMMON /VMR/MADRID
 
10	FORMAT(A)
 
C INCLUDE 'INTERIM(FMTPAR)'
	CALL JLP_BEGIN
 
	WRITE(6,11)
11	FORMAT(' PROGRAM PATCH1 ',/,
     1	' Version of 15/04/90 ')
	OPEN(3,FILE='patch1.log',STATUS='unknown')
	WRITE(3,11)
 
C Input :
	CALL JLP_INQUIFMT
        WRITE(6,*) 'Input image:'
	READ(5,10) FILE_IN
	CALL JLP_VM_READIMAG(IPNT,NX,NY,FILE_IN,COMMENTS)
	WRITE(3,240)FILE_IN
240	FORMAT(' INPUT IMAGE :',A)
 
	WRITE(6,12) NX,NY
12	FORMAT(' SIZE OF THE FILE : NX =',I5,'  NY =',I5)
	WRITE(3,12) NX,NY
 
C Output :
	WRITE(6,*) ' OUTPUT IMAGE :'
	READ(5,10) FILE_OUT
	WRITE(3,242)FILE_OUT
242	FORMAT(' OUTPUT IMAGE :',A)
 
C Get virtual memory:
	I=NX*NY*4
	CALL JLP_GETVM(IOPNT,I)
 
C Copy the input image on the output image:
	CALL OUTDATA(MADRID(IPNT),MADRID(IOPNT),NX,NY)
C
C SET UP ARRAY OF RANDOM ERRORS WITH GAUSSIAN DISTRIBUTION
C FOR APPROXIMATING THE NOISE WHEN INTERPOLATING
	PRINT *,' DO YOU KNOW THE NOISE OF THE IMAGE ?(N)'
	READ(5,10) ANS
	IF(ANS.NE.'Y'.AND.ANS.NE.'y')THEN
	 CALL AUTO_SKY(MADRID(IOPNT),NX,NY,NX,SKY,SIGMA)
	 WRITE(6,243)SKY,SIGMA
	 WRITE(3,243)SKY,SIGMA
243	 FORMAT(' AUTO-SKY DETERMINATION :',/,
     1	' SKY, SIGMA :',2(G12.5,2X))
	ELSE
	 PRINT *,' NOISE ESTIMATION OF THE IMAGE :'
	 READ(5,*) SIGMA
	 WRITE(3,244)SIGMA
244	 FORMAT(' SIGMA:',G12.5)
	ENDIF
 
C Use NAG routines G05CBF and G05DDF to generate normal errors, mean=0, sigma=1
C	CALL G05CBF(0)
	CALL JLP_RANDOM_INIT(1)
	  DO 2 I=1,200
C	  ERR1(I)=G05DDF(0.,1.0)
          CALL JLP_RANDOM_GAUSS(ERR1(I))
2	  CONTINUE
 
	PRINT *,' NAME OF THE FILE CONTAINING THE POSITIONS ?'
	READ(5,10) NAME
	 WRITE(3,245)NAME
245	 FORMAT(' NAME OF THE POSITION FILE :',A)
 
	OPEN(UNIT=9,STATUS='OLD',FILE=NAME)
	IOBJECT=0
 
C Loop as long as File 9 contains something :
70	CONTINUE
 
C Entering centre (IS,JS), and diameter NS
	READ (9,*,END=5) XS,YS,NS
        IS=INT(XS+0.5)
        JS=INT(YS+0.5)
	IOBJECT=IOBJECT+1
 
C Only even diameters :
	NS=((NS+1)/2)*2
 
C Going to the next step :
C ORDER OF THE POLYNOMIAL (0, 1, 2, OR 3)'
	NCODE=3
	CALL STRDELT(MADRID(IOPNT),NX,NY,IS,JS,NS,
     1	ERR1,NDIMER,NCODE,SIGMA)
	GO TO 70
 
C End :
5	 WRITE(6,*) ' END OF FILE :',IOBJECT,'  OBJECTS TREATED'
	 WRITE(3,246) IOBJECT
246	 FORMAT(' NUMBER OF PATCHES : ',I5)
 
C Writing the data in the output file:
	COMMENTS=' '
	CALL JLP_WRITEIMAG(MADRID(IOPNT),NX,NY,NX,
     1	FILE_OUT,COMMENTS)
 
	CLOSE(3)
	CLOSE(9)
	CALL JLP_END
	STOP
	END
C----------------------------------------------------------------------
	SUBROUTINE OUTDATA(IN,OUT,N,M)
	REAL IN(N,M),OUT(N,M)
	 DO J=1,M
	  DO I=1,N
	   OUT(I,J)=IN(I,J)
	  END DO
	 END DO
	RETURN
	END
C--------------------------------------------------------------------
	SUBROUTINE STRDELT(ARRAY,NX,NY,IS,JS,
     1	NS,ERR1,NDIMER,NCODE1,SIGMA)
C****************************************************************
C
C Deletes pixels within a circle around point (is,js) and
C interpolates them with a polynomial fitted to an annulus
C of surrounding points.
C
C IS, JS : Position of the centre
C NS : Diameter of the patch
C
C****************************************************************
C	INCLUDE 'INTERIM(FMTPAR)'
	PARAMETER (NDIM=1000)
	REAL*4 ARRAY(NX,NY),ERR1(NDIMER)
	REAL*4 XX(NDIM,2),YY(NDIM)
	REAL*8 ZZ(NDIM),D(30),SE(30),RDOE(30),DUMMY
	REAL*4 XZ,YZ
	DOUBLE PRECISION POLY
	CHARACTER ANS*1
	INTEGER*4 MADRID(1)
	COMMON /VMR/MADRID
	EXTERNAL POLY

	NCODE=NCODE1
 
C Treating the case when the diameter is 2 (one bad pixel only):
C Linear interpolation between the surrounding pixels
	IF(NS.EQ.2)THEN
	 JM1=MAX0(JS-1,1)
	 JP1=MIN0(JS+1,NY)
	 IM1=MAX0(IS-1,1)
	 IP1=MIN0(IS+1,NY)
	 ARRAY(IS,JS)=(ARRAY(IM1,JS)+ARRAY(IP1,JS)
     1	+ARRAY(IS,JM1)+ARRAY(IS,JP1))/4.0
 	 RETURN
	ENDIF
 
C Treating the case when the diameter is 4 (two bad pixels):
C Linear interpolation between the surrounding pixels
	IF(NS.EQ.4)THEN
	 JM1=MAX0(JS-1,1)
	 JP1=MIN0(JS+1,NY)
	 JM2=MAX0(JS-2,1)
	 JP2=MIN0(JS+2,NY)
	 IM1=MAX0(IS-1,1)
	 IP1=MIN0(IS+1,NY)
	 IM2=MAX0(IS-2,1)
	 IP2=MIN0(IS+2,NY)
	 ARRAY(IS,JS)=(ARRAY(IM2,JS)+ARRAY(IP2,JS)
     1	+ARRAY(IS,JM2)+ARRAY(IS,JP2))/4.0
	 ARRAY(IM1,JS)=(ARRAY(IM2,JS)+ARRAY(IS,JS)
     1	+ARRAY(IM1,JM1)+ARRAY(IM1,JP1))/4.0
	 ARRAY(IP1,JS)=(ARRAY(IP2,JS)+ARRAY(IS,JS)
     1	+ARRAY(IP1,JP1)+ARRAY(IP1,JP1))/4.0
	 ARRAY(IS,JP1)=(ARRAY(IM1,JP1)+ARRAY(IP1,JP1)
     1	+ARRAY(IS,JS)+ARRAY(IS,JP2))/4.0
	 ARRAY(IS,JM1)=(ARRAY(IM1,JM1)+ARRAY(IP1,JM1)
     1	+ARRAY(IS,JS)+ARRAY(IS,JM2))/4.0
 	 RETURN
	ENDIF
 
C Limiting the order of the polynomial for small patches :
	IF(NS.EQ.6)NCODE=MIN(1,NCODE)
	IF(NS.EQ.8)NCODE=MIN(2,NCODE)
 
	IRAD2=(NS/2)**2
C
C DEFINE LIMITS OF AREA TO DELETE
C
	I1=MAX(IS-NS/2,1)
	I2=MIN(IS+NS/2,NX)
	J1=MAX(JS-NS/2,1)
	J2=MIN(JS+NS/2,NY)
C
C TEMPORARILY STORE CURRENT ARRAY CONTENTS
C
	ISIZE=4*(I2-I1+1)*(J2-J1+1)
	CALL JLP_GETVM(IPNT,ISIZE)
	CALL STRCOPY(ARRAY,NY,NX,MADRID(IPNT),ISIZE,I1,I2,J1,J2)
	XOLD=ARRAY(IS,JS)
C
C SET UP ARRAYS FOR NEQSOL
C Fit the background in an annulus between IDIAM AND FACTOR*IDIAM
	IF (NCODE .EQ. 0)THEN
	  FACTOR=1.5
	ELSE
	  FACTOR=2.
	ENDIF
	NWIDE=NS*FACTOR
	NWIDE2=(NWIDE+1)/2
	NWIDE2S=NWIDE2*NWIDE2
	NINC=(NWIDE-1)/30+1
	I11=MAX(IS-NWIDE2,1)
	I22=MIN(IS+NWIDE2,NX)
	J11=MAX(JS-NWIDE2,1)
	J22=MIN(JS+NWIDE2,NY)
C
C Define normalizing factors such that the coords. of
C all the points are between (-1,-1) and (1,1).
 
	DX=2./(I22-I11)
	DY=2./(J22-J11)
	NPTS=0
	DO 20 J=J11,J22,NINC
	DO 20 I=I11,I22,NINC
C
C CALC RADIUS, AND REJECT IF TOO CLOSE TO STAR CENTRE
	  KRAD2=(J-JS)*(J-JS)+(I-IS)*(I-IS)
	  IF (KRAD2 .GT. IRAD2 .AND. KRAD2 .LE. NWIDE2S)THEN
	    NPTS=NPTS+1
	    XX(NPTS,1)=(I-I11)*DX-1.
	    XX(NPTS,2)=(J-J11)*DY-1.
	    YY(NPTS)=ARRAY(I,J)
          ENDIF
20 	CONTINUE
 
30	IF (NCODE .EQ. 3)THEN
	  NTERM=10
	ELSE IF (NCODE .EQ. 2)THEN
	  NTERM=6
	ELSE IF (NCODE .EQ. 1)THEN
	  NTERM=3
	ELSE
	  NTERM=1
	END IF
C
	IF (NPTS.LE.NTERM)THEN
	  WRITE(6,122) NPTS
122	  FORMAT('ERROR: Too few points to fit background, npts=',I5)
	  GO TO 200
	END IF
C
C ERASE ANY PREVIOUS SOLUTION
C
	DO I=1,30
	  D(I)=0.
	END DO
 
	DO 40 KQ=1,2
C FIT THE POLYNOMIAL
C
	  CALL NEQSOL(XX,YY,ZZ,NDIM,NPTS,NTERM,1,0,D,SE,RDOE,SDEV)
C
C DO A 2 SIGMA REJECTION
C
	  TEST=2.*SDEV
	  CALL REJECT(XX,YY,NDIM,D,NPTS,NTERM,TEST,NR,1)
 	  NPTS=NR
	  IF (NPTS .LE. NTERM)THEN
	    WRITE(6,123) NPTS
123	    FORMAT('ERROR: Too few points after rejection, npts=',I5)
	    GO TO 200
	  END IF
C
40	CONTINUE
 
C REFIT POLYNOMIAL : (Coeff in D)
C
	  CALL NEQSOL(XX,YY,ZZ,NDIM,NPTS,NTERM,1,0,D,SE,RDOE,SD)
C
C EVALUATE POLYNOMIAL AT EACH POINT WITHIN CIRCLE
C
C@	  PRINT *,' IX,IY,NS(DIAM),SDEV(FIT)',IS,JS,NS,SDEV
	DO 60 J=J1,J2
C
C FIND A RANDOM STARTING POINTS IN THE SEQUENCE OF NOISE POINTS
C
C	  XK=G05CAF(DUMMY)
	  CALL JLP_RANDOM(XK) 
	  K=XK*199.
C
	  YZ=(J-J11)*DY-1.
 
	DO 50 I=I1,I2
	  IF ((J-JS)*(J-JS)+(I-IS)*(I-IS) .LE. IRAD2)THEN
	  XZ=(I-I11)*DX-1.
	  K=K+1
	  IF (K .GT. 200)K=1
C	  ZNOISE=ERR1(K)*SIGMA
C	  PRINT *,' ERR1(K),SIGMA',ERR1(K),SIGMA
C	  PRINT *,' POLY, ARRAY_old',POLY(XZ,YZ,D),ARRAY(I,J) 
	  ARRAY(I,J)=POLY(XZ,YZ,D)+ERR1(K)*SIGMA
          ENDIF
50	CONTINUE
 
C PLOT NEW LINE ON ARGS
C LOGICAL TRIM,LLOG
c@	  CALL ARGSROW(ARRAY,NX,NY,I1,I2,J,
c@     1	TRIM,LLOG,VLO,VHI,IXOR,IYOR)
 
60	CONTINUE
 
200	CONTINUE
 
	CALL JLP_FREEVM(IPNT,ISIZE)
 
300	CONTINUE
	END
C-------------------------------------------------------------------
	SUBROUTINE STRCOPY(ARRAY,NY,NX,TEMP,ISIZE,I1,I2,J1,J2)
C
C STORE CURRENT STAR IMAGE IN TEMPORARY ARRAY
C
	REAL ARRAY(NX,NY),TEMP(ISIZE)
C
	N=0
	DO 11 J=J1,J2
	  DO 11 I=I1,I2
	   N=N+1
	   TEMP(N)=ARRAY(I,J)
11	CONTINUE
	RETURN
C
C
	ENTRY STRRSTR(ARRAY,NY,NX,ITEMP,ISIZE,I1,I2,J1,J2)
C RESTORE ARRAY TO PREVIOUS STATE
C
	N=0
	DO 21 J=J1,J2
	  DO 21 I=I1,I2
	    N=N+1
	    ARRAY(I,J)=TEMP(N)
21	CONTINUE
	END
C--------------------------------------------------------------
	include 'jlpsub:auto_sky.for'
	include 'jlpsub:patch_set.for'
