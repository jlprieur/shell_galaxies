C++**************************************************************
C	REMGAL
C
C From PLAGAL_BATCH and PLAGAL
C
C Removes the galaxy in a CCD image.
C Removes a smoothed profile with elliptical isophotes.
C The parameters of the ellipses change with the equivalent radius.
C (Cubic spline fitted to the ellipse parameters)
C The profile values are obtained by cubic spline interpolation of the
C input levels of the ellipse fitting procedure (made by FITELLI)
C
C Reads the parameters of the ellipses fitted to the galaxy,
C as generated by "FITELLI" (*.EPA), ASCII file with the following format:
C
C 3 lines of comments,
C 1 line with NPTS (number of levels),
C 1 line of comments,
C and then NPTS lines with 6 values (semi-major axis in decreasing order):
C       Semi-major axis (pixels), Axis ratio b/a, Position angle (degrees),
C       X centre (pixels), Y centre (pixels), and Level of the isophote (ADU)
C
C SYNTAX:
C  RUNS REMGAL in_image out_image [xmin,xmax,ymin,ymax : output window]
C           input_parameter_file
C Example:
C  RUNS REMGAL test test_REM 100.,300.,150.,400. test.EPA
C  RUNS REMGAL test test_REM 0,0,0,0 test.EPA
C
C Nota1: the output image is computed in a subwindow of the input image,
C        according to the values of xmin,xmax,ymin,ymax.
c        In the second example 0,0,0,0 implies that we are working with
C        the full image.
C
C Differences with REMGAL1:
C  Here: possibility of a smaller output file.
C  Here: cubic spline interpolation 
C         (Chebycheff polynomial fit to the parameters)
C
C
C JLP
C Version of 19-09-90
C--**************************************************************
	PROGRAM REMGAL
C ELLP0: 1=semi maj. axis
C        2=b/a
C        3=Theta (deg.)
C        4=Xcent
C        5=Ycent
C
	REAL*8 ELLP0(6)
	REAL*4 SMAJMIN,SMAJMAX,SKY,SIGMA,BACKG,XSTART,YSTART
	INTEGER*4 PNTR_I,PNTR_IN,PNTR_OUT
	CHARACTER NAMEPAR*40,NAMEIN*40,NAMEOUT*40,ANS*1,COMMENTS*80
	INTEGER*4 MADRID(1)
	COMMON /VMR/MADRID
	COMMON/PLG_PARAM/SMAJMIN,SMAJMAX,SKY,BACKG,ELLP0,XSTART,YSTART
 
10	FORMAT(A)
 
	CALL JLP_BEGIN
 
C Open "remgal.log" to keep a trace of what has been done:
	OPEN(9,FILE='remgal.log',STATUS='unknown',
     1	ACCESS='SEQUENTIAL')
 
C Write the title :
	WRITE(9,81)
	PRINT 81
81	FORMAT(' PROGRAM REMGAL - Version of 10/07/90-',/)
 
C Inquires about the format of the files :
	CALL JLP_INQUIFMT
 
C Input of the names of the files:
	PRINT *,' INPUT IMAGE ?'
	READ(5,10) NAMEIN
 
	CALL JLP_VM_READIMAG(PNTR_IN,NXIN,NYIN,NAMEIN,COMMENTS)
	WRITE(9,82)NAMEIN
82	FORMAT(' INPUT IMAGE : ',A)
 
	PRINT *,' OUTPUT IMAGE ?'
	READ(5,10) NAMEOUT
 
	PRINT *,' OUTPUT WINDOW X1,X2,Y1,Y2 ?(0,0,0,0 if all)'
	READ(5,*) I1,I2,J1,J2
 
C Reduces the size:
	IF((I1.LT.I2).AND.(J1.LT.J2))THEN
	   I1=MAX(1,I1)
	   J1=MAX(1,J1)
	   I2=MIN(NXIN,I2)
	   J2=MIN(NYIN,J2)
	   NX=I2-I1+1
	   NY=J2-J1+1
C Get dynamical memory for the new file:
	   CALL JLP_GETVM(PNTR_I,NX*NY*4)
	   CALL REDUCE(MADRID(PNTR_IN),NXIN,NYIN,MADRID(PNTR_I),
     1	I1,J1,NX,NY)
	   XSTART=FLOAT(I1)-1.
	   YSTART=FLOAT(J1)-1.
	ELSE
	   NX=NXIN
	   NY=NYIN
	   PNTR_I=PNTR_IN
	   XSTART=0.
	   YSTART=0.
	ENDIF
 
C Name of the parameter file :
	PRINT *,' Input file with the ellipse parameters: (*.epa)'
	READ(5,10) NAMEPAR
	WRITE(9,83) NAMEPAR
83	FORMAT(' ELLIPSE PARAMETER FILE: ',A)
 
C Get dynamical memory for the output:
	CALL JLP_GETVM(PNTR_OUT,NX*NY*4)
 
C Sky level (on the full image):
	CALL AUTO_SKY(MADRID(PNTR_IN),NXIN,NYIN,NXIN,SKY,SIGMA)
 
C We take a smaller value for the sky (for a better fit):
	BACKG=SKY-3.*SIGMA
	WRITE(9,85) SKY,BACKG
85	FORMAT(' Sky level:',G12.4,/,
     1	' Background level (for the fit): ',G12.4)
 
C Read the parameter file and fit cubicsplines to the parameters:
	CALL PLG_RDPARAM(NAMEPAR)
 
C Subtracting the galaxy :
	CALL PLG_REMOVE(MADRID(PNTR_I),MADRID(PNTR_OUT),NX,NY,NX)
 
C Writing the output file with comments :
	LPAR=INDEX(NAMEPAR,' ')-1
	WRITE(COMMENTS,303) NAMEPAR(1:LPAR)
303	FORMAT(' REMGAL WITH PARAMETER FILE: ',A,' //')
	CALL JLP_WRITEIMAG(MADRID(PNTR_OUT),NX,NY,NX,NAMEOUT,COMMENTS)
 
C End :
	CLOSE(9)
	PRINT *,' Logfile in "remgal.log"'
	CALL JLP_END
	STOP
	END
C*************************************************************
C Subroutine PLG_RDPARAM
C to read the parameters of the ellipses fitted to the galaxy,
C as generated by "FITELLI":
C
C ASCII file with the following format:
C 3 lines of comments, 1 line with NPTS (number of levels), 1 line of comments,
C and then NPTS lines with 6 values (semi-major axis in decreasing order):
C       Semi-major axis (pixels), Axis ratio b/a, Position angle (degrees),
C       X centre (pixels), Y centre (pixels), and Level of the isophote (ADU)
C
C Input:
C NAMEPAR
C BACKG: Background value (for the fit only)
C*************************************************************
	SUBROUTINE PLG_RDPARAM(NAMEPAR)
	PARAMETER (IDIM=2000,IDIM1=100)
	REAL*8 ELLPAR(IDIM1,6),XLEVEL(IDIM1)
	REAL*8 SPLINE(IDIM,5),KNOTS(IDIM,5)
	REAL*8 ELLP0(6),RESULT
	REAL*4 SMAJMIN,SMAJMAX,SKY,SIGMA,BACKG,XSTART,YSTART
	INTEGER*4 NCAP7(5)
	CHARACTER NAMEPAR*40,BUFFER*80
	COMMON/PLG_SPLINE/SPLINE,KNOTS,NCAP7
	COMMON/PLG_PARAM/SMAJMIN,SMAJMAX,SKY,BACKG,ELLP0,XSTART,YSTART
 
10	FORMAT(A)
 
	OPEN(3,FILE=NAMEPAR,STATUS='OLD',ERR=999)
 
C Go to the right position (skipping 3 lines) in the file:
	 DO I=1,3
	  READ(3,10,ERR=999)BUFFER
	 END DO
 
C NPTS
         NPTS = 0
	 READ(3,*,ERR=999) NPTS
         WRITE(6,*) 'NPTS= ',NPTS
         IF(NPTS.LE.1)GOTO 999
	 READ(3,10,ERR=999)BUFFER
 
C Read the parameters:
C ELLPAR: 1=semi maj. axis
C         2=b/a
C         3=Theta (deg.)
C         4=Xcent
C         5=Ycent
C
	DO I=NPTS,1,-1
	  READ(3,*,ERR=999) (ELLPAR(I,K),K=1,5),XLEVEL(I)
          IF(ELLPAR(I,3).GT.180) ELLPAR(I,3)=ELLPAR(I,3)-180. 
          WRITE(6,*) (ELLPAR(I,K),K=1,5),XLEVEL(I)
	END DO
	SMAJMIN=ELLPAR(1,1)
	SMAJMAX=ELLPAR(NPTS,1)
 
C Transform the levels in log for a better fit:
28	CONTINUE
	  DO J=1,NPTS
	    WORK=XLEVEL(J)-BACKG
	    IF(WORK.GT.0.)THEN
	      ELLPAR(J,6)=ALOG10(WORK)
	    ELSE
	      PRINT *,' PB WITH THE',I,'th POINT, NEGATIVE VALUE,'
	      PRINT *,' I TAKE A SMALLER VALUE FOR THE BACKGROUND'
	      BACKG=XLEVEL(J)-3.*SIGMA
	      GO TO 28
	    ENDIF
	  END DO
 
C Fitting cubic splines to the parameters:
	DO K=1,5
	  CALL SPLINE_INTER(ELLPAR(1,1),ELLPAR(1,K+1),NPTS,
     1	SPLINE(1,K),NCAP7(K),KNOTS(1,K))
	END DO
 
C Output the two curves in a file:
	OPEN(14,FILE='remgal_pro.dat',STATUS='unknown')
	DO J=1,NPTS
	   IFAIL=0
C Calling NAG routine E02BBF to get the cubic spline approximate:
	   CALL E02BBF(NCAP7(5),KNOTS(1,5),SPLINE(1,5),ELLPAR(J,1),
     1	RESULT,IFAIL)
	   WRITE(14,12) ELLPAR(J,1),ELLPAR(J,6),RESULT
12	   FORMAT(3(1X,G11.4))
	END DO
	CLOSE(14)
 
C Mean values for initialization:
	DO K=1,6
	  ELLP0(K)=0.
	  DO J=1,NPTS
	    ELLP0(K)=ELLPAR(J,K)+ELLP0(K)
	  END DO
	  ELLP0(K)=ELLP0(K)/FLOAT(NPTS)
	END DO
 
C Write in the logfile "remgal.log" :
	WRITE(9,73)NAMEPAR,NPTS
73	FORMAT(' INPUT PROFILE :',A,' NPTS =',I6)
 
C Return
	CLOSE(3)
	RETURN
 
C Exit if error :
999	WRITE(6,*) 'FATAL ERROR reading the input parameter file ',NAMEPAR
	STOP
	END
C*************************************************************
C Subroutine PLG_REMOVE
C
C Input :
C INPUT(IDIM1,*)
C NX,NY : Size of the image
C
C Output :
C OUTPUT(IDIM1,*)
C
C*************************************************************
	SUBROUTINE PLG_REMOVE(INPUT,OUTPUT,NX,NY,IDIM1)
	REAL*4 INPUT(IDIM1,*),OUTPUT(IDIM1,*)
	REAL*8 ELLP0(6)
	REAL*4 X1,Y1,Z1
	REAL*4 SMAJMIN,SMAJMAX,SKY,SIGMA,BACKG,XSTART,YSTART
	COMMON/PLG_PARAM/SMAJMIN,SMAJMAX,SKY,BACKG,ELLP0,XSTART,YSTART
 
10	FORMAT(A)
 
C Initialize the ellipse parameters:
	  CALL PLG_ELLINIT
C Main Loop :
 
	DO J=1,NY
	  Y1=FLOAT(J)
	  DO I=1,NX
	    X1=FLOAT(I)
            CALL PLG_VALUE(X1,Y1,Z1,ISTAT)
C Subtracting Z1, the level corresponding to the semi-major axis
C of the ellipse containing the pixel (x,y)
	    IF(ISTAT.EQ.0)THEN
	       OUTPUT(I,J)=INPUT(I,J)-BACKG-10**Z1
C If semi-major axis too big, simply subtract the sky level:
	     ELSEIF(ISTAT.EQ.-10)THEN
	       OUTPUT(I,J)=INPUT(I,J)-SKY
C Otherwise, set the value to 0 (central regions)
	     ELSE
	       OUTPUT(I,J)=0.
	     ENDIF
	  END DO
	  IF(MOD(J,20).EQ.0)PRINT *,' Done up to line:',J
	END DO
 
	RETURN
	END
C**********************************************************************
C Subroutine PLG_VALUE
C To compute the value in the pixel X1, Y1
C with an approximation: takes the
C position of the center which was valid
C for the previous pixel (could be divergent otherwise in some cases).
C The typical variation of the center from one pixel to another
C is of the order 0.01-0.1 which is smaller than the tolerance anyway.
C
C**********************************************************************
	SUBROUTINE PLG_VALUE(X1,Y1,Z1,ISTAT)
	REAL*8 ELLP(6),ELLP0(6),PI
	REAL*4 X1,Y1,Z1
	REAL*4 SMAJMIN,SMAJMAX,SKY,BACKG,XSTART,YSTART
	REAL*4 UU,VV,WW,X2,Y2,CO,SI,F11,TT
	INTEGER*4 ISTAT
	COMMON/PLG_ELLI/ELLP
	COMMON/PLG_PARAM/SMAJMIN,SMAJMAX,SKY,BACKG,ELLP0,XSTART,YSTART
        PI=3.14159
 
C Using the previous values of the ellipse parameters:
	X2=X1-ELLP(4)+XSTART
	Y2=Y1-ELLP(5)+YSTART
	X22=X2*X2
	Y22=Y2*Y2
	XY22=X2*Y2
   	CO=COS(ELLP(3)*PI/180.)
	SI=SIN(ELLP(3)*PI/180.)
	TT=1./(ELLP(2)*ELLP(2))
	UU=CO*CO+SI*SI*TT
	VV=SI*SI+CO*CO*TT
	WW=2.*SI*CO*(1.-TT)
	F11=UU*X22+VV*Y22+WW*XY22
	SMAJ=SQRT(F11)
	
C------------------------------------------------------------
C Computing the semi-major axis in less than 30 iterations
	TOLERANCE=AMAX1(0.5,SMAJ/100.)
 
	DO I=1,30
C Computing the ellipse parameters for SMAJ (Axis ratio and position angle):
	  CALL PLG_ELLPAR(SMAJ,ISTAT,1,2)
	  IF(ISTAT.NE.0) RETURN
 
C Computing the new value of the semi major axis SMAJ
	  CO=COS(ELLP(3)*PI/180.)
	  SI=SIN(ELLP(3)*PI/180.)
	  TT=1./(ELLP(2)*ELLP(2))
	  UU=CO*CO+SI*SI*TT
	  VV=SI*SI+CO*CO*TT
	  WW=2.*SI*CO*(1.-TT)
	  F11=UU*X22+VV*Y22+WW*XY22
	  SMAJ1=SQRT(F11)
 
C Test output (for debugging):	
	  IF(X1.EQ.190..AND.Y1.GT.100.)
     1    PRINT 29,X1,Y1,(ELLP(KI),KI=2,5),SMAJ1
29	  FORMAT(' X,Y,AXR,THE,XC,YC,SMAJ',2(F5.1,1X),5(F7.3))
 
C Exit from the loop if small errors:
	  ERROR=ABS(SMAJ1-SMAJ)
	  IF(ERROR.LT.TOLERANCE) GOTO 25
 
C Dampering factor to avoid oscillations:
	  SMAJ=SMAJ1*0.9+SMAJ*0.1
	END DO
 
	PRINT *,' WARNING: MORE THAN 30 ITERATIONS,  ERROR=',ERROR
	PRINT *,' TOO LARGE VARIATIONS FOR THE ELLIPSE PARAMETERS'
 
C Computing the coresponding level for this step, and the
C initial ellipse parameters for the next one:
25	CALL PLG_ELLPAR(SMAJ,ISTAT,1,5)
	Z1=ELLP(6)
 
	RETURN
	END
C**********************************************************************
C Subroutine PLG_ELLPAR
C To compute the ellipse parameters for a given semi major axis
C
C Input:
C SMAJ: semi major axis (pixels)
C
C Output:
C ELLP(6): ellipse parameters
C ISTAT: 0 if OK
C**********************************************************************
	SUBROUTINE PLG_ELLPAR(SMAJ,ISTAT,K1,K2)
	PARAMETER (IDIM=2000)
	REAL*8 SPLINE(IDIM,5),KNOTS(IDIM,5)
	REAL*8 ELLP0(6),ELLP(6),RESULT
	REAL*4 SMAJMIN,SMAJMAX,SKY,SIGMA,BACKG,XSTART,YSTART
	INTEGER*4 ISTAT,IFAIL,NCAP7(5)
	COMMON/PLG_ELLI/ELLP
	COMMON/PLG_SPLINE/SPLINE,KNOTS,NCAP7
	COMMON/PLG_PARAM/SMAJMIN,SMAJMAX,SKY,BACKG,ELLP0,XSTART,YSTART
 
 	ISTAT=0
 
C Computing the corresponding parameters for the ellipse
C of semi-major axis SMAJ:
	ELLP(1)=SMAJ
	IF(SMAJ.GT.SMAJMIN.AND.SMAJ.LT.SMAJMAX)THEN
	  DO K=K1,K2
C Calling NAG routine E02BBF to get the cubic spline approximate: 
	      IFAIL=1
	      CALL E02BBF(NCAP7(K),KNOTS(1,K),SPLINE(1,K),ELLP(1),
     1	RESULT,IFAIL)
	      IF(IFAIL.NE.0)THEN
	        PRINT *,' PGL_ELLPAR: FAILURE IN E02BBF, IFAIL=',IFAIL
	        ISTAT=-1
	      ELSE
 	        ELLP(K+1)=RESULT
	      ENDIF
	  ENDDO
C Problem when the point is outside of the range in radius:
	ELSEIF(SMAJ.GE.SMAJMAX)THEN
	  ISTAT=-10
	ELSE
	  ISTAT=-11
	ENDIF
 
	RETURN	
	END
 
C**********************************************************************
C Subroutine PLG_ELLINIT
C Initialize the ellipse parameters UU, VV and WW
C
C**********************************************************************
	SUBROUTINE PLG_ELLINIT
	REAL*8 ELLP0(6),ELLP(6)
	REAL*4 SMAJMIN,SMAJMAX,SKY,SIGMA,BACKG,XSTART,YSTART
	COMMON/PLG_PARAM/SMAJMIN,SMAJMAX,SKY,BACKG,ELLP0,XSTART,YSTART
	COMMON/PLG_ELLI/ELLP
 
C First guess, using the mean values of the ellipse parameters
	DO I=1,6
	  ELLP(I)=ELLP0(I)
	END DO
 
	RETURN
	END
C*************************************************************
C To reduce the size of an image
C
C*************************************************************
	SUBROUTINE REDUCE(IN,NXIN,NYIN,OUT,I1,J1,NXOUT,NYOUT)
	REAL*4 IN(NXIN,*),OUT(NXOUT,*)
 
	DO J=J1,J1+NYOUT-1
	  JJ=J-J1+1
	  DO I=I1,I1+NXOUT-1
	    II=I-I1+1
	    OUT(II,JJ)=IN(I,J)
	  END DO
	END DO
 
	RETURN
	END
C********************************************************
	include 'jlpsub:auto_sky.for'
C Contains SPLINE_INTER :
        include 'jlpsub:project.for'
