       PROGRAM SHELL_COLOURS
C++:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C  This program reads two (or more) profiles of shells, fits baselines,
C  computes the intensity above the baseline, and ratios of the two intensities
C
C Calls : BACKGROUND,
C         INDEX_MAX4, CALPOLY, POLYFIT.
C
C JLP
C Version of 06-07-97 
C--:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C  RAD(IDIM): radius of the input profile
C  PROF(IDIM): input profile
C  VNUMB(IDIM): number of pixels of the image used for computing
C               the corresponding value of the input profile
C  DIFF(IDIM): difference (input profile - background)
C  BACK(IDIM): background
C
C Integer*4:
C  NBINS : number of points of the input profile
C-----------------------------------------------------------------------------
	PARAMETER (IDIM=2000,KMAX=6)
	REAL*4 RAD(IDIM,KMAX),PROF(IDIM,KMAX),VNUMB(IDIM,KMAX)
	INTEGER*4 NBINS(KMAX)
	CHARACTER NAMECAT*40,ANS*1,TITLE*40,PLOTDEV*32
	LOGICAL PROFILE1_FMT
        CHARACTER FILENAME*60,COMMENTS*80
	CHARACTER NCHAR(KMAX)*4
	COMMON/DATA1/RAD,PROF,VNUMB,NBINS,NCHAR
	include 'jlpsub:jlp_graphic_help.for'
 
10	FORMAT(A)
 
81	PRINT 84
84	FORMAT(' Program SHELL_COLOURS Version of 06-07-97',/,
     1	' Fits baselines to profiles and computes luminosities',/,
     1	' You can work with many profiles simultaneously (shells',
     1	' in B,V,R, background,...)',/,
     1	' Number of profiles you want to',
     1	' superimpose on the same graph (<6) ?')
	READ(5,*) KMODE
	IF((KMODE.LT.1).OR.(KMODE.GT.6))GOTO 81
 
83	PRINT 86
86	FORMAT(' Graphic device (? for help, "Return" for &xterm)')
	READ(5,10) PLOTDEV
 
	IF(PLOTDEV(1:1).EQ.'?')THEN
	  PRINT 3333
	  GOTO 83
	ELSEIF(PLOTDEV(1:1).EQ.' ')THEN
	  PLOTDEV='&xterm'
 	ENDIF
	
	PRINT 85
85	FORMAT(' Possible format for input profiles:'
     1	'  -- 2 columns "radius,mean_value"',/,	
     1	'  -- profile created with "PROFILE1"',/,
     1	' Are-you working with the second format? (Y)')
	READ(5,10) ANS
	PROFILE1_FMT=(ANS.NE.'n'.AND.ANS.NE.'N')
 
C Opens a record output file
C	PRINT *,' Name of the catalog to be updated:'
	PRINT *,' Name of the output catalog:'
	READ(5,10) NAMECAT
C Opens a record output file ("APPEND" is unknown by IBM)
C	OPEN(10,FILE=NAMECAT,STATUS='UNKNOWN',ACCESS='APPEND')
	OPEN(10,FILE=NAMECAT,STATUS='UNKNOWN')
 
C--------------------------------------------------------------------
C  Reads the two profiles and returns NBINS, RAD, PROF and VNUMB
C
	DO K=1,KMODE
	  CALL READ_PROFILE(RAD(1,K),PROF(1,K),VNUMB(1,K),
     1	NBINS(K),FILENAME,COMMENTS,PROFILE1_FMT)
93	  PRINT 94
94	  FORMAT(' Type of line you want ("L0"=solid,... "?"',
     1	' for help)')
	  READ(5,10) NCHAR(K)
	  IF(NCHAR(K)(1:1).EQ.'?')THEN
	    PRINT 3334
	    GOTO 93
	  ELSEIF(NCHAR(K)(1:1).EQ.' ')THEN
	    NCHAR(K)='L0'
 	  ENDIF
	END DO
 
	PRINT *,' Enter title for the graphs:'
	READ(5,10) TITLE
 
 
88	PRINT 82
82	FORMAT(' Menu:',/,
     1	' 1. Fitting a baseline and computing total intensities',/,
     1	' 10. Exit',/,
     1	' Enter the option you want :',$)
	READ(5,*) IOPT
 
C----------------------------------------------------------------------
C Fits a background, compute intensity, colour ratios, etc.
	IF(IOPT.EQ.1)THEN
	   CALL BACKGROUND(TITLE,PLOTDEV,KMODE,FILENAME,COMMENTS)
	   GO TO 88
	ENDIF
 
C--------------------------------------------------------------
C End
999	CLOSE(9)
	CLOSE(10)
	PRINT 987,NAMECAT
987	FORMAT(' Output in ',A)
	END
C************************************************************
C Subroutine BACKGROUND
C
C KMODE (2 if 2 profiles, 3 if 3 profiles)
C DIFF : Real*8 array with the difference
C BACK : Real*8 array with the background
C
C Calls : INDEX_MAX4, CALPOLY, POLYFIT
C
C************************************************************
	SUBROUTINE BACKGROUND(TITLE,PLOTDEV,KMODE,FILENAME,COMMENTS)
	PARAMETER (IDIM=2000,KMAX=6)
	REAL*4 RAD(IDIM,KMAX),PROF(IDIM,KMAX),VNUMB(IDIM,KMAX)
	REAL*4 RSTART(KMAX),REND(KMAX)
	REAL*4 XOUT1(IDIM,KMAX),YOUT1(IDIM,KMAX)
	REAL*8 XCOEFF(10,KMAX)
	REAL*4 DIFF(IDIM,KMAX),BACK(IDIM,KMAX)
	REAL*4 XOUT,YOUT,DX,DY
	INTEGER*4 NBINS(KMAX),NOUT1(KMAX)
	INTEGER*4 NSTART(KMAX),NEND(KMAX)
	INTEGER*4 NSTART1(KMAX),NEND1(KMAX)
	CHARACTER CHAR1*30,CHAR2*30,PLOTDEV*32,TITLE*40
	CHARACTER NCHAR(KMAX)*4
        CHARACTER FILENAME*(*),COMMENTS*(*)
	COMMON/DATA1/RAD,PROF,VNUMB,NBINS,NCHAR
 
C Common block with NEWPLOT :
	 COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
 
10	 FORMAT(A)
 
C DISPLAYING THE INPUT PROFILE AND FITTING A POLYNOMIAL
200	 WRITE (6,201)
201	 FORMAT(' Enter start radius and end radius for the display : ',
     1	'(To exit type 0,0)')
	 READ(5,*) WORK1,WORK2
C If 0.,0. return to the main menu (If for example the user has typed the wrong
C option, he has not to fit a new background ...)
	  IF(WORK1.EQ.0.AND.WORK2.EQ.0.)RETURN
	 RRSTART=WORK1
	 RREND=WORK2
 
C Else computing the limiting indices :
	 DO K=1,KMODE
	   CALL INDEX_MAX4(RAD(1,K),NBINS(K),RRSTART,NSTART(K))
	   CALL INDEX_MAX4(RAD(1,K),NBINS(K),RREND,NEND(K))
	   RSTART(K)=RAD(NSTART(K),K)
	   REND(K)=RAD(NEND(K),K)
	 END DO
 
	PRINT *,' NSTART,NEND:',(NSTART(KK),NEND(KK),KK=1,KMODE)
 
	PRINT 23
23	FORMAT(' Please enter the points for fitting the baselines,',
     1	/,' Enter twice the last point of each set',
     1	' (even for the last set)')
 
	CHAR1=' Major axis (arcsec)'
	CHAR2=' Surf. Brightness'
	CALL SHC_DISPLAY(RAD,PROF,IDIM,KMODE,NSTART,NEND,NCHAR,
     1	RAD,PROF,IDIM,0,NSTART,NEND,NCHAR,
     1	CHAR1,CHAR2,TITLE,PLOTDEV,FILENAME,COMMENTS)
 
C If the user has not entered any points, I ask him another set of parameters
C for the frame.
	IF(NOUT.LE.1) GO TO 200
	PRINT *,NOUT,' POINTS'
 
C Sorting out the entered points:
	I=1
	J=1
	K=1
51	XOUT1(J,K)=XOUT(I)
	YOUT1(J,K)=YOUT(I)
C All the set of measurements should end by entering twice the
C same point (even the last):
C JLP97: I put a tolerance to allow for mouse uncertainties...
         DX=XOUT(I+1)-XOUT(I)
         IF(DX.LT.0) DX = -DX
         DY=YOUT(I+1)-YOUT(I)
         IF(DY.LT.0) DY = -DY
	 IF(DX.LT.0.2.AND.DY.LT.0.2)THEN
	   NOUT1(K)=J
	   J=0
	   K=K+1
	   I=I+1
	 ENDIF
	I=I+1
	J=J+1
	IF(I.LE.(NOUT-1).AND.(K.LE.KMODE))GOTO 51
C Here, just in case the user has not entered twice the last point:
	NOUT1(K)=J-1
 
C Looking for the minimum number of points:
	NNOUT=NOUT1(1)
        KMIN=1
	DO K=1,KMODE
          IF(NOUT1(K).LT.NNOUT)THEN
            NNOUT = NOUT1(K)
            KMIN=K
          ENDIF
	END DO
	PRINT *,'Minimum number of points is:',NNOUT,' for set #',KMIN
	IF(NNOUT.EQ.0)RETURN
 
C Fitting a polynomial to each set of points:
205	PRINT *,' Enter the order of the polynomial : (<9)'
	READ(5,*) KORDER
 
	IF(KORDER.GE.NNOUT)THEN
	  PRINT *,' Error, too few points'
	  RETURN
	ENDIF
 
	DO K=1,KMODE
	  CALL BACK_FIT(XOUT1(1,K),YOUT1(1,K),NOUT1(K),KORDER,
     1	RAD(1,K),PROF(1,K),VNUMB(1,K),
     1	NSTART(K),NEND(K),DIFF(1,K),BACK(1,K),
     1	NSTART1(K),NEND1(K),XCOEFF(1,K))
	END DO
 
C Displaying the fit
	PRINT *,'Displaying the background, now...'
	CHAR1=' Major axis (arcsec)'
	CHAR2=' Surf. Brightness'
	CALL SHC_DISPLAY(RAD,PROF,IDIM,KMODE,NSTART,NEND,NCHAR,
     1	RAD,BACK,IDIM,KMODE,NSTART1,NEND1,NCHAR,
     1	CHAR1,CHAR2,TITLE,PLOTDEV,FILENAME,COMMENTS)
 
208	PRINT 209
209	FORMAT('       Menu 2 :',/,
     1	' 1. Change the order of the polynomial',/,
     1	' 2. Displaying the difference and entering the limits',/,
     1	' 3. Calculating the intensity over the background',/,
     1	' 10. Return to the main menu',/,
     1	'       ENTER THE OPTION YOU WANT : ',$)
	READ(5,*) IOPT2
 
	IF(IOPT2.EQ.1)GO TO 205
	IF(IOPT2.EQ.10)RETURN
 
C------------------------------------------------------------------------
C Displaying the difference
	IF(IOPT2.EQ.2)THEN
 
	 PRINT 206
206	 FORMAT(' Displaying the difference...',/,
     1	' PLEASE ENTER THE LIMITS (2 points) IF YOU WANT',
     1	' TO COMPUTE THE TOTAL INTENSITY',/)
	CALL SHC_DISPLAY(RAD,DIFF,IDIM,KMODE,NSTART,NEND,NCHAR,
     1	RAD,DIFF,IDIM,0,NSTART,NEND,NCHAR,
     1	CHAR1,CHAR2,TITLE,PLOTDEV,FILENAME,COMMENTS)
	  IF(NOUT.EQ.2)THEN
	    XMIN=XOUT(1)
	    XMAX=XOUT(2)
	    PRINT 409,XMIN,XMAX
409	    FORMAT(/,' OK, two boundaries have been entered',/,
     1	2X,2(G12.4,2X),/)
	  ELSE
	    PRINT 410
410	    FORMAT(/,' WARNING : the boundaries have not been entered',/)
	  ENDIF
 
	GO TO 208
	ENDIF
 
C-----------------------------------------------------------------
C Calculates the intensity
 
	IF(IOPT2.EQ.3)THEN
 
	   DO K=1,KMODE
	   CALL SHELL_INTENSITY(RAD(1,K),PROF(1,K),VNUMB(1,K),
     1	DIFF(1,K),BACK(1,K),NSTART(K),NEND(K),
     1	XMIN,XMAX,XCOEFF(1,K),KORDER,XLUMI)
	   END DO
 
C	   RATIO=XLUMI1/XLUMI2
C	   PRINT 78,RATIO
C	   WRITE(10,78) RATIO
78	   FORMAT(' RATIO:',10X,G12.5)
 
	GO TO 208
	ENDIF
 
	RETURN
	END
C--------------------------------------------------------------------
C Subroutine READ_PROFILE
C Reads the profile and returns NBINS, RAD, PROF, VNUMB
C
C Input :
C PROFILE1_FMT : Logical (.TRUE. if file created with PROFILE1)
C
C Output :
C  RAD(IDIM): radius of the input profile
C  PROF(IDIM): input profile
C  VNUMB(IDIM): number of pixels of the image used for computing
C               the corresponding value of the input profile
C  NBINS : number of points of the input profile
C--------------------------------------------------------------------
	SUBROUTINE READ_PROFILE(RAD,PROF,VNUMB,NBINS,
     1	FILENAME,COMMENTS,PROFILE1_FMT)
	REAL*4 RAD(*),PROF(*),VNUMB(*)
	REAL*4 AMIN,POSITION_ANGLE,AXIS_RATIO,X0,Y0
	INTEGER*4 NBINS
	CHARACTER IMAGE_NAME*40,FILENAME*(*),COMMENTS*(*),BUFFER*80
	LOGICAL PROFILE1_FMT
10	FORMAT(A)
 
100	 PRINT 101
101	 FORMAT(' NAME OF THE PROFILE : ')
	 READ(5,10) FILENAME
102	 FORMAT(' PROFILE :',A)
	 OPEN (9,FILE=FILENAME,STATUS='OLD',ERR=100)
 
	 WRITE(10,905) FILENAME
905	 FORMAT('#',/,' PROFILE',13X,A)
 
C-------------------------------
C For profiles from "PROFILE1" read the 32 first lines :
	 IF(PROFILE1_FMT)THEN
  	    READ(9,10) BUFFER
	    READ(9,10) IMAGE_NAME
	    WRITE(10,906) IMAGE_NAME
	    WRITE(COMMENTS,906) IMAGE_NAME
906	    FORMAT(' IMAGE',14X,A)
	     DO I=3,9
 	      READ(9,10) BUFFER
	     END DO
	    READ(9,*) POSITION_ANGLE
	    WRITE(10,907) POSITION_ANGLE
907	    FORMAT(' POSITION ANGLE',4X,G12.5)
 
 	    READ(9,10) BUFFER
	    READ(9,*) AXIS_RATIO
	    WRITE(10,908) AXIS_RATIO
908	    FORMAT(' AXIS RATIO',9X,G12.5)
 
 	    READ(9,10) BUFFER
	    READ(9,*) X0,Y0
	    WRITE(10,909) X0,Y0
909	    FORMAT(' CENTRE',12X,G12.5,1X,G12.5)
 
	     DO I=15,19
	       READ(9,10) BUFFER
	     END DO
	    READ(9,*) AMIN,AMAX
	    WRITE(10,910) AMIN,AMAX
910	    FORMAT(' LIMITING ANGLES',3X,G12.5,1X,G12.5)
	     DO I=21,32
	      READ(9,10) BUFFER
	     END DO
 
C---------------------------
C Read the number of points :
	 READ(9,*)NBINS
	  DO I=1,NBINS
	   READ (9,*,END=111) RAD(I),PROF(I),VNUMB(I)
	   RAD(I)=RAD(I)/SQRT(1.-AXIS_RATIO/10.)
	  END DO
 
       ELSE
 
C---------------------------
C Simple format :
	 READ(9,*)NBINS
	  DO I=1,NBINS
	   READ (9,*,END=111) RAD(I),PROF(I)
	   VNUMB(I)=1.
	  END DO
 
	ENDIF
 
	 PRINT 103,NBINS
103	 FORMAT(2X,I4,' POINTS RECEIVED')
	 CLOSE(9)
	 RETURN
 
C Error message if not enough points :
111	 NBINS=I-1
	 PRINT 112
112	 FORMAT(' WARNING : LESS POINTS THAN EXPECTED !')
	 PRINT 103,NBINS
	 CLOSE(9)
 	 RETURN
 
	END
C------------------------------------------------------------------
C Subroutine BACK_FIT
C Fits a polynomial to the input points, and returns the background and
C the difference (profile-background)
C
C Input:
C XOUT,YOUT,NOUT,KORDER,RAD,PROF,VNUMB,NSTART,NEND
C Output:
C DIFF,BACK,NOUTSTART,NOUTEND,XCOEFF
C------------------------------------------------------------------
	SUBROUTINE BACK_FIT(XOUT,YOUT,NOUT,KORDER,RAD,PROF,VNUMB,
     1	NSTART,NEND,DIFF,BACK,NOUTSTART,NOUTEND,XCOEFF)
	PARAMETER (IDIM1=200)
	REAL*4 XOUT(*),YOUT(*),XOUTMAX,XOUTMIN
	REAL*8 XXOUT(IDIM1),YYOUT(IDIM1),WWORK1,WWORK2
	REAL*8 XCOEFF(10),SDEV(10),ERR
	REAL*4 RAD(*),PROF(*),VNUMB(*)
	REAL*4 DIFF(*),BACK(*)
	INTEGER*4 NOUT,NSTART,NEND,KORDER
 
C Conversion into real*8
	DO I=1,NOUT
	 XXOUT(I)=XOUT(I)
	 YYOUT(I)=YOUT(I)
c	 PRINT *,' XXOUT,YYOUT,I',XXOUT(I),YYOUT(I),I
	END DO
 
C Fitting a polynomial
	IF(KORDER.GE.NOUT)THEN
	  PRINT *,' Error, too few points'
	  RETURN
	ENDIF
	CALL POLYFIT(XXOUT,YYOUT,NOUT,KORDER,XCOEFF,SDEV,ERR)
	
C Computing the limits for plotting the polynomial
	XOUTMAX=XOUT(1)
	XOUTMIN=XOUT(1)
	DO I=1,NOUT
	 XOUTMAX=AMAX1(XOUTMAX,XOUT(I))
	 XOUTMIN=AMIN1(XOUTMIN,XOUT(I))
	END DO
	XOUTMAX=AMIN1(XOUTMAX,RAD(NEND))
	XOUTMIN=AMAX1(XOUTMIN,RAD(NSTART))
	CALL INDEX_MAX4(RAD,NEND,XOUTMIN,NOUTSTART)
	CALL INDEX_MAX4(RAD,NEND,XOUTMAX,NOUTEND)
 
C Computing the curves:
	 DO I=NSTART,NEND
	   WWORK1=RAD(I)
	   CALL CALPOLY(WWORK1,WWORK2,XCOEFF,KORDER)
	   DIFF(I)=PROF(I)-WWORK2
	   BACK(I)=WWORK2
	 END DO
	RETURN
 
	END
C-------------------------------------------------------------------------
C Subroutine SHELL_INTENSITY
C To compute the total intensity above the background: XLUMI
C-------------------------------------------------------------------------
	SUBROUTINE SHELL_INTENSITY(RAD,PROF,VNUMB,DIFF,BACK,
     1	NSTART,NEND,XMIN,XMAX,XCOEFF,KORDER,XLUMI)
	REAL*8 XCOEFF(*)
	REAL*4 RAD(*),PROF(*),VNUMB(*),DIFF(*),BACK(*)
	INTEGER*4 NSTART,NEND,KORDER
	CHARACTER BUFFER*80
10	FORMAT(A)
 
335	PRINT 301,XMIN,XMAX
301	FORMAT(' NOW, CALCULATING THE TOTAL INTENSITY : ',
     1	' ENTER THE LIMITS ',/,
     1	' (DEFAULT : XMIN = ',G12.5,' XMAX = ',G12.5)
	BUFFER=' '
	READ(5,10) BUFFER
	IF(BUFFER(1:2).NE.'  ')THEN
	   READ(BUFFER,*,ERR=335)XMIN,XMAX
	ENDIF
 
C Computing the boundaries :
	  CALL INDEX_MAX4(RAD,NEND,XMIN,NSTARTI)
	  CALL INDEX_MAX4(RAD,NEND,XMAX,NENDI)
 
	  PRINT 309,NSTARTI,NENDI,RAD(NSTARTI),RAD(NENDI)
309	  FORMAT(' LIMITING INDICES :',I5,2X,I5,/,
     1	' LIMITING RADII : ',G12.5,2X,G12.5)
 
C	  PRINT *,' DO YOU WANT TO COMPUTE THE LUMINOSITY ',
C     1	'WITH THESE VALUES ?(Y)'
C	  READ(5,10) ANS
C	  IF(ANS.EQ.'N'.OR.ANS.EQ.'n') GOTO 335
 
C If O.K. store the coefficients of the polynomial :
 
	WRITE(10,925) KORDER,(XCOEFF(K),K=1,KORDER+1)
925	FORMAT(/,' ORDER, COEFF.',T21,I2,1X,10(G12.5,1X))
 
C Compute the luminosities and store them in unit 10 :
 
	WRITE(10,915) RAD(NSTARTI),RAD(NENDI)
915	FORMAT(' LIMITING RADII',4X,G12.5,1X,G12.5)
 
	WORK1=RAD(NENDI)-RAD(NSTARTI)
	WORK2=NENDI-NSTARTI
C	PRINT 318,WORK1,WORK2
318	FORMAT(' DELTA R IN ARCSECONDS : ',G12.5,
     1	'  IN "INDICES" : ',G12.5)
	 IF(WORK2.NE.0.)THEN
	   WORK=WORK1/WORK2
C	   PRINT 319,WORK
319	   FORMAT(' MEAN SCALE IN ARCSEC/INDEX :',G12.5)
	   WRITE(10,339) WORK
339	   FORMAT(' SCALE "/INDEX',7X,G12.5)
	 ENDIF
 
C Computing the total luminosity:
	  XLUMI=0.D0
	  SUM2=0.D0
	  AREA=0.D0
	    DO I=NSTARTI,NENDI
	      XLUMI=XLUMI+DIFF(I)
	      SUM2=SUM2+DIFF(I)*VNUMB(I)
	      AREA=AREA+VNUMB(I)
	    END DO
	  PRINT 303,XLUMI,SUM2,AREA
303	  FORMAT(' LUMINOSITY OVER THE BACKGROUND: ',G12.5,/,
     1	' TOTAL LUMINOSITY (WITH THE ANGULAR EXTENT',
     1	' OF THE PROFILE) OVER THE BACKGROUND: ',/,
     1	2X,G12.5,/,' AREA (PIXELS): ',G12.5,/)
	  WRITE(10,917) XLUMI,SUM2,AREA
917	  FORMAT(' LUMINOSITIES, AREA',2X,3(G12.5,1X))
 
	RETURN
	END
C---------------------------------------------------------------------------
C Subroutine SHC_DISPLAY
C to display many curves on the same graph,
C (interactive possibility of entering points with a cursor)
C
C KCUR (=2*KMAX) Number of curves to be displayed on the same graph
C--------------------------------------------------------------------------
	SUBROUTINE SHC_DISPLAY(X1,Y1,IDIM1,KCUR1,NSTART1,NEND1,
     1	NCHAR1,X2,Y2,IDIM2,KCUR2,NSTART2,NEND2,NCHAR2,
     1	CHAR1,CHAR2,TITLE,PLOTDEV,FILENAME,COMMENTS)
 
	PARAMETER (IDIM=2000,KCUR=12)
	REAL*4 XPLOT(IDIM,KCUR),YPLOT(IDIM,KCUR)
	REAL*4 X1(IDIM1,*),X2(IDIM2,*),Y1(IDIM1,*),Y2(IDIM2,*)
	INTEGER*4 NPTS(KCUR)
	INTEGER*4 NSTART1(*),NEND1(*),NSTART2(*),NEND2(*)
	CHARACTER NCHAR1(*)*4,NCHAR2(*)*4,NCHAR3(KCUR)*4
	CHARACTER CHAR1*(*),CHAR2*(*),TITLE*(*),PLOTDEV*(*)
        CHARACTER FILENAME*(*),COMMENTS*(*)
 
C Copying the curves to enter NEWPLOT:
	DO K=1,KCUR1
	  NPTS(K)=NEND1(K)-NSTART1(K)+1
	  NCHAR3(K)=NCHAR1(K)
	  DO I=1,NPTS(K)
	    II=I+NSTART1(K)-1
	    XPLOT(I,K)=X1(II,K)
	    YPLOT(I,K)=Y1(II,K)
	  END DO
	END DO
 
	DO K=1,KCUR2
	  KK=K+KCUR1
	  NPTS(KK)=NEND2(K)-NSTART2(K)+1
	  NCHAR3(KK)=NCHAR2(K)
	  DO I=1,NPTS(KK)
	    II=I+NSTART2(K)-1
	    XPLOT(I,KK)=X2(II,K)
	    YPLOT(I,KK)=Y2(II,K)
	  END DO
	END DO
	KCURVE=KCUR1+KCUR2
 
	CALL NEWPLOT(XPLOT,YPLOT,NPTS,IDIM,KCURVE,CHAR1,CHAR2,
     1	TITLE,NCHAR3,PLOTDEV,FILENAME,COMMENTS)
 
 
	RETURN
	END
C---------------------------------------------------------------------------
c	include 'jlpsub:project.for'
C In POLYFIT.FOR there is CALPOLY and POLYFIT
	include 'jlpsub:polyfit.for'
