C++********************************************************************	
C Program SIMU_PLOT
C To plot shell distributions or orbits from Peter Quinn's programs
C
C JLP
C Version 02-02-88
C--********************************************************************	
	PROGRAM SIMU_PLOT
	PARAMETER (IDIM1=20000,IDIM=256,KCUR=2)
	REAL*4 XPLOT(2*IDIM1+1),YPLOT(2*IDIM1+1)
	REAL*4 COEFF(IDIM1)
	INTEGER NPTS(KCUR)
	CHARACTER NCHAR(KCUR)*4,PCOLOR(KCUR)*30
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,PLOTDEV*32
	CHARACTER ANS*1,NAME*40,BUFF*80
	LOGICAL GOOD_PLOT
 
10	FORMAT(A)
 
	PRINT 21
21	FORMAT(' PROGRAM SIMU_PLOT',/,
     1	' VERSION OF 07-04-88',/,
     1	' OPTIONS : ',/,'   1.POSITIONS OR VELOCITIES',/,
     1	'   2.ORBITS',/,'   3.COLOR OUTPUT',/,
     1	/,' ENTER THE OPTION YOU WANT : ')
	READ(5,*) IOPT
 
C Open the input file :
22	PRINT *,' INPUT FILE ?'
	READ(5,10) NAME
	OPEN(1,FILE=NAME,STATUS='OLD',ERR=22,FORM='UNFORMATTED')
 
	IF(IOPT.EQ.1.OR.IOPT.EQ.2)THEN
	  PRINT *,' OUTPUT DEVICE : FILE, TEKTRO, CIFER_T5,...'
	  READ(5,10) PLOTDEV
	ENDIF
 
C------------------------------------------------------
C Option 1: positions or velocities:
C------------------------------------------------------
	IF(IOPT.EQ.1)THEN
C Options :
	  PRINT *,' NUMBER OF THE FIRST AND LAST SET YOU WANT TO PLOT ?'
	  READ(5,*) ISET1,ISET2
	  PRINT 11
11	  FORMAT(' MENU :',/,
     1	' 1,1=RADIUS/VELOCITY_MODULE',/,
     1	' 2=X 3=Y 4=Z 5=VX 6=VY 7=VZ ',/,
     1	' 8,8=RADIUS/RADIAL_VELOCITY',/,
     1	' 9,0=HISTOGRAM VERSUS PROJECTED RADIUS (x**2+y**2)',/,
     1	' 9,9=HISTOGRAM VERSUS RADIUS (x**2+y**2+z**2)',/,
     1	' ENTER WHAT YOU WANT TO PLOT IN X AND Y :')
	  READ(5,*) IPLOTX,IPLOTY
 
C Read the number of particles :
	  READ(1)NPTS1
 
	  DO I=1,ISET2
	    GOOD_PLOT=(I.GE.ISET1)
 
	    NMAX=IDIM1
	    CALL GENE_PLOT1(NPTS1,NPTS2,IPLOTX,IPLOTY,XPLOT,YPLOT,
     1	NMAX,CHAR1,CHAR2,TITLE,GOOD_PLOT)
 
C Plot the curve :
	    IF(GOOD_PLOT)THEN
	      KCURVE=2
 
C First curve:
	      NPTS(1)=NPTS2
C Symbol 1=dot L=line
	      PCOLOR(1)='Default'
	      IF(IPLOTX.EQ.9)THEN
	        NCHAR(1)='L'
	      ELSE
	        NCHAR(1)='1'
	      ENDIF
 
C Draw the centre:
	      NPTS(2)=1
	      XPLOT(2*IDIM1+1)=0.
	      YPLOT(2*IDIM1+1)=0.
	      NCHAR(2)='45'
	      PCOLOR(2)='Default'
 
C Calling NEWPLOT
	      NMAX=IDIM1
	      CALL NEWPLOT(XPLOT,YPLOT,NPTS,NMAX,KCURVE,
     1	CHAR1,CHAR2,TITLE,NCHAR,PCOLOR,PLOTDEV,NAME,' ')
	    ENDIF
 
	  END DO
 
C------------------------------------------------------
C Option 2: Drawing orbits
C------------------------------------------------------
	ELSEIF(IOPT.EQ.2)THEN
	
	  PRINT *,' REMEMBER : 2=X 3=Y 4=Z'
	  PRINT *,' ENTER WHAT YOU WANT TO PLOT IN X AND Y :'
	  READ(5,*) IPLOTX,IPLOTY
 
	  NMAX=IDIM1
	  CALL GENE_PLOT2(NPTS1,IPLOTX,IPLOTY,XPLOT,YPLOT,
     1	NMAX,CHAR1,CHAR2,TITLE)
C Plot the curve :
	      KCURVE=2
 
C First curve:
	      NPTS(1)=NPTS1
	      NCHAR(1)='L'
              PCOLOR(1)='Default'
 
C Draw the centre:
	      NPTS(2)=1
	      XPLOT(2*IDIM1+1)=0.
	      YPLOT(2*IDIM1+1)=0.
	      NCHAR(2)='45'
              PCOLOR(2)='Default'
 
C Calling NEWPLOT
	  NMAX=IDIM1
	  CALL NEWPLOT(XPLOT,YPLOT,NPTS,NMAX,KCURVE,
     1	CHAR1,CHAR2,TITLE,NCHAR,PCOLOR,PLOTDEV,NAME,' ')
 
C------------------------------------------------------
C Option 3: Color plot
C------------------------------------------------------
	ELSEIF(IOPT.EQ.3)THEN
	
C Read the number of particles :
	  READ(1)NPTS1
 
	  PRINT *,' REMEMBER : 2=X 3=Y 4=Z'
	  PRINT *,' ENTER WHAT YOU WANT TO PLOT IN X AND Y :'
	  READ(5,*) IPLOTX,IPLOTY
 
	  PRINT *,' XSTART,XEND,YSTART,YEND :'
	  READ(5,*) XSTART,XEND,YSTART,YEND
	
	  PRINT *,' NUMBER OF THE FIRST AND LAST SETS YOU WANT TO PLOT ?'
	  READ(5,*) ISET1,ISET2
 
	  PRINT *,' LIMITING RADIUS FOR THE COLOURS ?'
	  READ(5,*) RAD1
	  RAD12=RAD1*RAD1
 
C Inquire the format of the image:
	  CALL JLP_INQUIFMT
 
	  DO I=1,ISET2
	    GOOD_PLOT=(I.GE.ISET1.AND.I.LE.ISET2)
 
	    CALL GENE_PLOT3(COEFF,NPTS1,IPLOTX,IPLOTY,
     1	XSTART,XEND,YSTART,YEND,RAD12,GOOD_PLOT)
 
	  END DO
 
	ENDIF
 
	CLOSE(1)
	END
C********************************************************************	
C Subroutine GENE_PLOT1
C To generate plot arrays from position/velocity files
C
C Structure of the file :
C	 npts1
C	 iteration       time
C        particle number 1    x     y     z     vx    vy    vz
C        particle number 2    x     y     z	vx    vy    vz
C        particle number 3    x     y     z	vx    vy    vz
C	 .....................................................
C
C Input :
C
C NPTS1          : Number of data points to read
C NPTS2          : Number of output points to plot
C IPLOTX, IPLOTY : Columns to be plotted
C NMAX           : First dimension of the arrays XPLOT, YPLOT
C GOOD_PLOT      : True if the values have to be plotted
C
C Output :
C
C XPLOT,YPLOT    : Arrays to be plotted
C TITLE          : Title of the graph
C CHAR1, CHAR2   : Labels of the axes
C
C Nota : UNIT_VEL=976.6 (Cf Quinn's programs)
C********************************************************************	
	SUBROUTINE GENE_PLOT1(NPTS1,NPTS2,IPLOTX,IPLOTY,
     1	XPLOT,YPLOT,NMAX,CHAR1,CHAR2,TITLE,
     1	GOOD_PLOT)
	PARAMETER (UNIT_VEL=976.6)
	REAL*4 INPUT(7)
	REAL*4 XPLOT(NMAX,*),YPLOT(NMAX,*)
	LOGICAL GOOD_PLOT
	CHARACTER TITLE*(*),CHAR1*(*),CHAR2*(*),LABELS(7)*8
	CHARACTER FILENAME*40
	DATA LABELS/'  ',' x (kpc)',' y (kpc)',' z (kpc)',
     1	'Vx (km/s)','Vy (km/s)','Vz (km/s)'/
 
C Read the header of the present step (TIME is in 1.E+06 units) :	
	 READ(1)ITERATION,TIME
	 TIME=TIME*1.E+06
	 PRINT 23,NPTS1,ITERATION,TIME
	 WRITE(TITLE,23) NPTS1,ITERATION,TIME
23	 FORMAT('NPTS:',I5,' ITER:',I5,' TIME:',1PE9.3,'yrs')
 
C Read the values and return if not a good plot :
	IF(.NOT.GOOD_PLOT)THEN
	    DO I=1,NPTS1
	      READ(1) (INPUT(K),K=1,7)
	    END DO
	  RETURN
	ENDIF
 
C Read the values :
	IF(IPLOTX.GE.2.AND.IPLOTX.LE.7
     1	.AND.IPLOTY.GE.2.AND.IPLOTY.LE.7)THEN
	    DO I=1,NPTS1
	      READ(1) (INPUT(K),K=1,7)
	      XPLOT(I,1)=INPUT(IPLOTX)
	      YPLOT(I,1)=INPUT(IPLOTY)
	    END DO
	  NPTS2=NPTS1
	  CHAR1=LABELS(IPLOTX)
	  CHAR2=LABELS(IPLOTY)
C Normalization of the velocities
	   IF(IPLOTX.GE.5)THEN
	    DO I=1,NPTS1
	      XPLOT(I,1)=XPLOT(I,1)*UNIT_VEL
	    END DO
	   ENDIF
	   IF(IPLOTY.GE.5)THEN
	    DO I=1,NPTS1
	      XPLOT(I,1)=XPLOT(I,1)*UNIT_VEL
	    END DO
	   ENDIF
 
	ELSEIF(IPLOTX.EQ.1)THEN
C Velocity module versus radius
	  DO I=1,NPTS1
	    READ(1) (INPUT(K),K=1,7)
	    XPLOT(I,1)=SQRT(1.E-15+INPUT(2)*INPUT(2)
     1	+INPUT(3)*INPUT(3)+INPUT(4)*INPUT(4))
	    YPLOT(I,1)=UNIT_VEL*SQRT(1.E-15+INPUT(5)*INPUT(5)
     1	+INPUT(6)*INPUT(6)+INPUT(7)*INPUT(7))
	  END DO
	  NPTS2=NPTS1
	  CHAR1=' Radius (Kpc)'
	  CHAR2='Velocity mod. (km/s)'
 
C Radial velocity versus radius
	ELSEIF(IPLOTX.EQ.8)THEN
	  DO I=1,NPTS1
	    READ(1) (INPUT(K),K=1,7)
	    XPLOT(I,1)=SQRT(1.E-15+INPUT(2)*INPUT(2)
     1	+INPUT(3)*INPUT(3)+INPUT(4)*INPUT(4))
	    YPLOT(I,1)=UNIT_VEL*(INPUT(2)*INPUT(5)
     1	+INPUT(3)*INPUT(6)+INPUT(4)*INPUT(7))
     1	/XPLOT(I,1)
	  END DO
	  NPTS2=NPTS1
	  CHAR1=' Radius (Kpc)'
	  CHAR2='Rad. velocity (km/s)'
 
	ELSE
C Histogram versus radius
	  PRINT *,' MIN, MAX AND NUMBER OF BINS :'
	  READ(5,*) VALMIN,VALMAX,NBINS
	  VAL_DELTA=(VALMAX-VALMIN)/FLOAT(NBINS)
	  NPTS2=2*NBINS
 
C Two possibilities (full radius or projected radius)
	  IF(IPLOTY.EQ.0)THEN
	    COEF1=0.
	    CHAR1='Proj. radius (Kpc)'
	    CHAR2=' N'
	  ELSE
	    COEF1=1.
	    CHAR1=' Radius (Kpc)'
	    CHAR2=' N'
	  ENDIF
 
C Initializing YPLOT  :
	    DO I=1,NBINS
	      YPLOT(I*2-1,1)=0.
	      YPLOT(I*2,1)=0.
	    END DO
	
C Reading the data:
	  DO I=1,NPTS1
	    READ(1) (INPUT(K),K=1,7)
	    RADIUS=SQRT(1.E-15+INPUT(2)*INPUT(2)
     1	+INPUT(3)*INPUT(3)+COEF1*INPUT(4)*INPUT(4))
	    IGOOD=NINT((RADIUS-VALMIN)/VAL_DELTA)
	    IF(IGOOD.GE.1.AND.IGOOD.LE.NBINS)THEN
	      YPLOT(IGOOD*2,1)=YPLOT(IGOOD*2,1)+1.
	      YPLOT(IGOOD*2-1,1)=YPLOT(IGOOD*2-1,1)+1.
	    ENDIF
	  END DO
 
C Storing the output in a file, and loading XPLOT :
	  IFMT=INT(ALOG10(FLOAT(ITERATION)))+1
	  WRITE(FILENAME,29)ITERATION
29	  FORMAT('histo_simu_',I<IFMT>,'.dat')
	  OPEN(3,FILE=FILENAME,STATUS='unknown',
     1	ACCESS='SEQUENTIAL')
	  WRITE(3,*) NBINS,ITERATION,TIME
	   DO I=1,NBINS
	      VAL_CENTRE=VALMIN+FLOAT(I)*VAL_DELTA
	      XPLOT(I*2-1,1)=VAL_CENTRE-VAL_DELTA/2.
	      XPLOT(I*2,1)=VAL_CENTRE+VAL_DELTA/2.
	      WRITE(3,*) VAL_CENTRE,YPLOT(2*I,1)
	   END DO
	  CLOSE(3)
 
	ENDIF
 
	RETURN
	END
C********************************************************************	
C Subroutine GENE_PLOT2
C To generate plot orbits from special files
C
C Structure of the file :
C        number of the step 1    x     y     z
C        number of the step 2    x     y     z
C        number of the step 3    x     y     z
C	 .....................................
C
C Input :
C
C IPLOTX, IPLOTY : Columns to be plotted
C NMAX           : First dimension of the arrays XPLOT, YPLOT
C
C Output :
C
C NPTS1          : Number of points to be plotted
C XPLOT,YPLOT    : Arrays to be plotted
C TITLE          : Title of the graph
C CHAR1, CHAR2   : Labels of the axes
C********************************************************************	
	SUBROUTINE GENE_PLOT2(NPTS1,IPLOTX,IPLOTY,
     1	XPLOT,YPLOT,NMAX,CHAR1,CHAR2,TITLE)
	REAL*4 INPUT(4)
	REAL*4 XPLOT(*),YPLOT(*)
	CHARACTER TITLE*(*),CHAR1*(*),CHAR2*(*),LABELS(7)*8
	DATA LABELS/'  ',' x (kpc)',' y (kpc)',' z (kpc)',
     1	'Vx (km/s)','Vy (km/s)','Vz (km/s)'/
 
	TITLE='ORBIT OF THE CENTRE OF THE COMPANION'
 
C Read the values:
	 DO I=1,NMAX
	   READ(1,END=999) (INPUT(K),K=1,4)
	   XPLOT(I)=INPUT(IPLOTX)
	   YPLOT(I)=INPUT(IPLOTY)
	 END DO
	PRINT *,' ERROR in GENE_PLOT2: more than',NMAX,' values'
	PRINT *,' I draw only the  first',NMAX,' points'
 
999	CLOSE(1)
	NPTS1=I-1
	PRINT *,NPTS1,' POINTS RECEIVED'
	CHAR1=LABELS(IPLOTX)
	CHAR2=LABELS(IPLOTY)
 
	RETURN
	END
C********************************************************************	
C Subroutine GENE_PLOT3
C To generate plot arrays from position/velocity files
C
C Structure of the file :
C	 npts1
C	 iteration       time
C        particle number 1    x     y     z     vx    vy    vz
C        particle number 2    x     y     z	vx    vy    vz
C        particle number 3    x     y     z	vx    vy    vz
C	 .....................................................
C
C Input :
C NPTS1          : Number of data points to read
C IPLOTX, IPLOTY : Columns to be plotted
C GOOD_PLOT      : True if the values have to be plotted
C
C********************************************************************	
	SUBROUTINE GENE_PLOT3(COEFF,NPTS1,IPLOTX,IPLOTY,
     1	XSTART,XEND,YSTART,YEND,RAD12,GOOD_PLOT)
	PARAMETER (IDIM=256,NX=256,NY=256)
	REAL*4 IMAGE(IDIM,IDIM),COEFF(*)
	REAL*4 INPUT(7)
	LOGICAL GOOD_PLOT
	CHARACTER FILENAME*40,COMMENTS*80,LABELS(7)*8
	DATA LABELS/'  ',' x (kpc)',' y (kpc)',' z (kpc)',
     1	'Vx (km/s)','Vy (km/s)','Vz (km/s)'/
 
C Read the header of the present step (TIME is in 1.E+06 units) :	
	 READ(1)ITERATION,TIME
	 TIME=TIME*1.E+06
	 PRINT 23,NPTS1,ITERATION,TIME,
     1	LABELS(IPLOTX),LABELS(IPLOTY)
	 WRITE(COMMENTS,23)NPTS1,ITERATION,TIME,
     1	LABELS(IPLOTX),LABELS(IPLOTY)
23	 FORMAT('NPTS:',I5,' ITER:',I5,' TIME:',1PE9.3,'yrs',
     1	A,A)
 
C If ITERATION=-1 fills the coefficient array :
	IF(ITERATION.EQ.1)THEN
	    DO IK=1,NPTS1
	      READ(1) (INPUT(K),K=1,7)
	      RADIUS=INPUT(2)*INPUT(2)+INPUT(3)*INPUT(3)+
     1	INPUT(4)*INPUT(4)
 
C Red if inside of RAD1, blue outside:
	     IF(RADIUS.LT.RAD12)THEN
	       COEFF(IK)=1.
	     ELSE
	       COEFF(IK)=-1.1
	     ENDIF
 
	    END DO
 
	    RETURN
	ENDIF
 
C Read the values and return if not a good plot :
	IF(.NOT.GOOD_PLOT)THEN
	    DO IK=1,NPTS1
	      READ(1) (INPUT(K),K=1,7)
	    END DO
	  RETURN
	ENDIF
 
C Set the background of the image to 0.
	DO J=1,NY
	  DO I=1,NX
	    IMAGE(I,J)=0.
	  END DO
	END DO
 
C Computes the bin width :
	XSTEP=(XEND-XSTART)/FLOAT(NX-1)
	YSTEP=(YEND-YSTART)/FLOAT(NY-1)
 
C Read the values :
	    DO IK=1,NPTS1
	      READ(1) (INPUT(K),K=1,7)
	      X=INPUT(IPLOTX)
	      Y=INPUT(IPLOTY)
	      I=NINT((X-XSTART)/XSTEP)+1
	      J=NINT((Y-YSTART)/YSTEP)+1
 
C Incrementing value at location I,J if it is inside of the window:
	      IF((I.GE.1.AND.I.LE.NX).AND.
     1	(J.GE.1.AND.J.LE.NY))THEN
	        IMAGE(I,J)=IMAGE(I,J)+COEFF(IK)
	      ENDIF
 
	    END DO
 
C Storing the output in a file :
	  IF(ITERATION.NE.0)THEN
	    IFMT=INT(ALOG10(FLOAT(ITERATION)))+1
	    WRITE(FILENAME,29)ITERATION
29	    FORMAT('SIMU_',I<IFMT>,'.BDF')
	  ELSE
	    FILENAME='SIMU_0.BDF'
	  ENDIF
	  CALL JLP_WRITEIMAG(IMAGE,NX,NY,IDIM,FILENAME,COMMENTS)
 
	RETURN
	END
