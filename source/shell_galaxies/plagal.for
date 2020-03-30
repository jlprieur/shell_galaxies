C++**************************************************************
C	PLAGAL
C
C Removes the galaxy in a CCD image
C Simple geometry : concentric ellipses with
C fixed or variable ellipticity and a fixed orientation.
C Removes a profile, a smoothed profile or an r**1/4 law
C
C IMAGE(NX,NY) declared as (IDIM,IDIM)
C
C JLP
C Version of 27/08/87
C--**************************************************************
	PROGRAM PLAGAL
	PARAMETER (IDIM=600,IDIM1=2000)
	REAL*4 INPUT(IDIM,IDIM),OUTPUT(IDIM,IDIM)
	REAL*8 VRAD,VPRO,WORK
	REAL*4 RAD2,AXRATIO2
	CHARACTER FICH3*40,NAMEIN*40,NAMEOUT*40,ANS*1,
     1	BUFFER*80,COMMENTS*80
	LOGICAL FIXED_ELLI
	COMMON/PLG1_PARAM/RADMIN,RADMAX,X0,Y0,SCALE,SKY,VPRO(IDIM1),
     1	VRAD(IDIM1),NPTS,RAD2(100),AXRATIO2(100),NPOINT2
	COMMON/FIXELLIP/UU,VV,WW,AXRATIO,CO,SI
 
10	FORMAT(A)
 
C Open 10 which is used in POLYFIT
	OPEN(10,FILE='plagal.tmp',STATUS='unknown')
	OPEN(9,FILE='plagal.log',STATUS='unknown',
     1	ACCESS='SEQUENTIAL')
	WRITE(9,81)
	PRINT 81
81	FORMAT(' PROGRAM PLAGAL - Version of 27/08/87-',/)
 
C Inquires about the format of the files :
	CALL JLP_INQUIFMT
 
C Input of the image :
	NAMEIN=' '
	CALL JLP_READIMAG(INPUT,NX,NY,IDIM,NAMEIN,COMMENTS)
	WRITE(9,82)NAMEIN
82	FORMAT(' INPUT IMAGE : ',A)
 
	PRINT *,' OUTPUT FILE ?'
	READ(5,10) NAMEOUT
 
	PRINT 89
89	FORMAT(' Program PLAGAL to remove the galaxy background',/,
     1	' Possibility of subtracting:',/,
     1	' 1: a profile smoothed in this program',/,
     1	' 2: a profile not smoothed',/,
     1	' 3: an r 1/4 law',/,
     1	' Enter the option: ',$)
	READ(5,*) IOP
	WRITE (9,89)
	WRITE (9,83) IOP
83	FORMAT(' OPTION = ',I2)
 
C Parameters of the ellipses :
	PRINT *,'Scale in arcseconds/pixel ?'
	READ(5,*) SCALE
	PRINT *,' Position angle of the major axis in degrees'
	READ(5,*) XPHI
    	CO=COS(XPHI*3.14159/180.)
	SI=SIN(XPHI*3.14159/180.)
	PRINT *,' Centre of the ellipses: X0,Y0 '
	READ(5,*) X0,Y0
	WRITE(9,84)SCALE,XPHI,X0,Y0
84	FORMAT(' Scale : ',F12.4,' Position angle :',F10.4,/,
     1	' CENTRE : OX,OY ',F10.3,2X,F10.3)
	
C---------------------------------------------------------------------
C Possibility of variable ellipticity
 
	PRINT 86
	WRITE(9,86)
86	FORMAT(' Do you want a fixed ellipticity? (Y) ',$)
	READ(5,10) ANS
	WRITE(9,10)ANS
	IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN
	  FIXED_ELLI=.TRUE.
	  PRINT *,' Axis ratio b/a :'
	  READ(5,*) AXRATIO
	  WRITE(9,70)AXRATIO
70	  FORMAT(' Axis ratio :',F12.3)
	ELSE
	  FIXED_ELLI=.FALSE.
	  PRINT 71
71	  FORMAT(' Enter upper equiv. radius in arcsec and',
     1	' axis ratio b/a :',/,
     1	' (To exit type 0.,0.)')
	  I=1
	  READ(5,*) RAD2(I),AXRATIO2(I)
94	    IF(RAD2(I).EQ.0..AND.AXRATIO2(I).EQ.0.) GOTO 95
	      I=I+1
	      READ(5,*) RAD2(I),AXRATIO2(I)
            GOTO 94
95	  NPOINT2=I-1
 
C Getting the first guess for AXRATIO : (Mean of the axis ratios)
	  AXRATIO=0.
	    DO I=1,NPOINT2
	    WRITE(9,72)I,RAD2(I),AXRATIO2(I)
72	    FORMAT(' Point number : ',I2,' Radius and axis ratio :',
     1	F9.2,1X,F8.4)
	    AXRATIO=AXRATIO2(I)+AXRATIO
	    END DO
	  AXRATIO=AXRATIO/NPOINT2
	ENDIF
 
C Computing the first guess for UU,VV,WW : (Passed through COMMON/FIXELLIP)
	UU=CO*CO+(SI/AXRATIO)**2
	VV=SI*SI+(CO/AXRATIO)**2
	WW=2.*SI*CO*(1.-1./(AXRATIO*AXRATIO))
C-----------------------------------------------------------------
 
	IF (IOP.EQ.1.OR.IOP.EQ.2) THEN
8	  PRINT *,' Input profile :'
	  READ(5,10) FICH3
	  OPEN(3,FILE=FICH3,STATUS='OLD',ERR=8)
	  PRINT *,' Is it a profile produced by "PROFILE1" ?(Y)'
	  READ(5,10) ANS
C If new format reads 32 lines of comments first:
	  IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN
	    DO I=1,32
	    READ(3,10)BUFFER
	   END DO
	  ENDIF
	  READ(3,*)NPTS
	  PRINT *,' NPTS =',NPTS
	    DO I=1,NPTS
	    READ(3,*) VRAD(I),VPRO(I),WORK
	    END DO
	  WRITE(9,73)FICH3,NPTS
73	  FORMAT(' Input profile :',A,' NPTS =',I6)
	CLOSE(3)
	ENDIF
 
C*************************************************************************
C Option 1 :
C*************************************************************************
	IF(IOP.EQ.1)THEN
 
101	  PRINT *,' Minimum and maximum radii for the fit ? (arcsec)'
	  READ(5,*) RADMIN,RADMAX
	  PRINT *,' Order of the polynomial ?'
	  READ(5,*) KORDER
	  PRINT *,' Sky level (approximative lower value',
     1	' for the fit) :'
	  READ(5,*) SKY
	
	  CALL PLAGAL1(INPUT,OUTPUT,NX,NY,IDIM,FIXED_ELLI,KORDER)
 
	ENDIF
 
C**************************************************************************
C Option 2 :
 
	IF(IOP.EQ.2)THEN
 
	RADMIN=VRAD(1)
	RADMAX=VRAD(NPTS-1)
 
	DO J=1,NY
	  Y1=FLOAT(J)-Y0
	 DO I=1,NX
	  X1=FLOAT(I)-X0
C F1: equivalent radius in arcsec of the ellipse containing the pixel (x,y)
	  CALL EQUIV_RADIUS(X1,Y1,F1,SCALE,
     1	RAD2,AXRATIO2,NPOINT2,FIXED_ELLI)
	    IF(F1.GT.RADMIN.AND.F1.LT.RADMAX
     1	.AND.INPUT(I,J).NE.0.)THEN
	      WORK=F1
	      CALL INDEX_MAX8(VRAD,NPTS,WORK,INDEX1)
	      WORK=(VPRO(INDEX1+1)-VPRO(INDEX1))/
     1	(VRAD(INDEX1+1)-VRAD(INDEX1))
	      VALUE=VPRO(INDEX1)+(F1-VRAD(INDEX1))*WORK
	      OUTPUT(I,J)=INPUT(I,J)-VALUE
	    ELSE
	      OUTPUT(I,J)=0.
	    ENDIF
	 END DO
	END DO
	ENDIF
 
C**************************************************************************
C Option 3 :
C R 1/4 law :
C**************************************************************************
 
	IF(IOP.EQ.3)THEN
 
C Entry of the parameters :
	  PRINT *,' Sky level:'
	  READ(5,*) SKY
 
	  PRINT *,' Zero point for the magnitudes:'
	  READ(5,*) CMAG0
	  PRINT 309
309	  FORMAT(' -2.5*LOG10(I-SKY)= Be - CMAG0 +',
     1	' 8.325 ((R/Re)**1/4 - 1)')
 
	  PRINT *,' Parameters for r 1/4 law:',
     1	' Be (mag), Re (arcsec)'
	  READ(5,*) VM0,REQUI
	  ALPHA=-3.33/(REQUI**0.25)
	  BETA=((VM0-CMAG0)/-2.5)+3.33
 
	  PRINT *,' Radmin (arcsec)?'
	  READ(5,*) RADMIN
 
C Main loop :
 
	DO J=1,NY
	  Y1=FLOAT(J)-Y0
	 DO I=1,NX
	  X1=FLOAT(I)-X0
C F1: equivalent radius in arcsec of the ellipse containing the pixel (x,y)
	  IF(INPUT(I,J).NE.0.)THEN
	   CALL EQUIV_RADIUS(X1,Y1,F1,SCALE,
     1	RAD2,AXRATIO2,NPOINT2,FIXED_ELLI)
	    IF(F1.GT.RADMIN)THEN
	      F2=SQRT(SQRT(F1))
	      F3=10**(ALPHA*F2+BETA)
	      OUTPUT(I,J)=INPUT(I,J)-F3-SKY
	    ELSE
	      OUTPUT(I,J)=0.
	    ENDIF
	  ELSE
	   OUTPUT(I,J)=0.
	  ENDIF
	 END DO
	END DO
 
	ENDIF
 
C**** Writing the output file with comments **************
	LNAMEIN=MIN(20,INDEX(NAMEIN,' '))
	LNAMEIN=MAX(1,LNAMEIN)
	LFICH3=MIN(20,INDEX(FICH3,' '))
	LFICH3=MAX(1,LFICH3)
	IF(IOP.EQ.1)
     1	WRITE(COMMENTS,303) NAMEIN(1:LNAMEIN),FICH3(1:LFICH3)
303	FORMAT(' PLAGAL/',A,' with a smoothed profile: ',A,'//')
	IF(IOP.EQ.2)
     1	WRITE(COMMENTS,304) NAMEIN(1:LNAMEIN),FICH3(1:LFICH3)
304	FORMAT(' PLAGAL/',A,' with a profile not smoothed: ',A,'//')
	IF(IOP.EQ.3)
     1	WRITE(COMMENTS,305) NAMEIN(1:LNAMEIN)
305	FORMAT(' PLAGAL/',A,' with an r 1/4 law//')
	CALL JLP_WRITEIMAG(OUTPUT,NX,NY,IDIM,NAMEOUT,COMMENTS)
 
	CLOSE(9)
	CLOSE(10)
	PRINT *,' Logfile in "plagal.log"'
	STOP
	END
C*************************************************************
	include 'jlpsub:plagal_set.for'
	include 'jlpsub:polyfit.for'
