C**********************************************************************
C PLAGAL_SET
C Set of subroutines to remove the galaxy from an image
C Used by PLAGAL and PLAGAL_BATCH
C JLP Version of 11/10/88
C
C Contains :
C            PLAGAL1, EQUIV_RADIUS
C
C*************************************************************
C Subroutine PLAGAL1
C Option 1 : Subtracts a smoothed profile
C
C Input :
C INPUT(IDIM,*)
C NX,NY : Size of the image
C All the common block /PLG1_PARAM/
C
C Output :
C OUTPUT(IDIM,*)
C
C*************************************************************
	SUBROUTINE PLAGAL1(INPUT,OUTPUT,NX,NY,IDIM,FIXED_ELLI,KORDER)
	PARAMETER (IDIM1=2000)
	REAL*4 INPUT(IDIM,*),OUTPUT(IDIM,*)
	REAL*8 VRAD,VPRO
	REAL*4 RAD2,AXRATIO2,RADMIN,RADMAX
	REAL*8 XCOEFF(20),SDEV(20),WORK
	REAL*8 VLOGPRO(IDIM1),VLOGRAD(IDIM1)
	LOGICAL FIXED_ELLI
	COMMON/PLG1_PARAM/RADMIN,RADMAX,X0,Y0,SCALE,SKY,VPRO(IDIM1),
	1	VRAD(IDIM1),NPTS,RAD2(100),AXRATIO2(100),NPOINT2
 
10	FORMAT(A)
C Determining the boundaries for the fit in arcseconds :
	WORK=RADMIN
	CALL INDEX_MAX8(VRAD,NPTS,WORK,IMIN)
	IMIN=MAX(IMIN,1)
	RADMIN=VRAD(IMIN)
 
	WORK=RADMAX
	CALL INDEX_MAX8(VRAD,NPTS,WORK,IMAX)
	RADMAX=VRAD(IMAX)
	PRINT *,' IMIN,IMAX :',IMIN,IMAX
 
C Transformation into Log/Log
	  DO II=IMIN,IMAX
	    I=II-IMIN+1
	    VLOGRAD(I)=LOG(VRAD(II))
	    WORK=VPRO(II)-SKY
	    IF(WORK.GT.0.)THEN
	      VLOGPRO(I)=LOG(WORK)
	    ELSE
	      PRINT *,' PB WITH THE',II,' POINT,   NEGATIVE VALUE.'
	      PRINT *,' I TAKE IT AS THE UPPER LIMIT'
	      PRINT *,' THE RADIUS OF THIS POINT IS',VRAD(II)
	      IMAX=II-1
	      GO TO 105
	    ENDIF
	  END DO
 
105	  NPOINTS=IMAX-IMIN+1
	    IF(NPOINTS.LE.2)THEN
	    PRINT *,' NPOINTS=',NPOINTS
	    PRINT *,' TRY AGAIN WITH OTHER VALUES FOR RADMIN AND RADMAX'
	    STOP
	    ENDIF
	  RADMIN=VRAD(IMIN)
	  RADMAX=VRAD(IMAX)
	  PRINT *,'NPOINTS=',NPOINTS,' RADMAX=',RADMAX
 
	  WRITE(9,74)RADMIN,RADMAX,KORDER,SKY
74	  FORMAT(' RADMIN, RADMAX :',F12.3,2X,F12.4,/,
	1	' ORDER OF THE POLYNOMIAL : ',I2,/,
	1	' SKY LEVEL (FOR THE FIT ONLY) :',F12.4)
 
C Fitting the profile in Log/Log
	  CALL POLYFIT(VLOGRAD,VLOGPRO,NPOINTS,KORDER,
	1	XCOEFF,SDEV,ERR)
 
C Output of the fit in "PLAGAL_LOGFIT.DAT"
	OPEN(4,FILE='PLAGAL_LOGFIT.DAT',STATUS='NEW')
	WRITE(4,*)NPOINTS
	 DO I=1,NPOINTS
	   CALL CALPOLY(VLOGRAD(I),Y,XCOEFF,KORDER)
	   WRITE(4,*)VLOGRAD(I),VLOGPRO(I),Y
	 END DO
	CLOSE(4)
 
C Working out the residuals
C Output in "PLAGAL_RESID.DAT"
	OPEN(4,FILE='PLAGAL_RESID.DAT',STATUS='NEW')
	WRITE(4,*)NPOINTS
	 DO I=1,NPOINTS
	   II=I+IMIN-1
	   CALL CALPOLY(VLOGRAD(I),Y,XCOEFF,KORDER)
	   RESID=VPRO(II)-(EXP(Y)+SKY)
	   WRITE(4,*)VRAD(II),RESID
	 END DO	
	CLOSE(4)
 
C------------------------------------------------------
C Main Loop :
 
	DO J=1,NY
	  Y1=FLOAT(J)-Y0
	 DO I=1,NX
	  X1=FLOAT(I)-X0
C F1: equivalent radius in arcsec of the ellipse containing the pixel (x,y)
	  CALL EQUIV_RADIUS(X1,Y1,F1,SCALE,
	1	RAD2,AXRATIO2,NPOINT2,FIXED_ELLI)
	    IF(F1.GT.RADMIN.AND.INPUT(I,J).NE.0.)THEN
	      X=LOG(F1)
	      CALL CALPOLY(X,Y,XCOEFF,KORDER)
	      F3=SKY+EXP(Y)
	      OUTPUT(I,J)=INPUT(I,J)-F3
	    ELSE
	      OUTPUT(I,J)=0.
	    ENDIF
	 END DO
	END DO
 
	RETURN
	END
 
C**********************************************************************
C Subroutine EQUIV_RADIUS
C Two possibilitities :
C FIXED_ELLI=.TRUE. Fixed ellipticity
C FIXED_ELLI=.FALSE. Variable ellipticity
C**********************************************************************
	SUBROUTINE EQUIV_RADIUS(X1,Y1,EQUIRAD,SCALE,
	1	RAD2,AXRATIO2,NPOINT2,FIXED_ELLI)
	REAL*4 RAD2(100),AXRATIO2(100)
	LOGICAL FIXED_ELLI
	COMMON/FIXELLIP/UU,VV,WW,AXRATIO,CO,SI
 
C F11 : Square of the semi-major axis (first guess)
	F11=UU*X1*X1+VV*Y1*Y1+WW*X1*Y1
	EQUIRAD=SCALE*SQRT(F11*AXRATIO)
 
C For a fixed ellipticity : return
	IF(FIXED_ELLI)RETURN
 
C------------------------------------------------------------
C Computing the equivalent radius in 4 iterations (4 is a mini number)
 
	DO KK=1,4
 
	CALL INDEX_MAX4(RAD2,NPOINT2,EQUIRAD,INDEX)
 
C--------------------------------------------------------------
C Computing the value of the axis ratio :
	IF(INDEX.EQ.NPOINT2)THEN
	 E2=AXRATIO2(NPOINT2)
	ELSE
	 R1=RAD2(INDEX)
	 R2=RAD2(INDEX+1)
	 EC1=AXRATIO2(INDEX)
	 EC2=AXRATIO2(INDEX+1)
	 E2=EC1+(EC2-EC1)*(EQUIRAD-R1)/(R2-R1)
	ENDIF
 
C-------------------------------------------------------------
C Computing the corresponding value for the equivalent radius
	U2=CO*CO+(SI/E2)**2
	V2=SI*SI+(CO/E2)**2
	W2=2.*SI*CO*(1.-1./(E2*E2))
	F11=U2*X1*X1+V2*Y1*Y1+W2*X1*Y1
	EQUIRAD=SCALE*SQRT(F11*E2)
 
	END DO
 
	RETURN
	END
