C++***************************************************************
C Program ARCSMOOTH
C Smoothing, gradient, laplacian along arcs within angular sectors
C
C JLP Version 30-08-93
C--***************************************************************
	PROGRAM ARCSMOOTH
	PARAMETER (IDIM=600,IDIM1=360,IDIM2=1000)
	REAL*4 IMAGE1(IDIM,IDIM),IMAGE2(IDIM,IDIM)
	REAL*4 PROF1(IDIM2,IDIM1),PROF2(IDIM2,IDIM1)
	REAL*4 PROF(IDIM2,IDIM1),LIMITS(IDIM1)
        REAL*4 PI
	CHARACTER IN_NAME*40,IN_COMMENTS*80
	CHARACTER OUT_NAME*40,OUT_COMMENTS*80
	COMMON /CONSTANT/PI
	COMMON /ACS_PARAM/NPL,NL,NPL2,NL2,X0,Y0,XINC,XRATIO,DTHETA
	PI=3.14159265358979323846
 
10	FORMAT(A)
 
	WRITE(6,867)
867	FORMAT(' PROGRAMME ARCSMOOTH : Version 30-08-93',/)

C**** Input of the file **********************
        CALL JLP_BEGIN
	CALL JLP_INQUIFMT
        WRITE(6,*) 'Input file: '
        READ(5,10) IN_NAME
	CALL JLP_READIMAG(IMAGE1,NPL,NL,IDIM,IN_NAME,IN_COMMENTS)
 
C Input of parameters:
        PRINT *,' COORDINATES OF THE CENTER OF THE GALAXY : X0,Y0 '
        READ(5,*) X0,Y0
        PRINT *,' Starting increment and ratio between two successive increments '
        PRINT *,' in radius for the profile (pixel units, real) '
        READ(5,*) XINC,XRATIO
        PRINT *,' Number of sectors (integer) <360'
        READ(5,*) NL2
        DTHETA=360./FLOAT(NL2)
        WRITE(6,34) X0,Y0,XINC,NL2,DTHETA
34      FORMAT('Profiles1/ Xcent =',F10.3,' Ycent =',F10.3,
     1         ' Delta R =',F10.3,/,' Number of sectors =',I4,' Delta Theta =',F10.4)


C Computing NPL2 with the four corners of the image
        X1=1.-X0
        Y1=1.-Y0
        RAD1=SQRT(X1*X1+Y1*Y1)

        X2=1.-X0
        Y2=FLOAT(NL)-Y0
        RAD2=SQRT(X2*X2+Y2*Y2)

        X3=FLOAT(NPL)-X0
        Y3=1.-Y0
        RAD3=SQRT(X3*X3+Y3*Y3)

        X4=FLOAT(NPL)-X0
        Y4=FLOAT(NL)-Y0
        RAD4=SQRT(X4*X4+Y4*Y4)

        RADMAX=AMAX1(RAD1,RAD2,RAD4)
        NPL2=1+INT(RADMAX/XINC)

C Computing LIMITS, i.e. the limits in radius for each annulus:
        CALL COMPUTE_IND(LIMITS)

C**** Generation of the profile ******************
	CALL PROFILE(PROF1,IMAGE1,LIMITS)
	PRINT *,'NPL2,NL2',NPL2,NL2
 
C**** "Smoothing" of the profiles separately
	CALL SMOOTHPRO1(PROF1,PROF2)
 
C**** "Smoothing" of the profiles along circular annuli
	CALL SMOOTHPRO2(PROF2,PROF)
 
800	PRINT 801
801	FORMAT(' MENU :',/,' 1: OUTPUT OF THE PROFILE',/,
     1	' 2: SMOOTHED IMAGE',/,
     1	' 3: GRADIENT IMAGE',/,
     1	' 4: LAPLACIAN IMAGE',/,
     1	' 10: EXIT',/,
     1	' Enter the option you want : ',$)
	READ(5,*) IOPT
 
C Output of all the profiles in an "image"
	IF(IOPT.EQ.1)THEN
C**** Output of the profiles ******************
	PRINT *,' OUTPUT PROFILE:'
        READ(5,10) OUT_NAME
        WRITE(OUT_COMMENTS,35) IN_NAME
35      FORMAT('Concentric profiles from',A) 
	CALL JLP_WRITEIMAG(PROF,NPL2,NL2,IDIM2,OUT_NAME,OUT_COMMENTS)
	GO TO 800
	ENDIF
 
C Output of a smoothed image
	IF(IOPT.EQ.2)THEN

C**** Generation of a synthetic image **************
	CALL SYNTHESIS(PROF,IMAGE1,IMAGE2,LIMITS)
 
C**** Output of the file ******************
	PRINT *,' OUTPUT SMOOTHED IMAGE:'
        READ(5,10) OUT_NAME
        WRITE(OUT_COMMENTS,36) IN_NAME
36      FORMAT('Arcsmooth from',A) 
	CALL JLP_WRITEIMAG(IMAGE2,NPL,NL,IDIM,OUT_NAME,OUT_COMMENTS)
 
	GO TO 800
	ENDIF
 
C Output of a gradient image
	IF(IOPT.EQ.3)THEN
C**** "Slope" of the profiles *****************
	CALL SLOPE(PROF,PROF1)
 
C**** Generation of a synthetic image **************
	CALL SYNTHESIS(PROF,IMAGE1,IMAGE2,LIMITS)
 
C**** Output of the file ******************
	PRINT *,' OUTPUT GRADIENT IMAGE:'
        READ(5,10) OUT_NAME
        WRITE(OUT_COMMENTS,37) IN_NAME
37      FORMAT('Radial gradient from',A) 
	CALL JLP_WRITEIMAG(IMAGE2,NPL,NL,IDIM,OUT_NAME,OUT_COMMENTS)
 
	GO TO 800
	ENDIF
 
C Output of a laplacian image
	IF(IOPT.EQ.4)THEN
C**** "Laplacian" of the profiles *****************
	CALL SLOPE(PROF,PROF1)
	CALL SLOPE(PROF1,PROF2)
 
C**** Generation of a synthetic image **************
	CALL SYNTHESIS(PROF,IMAGE1,IMAGE2,LIMITS)
 
C**** Output of the file ******************
	PRINT *,' OUTPUT LAPLACIAN IMAGE:'
        READ(5,10) OUT_NAME
        WRITE(OUT_COMMENTS,38) IN_NAME
38      FORMAT('Radial laplacian from',A) 
	CALL JLP_WRITEIMAG(IMAGE2,NPL,NL,IDIM,OUT_NAME,OUT_COMMENTS)
 
	GO TO 800
	ENDIF
 
        CALL JLP_END
	STOP
	END
C**************************************************************
C Subroutine PROFILE
C Generates an array PROF of the profile
C***************************************************************
	SUBROUTINE PROFILE(PROF,IMAGE1,LIMITS)
	PARAMETER (IDIM=600,IDIM1=360,IDIM2=1000)
	REAL*4 IMAGE1(IDIM,*),PROF(IDIM2,*),LIMITS(*)
	INTEGER NBER(IDIM2,IDIM1)
	COMMON /ACS_PARAM/NPL,NL,NPL2,NL2,X0,Y0,XINC,XRATIO,DTHETA
 
C Main loop
	DO J=1,NL
	  Y1=FLOAT(J)-Y0
	    DO I=1,NPL
	      X1=FLOAT(I)-X0
	      CALL POLARD(X1,Y1,ANGL,RAD,IPRO,LIMITS)
	      JPRO=1+INT(ANGL/DTHETA)
	      PROF(IPRO,JPRO)=PROF(IPRO,JPRO)+IMAGE1(I,J)
	      NBER(IPRO,JPRO)=NBER(IPRO,JPRO)+1
	    END DO
	END DO
 
C Computing the mean
	DO IPRO=1,NPL2
	  DO JPRO=1,NL2
	    NBR=NBER(IPRO,JPRO)
	      IF(NBR.NE.0)THEN
	        PROF(IPRO,JPRO)=PROF(IPRO,JPRO)/NBR
	      ENDIF
	  END DO
	END DO
 
	RETURN
	END
C***********************************************************************
C Subroutine POLARD
C Input:
C  X, Y: coordinates relative to center
C
C Output:
C ANGL: polar angle in degrees between 0. and 360.)
C INDEX: index for profile (with increasing steps...) 
C***********************************************************************
	SUBROUTINE POLARD(X,Y,ANGL,RADIUS,INDEX,LIMITS)
        REAL*4 LIMITS(*)
	REAL*4 X,Y,ANGL,RADIUS,COSINUS,PI
        INTEGER*4 INDEX
	COMMON /CONSTANT/PI
	COMMON /ACS_PARAM/NPL,NL,NPL2,NL2,X0,Y0,XINC,XRATIO,DTHETA

	RADIUS=SQRT(X*X+Y*Y)
	IF(RADIUS.EQ.0.) RADIUS=0.00001
	    COSINUS=X/RADIUS
	    ANGL=ACOS(COSINUS)
	    ANGL=ANGL*180./PI
	IF(Y.LT.0.) ANGL=360.-ANGL

C Now index:
        DO I=1,NPL2
          IF(LIMITS(I).GT.RADIUS)THEN
           INDEX = I-1 
           GOTO 99
          ENDIF
        END DO

99	RETURN
	END
C**************************************************************
C Subroutine SYNTHESIS to synthesize an image from PROF
C**************************************************************
	SUBROUTINE SYNTHESIS(PROF,IMAGE1,IMAGE2,LIMITS)
	PARAMETER (IDIM=600,IDIM1=360,IDIM2=1000)
	REAL*4 IMAGE1(IDIM,*),IMAGE2(IDIM,*),LIMITS(*),PROF(IDIM2,IDIM1)
        REAL*4 W1,W2,VAL1,VAL2
	COMMON /ACS_PARAM/NPL,NL,NPL2,NL2,X0,Y0,XINC,XRATIO,DTHETA
 
C Main loop
	DO J=1,NL
	  Y1=FLOAT(J)-Y0
	    DO I=1,NPL
	      X1=FLOAT(I)-X0
	      CALL POLARD(X1,Y1,ANGL,RAD,IPRO,LIMITS)
C Leaving the central pixels unsmoothed:
              IF(RAD.LT.8.)THEN
	        IMAGE2(I,J)=IMAGE1(I,J)
              ELSE
	        JPRO=1+INT(ANGL/DTHETA)
C Interpolation between the two values in radius:
                W1 = (RAD-LIMITS(IPRO))/(LIMITS(IPRO+1)-LIMITS(IPRO))
	        VAL1= PROF(IPRO,JPRO)+W1*(PROF(IPRO+1,JPRO)-PROF(IPRO,JPRO))
C Same quantity for next sector:
                JPRO1=JPRO+1
                IF(JPRO.EQ.NL2)JPRO1=1
	        VAL2= PROF(IPRO,JPRO1)+W1*(PROF(IPRO+1,JPRO1)-PROF(IPRO,JPRO1))
C Now interpolation between the sectors:
                W2 = (ANGL-DTHETA*FLOAT(JPRO-1))/DTHETA
                IF(W2.LT.0)W2=-W2
	        IMAGE2(I,J)= VAL1+W2*(VAL2-VAL1)
              ENDIF
             
	    END DO
	END DO
 

	RETURN
	END
C--------------------------------------------------------------------------
C**** "Smoothing" of the profiles separately
C--------------------------------------------------------------------------
	SUBROUTINE SMOOTHPRO1(PROF1,PROF2)
	PARAMETER (IDIM1=360,IDIM2=1000)
	REAL*4 PROF1(IDIM2,IDIM1),PROF2(IDIM2,IDIM1)
 
	COMMON /ACS_PARAM/NPL,NL,NPL2,NL2,X0,Y0,XINC,XRATIO,DTHETA
 
	DO J=1,NL2
	  DO I=2,NPL2-1
	    IF(PROF1(I-1,J).NE.0.)THEN
	      PROF2(I,J)=0.25*(PROF1(I-1,J)+PROF1(I+1,J))+
     1	0.5*PROF1(I+1,J)
	    ELSE
	      PROF2(I,J)=PROF1(I,J)
	    ENDIF
	  END DO
	END DO
 
C And then the edges :
	DO J=1,NL2
	  PROF2(1,J)=PROF2(2,J)
	  PROF2(NPL2,J)=PROF2(NPL2,J)
	END DO
 
	RETURN
	END
C-----------------------------------------------------------------------------
C**** "Smoothing" of the profiles along cicular annuli
	SUBROUTINE SMOOTHPRO2(PROF1,PROF2)
	PARAMETER (IDIM1=360,IDIM2=1000)
	REAL*4 PROF1(IDIM2,IDIM1),PROF2(IDIM2,IDIM1)
 
	COMMON /ACS_PARAM/NPL,NL,NPL2,NL2,X0,Y0,XINC,XRATIO,DTHETA
	
	DO I=1,NPL2
	  DO J=2,NL2-1
	    IF(PROF1(I,J-1).NE.0.)THEN
	      PROF2(I,J)=0.25*(PROF1(I,J-1)+PROF1(I,J+1))+
     1	0.5*PROF1(I,J)
	    ELSE
	      PROF2(I,J)=PROF1(I,J)
	    ENDIF
	  END DO
	END DO
 
C And then the edges :
	DO I=1,NPL2
	  PROF2(I,1)=0.25*(PROF1(I,2)+PROF1(I,NL2))+
     1	0.5*PROF1(I,1)
 
	  PROF2(I,NL2)=0.25*(PROF1(I,NL2-1)+PROF1(I,1))+
     1	0.5*PROF1(I,NL2)
	END DO
 
	RETURN
	END
C-----------------------------------------------------------------------------
C**** Gradients of the profiles *****************
	SUBROUTINE SLOPE(PROF1,PROF2)
	PARAMETER (IDIM1=360,IDIM2=1000)
	REAL*4 PROF1(IDIM2,IDIM1),PROF2(IDIM2,IDIM1)
 
	COMMON /ACS_PARAM/NPL,NL,NPL2,NL2,X0,Y0,XINC,XRATIO,DTHETA
	
	DO J=1,NL2
	  DO I=2,NPL2-1
	    IF(PROF1(I-1,J).NE.0.)THEN
	      PROF2(I,J)=PROF1(I-1,J)-PROF1(I+1,J)
	    ELSE
	      PROF2(I,J)=0.
	    ENDIF
	  END DO
	END DO
 
C And then the edges :
	DO J=1,NL2
	  PROF2(1,J)=PROF2(2,J)
	  PROF2(NPL2,J)=PROF2(NPL2,J)
	END DO
 
	RETURN
	END
C***************************************************************** 
        SUBROUTINE COMPUTE_IND(LIMITS)
        REAL*4 LIMITS(*),XINC1
        INTEGER*4 I,NPL2
	COMMON /ACS_PARAM/NPL,NL,NPL2,NL2,X0,Y0,XINC,XRATIO,DTHETA

        XINC1 = XINC
        LIMITS(1)=0.
        LIMITS(2)=XINC1
        DO I=3,NPL2
          XINC1 = XINC1 * XRATIO
          LIMITS(I) = LIMITS(I-1) + XINC1
        END DO
        RETURN
        END
