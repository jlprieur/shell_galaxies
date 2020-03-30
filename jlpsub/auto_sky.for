C++---------------------------------------------------------------------
C Set of routines to determine automatic center of galaxy
C and automatic sky level for Fitelli and other programs
C Contains: AUTO_CENTER and AUTO_SKY
C------------------------------------------------------------------------
C  Subroutine AUTO_CENTER :
C Assumes that the center of the galaxy is close to the center of the image
C
C------------------------------------------------------------------------
	SUBROUTINE AUTO_CENTER(IMAGE,NX,NY,IDIM,OX,OY)

	REAL*4 IMAGE(IDIM,*),OX,OY,VALMAX,SUMX,SUMY,SUMI
	INTEGER*4 IXMIN,IXMAX,IYMIN,IYMAX,IOX,IOY
 
C First determines working window:
        IXMIN=NX/4
        IXMAX=3*NX/4
        IYMIN=NY/4
        IYMAX=3*NY/4

C Scan window to look for maximum:
        IOX=IXMIN
        IOY=IYMIN
        VALMAX=IMAGE(IOX,IOY)
	DO IY=IYMIN,IYMAX
	 DO IX=IXMIN,IXMAX
           IF(IMAGE(IX,IY).GT.VALMAX)THEN
             VALMAX=IMAGE(IX,IY)
             IOX=IX
             IOY=IY
           ENDIF
	 END DO
	END DO

C More acurate determination with barycenter:
        SUMX=0. 
        SUMY=0. 
        SUMI=0.
        DO IY=IOY-1,IOY+1
         DO IX=IOX-1,IOX+1
            SUMX = SUMX + FLOAT(IX)*IMAGE(IX,IY)
            SUMY = SUMY + FLOAT(IY)*IMAGE(IX,IY)
            SUMI = SUMI + IMAGE(IX,IY)
	 END DO
	END DO
C 
        IF(SUMI.EQ.0.)THEN
           OX=IOX
           OY=IOY
        ELSE
           OX = SUMX/SUMI
           OY = SUMY/SUMI
        ENDIF

        RETURN
        END
C------------------------------------------------------------------------
C  Subroutine AUTO_SKY :
C  Works out the sky level and the noise of a CCD image
C
C Calculates the sky level of the image in 4 edge zones :
C
C Zone 1 : bottom left
C          I=NX/30+1,NX/6+1	J=NY/30+1,NY/6+1
C Zone 2 : bottom right
C          I=NX-NX/6,NX-NX/30	J=NY/30+1,NY/6+1
C Zone 3 : top left
C          I=NX/30+1,NX/6+1	J=NY-NY/6,NY-NY/30
C Zone 4 : top right
C          I=NX-NX/6,NX-NX/30	J=NY-NY/6,NY-NY/30
C
C INPUT :
C IMAGE(NX,NY)  REAL*4   declared as (IDIM,*)
C
C OUTPUT :
C SKY, SIGMA
C
C JLP
C Version 04-11-93
C------------------------------------------------------------------------
	SUBROUTINE AUTO_SKY(IMAGE,NX,NY,IDIM,SKY,SIGMA)

	REAL*4 IMAGE(IDIM,*),SKY,SIGMA,STATUS
	REAL*8 SUM(4),SUMSQ(4)
	REAL*4 SIG(4),XMEAN(4),VALMIN(4),VALMAX(4),COEF(4)
	REAL*4 XNUMB(4)
	INTEGER*4 IMIN(4),IMAX(4),JMIN(4),JMAX(4)
 
C Setting up the boundaries of the 4 areas:
        IF(NX.LT.6) THEN
          I1=0
          I2=1
        ELSE
	  I1=NX/30
	  I2=NX/6
        ENDIF

	IMIN(1)=I1+1
	IMIN(2)=NX-I2+1
	IMIN(3)=I1+1
	IMIN(4)=NX-I2+1
 
	IMAX(1)=I2
	IMAX(2)=NX-I1
	IMAX(3)=I2
	IMAX(4)=NX-I1

 
        IF(NY.LT.6) THEN
          J1=0
          J2=1
        ELSE
	  J1=NY/30
	  J2=NY/6
        ENDIF

	  JMIN(1)=J1+1
	  JMIN(2)=J1+1
	  JMIN(3)=NY-J2+1
	  JMIN(4)=NY-J2+1
 
	  JMAX(1)=J2
	  JMAX(2)=J2
	  JMAX(3)=NY-J1
	  JMAX(4)=NY-J1
 
C First loop on the four areas to determine mean and standard deviation :
 
	DO KK=1,4
 
	SUM(KK)=0.
	SUMSQ(KK)=0.
	XNUMB(KK)=0.
 
	DO J=JMIN(KK),JMAX(KK)
	 DO I=IMIN(KK),IMAX(KK)
	  SUM(KK)=IMAGE(I,J)+SUM(KK)
	  SUMSQ(KK)=IMAGE(I,J)*IMAGE(I,J)+SUMSQ(KK)
	  XNUMB(KK)=XNUMB(KK)+1.
	 END DO
	END DO

        IF(XNUMB(KK).NE.0) THEN	
	  XMEAN(KK)=SUM(KK)/XNUMB(KK)
	  WORK=(SUMSQ(KK)/XNUMB(KK))-(XMEAN(KK)*XMEAN(KK))
	  SIG(KK)=SQRT(WORK)
	  VALMIN(KK)=XMEAN(KK)-2.*SIG(KK)
	  VALMAX(KK)=XMEAN(KK)+2.*SIG(KK)
        ELSE
          WRITE(6,76)
76        FORMAT('AUTO_SKY: Unable to compute the sky level',/,
     1    ' Empty area, because image is too small')
          SKY=0.
          SIGMA=0.
          RETURN
        ENDIF
	
	END DO
	
C	WRITE(6,*) ' FIRST LOOP'
C	WRITE(6,75) (XMEAN(K),K=1,4),(SIG(K),K=1,4)
 
C Second loop on the four zones to discard the points over 2-sigma
	DO KK=1,4
	SUM(KK)=0.
	SUMSQ(KK)=0.
	XNUMB(KK)=0
 
	DO J=JMIN(KK),JMAX(KK)
	DO I=IMIN(KK),IMAX(KK)
	WORK=IMAGE(I,J)
	  IF(WORK.GT.VALMIN(KK).AND.WORK.LT.VALMAX(KK))THEN
	    XNUMB(KK)=XNUMB(KK)+1.
	    SUM(KK)=WORK+SUM(KK)
	    SUMSQ(KK)=WORK*WORK+SUMSQ(KK)
	  ENDIF
	END DO
	END DO
 
	
	END DO

C Test if bad area:
        ISTATUS=0
        DO KK=1,4
	  IF(XNUMB(KK).NE.0.)THEN
	    XMEAN(KK)=SUM(KK)/XNUMB(KK)
	    COEF(KK)=1.
	    WORK=(SUMSQ(KK)/XNUMB(KK))-(XMEAN(KK)*XMEAN(KK))
	    SIG(KK)=SQRT(WORK)
          ELSE
            COEF(KK)=0.
            ISTATUS=ISTATUS+1
	  ENDIF
        END DO
 
	WRITE(6,74) IMIN(1),IMIN(2),IMAX(1),IMAX(2),
     1        JMIN(1),JMIN(3),JMAX(1),JMAX(3)
74      FORMAT(' AUTO_SKY/X1,2,3,4',4(1X,I5),'  Y1,2,3,4',4(1X,I5))
C
	WRITE(6,75) (XMEAN(K),K=1,4),(SIG(K),K=1,4)
75	FORMAT(' 3-sigma mean in bottom left (X12Y12) bottom right',
	1	' (X34Y12) top left (X12Y34)',/,' & top right (X34Y34):',
	1	4(1PG11.4,1X),/,' Corresponding sigma: ',
	1	4(1PG11.4,1X))
 
C Initialize SKY and SIGMA, first (since it exists at least one good value):
	DO I=1,4
	 IF(COEF(I).NE.0.OR.ISTATUS.EQ.4)THEN
	   SKY=XMEAN(I)
	   SIGMA=SIG(I)
	 ENDIF
	END DO
 
C Then finds the minima:
	DO I=1,4
	 IF(COEF(I).NE.0.OR.ISTATUS.EQ.4)THEN
	   SKY=AMIN1(XMEAN(I),SKY)
	   SIGMA=AMIN1(SIG(I),SIGMA)
	 ENDIF
	END DO
 
C If status=4, no good points on second loop, so I keep first loop values:
        IF(ISTATUS.EQ.4) THEN
	  WRITE(6,77) SKY,SIGMA
77	  FORMAT('AUTO_SKY/ Sky level : ',1PG11.4,' sigma : ',
     1           1PG11.4,' (one loop)')
        ELSE
	  WRITE(6,78) SKY,SIGMA
78	  FORMAT('AUTO_SKY/ Sky level : ',1PG11.4,' sigma : ',
     1           1PG11.4)
        ENDIF
 
	RETURN
	END
