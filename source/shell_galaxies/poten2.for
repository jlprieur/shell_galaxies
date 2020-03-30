C++::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C       PROGRAM POTEN2
C
C  This program computes different triaxial models of elliptical
C  galaxies in the principal planes. Axis ratios :   X:P Y:Q Z:1.
C
C  PHI3(*,*,1),RHO3(*,*,1) : XY plane
C  PHI3(*,*,2),RHO3(*,*,2) : YZ plane
C  PHI3(*,*,3),RHO3(*,*,3) : ZX plane
C   To link with @SLINK1
C
C JLP
C Version 20-06-90
C--::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        PROGRAM POTEN2
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER (IDIM=1000)
	PARAMETER (IDIM1=400)
	REAL*8 PHI3(0:IDIM1,0:IDIM1,3)
	REAL*8 RHO3(0:IDIM1,0:IDIM1,3)
	REAL*4 IMAGE(IDIM,IDIM)
	CHARACTER NAME*40,COMMENTS*80,ANS*1,AGAIN*1,TITLE*40
	COMMON/POT1/PI,QQ1,PP1,PASX1,PASY1,PASZ1,NPTS
 
	PI=3.14159265358979323846
 
10	FORMAT(A)
 
	PRINT 338
338	FORMAT(' PROGRAM POTEN2  -Version 20-06-90')
 
C Inquire the format :
	CALL INQUIFMT
 
	OPEN(1,FILE='poten2.log',STATUS='unknown',
     1	ACCESS='SEQUENTIAL')
	WRITE(1,338)
C---------------------------------------------------------------
 
80	PRINT 84
84	FORMAT(' MENU :',/,
     1	' 1,IPLANE: DE ZEEUW AND MERRIT (1983)',/,
     1	' 2,IPLANE: RICHSTONE (1981)',/,
     1	' 3,IPLANE: "PSEUDO-HUBBLE"',/,
c     1	' 4,IPLANE: KING',/,
     1	' 5,IPLANE: "PERFECT ELLIPSOID"',/,
     1	' 6,IPLANE: "PERFECT ELLIPSOID" (MODIFIED)',/,
     1	'    ( IPLANE=1 FOR XY, 2 FOR YZ AND 3 FOR ZX)',/,
     1	' ENTER THE NUMBERS YOU WANT : ',$)
	READ(5,*) IMODEL,IPLANE
 
	IF(IPLANE.GT.3.AND.IPLANE.LT.1) GO TO 80
C
C  Input of the parameters
C
 
	PRINT *,' NUMBER OF POINTS FOR THE IMAGES (LESS THAN 400)?'
	READ(5,*) NPTS
1000	WRITE (6,335)
335	FORMAT(' Enter P and Q   (P > Q) : ',$)
	READ (5,*) PP1,QQ1
	PRINT *,' ENTER PASX, PASY, PASZ :'
	READ(5,*) PASX1,PASY1,PASZ1
 
	WRITE(1,340),PP1,QQ1,PASX1,PASY1,PASZ1
340	FORMAT(4X,'P =',F8.3,'  Q =',F8.3,5X,'PASX, PASY, PASZ :',
     1	3(F9.3,2X),/)
 
C--------------------------------------------------------------------
C Schwarzschild's model
 
	IF(IMODEL.EQ.1)THEN
	WRITE(1,101)IPLANE
101	FORMAT(/,'********** SCHWARZSCHILD MODEL:*********',/,
     1	' IPLANE =',I3,/)
	CALL SCHWARZ(PHI3,RHO3,IPLANE)
	ENDIF
 
C--------------------------------------------------------------------
C Richstone's model
 
	IF(IMODEL.EQ.2)THEN
	WRITE(1,201)IPLANE
201	FORMAT(/,'********** RICHSTONE MODEL:*********',/,
     1	' IPLANE =',I3,/)
	WRITE(1,902)
902	FORMAT(9X,'I',13X,'J',14X,'PHI',9X,'RHO',/)
	CALL RICHSTONE(PHI3,RHO3,IPLANE)
	ENDIF
 
C-------------------------------------------------------------------
C Pseudo-Hubble model
 
	IF(IMODEL.EQ.3)THEN
	WRITE(1,301)IPLANE
301	FORMAT(/,'********** PSEUDO-HUBBLE MODEL:*********',/,
     1	' IPLANE =',I3,/)
	WRITE(1,902)
	CALL HUBBLE_3D(PHI3,RHO3,IPLANE)
	ENDIF
 
C------------------------------------------------------------------------------
C Computation of  King's model : not finished yet...
C
	IF(IMODEL.EQ.4)THEN
	WRITE(1,401)IPLANE
401	FORMAT(/,'********** KING MODEL:*********',/,
     1	' IPLANE =',I3,/)
	WRITE(1,902)
	CALL  KING_3D(PHI3,RHO3,IPLANE)
	ENDIF
 
C-------------------------------------------------------------------
C Perfect ellipsoid (Hernquist and Quinn 1986)
 
	IF(IMODEL.EQ.5)THEN
	WRITE(1,501)IPLANE
501	FORMAT(/,'****** PERFECT ELLIPSOID MODEL: *******',/,
     1	' IPLANE =',I3,/)
	CALL ELLIPSOID1_3D(PHI3,RHO3,IPLANE)
	ENDIF
 
C-------------------------------------------------------------------
C Modified perfect ellipsoid (Hernquist and Quinn 1986)
 
	IF(IMODEL.EQ.6)THEN
	WRITE(1,601)IPLANE
601	FORMAT(/,'*** MODIFIED PERFECT ELLIPSOID MODEL: ****',/,
     1	' IPLANE =',I3,/)
	CALL ELLIPSOID2_3D(PHI3,RHO3,IPLANE)
	ENDIF
 
C---------------------------------------------------------------------
C Now output of the images :
 
	NPL=2*NPTS+1
	NL=2*NPTS+1
 
900	PRINT 901
901	FORMAT(' SECOND MENU :',/,
     1	' 1 : STORING DENSITY IMAGE',/,
     1	' 2 : STORING POTENTIAL IMAGE',/,
     1	' 3 : RETURN TO THE FIRST MENU',/,
     1	' 10 : EXIT',/,
     1	' ENTER THE OPTION YOU WANT : ',$)
	READ(5,*) IOPT9
 
	IF(IOPT9.EQ.3)GO TO 80
 
C-------------------------------------------------------------------
C Storing the density images :
	IF(IOPT9.EQ.1) THEN
	PRINT *,' MULTIPLYING FACTOR  (FACT=1000. ?)'	
	READ(5,10) FACT
	FACT=1.E+03
	CALL PLANE(RHO3,IMAGE,FACT,IPLANE)
        WRITE(6,*) 'Output density image: '
        READ(5,10) NAME
	COMMENTS=' '
	CALL JLP_WRITEIMAG(IMAGE,NPL,NL,IDIM,NAME,COMMENTS)
	GO TO 900
	ENDIF
 
C-------------------------------------------------------------------
C Storing the potential images :
	IF(IOPT9.EQ.2) THEN
	PRINT *,' MULTIPLYING FACTOR  (FACT=100. ?)'	
	READ(5,10) FACT
	FACT=1.E+03
	CALL PLANE(PHI3,IMAGE,FACT,IPLANE)
        WRITE(6,*) 'Output potential image: '
        READ(5,10) NAME
	COMMENTS=' '
	CALL JLP_WRITEIMAG(IMAGE,NPL,NL,IDIM,NAME,COMMENTS)
	GO TO 900
	ENDIF
 
999	CLOSE(1)
	PRINT *,' Output in "poten2.log".'
	END
C------------------------------------------------------------------------
C
	SUBROUTINE PLANE(PHI3,IMAGE,FACT,IPLANE)
C****************************************************
C	Subroutine PLANE to create an image of the potential
C	or the density in the plane chosen by the user.
C	 If IPLANE=1 plane X,Y
C	 If IPLANE=2 plane Y,Z
C	 If IPLANE=3 plane Z,X
C
C********************************************************
	PARAMETER (IDIM=1000)
	PARAMETER (IDIM1=400)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 PHI3(0:IDIM1,0:IDIM1,3)
	REAL*4 IMAGE(IDIM,IDIM)
	COMMON/POT1/PI,QQ1,PP1,PASX1,PASY1,PASZ1,NPTS
 
C Loading the image :
	DO 101 IX=0,NPTS
	DO 101 IY=0,NPTS
	RRR=FACT*PHI3(IX,IY,IPLANE)
 
	IXX=NPTS+IX+1
	IYY=NPTS+IY+1
	IMAGE(IXX,IYY)=SNGL(RRR)
	IXX=NPTS-IX+1
	IYY=IY+NPTS+1
	IMAGE(IXX,IYY)=SNGL(RRR)
	IXX=NPTS-IX+1
	IYY=NPTS-IY+1
	IMAGE(IXX,IYY)=SNGL(RRR)
	IXX=NPTS+IX+1
	IYY=NPTS-IY+1
	IMAGE(IXX,IYY)=SNGL(RRR)
 
101	CONTINUE
 
	RETURN
	END
C***************************************************************
C	Subroutine RICHSTONE (Ap.J. 1984 281,100)
C**************************************************************
	SUBROUTINE RICHSTONE(PHI3,RHO3,IPLANE)
	PARAMETER (IDIM=1000)
	PARAMETER (IDIM1=400)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 PHI3(0:IDIM1,0:IDIM1,3)
	REAL*8 RHO3(0:IDIM1,0:IDIM1,3)
	COMMON/POT1/PI,QQ1,PP1,PASX1,PASY1,PASZ1,NPTS
 
	C1=1.D0+(1.D0/(PP1*PP1))+(1.D0/(QQ1*QQ1))
 
 
C XY PLANE --------------------------------------------------------
	IF(IPLANE.EQ.1)THEN
	PP=PP1
	QQ=QQ1
	PASX=PASX1
	PASY=PASY1
	ENDIF
 
C YZ PLANE --------------------------------------------------------
	IF(IPLANE.EQ.2)THEN
	PP=QQ1
	QQ=1.
	PASX=PASY1
	PASY=PASZ1
	ENDIF
 
C ZX PLANE --------------------------------------------------------
	IF(IPLANE.EQ.3)THEN
	PP=1.
	QQ=PP1
	PASX=PASZ1
	PASY=PASX1
	ENDIF
 
C Calculates some constants :
 
	PP2=PP*PP
	QQ2=QQ*QQ
	PP4=PP2*PP2
	QQ4=QQ2*QQ2
 
 
	DO 101 IX=0,NPTS
	X=FLOAT(IX)*PASX
	X2=X*X
 
	DO 101 IY=0,NPTS
	Y=FLOAT(IY)*PASY
	Y2=Y*Y
 
	S2=(X2/PP2)+(Y2/QQ2)
 
C Avoids trouble in the center :
	IF(S2.LT.4.) S2=4.
	S=DSQRT(S2)
 
	PHI3(IX,IY,IPLANE)=DLOG(S)
	RHO3(IX,IY,IPLANE)=(C1-(2.D0*((X2/PP4)+(Y2/QQ4))/S2))/S2
 
	IF(IY.EQ.0)THEN
	WRITE(1,342)X,Y,PHI3(IX,IY,IPLANE),RHO3(IX,IY,IPLANE)
	ENDIF
342	FORMAT(2X,2(F12.3,2X),4X,E12.5,4X,E12.5)
 
101	CONTINUE
	WRITE(1,343)
343	FORMAT(/)
 
	RETURN
	END
C*************************************************************************
C	SUBROUTINE SCHWARZSCHILD
C  This subroutine calculates Schwarzschild's model
C  of a triaxial galaxy using De Zeeuw and Merrit's approximation (1983)
C  on the principal planes
C  PHI3(*,*,1),RHO3(*,*,1) : XY plane
C  PHI3(*,*,2),RHO3(*,*,2) : YZ plane
C  PHI3(*,*,3),RHO3(*,*,3) : ZX plane
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	SUBROUTINE SCHWARZ(PHI3,RHO3,IPLANE)
	PARAMETER (IDIM=1000)
	PARAMETER (IDIM1=400)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 PHI3(0:IDIM1,0:IDIM1,3)
	REAL*8 RHO3(0:IDIM1,0:IDIM1,3)
	COMMON/POT1/PI,QQ,PP,PASX,PASY,PASZ,NPTS
 
C Calculates C1 and C3
 
	PP2=PP*PP
	QQ2=QQ*QQ
	PP3=PP2*PP
	QQ3=QQ2*QQ
	ASYM2=2.D0*PP2*QQ2-PP2-QQ2
	SYM2=PP2*QQ2+PP2+QQ2
	SYM3=1.D0+PP3+QQ3
 
	D1=ASYM2/(14.D0*SYM2)
	D2=(1.D0-(3.D0/SYM3))/6.D0
	D3=(PP2-QQ2)/(28.D0*SYM2)
	D4=(PP3-QQ3)/(12.D0*SYM3)
 
C	C1=(4.D0/35.D0)*ASYM2/RRR
C	C3=(2.D0/35.D0)*(PP2-QQ2)/RRR
 
	C1=(D1**0.6)*(D2**0.4)
	C2=(D1/D2)**0.4
	C3=(D3**0.6)*(D4**0.4)
	C4=(D3/D4)**0.4
 
	WRITE(1,341) C1,C2,C3,C4
341	FORMAT(4X,'C1 =',E12.5,'   C2 =',E12.5,
     1	'  C3 =',E12.5,'   C4 =',E12.5,/,/,11X,'X',
     1	13X,'Y',13X,'Z',10X,'PHI3(X,Y,Z)',6X,'RHO3(X,Y,Z)',/)
 
 
C XY Plane ----------------------------------------------------------
	IF(IPLANE.EQ.1)THEN
 
	DO 101 IX=0,NPTS
	X=FLOAT(IX)*PASX
	X2=X*X
 
	DO 101 IY=0,NPTS
	Y=FLOAT(IY)*PASY
	Y2=Y*Y
 
	Z=0.D0
	Z2=0.D0
 
	RAD2=X2+Y2+Z2
	IF(RAD2.EQ.0.) RAD2=1.E-3
	RAD=DSQRT(RAD2)
 
C Calculating U, V and W
 
	URAD=(-1.D0/RAD)*DLOG(RAD+DSQRT(1+RAD2))+1.D0
	VRAD=(-1.D0*C1*RAD2)/((1.D0+C2*RAD2)**1.5)
	WRAD=(-1.D0*C3*RAD2)/((1.D0+C4*RAD2)**1.5)
	
	FRAD=1.D0/((1.D0+RAD2)**1.5)
	GRAD=3.D0*C1*C2*(7.D0+2.D0*C2*RAD2)*RAD2/
     1	((1.D0+C2*RAD2)**3.5)
	HRAD=3.D0*C3*C4*(7.D0+2.D0*C4*RAD2)*RAD2/
     1	((1.D0+C4*RAD2)**3.5)
 
 
	AA=(2.D0*Z2-X2-Y2)/(2.D0*RAD2)
	BB=3.D0*(X2-Y2)/RAD2
 
C
	PHI3(IX,IY,1)=URAD-AA*VRAD+BB*WRAD
	RHO3(IX,IY,1)=FRAD-AA*GRAD+BB*HRAD
 
	IF(IY.EQ.0)THEN
	WRITE(1,342)X,Y,Z,PHI3(IX,IY,1),RHO3(IX,IY,1)
	ENDIF
342	FORMAT(2X,3(F12.3,2X),4X,E12.5,4X,E12.5)
 
101	CONTINUE
	WRITE(1,343)
343	FORMAT(/)
	RETURN
	ENDIF
 
 
C YZ Plane ----------------------------------------------------------
	IF(IPLANE.EQ.2)THEN
 
	DO 102 IY=0,NPTS
	Y=FLOAT(IY)*PASY
	Y2=Y*Y
 
	DO 102 IZ=0,NPTS
	Z=FLOAT(IZ)*PASZ
	Z2=Z*Z
	
	X=0.D0
	X2=0.D0
	RAD2=X2+Y2+Z2
	IF(RAD2.EQ.0.) RAD2=1.E-3
	RAD=DSQRT(RAD2)
 
C Calculating U, V and W
 
	URAD=(-1.D0/RAD)*DLOG(RAD+DSQRT(1+RAD2))+1.D0
	VRAD=(-1.D0*C1*RAD2)/((1.D0+C2*RAD2)**1.5)
	WRAD=(-1.D0*C3*RAD2)/((1.D0+C4*RAD2)**1.5)
	
	FRAD=1.D0/((1.D0+RAD2)**1.5)
	GRAD=3.D0*C1*C2*(7.D0+2.D0*C2*RAD2)*RAD2/
     1	((1.D0+C2*RAD2)**3.5)
	HRAD=3.D0*C3*C4*(7.D0+2.D0*C4*RAD2)*RAD2/
     1	((1.D0+C4*RAD2)**3.5)
 
 
	AA=(2.D0*Z2-X2-Y2)/(2.D0*RAD2)
	BB=3.D0*(X2-Y2)/RAD2
 
C
	PHI3(IY,IZ,2)=URAD-AA*VRAD+BB*WRAD
	RHO3(IY,IZ,2)=FRAD-AA*GRAD+BB*HRAD
 
	IF(IZ.EQ.0)THEN
	WRITE(1,342)X,Y,Z,PHI3(IY,IZ,2),RHO3(IY,IZ,2)
	ENDIF
 
102	CONTINUE
	WRITE(1,343)
	RETURN
	ENDIF
 
C ZX Plane ---------------------------------------------------------
C
	IF(IPLANE.EQ.3)THEN
 
	DO 103 IZ=0,NPTS
	Z=FLOAT(IZ)*PASZ
	Z2=Z*Z
 
	DO 103 IX=0,NPTS
	X=FLOAT(IX)*PASX
	X2=X*X
 
	Y=0.D0
	Y2=0.D0
	RAD2=X2+Y2+Z2
	IF(RAD2.EQ.0.) RAD2=1.E-3
	RAD=DSQRT(RAD2)
 
C Calculating U, V and W
 
	URAD=(-1.D0/RAD)*DLOG(RAD+DSQRT(1+RAD2))+1.D0
	VRAD=(-1.D0*C1*RAD2)/((1.D0+C2*RAD2)**1.5)
	WRAD=(-1.D0*C3*RAD2)/((1.D0+C4*RAD2)**1.5)
	
	FRAD=1.D0/((1.D0+RAD2)**1.5)
	GRAD=3.D0*C1*C2*(7.D0+2.D0*C2*RAD2)*RAD2/
     1	((1.D0+C2*RAD2)**3.5)
	HRAD=3.D0*C3*C4*(7.D0+2.D0*C4*RAD2)*RAD2/
     1	((1.D0+C4*RAD2)**3.5)
 
 
	AA=(2.D0*Z2-X2-Y2)/(2.D0*RAD2)
	BB=3.D0*(X2-Y2)/RAD2
 
C
	PHI3(IZ,IX,3)=URAD-AA*VRAD+BB*WRAD
	RHO3(IZ,IX,3)=FRAD-AA*GRAD+BB*HRAD
 
	IF(IX.EQ.0)THEN
	WRITE(1,342)X,Y,Z,PHI3(IZ,IX,3),RHO3(IZ,IX,3)
	ENDIF
 
103	CONTINUE
	RETURN
	ENDIF
 
	RETURN
	END
C*************************************************************************
C	SUBROUTINE ELLIPSOID1_3D
C  This subroutine computes a "perfect ellipsoid" model
C  of a triaxial galaxy using Hernquist and Quinn's approximation (1986)
C  on the principal planes
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	SUBROUTINE ELLIPSOID1_3D(PHI3,RHO3,IPLANE)
	PARAMETER (IDIM=1000)
	PARAMETER (IDIM1=400)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 PHI3(0:IDIM1,0:IDIM1,3)
	REAL*8 RHO3(0:IDIM1,0:IDIM1,3)
	COMMON/POT1/PI,QQ,PP,PASX,PASY,PASZ,NPTS
 
C Computes some constants :
 
	PP2=PP*PP
	QQ2=QQ*QQ
	PP4=PP2*PP2
	QQ4=QQ2*QQ2
	ASYM2=2.D0*PP2*QQ2-PP2-QQ2
	SYM2=PP2*QQ2+PP2+QQ2
	SYM4=1.D0+PP4+QQ4
 
	C1=((PP4+QQ4-2.D0)*ASYM2*ASYM2
     1	/(98.D0*SYM4*SYM2*SYM2))**.3333333
	print *,' c1 =',c1
	C2=((2.D0/7.D0)*SYM4*ASYM2
     1	/((PP4+QQ4-2.D0)*SYM2))**.3333333
	print *,' c2 =',c2
	C3=0.5D0*((PP2-QQ2)*(PP2-QQ2)*(PP4-QQ4)
     1	/(98.D0*SYM4*SYM2*SYM2))**.3333333
	print *,' c3 =',c3
	C4=((2.D0/7.D0)*SYM4
     1	/((PP2+QQ2)*SYM2))**.333333
	print *,' c4 =',c4
 
	WRITE(1,341) C1,C2,C3,C4
341	FORMAT(4X,'C1 =',E12.5,'   C2 =',E12.5,
     1	'  C3 =',E12.5,'   C4 =',E12.5,/,/,11X,'X',
     1	13X,'Y',13X,'Z',10X,'PHI3(X,Y,Z)',6X,'RHO3(X,Y,Z)',/)
 
C XY Plane ----------------------------------------------------------
	IF(IPLANE.EQ.1)THEN
 
	DO 101 IX=0,NPTS
	X=FLOAT(IX)*PASX
	X2=X*X
 
	DO 101 IY=0,NPTS
	Y=FLOAT(IY)*PASY
	Y2=Y*Y
 
	Z=0.D0
	Z2=0.D0
 
	RAD2=X2+Y2+Z2
	IF(RAD2.EQ.0.) RAD2=1.E-3
	RAD=DSQRT(RAD2)
 
C Calculating U, V and W
 
	URAD=(-1.D0*DATAN(RAD))/RAD
	VRAD=(-1.D0*C1*RAD2)/((1.D0+C2*RAD2)**2)
	WRAD=(-1.D0*C3*RAD2)/((1.D0+C4*RAD2)**2)
	
	FRAD=1.D0/((1.D0+RAD2)**2)
	GRAD=4.D0*C1*C2*(7.D0+C2*RAD2)*RAD2/
     1	((1.D0+C2*RAD2)**4)
	HRAD=4.D0*C3*C4*(7.D0+C4*RAD2)*RAD2/
     1	((1.D0+C4*RAD2)**4)
 
	AA=(2.D0*Z2-X2-Y2)/(2.D0*RAD2)
	BB=3.D0*(X2-Y2)/RAD2
C
	PHI3(IX,IY,1)=URAD-AA*VRAD+BB*WRAD
	RHO3(IX,IY,1)=FRAD-AA*GRAD+BB*HRAD
 
	IF(IY.EQ.0)THEN
	WRITE(1,342)X,Y,Z,PHI3(IX,IY,1),RHO3(IX,IY,1)
	ENDIF
342	FORMAT(2X,3(F12.3,2X),4X,E12.5,4X,E12.5)
 
101	CONTINUE
	WRITE(1,343)
343	FORMAT(/)
	RETURN
	ENDIF
 
 
C YZ Plane ----------------------------------------------------------
	IF(IPLANE.EQ.2)THEN
 
	DO 102 IY=0,NPTS
	Y=FLOAT(IY)*PASY
	Y2=Y*Y
 
	DO 102 IZ=0,NPTS
	Z=FLOAT(IZ)*PASZ
	Z2=Z*Z
	
	X=0.D0
	X2=0.D0
	RAD2=X2+Y2+Z2
	IF(RAD2.EQ.0.) RAD2=1.E-3
	RAD=DSQRT(RAD2)
 
C Calculating U, V and W
 
	URAD=(-1.D0*DATAN(RAD))/RAD
	VRAD=(-1.D0*C1*RAD2)/((1.D0+C2*RAD2)**2)
	WRAD=(-1.D0*C3*RAD2)/((1.D0+C4*RAD2)**2)
	
	FRAD=1.D0/((1.D0+RAD2)**2)
	GRAD=4.D0*C1*C2*(7.D0+C2*RAD2)*RAD2/
     1	((1.D0+C2*RAD2)**4)
	HRAD=4.D0*C3*C4*(7.D0+C4*RAD2)*RAD2/
     1	((1.D0+C4*RAD2)**4)
 
	AA=(2.D0*Z2-X2-Y2)/(2.D0*RAD2)
	BB=3.D0*(X2-Y2)/RAD2
C
	PHI3(IY,IZ,2)=URAD-AA*VRAD+BB*WRAD
	RHO3(IY,IZ,2)=FRAD-AA*GRAD+BB*HRAD
 
	IF(IZ.EQ.0)THEN
	WRITE(1,342)X,Y,Z,PHI3(IY,IZ,2),RHO3(IY,IZ,2)
	ENDIF
 
102	CONTINUE
	WRITE(1,343)
	RETURN
	ENDIF
 
C ZX Plane ---------------------------------------------------------
C
	IF(IPLANE.EQ.3)THEN
 
	DO 103 IZ=0,NPTS
	Z=FLOAT(IZ)*PASZ
	Z2=Z*Z
 
	DO 103 IX=0,NPTS
	X=FLOAT(IX)*PASX
	X2=X*X
 
	Y=0.D0
	Y2=0.D0
	RAD2=X2+Y2+Z2
	IF(RAD2.EQ.0.) RAD2=1.E-3
	RAD=DSQRT(RAD2)
 
C Calculating U, V and W
 
	URAD=(-1.D0*DATAN(RAD))/RAD
	VRAD=(-1.D0*C1*RAD2)/((1.D0+C2*RAD2)**2)
	WRAD=(-1.D0*C3*RAD2)/((1.D0+C4*RAD2)**2)
	
	FRAD=1.D0/((1.D0+RAD2)**2)
	GRAD=4.D0*C1*C2*(7.D0+C2*RAD2)*RAD2/
     1	((1.D0+C2*RAD2)**4)
	HRAD=4.D0*C3*C4*(7.D0+C4*RAD2)*RAD2/
     1	((1.D0+C4*RAD2)**4)
C
	AA=(2.D0*Z2-X2-Y2)/(2.D0*RAD2)
	BB=3.D0*(X2-Y2)/RAD2
 
	PHI3(IZ,IX,3)=URAD-AA*VRAD+BB*WRAD
	RHO3(IZ,IX,3)=FRAD-AA*GRAD+BB*HRAD
 
	IF(IX.EQ.0)THEN
	WRITE(1,342)X,Y,Z,PHI3(IZ,IX,3),RHO3(IZ,IX,3)
	ENDIF
 
103	CONTINUE
	RETURN
	ENDIF
 
	RETURN
	END
C*************************************************************************
C	SUBROUTINE ELLIPSOID2_3D
C  This subroutine computes a "modified perfect ellipsoid" model
C  of a triaxial galaxy using Hernquist and Quinn's approximation (1986)
C  on the principal planes
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	SUBROUTINE ELLIPSOID2_3D(PHI3,RHO3,IPLANE)
	PARAMETER (IDIM=1000)
	PARAMETER (IDIM1=400)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 PHI3(0:IDIM1,0:IDIM1,3)
	REAL*8 RHO3(0:IDIM1,0:IDIM1,3)
	COMMON/POT1/PI,QQ,PP,PASX,PASY,PASZ,NPTS
 
C Computes some constants :
 
	PP2=PP*PP
	QQ2=QQ*QQ
	ASYM2=2.D0*PP2*QQ2-PP2-QQ2
	SYM2=PP2*QQ2+PP2+QQ2
 
	C1=(4.D0/35.D0)*ASYM2/SYM2
	C3=(2.D0/35.D0)*(PP2-QQ2)/SYM2
 
	WRITE(1,341) C1,C2,C3,C4
341	FORMAT(4X,'C1 =',E12.5,'   C2 =',E12.5,
     1	'  C3 =',E12.5,'   C4 =',E12.5,/,/,11X,'X',
     1	13X,'Y',13X,'Z',10X,'PHI3(X,Y,Z)',6X,'RHO3(X,Y,Z)',/)
 
C XY Plane ----------------------------------------------------------
	IF(IPLANE.EQ.1)THEN
 
	DO 101 IX=0,NPTS
	X=FLOAT(IX)*PASX
	X2=X*X
 
	DO 101 IY=0,NPTS
	Y=FLOAT(IY)*PASY
	Y2=Y*Y
 
	Z=0.D0
	Z2=0.D0
 
	RAD2=X2+Y2+Z2
	IF(RAD2.EQ.0.) RAD2=1.E-3
	RAD=DSQRT(RAD2)
 
C Calculating U, V and W
 
	URAD=(-1.D0*DATAN(RAD))/RAD
	VRAD=(-1.D0*C1*RAD2)/((1.D0+RAD2)**2.5)
	WRAD=(-1.D0*C3*RAD2)/((1.D0+RAD2)**2.5)
	
	FRAD=1.D0/((1.D0+RAD2)**2)
	GRAD=35.D0*C1*RAD2/
     1	((1.D0+RAD2)**4.5)
	HRAD=35.D0*C3*RAD2/
     1	((1.D0+RAD2)**4.5)
 
	AA=(2.D0*Z2-X2-Y2)/(2.D0*RAD2)
	BB=3.D0*(X2-Y2)/RAD2
C
	PHI3(IX,IY,1)=URAD-AA*VRAD+BB*WRAD
	RHO3(IX,IY,1)=FRAD-AA*GRAD+BB*HRAD
 
	IF(IY.EQ.0)THEN
	WRITE(1,342)X,Y,Z,PHI3(IX,IY,1),RHO3(IX,IY,1)
	ENDIF
342	FORMAT(2X,3(F12.3,2X),4X,E12.5,4X,E12.5)
 
101	CONTINUE
	WRITE(1,343)
343	FORMAT(/)
	RETURN
	ENDIF
 
 
C YZ Plane ----------------------------------------------------------
	IF(IPLANE.EQ.2)THEN
 
	DO 102 IY=0,NPTS
	Y=FLOAT(IY)*PASY
	Y2=Y*Y
 
	DO 102 IZ=0,NPTS
	Z=FLOAT(IZ)*PASZ
	Z2=Z*Z
	
	X=0.D0
	X2=0.D0
	RAD2=X2+Y2+Z2
	IF(RAD2.EQ.0.) RAD2=1.E-3
	RAD=DSQRT(RAD2)
 
C Calculating U, V and W
 
	URAD=(-1.D0*DATAN(RAD))/RAD
	VRAD=(-1.D0*C1*RAD2)/((1.D0+RAD2)**2.5)
	WRAD=(-1.D0*C3*RAD2)/((1.D0+RAD2)**2.5)
	
	FRAD=1.D0/((1.D0+RAD2)**2)
	GRAD=35.D0*C1*RAD2/
     1	((1.D0+RAD2)**4.5)
	HRAD=35.D0*C3*RAD2/
     1	((1.D0+RAD2)**4.5)
 
	AA=(2.D0*Z2-X2-Y2)/(2.D0*RAD2)
	BB=3.D0*(X2-Y2)/RAD2
C
	PHI3(IY,IZ,2)=URAD-AA*VRAD+BB*WRAD
	RHO3(IY,IZ,2)=FRAD-AA*GRAD+BB*HRAD
 
	IF(IZ.EQ.0)THEN
	WRITE(1,342)X,Y,Z,PHI3(IY,IZ,2),RHO3(IY,IZ,2)
	ENDIF
 
102	CONTINUE
	WRITE(1,343)
	RETURN
	ENDIF
 
C ZX Plane ---------------------------------------------------------
C
	IF(IPLANE.EQ.3)THEN
 
	DO 103 IZ=0,NPTS
	Z=FLOAT(IZ)*PASZ
	Z2=Z*Z
 
	DO 103 IX=0,NPTS
	X=FLOAT(IX)*PASX
	X2=X*X
 
	Y=0.D0
	Y2=0.D0
	RAD2=X2+Y2+Z2
	IF(RAD2.EQ.0.) RAD2=1.E-3
	RAD=DSQRT(RAD2)
 
C Calculating U, V and W
 
	URAD=(-1.D0*DATAN(RAD))/RAD
	VRAD=(-1.D0*C1*RAD2)/((1.D0+RAD2)**2.5)
	WRAD=(-1.D0*C3*RAD2)/((1.D0+RAD2)**2.5)
	
	FRAD=1.D0/((1.D0+RAD2)**2)
	GRAD=35.D0*C1*RAD2/
     1	((1.D0+RAD2)**4.5)
	HRAD=35.D0*C3*RAD2/
     1	((1.D0+RAD2)**4.5)
C
	AA=(2.D0*Z2-X2-Y2)/(2.D0*RAD2)
	BB=3.D0*(X2-Y2)/RAD2
 
	PHI3(IZ,IX,3)=URAD-AA*VRAD+BB*WRAD
	RHO3(IZ,IX,3)=FRAD-AA*GRAD+BB*HRAD
 
	IF(IX.EQ.0)THEN
	WRITE(1,342)X,Y,Z,PHI3(IZ,IX,3),RHO3(IZ,IX,3)
	ENDIF
 
103	CONTINUE
	RETURN
	ENDIF
 
	RETURN
	END
C--------------------------------------------------------------------
C Subroutine KING_3D to compute King's models
C----------------------------------------------------------------------
	SUBROUTINE KING_3D(PHI3,RHO3,IPLANE)
 
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER (IDIM2=10000)
	PARAMETER (IDIM1=400)
	REAL*8 SPLINE_PHI(IDIM2),K_PHI(IDIM2),S(5)
	REAL*8 SPLINE_RHO(IDIM2),K_RHO(IDIM2)
	REAL*8 RHO(IDIM2),RAD(IDIM2),
     1	PHI(IDIM2),DPHI(IDIM2)
	REAL*8 PHI3(0:IDIM1,0:IDIM1,3)
	REAL*8 RHO3(0:IDIM1,0:IDIM1,3)
 
	COMMON/SPLINE_PHI/SPLINE_PHI,NCAP7_PHI,K_PHI
	COMMON/SPLINE_RHO/SPLINE_RHO,NCAP7_RHO,K_RHO
	COMMON/POT1/PI,QQ1,PP1,PASX1,PASY1,PASZ1,NPTS
 
C Common blocks with KING_MODEL :
	COMMON/KING/RHO,PHI,RAD,NINDEX,DPHI
	COMMON/KING3/W0,RTIDAL,RHO0
 
10	FORMAT(A)
 
	WRITE (6,301)
301	FORMAT(' ENTER W0 (CENTRAL POTENTIAL) AND RCORE (KPC) : ',$)
	READ (5,*) W0,RCORE
 
C Number of points calculated for the model
	WRITE (6,302)
302	FORMAT(' Number of points for King model:(max 10000) :',$)
	READ (5,*) NPTK
	WRITE (1,306)W0,RCORE,NPTK
306	FORMAT(' CALCULATING KING''S MODEL',/,
     1	' W0=',F10.4,'  RCORE=',F12.3,'  NPTK =',I5)
 
C Setting up the radius array for the model (From 1.E-1 to 1.E+3)
	RAD(1)=0.D0
	DO I=2,NPTK
	WORK=-1.D0 +4.D0*FLOAT(I)/FLOAT(NPTK)
	RAD(I)=10.D0**WORK
	END DO
 
	PRINT *,' CALCULATING THE MODEL... '
	CALL KING(NPTK)
	NPTS=JMIN0(NPTK,NPTS)
	PRINT *,' NPTK =',NPTK
	PRINT *,' NPTS =',NPTS
 
C Computing the total mass :
	RADMAX=RTIDAL+10.D0
	CALL MASS_KING(RADMAX,XMU1,NPTK)
	XMU=RHO0*XMU1
	XMASS1=XMU*RCORE*RCORE*RCORE
	PRINT *,' MU =',XMU,'MASS1 =',XMASS1
 
C
	PRINT *,' TOTAL MASS YOU WANT FOR THIS MODEL ?',
     1	' (UNITS: 10.D+10 SOLAR MASS)'
	READ(5,*) XMASS
	XRHO0=RHO0*XMASS/XMASS1
	PRINT *,' NEW RHO0 :',XRHO0
	WRITE(1,307)XMU,XRHO0,XMASS1,XMASS
307	FORMAT(' XMU =',E12.4,' NEW RHO0 =',E12.4,
     1	'  XMASS1 = ',E12.4,/,
     1	' TOTAL MASS = ',E12.4,' IN 1.E+10 SOLAR MASS')
 
C Normalization of the potential to get the correct mass :
C (Potential energy is proportional to mass)
C PHI is shifted of G.M/RT because in King's model, it is assumed PHI=0.
C when RAD=RT. We take a positive form for the potential (-1.* true pot)
	GRAV=4.497D-02
	DO I=1,NPTK
	PHI(I)=(GRAV*XMASS/RTIDAL)-XMASS*PHI(I)/XMASS1
	IF(PHI(I).LE.0.D0)PRINT *,'I,PHI(I)',I,PHI(I)
	END DO
 
C Transformation of the computed radii in physical radii
	DO I=1,NPTK
	RAD(I)=RCORE*RAD(I)
	END DO
 
C Possibility of a Keplerian extension :
C	PRINT *,' DO YOU WANT AN EXTENSION TO THE OUTER PARTS ?'
C	READ(5,10) ANS
	ANS='N'
	IF(ANS.EQ.'Y')THEN
	PRINT *,' MAX. RADIUS AND NUMBER OF POINTS ?'
	READ(5,*) RADMAX,NUMBER
C	CALL KEPLER_EXTENSION(RAD,PHI,NPTK,XMASS,
C     1	RADMAX,NUMBER)
	WRITE(1,308)RADMAX,NUMBER
308	FORMAT(' KEPLERIAN EXTENSION IN THE OUTER PARTS',/,
     1	' UNTIL ',F12.3,' ARCSECONDS',/,
     1	' WITH ',I5,' MORE POINTS')
	ENDIF
 
 
C Fits a spline to the logarithm of PHI
	PRINT *,' FIT OF THE POTENTIAL :'
	CALL SPLINELOGFIT(RAD,PHI,NPTK,SPLINE_PHI,NCAP7_PHI,K_PHI)
 
C Fits a spline to the logarithm of RHO
	PRINT *,' FIT OF THE DENSITY :'
	CALL SPLINELOGFIT(RAD,RHO,NPTK,SPLINE_RHO,NCAP7_RHO,K_RHO)
 
C Puts the model into a triaxial shape
	CALL KING_3D_PRO(PHI3,RHO3,IPLANE)
 
	RETURN
	END
C-------------------------------------------------------------------------
C Subroutine KING_3D_PRO
C To project the potential and the density onto 3 planes in a triaxial
C geometry.
C-------------------------------------------------------------------------
	SUBROUTINE KING_3D_PRO(PHI3,RHO3,IPLANE)
 
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	PARAMETER (IDIM2=10000)
	PARAMETER (IDIM1=400)
	REAL*8 SPLINE_PHI(IDIM2),K_PHI(IDIM2),S(5)
	REAL*8 SPLINE_RHO(IDIM2),K_RHO(IDIM2)
	REAL*8 RHO(IDIM2),RAD(IDIM2),
     1	PHI(IDIM2),DPHI(IDIM2)
	REAL*8 PHI3(0:IDIM1,0:IDIM1,3)
	REAL*8 RHO3(0:IDIM1,0:IDIM1,3)
 
	COMMON/SPLINE_PHI/SPLINE_PHI,NCAP7_PHI,K_PHI
	COMMON/SPLINE_RHO/SPLINE_RHO,NCAP7_RHO,K_RHO
	COMMON/POT1/PI,QQ,PP,PASX,PASY,PASZ,NPTS
 
C Common blocks with KING_MODEL :
	COMMON/KING/RHO,PHI,RAD,NINDEX,DPHI
	COMMON/KING3/W0,RTIDAL,RHO0
 
	PP2=PP*PP
	QQ2=QQ*QQ
 
C XY PLANE -------------------------------------------------------------
	IF(IPLANE.EQ.1)THEN
 
	DO 101 IX=0,NPTS
	X=FLOAT(IX)*PASX
	X2=X*X
 
	DO 101 IY=0,NPTS
	Y=FLOAT(IY)*PASY
	Y2=Y*Y
 
	Z=0.D0
	Z2=0.D0
 
	S2=(X2/PP2)+(Y2/QQ2)+Z2
	IF(S2.EQ.0.) S2=1.E-3
C We take sqrt(s2) but in LOG10 since the spline fit was in log10/log10
	SS=0.5D0*DLOG10(S2)
 
C Calling NAG routine E02BCF:
	LEFT=0
	IFAIL=1
	CALL E02BCF(NCAP7_PHI,K_PHI,SPLINE_PHI,SS,LEFT,S,IFAIL)
	IF(IFAIL.NE.0)PRINT 989,IFAIL
	PHI3(IX,IY,1)=10.D0**S(1)
 
C Calling NAG routine E02BCF:
	LEFT=0
	IFAIL=1
	CALL E02BCF(NCAP7_RHO,K_RHO,SPLINE_RHO,SS,LEFT,S,IFAIL)
	IF(IFAIL.NE.0)PRINT 989,IFAIL
	RHO3(IX,IY,1)=10.D0**S(1)
 
989	FORMAT(' WARNING : PROBLEM IN E02BCF.    IFAIL=',I4)
 
	IF(IY.EQ.0)THEN
	WRITE(1,342)X,Y,Z,PHI3(IX,IY,1),RHO3(IX,IY,1)
	ENDIF
342	FORMAT(2X,3(F12.3,2X),4X,E12.5,4X,E12.5)
 
101	CONTINUE
	WRITE(1,343)
343	FORMAT(/)
	RETURN
	ENDIF
 
C YZ Plane ------------------------------------------------------------
 
	IF(IPLANE.EQ.2)THEN
 
	DO 102 IY=0,NPTS
	Y=FLOAT(IY)*PASY
	Y2=Y*Y
 
	DO 102 IZ=0,NPTS
	Z=FLOAT(IZ)*PASZ
	Z2=Z*Z
	
	X=0.D0
	X2=0.D0
 
	S2=(X2/PP2)+(Y2/QQ2)+Z2
	IF(S2.EQ.0.) S2=1.E-3
C We take sqrt(s2) but in LOG10 since the spline fit was in log10/log10
	SS=0.5D0*DLOG10(S2)
 
C Calling NAG routine E02BCF:
	LEFT=0
	IFAIL=1
	CALL E02BCF(NCAP7_PHI,K_PHI,SPLINE_PHI,SS,LEFT,S,IFAIL)
	IF(IFAIL.NE.0)PRINT 989,IFAIL
	PHI3(IY,IZ,2)=10.D0**S(1)
 
C Calling NAG routine E02BCF:
	LEFT=0
	IFAIL=1
	CALL E02BCF(NCAP7_RHO,K_RHO,SPLINE_RHO,SS,LEFT,S,IFAIL)
	IF(IFAIL.NE.0)PRINT 989,IFAIL
	RHO3(IY,IZ,2)=10.D0**S(1)
 
	IF(IZ.EQ.0)THEN
	WRITE(1,342)X,Y,Z,PHI3(IY,IZ,2),RHO3(IY,IZ,2)
	ENDIF
 
102	CONTINUE
	WRITE(1,343)
	RETURN
	ENDIF
 
C ZX Plane --------------------------------------------------------------
 
	IF(IPLANE.EQ.3)THEN
 
	DO 103 IZ=0,NPTS
	Z=FLOAT(IZ)*PASZ
	Z2=Z*Z
 
	DO 103 IX=0,NPTS
	X=FLOAT(IX)*PASX
	X2=X*X
 
	Y=0.D0
	Y2=0.D0
 
	S2=(X2/PP2)+(Y2/QQ2)+Z2
	IF(S2.EQ.0.) S2=1.E-3
C We take sqrt(s2) but in LOG10 since the spline fit was in log10/log10
	SS=0.5D0*DLOG10(S2)
 
C Calling NAG routine E02BCF:
	LEFT=0
	IFAIL=1
	CALL E02BCF(NCAP7_PHI,K_PHI,SPLINE_PHI,SS,LEFT,S,IFAIL)
	IF(IFAIL.NE.0)PRINT 989,IFAIL
	PHI3(IZ,IX,3)=10.D0**S(1)
 
C Calling NAG routine E02BCF:
	LEFT=0
	IFAIL=1
	CALL E02BCF(NCAP7_RHO,K_RHO,SPLINE_RHO,SS,LEFT,S,IFAIL)
	IF(IFAIL.NE.0)PRINT 989,IFAIL
	RHO3(IZ,IX,3)=10.D0**S(1)
 
	IF(IX.EQ.0)THEN
	WRITE(1,342)X,Y,Z,PHI3(IZ,IX,3),RHO3(IZ,IX,3)
	ENDIF
 
103	CONTINUE
	RETURN
	ENDIF
 
	RETURN
	END
C***************************************************************
C	Subroutine HUBBLE_3D
C	PHI(S)=1/(1+S)**ALPHA
C OR	PHI(S)=1/(1+S**2)**ALPHA
C**************************************************************
	SUBROUTINE HUBBLE_3D(PHI3,RHO3,IPLANE)
	PARAMETER (IDIM=1000)
	PARAMETER (IDIM1=400)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*4 ALPHA
	REAL*8 PHI3(0:IDIM1,0:IDIM1,3)
	REAL*8 RHO3(0:IDIM1,0:IDIM1,3)
	COMMON/POT1/PI,QQ1,PP1,PASX1,PASY1,PASZ1,NPTS
 
	C1=1.D0+(1.D0/(PP1*PP1))+(1.D0/(QQ1*QQ1))
 
C Setting the options :
	PRINT 77
77	FORMAT(' 2 POSSIBILITIES :',/,
     1	' 1. PHI(S)=1/(1+S)**ALPHA ',/,
     1	' 2. PHI(S)=1/(1+S**2)**ALPHA ',/,
     1	' ENTER THE OPTION YOU WANT : ',$)
	READ(5,*) IOPT
 
	PRINT *,' VALUE FOR ALPHA ?'
	READ(5,*) ALPHA
	WRITE(1,78)ALPHA
78	FORMAT(' ALPHA = ',F8.4)
	ALPHAM=-1.*ALPHA
 
C XY PLANE --------------------------------------------------------
	IF(IPLANE.EQ.1)THEN
	PP=PP1
	QQ=QQ1
	PASX=PASX1
	PASY=PASY1
	ENDIF
 
C YZ PLANE --------------------------------------------------------
	IF(IPLANE.EQ.2)THEN
	PP=QQ1
	QQ=1.
	PASX=PASY1
	PASY=PASZ1
	ENDIF
 
C ZX PLANE --------------------------------------------------------
	IF(IPLANE.EQ.3)THEN
	PP=1.
	QQ=PP1
	PASX=PASZ1
	PASY=PASX1
	ENDIF
 
C Calculates some constants :
 
	PP2=PP*PP
	QQ2=QQ*QQ
	PP4=PP2*PP2
	QQ4=QQ2*QQ2
 
C Computing the model :
 
C Option 1:
	IF(IOPT.EQ.1)THEN
 
	DO 101 IX=0,NPTS
	X=FLOAT(IX)*PASX
	X2=X*X
 
	DO 101 IY=0,NPTS
	Y=FLOAT(IY)*PASY
	Y2=Y*Y
 
	S2=(X2/PP2)+(Y2/QQ2)
 
C Avoids trouble in the center :
	IF(S2.LT.4.) S2=4.
	SS=DSQRT(S2)
	SSP1=1.D0+SS
	PHI3(IX,IY,IPLANE)=SSP1**ALPHAM
	RHO3(IX,IY,IPLANE)=(ALPHA/SS)*(SSP1**(-2.-ALPHA))*
     1	(C1*SSP1-(1.D0+(2.D0+ALPHA)*SS)*((X2/PP4)+(Y2/QQ4))/
     1	S2)
 
	IF(IY.EQ.0)THEN
	WRITE(1,342)X,Y,PHI3(IX,IY,IPLANE),RHO3(IX,IY,IPLANE)
	ENDIF
342	FORMAT(2X,2(F12.3,2X),4X,E12.5,4X,E12.5)
 
101	CONTINUE
	WRITE(1,343)
343	FORMAT(/)
	ENDIF
 
C---------------------------------------------------------------
C Option 2:
	IF(IOPT.EQ.2)THEN
 
	DO 201 IX=0,NPTS
	X=FLOAT(IX)*PASX
	X2=X*X
 
	DO 201 IY=0,NPTS
	Y=FLOAT(IY)*PASY
	Y2=Y*Y
 
	S2=(X2/PP2)+(Y2/QQ2)
 
	IF(S2.LT.1.D-06) S2=1.D-06
	SS=DSQRT(S2)
	SSP1=1.D0+SS*SS
	PHI3(IX,IY,IPLANE)=SSP1**ALPHAM
	RHO3(IX,IY,IPLANE)=2.D0*ALPHA*(SSP1**(-2.-ALPHA))*
     1	(C1*SSP1-2.D0*(1.D0+ALPHA)*((X2/PP4)+(Y2/QQ4)))
 
	IF(IY.EQ.0)THEN
	WRITE(1,342)X,Y,PHI3(IX,IY,IPLANE),RHO3(IX,IY,IPLANE)
	ENDIF
 
201	CONTINUE
	WRITE(1,343)
	ENDIF
 
	RETURN
	END
C-----------------------------------------------------------------------
	include 'jlpsub:project.for'
	include 'jlpsub:king_model.for'
