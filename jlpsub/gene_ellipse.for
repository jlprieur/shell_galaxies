C************************************************************
C  Subroutine GENE_ELLIPSE to generate a portion of an ellipse
C  in the arrays XX(NPOINTS), YY(NPOINTS).   (NPOINTS<1000)
C  Called by FITELLI and compatible with EFIT
C
C NPOINT : (output) number of points in the arrays XX and YY
C THEMI,THEMA: limiting angles in degrees
C ARRAY Z : REAL*8 !
C Z(1): major axis
C Z(2): minor axis
C Z(3): THETA0 in radians
C Z(4): OX
C Z(5): OY
C
C IOPT=0 : COMPLETE ELLIPSE
C IOPT=1 : PART OF AN ELLIPSE
C
C JLP
C Version 02-02-96
C************************************************************
	SUBROUTINE GENE_ELLIPSE(XX,YY,NPOINT,Z,THEMI,THEMA,IOPT)
	REAL*4 XX(1),YY(1)
	REAL*8 Z(5),THEMI,THEMA
        REAL*4 THETA_RAD,THETA0_RAD
	REAL*4 OX,OY,THETA0,MAJAXIS,MINAXIS,PI
        INTEGER*4 IOPT
	CHARACTER*4 NCHAR
	PI=ACOS(-1.)
 
	MAJAXIS=SNGL(Z(1))
	MINAXIS=SNGL(Z(2))
	THETA0=SNGL(Z(3))*180./PI
	OX=SNGL(Z(4))
	OY=SNGL(Z(5))
 
	IF(IOPT.EQ.1)THEN
 
	ECCEN=MINAXIS/MAJAXIS
	THEMI0=THEMI-THETA0
	THEMA0=THEMA-THETA0
 
	THEMIN=ELLIP_LIMIT(THEMI0,ECCEN)
	THEMAX=ELLIP_LIMIT(THEMA0,ECCEN)
	IF(THEMIN.GT.THEMAX)THEMIN=THEMIN-360.
 
	ELSE
 
	THEMIN=0.
	THEMAX=360.
	
	ENDIF
 
C Generating the ellipse (Step=.361 degree , i.e. less than 1000 pts max)
	THETA=THEMIN
        THETA0_RAD=THETA0*PI/180.
	JJ=0
94	IF (THETA.GT.THEMAX) GOTO 95
	JJ=JJ+1
        THETA_RAD=THETA*PI/180.
	CALL ELLIP(THETA_RAD,XX(JJ),YY(JJ),OX,OY,THETA0_RAD,MAJAXIS,MINAXIS)
	THETA=THETA+0.361
	GOTO 94
 
95	NPOINT=JJ
	RETURN
	END
C*******************************************************************
C Computing XX,YY for a given value of THETA (degrees):
C*******************************************************************
	SUBROUTINE ELLIP(THETA_RAD,X,Y,OX,OY,THETA0_RAD,MAJAXIS,MINAXIS)
	REAL*4 OX,OY,THETA0,MAJAXIS,MINAXIS
        REAL*4 THETA_RAD,THETA0_RAD
C Ellipse centred on (0.,0.) and with major axis OX
	XXX=MAJAXIS*COS(THETA_RAD)
	YYY=MINAXIS*SIN(THETA_RAD)
C Rotation of THETA0 and translation to (OX,OY)
	X=OX+COS(THETA0_RAD)*XXX-SIN(THETA0_RAD)*YYY
	Y=OY+SIN(THETA0_RAD)*XXX+COS(THETA0_RAD)*YYY
	RETURN
	END
C*******************************************************************
C Function ELLIP_LIMIT to compute the angular limits of an arc of ellipse
C taking into account the effects of the eccentricity
C*******************************************************************
	REAL FUNCTION ELLIP_LIMIT(THEMI0,ECCEN)
        REAL PI,THEMI0_RAD
	PI=ACOS(-1.)
        THEMI0_RAD=THEMI0*PI/180.
	 IF(COS(THEMI0_RAD).EQ.0)THEN
	   ELLIP_LIMIT=THEMI0
	 ELSE
	   TGTMIN=TAN(THEMI0_RAD)/ECCEN
	   THEMIN=ATAN(TGTMIN)*180./PI
	   IF(COS(THEMI0_RAD).LT.0.)THEMIN=THEMIN+180.
	   ELLIP_LIMIT=THEMIN
	 ENDIF
	END
 
