C++*****************************************************************
C Set of subroutines for plotting curves (SPLOT package) 
C UNIX version.
C
C Contains:
C ARRONDI, NEWSCALE, NEWSCALE_ERROR,
C INDEX_MAX4,INDEX_MAX8, DECODE_SYMBOL
C
C Version: 15-07-94 
C JLP
C--*****************************************************************
	SUBROUTINE ARRONDI(X,IOPTION,NPAR)
C*******************************************************************
C IOPT=1 NEAREST LOWER VALUE
C IOPT=2 NEAREST UPPER VALUE
	IOPT=IOPTION
 
	IF(X.EQ.0..OR.ABS(X).GT.1.E+09)RETURN
 
	IF(X.LT.0.)THEN
	SIGN=-1.
	X=SIGN*X
C For negative values upper is replaced by lower and lower by upper
	   IF(IOPT.EQ.1)THEN
	   IOPT=2
	   ELSE
	   IOPT=1
	   ENDIF
	ELSE
	SIGN=1.
	ENDIF
 
C Gets XMULT : the power of ten such as X= ~ IWORK*XMULT
	ISAFE=0
	IWORK=IFIX(X)
	XMULT=1.
 
56	  IF(IWORK.LT.1)XMULT=XMULT*10.	
	  IF(IWORK.GE.10)XMULT=XMULT/10.	
	ISAFE=ISAFE+1
	IWORK=IFIX(X*XMULT)
	IF(IWORK.LT.1.OR.IWORK.GE.10 .AND. ISAFE.LE.1000) GOTO 56 

	  IF(ISAFE.GT.1000)THEN
	  PRINT *,' PROBLEMS IN "ARRONDI"...'
	  RETURN
	  ENDIF
 
C Keeps NPAR figures for the output value by changing XMULT
	IF(NPAR.GT.1)THEN
	WORK=XMULT*(10.**(NPAR-1))
	ELSE
	WORK=XMULT
	ENDIF
 
C Generates the output value
	IF(IOPT.EQ.1)THEN
	X=FLOAT(IFIX(X*WORK))/WORK
	ELSE
	X=FLOAT(IFIX(X*WORK)+1)/WORK
	ENDIF
 
	X=X*SIGN
	RETURN
	END
C***********************************************************************
C MINMAX_SCALE subroutine
C to calculate XMIN and XMAX of the entire array XINPUT(NMAX,KCURVE)
C***********************************************************************
        SUBROUTINE MINMAX_SCALE(XINPUT,NMAX,KCURVE,NPTS,XMIN,XMAX)
        IMPLICIT NONE
        INTEGER*4 NMAX
	REAL*4 XINPUT(NMAX,*),XMIN,XMAX
        INTEGER*4 NPTS(*),KCURVE,IK,I
 
C Determination of the minimum and maximum of all the curves
        XMIN=XINPUT(1,1)
        XMAX=XMIN
        DO IK=1,KCURVE
          DO I=1,NPTS(IK)
           XMIN=AMIN1(XINPUT(I,IK),XMIN)
           XMAX=AMAX1(XINPUT(I,IK),XMAX)
          END DO
        END DO
 
C Make sure that those values are different:
          IF(XMIN.EQ.XMAX) XMAX=XMIN+1.
 
        RETURN
        END
C***********************************************************************
C MINMAX_ERROR_SCALE subroutine
C to calculate XMIN and XMAX of the entire array XINPUT(NMAX,KCURVE)
C***********************************************************************
        SUBROUTINE MINMAX_ERROR_SCALE(XINPUT,XERROR,NMAX,KCURVE,NPTS,
     1                                XMIN,XMAX)
        IMPLICIT NONE
        INTEGER NMAX
	REAL*4 XINPUT(NMAX,*),XERROR(NMAX,*),XMIN,XMAX
        REAL*4 WORK1,WORK2
        INTEGER*4 NPTS(*),KCURVE,IK,I
 
C Determination of the minimum and maximum of all the curves
        XMIN=XINPUT(1,1)
        XMAX=XMIN
        DO IK=1,KCURVE
          DO I=1,NPTS(IK)
	    WORK1=XINPUT(I,IK)-XERROR(I,IK)
	    WORK2=XINPUT(I,IK)+XERROR(I,IK)
            XMIN=AMIN1(WORK1,XMIN)
            XMAX=AMAX1(WORK2,XMAX)
          END DO
        END DO
 
C Make sure that those values are different:
          IF(XMIN.EQ.XMAX) XMAX=XMIN+1.
 
        RETURN
        END
C***********************************************************************
C Subroutine to calculate FIRSTX and XLASTX 
C of the entire array XINPUT(NMAX,KCURVE)
C***********************************************************************
	SUBROUTINE NEWSCALE(XINPUT,NMAX,KCURVE,NPTS,FIRSTX,XLASTX)
        INTEGER*4 NPTS(*),KCURVE,NMAX
	REAL*4 XINPUT(NMAX,*),FIRSTX,XLASTX
 
C First : determination of the minimum and maximum of all the curves
        CALL MINMAX_SCALE(XINPUT,NMAX,KCURVE,NPTS,FIRSTX,XLASTX)
 
C Take a frame slightly larger (5% larger) for the display on other devices :
	  FIRSTX=FIRSTX-(XLASTX-FIRSTX)/20.
	  XLASTX=XLASTX+(XLASTX-FIRSTX)/20.
	  CALL ARRONDI(FIRSTX,1,4)
	  CALL ARRONDI(XLASTX,2,4)

C Make sure that those values are different:
          IF(FIRSTX.EQ.XLASTX) XLASTX=FIRSTX+1.
 
	RETURN
	END
C*****************************************************************
C Subroutine to calculate FIRSTX and XLASTX of the array
C XINPUT(NMAX,KCURVE) taking into account the error bars of the points.
C*****************************************************************
	SUBROUTINE NEWSCALE_ERROR(XINPUT,XERROR,NMAX,KCURVE,
     1	NPTS,FIRSTX,XLASTX)
        INTEGER*4 NMAX
	REAL*4 XINPUT(NMAX,*),XERROR(NMAX,*)
        INTEGER*4 NPTS(*)
 
C Determination of the minimum and maxima of all the curves :
        CALL MINMAX_ERROR_SCALE(XINPUT,XERROR,NMAX,KCURVE,NPTS,
     1                          FIRSTX,XLASTX)
 
C Take a frame slightly larger for the display :
	  FIRSTX=FIRSTX-(XLASTX-FIRSTX)/10.
	  XLASTX=XLASTX+(XLASTX-FIRSTX)/10.
	  CALL ARRONDI(FIRSTX,1,3)
	  CALL ARRONDI(XLASTX,2,4)
 
	RETURN
	END
 
C----------------------------------------------------------------
C	SUBROUTINE INDEX_MAX4(ARRAY,NPOINT,VALUE,INDEX)
C Search for the higher INDEX verifying ARRAY(INDEX).LE.VALUE
C---------------------------------------------------------------
	SUBROUTINE INDEX_MAX4(ARRAY,NPOINT,VALUE,INDEX)
	REAL*4 ARRAY(*),VALUE
	INTEGER*4 NPOINT,INDEX

	INDEX=1
	DO I=NPOINT,1,-1
	  IF(ARRAY(I).LE.VALUE)THEN
	  INDEX=I
	  RETURN
	  ENDIF
	END DO
	RETURN
	END
C----------------------------------------------------------------
C	SUBROUTINE INDEX_MAX8(ARRAY,NPOINT,VALUE,INDEX)
C Search for the higher INDEX verifying ARRAY(INDEX).LE.VALUE
C---------------------------------------------------------------
	SUBROUTINE INDEX_MAX8(ARRAY,NPOINT,VALUE,INDEX)
	REAL*8 ARRAY(*),VALUE
	INTEGER INDEX,NPOINT

	INDEX=1
	DO I=NPOINT,1,-1
	  IF(ARRAY(I).LE.VALUE)THEN
	  INDEX=I
	  RETURN
	  ENDIF
	END DO
	RETURN
	END
 
C--------------------------------------------------------------------
C Subroutine used by NEWPLOT and MONGO_PLOT to decode NCHAR(I):
C
C CONNECTED : Logical to check if the points are to be connected or not
C LTYPE : Line type (dashed, solid,...)
C--------------------------------------------------------------------
	SUBROUTINE DECODE_SYMBOL(NCHAR1,CONNECTED,ISYMB,
     1	SIZE,LTYPE)
        REAL*4 SIZE
        INTEGER*4 ISYMB,LTYPE
	CHARACTER NCHAR1*4,CWORK*2
	LOGICAL CONNECTED
 
C Decoding the first lettter :
	CWORK=NCHAR1(1:1)
	CONNECTED=.TRUE.
	IF(CWORK.GE.'0'.AND.CWORK.LE.'9')THEN
          READ(CWORK,500)ISYMB
	  CONNECTED=.FALSE.
	ENDIF
500	FORMAT(I1)
 
C Reading the second and third characters :
	CWORK=NCHAR1(2:3)
 
C Decoding the size of the symbol :
	SIZE=0.15
	IF(.NOT.CONNECTED)THEN
          READ(CWORK,502,ERR=501)ISIZE
502	  FORMAT(I1)
	  SIZE=FLOAT(ISIZE)*0.05
	ENDIF
 
C Decoding the line type (Cf. Mongo code):
501	LTYPE=0
	IF(CONNECTED.AND.CWORK(1:1).GT.'0'.AND.CWORK(1:1).LE.'7')THEN
          READ(CWORK(1:1),500)LTYPE
	ENDIF
 
	RETURN
	END
