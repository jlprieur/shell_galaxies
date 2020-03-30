    	PROGRAM MORPHOLOGY
C++-----------------------------------------------------------------
C  Program to compute various morphological operations :
C 	  IVOISY    : NB DE LIGNES DE LA FENETRE <21		
C         IVOISX    : NB DE COLONNES DE LA FENETRE <21
C
C Version of 29/08/87
C JLP
C------------------------------------------------------------------
	PARAMETER (IDIM=600)
	REAL*4 IMAGE(IDIM,IDIM)
	CHARACTER NAME*40,COMMENTS*80
 
10	FORMAT(A)
	CALL JLP_INQUIFMT
 
C Input of the image :
         WRITE(6,*) 'Input file: '
         READ(5,10) NAME
	 CALL JLP_READIMAG(IMAGE,NX,NY,IDIM,NAME,COMMENTS)
	 LEN=INDEX(NAME,'  ')-1
 
	PRINT 31
31	FORMAT(' MENU :',/,
     1	' 1. EROSION',/,
     1	' 2. DILATATION',/,
     1	' 3. FERMETURE (DILA-ERO)',/,
     1	' 4. OUVERTURE (ERO-DILA)',/,
     1	' 5. FILTRE DE LA MEDIANE',/,
     1	' 6. ELIMINATION DES POINTS ISOLES',/,
     1	' 7. FERMETURE ET ELIMINATION DES POINTS ISOLES',/,
     1	' 10. SORTIE ET STOCKAGE DU RESULTAT')
	READ(5,*) IOPT
 
C DONNEES TRAITEMENT
 121	PRINT *,' VOISINAGE (IMPAIR): NX1, NY1   (< 19) '
	READ(5,*) IVOISX,IVOISY
	IF( IVOISX.GT.19.OR.IVOISY.GT.19) GO TO 121
	IVOISY=2*(IVOISY/2)+1
	IVOISX=2*(IVOISX/2)+1
 
	IF(IOPT.EQ.1)THEN
	  CALL EROSION(IMAGE,NX,NY,IVOISX,IVOISY)
	  WRITE(COMMENTS,11)NAME(1:LEN)
11	  FORMAT('EROSION OF:',A,' //')
	ELSEIF(IOPT.EQ.2)THEN
	  CALL DILATATION(IMAGE,NX,NY,IVOISX,IVOISY)
	  WRITE(COMMENTS,21)NAME(1:LEN)
21	  FORMAT('DILATATION OF:',A,' //')
	ELSEIF(IOPT.EQ.3)THEN
	  CALL DILATATION(IMAGE,NX,NY,IVOISX,IVOISY)
	  CALL EROSION(IMAGE,NX,NY,IVOISX,IVOISY)
	  WRITE(COMMENTS,32)NAME(1:LEN)
32	  FORMAT('FERMETURE OF:',A,' //')
	ELSEIF(IOPT.EQ.4)THEN
	  CALL EROSION(IMAGE,NX,NY,IVOISX,IVOISY)
	  CALL DILATATION(IMAGE,NX,NY,IVOISX,IVOISY)
	  WRITE(COMMENTS,41)NAME(1:LEN)
41	  FORMAT('OUVERTURE OF:',A,' //')
	ELSEIF(IOPT.EQ.5)THEN
	  CALL MEDIAN_FILTER(IMAGE,NX,NY,IVOISX,IVOISY)
	  WRITE(COMMENTS,51)NAME(1:LEN)
51	  FORMAT('MEDIAN FILTER OF:',A,' //')
	ELSEIF(IOPT.EQ.6)THEN
	  CALL ISOLATED_POINTS(IMAGE,NX,NY,IVOISX,IVOISY)
	  WRITE(COMMENTS,61)NAME(1:LEN)
61	  FORMAT('ISOLATED POINTS EXTRACTED FROM:',A,' //')
	ELSEIF(IOPT.EQ.7)THEN
	  CALL DILATATION(IMAGE,NX,NY,IVOISX,IVOISY)
	  CALL EROSION(IMAGE,NX,NY,IVOISX,IVOISY)
	  CALL ISOLATED_POINTS(IMAGE,NX,NY,IVOISX,IVOISY)
	  WRITE(COMMENTS,71)NAME(1:LEN)
71	  FORMAT('FERMETURE AND ISOL. PTS EXTRACTED FROM:',A,' //')
	ENDIF
 
C Output of the image :
         WRITE(6,*) 'Output file: '
         READ(5,10) NAME
	 CALL JLP_WRITEIMAG(IMAGE,NX,NY,IDIM,NAME,COMMENTS)
 
	STOP ' FIN DE TRAITEMENT '
 
	END
	
C-------------------------------------------------------------------------
C Subroutine EROSION
C
C-------------------------------------------------------------------------
	SUBROUTINE EROSION(IMAGE,NX,NY,IVOISX,IVOISY)
	PARAMETER (IDIM=600)
	REAL*4 IMAGE(IDIM,IDIM),WORK(IDIM,IDIM)
 
	IY1=IVOISY/2
	IX1=IVOISX/2
	NX1=NX+2*IX1
	NY1=NY+2*IY1
 
C---------------------------------------------------------------------------
C Filling the working array : extension of the initial image
 
	DO J1=1,NY1
	 J=MIN(J1-IY1,NY)
	 J=MAX(J1-IY1,1)
	  DO I1=1,NX1
	    I=MIN(I1-IX1,NX)
	    I=MAX(I1-IX1,1)
	    WORK(I1,J1)=IMAGE(I,J)
	  END DO
	END DO
 
C------------------------------------------------------------------------
C Main loop :
	DO 40 IY=1,NY
	 DO 50 IX=1,NX
	  VALUE=1000000.
 
C BALAYAGE FENETRE
	   DO 60 J2=1,IVOISY
	    LJ=IY+J2-1
	     DO 70 I2=1,IVOISX
	       LI=IX+I2-1
	       VALUE=AMIN1(VALUE,WORK(LI,LJ))
70	     CONTINUE
60	   CONTINUE
 
	  IMAGE(IX,IY)=VALUE
 
50	 CONTINUE
40	CONTINUE
 
	RETURN
	END
C-------------------------------------------------------------------------
C Subroutine DILATATION
C
C-------------------------------------------------------------------------
	SUBROUTINE DILATATION(IMAGE,NX,NY,IVOISX,IVOISY)
	PARAMETER (IDIM=600)
	REAL*4 IMAGE(IDIM,IDIM),WORK(IDIM,IDIM)
 
	IY1=IVOISY/2
	IX1=IVOISX/2
	NX1=NX+2*IX1
	NY1=NY+2*IY1
 
C---------------------------------------------------------------------------
C Filling the working array : extension of the initial image
 
	DO J1=1,NY1
	 J=MIN(J1-IY1,NY)
	 J=MAX(J1-IY1,1)
	  DO I1=1,NX1
	   I=MIN(I1-IX1,NX)
	   I=MAX(I1-IX1,1)
	   WORK(I1,J1)=IMAGE(I,J)
	  END DO
	END DO
 
C------------------------------------------------------------------------
C Main loop :
	DO 40 IY=1,NY
	 DO 50 IX=1,NX
 	  VALUE=-1000000.
 
C BALAYAGE FENETRE
	  DO 60 J2=1,IVOISY
 	   LJ=IY+J2-1
	    DO 70 I2=1,IVOISX
	     LI=IX+I2-1
	     VALUE=MAX(VALUE,WORK(LI,LJ))
70	    CONTINUE
60	  CONTINUE
 
	  IMAGE(IX,IY)=VALUE
 
50	 CONTINUE
40	CONTINUE
 
	RETURN
	END
C-------------------------------------------------------------------------
C Subroutine MEDIAN_FILTER
C Version of 28/08/87
C-------------------------------------------------------------------------
	SUBROUTINE MEDIAN_FILTER(IMAGE,NX,NY,IVOISX,IVOISY)
	PARAMETER (IDIM=600)
	REAL*4 IMAGE(IDIM,IDIM),WORK(IDIM,IDIM)
	REAL*8 NEIGHBOUR(200)
 
	IY1=IVOISY/2
	IX1=IVOISX/2
	NPOINT=IVOISY*IVOISX
	ICENT=NPOINT/2 +1
	NX1=NX+2*IX1
	NY1=NY+2*IY1
 
C---------------------------------------------------------------------------
C Filling the working array : extension of the initial image
 
	DO J1=1,NY1
	 J=MIN(J1-IY1,NY)
	 J=MAX(J1-IY1,1)
	  DO I1=1,NX1
	   I=MIN(I1-IX1,NX)
	   I=MAX(I1-IX1,1)
	   WORK(I1,J1)=IMAGE(I,J)
	  END DO
	END DO
 
C------------------------------------------------------------------------
C Main loop :
	DO 40 IY=1,NY
	DO 50 IX=1,NX
 
	KK=0
 
	DO J=1,IVOISY
	 LJ=IY+J-1
	  DO I=1,IVOISX
	    LI=IX+I-1
	    KK=KK+1
	    NEIGHBOUR(KK)=WORK(LI,LJ)
	   END DO
	  END DO
 
C Sorting the values: 
	  CALL CLASS_1(NEIGHBOUR,NPOINT,ICENT,VALUE)
 
	 IMAGE(IX,IY)=VALUE
 
50	 CONTINUE
40	CONTINUE
 
	RETURN
	END
C-------------------------------------------------------------------------
C Subroutine ISOLATED_POINTS
C To remove all the local maxima or minima.
C-------------------------------------------------------------------------
	SUBROUTINE ISOLATED_POINTS(IMAGE,NX,NY,IVOISX,IVOISY)
	PARAMETER (IDIM=600)
	REAL*4 IMAGE(IDIM,IDIM),WORK(IDIM,IDIM)
        INTEGER*4 IX1,IY1,NX1,NY1,I,J,I1,J1,LJ,LI
        REAL*4 SUM,XMIN,XMAX,XWORK,XCENTER,XOUT
 
	IY1=IVOISY/2
	IX1=IVOISX/2
	NPOINT=IVOISX*IVOISY
	NX1=NX+2*IX1
	NY1=NY+2*IY1
 
C---------------------------------------------------------------------------
C Filling the working array : extension of the initial image
 
	DO J1=1,NY1
	J=MIN(J1-IY1,NY)
	J=MAX(J1-IY1,1)
	  DO I1=1,NX1
	  I=MIN(I1-IX1,NX)
	  I=MAX(I1-IX1,1)
	  WORK(I1,J1)=IMAGE(I,J)
	  END DO
	END DO
 
C------------------------------------------------------------------------
C Main loop :
	DO 40 IY=1,NY
	DO 50 IX=1,NX
	 XCENTER=IMAGE(IX,IY)
	 XMIN=XCENTER
	 XMAX=XCENTER
	 SUM=0.0
 
C BALAYAGE FENETRE
	  DO 60 J2=1,IVOISY
	  LJ=IY+J2-1
	    DO 70 I2=1,IVOISX
	      LI=IX+I2-1
	      SUM=SUM+WORK(LI,LJ)
	      XMIN=MIN(XMIN,WORK(LI,LJ))
	      XMAX=MAX(XMAX,WORK(LI,LJ))
70	    CONTINUE
60	  CONTINUE
 
	  XOUT=(SUM-XCENTER)/(NPOINT-1)
	  IF(XOUT.EQ.XMIN.OR.XOUT.EQ.XMAX)THEN
	    IMAGE(IX,IY)=XOUT
	  ENDIF
 
50	 CONTINUE
40	CONTINUE
 
	RETURN
	END
C*******************************************************************
C Subroutine to sort the array XX into ascending order
C
C Output : VALUE= ICENT th value of the array
C*******************************************************************
	SUBROUTINE CLASS_1(XX,NPOINT,ICENT,VALUE)
	PARAMETER (IDIM=2000)
	REAL*4 XX(*),X1(200)
	INTEGER*4 INDEX(IDIM)	
	ISAFE=0
C
C Initialization of INDEX
C
	DO J=1,NPOINT
	  INDEX(J)=J
	END DO
C
C Method of the "bubble"
C
 
600	INVERS=0
 
	DO J=1,NPOINT-1
	  I1=INDEX(J)
	  I2=INDEX(J+1)
	  IF(XX(I1).GT.XX(I2))THEN
	   INDEX(J+1)=I1
	   INDEX(J)=I2
	   INVERS=INVERS+1
	  ENDIF	
	END DO
	ISAFE=ISAFE+1
 
	IF(ISAFE.GT.+1.000E+06)THEN
	  PRINT *,' I GIVE UP : TOO MANY LOOPS !!!'
	  RETURN
	ENDIF
 
	IF(INVERS.NE.0) GO TO 600
 
C Permutation of the values in the arrays XX
	DO I=1,NPOINT
	  KK=INDEX(I)
	  X1(KK)=XX(I)
	END DO
 
	VALUE=X1(ICENT)
	
	RETURN
	END
