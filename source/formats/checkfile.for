C++***************************************************************
C Program CHECKFILE
C
C To perform operations on BDF or integer CCD files, and check their content.
C
C JLP
C Version of 23-08-93
C--**************************************************************
	PROGRAM CHECKFILE
	CHARACTER REP*1,IN_NAME*40,IN_COMMENTS*80
	CHARACTER OUT_NAME*40,OUT_COMMENTS*80
	INTEGER*4 MADRID(1),PNTR_1,PNTR_2
	INTEGER*4 IPIXSTART,IPIXEND,LISTART,LIEND,NX2,NY2 
	INTEGER*4 NX,NY,IOPT,INPUT_FILE
	COMMON /VMR/MADRID
 
10	FORMAT(A)
 
	CALL JLP_BEGIN
	INPUT_FILE = 0
 
	PRINT *,' Program checkfile'
	PRINT *,' Version of 23-08-93'
 
C Inquires about the format of the files :
	CALL JLP_INQUIFMT
 
C Start by entering a file:
        IOPT = 1
        IF(IOPT.EQ.1) GOTO 100

C Then displays the menu:
5	PRINT 80
80	FORMAT(' OPTIONS :',/,
     1	' 1 : Loading a new file',/,
     1	' 3 : Operations on this file',/,
     1	' 4 : Inspect this file (mean,display,...)',/,
     1	' 5 : Creation of a smaller file (giving pixel limits)',/,
     1	' 6 : Creation of a smaller file (giving center and size)',/,
C     1	' 7 : Creation of a larger file',/,
C     1	' 9 : Creation of a mask (with a threshold)',/,
     1	' 10 : Exit',/,
     1	' Enter the option you want ?',$)
	READ(5,*) IOPT

	IF(IOPT.GE.3.AND.IOPT.LE.6.AND.INPUT_FILE.EQ.0)THEN
	  PRINT 65
65	FORMAT(/,
     1         '***** ERROR: no input file has been entered! ****',/)
	  GOTO 5
	ENDIF

C************************************************************
C Option 1 : Loading a file
C************************************************************
100	IF(IOPT.EQ.1) THEN
          WRITE(6,*) 'Input file: '
          READ(5,10) IN_NAME
	  CALL JLP_VM_READIMAG(PNTR_1,NX,NY,IN_NAME,IN_COMMENTS)
	  INPUT_FILE = 1
	  GO TO 5
	ENDIF
 
C***************************************************************
C Option 3 : OPERATIONS DIVERSES ET STOCKAGE DANS
C	UN FICHIER
C***************************************************************
	IF(IOPT.EQ.3) THEN
C Getting virtual memory:
	  CALL JLP_GETVM(PNTR_2,NX*NY*4)
 
	  CALL OPER(MADRID(PNTR_1),MADRID(PNTR_2),NX,NY,ISTAT)
 
C**** Output file if something has been done:
	  IF(ISTAT.EQ.0)THEN
            WRITE(6,*) 'Output file: '
            READ(5,10) OUT_NAME
	    WRITE(OUT_COMMENTS,1003) IN_NAME(1:12)
1003         FORMAT('Operations from',A)
 	    CALL JLP_WRITEIMAG(MADRID(PNTR_2),NX,NY,NX,OUT_NAME,OUT_COMMENTS)
	  ELSE
	    GO TO 5
	  ENDIF
 
	ENDIF
 
C**************************************************************
C	OPTION 4 : INSPECTING THE INPUT FILE
C**************************************************************
	IF(IOPT.EQ.4) THEN
	  CALL INSPECT_IMAGE(MADRID(PNTR_1),NX,NY,NX,IN_NAME,IN_COMMENTS)
	  GO TO 5
	ENDIF
 
C***********************************************************
C Subframe with pixel limits: 
C***********************************************************
	IF(IOPT.EQ.5)THEN
	  PRINT *,' Enter xstart,ystart,xend,yend (inclusive)'
	  READ(5,*) IPIXSTART,LISTART,IPIXEND,LIEND
	  NX2=IPIXEND-IPIXSTART+1
	  NY2=LIEND-LISTART+1
 
C Getting virtual memory:
	  CALL JLP_GETVM(PNTR_2,NX2*NY2*4)
 
C Calling DECOUP:
	  CALL DECOUP(MADRID(PNTR_1),NX,NY,MADRID(PNTR_2),
     1	NX2,NY2,IPIXSTART,IPIXEND,LISTART,LIEND)
 
C**** Output file **************
          WRITE(6,*) 'Output file: '
          READ(5,10) OUT_NAME
	  WRITE(OUT_COMMENTS,1005) IN_NAME(1:12),IPIXSTART,LISTART,IPIXEND,LIEND
1005      FORMAT('From ',A,4(1X,I4),' (IX1,IY1,IX2,IY2)')
 	  CALL JLP_WRITEIMAG(MADRID(PNTR_2),NX2,NY2,NX2,
     1	OUT_NAME,OUT_COMMENTS)
	ENDIF

C***********************************************************
C Subframe with center and size 
C***********************************************************
        IF(IOPT.EQ.6)THEN
          PRINT *,' Please note that here, we adopt FFT convention' 
          PRINT *,'   (For example, center of 128x128 frame is at (65,65))' 
          PRINT *,' Enter in_xcenter,in_ycenter,out_nx,out_ny'
          READ(5,*) IXCENT,IYCENT,NX2,NY2
          IPIXSTART=IXCENT-NX2/2
          IPIXSTART=MAX(1,IPIXSTART)
          LISTART=IYCENT-NY2/2
          LISTART=MAX(1,LISTART)
          IPIXEND=IPIXSTART+NX2-1
          IPIXEND=MIN(IPIXEND,NX)
          LIEND=LISTART+NY2-1
          LIEND=MIN(LIEND,NY)
          WRITE(6,1005) IN_NAME(1:12),IPIXSTART,LISTART,IPIXEND,LIEND
C Test if troncation because of boundaries of initial frame:
          I=IPIXEND-IPIXSTART+1
          IF(I.LT.NX2)THEN
            WRITE(6,1004) I
1004        FORMAT(' Warning: new frame is smaller than expected: NX=',I5) 
            NX2=I
          ENDIF
          I=LIEND-LISTART+1
          IF(I.LT.NY2)THEN
            WRITE(6,1008) I
1008        FORMAT(' Warning: new frame is smaller than expected: NY=',I5) 
            NY2=I
          ENDIF

C Getting virtual memory:
          CALL JLP_GETVM(PNTR_2,NX2*NY2*4)

C Calling DECOUP:
          CALL DECOUP(MADRID(PNTR_1),NX,NY,MADRID(PNTR_2),
     1  NX2,NY2,IPIXSTART,IPIXEND,LISTART,LIEND)

C**** Output file **************
          WRITE(6,*) 'Output file: '
          READ(5,10) OUT_NAME
          WRITE(OUT_COMMENTS,1005) IN_NAME(1:12),IPIXSTART,LISTART,IPIXEND,LIEND
          CALL JLP_WRITEIMAG(MADRID(PNTR_2),NX2,NY2,NX2,
     1  OUT_NAME,OUT_COMMENTS)
        ENDIF

C***********************************************************
C Enlargement :
C***********************************************************
	IF(IOPT.EQ.7)THEN
	  NX1=NX
	  NY1=NY
c	  CALL ENLARGE(IMAGE1,IMAGE2,NX1,NY1,IDIM)
          WRITE(6,*) 'Output file: '
          READ(5,10) OUT_NAME
	  WRITE(OUT_COMMENTS,1007) IN_NAME(1:12)
1007      FORMAT('Larger version of ',A)
c	  CALL JLP_WRITEIMAG(IMAGE2,NX1,NY1,IDIM,OUT_NAME,OUT_COMMENTS)
	ENDIF
	
C***********************************************************
	IF(IOPT.EQ.9)THEN
c	  CALL MASK(IMAGE1,IMAGE2,NX,NY,IDIM)
          WRITE(6,*) 'Output file: '
          READ(5,10) OUT_NAME
	  WRITE(OUT_COMMENTS,1009)
1009      FORMAT('Mask')
c	  CALL JLP_WRITEIMAG(IMAGE2,NX,NY,IDIM,OUT_NAME,OUT_COMMENTS)
	ENDIF
 
C***********************************************************
 
999	CLOSE(1)
	CALL JLP_END
	STOP
	END
C**************************************************************
C	Subroutine DECOUP
C	En entree NX et NY : dimensions de l'image de depart
C**************************************************************
	SUBROUTINE DECOUP(IMAGE1,NX,NY,IMAGE2,NX2,NY2,
     1	IPIXSTART,IPIXEND,LISTART,LIEND)
	REAL*4 IMAGE1(NX,NY),IMAGE2(NX2,NY2)
 
	DO 1 J=LISTART,LIEND
	  DO 2 I=IPIXSTART,IPIXEND
	    I2=I-IPIXSTART+1
	    J2=J-LISTART+1
	    IMAGE2(I2,J2)=IMAGE1(I,J)
2	  CONTINUE
1	CONTINUE
 
	RETURN
	END
C**************************************************************
C	Subroutine ENLARGE
C	En entree NX et NY : dimensions de l'image de depart
C  	En sortie : dimensions de l'image traitee
C**************************************************************
	SUBROUTINE ENLARGE(IMAGE1,IMAGE2,NX,NY,IDIM)
	REAL*4 IMAGE1(IDIM,*),IMAGE2(IDIM,*)
 
	PRINT *,'LINES YOU WANT TO ADD BEFORE THE FIRST,'
	PRINT *,' AND AFTER THE LAST ?'
	READ(5,*) LIBEFOR,LIAFTER
 
	DO ILIN=1,LIBEFOR
	DO IPIX=1,NX
	IMAGE2(IPIX,ILIN)=0.
	END DO
	END DO
 
	DO 1 ILIN1=1,NY
	DO 2 IPIX=1,NX
	ILIN2=ILIN1+LIBEFOR
	IMAGE2(IPIX,ILIN2)=IMAGE1(IPIX,ILIN1)
2	CONTINUE
1	CONTINUE
 
	NY1=NY+LIBEFOR
	NY2=NY1+LIAFTER
 
	DO ILIN=NY1,NY2
	DO IPIX=1,NX
	IMAGE2(IPIX,ILIN)=0.
	END DO
	END DO
 
	NY=NY2
	RETURN
	END
C**************************************************************
C	Subroutine MASK
C NX et NY : Size of the image
C IMAGE1 : Input
C IMAGE2 : output
C**************************************************************
	SUBROUTINE MASK(IMAGE1,IMAGE2,NX,NY,IDIM)
	REAL*4 IMAGE1(IDIM,*),IMAGE2(IDIM,*)
	PRINT *,' THRESHOLD ?'
	READ(5,*) THRESHOLD
	PRINT 20
20	FORMAT(' MENU :',/,
     1	' 1. (UPPER VALUES=1.)',/,
     1	' 2. (LOWER VALUES=1.)',/,
     1	' ENTER THE OPTION YOU WANT : ',$)
	READ(5,*) IOPT
 
	IF(IOPT.EQ.1)THEN
 
	DO 101 I=1,NY
	DO 102 J=1,NX
	  IF(IMAGE1(J,I).GT.THRESHOLD) THEN
	  IMAGE2(J,I)=1.
	  ELSE
	  IMAGE2(J,I)=0.
	  ENDIF
102	CONTINUE
101	CONTINUE
 
	ELSE
 
	DO 201 I=1,NY
	DO 202 J=1,NX
	  IF(IMAGE1(J,I).GT.THRESHOLD) THEN
	  IMAGE2(J,I)=0.
	  ELSE
	  IMAGE2(J,I)=1.
	  ENDIF
202	CONTINUE
201	CONTINUE
 
	ENDIF
 
	RETURN
	END
C---------------------------------------------------------------------------
C Operations in a  file
C--------------------------------------------------------------------------
	SUBROUTINE OPER(IMAGE1,IMAGE2,NY,NX,ISTAT)
	REAL*4 IMAGE1(NY,NX),IMAGE2(NY,NX)
 
	ISTAT=0
 
30	PRINT 31
31	FORMAT(' OPTIONS POSSIBLES :',/,
     1	' 1 : CONVERSION DE L''IMAGE EN C1*(IMAGE-SKY) + C2 ',/,
     1	' 2 : CONVERSION EN C1*LOG10(IMAGE-SKY) + C2',/,
     1	' 10 : RETOUR AU MENU',/,
     1	' OPTION CHOISIE ?',$)
	READ(5,*) IOPT1
 
C If no option has been chosen, return with error status
	IF(IOPT1.GT.2)THEN
	  ISTAT=2
	  RETURN
	ENDIF
 
	PRINT *,'VALUES OF C1,C2,SKY ?  ?'
	READ(5,*) C1,C2,SKY
 
 	IF(IOPT1.EQ.1) THEN
	  DO J=1,NY
	    DO I=1,NX
	    IMAGE2(I,J)=C1*(IMAGE1(I,J)-SKY)+C2
	    END DO
	  END DO
 
	ELSEIF(IOPT1.EQ.2)THEN
	  DO J=1,NY
	    DO I=1,NX
	    VV=IMAGE1(I,J)-SKY
	      IF(VV.LT.0.)THEN
	       IMAGE2(I,J)=35.
	      ELSE
	       IMAGE2(I,J)=C1*ALOG10(VV)+C2
	      ENDIF
	    END DO
	  END DO
	ENDIF
 
 
	RETURN
	END
C----------------------------------------------------------------------
	include 'jlpsub:inspect_image.for'
