C++---------------------------------------------------------
C Program to multiply an image with a mask
C
C JLP 
C Version of 20-04-2008
C---------------------------------------------------------
	PROGRAM MASK
	PARAMETER (IDIM=512)
	REAL*4 IMAGE2(IDIM,IDIM),IMAGE1(IDIM,IDIM),MASK2(IDIM,IDIM)
        REAL*4 XLEVEL,RADIUS,RAD2,RR2,EQUIV_RADIUS
        INTEGER*4 IXC,IYC,NPOINTS,NX,NY,ICHOICE 
	CHARACTER ANS*1,NAME*40,COMMENTS*80
 
10	FORMAT(A)
 
	CALL JLP_BEGIN
C Inquire the format of the files :
	CALL JLP_INQUIFMT
 
	PRINT 85
85	FORMAT(' MENU :',/,
     1	' 1. Creation of a circular mask ',/,
     1	' 2. Creation of a mask with isophotes',/,
     1	' 3. Multiplying with a mask (file)',/,
     1	' 4. Multiplying with a circular mask',/,
     1	' 10. Exit',/,
     1	' Enter your choice : ',$)
	READ(5,*) ICHOICE
        IF(ICHOICE.GT.4) THEN
          CALL JLP_END
          STOP
        ENDIF
 
C------------------------------------------------------------------
C Circular mask:
C---------------
	IF(ICHOICE.EQ.1)THEN
200	PRINT *,' SIZE OF THE OUTPUT FILE : NX,NY ? (MAX=',IDIM,'x',IDIM,')'
	READ(5,*) NX,NY
        IF(NX.GT.IDIM.OR.NY.GT.IDIM) GOTO 200

	PRINT *,' Warning: range of coordinates from 1 to NX or NY...'
	PRINT *,' CENTRE IXC,IYC (0,0 for centered circle) AND RADIUS: ? '
	READ(5,*) IXC,IYC,RADIUS
        IF(IXC.EQ.0.AND.IYC.EQ.0)THEN
          IXC = NX/2 + 1
          IYC = NY/2 + 1
        ENDIF
	RAD2=RADIUS*RADIUS
 
C Erasing output file:
        DO IY=1,NY
          DO IX=1,NX
           IMAGE2(IX,IY)=0.
          ENDDO
        ENDDO

C Generating the array IMAGE2 :
        DO IY=1,NY
        DO IX=1,NX
	RR2=(IX-IXC)*(IX-IXC)+(IY-IYC)*(IY-IYC)
	   IF(RR2.LE.RAD2)THEN
	     IMAGE2(IX,IY)=1.
	   ENDIF
	END DO
	END DO
        WRITE(COMMENTS,11) IXC,IYC,RADIUS
11      FORMAT('Circular mask: IXC=',I4,' IYC=',I4,' Radius=',G9.2)
 
C------------------------------------------------------------------
C Isophotes:
C-----------
	ELSEIF(ICHOICE.EQ.2)THEN
 
C Input of the file:
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
	CALL JLP_READIMAG(IMAGE1,NX,NY,IDIM,NAME,COMMENTS)
	
C Input of the parameters :
	PRINT *,' Isophote level ?'
	READ(5,*) XLEVEL 

C Scanning the image :
        NPOINTS=0
	DO IY=1,NY
	 DO IX=1,NX
          IF(IMAGE1(IX,IY).GE.XLEVEL)THEN
	    IMAGE2(IX,IY)=1.
            NPOINTS=NPOINTS+1
          ENDIF
	 END DO
	END DO
 
        EQUIV_RADIUS=SQRT(FLOAT(NPOINTS)/3.14159)
        WRITE(6,22) NPOINTS,EQUIV_RADIUS
22      FORMAT('NPOINTS=',I5,' EQUIV_RADIUS=',G9.2)
        WRITE(COMMENTS,21) XLEVEL,NAME
21      FORMAT('Isophotal mask: Level=',G9.2,' from:',A)

C------------------------------------------------------------------
C Multiplication with a mask:
C----------------------------
	ELSEIF(ICHOICE.EQ.3)THEN
 
C Entering the mask :
        WRITE(6,*) 'Input mask: '
        READ(5,10) NAME
	CALL JLP_READIMAG(MASK2,NX,NY,IDIM,NAME,COMMENTS)
 
C Input of the file:
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
	CALL JLP_READIMAG(IMAGE1,NX,NY,IDIM,NAME,COMMENTS)
	
C Masking the image :
	DO IY=1,NY
	 DO IX=1,NX
	  IMAGE2(IX,IY)=IMAGE1(IX,IY)*MASK2(IX,IY)
	 END DO
	END DO
 
C------------------------------------------------------------------
C Multiplication with a circular mask:
C-------------------------------------
	ELSEIF(ICHOICE.EQ.4)THEN
 
C Input of the parameters :
	PRINT *,' Center : IX,IY and radius?'
	READ(5,*) IXC,IYC,RADIUS
 
C Input of the file:
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
	CALL JLP_READIMAG(IMAGE1,NX,NY,IDIM,NAME,COMMENTS)
 
C Masking the image :
	CALL MASK_CIRC(IMAGE1,IMAGE2,NX,NY,IDIM,IXC,IYC,RADIUS)
 
	COMMENTS=' '
        WRITE(COMMENTS,41) NAME,IXC,IYC,RADIUS
41      FORMAT(A,' masked: IXC=',I4,' IYC=',I4,' Radius=',G9.2)
C------------------------------------------------------------------
	ENDIF
 
C------------------------------------------------------------------
C Output :
        WRITE(6,*) 'Output file: '
        READ(5,10) NAME
	CALL JLP_WRITEIMAG(IMAGE2,NX,NY,IDIM,NAME,COMMENTS)

	CALL JLP_END
	STOP
	END
C---------------------------------------------------------
C Subroutine to multiply with a circular mask
C
C Input :
C IMAGE1(NX,NY)
C IXC,IYC : centre coordinates
C RADIUS : radius
C
C Output :
C IMAGE2(NX,NY)
C
C---------------------------------------------------------
	SUBROUTINE MASK_CIRC(IMAGE1,IMAGE2,NX,NY,IDIM,IXC,IYC,RADIUS)
	REAL*4 IMAGE1(IDIM,*),IMAGE2(IDIM,*)
        INTEGER*4 IX,IY,NX,NY,IDIM,IXC,IYC
        REAL*4 RR2,RADIUS
 
	RAD2=RADIUS*RADIUS
 
C Main loop :
	DO IY=1,NY
	DO IX=1,NX
	  RR2=(IX-IXC)*(IX-IXC)+(IY-IYC)*(IY-IYC)
	  IF(RR2.GT.RAD2)THEN
	    IMAGE2(IX,IY)=0.
          ELSE
	    IMAGE2(IX,IY)=IMAGE1(IX,IY)
	  ENDIF
	END DO
	END DO
	
	RETURN
	END
