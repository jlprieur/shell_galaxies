C++***************************************************************
C Program to compute some statistical parameters
C of an image, to be used as scaling values for further display
C
C JLP
C Version of 04-08-2008
C--***************************************************************
        PROGRAM AUTO_SCALE
        IMPLICIT NONE
C For 32-bit computers, PNTR should be declared as integer*4:
C        INTEGER*4 PNTR_RE,PNTR_IM
C For 64-bit computers, PNTR should be declared as integer*8:
        INTEGER*8 PNTR_IMAGE,PNTR_WORK
        INTEGER*4 MADRID(1)
        REAL*4 MEAN1,SIGMA1,MEAN2,SIGMA2,MIN1,MAX1
        REAL*4 SKY,SUM,R,SIGMA
        INTEGER*4 IXMI,IYMI,IXMA,IYMA,ISIZE,NX,NY
        INTEGER*4 IX1,IX2,IY1,IY2,NX2,NY2
        LOGICAL LIMITS
        CHARACTER NAME*40,COMMENTS*80,BUFFER*80
        COMMON /VMR/MADRID
 
C To be able to use parameters in command line
        CALL JLP_BEGIN
 
C Inquire the format (input/output) :
        CALL JLP_INQUIFMT
 
C Input :
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
        WRITE(6,*) 'OK, input file: >',NAME,'<'
        NX=0
        CALL JLP_VM_READIMAG(PNTR_IMAGE,NX,NY,NAME,COMMENTS)
        IF(NX.EQ.0) THEN
          CALL JLP_END
          STOP
        ENDIF
 
C debug: 
C        CALL DD(MADRID(PNTR_IMAGE))

C Check if limits:
        LIMITS=.FALSE.
        WRITE(6,60)
60      FORMAT(' Enter limits: IX1,IX2,IY1,IY2 (return if all image)')
        READ(5,10,ERR=98,END=98) BUFFER
10        FORMAT(A)
        READ(BUFFER,*,ERR=98,END=98) IX1,IX2,IY1,IY2
        IF((IX1.LT.0.OR.IX1.GT.NX.OR.IY1.LT.0.OR.IY1.GT.NY).OR.
     1     (IX2.LT.0.OR.IX2.GT.NX.OR.IY2.LT.0.OR.IY2.GT.NY))THEN
          WRITE(6,*)' AUTO_SCALE/Fatal error, wrong limits:',
     1               IX1,IX2,IY1,IY2
          CALL JLP_END
          STOP
        ENDIF
        LIMITS=.TRUE.
C
98      IF(LIMITS)THEN
          NX2=IX2-IX1+1
          NY2=IY2-IY1+1
          ISIZE=NX2*NY2*4
          CALL JLP_GETVM(PNTR_WORK,ISIZE) 
          CALL TRANSFER(MADRID(PNTR_IMAGE),MADRID(PNTR_WORK),
     1      NX,NY,IX1,IY1,NX2,NY2)
          NX=NX2
          NY=NY2
        ELSE
          IX1=1
          IY1=1
          PNTR_WORK=PNTR_IMAGE
        ENDIF

C debug: 
C        CALL DD(MADRID(PNTR_IMAGE))
C Statistics:
          CALL JLP_AUTO_SCALE(MADRID(PNTR_WORK),NX,NY,NX,MIN1,MAX1,
     1        MEAN1,MEAN2,SIGMA1,SIGMA2,IXMI,IYMI,IXMA,IYMA)
 
C Compute actual coordinates of maximum and minimum:
        IXMI=IXMI+IX1-1
        IXMA=IXMA+IX1-1
        IYMI=IYMI+IY1-1
        IYMA=IYMA+IY1-1

C Output:
        WRITE(6,87) MIN1,IXMI,IYMI,MAX1,IXMA,IYMA,MEAN1,
     1        SIGMA1,MEAN2,SIGMA2
87        FORMAT(' Minimum : ',1PG13.5,' at (ix,iy): ',I6,1X,I6,/,
     1        ' Maximum : ',1PG13.5,' at (ix,iy): ',I6,1X,I6,/,
     1        ' Mean and sigma with all points: ',1PG13.5,1X,1PG13.5,/,
     1        ' Mean and sigma with 3 sigma rejection: ',1PG13.5,
     1        1X,1PG13.5)

C Output:
        SUM=FLOAT(NX*NY)*MEAN1
        ISIZE=NX*NY
        WRITE(6,89) SUM,ISIZE
89        FORMAT(' Total sum: ',1PG13.5,'   Total number of points: ',I8)
 
C Background determination:
        IF(.NOT.LIMITS)THEN
          CALL AUTO_SKY(MADRID(PNTR_IMAGE),NX,NY,NX,SKY,SIGMA)
        ENDIF
 
        CALL JLP_END
        STOP
        END
C*************************************************************************
        SUBROUTINE  DD(ARRAY)
        REAL ARRAY(1)
         PRINT *,'DD/ DEBUG'
         PRINT *,'DD/ DEBUG: array(1)=',array(1)
        RETURN
        END
C*************************************************************************
        SUBROUTINE TRANSFER(FROM,TTOO,NX,NY,IX1,IY1,NX2,NY2)
        INTEGER*4 NX,NY,IX1,IY1,NX1,NX2
        REAL*4 FROM(NX,NY),TTOO(NX2,NY2)
C
        DO J=1,NY2
          JJ=IY1+J-1
          DO I=1,NX2
            II=IX1+I-1
            TTOO(I,J)=FROM(II,JJ)
          END DO
        END DO

        RETURN
        END
C*************************************************************************
        include 'jlpsub:jlp_auto_scale.for'
        include 'jlpsub:auto_sky.for'
