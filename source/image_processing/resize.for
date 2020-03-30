C++***************************************************************
C Program to change the size of an image file
C enlarge or reduce it (for FFT, when the image is not square)
C Can keep the center in the middle of the enlarged frame.
C
C JLP
C Version of 17-02-99
C Version of 17-11-2010 
C--***************************************************************
	PROGRAM RESIZE 
C Linux 32 bits:
C       INTEGER*4 IN1,OUT2
C Linux 64 bits: I switch to INTEGER*8:
        INTEGER*8 IN1,OUT2
C
	INTEGER*4 IOPT,IOPT1,MADRID(1)
	CHARACTER NAME*40,COMMENTS*80
	COMMON /VMR/MADRID
 
C To get the possibility of command line
	CALL JLP_BEGIN
 
C Inquire the format (input/output) :
	CALL JLP_INQUIFMT
 
        PRINT 81
81	FORMAT(' Program RESIZE to enlarge (or reduce) an image ',
     1      '(for FFT for instance). JLP. Version 20-04-93',/,
     1      ' Possibility of keeping the center in the middle...',/,
     1      'Menu:',/,' 1: Larger/smaller window centered on the image',/,
     1      ' 2: Larger window not centered',/,
     1      ' 3: Smaller window not centered',/,
     1      ' Enter your option: (10 to exit) ',/)
        READ(5,*,ERR=99) IOPT
        IF(IOPT.LT.1.OR.IOPT.GT.3) GOTO 99

C Input :
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
10      FORMAT(A)
	CALL JLP_VM_READIMAG(IN1,NX1,NY1,NAME,COMMENTS)
 
        IF(IOPT.EQ.1.OR.IOPT.EQ.2)THEN
	  PRINT *,' Size of the output image: (should be larger or smaller)'
          READ(5,*) NX2,NY2
          IF(NX2.GE.NX1.AND.NY2.GE.NY1)THEN
             IOPT1=1
             PRINT *,' OK: larger size, hence enlargement'
          ELSEIF(NX1.GE.NX2.AND.NY1.GE.NY2)THEN
             IF(IOPT.EQ.2)THEN
               PRINT *,' Fatal: smaller size, not compatible with this option'
             ENDIF
             IOPT1=2
             PRINT *,' OK: smaller size, hence reduction'
          ELSE
             PRINT 25
25           FORMAT(' Wrong window: SHOULD BE LARGER (OR SMALLER)',
     1              ' for BOTH AXES X AND Y')
             GOTO 99
          ENDIF
        ELSEIF(IOPT.EQ.3)THEN
	  PRINT *,' Window limits (limits included): Xstart,Xend,Ystart,Yend'
          READ(5,*) IX1,IX2,IY1,IY2 
          NX2=IX2-IX1+1
          NY2=IY2-IY1+1
          IF(NX2.LE.0.OR.NY2.LE.0.OR.IX2.GT.NX1.OR.IY2.GT.NY1)THEN
             PRINT *,' Wrong limits: IX1,IX2,IY1,IY2',IX1,IX2,IY1,IY2
             GOTO 99
          ENDIF
        ENDIF

C Handle Option IOPT=2:
        IF(IOPT.EQ.2)THEN
C        
         IF(NX2.GT.NX1)THEN
           PRINT *,' Number of colomns to be added before actual first column'
           READ(5,*) IX1
          ELSE
           IX1=0
          ENDIF
C
          IF(NY2.GT.NY1)THEN
           PRINT *,' Number of lines to be added before actual first line'
           READ(5,*) IY1
          ELSE
           IY1=0
          ENDIF
C
        ENDIF

C Get memory space:
	ISIZE=NX2*NY2*4
	CALL JLP_GETVM(OUT2,ISIZE)

C Processing
        IF(IOPT.EQ.1.AND.IOPT1.EQ.1)THEN
	  WRITE(COMMENTS,84) NAME(1:40)
84	  FORMAT(' Enlarged (centered) version of ',A40)
	  CALL ENLARGE(MADRID(IN1),NX1,NY1,MADRID(OUT2),NX2,NY2)
        ELSEIF(IOPT.EQ.1.AND.IOPT1.EQ.2)THEN
	  WRITE(COMMENTS,85) NAME(1:40)
85	  FORMAT(' Smaller (centered) version of ',A40)
	  CALL REDUCE(MADRID(IN1),NX1,NY1,MADRID(OUT2),NX2,NY2)
        ELSEIF(IOPT.EQ.2)THEN
	  WRITE(COMMENTS,87) NAME(1:40)
87	  FORMAT(' Enlarged version of ',A40)
	  CALL ENLARGE2(MADRID(IN1),NX1,NY1,MADRID(OUT2),NX2,NY2)
        ELSE
	  WRITE(COMMENTS,86) IX1,IY1,NAME(1:40)
86	  FORMAT(' Subwindow starting at ',2(1X,I4),' of ',A40)
          CALL EXTRACT(MADRID(IN1),NX1,NY1,MADRID(OUT2),NX2,NY2,IX1,IY1)
        ENDIF

C Output :
        WRITE(6,*) 'Output file: '
        READ(5,10) NAME
	  CALL JLP_WRITEIMAG(MADRID(OUT2),NX2,NY2,NX2,
     1	NAME,COMMENTS)
 
99	CALL JLP_END
	STOP
	END
C*********************************************************************
C Offset in X: IX1
C Offset in Y: IY1
	SUBROUTINE ENLARGE2(IN1,NX1,NY1,OUT2,NX2,NY2,IX1,IY1)
	INTEGER NX1,NY1,NX2,NY2
	INTEGER II,JJ,I,J
	REAL*4 IN1(NX1,*),OUT2(NX2,*)

C Erases the output image:
	DO J=1,NY2
          DO I=1,NX2
	    OUT2(I,J)=0
          ENDDO
        ENDDO

C Fills with the input image:
	DO J=1,NY1
	  JJ=J+IY1
          DO I=1,NX1
	    II=I+IX1
	    OUT2(II,JJ)=IN1(I,J)
          ENDDO
        ENDDO

	RETURN
	END
C*********************************************************************
C Offset in X: IX1 = (NX2-NX1)/2
C Offset in Y: IY1 = (NY2-NY1)/2
	SUBROUTINE ENLARGE(IN1,NX1,NY1,OUT2,NX2,NY2)
	INTEGER NX1,NY1,NX2,NY2
	INTEGER II,JJ,I,J
	REAL*4 IN1(NX1,*),OUT2(NX2,*)

C Erases the output image:
	DO J=1,NY2
          DO I=1,NX2
	    OUT2(I,J)=0
          ENDDO
        ENDDO

C Offset in X and Y: 
	IC=(NX2-NX1)/2
	JC=(NY2-NY1)/2

C Fills with the input image:
	DO J=1,NY1
	  JJ=J+JC
          DO I=1,NX1
	    II=I+IC
	    OUT2(II,JJ)=IN1(I,J)
          ENDDO
        ENDDO

	RETURN
	END
C*********************************************************************
	SUBROUTINE REDUCE(IN1,NX1,NY1,OUT2,NX2,NY2)
	INTEGER NX1,NY1,NX2,NY2
	INTEGER II,JJ,I,J
	REAL*4 IN1(NX1,*),OUT2(NX2,*)

C Shift in X and Y: 
	IC=(NX1-NX2)/2
	JC=(NY1-NY2)/2

C Fills with the input image:
	DO J=1,NY2
	  JJ=J+JC
          DO I=1,NX2
	    II=I+IC
	    OUT2(I,J)=IN1(II,JJ)
          ENDDO
        ENDDO

	RETURN
	END
C*********************************************************************
	SUBROUTINE EXTRACT(IN1,NX1,NY1,OUT2,NX2,NY2,IX1,IY1)
	INTEGER NX1,NY1,NX2,NY2
	INTEGER II,JJ,I,J,IX1,IY1
	REAL*4 IN1(NX1,*),OUT2(NX2,*)

C Fills with the input image:
	DO J=1,NY2
	  JJ=J+IY1-1
          DO I=1,NX2
	    II=I+IX1-1
	    OUT2(I,J)=IN1(II,JJ)
          ENDDO
        ENDDO

	RETURN
	END
