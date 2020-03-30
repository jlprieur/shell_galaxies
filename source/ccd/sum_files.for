C++**********************************************************************
C Program SUM_FILES to compute the sum of many files 
C JLP
C Version of 10-01-93 
C--*************************************************************************
	PROGRAM SUM_FILES
        REAL*4 CONSTANT
	INTEGER*4 PNTR_IM(30),PNTR_OUTPUT
	CHARACTER FILENAME(30)*40,OUTNAME*40,COMMENTS*80,ANSWER*1
        LOGICAL SUM
        INTEGER*4 IOPT,NPL,NL,NX,NY
	INTEGER*4 MADRID(1)
	COMMON /VMR/MADRID
 
10	FORMAT(A)
 
        WRITE(6,87)
87      FORMAT(' Program SUM_FILES   Version 10-01-93',/,
     1     ' Sum (or mean) of images, up to 30 frames')
 
C Option for the file formats :
	CALL JLP_BEGIN
	CALL JLP_INQUIFMT
 
C Input of files
	PRINT *,' Number of files : (max=30)'
	READ(5,*) NFILE
	
	 DO K=1,NFILE
	   PRINT *,' Enter the name of file #',K,':'
           READ(5,10) FILENAME(K)
         END DO

C Output file name:
	PRINT *,' Output file name:'
        READ(5,10) OUTNAME

C Mean or sum:
        PRINT *,' Sum or Mean [S or M] ?'
	READ(5,10) ANSWER
        IF(ANSWER.NE.'M'.AND.ANSWER.NE.'m')THEN
           SUM=.TRUE.
        ELSE
           SUM=.FALSE.
        ENDIF

C Reading first image:
	  CALL JLP_VM_READIMAG(PNTR_IM(1),NX1,NY1,
     1	FILENAME(1),COMMENTS)

C Getting memory space for the output :
	MEMS_OUTPUT=4*NX1*NY1
	CALL JLP_GETVM(PNTR_OUTPUT,MEMS_OUTPUT)

        CALL COPYFILE(MADRID(PNTR_OUTPUT),MADRID(PNTR_IM(1)),
     1   NX1,NY1,NX1)

C Free memory:
        CALL JLP_FREEVM(PNTR_IM(1))

C Now adding all frames successively:
	 DO K=2,NFILE
	  CALL JLP_VM_READIMAG(PNTR_IM(K),NX,NY,
     1	FILENAME(K),COMMENTS)
 
C Check if same size or not:
	   IF(NX1.NE.NX.OR.NY1.NE.NY)THEN
	     PRINT *,' Fatal error: the input images',
     1	' have not the same size'
	     STOP
	   ENDIF

C Now add new frame to OUTPUT:
          CALL ADDFILE(MADRID(PNTR_OUTPUT),MADRID(PNTR_IM(K)),
     1   NX1,NY1,NX1)

C Free memory:
           CALL JLP_FREEVM(PNTR_IM(K))
	 END DO
 
C Output file :
        IF(SUM)THEN
	  WRITE(COMMENTS,104) NFILE
104       FORMAT('Multiple sum of ',I3,' images with "sum_files" //')
        ELSE
	  WRITE(COMMENTS,105) NFILE
105       FORMAT('Mean of ',I3,' images with "sum_files" //')
          CONSTANT=FLOAT(NFILE)
          CALL CDIVIDE(MADRID(PNTR_OUTPUT),MADRID(PNTR_OUTPUT),
     1   NX1,NY1,NX1,CONSTANT)
        ENDIF

	CALL JLP_WRITEIMAG(MADRID(PNTR_OUTPUT),NX,NY,NX,
     1	OUTNAME,COMMENTS)
 
	CALL JLP_END
	STOP
	END
 
C**********************************************************************
C Subroutine COPYFILE
C**********************************************************************
        SUBROUTINE COPYFILE(OUTPUT,INPUT,NX,NY,IDIM)
        INTEGER NX,NY
        REAL OUTPUT(IDIM,*),INPUT(IDIM,*)
         DO J=1,NY
           DO I=1,NX
             OUTPUT(I,J)=INPUT(I,J)
           END DO
         END DO
        RETURN
        END
C**********************************************************************
C Subroutine CDIVIDE
C**********************************************************************
        SUBROUTINE CDIVIDE(OUTPUT,INPUT,NX,NY,IDIM,CONSTANT)
        INTEGER NX,NY
        REAL OUTPUT(IDIM,*),INPUT(IDIM,*),CONSTANT
         DO J=1,NY
           DO I=1,NX
             OUTPUT(I,J)=INPUT(I,J)/CONSTANT
           END DO
         END DO
        RETURN
        END
C**********************************************************************
C Subroutine ADDFILE
C**********************************************************************
        SUBROUTINE ADDFILE(OUTPUT,INPUT,NX,NY,IDIM)
        INTEGER NX,NY
        REAL OUTPUT(IDIM,*),INPUT(IDIM,*)
         DO J=1,NY
           DO I=1,NX
             OUTPUT(I,J)=OUTPUT(I,J)+INPUT(I,J)
           END DO
         END DO
        RETURN
        END
