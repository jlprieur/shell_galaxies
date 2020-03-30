C++***************************************************************
C TEST_OSD
C
C To test  jlp0_osd.c (in jlp/jlpacc.a) and osd.c (in midas/oslib.a library)
C Remember: osd is used as an interface between C/Fortran files...
C
C JLP
C Version 16-11-90
C--***************************************************************
	PROGRAM TEST_OSD
	PARAMETER (IDIM=1024,IDIM2=256)
	INTEGER*4 ARRAY(IDIM2)
	INTEGER*4 IMODE,IFID,ISTATUS
	CHARACTER FILENAME*60
10	FORMAT(A)
        CALL JLP_GETENV('JLP_PROMPT',10,FILENAME,ISTATUS)
        PRINT *,' Value of JLP_PROMPT: ',FILENAME
	PRINT *,' Name of the file ?'
	READ(5,10) FILENAME
C Opening the file:
	PRINT *,' IMODE (0=read 1=write 2=read_write 3=append) ?'
	READ(5,*) IMODE
	PRINT *,' IMODE:',IMODE
	CALL JLP_OSDOPEN(FILENAME,60,IMODE,IFID,ISTATUS)
	PRINT *,' IFID =',IFID
	IF(IFID.EQ.-1)THEN
	  PRINT *,' Error opening the file'
	  GOTO 999
	ENDIF
C Writing something in the file:
	IF (IMODE.GT.0)THEN
	      PRINT *,' Writing an integer array to the file:'
	      PRINT *,' Number of points ?'
	      READ(5,*) NPTS
	      DO I=1,NPTS
	        ARRAY(I)=I
	      END DO
	      NBYTES=NPTS*4
	      CALL JLP_OSDWRITE(IFID,ARRAY,NBYTES,ISTATUS)
	      IF(ISTATUS.EQ.-1)THEN
	        PRINT *,' Error writing in the file'
	      ENDIF
	ELSE
	      PRINT *,' Reading an integer array from the file:'
	      PRINT *,' Number of points ?'
	      READ(5,*) NPTS
	      NBYTES=NPTS*4
	      CALL JLP_OSDREAD(IFID,ARRAY,NBYTES,ISTATUS)
	      IF(ISTATUS.NE.0)THEN
	       PRINT *,' Error reading the file'
	      ELSE
	       DO I=1,NPTS
	        PRINT *,ARRAY(I)
	        END DO
	      ENDIF
	ENDIF
C Closing the file:
	CALL JLP_OSDCLOSE(IFID,ISTATUS)
	IF(ISTATUS.EQ.-1)THEN
	  PRINT *,' Error closing the file'
	  GOTO 999
	ENDIF
999	PRINT *,' end'
	STOP
	END
