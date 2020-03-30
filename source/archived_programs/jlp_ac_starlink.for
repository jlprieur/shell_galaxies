C----------------------------------------------------------
C Set of subroutines to access BDF files
C Contains JLP_RDIBDF,  JLP_WRIBDF, JLP_WRRBDF, JLP_WRRBDF1,
C      JLP_VM_RDBDF, JLP_VM_RDIBDF, JLP_VM_RDIBDF1
C
C Corrected bug when too many files (Nov93): 
C Suppress barrier at 10 and put it to 100
C
C JLP
C Version of 05-11-93
C----------------------------------------------------------
C Subroutine JLP_RDIBDF
C To copy a INTEGER*2 file into a real array IMAGE(IDIM,IDIM)
C----------------------------------------------------------
	SUBROUTINE JLP_RDIBDF(IMAGE,NX,NY,IDIM,
	1	FILENAME,COMMENTS,ISTAT)
	REAL*4 IMAGE(IDIM,IDIM)
	CHARACTER COMMENTS*(*)
	CHARACTER NAME*7,FILENAME*(*),INTEG*7
	INTEGER*4 NAXIS(2)
	DATA NAME/'INPUT  '/
	COMMON/JLP_ACCESSFMT/IFMT_IN,IFMT_OUT,NFILE_IN,NFILE_OUT
 
	ISTAT=0
        IF(NFILE_IN.LT.10)THEN 
            CALL ITOC(NFILE_IN,NAME(6:6),IERR)
        ELSE
            I = (NFILE_IN/10)
            I = NFILE_IN - I * 10
            CALL ITOC(I,NAME(7:7),IERR)
        ENDIF
	IF(IERR.NE.0)THEN
           PRINT *,' JLP_RDIBDF/Fatal error with ITOC (>100 files)'
           STOP
        ENDIF
 
C Integer image:
	 NTYPE=102
 
11	CALL JLP_RDIMAG(NAME,NTYPE,2,NAXIS,ND,NPT1,FILENAME,ISTAT)
	 IF(ISTAT.NE.0) THEN
	  CALL FRDATA(NAME,IERR)
	  CALL CNPAR(NAME,IERR)
	  RETURN
	 ENDIF
 
C Return if ND (Actual number of dimensions) is not 2
	IF(ND.NE.2)THEN
	  ISTAT=1
	  PRINT *,' Error in JLP_RDIMAG : number of dimensions =',ND
	  RETURN
	ENDIF
 
	NX=NAXIS(1)
	NY=NAXIS(2)
 
C Read the comments (Number of elements= 1)
	CALL RDDSCR(NAME,'TITLE',1,COMMENTS,ICOM,IERR)
	IF(IERR.NE.0)COMMENTS=' '
 
	CALL JLP_RDIBDF1(%VAL(NPT1),IMAGE,NX,NY,IDIM)
	CALL FRDATA(NAME,IERR)
	CALL CNPAR(NAME,IERR)
 
	RETURN
	END
C*********************************************************
	SUBROUTINE JLP_RDIBDF1(INPUT,OUTPUT,NX,NY,IDIM)
	INTEGER*2 INPUT(NX,NY)
	REAL*4 OUTPUT(IDIM,IDIM)
	DO 1 J=1,NY
	 DO 1 I=1,NX
	   OUTPUT(I,J)=REAL(INPUT(I,J))
1	CONTINUE
	RETURN
	END
C----------------------------------------------------------
C Subroutine JLP_WRIBDF
C To copy a real array IMAGE(IDIM,IDIM) in a INTEGER file
C----------------------------------------------------------
	SUBROUTINE JLP_WRIBDF(IMAGE,NX,NY,IDIM,
	1	FILENAME,COMMENTS,ISTAT)
	REAL*4 IMAGE(IDIM,IDIM)
	CHARACTER COMMENTS*(*)
	CHARACTER NAME*8,FILENAME*(*),INTEG*7
	INTEGER*4 NAXIS(2)
	DATA NAME/'OUTPUT  '/
	COMMON/JLP_ACCESSFMT/IFMT_IN,IFMT_OUT,NFILE_IN,NFILE_OUT
 
	ISTAT=0
        IF(NFILE_OUT.LT.10)THEN
            CALL ITOC(NFILE_OUT,NAME(7:7),IERR)
        ELSE
            I = (NFILE_OUT/10)
            I = NFILE_OUT - I * 10
            CALL ITOC(I,NAME(8:8),IERR)
        ENDIF
	IF(IERR.NE.0)THEN
           PRINT *,' JLP_WRIBDF/Fatal error with ITOC (>100 files)'
           STOP
        ENDIF
 
C Integer file:
	 NTYPE=102
 
	NAXIS(1)=NX
	NAXIS(2)=NY
 
11	CALL JLP_WRIMAG(NAME,NTYPE,NAXIS,2,NPT1,FILENAME,ISTAT)
	 IF(ISTAT.NE.0) THEN
	  CALL FRDATA(NAME,IERR)
	  CALL CNPAR(NAME,IERR)
	  RETURN
	 ENDIF
 
C Write the comments (ELEMENTS= 1)
	CALL WRDSCR(NAME,'TITLE',COMMENTS,1,IERR)
	IF(IERR.NE.0)PRINT *,' Error when writing the comments'
 
	CALL JLP_WRIBDF1(IMAGE,%VAL(NPT1),NX,NY,IDIM)
	CALL FRDATA(NAME,IERR)
	CALL CNPAR(NAME,IERR)
 
	RETURN
	END
C*********************************************************
	SUBROUTINE JLP_WRIBDF1(INPUT,OUTPUT,NX,NY,IDIM)
	REAL*4 INPUT(IDIM,IDIM)
	INTEGER*2 OUTPUT(NX,NY)
	DO 1 J=1,NY
	  DO 1 I=1,NX
	    OUTPUT(I,J)=NINT(INPUT(I,J))
1	CONTINUE
	RETURN
	END
C----------------------------------------------------------
C Subroutine JLP_RDRBDF
C To copy a REAL file into a real array IMAGE(IDIM,IDIM)
C----------------------------------------------------------
	SUBROUTINE JLP_RDRBDF(IMAGE,NX,NY,IDIM,
	1	FILENAME,COMMENTS,ISTAT)
	REAL*4 IMAGE(IDIM,IDIM)
	CHARACTER COMMENTS*(*)
	CHARACTER NAME*7,FILENAME*(*),INTEG*7
	INTEGER*4 NAXIS(2)
	DATA NAME/'INPUT  '/
	COMMON/JLP_ACCESSFMT/IFMT_IN,IFMT_OUT,NFILE_IN,NFILE_OUT
 
	ISTAT=0
        IF(NFILE_IN.LT.10)THEN
            CALL ITOC(NFILE_IN,NAME(6:6),IERR)
        ELSE
            I = (NFILE_IN/10)
            I = NFILE_IN - I * 10
            CALL ITOC(I,NAME(7:7),IERR)
        ENDIF
	IF(IERR.NE.0)THEN
           PRINT *,' JLP_RDRBDF/Fatal error with ITOC (>100 files)'
           STOP
        ENDIF
 
C Real file:
	 NTYPE=204
 
11	CALL JLP_RDIMAG(NAME,NTYPE,2,NAXIS,ND,NPT1,FILENAME,ISTAT)
	 IF(ISTAT.NE.0) THEN
	  CALL FRDATA(NAME,IERR)
	  CALL CNPAR(NAME,IERR)
	  RETURN
	 ENDIF
 
C Return if ND (Actual number of dimensions) is not 2
	IF(ND.NE.2)THEN
	  ISTAT=1
	  PRINT *,' Error in RDIMAG : number of dimensions =',ND
	  RETURN
	ENDIF
 
	NX=NAXIS(1)
	NY=NAXIS(2)
 
C Read the comments (Number of elements= 1)
	CALL RDDSCR(NAME,'TITLE',1,COMMENTS,ICOM,IERR)
	IF(IERR.NE.0)COMMENTS=' '
 
	CALL JLP_RDRBDF1(%VAL(NPT1),IMAGE,NX,NY,IDIM)
	CALL FRDATA(NAME,IERR)
	CALL CNPAR(NAME,IERR)
 
	RETURN
	END
C*********************************************************
	SUBROUTINE JLP_RDRBDF1(INPUT,OUTPUT,NX,NY,IDIM)
	REAL*4 INPUT(NX,NY)
	REAL*4 OUTPUT(IDIM,IDIM)
	DO 1 J=1,NY
	 DO 1 I=1,NX
 	   OUTPUT(I,J)= INPUT(I,J)
1	CONTINUE
	RETURN
	END
C----------------------------------------------------------
C Subroutine JLP_WRRBDF
C To copy a real array IMAGE(IDIM,IDIM) in a REAL file
C----------------------------------------------------------
	SUBROUTINE JLP_WRRBDF(IMAGE,NX,NY,IDIM,
	1	FILENAME,COMMENTS,ISTAT)
	REAL*4 IMAGE(IDIM,IDIM)
	CHARACTER COMMENTS*(*)
	CHARACTER NAME*8,FILENAME*(*),INTEG*7
	INTEGER*4 NAXIS(2)
	DATA NAME/'OUTPUT  '/
	COMMON/JLP_ACCESSFMT/IFMT_IN,IFMT_OUT,NFILE_IN,NFILE_OUT
 
	ISTAT=0
        IF(NFILE_OUT.LT.10)THEN
            CALL ITOC(NFILE_OUT,NAME(7:7),IERR)
        ELSE
            I = (NFILE_OUT/10)
            I = NFILE_OUT - I * 10
            CALL ITOC(I,NAME(8:8),IERR)
        ENDIF
	IF(IERR.NE.0)THEN
           PRINT *,' JLP_WRRBDF/Fatal error with ITOC (>100 files)'
           STOP
        ENDIF
 
C Real file:
	 NTYPE=204
 
	NAXIS(1)=NX
	NAXIS(2)=NY
 
11	CALL JLP_WRIMAG(NAME,NTYPE,NAXIS,2,NPT1,FILENAME,ISTAT)
	 IF(ISTAT.NE.0) THEN
	  CALL FRDATA(NAME,IERR)
	  CALL CNPAR(NAME,IERR)
	  RETURN
	 ENDIF
 
C Write the comments (ELEMENTS= 1)
	CALL WRDSCR(NAME,'TITLE',COMMENTS,1,IERR)
	IF(IERR.NE.0)PRINT *,' Error when writing the comments'
 
	CALL JLP_WRRBDF1(IMAGE,%VAL(NPT1),NX,NY,IDIM)
	CALL FRDATA(NAME,IERR)
	CALL CNPAR(NAME,IERR)
 
	RETURN
	END
C*********************************************************
	SUBROUTINE JLP_WRRBDF1(INPUT,OUTPUT,NX,NY,IDIM)
	REAL*4 INPUT(IDIM,IDIM)
	REAL*4 OUTPUT(NX,NY)
	DO 1 J=1,NY
	DO 1 I=1,NX
	OUTPUT(I,J)= INPUT(I,J)
1	CONTINUE
	RETURN
	END
C----------------------------------------------------------
C Subroutine JLP_VM_RDBDF ---> pointer of IMAGE(NX,NY)
C For real*4 images, no transfer is done.
C----------------------------------------------------------
	SUBROUTINE JLP_VM_RDBDF(PNTR_IMAGE,NX,NY,FILENAME,
	1	COMMENTS,ISTAT)
	INTEGER*4 NAXIS(2),NX,NY,ISTAT,PNTR_IMAGE
	CHARACTER*(*) FILENAME,COMMENTS
	CHARACTER NAME*7
	DATA NAME/'INPUT  '/
	COMMON/JLP_ACCESSFMT/IFMT_IN,IFMT_OUT,NFILE_IN,NFILE_OUT
 
	ISTAT=0
        IF(NFILE_IN.LT.10)THEN
            CALL ITOC(NFILE_IN,NAME(6:6),IERR)
        ELSE
            I = (NFILE_IN/10)
            I = NFILE_IN - I * 10
            CALL ITOC(I,NAME(7:7),IERR)
        ENDIF
	IF(IERR.NE.0)THEN
           PRINT *,' JLP_VM_RDBDF/Fatal error with ITOC (>100 files)'
           STOP
        ENDIF
 
C Real:
	NTYPE=204
 
11	CALL JLP_RDIMAG(NAME,NTYPE,2,NAXIS,ND,NPT1,FILENAME,ISTAT)
	 IF(ISTAT.NE.0) THEN
	   CALL FRDATA(NAME,IERR)
	   CALL CNPAR(NAME,IERR)
	   RETURN
	 ENDIF
 
C Return if ND (Actual number of dimensions) is not 2
	IF(ND.NE.2)THEN
	  ISTAT=1
	  PRINT *,' Error in RDIMAG : number of dimensions =',ND
	  RETURN
	ENDIF
 
	PNTR_IMAGE=NPT1
	NX=NAXIS(1)
	NY=NAXIS(2)
 
 
C Read the comments (Number of elements= 1)
	CALL RDDSCR(NAME,'TITLE',1,COMMENTS,ICOM,IERR)
	IF(IERR.NE.0)COMMENTS=' '
 
 
	RETURN
	END 
C----------------------------------------------------------
C Subroutine JLP_VM_RDIBDF ---> pointer of IMAGE(NX,NY)
C to read integer*2 BDF files without increasing their size !
C----------------------------------------------------------
	SUBROUTINE JLP_VM_RDIBDF(PNTR_IMAGE,NX,NY,FILENAME,
	1	COMMENTS,ISTAT)
	CHARACTER*(*) COMMENTS,FILENAME
	CHARACTER NAME*7
	INTEGER*4 MADRID(1),PNTR_IMAGE,ISIZE,NAXIS(2)
	DATA NAME/'INPUT  '/
	COMMON/JLP_ACCESSFMT/IFMT_IN,IFMT_OUT,NFILE_IN,NFILE_OUT
	COMMON /VMR/MADRID
 
	ISTAT=0
        IF(NFILE_IN.LT.10)THEN
            CALL ITOC(NFILE_IN,NAME(6:6),IERR)
        ELSE
            I = (NFILE_IN/10)
            I = NFILE_IN - I * 10
            CALL ITOC(I,NAME(7:7),IERR)
        ENDIF
	IF(IERR.NE.0)THEN
           PRINT *,' JLP_VM_RDIBDF/Fatal error with ITOC (>100 files)'
           STOP
        ENDIF
 
C Integer:
	NTYPE=102
 
11	CALL JLP_RDIMAG(NAME,NTYPE,2,NAXIS,ND,NPT1,FILENAME,ISTAT)
	 IF(ISTAT.NE.0) THEN
	   CALL FRDATA(NAME,IERR)
	   CALL CNPAR(NAME,IERR)
	   RETURN
	 ENDIF
 
C Return if ND (Actual number of dimensions) is not 2
	IF(ND.NE.2)THEN
	  ISTAT=1
	  PRINT *,' Error in RDIMAG : number of dimensions =',ND
	  RETURN
	ENDIF
 
	NX=NAXIS(1)
	NY=NAXIS(2)
 
C Getting memory space for the array "IMAGE":
	ISIZE=4*NX*NY
	CALL JLP_GETVM(PNTR_IMAGE,ISIZE)
 
C Read the comments (Number of elements= 1)
	CALL RDDSCR(NAME,'TITLE',1,COMMENTS,ICOM,IERR)
	IF(IERR.NE.0)COMMENTS=' '
 
C Copying the input file to the array "IMAGE"
	CALL JLP_VM_RDIBDF1(%VAL(NPT1),NX,NY,MADRID(PNTR_IMAGE))
 
C Freeing the memory space for the input file, and the logical parameter NAME
	CALL FRDATA(NAME,IERR)
	CALL CNPAR(NAME,IERR)
 
	RETURN
	END
C*********************************************************
	SUBROUTINE JLP_VM_RDIBDF1(A,NX,NY,B)
	REAL*4 B(NX*NY)
	INTEGER*2 A(NX*NY)
	  DO I=1,NX*NY
	    B(I)=FLOAT(A(I))
	  END DO
	RETURN
	END
