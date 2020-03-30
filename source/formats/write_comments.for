C++***************************************************************
C Program to write comments on a 2D image file 
C
C Syntax:
C       RUNS WRITE_COMMENTS infile comments outfile 
C Example:
C       RUNS WRITE_COMMENTS TEST1 "This is a test" TEST2 
C
C Warning: infile and outfile should be different!
C
C JLP
C Version of 26-06-94
C--***************************************************************
	PROGRAM WRITE_COMMENTS 
	INTEGER*4 PNTR_IMAGE,MADRID(1)
	INTEGER NX,NY,IFMT_IN,IFMT_OUT
	CHARACTER NAME*40,OLD_COMMENTS*80,NEW_COMMENTS*80
	COMMON /VMR/MADRID

10	FORMAT(A)
 
C To get the possibility of command line
	CALL JLP_BEGIN
 
C Inquire the format (input/output) :
	CALL JLP_INQUIFMT
 
C Input :
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
        CALL JLP_VM_READIMAG(PNTR_IMAGE,NX,NY,NAME,OLD_COMMENTS)
 
C Comments:
        WRITE(6,102) OLD_COMMENTS(1:80)
102	FORMAT(' Old comments: ',/,A80)
        WRITE(6,103)
103	FORMAT(' New comments ? ')
        READ(5,10) NEW_COMMENTS

C Work on descriptors (For E. Davoust ...)
        CALL TEST_DESCRIP

C Write new descriptors
        CALL WRITE_DESCRIP

C Output :
        WRITE(6,*) 'Output file: '
        READ(5,10) NAME
	CALL JLP_WRITEIMAG(MADRID(PNTR_IMAGE),NX,NY,NX,
     1	NAME,NEW_COMMENTS)
 
	CALL JLP_END
	STOP
	END
C-----------------------------------------------------------------------
C Calls read/write descriptor routines
C-----------------------------------------------------------------------
        SUBROUTINE WRITE_DESCRIP
        INTEGER*4 IN_DESCR,OUT_DESCR,LENGTH,ISTATUS,IOPT
	CHARACTER BUFFER*100,DESCR_NAME*20
        CHARACTER CDESCR(1024)*1
        COMMON/JLP_DESCRIPTORS/IN_DESCR,OUT_DESCR,CDESCR

10      FORMAT(A)

C Descriptors: 
	WRITE(6,202) 
202	FORMAT(' Old descriptors: ')
        CALL DISP_DESCR(CDESCR)

88      WRITE(6,203)
203     FORMAT(' Menu: ',/,
     1         2X,' 1. Read the value of a descriptor',/, 
     1         2X,' 2. Update an old descriptor',/, 
     1         2X,' 3. Add a new descriptor',/, 
     1         2X,' 4. Display current state of descriptors',/, 
     1         2X,' 10. Return to main program',/,
     1         ' Enter your choice: ')
        READ(5,*) IOPT

C
C IOPT=1 Read the descriptor:
        IF(IOPT.EQ.1)THEN
	  WRITE(6,204) 
204       FORMAT(' Descriptor name to be read :')
          READ(5,10) DESCR_NAME 
	  CALL JLP_RDESCR(DESCR_NAME,BUFFER,LENGTH,ISTATUS)
	  WRITE(6,205) LENGTH,ISTATUS,DESCR_NAME,BUFFER(:LENGTH)
205       FORMAT(' Length =',I4,2X,'Status = ',I1,2X,A,' = ',A)
          GOTO 88
C
C IOPT=2 Update an old descriptor:
        ELSEIF(IOPT.EQ.2)THEN
	  WRITE(6,207) 
207       FORMAT(' Descriptor name to be updated :')
          READ(5,10) DESCR_NAME 
	  WRITE(6,208) 
208       FORMAT(' New value :')
          BUFFER=' '
          READ(5,10) BUFFER 
          LENGTH=INDEX(BUFFER,'          ')
	  CALL JLP_WDESCR(DESCR_NAME,BUFFER,LENGTH,ISTATUS)
	  WRITE(6,205) LENGTH,ISTATUS,DESCR_NAME,BUFFER(:LENGTH)
          GOTO 88

C
C IOPT=3  Add a descriptor:
        ELSEIF(IOPT.EQ.3)THEN
	  WRITE(6,209) 
209       FORMAT(' Name of new descriptor :')
          READ(5,10) DESCR_NAME 
	  WRITE(6,210) 
210       FORMAT(' New value :')
          BUFFER=' '
          READ(5,10) BUFFER 
	  WRITE(6,211) 
211       FORMAT(' Field length (for the values) :')
          READ(5,*) LENGTH
	  CALL JLP_WDESCR(DESCR_NAME,BUFFER,LENGTH,ISTATUS)
	  WRITE(6,205) LENGTH,ISTATUS,DESCR_NAME,BUFFER(:LENGTH)
          GOTO 88
C
C IOPT=4  Display current state of descriptors:
        ELSEIF(IOPT.EQ.4)THEN
	  WRITE(6,221)
221        FORMAT(' Current values of descriptors: ')
          CALL DISP_DESCR(CDESCR)
          GOTO 88
        ENDIF

C Display new value of descriptors:
	WRITE(6,51)
51      FORMAT(' New values of descriptors (final): ')
        CALL DISP_DESCR(CDESCR)

        RETURN
        END
C-----------------------------------------------------------------------
C To display the descriptors
C Packs of 78 characters (to avoid carriage return...):
C-----------------------------------------------------------------------
        SUBROUTINE DISP_DESCR(CDESCR)
        PARAMETER(ILENGTH=78)
        CHARACTER CDESCR(*)*1,BUFFER*(ILENGTH)
        INTEGER*4 K,I,IMIN,IMAX,KMAX
C 
C Display the whole content of the descriptors:
        IMIN=1
        KMAX=1+1024/ILENGTH
        DO K=1,KMAX
           IMAX=IMIN+ILENGTH-1
           IMAX=MIN(IMAX,1024)
           WRITE(BUFFER,52) (CDESCR(I),I=IMIN,IMAX)
52         FORMAT(78A)
           IF(BUFFER.NE.' ')THEN
              WRITE(6,53) BUFFER(1:ILENGTH)
53            FORMAT(78A)
           ELSE
              GOTO 99 
           ENDIF
           IMIN=IMAX+1
        END DO

99      RETURN
        END
C-----------------------------------------------------------------------
C Test to tune read/write descriptor routines
C-----------------------------------------------------------------------
        SUBROUTINE TEST_DESCRIP
        INTEGER*4 IN_DESCR,OUT_DESCR,LENGTH,ISTATUS
	CHARACTER BUFFER*102
        CHARACTER CDESCR(1024)*1
        COMMON/JLP_DESCRIPTORS/IN_DESCR,OUT_DESCR,CDESCR

C If not set, do it here:
	OUT_DESCR=1

C Descriptors: 
	WRITE(6,202) 
202	FORMAT(' Old descriptors: ')
        CALL DISP_DESCR(CDESCR)

C Tests:
	BUFFER=' $FATUM=1.3 3.4 5.6544 23.443 $NATUM=12123 3221 332 @'
        DO I=1,100
          CDESCR(I)=BUFFER(I:I)
        END DO
	WRITE(6,50) (CDESCR(I),I=1,1023)
50      FORMAT(' New descriptors: ')
        CALL DISP_DESCR(CDESCR)

C First step: read the descriptors:
	CALL JLP_RDESCR('FATUM',BUFFER,LENGTH,ISTATUS)
	WRITE(6,205) LENGTH,ISTATUS,'FATUM',BUFFER(:LENGTH)
205       FORMAT(' Length =',I4,2X,'Status = ',I1,2X,A,' = ',A)
C
	CALL JLP_RDESCR('NATUM',BUFFER,LENGTH,ISTATUS)
	WRITE(6,205) LENGTH,ISTATUS,'NATUM',BUFFER(:LENGTH)

C Then changes the descriptors, add a new descriptor:
	WRITE(BUFFER,'(a)')'Value of irtum'
        LENGTH=20
	CALL JLP_WDESCR('IRTUM',BUFFER,LENGTH,ISTATUS)
	WRITE(6,205) LENGTH,ISTATUS,'IRTUM',BUFFER(:LENGTH)

C And update an old one:
	WRITE(BUFFER,'(a)')'New value of fatum'
	CALL JLP_WDESCR('FATUM',BUFFER,LENGTH,ISTATUS)
	WRITE(6,205) LENGTH,ISTATUS,'FATUM',BUFFER(:LENGTH)

	WRITE(6,51) 
51      FORMAT(' New descriptors (final): ')
        CALL DISP_DESCR(CDESCR)

	OUT_DESCR=1

        RETURN
        END
C----------------------------------------------------------------
C	include 'jlpsub:jlp_access.for'
