C++ ...................................................................
C    Nom du programme   PIPCCD
C
C    Application  Transformation into integer direct access files from a
C		   tape written with the PDP/11 (PIP/BS=2048) : real*4 ou
C                 integer*2.
C
C    Auteur JLP       (from PIPCDCA, Ph. Pruniel)
C
C
C    Entrees
C
C    Sorties
C
C    MTACCESS :
C
C-- ...................................................................
	PROGRAM PIPCCD
	LOGICAL*1 TAB(32000),DENT(32000)
	INTEGER*2 IDENT(16000),ITAB(16000)
	INTEGER*2 ILUN,IFUNC,INCAR,ISKIP,IEOF
	REAL*4 VTAB(8000),VAL(2000)
	INTEGER*2 VMES(2000),NPL,NLI
	CHARACTER KDENT*80,DEVNAM*5
	CHARACTER ANS*1,NAME*80
	LOGICAL IREAL*1
	EQUIVALENCE(ITAB(1),TAB(1))
	EQUIVALENCE(IDENT(1),DENT(1))
	EQUIVALENCE(VTAB(1),TAB(1))
	DATA DEVNAM/'MUA0:'/
 
10	FORMAT(A)
13	FORMAT(A20)
 
	PRINT 122
122	FORMAT(' PROGRAM PIPCCD  -Version of April 16th 1987 -')
C     1	' I HOPE YOU HAVE TYPED $ASS MTA1: MUA0:'
 
C Assigning DEVNAM to logical unit of the tape:
	IFUNC=5
	CALL MTACCESS(ILUN,IFUNC,TAB,INCAR,ISKIP,IEOF,DEVNAM)
	IF(IEOF.NE.0)PRINT 14,IEOF
14	FORMAT(' WARNING : ERROR WITH MTACCESS , IEOF=',I4)
	PRINT *,DEVNAM,' associated to the tape driver'
 
C Rewinding the tape
	PRINT *,' Rewinding the tape'
	IFUNC=1
	CALL MTACCESS(ILUN,IFUNC,TAB,INCAR,ISKIP,IEOF,DEVNAM)
	IF(IEOF.NE.0)PRINT 14,IEOF
 
C Reading the first line (label)
C INCAR is not used here :
	IFUNC=2
C Initialization (not important, only to avoid 0-dimensioned arrays...
	INCAR=32000
	CALL MTACCESS(ILUN,IFUNC,DENT,INCAR,ISKIP,IEOF,DEVNAM)
	IF(IEOF.NE.0)PRINT 14,IEOF
	ENCODE(60,440,NAME(20:))(IDENT(K),K=3,30)
	NAME(30:80)=' '
	PRINT 113,NAME
113	FORMAT(' LABEL = ',A)
 
C ----------------------------------------------------------
111	IFUNC=2
	INCAR=32000
	CALL MTACCESS(ILUN,IFUNC,DENT,INCAR,ISKIP,IEOF,DEVNAM)
	IF(IEOF.NE.0)PRINT 14,IEOF
	ENCODE(60,440,NAME(20:))(IDENT(K),K=3,30)
	IND=INDEX(NAME,'.')
	  IF(IND.EQ.0)THEN
	  PRINT *,' I CANNOT FIND ANY SENSIBLE NAME'
	  PRINT *,' I TRY AGAIN '
	  GOTO 111
	  ENDIF
	NAME(IND+4:80)=' '
	PRINT 112,NAME
112	FORMAT(/,' NAME OF THE FILE = ',A)
 
C Skipping 1 tape mark (end of the name file)
C INCAR is not used here :
	IFUNC=3
	ISKIP=1
	CALL MTACCESS(ILUN,IFUNC,DENT,INCAR,ISKIP,IEOF,DEVNAM)
	IF(IEOF.NE.0)PRINT 14,IEOF
 
C Reading the first record : (output: INCAR,number of characters in record,
C and DENT, output array)
	IFUNC=2
	INCAR=32000
	CALL MTACCESS(ILUN,IFUNC,DENT,INCAR,ISKIP,IEOF,DEVNAM)
	IF(IEOF.NE.0)PRINT 14,IEOF
	ENCODE(60,440,KDENT(20:))(IDENT(K),K=1,30)
440	FORMAT(30A2)
	PRINT *,' HEADER :'
	PRINT 10,KDENT
	IF(KDENT(1:1).EQ.'R') THEN
	  IREAL=.TRUE.
	  PRINT *,' REAL*4 FILE'
	ELSE
	  IREAL=.FALSE.
	  PRINT *,' INTEGER*2 FILE'
	ENDIF
	NPL=IDENT(41)
	NLI=IDENT(42)
	PRINT 441,NPL,NLI
441	FORMAT(' NPL, NLI =',I5,2X,I5,/)
 
	PRINT *,' Do you want to read this file ? (Y)'
	READ(5,10) ANS
	IF(ANS.EQ.'N'.OR.ANS.EQ.'n')GO TO 300
 
C ------------------------------------------------------
C CREATION DU FICHIER DE SORTIE
	   IF(.NOT.IREAL)THEN
	   INISIZE=NPL*(NLI/256)
	   NREC=(NPL+1)/2
	   ELSE
	   INISIZE=NPL*(NLI/128)
	   NREC=NPL
	   ENDIF
 
	OPEN(5,FILE=NAME,STATUS='NEW',ACCESS='DIRECT',
     1	RECL=NREC,INITIALSIZE=INISIZE)
	WRITE(5'1)KDENT,NPL,NLI
C ------------------------------------------------------
	INCAR=32000
	IF(.NOT.IREAL)THEN
	  DO 100 JK=1,NLI
	  CALL MTACCESS(ILUN,2,TAB,INCAR,ISKIP,IEOF,DEVNAM)
	  IF(IEOF.NE.0)GO TO 300
	    DO M=1,NPL
	    VMES(M)=ITAB(M)
	    END DO
	  WRITE(5'JK+1) (VMES(M),M=1,NPL)
100	  CONTINUE
	ELSE
	   DO 200 JK=1,NLI
	   CALL MTACCESS(ILUN,2,TAB,INCAR,ISKIP,IEOF,DEVNAM)
	   IF(IEOF.NE.0)GO TO 300
	   WRITE(5'JK+1) (VTAB(M),M=1,NPL)
	   PRINT *,' JK,VTAB',JK
	     DO M=1,NPL
	     PRINT *,M,VTAB(M)
	     END DO
200	   CONTINUE
	ENDIF
 
	CLOSE(5)
 
C Skipping 2 tape marks (end of the data file and of another file...)
C INCAR is not used here :
300	IFUNC=3
	ISKIP=2
	IF(IEOF.NE.0)PRINT 14,IEOF
C If "end of file during read"
	  IF(IEOF.EQ.1)THEN
	  PRINT *,' END OF FILE WHILE READING'
	  ISKIP=1
	  ENDIF
C If wrong lenth for the file
	  IF(IEOF.EQ.2)THEN
	  PRINT *,' WRONG LENTH FOR THE FILE !'
	  ISKIP=1
	  ENDIF
	PRINT *,' Skipping ',ISKIP,' tape-marks'
	INCAR=32000
	CALL MTACCESS(ILUN,IFUNC,DENT,INCAR,ISKIP,IEOF,DEVNAM)
	IF(IEOF.NE.0)PRINT 14,IEOF
 
C---------------------------------------------------------------
	PRINT *,' Do you want to read the next file ? (Y)'
	READ(5,10) ANS
	IF(ANS.NE.'N'.AND.ANS.NE.'n')GO TO 111
 
C----------------------------------------------------------------
C Possibility of skipping some files
	PRINT *,' Number of files you want to skip ? (0=exit)'
	READ(5,*) IFIL
 
	IF(IFIL.NE.0)THEN
	INCAR=32000
	ISKIP=3*IFIL
	PRINT *,' Skipping ',ISKIP,' tape-marks'
	CALL MTACCESS(ILUN,IFUNC,DENT,INCAR,ISKIP,IEOF,DEVNAM)
	IF(IEOF.NE.0)PRINT 14,IEOF
 
C End of the tape :
	  IF(IEOF.EQ.4)THEN
	  PRINT *,' END OF THE TAPE'
	  GO TO 999
	  ENDIF
 
	GO TO 111
	ENDIF
 
999	STOP
	END
C--------------------------------------------------------------
	include 'jlpsub:mtaccess.for'
