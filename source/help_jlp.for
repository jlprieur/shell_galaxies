C++************************************************************
C Program HELP_JLP
C On line facility to get information about JLP programs
C
C Format of "SOURCE:JLP_PROG.DOC" or "SOURCE:JLP_SUB.DOC"
C where the information is read :
C
C # NAME : name of the module
C % THEME : theme of the application
C   ( 1. Image processing, 2. Spectro/profiles, 3. Modelling/Mathematics
C   4. Transfer/access to different formats, 5. Graphic/display
C   6. Miscellaneous, 7. Aperture_synthesis, 
C   8 : Celestial mechanics/atmospheric dispersion)
C & SDIRECT : source directory (location of the directory)
C @ CTYPE : type of the module (P program, S subroutine, D doc)
C > COMMENT : Comments
C $ LANGUAGE : Fortran or C
C
C JLP
C Version of 13-02-90
C--************************************************************
	PROGRAM HELP_JLP
	PARAMETER (IDIM=300,NLINES=30,NTHEMES=8)
	LOGICAL PROGR,FOUND
	INTEGER*4 ICOMM(IDIM),THEME(IDIM)
	INTEGER LENGTH,ISTATUS,ILAST
	CHARACTER*30 NAME(IDIM),CTYPE(IDIM),SDIRECT(IDIM)
	CHARACTER COMM(NLINES,IDIM)*80,LANG(IDIM)*1
	CHARACTER CTHEME*1,ANS*1,SDIR*30,LANGUAGE*1
	CHARACTER NAME1*30,SRC_DIR*30,FULL_NAME*60
 
C Catalogue:
	COMMON /HLPCAT/NAME,COMM,ICOMM,CTYPE,SDIRECT,LANG,THEME,NMOD
 
10	FORMAT(A)
	PRINT 11
11	FORMAT( ' "HELP_JLP" to use JLP programs and routines',/,
     1	' Version of 23-04-91',/)
 
	LENGTH=7
C Environement variable JLPSRC should be set to the
C directory containing the "source" files, e.g. "/home/prieur/src"
        CALL JLP_GETENV("JLPSRC",LENGTH,SRC_DIR,ISTATUS)
C JLP98: (linux) 
        PRINT *,' JLP_GETENV/Status=',ISTATUS,' src dir: ',SRC_DIR(1:20)

C Last significant character is in ILASTth position:
        I=1
194      IF(((SRC_DIR(I:I).GE.'a'.AND.SRC_DIR(I:I).LE.'z')
     1      .OR.(SRC_DIR(I:I).GE.'A'.AND.SRC_DIR(I:I).LE.'Z')
     1      .OR.(SRC_DIR(I:I).GE.'0'.AND.SRC_DIR(I:I).LE.'9')
     1      .OR.SRC_DIR(I:I).EQ.'/'.OR.SRC_DIR(I:I).EQ.'_'
     1      .OR.SRC_DIR(I:I).EQ.'-').AND.(I.LT.LEN(SRC_DIR)-1))THEN
	 I=I+1
	 GOTO 194
	 ENDIF
         ILAST=I-1
        PRINT *,' JLPSRC dir: ',SRC_DIR(1:ILAST)

C------------------------------------------------------------
C Input of the catalogue :
	WRITE(6,12)
12	FORMAT(' Do you want to read the information',/,
     1	' about the programs (P),',
     1	' or about the subroutines (S) ?')
	READ(5,10) ANS
 
	PROGR=(ANS.NE.'S'.AND.ANS.NE.'s')
	IF(PROGR)THEN
	  FULL_NAME=SRC_DIR(1:ILAST)//'/source/jlp_prog.doc'
	  PRINT *,' Full name: >',FULL_NAME,'<'
	  OPEN(1,FILE=FULL_NAME,STATUS='OLD',ACCESS='SEQUENTIAL',
     1	ERR=9999)
	ELSE
	  FULL_NAME=SRC_DIR(1:ILAST)//'/source/jlp_subr.doc'
	  PRINT *,' Full name: >',FULL_NAME,'<'
	  OPEN(1,FILE=FULL_NAME,STATUS='OLD',ACCESS='SEQUENTIAL',
     1	ERR=9999)
	ENDIF
 
C Reading the catalogue :
	CALL HLP_READCAT
	CLOSE(1)
	PRINT 80,NMOD
80	FORMAT(2X,' Number of entries :',I5)
        IF(NMOD.GT.IDIM)THEN
          WRITE(6,28) IDIM
28        FORMAT(' Fatal error: maximum capacity is',I4,' entries')
        ENDIF
 
C********************************************************************
C Options :
C********************************************************************
 
88	PRINT 20
20	FORMAT(70('*'),/,' Description of the themes :',/,
     1	' 1. Image processing',8x,
     1	' 4. Transfer/access to different formats',/,
     1	' 2. Spectro/profiles',8X,
     1	' 5. Graphic/display',/,
     1	' 3. Modelling/mathematics',3X,
     1	' 6. Miscellaneous',6X,
     1	' 7. Aperture synthesis',/,
     1	' 8. Celestial mechanics/atmospheric disp.',/,
     1	' E. EXIT   P. PRINT',/,
     1	' Enter the number of the theme : ',$)
	READ(5,10) CTHEME
 
C Exit:
	IF(CTHEME.EQ.'E'.OR.CTHEME.EQ.'e')THEN
	   GO TO 999
	
C Printing the content (titles) of for each theme :
	ELSEIF(CTHEME.EQ.'P'.OR.CTHEME.EQ.'p')THEN
	  OPEN(2,FILE='help_jlp.tmp',STATUS='unknown',
     1	ACCESS='SEQUENTIAL')
	  WRITE(2,11)
	  WRITE(2,80)NMOD
	   DO ITHEM=1,NTHEMES
	     LUNIT=2
	     CALL HLP_OUTPUT(LUNIT,ITHEM)
	   END DO
 
C	  CLOSE(2,DISPOSE='PRINT')
	  CLOSE(2)
	  GO TO 999
	ENDIF
 
C********************************************************************
C Output of the modules of the selected theme:
	LUNIT=6
	READ(CTHEME,'(I1)') ITHEM
	CALL HLP_OUTPUT(LUNIT,ITHEM)
 
C Possibility of more information on a given module:
	PRINT 22
22	FORMAT(/,
     1	' Name of the module you want some information about: ',$)
	READ(5,10) NAME1
 
C First level of information (read in the catalogue):
	CALL HLP_INFO1(NAME1,IL1,SDIR,LANGUAGE,ITHEM,FOUND)
 
C Second level of information (read in the file itself):
	IF(FOUND)THEN
	  PRINT 23
23	  FORMAT(/,' Do you want some more information ? (Y)  :== ',$)
	  READ(5,10)ANS
	    IF(ANS.NE.'N'.AND.ANS.NE.'n')THEN
	     CALL HLP_INFO2(NAME1,IL1,PROGR,SDIR,LANGUAGE,
     1                      ITHEM,SRC_DIR,ILAST)
	    ENDIF
	ENDIF
 
	GO TO 88
 
999      STOP	
9999	 PRINT*,' Fatal error opening:',FULL_NAME
	 STOP
	 END
 
C*******************************************************************
C Subroutine HLP_READCAT
C to read the catalogue
C*******************************************************************
	SUBROUTINE HLP_READCAT
	PARAMETER (IDIM=300,NLINES=30)
	INTEGER*4 ICOMM(IDIM),THEME(IDIM),NMOD
	CHARACTER*30 NAME(IDIM),CTYPE(IDIM),SDIRECT(IDIM)
	CHARACTER COMM(NLINES,IDIM)*80,LANG(IDIM)*1
	CHARACTER BUFF*90
 
	COMMON /HLPCAT/NAME,COMM,ICOMM,CTYPE,SDIRECT,LANG,THEME,NMOD
 
10	FORMAT(A)
 
	JMAX=IDIM*30
 
C********************************************************************
C Reading the catalogue :
C********************************************************************
	I=0
 
	DO J=1,JMAX
 
C Reading the name:
	READ(1,10,END=9999,ERR=9999)BUFF
C	PRINT 89,BUFF
C89	FORMAT('Buff:',A)

	IF(BUFF(1:1).EQ.'#')THEN
	 I=I+1
	 NAME(I)=BUFF(3:32)
	 ICOMM(I)=0
	ENDIF
 
C Reading the type:
	IF(BUFF(1:1).EQ.'@')THEN
	 CTYPE(I)=BUFF(3:32)
	ENDIF
 
C Reading the theme:
	IF(BUFF(1:1).EQ.'%')THEN
	 READ(BUFF(3:3),'(I1)') THEME(I)
	ENDIF
 
C Reading the source directory:
	IF(BUFF(1:1).EQ.'$')THEN
	 LANG(I)=BUFF(3:3)
	ENDIF
 
C Reading the source directory:
	IF(BUFF(1:1).EQ.'&')THEN
	 SDIRECT(I)=BUFF(3:32)
	ENDIF
 
C Reading the comments:
	IF(BUFF(1:1).EQ.'>')THEN
	 ICOMM(I)=ICOMM(I)+1
	 COMM(ICOMM(I),I)=BUFF(3:82)
	ENDIF
 
	END DO
 
9999	NMOD=I
	RETURN
	END
C*****************************************************************
C Subroutine HLP_OUTPUT
C Output of the names of the selected theme
C
C Input::
C LUNIT: number of the output logical unit
C ITHEM: number of the selected theme
C*****************************************************************
	SUBROUTINE HLP_OUTPUT(LUNIT,ITHEM)
	PARAMETER (IDIM=300,NLINES=30,NTHEMES=8)
	INTEGER*4 ICOMM(IDIM),IOUTPUT(IDIM),NMOD,THEME(IDIM)
	CHARACTER*30 NAME(IDIM),CTYPE(IDIM),SDIRECT(IDIM)
	CHARACTER COMM(NLINES,IDIM)*80,LANG(IDIM)*1
	CHARACTER TITLES(NTHEMES)*40
	COMMON /HLPCAT/NAME,COMM,ICOMM,CTYPE,SDIRECT,LANG,THEME,NMOD
 
C Labels for the output:
	DATA TITLES/'1.Image processing (2D)',
     1	'2. Spectro/profiles (1D)',
     1	'3. Modelling/mathematics',
     1	'4. Transfer/access to different formats',
     1	'5. Graphic/display','6. Miscellaneous',
     1	'7. Aperture synthesis','8. Celestial mechanics/atmosph. disp.'/
 
	NOUT=0
	DO I=1,NMOD
	  IF(THEME(I).EQ.ITHEM)THEN
	    NOUT=NOUT+1
	    IOUTPUT(NOUT)=I
	  ENDIF
	END DO
 
C Title (name of the theme):
	WRITE(LUNIT,125)TITLES(ITHEM)
125	FORMAT(/,2X,A,/)
 
C Output of the names (two by two)
	IKMAX=NOUT/2
	DO IK=1,IKMAX
	  K0=(IK*2)-1
	  K1=K0+1
	  I0=IOUTPUT(K0)
	  I1=IOUTPUT(K1)
	  WRITE(LUNIT,101) NAME(I0)(1:18),CTYPE(I0)(1:10),
     1	NAME(I1)(1:18),CTYPE(I1)(1:10)
101	FORMAT(2X,A18,2X,A10,4X,A18,2X,A10)
	END DO
 
C Output of the complement if odd number :
	IF(IKMAX*2.LT.NOUT)THEN
	  I=IOUTPUT(NOUT)
	  WRITE(LUNIT,104) NAME(I)(1:18),CTYPE(I)(1:10)
104	  FORMAT(2X,A18,2X,A10)
	ENDIF
 
	RETURN
	END
C*****************************************************************
C Subroutine HLP_INFO1
C First level of information for a given module
C
C Input::
C NAME1: name of the selected module
C IL1: length of the input string
C ITHEM: Number of the theme where the module has to be found
C LANGUAGE: C or Fortran, source language. 
C
C Output:
C NAME1: full name of the corresponding module when found
C SDIR: source directory
C IL1: length of the output name (NAME1)
C FOUND: Logical name (true when a module has been found)
C*****************************************************************
	SUBROUTINE HLP_INFO1(NAME1,IL1,SDIR,LANGUAGE,ITHEM,FOUND)
	PARAMETER (IDIM=300,NLINES=30)
	LOGICAL FOUND
	INTEGER*4 ICOMM(IDIM),IOUTPUT(IDIM),NMOD,THEME(IDIM)
	CHARACTER*30 NAME(IDIM),CTYPE(IDIM),SDIRECT(IDIM)
	CHARACTER COMM(NLINES,IDIM)*80,LANG(IDIM)*1
	CHARACTER NAME1*(*),NAME2*30,SDIR*(*),LANGUAGE*1
	COMMON /HLPCAT/NAME,COMM,ICOMM,CTYPE,SDIRECT,LANG,THEME,NMOD
 
C Length of the input string:
	IL1=MIN(INDEX(NAME1,'    ')-1,30)
	IL1=MAX(1,IL1)
 
C Preparation of the input name (upper case conversion)
C Please do not put the loop inside (pb with "." for example...)
	DO I=1,IL1
	  IF(NAME1(I:I).GE.'a'.AND.NAME1(I:I).LE.'z')THEN
C This does not work on the Sun: (so I take 32)
C	    NAME1(I:I)=CHAR(ICHAR(NAME1(I:I))+'A'-'a')
	    NAME1(I:I)=CHAR(ICHAR(NAME1(I:I))-32)
	  ENDIF
	ENDDO
	
C Looking for the name within the modules of the selected theme:
	FOUND=.FALSE.
	DO I=1,NMOD
	 IF(THEME(I).EQ.ITHEM)THEN
	   NAME2=NAME(I)
C When found:
	    IF(NAME1(1:IL1).EQ.NAME2(1:IL1))THEN
	      FOUND=.TRUE.
	      IFOUND=I
C The input name is assigned to the full name of the module
C which has been found:
	      NAME1=NAME2
	      GOTO 86
	    ENDIF
	  ENDIF
	END DO
 
C Output of the comments when the name is known :
86	IF(.NOT.FOUND)THEN
	  PRINT *,' Module not found'
	ELSE
	  WRITE(6,103) NAME(IFOUND)(1:18),CTYPE(IFOUND)(1:18)
	    DO IC=1,ICOMM(IFOUND)
	     WRITE(6,10) COMM(IC,IFOUND)(1:78)
	    END DO
          SDIR=SDIRECT(I)
          LANGUAGE=LANG(I)
	ENDIF
 
C Length of the output string:
	IL1=MIN(INDEX(NAME1,'    ')-1,30)
	IL1=MAX(1,IL1)
 
10	FORMAT(A)
103	FORMAT(/,2X,A18,2X,A18,/)
 	RETURN
	END
 
C***************************************************************
C Second level of information for a given module
C Reads comments from the source itself
C
C***************************************************************
	SUBROUTINE HLP_INFO2(NAME1,IL1,PROGR,SDIR,LANGUAGE,
     1                       ITHEM,SRC_DIR,ILAST)
	LOGICAL PROGR
	INTEGER ILAST,USEFUL_LENGTH
	CHARACTER NAME1*(*),OUTPUT(500)*60,SDIR*(*),LANGUAGE*(*)
C Be careful: FILE_NAME should be large enough compared to GENERIC_NAME...
	CHARACTER GENERIC_NAME*40,FILE_NAME*80,SRC_DIR*(*)
	COMMON /JLP_DIRECT/OUTPUT

10	FORMAT(A)
	
C Prepare the generic name to be looked for:
	IDOT=INDEX(NAME1,'.')
 
	IF(IDOT.EQ.0)THEN
           IF(LANGUAGE(1:1).EQ.'C'.OR.LANGUAGE(1:1).EQ.'c')THEN
 	     GENERIC_NAME=NAME1(1:IL1)//'*.c'
           ELSE
 	     GENERIC_NAME=NAME1(1:IL1)//'*.for'
           ENDIF
	ELSE
	   GENERIC_NAME=NAME1(1:IL1)//'*'
	ENDIF

C Conversion to lower case:
C	PRINT*,' Before:',GENERIC_NAME
        DO 40 J=1,40
	 IF((GENERIC_NAME(J:J).GE.'A')
     1	.AND.(GENERIC_NAME(J:J).LE.'Z')) THEN
C This does not work on the Sun: (so I take 32)
C       GENERIC_NAME(J:J)=CHAR(ICHAR(GENERIC_NAME(J:J))+'A'-'a')
          GENERIC_NAME(J:J)=CHAR(ICHAR(GENERIC_NAME(J:J))+32)
	ENDIF
40      CONTINUE
C	PRINT*,' After: (lower?) ',GENERIC_NAME

C Add the subdirectory:
        FILE_NAME=' '

C  FILE_NAME=SRC_DIR(1:ILAST)//'/hrsa/'//GENERIC_NAME
C  FILE_NAME=SRC_DIR(1:ILAST)//'/source/'//GENERIC_NAME
C  FILE_NAME=SRC_DIR(1:ILAST)//'/jlpsub/'//GENERIC_NAME
          USEFUL_LENGTH=MAX(INDEX(SDIR,'    ')-1,1)
	  WRITE(FILE_NAME,29) SRC_DIR(1:ILAST),
     1         SDIR(1:USEFUL_LENGTH),GENERIC_NAME
29        FORMAT(A,'/',A,'/',A)
 
C Look for this name in the corresponding directory:
C (quiet is set to true, i.e. no output of the names on the terminal)
	CALL JLP_DIRECTORY(FILE_NAME,NUMBER,1)
	IF(NUMBER.EQ.0)THEN
	  WRITE(6,*)' No modules corresponding to ',FILE_NAME
	  RETURN
	ENDIF
 
C We write the information of the first name only (OUTPUT(1)):
        FILE_NAME=OUTPUT(1)
	WRITE(6,48) FILE_NAME
48      FORMAT(' Reading the file: ',A,/,70('*'))
	CALL READ_COMMENTS(FILE_NAME)
 
	RETURN
	END
C*************************************************************************
C Subroutine READ_COMMENTS
C To read the comments in the header of a file
C
C The comments should start with /*, C++, or *++, and end with */, C--, or *--
C*************************************************************************
	SUBROUTINE READ_COMMENTS(NAME)
	LOGICAL FOUND
	CHARACTER BUFFER*90,NAME*(*)
 
10	FORMAT(A)
 
C Input
	  OPEN(1,FILE=NAME,STATUS='OLD',ERR=999)
 
C Reading the input file:
C Look only in the first 10 lines, to check if 'C++' is found
	  FOUND=.FALSE.
	  DO I=1,10
	    READ(1,10,END=99)BUFFER
	    IF(BUFFER(1:3).EQ.'C++'.OR.BUFFER(1:3).EQ.'c++'
     1	  .OR.BUFFER(1:3).EQ.'*++'.OR.BUFFER(1:2).EQ.'/*')THEN
	      FOUND=.TRUE.
C Display less than 200 lines:
	       DO J=1,200
	         READ(1,10,END=99)BUFFER
C Return when 'C--' has been found:
	         IF(BUFFER(1:3).EQ.'C--'.OR.BUFFER(1:3).EQ.'c--'
     1	  .OR.BUFFER(1:3).EQ.'*--'.OR.INDEX(BUFFER,'*/').GT.0)GOTO 99
C Limit to 78 characters (to avoid wrapping on xterm windows...)
	         WRITE(6,10)BUFFER(1:78)
	       ENDDO
	    ENDIF
	  END DO
99	  CLOSE(1)
 
	IF(.NOT.FOUND) WRITE(6,*) ' Comments not found in the header'
	RETURN
999	WRITE(6,*)' Module not accessible :',NAME
	RETURN
	END
