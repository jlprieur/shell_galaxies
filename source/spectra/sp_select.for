C++**********************************************************************
C SP_SELECT
C  To process radio HI spectra from PARKES (Observations from Nov. 1989)
C  Interactive selection of the quadrants, after displaying them.
C  Output: spectrum which is a weighted sum of the selected quadrants.
C
C JLP
C Version of 05-10-90
C--**********************************************************************
	PROGRAM SP_SELECT
	PARAMETER (IDIM=1024,NCPBMAX=10,NPIX=256)
	REAL*4 INPUT1(IDIM,NCPBMAX),OUTPUT1(IDIM,NCPBMAX)
	REAL*4 SPECT(NPIX),VELO(NPIX)
	CHARACTER FILE_IN*40,FILE_OUT*40,NAME*40,BUFFER*80
	CHARACTER TITLE*40,PLOTDEV*32,ANSWER*1
 
10	FORMAT(A)
 
	CALL JLP_BEGIN
 
	WRITE(6,63)
63	FORMAT(' Program SP_SELECT Version 05-10-90',/,
     1	' To select the quadrants from Parkes Observations',/,
     1	' and add them up into a single spectrum')
 
C Environment:
	PRINT *,' Output graphic device ? ($TEKTRO?)'
	READ(5,10) PLOTDEV
 
C Resetting the spectrum:
	DO I=1,NPIX
	  SPECT(I)=0.
	END DO
 
*
* Obtain input data in a big loop (see GOTO 61):
*
61	PRINT *,' INPUT DATA FILE (4 quadrants, a few CPB...) ?'
	READ(5,10) FILE_IN
	CALL READSP4(INPUT1,IDIM,FILE_IN,CVELOCITY,NCPB,IFIRST,
     1	TITLE,ISTATUS)
 	IF(ISTATUS.NE.0)GOTO 61
 
	PRINT 87,NCPB,CVELOCITY
87	FORMAT(' Data set successfully read',/,
     1	' Number of check-point-blocks : ',I3,/,
     1	' Central velocity: ',F9.2)
 
64	CALL PLOTSP4(INPUT1,CVELOCITY,NCPB,PLOTDEV,TITLE)
	WRITE(6,*) ' Do you want to display the data again? [N]'
	READ(5,10) ANSWER
	IF(ANSWER.EQ.'Y'.OR.ANSWER.EQ.'y')GOTO 64
 
C Selection of the quadrants and sum in SPECT:
	CALL SUMSP(INPUT1,NCPB,SPECT)
 
* Write the output data file in the same format as input:
c	PRINT *,' OUTPUT DATA FILE ?'
c	READ(5,10) FILE_OUT
c	CALL WRITESP4(INPUT1,IDIM,FILE_IN,FILE_OUT,CVELOCITY,
c     1	NCPB,IFIRST)
 
	WRITE(6,62)
62	FORMAT(' Do you want to add another data file to',
     1	' this spectrum ? [N]')
	READ(5,10)ANSWER
	IF(ANSWER.EQ.'Y'.OR.ANSWER.EQ.'y')GOTO 61
 
* Normalization:
	WRITE(6,*) ' Constant for normalization of the spectrum? (1.?)'
	READ(5,*) CTE
	
C Normalizing the spectrum, and computing the X axis:
	VSTEP=4.
	DO I=1,NPIX
	  SPECT(I)=SPECT(I)/CTE
	  VELO(I)=CVELOCITY+FLOAT(I-128)*VSTEP
	END DO
 
* Displaying the spectrum:
	WRITE(6,*) ' Now displaying the spectrum'
	CALL PLOTSP1(SPECT,VELO,NPIX,PLOTDEV,TITLE)
 
* Write the output spectrum as an ASCII file:
* with some comments on the first line, after the data:
	PRINT *,' Output ASCII file (spectrum) ?'
	READ(5,10) FILE_OUT
	OPEN(2,FILE=FILE_OUT,STATUS='NEW',ACCESS='SEQUENTIAL')
	WRITE(2,66) VELO(1),SPECT(1),TITLE(1:20),CVELOCITY
66	FORMAT(2(X,G12.5),A,' CVEL:',F9.2)
	DO I=2,NPIX
	  WRITE(2,*) VELO(I),SPECT(I)
	END DO
	CLOSE(2)
 
	CALL JLP_END
	STOP
	END
C*************************************************************************
C READSP4
C To read ".DAT" files, as created by Tom from *.SPC
C
C Example of structure:
C
C********* FILE : SH1.SPC
C
C SEQUENCE LIST : 74
C
C Reg Seq   Source         RA or L      Dec or B      Vel      ZA      Tsys
C
C  1   74  NGC 2502       7 54 33.96  -52 10 21.0  1300.00    33.89      74
C  2   74  NGC 2502       7 54 33.96  -52 10 21.0  1300.00    33.34      74
C  3   74  NGC 2502       7 54 33.96  -52 10 21.0  1300.00    32.80      74
C
C      channel       CPB 1          CPB 2          CPB 3
C         1        9.02571E-02   -1.08826E+00   -1.16413E+00
C         2        7.80524E-02   -1.24985E+00   -1.30706E+00
C         3       -2.20824E-02   -1.18392E+00   -1.34611E+00
C...
C      1024       -2.20824E-02   -1.18392E+00   -1.34611E+00
C*************************************************************************
	SUBROUTINE READSP4(ARRAY,IDIM,FNAME,CVELOCITY,NCPB,
     1	IFIRST,TITLE,ISTATUS)
	PARAMETER (NCPBMAX=10)
	REAL*4 ARRAY(IDIM,NCPBMAX)
	INTEGER*4 NCPB,IFIRST,ISTATUS
	REAL*4 CVELOCITY
	CHARACTER FNAME*(*),BUFFER*80,TITLE*40
 
C First opening:
	OPEN(1,FILE=FNAME,STATUS='OLD',ACCESS='SEQUENTIAL',ERR=999)
 
C Determines the number of CPB, and the number of the first data line:
	NCPB=0
	IFIRST=0
	CVELOCITY=0.
	DO 79 I=1,40
	  BUFFER=' '
	  READ(1,10) BUFFER
10	  FORMAT(A)
C Displaying the line:
	  WRITE(6,10) BUFFER(1:79)
	  READ(BUFFER(1:5),*,ERR=78) NCPB
C When no error, reading the central velocity, and the name of the object:
	  READ(BUFFER(51:60),*,ERR=78) CVELOCITY
	  TITLE=' '
	  TITLE=BUFFER(6:24)
C Exit from the loop when the first data line is found:
78	  READ(BUFFER(9:13),*,ERR=79) KK
	  IF(KK.EQ.1)THEN
	    IFIRST=I
            GOTO 82
	  ENDIF
79	CONTINUE
 
82	CLOSE(1)
C	WRITE(6,13)NCPB,IFIRST,CVELOCITY
13	FORMAT(' Number of CPB: ',I4,/,
     1	' Row number of the first data line: ',I4,/,
     1	' Central velocity: ',F9.2)
 
	IF(NCPB.EQ.0)THEN
	  PRINT *,' READSP/Fatal error decoding number of CPB'
	   ISTATUS=2
	   RETURN
	ENDIF
 
	IF(IFIRST.EQ.0)THEN
	  PRINT *,' READSP/Fatal error looking for the first data line'
	   ISTATUS=3
	   RETURN
	ENDIF
 
C Second opening:
	OPEN(1,FILE=FNAME,STATUS='OLD',ACCESS='SEQUENTIAL',ERR=999)
 
C Skipping the first rows:
	DO I=1,IFIRST-1
	  READ(1,10)BUFFER
	END DO
 
C Reading the data:
	DO I=1,1024
	  READ(1,*,ERR=85) KK,(ARRAY(I,K),K=1,NCPB)
	  IF(KK.NE.I)THEN
85	   PRINT *,' READSP/Error reading the data, KK,I',KK,I
	   ISTATUS=4
	   RETURN
	  ENDIF
	END DO
 
	CLOSE(1)
 
	ISTATUS=0
	RETURN
999	PRINT *,' READSP/Error opening input file:',FNAME
	ISTATUS=1
	RETURN
	END
C*************************************************************************
C WRITESP
C To write ".DAT" files, as created by Tom from *.SPC
C
C Example of structure:
C
C********* FILE : SH1.SPC
C
C SEQUENCE LIST : 74
C
C Reg Seq   Source         RA or L      Dec or B      Vel      ZA      Tsys
C
C  1   74  NGC 2502       7 54 33.96  -52 10 21.0  1300.00    33.89      74
C  2   74  NGC 2502       7 54 33.96  -52 10 21.0  1300.00    33.34      74
C  3   74  NGC 2502       7 54 33.96  -52 10 21.0  1300.00    32.80      74
C
C      channel       CPB 1          CPB 2          CPB 3
C         1        9.02571E-02   -1.08826E+00   -1.16413E+00
C         2        7.80524E-02   -1.24985E+00   -1.30706E+00
C         3       -2.20824E-02   -1.18392E+00   -1.34611E+00
C...
C      1024       -2.20824E-02   -1.18392E+00   -1.34611E+00
C*************************************************************************
	SUBROUTINE WRITESP4(ARRAY,IDIM,FILE_IN,FILE_OUT,
     1	CVELOCITY,NCPB,IFIRST)
	PARAMETER (NCPBMAX=10)
	REAL*4 ARRAY(IDIM,NCPBMAX)
	INTEGER*4 NCPB,IFIRST
	REAL*4 CVELOCITY
	CHARACTER FILE_IN*(*),FILE_OUT*(*),BUFFER*80,LINE1*80
 
C Opening input file:
	OPEN(1,FILE=FILE_IN,STATUS='OLD',ACCESS='SEQUENTIAL',
     1	ERR=999)
 
C Opening output file:
	OPEN(2,FILE=FILE_OUT,STATUS='NEW',ACCESS='SEQUENTIAL',
     1	ERR=997)
 
C Copy the input header to output:
	DO I=1,IFIRST-1
	  BUFFER=' '
	  READ(1,10) BUFFER
10	  FORMAT(A)
 
C Exit if the current line is the first description of the object,
	  READ(BUFFER(1:5),*,ERR=78) KK
	  GOTO 76
 
78	  WRITE(2,10) BUFFER
	END DO
 
C Updating the central velocity:
76	  LINE1=BUFFER
	  WRITE(LINE1(52:60),54) CVELOCITY
54	  FORMAT(F9.2)
 
C Writing as many lines as necessary:
	DO K=1,NCPB
	  WRITE(LINE1(1:3),53,ERR=78) K
53	  FORMAT(I3)
	  WRITE(2,10) LINE1
	END DO
 
	WRITE(2,52)
52	FORMAT(/,'      channel       CPB 1          ',
     1	'CPB 2          CPB 3')
 
	CLOSE(1)
 
C Writing the data:
	DO I=1,1024
	  WRITE(2,*) I,(ARRAY(I,K),K=1,NCPB)
	END DO
 
	CLOSE(2)
 
	RETURN
999	PRINT *,' READSP/Fatal error opening input file:',FILE_IN
	STOP
997	PRINT *,' READSP/Fatal error opening output file:',FILE_OUT
	STOP
	END
 
C**********************************************************************
C Subroutine PLOTSP4
C
C To plot raw data (4 quadrants)
C Flux versus channel number
C
C**********************************************************************
	SUBROUTINE PLOTSP4(ARRAY,CVELOCITY,NCPB,PLOTDEV,TITLE)
	PARAMETER (IDIM=1024,NCPBMAX=10)
	REAL*4 ARRAY(IDIM,NCPBMAX)
	REAL*4 XPLOT(IDIM,NCPBMAX),YPLOT(IDIM,NCPBMAX)
	INTEGER*4 NCPB,NPTS(NCPBMAX)
	REAL*4 CVELOCITY,YDIST
	CHARACTER CHAR1*30,CHAR2*30,NCHAR(10)*4,PCOLOR(10)*30
	CHARACTER TITLE*40,PLOTDEV*32
 
	WRITE(6,*) ' Distance in Y between two CPB ?'
	READ(5,*) YDIST
 
C Generating X and Y axes:
	DO K=1,NCPB
	  DO I=1,1024
	    XPLOT(I,K)=FLOAT(I)
	    YPLOT(I,K)=ARRAY(I,K)+FLOAT(K-1)*YDIST
	  END DO
	END DO
 
	CHAR1=' Channel'
	CHAR2=' Flux'
	
	DO K=1,NCPB
	  NCHAR(K)='L0'
          PCOLOR(K)='Default'
	  NPTS(K)=1024
	END DO
 
	CALL NEWPLOT(XPLOT,YPLOT,NPTS,IDIM,NCPB,CHAR1,
     1	CHAR2,TITLE,NCHAR,PCOLOR,PLOTDEV,' ',' ')
 
	RETURN
	END
C**********************************************************************
C Subroutine PLOTSP1
C
C To plot processed data (1 quadrant)
C Flux versus velocity
C
C**********************************************************************
	SUBROUTINE PLOTSP1(SPECT,VELO,NPIX,PLOTDEV,TITLE)
	REAL*4 SPECT(NPIX),VELO(NPIX)
	CHARACTER CHAR1*30,CHAR2*30,NCHAR(10)*4,PCOLOR(10)*30
	CHARACTER TITLE*40,PLOTDEV*32
 
	CHAR1=' Velocity'
	CHAR2=' Flux'
	NCHAR(1)='L0'
        PCOLOR(1)='Default'
 
	CALL NEWPLOT(VELO,SPECT,NPIX,NPIX,1,CHAR1,
     1	CHAR2,TITLE,NCHAR,PCOLOR,PLOTDEV,' ',' ')
 
	RETURN
	END
 
C**********************************************************************
C Subroutine SUMSP
C
C To select the quadrants and add them up
C
C**********************************************************************
	SUBROUTINE SUMSP(ARRAY,NCPB,SPECT)
	PARAMETER (IDIM=1024,NCPBMAX=10,NPIX=256)
	REAL*4 ARRAY(IDIM,NCPBMAX),SPECT(NPIX)
	INTEGER*4 NCPB
	REAL*4 MASK(4)
 
	WRITE(6,54)
54	FORMAT(' Now selecting the quadrants you want:',/,
     1	' This will be done with a mask, 0 to discard it,',
     1	' 1 or a coeff to select it',/,
     1	' This coeff will be multiplied with the values',
     1	' before adding them up')
 
C Main loop on the CPB:
	DO K=1,NCPB
	  WRITE(6,55) K
55	  FORMAT(' For CPB number ',I3,/,
     1	' Enter the mask you want on the quadrants',
     1	' (Ex: 0 .5 1. .5 )')
	  READ(5,*) (MASK(KK),KK=1,4)
 
	  J=0
	  DO IQUAD=1,4
	    DO I=1,NPIX
	      J=J+1
	      SPECT(I)=SPECT(I)+MASK(IQUAD)*ARRAY(J,K)
	    END DO
	  END DO
 
	END DO
 
	RETURN
	END
