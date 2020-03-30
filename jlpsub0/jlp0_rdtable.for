C++**********************************************************************
C Set of subroutines
C to read Midas tables and spectra (1-D image)
C
C Contains:
C JLP_RDTABLE, JLP_RDSPEC, JLP_TRANMI
C
C JLP
C Version 15-07-94
C--**********************************************************************
C++**********************************************************************
C Subroutine JLP_RDTABLE
C To read MIDAS tables (the first two columns)
C Use MIDAS routines
C--**********************************************************************
	SUBROUTINE JLP_RDTABLE(XX,YY,NROW)
	REAL XX(*),YY(*)
	INTEGER ISTAT
	INTEGER NCOL,NROW,NSC1,JLP_INIT_MIDAS
	INTEGER MADRID(1),ITID,ACOL,AROW
	LOGICAL*4 NULL
	CHARACTER TABLE*40
	COMMON /JLP_MIDAS/JLP_INIT_MIDAS
	COMMON /VMR/ MADRID
	INCLUDE 'jlpsub:st_def.inc'
	INCLUDE 'jlpsub:st_dat.inc'
 
C Midas environment:
C Initializing MIDAS parameters:
	IF(JLP_INIT_MIDAS.NE.1)THEN
	  CALL JLP_SCSPRO('-1')
	  JLP_INIT_MIDAS = 1
	ENDIF
 
 
C Get the table name
	WRITE(6,*) ' Input table :==  '
	READ(5,10) TABLE
10	FORMAT(A)
 
C Read the table:
	CALL TBTOPN(TABLE,F_I_MODE,ITID,ISTAT)
	IF(ISTAT.NE.0) GO TO 1000
	CALL TBIGET(ITID,NCOL,NROW,NSC1,ACOL,AROW,ISTAT)
	IF(ISTAT.NE.0) GO TO 1000
 
C Displays the table description:
	WRITE(6,77) NCOL,NROW,NSC1
77	FORMAT(' NCOL=',I5,' NROW=',I5,' NSC1=',I5,
     1	/,' ACOL=',I5,' AROW=',I5)
 
C Read the first two columns:
	DO J=1,NROW
	  CALL TBERDR(ITID,J,1,XX(J),NULL,ISTAT)
	  IF(ISTAT.NE.0) GO TO 1000
	  CALL TBERDR(ITID,J,2,YY(J),NULL,ISTAT)
	  IF(ISTAT.NE.0) GO TO 1000
 
C Displays the output:
C	  WRITE(6,78) J,XX(J),YY(J)
C78	  FORMAT(' J,X,Y',X,I5,2X,F12.6,2X,F12.6)
 
	END DO
 
	RETURN
 
C When error:
1000 	WRITE(6,*) ' Fatal error reading MIDAS table'	
	CALL JLP_END
	RETURN
	END
 
C++**********************************************************************
C Subroutine JLP_RDSPEC
C To read MIDAS spectra (1-D images)
C Uses MIDAS routines
C
C--**********************************************************************
	SUBROUTINE JLP_RDSPEC(XX,YY,NX)
	REAL*4 XX(*),YY(*)
	REAL*8 START(2),STEP(2)
	INTEGER*4 NPIX(2),PNTR_IMAGE
	INTEGER MADRID(1),JLP_INIT_MIDAS
	CHARACTER NAME*80,COMMENTS*80
	CHARACTER CUNIT*80,IDENT*72
	INCLUDE 'jlpsub:st_def.inc'
	INCLUDE 'jlpsub:st_dat.inc'
	COMMON /VMR/MADRID
	COMMON /JLP_MIDAS/JLP_INIT_MIDAS
 
C Initializing MIDAS parameters:
	IF(JLP_INIT_MIDAS.NE.1)THEN
	  CALL JLP_SCSPRO('-1')
	  JLP_INIT_MIDAS = 1
	ENDIF
 
C Opening the MIDAS file
5	WRITE(6,*) ' Input 1-D image ?'
	READ(5,10) NAME
10	FORMAT(A)
	CALL STIGET(NAME,D_R4_FORMAT,F_I_MODE,F_IMA_TYPE,
     1	1,NAXIS,NPIX,START,STEP,IDENT,CUNIT,
     1	PNTR_IMAGE,INUMB,ISTAT)
	IF(ISTAT.NE.0)THEN
	  PRINT *,' ERROR JLP_RDMIDAS/STIGET:  ISTAT=',ISTAT
	  PRINT *,' ERROR WHEN OPENING ',NAME
	  GOTO 5
	ENDIF
 
	NX=NPIX(1)
	COMMENTS=' '
	COMMENTS(1:72)=IDENT(1:72)
c
c Transfering the data:
c
	CALL JLP_TRANMI(YY,MADRID(PNTR_IMAGE),NX)
 
C Generating the wavelengths:
	DO I=1,NX
	   XX(I)=START(1)+(I-1)*STEP(1)
	END DO
 
	RETURN
	END
C----------------------------------------------------------------------
C Transfering the input MIDAS BDF file into the output array
C Called by JLP_WRMIDAS
C----------------------------------------------------------------------
	SUBROUTINE JLP_TRANMI(OUTPUT,INPUT,NX)
	REAL*4 INPUT(*),OUTPUT(*)
 
	  DO IX=1,NX
	    OUTPUT(IX)=INPUT(IX)
	  END DO
 
	RETURN
	END
