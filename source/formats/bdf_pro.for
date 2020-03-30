       PROGRAM BDF_PRO
C++:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C Program BDF_PRO
C to convert BDF spectra into standard files number,(x,y)
C
C JLP Version 01-01-87
C--:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
	PARAMETER (IDIM=2000)
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*4 IMAGE(IDIM,IDIM)
	REAL*8 RAD1(IDIM),PROF1(IDIM),WORK1(IDIM),WORK2(IDIM)
	CHARACTER ANS*1,NAME*40,COMMENTS*80
 
10	FORMAT(A)
 
	PRINT 80
80	FORMAT(' MENU :',/,
     1	' 1. BDF FILE CREATED BY XYCURA',/,
     1	' 2. BDF FILE CREATED BY SADBDF (SPECTRUM)',/,
     1	' ENTER THE OPTION :')
	READ(5,*) IOPT
 
 
C-------------------------------------------------------------------------
C  Reads the input file (Option=4  ==>  BDF file)  created by XYCURA
C
	PRINT *,' INPUT MONODIMENSIONAL BDF FILE :'
	IF(IOPT.EQ.1)THEN
           IOP=4
           CALL DREADFILE(RAD1,PROF1,NPTS1,WORK1,WORK2,IWORK,IDIM,
     1                    NAME,COMMENTS,IOP)
	ELSE
	   CALL INQUIFMT
           WRITE(6,*) 'Input file: '
           READ(5,10) NAME
	   CALL JLP_READIMAG(IMAGE,NX,NY,IDIM,NAME,COMMENTS)
	PRINT *,' NX,NY :',NX,NY
 
	NPTS1=NX
	DO I=1,NPTS1
	  PROF1(I)=IMAGE(I,1)
	END DO
 
	PRINT *,' ENTER FIRST X VALUE AND INCREMENT :'
	READ(5,*) XSTART,XINC
	
	RAD1(1)=XSTART
	DO I=2,NPTS1
	  RAD1(I)=RAD1(I-1)+XINC
	END DO
 
	ENDIF
	
 
	PRINT 112,NPTS1,RAD1(1),RAD1(NPTS1)
112	FORMAT(2X,I5,' POINTS RECEIVED',/,' PROFILE FROM ',
     1	F12.3,' TO ',F12.3,/)
 
	PRINT *,' DO YOU WANT TO CHECK THE INPUT VALUES ? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
	DO I=1,NPTS1
	PRINT *,RAD1(I),PROF1(I)
	END DO
	ENDIF
	
C---------------------------------------------------------------------
C Output of the file
105	PRINT *,' NAME OF THE OUTPUT FILE ?'
	READ(5,10) NAME
 
	OPEN(1,FILE=NAME,STATUS='NEW',ERR=105)
	WRITE(1,*)NPTS1
	DO I=1,NPTS1
	WRITE(1,*)RAD1(I),PROF1(I)
	END DO
	CLOSE(1)
	PRINT *,' FILE CREATED'
 
	STOP
	END
C---------------------------------------------------------------------
