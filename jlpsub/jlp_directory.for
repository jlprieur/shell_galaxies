C++***********************************************************************
C Set of routines to process lists of files
C and generate lists of files from input with wild cards
C Called by FITS_READ and FITS_WRITE 
C
C Should be in "JLP_UTILITIES"
C
C Contains:
C JLP_NFRAME1, JLP_NFRAME2
C
C JLP
C Version 27-07-90
C--***********************************************************************
C Subroutine to decode the list of files
C From *.ext1 generate *.ext2
C*************************************************************************
	SUBROUTINE JLP_NFRAME1(OUTFRAME,OUTFRAM,INFRAM,NFILES)
	CHARACTER OUTFRAME*60,OUTFRAM(*)*60,INFRAM(*)*60
	INTEGER*4 NFILES

C Case when no star is present:
	II=INDEX(OUTFRAME,'*')
	IF((II.LE.0))THEN
          INFRAM(1)=OUTFRAME
          OUTFRAM(1)=OUTFRAME
	  NFILES=1
	  RETURN
	ENDIF

	JJ=INDEX(OUTFRAME,'.')
	IF((JJ.NE.II+1))THEN
	  PRINT 45,OUTFRAME
45	  FORMAT(' JLP_NFRAME1/Fatal error: wrong syntax for output files',/,
     1   '  Should have been (suff*.ext), but is :',A)
	  STOP
	ENDIF

	IF(INDEX(OUTFRAME(II+1:),'*').GE.1)THEN
	  PRINT 46,OUTFRAME
46	  FORMAT(' JLP_NFRAME1/Fatal error: wrong syntax for output files',/,
     1    '  Only one wild card allowed. Should have been (suff*.ext)',/,
     1    '  but is :',A)
	  STOP
	ENDIF

C Case "*.ext"
	IF(II.EQ.1)THEN
	  DO I=1,NFILES
C Look for the directory (Vax)
	    K1=INDEX(INFRAM(I),']')
	    IF(K1.GT.0)THEN
	      K=K1+INDEX(INFRAM(I)(K1:),'.')-1
	      OUTFRAM(I)=INFRAM(I)(K1+1:K)//OUTFRAME(3:20)
	    ELSE
	      K=INDEX(INFRAM(I),'.')
	      OUTFRAM(I)=INFRAM(I)(1:K)//OUTFRAME(3:20)
	    ENDIF
	  END DO
	ELSE
	  PRINT 47,OUTFRAME
47	  FORMAT(' JLP_NFRAME1/Fatal error: wrong syntax for output files',/,
     1    '  Should have been (suff*.ext), but is :',A)
	  STOP
	ENDIF	


	RETURN
	END
C*************************************************************************
C Subroutine to generate the list of output files (for tape)
C From *.ext1 generate 001.ext1, etc following the list of input values
C From suff*.ext1 generate suff001.ext1, etc
C*************************************************************************
	SUBROUTINE JLP_NFRAME2(OUTFRAME,OUTFRAM,NFIL,NFILES)
	CHARACTER OUTFRAME*60,OUTFRAM(*)*60
	INTEGER*4 NFILES,NFIL(*)

	II=INDEX(OUTFRAME,'*')
	JJ=INDEX(OUTFRAME,'.')
	IF((JJ.NE.II+1).OR.(II.LE.0))THEN
	  PRINT *,' JLP_NFRAME2: wrong syntax for output files'
	  PRINT *,' (only "*.ext" or "suff*.ext" allowed here)'
	  STOP
	ENDIF

C Case "*.ext"
	IF(II.EQ.1)THEN
	  DO I=1,NFILES
C	    PRINT *,' NFIL(I)',NFIL(I)
	    I1=NFIL(I)/100
	    I2=(NFIL(I)/10)-I1*10
	    I3=NFIL(I)-I2*10-I1*100
C	    PRINT *,' I1,I2,I3',I1,I2,I3
	    WRITE(OUTFRAM(I),210) I1,I2,I3,OUTFRAME(JJ:JJ+10)
210	    FORMAT(3(I1),A)
	  END DO
C Case "suff*.ext"
	ELSE
	  DO I=1,NFILES
C	    PRINT *,' NFIL(I)',NFIL(I)
	    I1=NFIL(I)/100
	    I2=(NFIL(I)/10)-I1*10
	    I3=NFIL(I)-I2*10-I1*100
C	    PRINT *,' I1,I2,I3',I1,I2,I3
	    WRITE(OUTFRAM(I),211) OUTFRAME(1:II),
	1	I1,I2,I3,OUTFRAME(JJ:JJ+10)
211	    FORMAT(A,3(I1),A)
	  END DO
	ENDIF


	RETURN
	END
C*************************************************************************
