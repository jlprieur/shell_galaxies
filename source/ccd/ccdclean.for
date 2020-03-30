C++**************************************************************
C Program CCDCLEAN
C to clean CCD frames.
C
C JLP
C Version of 20-04-98
C--**********************************************************
	PROGRAM CCDCLEAN
	PARAMETER (IDIM=600)
	REAL*4 IMAGE(IDIM,IDIM)
	CHARACTER ANS*1,NAME*40,COMMENTS*80
10	FORMAT(A)
	OPEN(3,FILE='ccdclean.log',STATUS='unknown')
 
	CALL JLP_BEGIN
 
	PRINT 20
	WRITE(3,20)
20	FORMAT(' Program CCDCLEAN : Version of 20-04-98')
 
C Inquire the format of the files :
	CALL JLP_INQUIFMT
 
C Flag to know if a file has been entered
	IFILE=0
 
	PRINT 90
90	FORMAT(' MENU :',/,
     1	' 0,0   Flat field and bias correction',/,
     1	' 1,0   Correction or suppression of lines in the edges',/,
     1	' 2,0   Correction of columns in the edges',/,
     1	' 31,n  Suppression of columns from 1 to n',/,
     1	' 32,n  Suppression of columns from n to NPL',/,
     1	' 4,n   Correction of a single bad column #n',/,
     1	' 5,n   Correction of a double bad column #n,#n+1',/,
     1	' 6,n   Correction of a triple bad column #n,#n+1,#n+2',/,
     1	' 7,0   Correction of cosmic rays',/,
     1	' 8,0   Correction of aberrant points with a threshold',/,
     1	' 9,0   Correction of known bad pixels (eso or sso)',/,
     1	' 10,0  Exit and storage of the image')
 
88	PRINT 89
89	FORMAT(' ENTER THE OPTION YOU WANT : ',$)
	READ(5,*) IOPT,NCOL
        IBADC=NCOL
 
C--------------------------------------------------------------------
C Flat field and bias correction :
 
	IF(IOPT.EQ.0)THEN
C Flag to know if a file has been entered
	IFILE=1
	CALL CLINET(IMAGE,NPL,NL,NAME)
	GO TO 88
	ENDIF
 
C-------------------------------------------------------------------
C For all the following options, if no image has been entered, input of
C an image
 
	IF(IFILE.EQ.0)THEN
	IFILE=1
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
	CALL JLP_READIMAG(IMAGE,NPL,NL,IDIM,NAME,COMMENTS)
	ENDIF
 
 
C--------------------------------------------------------------------
C Correction of the lines in the edges :
 
	IF(IOPT.EQ.1)THEN
 
100	PRINT 101
101	FORMAT(' MENU :',/,
     1	' 1,nlin,nl : Correction of the lines #nlin to #nl',/,
     1	' 2,nlin : Correction of the lines #1 to #nlin',/,
     1	' 3,nlin : Suppression of the lines #nlin to #npl',/,
     1	' 4,nlin : Suppression of the lines #1 to #nlin',/,
     1	' ENTER OPTION AND NLIN : ',$)
	READ(5,*) IOPT1,NLIN
 
C Correction of the lines NLIN to NL
	IF(IOPT1.EQ.1)THEN
	PRINT 111,NLIN,NL
	WRITE(3,111)NLIN,NL
111	FORMAT('Correction of the lines: ',I4,' to ',I4)
	DO ICOL=1,NPL
	  IM1=MAX(ICOL-1,1)
	  IP1=MIN(ICOL+1,NPL)
	  X=0.5*(IMAGE(IM1,NLIN-2)+IMAGE(IM1,NLIN-1)+
     1	IMAGE(IP1,NLIN-2)+IMAGE(IP1,NLIN-1))
	  X=X+IMAGE(ICOL,NLIN-2)+IMAGE(ICOL,NLIN-1)
	     DO IL=NLIN,NL
	       IMAGE(ICOL,IL)=X/4.
	     END DO
	  END DO
	GO TO 88
	ENDIF
 
C Correction of the lines 1 to NLIN
	IF(IOPT1.EQ.2)THEN
	PRINT 112,NLIN
	WRITE(3,112)NLIN
112	FORMAT('Correction of the lines: 1 to ',I4)
	DO ICOL=1,NPL
	  IM1=MAX(ICOL-1,1)
	  IP1=MIN(ICOL+1,NPL)
	  X=0.5*(IMAGE(IM1,NLIN+2)+IMAGE(IM1,NLIN+1)+
     1	IMAGE(IP1,NLIN+2)+IMAGE(IP1,NLIN+1))
	  X=X+IMAGE(ICOL,NLIN+2)+IMAGE(ICOL,NLIN+1)
	    DO IL=1,NLIN
	     IMAGE(ICOL,IL)=X/4.
	    END DO
	END DO
	GO TO 88
	ENDIF
 
 
C Suppression of the last lines
	IF(IOPT1.EQ.3)THEN
	PRINT 113,NLIN,NL
	WRITE(3,113)NLIN,NL
113	FORMAT('Supression of the lines: ',I4,' to ',I4)
	NL=NLIN-1
	PRINT *,' NEW NL =',NL
	GO TO 88
	ENDIF
 
C Suppression of the first lines
	IF(IOPT1.EQ.4)THEN
	PRINT 114,NLIN
	WRITE(3,114)NLIN
114	FORMAT('Supression of the lines: 1 to',I4)
 
	DO IL=NLIN+1,NL
	 IIL=IL-NLIN
	 DO IP=1,NPL
	  IMAGE(IP,IIL)=IMAGE(IP,IL)
	 END DO
	END DO
 
	NL=NL-NLIN
	PRINT *,' NEW NL =',NL
	GO TO 88
	ENDIF
 
	GO TO 88
	ENDIF
 
C--------------------------------------------------------------------
C Correction of columns in the edges :
 
	IF(IOPT.EQ.2)THEN
200	PRINT 201
201	FORMAT(' MENU :',/,
     1	' 1,ncol : Correction of the columns ncol to npl',/,
     1	' 2,ncol : Correction of the columns 1 to ncol',/,
     1	' Enter option and ncol: ',$)
	READ(5,*) IOPT2,NCOL

 
C Correction of the columns NCOL to NPL
	IF(IOPT2.EQ.1)THEN
	PRINT 211,NCOL,NPL
	WRITE(3,211)NCOL,NPL
211	FORMAT('Correction of the columns: ',I4,' to ',I4)
	DO ILINE=1,NL
	  IM1=MAX(ILINE-1,1)
	  IP1=MIN(ILINE+1,NL)
	  X=0.5*(IMAGE(NCOL-2,IM1)+IMAGE(NCOL-1,IM1)+
     1	IMAGE(NCOL-2,IP1)+IMAGE(NCOL-1,IP1))
	  X=X+IMAGE(NCOL-2,ILINE)+IMAGE(NCOL-1,ILINE)
	    DO IC=NCOL,NPL
	     IMAGE(IC,ILINE)=X/4.
	    END DO
	END DO
	GO TO 88
	ENDIF
 
 
C Correction of the columns 1 to NCOL
	IF(IOPT2.EQ.2)THEN
	PRINT 212,NCOL
	WRITE(3,212)NCOL
212	FORMAT('Correction of the columns: 1 to ',I4)
	DO ILINE=1,NL
	  IM1=MAX(ILINE-1,1)
	  IP1=MIN(ILINE+1,NL)
	  X=0.5*(IMAGE(NCOL+2,IM1)+IMAGE(NCOL+1,IM1)+
     1	IMAGE(NCOL+2,IP1)+IMAGE(NCOL+1,IP1))
	  X=X+IMAGE(NCOL+2,ILINE)+IMAGE(NCOL+1,ILINE)
	    DO IC=1,NCOL
	      IMAGE(IC,ILINE)=X/4.
	    END DO
	END DO
	GO TO 88
	ENDIF
 
 
C--------------------------------------------------------------------
C Suppression of the first columns from 1 to ncol 
	IF(IOPT.EQ.31)THEN
	PRINT 214,NCOL
	WRITE(3,214)NCOL
214	FORMAT('Supression of the columns: 1 to',I4)
 
	DO IP=NCOL+1,NPL
	  IIP=IP-NCOL
	  DO IL=1,NL
	   IMAGE(IIP,IL)=IMAGE(IP,IL)
	  END DO
	END DO
 
	NPL=NPL-NCOL
	PRINT *,' NEW NPL =',NPL
	GO TO 88
	ENDIF
 
	GO TO 88
	ENDIF
 
C--------------------------------------------------------------------
C Suppression of the last columns from ncol to npl:
	IF(IOPT.EQ.32)THEN
	PRINT 213,NCOL,NPL
	WRITE(3,213)NCOL,NPL
213	FORMAT('Supression of the columns: ',I4,' to ',I4)
	NPL=NCOL-1
	PRINT *,' NEW NPL =',NPL
	GO TO 88
	ENDIF
 
C--------------------------------------------------------------------
C Correction of a single bad column
	IF(IOPT.EQ.4)THEN
 
	PRINT 419
419	FORMAT(' REMEMBER: KNOWN BAD COLUMNS:',/,
     1	' - T2M RCA CCD CAMERA (UNTIL MAY 86) : ',
     1	'(124 & 125),165,173',/,
     1	' - ESO RCA CCD CAMERA : (48 & 49)',/)
 
C	PRINT *,' NUMBER OF THE BAD COLUMN ?'
C	READ(5,*) IBADC
	PRINT 401,IBADC
	WRITE(3,401)IBADC
401	FORMAT(' CORRECTION OF THE BAD COLUMN NUMBER',I5)
	CALL BADCOLUMN1(IMAGE,NPL,NL,IBADC)
 
	GO TO 88
	ENDIF
 
 
C--------------------------------------------------------------------
C Correction of a double bad column
	IF(IOPT.EQ.5)THEN
 
	PRINT 419
C	PRINT *,' NUMBER OF THE FIRST OF THE 2 BAD COLUMNS ?'
C	READ(5,*) IBADC
	PRINT 501,IBADC,IBADC+1
	WRITE(3,501)IBADC,IBADC+1
501	FORMAT(' CORRECTION OF THE BAD COLUMNS NUMBER',I5,
     1	' AND ',I5)
	CALL BADCOLUMN2(IMAGE,NPL,NL,IBADC)
 
	GO TO 88
	ENDIF
 
C--------------------------------------------------------------------
C Correction of a triple bad column
	IF(IOPT.EQ.6)THEN
 
	PRINT 419
C	PRINT *,' Number of the first of the 3 bad columns?'
C	READ(5,*) IBADC
	PRINT 601,IBADC,IBADC+1,IBADC+2
	WRITE(3,601)IBADC,IBADC+1,IBADC+2
601	FORMAT(' Correction of the bad columns number',I5,1X,I5,
     1	' and ',I5)
	CALL BADCOLUMN3(IMAGE,NPL,NL,IBADC)
 
	GO TO 88
	ENDIF
 
C--------------------------------------------------------------------
C Correction of the cosmic rays
	IF(IOPT.EQ.7)THEN
 
        PRINT *,' Sigma limit for point rejection?'
	PRINT *,'           (3 SIGMA, 4.5 SIGMA...)'
        READ(5,*) ASIGMA
 
	PRINT 701,ASIGMA
	WRITE(3,701)ASIGMA
701	FORMAT(' REJECTION OF THE COSMIC RAYS AT ',
     1	F9.3,' SIGMA(s)')
	CALL COSMIC(IMAGE,NPL,NL,ASIGMA)
 
	GO TO 88
	ENDIF
 
C--------------------------------------------------------------------
C Rejection of aberrant points
	IF(IOPT.EQ.8)THEN
 
        PRINT *,' Lower and upper thresholds for point rejection?'
        READ(5,*) TMIN,TMAX
 
	PRINT 801,TMIN,TMAX
	WRITE(3,801)TMIN,TMAX
801	FORMAT('Lower and upper thresholds for point rejection',
     1	F12.3,2X,F12.3)
	CALL THRESHOLD(IMAGE,NPL,NL,TMIN,TMAX)
 
	GO TO 88
	ENDIF
 
C--------------------------------------------------------------------
C Rejection of known bad pixels
	IF(IOPT.EQ.9)THEN
 
	CALL BADPIXELS(IMAGE,NPL,NL)
 
	GO TO 88
	ENDIF
 
C------------------------------------------------------------------
C Output of the image and exit
	PRINT *,' OUTPUT IMAGE :'
	I=MAX(1,INDEX(COMMENTS,'//'))
	J=MAX(1,INDEX(NAME,'  '))
	WRITE(COMMENTS(I:80),854) NAME(1:J)
854	FORMAT('ccdclean with ',A,'//')
        WRITE(6,*) 'Output file: '
        READ(5,10) NAME
	CALL JLP_WRITEIMAG(IMAGE,NPL,NL,IDIM,NAME,COMMENTS)
 
	CLOSE(3)
	CALL JLP_END
	STOP
	END
C*************************************************************
C	SUBROUTINE COSMIC TO SUPPRESS THE COSMIC RAYS
C	ON CCD FRAMES
C*************************************************************
	SUBROUTINE COSMIC(IMAGE,NPL,NL,ASIGMA)
	PARAMETER (IDIM=600)
	REAL*4 IMAGE(IDIM,IDIM)
	REAL*8 TOTSUM,TOT2
 
C  EVALUATE NOISE
 
C Defines the working area
	JMIN=NL/2
	JMAX=MIN0(JMIN+100,NL-20)
	IMIN=NPL/2
	IMAX=MIN0(IMIN+100,NPL-20)
 
C First passage to calculate the mean
        NSUM=0
        TOTSUM=0.0D0
        TOT2=TOTSUM
 
        DO 18 J=JMIN,JMAX
        DO 18 I=IMIN,IMAX
        AIJ=IMAGE(I,J)
        NSUM=NSUM+1
        TOTSUM=TOTSUM+AIJ
        TOT2=TOT2+AIJ**2
18      CONTINUE
 
C TOTSUM : first mean
        TOTSUM=TOTSUM/DFLOAT(NSUM)
 
C TOT2 : first standard deviation
        TOT2=(TOT2/DFLOAT(NSUM))-TOTSUM**2
        TOT2=DSQRT(TOT2)
 
C ZUL: upper level
        ZUL=TOTSUM+3.*TOT2
 
C ZLL: low level
        ZLL=TOTSUM-3.*TOT2
 
C Second passage to calculate the standard deviation (discarding
C some points)
        NSUM=0
        TOTSUM=0.0D0
        TOT2=TOTSUM
 
        DO 19 J=JMIN,JMAX
        DO 19 I=IMIN,IMAX
        AIJ=IMAGE(I,J)
	  IF((AIJ.LE.ZUL).OR.(AIJ.GE.ZLL))THEN
	  NSUM=NSUM+1
	  TOTSUM=TOTSUM+AIJ
	  TOT2=TOT2+AIJ**2
	  ENDIF
19      CONTINUE
 
C TOTSUM : second mean (without extreme values)
        TOTSUM=TOTSUM/DFLOAT(NSUM)
 
C TOT2 : second standard deviation (in intensity)
        TOT2=(TOT2/DFLOAT(NSUM))-TOTSUM**2
        TOT2=DSQRT(TOT2)
 
	WRITE(6,1700) TOTSUM,TOT2
	WRITE(3,1700) TOTSUM,TOT2
1700    FORMAT(' MEAN OF PICTURE = ',E16.5,' NOISE = ',E16.5,
     1	/,' REJECTED POINTS :')
 
C SIG5 : criterium (normalised) to discard the points
        SIG5=ASIGMA*TOT2/TOTSUM
 
C NREJ: number of points discarded
        NREJ=0
 
C Main loop on the whole image
        DO 22 J=3,NL-2
        DO 22 I=3,NPL-2
        AIJ=IMAGE(I,J)
        NSUM=0
        TS=0.0
 
C First passage to calculate the mean in an elementary grid (3x3)
        DO 23 JJ=J-1,J+1
        DO 23 II=I-1,I+1
        IF((II.EQ.I).AND.(JJ.EQ.J)) GO TO 23
        NSUM=NSUM+1
        TS=TS+IMAGE(II,JJ)
23      CONTINUE
 
C A0 : local mean
        A0=TS/FLOAT(NSUM)
 
        TS=0.0
        NSUM=0
 
C Second passage to calculate the mean in an elementary grid (3x3)
C but without taking into account the "aberrant" values
        DO 27 JJ=J-1,J+1
        DO 27 II=I-1,I+1
        IF((II.EQ.I).AND.(JJ.EQ.J)) GO TO 27
        AIIJJ=IMAGE(II,JJ)
        SD=ABS(AIIJJ-A0)
        ZNOI=SIG5*A0*1.5
 
C Test whether the value is worth taking into account for the mean
C (CRITERIUM*1.5)
          IF(SD.LE.ZNOI)THEN
          NSUM=NSUM+1
          TS=TS+AIIJJ
	  ENDIF
 
27      CONTINUE
 
        IF(NSUM.EQ.0) GO TO 22
        TS=TS/FLOAT(NSUM)
        SD=ABS(AIJ-TS)
        ZNOI=SIG5*TS
 
C Test whether the central value of the grid is acceptable or not
C If not, we replace the previous value by the mean TS
 
        IF(SD.GE.ZNOI)THEN
	IMAGE(I,J)=TS
        NREJ=NREJ+1
C        WRITE(6,39) I,J,IMAGE(I,J),AIJ
	WRITE(3,39) I,J,IMAGE(I,J),AIJ
39	FORMAT(' PIXEL=',I4,' LINE=',I4,' NEW VALUE =',
     1	E12.3,' PREVIOUS VALUE =',E12.3)
        GO TO 22
222     PRINT *,' POINT ',I,J,' NOT REJECTED BUT DUFF'
	ENDIF
 
22      CONTINUE
 
	PRINT 40,NREJ
	WRITE(3,40)NREJ
40	FORMAT(2X,I6,' GROTTY POINTS REJECTED')
	RETURN
	END
 
C*************************************************************
C	Subroutine THRESHOLD to eliminate aberrant points
C	from CCD frames
C*************************************************************
	SUBROUTINE THRESHOLD(IMAGE,NPL,NL,TMIN,TMAX)
	PARAMETER (IDIM=600)
	REAL*4 IMAGE(IDIM,IDIM)
 
	NUMBER=0
 
	DO ILIN=2,NL-1
	DO IPIX=2,NPL-1
	WORK=IMAGE(IPIX,ILIN)
 
	IF(WORK.LT.TMIN.OR.WORK.GT.TMAX)THEN
	NUMBER=NUMBER+1
C	ILINM1=MAX(1,ILIN-1)
C	ILINP1=MIN(ILIN+1,NL)
C	IPIXM1=MAX(1,IPIX-1)
C	IPIXP1=MIN(IPIX+1,NPL)
	IMAGE(IPIX,ILIN)=0.25*(IMAGE(IPIX,ILIN-1)+
     1	IMAGE(IPIX,ILIN+1)+IMAGE(IPIX-1,ILIN)+
     1	IMAGE(IPIX+1,ILIN))
        PRINT 30,IPIX,ILIN,IMAGE(IPIX,LIN),WORK
	WRITE(3,30)IPIX,ILIN,IMAGE(IPIX,ILIN),WORK
30	FORMAT(' PIXEL=',I4,' LINE=',I4,' NEW VALUE =',
     1	E12.3,' PREVIOUS VALUE =',E12.3)
	ENDIF
 
	END DO
	END DO
 
	PRINT 31,NUMBER
	WRITE(3,31)NUMBER
31	FORMAT(2X,I5,' POINTS REJECTED')
 
	RETURN
	END
C------------------------------------------------------------------
C	Subroutine CLINET from "CLINET.FOR"
C Version of December 22nd 1986
C
C CLICHE NET=(CLICHE BRUT EN I*2 (+0.5)- OFFSET*(K1/KM))/PLU MOYEN
C
C PLU MOYEN (NORMALISE A 1.) : real*4 BDF file
C CLICHE BRUT : real*4 BDF file
C OFFSET : constant or real*4 BDF file
C K1 : VALEUR MOY DE L"OFFSET PRES DE L"OBJET
C KM : VALEUR MOY DE L"OFFSET APRES LA SERIE TRITIUM
C-------------------------------------------------------------------
	SUBROUTINE CLINET(OUTPUT,NPL,NL,NAME1)
	PARAMETER (IDIM=600)
	REAL*4 PLU(IDIM,IDIM),OFFSET(IDIM,IDIM)
	REAL*4 INPUT(IDIM,IDIM),OUTPUT(IDIM,IDIM)
	CHARACTER REP*1,NAME*40,NAME1*40,COMMENTS*80
	CHARACTER REPCOLUMN*1
 
10	FORMAT(A)
 
	PRINT 13
13	FORMAT(/,' OPERATION QUE L"ON VA FAIRE:',/,
     1	' CTE*(CLICHE BRUT+.5 - BIAS)',
     1	'/FLAT FIELD')
	PRINT 104
104	FORMAT(' CONSTANTE QUE L"ON MULTIPLIE AU RESULTAT:')
	READ(5,*) CTE
 
C Input of the image :
	PRINT *,' INPUT IMAGE :'
	NAME1=' '
	CALL JLP_READIMAG(INPUT,NPL,NL,IDIM,NAME1,COMMENTS)
 
C Input of the flat field :
	PRINT *,' FLAT FIELD :'
        READ(5,10) NAME
	CALL JLP_READIMAG(PLU,NPLU,NLU,IDIM,NAME,COMMENTS)
	I=MAX(1,INDEX(NAME,'  '))
	J=40-I-1
	IF(J.GT.3) WRITE(NAME1(I:40),10) NAME(1:J)
 
C Input of the bias :
	PRINT 879
879	FORMAT(' FOR THE BIAS SUBTRACTION DO YOU WANT TO USE :',
     1	/,' 1: A CONSTANT ?',/,
     1	' 2: A FILE ?')
	READ(5,*) IFO
 
	IF( IFO.EQ.2) THEN
	   PRINT *,' MEAN BIAS'
           READ(5,10) NAME
	   CALL JLP_READIMAG(OFFSET,NPLO,NLO,IDIM,NAME,COMMENTS)
	ELSE
	   PRINT *,' BIAS VALUE :'
	   READ(5,*) COPF
	ENDIF
 
C Option 1 : subtraction of a constant for the bias :
	IF(IFO.EQ.1) THEN
 
	DO ILINE=1,NL
	DO IPIX=1,NPL
	DIF=PLU(IPIX,ILINE)
 
	WORK=INPUT(IPIX,ILINE)+0.5-COPF
	   IF(DIF.GT.0.0001) THEN
	   OUTPUT(IPIX,ILINE)=CTE*WORK/DIF
	   ELSE
	   OUTPUT(IPIX,ILINE)=0.
	   ENDIF
	END DO
	END DO
 
C Option 2 : subtraction of a mean bias image :
	ELSE
 
	DO ILINE=1,NL
	DO IPIX=1,NPL
	DIF=PLU(IPIX,ILINE)
 
	WORK=INPUT(IPIX,ILINE)+0.5-OFFSET(IPIX,ILINE)
	   IF(DIF.GT.0.0001) THEN
	   OUTPUT(IPIX,ILINE)=CTE*WORK/DIF
	   ELSE
	   OUTPUT(IPIX,ILINE)=0.
	   ENDIF
	END DO
	END DO
 
	ENDIF
 
C Correction of bad columns for the RCA chip of PIC DU MIDI (February 1986)
C
C	PRINT *,' DO YOU WANT TO CORRECT THE BAD COLUMNS'
C	PRINT *,' OF THE T2M RCA CCD (FEB. 86) ? (N)'
C	READ(5,10) REPCOLUMN
C
C	IF(REPCOLUMN.EQ.'Y'.OR.REPCOLUMN.EQ.'y')THEN
C	PRINT 19
C19	FORMAT(' CORRECTION OF THE BAD COLUMNS OF THE',
C     1	' T2M RCA CCD (FEB. 86)',/,
C     1	'          (124 & 125),(165,166 & 167),173',/)
C
C	IBADC=124
C	CALL BADCOLUMN2(OUTPUT,NPL,NL,IBADC)
C	IBADC=165
C	CALL BADCOLUMN3(OUTPUT,NPL,NL,IBADC)
C	IBADC=173
C	CALL BADCOLUMN1(OUTPUT,NPL,NL,IBADC)
C	ENDIF
 
	RETURN
	END
C*************************************************************
C	Subroutine BADPIXELS to eliminate known bad pixels
C	from ESO CCD frames or SSO 40 inch CCD frames.
C*************************************************************
	SUBROUTINE BADPIXELS(IMAGE,NPL,NL)
	PARAMETER (IDIM=600)
	REAL*4 IMAGE(IDIM,IDIM)
	INTEGER*4 IXBAD(100),IYBAD(100)
 
	PRINT 20
20	FORMAT(' ONLY 2 POSSIBILITIES :',/,
     1	' 1. ESO 1985',/,
     1	' 2. 40" CCD SSO',/,
     1	' ENTER THE OPTION : ')
	READ(5,*) IOPT
 
C Option 1 : ESO
 
	IF(IOPT.EQ.1)THEN
	NUMBER=32
 
	IXBAD(1)=31
	IYBAD(1)=203
	IXBAD(2)=31
	IYBAD(2)=204
	IXBAD(3)=32
	IYBAD(3)=65
	IXBAD(4)=63
	IYBAD(4)=82
	IXBAD(5)=81
	IYBAD(5)=43
	IXBAD(6)=124
	IYBAD(6)=51
	IXBAD(7)=129
	IYBAD(7)=304
	IXBAD(8)=132
	IYBAD(8)=445
	IXBAD(9)=108
	IYBAD(9)=404
	IXBAD(10)=114
	IYBAD(10)=107
	IXBAD(11)=114
	IYBAD(11)=108
	IXBAD(12)=114
	IYBAD(12)=109
	IXBAD(13)=159
	IYBAD(13)=85
	IXBAD(14)=138
	IYBAD(14)=225
	IXBAD(15)=164
	IYBAD(15)=127
	IXBAD(16)=188
	IYBAD(16)=197
	IXBAD(17)=215
	IYBAD(17)=149
	IXBAD(18)=226
	IYBAD(18)=19
	IXBAD(19)=226
	IYBAD(19)=20
	IXBAD(20)=262
	IYBAD(20)=42
	IXBAD(21)=279
	IYBAD(21)=38
	IXBAD(22)=282
	IYBAD(22)=295
	IXBAD(23)=304
	IYBAD(23)=444
	IXBAD(24)=304
	IYBAD(24)=445
	IXBAD(25)=305
	IYBAD(25)=444
	IXBAD(26)=305
	IYBAD(26)=445
	IXBAD(27)=317
	IYBAD(27)=217
	IXBAD(28)=317
	IYBAD(28)=218
	IXBAD(29)=317
	IYBAD(29)=219
	IXBAD(30)=302
	IYBAD(30)=238
	IXBAD(31)=231
	IYBAD(31)=449
	IXBAD(32)=121
	IYBAD(32)=494
 
C Correction of the bad pixels :
 
	DO I=1,NUMBER
	IPIX=IXBAD(I)
	ILIN=IYBAD(I)
 
	WORK=IMAGE(IPIX,ILIN)
 
	IMAGE(IPIX,ILIN)=0.25*(IMAGE(IPIX,ILIN-1)+
     1	IMAGE(IPIX,ILIN+1)+IMAGE(IPIX-1,ILIN)+
     1	IMAGE(IPIX+1,ILIN))
 
C        PRINT 30,IPIX,ILIN,IMAGE(IPIX,LIN),WORK
	WRITE(3,30)IPIX,ILIN,IMAGE(IPIX,ILIN),WORK
30	FORMAT(' PIXEL=',I4,' LINE=',I4,' NEW VALUE =',
     1	E11.3,' PREVIOUS VALUE =',E11.3)
 
	END DO
 
	ENDIF
 
C Option 2 : 40"
	IF(IOPT.EQ.2)THEN
	NUMBER=72
 
	IXBAD(1)=64
	IYBAD(1)=43
	IXBAD(2)=64
	IYBAD(2)=44
	IXBAD(3)=51
	IYBAD(3)=174
	IXBAD(4)=51
	IYBAD(4)=174
	IXBAD(5)=28
	IYBAD(5)=225
	IXBAD(6)=28
	IYBAD(6)=226
	IXBAD(7)=28
	IYBAD(7)=227
	IXBAD(8)=28
	IYBAD(8)=228
	IXBAD(9)=28
	IYBAD(9)=229
	IXBAD(10)=159
	IYBAD(10)=356
	IXBAD(11)=159
	IYBAD(11)=357
	IXBAD(12)=159
	IYBAD(12)=358
	IXBAD(13)=159
	IYBAD(13)=359
	IXBAD(14)=303
	IYBAD(14)=157
	IXBAD(15)=303
	IYBAD(15)=158
	IXBAD(16)=303
	IYBAD(16)=159
	IXBAD(17)=303
	IYBAD(17)=160
	IXBAD(18)=303
	IYBAD(18)=161
	IXBAD(19)=303
	IYBAD(19)=162
	IXBAD(20)=303
	IYBAD(20)=163
	IXBAD(21)=140
	IYBAD(21)=376
	IXBAD(22)=140
	IYBAD(22)=377
	IXBAD(23)=140
	IYBAD(23)=378
	IXBAD(24)=140
	IYBAD(24)=379
	IXBAD(25)=140
	IYBAD(25)=380
	IXBAD(26)=140
	IYBAD(26)=381
	IXBAD(27)=140
	IYBAD(27)=382
	IXBAD(28)=140
	IYBAD(28)=383
	IXBAD(29)=140
	IYBAD(29)=384
	IXBAD(30)=124
	IYBAD(30)=385
	IXBAD(31)=124
	IYBAD(31)=386
	IXBAD(32)=108
	IYBAD(32)=436
	IXBAD(33)=108
	IYBAD(33)=437
	IXBAD(34)=108
	IYBAD(34)=438
	IXBAD(35)=108
	IYBAD(35)=439
	IXBAD(36)=108
	IYBAD(36)=440
	IXBAD(37)=108
	IYBAD(37)=441
	IXBAD(38)=108
	IYBAD(38)=442
	IXBAD(39)=108
	IYBAD(39)=443
	IXBAD(40)=59
	IYBAD(40)=492
	IXBAD(41)=59
	IYBAD(41)=493
	IXBAD(42)=59
	IYBAD(42)=494
	IXBAD(43)=59
	IYBAD(43)=495
	IXBAD(44)=59
	IYBAD(44)=496
	IXBAD(45)=59
	IYBAD(45)=497
	IXBAD(46)=59
	IYBAD(46)=498
	IXBAD(47)=67
	IYBAD(47)=513
	IXBAD(48)=67
	IYBAD(48)=514
	IXBAD(49)=67
	IYBAD(49)=515
	IXBAD(50)=67
	IYBAD(50)=516
	IXBAD(51)=67
	IYBAD(51)=517
	IXBAD(52)=67
	IYBAD(52)=518
	IXBAD(53)=67
	IYBAD(53)=519
	IXBAD(54)=67
	IYBAD(54)=520
	IXBAD(55)=67
	IYBAD(55)=521
	IXBAD(56)=67
	IYBAD(56)=522
	IXBAD(57)=67
	IYBAD(57)=523
	IXBAD(58)=225
	IYBAD(58)=435
	IXBAD(59)=225
	IYBAD(59)=436
	IXBAD(60)=225
	IYBAD(60)=437
	IXBAD(61)=225
	IYBAD(61)=438
	IXBAD(62)=225
	IYBAD(62)=439
	IXBAD(63)=240
	IYBAD(63)=433
	IXBAD(64)=240
	IYBAD(64)=434
	IXBAD(65)=240
	IYBAD(65)=435
	IXBAD(66)=240
	IYBAD(66)=436
	IXBAD(67)=240
	IYBAD(67)=437
	IXBAD(68)=240
	IYBAD(68)=438
	IXBAD(69)=240
	IYBAD(69)=439
	IXBAD(70)=240
	IYBAD(70)=440
	IXBAD(71)=205
	IYBAD(71)=413
	IXBAD(72)=205
	IYBAD(72)=414
 
C Correction of the bad pixels with a mean of the adjacent columns
C (because for SSO there are mainly parts of bad columns) :
 
	DO I=1,NUMBER
	IPIX=IXBAD(I)
	ILIN=IYBAD(I)
 
	WORK=IMAGE(IPIX,ILIN)
 
	IMAGE(IPIX,ILIN)=0.5*(IMAGE(IPIX-1,ILIN)+
     1	IMAGE(IPIX+1,ILIN))
 
C        PRINT 30,IPIX,ILIN,IMAGE(IPIX,LIN),WORK
	WRITE(3,30)IPIX,ILIN,IMAGE(IPIX,ILIN),WORK
 
	END DO
 
	ENDIF
 
	PRINT 31,NUMBER
	WRITE(3,31)NUMBER
31	FORMAT(2X,I5,' POINTS REJECTED')
 
	RETURN
	END
C*************************************************************
	include 'jlpsub:badcolumn.for'
