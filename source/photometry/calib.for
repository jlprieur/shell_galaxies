C++*************************************************************
C	PROGRAM CALIB
C Program to get the values for the constants to calibrate
C CCD frames
C
C Version of July 18 th 1986
C***************************************************************
C
C Colors:
C	B : 1
C	V : 2
C	R : 3
C	I : 4
C
C CATMAG (N,ICOLOR) : Catalogue magnitude in ICOLOR of the object N
C OUTMAG : Magnitude of the data corrected from atmospheric absorption
C MAGREF : working array where the objects are sorted according to the data
C XX1 : "color index" in intensity (data)
C X1 :
C X2 :
C Y1 :
C--*************************************************************
	PROGRAM CALIB
	PARAMETER (IDIM=1000)
	REAL*4 AIRMASS(IDIM),MAGREF(IDIM,5)
	REAL*4 OUTMAG(IDIM),CATMAG(IDIM,5),COUNTS(IDIM)
	REAL*8 XX1(IDIM),XX(IDIM),YY(IDIM),XC(20),SDEV(20)
	REAL*8 ERR,WW1,WW2
	REAL*4 X1(IDIM),X2(IDIM),Y1(IDIM)
	INTEGER*4 IFILTER(IDIM),NUMB1(IDIM),NUMB2(IDIM)
	INTEGER*4 INDEX(IDIM),INDEX1(IDIM)
	CHARACTER*2 NAMECAT(IDIM),NAMEDATA(IDIM),WORD
	CHARACTER*1 FILTER(IDIM),SYMBOL(4)
	CHARACTER NAME*30,TITLE*80,ANS*1
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40
 
	COMMON/BLOCKA/NCAT,NUMBER,NAMECAT,OUTMAG,IFILTER,
     1	INDEX,SYMBOL,CATMAG,AIRMASS,COUNTS
	DATA SYMBOL/'B','V','R','I'/
	OPEN(10,FILE='calib.log',STATUS='unknown',
     1	ACCESS='SEQUENTIAL')
10	FORMAT(A)
11	PRINT 300
300	FORMAT(' MENU:',/,
     1	' 1: INPUT OF A CATALOGUE',/,
     1	' 2: INPUT OF A DATA FILE',/,
     1	' 3: CREATION OF A CATALOGUE',/,
     1	' 4: CREATION OF A DATA FILE',/,
     1	' 5: CORRECTION FOR ATMOSPHERIC ABSORPTION',/,
     1	' 6: DETERMINATION OF THE CONSTANTS WITHOUT COLOR TERM',/,
     1	' 7: CONSTANTS WITH COLOR TERM AND MEAN FOR EACH STAR',/,
     1	' 8: CONSTANTS WITH COLOR TERM AND ALL THE MEASUREMENTS',/,
     1	' 10: EXIT',/,' ENTER THE OPTION YOU WANT: ',$)
	READ(5,*) IOPT
 
C*******************************************************************
C Option 1: Input of a catalogue
C with the published photometric measurements:
C*******************************************************************
 
	IF(IOPT.EQ.1)THEN
201	PRINT *,' NAME OF THE INPUT CATALOGUE ?'
	READ(5,10) NAME
	WRITE(10,208)NAME
208	FORMAT(/,/,5(1H*),' READING THE CATALOGUE :',A)
	OPEN(1,FILE=NAME,STATUS='OLD',ACCESS='SEQUENTIAL',
     1	ERR=201)
	READ(1,109)TITLE,NCAT
	PRINT 202,TITLE,NCAT
	WRITE(10,202)TITLE,NCAT
202	FORMAT(' TITLE : ',A,/,' NUMBER OF STARS :',I5,/)
	PRINT 205
	WRITE(10,205)
205	FORMAT(4X,'NAME',8X,'V',12X,'B-V',10X,'V-R',10X,'R-I',/)
 
	DO I=1,NCAT
	READ(1,104)NAMECAT(I),(CATMAG(I,KK),KK=1,4)
 
C Creation of a 5th term to facilitate the handling of the
C color indices in the loops
	CATMAG(I,5)=CATMAG(I,4)
 
C Printing the catalogue
	VV=CATMAG(I,2)
	BV=CATMAG(I,1)-CATMAG(I,2)
	VR=CATMAG(I,2)-CATMAG(I,3)
	RI=CATMAG(I,3)-CATMAG(I,4)
	WRITE(10,204)NAMECAT(I),VV,BV,VR,RI
	PRINT 204,NAMECAT(I),VV,BV,VR,RI
	END DO
 
204	FORMAT(5X,A2,5X,4(F8.3,5X))
	CLOSE(1)
 
	GO TO 11
	ENDIF
 
 
C*******************************************************************
C Option 2 : Input of a data file :
C*******************************************************************
 
	IF(IOPT.EQ.2)THEN
401	PRINT *,' NAME OF THE INPUT FILE ?'
	READ(5,10) NAME
	WRITE(10,408)NAME
408	FORMAT(/,/,5(1H*),' READING THE DATA FILE :',A)
	OPEN(1,FILE=NAME,STATUS='OLD',ACCESS='SEQUENTIAL',
     1	ERR=401)
	READ(1,10)TITLE
	READ(1,*)NUMBER
	PRINT 402,TITLE,NUMBER
	WRITE(10,402)TITLE,NUMBER
402	FORMAT(' TITLE : ',A,/,' NUMBER OF STARS :',I5,/)
	PRINT 405
	WRITE(10,405)
405	FORMAT(4X,'NAME',2X,'FILTER',5X,'COUNTS/S',8X,
     1	'RAW MAG.',4X,'AIRMASS',/)
	DO I=1,4
	NUMB1(I)=0
	END DO
 
	DO I=1,NUMBER
	READ(1,304)NAMEDATA(I),FILTER(I),COUNTS(I),AIRMASS(I)
	RAWMAG=-2.5*ALOG10(COUNTS(I))
	PRINT 404,NAMEDATA(I),FILTER(I),COUNTS(I),
     1	RAWMAG,AIRMASS(I)
	WRITE(10,404)NAMEDATA(I),FILTER(I),COUNTS(I),
     1	RAWMAG,AIRMASS(I)
 
	WORD=NAMEDATA(I)
C search for the name in the catalog
	CALL SEARCH(WORD,NAMECAT,NCAT,INDEX(I))
	IF(INDEX(I).EQ.0)THEN
	PRINT 18,I
	WRITE(10,18)I
18	FORMAT(' DATA NUMBER :',I4,' NOT IN THE CATALOGUE !')
	ENDIF
 
	DO J=1,4
	IF(FILTER(I).EQ.SYMBOL(J))THEN
	IFILTER(I)=J
	NUMB1(J)=NUMB1(J)+1
	ENDIF
	END DO
 
	END DO
 
404	FORMAT(5X,A2,6X,A1,3X,E12.3,5X,F10.3,5X,F6.3)
	PRINT 19,(SYMBOL(KK),NUMB1(KK),KK=1,4)
	WRITE(10,19)(SYMBOL(KK),NUMB1(KK),KK=1,4)
19	FORMAT(/,' NUMBER OF MEASUREMENTS IN :',/,
     1	4(4X,A1,' : ',I4,/),/)
 
	CLOSE(1)
	GO TO 11
	ENDIF
 
C***************************************************************
C Option 3 : Creation of a catalogue
C with the published photometric measurements:
C***************************************************************
 
	IF(IOPT.EQ.3)THEN
101	PRINT *,' NAME OF THE OUTPUT CATALOGUE ?'
	READ(5,10) NAME
	WRITE(10,108)NAME
108	FORMAT(/,/,5(1H*),' CREATION OF THE CATALOGUE :',A)
	OPEN(1,FILE=NAME,STATUS='NEW',ACCESS='SEQUENTIAL',
     1	ERR=101)
	PRINT *,' TITLE :'
	READ(5,10) TITLE
	WRITE(10,105)
105	FORMAT(4X,'NAME',9X,'B',13X,'V',13X,'R',13X,'I'/)
 
	DO I=1,IDIM
	PRINT 102
	READ(5,10) NAMECAT(I)
	IF(NAMECAT(I).EQ.'  ')GO TO 110
	PRINT 103
	READ(5,*) VV,BV,VR,RI
	CATMAG(I,1)=BV+VV
	CATMAG(I,2)=VV
	CATMAG(I,3)=VV-VR
	CATMAG(I,4)=CATMAG(I,3)-RI
	WRITE(10,104)NAMECAT(I),(CATMAG(I,KK),KK=1,4)
	END DO
102	FORMAT(' ENTER THE NAME OF THE STAR : (2 CHARACTERS)')
103	FORMAT(' V, B-V, V-R, AND R-I ?')
104	FORMAT(5X,A2,5X,4(F9.4,5X))
 
110	NCAT=I-1
	WRITE(1,109)TITLE,NCAT
109	FORMAT(4X,'TITLE :',A,/,4X,'NUMBER OF STARS :',I5)
	WRITE(10,105)
	DO I=1,NCAT
	WRITE(1,104)NAMECAT(I),(CATMAG(I,KK),KK=1,4)
	END DO
	CLOSE(1)
	GO TO 11
	ENDIF
 
C*******************************************************************
C Option 4 : Creation of a data file :
C*******************************************************************
 
	IF(IOPT.EQ.4)THEN
301	PRINT *,' NAME OF THE OUTPUT DATA FILE ?'
	READ(5,10) NAME
	WRITE(10,308)NAME
308	FORMAT(/,/,5(1H*),' CREATION OF THE DATA FILE :',A)
	OPEN(1,FILE=NAME,STATUS='NEW',ACCESS='SEQUENTIAL',
     1	ERR=301)
	PRINT *,' TITLE :'
	READ(5,10) TITLE
	WRITE(1,10)TITLE
 
	DO I=1,IDIM
	PRINT 302
	READ(5,10) NAMEDATA(I)
	IF(NAMEDATA(I).EQ.'  ')GO TO 310
	PRINT 303
	READ(5,10) FILTER(I)
	
	DO J=1,4
	IF(FILTER(I).EQ.SYMBOL(J))IFILTER(I)=J
	END DO
 
	PRINT 306
	READ(5,*) CTS,TIME,AIRMASS(I)
	COUNTS(I)=CTS/TIME
	END DO
 
302	FORMAT(' ENTER THE NAME OF THE STAR : (2 CHARACTERS)')
303	FORMAT(' FILTER ? ',$)
306	FORMAT(' TOTAL COUNT NUMBER, EXPOSURE, AND AIRMASS ?')
310	NUMBER=I-1
	WRITE(1,*)NUMBER
	WRITE(10,305)
305	FORMAT(4X,'NAME',4X,'FILTER',8X,'COUNTS/S',
     1	11X,'AIR MASS',/)
 
	DO I=1,NUMBER
	WRITE(10,304)NAMEDATA(I),FILTER(I),COUNTS(I),AIRMASS(I)
	WRITE(1,304)NAMEDATA(I),FILTER(I),COUNTS(I),AIRMASS(I)
	END DO
304	FORMAT(5X,A2,8X,A1,7X,E12.3,5X,F7.3)
 
	CLOSE(1)
	GO TO 11
	ENDIF
C*****************************************************************
C Option 5 : correction for atmsospheric absorption
C*****************************************************************
	IF(IOPT.EQ.5)THEN
	  CALL ATMCORRECTION
	  GO TO 11
	ENDIF
C******************************************************************
C Option 6 : determination of the constants without color terms
C******************************************************************
	IF(IOPT.EQ.6)THEN
	  WRITE(10,612)
612	  FORMAT(/,/,5(1H*),' OPTION 6 : CONSTANT WITHOUT'
     1	' COLOR TERM')
 
	  PRINT 603
603	  FORMAT(' REMEMBER :',/,' 1:=B   2:=V  ',
     1	' 3:=R   4:=I',/,' NUMBER YOU WANT ? ',$)
	  READ(5,*) KCOLOR
 
	  PRINT *,' DO YOU WANT TO WORK WITH THE MEAN ',
     1	'FOR EACH STAR ? (N)'
	  READ(5,10) ANS
 
C Calculating the mean and the standard deviation
	IF(ANS.EQ.'Y')THEN
	  CALL OPTION6(KCOLOR,CSTE0,STDEV,XX,YY,NPOINT,
     1	CATMAG)
	ELSE
	  CALL OPTION8(KCOLOR,CSTE0,STDEV,XX,YY,NPOINT,
     1	CATMAG)
	ENDIF
 
C Generating a graphic file
C	CHAR1=' INDEX'
C	CHAR2(1:1)=SYMBOL(KCOLOR)
C	CHAR2(2:11)=' INSTR. - '
C	CHAR2(12:12)=SYMBOL(KCOLOR)
C	CHAR2(13:20)=' CAT.'
C	CHAR3=' '
C	CHAR3(1:30)=NAME
C	KDEGREE=0
C	XC(1)=CSTE0
C	XSTART=0.
C	XEND=FLOAT(NPOINT)
C
C	CALL OPTION9(XX,YY,NPOINT,XC,KDEGREE,XSTART,XEND,
C     1	CHAR1,CHAR2,CHAR3)
 
C Return to the menu...
	GO TO 11
	ENDIF
 
C******************************************************************
C Options 7 and 8 :
C Determination of the constants without color terms
C Displaying the curve COLOR CAT. versus COLOR INSTR., and fitting
C a straight line (indices calculated for each star with the mean
C of all the measurements). Then the constants can be calculted in
C two different ways (either taking all the measurements independantly
C or by calculating the mean for each star (in intensity) before.
C******************************************************************
	IF(IOPT.EQ.7.OR.IOPT.EQ.8)THEN
	  IF(IOPT.EQ.7)THEN
	    WRITE(10,712)
712	    FORMAT(/,/,5(1H*),' OPTION 7 : CONSTANTS WITH'
     1	' COLOR TERM TAKING THE MEAN FOR',
     1	' EACH STAR')
	  ELSE
	    WRITE(10,812)
812	    FORMAT(/,/,5(1H*),' OPTION 8 : CONSTANTS WITH'
     1	' COLOR TERM WITH ALL THE MEASUREMENTS')
	  ENDIF
 
	  PRINT 701
701	  FORMAT(' COLOR INDEX YOU WANT TO WORK WITH :',/,
     1	' 1,2 : B-V',/,' 2,3 : V-R',/,' 1,3 : B-R',
     1	/,' ENTER THE NUMBERS YOU WANT :',$)
	  READ(5,*) KK1,KK2
 
	    IF(KK1.EQ.1.AND.KK2.EQ.3)IOPT7=3
 
	  PRINT 729,SYMBOL(KK1),SYMBOL(KK2),SYMBOL(KK1),
     1	SYMBOL(KK2),SYMBOL(KK1),SYMBOL(KK2)
	  WRITE(10,729)SYMBOL(KK1),SYMBOL(KK2),SYMBOL(KK1),
     1	SYMBOL(KK2),SYMBOL(KK1),SYMBOL(KK2)
729	  FORMAT(/,4X,'NAME',5X,A1,'-',A1,' INST',6X,
     1	A1,'-',A1,'  CAT',
     1	2X,'NUMBER OF VALUES IN ',A1,' AND ',A1,/)
 
C Generating an array of mean measurements for all the stars
C for which it is possible
 
	  NPT1=0
	  DO J=1,NCAT
	    Y1(J)=CATMAG(J,KK1)-CATMAG(J,KK2)
	    X1(J)=0.
	    X2(J)=0.
	    NUMB1(J)=0
	    NUMB2(J)=0
 
	    DO I=1,NUMBER
	      KK=IFILTER(I)
	      INDX=INDEX(I)
 
	      IF(INDX.EQ.J)THEN
 
	        IF(KK.EQ.KK1)THEN
	          X1(J)=X1(J)+OUTMAG(I)
	          NUMB1(J)=NUMB1(J)+1
	        ENDIF
 
	        IF(KK.EQ.KK2)THEN
	          X2(J)=X2(J)+OUTMAG(I)
	          NUMB2(J)=NUMB2(J)+1
	        ENDIF
 
	     ENDIF
 
	  END DO
 
	  IF(NUMB1(J).NE.0.AND.NUMB2(J).NE.0)THEN
	    NPT1=NPT1+1
	    YY(NPT1)=Y1(J)
	    XX1(NPT1)=(X1(J)/NUMB1(J))-(X2(J)/NUMB2(J))
	    INDEX1(NPT1)=J
	    PRINT 730,NAMECAT(J),XX1(NPT1),YY(NPT1),NUMB1(J),
     1	NUMB2(J)
	    WRITE(10,730) NAMECAT(J),XX1(NPT1),YY(NPT1),
     1	NUMB1(J),NUMB2(J)
730	    FORMAT(4X,A2,5X,F9.4,5X,F9.4,10X,I4,5X,I4)
	  ENDIF
 
	END DO
 
	PRINT 736,NPT1
	WRITE(10,736)NPT1
736	FORMAT(/,' CALLING POLYFIT WITH ',I4,' POINTS')
 
C Calling POLYFIT with degree=1, the coeff. are in XC
	KK=1
	CALL POLYFIT(XX1,YY,NPT1,KK,XC,SDEV,ERR)
	PRINT 731,XC(2),SDEV(2),XC(1),SDEV(1),ERR
	WRITE(10,731) XC(2),SDEV(2),XC(1),SDEV(1),ERR
731	FORMAT(/,' SOLUTION FOUND  FITTING A STRAIGHT LINE :',
     1	3X,' COLOR CAT = AA*(COLOR INSTR) + BB',/,
     1	5X,' AA =',F8.3,' +/-',F8.3,5X,' BB =',F8.3,
     1	' +/-',F8.3,/,
     1	5X,' MEAN DEVIATION FOR THIS FIT : ',F8.3)
 
C Generating a graphic file
	  WRITE(CHAR1,786)SYMBOL(KK1),SYMBOL(KK2)
786	  FORMAT(' ',A1,'-',A1,' INSTR.')
	  WRITE(CHAR2,787)SYMBOL(KK1),SYMBOL(KK2)
787	  FORMAT(' ',A1,'-',A1,' CATAL.')
	  CHAR3=' '
 	  CHAR3(1:30)=NAME
	  KDEGREE=1
	  XSTART=0.
	  XEND=1
 
 	  CALL OPTION9(XX1,YY,NPT1,XC,KDEGREE,XSTART,XEND,
     1	CHAR1,CHAR2,CHAR3)
 
 
C*********************************************************************
C For AAT 2-color photometry, we propose here a set of formulae
C proposed by Dave Carter :
C*********************************************************************
	IF(IOPT7.EQ.3)THEN
 
	  COEFF1=XC(1)
	  ERRCOEFF1=SDEV(1)
	  COEFF2=1.-XC(2)
	  ERRCOEFF2=SDEV(2)
 
C Calculating the mean and the standard deviation with all the values
C Calling OPTION6 if IOPT=7 and OPTION8 if IOPT=8
	  KCOLOR=3	!RED
 
	  IF(IOPT.EQ.7)THEN
	    CALL OPTION6(KCOLOR,CSTE0,STDEV,XX,YY,NPOINT,
     1	CATMAG)
	  ELSE
	    CALL OPTION8(KCOLOR,CSTE0,STDEV,XX,YY,NPOINT,
     1	CATMAG)
	  ENDIF
 
	  COEFF3=CSTE0
	  ERRCOEFF3=STDEV
	  ERR3=STDEV
 
C Calculating the mean and the standard deviation with all the values
C Calling OPTION6 if IOPT=7 OPTION8 if IOPT=8
	  KCOLOR=1	!BLUE
 
	  DO I=1,NPT1
	    J=INDEX1(I)
C Option with term in (INSTRUMENTAL COLOR) :
C	    MAGREF(J,KCOLOR)=CATMAG(J,KCOLOR)+COEFF2*XX1(I)
C Option with term in (CATALOGUE COLOR) :
	    MAGREF(J,KCOLOR)=CATMAG(J,KCOLOR)+COEFF2*Y1(I)
	    PRINT 758,J,KCOLOR,MAGREF(J,KCOLOR),CATMAG(J,KCOLOR)
758	    FORMAT(' J,KCOLOR,MAGREF,CATMAG :',I4,2X,I4,2X,
     1	F9.3,2X,F9.3)
	  END DO
	
	  IF(IOPT.EQ.7)THEN
	    CALL OPTION6(KCOLOR,CSTE0,STDEV,XX,YY,NPOINT,
     1	MAGREF)
	  ELSE
	    CALL OPTION8(KCOLOR,CSTE0,STDEV,XX,YY,NPOINT,
     1	MAGREF)
	  ENDIF
 
	  COEFF1=CSTE0
	  ERRCOEFF1=STDEV
	  ERR1=STDEV
 
C Computing the values of the constants:
 
	  PRINT 733,COEFF1,ERRCOEFF1,COEFF2,ERRCOEFF2,
     1	COEFF3,ERRCOEFF3
	  WRITE(10,733) COEFF1,ERRCOEFF1,COEFF2,ERRCOEFF2,
     1	COEFF3,ERRCOEFF3
733	  FORMAT(/,' PROPOSED FORMULAE :',/,/,5X,
     1	'B(INST) = B(CAT) + COEFF2 *(B-R)CAT +COEFF1',
     1	/,5X,'R(INST) = R(CAT) + COEFF3',/,
     1	/,5X,'WITH COEFF1 =',F8.3,' +/-',F8.3,/,
     1	/,5X,'WITH COEFF2 =',F8.3,' +/-',F8.3,/,
     1	/,5X,'WITH COEFF3 =',F8.3,' +/-',F8.3)
	  PRINT 734,ERR1,ERR3
	  WRITE(10,734) ERR1,ERR3
734	  FORMAT(/,' MEAN DEVIATION OF THE FORMULAE :',/
     1	5X,'IN B :',F8.3,8X,'IN R :',F8.3)
 
C************* End of the set of formulae proposed by Dave Carter ************
	ENDIF
 
C**** End of options 7 and 8
	GO TO 11
	ENDIF
 
C******************************************************************
C End of the program :
 
	CLOSE(10)
	PRINT *,' OUTPUT IN calib.log'
	STOP
	END
 
C*****************************************************************
C Subroutine to look for a name in the catalog
C*****************************************************************
	SUBROUTINE SEARCH(WORD,NAMECAT,NCAT,INDX)
	PARAMETER (IDIM=1000)
	CHARACTER*2 NAMECAT(IDIM),WORD,CHAR
	INDX=0
	DO I=1,NCAT
	  CHAR=NAMECAT(I)
	  IF(WORD.EQ.CHAR)INDX=I
	END DO
	RETURN
	END
 
C--------------------------------------------------------------------
C Subroutine to display XX,YY
C-------------------------------------------------------------------
	SUBROUTINE OPTION9(XX,YY,NPOINT,XC,KDEGREE,XSTART,XEND,
     1	CHAR1,CHAR2,CHAR3)
 
	PARAMETER (IDIM=1000)
	PARAMETER (IDIM1=6000)
	REAL*8 XX(IDIM),YY(IDIM),XC(20)
	REAL*4 XPLOT1(IDIM1),XPLOT2(IDIM1),YPLOT1(IDIM1),
     1	YPLOT2(IDIM1)
	CHARACTER CHAR1*30,CHAR2*30,CHAR3*40
	CHARACTER PLOTDEV*32,SYMB*4
 
10	FORMAT(A)
 
	PRINT *,' GENERATING THE GRAPHIC FILE ...'
	DO I=1,NPOINT
	  XPLOT1(I)=XX(I)
	  YPLOT1(I)=YY(I)
	END DO
 
	DELTAX=(XEND-XSTART)/100.
 
	DO I=1,100
	  XPLOT2(I)=XSTART+FLOAT(I)*DELTAX
	  WW1=XPLOT2(I)
	  CALL CALPOLY(WW1,WW2,XC,KDEGREE)
	  YPLOT2(I)=WW2
	END DO
 
	NSTART1=1
	NSTART2=1
	NEND1=NPOINT
	NEND2=100
	PRINT *,' OUTPUT GRAPHIC DEVICE ? (FILE, TEKTRO, ',
     1	'or PGPLOT or GKS station)'
	READ(5,10) PLOTDEV
	PRINT *,' SYMBOL ? (44,84,...)'
	READ(5,10) SYMB
	CALL DISPLAY2(XPLOT1,YPLOT1,NSTART1,NEND1,
     1	XPLOT2,YPLOT2,NSTART2,NEND2,CHAR1,
     1	CHAR2,CHAR3,PLOTDEV,SYMB,'L',' ',' ')
 
	RETURN
	END
C------------------------------------------------------------------
C Subroutine to calculate the constant for the magnitudes
C
C Input :
C KCOLOR
C MAGREF
C
C Output :
C NPOINT
C XX,YY
C CSTE0, STDEV
C------------------------------------------------------------------
	SUBROUTINE OPTION6(KCOLOR,CSTE0,STDEV,XX,YY,NPOINT,
     1	MAGREF)
 
	PARAMETER (IDIM=1000)
	REAL*8 XX(IDIM),YY(IDIM)
	REAL*4 OUTMAG(IDIM),MAGREF(IDIM,5),CSTE(IDIM)
	REAL*4 CATMAG(IDIM,5),COUNTS(IDIM),AIRMASS(IDIM)
	INTEGER*4 IFILTER(IDIM),INDEX(IDIM),INDEX2(IDIM)
	CHARACTER*2 NAMECAT(IDIM)
	CHARACTER*1 SYMBOL(4)
 
	COMMON/BLOCKA/NCAT,NUMBER,NAMECAT,OUTMAG,IFILTER,
     1	INDEX,SYMBOL,CATMAG,AIRMASS,COUNTS
 
	PRINT *,' CALLING "OPTION6"'
 
	NPOINT=0
 
	DO J=1,NCAT
 
	SUM=0.
	SUMSQ=0.
	NBER=0
 
	DO 21 I=1,NUMBER
	 KK=IFILTER(I)
	 INDX=INDEX(I)
 
	  IF(KK.EQ.KCOLOR.AND.INDX.EQ.J)THEN
	    CSTEI=OUTMAG(I)
	    INTENS=10.**(CSTEI/-2.5)
	    SUM=INTENS+SUM
	    SUMSQ=SUMSQ+CSTEI*CSTEI
	    NBER=NBER+1
	  ENDIF
 
21	CONTINUE
 
C Calculating the mean for each star
	  IF(NBER.NE.0)THEN
	    NPOINT=NPOINT+1
	    INDEX2(NPOINT)=J
	      IF(SUM.GT.0.)THEN
	        CSTE(NPOINT)=-2.5*ALOG10(SUM/FLOAT(NBER))-
     1	MAGREF(J,KCOLOR)
	      ELSE
	        PRINT *,' NEGATIVE VALUE FOR SUM :',SUM
	        PRINT *,' (NPOINT=',NPOINT
	      ENDIF
	  ENDIF
 
	END DO
 
 
C Return if no values
	IF(NPOINT.EQ.0)THEN
	  PRINT 23,SYMBOL(KCOLOR)
	  WRITE(10,23) SYMBOL(KCOLOR)
23	  FORMAT(' NO MEASUREMENTS AVAILABLE IN ',A1)
	  RETURN
	ENDIF
C Else calculates the mean and the standard deviation
	SUM=0.
	SUMSQ=0.
 
	DO J=1,NPOINT
	  SUM=CSTE(J)+SUM
	  SUMSQ=SUMSQ+CSTE(J)*CSTE(J)
	END DO
 
	CSTE0=SUM/FLOAT(NPOINT)
	IF(NPOINT.GT.1)THEN
	  WORK=SUMSQ-(CSTE0*CSTE0*FLOAT(NPOINT))
	  STDEV=SQRT(WORK/FLOAT(NPOINT-1))
	ELSE
	  STDEV=0.
	ENDIF
 
C Printing the values
	PRINT 602,SYMBOL(KCOLOR),CSTE0,STDEV,NPOINT
	WRITE(10,602) SYMBOL(KCOLOR),CSTE0,STDEV,NPOINT
 
602	FORMAT(/,5X,'CONSTANT IN ',A,' =',F9.3,2X,'+/-',F9.4,
     1	5X,'(WITH ',I4,' STARS)')
 
C printing the deviations for all the measurements taken into
C account for the calculation of the mean and the standard deviation.
	WRITE(10,603)SYMBOL(KCOLOR)
603	FORMAT(/,5X,'COLOR : ',A1,/,/,3X,'INDEX',4X,
     1	'NAME',5X,'MAG. REF.',5X,
     1	'CONSTANT',5X,'DEVIATION',/)
 
C MAGREF is related to CATMAG, but is sorted differently
C (according to the data).
	DO I=1,NPOINT
	  YY(I)=CSTE(I)
	  XX(I)=FLOAT(I)
	  DEVIAT=CSTE(I)-CSTE0
	  J=INDEX2(I)
	  WRITE(10,605)I,NAMECAT(J),MAGREF(J,KCOLOR),
     1	CSTE(I),DEVIAT
605	  FORMAT(3X,I4,5X,A2,5X,F8.3,5X,F8.3,5X,F9.4)
	END DO
 
 
	RETURN
	END
C------------------------------------------------------------------
C Subroutine to calculate the constant for the magnitudes
C taking all the measurements
	SUBROUTINE OPTION8(KCOLOR,CSTE0,STDEV,XX,YY,NPOINT,
     1	MAGREF)
 
	PARAMETER (IDIM=1000)
	REAL*8 XX(IDIM),YY(IDIM)
	REAL*4 OUTMAG(IDIM),MAGREF(IDIM,5),CSTE(IDIM)
	REAL*4 CATMAG(IDIM,5),COUNTS(IDIM),AIRMASS(IDIM)
	INTEGER*4 IFILTER(IDIM),INDEX(IDIM),INDEX2(IDIM)
	CHARACTER*2 NAMECAT(IDIM)
	CHARACTER*1 SYMBOL(4)
 
	COMMON/BLOCKA/NCAT,NUMBER,NAMECAT,OUTMAG,IFILTER,
     1	INDEX,SYMBOL,CATMAG,AIRMASS,COUNTS
 
	PRINT *,' CALLING "OPTION8"'
 
	NPOINT=0
 
	SUM=0.
	SUMSQ=0.
 
	DO 21 I=1,NUMBER
	  KK=IFILTER(I)
	  INDX=INDEX(I)
 
	  IF(KK.EQ.KCOLOR)THEN
	    CSTE(I)=OUTMAG(I)-MAGREF(INDX,KK)
	    SUM=CSTE(I)+SUM
	    SUMSQ=SUMSQ+CSTE(I)*CSTE(I)
	    NPOINT=NPOINT+1
	    INDEX2(NPOINT)=INDX
	  ENDIF
 
21	CONTINUE
 
C Return if no values
	IF(NPOINT.EQ.0)THEN
	  PRINT 23,SYMBOL(KCOLOR)
	  WRITE(10,23) SYMBOL(KCOLOR)
23	  FORMAT(' NO MEASUREMENTS AVAILABLE IN ',A1)
	  RETURN
	ENDIF
 
C Else calculates the mean and the standard deviation
 
	CSTE0=SUM/FLOAT(NPOINT)
	IF(NPOINT.GT.1)THEN
	  WORK=SUMSQ-(CSTE0*CSTE0*FLOAT(NPOINT))
	  STDEV=SQRT(WORK/FLOAT(NPOINT-1))
	ELSE
	  STDEV=0.
	ENDIF
 
C Printing the values
	PRINT 602,SYMBOL(KCOLOR),CSTE0,STDEV,NPOINT
	WRITE(10,602) SYMBOL(KCOLOR),CSTE0,STDEV,NPOINT
 
602	FORMAT(/,5X,'CONSTANT IN ',A,' =',F9.3,2X,'+/-',F9.4,
     1	5X,'(WITH ',I4,' POINTS)')
 
C Printing the deviations for all the measurements taken into
C account for the calculation of the mean and the standard deviation.
	WRITE(10,603)SYMBOL(KCOLOR)
 
	DO I=1,NPOINT
	  YY(I)=CSTE(I)
	  XX(I)=FLOAT(I)
	  DEVIAT=CSTE(I)-CSTE0
	  J=INDEX2(I)
	  WRITE(10,605)I,NAMECAT(J),MAGREF(J,KCOLOR),
     1	OUTMAG(I),CSTE(I),DEVIAT
	END DO
 
603	FORMAT(/,5X,'COLOR : ',A1,/,/,3X,'INDEX',4X,
     1	'NAME',5X,'MAG. REF.',5X,'OUTMAG',5X,
     1	'CONSTANT',5X,'DEVIATION',/)
605	FORMAT(3X,I4,5X,A2,4(5X,F8.3))
 
	RETURN
	END
C*****************************************************************
C Correction for atmospheric absorption :
C OUT=CCDMAG-AIRMASS*(ABSO(1,KCOLOR)
C	+ABSO(2,KCOLOR)*(CATMAG(KCOLOR)-CATMAG(KCOLOR+1)))
C Except for CARTER's correction (2 color B,R photometry)
C where there is also a correction B-R in B
C
C Colors:
C	B : 1
C	V : 2
C	R : 3
C	I : 4
C******************************************************************
	SUBROUTINE ATMCORRECTION
 
	PARAMETER (IDIM=1000)
	REAL*8 XX(IDIM),YY(IDIM)
	REAL*4 OUTMAG(IDIM),MAGREF(IDIM,5),CSTE(IDIM)
	REAL*4 CATMAG(IDIM,5),COUNTS(IDIM),AIRMASS(IDIM)
	REAL*4 ABSO(2,4)
	INTEGER*4 IFILTER(IDIM),INDEX(IDIM)
	CHARACTER*2 NAMECAT(IDIM)
	CHARACTER*1 SYMBOL(4)
 
	COMMON/BLOCKA/NCAT,NUMBER,NAMECAT,OUTMAG,IFILTER,
     1	INDEX,SYMBOL,CATMAG,AIRMASS,COUNTS
 
C MIKE BESSEL: 0.30,-0.04,0.17,0.0,0.13,0.0
	PRINT 508
508	FORMAT(' WHICH CORRECTION DO YOU WANT ?',/,
     1	' 1: MIKE BESSEL''S',/,
     1	' 2: AAT "STANDARD"',/,
     1	' 3: DAVE CARTER''S',/,
     1	' ENTER THE NUMBER YOU WANT :',$)
	READ(5,*) IOPT5
 
	WRITE(10,519)
519	FORMAT(/,/,5(1H*),' CORRECTION FOR ATMOSPHERIC ABSOPTION')
 
C Mike Bessel's corrections :
	IF(IOPT5.EQ.1)THEN
	  ABSO(1,1)=+0.30
	  ABSO(2,1)=-0.04
	  ABSO(1,2)=+0.17
	  ABSO(2,2)=+0.0
	  ABSO(1,3)=+0.13
	  ABSO(2,3)=+0.0
	  ABSO(1,4)=+0.0
	  ABSO(2,4)=+0.0
	  PRINT 509
	  WRITE(10,509)
509	  FORMAT(' (CORRECTION "MIKE BESSEL")',/,
     1	' Binst = -2.5 log10(Counts/sec) '
     1	'-(0.30 - 0.04*(B-V)cat)*SEC(Z)',/,
     1	' Vinst = -2.5 log10(Counts/sec) - 0.17*SEC(Z)',/,
     1	' Rinst = -2.5 log10(Counts/sec) - 0.13*SEC(Z)',/,
     1	' (No correction in I)')
	ENDIF
 
C AAT corrections
	IF(IOPT5.EQ.2)THEN
	  ABSO(1,1)=+0.27
	  ABSO(2,1)=-0.01
	  ABSO(1,2)=+0.13
	  ABSO(2,2)=+0.04		!for B-V
	  ABSO(1,3)=+0.13
	  ABSO(2,3)=-0.05		!for V-R
	  ABSO(1,4)=+0.08
	  ABSO(2,4)=+0.01		!for R-I
	  PRINT 510
	  WRITE(10,510)
510	  FORMAT(' (CORRECTION AAT "STANDARD")',/,
     1	' Binst = -2.5 log10(Counts/sec) '
     1	'-(0.27 - 0.01*(B-V)cat)*SEC(Z)',/,
     1	' Vinst = -2.5 log10(Counts/sec)',
     1	'-(0.13 + 0.04*(B-V)cat)*SEC(Z)',/,
     1	' Rinst = -2.5 log10(Counts/sec)',/,
     1	'-(0.13 - 0.05*(V-R)cat)*SEC(Z)',/,
     1	' Iinst = -2.5 log10(Counts/sec)',/,
     1	'-(0.08 + 0.01*(R-I)cat)*SEC(Z)')
	ENDIF
 
C Dave Carter's corrections
	IF(IOPT5.EQ.3)THEN
	  ABSO(1,1)=+0.30
	  ABSO(2,1)=-0.022	!for B-R
	  ABSO(1,2)=+0.17
	  ABSO(2,2)=+0.0
	  ABSO(1,3)=+0.13
	  ABSO(2,3)=+0.0
	  ABSO(1,4)=+0.0
	  ABSO(2,4)=+0.0
	  PRINT 511
	  WRITE(10,511)
511	  FORMAT(' (CORRECTION "DAVE CARTER")',/)
     1	' Binst = -2.5 log10(Counts/sec) '
     1	'-(0.30 - 0.022*(B-R)cat)*SEC(Z)',/,
     1	' Vinst = -2.5 log10(Counts/sec) - 0.17*SEC(Z)',/,
     1	' Rinst = -2.5 log10(Counts/sec) - 0.13*SEC(Z)',/,
     1	' (No correction in I)')
	ENDIF
 
	DO I=1,NUMBER
	KCOLOR=IFILTER(I)
	INDX=INDEX(I)
	COLOR=CATMAG(INDX,KCOLOR)-CATMAG(INDX,KCOLOR+1)
 
C For Carter's correction and color B
	IF(IOPT5.EQ.3.AND.KCOLOR.EQ.1)THEN
	  COLOR=CATMAG(INDX,1)-CATMAG(INDX,3)
	ENDIF
 
C For AAT standard correction and color V
	IF(IOPT5.EQ.2.AND.KCOLOR.EQ.2)THEN
	  COLOR=CATMAG(INDX,1)-CATMAG(INDX,2)
	ENDIF
 
C For AAT standard correction and color R
	IF(IOPT5.EQ.2.AND.KCOLOR.EQ.3)THEN
	  COLOR=CATMAG(INDX,2)-CATMAG(INDX,3)
	ENDIF
 
C For AAT standard correction and color I
	IF(IOPT5.EQ.2.AND.KCOLOR.EQ.4)THEN
	  COLOR=CATMAG(INDX,3)-CATMAG(INDX,4)
	ENDIF
 
	OUTMAG(I)=-2.5*ALOG10(COUNTS(I))
     1	-AIRMASS(I)*(ABSO(1,KCOLOR)+ABSO(2,KCOLOR)*COLOR)
 
 
	END DO
	RETURN
	END
C-------------------------------------------------------------------------
	include 'jlpsub:polyfit.for'
