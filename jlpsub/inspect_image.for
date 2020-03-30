C----------------------------------------------------------------
C Set of subroutines to inspect a file
C Contains : INSPECT_IMAGE, MEAN_SIGMA
C Version of 03-09-90
C---------------------------------------------------------------
C
C***************************************************************
C Subroutine to inspect a file
C**************************************************************
	SUBROUTINE INSPECT_IMAGE(IMAGE1,NX,NY,IDIM,IN_FILE,IN_COMMENTS)
	PARAMETER (IDIM1=2000)
	REAL*4 IMAGE1(IDIM,*)
	REAL*4 XPLOT(IDIM1),YPLOT(IDIM1)
	INTEGER*4 IXSTART,IXEND,IYSTART,IYEND,NPTS
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,PLOTDEV*32
        CHARACTER IN_FILE*(*),IN_COMMENTS*(*)
	CHARACTER ANS*1,FILENAME*40,NCHAR*4
 
C Common block with NEWPLOT :
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
 
10	FORMAT(A)
 
55	PRINT 80
80	FORMAT(' OPTIONS :',/,
	1	' 3 : Mean and standard deviation',/,
	1	' 4 : Reading a part of this file',/,
	1	' 5 : Displaying a slice',/,
	1	' 6 : Output of a slice to a file',/,
	1	' 7 : Displaying a line',/,
	1	' 8 : Displaying a column',/,
	1	' 10 : Return to the main program',/,
	1	5X,' Enter the option you want: ',$)
	READ(5,*) IOPT
 
C***************************************************************
C Option 3 : Mean and standard deviation
C***************************************************************
	IF(IOPT.EQ.3)THEN
 
C Area chosen by the user:
31	PRINT *,' Starting and ending pixels: (0,0 for all)'
	READ(5,*) IXSTART,IXEND
	IF(IXSTART.GT.IXEND)GO TO 31
	IF(IXSTART.EQ.0.AND.IXEND.EQ.0)THEN
           IXSTART=1
           IXEND=NX
        ENDIF
	IF(IXSTART.LT.1.OR.IXEND.GT.NX)GO TO 31
 
32	PRINT *,' Starting and ending lines: (0,0 for all)'
	READ(5,*) IYSTART,IYEND
	IF(IYSTART.GT.IYEND)GO TO 32
	IF(IYSTART.EQ.0.AND.IYEND.EQ.0)THEN
           IYSTART=1
           IYEND=NY
        ENDIF
	IF(IYSTART.LT.1.OR.IYEND.GT.NY)GO TO 32
 
	CALL MEAN_SIGMA(IMAGE1,IXSTART,IXEND,IYSTART,
	1	IYEND,IDIM,XMEAN,SIGMA)
	PRINT 333,XMEAN,SIGMA
333	FORMAT(' MEAN VALUE :',1PE12.4,'   SIGMA :',1PE12.4)
	GO TO 55
	ENDIF
 
C**************************************************************
C	OPTION 4 : READING A PART OF A FITS/BDF FILE
C**************************************************************
	IF(IOPT.EQ.4) THEN
 
C Area chosen by the user:
	PRINT *,' Area you want to check'
 
41	PRINT *,' Starting and ending pixels: (0,0 for all)'
	READ(5,*) IXSTART,IXEND
	IF(IXSTART.GT.IXEND)GO TO 41
	IF(IXSTART.EQ.0.AND.IXEND.EQ.0)THEN
           IXSTART=1
           IXEND=NX
        ENDIF
	IF(IXSTART.LT.1.OR.IXEND.GT.NX)GO TO 41
 
42	PRINT *,' Starting and ending lines: (0,0 for all)'
	READ(5,*) IYSTART,IYEND
	IF(IYSTART.GT.IYEND)GO TO 42
	IF(IYSTART.EQ.0.AND.IYEND.EQ.0)THEN
           IYSTART=1
           IYEND=NY
        ENDIF
	IF(IYSTART.LT.1.OR.IYEND.GT.NY)GO TO 42
 
C Output of the values:
 
	  DO IY=IYSTART,IYEND
            DO IX=IXSTART,IXEND
	      WRITE(6,100) IX,IY,IMAGE1(IX,IY)
C For obscure reasons PE12.3 does not work at the screen of the terminal:
100	      FORMAT('i=',I5,' j=',I5,' val=',E12.5)
	    END DO
	  END DO
 
C Possibility of output in a file:
	PRINT *,' Do you want to write the data to a file ? (N)'
	READ(5,10) ANS
	IF(ANS.EQ.'Y'.OR.ANS.EQ.'y') THEN
          WRITE(6,*) ' Filename ?'
          READ(5,10) FILENAME
	  OPEN(3,FILE=FILENAME,STATUS='UNKNOWN')
	  WRITE(3,102)IYSTART,IYEND,IXSTART,IXEND
102	  FORMAT(2X,'Starting and ending line',2I5,
	1	/,'Starting and ending point',2I5)
	  DO IY=IYSTART,IYEND
            DO IX=IXSTART,IXEND
	      WRITE(3,101) IX,IY,IMAGE1(IX,IY)
101	      FORMAT(2X,I5,2X,I5,1PE12.3)
	    END DO
	  END DO
	  CLOSE(3)
	ENDIF
 
	GO TO 55
	ENDIF
 
C***********************************************************
C Displaying a slice
	IF(IOPT.EQ.5)THEN
45	  PRINT *,' Coordinates of starting pixel X,Y ?'
	  READ(5,*) IXSTART,IYSTART
	  PRINT *,' Coordinates of ending pixel X,Y ?'
	  READ(5,*) IXEND,IYEND
          CALL SLICE1(IMAGE1,NX,NY,IDIM,IXSTART,IYSTART,
     1    IXEND,IYEND,XPLOT,YPLOT,NPTS)
          IF(NPTS.EQ.0)GOTO 45

	WRITE(TITLE,790) IXSTART,IYSTART,IXEND,IYEND
790	FORMAT('From (',I4,',',I4,') to (',I4,',',I4,')')
	CHAR1=' '
	ENDIF
 
C***********************************************************
C Output of a slice to a file:
	IF(IOPT.EQ.6)THEN
	  PRINT *,' Coordinates of starting pixel X,Y ?'
	  READ(5,*) IXSTART,IYSTART
	  PRINT *,' Coordinates of ending pixel X,Y ?'
	  READ(5,*) IXEND,IYEND
          CALL SLICE2(IMAGE1,NX,NY,IDIM,IXSTART,IYSTART,
     1    IXEND,IYEND)
	ENDIF
 
C***********************************************************
C Displaying a line
	IF(IOPT.EQ.7)THEN
 
71	PRINT *,' Number of the line ?'
	READ(5,*) IYSTART
	IF(IYSTART.LT.1.OR.IYSTART.GT.NY)GO TO 71
	WRITE(TITLE,1790) IYSTART
1790	FORMAT(' Line #',I4)
	CHAR1=' Pixel '
 
72	PRINT *,' Starting and ending point: (0,0 for all)'
	READ(5,*) IXSTART,IXEND
	IF(IXSTART.GT.IXEND)GO TO 72
	IF(IXSTART.EQ.0.AND.IXEND.EQ.0)THEN
           IXSTART=1
           IXEND=NX
        ENDIF
	IF(IXSTART.LT.1.OR.IXEND.GT.NX)GO TO 72
 
        NPTS=IXEND-IXSTART+1
	DO I=1,NPTS
          II=I+IXSTART-1
	  XPLOT(I)=FLOAT(II)
	  YPLOT(I)=IMAGE1(II,IYSTART)
	END DO
  
	ENDIF
 
C***********************************************************
C Displaying a column
	IF(IOPT.EQ.8)THEN
 
81	PRINT *,' Number of the column:'
	READ(5,*) IXSTART
	IF(IXSTART.LT.1.OR.IXSTART.GT.NX)GO TO 81
 
82	PRINT *,' Starting and ending line: (0,0 for all)'
	READ(5,*) IYSTART,IYEND
	IF(IYSTART.GT.IYEND)GO TO 82
	IF(IYSTART.EQ.0.AND.IYEND.EQ.0)THEN
           IYSTART=1
           IYEND=NY
        ENDIF
	IF(IYSTART.LT.1.OR.IYEND.GT.NY)GO TO 82
 
	 CHAR1=' Line '
	 WRITE(TITLE,1789)IXSTART
1789	 FORMAT(' Column #',I4)

        NPTS=IYEND-IYSTART+1
	DO J=1,NPTS
          JJ=J+IYSTART-1
	  XPLOT(J)=FLOAT(JJ)
	  YPLOT(J)=IMAGE1(IXSTART,JJ)
	END DO

        ENDIF

C Now displaying the data:
	IF((IOPT.EQ.5.OR.IOPT.EQ.7.OR.IOPT.EQ.8)
     1     .AND.NPTS.GT.0)THEN
	 CHAR2=' '
	 NOUT=0
	 PRINT *,' Device : &xterm, &postscript, &tektronix ?'
	 READ(5,10) PLOTDEV
         NCHAR='L0'
50         CALL NEWPLOT(XPLOT,YPLOT,NPTS,IDIM1,1,CHAR1,CHAR2,TITLE,
     1   NCHAR,PLOTDEV,IN_FILE,IN_COMMENTS)
	 PRINT *,' ',NOUT,' POINTS:'
	  IF(NOUT.NE.0)THEN
	   DO I=1,NOUT
	    PRINT 233,I,XOUT(I),YOUT(I)
233	    FORMAT(' POINT :',I5,' PIXEL :',F12.3,' INTENSITY :',
	1	F12.3)
	   END DO
	  ENDIF
	 NOUT=0
         PRINT *,' DO YOU WANT TO CHANGE THE PARAMETERS OF',
     1  ' THE FRAME ?(N)'
         READ(5,10) ANS
         IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')GO TO 50
 
	GO TO 55
	ENDIF
 
999	CLOSE(1)
	RETURN
	END
C*************************************************************
C	Subroutine MEAN_SIGMA to compute the mean and sigma
C	of an area defined by IMIN, IMAX JMIN and JMAX
C
C*************************************************************
	SUBROUTINE MEAN_SIGMA(IMAGE,IMIN,IMAX,JMIN,
	1	JMAX,IDIM,XMEAN,SIGMA)
	REAL*4 IMAGE(IDIM,*)
	REAL*8 TOTSUM,TOT2,XMEAN1,WORK
 
C First passage to calculate the mean
	IT=0
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
 
30	XMEAN1=TOTSUM
 
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
            IF((AIJ.NE.0.0).OR.(AIJ.LE.ZUL).OR.(AIJ.GE.ZLL))THEN
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
	   IF(XMEAN1.NE.0)THEN
	    WORK=DABS((TOTSUM-XMEAN1)/XMEAN1)
	   ELSE
	    WORK=DABS(TOTSUM-XMEAN1)
	   ENDIF
	
C Loop until the mean is stable ...
	IF(WORK.GT.0.1E-09)THEN
	  IT=IT+1
 	  PRINT *,' IT =',IT
	  GO TO 30
	ENDIF
 
	XMEAN=TOTSUM
	SIGMA=TOT2
 
	RETURN
	END
C***********************************************************************
	SUBROUTINE SLICE1(IMAGE1,NX,NY,IDIM,IXSTART,IYSTART,
     1            IXEND,IYEND,XPLOT,YPLOT,NPTS)
	REAL*4 IMAGE1(IDIM,*)
	REAL*4 XPLOT(*),YPLOT(*),STEPX,STEPY
	INTEGER*4 IXSTART,IXEND,IYSTART,IYEND,NPTS,IX,IY

C Compute max number of pixels:
        IRANGE=IXEND-IXSTART
        JRANGE=IYEND-IYSTART
        IF((IXEND.GT.NX.OR.IYEND.GT.NY)
     1   .OR.(IXSTART.LT.0.OR.IYSTART.LT.0))THEN
          NPTS=0
          WRITE(6,*)' SLICE1/Error: wrong boundaries'
          RETURN
        ENDIF
        NPTS=MAX(IRANGE,JRANGE)+1

C Look for the nearest neighbour:
        STEPX=FLOAT(IRANGE)/FLOAT(NPTS-1)
        STEPY=FLOAT(JRANGE)/FLOAT(NPTS-1)
        DO I=1,NPTS
          IX=IXSTART+NINT(FLOAT(I-1)*STEPX)
          IY=IYSTART+NINT(FLOAT(I-1)*STEPY)
          XPLOT(I)=FLOAT(I-1)
          YPLOT(I)=IMAGE1(IX,IY)
        END DO

	RETURN
	END
C***********************************************************************
C Same as SLICE1 but output to a file:
	SUBROUTINE SLICE2(IMAGE1,NX,NY,IDIM,IXSTART,IYSTART,
     1            IXEND,IYEND)
	REAL*4 IMAGE1(IDIM,*)
	INTEGER*4 IXSTART,IXEND,IYSTART,IYEND,NPTS
        CHARACTER OUT_NAME*60

        WRITE(6,12)
12      FORMAT(' Output ASCII file name:')
        READ(5,10) OUT_NAME
10      FORMAT(A)
        OPEN(3,FILE=OUT_NAME,STATUS='NEW')

C Compute max number of pixels:
        IRANGE=IXEND-IXSTART
        JRANGE=IYEND-IYSTART
        IF((IXEND.GT.NX.OR.IYEND.GT.NY)
     1   .OR.(IXSTART.LT.0.OR.IYSTART.LT.0))THEN
          NPTS=0
          WRITE(6,*)' SLICE2/Error: wrong boundaries'
          RETURN
        ENDIF
        NPTS=MAX(IRANGE,JRANGE)+1

        WRITE(6,14) NPTS
14      FORMAT(' SLICE2/Writing ',I5,' points. Format is #,VALUE,X,Y')
C Look for the nearest neighbour:
        STEPX=FLOAT(IRANGE)/FLOAT(NPTS-1)
        STEPY=FLOAT(JRANGE)/FLOAT(NPTS-1)
        DO I=1,NPTS
          IX=IXSTART+NINT(FLOAT(I-1)*STEPX)
          IY=IYSTART+NINT(FLOAT(I-1)*STEPY)
          WRITE(3,*) FLOAT(I-1),IMAGE1(IX,IY),IX,IY
        END DO

        CLOSE(3)
	RETURN
	END
