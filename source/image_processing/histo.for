C++**************************************************************************
C Program HISTO to compute histograms of lines from an image
C Written by AW, JLP (Manchester 87)
C
C JLP
C Version of 04-06-00
C--**************************************************************************
	PROGRAM HISTO
	INTEGER*4 NX,NY,NBINS,IX1,IX2,IY1,IY2
        REAL*4 XMIN,XMAX
	CHARACTER PLOTDEV*32,ANS*1,NAME*40,COMMENTS*80
	INTEGER*4 MADRID(1),IPTR
	COMMON /VMR/MADRID
 
10	FORMAT(A) 	
 
	CALL JLP_BEGIN
	CALL JLP_INQUIFMT
 
	WRITE(6,3)
3	FORMAT(' Program HISTO, Version of 04-06-00',/,
     1	' Displays the histogram of a line or a column of an image')
 
C Reading the image :
	PRINT *,' Enter the name of the file :'
	READ(5,10) NAME
	CALL JLP_VM_READIMAG(IPTR,NX,NY,NAME,COMMENTS)
 
C Drawing the histogram of a line :
 	WRITE(6,17)
17	FORMAT(' Enter the boundaries: IX1,IX2,IY1,IY2 (between 1,1 and NX,NY)')
	READ(5,*) IX1,IX2,IY1,IY2
	IF((IX1.LT.0).OR.(IX2.GT.NX).OR.(IY1.LT.0).OR.(IY2.GT.NY))THEN
	   WRITE(6,*)' Fatal error/Wrong values, outside boundaries.'
           STOP
	ENDIF
 
C Enter the number of bins :
	PRINT *,' Enter min, max and the number of bins (less than 2000)'
	PRINT *,' (enter (0,0) for min, max if automatic scale is wanted): '
	READ(5,*) XMIN,XMAX,NBINS
        IF(NBINS.GT.2000)THEN
        WRITE(6,*)' Fatal error, NBINS > 2000'
        STOP
        ENDIF
 
C Select the output device for the display :
	PRINT *,' Enter the device for displaying the histogram :'
	READ(5,10) PLOTDEV
 
	CALL HISTO1(MADRID(IPTR),NX,NY,IX1,IX2,IY1,IY2,
     1              XMIN,XMAX,NBINS,PLOTDEV,NAME)
 
C End:
	CALL JLP_END
	STOP
	END
 
C**************************************************************************
C Main part of HISTO :
C
C**************************************************************************
	SUBROUTINE HISTO1(IMAGE,NX,NY,IX1,IX2,IY1,IY2,
     1                    XMIN,XMAX,NBINS,PLOTDEV,NAME)
	PARAMETER (IDIM=2000)
	REAL*4 XPLOT(IDIM),YPLOT(IDIM)
	REAL*4 IMAGE(NX,NY),XMIN,XMAX
        INTEGER*4 IX1,IX2,IY1,IY2
	CHARACTER NAME*30
	CHARACTER CHAR1*30,CHAR2*30,TITLE*40,PLOTDEV*32
	
C Common block with NEWPLOT :
	COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
 
C Calling HISTO2 :
	CALL HISTO2(IMAGE,NX,NY,IX1,IX2,IY1,IY2,XPLOT,YPLOT,
     1              XMIN,XMAX,NBINS)
 
C Displaying the histogram :
	  CHAR1='Level'
	  CHAR2='Number of pixels'
	  WRITE(TITLE,28)NAME(1:20),IX1,IX2,IY1,IY2
28	  FORMAT(A20,4(1X,I4))
 
	  CALL DISPLAY1(XPLOT,YPLOT,1,NBINS,CHAR1,CHAR2,TITLE,PLOTDEV)
	  IF(NOUT.NE.0)THEN
	   DO I=1,NOUT
	    PRINT *,' X, Y :',XOUT(I),YOUT(I)
	   END DO
	  ENDIF
 
	RETURN
	END
C**************************************************************************
*  Compute histogram of nbins from IMAGE array 
*
* Input:
*     image   real array       
*     nbins   integer          no of bins required
*
* Output:
*   XPLOT, YPLOT
C*****************************************************************************
	SUBROUTINE HISTO2(IMAGE,NX,NY,IX1,IX2,IY1,IY2,XPLOT,YPLOT,
     1                    XMIN,XMAX,NBINS)
	PARAMETER (IDIM=2000)
	REAL*4 IMAGE(NX,NY),XPLOT(*),YPLOT(*),V
	INTEGER*4 IX,IY,IB,IX1,IX2,IY1,IY2,IHIST(IDIM)
        CHARACTER NAME*60,COMMENTS*80
 
*
* Check that histogram array is clear:
	DO IB=1,NBINS
	   IHIST(IB)=0
	END DO
*
* Determine bin interval
        IF(XMIN.EQ.-1..AND.XMAX.EQ.-1.)THEN
	XMIN=IMAGE(IX1,IY1)
	XMAX=IMAGE(IX1,IY1)
	DO IY=IY1,IY2
	DO IX=IX1,IX2
	  V=IMAGE(IX,IY)
	  XMAX=AMAX1(V,XMAX)
	  XMIN=AMIN1(V,XMIN)
	END DO
	END DO
        write(6,*) 'Automatic scale: min/max',xmax,xmin
        ENDIF

C Compute the histogram now: 
	XBIN=(XMAX-XMIN)/REAL(NBINS)
 
	DO IY=IY1,IY2
	DO IX=IX1,IX2
	  V=IMAGE(IX,IY)
	   IBIN=INT((V-XMIN)/XBIN)+1
	   IHIST(IBIN)=IHIST(IBIN)+1
	END DO
	END DO
        OPEN(9,NAME='histo.dat',STATUS='unknown',ACCESS='sequential')
	DO IB=1,NBINS
        WRITE(9,39) XMIN+XBIN*FLOAT(IB-1),IHIST(IB)
39      FORMAT(F12.3,1X,I5)
        END DO
        CLOSE(9)
*
* Generates the two arrays XPLOT, YPLOT to be plotted :
	DO IB=1,NBINS
	  XPLOT(IB)=XMIN+XBIN*FLOAT(IB-1)
	  YPLOT(IB)=FLOAT(IHIST(IB))
	END DO
 
        NBY=2
        NAME='histo'
        WRITE(COMMENTS,8)XMIN,XMAX,NBINS
8       FORMAT('Min/Max=',2(1X,F12.3),' Nbins:',NBINS)
        JLP_WRITE_IMAG(XPLOT,YPLOT,NBINS,NBY,NBINS,NAME,COMMENTS) 
	DO IB=1,NBINS
        WRITE(9,39) XMIN+XBIN*FLOAT(IB-1),IHIST(IB)
39      FORMAT(F12.3,1X,I5)
        END DO
        CLOSE(9)
	RETURN
	END
