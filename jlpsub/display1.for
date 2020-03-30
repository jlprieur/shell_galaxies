C*******************************************************************
C Subroutine DISPLAY1 to display 1 curve XX,YY in real*4, with a line
C
C Input :
C  XX, YY
C  NSTART, NEND
C  CHAR1*30,CHAR2*30,TITLE*40
C  PLOTDEV*32
C
C Output  (in common block STR_OUTPUT)
C  XOUT, YOUT
C  NOUT
C
C JLP
C Version of 04/10/2008
C*******************************************************************
        SUBROUTINE DISPLAY1(XX,YY,NSTART,NEND,
     1                      CHAR1,CHAR2,TITLE,PLOTDEV)
        REAL*4 XX(*),YY(*)
        INTEGER*4 NPTS
        CHARACTER CHAR1*(*),CHAR2*(*),TITLE*(*),PLOTDEV*(*)
        CHARACTER IN_FILE*40,IN_COMMENTS*80
        CHARACTER NCHAR*4,PCOLOR*30
        COMMON /STR_OUTPUT/XOUT(200),YOUT(200),NOUT
 
        NPTS=NEND-NSTART+1
        IN_FILE=' '
        IN_COMMENTS=' '
 
C Drawing the curve:
        NCHAR='L'
        PCOLOR='Default'
        KK=1                ! Number of curves
        CALL NEWPLOT(XX(NSTART),YY(NSTART),NPTS,
     1               NPTS,KK,CHAR1,CHAR2,TITLE,NCHAR,PCOLOR,PLOTDEV,
     1               IN_FILE,IN_COMMENTS)
 
        RETURN
        END
