C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Program   USMTUNED1
C Convolves an image with a  2-D  function  which  may  be
C constant (a top hat) or a Gaussian, and of variable size.
C JLP
C Version 20-04-90
C--*******************************************************************
C         USER PARAMETERS:-
C
C         INPUT                               This   is   the   input
C                                             Starlink image.
C
C         OUTPUT                              This  is   the   output   2-D
C                                             Starlink image.
C
C         BOXSIZ          5                   The size  of  the  convolving
C                                             box - between 3 and 33
C                                             and odd.
C
C         TYPE            GAUSS               This  may   be   Tophat   (or
C                                             anything starting with T) for
C                                             convolution with  a  top  hat
C                                             (constant);     any     other
C                                             response    gives    Gaussian
C                                             smoothing.
C
C         SIGMA           1                   If the TYPE  is  Gauss,  then
C                                             this  is  the  sigma  of  the
C                                             convolving Gaussian
C
C         FRACTION        1                   The fraction of the smoothed
C                                             image which is subtracted
C
C         RMS             0                   The minimum dispersion in the
C                                             image (i.e. dispersion of sky)
C
C         IX0,IY0                               Position of the nucleus
C
C
C         M.J.Currie               RAL                            27-Apr-84
C         W.B.Sparks               RGO                            14-Jul-86
C
C Example: RUNS USMTUNED /INPUT=  /OUTPUT=  /BOXSIZE=5 /TYPE=TOPHAT /X0=
C                        /Y0=  /RMS=  /SIGMA=
C--------------------------------------------------------------------------
	PROGRAM USMTUNED1
	PARAMETER (MAXSIZ=33)
 
	INTEGER*4 AXIS(2),KIND,SIZE,PIN,POUT,ISTAT
	INTEGER*4 MADRID(1)
	REAL*4 SIGMA
	CHARACTER*6 TYPE,BUFFER*32,COMMENTS*80,NAME*40
	COMMON /VMR/MADRID
C
	CALL JLP_BEGIN
        CALL JLP_INQUIFMT
 
C Set up input and output images
        WRITE(6,*) 'Input file: '
        READ(5,10) NAME
10      FORMAT(A)
	CALL JLP_VM_READIMAG(PIN,AXIS(1),AXIS(2),NAME,COMMENTS)
	ISIZE=AXIS(1)*AXIS(2)*4
	CALL JLP_GETVM(POUT,ISIZE)
C
C Now pick up the boxsize
C
	SIZE=5
  100 CALL RDKEYI('BOXSIZ',.TRUE.,1,SIZE,I,ISTAT)
27	WRITE(6,28)
28	FORMAT(' Size of the box (odd number between 3 and 33)')
	READ(5,*,ERR=27) SIZE
	IF (SIZE.LT.3.OR.SIZE.GT.MAXSIZ.OR. MOD(SIZE,2) .EQ. 0 ) THEN
	  WRITE(6,*) ' Error: try again'
	  GOTO 27
	ENDIF
C
C And the type of profile
C
	TYPE='GAUSS '
	WRITE(6,*)' Option (GAUSS or TOPHAT)?'
	READ(5,10)TYPE
10	FORMAT(A)
	IF((TYPE(1:1).EQ.'T').OR.(TYPE(1:1).EQ.'t')) THEN
	  KIND=1
	ELSE
	  KIND=2
C Pick up sigma if a gaussian profile is to be used
	  SIGMA=1
	  WRITE(6,*)' Sigma (pixels) :'
	  READ(5,*) SIGMA
	END IF
C
	fraction=1.0
	WRITE(6,*)' Fraction to be removed (between 0. and 1.)'
	READ(5,*) FRACTION
	rms=0.0
	WRITE(6,*) 'Input intensity dispersion of sky'
	READ(5,*) RMS
	WRITE(6,*) 'Position of nucleus of galaxy: IX0,IY0'
	READ(5,*) IX0,IY0
C
C Now call the working subroutine
C
	CALL USMTUNED_SUB(MADRID(PIN),MADRID(POUT),AXIS(1),AXIS(2),
     1            KIND,SIZE,SIGMA,FRACTION,RMS,IX0,IY0)
 
	WRITE(COMMENTS,11) NAME(1:20)
11	FORMAT(' USMTUNED of ',A20)
        WRITE(6,*) 'Output file: '
        READ(5,10) NAME
	CALL JLP_WRITEIMAG(MADRID(POUT),AXIS(1),AXIS(2),AXIS(1),
     1	NAME,COMMENTS)
 
	CALL JLP_END
	STOP
	END
C************************************************************************
      SUBROUTINE USMTUNED_SUB(IN,OUT,NX,NY,KIND,NB,SIGMA,FRACTION,
     1 rms,ix0,iy0)
C+************************************************************************
C
C
C      CONVOLVES A 2-D ARRAY WITH A BOX WITH GAUSSIAN OR TOP-HAT SHAPE
C
C      IN IS THE INPUT ARRAY , AND OUT THE OUTPUT ARRAY BOTH OF SIZE
C      NX BY NY
C      KIND = 1 GIVES TOP-HAT KIND = 2 (OR ANYTHING EXCEPT 1) GIVES GAUSSIAN
C      THE CONVOLVING BOX IS OF SIZE NB BY NB
C      IF KIND=2 , SIGMA IS THE STANDARD DEVIATION OF THE GAUSSIAN
C
C-************************************************************************
      REAL WT(33,33)
      REAL IN(NX,NY),OUT(NX,NY)
      INTEGER*4 KIND,NB
      REAL*4 SIGMA
      CHARACTER*72 LIST
      CHARACTER*13 MESS
      external delta
C
C      FIRST GENERATE THE CONVOLVING FUNCTION
C
C      THE SUM OF THE ELEMENTS SHOULD BE 1 , SO FIND TOTAL
C
      TOT=0
      IF (KIND.EQ.1) THEN
         DO 20 J=1,NB
            DO 10 I=1,NB
               WT(I,J)=1.0
               TOT=TOT+WT(I,J)
   10       CONTINUE
   20    CONTINUE
      ELSE
         ALPHA=1.0/(1.4142*SIGMA)
         A2=ALPHA*ALPHA
         C=REAL(NB+1)/2.0
         DO 40 J=1,NB
            Y=REAL(J)
            DO 30 I=1,NB
               X=REAL(I)
               R2=(X-C)*(X-C)+(Y-C)*(Y-C)
               WT(I,J)=EXP(-A2*R2)
               TOT=TOT+WT(I,J)
   30       CONTINUE
   40    CONTINUE
      END IF
      write(6,98)tot
   98 format(1h ,'Sum of weights before normalization',2x,e13.6)
c
c Renormalize to sum=1 so that delta-function is properly scaled
c
      do j=1,nb
        do i=1,nb
          wt(i,j)=wt(i,j)/tot
        enddo
      enddo
 
      IEDGE=(NB-1)/2
c      if(iedge.gt.2)then
c        write(6,*) ' Will estimate intensity dispersions in 5x5 box'
c        iside=2
c      else
        iside=iedge
c      endif
      nb2=iside+1
c
c Find max intensity dispersion corresponding to a delta-function filter
c
      s=0.0
      sx=0.0
      sxx=0.0
      jjm=max(1,iy0-iside)
      jjp=min(ny,iy0+iside)
      iim=max(1,ix0-iside)
      iip=min(nx,ix0+iside)
      do kj=jjm,jjp
        do ki=iim,iip
          s=s+1.0
          sx=sx+in(ki,kj)
          sxx=sxx+in(ki,kj)*in(ki,kj)
        enddo
      enddo
      sigmax=sqrt((sxx-(sx*sx/s))/(s-1.0))
      write(6,99)rms,sigmax,sigma
   99 format(1h ,'Sky rms, galaxy rms, length scale (sigma):',
     1 3(2x,e13.6))
      if(sigmax.le.rms)then
        write(6,*) 'Sigmax<=rms'
        go to 80
      endif
      delsig=sigmax-rms
 
      DO J=1,NY
C
C   JM,JP ARE THE MINIMUM AND MAXIMUM ROWS USED IN THE SMOOTH
C   IM,IP ARE THE MINIMUM AND MAXIMUM COLUMNS USED IN THE SMOOTH
C
         JM=MAX(1,J-IEDGE)
         JP=MIN(NY,J+IEDGE)
         jjm=max(1,j-iside)
         jjp=min(ny,j+iside)
         DO I=1,NX
            IM=MAX(1,I-IEDGE)
            IP=MIN(NX,I+IEDGE)
            iim=max(1,i-iside)
            iip=min(nx,i+iside)
c
c Find mean and sd in box
c
            s=0.0
            sx=0.0
            sxx=0.0
            do kj=jjm,jjp
              do ki=iim,iip
                s=s+1.0
                sx=sx+in(ki,kj)
                sxx=sxx+in(ki,kj)*in(ki,kj)
              enddo
            enddo
            if(s.gt.1.0)then
             term=(sxx-(sx*sx/s))/(s-1.0)
             if(term.gt.0.0)then
               sigm=sqrt(term)
             else
               write(6,88)term,i,j
   88 format(1h ,'Term,i,j=',e13.6,2i4)
               sigm=0.0
             endif
            else
             sigm=0.0
            endif
            dfrac=(min(sigmax,max(rms,sigm))-rms)/delsig
            eps=1.0-dfrac
 
            S=0.
C
C   JJ IS THE NUMBER OF ROWS OF THE BOX CLIPPED BY THE IMAGE'S EDGE
C   II IS THE NUMBER OF COLUMNS OF THE BOX CLIPPED BY THE IMAGE'S EDGE
C
            JJ=MIN(JM-J+IEDGE,IEDGE)
            WTS=0.
            DO KJ=JM,JP
               JJ=JJ+1
               II=MIN(IM-I+IEDGE,IEDGE)
               DO KI=IM,IP
                  II=II+1
                  weight=eps*WT(ii,jj)+dfrac*delta(ii,jj,nb2)
                  S=S+IN(KI,KJ)*weight
C
C   WTS SUMS THE WEIGHTS FOR NORMALISATION
C
                  WTS=WTS+weight
               END DO
            END DO
            if(wts.gt.0.0)then
               OUT(I,J)=IN(I,J)-FRACTION*S/WTS
            else
               out(i,j)=0.0
            endif
         END DO
C
C   OUTPUT TO ENCOURAGE THE USER
C
         IF (MOD(J,30) .EQ. 0) THEN
            MESS='FINISHED LINE'
            WRITE (6,'(A,2X,I4)') MESS,J
         END IF
      END DO
C
   80 return
      END
      function delta(i,j,nb2)
      if(i.eq.j.and.i.eq.nb2)then
        delta=1.0
      else
        delta=0.0
      endif
      return
      end
