C----------------------------------------------------------------------
C PATCH2_SET.FOR
C Set of subroutines:
C
C PATCH3, PATCH2, PATCH22, STRDELT, STRCOPY, STRESTO.
C
C     Package of routines for finding and removing defects from
C     an image array.  The position of the area to delete may
C     be specified manually using the cursor, or by reading a file. 
C     The user is prompted for the diameter of a circle
C     of pixels to delete around this position, which are
C     replaced by an interpolation of a surface fitted to
C     an annulus of surrounding pixels.  A constant, planar,
C     quadratic, or cubic surface may be specified (1,3,6, or 10
C     terms).  The interpolation is then displayed on the screen 
C     and the user judges the quality of fit.  If necessary, the
C     original values of the pixels may be restored and a
C     different interpolation attempted.
C
C JLP
C Version of 29-11-2006
C----------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C          CALLING SEQUENCE:-
C               PATCH  [ NOISE=f ]  [ ORDER=n ]  [ FILE=f ]
C
C
C   FUNCTION:-
C         It allows the user to replace several circular  patches  in
C         an image with a fitted noisy piece of synthetic data. It is
C         derived from a program, by  W  D  Pence (at  University  of
C         Sussex).
C
C
C   USE:-
C         It may be used to remove large defects, bright galaxies  or
C         any other localised unwanted pixels. The result can be very
C         convincing. 
C
C         USER PARAMETERS:-
C
C         IN                                  The input 2-D image which  is
C                                             being  displayed on the ARGS.
C
C         OUTPUT                              The  new   image   with   the
C                                             patched regions.
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         NOISE           1                   This is a noise factor  which
C                                             may  be  between 0 (no noise)
C                                             and 1 (noise level calculated
C                                             from the whole image).
C
C         ORDER           3                   This is the the degree of the
C                                             2-dimensional  surface  which
C                                             is to be fitted to an annulus
C                                             around the circular patch. It
C                                             may  be  0  (constant)  to  3
C                                             (bi-cubic).
C
C
C         FILE            FALSE               This defines  if  the position
C                                             and size of  the  patches will
C                                             be   written   to   the   file
C                                             PATCHES.DAT
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     Accept this size of patch  and  then  do  a  fit  and
C                     display  the results. AFTER THIS BUTTON has been used
C                     then GREEN means accept the result  and  move  on  to
C                     another  location,  whilst  RED  means  revert to the
C                     previous state.
C
C         WHITE 2     Decrease the size of the patch.
C
C         WHITE 3     Increase the size of the patch.
C
C         RED   4     Exit from the program.
C
C
C         D J King-W D Pence       RGO-U. of Sussex                7-JAN-82
C
C--------------------------------------------------------------------------
C*************************************************************************
C Non-interactive version
C with only a few questions
C
C INPUT:
C I_MIN,I_MAX,J_MIN,J_MAX: boundaries of the zoomed image ARRAY
C                          (expressed as C indices starting at (0,0)!)
C*************************************************************************
	SUBROUTINE PATCH3(ARRAY,NX,NY,IDIM,IMAGE2,IDIM2,
     1                    I_MIN,I_MAX,J_MIN,J_MAX,
     1                    NCOLORS,ITT_IS_LINEAR,LOWER_ITT,UPPER_ITT,
     1                    XP,YP,DIAM,NCODE,SIG,ISTATUS,GAMMA_D,IDV1)
        IMPLICIT NONE
        INTEGER*4 NX,NY,IDIM,IDIM2,NCOLORS,NDIMER,IDV1
        INTEGER*4 I_MIN,I_MAX,J_MIN,J_MAX,ITT_IS_LINEAR
	PARAMETER (NDIMER=200)
	REAL*4 ARRAY(IDIM,*),ER(NDIMER)
        REAL*4 LOWER_ITT,UPPER_ITT,SIG,XP,YP,DIAM
	INTEGER*4 IMAGE2(IDIM2,*),ISTATUS,GAMMA_D
	INTEGER*4 DRAW_CROSS,AUTOMATIC_PATCH,NCODE
        INTEGER*4 I,IN_FRAME,IBUTTON,IXP,IYP
	LOGICAL FILE
	CHARACTER ANS*1,BUFFER*80
 
10	FORMAT(A)
 
        AUTOMATIC_PATCH=1
        FILE=.FALSE.
        ISTATUS=-1
 
C Order of polynomial:
        NCODE=3
C Sigma of the noise:
        SIG=0.
 
C  Set up array of random errors with gaussian distribution
C  for approximating the noise when interpolating
C  Generate normal errors, mean=0.
        IF(SIG.GT.0.)THEN
	  CALL JLP_RANDOM_INIT(1)
          DO I=1,NDIMER
           CALL JLP_RANDOM_GAUSS(ER(I))
           ER(I)=ER(I)*SIG
	  END DO
        ELSE
          DO I=1,NDIMER
           ER(I)=0.
	  END DO
        ENDIF
 

17     WRITE(BUFFER,10)'Click on the mouse to select the center'
       CALL JLP_DRAW_TO_STATUS_BAR(BUFFER,IDV1)
 
C Reading the position of the cursor:
        DRAW_CROSS = 0
        CALL JLP_WHERE(XP,YP,IN_FRAME,IBUTTON,DRAW_CROSS,IDV1)
C JLP_WHERE is written in C and returns an index starting at 0,0:
        IXP=INT(XP)
        IYP=INT(YP)
         
C Test to check if the point is on the image :
        IF(IXP.LT.0.OR.IXP.GE.NX
     1 	.OR.IYP.LT.0.OR.IYP.GE.NY.OR.IN_FRAME.EQ.0)THEN
	  WRITE(6,77) IXP,IYP,IN_FRAME
77	  FORMAT(' PATCH3/Exit: Point outside of the image '
     1      ' IX, IY, IN_FRAME:',3I5)
	  GO TO 99 
	ENDIF
 
       WRITE(BUFFER,10)'Please enter some data on the terminal...'
       CALL JLP_DRAW_TO_STATUS_BAR(BUFFER,IDV1)
C JLP2006: I tried with NCODE=4 or 5, but it was always worse!
700	WRITE(6,*) ' Diameter, order of polynomial (between 0 and 3):'
	READ(5,*) DIAM,NCODE
        IF(NCODE.LT.0.OR.NCODE.GT.3)NCODE=3
	WRITE(6,71) XP,YP,DIAM,NCODE,SIG
71	FORMAT(' XP,YP,DIAMETER,NCODE,SIG :',3(F7.1,2X),I5,2X,F7.3)
 
C Displaying a circle:
C Warning: convention here is that (0,0) is the lower left corner,
        CALL JLP_CIRCLE1(XP,YP,DIAM,IDV1)
	CALL JLP_GFLUSH(IDV1)
	WRITE(6,*) ' Diameter is OK [y] ?' 
        READ(5,10) ANS
        IF(ANS.EQ.'N'.OR.ANS.EQ.'n') THEN
C Refresh screen to erase previous circle: 
          CALL PATCH_DISP(ARRAY,NX,NY,IDIM,I_MIN,I_MAX,J_MIN,J_MAX,
     1                    IMAGE2,IDIM2,NCOLORS,ITT_IS_LINEAR,
     1                    LOWER_ITT,UPPER_ITT,GAMMA_D,IDV1)
          GOTO 700
        ENDIF
 
C Correcting the selected patch :
C NCODE: polynomial order
	CALL STRDELT(ARRAY,NX,NY,IDIM,I_MIN,I_MAX,J_MIN,J_MAX,
     1               IMAGE2,IDIM2,
     1               XP,YP,SIG,DIAM,ER,NDIMER,NCODE,FILE,
     1               NCOLORS,ITT_IS_LINEAR,LOWER_ITT,UPPER_ITT,
     1               AUTOMATIC_PATCH,ISTATUS,GAMMA_D,IDV1)

C Counting the number of patches :
	IF(ISTATUS.NE.0)THEN
	  WRITE(6,*) ' Do you want another patch ? (Y)'
	  READ(5,10) ANS
	  IF(ANS.NE.'N'.AND.ANS.NE.'n')GO TO 17
        ENDIF
 
99      CALL JLP_ERASE_STATUS_BAR(IDV1)
	RETURN
	END
C*************************************************************************
C Interactive version
C with many questions
C*************************************************************************
	SUBROUTINE PATCH2(ARRAY,NX,NY,IDIM,IMAGE2,IDIM2,
     1                    I_MIN,I_MAX,J_MIN,J_MAX,
     1                    NCOLORS,ITT_IS_LINEAR,LOWER_ITT,UPPER_ITT,
     1                    GAMMA_D,IDV1)
        IMPLICIT NONE
        INTEGER*4 NX,NY,IDIM,NX2,NY2,IDIM2,NCOLORS,NDIMER,IDV1
	PARAMETER (NDIMER=200)
	REAL*4 ARRAY(IDIM,*),ER(NDIMER)
        REAL*4 LOWER_ITT,UPPER_ITT,SIG,XP,YP,DIAM
        INTEGER*4 I_MIN,I_MAX,J_MIN,J_MAX,ITT_IS_LINEAR
	INTEGER*4 IMAGE2(IDIM2,*),ISTATUS,GAMMA_D
	INTEGER*4 DRAW_CROSS,AUTOMATIC_PATCH,IOBJECT,NCODE
        INTEGER*4 I,IN_FRAME,IBUTTON,IXP,IYP
	LOGICAL FILE,CURSOR
	CHARACTER NAME*40,ANS*1,BUFFER*80
 
10	FORMAT(A)

	IOBJECT=0
 
        AUTOMATIC_PATCH=0
           WRITE(6,12)
12         FORMAT(' Input positions with a file (f),',
     1             ' with the cursor (c) or manually (m)? (Return=c)')
           READ(5,10) ANS
 
	   FILE=.FALSE.
           CURSOR=.TRUE.
	   IF(ANS.EQ.'f'.OR.ANS.EQ.'F')FILE=.TRUE.
	   IF(ANS.EQ.'m'.OR.ANS.EQ.'M')CURSOR=.FALSE.
 
C----------------------------------------------------------------------
C First option : Positions in a file :
C----------------------------------------------------------------------
 
	IF(FILE) THEN
	  WRITE(6,*) ' Name of the file containing the positions ?'
	  READ(5,10) NAME
	  OPEN(UNIT=9,STATUS='OLD',FILE=NAME)
 
C Loop as long as file 9 contains something :
C Reading centre (XP,YP), and diameter DIAM from the file :
70	READ (9,*,END=55) XP,YP,DIAM,NCODE,SIG
	WRITE(6,71) XP,YP,DIAM,NCODE,SIG
71	FORMAT(' XP,YP,DIAMETER,NCODE,SIG :',3(F7.2,2X),I5,2X,F7.3)
 
C Go to next point if point outside of the image :
        IXP=INT(XP)
        IYP=INT(YP)
	IF(IXP.LT.0.OR.IXP.GE.NX.OR.IYP.LT.0.OR.IYP.GE.NY)THEN
	   WRITE(6,*) ' PATCH2/Error '
	   WRITE(6,*) ' The patch is outside the limits of the image'
	   GO TO 70
	ENDIF
 
C  Set up array of random errors with gaussian distribution
C  for approximating the noise when interpolating
C MEAN=0
        IF(SIG.GT.0.)THEN
	  CALL JLP_RANDOM_INIT(1)
          DO I=1,NDIMER
           CALL JLP_RANDOM_GAUSS(ER(I))
           ER(I)=ER(I)*SIG
	  END DO
        ELSE
          DO I=1,NDIMER
           ER(I)=0.
	  END DO
        ENDIF
 
C Counting the number of patches :
	IOBJECT=IOBJECT+1
 
C Displaying a circle:
C Warning: convention here is that (0,0) is the lower left corner,
        CALL JLP_CIRCLE1(XP,YP,DIAM,IDV1)
	CALL JLP_GFLUSH(IDV1)
 
C Correcting the selected patch :
	CALL STRDELT(ARRAY,NX,NY,IDIM,I_MIN,I_MAX,J_MIN,J_MAX,
     1               IMAGE2,IDIM2,
     1               XP,YP,SIG,DIAM,ER,NDIMER,NCODE,FILE,
     1               NCOLORS,ITT_IS_LINEAR,LOWER_ITT,UPPER_ITT,
     1               AUTOMATIC_PATCH,ISTATUS,GAMMA_D,IDV1)
 
	GO TO 70
 
55	WRITE(6,*) ' End of patch2: ',IOBJECT,' patches treated'
 
	CLOSE(9)
 
	ENDIF
 
C----------------------------------------------------------------------
C Second option : Interactive mode
C----------------------------------------------------------------------
 
	IF(.NOT.FILE) THEN
 
	  NAME='patch2.dat'
	  WRITE(6,63) NAME
63        FORMAT(' Creating an output file to store the positions',
     1 	/,' Name = ',A)
	  OPEN(4,STATUS='unknown',FILE=NAME)
	    WRITE(6,*) ' Order of the polynomial (0 to 5) and noise',
     1 	' (about 1.0) ?'
	    READ(5,*) NCODE,SIG
 
C  Set up array of random errors with gaussian distribution
C  for approximating the noise when interpolating
C  Generate normal errors, mean=0.
 
	  CALL JLP_RANDOM_INIT(1)
          DO I=1,NDIMER
           CALL JLP_RANDOM_GAUSS(ER(I))
           ER(I)=ER(I)*SIG
	  END DO
 
17	  IF(CURSOR)THEN 
            WRITE(BUFFER,10)'Click on the mouse to select the center'
            CALL JLP_DRAW_TO_STATUS_BAR(BUFFER,IDV1)
C Read the position of the cursor:
            DRAW_CROSS = 0
	    CALL JLP_WHERE(XP,YP,IN_FRAME,IBUTTON,DRAW_CROSS,IDV1)
          ELSE
            WRITE(6,29) 
29          FORMAT(' Enter center coordinates XP,YP',
     1           ' (0,0 is origin at bottom left) :')
            READ(5,*)XP,YP
            IN_FRAME=1
          ENDIF
         
C JLP_WHERE is written in C and returns an index starting at 0,0:
            IXP=INT(XP)
            IYP=INT(YP)

C Test to check if the point is on the image :
	  IF(IXP.LT.0.OR.IXP.GE.NX
     1 	.OR.IYP.LT.0.OR.IYP.GE.NY.OR.IN_FRAME.EQ.0)THEN
	    WRITE(6,77) IXP,IYP,IN_FRAME
77	    FORMAT(' PATCH2/Exit: Point outside of the image '
     1      ' IX, IY, IN_FRAME:',3I5)
	    GO TO 99 
	  ENDIF
 
          WRITE(BUFFER,10)'Please enter some data on the terminal...'
          CALL JLP_DRAW_TO_STATUS_BAR(BUFFER,IDV1)
	  WRITE(6,*) ' Diameter (in pixel units) ?'
	  READ(5,*) DIAM
C Display current parameters to terminal:
	  WRITE(6,71) XP,YP,DIAM,NCODE,SIG
 
C Displaying a circle:
        CALL JLP_CIRCLE1(XP,YP,DIAM,IDV1)
	CALL JLP_GFLUSH(IDV1)
 
C Correcting the selected patch :
C NCODE: polynomial order
	CALL STRDELT(ARRAY,NX,NY,IDIM,I_MIN,I_MAX,J_MIN,J_MAX,
     1               IMAGE2,IDIM2,
     1               XP,YP,SIG,DIAM,ER,NDIMER,NCODE,FILE,
     1               NCOLORS,ITT_IS_LINEAR,LOWER_ITT,UPPER_ITT,
     1               AUTOMATIC_PATCH,ISTATUS,GAMMA_D,IDV1)
C Counting the number of patches :
C and writing parameters to "patch2.dat":
	IF(ISTATUS.EQ.0)THEN
          IOBJECT=IOBJECT+1
	  WRITE(4,*) XP,YP,DIAM,NCODE,SIG
        ENDIF
 
	  WRITE(6,*) ' DO YOU WANT ANOTHER PATCH ? (Y)'
	  READ(5,10) ANS
	  IF(ANS.NE.'N'.AND.ANS.NE.'n')GO TO 17
 
99      CALL JLP_ERASE_STATUS_BAR(IDV1)
        CLOSE(4)
	ENDIF
 
	RETURN
	END
C ****************************************************************
C	SUBROUTINE STRDELT
C
C  Deletes pixels within a circle around point (XP,YP) and
C  interpolates them with a polynomial fitted to an annulus
C  of surrounding points.
C
C INPUT:
C NCODE: polynomial order
C I_MIN,I_MAX,J_MIN,J_MAX: boundaries of the zoomed image ARRAY
C                          (expressed as C indices starting at (0,0)!)
C
C OUPUT:
C ISTATUS: 0 if patch has been validated
C          1 otherwise
C ****************************************************************
	SUBROUTINE STRDELT(ARRAY,NX,NY,IDIM,I_MIN,I_MAX,J_MIN,J_MAX,
     1                     IMAGE2,IDIM2,
     1                     XP,YP,SIG,DIAM,ER,NDIMER,NCODE,FILE,
     1                     NCOLORS,ITT_IS_LINEAR,LOWER_ITT,UPPER_ITT,
     1                     AUTOMATIC_PATCH,ISTATUS,GAMMA_D,IDV1)
        IMPLICIT NONE
        INTEGER*4 NX,NY,IDIM,I_MIN,I_MAX,J_MIN,J_MAX,IDV1
	INTEGER*4 IDIM2,IMAGE2(IDIM2,*)
	INTEGER*4 NCODE,NDIMER,NCOLORS,AUTOMATIC_PATCH
        INTEGER*4 ISTATUS,GAMMA_D,ITT_IS_LINEAR
        REAL ARRAY(IDIM,*),ER(NDIMER)
        REAL LOWER_ITT,UPPER_ITT,XP,YP,SIG,DIAM
        REAL*4 NEW_VAL
	CHARACTER ANS*1,BUFFER*40
	LOGICAL FILE
	INTEGER*4 I1,I2,J1,J2,IS,JS,IRAD
	INTEGER*4 MADRID(1),ISIZE,IPNT,I
	COMMON /VMR/MADRID

10	FORMAT(A)
        ISTATUS=1
C
C DEFINE LIMITS OF AREA TO DELETE
C
      IF(DIAM.GT.2)THEN
        IRAD=INT(DIAM/2.)
      ELSE
C Select +/-2 pixels around the central pixel for display:
        IRAD=2
      ENDIF

C Conversion to Fortran convention:
      IS = INT(XP)+1
      JS = INT(YP)+1
      I1=MAX(IS-IRAD,I_MIN+1)
      I2=MIN(IS+IRAD,I_MAX)
      J1=MAX(JS-IRAD,J_MIN+1)
      J2=MIN(JS+IRAD,J_MAX)
C      WRITE(6,*) '(Fortran) boundaries (Imax,Imin,Jmax,Jmin): ',I1,I2,J1,J2
C
C Temporarily store current array contents
C
      ISIZE=4*(I2-I1+1)*(J2-J1+1)
      CALL JLP_GETVM(IPNT,ISIZE)
      CALL STRCOPY(ARRAY,NX,NY,IDIM,MADRID(IPNT),I1,I2,J1,J2)

C Fit a polynomial to the data:
30      IF(DIAM.GT.2)THEN
C If large diameter, call main routine which fits a polynomial to the data:
C NCODE: polynomial order
           CALL PATCH22(ARRAY,NX,NY,IDIM,XP,YP,
     1                  I_MIN,I_MAX,J_MIN,J_MAX,
     1                  I1,I2,J1,J2,DIAM,ER,NDIMER,NCODE)
        ELSE
C Else linear interpolation only (when DIAM <=2)
          I1=MAX(IS-1,I_MIN+1)
          I2=MIN(IS+1,I_MAX)
          J1=MAX(JS-1,J_MIN+1)
          J2=MIN(JS+1,J_MAX)
C Store new value (used later)
          NEW_VAL=(ARRAY(I1,JS)+ARRAY(I2,JS)
     1          +ARRAY(IS,J1)+ARRAY(IS,J2))/4.
          ARRAY(IS,JS)=NEW_VAL
        ENDIF

C New version: display the whole image (since computers are fast nowadays):
	CALL PATCH_DISP(ARRAY,NX,NY,IDIM,I_MIN,I_MAX,J_MIN,J_MAX,
     1                  IMAGE2,IDIM2,NCOLORS,ITT_IS_LINEAR,
     1                  LOWER_ITT,UPPER_ITT,GAMMA_D,IDV1)
  
C---------------------------------------------------------------
C Interactive choice
 
	IF(.NOT.FILE)THEN
	WRITE(6,*) ' Are you satisfied ? (Y)'
	READ(5,10) ANS
 
	IF(ANS.NE.'n'.AND.ANS.NE.'N')THEN
          ISTATUS = 0
        ELSE
C Restores previous data on the image:
	  CALL STRESTO(ARRAY,NX,NY,IDIM,MADRID(IPNT),I1,I2,J1,J2)
C New version: display the whole image (since computers are fast nowadays):
	  CALL PATCH_DISP(ARRAY,NX,NY,IDIM,I_MIN,I_MAX,J_MIN,J_MAX,
     1                    IMAGE2,IDIM2,NCOLORS,ITT_IS_LINEAR,
     1                    LOWER_ITT,UPPER_ITT,GAMMA_D,IDV1)
  
          IF(AUTOMATIC_PATCH.NE.1)THEN
C Possibility of another try with different order and noise parameters
          IF(DIAM.GT.2)THEN
	    WRITE(6,864) NCODE,SIG
864	    FORMAT(' Remember  : order =',I2,' noise :',F6.3,/,
     1	' Do you want to change these parameters ? (Y)')
	    READ(5,10) ANS
 
	    IF(ANS.NE.'n'.AND.ANS.NE.'N')THEN
	       WRITE(6,*) ' Order of the polynomial (0 to 3)',
     1	' and noise (sigma) ?'
	       READ(5,*) NCODE,SIG
 
C  Set up array of random errors with gaussian distribution
C  for approximating the noise when interpolating
              IF(SIG.GT.0.)THEN
	        CALL JLP_RANDOM_INIT(1)
                DO I=1,NDIMER
                 CALL JLP_RANDOM_GAUSS(ER(I))
                 ER(I)=ER(I)*SIG
	        END DO
              ELSE
                DO I=1,NDIMER
                 ER(I)=0.
               END DO
              ENDIF
 
	      GO TO 30
C End of case with change of parameters 
	   ENDIF	
C Case when DIAM.LT.2
          ELSE
            WRITE(6,34) ARRAY(IS,JS),NEW_VAL
34          FORMAT(' Current (old) value is',G12.5,/,
     1          ' Proposed new value is',G12.5,/
     1          ' Which new value do you want to put instead? (E to exit)')
            READ(5,10)BUFFER
            IF(BUFFER(1:1).EQ.'E'.OR.BUFFER(1:1).EQ.'e')GOTO 200
            READ(BUFFER,*,ERR=200)NEW_VAL
	    GO TO 30
C End of DIAM.LT.2...
          ENDIF
C End of not AUTOMATIC_PATCH 
          ENDIF
 
C End of case not satisfied .... 
	ENDIF
 
C End of condition "IF (.NOT.FILE)"
        ELSE
         ISTATUS = 0
	ENDIF
C-----------------------------------------------------------------------
 
C Label for exit when problem
200	CONTINUE
 
	CALL JLP_FREEVM(IPNT,ISIZE)
 
	RETURN
	END
C************************************************************************
C
C New version: display the whole image (since computers are fast nowadays):
C
C I_MIN,I_MAX,J_MIN,J_MAX: boundaries of the zoomed image ARRAY
C                          (expressed as C indices starting at (0,0)!)
C************************************************************************
	SUBROUTINE PATCH_DISP(ARRAY,NX,NY,IDIM,I_MIN,I_MAX,J_MIN,J_MAX,
     1                        IMAGE2,IDIM2,NCOLORS,ITT_IS_LINEAR,
     1                        LOWER_ITT,UPPER_ITT,GAMMA_D,IDV1)
        IMPLICIT NONE
        INTEGER*4 NX,NY,IDIM,I_MIN,I_MAX,J_MIN,J_MAX,IDV1
	INTEGER*4 IDIM2,IMAGE2(IDIM2,*)
        INTEGER*4 NCOLORS,GAMMA1,GAMMA_D,BLACK_AND_WHITE
        REAL ARRAY(IDIM,*)
        REAL LOWER_ITT,UPPER_ITT,XMIN,XMAX,YMIN,YMAX
	INTEGER*4 MADRID(1),ITT_IS_LINEAR
        INTEGER*4 NI,NJ,NI2,NJ2,OFFX,OFFY,AXLEN,AYLEN,PLAN
	COMMON /VMR/MADRID

        NI = I_MAX - I_MIN
        NJ = J_MAX - J_MIN
        GAMMA1 = IDIM/IDIM2 
	NI2=NI/GAMMA1
	NJ2=NJ/GAMMA1
	CALL CONVERT_TO_LUT(ARRAY(I_MIN+1,J_MIN+1),NI,NJ,IDIM,
     1      IMAGE2,NI2,NJ2,IDIM2,NCOLORS,
     1      ITT_IS_LINEAR,LOWER_ITT,UPPER_ITT,IDV1)
        BLACK_AND_WHITE=0
        CALL JLP_GET_PLOT_PARAM(OFFX,OFFY,AXLEN,AYLEN,XMIN,XMAX,
     1                          YMIN,YMAX,PLAN,IDV1)
	CALL JLP_PLOT_IMAGE(IMAGE2,NI2,NJ2,IDIM2,OFFX,OFFY,
     1                      GAMMA_D,BLACK_AND_WHITE,IDV1)
	CALL JLP_GFLUSH(IDV1)

	RETURN
	END
C************************************************************************
C
C Store current star image in temporary array
C
C************************************************************************
      SUBROUTINE STRCOPY(ARRAY,NX,NY,IDIM,TEMP,I1,I2,J1,J2)
      IMPLICIT NONE
      INTEGER NX,NY,IDIM,I1,I2,J1,J2
      REAL ARRAY(IDIM,*),TEMP(*)
      INTEGER I,J,N
C
      N=0
      DO J=J1,J2
       DO I=I1,I2
        N=N+1
        TEMP(N)=ARRAY(I,J)
       END DO
      END DO

      RETURN
      END
C************************************************************************
C
C Restore array to previous state
C
C************************************************************************
      SUBROUTINE STRESTO(ARRAY,NX,NY,IDIM,TEMP,I1,I2,J1,J2)
      IMPLICIT NONE
      INTEGER NX,NY,IDIM,I1,I2,J1,J2
      REAL ARRAY(IDIM,*),TEMP(*)
      INTEGER I,J,N
      
      N=0
      DO J=J1,J2
        DO I=I1,I2
         N=N+1
         ARRAY(I,J)=TEMP(N)
       END DO
      END DO

      RETURN
      END
C******************************************************************
C Subroutine to fit a polynomial to an annulus around the center
C and replace input values by noised computed values
C Real "core" of patch2
C 
C INPUT:
C NCODE: polynomial order
C******************************************************************
	SUBROUTINE PATCH22(ARRAY,NX,NY,IDIM,XP,YP,
     1                     I_MIN,I_MAX,J_MIN,J_MAX,
     1	                   I1,I2,J1,J2,DIAM,ER,NDIMER,NCODE)
        IMPLICIT NONE
        INTEGER*4 NX,NY,IDIM,IS,JS,I1,I2,J1,J2,NDIMER,NCODE
        INTEGER*4 NDIM,I_MIN,I_MAX,J_MIN,J_MAX
	PARAMETER (NDIM=1000)
C Maximum: NTERMS=30
	DOUBLE PRECISION ZZ(NDIM),D(30),SE(30),RDOE(30)
	DOUBLE PRECISION POLY
	REAL ARRAY(IDIM,*),ER(NDIMER)
	REAL XX(NDIM,2),YY(NDIM),XP,YP
	REAL FACTOR,XRAN,XZ,YZ,DIAM,XNOISE
        REAL TEST,SDOR,DX,DY 
	REAL RADMIN2,DIAM_MAX,RADMAX,RADMAX2,RAD2
	INTEGER*4 I,J,K,KQ,NR,NINC
	INTEGER*4 NTERMS,NPTS,IMIN,IMAX,JMIN,JMAX
C Contained in "patch_set.for"
	EXTERNAL POLY

C
C     SET UP ARRAYS FOR NEQSOL
C
C FACTOR=2 => annulus of diameters DIAM (inside) and 2*DIAM (outside)
C Before 2006:
C      FACTOR=2
C JLP2006: FACTOR=2 is bad: I reduce it to 1.5
      FACTOR=1.5
      DIAM_MAX=DIAM*FACTOR
      RADMAX=(DIAM_MAX+1)/2
      RADMAX2=RADMAX*RADMAX
      NINC=(DIAM_MAX-1)/30+1
C Conversion to Fortran convention:
      IS = INT(XP)+1
      JS = INT(YP)+1
      IMIN=MAX(IS-RADMAX,I_MIN+1)
      IMAX=MIN(IS+RADMAX,I_MAX)
      JMIN=MAX(JS-RADMAX,J_MIN+1)
      JMAX=MIN(JS+RADMAX,J_MAX)
C
C     DEFINE NORMALIZING FACTORS SUCH THAT THE COORDS. OF
C     ALL THE POINTS ARE BETWEEN (-1,-1) AND (1,1).
C
      RADMIN2=(DIAM/2.)**2
      DX=2./(IMAX-IMIN)
      DY=2./(JMAX-JMIN)
      NPTS=0
      DO 20 J=JMIN,JMAX,NINC
       DO 20 I=IMIN,IMAX,NINC
C
C Calculate radius, and reject if too close to star centre
C
C JLP2006: The cursor gives (0.5,0.5) when the cursor
C is centered on the pixel (0,0) in the bottom-left corner:
        RAD2=(REAL(J - 1) - (YP - 0.5))**2 
     1         + (REAL(I - 1) - (XP - 0.5))**2
        IF (RAD2 .GT. RADMIN2 .AND. RAD2 .LE. RADMAX2)THEN
          NPTS=NPTS+1
          XX(NPTS,1)=(I-IMIN)*DX-1.
          XX(NPTS,2)=(J-JMIN)*DY-1.
	  YY(NPTS)=ARRAY(I,J)
	ENDIF
20    CONTINUE

      IF (NCODE .EQ. 5)THEN
        NTERMS=21
      ELSE IF (NCODE .EQ. 4)THEN
        NTERMS=15
      ELSE IF (NCODE .EQ. 3)THEN
        NTERMS=10
      ELSE IF (NCODE .EQ. 2)THEN
        NTERMS=6
      ELSE IF (NCODE .EQ. 1)THEN
        NTERMS=3
      ELSE
        NTERMS=1
      END IF
C
      IF (NPTS .LT. NTERMS)THEN
       WRITE(6,62)
62	FORMAT('PATCH22/Error: too few points to fit background')
	WRITE(6,*) ' NPTS, NTERMS',NPTS,NTERMS
        GO TO 200
      END IF
C
C Erase any previous solution
C
      DO I=1,30
         D(I)=0.
      END DO
C
C Fit the polynomial with all points:
C
      CALL NEQSOL(XX,YY,ZZ,NDIM,NPTS,NTERMS,1,0,D,SE,RDOE,SDOR)

C Perform twice a 2 sigma rejection:
      DO KQ=1,2
        TEST=2.*SDOR
        CALL REJECT(XX,YY,NDIM,D,NPTS,NTERMS,TEST,NR,1)
C NR: number of points after rejection
        NPTS=NR
        IF (NPTS .LT. NTERMS)THEN
          WRITE(6,63) NPTS
63        FORMAT('PATCH22/Error: too few points after rejection (NPTS=',
     1           I3,')')
          GO TO 200
        END IF

C Refit polynomial with less points: 
C
      CALL NEQSOL(XX,YY,ZZ,NDIM,NPTS,NTERMS,1,0,D,SE,RDOE,SDOR)

      END DO 
C
C Evaluate polynomial at each point within circle
C
      DO 60 J=J1,J2
C
C Find a random starting points in the sequence of noise points
C To obtain random numbers between 0. and 1.
      CALL JLP_RANDOM(XRAN)
      K=INT(XRAN*REAL(NDIMER))
      K=MAX(1,K)
      K=MIN(K,NDIMER)
C
      YZ=(J-JMIN)*DY-1.
      DO 50 I=I1,I2
C JLP2006: The cursor gives (0.5,0.5) when the cursor
C is centered on the pixel (0,0) in the bottom-left corner:
        RAD2=(REAL(J - 1) - (YP - 0.5))**2 
     1         + (REAL(I - 1) - (XP - 0.5))**2
        IF (RAD2 .LE. RADMIN2) THEN
          XZ=(I-IMIN)*DX-1.
          K=K+1
          IF (K .GT. NDIMER)K=1
          XNOISE=ER(K)*SDOR
C Calling POLY, contained in "patch_set.for"
          ARRAY(I,J)=POLY(XZ,YZ,D)+XNOISE
        ENDIF
50    CONTINUE
 
60    CONTINUE

200    RETURN
       END
