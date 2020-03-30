C++--------------------------------------------------------------------
C Program ARITHM
C To perform arithmetic operations on files
C Uses the dynamical allocation of memory
C
C SYNTAX (some examples only)
C To add two files:
C   RUNS ARITHM +F input1 input2 output
C Or:
C   RUNS ARITHM +2F input1,input2 output
C To add three files:
C   RUNS ARITHM +3F input1,input2,input3 output
C To add 6 files:
C   RUNS ARITHM +6F in1,in2,in3,in4,in5,in6 output
C For subtracting input2 from input1: (output=input1-input2)
C   RUNS ARITHM -F input1 input2 output
C For multiplying two files: (output=input1*input2)
C   RUNS ARITHM *F input1 input2 output
C For subtracting a constant (0.345 here) from input1:
C   RUNS ARITHM -C 0.345 input output
C
C (Please use prompting mode to see the whole menu with all options)
C
C JLP
C Version of 20-08-2008
C--------------------------------------------------------------------
C NOTA to the programmer: keep the same options, since command procedures use
C this program...
C**************************************************************************
	PROGRAM ARITHM
        PARAMETER (NFILES_MAX=10)
C For 32-bit computers, PNTR should be declared as integer*4:
C	INTEGER*4 PNTR_INPUT(NFILES_MAX),PNTR_OUTPUT,LIN(NFILES_MAX)
C For 64-bit computers, PNTR should be declared as integer*8:
	INTEGER*8 PNTR_INPUT(NFILES_MAX),PNTR_OUTPUT,LIN(NFILES_MAX)
	CHARACTER NAMEIN(NFILES_MAX)*60,NAMEOUT*60
	CHARACTER COMMENTS*80
C BUFFER set to 160, i.e., two lines (when 10 files, long lines...)
	CHARACTER ANS*4, BUFFER*160
	INTEGER*4 MADRID(1),K,I1,I2,NFILES,NX1,NY1,MEMS_OUTPUT
	COMMON /VMR/MADRID
 
10	FORMAT(A)
 
	CALL JLP_BEGIN
	CALL JLP_INQUIFMT
 
	PRINT 70
70	FORMAT(' MENU : ',/,
     1	' "+C" : OUT = IMAGE1 + C',/,
     1	' "-C" : OUT = IMAGE1 - C',/,
     1	' "*C" : OUT = IMAGE1  * C',/,
     1	' "/C" : OUT = IMAGE1  / C',/,
     1	' "LOG": OUT = LOG10(IMAGE1 - C)',/,
     1	' "+F" : OUT = (IMAGE1 + IMAGE2)*C ',/,
     1	' "+2F" : OUT = IMAGE1 + IMAGE2 ',/,
     1	' "+3F" : OUT = IMAGE1 + IMAGE2 + IMAGE3',/,
     1	' "+4F" : OUT = IMAGE1 + IMAGE2 + IMAGE3 + IMAGE4',/,
     1  ' ........  ',/,
     1	' "+10F" : OUT = IMAGE1 + IMAGE2 + ... + IMAGE9 + IMAGE10',/,
     1	' "-F" : OUT = IMAGE1 - IMAGE2 ',/,
     1	' "/F" : OUT = (IMAGE1 - C1) / (IMAGE2 - C2)',/,
     1	' "*F" : OUT = (IMAGE1 - C1) * (IMAGE2 - C2)',/,
     1	' Else : EXIT',/,
     1	' Enter your choice: ',$)
	READ(5,10) ANS

C Goes to upper case:
        CALL JLP_UPCASE(ANS,4)

C Check if option is good and set NFILES:
        IF(ANS(1:1).EQ.'E'.OR.ANS(1:1).EQ.'e')THEN
          PRINT *,' OK, exit'
          STOP
        ENDIF
        IF(ANS(1:1).EQ.'+'.AND.ANS(3:3).EQ.'F')THEN
          READ(ANS(2:2),23) NFILES
23        FORMAT(I1)
          PRINT *,' OK, multiple sum of ',NFILES,' files'
          IF(NFILES.GT.NFILES_MAX)THEN
            WRITE(6,22) NFILES_MAX
22          FORMAT(' Error: only ',I1,' files are authorized')
            STOP
          ENDIF
        ELSEIF(ANS.EQ.'+10F')THEN
          NFILES=10
          PRINT *,' OK, multiple sum of ',NFILES,' files'
        ELSEIF(ANS.NE.'+C'.AND.ANS.NE.'-C'.AND.ANS.NE.'*C'.AND.
     1    ANS.NE.'/C'.AND.
     1    ANS.NE.'LOG'.AND.ANS.NE.'+F'.AND.ANS.NE.'-F'.AND.
     1    ANS.NE.'/F'.AND.ANS.NE.'*F')THEN
          PRINT *,' Bad syntax, so I exit'
          STOP
        ENDIF
        IF(ANS(2:2).EQ.'C'.OR.ANS(2:2).EQ.'O')THEN
          NFILES = 1
          WRITE(6,*) 'Input file: '
          READ(5,10) NAMEIN(1)
	ELSEIF(ANS(2:2).EQ.'F')THEN
          NFILES = 2
          WRITE(6,*) 'Input 1st file: '
          READ(5,10) NAMEIN(1)
          WRITE(6,*) 'Input 2nd file: '
          READ(5,10) NAMEIN(2)
        ELSE
          PRINT *,' Enter the input file names (separated by commas):'
          READ(5,10)BUFFER
C Debug:
          PRINT *,BUFFER
21        FORMAT('A')
          I1=1
          DO K=1,NFILES
	    I2=INDEX(BUFFER(I1:),',')-2
            IF(I2.LT.0)THEN
	      I2=INDEX(BUFFER(I1:),' ')-1
C When ','  or ' ' not found, exit next time
              IF(I2.LT.0)I2=160-I1
            ENDIF
            I2 = I1 + I2
          PRINT *,'I1,I2',I1,I2
            WRITE(NAMEIN(K),10) BUFFER(I1:MIN(I2,I1+39))
            LIN(K)=INDEX(NAMEIN(K),' ')
            LIN(K)=MIN(LIN(K),80/NFILES)
          PRINT *,'name,length:',NAMEIN(K),LIN(K)
            I1=I2+2
            IF(I1.GE.160)GOTO 324
          ENDDO
C Exit from the loop
324       CONTINUE
        ENDIF

C Set new code LC for LOG:
        IF(ANS.EQ.'LOG')ANS='LC'
 
C Check if constant is needed:
	IF(ANS(3:3).NE.'F'.AND.ANS(2:2).EQ.'C'.OR.ANS.EQ.'+F')THEN
	  PRINT *,' Enter the value of the constant C :'
	  READ(5,*) CT1
        ENDIF
 
C Input of the images:
       CALL READ_IMAGES1(PNTR_INPUT,NX1,NY1,NAMEIN,COMMENTS,NFILES)
 
C Checking max size for writing filenames on COMMENTS:
	 LIN(1)=INDEX(NAMEIN(1),'  ')
	 LIN(2)=INDEX(NAMEIN(2),'  ')
	  IF((LIN(1)+LIN(2)).GT.36)THEN
	   LIN(1)=18
	   LIN(2)=18
	  ENDIF

C--------------------------------------------------------------
C Getting dynamic memory for the output :
	  MEMS_OUTPUT=4*NX1*NY1
 	  CALL JLP_GETVM(PNTR_OUTPUT,MEMS_OUTPUT)
 
C Output file :
	  PRINT *,' OUTPUT FILE ?'
	  READ(5,10) NAMEOUT
 
C-------------------------------------------------------------
C +C Adding a constant :
	IF(ANS(1:2).EQ.'+C')THEN
          CT2=-CT1
	  CALL C_MUL(MADRID(PNTR_INPUT(1)),MADRID(PNTR_OUTPUT),
     1	NX1,NY1,NX1,CT2,1.)
	  WRITE(COMMENTS,102) NAMEIN(1)(1:LIN(1)),CT1
102	FORMAT(A,' + ',G12.5,'//')
 
C-------------------------------------------------------------
C -C Subtracting a constant :
	ELSEIF(ANS(1:2).EQ.'-C')THEN
	  CALL C_MUL(MADRID(PNTR_INPUT(1)),MADRID(PNTR_OUTPUT),
     1	NX1,NY1,NX1,CT1,1.)
	  WRITE(COMMENTS,103) NAMEIN(1)(1:LIN(1)),CT1
103	FORMAT(A,' - ',G12.5,'//')
 
C-------------------------------------------------------------
C *C Multipying with a constant :
	ELSEIF(ANS(1:2).EQ.'*C')THEN
	  CALL C_MUL(MADRID(PNTR_INPUT(1)),MADRID(PNTR_OUTPUT),
     1	NX1,NY1,NX1,0.,CT1)
	  WRITE(COMMENTS,104) NAMEIN(1)(1:LIN(1)),CT1
104	FORMAT(A,' *',G12.5,'//')
 
C-------------------------------------------------------------
C /C Dividing with a constant :
	ELSEIF(ANS(1:2).EQ.'/C')THEN
          IF(CT1.EQ.0)THEN
            PRINT *,' Fatal error: C = 0 !'
            STOP
          ENDIF
          CT2=1./CT1
	  CALL C_MUL(MADRID(PNTR_INPUT(1)),MADRID(PNTR_OUTPUT),
     1	NX1,NY1,NX1,0.,CT2)
	  WRITE(COMMENTS,105) NAMEIN(1)(1:LIN(1)),CT1
105	FORMAT(A,' /',G12.5,'//')
 
C-------------------------------------------------------------
C LC or LOG: 
	ELSEIF(ANS(1:2).EQ.'LC')THEN
	  CALL LOG_FILE(MADRID(PNTR_INPUT(1)),MADRID(PNTR_OUTPUT),
     1	NX1,NY1,NX1,CT1,1.)
	  WRITE(COMMENTS,204) NAMEIN(1)(1:LIN(1)),CT1
C JLP2001: I replace ( ) by [ ] to avoid pb with postscript hardcopies...
204	FORMAT('LOG10[',A,' -',G12.5,' ]//')
 
C-------------------------------------------------------------
C +F Adding 2 files :
	ELSEIF(ANS(1:2).EQ.'+F')THEN
	  CALL ADD_FILE(MADRID(PNTR_INPUT(1)),MADRID(PNTR_INPUT(2)),
     1	MADRID(PNTR_OUTPUT),NX1,NY1,NX1)
	  CALL C_MUL(MADRID(PNTR_OUTPUT),MADRID(PNTR_OUTPUT),
     1	NX1,NY1,NX1,0.,CT1)
	  WRITE(COMMENTS,302) NAMEIN(1)(1:LIN(1)),NAMEIN(2)(1:LIN(2)),CT1 
C JLP2001: I replace ( ) by [ ] to avoid pb with postscript hardcopies...
302	  FORMAT('[',A,' + ',A,']*',G12.5,' //')
 
C-------------------------------------------------------------
C +F Adding 2 or more files :
	ELSEIF(ANS(3:3).EQ.'F'.OR.ANS.EQ.'+10F')THEN
          CALL ERASE_ARRAY(MADRID(PNTR_OUTPUT),NX1,NY1,NX1)
          DO K=1,NFILES
	    CALL ADD_ONE_MORE_FILE(MADRID(PNTR_INPUT(K)),
     1	           MADRID(PNTR_OUTPUT),NX1,NY1,NX1)
          ENDDO
C To prevent overloading "comments":
          IF(NFILES.LE.3)THEN
	    WRITE(COMMENTS,303) (NAMEIN(K)(1:LIN(K)),K=1,NFILES) 
          ELSE
	    WRITE(COMMENTS,303) (NAMEIN(K)(1:LIN(K)),K=1,3) 
          ENDIF
303	  FORMAT(5(A,' + '))
 
C-------------------------------------------------------------
C -F Subtracting 2 files :
	ELSEIF(ANS(1:2).EQ.'-F')THEN
	  CALL SUB_FILE(MADRID(PNTR_INPUT(1)),MADRID(PNTR_INPUT(2)),
     1	MADRID(PNTR_OUTPUT),NX1,NY1,NX1)
	  WRITE(COMMENTS,402) NAMEIN(1)(1:LIN(1)),NAMEIN(2)(1:LIN(2))
402	  FORMAT(A,' - ',A,' //')
 
C-------------------------------------------------------------
C /F Dividing two files :
	ELSEIF(ANS(1:2).EQ.'/F')THEN
	  PRINT *,' Sky levels (C1, C2) for images 1 and 2 :'
	  READ(5,*) SKY1,SKY2
	  WRITE(COMMENTS,502) NAMEIN(1)(1:LIN(1)),SKY1,
     1	NAMEIN(2)(1:LIN(2)),SKY2
502	  FORMAT(A,'-',G11.4,' / ',A,'-',G11.4)
	  CALL DIV_FILE(MADRID(PNTR_INPUT(1)),MADRID(PNTR_INPUT(2)),
     1	MADRID(PNTR_OUTPUT),NX1,NY1,NX1,SKY1,SKY2)
 
C-------------------------------------------------------------
C *F Multiplying two files :
	ELSEIF(ANS(1:2).EQ.'*F')THEN
	  PRINT *,' Sky levels (C1, C2) for images 1 and 2 :'
	  READ(5,*) SKY1,SKY2
	  WRITE(COMMENTS,602) NAMEIN(1)(1:LIN(1)),SKY1,
     1	NAMEIN(2)(1:LIN(2)),SKY2
602	  FORMAT(A,' - ',G11.4,' * ',A,' - ',G11.4)
	  CALL MUL_FILE(MADRID(PNTR_INPUT(1)),MADRID(PNTR_INPUT(2)),
     1	MADRID(PNTR_OUTPUT),NX1,NY1,NX1,SKY1,SKY2)
 
C--------------------------------------------------------------
	ENDIF
 
C Output :
	  CALL JLP_WRITEIMAG(MADRID(PNTR_OUTPUT),NX1,NY1,NX1,NAMEOUT,
     1	COMMENTS)
 
	CALL JLP_END
	STOP
	END
C-------------------------------------------------------------
C LOG_FILE
C OUTPUT = LOG10((IMAGE2 - CT1) / CT2)
C--------------------------------------------------------------
	SUBROUTINE LOG_FILE(INPUT,OUTPUT,NX1,NY1,IDIM,CT1,CT2)
	INTEGER*4 NX1,NY1,IDIM
	REAL*4 INPUT(IDIM,*),OUTPUT(IDIM,*)
	REAL*4 CT1,CT2
	DO IY=1,NY1
	  DO IX=1,NX1
	   WORK=(INPUT(IX,IY)-CT1)/CT2
	   IF(WORK.LE.0.)THEN
	     OUTPUT(IX,IY)=-30.	
	   ELSE
	     OUTPUT(IX,IY)=ALOG10(WORK)
	   ENDIF
	  END DO
	END DO
	RETURN
	END
C--------------------------------------------------------------
C READ_IMAGES1
C--------------------------------------------------------------
       SUBROUTINE  READ_IMAGES1(PNTR_INPUT,NX1,NY1,NAMEIN,COMMENTS,NFILES)
C For 32-bit computers, PNTR should be declared as integer*4:
C       INTEGER*4 PNTR_INPUT(*)
C For 64-bit computers, PNTR should be declared as integer*8:
       INTEGER*8 PNTR_INPUT(*)
       INTEGER*4 NX1,NY1,NFILES,NX2,NY2
       CHARACTER NAMEIN(*)*60,COMMENTS*80
       
C Entering the first image :
         WRITE(6,*) 'Reading: ',namein(1)
	 CALL JLP_VM_READIMAG(PNTR_INPUT(1),NX1,NY1,NAMEIN(1),COMMENTS)
 
C Entering more images if needed:
         DO K=2,NFILES
           WRITE(6,*) 'Now reading: ',namein(k)
	   CALL JLP_VM_READIMAG(PNTR_INPUT(K),NX2,NY2,NAMEIN(K),COMMENTS)
 
C Checking the size of the input files:
	   IF(NX1.NE.NX2.OR.NY1.NE.NY2)THEN
	     PRINT *,' Fatal error: different size of the input files'
	     STOP
	   ENDIF
         ENDDO

        RETURN
        END
C------------------------------------------------------------------------
        SUBROUTINE ERASE_ARRAY(ARRAY,NX,NY,IDIM)
        INTEGER*4 NX,NY,IDIM
        REAL*4 ARRAY(IDIM,*)
          DO J=1,NY
            DO I=1,NX
              ARRAY(I,J)=0.
            ENDDO
          ENDDO
        RETURN
        END
C------------------------------------------------------------------------
	SUBROUTINE ADD_ONE_MORE_FILE(IN,OUT,NX,NY,IDIM)
        INTEGER*4 NX,NY,IDIM
        REAL*4 IN(IDIM,*),OUT(IDIM,*) 
          DO J=1,NY
            DO I=1,NX
              OUT(I,J)=OUT(I,J)+IN(I,J)
            ENDDO
          ENDDO
        RETURN
        END
C------------------------------------------------------------------------
        include 'jlpsub:jlp_upcase.for'
	include 'jlpsub:arithm_set.for'
