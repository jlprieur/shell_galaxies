C++--------------------------------------------------------
C Set of subroutines to access different formats
C Fortran interface with "jlp0_accc.c"
C
C Contains 
C  JLP_FROM_MADRID, JLP_LOC_MADRID
C  JLP_VM_READIMAG
C
C JLP
C Version of 20-11-2007 
C----------------------------------------------------------
C**********************************************************************
C Conversion of the pointer, to make it absolute
C and not dependent of common block MADRID:
C (For virtual memory in C)
C MADRID_PNTR = (PNTR_IMAGE - JLP_LOC(MADRID(1)))/4 +1
C Thus
C PNTR_IMAGE = JLP_LOC(MADRID(1)) + 4*(MADRID_PNTR -1)
C**********************************************************************
        SUBROUTINE JLP_FROM_MADRID(MADRID_PNTR,OUT_PNTR)
C Nota: INTEGER*8 is taken into account on OSF/DEC systems
C       and not for LINUX, but that is what we want:
C OSF/DEC, 8-byte address:
C        INTEGER*8 MADRID_PNTR,OUT_PNTR
C Linux 64 bits: I switch to INTEGER*8
        INTEGER*8 MADRID_PNTR,OUT_PNTR
C But not MADRID!
        INTEGER*4 MADRID(1)
        COMMON/VMR/MADRID
C Before jan 1999:
C          OUT_PNTR = JLP_LOC(MADRID(1)) + 4*(MADRID_PNTR -1)
          CALL JLP_LOC(MADRID(1),OUT_PNTR)
          OUT_PNTR = OUT_PNTR + 4*(MADRID_PNTR -1)
        RETURN
        END

C**********************************************************************
C Fortran interface to "C" programs, through common block MADRID
C**********************************************************************
        SUBROUTINE JLP_LOC_MADRID(OUT_PNTR)
C OSF/DEC, 8-byte address:
C	INTEGER*8 OUT_PNTR
C Linux, 4-byte address:
C	INTEGER*4 OUT_PNTR, MADRID(1)
C Linux 64 bits: I switch to INTEGER*8
        INTEGER*8 OUT_PNTR
        INTEGER*4 MADRID(1)
        COMMON/VMR/MADRID
C Before jan 1999:
C        OUT_PNTR = JLP_LOC(MADRID(1))
          CALL JLP_LOC(MADRID(1),OUT_PNTR)
        RETURN
        END
C----------------------------------------------------------
C Subroutine JLP_VM_READIMAG
C To read a file in different formats and
C to write it in a real*4 array IMAGE(NX,NY) starting at PNTR_IMAGE (pointer)
C
C
C Output :
C PNTR_IMAGE : location of the starting address of the array IMAGE
C              relative to common block MADRID...
C NX, NY : Size of the input image
C FILENAME *40
C COMMENTS *80
C----------------------------------------------------------
	SUBROUTINE JLP_VM_READIMAG(PNTR_IMAGE,NX,NY,FILENAME,
	1	COMMENTS)
C OSF/DEC, 8-bit address:
C        INTEGER*8 PNTR_IMAGE,PNTR_MADRID
C Linux, 4-bit address:
C Linux 64 bits:
        INTEGER*8 PNTR_IMAGE,PNTR_MADRID
        INTEGER*4 NX,NY,MADRID(1)
        CHARACTER COMMENTS*80,FILENAME*40
        COMMON/VMR/MADRID
 
C Calling C routine in "jlp0_accc.c"
	CALL JLP_VM_READIMAG1(PNTR_IMAGE,NX,NY,FILENAME,COMMENTS,ISTAT)

C Conversion of the pointer, to make it relative to common block
C MADRID:
          CALL JLP_LOC(MADRID(1),PNTR_MADRID)
          PNTR_IMAGE = (PNTR_IMAGE - PNTR_MADRID)/4 +1
 
C        CALL TEST(MADRID(PNTR_IMAGE),NX,NY)

	RETURN
	END
C******************************************************************
C        SUBROUTINE TEST(INPUT,NX,NY)
C        INTEGER*4 NX,NY
C        REAL*4 INPUT(NX,*)
C        PRINT *,'jlp_access2: central value:',INPUT(NX/2,NY/2)
C        RETURN
C        END
