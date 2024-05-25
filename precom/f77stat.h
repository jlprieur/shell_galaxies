/* @(#)f77stat.h	6.1.1.1 (ESO-IPG) 7/16/93 20:23:53 */
/*+++++++++  Remove ESO extensions to FORTRAN 77    ++++++++++++++++++++
.COPYRIGHT   (c) 1991 European Southern Observatory
.LANGUAGE    C
.IDENT       f77stat.h
.AUTHOR      Preben J. Grosbol [ESO/IPG]
.KEYWORDS    fortran statement type
.ENVIRONMENT UNIX
.COMMENT     include file with definitions of f77 statements
.VERSION     1.0   30-Oct-1987: Creation,     PJG
.VERSION     1.1    1-Feb-1988: Insert typed functions,     PJG
.VERSION     1.2   17-Apr-1991: Define max no. statement types, PJG
------------------------------------------------------------------------*/
/*  Type definitions for FORTRAN statements.
    The type consists of two byte: the LSB is a sequencial no. while
	the MSB is the statement group.
*/
#ifndef f77stat_h
#define f77stat_h

#define  ASSIGN                0x1001   /* ASSIGN statement             */
#define  BACKSPACE             0x1002   /* BACKSPACE statement          */
#define  BLOCKDATA             0x0103   /* BLOCDATA statement           */
#define  CALL                  0x1004   /* CALL statement               */
#define  CHARACTER             0x0405   /* CHARACTER statement          */
#define  CLOSE                 0x1006   /* CLOSE statement              */
#define  COMMON                0x0407   /* COMMON statement             */
#define  COMPLEX               0x0408   /* COMPLEX statement            */
#define  CONTINUE              0x1009   /* CONTINUE statement           */
#define  DATA                  0x080A   /* DATA statement               */
#define  DIMENSION             0x040B   /* DIMENSION statement          */
#define  DO                    0x100C   /* DO statement                 */
#define  DOUBLEPRECISION       0x040D   /* DOUBLE PRECISION statement   */
#define  ELSE                  0x100E   /* ELSE statement               */
#define  ELSEIF                0x100F   /* ELSEIF statement             */
#define  END                   0x2010   /* END statement                */
#define  ENDIF                 0x1011   /* ENDIF statement              */
#define  ENDFILE               0x1012   /* ENDFILE statement            */
#define  ENTRY                 0x1F13   /* ENTRY statement              */
#define  EQUIVALENCE           0x0414   /* EQUIVALENCE statement        */
#define  EXTERNAL              0x0415   /* EXTERNAL statement           */
#define  FORMAT                0x1F16   /* FORMAT statement             */
#define  FUNCTION              0x0117   /* FUNCTION statement           */
#define  GOTO                  0x1018   /* GOTO statement               */
#define  IF                    0x1019   /* IF statement                 */
#define  IMPLICIT              0x021A   /* IMPLICIT statement           */
#define  INQUIRE               0x101B   /* INQUIRE statement            */
#define  INTEGER               0x041C   /* INTEGER statement            */
#define  INTRINSIC             0x041D   /* INTRISIC statement           */
#define  LOGICAL               0x041E   /* LOGICAL statement            */
#define  OPEN                  0x101F   /* OPEN statement               */
#define  PARAMETER             0x0620   /* PARAMETER statement          */
#define  PAUSE                 0x1021   /* PAUSE statement              */
#define  PRINT                 0x1022   /* PRINT statement              */
#define  PROGRAM               0x0123   /* PROGRAM statement            */
#define  READ                  0x1024   /* READ statement               */
#define  REAL                  0x0425   /* REAL statement               */
#define  RETURN                0x1026   /* RETURN statement             */
#define  REWIND                0x1027   /* REWIND statement             */
#define  SAVE                  0x0428   /* SAVE statement               */
#define  STOP                  0x1029   /* STOP statement               */
#define  SUBROUTINE            0x012A   /* SUBROUTINE statement         */
#define  WRITE                 0x102B   /* WRITE statement              */
#define  IMPLICITNONE          0x022C   /* IMPLICIT NONE statement      */
#define  ENDDO                 0x102D   /* ENDDO statement              */
#define  INCLUDE               0x1E2E   /* INCLUDE statement            */
#define  RFUNCTION             0x012F   /* REAL FUNCTION statement      */
#define  IFUNCTION             0x0130   /* INTEGER FUNCTION statement   */
#define  LFUNCTION             0x0131   /* LOGICAL FUNCTION statement   */
#define  DFUNCTION             0x0132   /* DP FUNCTION statement        */
#define  CFUNCTION             0x0133   /* CHAR FUNCTION statement      */

#define  MXFSTAT                   52   /* Max. F77 statement types + 1 */

#define  PROG_STAT             0x0100   /* program statement            */
#define  IMPL_STAT             0x0200   /* implicit statement           */
#define  DECL_STAT             0x0400   /* declaration statement        */
#define  DATA_STAT             0x0800   /* data statement               */
#define  EXEC_STAT             0x1000   /* executable statement         */
#define  END_STAT              0x2000   /* end statement                */

#define  PROG_SEC                   1   /* program section              */
#define  IMPL_SEC                   2   /* implicit section             */
#define  DECL_SEC                   3   /* declaration section          */
#define  DATA_SEC                   4   /* data section                 */
#define  EXEC_SEC                   5   /* executable section           */
#define  END_SEC                    0   /* end of program section       */

typedef  struct {                       /* FORTRAN statements           */
           int                   type;  /* statement type               */
	   char                   *id;  /* statement identifier         */
         } FSTAT;

#endif
