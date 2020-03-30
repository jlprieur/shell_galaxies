/*+++++++++  Remove ESO extensions to FORTRAN 77    ++++++++++++++++++++

.COPYRIGHT   (c) 1987 European Southern Observatory
.LANGUAGE    C
.IDENT       esoext.h
.AUTHOR      Preben J. Grosbol [ESO/IPG]
.KEYWORDS    fortran extensions, ESO fortran
.ENVIRONMENT UNIX
.COMMENT     include file with definitions for esoext.c
.VERSION     1.0    5-Nov-1987: Creation,     PJG
.VERSION     1.1   14-Jan-1988: Add lower case include names,  PJG
.VERSION     1.2   02-Apr-1991: Increase MXLID, JLP 
------------------------------------------------------------------------*/

#define    DO_FLAG     0x0001        /* substitute ENDDO with CONTINUE  */
#define    IMP_FLAG    0x0002        /* remove IMPLICIT NONE statement  */
#define    INC_FLAG    0x0004        /* insert include files            */
#define    LN_FLAG     0x0008        /* remove long names               */
#define    EXC_FLAG    0x0010        /* remove exclamation mark         */
#define    COM_FLAG    0x0020        /* remove comment                  */
#define    SPC_FLAG    0x0040        /* remove spaces                   */
#define    UPC_FLAG    0x0080        /* conversion to upper case        */
#define    VER_FLAG    0x0100        /* verbose flag                    */
#define    LCI_FLAG    0x0200        /* force lower case include name   */

#define    MXSTAT        1321        /* maximum FORTRAN statement       */
#define    MXLBUF          20        /* size of line buffer             */
#define    MXLINE          81        /* max. char. on a line            */
#define    MXLNAME         17        /* max. no. of char. in long name  */
#define    MXIDENT          7        /* max. no. of char. in identifier */
#define    MXID          1000        /* max. no. of identifiers         */
#define    MXLEVEL         16        /* max. level of include file      */
#define    MXFNAME         64        /* max. char. in full file name    */
#define    MXSNO          500        /* max. no. of statement labels    */
#define    MXLID         1000        /* max. no. of ident. in statement */
#define    MXLDO           16        /* max. level of DO - ENDDO        */
#define    DO_LABEL     80000        /* start label for ENDDO statement */

#define    NO_ACTION        0        /* no special action on line       */
#define    RM_ACTION        1        /* remove line                     */
#define    RP_ACTION        2        /* replace line                    */
#define    DO_ACTION        3        /* replace DO statement            */
#define    IN_ACTION        4        /* INCLUDE statement               */

typedef  struct {                    /* cross-reference of identifiers  */
           char     lname[MXLNAME];  /* long name for identifier        */
           char     sname[MXIDENT];  /* standard name for identifier    */
           char               type;  /* identifier type: I,R,D,C,S,X    */
           char              group;  /* identifier group: S,A,F,R,C     */
           int                size;  /* length of original identifier   */
         } ID;

typedef  struct {                    /* identifier in statement         */
           char               *sid;  /* pointer to statement id.        */
           char               *lid;  /* pointer to line id.             */
           int                size;  /* length of identifier            */
           ID                  *id;  /* pointer to identifier structure */
           int                 lno;  /* start line of identifier        */
           int               level;  /* parenthesis level of identifier */
         } LID;
