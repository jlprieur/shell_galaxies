/*+++++++++  Remove ESO extensions to FORTRAN 77    ++++++++++++++++++++

.COPYRIGHT   (c) 1987 European Southern Observatory
.LANGUAGE    C
.IDENT       linetype.c
.AUTHOR      Preben J. Grosbol [ESO/IPG]
.KEYWORDS    fortran, statement type
.ENVIRONMENT UNIX
.COMMENT
.VERSION     1.0   12-Nov-1987: Creation,     PJG
.VERSION     1.1    1-Feb-1988: insert typed functions + correct IF,  PJG
.VERSION     1.2   23-Mar-1988: Redefine 'c' as int,  PJG
.VERSION     1.3   24-Mar-1988: Initiate 'action and check END,  PJG
.VERSION     1.4   08-Sep-1988: Standard error lists,  PJG
.VERSION     1.5   02-Mar-1990: Add SAVE statement to list,  PJG
------------------------------------------------------------------------*/

#include   <stdio.h>                        /* standard I/O functions   */
#include   <ctype.h>                        /* character types          */
#include   <string.h>                       /* string functions         */
#include   <f77stat.h>                      /* FORTRAN statement types  */
#include   <esoext1.h>                       /* definition of constants  */

static    FSTAT   fs_none[] = {             /* f77 stat. without , or = */
                  { IF,               "IF"},
                  { ELSEIF,           "ELSEIF"},
                  { ELSE,             "ELSE"},
                  { CALL,             "CALL"},
                  { CONTINUE,         "CONTINUE"},
                  { GOTO,             "GOTO"},
                  { CLOSE,            "CLOSE"},
                  { ENDIF,            "ENDIF"},
                  { ENDDO,            "ENDDO"},
                  { END,              "END"},
                  { WRITE,            "WRITE"},
                  { RFUNCTION,        "REALFUNCTION"},
                  { IFUNCTION,        "INTEGERFUNCTION"},
                  { DFUNCTION,        "DOUBLEPRECISIONFUNCTION"},
                  { CFUNCTION,        "CHARACTERFUNCTION"},
                  { LFUNCTION,        "LOGICALFUNCTION"},
                  { REAL,             "REAL"},
                  { INTEGER,          "INTEGER"},
                  { DOUBLEPRECISION,  "DOUBLEPRECISION"},
                  { CHARACTER,        "CHARACTER"},
                  { LOGICAL,          "LOGICAL"},
                  { RETURN,           "RETURN"},
                  { ASSIGN,           "ASSIGN"},
                  { OPEN,             "OPEN"},
                  { PARAMETER,        "PARAMETER"},
                  { INCLUDE,          "INCLUDE"},
                  { STOP,             "STOP"},
                  { SUBROUTINE,       "SUBROUTINE"},
                  { FORMAT,           "FORMAT"},
                  { FUNCTION,         "FUNCTION"},
                  { READ,             "READ"},
                  { COMMON,           "COMMON"},
                  { COMPLEX,          "COMPLEX"},
                  { DATA,             "DATA"},
                  { DIMENSION,        "DIMENSION"},
                  { EQUIVALENCE,      "EQUIVALENCE"},
                  { IMPLICITNONE,     "IMPLICITNONE"},
                  { IMPLICIT,         "IMPLICIT"},
                  { PROGRAM,          "PROGRAM"},
                  { EXTERNAL,         "EXTERNAL"},
                  { INTRINSIC,        "INTRINSIC"},
                  { REWIND,           "REWIND"},
                  { SAVE,             "SAVE"},
                  { BACKSPACE,        "BACKSPACE"},
                  { BLOCKDATA,        "BLOCKDATA"},
                  { ENDFILE,          "ENDFILE"},
                  { ENTRY,            "ENTRY"},
                  { INQUIRE,          "INQUIRE"},
                  { PAUSE,            "PAUSE"},
                  { 0,                (char *) 0}};

static    FSTAT   fs_c[] = {                /* f77 stat. with , only    */
                  { IF,               "IF"},
                  { WRITE,            "WRITE"},
                  { REAL,             "REAL"},
                  { INTEGER,          "INTEGER"},
                  { DOUBLEPRECISION,  "DOUBLEPRECISION"},
                  { CHARACTER,        "CHARACTER"},
                  { LOGICAL,          "LOGICAL"},
                  { COMMON,           "COMMON"},
                  { COMPLEX,          "COMPLEX"},
                  { DATA,             "DATA"},
                  { DIMENSION,        "DIMENSION"},
                  { EQUIVALENCE,      "EQUIVALENCE"},
                  { EXTERNAL,         "EXTERNAL"},
                  { IMPLICIT,         "IMPLICIT"},
                  { INTRINSIC,        "INTRINSIC"},
                  { READ,             "READ"},
                  { SAVE,             "SAVE"},
                  { 0,                (char *) 0}};

static    FSTAT   fs_e[] = {                /* f77 stat. with = only    */
                  { IF,               "IF"},
                  { 0,                (char *) 0}};

static    FSTAT   fs_ce[] = {               /* f77 stat. with , and =   */
                  { DO,               "DO"},
                  { 0,                (char *) 0}};

static    FSTAT   fs_if[] = {               /* f77 stat. in IF stat.    */
                  { READ,             "READ"},
                  { IF,               "IF"},
                  { CALL,             "CALL"},
                  { CONTINUE,         "CONTINUE"},
                  { GOTO,             "GOTO"},
                  { CLOSE,            "CLOSE"},
                  { RETURN,           "RETURN"},
                  { ASSIGN,           "ASSIGN"},
                  { OPEN,             "OPEN"},
                  { STOP,             "STOP"},
                  { READ,             "READ"},
                  { WRITE,            "WRITE"},
                  { REWIND,           "REWIND"},
                  { BACKSPACE,        "BACKSPACE"},
                  { ENDFILE,          "ENDFILE"},
                  { INQUIRE,          "INQUIRE"},
                  { PAUSE,            "PAUSE"},
                  { 0,                (char *) 0}};

int line_type(int *ptype)
{
  int    no,action,n,err,f77_sect();
  char   *pc,type,group,*find_f77();
  ID     *pid;
  LID    *plid;
/* JLP 91 */
  int i;
  char   stmt_upper[MXSTAT];

  action = NO_ACTION;                        /* initiate action to none  */

/* Convert systematically to upper case in order to find the 
   statement type: */
   strcpy(stmt_upper,stmt);
      for(i=0; i<strlen(stmt_upper); i++)
      {
      if (islower((int)stmt_upper[i])) 
	 stmt_upper[i] = toupper((int)stmt_upper[i]);
      }

/* Sort the statement (whole line) according to the presence 
  of comma(,) and equal(=) signs */
  if (!comma && !equal) pc = find_f77(fs_none,stmt_upper,&no,ptype);
  if (comma && !equal) pc = find_f77(fs_c,stmt_upper,&no,ptype);
  if (!comma && equal) pc = find_f77(fs_e,stmt_upper,&no,ptype);
  if (comma && equal) pc = find_f77(fs_ce,stmt_upper,&no,ptype);

  section = f77_sect(ptype,section);        /* update and check section */

  plid = lid;
  if (*ptype != EXEC_STAT) {                /* modify identifier        */
    plid->sid = pc; plid->size -= no;
  }

  if (*ptype == IF || *ptype == ELSEIF) {    /* check conditioned stat. */
    plid++; n = 1;
    while (plid->level && n<no_lid) { plid++; n++; }
    if (n<no_lid) {                          /* if more ident. on line  */
       if (!strncmp(plid->sid,"THEN",4)) plid->size = 0;
        else {
          pc = find_f77(fs_if,plid->sid,&no,ptype);
          if (*ptype != EXEC_STAT) {
             plid->sid = pc; plid->size -=no;
          }
       }
    }
    chk_exp(lid);
  }

  plid = lid;
  switch (*ptype) {                        /* special things            */
    case EXEC_STAT    : chk_exp(lid); break;
    case FORMAT       : if (!equal) { no_lid = 0; return NO_ACTION; } 
                        break;
    case ENDDO        : no_lid = 0; return DO_ACTION;
    case DO           : if (isdigit((int)*pc)) { /* f77 DO statement    */
                          no = 1;
                          while (isdigit((int)*(++pc))) no++;
                          if (isalpha((int)*pc)) {
                            plid->sid = pc; plid->size -= no; }
                          else plid->size = 0;
                          action = NO_ACTION;
                        }
                        else action = DO_ACTION;
                        break;
    case GOTO         : if (isdigit((int)*plid->sid)) {
			  plid->size = 0; action = NO_ACTION;
                        }
                        break;
    case IMPLICIT     : no_lid = 0; return NO_ACTION;
/* With option -n in command line: removes "implicit none" statement: 
*/
    case IMPLICITNONE : no_lid = 0; return RM_ACTION;
    case INCLUDE      : no_lid = 0; return IN_ACTION;
    case STOP         : no_lid = 0; return NO_ACTION;
    case END          : no_lid = 0; return NO_ACTION;
    case PAUSE        : no_lid = 0; return NO_ACTION;
    case ASSIGN       : no = 0; pc = plid->sid;
                        while (isdigit((int)*(plid->sid++))) no++;
                        plid->sid += 2; plid->size -= no + 2;
                        break;
    case READ         :
    case WRITE        :
    case PRINT        :
    case OPEN         :
    case CLOSE        :
    case ENDFILE      :
    case REWIND       :
    case BACKSPACE    :
    case INQUIRE      : chk_io(lid); break;
  }

  type = '?'; group = 'V';
  if (*ptype == CALL) { type = 'X'; group = 'S'; }
  if (*ptype & PROG_STAT)                   /* check program block      */
    switch (*ptype) {
	   case SUBROUTINE : group = 'S'; break;
	   case FUNCTION   : group = 'F'; break;
	   case IFUNCTION  : type = 'I'; group = 'F'; break;
	   case RFUNCTION  : type = 'R'; group = 'F'; break;
	   case DFUNCTION  : type = 'D'; group = 'F'; break;
	   case LFUNCTION  : type = 'L'; group = 'F'; break;
	   case CFUNCTION  : type = 'C'; group = 'F'; break;
	   case PROGRAM    : group = 'P'; break;
	   case BLOCKDATA  : group = 'D'; break;
    }

  if (*ptype & DECL_STAT)                   /* check type decleration   */
    switch (*ptype) {
      case REAL            : type = 'R'; break;
      case INTEGER         : type = 'I'; break;
      case DOUBLEPRECISION : type = 'D'; break;
      case CHARACTER       : type = 'S'; break;
      case LOGICAL         : type = 'L'; break;
      case EXTERNAL        : group = 'F'; break;
      case INTRINSIC       : group = 'F'; break;
      case PARAMETER       : type = 'P'; group = 'P'; break;
      case COMMON          : group = 'C'; break;
    }

  plid = lid;

  return action;
}
