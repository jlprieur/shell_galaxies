/*+++++++++  Remove ESO extensions to FORTRAN 77    ++++++++++++++++++++

.COPYRIGHT   (c) 1987 European Southern Observatory
.LANGUAGE    C
.IDENT       f77utl.c
.AUTHOR      Preben J. Grosbol [ESO/IPG]
.KEYWORDS    fortran statements, program sections
.ENVIRONMENT UNIX
.COMMENT
.VERSION     1.0   12-Nov-1987: Creation,     PJG
.VERSION     1.1   15-Jan-1988: Only warning on structure error,   PJG
------------------------------------------------------------------------*/

#include   <stdio.h>                        /* Standard I/O library     */
#include   <f77stat.h>                      /* FORTRAN statement types  */

int f77_sect(int *ptype, int sect)
{
  int    section,err;

  err = 0;
  section = sect;
  switch (sect) {                           /* update and check section */
    case PROG_SEC : if (*ptype & PROG_STAT) break;
                    if (*ptype & (IMPL_STAT | DECL_STAT | DATA_STAT | 
                      EXEC_STAT | END_STAT)) section = IMPL_SEC;
                     else { err = 1; break; }
    case IMPL_SEC : if (*ptype & IMPL_STAT) break;
                    if (*ptype & (DECL_STAT | DATA_STAT | EXEC_STAT | 
                      END_STAT)) section = DECL_SEC;
                     else { err = 1; break; }
    case DECL_SEC : if (*ptype & DECL_STAT) break;
                    if (*ptype & (DATA_STAT | EXEC_STAT | 
                      END_STAT)) section = DATA_SEC;
                     else { err = 1; break; }
    case DATA_SEC : if (*ptype & DATA_STAT) break;
                    if (*ptype & (EXEC_STAT | END_STAT)) section = EXEC_SEC;
                     else { err = 1; break; }
    case EXEC_SEC : if (*ptype & EXEC_STAT) break;
                    if (*ptype & END_STAT) section = END_SEC;
                     else { err = 1; break; }
    case END_SEC  : if (*ptype & END_STAT) break;
/* JLP 91: modif to allow include statement after END: */
		    if (*ptype & INCLUDE) {section = DATA_SEC; break;}
		    if (*ptype & PROG_STAT && *ptype != EXEC_SEC)
                      section = PROG_SEC;
                     else { err = 1; break; }
  }
/* JLP91
  if (err) {
    fprintf(stderr,"Warning: statement type %x in wrong section %d\n",
            *ptype,section);
  }
*/
  return section;
}

/* compare with f77 statements */
char *find_f77(FSTAT *list, char *id, int *pno, int *ptype)        
{
  char     c,*pid,*pstat;

  while (pstat=list->id) {             /* loop though statement list  */
    pid = id; *pno = 0;
    while ((c = *pstat++) == *pid++ && c != '\0') (*pno)++;
    if (!c) break;
    list++;
  }

  *ptype = (!c) ? list->type : EXEC_STAT;

  return --pid;
}
