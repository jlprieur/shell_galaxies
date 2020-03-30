/*+++++++++  Remove ESO extensions to FORTRAN 77    ++++++++++++++++++++

.COPYRIGHT   (c) 1987 European Southern Observatory
.LANGUAGE    C
.IDENT       lidtbl.c
.AUTHOR      Preben J. Grosbol [ESO/IPG]
.KEYWORDS    identifier table
.ENVIRONMENT UNIX
.COMMENT
.VERSION     1.0    2-Nov-1987: Creation,     PJG
.VERSION     1.1   23-Mar-1988: Cast arg. of isdigit etc as int,     PJG
------------------------------------------------------------------------*/

#include   <stdio.h>                        /* standard I/O functions   */
#include   <ctype.h>                        /* character types          */
#include   <esoext.h>                       /* definition of constants  */
#include   <f77stat.h>                      /* f77 statements           */

extern    int                  no_lid;      /* no. of line identifiers  */
extern    LID                   lid[];      /* list of line identifiers */

static    char    *list[] =                 /* logical exp. and const.  */
                   {"EQ","NE","AND","OR","NOT","LT","LE","GT","GE",
                   "EQV","NEQV","TRUE","FALSE",(char *)0 };

chk_io(plid)                      /* check identifiers in I/O statement */
LID   *plid;
{
  int   n;
  char  *pc;

  for (n=0; n<no_lid; n++, plid++) {      /* go through all identiifers */
    if (!plid->size || !plid->level) continue;
    pc = plid->sid + plid->size + 1;
    if (*pc == '=') plid->size = 0;
  }
  return;
}

chk_exp(plid)                     /* check identifiers in expression   */
LID    *plid;
{
  int   n,i;
  char  *pc,*pl;

  plid++;                                 /* skip first identifier     */
  for (n=1; n<no_lid; n++, plid++) {      /* check all the rest        */
    if (!plid->size) continue;
    pc = plid->sid;
    if (*(--pc) == '.') {                 /* maybe logical or exponent */
      pl = list[0]; i = 1;
      do {                                /* compare with logicals     */
	pc = plid->sid;
	while (*pc == *pl) { pc++; pl++; }
	if (!(*pl) && *pc == '.') {       /* it is a logical constant  */
	  plid->size = 0; break;
        }
      } while (pl = list[i++]);
      pc = plid->sid;
      if (*pc == 'E' || *pc == 'D') {     /* test if exponent          */
	pc -= 2; if (isdigit((int)*pc)) plid->size = 0;
      }
    }
    else if (isdigit((int)*pc)) plid->size = 0;
  }

  return;
}
