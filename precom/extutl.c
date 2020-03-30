/*+++++++++  Remove ESO extensions to FORTRAN 77    ++++++++++++++++++++

.COPYRIGHT   (c) 1987 European Southern Observatory
.LANGUAGE    C
.IDENT       fpntr.c
.AUTHOR      Preben J. Grosbol [ESO/IPG]
.KEYWORDS    file pointer, input/output files, include files
.ENVIRONMENT UNIX
.COMMENT
.VERSION     1.0   12-Nov-1987: Creation,     PJG
.VERSION     1.1   14-Jan-1988: Add LC include + labels,   PJG
.VERSION     1.2    8-Feb-1988: correct 'tolower',   PJG
.VERSION     1.3   23-Mar-1988: Redefine 'c' as int,   PJG
.VERSION     1.4   08-Sep-1988: Standard error lists,   PJG
------------------------------------------------------------------------*/

#include   <stdio.h>                        /* standard I/O routines    */
#include   <ctype.h>                        /* get type definitions     */
#include   <esoext.h>                       /* definition of constants  */

extern    int                  x_flag;      /* extension/option flag    */

static    char         xname[MXFNAME];      /* storage for extension    */
static    char         fname[MXFNAME];      /* storage for file name    */
static    char         iname[MXFNAME];      /* storage for include name */
static    char     lstack[MXLEVEL][6];      /* stack for labels         */
static    FILE        *stack[MXLEVEL];      /* file pointer stack       */
static    int              s_pntr = 0;      /* stack pointer            */
static    int              l_pntr = 0;      /* stack pointer            */

FILE *push_fp(fp,path,name)               /* push include file on stack */
FILE    *fp;
char    *path;
char    *name;
{
  char   *pc;
  FILE   *nfp;

  stack[s_pntr++] = fp;                   /* push it on stack            */

  pc = fname;
  if (path) {                             /* path of include file given  */
    while (*pc = *path++) pc++;    /* copy path over     */
    *pc++ = '/';
  }
  while (*pc++ = *name++);    /* copy name of include file   */
  nfp = fopen(fname,"r");                 /* open include file           */
  if (!nfp) {                             /* cannot open include file    */
    fprintf(stderr,"Error: Cannot open include file >%s<\n",fname);
    nfp = stack[--s_pntr];
  }
  if (MXLEVEL<=s_pntr) {                  /* stack full - error          */
    fprintf(stderr,"Error: Include stack full\n"); exit(1);
  }
  return nfp;
}

FILE *pop_fp(fp)                          /* pop include file off stack  */
FILE        *fp;
{
  FILE   *nfp;

  fclose(fp);                             /* close active file           */
  if (0<s_pntr) {                         /* files on the stack          */
    nfp = (s_pntr) ? stack[--s_pntr] : (FILE *) 0;
  }
  else {                                  /* no files on stack           */
    s_pntr = 0; nfp = (FILE *) 0;
  }
  return nfp;
}

char *push_lab(label)          /* push a label string on the stack       */
char     *label;
{
  char   *pc;

  pc = &(lstack[l_pntr++][0]);
  if (label) while ((*pc++ = *label++));
  *pc = '\0';

  if (MXLEVEL<=l_pntr) {                  /* stack full - error          */
    fprintf(stderr,"Error: Label stack full\n"); exit(1);
  }
  return (char *) 0;
}

char *pop_lab()                /* pop label string off the stack         */
{
  char   *plab;

  if (0<l_pntr) {                         /* labels on the stack         */
    plab = (l_pntr) ? &(lstack[--l_pntr][0]) : (char *) 0;
  }
  else {                                  /* no labels on stack          */
    l_pntr = 0; plab = (char *) 0;
  }
  return plab;
}

char *new_ext(name,ext)        /* generate file name with new extension */
char   *name;
char   *ext;
{
  char   *pc,*pcx;

  pc = xname; pcx = (char *) 0;
  while (*pc = *name++)  /* search for last '.' in file name */
    if (*pc++ == '.') pcx = pc;
  if (!pcx) *pc++ = '.';            /* if no '.' add it to end of name  */
    else pc = pcx;
  while ((*pc++ = *ext++));
  if (MXFNAME<=strlen(xname)) {     /* file name was too long - error   */
    fprintf(stderr,"Error: File name >%s< too long\n",xname); exit(1);
  }

  return xname;
}

char *new_file()               /* get new file name from standard input */
{
  char *pc;
  int  n,c;

  n = 0; pc = fname;
  while ((c=getc(stdin)) != EOF && !isspace(c))   /* read new file name */
    if (n++<MXFNAME) *pc++ = c;
  while (isspace(c)) c = getc(stdin);             /* skip through space */
  ungetc(c,stdin);                        /* put first no-space back    */

  if (MXFNAME<=n) {                       /* file name too long - error */
    fprintf(stderr,"Error: File name >%s< too long - skipped!\n",fname);
    exit(1);
  }
  *pc = '\0';

  return (c==EOF && !n) ? (char *) 0 : fname;
}

char *incl_file(line)                 /* extract include file from line */
char      *line;
{
  char   *pcl,c; 
  int    n;

  pcl = (char *) 0;
  while ((c = *line++) != '\'' && c)       /* find first ' in line      */
    if (c == '\'') pcl = line;
  if (!c) 
    { fprintf(stderr,"Error: Include statement error\n"); 
    return((char *) 0); }

  n = 0;
  while ((c = *(line + n)) != ':' && c)  n++;    /* check if ':' in line   */
     n++;
  if(c == ':') line = line + n;

  n = 0;                                   /* copy include file name    */ 
  while ((c = *line++) != '\'' && c != '/' && c)
    if (n<MXFNAME) iname[n++] = c;
  iname[n] = '\0';

  if (x_flag & LCI_FLAG) {                 /* convert to lower case     */
     pcl = iname;
     while (*pcl) {
       if (isupper((int)*pcl)) *pcl = tolower((int)*pcl); pcl++;
     }
  }

  return (c) ? iname : (char *) 0;
}
