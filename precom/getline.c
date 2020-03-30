/*+++++++++  Remove ESO extensions to FORTRAN 77    ++++++++++++++++++++

.COPYRIGHT   (c) 1987 European Southern Observatory
.LANGUAGE    C
.IDENT       getline.c
.AUTHOR      Preben J. Grosbol [ESO/IPG]
.KEYWORDS    ESO fortran, line input
.ENVIRONMENT UNIX
.COMMENT
.VERSION     1.0   10-Nov-1987: Creation,     PJG
.VERSION     1.1   14-Jan-1988: Replace tabs with space, PJG
.VERSION     1.2   15-Feb-1988: Replace form-feed with space, PJG
.VERSION     1.3   23-Mar-1988: Redefine 'c' as int, PJG
.VERSION     1.4   20-Dec-1988: Check for max. line ident., PJG
------------------------------------------------------------------------*/

#include   <stdio.h>                        /* standard I/O functions   */
#include   <ctype.h>                        /* character types          */
#include   <esoext.h>                       /* definition of constants  */

extern    int                  x_flag;      /* extension option flag    */
extern    int                   equal;      /* level zero equal sign    */
extern    int                   comma;      /* level zero comma         */
extern    int                     nlb;      /* present index in 'lbuf'  */
extern    int                 id_size;      /* length of identifier     */
extern    int                  no_lid;      /* no. of line identifiers  */
extern    int                   nstat;      /* char. index in 'stat'    */
extern    char                 stmt[];      /* present statement        */
extern    char   lbuf[MXLBUF][MXLINE];      /* buffer for input lines   */
extern    LID                   lid[];      /* list of line identifiers */

static    int                 q_level;      /* quote level in statement */
static    int                 p_level;      /* parenthesis level        */
static    char                 *pstat;      /* pointer to statement     */

char get_line(fp)                           /* add character to buffer  */
FILE         *fp;
{
  int          c,nlbuf,e_mark;
  char         *plbuf;

  if (!nstat) {                        /* reset at start of statement   */
    q_level = 0; p_level = 0;
    pstat = stmt; id_size = 0;
  }
/* JLP : debug
  printf("GET_LINE: statement: >%s< \n",pstat);
*/

  nlbuf = 0; e_mark = 0;
  plbuf = &lbuf[nlb][0];

  while ((c=getc(fp)) != '\n' && c != EOF) {  /* read rest of line      */
    if (c=='\t' || c=='\f') c = ' ';
    if (MXLINE<=nlbuf) continue;
    if (e_mark) {
/* If exclamation mark: */
      if (!(x_flag & EXC_FLAG)) { *plbuf++ = c; nlbuf++; }
      continue;
    }
    if (c=='\'') q_level = !q_level;
    if (c=='!' && !q_level) {
      if (!(x_flag & EXC_FLAG)) { *plbuf++ = c; nlbuf++; }
      e_mark = 1;
      continue;
    }
    if (c==',' && !p_level && !q_level) comma = 1;
    if (c=='=' && !p_level && !q_level) equal = 1;
    /*
    if (x_flag & UPC_FLAG) printf(" UPC_FLAG (upper case)is on");
    */
    if (x_flag & UPC_FLAG) {c = (islower(c)) ? toupper(c) : c;}

    if (id_size && c!=' ') {                  /* check for identifiers */
      if (isalpha(c) || isdigit(c) || c=='_') /* continue identifier   */
        id_size++;
      else {                                  /* end of identifier     */
        lid[no_lid].size = id_size;
        lid[no_lid].level = p_level;
        lid[no_lid].id = (ID *) 0;
        id_size = 0;
        if (MXLID<=++no_lid)
           {
           fprintf(stderr,"GET_LINE/error: statement: >%s< \n",pstat);
           fprintf(stderr,"Error: Too many identifiers in statement\n");
           }
      }
    }
    else if (!q_level && isalpha(c)) {        /* start of identifier   */
      lid[no_lid].sid = pstat;
      lid[no_lid].lid = plbuf;
      lid[no_lid].lno = nlb;
      id_size++;
    }
    if (c=='(') p_level++;
    if (c==')') p_level--;
    *plbuf++ = c; nlbuf++;
    if (c!=' ' || q_level) { *pstat++ = c; nstat++; }
  }
  if (id_size)  {                             /* end of identifier     */
        lid[no_lid].size = id_size;
        lid[no_lid].level = p_level;
        lid[no_lid].id = (ID *) 0;
        id_size = 0;
        if (MXLID<=++no_lid)
           {
           fprintf(stderr,"GET_LINE/error: statement: >%s< \n",pstat);
           fprintf(stderr,"Error: Too many identifiers in statement\n");
           }
  }
  *plbuf = '\0';
  return c;
}
