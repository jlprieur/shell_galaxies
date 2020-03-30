/*+++++++++  Remove ESO extensions to FORTRAN 77    ++++++++++++++++++++

.COPYRIGHT   (c) 1987 European Southern Observatory
.LANGUAGE    C
.IDENT       putline.c
.AUTHORS     Preben J. Grosbol [ESO/IPG], JLP
.KEYWORDS    ESO fortran, output line
.ENVIRONMENT UNIX
.COMMENT
.VERSION     1.0   10-Nov-1987: Creation,     PJG
.VERSION     1.1   22-Mar-1988: Correct ENDDO,     PJG
.VERSION     1.2   23-Mar-1988: Redefine 'c' as int,     PJG
.VERSION     1.3   08-Sep-1988: Standard error lists,     PJG
------------------------------------------------------------------------*/
/*
#define DEBUG
*/

#include   <stdio.h>                        /* standard I/O functions   */
#include   <ctype.h>                        /* character types          */
#include   <esoext.h>                       /* definition of constants  */
#include   <f77stat.h>                      /* F77 statements           */

extern    int                  x_flag;      /* extension option flag    */
extern    int                     nlb;      /* present index in 'lbuf'  */
extern    int                  no_lid;      /* no. of line identifiers  */
extern    int                     sno;      /* current no. of labels    */
extern    int                statno[];      /* statement label no.      */
extern    char                 stmt[];      /* present statement        */
extern    char   lbuf[MXLBUF][MXLINE];      /* buffer for input lines   */
extern    LID                   lid[];      /* list of line identifiers */
extern    int                do_level;      /* DO stack pointer         */
extern    int                do_label;      /* DO label                 */

static    int            dostk[MXLDO];      /* DO label stack           */

/************************************************************************/
/*** Remember: LID structure is
typedef  struct {                    * identifier in statement    
           char               *sid;  * pointer to statement id.    
           char               *lid;  * pointer to line id.          
           int                size;  * length of identifier          
           ID                  *id;  * pointer to identifier structure
           int                 lno;  * start line of identifier        
           int               level;  * parenthesis level of identifier
         } LID;
***/
/************************************************************************
* put_line:
* To correct and output line directly to the output file.
/************************************************************************/
int put_line(FILE *ofp, int action, int stype, int labno)
/*
FILE         *ofp;                         // Output file pointer
int          action;                       // action to perform
int          stype;                        // statement type
int          labno;                        // label number 
*/
{
  int          c,n,i,newlab;
  char         *pc,*pnid;

  if (stype == INCLUDE && x_flag & INC_FLAG) return(0);

/* With option -n in command line: removes "implicit none" statement: 
*/  
  if (stype == IMPLICITNONE && (x_flag & IMP_FLAG)) 
    {
/*
    printf("JLP2002: current statement=%s\n", stmt);
    printf("JLP2002: implicit none inhibited: give up\n");
*/
    return(0);
    }

  if ((x_flag & DO_FLAG) && action == DO_ACTION) { /* remove DO - ENDDO */
    if (stype == DO) {
      newlab = do_label++;
      dostk[do_level++] = newlab;
      if (MXLDO<do_level) 
	fprintf(stderr,"Error: Stack error in DO-ENDDO (unclosed DO loop...)");
      statno[sno++] = -newlab;
/* Add "do label" */
      if (labno) fprintf(ofp,"%5d DO%5d",labno,newlab);
	else fprintf(ofp,"      DO%5d",newlab);
/* Copy remaining characters of the line */
      for (n=0; !lid[n].size && n<no_lid; n++);
/* ? why this before : pc = lid[n].sid ?? */
/* JLP 92 : */
      pc = lid[n].lid + 2;
#ifdef DEBUG
      printf(" PUTLINE/DO processing: lid[n].sid = >%s< n=%d \n",pc,n);
      printf("    lid[n].lid = >%s< lid.lno=%d \n",lid[n].lid,lid[n].lno);
      printf("    lid[n].size=%d \n",lid[n].size);
      printf(" newlab = %d pc = >%s< no_lid = %d\n",newlab,pc,no_lid);
#endif

/* Write out one statement  */
      n = 7;
      while (c = *pc++)                        
       {
        if(c != 32)
        {
         if (n++<66) 
            putc(c,ofp);
         else 
/* Add continuation line if more than 66 characters */
          { n = 1; fprintf(ofp,"\n     +%c",c); }
         }
       }
      putc('\n',ofp);
    }
    else if (stype == ENDDO) {
      if (do_level<0)
        fprintf(stderr,"Error: Stack error in DO-ENDDO (unclosed DO loop...)");
      if (labno) fprintf(ofp,"%5d CONTINUE\n",labno);
      fprintf(ofp,"%5d CONTINUE\n",dostk[--do_level]);
    }
    return(-1);
  }

  if (labno)                                /* write label if any       */
    fprintf(ofp,"%5d ",labno);
   else fprintf(ofp,"      ");

  n = 0; pc = stmt;
  while (c = *pc++) {                       /* write out one statement  */
    if (n++<66) putc(c,ofp);
      else { n = 1; fprintf(ofp,"\n     +%c",c); }
  }
  putc('\n',ofp);

  return(0);
}
