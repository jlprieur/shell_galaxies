/*+++++++++  Remove ESO extensions to FORTRAN 77    ++++++++++++++++++++

.COPYRIGHT   (c) 1987 European Southern Observatory
.LANGUAGE    C
.IDENT       From esoext.c
.AUTHORS     Preben J. Grosbol [ESO/IPG], JLP
.KEYWORDS    fortran extensions, ESO fortran
.ENVIRONMENT UNIX
.COMMENT
.VERSION     1.0   12-Nov-1987: Creation,     PJG
.VERSION     1.1   15-Jan-1988: Correct include file, PJG
.VERSION     1.2   15-Feb-1988: Remove '\f', PJG
.VERSION     1.3   10-Mar-1988: Remove file name print, PJG
.VERSION     1.4   23-Mar-1988: Redefine 'c' as int, PJG
.VERSION     1.5   22-Apr-1988: Insert exit in end, PJG
.VERSION     1.6   08-Sep-1988: Change default prints, PJG
.VERSION     1.7   06-Dec-1988: Include '*' comments, PJG
.VERSION     JLP   01-02-91   Allow Include files even at the last line
	           and does not shorten long names of subroutines
                   (Also changed COM_FLAG in its opposite...)
                   "implicit none" statements are now allowed (default option) 
------------------------------------------------------------------------*/

#include   <stdio.h>                        /* standard I/O functions   */
#include   <stdlib.h>                       /* with void exit(int status) */
#include   <ctype.h>                        /* C type definitions       */
#include   <esoext.h>                       /* definition of constants  */
#include   <f77stat.h>                      /* FORTRAN statement types  */

int                    section;             /* program section          */
int                      equal;             /* level zero equal sign    */
int                      comma;             /* level zero comma         */
int                        lno;             /* current line number      */
int                        sno;             /* no. of statement labels  */
int                        nlb;             /* present index in 'lbuf'  */
int                      nstat;             /* char. index in statement */
int                     x_flag;             /* extension flags          */
int                     f_flag;             /* file name flag           */
int                     no_lid;             /* no. of line identifiers  */
int                    id_size;             /* length of identifier     */
int                   do_level;             /* DO stack pointer         */
int                   do_label;             /* DO label                 */
ID                 idtbl[MXID];             /* list of identifiers      */
LID                 lid[MXLID];             /* list of line identifiers */
int              statno[MXSNO];             /* statement numbers        */
char              stmt[MXSTAT];        /* present statement (whole line)*/
char      lbuf[MXLBUF][MXLINE];             /* buffer for input lines   */
char                  u_text[] =            /* usage text               */
         "usage: esoext [-csdnilxuv] [-f file] [-I path]\n";

int main(int argc, char *argv[])
{
  int     c,ns,n,i,stype,action,labno,hstat[50];
  int     put_line(),line_type();
  char    *f_name,*o_name,*new_ext();
  char    *incl_path,*incl_name,*incl_file();
  char    *p,cont,*plab,label[6],get_line();
  char    *push_lab(),*pop_lab(),*new_file();
  FILE    *fp,*ofp,*push_fp(),*pop_fp();

  idtbl[0].lname[0] = '\0';
  for (n=0; n<50; n++) hstat[n] = 0;

  f_flag = 0; x_flag = 0; fp = (FILE *) 0;
  f_name = (char *) 0; incl_path = (char *) 0;

  if(argc == 1) {
   printf("%s\n", u_text);
   return(-1);
   }

  argv++;
  if (--argc != 0)                 /* decode command line parameters         */
    while (argc--) {
       p = *argv++;
       if (*p++ == '-')
         switch (*p) {
                case 'c' :                        /* leave comments    */
                case 'd' :                        /* ENDDO extension   */
                case 'n' :                        /* IMPLICIT NONE     */
                case 'i' :                        /* INCLUDE extension */
                case 'l' :                        /* long names        */
                case 'x' :                        /* exclamation mark  */
                case 'u' :                        /* to UPPER case     */
		case 'L' :                        /* include to LOWER  */
                case 'v' :                        /* verbose option    */
                           while (*p)
                             switch (*p++) {
                                  case 'c' : x_flag |= COM_FLAG; break;
                                  case 'd' : x_flag |= DO_FLAG; break;
/* With option -n in command line: removes "implicit none" statements */  
                                  case 'n' : x_flag |= IMP_FLAG; break;
                                  case 'i' : x_flag |= INC_FLAG; break;
                                  case 'x' : x_flag |= EXC_FLAG; break;
                                  case 'u' : x_flag |= UPC_FLAG; break;
                                  case 'L' : x_flag |= LCI_FLAG; break;
                                  case 'v' : x_flag |= VER_FLAG; break;
                                  default  :
                                             printf("%s",u_text);
                                             exit(1);
                             }
                           break;
                case 'f' :                /* name of file to convert */
                           if (!argc--) { printf("%s",u_text); exit(1); }
                           f_name = *argv++; f_flag = 1;
                           break;
                case 'I' :                /* path name of includes   */
                           if (!argc--) { printf("%s",u_text); exit(1); }
                           incl_path = *argv++;
                           break;
                default  :
                           printf("%s",u_text);
                           exit(1);
         }
    }
  if (!x_flag)                                   /* define default flags  */
    x_flag = DO_FLAG | EXC_FLAG |
             INC_FLAG | LCI_FLAG;

  if (x_flag==VER_FLAG)                         /* define default flags  */
    x_flag = DO_FLAG | EXC_FLAG |
             INC_FLAG | LCI_FLAG | VER_FLAG;

  if (x_flag & VER_FLAG) printf("Option flag: %x\n",x_flag);

  if (!f_flag) f_name = new_file();

/* loop through one file       */
  do {                                    
    if (!fp) fp = fopen(f_name,"r");      /* open input file to process  */
    if (!fp) 
      { 
      fprintf(stderr,"Error: Opening input file >%s<\n",f_name);
      exit(1);
      }
/* Output file: */
    o_name = new_ext(f_name,"f");
    ofp = fopen(o_name,"w");
    if (!ofp) 
      { 
      fprintf(stderr,"Error: Opening output file >%s<\n",o_name);
      exit(1);
      }
// JLP 2024: output even when -v is not selected
//    if (x_flag & VER_FLAG)
        printf("Input file: >%s<, Output: >%s<\n",f_name,o_name);

/* Initializing the parameters for this file: */
    nstat = 0; nlb = 0; c = ' ';
    equal = 0; comma = 0; section = PROG_SEC;
    lno = 0; labno = 0; plab = (char *) 0;
    no_lid = 0; id_size = 0;
    sno = 0; do_level = 0; do_label = DO_LABEL;

/* loop through each line      */
   while (1) 
   {                           

/*******************************************************************/
/* JLP 91 to allow include files at the last line: */
     if (c==EOF )
     {
/*
End up with last fortran statement (whole line): 
*/
       if (nstat) {
          stmt[nstat] = '\0';
/* Get correct value for statement type (stype) :*/
          action = line_type(&stype);
          if (stype == INCLUDE && (x_flag & INC_FLAG)) 
	  {
            incl_name = incl_file(stmt);
            printf("Line %d, including: path >%s<, name >%s<\n",
		    lno,incl_path,incl_name);
	    if(incl_name == NULL ) 
	      {printf(" Fatal error/include file \n"); exit(-1);}

	    fp = push_fp(fp,incl_path,incl_name);
            plab = push_lab(plab);
            hstat[stype & 0xFF] += 1;
            nstat = 0; nlb = 0; equal = 0; comma = 0; no_lid = 0;
            labno = 0;
	    /*
            c = get_line(fp);
	    */
            c = ' ';
            continue;
          }
	  else
/* If not include file statement, process curent line: */
	  {
            put_line(ofp,action,stype,labno);
            hstat[stype & 0xFF] += 1;
            nstat = 0; nlb = 0; equal = 0; comma = 0; no_lid = 0;
          }
       }

         cont = '\0';                     /* no cont. line across files  */
         if (!(fp=pop_fp(fp))) 
	  {
	   break;                         /* if no include file          */
	  }
         plab = pop_lab();                /* get old label if include    */
    }
/* End of JLP 91's inclusion to allow include files at the last line. */
/*******************************************************************/

/* Loop on the lines (while...): increment line number */
     lno++;

/*******************************************************************/
/* First check for comment line or '\f'   */
/*******************************************************************/
     if (!plab && ((c=getc(fp))=='C' || c=='*' ||
           c=='c' || c=='\f'))
     {
/* If COM_FLAG, then leave comments: */
        if (x_flag & COM_FLAG)
        {
/* Copy back the commented line without alteration: */
          putc('C',ofp);
	  n = 0;
          while ((c=getc(fp)) != '\n' && c != EOF)
	    if (++n < MXLINE) putc(c,ofp);
          if (c == '\n') putc('\n',ofp);
        }
/* Else remove the comments: */
        else {
          while ((c=getc(fp)) != '\n' && c != EOF);
	}
        continue;
     }

/*******************************************************************/
/* Then check duplicate labels in program section: */
/*******************************************************************/
     if (section == PROG_SEC)
     {
        for (n=0; n<sno; n++)           /* check duplicate labels      */
	  {
          if (0<statno[n]) continue;
          ns = -statno[n];
          for (i=0; i<sno; i++)
	    if (ns==statno[i])
	      fprintf(stderr,"Error: Duplicate label %5d, line %d\n",ns,lno);
          }
        if (x_flag & VER_FLAG)
          for (n=0; n<sno; n++) printf("Label : %5d\n",statno[n]);
	sno = 0; do_level = 0; do_label = DO_LABEL;
     }

/*******************************************************************
* Check label, tab, or continuation line (first 6 characters)  
*******************************************************************/
      if (!plab) {                   /* find new label if none on stack */
         if (c=='\t')                 /* check VAX tab extension       */
         {
           if (x_flag & VER_FLAG)
              fprintf(stderr,"Warning: TAB detected in line %d\n",lno);
           c = getc(fp);
           if (isalpha(c)) { ungetc(c,fp); c = ' '; }
         }
         else                         /* check for statement label     */
         {
           for (n=0; n<5 && c!='\t' && c!='\n' && c!=EOF; c = getc(fp))  
            {
             if (!plab && c!=' ') plab = label;
             label[n++] = c;
            }
           label[n] = '\0';
           if (c=='\t') c = ' ';
         }
         if (c == '\n' || c == EOF) continue;
/* check if continuation line  */
         cont = (c != ' ') ? c : '\0';   
      }

/*******************************************************************
* Deals with continuation line: 
*******************************************************************/
    if (cont)                       /* add continuation line         */
      {
        nlb++;
        c = get_line(fp);
      }
/*******************************************************************
* Analyse statement (whole line): 
*******************************************************************/
     else                            /* analyze statement (whole line) */
      {
/************* Check if nstat is non null (i.e. non empty line) */
/* get_line sets nstat to the index of the last character of the currentline */
       if (nstat) 
        {
          stmt[nstat] = '\0';
/* Get correct value for statement type (stype) :*/
          action = line_type(&stype);
          if (stype == INCLUDE && (x_flag & INC_FLAG)) 
           {
            incl_name = incl_file(stmt);
            printf("Line %d, including: path >%s<, name >%s<\n",
		    lno,incl_path,incl_name);
	    if(incl_name == NULL ) 
	      {printf(" Fatal error/include file \n"); exit(-1);}
	    fp = push_fp(fp,incl_path,incl_name);
            plab = push_lab(plab);
            hstat[stype & 0xFF] += 1;
            nstat = 0; nlb = 0; equal = 0; comma = 0; no_lid = 0;
            labno = 0;
            continue;
           }
          else 
/* If not include file statement, process curent line: */
           {
             put_line(ofp,action,stype,labno);
             hstat[stype & 0xFF] += 1;
             nstat = 0; nlb = 0; equal = 0; comma = 0; no_lid = 0;
             labno = 0;
           }
        }

/************* Check if plab */
       if (plab)                     /* read statement no. if present */
         {
          if (MXSNO<=sno) 
           { 
            fprintf(stderr,"Error: Max. no. of label reached %d\n",sno);
              exit(1);
           }
	  if (*plab)                  /* there is a statement label   */
           {
             labno = atoi(plab);
             statno[sno++] = labno;
           }
           else 
             labno = 0;

/************* All cases */
       plab = (char *) 0; }
/* Read new line: (get_line returns c=EOF if last line*/
      c = get_line(fp);
      }                           /* End of analyse of the statement */
    }                             /* End of "while" loop on the lines (EOF) */

/*******************************************************************
End of main loop on the lines: tidy up things now. 
*******************************************************************/
    for (n=0; n<sno; n++)             /* check duplicate labels      */
      {
	if (0<statno[n]) continue;
        ns = -statno[n];
        for (i=0; i<sno; i++)
        if (ns==statno[i])
           fprintf(stderr,"Error: Duplicate label %5d\n",ns);
      }
    if (x_flag & VER_FLAG) 
      {
      for (n=0; n<sno; n++) printf("Label : %5d\n",statno[n]);
      printf("Source file had %d lines.\n",lno);
      for (n=0; n<50; n++) 
        if (hstat[n]) printf("Statement #%2x: %4d\n",n,hstat[n]);
      }

    f_name = (f_flag) ? (char *) 0 : new_file();
  } while (f_name);                                     /* next file  */
/*******************************************************************
End of the loop on the files: exit. 
*******************************************************************/
  return(0);
}
