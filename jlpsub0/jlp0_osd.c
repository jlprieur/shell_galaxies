/*************************************************************************
 *  jlp0_osd.c 
 *  Routines to access C files from Fortran programs (only). 
 *  (Fortran interface to osd.c, in oslib.a)
 *  Use "test_osd.for" for debugging... 
 *  As it is used for Fortran programs, all the parameters should be passed by 
 *  reference.
 *
 *  JLP
 *  Version 16-07-91
 **************************************************************************/
#include <stdio.h>
#include <time.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include <jlp_ftoc.h>

/*
#define DEBUG 1
*/

#ifdef VAX
#include <stdlib.h>
# include <descrip.h>
# define DEC_STRING(V) struct dsc$descriptor *V 
# define STRING_COPY(S,V,L) strncpy(S,V->dsc$a_pointer,L)
# define STRING_COPY2(V,S,L) strncpy(V->dsc$a_pointer,S,L)
#else
#define DEC_STRING(V) char *V 
#define STRING_COPY(S,V,L) strncpy(S,V,L)
#define STRING_COPY2(V,S,L) strncpy(V,S,L)
#endif

/* main()
{
 char filename[61];
 int iomode,fid,istatus,length;
 strcpy(filename,"test.bin            ");
 length = strlen(filename);
 JLP_OSDOPEN(filename,&length,&iomode,&fid,&istatus);
 JLP_OSDCLOSE(&fid,&istatus);
} */

/*************************************************** 
* JLP_OSDOPEN opens a file (binary)
* iomode: 0=read 1=write 2=read_write 3=append 
****************************************************/
 int JLP_OSDOPEN(DEC_STRING(filename), INT4 *length, INT4 *iomode,
                 INT4 *fid, INT4 *istatus)
 {
   char phname[81];
   unsigned mode;
   int i;
   mode = *iomode;
#if DEBUG
#ifdef VAX
   printf(" JLP_OSDOPEN/STRING_COPY....\n");
   printf(" filename point... >%s<\n",filename->dsc$a_pointer);
#endif
#endif
   STRING_COPY(phname,filename,*length);
   phname[*length]='\0';
   i = 1;
   while (phname[i] != ' '&& phname[i] != '\0') i++;
   phname[i] = '\0';
#if DEBUG
   printf(" open: length %d\n",*length);
   printf(" open: phname >%s<\n",phname);
#endif
  *fid=osdopen(phname,mode);
   if(*fid == -1) *istatus = -1;
   else *istatus = 0;
   return(0);
 }
/***********************************************
* JLP_OSDCLOSE closes a file (binary) 
************************************************/
 int JLP_OSDCLOSE(INT4 *fid, INT4 *istatus) 
 {
   *istatus = osdclose(*fid);
   return(0);
 }
/***********************************************
* JLP_OSDREAD reads a file (binary)
* nochar: in input is the required number of values
*         in output is the actual number of characters read
************************************************/
 int JLP_OSDREAD(INT4 *fid, char *pbuf, INT4 *nochar, INT4 *istatus) 
 {
  int status;
   unsigned nobyt;
   nobyt = *nochar;
   status = osdread(*fid,pbuf,nobyt);
   if(status == -1)
     {*istatus = 1;
     *nochar = 0;
     return(1);
     }
   else
     {*istatus = 0;
      *nochar = status;
      return(0);}
 }

/***********************************************
* JLP_OSDWRITE writes in a file (binary) 
************************************************/
 int JLP_OSDWRITE(INT4 *fid, char *pbuf, INT4 *nochar, INT4 *istatus) 
 {
   unsigned nobyt;
   nobyt = *nochar;
   *istatus = osdwrite(*fid,pbuf,nobyt);
   if(*istatus != *nochar) return(-1);
   return(0);
 }
