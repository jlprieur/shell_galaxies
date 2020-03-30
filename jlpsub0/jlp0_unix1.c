/**********************************************************************
*  jlp0_unix1.c 
*  Routines to get and free virtual memory and emulate some basic VAX routines 
*  (called by jlp0_vax1.for)
*
* Contains:
* int JLP_GVM (pntr,memsize)
* int JLP_GVMI (pntr,memsize)
* int JLP_FVM (pntr)
* int JLP_SETENV (symbol,length1,string,length2,istat)
* int JLP_GETENV (symbol,length,string,istat)
* int JLP_CTIME(string,istat)
* int JLP_TIMER(float *cpu_time)
* int JLP_DIR(input,length,output,nfiles,quiet,istat)
* int JLP_LOC (var, loc)
* (and main program to test...)
*
*  JLP
* Version 15-04-1991
**********************************************************************/
/*
#define DEBUG 1
*/

#include <stdio.h>
#include <malloc.h>
#include <sys/types.h>    /* Needed for fork() and execl() */
#include <time.h>
#include <ctype.h>
#include <math.h>
#include <string.h>

#include <jlp_ftoc.h>

#ifdef VAX
#include <stdlib.h>
# include <descrip.h>
# define DEC_STRING(V) struct dsc$descriptor *V 
# define STRING_COPY(S,V,L) strncpy(S,V->dsc$a_pointer,L)
# define STRING_COPY2(V,S,L) strncpy(V->dsc$a_pointer,S,L)
#else
#define DEC_STRING(V) char *V 
#define STRING_COPY(S,V,L) strncpy(S,V,L);S[L] = '\0'
#define STRING_COPY2(V,S,L) strncpy(V,S,L);V[L] = '\0'
#endif

/* Maximum number of files for the directory "suff*.ext" */
#define NFILES_MAX 100
static int myprocess(char *str1, char *str2);
static int JLP_GETENV1(DEC_STRING(symbol), INT4 *length, 
                       DEC_STRING(string), INT4 *istat);

/**********************************************************
*  Main routine used for debugging...
*  Only for unix.
#define MAINPRO
*/
#ifdef MAINPRO
main()
{
  int memsize;
  float *t;
  register int i;
  INT4 length1, length2, istat;
  char symbol[41],string[41];
  printf(" TEST program for jlp0_vax1\n");
  memsize=1024*1024*sizeof(float);
  JLP_GVM(&t,&memsize);

  for (i=0; i<100; i++) t[i]=1.0;
  i=1024*1024; t[i]=2.;
  printf(" t(i), i=1024*1024, %f \n",t[i]);

  JLP_FVM(&t);
  strcpy(symbol,"JLPVERSION");
  strcpy(string,"01AUG91");
  length1 = strlen(symbol);
  length2 = strlen(string);
  JLP_SETENV(symbol,&length1,string,&length2,&istat);
  printf(" SETENV: istat : %d\n",istat);
  printf(" Symbol: %s\n",symbol);
  printf(" Value: %s\n",string);
  length1 = strlen(symbol);
  JLP_GETENV(symbol,&length1,string,&istat);
  printf(" GETENV: istat : %d\n",istat);
  printf(" Symbol: %s\n",symbol);
  printf(" Value: %s\n",string);

  JLP_CTIME(string,&istat);
  printf(" istat : %d\n",istat);
  printf(" date-time %s",string);
}
#endif
/**************************************************************** 
* JLP_GVM 
* To allocate virtual memory space for real arrays
*****************************************************************/
 int JLP_GVM (float **pntr, INT4 *memsize)
 {
   unsigned memsiz;
   memsiz = *memsize;
   if((*pntr=(float *) malloc(memsiz)) != NULL)
	return(0);
   printf("Fatal error in jlp_gvm : alloc. of mem.\n");  
   printf(" memsize = %d \n",memsiz);
   exit(1);
 }
/**************************************************************** 
* JLP_GVMI 
* To allocate virtual memory space for integer arrays
*****************************************************************/
 int JLP_GVMI (INT4 **pntr, INT4 *memsize)
 {
   unsigned memsiz;
   memsiz = *memsize;
   if((*pntr=(INT4 *) malloc(memsiz)) != NULL)
	return(0);
   printf("Fatal error in jlp_gvmi : alloc. of mem.\n");
   printf(" memsize = %d \n",memsiz);
   exit(1);
 }
/********************************************************** 
* JLP_FVM
* To free the memory space previously allocated
************************************************************/
 int JLP_FVM (float **pntr)
 {
   free(*pntr);
   return(0);
 }
/********************************************************** 
* JLP_RENAME 
* to rename a file 
************************************************************/
 int JLP_RENAME (DEC_STRING(infile),INT4 *length1, 
                 DEC_STRING(outfile), INT4 *length2, INT4 *istat)
 {
 char command1[121], *pc;
 char myinfile[41], myoutfile[81];

 STRING_COPY(myinfile,infile,(int)(*length1));
 pc = myinfile; while(*pc && *pc !=' ')pc++; *pc = '\0';

 STRING_COPY(myoutfile,outfile,(int)(*length2));
 pc = myoutfile; while(*pc && *pc !=' ')pc++; *pc = '\0';

#ifdef VAX
 sprintf(command1,"copy %s %s",myinfile,myoutfile);
#else
 sprintf(command1,"mv -i %s %s",myinfile,myoutfile);
#endif
 command1[121]='\0';
/* JLP_SYSTEM returns 0 if success */
 *istat = JLP_SYSTEM(command1);

 return(0);
 }
/**************************************************************** 
* JLP_SETENV 
* To write a symbol as an environment variable :
* Problems since "runs" opens up a new session each time and therefore takes
* the values defined in ".cshrc" ...
*
****************************************************************/
 int JLP_SETENV (DEC_STRING(symbol), INT4 *length1, DEC_STRING(string),
                INT4 *length2, INT4 *istat)
 {
 char command1[121], *pc;
 char mysymbol[41], mystring[81], *mystr;
 register int i;

 STRING_COPY(mysymbol,symbol,*length1);
 pc = mysymbol; while(*pc && *pc !=' ')pc++; *pc = '\0';

 STRING_COPY(mystring,string,*length2);
/* Removing first blanks if present: */
 mystr = mystring; 
 i = 0;
 while(mystring[i] == ' ' && i < *length2-1) {mystr = &mystring[i+1]; i++;}

/* Removing last blanks: */
 pc = mystr;
 while(*pc && *pc !=' ')pc++; 
 *pc = '\0';

#ifdef VAX
 sprintf(command1,"define %s %s",mysymbol,mystr);
#else
/* JLP99: use sh instead of csh since pb with "setenv": */
#ifdef linux
 sprintf(command1,"%s=%s ; export %s ",mysymbol,mystr,mysymbol);
#else
 sprintf(command1,"setenv %s %s",mysymbol,mystr);
#endif
#endif
 command1[121]='\0';

 printf("JLP_SETENV/>%s< \n",command1);

/* JLP_SYSTEM returns 0 if success */
 *istat = JLP_SYSTEM(command1);

 return(0);
 }
/***************************************************************** 
* JLP_CTIME  to get the date and time:
******************************************************************/
 int JLP_CTIME(DEC_STRING(string), INT4 *istat)
 {
   long clock;
   char *p;
   *istat=1;
     time(&clock);
     if( (p=ctime(&clock)) != NULL)
          {*istat=0; STRING_COPY2(string,p,20);} 
   return(0);
 }
/*****************************************************************
* JLP_TIMER to compute the cpu time
*
* Usage:
* First call to initialize the timer
* Second call to read the value of the elapsed time
******************************************************************/
 int JLP_TIMER(float *cpu_time)
 {
   *cpu_time = (float)clock() / CLOCKS_PER_SEC;
   return(0);
 }
/***************************************************************** 
* JLP_GETENV1 reads a symbol predefined with setenv :
* Problems since "runs" opens up a new session each time and therefore takes
* the values defined in ".cshrc" ...
*
* Old version
*****************************************************************/
static int JLP_GETENV1(DEC_STRING(symbol), INT4 *length, 
                       DEC_STRING(string), INT4 *istat)
 {
   char *getenv(),*p, *pc;
   char mysymbol[41];

   STRING_COPY(mysymbol,symbol,*length);
   mysymbol[*length]='\0';
   pc = mysymbol; while(*pc && *pc !=' ')pc++; *pc = '\0';

   if( (p=getenv(mysymbol)) == NULL)
       *istat=1;
   else
      {*istat=0; 
      STRING_COPY2(string,p,20); 
#ifdef DEBUG
      printf(" symbol: >%s< string: >%s< \n",mysymbol,p);
#endif
      }
   return(0);
 }

/***************************************************************** 
* JLP_GETENV reads a symbol predefined with setenv :
* Problems since "runs" opens up a new session each time and therefore takes
* the values defined in ".cshrc" ...
*
* New version
******************************************************************/
 int JLP_GETENV(DEC_STRING(symbol), INT4 *length, DEC_STRING(string),
                INT4 *istat)
 {
   long int nochar, status;
   int len;
   char *getenv(), *pc;
   char mysymbol[41];
   char command[90], filename[31], pbuf[60];
   FILE *fp;

   len = *length;
   STRING_COPY(mysymbol,symbol,len);
   mysymbol[len]='\0';

/* Removes blanks at the end: */
   pc = mysymbol; while(*pc && *pc !=' ')pc++; *pc = '\0';

   strcpy(filename,"jlp_symbol.tmp");
#ifdef VAX
   sprintf(command,"show logical/output=%s  %s ",filename,mysymbol);
#else
   sprintf(command,"printenv %s > %s",mysymbol,filename);
#endif
#if DEBUG
   printf("jlp_getenv: length = %d, mysymbol: >%s< \n",*length,mysymbol);
   printf("command: >%s< \n",command);
#endif

/* Actual command to the system  (be carefull with IBM to link
fortran programs with /lib/libc.a*/
   status = JLP_SYSTEM(command);

/* Doesn't output an error message if error, since this simply
means the symbol is not there. The non-zero status should be handled
in the calling routine. */
/*
   if(status) {printf("jlp_getenv/ error in 'JLP_SYSTEM' :\n >%s< \n",
                 command);
               *istat = -2; return(-2);}
*/
   if(status) { *istat = -2; return(-2);}

/* Open file in read mode (0): */
/* JLP99
 iomode = 0;
 fid=osdopen(filename,iomode);
 if(fid == -1) {*istat = -1; return(-1);}
*/
 if((fp = fopen(filename, "r")) == NULL)
 {
  *istat = -1; 
  printf("JLP_GETENV/error opening file %s \n",filename); 
  return(-1);
  }

/* Read it: */
 nochar = 60;
/*
  status = osdread(fid,pbuf,nochar);
*/
  status = fread(pbuf, sizeof(char), nochar, fp);
  if(status == 0)
  {*istat = 1; return(1);}
  else
  nochar = status;

#ifdef DEBUG
  printf("nochar %d\n",nochar);
  printf(" Buffer: \n >%s<\n",pbuf);
#endif

#ifdef VAX
  i = 0;
  while(pbuf[i] != '=' && i < nochar) i++;
  if(i != nochar)
   {
/* "symbol" = "jdksdsj" (LNM$PROCESS_TABLE) */
   i0 = i+3; pc = pbuf + i0; i = i0;
   while( pbuf[i] != '\0' && pbuf[i] != '"') i++;
   nochar = i - i0;
   STRING_COPY2(string,pc,nochar);
   }
   else *istat = 2;
#else
/* Note that for Unix, the last character is always '\n' (end of line) */
  nochar--;
  STRING_COPY2(string,pbuf,nochar);
#endif

/* Close it: */
/*
 status = osdclose(fid);
*/
 fclose(fp);

/* Removes the temporary file: */
#ifdef VAX
 sprintf(command,"delete/nolog/noconfirm %s.*",filename);
#else
 sprintf(command,"rm -f %s",filename);
#endif
#if DEBUG
   printf("command: >%s< \n",command);
#endif
 status = JLP_SYSTEM(command);

   *istat = 0;
#if DEBUG
   printf("string: >%s< \n",string);
#endif
   return(0);
 }
/**************************************************************** 
 JLP_DIR  
 to get the files corresponding to a wild card input  
 in the current directory
 output is a series of char*60
******************************************************************/
int JLP_DIR(DEC_STRING(input), INT4 *length, DEC_STRING(output),
            INT4 *nfiles, INT4 *quiet, INT4 *istat)
 {
 int nochar, status;
 int i, eof_flag;
 char command[90], myinput[60], pbuf[60*NFILES_MAX];
 char *pc, *pc1, filename[61];
 FILE *fp;

 *istat = 0;

 STRING_COPY(myinput,input,*length);
 myinput[*length]='\0';
 pc = myinput; while(*pc && *pc !=' ')pc++; *pc = '\0';

 /* Actual command to the system: */
#ifdef VAX
 sprintf(command,"directory/brief/columns=1/output=jlp_dir.tmp %s ",myinput);
#else
 sprintf(command,"ls %s > jlp_dir.tmp",myinput);
#endif
#if DEBUG
   printf("command: >%s< \n",command);
#endif
 status = JLP_SYSTEM(command);
   if(status) {printf("jlp_dir/ error in 'JLP_SYSTEM' :\n >%s< \n",
                 command);
               *istat = -2; return(-2);}

 strcpy(filename,"jlp_dir.tmp");
/* Open file in read mode (0): */
/* JLP99
 iomode = 0;
 fid=osdopen(filename,iomode);
 if(fid == -1) {*istat = -1; return(-1);}
*/
 if((fp = fopen(filename, "r")) == NULL)
 {
  *istat = -1; 
  printf("JLP_DIR/error opening file %s \n",filename); 
  return(-1);
  }

/* Read it: */
 nochar = 60*NFILES_MAX;
/*
  status = osdread(fid,pbuf,nochar);
*/
 status = fread(pbuf, sizeof(char), nochar, fp);
 if(status == 0)
  {*istat = 1; return(1);}
 else
  nochar = status;

#ifdef DEBUG
  printf("nochar %d\n",nochar);
  printf(" Buffer: \n >%s<\n",pbuf);
#endif

 /* Close it: */
 fclose(fp);

/* Removes the temporary file: */
#ifdef VAX
 status = JLP_SYSTEM("delete/nolog/noconfirm jlp_dir.tmp.");
#else
 status = JLP_SYSTEM("rm -f jlp_dir.tmp");
#endif

/* Sort out the individual files: 
   Format of directory is filename <CR> for unix
   for Vax it is the same except that there are empty lines and other
   heading and trailing lines, so I stop when I found ";" */
 pc = pbuf;
 i = 0; eof_flag = 0;
 while (!eof_flag && i < NFILES_MAX)
 {
   pc1 = pc;
#ifdef VAX
   while(*pc && *pc != '\n' && *pc != ';') pc++;
   if(*pc == NULL && *(pc+1) == NULL && *(pc+2) == NULL
   && *(pc+4) == NULL)
     {
      eof_flag = 1;
#if DEBUG
      printf("eof!");
#endif
     }
   if(*pc == NULL) pc++;
/* Check if a file is present in the line: */
   if(*pc == ';')
    {*pc = '\0';
     strcpy(output->dsc$a_pointer + i * 60,pc1);
     if(!(*quiet))
         printf(" File # %d >%s< \n",i,output->dsc$a_pointer + i * 60);
/* Go to the end of the line: */
     i++; pc++;
     while(*pc && *pc != '\n') pc++;
    }
#else
   while(*pc && *pc != ' ' && *pc != '\n') pc++;
   if(*pc == '\0') eof_flag = 1;
   *pc = '\0'; 
   strcpy(output + i * 60,pc1);
   if(!(*quiet)) printf(" File # %d >%s< \n",i,output + i * 60);
   i++; pc++;
#endif
 }

#ifdef VAX
  i++;
#endif

 *nfiles = i - 1;
 if(*nfiles <= 0) *istat = 1;
#if DEBUG
     printf(" nfiles = %d \n",*nfiles);
#endif

 return(0);
 }

/******************************************************************* 
* Now no longer used: 
*******************************************************************/ 
#ifdef sunold

#include <sys/types.h>
#include <dirent.h>

/******************************************************************* 
* jlp_dirsun_
*******************************************************************/
int jlp_dirsun_(input,output,number,istat)

 long int *number, *istat;
 DEC_STRING(input);
 char output[];
 {
 register int i;
 char myinput[60];
 DIR *dirp;
 struct dirent *dp;
 dirp = opendir(".");

 STRING_COPY(myinput,input);
#ifdef DEBUG
  printf(" input file before: %s \n",myinput);
#endif

 for (i=0; i<60; ++i) 
   if(myinput[i] == ' ' || myinput[i] == '\0'){myinput[i] = '\0';
   break;}
#ifdef DEBUG
 printf(" input file after: %s \n",myinput);
#endif

 /* loop on the files: */
 /* Output is an array of char*60 ... */
 *istat = 0;
 *number = 0;
 for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp))
#ifdef DEBUG
      printf(" file: %s ",dp->d_name); 
#endif
     if(myprocess(myinput,dp->d_name) == 0)
	{ strcpy(output+60* *number,dp->d_name); 
          *number = *number+1;}

 closedir(dirp);

 /* Not found: */
 if (number == 0) {*istat = -1; return(-1);}

 return(0);
 }
/******************************************************************
* Check if str2 corresponds to the specifications imposed by str1
* (with wild card). The context is file search in a directory...
******************************************************************/
 static int myprocess(char *str1, char *str2)
 {
 int i, j, imax, jmax;

   imax = strlen(str1);
   jmax = strlen(str2);
   j = 0;
   for (i=0; i<imax; ++i)
     if(*(str1+i) != '*')
/* Exit if characters are not the same: */
       {if(*(str1+i) != *(str2+j)) return(-1);
	else j = j+1;}
/* Wild card, look for the index j of the next string in str2: */
      else
/* End of string, successful exit: */
       {if(i+1 >= imax) return(0);
       while(j < jmax && *(str2+j) != *(str1+i+1) ) ++j;
       if(j >= jmax) return(-2);
#ifdef DEBUG
       printf(" i %d j %d \n",i,j);
#endif
       }
 }
#endif
/******************************************************************* 
* JLP_LOC returns the address of the integer variable 
********************************************************************/
 int JLP_LOC (INT4 *var, INT_PNTR *loc)
 {
   *loc = (INT_PNTR)var;
   return(0);
 }
