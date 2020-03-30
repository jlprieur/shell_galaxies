/*****************************************************************
* Attempt to convert fortran JLP_ACCESS to c ....
*
* Set of subroutines to access 2-D images in different formats
*
* Contains:
*  JLP_INQUIFMT, JLP_FORMAT, JLP_BEGIN_C, JLP_END
*  JLP_READIMAG, JLP_WRITEIMAG, JLP_VM_READIMAG1, JLP_CHECK_ASCII
*
* JLP
* Version of 17-02-98
*****************************************************************/
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include <jlp_ftoc.h>

/*
#define DEBUG
*/
#define NFILES_MAX 100 

#define jlp_from_madrid
#define JLP_RDIBDF jlp_rdibdf
#define JLP_RDRBDF jlp_rdrbdf
#define JLP_RDICCD jlp_rdiccd
#define JLP_RDCCD jlp_rdccd
#define JLP_RDCDCA jlp_rdcdca
#define JLP_WRIBDF jlp_wribdf
#define JLP_WRRBDF jlp_wrrbdf
#define JLP_WRICCD jlp_wriccd
#define JLP_WRCCD jlp_wrccd
#define JLP_WRCDCA jlp_wrcdca
#define JLP_LOC_MADRID jlp_loc_madrid
#define JLP_VM_RDIBDF jlp_vm_rdibdf
#define JLP_VM_RDBDF jlp_vm_rdbdf
#define JLP_VM_RDICCD jlp_vm_rdiccd
#define JLP_VM_RDCCD jlp_vm_rdccd
#define JLP_VM_RDCDCA jlp_vm_rdcdca


static int writeimag_to_single(double *d_image, float *image,
                        long nx, long ny, long d_idim, long idim);
static int jlp_dispinfo(char *filename, long *nx, long *ny, char *comments);
static int JLP_CHECK_SUFFIX(char *string, long *length, char *suffix);

static long ifmt_in, ifmt_out, nfile_in, nfile_out;
static long jlp_init_midas, in_descr, out_descr;
static char cdescr[1024];
/* Input/output Logical Unit (cf. Fortran): */
static FILE *jlp_lu5, *jlp_lu6;

/*----------------------------------------------------------
* Subroutine JLP_INQUIFMT
* To inquire the format of the input and output files
* Should be called in the beginning of all the programs
*----------------------------------------------------------*/
int JLP_INQUIFMT()
{
long i_in, i_out;
i_in = -1;
i_out = -1;
JLP_FORMAT(&i_in,&i_out);
return(0);
}
/*----------------------------------------------------------
* Subroutine JLP_FORMAT
* To inquire the format of the input and output files
*----------------------------------------------------------*/
int JLP_FORMAT(long *i_in, long *i_out)
{
long found, isafe;
long ilength, istatus;
char buffer[40], symbol_1[12];
register int i;
 
/* Reset the counter for the open files: */
 nfile_in = 0;
 nfile_out = 0;
 
/* Input/output of descriptors (set to one in versions after june 1994): */
 in_descr = 1; out_descr = 1;
 for(i = 0; i < 1023; i++) cdescr[i] = ' ';
 cdescr[1023] = '\0';

 found = 0;
 ifmt_in = *i_in;
 ifmt_out = *i_out;
/* Check if direct input of parameters is OK: */
 if((ifmt_in > 0 && ifmt_in < 11) && (ifmt_out > 0 && ifmt_out < 11)) 
      found = 1;
   else if(ifmt_in != -1 || ifmt_out != -1) 
      {printf("JLP_INQUIFMT/Format not available: %d,%d\n",ifmt_in,ifmt_out);}

/* Try to find the working input/output formats in the symbol "JLP_FORMAT": */
/* Note that if ifmt_in=ifmt_out=0: should not try JLP_GETENV ! */
  if(!found && (ifmt_in != 0 || ifmt_out != 0))
   {
   strcpy(symbol_1,"JLP_FORMAT");
   ilength = 10;
   JLP_GETENV(symbol_1,&ilength,buffer,&istatus);
#ifdef DEBUG
   printf("JLP_FORMAT = >%s<\n",buffer);
#endif
/* Then check the error flag: */
   if(istatus == 0 && (buffer[1] == ','  || buffer[2] == ',')) 
    {
/* Read the buffer: */
      sscanf(buffer,"%ld,%ld",&ifmt_in,&ifmt_out);

      if(ifmt_in > 0 && ifmt_in < 12 && ifmt_out > 0 && ifmt_out < 12) 
           found = 1;
         else 
           {
           printf("JLP_INQUIFMT/ Error reading buffer: >%s< in=%d out=%d\n",
                  buffer,ifmt_in,ifmt_out);
           }
    }
   }

/* If the search has not been successful ask for the format: */ 
  isafe = 0;
  while(!found && isafe < 3)
    {
    isafe++;
    printf(" Possible formats :\n\
 1=Starlink int   2=Starlink real   3=CCD int   4=CCD real \n\
 5=MIDAS   6=CDCA  7=FITS int  8=FITS real  9=Karim's  10=Eric's 11=TIFF\n\
 Enter your choice for the input AND the output: (f.i.: 8,8) :");
   scanf("%ld,%ld",&ifmt_in,&ifmt_out);
      if(ifmt_in > 0 && ifmt_in < 12 && ifmt_out > 0 && ifmt_out < 12) found = 1;
      else 
      {printf("JLP_INQUIFMT/Format not available: %d,%d\n",ifmt_in,ifmt_out);}
   }
 
/* Initializing MIDAS parameters (necessary also for FITS format): */
      if(ifmt_in == 5 || ifmt_out == 5 || ifmt_in == 7 || ifmt_out == 7
         || ifmt_in == 8 || ifmt_out == 8)
         { JLP_SCSPRO(); jlp_init_midas = 1;}

/* Just to check: */
   if(found)
     {
#ifdef DEBUG
     printf(" Input/output formats: IFMT_IN,IFMT_OUT: %d,%d\n",ifmt_in,ifmt_out);
#endif
     istatus = 0;
     }
   else
     istatus = 1;

return(istatus);
}
/*----------------------------------------------------------
* Subroutine JLP_END
* To tidy up the output (necessary if Midas option has been called)
*----------------------------------------------------------*/
int JLP_END()
{
/* Updating MIDAS parameters (necessary also for FITS format): */
      if(ifmt_in == 5 || ifmt_out == 5 || ifmt_in == 7 || ifmt_out == 7
         || ifmt_in == 8 || ifmt_out == 8)
         { JLP_SCSEPI(); jlp_init_midas = 0;}
return(0);
}
/*----------------------------------------------------------
* Subroutine JLP_BEGIN_C
* To check if the input parameters have to be found
* in the file "JLP_LU5.TMP", or read from LU=5 (Fortran default for input
* from a terminal)
* and if the output has to be written
* in the file "JLP_LU6.TMP", or directly on LU=6 (Fortran default for output
* from a terminal)
*----------------------------------------------------------*/
static int JLP_BEGIN_C()
{
long istatus, ilength;
char buffer[40], symbol_1[40], lu5_filename[60], lu6_filename[60];

/******************************* Input: ********************/
   jlp_lu5 = NULL;
/* Read the symbol JLP_PROMPT */
   buffer[0] = '\0';
   strcpy(symbol_1,"JLP_PROMPT");
   ilength = 10;
   JLP_GETENV(symbol_1,&ilength,buffer,&istatus);
/* Then check the error flag: */
   if(istatus != 0)
     {
/* Reads the buffer: */
#ifdef DEBUG
     printf(" JLP_BEGIN/JLP_PROMPT=>%s<\n",buffer);
#endif
     if(buffer[0] == 'n' || buffer[0] == 'N')
        {
         strcpy(lu5_filename,"jlp_lu5.tmp");
#ifdef DEBUG
         printf(" JLP_BEGIN/Opening=>%s<\n",lu5_filename);
#endif
         if ((jlp_lu5 = fopen(lu5_filename,"r")) != NULL) 
            {
#ifdef DEBUG
             printf(" Direct input of parameters from %s\n",lu5_filename);
#endif
            }
        }
     }

/******************************* Output: ********************/
   jlp_lu6 = NULL;
/* Read the symbol JLP_QUIET */
   buffer[0] = '\0';
   strcpy(symbol_1,"JLP_QUIET");
   ilength = 9;
   JLP_GETENV(symbol_1,&ilength,buffer,&istatus);
/* Then check the error flag: */
   if(istatus != 0)
     {
/* Reads the buffer: */
#ifdef DEBUG
     printf(" JLP_BEGIN/JLP_QUIET=>%s<\n",buffer);
#endif
     if(buffer[0] == 'y' || buffer[0] == 'Y')
        {
         strcpy(lu6_filename,"jlp_lu6.tmp");
#ifdef DEBUG
         printf(" JLP_BEGIN/Opening=>%s<\n",lu6_filename);
#endif
         if ((jlp_lu6 = fopen(lu6_filename,"w")) != NULL) 
            {
#ifdef DEBUG
             printf(" Redirection of messages to %s\n",lu5_filename);
#endif
            }
        }
     }

return(0);
}
/*----------------------------------------------------------
* Subroutine JLP_READIMAG
* To read a file in different formats and to write it in a real*4 array
* IMAGE(IDIM,*)
*
* Input :
* IDIM : Size of the array image (IMAGE(IDIM,*))
*
* Output :
* NX, NY : Actual size of the input file stored in the array IMAGE(IDIM,*)
* FILENAME *60
* COMMENTS *80
*----------------------------------------------------------*/
int JLP_READIMAG(float *image, long *nx, long *ny, long *idim,
                 char *filename, char *comments)
{
long isafe, found, quiet, number, istat, iwork;
char *pc, output[60*NFILES_MAX];

/* Prompt the name of the input file if not already in: */
isafe = 0; found = 0;
while(!found && isafe < 3)
 {
  if(filename[0] == ' ' || filename[0] == '\0')
    {
    printf("Name of the input image (wild card accepted): \n");
    if(jlp_lu5 != NULL && isafe == 0) fgets(filename,60,jlp_lu5); 
         else gets(filename);
    }

/* Look for the names concerned by the order when there 
is a wild card "*" in the name,
 If only one is concerned it is taken as input file :
*/
  filename[59] = '\0';
  pc = filename;
  while(*pc && *pc != '*') pc++;
  if(*pc == '*')
    {
/* Not quiet (0): display the names of the files on the terminal: */
     quiet = 0; 
/* Length of filename: */
     iwork = 60;
     JLP_DIR(filename,&iwork,output,&number,&quiet,&istat);
/* Prompt the name of the input file: */
     if(number != 1)
      {
      printf("More than one file, please choose now\n");
      }
     else
      found = 1;
    }
 isafe++;
 }

/* Remove trailing blanks (if input from Fortran): */
  pc = filename;
  while(*pc) pc++;
  pc--;
  while(*pc == ' ') {*pc = '\0'; pc--;}

/* If error when reading the file prompt for another name 
*   (3 times max): */
 isafe = 0; istat = 1;
 while(isafe < 3 && istat != 0)
 {
 switch(ifmt_in)
   {
#ifdef ALL_FORMATS
   case 1:
      JLP_RDIBDF(image,nx,ny,idim,filename,comments,&istat);
      break;
   case 2:
      JLP_RDRBDF(image,nx,ny,idim,filename,comments,&istat);
      break;
   case 3:
      JLP_RDICCD(image,nx,ny,idim,filename,comments,&istat);
      break;
   case 4:
      JLP_RDCCD(image,nx,ny,idim,filename,comments,&istat);
      break;
   case 6:
      JLP_RDCDCA(image,nx,ny,idim,filename,comments,&istat);
      break;
#else
   case 1:
   case 2:
   case 3:
   case 4:
   case 6:
      printf(" JLP_READIMAG Fatal error: \
format no longer available IFMT_IN =%d\n",ifmt_in);
      return(-1);
      break;
#endif
   case 5:
      JLP_RDMIDAS(image,nx,ny,idim,filename,comments,cdescr,
                  &in_descr,&istat);
      break;
   case 7:
   case 8:
      iwork = 0;
      JLP_RDFITS(image,nx,ny,idim,filename,comments,cdescr,
                 &in_descr,&istat);
      break;
   case 9:
      KARIM_READIMAG(image,nx,ny,idim,filename,comments,&istat);
      break;
   case 10: 
      iwork = 60;
      JLP_CHECK_SUFFIX(filename,&iwork,".bin");
      ERIC_READIMAG(image,nx,ny,idim,filename,comments,&istat);
      break;
   case 11:
      JLP_RDTIFF(image,nx,ny,idim,filename,comments,&istat);
      break;
   default:
      printf(" JLP_READIMAG Fatal error: \
format not available IFMT_IN =%d\n",ifmt_in);
      return(-1);
   }
 isafe++;
 }
 
/* Display filename, comments and descriptors on the screen: */
   if(!istat)
      {
      jlp_dispinfo(filename,nx,ny,comments);
      nfile_in++;
      }
 
return(istat);
}
/*----------------------------------------------------------
* Subroutine JLP_D_WRITEIMAG
* To write a double precision array IMAGE(IDIM,*) 
* into a file in different formats
*
* Input :
* IDIM : Size of the array image (IMAGE(IDIM,*))
* NX, NY : Actual size of the image stored in the array IMAGE(IDIM,*)
* COMMENTS *80
*
* Output :
* FILENAME *60
*----------------------------------------------------------*/
int JLP_D_WRITEIMAG(double *d_image, long *nx, long *ny,
                    long *idim, char *filename, char *comments)
{
float *image;
int istat;
long isize;
isize = (*nx) * (*ny) * sizeof(float);
JLP_GVM(&image,&isize);
writeimag_to_single(d_image,image,*nx,*ny,*nx,*idim);
istat = JLP_WRITEIMAG(image,nx,ny,nx,filename,comments);
JLP_FVM(&image);
return(istat);
}
/*----------------------------------------------------------
* Transfer from double precision to simple precision:
*----------------------------------------------------------*/
static int writeimag_to_single(double *d_image, float *image,
                        long nx, long ny, long d_idim, long idim)
{
register int i, j;
for(j = 0; j < ny; j++)
  for(i = 0; i < nx; i++)
    {
    image[i + j * idim] = d_image[i + j * d_idim];
    }
}
/*----------------------------------------------------------
* Subroutine JLP_WRITEIMAG
* To write a real*4 array IMAGE(IDIM,*) into a file in different formats
*
* Input :
* IDIM : Size of the array image (IMAGE(IDIM,*))
* NX, NY : Actual size of the image stored in the array IMAGE(IDIM,*)
* COMMENTS *80
*
* Output :
* FILENAME *60
*----------------------------------------------------------*/
int JLP_WRITEIMAG(float *image, long *nx, long *ny, long *idim,
                  char *filename, char *comments)
{
long isafe, istat, iwork;
char *pc;

/* Prompt the name of the input file if not already in: */
  if(filename[0] == ' ' || filename[0] == '\0')
    {
    printf("Name of the output image: \n");
    if(jlp_lu5 != NULL && isafe == 0) fgets(filename,60,jlp_lu5); 
         else gets(filename);
    }
 
/* If error when reading the file prompt for another name 
*   (3 times max): */
 isafe = 0; istat = 1;
 while(isafe < 3 && istat != 0)
 {
 switch(ifmt_out)
   {
#ifdef ALL_FORMATS
   case 1:
      JLP_WRIBDF(image,nx,ny,idim,filename,comments,&istat);
      break;
   case 2:
      JLP_WRRBDF(image,nx,ny,idim,filename,comments,&istat);
      break;
   case 3:
      JLP_WRICCD(image,nx,ny,idim,filename,comments,&istat);
      break;
   case 4:
      JLP_WRCCD(image,nx,ny,idim,filename,comments,&istat);
      break;
   case 6:
      JLP_WRCDCA(image,nx,ny,idim,filename,comments,&istat);
      break;
#else
   case 1:
   case 2:
   case 3:
   case 4:
   case 6:
      printf(" JLP_WRITEIMAG Fatal error: \
format no longer available IFMT_IN =%d\n",ifmt_in);
      return(-1);
      break;
#endif
   case 5:
      JLP_WRMIDAS(image,nx,ny,idim,filename,comments,
                  cdescr,&out_descr,&istat);
      break;
   case 7:
      iwork = 0;
      JLP_WRFITS(image,nx,ny,idim,filename,comments,
                  cdescr,&out_descr,&istat,&iwork);
      break;
   case 8:
      iwork = 1;
      JLP_WRFITS(image,nx,ny,idim,filename,comments,
                  cdescr,&out_descr,&istat,&iwork);
      break;
   case 9:
      KARIM_WRITEIMAG(image,nx,ny,idim,filename,comments,&istat);
      break;
   case 10: 
      iwork = 60;
      JLP_CHECK_SUFFIX(filename,&iwork,".bin");
      ERIC_WRITEIMAG(image,nx,ny,idim,filename,comments,&istat);
      break;
   case 11:
      JLP_WRTIFF(image,nx,ny,idim,filename,comments,&istat);
      break;
   default:
      printf(" JLP_WRITEIMAG Fatal error: \
format not available IFMT_OUT =%d\n",ifmt_out);
      return(-1);
   }
 isafe++;
 }
 
/* Display filename, comments and descriptors on the screen: */
  if(!istat)
   {
   jlp_dispinfo(filename,nx,ny,comments);
   nfile_out++;
   }
 
return(istat);
}
/*----------------------------------------------------------
* Subroutine JLP_VM_READIMAG1
* To read a file in different formats and
* to write it in a real*4 array IMAGE(NX,NY) starting at PNTR_IMAGE (pointer)
*
*
* Output :
* PNTR_IMAGE : absolute location of the starting address of the array IMAGE
* NX, NY : Size of the input image
* FILENAME *40
* COMMENTS *80
*----------------------------------------------------------*/
int JLP_VM_READIMAG1(long *pntr_image, long *nx, long *ny,
                     char *filename, char *comments)
{
long isafe, found, quiet, number, istat, iwork;
char *pc, output[60*NFILES_MAX];

/* Prompt the name of the input file if not already in: */
isafe = 0; found = 0;
while(!found && isafe < 3)
 {
  if(filename[0] == ' ' || filename[0] == '\0')
    {
/* print isafe for debug... */
    if(isafe)printf("%d ",isafe);
    printf("Name of the input image (wild card accepted): \n");
    if(jlp_lu5 != NULL && isafe == 0) fgets(filename,60,jlp_lu5); 
         else gets(filename);
    }

/* Look for the names concerned by the order when there 
is a wild card "*" in the name,
 If only one is concerned it is taken as input file :
*/
  filename[59] = '\0';
  pc = filename;
  while(*pc && *pc != '*') pc++;
  if(*pc == '*')
    {
/* Not quiet (0): display the names of the files on the terminal: */
     quiet = 0;
/* Length of filename: */
     iwork = 60;
     JLP_DIR(filename,&iwork,output,&number,&quiet,&istat);
/* Prompt the name of the input file: */
     if(number != 1)
      {
      printf("More than one file, please choose now\n");
      }
     else
      found = 1;
    }
 isafe++;
 }

/* Remove trailing blanks (if input from Fortran): */
  pc = filename;
  while(*pc) pc++;
  pc--;
  while(*pc == ' ') {*pc = '\0'; pc--;}

/* If error when reading the file prompt for another name 
*   (3 times max): */
 isafe = 0; istat = 1;
 while(isafe < 3 && istat != 0)
 {
 switch(ifmt_in)
   {
#ifdef ALL_FORMATS
   case 1:
      JLP_VM_RDIBDF(pntr_image,nx,ny,filename,comments,&istat);
      break;
   case 2:
      JLP_VM_RDBDF(pntr_image,nx,ny,filename,comments,&istat);
      break;
   case 3:
      JLP_VM_RDICCD(pntr_image,nx,ny,filename,comments,&istat);
      break;
   case 4:
      JLP_VM_RDCCD(pntr_image,nx,ny,filename,comments,&istat);
      break;
   case 6:
      JLP_VM_RDCDCA(pntr_image,nx,ny,filename,comments,&istat);
      break;
#else
   case 1:
   case 2:
   case 3:
   case 4:
   case 6:
      printf(" JLP_VM_READIMAG1 Fatal error: \
format no longer available IFMT_IN =%d\n",ifmt_in);
      return(-1);
      break;
#endif
   case 5:
      JLP_VM_RDMIDAS(pntr_image,nx,ny,filename,comments,cdescr,
                  &in_descr,&istat);
      break;
   case 7:
   case 8:
      iwork = 0;
      JLP_VM_RDFITS(pntr_image,nx,ny,filename,comments,cdescr,
                 &in_descr,&istat);
      break;
   case 9:
      KARIM_VM_READIMAG(pntr_image,nx,ny,filename,comments,&istat);
      break;
   case 10: 
      iwork = 60;
      JLP_CHECK_SUFFIX(filename,&iwork,".bin");
      ERIC_VM_READIMAG(pntr_image,nx,ny,filename,comments,&istat);
      break;
   case 11:
      JLP_VM_RDTIFF(pntr_image,nx,ny,filename,comments,&istat);
      break;
   default:
      printf(" JLP_READIMAG Fatal error: \
format not available IFMT_IN =%d\n",ifmt_in);
      return(-1);
   }
 isafe++;
 }
 
/* Display filename, comments and descriptors on the screen: */
   if(!istat)
    {
    jlp_dispinfo(filename,nx,ny,comments);
    nfile_in++;
    }
 
return(istat);
}

/*************************************************************************
* JLP_CHECK_SUFFIX
*************************************************************************/
static int JLP_CHECK_SUFFIX(char *string, long *length, char *suffix)
{
register int i;
char *pc;

/* First determines string length: either first blank or zero 
* (zero if called from C routine): */
for(i = 0; i < *length; i++)
   {
   if(string[i] == '\0' || string[i] == ' ' || string[i] == '.') break;
   }
#ifdef DEBUG
  printf("string: %s + suffix: %s -> ",string,suffix);
#endif
 
pc = suffix;
while(*pc && *pc != ' ') string[i] = *pc;
#ifdef DEBUG
  printf(" %s \n",string);
#endif
return(0);
}
/*************************************************************************
* JLP_RDESCR
* Usual syntax for CDESCR is :
*    $KEYWORD1=1.2 3.21 323.133131 $KEYWORD2=this is a test @
*
* Routine to look for a descriptor "KEYWORD" in CDESCR
* ISTATUS: 0 if NAME has been found, 1 otherwise.
*************************************************************************/
int JLP_RDESCR(char *keyword, char *value, long *length, long *istatus)
{
long lkeyw, istart, k1, k2, kstart, kmax, iw;
register int i;
char full_keyword[60], *pc;
*istatus = 1;

/* Look for end of keyword: */
pc = keyword; lkeyw = 0;
while(*pc && *pc != ' ') {pc++; lkeyw++;}

/* Exit from this routine if empty name */
if(!lkeyw) return(-1); 

/* Write end_of_string at the end if necessary: */
*pc = '\0';

/* Full keyword starts with "$": */
sprintf(full_keyword,"$%s",keyword);
lkeyw++;

/* Look for location of beginning of NAME (K1): */
 istart = 1;
 k1 = JLP_INDEX_DESCR(&istart,full_keyword,&lkeyw);
/* If NAME was found proceed further: */
 if(k1 > 0)
   {
/* Look for next '$' or "end of descriptor" symbol '@' */
     istart = k1 + 1;
     iw = 1;
     k2 = JLP_INDEX_DESCR(&istart,"$",&iw) - 1;
/* If not found, take the whole lot: */
     if(k2 <= 0) k2 = 1023;
     kmax = JLP_INDEX_DESCR(&istart,"@",&iw) - 1;
/* If not found, take the whole lot: */
     if(kmax <= 0) kmax = 1023;
     if(k2 > kmax) k2 = kmax;
     kstart = k1 + lkeyw;
     *length = k2 - kstart; 
     for(i = 0; i < *length; i++)
         value[i] = cdescr[i + kstart];

     value[i] = '\0';

#ifdef DEBUG
  printf(" length=%d value=%s\n",*length,value);
#endif
     *istatus = 0;
   }

return(*istatus);
}
/*************************************************************************
* JLP_WDESCR
* Usual syntax for CDESCR is :
*    $KEYWORD1=1.2 3.21 323.133131 $KEYWORD2=THIS IS A TEST
*
* Routine to update or write a new descriptor "KEYWORD" in CDESCR
* ISTATUS: 0 if descriptor can be written, 1 otherwise.
*************************************************************************/
int JLP_WDESCR(char *keyword, char *value, long *length, long *istatus)
{
long new_descriptor, old_length, max_length, lkeyw;
long istart, kk, k1, k2, kstart, kmax, iw, istat;
register int i;
char old_value[60], full_keyword[60], *pc, cwork[10];
*istatus = 1;

/* Look for end of keyword: */
pc = keyword; lkeyw = 0;
while(*pc && *pc != ' ' && lkeyw < 60) {pc++; lkeyw++;}

/* Exit from this routine if empty name */
if(!lkeyw) return(-1); 

/* Write end_of_string at the end if necessary: */
*pc = '\0';

/* Full keyword starts with "$": */
sprintf(full_keyword,"$%s",keyword);
lkeyw++;

/* First look if descriptor is already present: */
   JLP_RDESCR(keyword,old_value,&old_length,&istat);

/* ISTAT != 0: New descriptor */
   if(istat) new_descriptor = 1; 
       else  new_descriptor = 0;

/********************* New descriptor: */
   if(new_descriptor)
     {
#ifdef DEBUG
      printf(" Writing new descriptor: %s \n",keyword);
#endif
/* Check if enough space is available, look for "end of descriptor symbol" @: */
      istart = 1; iw = 1;
      strcpy(cwork,"@");
      k1 = JLP_INDEX_DESCR(&istart,cwork,&iw);
/* If not found, set k1 to one (i.e. empty descriptor): */
      if(k1 <= 0) k1 = 1;
/* K2 is the index of the last value: */
       k2 = k1 + lkeyw + *length;
       if(k2 >= 1023)
         {
           printf(" JLP_WDESCR/Error: No more space available for\
 descriptors \n");
           max_length = 1023 - k1 - lkeyw;
           printf(" lkeyword = %d length =%d Max length =  %d\n",
                  lkeyw,*length,max_length);
 	   *istatus = 1;
         }
        else
 	   *istatus = 0;
    }
/********************* Old descriptor: */
    else
    {
#ifdef DEBUG
     printf(" Updating old descriptor: %s Old value: %s\n",keyword,old_value);
#endif
     if(*length > old_length)
       {
	 printf(" JLP_WDRESCR/Error: new value is too long \n");
         printf("  (old_length=%d, new length=%d)\n",old_length,*length);
 	 *istatus = 1;
       }
     else
       {
         istart = 1;
         k1 = JLP_INDEX_DESCR(&istart,full_keyword,&lkeyw);
         k2 = k1 + lkeyw + old_length;
         *istatus = 0;
       }
    }

/* If OK, write new statement from K1 to K2 location
* Write: 'LNAME=VALUE    @' */
   if(!*istatus)
     {
       kk = k1;
       for(i = 0; i < lkeyw; i++)
         {
         cdescr[kk] = full_keyword[i];
         kk++;
         }
       cdescr[kk] = '='; kk++;
       for(i = 0; i < *length; i++)
         {
         cdescr[kk] = value[i];
         kk++;
         }
/* Fills the gap with blanks: */
       for(i = kk; i < k2; i++) cdescr[i] = ' ';

/* Last position if new descriptor: */
          if(new_descriptor) {cdescr[k2] = '@'; cdescr[k2+1] = '\0';} 
          
      }
/* End of case (old descriptor) */

return(0);
}
/**********************************************************************
*
* To replace INDEX fortran function for our purpose...
* But slightly different: here we return index of NAME starting location.
**********************************************************************/
int JLP_INDEX_DESCR(long *kstart, char *name, long *name_length)
{
long index_descr, found;
register int k;


/* For C compared to Fortran, I decrease the index by one: */
*kstart--;

found = 0;
for(k = *kstart; k < 1024; k++)
  {
    if(!strncmp(&cdescr[k],name,*name_length))
      {
        found = 1;
        break;
     }
  }

#ifdef DEBUG
if(found)
    printf(" JLP_INDEX_DESCR/OK: Name >%s< found  at k = %d\n",name,k);
else
    printf(" JLP_INDEX_DESCR/Sorry, name >%s< not found (k=%d)\n",name,k);
#endif
         
index_descr = k;
if(!found) index_descr = -1;

return(index_descr);
}
/******************************************************************
* To display filename, comments and descriptors on the screen
******************************************************************/
static int jlp_dispinfo(char *filename, long *nx, long *ny, char *comments)
{
/* Output some information about the file: */
 printf("Filename: %.31s  NX = %d NY = %d\n",filename,*nx,*ny);
 printf("Comments: %.69s\n",comments);

/* Display first two lines of descriptors (if not empty): */
 if(*cdescr && strncmp(cdescr,"     ",5))
      printf("Descrip.: %.69s\n",cdescr);

return(0);
}
