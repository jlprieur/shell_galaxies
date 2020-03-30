/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* jlp0_wrfits.c
* To write FITS formatted 2-D image files
* Formats supported are : FITS 32,-32 
* i.e., 4-byte integer (JLP_FORMAT=7) or 4-byte float values (JLP_FORMAT=8)
* JLP comments and jlp_descriptors
*
* June 1996: I remove as many calls as possible to MIDAS include files
* since it can change with MIDAS versions...
*
* Contains:
* int JLP_WRFITS(array,nx1,ny1,idim,filename,comments,jlp_descr,dflag,
*               istatus,out_type)
* int fitswhd1(filename,comments,jlp_descr,lcut,hcut,nx,ny,nz,naxis)
* static int jlp_wdescr_fits(jlp_descr)
* int fitswdm1(array,nx,ny,idim)
* fitswkl(kw,hkw,hkn,no,val,com)
* fitswki(kw,hkw,hkn,no,val,com)
* fitswkd(kw,hkw,hkn,no,val,com)
* fitswks(kw,hkw,hkn,no,val,com)
* int kwput(kw,hkw,hkn,no)
* kwcom(com)
* (and main program to test...)
*
* JLP
* Version 27-06-96
---------------------------------------------------------------------*/
#include   <stdio.h>

/* JLP96: */
#ifdef TTTT
#include   <datafmt.h>             /* general data definitions       */
#include   <fitsdef.h>             /* basic FITS definitions         */
#include   <midas_def.h>
#include   <fitskwt.h>
#include   <fitsdkw.h>
#endif
/* JLP 96: */
#define  WRITE        1
#define       MXS                17   /* max. length of char string */
#include   <computer.h>
#include   <jlp_ftoc.h>

#define DEBUG 1

static int jlp_wdescr_fits(char *jlp_descr);
int fitswdm1(float *array, int nx, int ny, int idim);

/* Options (declared as extern elsewhere)*/
int               popt;                 /* print option              */
char            opt[4];                 /* general option flags      */
char           fopt[4];                 /* file option flags         */

/* Main program to test JLP_WRFITS */

/**********************************
main()
{
  float        array[80000];
  long int     nx1, ny1, idim;
  char         infile[60], outfile[60];
  char         comments[81], jlp_descr[1024];
  long int     istatus, dflag, out_type;

JLP_BEGIN();
JLP_INQUIFMT();

  printf(" jlp0_wrfits to convert MIDAS bdf files to FITS format\n");
  printf(" Version 11-11-91\n");

      printf(" Input fits file : ");
      scanf("%s",infile);
      printf(" Output file : ");
      scanf("%s",outfile);

#if DEBUG
    printf(" Input fits file : >%s< \n",infile);
    printf(" Output file     : >%s< \n",outfile);
#endif

idim = 256; dflag = 0;
JLP_RDMIDAS(array,&nx1,&ny1,&idim,infile,comments,jlp_descr,&dflag,&istatus);
#if DEBUG
 printf(" JLP_RDMIDAS/istatus = %d \n",istatus);
#endif

#if DEBUG
 printf(" nx = %d, ny = %d \n",nx1,ny1);
 printf(" comments: %s \n",comments);
#endif

out_type = 0;
JLP_WRFITS(array,&nx1,&ny1,&idim,outfile,comments,jlp_descr,
           &dflag,&istatus,&out_type);
#if DEBUG
 printf(" JLP_WRFITS/istatus = %d \n",istatus);
#endif

JLP_END();
}
******************************/

/**********************************************************************
* JLP_WRFITS
*
* Output format:
* out_type = 0 : integer (32)
* out_type = 1 : real (-32) 
**********************************************************************/
int JLP_WRFITS(float *array, long *nx1, long *ny1, long *idim1,
               char *filename, char *comments, char *jlp_descr, long *dflag,
               long *istatus, long *out_type)
{
  char         devt, *pcc;
  int          iparm[5];
  int          den, bf, nx, ny, idim;
  float        work;
  double       lcut, hcut;
  int          fmt,num,n,nval,nz1,naxis;
  register int i, j;

#if DEBUG
   printf("Start of JLP_WRFITS\n");
#endif

  devt = 'S';
  nx = *nx1; ny = *ny1; idim = *idim1;

*istatus = 0;

/* Check input characters strings (pb if fortran...) */
   pcc = filename;
   while(*pcc && *pcc != ' ') pcc++;
   if(*pcc == ' ') *pcc = '\0';

   comments[79]='\0';
   jlp_descr[1023]='\0';

/* Check filename syntax and add ".fits" if no extension is present: */
   pcc = filename;
   while(*pcc && *pcc != '.') pcc++;
   if(*pcc != '.') strcpy(pcc,".fits");

/* Options: */
  opt[0] = 'N';              /* append flag : No append, Append       */
  opt[1] = 'S';              /* print  flag : No, Short, Full         */
  opt[2] = 'N';              /* cut    flag : No cuts, Cuts used      */
  opt[3] = '\0';
  switch (opt[1]) {                          /* check print option    */
     case 'F' : popt = 2; break;             /* Full print            */
     case 'S' : popt = 1; break;             /* Short print           */
     case 'N' : popt = 0; break;             /* No printing at all    */
#if DEBUG
     default  : popt = 2;
#else
     default  : popt = 0;
#endif
  }

/* out_type = 0 : integer (32)
 out_type = 1 : real (-32) */ 
if(*out_type == 1)
  fopt[0] = 'O';             /* format flag : Original (i.e. real -32)    */
else
  fopt[0] = 'B';             /* format flag : Basic FITS (i.e. integer 32)*/

  fopt[1] = '\0';
  fopt[2] = '\0';
  fopt[3] = '\0';
/*
  read file options
*/
  num = 1; nval = 0;
  SCECNT("PUT",&num,&nval,&nval);             /* disable SC errors     */

/*
  outname(filename,0);
*/

#if DEBUG
   printf("Opening output file >%s< nx=%d ny=%d\n",filename,nx,ny);
#endif
   den = 1600;
   dopen(filename,WRITE,devt,den);
/* bf: blocking factor */
   bf = 1;
   dwinit(bf);

/* compute max/min cuts of data */
  lcut = *array; hcut = lcut;
  for(j=0; j < ny; j++)
   {for(i=0; i < nx; i++)
/* JLP99: Causing "unaligned access pid=6587 ... " with astro */
      { 
        work = array[i + j * idim];
        if(!isNULLFLOAT(work))
          {
          if ( work < lcut) lcut = work;
           else if (hcut < work) hcut = work;
          }
      }
   }

/* write FITS header      */
  nz1=0; naxis=2;
  *istatus = fitswhd1(filename,comments,jlp_descr,lcut,hcut,nx,ny,nz1,naxis);      
  if(*istatus)
  { printf("JLP_WRFITS/Error when writing header to file\n"); return(-1);
  }

/* write prime data       */
  *istatus = fitswdm1(array,nx,ny,idim);                
  if(*istatus)
  { printf("JLP_WRFITS/Error when writing data to file\n"); return(-1);
  }

/* Close file on disk: */
    n = dweof();

#if DEBUG
    printf("File %-16s written to disk \n",filename);
#endif

return(0); 
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.COPYRIGHT     (c)  1990   European Southern Observatory
.IDENT         fitswhd.c
.LAUGUAGE      C
.AUTHOR        P.Grosbol   ESO/IPG
.KEYWORDS      FITS header, keywords
.COMMENT       write FITS header
.VERSION       1.0  1988-Dec-11 : Creation,   PJG 
.VERSION       1.1  1989-Jan-17 : Change flag + include Btable,   PJG 
.VERSION       1.2  1989-Feb-14 : Binary-table support,   PJG 
.VERSION       1.3  1989-Jun-12 : Change definition of unit+kunit, PJG 
.VERSION       1.4  1989-Aug-24 : Remove check on I-display format, PJG 
.VERSION       1.5  1989-Nov-06 : Include I*4 and R*8 formats, PJG 
.VERSION       1.6  1990-Feb-02 : Update to new keyword structure, PJG 
.VERSION       1.7  1990-Feb-25 : Check fp-exeption + UNIT desc., PJG
---------------------------------------------------------------------*/

#define    BSIZE       10240         /* size of internal buffer      */
#define    MXLB           81         /* max. char. in line buffer    */

static double                  bfac;        /* inverse scaling factor       */
static double                  boff;        /* zero offset of data          */

int fitswhd1(filename,comments,jlp_descr,lcut,hcut,nx,ny,nz,naxis)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       write FITS header
.RETURN        return status  0:OK, -1:error
---------------------------------------------------------------------*/
int nx, ny, nz, naxis;
double lcut, hcut;
char filename[], comments[], jlp_descr[];
{
  char    cunit[MXS], *pc, fc;
  int     n,ne,nc,nbp,nv,null;
  register int i;
  float   *f,*fb;
  double  dval;
  double  crval,crpix,cdelt;

          if (fopt[0]=='B') {      /* floating point format  */
            nbp = 32;
          }
          else {                   /* 32-bit integer format  */
            nbp = -32; 
          }
          fitswkl("SIMPLE","",0,-1,1,"Standard FITS format");
          fitswki("BITPIX","",0,-1,nbp,"No. of bits per pixel");
          fitswki("NAXIS","",0,-1,naxis,"No. of axes in image");
          fitswki("NAXIS","",0,1,nx,"No. of pixels");
          fitswki("NAXIS","",0,2,ny,"No. of pixels");
          if(naxis > 2)
              fitswki("NAXIS","",0,3,nz,"No. of pixels");
          fitswkl("EXTEND","",0,-1,1,"FITS extension may be present");
          fitswkl("BLOCKED","",0,-1,1,"FITS file may be blocked");
          fitswkc("","");

          strcpy(cunit," intensity ");
          fitswks("BUNIT","",0,-1,cunit,"Units of data values");
          if (nbp==32) {   /* scaling data       */
             bfac = 0.5 * (hcut-lcut)/((double)(MAXLONG-2));
             fitswkd("BSCALE","",0,-1,bfac,"Scaling factor: r = f*i + z");
             bfac = 1.0 / bfac; boff = 0.5 * (hcut + lcut);
             fitswkd("BZERO","",0,-1,boff,"Zero offset: r = f*i + z");
          }
          else
          {
             bfac = 1.0; boff = 0.0;
             fitswkd("BSCALE","",0,-1,bfac,"Scaling factor: r = f*i + z");
             fitswkd("BZERO","",0,-1,boff,"Zero offset: r = f*i + z");
          }
          fitswkc("","");

          for (i=1; i<=naxis; i++) {          /* define axes         */
              cdelt = 1.0;                  /* step */
              crpix = 1.0;                  /* start */
              crval = 1.0;
              fitswkd("CRPIX","",0,i,crpix,"Reference pixel");
              fitswkd("CRVAL","",0,i,crval,"Coordinate at reference pixel");
              fitswkd("CDELT","",0,i,cdelt,"Coordinate increment per pixel");
              strcpy(cunit," pixels");
              fitswks("CTYPE","",0,i,cunit,"Units of coordinate");
          }
          fitswkc("","");

/* JLP comments: */
  comments[62] = '\0';
  fitswks("OBJECT","",0,-1,comments,"");

/* jlp_descriptors: */
  jlp_wdescr_fits(jlp_descr);

/* End of header: */
  fitswks("ORIGIN","",0,-1,"JLP-OMP","Written by JLP");
  fitswkc("END","");
  dbfill(' ');

  return 0;
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* Routine to write jlp_descriptors in FITS format
* 
* JLP
---------------------------------------------------------------------*/
static int jlp_wdescr_fits(char *jlp_descr)
{
 register int i, k;
 char buffer[64], mydescr[1024], *pc;

  strcpy(mydescr,jlp_descr);

/********* First fills the end of descriptors with zeroes ********/
/* Look for the first zero: */
  mydescr[1023] = '\0';
  pc = mydescr;
  while(*pc) pc++;
 
/* Then replaces the blanks at the end with zeroes */ 
  pc--;
  while(*pc == ' ') {*pc = '\0'; pc--;}

/***** Then copies the descriptors to FITS header: (1024/62=17 lines maxi) ****/
  for (k=0; k<17; k++)
  {
  strncpy(buffer,&mydescr[k*62],62);
  buffer[62] = '\0';
  fitswks("HISTORY","",0,-1,buffer,"");
/* Stop when first zero has been found */
    for (i=0; i<62; i++) if(buffer[i] == '\0') break;
    if(i < 62) break;
  }

/* end: */
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.COPYRIGHT     (c)  1990   European Southern Observatory
.IDENT         fitswdm.c
.LAUGUAGE      C
.AUTHOR        P.Grosbol   ESO/IPG
.KEYWORDS      FITS prime data matrix
.COMMENT       write FITS prime data matrix
.VERSION       1.0  1988-Dec-10 : Creation,   PJG 
.VERSION       1.1  1989-Jan-12 : Modify format flag,   PJG 
.VERSION       1.2  1989-Nov-07 : Include I*4 and R*8 format,   PJG 
.VERSION       1.3  1990-Feb-04 : Change call-seq. for cv-routine, PJG 
---------------------------------------------------------------------*/

int fitswdm1(float *array, int nx, int ny, int idim)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       write FITS prime data matrix
.RETURN        return status  0:OK, -1:error
---------------------------------------------------------------------*/
{
  int     dsize;  /* size of data matrix in pixels      */
  int     nbyte_size;  /* size of data matrix in pixels      */
  int     status, *pi;
  register int i, j;
  float   *pf;
  union {
          unsigned char   *b;
          short           *s;
          int             *i;
          long            *l;
          float           *f;
          double          *d;
        } p;

/* JLP99: */
    printf("nx=%ld ny=%ld idim=%ld\n",nx,ny,idim);

  dsize = nx * ny;
  nbyte_size = dsize * sizeof(float);
  status = 0;

/* Get memory space: */
  if((p.f = (float *)malloc(nbyte_size)) == NULL)
  { printf("JLP_WRFITS/Error when allocating memory space\n"); 
    return(-1);
  }

/* read-convert-write data      */
    if (fopt[0]=='B') {                 /* Basic FITS: 32-bit integers */
       pi = p.i;
       for(j=0; j<ny; j++)
       {
         for(i=0; i<nx; i++)
         {
           if (isNULLFLOAT(array[i + j * idim])) toNULLLONG(*pi++);
            else *pi++ = bfac * (array[i + j * idim] -  boff);
         }
       }
/* Data conversion: */
       status = cvi4(p.i,dsize,1);
       if (status) 
         printf("Fitswdm1/cvr4/Error: status=%d\n", status);
       else
          if (dwrite(p.i,nbyte_size)!=nbyte_size) status = -1;
     }
     else {                             /* Float format: nbp = -32 */
       pf = p.f;
       for(j=0; j<ny; j++)
       {
         for(i=0; i<nx; i++)
         *pf++ = array[i + j * idim]; 
       }
/* Data conversion: */
       status = cvr4(p.f,dsize,1);
       if (status) 
         printf("Fitswdm1/cvr4/Error: status=%d\n", status);
       else
          if (dwrite(p.f,nbyte_size)!=nbyte_size) status = -1;
     }

/* Free memory space */
 free(p.f);

 if (status == -1) 
   printf("Fitswdm1/Error: wrong byte-count in write to device\n");

  dbfill('\0');
  return status;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.COPYRIGHT     (c)  1990   European Southern Observatory
.IDENT         fitswkw.c
.LAUGUAGE      C
.AUTHOR        P.Grosbol   ESO/IPG
.KEYWORDS      write FITS keyword, FITS header card
.COMMENT       format FITS header card of differebt types
.VERSION       1.0  1988-Nov-18 : Creation,   PJG 
.VERSION       1.1  1990-Feb-02 : Update for hierachical keywords, PJG 
.VERSION       1.2  1990-May-02 : Correct error in string/comment, PJG 
---------------------------------------------------------------------*/

#define    MXFHC              80   /* characters in FITS header card */

static     char     fhc[MXFHC+1];  /* buffer for a FITS header card  */
static     int                 n;  /* character index in line buffer */

fitswkl(kw,hkw,hkn,no,val,com)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       create FITS header card with logical value
.RETURN        status 0: OK, else 1
---------------------------------------------------------------------*/
char          *kw;             /* IN: pointer to first keyword       */
char       *hkw[];             /* IN: array of pointer to kyywords   */
int           hkn;             /* IN: no. of hierachical keywords    */
int            no;             /* IN: sequence of keyword            */
int           val;             /* IN: integer value of keyword       */
char         *com;             /* IN: pointer to keyword comment     */
{
  kwput(kw,hkw,hkn,no);        /* write keyword labels to line       */

  fhc[n++] = '=';

  n = (n<29) ? 29 : n++;              /* write integer value to line */
  fhc[n++] = (val) ? 'T' : 'L';

  kwcom(com); 

  return 0;
}

fitswki(kw,hkw,hkn,no,val,com)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       create FITS header card with integer value
.RETURN        status 0: OK, else 1
---------------------------------------------------------------------*/
char          *kw;             /* IN: pointer to first keyword       */
char       *hkw[];             /* IN: array of pointers to  keyword  */
int           hkn;             /* IN: no. of hierachical keywords    */
int            no;             /* IN: sequence of keyword            */
int           val;             /* IN: integer value of keyword       */
char         *com;             /* IN: pointer to keyword comment     */
{
  kwput(kw,hkw,hkn,no);        /* write keyword labels to line       */

  fhc[n++] = '=';

  n = (n<20) ? 20 : n++;              /* write integer value to line */
  sprintf(&fhc[n],"%10d",val);
  n += 10; fhc[n] = ' ';

  kwcom(com); 

  return 0;
}

fitswkd(kw,hkw,hkn,no,val,com)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       create FITS header card with double value
.RETURN        status, 0: OK, else 1
---------------------------------------------------------------------*/
char          *kw;             /* IN: pointer to first keyword       */
char       *hkw[];             /* IN: array of pointers to keyword   */
int           hkn;             /* IN: no. of hierachical keywords    */
int            no;             /* IN: sequence no. of keyword        */
double        val;             /* IN: double  value of keyword       */
char         *com;             /* IN: pointer to keyword comment     */
{
  kwput(kw,hkw,hkn,no);        /* write keyword labels to line       */

  fhc[n++] = '=';

  n = (n<10) ? 10 : n++;               /* write double value to line */
  sprintf(&fhc[n],"%20.12E",val);
  n += 20; fhc[n] = ' ';

  kwcom(com); 

  return 0;
}

fitswks(kw,hkw,hkn,no,val,com)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       create FITS header card with string
.RETURN        status, 0:OK, else 1:Error
---------------------------------------------------------------------*/
char          *kw;             /* IN: pointer to first keyword       */
char       *hkw[];             /* IN: array of pointers to keyword   */
int           hkn;             /* IN: no. of hierachical keywords    */
int            no;             /* IN: sequence no. of keyword        */
char         *val;             /* IN: pointer to string of keyword   */
char         *com;             /* IN: pointer to keyword comment     */
{
  int        i,nspc;
  char       *pc;

  kwput(kw,hkw,hkn,no);        /* write keyword labels to line       */

  fhc[n++] = '='; n++;
  fhc[n++] = '\'';

  if (!val) val = "";
  i = 1; nspc = 0; pc = val;
  while (*pc) {
     if (*pc<' ' || *pc>'~') *pc = ' ';
     if (*pc!=' ') nspc = i;
     pc++; i++;
  }

  i = (nspc<8) ? 8 : nspc;
  while (n<MXFHC-1 && (0<i-- || *val))
     fhc[n++] = (*val) ? *val++ : ' ';
  fhc[n++] = '\'';

  kwcom(com); 

  return 0;
}

fitswkc(kw,com)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       create FITS header card with comment
.RETURN        status, 0:OK, else 1:Error
---------------------------------------------------------------------*/
char          *kw;             /* IN: pointer to first keyword       */
char         *com;             /* IN: pointer to keyword comment     */
{
  if (!kw) kw = "";
  for (n=0; n<8; n++) fhc[n] = (*kw) ? *kw++ : ' ';

  if (!com) com = "";
  while (n<MXFHC) fhc[n++] = (*com) ? *com++ : ' ';
  fhc[n] = '\0';

  dwrite(fhc,MXFHC);
  if (1<popt) printf("%s\n",fhc);
  return 0;
}

int kwput(kw,hkw,hkn,no)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       initiate keyword card buffer and put keyword labels
.RETURN        index number in buffer, if error 0
---------------------------------------------------------------------*/
char          *kw;             /* IN: pointer to first keyword       */
char       *hkw[];             /* IN: array of pointers to keywords  */
int           hkn;             /* IN: no. of hierachical keywords    */
int            no;             /* IN: sequence no. of keyword        */
{
  char       cn[9];
  int        i,k;

  if (0<no && no<1000000) sprintf(cn,"%d",no);  /* encode seq. no.   */
   else cn[0] = '\0';

  for (n=0; n<MXFHC;) fhc[n++] = ' ';       /* init. line with blank */
  fhc[n] = '\0';

  if (!kw) return 0;

  k = 0;
  for (n=0; n<8; n++)
      if (*kw) fhc[n] = *kw++;
       else fhc[n] = (!cn[k]) ? ' ' : cn[k++];

  if (hkw && hkn) {                        /* hierachical keywords  */
     n++; i = 8;
  }

  return n;
}

kwcom(com)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       insert comment in FITS keyword line
.RETURN        status, 0:OK, 1:no space for comment 
---------------------------------------------------------------------*/
char         *com;             /* IN: pointer to keyword comment     */
{
  if (n<30) n = 30;            /* comment in column 32 if possible   */

  if (n+3<MXFHC) {
     n++; fhc[n++] = '/'; n++;
     while (n<MXFHC && *com) fhc[n++] = *com++;
  }

  dwrite(fhc,MXFHC);
  if (1<popt) printf("%s\n",fhc);

  return 0;
}

