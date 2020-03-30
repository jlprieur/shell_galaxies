/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* jlp0_rdfits.c
* To read FITS formatted 1-D, 2-D, and 3-D image files
* Formats supported are : FITS 8,16,32,-32,-64 
* (i.e. 1-byte, 2-byte or 4-byte integer, and 4-byte or 8-byte float values)
* JLP: comments and jlp_descriptors
*
* PIC DU MIDI, April 1996: I remove direct references to MIDAS include
* files in order to make this program more robust relative to MIDAS changes
* This version has been tested on the DEC system in April 1996.
*
* Contains:
* int JLP_VM_RDFITS(pntr_array,nx1,ny1,infile,comments,jlp_descr,dflag,istatus)
* int JLP_RDFITS(array,nx1,ny1,idim,infile,comments,jlp_descr,dflag,istatus)
* int jlp0_rdfits(pntr_array,array,nx1,ny1,nz1,idim,
*                infile,comments,jlp_descr,dflag,istatus,vm_flag)
* int fitsrdm1(array,nx,ny,nz,idim)
* int fitsrhd1(nx1,ny1,nz1,comments,jlp_descr,dflag)
* int fitsckw1(htype,kw,dflag)
* int fitsrkw1(line,kw)
* (and main program to test...)
*
* JLP
* Version 06-03-96
---------------------------------------------------------------------*/
/* basic FITS definitions         */
#ifndef       FITSLR
#define       FITSLR           2880   /* Logical record size (byte) */
#endif

#define       MXS                17   /* max. length of char string */
#define       MXDIM              16   /* max. no. of dimensions     */
#define       MXPAR              64   /* max. no. of parameters     */
#define       MXMDB              72   /* max. descriptor buffer     */
#define       MXIDNT             73   /* max. identifier length     */
#define       MXHKW               8   /* max. no. of hierachical kw */

#define       NOFITS             -3   /* No FITS header             */
#define       EOFITS             -2   /* End Of FITS file           */
#define       FBFITS             -1   /* False basic FITS format    */
#define       BFITSE              0   /* Basic FITS format          */
#define       BFITS               1   /* Basic FITS format Empty    */
#define       RGROUP              2   /* Random groups format       */
#define       UKNOWN              3   /* Unknown extension          */

typedef struct {                      /* FITS keyword structure     */
		 char       kw[MXS];  /* prime FITS keyword         */
		 char   *hkw[MXHKW];  /* pointers to hierachical kw */
		 int            hkn;  /* no. of hierachical keyword */
		 int            kno;  /* number field of keyword    */
		 char           fmt;  /* format of FITS keyword     */
                                      /* Int, Real, String, Logical */
                                      /* compleX, Comment           */
		 union {
		    char     *pc;     /* pointer to string value    */
		    int        i;     /* integer value              */
		    double  d[2];     /* double precision value     */
		       }        val;  /* keyword value              */
		 char         *pcom;  /* pointer to keyword comment */
		 char   buf[MXIDNT];  /* buffer to keep strings     */
		 char   hkb[MXIDNT];  /* buffer for hierachical kw  */
               } KWORD;

typedef struct {                      /* MIDAS descriptor buffer    */
		 char     desc[MXS];  /* name of MIDAS descriptor   */
		 int            idx;  /* idex in descriptor         */
		 char          type;  /* type of MIDAS descriptor   */
                                      /* Int, Real, String, Logical */
                                      /* compleX, Comment           */
		 union {
		    char     *pc;     /* pointer to string value    */
		    int        i;     /* integer value              */
		    double  d[2];     /* double precision value     */
		       }        val;  /* keyword value              */
		 char   buf[MXIDNT];  /* buffer to keep strings     */
               } MDBUF;


typedef struct {                    /* Definition of data matrix    */
                 int        naxis;  /* no. of pixels for axis       */
                 double     crval;  /* Coordinate of ref. pixel     */
                 double     crpix;  /* Reference pixel              */
                 double     cdelt;  /* Coordinate increment         */
                 double     crota;  /* Rotation value for axis      */
                 char  ctype[MXS];  /* Type of coordinate axis      */
               } ADEF;

typedef struct {                    /* Definition of parm's block   */
                 double     pscal;  /* Scaling factor of parm.      */
                 double     pzero;  /* Zero offset for parm.        */
                 char  ptype[MXS];  /* Type of parameter            */
               } PDEF;

typedef struct {                      /* Basic data parameters      */
                 int         bitpix;  /* Bits per pixel             */
                 int          naxis;  /* no. of axes in data matrix */
                 int         pcount;  /* parameter count value      */
                 int         gcount;  /* group count value          */
                 int         kwflag;  /* flags for given keywords   */
                 int          cflag;  /* frame create flag          */
                 int          bflag;  /* flag for valid BLANK value */
                 int          blank;  /* BLANK value                */
                 int       extlevel;  /* Level no. of extension     */
                 int         extver;  /* Version no. of extension   */
                 int          sflag;  /* flag for valid scaling     */
                 double      bscale;  /* scaling factor on values   */
                 double       bzero;  /* zero offset for values     */
                 char    bunit[MXS];  /* units of values            */
                 char  extname[MXS];  /* Name of extension          */
                 char ident[MXIDNT];  /* Identifier                 */
                 ADEF         *data;  /* pointer to data def's      */
                 PDEF         *parm;  /* pointer to parm. def's     */
                 char         *extd;  /* pointer to ext-data def's  */
               } BFDEF;
typedef struct {                      /* keyword definition         */
		 char           *kw;  /* Keyword to be defined      */
		 char           *sk;  /* Second level keyword       */
		 char           fmt;  /* Keyword data format        */
		 int          group;  /* Group of keyword action    */
		 int         action;  /* action within group        */
		 char         *desc;  /* Associated descriptor      */
		 int            idx;  /* Index for value in desc.   */
		 char          type;  /* Variable type of desc.     */
               } KWDEF;


#define      NOACT          0  /* Keyword group: NO action          */
#define      WDESC          1  /* Keyword group: Write descriptor   */
#define      BFCTL          2  /* Keyword group: Basic FITS control */

#define      BITPIX         1  /* Keyword action for Basic FITS     */
#define      NAXIS          2
#define      CRVAL          3
#define      CRPIX          4
#define      CDELT          5
#define      CTYPE          6
#define      CROTA          7
#define      BSCALE         8
#define      BZERO          9
#define      BLANK         10
#define      BUNIT         11
#define      PCOUNT        12
#define      GCOUNT        13
#define      EXTNAME       14
#define      EXTVER        15
#define      EXTLEVEL      16
#define      PTYPE         17
#define      PSCAL         18
#define      PZERO         19
#define      END           20
#define      OBJECT        21

#define      TMSTART        1  /* Keyword action for descriptors    */
#define      TMEND          2

static  KWDEF  bkw[] = { 
	 {"HISTORY ","",'H',WDESC,0,"HISTORY",-1,'S'},
	 {"COMMENT ","",'C',WDESC,0,"COMMENT",-1,'S'},
	 {"        ","",'C',WDESC,0,"COMMENT",-1,'S'},
	 {"BITPIX  ","",'I',BFCTL,BITPIX,"",0,'\0'},
	 {"NAXIS",   "",'I',BFCTL,NAXIS,"",0,'\0'},
	 {"CRVAL",   "",'R',BFCTL,CRVAL,"",0,'\0'},
	 {"CRPIX",   "",'R',BFCTL,CRPIX,"",0,'\0'},
	 {"CDELT",   "",'R',BFCTL,CDELT,"",0,'\0'},
	 {"CTYPE",   "",'S',BFCTL,CTYPE,"",0,'\0'},
	 {"CROTA",   "",'R',BFCTL,CROTA,"",0,'\0'},
	 {"BSCALE  ","",'R',BFCTL,BSCALE,"",0,'\0'},
	 {"BZERO   ","",'R',BFCTL,BZERO,"",0,'\0'},
	 {"BLANK   ","",'I',BFCTL,BLANK,"",0,'\0'},
	 {"BUNIT   ","",'S',BFCTL,BUNIT,"",0,'\0'},
	 {"PCOUNT  ","",'I',BFCTL,PCOUNT,"",0,'\0'},
	 {"GCOUNT  ","",'I',BFCTL,GCOUNT,"",0,'\0'},
	 {"EXTNAME ","",'S',BFCTL,EXTNAME,"",0,'\0'},
	 {"EXTVER  ","",'I',BFCTL,EXTVER,"",0,'\0'},
	 {"EXTLEVEL","",'I',BFCTL,EXTLEVEL,"",0,'\0'},
	 {"PTYPE",   "",'S',BFCTL,PTYPE,"",0,'\0'},
	 {"PSCAL",   "",'R',BFCTL,PSCAL,"",0,'\0'},
	 {"PZERO",   "",'R',BFCTL,PZERO,"",0,'\0'},
	 {"END     ","",'C',BFCTL,END,"",0,'\0'},
	 {"OBJECT  ","",'S',BFCTL,OBJECT,"",0,'\0'},
	 {"BLOCKED ","",'L',NOACT,0,"",0,'\0'},
	 {"EXTEND  ","",'L',NOACT,0,"",0,'\0'},
	 {"GROUPS  ","",'L',NOACT,0,"",0,'\0'},
	 {"SIMPLE  ","",'L',NOACT,0,"",0,'\0'},
	 {"XTENSION","",'S',NOACT,0,"",0,'\0'},
	 {"DATE    ","",'T',WDESC,0,"DATE",1,'R'},
	 {"DATE-OBS","",'T',WDESC,0,"O_TIME",1,'D'},
	 {"ORIGIN  ","",'S',WDESC,0,"ORIGIN",1,'S'},
	 {"TELESCOP","",'S',WDESC,0,"TELESCOP",1,'S'},
	 {"OBSERVER","",'S',WDESC,0,"OBSERVER",1,'S'},
	 {"INSTRUME","",'S',WDESC,0,"INSTRUME",1,'S'},
	 {"TM-START","",'R',WDESC,TMSTART,"O_TIME",5,'D'},
	 {"TM-END  ","",'R',WDESC,TMEND,"O_TIME",7,'D'},
	 {"EXPTIME ","",'R',WDESC,0,"O_TIME",7,'D'},
	 {"POSTN-RA","",'R',WDESC,0,"O_POS",1,'D'},
	 {"POSTN-DE","",'R',WDESC,0,"O_POS",2,'D'},
	 {"AIRMASS ","",'R',WDESC,0,"AIRMASS",1,'R'},
	 {"DATAMIN ","",'R',WDESC,0,"LHCUTS",3,'R'},
	 {"DATAMAX ","",'R',WDESC,0,"LHCUTS",4,'R'},
	 {"EPOCH   ","",'R',WDESC,0,"O_POS",4,'D'},
	 {"AUTHOR  ","",'S',WDESC,0,"AUTHOR",1,'S'},
	 {"REFERENC","",'S',WDESC,0,"REFERENC",1,'S'},
	 {"RADECSYS","",'S',WDESC,0,"RADECSYS",1,'S'},
	 {"EQUINOX ","",'R',WDESC,0,"O_POS",3,'D'},
	 {"MJD-OBS ","",'R',WDESC,0,"O_TIME",4,'D'},
         {(char *) 0,"",'\0',0,0,"",0,'\0'} };
/*********************************************************************/
#include   <jlp_ftoc.h>
#include   <computer.h>

/*
#define DEBUG 1
*/

/* Options (declared as extern elsewhere)*/
int                mfd;                 /* MIDAS file descriptor     */
int               mfdt;                 /* MIDAS file desc. (table)  */
int               popt;                 /* print option              */
char            opt[4];                 /* general option flags      */

/* Main program to test JLP_RDFITS */
/*
#define MAIN_TEST 1
*/
#ifdef MAIN_TEST
main()
{
  float        array[80000];
  long int     nx1, ny1, idim;
  char         infile[60], outfile[60];
  char         comments[81], jlp_descr[1024];
  long int     istatus, dflag;

JLP_INQUIFMT();

  printf(" jlp0_rdfits to read FITS files on disk\n");
  printf(" Version 08-01-93\n"); 

      printf(" Input FITS file   : ");
      scanf("%s",infile);
      printf(" Output MIDAS file : ");
      scanf("%s",outfile);

#if DEBUG
    printf(" Input FITS file   : >%s< \n",infile);
    printf(" Output MIDAS file : >%s< \n",outfile);
#endif

idim = 256; dflag = -1;
JLP_RDFITS(array,&nx1,&ny1,&idim,infile,comments,jlp_descr,&dflag,&istatus);
#if DEBUG
 printf(" JLP_RDFITS/istatus = %d \n",istatus);
#endif

#if DEBUG
 printf(" nx = %d, ny = %d \n",nx1,ny1);
 printf(" comments: %s \n",comments);
#endif
 dflag = 0;
 JLP_WRMIDAS(array,&nx1,&ny1,&nx1,outfile,comments,jlp_descr,&dflag,&istatus);
#if DEBUG
 printf(" JLP_WRMIDAS/istatus = %d \n",istatus);
#endif

JLP_END();
}
#endif

/**********************************************************************
* JLP_VM_RDFITS
*
* dflag = -1 (no error (warning) messages and no descriptors) 
*       = 0 (no descriptors) 
*       = 1 (descriptors) 
**********************************************************************/
int JLP_VM_RDFITS(long *pntr_array, long *nx1, long *ny1, char *infile,
                  char *comments, char *jlp_descr, long *dflag, long *istatus)
{
long int vm_flag, idim, istat, nz1;
float *array;

vm_flag = 1; nz1=0;
istat = jlp0_rdfits(pntr_array,array,nx1,ny1,&nz1,&idim,
    infile,comments,jlp_descr,dflag,istatus,&vm_flag);

return(istat);
}
/**********************************************************************
* JLP_RDFITS
*
* dflag = -1 (no error (warning) messages and no descriptors) 
*       = 0 (no descriptors) 
*       = 1 (descriptors) 
**********************************************************************/
int JLP_RDFITS(float *array, long *nx1, long *ny1, long *idim,
               char *infile, char *comments, char *jlp_descr,
               long *dflag, long *istatus)
{
long int vm_flag, istat, nz1;
long int pntr_array;

vm_flag = 0; nz1=0;
istat = jlp0_rdfits(&pntr_array,array,nx1,ny1,&nz1,idim,
    infile,comments,jlp_descr,dflag,istatus,&vm_flag);

return(istat);
}
/**********************************************************************
* jlp0_rdfits
*
* dflag = -1 (no error (warning) messages and no descriptors) 
*       = 0 (no descriptors) 
*       = 1 (descriptors output to screen) 
**********************************************************************/
int jlp0_rdfits(pntr_array,array,nx1,ny1,nz1,idim,
                infile,comments,jlp_descr,dflag,istatus,vm_flag)
long int     *pntr_array, *nx1, *ny1, *nz1, *idim, *dflag, *istatus, *vm_flag;
char infile[], comments[], jlp_descr[];
float *array;
{
  char         filename[61], *pcc;
  int          fmt,num,type,nval,dsize,nz;
  char         devt;
  long int     istat;

*istatus = 0;

   strncpy(filename,infile,40);
   filename[40]='\0';
/* Check input characters strings (pb if fortran...) */
   pcc = filename;
   while(*pcc && *pcc != ' ') pcc++;
   if(*pcc == ' ') *pcc = '\0';

/* Check filename syntax and add ".fits" if no extension is present: */
   pcc = filename;
   while(*pcc && *pcc != '.') pcc++;
   if(*pcc != '.') strcpy(pcc,".fits");
   
/* Select standard options according to dflag:
 dflag <= 0 (no descriptors) 
 dflag > 0 (descriptors) 
  */
   if(*dflag > 0)
   {
     popt = 2;          /* print  */
     opt[0] = 'F';      /* Full print */
   }
   else
   {
     popt = 0;          /* No print  */
     opt[0] = 'S';      /* Short print */
   }
   opt[1] = 'O';      /* Original format    */
   opt[2] = 'Y';      /* Yes-history */


/* Then DEBUG (which has higher priority): */
#if DEBUG
     popt = 2;          /* print  */
     opt[0] = 'F';      /* Full print */
#endif


/* disable SC-errors        */
  num = 1; nval = 0;
  SCECNT("PUT",&num,&nval,&nval);  

   devt = 'S';
/* JLP96: #define  READ         0
   if ((istat = dopen(filename,READ,devt,0)) < 0) {
*/
   if ((istat = dopen(filename,0,devt,0)) < 0) {
     printf("jlp0_rdfits/dopen; Cannot open input file : >%s< \n",filename);
     *istatus = -2; return(-1);
   }                     /* open file  */
/* Initialize output midas units (mfd for file, and mfdt for table) */
   mfd = -1; mfdt = -1;
/* JLP96: #define     FITS         1             
   fmt = FITS;
*/
   fmt = 1;

/* Initialize buffers: */
   fmt=drinit();

/* decode header    */
    if (istat = fitsrhd1(nx1,ny1,&nz,comments,jlp_descr,*dflag)) {   
          printf("NOT supported FITS format! istat = %d\n",istat);
          *istatus = -1; dclose(); return(-1);
        }

/* Correction for one-dimensional images, if ny1 = 0 */
   if(*nx1 <= 0)
       {
          printf(" JLP_RDFITS/Fatal error: nx = %d \n",*nx1);
          *istatus = -2; dclose(); return(-2);
       }

    if(*ny1 <= 0) *ny1 = 1;

/* In the case of 3-D images, keep the old value of nz, otherwise set it to 0: */
    if(nz == 0 )
      {
         if(*nz1 != 0)
         printf(" JLP_RDFITS/Error: number of planes is nz = %d  while nz1 = %d \n",
               nz,*nz1);
       *nz1 = 0;
      }

/* Allocate memory space if needed: */
    if(*vm_flag)
    {
    dsize = *nx1 * *ny1 * sizeof(float);
    array = (float *) malloc(dsize);
    *idim = *nx1;
    }

/* Check if size of image is consitent with buffer: */
     if(*nx1 > *idim)
       {
          printf(" JLP_RDFITS/Fatal error: input idim is smaller than nx \n");
          *istatus = -2;
       }

/* Read data:    */
       else
          *istatus = fitsrdm1(array,*nx1,*ny1,*nz1,*idim);

    dclose();

*pntr_array = (long int)array;
return(*istatus);
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.COPYRIGHT     (c)  1990  European Southern Observatory
.IDENT         fitsrdm.c
.LAUGUAGE      C
.AUTHOR        P.Grosbol   ESO/IPG
.KEYWORDS      FITS data matrix, decode, read
.COMMENT       read the prime data matrix of FITS file
.VERSION       1.0  1988-Dec-10 : Creation,   PJG 
.VERSION       1.1  1989-Oct-08 : Save Group parm. in table,   PJG 
.VERSION       1.2  1989-Oct-24 : Check of data to store,   PJG 
.VERSION       1.3  1990-Feb-04 : Change call-seq. for cv-routine, PJG 

nz : z index of the image to be read (when 3-D image file)
---------------------------------------------------------------------*/

#define    MXFB              2880  /* max. size of scaling buffer    */

/* parm's of basic FITS header    */
/* JLP96:
extern     BFDEF           bfdef;  
*/
BFDEF           bfdef;  

int fitsrdm1(array,nx,ny,nz,idim)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       Read prime data matrix in FITS
.RETURN        error code - 0: OK, -1: error
---------------------------------------------------------------------*/
int nx, ny, nz, idim;
float     array[];
{
  register int  bf, i, j, kk;
  unsigned char uc;
  int       dsize;                    /* size of data matrix (bytes)    */
  short         s;
  int           dno,gno,pno,npix,n, nskip;
  int           dn,pn,ndata,dfmt,nb,pcnt;
  long          felem,l;
  float         *pf;
  double        fac,zero,d,*pd;
  PDEF          *pp;
  char buffer[81];
  union { 
	  float    f[2*MXFB];
	  double     d[MXFB];
        } buf;
  union { 
	  unsigned char   *b;
	  short           *s;
	  long            *l;
	  float           *f;
	  double          *d;
        } p;

  pno = bfdef.pcount; pcnt = 0; kk=0;              /* initiate counters     */
  pp = bfdef.parm;
  gno = 0;
  bf = bfdef.bflag;
/* Number of bits per pixel: */
  dfmt = bfdef.bitpix;
  nb = (dfmt<0) ? -dfmt/8 : dfmt/8;
/* Size of matrix in bytes: */
  dsize = nx * ny * nb;
  ndata = dsize/(nb*bfdef.gcount) - pno; dno = ndata;
  fac = bfdef.bscale; zero = bfdef.bzero;
  felem = 1;

/* Skip data if 3-D image, to get to the beginning of the selected
   image
*/
    for(i = 0; i < nz; i++)
     {
     nskip = nx * ny * nb;
     if ((n=dread(&p.b,nskip)) != nskip) 
       {  
       printf("fitsrdm1/Error: unexpected EOF skipping image #%d \n",i);
       return -3;
       }
    }

/* Now read selected image: */
  while (dsize > 0) {                 /* read all data in prime matrix  */
    if ((n = dread(&p.b,FITSLR)) != FITSLR) {  /* read next data record  */
       printf("fitsrdm1/Error: unexpected EOF reading image data \n");
/* JLP debug94: */
       printf(" Last number of input values read: n=%d, FITSLR=%d\n",n,FITSLR);
       printf(" Remaining number of values to be read: dsize/nb = %d\n",dsize/nb);
/*
       printf(" Do you want me to fill the gap with zeroes? (N)\n");
       gets(buffer); 
*/
       printf(" fitsrdm1/ I fill the gap with zeroes.\n");
       buffer[0] = 'y';
       if(buffer[0] == 'y' || buffer[0] == 'Y')
         {
         for (i=0; i<(dsize/nb); i++) array[i] = 0.; 
         n = 0; dsize = 0; /* To neutralize next decoding loops... */
         }
       else
         return -3;
    }
    if (dsize < n) n = dsize;
    dsize -= n;                     /* decrement remaining bytes      */
    switch (dfmt) {                /* convert to local data format   */
       case   8 : npix = n;
                  break;
       case  16 : npix = n/2; cvi2(p.s,npix,0);
                  break;
       case  32 : npix = n/4; cvi4(p.l,npix,0);
                  break;
       case -32 : npix = n/4; cvr4(p.f,npix,0);
                  break;
       case -64 : npix = n/8; cvr8(p.d,npix,0);
                  break;
    }
    do {                           /* scale all values if needed     */
       if (!pno && 0<dno && npix) {    /* decode data values            */
	  dn = (dno<npix) ? dno : npix;
	  dno -= dn; npix -= dn;
	     switch (dfmt) {
		case   8 : pf = buf.f; uc = bfdef.blank;
			   for (i=0; i<dn; i++, pf++, p.b++) {
                               if (bf && *p.b==uc) toNULLFLOAT(*pf);
                                  else *pf = fac*(*p.b) + zero;
                           }
	                   break;
		case  16 : pf = buf.f; s = bfdef.blank;
			   for (i=0; i<dn; i++, pf++, p.s++) {
                               if (bf && *p.s==s) toNULLFLOAT(*pf);
                                  else *pf = fac*(*p.s) + zero;
                           }
	                   break;
		case  32 : pf = buf.f; l = bfdef.blank;
			   for (i=0; i<dn; i++, pf++, p.l++) {
                               if (bf && *p.l==l) toNULLFLOAT(*pf);
                                  else *pf = fac*(*p.l) + zero;
                           }
	                   break;
		case -32 : pf = buf.f;
			   for (i=0; i<dn; i++, pf++)
                               *pf = fac * (*(p.f++)) + zero;
	                   break;
		case -64 : pf = buf.f;
			   for (i=0; i<dn; i++, pf++)
                               *pf = fac * (*(p.d++)) + zero;
	                   break;
	     }
/* store data in MIDAS file
*******     SCFPUT(mfd,felem,dn,buf.f);  
*/
/* Copy to output array: */
             for (i=0; i<dn; i++, kk++)
                 array[kk] = buf.f[i]; 
            
          felem += dn;
          if (!dno) {
             gno++;
             pno = bfdef.pcount; pcnt = 0;
             pp = bfdef.parm;
             dno = ndata;
          }
       }
    } while (npix && gno<bfdef.gcount);
  }

/* Rearrange array (because of idim, array may be not compact...) */
  if(idim != nx)
  {
    kk=nx*ny;
    for (j=ny-1; j>=0; j--)
    {
      for (i=nx-1; i>=0; i--)
      {
      kk--; array[i + j * idim] = array[kk];
      }
    }
  }
  return(0);
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.COPYRIGHT     (c)  1990   European Southern Observatory
.IDENT         fitsrhd.c
.LAUGUAGE      C
.AUTHOR        P.Grosbol   ESO/IPG
.KEYWORDS      FITS header, decode, transport format
.COMMENT       Both main and extension headers are decoded.
.VERSION       1.0  1988-Dec-10 : Creation,   PJG 
.VERSION       1.1  1989-Jul-05 : Include more data type in tables, PJG 
.VERSION       1.2  1989-Aug-24 : Change .BDF data formats, PJG 
.VERSION       1.3  1989-Oct-23 : Create table for RGROUP, PJG 
.VERSION       1.35 1990-Mar-08 : Change CUNIT desc. to C*1, PJG 
.VERSION       1.4  1990-Mar-21 : Check file data format, PJG 
.VERSION       1.41 1990-Apr-11 : Correct error in file rename, PJG 
---------------------------------------------------------------------*/

#define    DUMMY "middumma.bdf"      /* Name of dummy file           */

BFDEF                    bfdef;      /* definitions of basic FITS    */
int                       nmdb;      /* entries in keyword buffer    */
MDBUF               mdb[MXMDB];      /* MIDAS descriptor buffer      */

typedef struct {                     /* one FITS header line         */
                  char   c[80];
                } LINE;

int fitsrhd1(nx1,ny1,nz1,comments,jlp_descr,dflag)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       Decode FITS header
.RETURN        type of FITS header, -1: error
 JLP Version 26-06-94
---------------------------------------------------------------------*/
long int  *nx1, *ny1, *nz1, dflag;
char comments[], jlp_descr[];
{
  int      ktype,htype,i,n,nl,nx,nfz,nb,idescr;
  int      ftype,dtype,na[MXDIM];
  double   fa[MXDIM];
  LINE     *pl;
  KWORD    kw;
  ADEF     *ad;
  PDEF     *pm;

  htype = NOFITS; ktype = 1; nl = 0; nb = 0;
  nmdb = 0; nfz = 0;

  bfdef.cflag = -1; bfdef.bflag = 0; bfdef.sflag = 0;
  bfdef.kwflag = 0;
  bfdef.naxis = 0; bfdef.pcount = 0; bfdef.gcount = 1;
  for (n=0; n<MXIDNT; n++) bfdef.ident[n] = ' ';
  bfdef.ident[MXIDNT-1] = '\0';
/* Line index of jlp_descr: */
  idescr = 0;

  do {                                    /* go through FITS header  */
     if ((n=dread(&pl,FITSLR)) != FITSLR) { /* read FITS header blocks */
        printf("fitsrhd1/error reading header (error -3) \n");
	return -3;
     }
     nb++;
     for (n=0; n<36 && ktype; n++, pl++) {    /* decode the 36 lines */
         nl++;
         fitsrkw1(pl->c,&kw);           /* decode single keyword line */
         if (nl<5) {                   /* check type of FITS header  */
            if ((htype=fitsthd(nl,&kw))<FBFITS) 
                {
                printf("fitsrhd1/error decoding header (error -2) \n");
                return -2;
                }
	 }
/* If print option, displays current line */
/*
         if (1<popt) { pl->c[79] = '\0'; printf(" %s\n",pl->c); }
*/
         ktype = fitsckw1(htype,&kw,dflag);         /* decode keyword       */
/* jlp_descriptors: */
         if(!strcmp((&kw)->kw,"HISTORY "))
           {
#if DEBUG
           printf(" OK descriptor found\n"); 
           printf(" value>%s< \n",kw.val.pc); 
#endif
           strcpy((jlp_descr + idescr * 62),kw.val.pc);
           idescr++;
           }

         if (0<bfdef.cflag) {                /* create MIDAS frame   */
            switch (htype) {
               case BFITS  :
/* compute total size in byte */
                    nfz = 1;                    
                    nx = bfdef.naxis;
                    for (i=0; i<nx; i++) nfz *= bfdef.data[i].naxis;
                    nfz = (bfdef.naxis) ?
			  (nfz + bfdef.pcount) * bfdef.gcount : 0;
/* don't create empty file  */
                    if (!nfz) 
                      {
                      printf("fitsrhd1/error: empty file (error -4) \n");
                      return -4;
                      }

/* Other parameters: */
		    *nx1 = bfdef.data[0].naxis;
		    *ny1 = bfdef.data[1].naxis;
		    *nz1 = bfdef.data[2].naxis;
                    break;
               default     :
                    printf("fitsrhd1/error: unknown format (error -1) \n");
		    return -1;
            }
            bfdef.cflag = -1;
         }
     }
  } while (0<ktype);

/* Copy "ident" to "comments" if file sucessfully read: */
  if (nfz)
  {
   bfdef.ident[79]='\0';
   strcpy(comments,bfdef.ident);
  }

#ifdef DEBUG
   printf(" fitsrhd1/Comments written: >%s< \n",comments);
   printf(" fitsrhd1/Descriptors : >%s< \n",jlp_descr);
#endif

/* Case of 3-D objects: */
if(*nz1 > 0 && dflag >= 0)
  printf("fitsrhd1/Warning: 3_D image file with  nz = %d \n",*nz1);

  return 0;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.COPYRIGHT     (c)  1990  European Southern Observatory
.IDENT         fitsckw.c
.LAUGUAGE      C
.AUTHOR        P.Grosbol   ESO/IPG
.KEYWORDS      FITS, check keyword, classify
.COMMENT       classify a FITS keyword
.VERSION       1.0  1988-Dec-10 : Creation,   PJG 
.VERSION       1.1  1989-Feb-24 : Upgrade for B-tables,   PJG 
.VERSION       1.2  1989-Apr-14 : Omit wrong keywords,   PJG 
.VERSION       1.3  1989-Sep-28 : Correct string type 'C'->'S', PJG 
.VERSION       1.4  1989-Oct-17 : Convert real to int in KEYWORD, PJG 
.VERSION       1.45 1989-Oct-23 : Upgrade for RGROUPS format, PJG 
.VERSION       1.50 1989-Dec-21 : Check FORTRAN formats, PJG 
.VERSION       1.60 1990-Feb-05 : Data format conversion of keyword, PJG 
.VERSION       1.61 1990-Feb-15 : Include exposure time, PJG 
.VERSION       1.7  1990-Feb-26 : Restructure MIDAS desc. decode, PJG 
.VERSION       1.8  1990-Mar-21 : Remove special char. in names, PJG 
---------------------------------------------------------------------*/

#define    MXF           512     /* max. no. of fields in table      */

ADEF             adef[MXDIM];    /* definition of data matrix axes   */
/* JLP96: pdef renamed ppdef since pb with SUN compiler
* and confusion with pdef defined in jlp_splot.a ...
*/
PDEF             ppdef[MXPAR];    /* definition of groups parameters  */
/* JLP98
*/
/*
FDEF               fdef[MXF];   
*/
/* definition of table fields       */

int fitsckw1(htype,kw,dflag)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       classify and store FITS keyword
.RETURN        keyword type   0:END, -1:not found, -2:error
                              1: Inconsistent data types (Warning)
---------------------------------------------------------------------*/
int          htype;                /* type of FITS header            */
int          dflag;                /* Talks if positive or null */
KWORD          *kw;                /* pointer to keyword structure   */
{
  char       c,*ps,*pc,line[80];
  int        ktype,n,m,i,k,found;
  float      f;
  double     d;
  KWDEF      *kwd,ndkw;

  kwd = bkw; found = 0; ktype = -1;
  while (kwd->kw) {               /* compare with basic keyword list */
     if (found = kwcmp(kw->kw,kwd->kw)) break;
     kwd++;
  }
  if (!found) {                   /* not found - check other lists   */
     kwd = (KWDEF *) 0;
     if (kwd)  
	while (kwd->kw) {         /* compare with other lists        */
           if (found = kwcmp(kw->kw,kwd->kw)) break;
           kwd++;
        }
     if (!found) {                /* not found in any list           */
        ndkw.kw = kw->kw;
        ndkw.fmt = 'N'; ndkw.group = WDESC;
        ndkw.action = 0; ndkw.desc = kw->kw;
        ndkw.idx = 1; 
        switch (kw->fmt) {
	   case 'L'  :
           case 'I'  : ndkw.type = 'I'; break;
           case 'X'  :
           case 'R'  : ndkw.type = 'D'; break;
           case 'C'  :
           case 'S'  : ndkw.type = 'S'; break;
        }
	kwd = &ndkw;
     }
  }
  if (kwd->group==WDESC &&               /* skip blank keyword cards */
      kw->fmt=='C' && !(*kw->val.pc)) return 2;

  if (fitstkw1(kw,kwd->fmt)) {            /* convert data format      */
/* Print error message if dflag is positive or null */
     if(dflag >= 0) 
        printf("fitsckw1/Error: Inconsistent data types [%c-%c] for >%s< ! \n",
                kwd->fmt,kw->fmt,kw->kw);
     return 1;
  }
  ktype = 1;

  switch (kwd->group) {           /* goto keyword group for action   */
     case NOACT : break;
     case BFCTL :
          if (kw->kno && bfdef.naxis<kw->kno) {
             ktype = -2; break;
          }
          n = kw->kno - 1;
          switch (kwd->action) {
             case BITPIX   : 
                  bfdef.bitpix = kw->val.i;
                  bfdef.data = adef;
                  bfdef.parm = ppdef;
                  bfdef.extd = (char *) 0;
                  break;
             case NAXIS    :
                  if (n<0) {
                     bfdef.naxis = kw->val.i;
                     if (opt[1]!='N' && htype<UKNOWN) bfdef.cflag = 0;
                     if (MXDIM<bfdef.naxis) {
		        printf("fitsckw1/Max. NAXIS exceeded!");
		        return -2;
                     }
		     bfdef.bscale = 1.0; bfdef.bzero = 0.0;
		     bfdef.bunit[0] = '\0';
		     bfdef.pcount = 0; bfdef.gcount = 1;
		     for (i=0; i<MXDIM; i++) {
			 adef[i].crval = 1.0; adef[i].crpix = 1.0;
			 adef[i].cdelt = 1.0; adef[i].crota = 0.0;
			 adef[i].naxis = 0; adef[i].ctype[0] = '\0';
                     }
                  }
                  else {
                     if (htype==RGROUP) n--;
                     adef[n].naxis = kw->val.i;
                  }
             case CRVAL    :
                  if (htype==RGROUP) n--;
                  adef[n].crval = kw->val.d[0];
                  break;
             case CRPIX    :
                  if (htype==RGROUP) n--;
                  adef[n].crpix = kw->val.d[0];
                  break;
             case CDELT    :
                  if (htype==RGROUP) n--;
                  adef[n].cdelt = kw->val.d[0];
                  break;
             case CTYPE    :
	          if (htype==RGROUP) n--;
		  pc = kw->val.pc; ps = adef[n].ctype; i = MXS;
		  while (--i && (*ps++ = *pc++)); *ps = '\0';
                  break;
             case CROTA    :
	          if (htype==RGROUP) n--;
                  adef[n].crota = kw->val.d[0];
                  break;
             case BSCALE   :
                  bfdef.bscale = kw->val.d[0];
                  bfdef.sflag = bfdef.sflag || (bfdef.bscale != 1.0);
                  break;
             case BZERO    :
                  bfdef.bzero = kw->val.d[0];
                  bfdef.sflag = bfdef.sflag || (bfdef.bzero != 0.0);
                  break;
             case BUNIT    :
		  pc = kw->val.pc; ps = bfdef.bunit; i = MXS;
		  while (--i && (*ps++ = *pc++)); *ps = '\0';
                  break;
             case BLANK    :
                  bfdef.blank = kw->val.i; bfdef.bflag = 1;
                  break;
             case PCOUNT   :
                  bfdef.pcount = kw->val.i;
                  bfdef.kwflag |= 1;
                  if (MXPAR<bfdef.pcount) {
		     printf("fitsckw1/Error: Max. PCOUNT exceeded!");
		     return -2;
                  }
	          for (i=0; i<bfdef.pcount; i++) {
	              ppdef[i].pscal = 1.0; ppdef[i].pzero = 0.0;
	              ppdef[i].ptype[0] = '\0';
		  }
                  break;
             case GCOUNT   :
                  bfdef.gcount = kw->val.i;
                  bfdef.kwflag |= 2;
                  if (1<bfdef.gcount) {
		     if (htype!=RGROUP) bfdef.naxis++;
                     adef[bfdef.naxis-1].naxis = bfdef.gcount;
                  }
                  break;
             case PTYPE    :
		  pc = kw->val.pc; ps = ppdef[n].ptype; i = MXS;
		  while (--i && (*ps++ = *pc++)); *ps = '\0';
                  break;
             case PSCAL    :
                  ppdef[n].pscal = kw->val.d[0];
                  break;
             case PZERO    :
                  ppdef[n].pzero = kw->val.d[0];
                  break;
             case EXTNAME  :
		  pc = kw->val.pc; ps = bfdef.extname; i = MXS;
		  while (--i && (*ps++ = *pc++)); *ps = '\0';
                  break;
             case OBJECT   :
		  pc = kw->val.pc; ps = bfdef.ident; i = MXIDNT;
		  while (--i && (*ps++ = *pc++)); *ps = '\0';
                  break;
             case EXTVER   :
                  bfdef.extver = kw->val.i;
                  break;
             case EXTLEVEL :
                  bfdef.extlevel = kw->val.i;
                  break;
             case END      : 
		  ktype = 0;
		  break;
             default       : 
                  printf("fitsckw1/Warning: Undef. basic action");
          }
  }

  if (!bfdef.cflag)               /* check if data file can be created */
     switch (htype) {
        case BFITS  : if (kwd->action==NAXIS && kw->kno==bfdef.naxis) 
                          bfdef.cflag = 1;
             break;
        case RGROUP : if ((bfdef.kwflag & 3) == 3) bfdef.cflag = 1;
             break;
     }
  return ktype;
}

kwcmp(pk,ps)
char  *pk,*ps;
{
  if (!pk || !ps) return 0;
  while (*ps && (*pk++ == *ps)) ps++;
  return !(*ps);
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.COPYRIGHT     (c)  1990   European Southern Observatory
.IDENT         fitsrkw1.c
.LAUGUAGE      C
.AUTHOR        P.Grosbol   ESO/IPG
.KEYWORDS      FITS, decode, keyword
.COMMENT       decode a keyword line in a FITS header
.VERSION       1.0  1988-Dec-10 : Creation,   PJG 
.VERSION       1.1  1990-Jan-02 : Decode hierachical keywords, PJG 

 JLP
 Version 28-06-94
---------------------------------------------------------------------*/
int fitsrkw1(line,kw)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       Decode keyword in FITS header
.RETURN        status 0:OK, -1:Illegal keyword syntax
---------------------------------------------------------------------*/
char         *line;                /* pointer to start of keyword    */
KWORD          *kw;                /* pointer to keyword structure   */
{
  char                 c,*pc;
  int         idx,n,err,no,i;
  double                   d;

/* Initializes keyword structure: */
  kw->hkw[0] = (char *) 0; kw->hkn = 0; kw->fmt = '?';
  kw->pcom = (char *) 0; kw->buf[0] = '\0';
  kw->val.d[0] = 0.0; kw->val.d[1] = 0.0;
  idx = 0; n = 0; err = 0; i = 0; no = 0; d= 0.0;

  while (idx++<8) {             /* check and transfer prime keyword */
      c = *line++;
      if (' '<=c && c<='`') {              /* legal character       */
         if ('0'<=c && c<='9') {           /* number - decode it !  */
            no++; i = 10*i + (c-'0');
         }
         else if (c!=' ') {
            i = 0; no = 0;
         }
      }
      else err = 1;                        /* illegal character     */
      kw->kw[n++] = c;
  }
  kw->kw[8] = '\0';
/* Now kw->kw has been loaded with keyword */
  kw->kno = (no && i) ? i : 0;             /* keyword index         */
  if (err) return err;

  if (*line!='=') {                    /* check if leveled keywords */
     n = 0; i = idx; pc = line;
/* Move to starting position (jumps "   keyword_1stpart keyword_2ndpart "): */
     while (i<80 && *pc==' ') { pc++; i++; }
     while (i<80 && *pc!=' ') { pc++; i++; }
     while (i<80 && *pc==' ') { pc++; i++; }
     while (i<80 && *pc!=' ' && *pc!='=') { pc++; i++; }
     while (i<80 && *pc == ' ') { i++; pc++; }

/* From this location should find "=" */
     if (*pc!='=' ||                   /* No_leveled keyword found */
         !strcmp(kw->kw,"HISTORY ") ||     /* or history or comment keyword */
         !strcmp(kw->kw,"COMMENT ") ||
         !strcmp(kw->kw,"        ")) {
        kw->hkw[0] = (char *) 0; kw->hkn = 0;
/* Format is C (character) */
        kw->fmt = 'C';
        kw->pcom = kw->buf;
        n = 0;
        while (idx++<80) 
           kw->buf[n++] = ((c = *line++)<' ' || '~'<c) ? ' ' : c;
        kw->buf[n] = '\0';
	n = 0;
        while (kw->buf[n]==' ') n++;
	kw->val.pc = &(kw->buf[n]);
/* From now on kw->val.pc holds the current value */
        return err;
     }
  }

  idx++; line++;                           /* decode keyword value */
  while (*line==' ') {                     /* first char in value  */
     if (79<idx++) return -1;
     line++;
  } 

  n = 0;
  if ((c = *line)=='\'') {          /* determine type of parameter */
/* Format is S (string) */
     kw->fmt = 'S';                            /* character string */
     line++;
     while (idx<80 && *line!='\'') {
        kw->buf[n++] = *line++; idx++;
     }
     kw->buf[n++] = '\0';
     kw->val.pc = kw->buf;
  }
  else if (c=='T' || c=='F') {                 /* logical value    */
/* Format is L (logical) */
     kw->fmt = 'L';
     kw->val.i = *line=='T';
  }
  else if (c=='+' || c=='-' || c=='.' ||
	   c=='e' || c=='E' || c=='d' || c=='D' ||
	   ('0'<=c && c<='9')) {               /* numeric value    */
     no = getval(line,72,&i,&d);
     while (no--) { idx++; line++; }
     if (*line!=' ' && *line!='/') err = 1;
     if (i) { kw->val.i = d; kw->fmt = 'I'; }
     else { kw->val.d[0] = d; kw->fmt = 'R'; }
  }
  else {                                       /* blank field      */
     kw->val.d[0] = 0.0; kw->val.d[1] = 0.0;
/* Format is ? (unknown) */
     kw->fmt = '?';
  }

  while (idx++ < 80 && *line++ != '/');            /* find comment */
  while (idx<80 && *line==' ') { line++; idx++; }
  kw->pcom = &kw->buf[n];
  while (idx++<80) 
     kw->buf[n++] = ((c = *line++)<' ' || '~'<c) ? ' ' : c;
  kw->buf[n] = '\0';
  while (n && kw->buf[--n]==' ') kw->buf[n] = '\0';

  return err;
}
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.COPYRIGHT     (c)  1990  European Southren Observatory
.IDENT         fitstkw.c
.LAUGUAGE      C
.AUTHOR        P.Grosbol   ESO/IPG
.KEYWORDS      FITS keywords, data type ckeck, conversion
.VERSION       1.0  1990-Feb-15 : Creation,   PJG 
.VERSION       1.1  1990-Mar-19 : Error on unknown type,   PJG 
---------------------------------------------------------------------*/
int fitstkw1(kw,fmt)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       perform data type/format check of FITS keyword and
               convert data format if possible.
.RETURN        status  0:OK, -1: error - incompatible data types
---------------------------------------------------------------------*/
KWORD      *kw;                 /* pointer to FITS keyword structure */
char       fmt;                 /* expected data format of keyword   */
{
  double   d,dateymd();
  int      k,i,m,n;
  char     *pc,text[80];

  if (kw->fmt == fmt) return 0;             /* expected type - OK    */
  if (kw->fmt == '?') return -1;            /* unknown type - error  */

  switch (fmt) {
     case 'H'  :                       /* Histiry keyword card       */
     case 'C'  :                       /* Comment keyword card       */
     case 'N'  :                       /* Not defined data type      */
     case '\0' : return  0;            /* data type not checked - OK */
     case 'I'  : if (kw->fmt == 'R') {    /* convert real -> int     */
                    d = kw->val.d[0];
                    kw->val.i = d; kw->fmt = 'I';
                    printf(
                     "Warning: Keyword >%s< truncated to int!",kw->kw);
                    return 0;
		  }
                  break;
     case 'R'  :
     case 'D'  : if (kw->fmt == 'I') {   /* convert int -> real     */
                    d = kw->val.i; 
                    kw->val.d[0] = d; kw->fmt = 'R';
                    return 0;
		 }
                 break;
     case 'T'  : if (kw->fmt == 'S') {   /* convert string -> time  */
                    pc = kw->val.pc;
                    if (!getint(pc,4,&k,&n)) break;
                    pc += 3; getint(pc,4,&k,&i);
                    pc += 3; getint(pc,4,&k,&m); if (m<100) m += 1900;
                    kw->fmt = 'R'; kw->val.d[0] = dateymd(m,i,n);
                    return 0;
                 }
                 else if (kw->fmt == 'R') return 0;
                 break;
     default   : return -1;
  }

  return -1;
}

static int                 htype;  /* present header type            */
static int                 exthd;  /* flag for extension header      */
static int                   nax;  /* no. of axes in data matrix     */

int fitsthd(lno,kw)
/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE       Check format and type of FITS header
.RETURN        type of FITS header (see fitsdef.h), -2: error
---------------------------------------------------------------------*/
int           lno;                 /* line no. of keyword in header  */
KWORD         *kw;                 /* pointer to keyword structure   */
{
  int            ok,n;

  switch (lno) {                /* check position dependent keywords */
       case  1 :                             /* first header line    */
                 htype = -2;
                 if (kwcomp(kw->kw,"SIMPLE  ")) {    /* main header  */
                    exthd = 0; htype = FBFITS;
                    if (kw->fmt=='L' && kw->val.i) htype = BFITS;
                 }
                 break;
       case  2 :                             /* second header line   */
                 ok = 0;
                 if (kwcomp(kw->kw,"BITPIX  "))
                    if (kw->fmt=='I')
                       switch (htype) {
                          case BFITS  : if ((n=kw->val.i)==32 ||
                                             n==16 || n==-32 ||
                                             n==8 || n==-64) ok = 1;
                                        break;
                          case UKNOWN : ok = 1;
                                        break;
                       }
                 if (!ok) htype = FBFITS;
                 break;
       case  3 :                             /* third header line    */
                 if (!kwcomp(kw->kw,"NAXIS   ") ||
                     kw->fmt!='I' || kw->val.i<0) htype = NOFITS;
                 nax = kw->val.i;
                 break;
       case  4 :                             /* fourth header line   */
                 if (0<nax)
                    if (kwcomp(kw->kw,"NAXIS1  ") && kw->fmt=='I') {
                       if (!(kw->val.i || exthd)) htype = RGROUP;
                    }
                    else htype = FBFITS;
                 break;
  }
  return htype;
}

int kwcomp(pk,ps)
/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
.PURPOSE      compare two strings
.RETURN       0: not equal, 1: equal
---------------------------------------------------------------------*/
char          *pk;               /* keyword string                   */
char          *ps;               /* string to compare keyword with   */
{
  while (*ps && (*pk++ == *ps++));
  return !(*ps || *pk);
}
