/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* jlp0_rdfitsio.c
* To read FITS formatted 1-D, 2-D, and 3-D image files
* Using "FITSIO" C package.
* Formats supported are : FITS 8,16,32,-32,-64 
* (i.e. 1-byte, 2-byte or 4-byte integer, and 4-byte or 8-byte float values)
* JLP: comments and jlp_descriptors
*
* Contains:
* int JLP_VM_RDFITS(pntr_array,nx1,ny1,infile,comments,jlp_descr,dflag,istatus)
* int JLP_VM_RDFITS_3D(pntr_array,nx1,ny1,nz1,iplane,infile,comments,jlp_descr,
*                      dflag,istatus)
* int JLP_RDFITS(array,nx1,ny1,idim,infile,comments,jlp_descr,dflag,istatus)
* int jlp0_flt_rdfits(pntr_array,array,nx1,ny1,nz1,iplane,idim,
*                     infile,comments,jlp_descr,dflag,istatus,vm_flag)
* (and main program to test...)
*
* JLP
* Version 06-07-2012
---------------------------------------------------------------------*/
#include   <jlp_ftoc.h>
/* #include <cfitsio/fitsio.h>   fitsio.h should be in "/usr/include/cfitsio" */
#include <fitsio.h>   /* fitsio.h should be in $(FITS_INCL_DIR) */

/*
#define DEBUG 
*/
/*
int jlp0_dble_rdfits(double **array1, int *nx1, int*ny1, int *nz1, 
                     int iplane, char *infile, char *comments, 
                     char *jlp_descr, int dflag, char *err_mess); 
*/

static int jlp0_flt_rdfits(INT_PNTR *pntr_array, float *array, INT4 *nx1, 
                           INT4 *ny1, INT4 *nz1, INT4 *iplane, INT4 *idim, 
                           char *infile, char *comments, char *jlp_descr, 
                           INT4 *dflag, INT4 *istatus, int vm_flag);
static int jlp0_rdfits_header(fitsfile *fptr, INT4 *nx1, INT4 *ny1, INT4 *nz1, 
                              int *naxis, int *bitpix, char *comments, 
                              char *jlp_descr, int dflag, INT4 *istatus);
static int jlp0_flt_rdfits_data(char *filename, fitsfile *fptr, 
                                float *array1, int nx1, int ny1, int nz1, 
                                int naxis, int bitpix, int iplane, int idim, 
                                INT4 *istatus);
static int jlp0_dble_rdfits_data(char *filename, fitsfile *fptr, 
                                 double *array1, int nx1, int ny1, int nz1, 
                                 int naxis, int bitpix, int iplane, int idim, 
                                 INT4 *istatus);

/* Main program to test JLP_RDFITS */
#ifdef TEST_PROGRAM
main()
{
  float        array[1000000];
  INT4         nx1, ny1, idim;
  char         infile[100], outfile[100];
  char         comments[81], jlp_descr[1024];
  INT4         istatus, dflag;
  register int i;

JLP_INQUIFMT();

  printf(" Test of jlp0_rdfits to read FITS files on disk\n");
  printf(" Version 21-01-2003\n"); 

      printf(" Input FITS file   : ");
      scanf("%s",infile);
      printf(" Output MIDAS/FITS file : ");
      scanf("%s",outfile);

#if DEBUG
    printf(" Input FITS file   : >%s< \n",infile);
    printf(" Output FITSfile : >%s< \n",outfile);
#endif

/* Set dflag to -1, since no descriptors are wanted */
idim = 1000; dflag = -1;
JLP_RDFITS(array,&nx1,&ny1,&idim,infile,comments,jlp_descr,&dflag,&istatus);
#if DEBUG
 printf(" JLP_RDFITS/istatus = %d \n",istatus);
#endif

#if DEBUG
 printf(" nx = %d, ny = %d \n",nx1,ny1);
 printf(" comments: %s \n",comments);
 printf(" image[0...20]: \n");
 for(i = 0; i < 20; i++) printf(" %f ",array[i]);
#endif

 JLP_WRITEIMAG(array,&nx1,&ny1,&idim,outfile,comments);
#if DEBUG
 printf(" JLP_WRFITS/istatus = %d \n",istatus);
#endif

JLP_END();
}
#endif

#ifdef SIMPLE_VERSION
int JLP_VM_READIMAG1(INT_PNTR *pntr_array, INT4 *nx1, INT4 *ny1, char *infile,
                     char *comments)
{
char jlp_descr[1024];
INT4 dflag, istatus;
/* Set dflag to 0, since descriptors are wanted */
dflag = 0;
JLP_VM_RDFITS(pntr_array, nx1, ny1, infile, comments, 
              jlp_descr, &dflag, &istatus);
return(0);
}
#endif

/**********************************************************************
* JLP_VM_RDFITS
*
* INPUT:
* dflag = -1 (no error (warning) messages and no descriptors) 
*       = 0 (no descriptors) 
*       = 1 (descriptors) 
* OUTPUT:
* nx1, ny1: size of images 
**********************************************************************/
int JLP_VM_RDFITS(INT_PNTR *pntr_array, INT4 *nx1, INT4 *ny1, char *infile,
                  char *comments, char *jlp_descr, INT4 *dflag, INT4 *istatus)
{
INT4 idim, nz1, iplane;
int vm_flag;
float *array;
int istat;

/* iplane: number of image plane to be read (for 2D only)*/
vm_flag = 1; 
iplane = 1;
istat = jlp0_flt_rdfits(pntr_array,array,nx1,ny1,&nz1,&iplane,&idim,
                        infile,comments,jlp_descr,dflag,istatus,vm_flag);

return(istat);
}
/**********************************************************************
* JLP_RDFITS
*
* INPUT:
* dflag = -1 (no error (warning) messages and no descriptors) 
*       = 0 (no descriptors) 
*       = 1 (descriptors) 
* OUTPUT:
* nx1, ny1: size of images 
**********************************************************************/
int JLP_RDFITS(float *array, INT4 *nx1, INT4 *ny1, INT4 *idim1,
               char *infile, char *comments, char *jlp_descr,
               INT4 *dflag, INT4 *istatus)
{
int istat, vm_flag;
INT4 nz1, iplane;
INT_PNTR pntr_array;

#ifdef DEBUG
printf("JLP_RDFITS/DEBUG: nx1=%d ny1=%d idim=%d dflag=%d\n", 
        *nx1, *ny1, *idim1, *dflag);
#endif

/* iplane: number of image plane to be read (for 2D only)*/
vm_flag = 0; 
iplane = 1;
istat = jlp0_flt_rdfits(&pntr_array,array,nx1,ny1,&nz1,&iplane,idim1,
                        infile,comments,jlp_descr,dflag,istatus,vm_flag);

return(istat);
}
/**********************************************************************
* JLP_VM_RDFITS_3D
*
* INPUT:
*   dflag = -1 (no error (warning) messages and no descriptors)
*         = 0 (no descriptors)
*         = 1 (descriptors)
*   iplane > 0 if 2D image only is wanted for output
*          = 0 if 3D image with all data
* OUTPUT:
*   nx1, ny1: size of an image plane
*   nz1: number of image planes 
**********************************************************************/
int JLP_VM_RDFITS_3D(INT_PNTR *pntr_array, INT4 *nx1, INT4 *ny1, INT4 *nz1, 
                     INT4 *iplane, char *infile, char *comments, 
                     char *jlp_descr, INT4 *dflag, INT4 *istatus)
{
INT4 idim;
float *array;
int istat, vm_flag;

vm_flag = 1; 
idim = 0; 
istat = jlp0_flt_rdfits(pntr_array,array,nx1,ny1,nz1,iplane,&idim,
                        infile,comments,jlp_descr,dflag,istatus,vm_flag);

return(istat);
}
/**********************************************************************
* jlp0_flt_rdfits
*
* INPUT:
*   dflag = -1 (no error (warning) messages and no descriptors)
*         = 0 (no descriptors)
*         = 1 (descriptors)
*   iplane > 0 if 2D image only is wanted for output
*          = 0 if 3D image with all data
*   vm_flag = 1 if allocation of virtual memory is required 
*           = 0 otherwise 
* OUTPUT:
*   nx1, ny1: size of an image plane
*   nz1: number of image planes 
**********************************************************************/
static int jlp0_flt_rdfits(INT_PNTR *pntr_array, float *array, INT4 *nx1, 
                           INT4 *ny1, INT4 *nz1, INT4 *iplane, INT4 *idim, 
                           char *infile, char *comments, char *jlp_descr, 
                           INT4 *dflag, INT4 *istatus, int vm_flag)
{
char  filename[100], *pcc, err_message[81];
int   istat, naxis, bitpix;
float *array1;
fitsfile *fptr;

*istatus = 0;

   strncpy(filename,infile,100);
   filename[99]='\0';
/* Check input characters strings (pb if fortran...) */
   pcc = filename;
   while(*pcc && *pcc != ' ') pcc++;
   if(*pcc == ' ') *pcc = '\0';

/* Check filename syntax and add ".fits" if no extension is present: */
   pcc = filename;
   while(*pcc && *pcc != '.') pcc++;
   if(*pcc != '.') strcpy(pcc,".fits");

   istat = 0;
   fits_open_file(&fptr,filename,READONLY,&istat);
   if (istat) {
     fits_read_errmsg(err_message);
     fprintf(stderr, "jlp0_rdfits/ Cannot open input file : >%s< istat=%d\n %s \n",
            filename,istat,err_message);
     *istatus = -1; return(-1);
   }                     /* open file  */

/* decode header    */
   jlp0_rdfits_header(fptr, nx1, ny1, nz1, &naxis, &bitpix,
                      comments, jlp_descr, *dflag, istatus);

/* If no allocation of virtual memory, check 
* if buffer and image sizes are consistent
* (for 1D and 2D arrays) */
   if(vm_flag == 0) {
    if(idim < nx1) { 
      fprintf(stderr, "jlp0_rdfits/Fatal error/Inconsistent size of array: nx1=%d ny1=%d idim=%d\n",
              *nx1, *ny1, *idim);
      fprintf(stderr, "whereas fits image (%s) is such that: naxis=%d naxes[0]=%d naxes[1]=%d\n",
              filename, naxis, *nx1, *ny1);
      exit(-1);
      }
     array1 = array;
/* Allocation of memory if needed: */
   } else {
     array1 = (float *)malloc((*nx1) * (*ny1) * sizeof(float));
     *idim = *nx1;
     if(array1 == NULL) {
      fprintf(stderr, "jlp0_rdfits_data/Fatal error allocating memory: nx1=%d ny1=%d\n",
              *nx1, *ny1);
      exit(-1);
      }
   }

/* Read data and copy to float array: */
   jlp0_flt_rdfits_data(filename, fptr, array1, *nx1, *ny1, *nz1, 
                        naxis, bitpix, *iplane, *idim, istatus);

/* Close fits file: */
    fits_close_file(fptr,&istat);

/* Copy pointer array (for FORTRAN): */
*pntr_array = (INT_PNTR)array1; 

return(*istatus);
}
/**********************************************************************
* jlp0_dble_rdfits
*
* INPUT:
* dflag = -1 (no error (warning) messages and no descriptors)
*       = 0 (no descriptors)
*       = 1 (descriptors)
* iplane > 0 if 2D image only is wanted for output
*        = 0 if 3D image with all data
* err_mess: character string used to write error messages
*         (Warning: err_mess should be declared as char *err_mess[200])
* jlp_descr: character string to write the descriptors
*         (Warning: descriptors should be declared as char jlp_descr[1024])
*
* OUTPUT:
* nx1, ny1: size of an image plane
* nz1: number of images in input data cube 
**********************************************************************/
int jlp0_dble_rdfits(double **array1, int *nx1, int*ny1, int *nz1, 
                     int iplane, char *infile, char *comments, 
                     char *jlp_descr, int dflag, char *err_mess)
{
char  filename[100], *pcc, err_message[81];
int   istat, naxis, bitpix;
fitsfile *fptr;

   strncpy(filename,infile,100);
   filename[99]='\0';
/* Check input characters strings (pb if fortran...) */
   pcc = filename;
   while(*pcc && *pcc != ' ') pcc++;
   if(*pcc == ' ') *pcc = '\0';

/* Check filename syntax and add ".fits" if no extension is present: */
   pcc = filename;
   while(*pcc && *pcc != '.') pcc++;
   if(*pcc != '.') strcpy(pcc,".fits");

   istat = 0;
   fits_open_file(&fptr,filename,READONLY,&istat);
   if (istat) {
     fits_read_errmsg(err_message);
     sprintf(err_mess,"jlp0_dble_rdfits/ Cannot open input file : >%s< istat=%d\n %s \n",
            filename,istat,err_message);
     return(-1);
   }                  

/* Decode header */
   jlp0_rdfits_header(fptr, nx1, ny1, nz1, &naxis, &bitpix,
                      comments, jlp_descr, dflag, &istat);
   if (istat) {
     sprintf(err_mess,"jlp0_dble_rdfits/Error reading header of >%s< istat=%d\n",
            filename, istat);
     return(-1);
   }                  

/* Allocation of memory if needed: */
    *array1 = (double *)malloc((*nx1) * (*ny1) * sizeof(double));
    if(array1 == NULL) {
     fprintf(stderr, "jlp0_rdfits_data/Fatal error allocating memory: nx1=%d ny1=%d\n",
             *nx1, *ny1);
     exit(-1);
     }

/* Read data and copy to float array: */
   jlp0_dble_rdfits_data(filename, fptr, *array1, *nx1, *ny1, *nz1, 
                         naxis, bitpix, iplane, *nx1, &istat);
   if (istat) {
     sprintf(err_mess,"jlp0_dble_rdfits/Error reading data of >%s< istat=%d\n",
            filename, istat);
     return(-1);
   }                  


/* Close fits file: */
   istat = 0;
   fits_close_file(fptr,&istat);

return(istat);
}
/**********************************************************************
* jlp0_flt_rdfits_data
*
* INPUT:
*   array1: pointer to a float array where to copy the FITS data 
*   iplane > 0 if 2D image only is wanted for output
*          = 0 if 3D image with all data
*   bitpix: bits per pixel
*
* OUTPUT:
*   nx1, ny1: size of an image plane
*   nz1: number of image planes 
**********************************************************************/
static int jlp0_flt_rdfits_data(char *filename, fitsfile *fptr, 
                                float *array1, int nx1, int ny1, int nz1, 
                                int naxis, int bitpix, int iplane, int idim, 
                                INT4 *istatus)
{
 char         err_message[81];
 int          istat, any_null_values;
 long         nelements;
 float        *f_array1;
 INT2         *i2_array1;
 INT4         *i4_array1;
 register int i, j;

*istatus = 0;
f_array1 = NULL;
i2_array1 = NULL;
i4_array1 = NULL;

if(naxis > 2 && iplane > nz1) {
  fprintf(stderr, "jlp0_rdfits/Fatal error: iplane = %d > nz_image = %d\n", iplane, nz1);
  *istatus = -3;  fits_close_file(fptr,&istat); return(-3); 
  }

/* Correction for one-dimensional images, if ny1 = 0 */
  if(nx1 <= 0) {
    fprintf(stderr, " jlp0_rdfits/Fatal error: nx = %d \n", nx1);
    *istatus = -2;  fits_close_file(fptr,&istat); return(-2);
    }

   nelements = nx1;
   if(naxis > 1) nelements *= ny1;
/* 3-D: */
   if(naxis > 2 && iplane == 0) nelements *= nz1;

/* Allocate memory space: */
    if(bitpix < 0) {
     f_array1 = (float *) malloc(nelements * sizeof(float));
    } else if(bitpix == 16){
     i2_array1 = (INT2 *) malloc(nelements * sizeof(INT2));
    } else if(bitpix == 32){
     i4_array1 = (INT4 *) malloc(nelements * sizeof(INT4));
    } else {
     fprintf(stderr, "jlp0_rdfits_data/Fatal error: bitpix=%d is unsupported!\n",
             bitpix);
     exit(-1);
    }

    if(f_array1 == NULL && i2_array1 == NULL && i4_array1 == NULL) {
     fprintf(stderr, "jlp0_rdfits/fatal error allocating memory, nel=%ld\n",nelements);
     *istatus = -2;  fits_close_file(fptr,&istat); return(-2);
     }

#ifdef DEBUG
   printf(" nx=%d, ny=%d, nz=%d bitpix=%d nelements=%ld \n", 
         nx1, ny1, nz1, bitpix, nelements);
#endif

   istat = 0;

/* Float data: */
 if(bitpix < 0) {
/* Read data:    */
    if(iplane == 0 || nz1 == 0)
       fits_read_img(fptr, TFLOAT, 1, nelements, 0, f_array1, 
                     &any_null_values, &istat); 
/* Case of extraction of a 2-D plane from 3-D array: */
    else
       fits_read_img(fptr, TFLOAT, 1 + (iplane - 1) * nelements, 
                     nelements, 0, f_array1, 
                     &any_null_values, &istat); 
/* 16 bit integer data: */
 } else if(bitpix == 16) {
#ifdef DEBUG
  printf("iplane=%d nz1=%d\n", iplane, nz1);
#endif
/* Read data:    */
    if(iplane == 0 || nz1 == 0)
       fits_read_img(fptr, TSHORT, 1, nelements, 0, i2_array1, 
                     &any_null_values, &istat); 
/* Case of extraction of a 2-D plane from 3-D array: */
    else
       fits_read_img(fptr, TSHORT, 1 + (iplane - 1) * nelements, 
                     nelements, 0, i2_array1, 
                     &any_null_values, &istat); 
/* 32 bit integer data: */
 } else if(bitpix == 32) {
/* Read data:    */
    if(iplane == 0 || nz1 == 0)
       fits_read_img(fptr, TINT, 1, nelements, 0, i4_array1, 
                     &any_null_values, &istat); 
/* Case of extraction of a 2-D plane from 3-D array: */
    else
       fits_read_img(fptr, TINT, 1 + (iplane - 1) * nelements, 
                     nelements, 0, i4_array1, 
                     &any_null_values, &istat); 
 } else {
  istat = 0;
 }
    if (istat) {
      fits_read_errmsg(err_message);
      fprintf(stderr, "jlp0_rdfits/fits_read_img; error reading file : >%s<\
 istat=%d (bitpix=%d)\n %s \n",
            filename, istat, bitpix, err_message);
    *istatus = -3;
    }                     

/* Transfer to output array: */ 
  if(bitpix < 0) {
   for(j = 0; j < ny1; j++)
      for(i = 0; i < nx1; i++)
         array1[i + j * idim] = f_array1[i + j * nx1];
  } else if(bitpix == 16) {
   for(j = 0; j < ny1; j++)
      for(i = 0; i < nx1; i++)
         array1[i + j * idim] = (float)i2_array1[i + j * nx1];
  } else if(bitpix == 32) {
   for(j = 0; j < ny1; j++)
      for(i = 0; i < nx1; i++)
         array1[i + j * idim] = (float)i4_array1[i + j * nx1];
  }

if(f_array1 != NULL) free(f_array1);
if(i2_array1 != NULL) free(i2_array1);
if(i4_array1 != NULL) free(i4_array1);

return(*istatus);
}
/**********************************************************************
* jlp0_dble_rdfits_data
*
* INPUT:
*   array1: pointer to a double array where to copy the FITS data 
*   iplane > 0 if 2D image only is wanted for output
*          = 0 if 3D image with all data
*   bitpix: bits per pixel
*
* OUTPUT:
*   nx1, ny1: size of an image plane
*   nz1: number of image planes 
**********************************************************************/
static int jlp0_dble_rdfits_data(char *filename, fitsfile *fptr, 
                                double *array1, int nx1, int ny1, int nz1, 
                                int naxis, int bitpix, int iplane, int idim, 
                                INT4 *istatus)
{
 char         err_message[81];
 int          istat, any_null_values;
 long         nelements;
 float        *f_array1;
 INT2         *i2_array1;
 INT4         *i4_array1;
 register int i, j;

*istatus = 0;
f_array1 = NULL;
i2_array1 = NULL;
i4_array1 = NULL;

if(naxis > 2 && iplane > nz1) {
  fprintf(stderr, "jlp0_rdfits/Fatal error: iplane = %d > nz_image = %d\n", iplane, nz1);
  *istatus = -3;  fits_close_file(fptr,&istat); return(-3); 
  }

/* Correction for one-dimensional images, if ny1 = 0 */
  if(nx1 <= 0) {
    fprintf(stderr, " jlp0_rdfits/Fatal error: nx = %d \n", nx1);
    *istatus = -2;  fits_close_file(fptr,&istat); return(-2);
    }

   nelements = nx1;
   if(naxis > 1) nelements *= ny1;
/* 3-D: */
   if(naxis > 2 && iplane == 0) nelements *= nz1;

/* Allocate memory space: */
    if(bitpix < 0) {
     f_array1 = (float *) malloc(nelements * sizeof(float));
    } else if(bitpix == 16){
//     i2_array1 = (INT2 *) malloc(nelements * sizeof(INT2));
     i2_array1 = (INT2 *) malloc(nelements * sizeof(INT4));
    } else if(bitpix == 32){
     i4_array1 = (INT4 *) malloc(nelements * sizeof(INT4));
    } else {
     fprintf(stderr, "jlp0_rdfits_data/Fatal error: bitpix=%d is unsupported!\n",
             bitpix);
     exit(-1);
    }

    if(f_array1 == NULL && i2_array1 == NULL && i4_array1 == NULL) {
     fprintf(stderr, "jlp0_rdfits/fatal error allocating memory, nel=%ld\n",nelements);
     *istatus = -2;  fits_close_file(fptr,&istat); return(-2);
     }

#ifdef DEBUG
   printf(" nx=%d, ny=%d, nz=%d bitpix=%d nelements=%ld \n", 
         nx1, ny1, nz1, bitpix, nelements);
#endif

   istat = 0;

/* Float data: */
 if(bitpix < 0) {
/* Read data:    */
    if(iplane == 0 || nz1 == 0)
       fits_read_img(fptr, TFLOAT, 1, nelements, 0, f_array1, 
                     &any_null_values, &istat); 
/* Case of extraction of a 2-D plane from 3-D array: */
    else
       fits_read_img(fptr, TFLOAT, 1 + (iplane - 1) * nelements, 
                     nelements, 0, f_array1, 
                     &any_null_values, &istat); 
/* 16 bit integer data: */
 } else if(bitpix == 16) {
/* Read data:    */
    if(iplane == 0 || nz1 == 0)  {
       fits_read_img(fptr, TSHORT, 1, nelements, 0, i2_array1, 
                     &any_null_values, &istat); 
/* Case of extraction of a 2-D plane from 3-D array: */
    } else {
       fits_read_img(fptr, TSHORT, 1 + (iplane - 1) * nelements, 
                     nelements, 0, i2_array1, 
                     &any_null_values, &istat); 
    }
/* 32 bit integer data: */
 } else if(bitpix == 32) {
/* Read data:    */
    if(iplane == 0 || nz1 == 0)
       fits_read_img(fptr, TINT, 1, nelements, 0, i4_array1, 
                     &any_null_values, &istat); 
/* Case of extraction of a 2-D plane from 3-D array: */
    else
       fits_read_img(fptr, TINT, 1 + (iplane - 1) * nelements, 
                     nelements, 0, i4_array1, 
                     &any_null_values, &istat); 
 } else {
  istat = 0;
 }
    if (istat) {
      fits_read_errmsg(err_message);
      fprintf(stderr, "jlp0_rdfits/fits_read_img; error reading file : >%s<\
 istat=%d (bitpix=%d)\n %s \n",
            filename, istat, bitpix, err_message);
    *istatus = -3;
    }                     

/* Transfer to output array: */ 
  if(bitpix < 0) {
   for(j = 0; j < ny1; j++)
      for(i = 0; i < nx1; i++)
         array1[i + j * idim] = (double)f_array1[i + j * nx1];
  } else if(bitpix == 16) {
   for(j = 0; j < ny1; j++)
      for(i = 0; i < nx1; i++)
         array1[i + j * idim] = (double)i2_array1[i + j * nx1];
  } else if(bitpix == 32) {
   for(j = 0; j < ny1; j++)
      for(i = 0; i < nx1; i++)
         array1[i + j * idim] = (double)i4_array1[i + j * nx1];
  }

if(f_array1 != NULL) free(f_array1);
if(i2_array1 != NULL) free(i2_array1);
if(i4_array1 != NULL) free(i4_array1);

return(*istatus);
}
/**********************************************************************
* jlp0_rdfits_header
*
* INPUT:
*   dflag = -1 (no error (warning) messages and no descriptors)
*         = 0 (no descriptors)
*         = 1 (descriptors)
* OUTPUT:
*   nx1, ny1: size of an image plane
*   nz1: number of image planes 
*   naxis: number of axes
*   istatus: -2 if not supported format
*   comments: comments (OBJECT keyword)
*   jlp_descr: descriptors
**********************************************************************/
static int jlp0_rdfits_header(fitsfile *fptr, INT4 *nx1, INT4 *ny1, INT4 *nz1, 
                              int *naxis, int *bitpix, char *comments, 
                              char *jlp_descr, int dflag, INT4 *istatus)
{
 char         buffer[81], buf1[80];
 int          maxdim = 3, simple, istat, extend;
 long         naxes[3], pcount, gcount;
 register int i;

/* decode header    */
    istat = 0;
    fits_read_imghdr(fptr, maxdim, &simple, bitpix, naxis, naxes, &pcount,
                     &gcount, &extend, &istat); 
    if (istat) {   
          fprintf(stderr, "jlp0_rdfits_header/NOT supported FITS format! istat=%d\n",istat);
          *istatus = -2;  fits_close_file(fptr,&istat); return(-1);
        }

/* Axes values: */
   *nx1 = naxes[0];
   if(*naxis > 1 ) *ny1 = naxes[1];
/* In the case of 3-D images, set nz1 to naxes[2], otherwise set it to 0: */
   if(*naxis > 2) 
        *nz1 = naxes[2]; 
      else 
        *nz1 = 0;

/* Try to fill "comments" with many keywords */

/* Try from the beginning each time: */
    istat = 0;
    fits_read_record(fptr,0,buffer,&istat);
/* fits_read_key needs istat=0 in input ! */
    istat = 0;
    fits_read_key(fptr,TSTRING,"COMMENT",comments,buffer,&istat);
#ifdef DEBUG
      printf(" DEBUG/ istat=%d COMMENT = %s \n",
              istat, comments);
#endif
    if(istat == KEY_NO_EXIST) {
      istat = 0;
      fits_read_record(fptr,0,buffer,&istat);
      istat = 0;
      fits_read_key(fptr,TSTRING,"OBJECT",comments,buffer,&istat);
#ifdef DEBUG
      printf(" DEBUG/ istat=%d OBJECT = %s \n",istat,comments);
#endif
      if(istat == KEY_NO_EXIST) {
         istat = 0;
         fits_read_record(fptr,0,buffer,&istat);
         istat = 0;
         fits_read_key(fptr,TSTRING,"DESCRIP",comments,buffer,&istat);
#ifdef DEBUG
         printf(" DEBUG/ istat=%d DESCRIP = %s \n",istat,comments);
#endif
         if(istat == KEY_NO_EXIST) 
             {
             istat = 0;
             fits_read_record(fptr,0,buffer,&istat);
             istat = 0;
             fits_read_key(fptr,TSTRING,"HISTORY",comments,buffer,&istat);
             }
         }
      }

#ifdef DEBUG
printf("DEBUG/comments: >%s<\n",comments);
#endif

/* Descriptors if present (jlp_descr[1024]) and if dflag is set to 1 */
 if(dflag == 1) {
   for(i = 0; i < strlen(jlp_descr); i++) jlp_descr[i] = '\0';
   i = 0;
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"OBJECT",buf1,buffer,&istat);
   trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) { 
     sprintf(&jlp_descr[i],"OBJECT= %s", buf1);
#ifdef DEBUG
   printf(" DEBUG/ istat=%d OBJECT = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;  
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"DESCRIP",buf1,buffer,&istat);
   trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) { 
     sprintf(&jlp_descr[i],"DESCRIP= %s", buf1);
#ifdef DEBUG
   printf(" DEBUG/ istat=%d DESCRIP = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;  
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
/* DATE (compatibility with files created before september 2012) */
   istat = 0; fits_read_key(fptr,TSTRING,"DATE",buf1,buffer,&istat);
   trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) { 
     sprintf(&jlp_descr[i],"DATE= %s", buf1);
#ifdef DEBUG
   printf(" DEBUG/ istat=%d DATE = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;  
      }
/* DATE-OBS (after september 2012) */
   istat = 0; fits_read_key(fptr,TSTRING,"DATE-OBS",buf1,buffer,&istat);
   trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) { 
     sprintf(&jlp_descr[i],"DATE-OBS= %s", buf1);
#ifdef DEBUG
   printf(" DEBUG/ istat=%d DATE-OBS = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;  
      }
/* jlp_descr: 
 OBJECT, DESCRIP, COUNTERS, ANDOR0, ANDOR1, ANDOR2, ANDOR3, ANDOR4, HISTORY 
*/
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"COUNTERS",buf1,buffer,&istat);
   trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) { 
     sprintf(&jlp_descr[i],"COUNTERS= %s", buf1);
#ifdef DEBUG
   printf(" DEBUG/ istat=%d COUNTERS = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;  
      }
/* jlp_descr: 
 OBJECT, DESCRIP, COUNTERS, ANDOR0, ANDOR1, ANDOR2, ANDOR3, ANDOR4, HISTORY 
*/
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"ANDOR0",buf1,buffer,&istat);
   trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) { 
     sprintf(&jlp_descr[i],"ANDOR0= %s", buf1);
#ifdef DEBUG
   printf(" DEBUG/ istat=%d ANDOR0 = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;  
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"ANDOR1",buf1,buffer,&istat);
   trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) { 
     sprintf(&jlp_descr[i],"ANDOR1= %s", buf1);
#ifdef DEBUG
   printf(" DEBUG/ istat=%d ANDOR1 = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;  
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"ANDOR2",buf1,buffer,&istat);
   trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) { 
     sprintf(&jlp_descr[i],"ANDOR2= %s", buf1);
#ifdef DEBUG
   printf(" DEBUG/ istat=%d ANDOR2 = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;  
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"ANDOR3",buf1,buffer,&istat);
   trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) { 
     sprintf(&jlp_descr[i],"ANDOR3= %s", buf1);
#ifdef DEBUG
   printf(" DEBUG/ istat=%d ANDOR3 = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;  
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"ANDOR4",buf1,buffer,&istat);
   trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) { 
     sprintf(&jlp_descr[i],"ANDOR4= %s", buf1);
#ifdef DEBUG
   printf(" DEBUG/ istat=%d ANDOR4 = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;  
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"ANDOR5",buf1,buffer,&istat);
   trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) { 
     sprintf(&jlp_descr[i],"ANDOR5= %s", buf1);
#ifdef DEBUG
   printf(" DEBUG/ istat=%d ANDOR5 = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;  
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"COUNTERS",buf1,buffer,&istat);
   trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) { 
     sprintf(&jlp_descr[i],"COUNTERS= %s", buf1);
#ifdef DEBUG
   printf(" DEBUG/ istat=%d COUNTERS = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      i += 80;  
      }
   jlp_descr[i] = '\0';
   istat = 0; fits_read_record(fptr,0,buffer,&istat);
   istat = 0; fits_read_key(fptr,TSTRING,"HISTORY",buf1,buffer,&istat);
   trim_string(buf1, 80);
   if(istat != KEY_NO_EXIST) { 
     sprintf(&jlp_descr[i],"HISTORY= %s", buf1);
#ifdef DEBUG
   printf(" DEBUG/ istat=%d HISTORY = %s i=%d\n",istat,&jlp_descr[i],i);
#endif
      }
 } /* EOF case dflag != 1 */

return(*istatus);
}
