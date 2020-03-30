/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* set of routines to read TIFF format
*
* JLP 
* Version 16-10-2009
-----------------------------------------------------------------------*/
#ifndef __rdtiff_include  /* BEOF sentry */
#define __rdtiff_include  
/*
#define DEBUG
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <jlp_ftoc.h>

int jlp0_rdtiff(float **real_array, int *nx, int *ny, char *in_name,
                char *comments, int *status);
int JLP_VM_RDTIFF(INT_PNTR *pntr_array, INT4 *nx, INT4 *ny,
                  char *in_name, char *comments, int *status);
int JLP_RDTIFF(float *real_array, INT4 *nx, INT4 *ny, INT4 *idim,
               char *in_name, char *comments, int *status);
int JLP_WRTIFF(float *real_array, INT4 *nx, INT4 *ny, INT4 *idim,
               char *out_name, char *comments, int *status);


#endif  /*EOF sentry */
