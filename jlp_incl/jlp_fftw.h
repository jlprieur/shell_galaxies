/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Prototypes for FFT from "fftw_set.c"
//
// JLP
// Version 12/10/2008
//------------------------------------------------------------*/
#ifndef _fftw_set_define
#define _fftw_set_define
#include "fftw.h"

#ifdef __cplusplus
extern "C" {
#endif                          /* __cplusplus */

int FFTW_1D_Y_FLT(float *re, float *im, INT4 *nx, INT4 *ny, INT4 *idim, 
                   INT4 *direct);
int fftw_1D_Y_float(float *re, float *im, int nx, int ny, int direct);
int FFTW_1D_Y_DBLE(double *re, double *im, INT4 *nx, INT4 *ny, INT4 *idim, 
             INT4 *direct);
int fftw_1D_Y_double(double *re, double *im, int nx, int ny, int direct);
int FFTW_2D_DBLE(double *re, double *im, int *nx, int *ny, int *direct);
int fftw_2D_double(double *re, double *im, int nx, int ny, int direct);
int fftw_2D_flt(float *re, float *im, INT4 *nx, INT4 *ny, INT4 *idim, 
                  INT4 *kod);
int fftw_2D_float(float *re, float *im, int nx, int ny, int direct);
int fftw_setup(int nx, int ny);
int FFTW_FSETUP(int *nx, int *ny);
int fftw_fast(FFTW_COMPLEX *image, int nx, int ny, int direct);
int FFTW_SHUTDOWN();
#ifdef __cplusplus
}                               /* extern "C" */
#endif                          /* __cplusplus */

#endif
