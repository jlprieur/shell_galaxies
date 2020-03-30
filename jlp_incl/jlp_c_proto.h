/********************************************************
 * "jlp_c_proto.h"
 * Prototypes for JLP's C programs
 *
 * JLP
 * Version 08-12-98
 *********************************************************/
#if !defined(jlp_c_proto_)
#define jlp_c_proto_

#ifdef __cplusplus
extern "C" {
#endif                          /* __cplusplus */

/* "sourcc/fit_gauss.c" */
int jlp_fit_gauss_flt(float *xx, float *yy, float *f1, INT4 *npts,
                  float *sigx, float *sigy, float *xc, float *yc,
                  float *rho, float *errors, INT4 *ifail);
int jlp_fit_gauss(double *xx, double *yy, double *f1, INT4 *npts,
                  double *sigx, double *sigy, double *xc, double *yc,
                  double *rho, double *errors, INT4 *ifail);
int calpoly_0(double xx, double yy, double *phi, INT4 ncoeff, INT4 kmax, 
              double *value);

/* jlpsub0/jlp_system.c (jlpacc.make) */
int jlp_system(char *string);

/* jlpsub0/jlp0_begin_c.c */
int JLP_BEGIN_C();
int RD_COMMAND_LINE(char *string,INT4 len, INT4 *status);

/* sourcc/rotate_set.c (jlputil.make) */
int rotimage(float *ima, float *imarot, INT4 nx, INT4 ny, double angle);

/* fft/recent_fft.c (jlputil.make) */
int RECENT_FFT(float *in, float *out, INT4 *nx, INT4 *ny, INT4 *idim);
int RECENT_FFT_DOUBLE(double *in, double *out, INT4 *nx, INT4 *ny, INT4 *idim);
int RECENT_FFT_1D_Y_FLOAT(float *in, float *out, INT4 *nx, INT4 *ny, INT4 *idim);
int RECENT_FFT_1D_X_FLOAT(float *in, float *out, INT4 *nx, INT4 *ny, INT4 *idim);
int RECENT_FFT_1D_Y(double *in, double *out, INT4 *nx, INT4 *ny, INT4 *idim);
int RECENT_FFT_1D_X(double *in, double *out, INT4 *nx, INT4 *ny, INT4 *idim);
int TO_SINGLE(double *in, float *out, INT4 *nx, INT4 *ny, INT4 *idim);
int TO_DOUBLE(float *in, double *out, INT4 *nx, INT4 *ny, INT4 *idim);

/* jlp_random.c (jlputil.make) */
int JLP_RANDOM_INIT(long *seed);
int JLP_RANDOM(float *x);
int JLP_RANDOM_GAUSS(float *x);

/* In jlp0_accc.c (jlpacc.make) */
int jlp_writematrix_dble(double **phi, int nx, int ny,
                         char *filename, char *comments);
int jlp_readmatrix_dble(double **phi, int nx, int ny,
                        char *filename, char *comments);

/* In jlp0_spectra_fitsio.c (jlpacc.make) */
int jlp_read_spfits(char *infile, char *comments, float **wavelength,
                    float **flux_unnorm, float **flux_norm, float **snr,
                    long **order_nb, long *nrows, int firstrow,
                    int nelements, int icorot, int vm, int italk);
int jlp_read_spfits_fast(char *infile, char *comments, float *flux_norm,
                         int firstrow, int nelements, int icorot, int italk);
int jlp_write_spfits(char *filename, char *comments, float *wavel,
                     float *flux, float *snr, long npts, int italk);

#ifdef __cplusplus
}                               /* extern "C" */
#endif                          /* __cplusplus */

#endif /* End of jlp_c_proto_ sentry */
