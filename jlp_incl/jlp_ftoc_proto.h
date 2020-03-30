/********************************************************
* "jlp_ftoc_proto.h"
* for Fortran / C interface
* JLP
* Version 20-11-2006
*********************************************************/
#if !defined(JLP_PROTO_INCLUDED)

/* For compatibility with VAX/VMS ... */
#if !defined(DEC_STRING)
#  ifdef VAX
#    include <stdlib.h>
#    include <descrip.h>
#    define DEC_STRING(V) struct dsc$descriptor *V
#  else
#    define DEC_STRING(V) char *V
#  endif
#endif

#ifdef __cplusplus
extern "C" {
#endif                          /* __cplusplus */

/* "jlpsub0/jlp_random.c" */
int JLP_RANDOM_INIT(long *seed);
int JLP_RANDOM(float *x);
int JLP_RANDOM_GAUSS(float *x);

/* "sourcc/conju_grad.c" */
int JLP_CGRAD(double *aa, double *psi, double *phi, INT4 *nx1, INT4 *ny1,
              INT4 *ifail);
int JLP_FSTEP(double *aa, double *psi, double *phi, INT4 *nx1, INT4 *ny1, 
              INT4 *ipositiv1);

/* "sourcc/polyfit.c" */
int POLYFIT(double *xx, double *yy, int *npts, int *poly_order,
            double *xc, double *error_xc, double *rms_resid);
int CALPOLY(double *x, double *y, double *xc, int *poly_order);

/* "sourcc/jlp_sort.c" */
int JLP_QSORT_INDX_CHAR(char *array, int *length, int *index, int *nn);
int JLP_QSORT_INDX_DBLE(double *array, int *index, int *nn);
int JLP_QSORT_INDX(float *array, int *index, int *nn);
int JLP_QSORT(float *array, int *nn);
int JLP_QSORT_DBLE(double *array, int *nn);
int JLP_MEDIAN(float *data, int npts, float *value);
int JLP_MEDIAN_DBLE(double *data, int npts, double *value);

/* jlp0_acc_midas.c */
int JLP_WRMIDAS(float *image, INT4 *nx, INT4 *ny, INT4 *idim,
                DEC_STRING(filename), DEC_STRING(comments),
                DEC_STRING(jlp_descr), INT4 *dflag, INT4 *istat);
int JLP_RDMIDAS(float *image, INT4 *nx, INT4 *ny, INT4 *idim,
                DEC_STRING(filename), DEC_STRING(comments),
                DEC_STRING(jlp_descr), INT4 *dflag, INT4 *istat);
int JLP_VM_RDMIDAS(INT_PNTR *pntr_image, INT4 *nx, INT4 *ny,
                   DEC_STRING(filename), DEC_STRING(comments),
                   DEC_STRING(jlp_descr), INT4 *dflag, INT4 *istat);
/* Midas (complement) in jlp0_acc_midas.c */
int JLP_SCSPRO();
int JLP_SCSEPI();

/* karim_format */
int KARIM_VM_READIMAG(INT_PNTR *pntr_array, INT4 *nx, INT4 *ny,
                      char *name, char *comments, INT4 *status);
int KARIM_READIMAG(float *array, INT4 *nx, INT4 *ny, INT4 *idim,
                   char *name, char *comments, INT4 *status);
int KARIM_WRITEIMAG(float *array, INT4 *nx, INT4 *ny, INT4 *idim,
                     char *name, char *comments, INT4 *status);

/* eric_format */
int ERIC_VM_READIMAG(INT_PNTR *pntr_array, INT4 *nx, INT4 *ny,
                      char *file_name, char *comments, INT4 *istat);
int ERIC_READIMAG(float *array, INT4 *nx, INT4 *ny,
                   INT4 *idim, char *file_name, char *comments, INT4 *istat);
int ERIC_WRITEIMAG(float *array, INT4 *nx, INT4 *ny, INT4 *idim,
                    char *file_name, char *comments, INT4 *istat);

/* jlpsub0/jlp0_rdfits.c and jlpsub0/jlp0_wrfits.c */
int jlp0_dble_rdfits(double **array1, int *nx1, int *ny1, int *nz1,
                     int iplane, char *infile, char *comments,
                     char *jlp_descr, int dflag, char *err_mess);
int JLP_VM_RDFITS(INT_PNTR *pntr_array, INT4 *nx1, INT4 *ny1, char *infile,
                  char *comments, char *jlp_descr, INT4 *dflag, INT4 *istatus);
int JLP_VM_RDFITS_3D(INT_PNTR *pntr_array, INT4 *nx1, INT4 *ny1, INT4 *nz1, 
                     INT4 *iplane, char *infile, char *comments, 
                     char *jlp_descr, INT4 *dflag, INT4 *istatus);
int JLP_RDFITS(float *array, INT4 *nx1, INT4 *ny1, INT4 *idim,
               char *infile, char *comments, char *jlp_descr, 
               INT4 *dflag, INT4 *istatus);
int JLP_WRFITS(float *array, INT4 *nx1, INT4 *ny1, INT4 *idim,
               char *filename, char *comments, char *jlp_descr, INT4 *dflag,
               INT4 *istatus, INT4 *out_type);

/* jlpsub0/jlp0_fits_utils.c */
int get_epoch_from_fits_file(char *fits_filename, char *full_directory,
                             double *epoch0, char *date0, int *eyepiece0,
                             int *epoch_was_found);
int descrip_decode_date(char *d_date, char *d_counters, char *date0,
                        double *year0, double *time0, double *epoch0);
int JLP_compute_epoch(char *date0, const double time0, double *epoch0);
int JLP_julian(double aa, int mm, int idd, double time, double *djul);
int JLP_besselian_epoch(double aa, int mm, int idd, double time, 
                        double *b_date);

/* jlp_access.for / in "jlp0_accc.c" */
int JLP_BEGIN();
int JLP_INQUIFMT();
int JLP_FORMAT(INT4 *i_in, INT4 *i_out);
int JLP_END();
int JLP_READIMAG(float *image, INT4 *nx, INT4 *ny, INT4 *idim,
                 char *filename, char *comments);
int JLP_D_WRITEIMAG(double *d_image, INT4 *nx, INT4 *ny,
                    INT4 *idim, char *filename, char *comments);
int JLP_WRITEIMAG(float *image, INT4 *nx, INT4 *ny, INT4 *idim,
                  char *filename, char *comments);
int JLP_VM_READIMAG1(INT_PNTR *pntr_image, INT4 *nx, INT4 *ny,
                     char *filename, char *comments);
int JLP_RDESCR(char *keyword, char *value, INT4 *length, INT4 *istatus);
int JLP_WDESCR(char *keyword, char *value, INT4 *length, INT4 *istatus);
int JLP_INDEX_DESCR(INT4 *kstart, char *name, INT4 *name_length);

/* jlp_fft.for */
int FFT_2D(float *re, float *im, INT4 *nx, INT4 *ny, INT4 *idim, INT4 *kod);

/* jlp_system.c */
int JLP_SYSTEM(char *string);

/* math: jlp_ctime0: */
int JLP_CTIME0(DEC_STRING(string), INT4 *istat);

/* jlp0_unix1.c */
int JLP_GVM(float **pntr, INT4 *memsize);
int JLP_GVMI(INT4 **pntr, INT4 *memsize);
int JLP_FVM(float **pntr);
int JLP_RENAME(DEC_STRING(infile),INT4 *length1, 
               DEC_STRING(outfile), INT4 *length2, INT4 *istat);
int JLP_SETENV (DEC_STRING(symbol), INT4 *length1, DEC_STRING(string),
                INT4 *length2, INT4 *istat);
int JLP_CTIME(DEC_STRING(string), INT4 *istat);
int JLP_GETENV(DEC_STRING(symbol), INT4 *length, DEC_STRING(string), 
               INT4 *istat);
int JLP_DIR(DEC_STRING(input), INT4 *length, DEC_STRING(output),
            INT4 *nfiles, INT4 *quiet, INT4 *istat);
int JLP_LOC(INT4 *var, INT_PNTR *loc);

/* jlp0_osd.c */
int JLP_OSDOPEN(DEC_STRING(filename), INT4 *length, INT4 *iomode,
                INT4 *fid, INT4 *istatus);
int JLP_OSDCLOSE(INT4 *fid, INT4 *istatus);
int JLP_OSDREAD(INT4 *fid, char *pbuf, INT4 *nochar, INT4 *istatus);
int JLP_OSDWRITE(INT4 *fid, char *pbuf, INT4 *nochar, INT4 *istatus);

/* fft/fourn.c */
void fourn_(float *data, int *nn, int *ndim, int *isign);
void fourn(float *data, int *nn, int ndim, int isign);


/* jlp_string.c (in jlpsub0) */
int trim_string(char *str1, int len1);
int compact_string(char *str1, int len1);
int cleanup_string(char *str1, int len1);

/******************************************************
* Fortran programs used for graphic interface: 
*******************************************************/
/* splot/lib/jgc_common.for */
int JGC_TO_COMMON(INT4 *idv1);
int COMMON_TO_JGC(INT4 *idv1);

/* splot/lib/splot_for.c */
void JLP_SPDEVICE_CURVE(char *plotdev1, float *xminuser1, float *xmaxuser1, 
                        float *yminuser1, float *ymaxuser1, INT4 *plan, 
                        char *title, INT4 *idv1);
int JLP_SPDEVICE_IMAGE(char *plotdev, char *title, 
                     float *image_f, INT4 *nx, INT4 *ny, INT4 *gamma1,
                     INT4 *gamma_d, INT4 *idv1);
void JLP_SPLABEL(char *xlabel, INT4 *max_length, INT4 *ix, INT4 *iy,
                 float *angle, float *expand, INT4 *idrawit, float *length,
                 INT4 *idv1);
void JLP_SPBOX(char *xlabel, char *ylabel, char *title, INT4 *ticks_in, 
               INT4 *box_numbers, char *filename, char *comments, INT4 *idv1);
int JLP_SETPCOLOR(char *pcolor, INT4 *idv1);
int JLP_SETLINEPARAM(INT4 *lwidth, INT4 *ltype, INT4 *idv1);

/* splot/lib/jlp_splot2.c */
int NEWPLOT2(float *xplot, float *yplot, float *errx, float *erry,
             INT4 *npoints, INT4 *nmax, INT4 *ncurves,
             char *xlabel, char *ylabel, char *title, char *nchar,
             char *pcolor, float *xout, float *yout, INT4 *nout, 
             INT4 *nout_max, INT4 *error_bars, char *filename, char *comments,
             INT4 *full_caption, float *expand, INT4 *ticks_in, INT4 *idv);
int NEWPLOT2_HARDCOPY(float *xplot, float *yplot, float *errx, float *erry,
                      INT4 *npoints, INT4 *nmax, INT4 *ncurves,
                      char *xlabel, char *ylabel, char *title, char *nchar,
                      char *pcolor, INT4 *error_bars, char *filename,
                      char *comments, INT4 *full_caption, 
                      INT4 *jlp_axes_are_wanted, INT4 *xgrid_is_wanted,
                      INT4 *ygrid_is_wanted, INT4 *xaxis_type, INT4 *yaxis_type,
                      float *expand, INT4 *idv1, char *pst_format,
                      char *pst_filename);

/* splot/lib/jlp_splot21.c */
int NEWPLOT21(float *xplot, float *yplot, float *errx, float *erry,
              INT4 *npoints, INT4 *nmax, INT4 *ncurves,
              char *xlabel, char *ylabel, char *title, char *nchar,
              char *pcolor, float *xout, float *yout, INT4 *nout, 
              INT4 *nout_max, INT4 *error_bars, char *filename, char *comments,
              INT4 *full_caption, INT4 *jlp_axes_are_wanted, 
              INT4 *xgrid_is_wanted, INT4 *ygrid_is_wanted,
              INT4 *xaxis_type, INT4 *yaxis_type, float *expand, 
              INT4 *ticks_in, INT4 *idv1);

/* splot/lib/jlp_splot.c */
int JLP_DEVICE_IMAGE(char *plotdev, char *out_filename, char *title, 
                     float *image_f, INT4 *nx, INT4 *ny, INT4 *gamma1,
                     INT4 *gamma_d, INT4 *idv1);
int JLP_DEVICE_CURVE(char *plotdev, char *out_filename, 
                     float *xminuser1, float *xmaxuser1, 
                     float *yminuser1, float *ymaxuser1, int *plan, 
                     char *title, INT4 *idv1);
int JLP_SET_NEW_LIMITS(float *xmin, float *xmax, float *ymin, float *ymax,
                       INT4 *idv1);
int JLP_GET_PLOT_PARAM(INT4 *offx1, INT4 *offy1, INT4 *axlen1,
                       INT4 *aylen1, float *xmin, float *xmax,
                       float *ymin, float *ymax, INT4 *plan, INT4 *idv1);
int JLP_SET_PLOT_PARAM(INT4 *offx1, INT4 *offy1, INT4 *axlen1,
                       INT4 *aylen1, float *xmin, float *xmax,
                       float *ymin, float *ymax, INT4 *idv1);
int JLP_DRAW(INT4 *x, INT4 *y, INT4 *idv1);
int JLP_RELOC(INT4 *x, INT4 *y, INT4 *idv1);
int JLP_LINE1(float *xx1, float *yy1, float *xx2, float *yy2, INT4 *idv1);
int JLP_LINE1_BACKUP(float *xx1, float *yy1, float *xx2, float *yy2, 
                     INT4 *line_width, INT4 *backup_to_file, INT4 *idv1);
int JLP_SETCOLOR(INT4 *r, INT4 *g, INT4 *b, INT4 *idv1);
int JLP_EVENTS(INT4 *idv1);
int JLP_GFLUSH(INT4 *idv1);
int JLP_WHERE(float *x, float *y, INT4 *in_frame, INT4 *pressed_button, 
              INT4 *draw_cross, INT4 *idv1);
int JLP_GET_2CIRCLES(float *x_cent, float *y_cent, float *diam1, 
                     float *diam2, INT4 *ncirc, INT4 *idv1);
int JLP_SPCLOSE(INT4 *idv1);
int JLP_PLOT_IMAGE(INT4 *image, INT4 *nx1, INT4 *ny1, INT4 *idim1,
                   INT4 *xstart, INT4 *ystart, INT4 *gamma_d, 
                   INT4 *black_and_white, INT4 *idv1);
int JLP_CURVE_LINE(float *xplot, float *yplot, INT4 *npoints, char *nchar1, 
                   char *pcolor1, INT4 *idv1);
int JLP_CURVE_HISTO(float *xplot, float *yplot, INT4 *npoints, char *nchar1, 
                    char *pcolor1, INT4 *idv1);
int JLP_ERASE_STATUS_BAR(INT4 *idv1);
int JLP_DRAW_TO_STATUS_BAR(char *label, INT4 *idv1);
int CONV_USER_TO_MGO(float *x_user, float *y_user, INT4 *ix, INT4 *iy,
                     INT4 *idv1);
int CONV_MGO_TO_USER(INT4 *ix, INT4 *iy, float *x_user, float *y_user,
                     INT4 *in_frame, INT4 *idv1);

/* splot/lib/jlp_lut1.cpp */
int CONVERT_TO_LUT(float *image1, INT4 *nx1, INT4 *ny1, INT4 *idim1,
                   INT4 *image2, INT4 *nx2, INT4 *ny2, INT4 *idim2,
                   INT4 *ncolors, INT4 *itt_is_linear, float *lower_itt, 
                   float *upper_itt, INT4 *idv1);
int JLP_LOAD_LUT(INT4 *r, INT4 *g, INT4 *b, INT4 *ncolors, INT4 *idv1);
int JLP_REVERSE_LUT(INT4 *idv1);


/* in jlpsub/circphot_set.for */
int CIRCPHOT(float *image1, INT4 *nx1, INT4 *ny1, INT4 *idim1,
             INT4 *image2, INT4 *nx2, INT4 *ny2, INT4 *idim2, 
             char *filename, INT4 *ncolors, INT4 *itt_is_linear, 
             float *lower_itt, float *upper_itt, INT4 *gamma_d,
             INT4 *gamma1, INT4 *idv1);

/* in jlpsub/patch2_set.for */
int PATCH2(float *image1, INT4 *nx1, INT4 *ny1, INT4 *idim1,
           INT4 *image2, INT4 *idim2, 
           INT4 *i_min, INT4 *i_max, INT4 *j_min, INT4 *j_max, 
           INT4 *ncolors, INT4 *itt_is_linear, float *lower_itt, 
           float *upper_itt, INT4 *gamma_d, INT4 *idv1);
int PATCH3(float *image1, INT4 *nx1, INT4 *ny1, INT4 *idim1,
           INT4 *image2, INT4 *idim2, 
           INT4 *i_min, INT4 *i_max, INT4 *j_min, INT4 *j_max, 
           INT4 *ncolors, INT4 *itt_is_linear, float *lower_itt, 
           float *upper_itt, float *xp, float *yp, float *diam, 
           INT4 *poly_order, float *sig, INT4 *status, INT4 *gamma_d, 
           INT4 *idv1);


/* splot/lib/jlp_gd_symbol.c */
int JLP_SYMBOL(INT4 *x, INT4 *y, INT4 *isize, INT4 *isymb, INT4 *idv1);
int JLP_SYMBOL1(float *x, float *y, INT4 *isize, INT4 *isymb, INT4 *idv1);
int JLP_SYMBOL2(float *x, float *y, float *size, INT4 *isymb, INT4 *idv1);
int JLP_SYMBOL_ERRORY1(float *x, float *y, float *erry, INT4 *size, INT4 *idv1);
int JLP_SYMBOL_ERRORX1(float *x, float *y, float *errx, INT4 *size, INT4 *idv1);
int JLP_CIRCLE1(float *x, float *y, float *diam, INT4 *idv1);

/* splot/lib/jlp_splot2.c */
int JLP_CURVE(float *xplot, float *yplot, float *errx, float *erry, 
              INT4 *npoints, char *nchar1, char *pcolor1, INT4 *error_bars, 
              INT4 *idv1);

/* splot/lib/jlp_display2.c */
int SPLOT_IMAGE(float *image_f1, INT4 *nx1, INT4 *ny1, INT4 *ncolors,
               char *filename, char *comments, char *lut_type,
               INT4 *itt_is_linear, float *lower_itt, float *upper_itt,
               char *xlabel, char *ylabel, char *zlabel, char *title,
               char *plotdev, INT4 *idv1, float *xminuser0, float *xmaxuser0,
               float *yminuser0, float *ymaxuser0, INT4 *nobox,
               INT4 *lut_scale);
int SPLOT_DEVICE2(char *filename, float *image_f1, INT4 *nx1, INT4 *ny1,
                  INT4 *nx2, INT4 *ny2, INT4 *max_size,
                  INT4 *ncolors, INT4 *color_output, char *lut_type,
                  INT4 *lut_offset, INT4 *lut_slope, char *title,
                  char *plotdev, INT4 *idv1);
int SPLOT_IMAGE2(float *image_f1, INT4 *nx1, INT4 *ny1,
                 INT4 *nx2, INT4 *ny2, INT4 *ncolors, char *filename, 
                 char *comments, INT4 *itt_is_linear, float *lower_itt, 
                 float *upper_itt, char *xlabel, char *ylabel, char *zlabel, 
                 char *title, INT4 *full_caption, float *xminuser0, 
                 float *xmaxuser0, float *yminuser0, float *ymaxuser0, 
                 INT4 *nobox, INT4 *lut_scale, INT4 *idv1, INT4 *color_output,
                 INT4 *erase);

#ifdef __cplusplus
}                               /* extern "C" */
#endif                          /* __cplusplus */


#define JLP_PROTO_INCLUDED
#endif /* End of JLP_PROTO_INCLUDED sentry */
