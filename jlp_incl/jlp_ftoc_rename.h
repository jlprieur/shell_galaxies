/********************************************************
* "jlp_rename.h"
* Interface C / Fortran
*           declared name in C ----> name when called from fortran
* IBM version      : JLP_EXAMPLE -----> jlp_example
* VAX/vms version  : JLP_EXAMPLE -----> JLP_EXAMPLE
* SUN version      : JLP_EXAMPLE -----> jlp_example_
* DEC/unix version : JLP_EXAMPLE -----> jlp_example_
* Linux version    : JLP_EXAMPLE -----> jlp_example__
* Linux/Debian10 version    : JLP_EXAMPLE -----> jlp_example_
* (GNU g77, Linux:
* With -funderscoring in effect, g77 appends two underscores to names with
* underscores and one underscore to external names with no underscores.)
*
Example in Cambridge:
printenv OSTYPE
solaris
linux
*
printenv uname
SunOS
* JLP
* Version 12-10-2008
 *********************************************************/
#if !defined(jlp_rename_included_) /* Beginning of sentry */
#define jlp_rename_included_ 

/* Concatenates name and extension: */
#ifdef ibm 
#define RENAME(name) name
#define RENAME_(name) name
#elif defined(sun) || defined(dec)
#define RENAME(name) name ## _
#define RENAME_(name) name ## _
#elif defined(linux) && !defined(gnu_f90) 
#define RENAME(name) name ## _
#define RENAME_(name) name ## __
/* Cambridge gcc and f90 in Solaris: */
/* Debian gcc and f90 in Merate*/
#elif defined(solaris) || defined(gnu_f90) 
#define RENAME(name) name ## _
#define RENAME_(name) name ## _
#else   /* Just in case JLP_SYSTEM is not defined */
#pragma message('jlp_ftoc_rename/WARNING: JLP_SYSTEM is not defined!')
#define RENAME(name) name ## _
#define RENAME_(name) name ## __
#endif

/* Debian10 */
#define RENAME(name) name ## _
#define RENAME_(name) name ## _

/* jlp_sort.c (in sourcc) */
#define JLP_QSORT              RENAME_(jlp_qsort)
#define JLP_QSORT_DBLE         RENAME_(jlp_qsort_dble)
#define JLP_QSORT_INDX         RENAME_(jlp_qsort_indx)
#define JLP_QSORT_INDX_CHAR    RENAME_(jlp_qsort_indx_char)
#define JLP_QSORT_INDX_DBLE    RENAME_(jlp_qsort_indx_dble)
#define JLP_MEDIAN             RENAME_(jlp_median)
#define JLP_MEDIAN_DBLE        RENAME_(jlp_median_dble)

/* jlp0_acc_midas.c */
#define JLP_WRMIDAS            RENAME_(jlp_wrmidas)
#define JLP_RDMIDAS            RENAME_(jlp_rdmidas)
#define JLP_VM_RDMIDAS         RENAME_(jlp_vm_rdmidas)
/* Midas (complement) */
#define JLP_SCSPRO             RENAME_(jlp_scspro)
#define JLP_SCSEPI             RENAME_(jlp_scsepi)

/* karim_format */
#define KARIM_READIMAG         RENAME_(karim_readimag)
#define KARIM_VM_READIMAG      RENAME_(karim_vm_readimag)
#define KARIM_WRITEIMAG        RENAME_(karim_writeimag)

/* eric_format */
#define ERIC_READIMAG          RENAME_(eric_readimag)
#define ERIC_VM_READIMAG       RENAME_(eric_vm_readimag)
#define ERIC_WRITEIMAG         RENAME_(eric_writeimag)

/* math: jlp_ctime0.c */
#define JLP_CTIME0             RENAME_(jlp_ctime0)

/* jlp0_unix1.c */
#define JLP_CTIME              RENAME_(jlp_ctime)
#define JLP_DIR                RENAME_(jlp_dir)
#define JLP_FVM                RENAME_(jlp_fvm)
#define JLP_GVM                RENAME_(jlp_gvm)
#define JLP_GVMI               RENAME_(jlp_gvmi)
#define JLP_GETENV             RENAME_(jlp_getenv)
#define JLP_LOC                RENAME_(jlp_loc)
#define JLP_RENAME             RENAME_(jlp_RENAME_)
#define JLP_SETENV             RENAME_(jlp_setenv)

/* jlp_system.c */
#define JLP_SYSTEM             RENAME_(jlp_system)

/* jlp0_rdfits.c and jlp0_wrfits.c */
#define jlp0_dble_rdfits       RENAME_(jlp0_dble_rdfits)
#define JLP_WRFITS             RENAME_(jlp_wrfits)
#define JLP_RDFITS             RENAME_(jlp_rdfits)
#define JLP_VM_RDFITS          RENAME_(jlp_vm_rdfits)
#define JLP_VM_RDFITS_3D       RENAME_(jlp_vm_rdfits_3d)
#define JLP_RDFITS_2D_dble     RENAME_(jlp_rdfits_2d_dble)

/* jlp0_osd.c */
#define JLP_OSDOPEN            RENAME_(jlp_osdopen)
#define JLP_OSDCLOSE           RENAME_(jlp_osdclose)
#define JLP_OSDREAD            RENAME_(jlp_osdread)
#define JLP_OSDWRITE           RENAME_(jlp_osdwrite)

/* jlp0_accc.c */
#define JLP_INQUIFMT           RENAME_(jlp_inquifmt)
#define JLP_READIMAG           RENAME_(jlp_readimag)
#define JLP_WRITEIMAG          RENAME_(jlp_writeimag)
#define JLP_D_WRITEIMAG        RENAME_(jlp_d_writeimag)
#define JLP_VM_READIMAG        RENAME_(jlp_vm_readimag)
#define JLP_VM_READIMAG1       RENAME_(jlp_vm_readimag1)
#define JLP_END                RENAME_(jlp_end)
#define JLP_FROM_MADRID        RENAME_(jlp_from_madrid)
#define JLP_INDEX_DESCR        RENAME_(jlp_index_descr)
#define JLP_RDESCR             RENAME_(jlp_rdescr)
#define JLP_WDESCR             RENAME_(jlp_wdescr)
#define JLP_FORMAT             RENAME_(jlp_format)

/* jlp0_begin.c */
#define JLP_BEGIN_C            RENAME_(jlp_begin_c)
#define RD_COMMAND_LINE        RENAME_(rd_command_line)

/* newplot0.for */
#define NEWPLOT                RENAME(newplot)

/* jgc_common.for */
#define JGC_TO_COMMON          RENAME_(jgc_to_common)
#define COMMON_TO_JGC          RENAME_(common_to_jgc)

/* splot_for.c */
#define JLP_SPBOX              RENAME_(jlp_spbox)
#define JLP_SPDEVICE_CURVE     RENAME_(jlp_spdevice_curve)
#define JLP_SPDEVICE_IMAGE     RENAME_(jlp_spdevice_image)
#define JLP_SPLABEL            RENAME_(jlp_splabel)

/* jlp_display2.c */
#define SPLOT_IMAGE            RENAME_(splot_image)
#define SPLOT_IMAGE2           RENAME_(splot_image2)
#define SPLOT_DEVICE2          RENAME_(splot_device2)

/* jlp_splot.c */
#define JLP_DEVICE_CURVE       RENAME_(jlp_device_curve)
#define JLP_DEVICE_IMAGE       RENAME_(jlp_device_image)
#define JLP_DRAW               RENAME_(jlp_draw)
#define JLP_RELOC              RENAME_(jlp_reloc)
#define JLP_WHERE              RENAME_(jlp_where)
#define JLP_LINE1              RENAME_(jlp_line1)
#define JLP_LINE1_BACKUP       RENAME_(jlp_line1_backup)
#define JLP_CIRCLE1            RENAME_(jlp_circle1)
#define JLP_SYMBOL_ERRORX1     RENAME_(jlp_symbol_errorx1)
#define JLP_SYMBOL_ERRORY1     RENAME_(jlp_symbol_errory1)
#define JLP_SYMBOL             RENAME_(jlp_symbol)
#define JLP_SYMBOL1            RENAME_(jlp_symbol1)
#define JLP_SYMBOL2            RENAME_(jlp_symbol2)
#define JLP_CURVE              RENAME_(jlp_curve)
#define JLP_CURVE_LINE         RENAME_(jlp_curve_line)
#define JLP_CURVE_HISTO        RENAME_(jlp_curve_histo)
#define JLP_SETPCOLOR          RENAME_(jlp_setpcolor)
#define JLP_SETLINEPARAM       RENAME_(jlp_setlineparam)
#define JLP_SPCLOSE            RENAME_(jlp_spclose)
#define JLP_GFLUSH             RENAME_(jlp_gflush)
#define JLP_PLOT_IMAGE         RENAME_(jlp_plot_image)
#define JLP_SETCOLOR           RENAME_(jlp_setcolor)
#define JLP_GET_2CIRCLES       RENAME_(jlp_get_2circles)
#define CONVERT_TO_LUT         RENAME_(convert_to_lut)
#define REVERSE_LUT            RENAME_(reverse_lut)
#define JLP_SET_NEW_LIMITS     RENAME_(jlp_set_new_limits)
#define JLP_GET_PLOT_PARAM     RENAME_(jlp_get_plot_param)
#define JLP_SET_PLOT_PARAM     RENAME_(jlp_set_plot_param)
#define JLP_LOAD_LUT           RENAME_(jlp_load_lut)
#define JLP_ERASE_STATUS_BAR   RENAME_(jlp_erase_status_bar)
#define JLP_DRAW_TO_STATUS_BAR RENAME_(jlp_draw_to_status_bar)
#define CONV_USER_TO_MGO       RENAME_(conv_user_to_mgo)
#define CONV_MGO_TO_USER       RENAME_(conv_mgo_to_user)

/* jlp_splot2.c */
#define NEWPLOT2               RENAME(newplot2)
#define NEWPLOT2_HARDCOPY      RENAME_(newplot2_hardcopy)

/* jlp_splot21.c */
#define NEWPLOT21              RENAME(newplot21)

/* jlp_random.c */
#define JLP_RANDOM             RENAME_(jlp_random)
#define JLP_RANDOM_INIT        RENAME_(jlp_random_init)
#define JLP_RANDOM_GAUSS       RENAME_(jlp_random_gauss)

/* patch2_set.for */
#define PATCH2                 RENAME(patch2)
#define PATCH3                 RENAME(patch3)

/* patch2_set_c.c */
#define PATCH21                RENAME(patch21)
#define PATCH31                RENAME(patch31)
#define CIRC_PATCH             RENAME_(circ_patch)
#define CIRC_PATCH_AND_DISPLAY RENAME_(circ_patch_and_display)

/* circphot_set.for */
#define CIRCPHOT               RENAME(circphot) 

/* sourcc/jlp_cgrad.c */
#define JLP_CGRAD              RENAME_(jlp_cgrad)
#define JLP_FSTEP              RENAME_(jlp_fstep)

/* sourcc/polyfit.c */
#define POLYFIT                RENAME(polyfit)
#define CALPOLY                RENAME(calpoly)

/* risley.c */
#define GET_PW_SAT             RENAME_(get_pw_sat) 
#define GET_CROSSANGLE         RENAME_(get_crossangle) 

/* jlp_cover_mask.c */
#define COVERA                 RENAME(covera)
#define COVERA_MASK            RENAME_(covera_mask) 
#define COVER_NGT              RENAME_(cover_ngt)
#define COVER_IXY              RENAME_(cover_ixy)
#define COVER_NBCOUV           RENAME_(cover_nbcouv)
#define COVER_KLM              RENAME_(cover_klm)

#define JLP_RENAME_INCLUDED

/* FFT/RECENTRE  fftw_set.c, recent_fft.c */
/* fftw_set.c */
#define FFTW_1D_Y_FLT          RENAME_(fftw_1D_y_flt)
#define FFTW_1D_Y_DBLE         RENAME_(fftw_1D_y_dble)
#define FFTW_2D_FLT            RENAME_(fftw_2D_flt)
#define FFTW_2D_DBLE           RENAME_(fftw_2D_dble)
#define FFTW_FSETUP            RENAME_(fftw_fsetup)
#define FFTW_SHUTDOWN          RENAME_(fftw_shutdown)
/* jlp_fft.for */
#define FFT_1D_Y_FOURN         RENAME_(fft_1D_y_fourn_float)
#define FFT_1D_Y_FOURN_DBLE    RENAME_(fft_1D_y_fourn_dble)
#define FFT_2D_FOURN           RENAME_(fft_2D_fourn)
#define FFT_2D_FOURN_DBLE      RENAME_(fft_2D_fourn_dble)
/* recent_fft.c */
#define RECENT_FFT             RENAME_(recent_fft)
#define RECENT_FFT_1D_X_FLOAT  RENAME_(recent_fft_1D_x_float)
#define RECENT_FFT_1D_X        RENAME_(recent_fft_1D_x)
#define RECENT_FFT_1D_Y_FLOAT  RENAME_(recent_fft_1D_y_float)
#define RECENT_FFT_1D_Y        RENAME_(recent_fft_1D_y)
#define RECENT_FFT_DOUBLE      RENAME_(recent_fft_double)
#define FFT_SETUP              RENAME_(fft_setup)
#define FFT_FLOAT              RENAME_(fft_float)
#define TO_SINGLE              RENAME_(to_single)
#define TO_DOUBLE              RENAME_(to_double)
/* Warning:
 * (GNU g77, Linux:
 * With -funderscoring in effect, g77 appends two underscores to names with
 * underscores and one underscore to external names with no underscores.)
*/

#endif /* EOF sentry */
