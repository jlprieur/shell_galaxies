/********************************************************
 * Fortran to C interface
 *           declared name in C ----> name when called from fortran
 * IBM version      : JLP_EXAMPLE -----> jlp_example
 * VAX/vms version  : JLP_EXAMPLE -----> JLP_EXAMPLE
 * SUN version      : JLP_EXAMPLE -----> jlp_example_
 * DEC/unix version : JLP_EXAMPLE -----> jlp_example_
 * Linux version    : JLP_EXAMPLE -----> jlp_example__
 * JLP
 * Version 03-12-1998
 *********************************************************/
#if !defined(__jlp_ftoc_included)
#define __jlp_ftoc_included

#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>                   /* isprint... */


/* See also M_PI in math.h ... */
#ifndef PI 
#define PI 3.14159265358979323846 
#endif

#ifndef MAXI
#define MAXI(x,y) ((x) > (y) ? (x) : (y))
#endif
#ifndef MINI
#define MINI(x,y) ((x) > (y) ? (y) : (x))
#endif
#ifndef ABS 
#define ABS(x) ((x) > 0. ? (x) : (-(x)))
#endif
#ifndef SQUARE 
#define SQUARE(x) ((x) * (x))
#endif
#ifndef NINT 
#define NINT(x) (int)((x) + 0.5)
#endif


#ifdef dec
/* For fortran interface: 
   INTEGER*4 is equivalent to int 
   for OSF1/DEC systems (short=2, int=4, long=8, addresses on 8 bytes)*/
typedef     char           INT1;
typedef     short          INT2;
typedef     int            INT4;
typedef     long           INT_PNTR;
#else
/* For fortran interface: 
   INTEGER*4 is equivalent to long int 
   for LINUX on PC's (short=2, int=4, long=4, addresses on 4 bytes)
   for other systems (short=2, int=4, long=4, addresses on 4 bytes)*/
typedef     char           INT1;
typedef     short          INT2;
typedef     int            INT4;
typedef     long           INT_PNTR;
#endif
/* Tests on 64-bit PC (Feb. 2008):
 sizeof(int)=4 sizeof(long)=8 sizeof(short)=2 sizeof(address)=8 sizeof(char)
 sizeof(float)=4 sizeof(double)=8 sizeof(long double)=16
*/

#include "jlp_ftoc_rename.h"
#include "jlp_ftoc_proto.h"
#include "jlp_c_proto.h"
#include "jlp_fftw.h"

#define JLP_FTOC_INCLUDED
#endif /* end of "JLP_FTOC_INCLUDED" sentry */
