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
#if !defined(JLP_FTOC_INCLUDED)

#include "jlp_ftoc_rename.h"
#include "jlp_ftoc_proto.h"
#include "jlp_c_proto.h"

/* previous version: (before december 1998)
#ifdef ibm
#include "jlp_ftoc_ibm.h"
#endif

#ifdef linux 
#include "jlp_ftoc_linux.h"
#endif

#ifdef dec 
#include "jlp_ftoc_sun.h"
#endif

#ifdef sun
#include "jlp_ftoc_sun.h"
#endif
*/
#endif /* end of "JLP_FTOC_INCLUDED" sentry */
