/* "jlp_def.h" 
*
* JLP
* Version 04-06-91
* To handle Fortran string interface with UNIX and VMS: 
*/

#include <string.h>

#ifdef VAX
#include <stdlib.h>
# include <descrip.h>
# define DEC_STRING(V) struct dsc$descriptor *V
/*
 define STRING_COPY(S,V) strncpy(S,V->dsc$a_pointer,V->dsc$w_length+1)
*/
# define STRING_COPY(S,V,L) strncpy(S,V->dsc$a_pointer,L)
#else
#define DEC_STRING(V) char *V
#define STRING_COPY(S,V,L) strncpy(S,V,L)
#endif
