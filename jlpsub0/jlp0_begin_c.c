/*****************************************************************
* Routine to re-direct parameters input in the command line
* to the answers of the appropriate questions within a program
*
* Contains:
*  JLP_BEGIN_C
*
* JLP
* Version of 30-11-2001
*****************************************************************/
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include <jlp_ftoc.h>

/*
#define DEBUG
*/

/* Input/output Logical Unit (cf. Fortran): */
static FILE *jlp_lu5, *jlp_lu6;

int JLP_BEGIN_C();
int RD_COMMAND_LINE(char *string,INT4 len, INT4 *status);

/*----------------------------------------------------------
* Subroutine JLP_BEGIN_C
* To check if the input parameters have to be found
* in the file "JLP_LU5.TMP", or read from LU=5 (Fortran default for input
* from a terminal)
* and if the output has to be written
* in the file "JLP_LU6.TMP", or directly on LU=6 (Fortran default for output
* from a terminal)
*----------------------------------------------------------*/
int JLP_BEGIN_C()
{
INT4 istatus, ilength;
char buffer[40], symbol_1[40], lu5_filename[60], lu6_filename[60];

/******************************* Input: ********************/
   jlp_lu5 = NULL;
/* Read the symbol JLP_PROMPT */
   buffer[0] = '\0';
   strcpy(symbol_1,"JLP_PROMPT");
   ilength = 10;
   JLP_GETENV(symbol_1,&ilength,buffer,&istatus);
/* Then check the error flag: */
   if(istatus != 0)
     {
/* Reads the buffer: */
#ifdef DEBUG
     printf(" JLP_BEGIN/JLP_PROMPT=>%s<\n",buffer);
#endif
     if(buffer[0] == 'n' || buffer[0] == 'N')
        {
         strcpy(lu5_filename,"jlp_lu5.tmp");
#ifdef DEBUG
         printf(" JLP_BEGIN/Opening=>%s<\n",lu5_filename);
#endif
         if ((jlp_lu5 = fopen(lu5_filename,"r")) != NULL) 
            {
#ifdef DEBUG
             printf(" Direct input of parameters from %s\n",lu5_filename);
#endif
            }
        }
     }

/******************************* Output: ********************/
   jlp_lu6 = NULL;
/* Read the symbol JLP_QUIET */
   buffer[0] = '\0';
   strcpy(symbol_1,"JLP_QUIET");
   ilength = 9;
   JLP_GETENV(symbol_1,&ilength,buffer,&istatus);
/* Then check the error flag: */
   if(istatus != 0)
     {
/* Reads the buffer: */
#ifdef DEBUG
     printf(" JLP_BEGIN/JLP_QUIET=>%s<\n",buffer);
#endif
     if(buffer[0] == 'y' || buffer[0] == 'Y')
        {
         strcpy(lu6_filename,"jlp_lu6.tmp");
#ifdef DEBUG
         printf(" JLP_BEGIN/Opening=>%s<\n",lu6_filename);
#endif
         if ((jlp_lu6 = fopen(lu6_filename,"w")) != NULL) 
            {
#ifdef DEBUG
             printf(" Redirection of messages to %s\n",lu5_filename);
#endif
            }
        }
     }

return(0);
}
/*********************************************************
* Read string from file "jlp_lu5"
*********************************************************/
int RD_COMMAND_LINE(char *string,INT4 len, INT4 *status)
{
*status = 0;
if(jlp_lu5 == NULL) 
  *status = -1;  
else
  fgets(string,len,jlp_lu5);                                
return(*status);
}
