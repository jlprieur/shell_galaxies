/*********************************************************
*
*	MODULE : FORMAT_KARIM				 
*						
*	PURPOSE : reads and writes in Karim format	
*
*	CONTAINS : alloc_square_float	
*		   read_header
*		   read_data	
*		   write_data
*		   KARIM_VM_READIMAG
*		   KARIM_READIMAG
*		   KARIM_WRITEIMAG
*
*	VERSION : December  1992
*
*	AUTHOR: Karim BOUYOUCEF
*
*********************************************************/
/************************************************************************
*				
*	KARIM_FORMAT :	
*		
*   The header of the file is always written in ASCII mode:
*	NAXIS number of dimensions (long int)
*	NAXIS1 first dimension (long int)
*	NAXIS2 second dimension (long int)
*	 ........			
*	NAXISn last dimension (long int)
*	TYPE  of data (char[8])		int,float,double,...
*	MODE  of data storage(char[4])	bin,txt
*	NATURE of data (char[8])	real,complex,...
*	COMMENTS (char[1024])
*       DATA as:
*         data[i1,i2,i3,...,in]
* Example with NAXIS1=2, NAXIS2=4, NAXIS3=4:
*         data[0,0,0], data[0,0,1], data[0,0,2], data[0,0,3], 
*         data[0,1,0], data[0,1,1], data[0,1,2], data[0,1,3],
*         data[0,2,0], data[0,2,1], data[0,2,2], data[0,2,3],
*         data[0,3,0], data[0,3,1], data[0,3,2], data[0,3,3],
*         data[1,0,0], data[1,0,1], data[1,0,2], data[1,0,3],
*         data[1,1,0], data[1,1,1], data[1,1,2], data[1,1,3],
*         data[1,2,0], data[1,2,1], data[1,2,2], data[1,2,3],
*         data[1,3,0], data[1,3,1], data[1,3,2], data[1,3,3],
*		
************************************************************************/
#include <malloc.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "jlp_ftoc.h"
/*
#define DEBUG
*/

static	int read_data();
static	float	**alloc_square_float();
static	void	read_header();
static	void	write_data();

/**********************************************************/
/*                                                        */
/* FUNCTION:  alloc_square_float                          */
/*                                                        */
/* PURPOSE: Allocates a square of float in memory         */
/*                                                        */
/* INPUT:  nbr_lin = number of lines                      */
/*         nbr_col = number of columns                    */
/*                                                        */
/* RETURN: square = pointer to matrix of pointers         */
/*                (NULL if memory allocation failure)     */
/*                                                        */
/* VERSION: December 1992                                 */
/*                                                        */
/* AUTHOR: Karim BOUYOUCEF                                */
/*                                                        */
/**********************************************************/
/**********************************************************/
static	float	**alloc_square_float(nbr_lin,nbr_col)
int      nbr_lin, nbr_col;
/**********************************************************/
{
auto     float   **square;
register int      i, j;

square = (float **) malloc((unsigned) nbr_lin*sizeof(float *));
if (square != 0)
  {
  for (i=0; i<nbr_lin; i++)
    {
    square[i] = (float *)malloc((unsigned) nbr_col*sizeof(float));
    if (square[i] == 0) square = 0;
    }
  }

for (i=0; i<nbr_lin; i++)
for (j=0; j<nbr_col; j++)
square[i][j]=0.0;

return(square);
}


/**********************************************************/
/*                                                        */
/* FUNCTION: free_square_float                            */
/*                                                        */
/* PURPOSE: Frees a square of float allocated dynamically */
/* by alloc_square_float()                                */
/*                                                        */
/* INPUT: square = pointer to matrix of pointers          */
/*        nbr_lin = number of lines                       */
/*                                                        */
/* VERSION: 1.1  March  1992                              */
/*                                                        */
/* AUTHOR: Karim BOUYOUCEF                                */
/*                                                        */
/**********************************************************/

/**********************************************************/
void    free_square_float(square,nbr_lin)
float	**square;
int     nbr_lin;
/**********************************************************/

{
register  int    i;

for (i=0; i<nbr_lin; i++)
  free((float *) square[i]);
free((float *) square);
}

/****************************************************************/
/*                                                      	*/
/* FUNCTION: read_header					*/
/*                                                      	*/
/* PURPOSE: reads the header of a data file	         	*/
/*                                                      	*/
/* INPUT: file					               	*/
/*        pointer to dimension number                      	*/
/*        pointer to size of each dimension	           	*/
/*        pointer to data type (int,float,double)              	*/
/*	  pointer to data mode (bin,txt)			*/
/*	  pointer to data nature (real,complex)			*/
/*	  pointer to comments area on the file			*/
/*                                                      	*/
/* VERSION: December  1992                                	*/
/*                                                      	*/
/* AUTHOR: Karim BOUYOUCEF                              	*/
/*                                                      	*/
/****************************************************************/
/****************************************************************/
static	void	read_header(file,dim,size,type,mode,nature,comments)
/****************************************************************/
register	char	file[60];
register	int	dim[1];
register	int	size[4];
register	char	type[8];
register	char	mode[4];
register	char	nature[8];
register	char	comments[1024];
{
/***************  declarations  ***************/
auto		FILE	*fp;	/* nom interne du fichier a traiter */
register	int	i;	/* indice de boucle */

/**************  lecture du header  ***************/
fp=fopen(file,"r");
if (fp != NULL)
	{
	fscanf(fp,"%d\n",&dim[0]);
	for (i=0;i<dim[0];i++)
		fscanf(fp,"%d\n",&size[i]);
	fscanf(fp,"%s\n",type);
	fscanf(fp,"%s\n",mode);
	fscanf(fp,"%s\n",nature);
	fgets(comments,1024,fp);
	fclose(fp);
	}
else
	{
	fprintf(stderr,"\nFATAL ERROR reading file %s\n\n",file);
	exit(-1);
	}
}


/***************************************************************/
/*                                                             */
/* FUNCTION: read_data                                         */
/*                                                             */
/* PURPOSE: reads data from a file			       */
/*                                                             */
/* INPUT:  file         = output file                          */
/*         dim          = dimension of the data                */
/*         size         = number of data in each dimension     */
/*         type         = type of data (int,float,double)      */
/*         mode         = mode of storage (bin,txt)            */
/*         nature       = nature of data (real,complex)        */
/*         comments     = file comments                        */
/*                                                             */
/* VERSION: December  1992                                     */
/*                                                             */
/* AUTHOR: Karim BOUYOUCEF                                     */
/*                                                             */
/***************************************************************/
/**************************************************************/
static	int	read_data(file,dim_r,size,type_r,mode,nature_r,comments)
/**************************************************************/
register	char	*file;
register	int	dim_r;
register	int	size[4];
register	char	*type_r;
register	char	mode[4];
register	char	*nature_r;
register	char	comments[1024];
{
/****************  declarations  **************************/
FILE		*fp;
register	int	i,j;
auto		int	dim;
auto		char	type[8];
auto		char	nature[8];
auto		float	**square_f_real;

/*****************  verification du format du fichier  *****************/
if ((fp=fopen(file,"r")) != NULL)
  {
  fscanf(fp,"%d\n",&dim);
  for (i=0;i<dim;i++)
    fscanf(fp,"%d\n",&size[i]);
  fscanf(fp,"%s %s %s\n",type,mode,nature);
  fscanf(fp,"%s\n",comments);
  }
else
  {
  fprintf(stderr,"\n\nFATAL ERROR reading file %s that doesn't exist\n",file);
  exit(-1);
  }
fclose(fp);

if (dim_r != dim)
  {
  fprintf(stderr,"\nFATAL ERROR file %s must be of dimension %d\n",file,dim_r);
  exit(-1);
  }
if (strcmp(type,type_r) != 0)
  {
  fprintf(stderr,"\n\nFATAL ERROR file %s must be of type %s\n",file,type_r);
  exit(-1);
  }
if (strcmp(nature,nature_r) != 0)
  {
  fprintf(stderr,"\n\nFATAL ERROR file %s must be of natue %s\n",file,nature_r);
  exit(-1);
  }

/********************  lecture du fichier  ***************************/
fp=fopen(file,"r");
fscanf(fp,"%d\n",&dim);
for (i=0;i<dim;i++)
  fscanf(fp,"%d\n",&size[i]);
fscanf(fp,"%s %s %s\n",type,mode,nature);
fscanf(fp,"%s\n",comments);

if(strcmp(mode,"bin") == 0)
  {
  square_f_real = (float **)alloc_square_float(size[0],size[1]);
  for (i=0;i<size[0];i++)
    fread(square_f_real[i],sizeof(float),size[1],fp);
  return((int)square_f_real);
  }

else if(strcmp(mode,"txt") == 0)
  {
  square_f_real = alloc_square_float(size[0],size[1]);
  for (i=0;i<size[0];i++)
  for (j=0;j<size[1];j++)
   fscanf(fp,"%f\n",&square_f_real[i][j]);
  return((int)square_f_real);
  }

else
  {
  fprintf(stderr,"\n\nFATAL ERROR file %s have unknown mode %s\n",file,mode);
  exit(-1);
  }
return((int)square_f_real);
}


/***************************************************************/
/*                                                             */
/* FUNCTION: write_data                                        */
/*                                                             */
/* PURPOSE: writes data in a file			       */
/*                                                             */
/* INPUT:  file		= output file                          */
/*	   data		= pointer to data		       */
/*	   dim		= dimension of the data		       */
/*	   size		= number of data in each dimension     */
/*	   type		= type of data (int,float,double)      */
/*	   mode		= stroring mode of data (txt,bin)      */
/*	   nature	= nature of data (real,complex)        */
/*	   comments	= file comments			       */
/*                                                             */
/* VERSION: December  1992                                     */
/*                                                             */
/* AUTHOR: Karim BOUYOUCEF                                     */
/*                                                             */
/***************************************************************/
/**************************************************************/
static	void	write_data(file,data,dim,size,type,mode,nature,comments)
/**************************************************************/
register	char		*file;
register	long int	data;
register	int		dim;
register	int		*size;
register	char		*type;
register        char    	*mode;
register	char		*nature;
register	char		*comments;
{
/****************  declarations  **************************/
FILE		*fp;
register	int	i,j;
auto		float	**square_f_real;
auto		int	nb;

/*****************  verification du format du fichier  *****************/
fp=fopen(file,"w");
if (fp != NULL)
  {
  fprintf(fp,"%d\n",dim);
  for (i=0;i<dim;i++)
    fprintf(fp,"%d\n",size[i]);
  strcpy(type,"float");
  strcpy(mode,"bin");
  strcpy(nature,"real");
  fprintf(fp,"%s %s %s\n",type,mode,nature);
  sprintf(comments,"surprise...");
  fprintf(fp,"%s\n",comments);
  }
else
  {
  fprintf(stderr,"\n\nFATAL ERROR writing file %s \n",file);
  exit(-1);
  }

if (strcmp(type,"float") != 0)
  {
  fprintf(stderr,"\n\nFATAL ERROR file %s must be of type %s\n",file,type);
  exit(-1);
  }
if (strcmp(mode,"bin") != 0)
  {
  fprintf(stderr,"\n\nFATAL ERROR file %s must be of mode bin or txt\n",file);
  exit(-1);
  }
if (strcmp(nature,"real") != 0)
  {
  fprintf(stderr,"\n\nFATAL ERROR file %s must be of nature real\n",file);
  exit(-1);
  }

/********************  Ecriture dans le fichier  ***************************/
square_f_real = (float **)data;
for (i=0;i<size[0];i++)
  fwrite(square_f_real[i],sizeof(float),size[1],fp);
fprintf(fp,"\n");
fclose(fp);
/*
if (strcmp(mode,"bin") == 0)
        if ((strcmp(type,"float") == 0) && (strcmp(nature,"real") == 0))
                {
		square_f_real = data;
		for (i=0;i<size[0];i++)
                        fwrite(square_f_real[i],sizeof(float),size[1],fp);
                }
else if (strcmp(mode,"txt") == 0)
        if ((strcmp(type,"float") == 0) && (strcmp(nature,"real") == 0))
                {
                square_f_real = data;
                for (i=0;i<size[0];i++)
		for (j=0;j<size[1];j++)
                        fprintf(fp,"%f\n",square_f_real[i][j]);
                }
if (strcmp(mode,"bin") == 0)
  fprintf(fp,"\n");
fclose(fp);
*/
}


/****************************************************************
*  FONCTION:   KARIM_VM_READIMAG
*  PURPOSE:    To read an image
*  VERSION:    04-09-92
*  AUTHOR:     JLP
****************************************************************/

int KARIM_VM_READIMAG(INT4 *pntr_array, INT4 *nx, INT4 *ny,
                      char *name, char *comments, INT4 *status)
{
float array;
INT4 idim;
idim = 0;
*status = KARIM_READIMAG(&array,nx,ny,&idim,name,comments,pntr_array);
return(*status);
}


/****************************************************************/
/*                                                              */
/*  FONCTION:   KARIM_READIMAG                                  */
/*                                                              */
/*  PURPOSE:    reads a picture					*/
/*                                                              */
/*  VERSION:    December  1992                                  */
/*                                                              */
/*  AUTHOR:     Karim BOUYOUCEF                                 */
/*                                                              */
/****************************************************************/

int KARIM_READIMAG(float *array, INT4 *nx, INT4 *ny, INT4 *idim,
                   char *name, char *comments, INT4 *status)
{
extern  void  free_square_float();
/* WARNING: uses status to return pointer if VM_READIMAG is used! */
/***************  declarations  ***************/
register        int     i,j;
auto		int	istat;
auto		int	dim;
auto		int	size[4];
auto            char    type[8];
auto		char	mode[4];
auto		char	nature[8];
auto		char	name1[60];
auto            char    comment[1024];
auto		float	*array1;
auto		float	**s;

istat = 0;

/*** To solve problems with characters in Fortran .... */
strcpy(name1,name);
#ifdef DEBUG
printf(" name1: >%s< \n",name1);
#endif
i=0;
while(i<59 && name1[i] != ' ' && name1[i] != '\0') i++;
name1[i] = '\0';
#ifdef DEBUG
printf(" name1: >%s< i = %d\n",name1,i);
#endif
read_header(name1,&dim,size,type,mode,nature,comment);

*nx = size[0];
*ny = size[1];
/* Check if dynamic allocation of memory is necessary: */
if((*idim) <= 0)
 {
  array1 = (float *) malloc((unsigned) (size[0] * size[1] *sizeof(float)) );
/* WARNING: uses status to return pointer if VM_READIMAG is used! */
  *status = (long int) array1;
 }
 else
  array1 = array;

/*************  lecture du fichier image  ****************/
dim = 2;
strcpy(type,"float");
strcpy(nature,"real");
s = (float **)read_data(name1,dim,size,type,mode,nature,comment);

#ifdef DEBUG
  fprintf(stderr,"\n\nLecture du fichier %s correcte",name);
#endif

*nx = size[0];
*ny = size[1];
*idim = size[0];
sprintf(comments,"%s",comment);

for(i=0;i<size[0];i++)
for(j=0;j<size[1];j++)
	array1[j+i*size[1]] = s[i][j];
free_square_float(s,size[0]);
return(istat);
}


/****************************************************************/
/*                                                              */
/*  FONCTION:   KARIM_WRITEIMAG                                 */
/*                                                              */
/*  PURPOSE:    writes a picture in my format			*/
/*                                                              */
/*  VERSION:    December  1992                                  */
/*                                                              */
/*  AUTHOR:     Karim BOUYOUCEF                                 */
/*                                                              */
/****************************************************************/

int  KARIM_WRITEIMAG(float *array, INT4 *nx, INT4 *ny, INT4 *idim,
                     char *name, char *comments, INT4 *status)
{
/***************  declarations  ***************/
register        int     i;
auto		int	dim,size[4],err,nb;
auto		char	type[8],mode[4],nature[8];
auto		float 	**s;
auto		char    name1[61];

/***  called functions :  ***********/
float **alloc_square_float();
void  free_square_float();

/*** To solve problems with characters in Fortran .... */
strcpy(name1,name);
#ifdef DEBUG
printf(" name1: >%s< \n",name1);
#endif
i=0;
while(i<59 && name1[i] != ' ' && name1[i] != '\0') i++;
name1[i] = '\0';
#ifdef DEBUG
printf(" name1: >%s< i = %d\n",name1,i);
#endif

/***************  ecriture dans le fichier image  ************/
s = alloc_square_float((*nx),(*ny));
for (i=0;i<(*nx)*(*ny);i++)
        s[i/(*ny)][i-i/(*ny)*(*ny)] = array[i];
dim = 2;
size[0] = (*nx);
size[1] = (*ny);
strcpy(type,"float");
strcpy(mode,"bin");
strcpy(nature,"real");
write_data(name1,s,dim,size,type,mode,nature,comments);

(void) free_square_float(s,size[0]);

#ifdef DEBUG
  fprintf(stderr,"\nKARIM_WRITEIMAG/Ecriture correcte dans %s\n\n",name1);
#endif
*status = 0;
return(*status);
}
