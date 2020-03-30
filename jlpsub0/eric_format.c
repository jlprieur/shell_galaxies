/**********************************************************
* eric_format.c
*                                             
* PURPOSE: To read/write images in Eric Anterrieu's format.
* (modified in sept. 2000 to accept Eric Aristidi's format.
*           int nx
*           int ny
*           data as a one dimensionnal array (float).
*                                               
* Set of routines: 
* int     ERIC_VM_READIMAG(array,nx,ny,file_name)
* int     ERIC_READIMAG(array,nx,ny,idim,file_name)
* int     ERIC_WRITEIMAG(array,nx,ny,idim,file_name,istat)
* int     jlp_fread_eric(array,nx,ny,idim,file_name)
* int     jlp_fwrite_eric(array,nx,ny,idim,file_name)
* int     eric_copy2D_to_1D(image,array,col,lin,idim)
* int     eric_copy1D_to_2D(array,image,col,lin,idim)
*                                           
* These routines (read and write) can be tested with main program inserted here.
*
* From Eric ANTERRIEU (Version 03-11-92)
* static int    read_f_raster(image,lin,col,file_name)
* static int    header_raster(lin,col,byt,file_name)
* static float  **float_matrix_alloc(nbr_lin,nbr_col)
* static void   f_matrix_free(matrix,nbr_lin)
*
* JLP
* Version 18-11-92
**********************************************************/

/*
#define MAIN_2365
*/
#define DEBUG

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <jlp_ftoc.h>
#include <malloc.h>

static int    header_raster();
static void   f_matrix_free();
static int jlp_fread_eric(float **array, INT4 *nx, INT4 *ny, INT4 *idim,
                             char *file_name);
static int jlp_fread_eric_anterrieu(float **array, INT4 *nx, INT4 *ny, 
                                    INT4 *idim, char *file_name);

/* Can be compiled as a stand-alone program:*/
#ifdef MAIN_2365 
void  main(argc,argv)
int   argc;
char  *argv[];
{
float *array;
INT4 nx, ny;
char outfile[60], infile[60], comments[81];

JLP_BEGIN();

  printf(" Read_f_raster to convert Eric's formatted files to images\n");
  printf(" Version 03-11-92\n");

if (argc != 3 )
  {
 printf("\nUSAGE:\n");
 printf("read_f_raster input_file  output_file \n");
  exit(-1);
  }

JLP_INQUIFMT();

/* Input file name: */
   strcpy(infile,argv[1]);
/* Output file name: */
   strcpy(outfile,argv[2]);
#ifdef DEBUG
    printf(" Input bin file : >%s< \n",infile);
    printf(" Output image : >%s< \n",outfile);
#endif

jlp_readf_raster(&array,&nx,&ny,infile);

printf(" nx = %d ny= %d \n",nx,ny);
printf(" OK");

/* Now write output image : */
infile[39]='\0',
sprintf(comments," Conversion of %s",infile);
JLP_WRITEIMAG(array,&nx,&ny,&nx,outfile,comments);

JLP_END();
}
/* End of test program: */
#endif
/**********************************************************
* Fortran interface: 
**********************************************************/
int    ERIC_VM_READIMAG(long *pntr_array, INT4 *nx, INT4 *ny,
                        char *file_name, char *comments, INT4 *istat)
{
float *array;
char name[41], *pc;

strncpy(name,file_name,40);
name[40] = '\0';
pc = name;
while(*pc != '\0' && *pc != ' ')pc++;
*pc='\0';

array = NULL;
/*
*istat = jlp_fread_eric(&array,nx,ny,nx,name);
*/
*istat = jlp_fread_eric_anterrieu(&array,nx,ny,nx,name);
*pntr_array = (long)array;

sprintf(comments,"Filename=%s",name);

return(*istat);
}
/**********************************************************/
int  ERIC_READIMAG(float *array, INT4 *nx, INT4 *ny,
                   INT4 *idim, char *file_name, char *comments, INT4 *istat)
{
char name[41], *pc;

strncpy(name,file_name,40);
name[40] = '\0';
pc = name;
while(*pc != '\0' && *pc != ' ')pc++;
*pc='\0';

/*
*istat = jlp_fread_eric(&array,nx,ny,idim,name);
*/
*istat = jlp_fread_eric_anterrieu(&array,nx,ny,idim,name);

sprintf(comments,"Filename=%s",name);

return(*istat);
}
/**********************************************************/
int     ERIC_WRITEIMAG(float *array, INT4 *nx, INT4 *ny, INT4 *idim,
                       char *file_name, char *comments, INT4 *istat)
{
char name[41], *pc;
int jlp_fwrite_eric();

strncpy(name,file_name,40);
name[40] = '\0';
pc = name;
while(*pc != '\0' && *pc != ' ')pc++;
*pc='\0';

*istat = jlp_fwrite_eric(array,nx,ny,idim,name);
return(*istat);
}
/**********************************************************
*                                             
* FUNCTION: read_f_raster                      
*                                               
* PURPOSE: To read a float image from a rasterfile.
*                                           
* INPUT:  lin = number of lines              
*         col = number of pixels per line     
*         file_name = rasterfile name          
*                                               
* OUTPUT: image[0..lin-1][0..col-1] = float image
*                                                 
* RETURN: 0 if everything OK                       
*         -1 if unable to open rasterfile           
*         -2 if image size not expected              
*         -3 if error encountered while reading stream
*         -4 if unable to close rasterfile 
*                                         
* AUTHOR: Eric ANTERRIEU (Version 03-11-92)
*
**********************************************************/
static int     read_f_raster(image,lin,col,file_name)
float   **image;
int     lin, col;
char    *file_name;
/**********************************************************/
{
FILE     *fp;
auto     int n, m;
register int i;

if ((fp = fopen(file_name,"r")) == NULL) return(-1);

if ((fread(&n,sizeof(int),1,fp)) != 1) return(-3);
if ((fread(&m,sizeof(int),1,fp)) != 1) return(-3);
if ((n != lin) || (m != col)) return(-2);
for (i=0; i<lin; i++) 
  {
  if ((fread(image[i],sizeof(float),col,fp)) != col) return(-3);
  }

if ((fclose(fp)) == EOF) return(-4);

return(0);
}
/**********************************************************
*                                             
* FUNCTION: jlp_fread_eric_anterrieu
*                                               
* PURPOSE: To read a float image from a rasterfile.
*                                           
* INPUT:  nx = number of lines              
*         ny = number of pixels per line     
*         file_name = rasterfile name          
*                                               
* OUTPUT: array[] = float image
*                                                 
* RETURN: 0 if everything OK                       
*         -1 if unable to open rasterfile           
*         -2 if image size not expected              
*         -3 if error encountered while reading stream
*         -4 if unable to close rasterfile 
*                                         
* JLP
* Version 06-11-92
**********************************************************/
static int jlp_fread_eric_anterrieu(float **array, INT4 *nx, INT4 *ny, 
                                    INT4 *idim, char *file_name)
{
int status, byt, lin, col, isize;
float   **image, **float_matrix_alloc();
void   f_matrix_free();
int eric_copy2D_to_1D();

/* Initialize nx and ny, for error handling: */
*nx = *ny = 0;

/* Check size of file: */
if ((header_raster(&lin,&col,&byt,file_name)) < 0)
  {
  printf("jlp_fread_eric_anterrieu/Error opening >%s< \n",file_name);
  return(-1);
  }

/* Check with size that it is really in Eric's format: */
if(lin <= 0 || lin > 10000 || col <= 0 || col > 10000)
{
printf(" lin=%d col=%d byt=%d\n",lin,col,byt);
printf(" jlp_fread_eric_anterrieu/Error: >%s< not in Eric's format\n",file_name);
return(-2);
}

/* Allocate dynamical memory for 2-D array: */
image = float_matrix_alloc(lin,col);

/* Read data (2-D array): */
status = read_f_raster(image,lin,col,file_name);
*nx = col;
*ny = lin;

/* Allocate dynamical memory for 2-D array if needed: */
if(*array == NULL)
{
isize = lin * col * sizeof(float);
*array = (float*)malloc(isize);

/* Transfer to 1-D array: */
eric_copy2D_to_1D(image,*array,col,lin,*idim);

/* Frees memory: */
f_matrix_free(image,lin);
}
else
/* Transfer to 1-D array: */
eric_copy2D_to_1D(image,*array,col,lin,*idim);

return(0);
}
/* -----------------------------------
* Lecture des fichiers non formattes
*
* Eric Aristidi format
* 128x128 pixels
* ----------------------------------*/
static int jlp_fread_eric(float **array, INT4 *nx, INT4 *ny, INT4 *idim,
                             char *file_name)
{
 int dim, isize;
 register int i;
 FILE *fic,*fopen();

 *nx = 128; *ny = 128;
 dim = *nx * *ny;
 isize = dim * sizeof(float);
 *array = (float*)malloc(isize);

 if ((fic = fopen(file_name,"r")) != NULL)
 {
  for (i=0; i<dim; i++) (*array)[i] = (float) getc(fic);
  fclose(fic);
 }
 else
 {
  printf("jlp_fread_eric_aristidi/Fatal error opening file >%s< \n",file_name);
  exit(-1);
 }

/* tab[i + j * dim] =
Attention, inversion en Y par rapport aux fichiers TIFF
*/
return(0);
}
/*********************************************************************
* Transfer from 1-D to 2-D array:
*********************************************************************/
int eric_copy1D_to_2D(array,image,col,lin,idim)
float **image, *array;
int lin, col, idim;
{
auto int i, j;

for (j=0; j<lin; j++)
  for (i=0; i<col; i++)
    image[j][i] = array[i + idim * j];

return(0);
}
/*********************************************************************
* Transfer from 2-D to 1-D array:
*********************************************************************/
int eric_copy2D_to_1D(image,array,col,lin,idim)
float **image, *array;
int lin, col, idim;
{
auto int i, j;

for (j=0; j<lin; j++)
  for (i=0; i<col; i++)
    array[i + idim * j] = image[j][i];

return(0);
}
/**********************************************************
*                                        
* FUNCTION: header_raster                 
*                                          
* PURPOSE: Reads the header of a rasterfile.
*                                    
* INPUT:  file_name = rasterfile name 
*                                      
* OUTPUT: lin = number of lines         
*         col = number of pixels per line
*         byt = number of bytes per pixel
*                                       
* RETURN: 0 if everything OK             
*         -1 if unable to open rasterfile 
*         -3 if error encoutered while reading stream 
*         -4 if unable to close rasterfile           
*         -5 if unable to rewind file pointer       
*                                                  
* VERSION: July 1990                              
*                                                
* AUTHOR: Eric ANTERRIEU                        
*                                              
**********************************************************/
static int     header_raster(lin,col,byt,file_name)
int     *lin, *col, *byt;
char    *file_name;
/**********************************************************/

{
FILE  *fp;
auto  int  n, m;

if ((fp = fopen(file_name,"r")) == NULL) return(-1);

if ((fread(&n,sizeof(int),1,fp)) != 1) return(-3);
if ((fread(&m,sizeof(int),1,fp)) != 1) return(-3);
if ((fseek(fp,0L,2)) != 0) return(-5);
*lin = n;
*col = m;
*byt = (ftell(fp) - 2*sizeof(int))/(n*m);

if ((fclose(fp)) == EOF) return(-4);

return(0);
}
/**********************************************************
*                                             
* FUNCTION: float_matrix_alloc                     
*                                               
* PURPOSE: Allocates a float matrix.             
*                                                 
* INPUT:  nbr_lin = number of lines                
*         nbr_col = number of columns               
*                                                    
* RETURN: matrix = float pointer to vector of pointers
*                  (NULL if memory allocation failure)
*                                                     
* VERSION: June 1990                                   
*
* AUTHOR: Eric ANTERRIEU                                
*
**********************************************************/
float  **float_matrix_alloc(nbr_lin,nbr_col)
int    nbr_lin, nbr_col;
/**********************************************************/

{
auto     float  **matrix;
register int    i, j;

matrix = (float **) malloc((unsigned) nbr_lin*sizeof(float *));
if (matrix != 0)
  {
  for (i=0; i<nbr_lin; i++)
    {
    matrix[i] = (float *) malloc((unsigned) nbr_col*sizeof(float));
    if (matrix[i] == 0)  
     {
      printf(" float_matrix_alloc/Fatal error allocating memory of line #%d : nx = %d\n",
         i,nbr_col);
      exit(-1);
     }
    }
  }
else
  {
  printf(" float_matrix_alloc/Fatal error allocating memory of line pointers: ny = %d\n",
         nbr_lin);
  exit(-1);
  }

/* Erases new array: */
for (i=0; i<nbr_lin; i++)
for (j=0; j<nbr_col; j++)  matrix[i][j] = 0.0;

return(matrix);
}
/**********************************************************
*                                                 
* FUNCTION: f_matrix_free                          
*                                                   
* PURPOSE: Frees a float matrix allocated dynamically
* by float_matrix_alloc().                             
*                                                   
* INPUT: matrix = float pointer to vector of pointers
*        nbr_lin = number of lines                
*                                                  
* VERSION: June 1990                                
*                                                    
* AUTHOR: Eric ANTERRIEU                              
*
**********************************************************/
static void   f_matrix_free(matrix,nbr_lin)
float  **matrix;
int    nbr_lin;
/**********************************************************/

{
register  int    i;

for (i=0; i<nbr_lin; i++)  free((char *) matrix[i]);
free ((char *) matrix);
}
/***********************************************************
*                                             
* FUNCTION: jlp_fwrite_eric
*                                               
* PURPOSE: Writes a float image in a rasterfile (Eric's format) 
* Can be compiled as a stand-alone program:
*
* JLP
* Version 08-11-92
***********************************************************/
/*
#define MAIN_1435 
*/

/* Can be compiled as a stand-alone program:*/
#ifdef MAIN_1435 
void  main(argc,argv)
int   argc;
char  *argv[];
{
float *array;
INT4 nx, ny, status, pntr;
char outfile[60], infile[60], comments[81];

JLP_BEGIN();

  printf(" Write_f_raster  to convert images to Eric's format\n");
  printf(" Version 03-11-92\n");

if (argc != 3 )
  {
 printf("\nUSAGE:\n");
 printf("write_f_raster input_file  output_file \n");
  exit(-1);
  }

JLP_INQUIFMT();

/* Input file name: */
   strcpy(infile,argv[1]);
/* Output file name: */
   strcpy(outfile,argv[2]);
#ifdef DEBUG
    printf(" Input image : >%s< \n",infile);
    printf(" Output bin file : >%s< \n",outfile);
#endif

/* Reads input image : */
JLP_VM_READIMAG(&pntr,&nx,&ny,infile,comments);
JLP_FROM_MADRID(&pntr,&array);

/* Now writes output image : */
jlp_fwrite_eric(array,&nx,&ny,&nx,outfile);

JLP_END();
}
/* End of test program: */
#endif
/**********************************************************
* RETURN: 0 if everything OK
*         -1 if unable to open rasterfile
*         -3 if error encountered while writing on stream
*         -4 if unable to close rasterfile
**********************************************************/
int     jlp_fwrite_eric(array,nx,ny,idim,file_name)
float   *array;
INT4     *nx, *ny, *idim;
char    *file_name;
{
int     lin, col;
FILE     *fp;
register int  j;

/* Transfer X and Y sizes: */
col = *nx; lin = *ny; 

if ((fp = fopen(file_name,"w")) == NULL) return(-1);

if ((fwrite(&lin,sizeof(int),1,fp)) != 1) return(-3);
if ((fwrite(&col,sizeof(int),1,fp)) != 1) return(-3);
for (j=0; j<lin; j++) 
  {
  if ((fwrite(&array[j * *idim],sizeof(float),col,fp)) != col) return(-3);
  }

if ((fclose(fp)) == EOF) return(-4);

return(0);
}
