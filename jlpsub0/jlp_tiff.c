/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
* set of routines to read TIFF format
*
* Contains:
*
* int JLP_RDTIFF(pntr_array,nx,ny,idim,in_name,comments,status)
* int JLP_VM_RDTIFF(real_array,nx,ny,in_name,comments,status)
* int JLP_WRTIFF(real_array,nx,ny,idim,in_name,comments,status)
* From Eric Anterrieu (1991..)
*
* JLP 
* Version 16-09-2009
-----------------------------------------------------------------------*/
#include "jlp_tiff.h"

static void swap_int(unsigned short *i);
static void swap_lint(unsigned long *i);

#ifdef JLP_MAIN
main(argc,argv)
int argc;
char *argv[];
{
char in_name[61], out_name[61], comments[81];
float *real_array;
int nx, ny, status, iformat;
register int i;

JLP_BEGIN();
JLP_INQUIFMT();

 printf("Input TIFF file: ");
 gets(in_name);

/* Calling reading subroutine: */
  status = JLP_VM_RDTIFF(&real_array,&nx,&ny,in_name,comments);

if(!status)
  {

#ifdef DEBUG
for(i=0; i<100; i++) printf(" a[%d]: %f ",i,real_array[i]);
#endif

  printf(" Conversion of TIFF image:  %s    to:    %s \n",in_name,out_name);
  sscanf(comments," From  %s",in_name);
  JLP_WRITEIMAG(real_array,&nx,&ny,&nx,out_name,comments);
  }

JLP_END();
}
#endif
/****************************************************************
*
* TIFF Tag Image Format File
* From La Revue de l'utilisateur PC Vol 78 pp 49-64 June 1992.
****************************************************************/

/****************************************************************
* Subroutine JLP_VM_RDTIFF to read TIFF format 
****************************************************************/
int JLP_VM_RDTIFF(INT_PNTR *pntr_array, INT4 *nx, INT4 *ny,
                  char *in_name, char *comments, int *status)
{
float *real_array;

jlp0_rdtiff(&real_array,nx,ny,in_name,comments,status);

/* Save the address of real array: */
 *pntr_array = (long)real_array;

return(*status);
}
/****************************************************************
* Subroutine to read TIFF format 
****************************************************************/
int jlp0_rdtiff(float **real_array, int *nx, int *ny, char *in_name,
                char *comments, int *status)
{
FILE *fd;
char *char_array, header[512];
int nbytes_to_read, nbytes, to_swap;
unsigned short version, nber_of_tags, tag_type, data_type, value1;
unsigned long first_address, value_length, value, value2, strip_offset;
register int i, j;

/* Value size according to data type: 0: 0 byte, 
    1= 1 byte, 2=ASCII (1 byte), 3=short (2 bytes), 
    4=long (4 bytes), 5=rational (8 bytes) */
int tag_size[6] = {0,1,1,2,4,8};

#ifdef DEBUG
printf(" \n rdtiff/reading file : %s \n",in_name);
#endif

/* Opens the input file */
if((fd = fopen(in_name,"r")) == NULL)
  {
  printf("rdtiff/error opening input file: >%s< \n",in_name);
  *status = -1;
  return(*status);
  }

/***************************************************************/
/* Read first eight bytes to know if swap is needed, version, etc */
 nbytes = fread(header,1L,8L,fd);
 if(nbytes != 8)
   {printf("rdtiff/error reading beginning of header, only %d bytes read\n", nbytes);
   *status = -2; return(*status);}

#ifdef DEBUG
for(i=0; i<8; i++) printf(" h[%d]: %d ",i,header[i]);
printf("\n");
#endif

/* 'M''M' if Motorola, 'I''I' if Intel */
/* swap is not needed for ibm with Motorola: */
if( header[0] == 'M' && header[1] == 'M')
  {
#ifdef DEBUG
   printf("rdtiff/ OK: Motorola format \n");
#endif
   to_swap = 0;
  }
else if( header[0] == 'I' && header[1] == 'I')
  {
#ifdef DEBUG
   printf("rdtiff/ OK: Intel format \n");
#endif
   to_swap = 1;
  }
else
 {
  printf("rdtiff/Fatal error, wrong header: \n");
  for(i=0; i<8; i++) printf(" h[%d]: %d ",i,header[i]);
  return(-1);
 }

/* Version number: (usually 42) */
  memcpy(&version,header+2,2);
  if(to_swap) swap_int(&version);

#ifdef DEBUG
   printf(" Version %u \n",version);
#endif

/* First address */
  memcpy(&first_address,header+4,4);
  if(to_swap) swap_lint(&first_address);
#ifdef DEBUG
   printf(" First address %u \n",first_address);
#endif

/* Skip to this position (relative to the beginning of the file): */
if(fseek(fd,first_address,SEEK_SET))
  {printf("rdtiff/fseek error \n"); *status = -2; return(*status);}
if( (nbytes = fread(header,1L,2L,fd)) != 2L)
  {printf("rdtiff/fatal error reading header (first address) \n");
   *status = -3; return(*status);}

/* And read number of tags: */
  memcpy(&nber_of_tags,header,2);
  if(to_swap) swap_int(&nber_of_tags);

#ifdef DEBUG
     printf(" Number of tags %d \n",nber_of_tags);
#endif

/***************************************************************/
/* Decode central part of header: */

/* Main loop: */
for (i=0; i<nber_of_tags; i++)
  {
  if( (nbytes = fread(header,1L,8L,fd)) != 8L)
    {printf("rdtiff/fatal error reading header (tag #%d) \n",i);
    *status = -4; return(*status);}

/* Tag type: */
   memcpy(&tag_type,header,2);
      if(to_swap) swap_int(&tag_type);

/* Data type: */
   memcpy(&data_type,header+2,2);
      if(to_swap) swap_int(&data_type);

/* Length: */
   memcpy(&value_length,header+4,4);
      if(to_swap) swap_lint(&value_length);

/* Value size according to tag type: 0= 0 byte 
    1= 1 byte, 2=ASCII (1 byte), 3=short (2 bytes), 
    4=long (4 bytes), 5=rational (8 bytes) */

#ifdef DEBUG
   printf(" Tag #%d ; tag type: %d, data type: %d value length: %d \n",
            i,tag_type,data_type,value_length);
#endif

/* Now read value: 
 (Note that we have to read 4 bytes in all cases)*/
   switch( tag_size[data_type] )
     {
/* unsigned short: */
      case 2 :
        if( (nbytes = fread(header,1L,4L,fd)) != 4L)
        {printf("rdtiff/fatal error reading header (tag #%d)\n",i);
        *status = -4; return(*status);}
        memcpy(&value1,header,2);
        if(to_swap) swap_int(&value1);
        value = (long)value1;
        break;
/* unsigned long: */
      case 4 :
        if( (nbytes = fread(header,1L,4L,fd)) != 4L)
        {printf("rdtiff/fatal error reading header (tag #%d)\n",i);
        *status = -4; return(*status);}
        memcpy(&value,header,4);
        if(to_swap) swap_lint(&value);
        break;
/* rational: not fully tested yet...*/
      case 8 :
/* JLP97:
        printf(" Sorry 8-bit rational numbers not tested yet...\n");
*/
        if( (nbytes = fread(header,1L,4L,fd)) != 4L)
        {printf("rdtiff/fatal error reading header (tag #%d)\n",i); return(-4);}
        memcpy(&value,header,4);
        if(to_swap) swap_lint(&value);
        memcpy(&value2,header+4,4);
        if(to_swap) swap_lint(&value2);
        break;
/* Default: (0 or 1 byte)
 When only one byte, the value is in "value_length" field
*/
       default:
        value = value_length * tag_size[data_type];
        break;
     }

#ifdef DEBUG
   printf(" Tag #%d ; value: %d \n",i,value);
#endif

/* Decoding the tag number for each tag: */
   switch(tag_type)
   {
/* Width of the image: 256 or 0x100*/
      case 256 :
        *nx = value;
        break;
/* Length of the image: 257 or 0x101*/
      case 257 :
        *ny = value;
        break;
/* Photometric interpretation: 262 or 0x106*/
      case 262 :
         switch(value)
          {
            case 0:
/* JLP97: I switch black/white: */
              printf(" Grey map (0=white)\n");
              break;
            case 1:
              printf(" Grey map (0=black)\n");
              break;
            case 2:
              printf(" Color map (RVB)\n");
              break;
          }
        break;
/* Strip offset (pointer to the beginning of the image) */
      case 273 :
        strip_offset = value;
        break;
   }

  }

#ifdef DEBUG
printf(" nx = %d ny = %d strip_offset = %d\n",*nx,*ny,strip_offset);
#endif

/***************************************************************/
/* Now the data: */

/* Allocate memory space: */
nbytes_to_read = *nx * *ny * sizeof(char);
if(!( char_array = malloc(nbytes_to_read) ) )
  {printf("rtiff/fatal error: no memory space available!\n");
   *status = -1; return(*status);
  }

/* Go to the beginning of the data (relative to the beginning of the file) */
if(fseek(fd,strip_offset,SEEK_SET))
  {printf("rdtiff/Fatal: fseek error looking for the data.\n"); return(-6);}

/* Read data: */
nbytes = fread(char_array,sizeof(char),nbytes_to_read,fd);
if(nbytes != nbytes_to_read)
  {
  printf("rdtiff/error reading input file: >%s< \n",in_name);
  printf("       Only %d bytes read \n", nbytes);
  *status = -3; return(*status);
  }

/* Transfer to real array: */
if(!(*real_array = (float *)malloc(*nx * *ny * sizeof(float))) )
  {printf("rtiff/fatal error: no memory space available!\n");
   *status = -1; return(*status);
  }
for(j = 0; j < *ny; j++) 
  for(i = 0; i < *nx; i++) 
    (*real_array)[i + j * *nx] = 
/* JLP97: I switch black/white: */
/* JLP97: I replace this  
          (float)(255 - char_array[i + (*ny - j - 1) * *nx]);  
by:
*/
          (float)(char_array[i + (*ny - j - 1) * *nx]);  

/* Frees memory: */
   free(char_array);

/* Closes the input file */
  fclose(fd);

  *status = 0.;
  return(*status);
}
/**************************************************************
* Swap two bytes of an unsigned short integer
***************************************************************/
static void swap_int(unsigned short *i)
{
union {
unsigned short ii;
char         ch[2];
      } tmp;
char ch0; 

tmp.ii = *i;

#ifdef DEBUG
printf(" swap_int/before: *i= %d ch[O,1] %d %d \n",*i,tmp.ch[0],tmp.ch[1]);
printf(" swap_int/before: tmp.ii= %d  \n",tmp.ii);
#endif

ch0 = tmp.ch[0]; 
tmp.ch[0] = tmp.ch[1]; 
tmp.ch[1] = ch0; 
*i = tmp.ii;

#ifdef DEBUG
printf(" swap_int/after: *i= %d ch[O,1] %d %d \n",*i,tmp.ch[0],tmp.ch[1]);
#endif
}
/**************************************************************
* Inversion of an unsigned long integer
* From [0 1 2 3] to [3 2 1 0]
***************************************************************/
static void swap_lint(unsigned long *i)
{
union {
unsigned long int ii;
char         ch[4];
      } tmp;
char ch0, ch1; 

tmp.ii = *i;
ch0 = tmp.ch[0]; 
ch1 = tmp.ch[1]; 
tmp.ch[0] = tmp.ch[3]; 
tmp.ch[1] = tmp.ch[2]; 
tmp.ch[2] = ch1; 
tmp.ch[3] = ch0; 
*i = tmp.ii;
}
/****************************************************************
* Subroutine JLP_RDTIFF to read TIFF format 
****************************************************************/
int JLP_RDTIFF(float *real_array, INT4 *nx, INT4 *ny, INT4 *idim,
               char *in_name, char *comments, int *status)
{
printf("JLP_RDTIFF/Error: not yet available\n");
*status = -1;
return(*status);
}
/****************************************************************
* Subroutine JLP_WRTIFF to write in TIFF format 
****************************************************************/
int JLP_WRTIFF(float *real_array, INT4 *nx, INT4 *ny, INT4 *idim,
               char *out_name, char *comments, int *status)
{
printf("JLP_WRTIFF/Error: not yet available\n");
*status = -1;
return(*status);
}
