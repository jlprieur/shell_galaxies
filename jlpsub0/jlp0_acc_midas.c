/*****************************************************************
*
* Set of routines to read and write BDF MIDAS files
* (BDF = Bulk Data Frame).
* MIDAS Version MAY90.
*
* Contains:
* jlp_wrmidas,jlp_rdmidas,jlp_vm_rdmidas, JLP_SCSPRO, JLP_SCSEPI
*
*
* JLP
* Version 15-04-91
*****************************************************************/
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <string.h>

#include <jlp_ftoc.h>

#ifdef VAX
#include <stdlib.h>
# include <descrip.h>
# define DEC_STRING(V) struct dsc$descriptor *V
# define STRING_COPY(S,V,L) strncpy(S,V->dsc$a_pointer,L)
# define STRING_COPY1(V,S,L) strncpy(V->dsc$a_pointer,S,L)
#else
#define DEC_STRING(V) char *V
#define STRING_COPY(S,V,length) strncpy(S,V,length)
#define STRING_COPY1(V,S,length) strncpy(V,S,length)
#endif

/* Midas definitions : */

#define F_I_MODE     0L                /* map file for input only    */
#define F_O_MODE     1L                /* map file for output        */
#define F_IMA_TYPE	1L		/* type no. for image files	*/
#define F_TBL_TYPE	3L		/* type no. for table files	*/
#define F_FIT_TYPE	4L		/* type no. for fit files	*/
#define D_R4_FORMAT    10L		/* R*4 = 32 bit floating point	*/

static int jlp_rddmidas(long imno, char *jlp_descr, long *nvals);
static int jlp_wrdmidas(long imno, char *jlp_descr, long nvals);

/*------------------------------------------------------------------
* jlp_wrmidas
* Subroutine to create a new MIDAS BDF file :
------------------------------------------------------------------*/
int JLP_WRMIDAS(float *image, INT4 *nx, INT4 *ny, INT4 *idim,
                DEC_STRING(filename), DEC_STRING(comments),
                DEC_STRING(jlp_descr), INT4 *dflag, INT4 *istat)
{
 long naxis, dattype, iomode, filtype;
 long npix[2], imno, nvals;
 register int i, j;
 double start[2], step[2];
 char ident[72], cunit[48], fname[41], buffer[1001];
 float *pntr;
 long int err_cont, err_log, err_disp;

/* Error control cont=1 (continue) log=0 (no log since no log file...) 
   disp=1 (disp on errors)*/
 err_cont = 1; 
 err_log = 0;
 err_disp = 1;
 SCECNT("PUT",&err_cont,&err_log,&err_disp);

/* Sets the parameters for SCIPUT : */

	STRING_COPY(fname,filename,40); fname[40] = '\0' ;

/* Fills the complement of comments with blank characters: */
	STRING_COPY(ident,comments,71); 
	ident[71]='\0';
	i=0;
	while(ident[i] != '\0' && ident[i] != '/') i++; 
	while(i<79) {ident[i] = ' '; i++;}
	ident[79] = '\0';

	start[0] = 1.; start[1] = 1.;
	step[0] = 1.; step[1] = 1.;
	npix[0] = *nx; npix[1] = *ny;
	naxis = 2; if(*ny <= 1) naxis = 1;
	strcpy(cunit,"pixels          pixels          ");

/* SCIPUT opens the file and returns the pointer where to copy the
data */

 dattype = D_R4_FORMAT;
 iomode = F_O_MODE; filtype = F_IMA_TYPE;
*istat = SCIPUT(fname,dattype,iomode,filtype,naxis,npix,start,step,ident,cunit,&pntr,&imno);

	if (*istat != 0) 
	{
	printf(" Error in jlp_wrmidas/sciput, istat= %d \n",*istat);
	printf(" Error when opening ",filename);
	return(-1);}
 
/* Transfering the data: */

	for (j = 0; j < *ny; ++j) 
	{ for (i = 0; i < *nx; ++i) 
	   { *(pntr + (i + j * *nx)) = *(image + (i + j * *idim));} }

/* Descriptors: */
  if(*dflag == 1)
  {
   nvals = 500;
   STRING_COPY(buffer,jlp_descr,nvals);
   buffer[nvals] = '\0';
   jlp_wrdmidas(imno,buffer,nvals);
  }
  return(0);
}
/*------------------------------------------------------------------
* JLP_RDMIDAS
* Subroutine to read a MIDAS BDF file :
------------------------------------------------------------------*/
int JLP_RDMIDAS(float *image, INT4 *nx, INT4 *ny, INT4 *idim,
                DEC_STRING(filename), DEC_STRING(comments),
                DEC_STRING(jlp_descr), INT4 *dflag, INT4 *istat)
{
 long int naxis, dattype, iomode, filtype;
 long int npix[2], imno, nvals;
 register int i, j;
 double start[2], step[2];
 char ident[73], cunit[49], fname[41], buffer[1001], mycomments[80];
 float *pntr;
 long int err_cont, err_log, err_disp;

/* Error control cont=1 (continue) log=0 (no log since no log file...) 
   disp=1 (disp on errors)*/
 err_cont = 1; 
 err_log = 0;
 err_disp = 1;
 SCECNT("PUT",&err_cont,&err_log,&err_disp);

/* Sets the parameters for SCIGET : */
	for (i = 0; i < 48; ++i) cunit[i]=' '; cunit[48]='\0';
	for (i = 0; i < 72; ++i) ident[i]=' '; ident[72]='\0';
	STRING_COPY(fname,filename,40); fname[40] = '\0' ;
	naxis = 2;

/* SCIGET opens the file and returns the pointer of the first pixel: */

 dattype = D_R4_FORMAT;
 iomode = F_I_MODE; filtype = F_IMA_TYPE;
*istat = SCIGET(fname,dattype,iomode,filtype,2,&naxis,npix,start,step,ident,cunit,&pntr,&imno);

/*	printf(" istat %d \n",*istat);
	printf(" naxis %d npix %d %d \n",naxis,npix[0],npix[1]);
	printf(" step %f %f \n",step[0],step[1]);
	printf(" ident %s \n",ident);
*/
	if (*istat != 0) 
	{
	printf(" Error in jlp_rdmidas/sciget, istat= %d \n",*istat);
	printf(" Error when opening ",filename);
	return(-1);}
 
/* Fills the complement of comments with blank characters: */
	strncpy(mycomments,ident,71); 
	mycomments[71]='\0';
	i=0;
	while(mycomments[i] != '\0' && mycomments[i] != '/') i++; 
	while(i<79) {mycomments[i] = ' '; i++;}
	mycomments[79] = '\0';
        STRING_COPY1(comments,mycomments,80);

	*nx = npix[0] ; *ny = npix[1];

/* Transfering the data: */

	for (j = 0; j < *ny; ++j) 
	{ for (i = 0; i < *nx; ++i) 
	   { *(image + (i + j * *idim)) = *(pntr + (i + j * *nx)); }
	}

/* Descriptors: */
  if(*dflag == 1)
  {
   jlp_rddmidas(imno,buffer,&nvals);
   buffer[nvals-1]='\0';
   STRING_COPY1(jlp_descr,buffer,nvals);
  }
  return(0);
}
/*------------------------------------------------------------------
* JLP_VM_RDMIDAS
* Subroutine to read a MIDAS BDF file :
* dflag = descriptor flag (1 ==> read descriptors, 0 ==> do not read)
------------------------------------------------------------------*/
int JLP_VM_RDMIDAS(INT4 *pntr_image, INT4 *nx, INT4 *ny,
                   DEC_STRING(filename), DEC_STRING(comments),
                   DEC_STRING(jlp_descr), INT4 *dflag, INT4 *istat)
{
 long int naxis, dattype, iomode, filtype;
 long int npix[2], imno, nvals;
 register int i, j;
 double start[2], step[2];
 char ident[73], cunit[49], fname[41], mycomments[80], buffer[1001];
 float *pntr;
 long int err_cont, err_log, err_disp;

/* Error control cont=1 (continue) log=0 (no log since no log file...) 
   disp=1 (disp on errors)*/
 err_cont = 1; 
 err_log = 0;
 err_disp = 1;
 SCECNT("PUT",&err_cont,&err_log,&err_disp);

/* Sets the parameters for SCIGET : */
	for (i = 0; i < 48; ++i) cunit[i]=' '; cunit[48]='\0';
	for (i = 0; i < 72; ++i) ident[i]=' '; ident[72]='\0';
	STRING_COPY(fname,filename,40); fname[40] = '\0' ;
	naxis = 2;

/* SCIGET opens the file and returns the pointer of the first pixel: */

 dattype = D_R4_FORMAT;
 iomode = F_I_MODE; filtype = F_IMA_TYPE;
*istat = SCIGET(fname,dattype,iomode,filtype,2,&naxis,npix,start,step,ident,cunit,&pntr,&imno);

/*	printf(" istat %d \n",*istat);
	printf(" naxis %d npix %d %d \n",naxis,npix[0],npix[1]);
	printf(" step %f %f \n",step[0],step[1]);
	printf(" ident %s \n",ident);
*/
	if (*istat != 0) 
	{
	printf(" Error in jlp_vm_rdmidas/sciget, istat= %d \n",*istat);
	printf(" Error when opening ",filename);
	return(-1);}
 
/* Fills the complement of comments with blank characters: */
	strncpy(mycomments,ident,71); 
	mycomments[71]='\0';
	i=0;
	while(mycomments[i] != '\0' && mycomments[i] != '/') i++; 
	while(i<79) {mycomments[i] = ' '; i++;}
	mycomments[79] = '\0';
        STRING_COPY1(comments,mycomments,80);

	*nx = npix[0] ; *ny = npix[1];

/* Conversion of the pointer: */

	*pntr_image = (long int) pntr;

/* Descriptors: */
  if(*dflag == 1)
  {
   jlp_rddmidas(imno,buffer,&nvals);
   buffer[nvals-1]='\0';
   STRING_COPY1(jlp_descr,buffer,nvals);
  }
  return(0);
}
/* Complement for Fortran interface: */
int JLP_SCSPRO()
{
  SCSPRO("-1");
}
int JLP_SCSEPI()
{
  SCSEPI();
}
/*------------------------------------------------------------------
* jlp_rddmidas
* Subroutine to read descriptors from a MIDAS BDF file :
------------------------------------------------------------------*/
static int jlp_rddmidas(long imno, char *jlp_descr, long *nvals)
{
 int status;
 long int null, actvals;
 char *unit;

/* SCDRDC read the descriptors */ 
 status = SCDRDC(imno,"JLPDSC",1,1,500,&actvals,jlp_descr,&unit,&null);

*nvals = actvals;

if(status != 0)
{
  printf(" jlp_rddmidas/error SCDRDC/status = %d\n",status);
  *nvals = 0;
}

return(status);
}
/*------------------------------------------------------------------
* jlp_wrdmidas
* Subroutine to read descriptors from a MIDAS BDF file :
------------------------------------------------------------------*/
static int jlp_wrdmidas(long imno, char *jlp_descr, long nvals)
{
 int status;
 char *unit;

/* SCDWRC write the descriptors */ 
 status = SCDWRC(imno,"JLPDSC",1,jlp_descr,1,nvals,&unit);

if(status != 0)
{
  printf(" jlp_wrdmidas/error SCDWRC/status = %d\n",status);
}

return(status);
}
