/*
 * These are subroutines to read unformatted files, given a
 * description of their vaguaries in a `filecap' file. The files are
 * permitted to have records which are padded either at the front or
 * the back (or both) with extraneous bytes, to have a padding in front
 * of the entire file, and to have a header. Various fields are extracted
 * from this header, if requested in the filecap file.
 *
 * Filecap capabilities used are :
 *
 * DA		DAta type	char fits float(D) int long short (string)
 * FS		File Start	Unwanted bytes at start of file
 * HS		Header Size	Size of header (alias: HH)
 * NS		No Swap		Don't swap bytes for FITS, even on (e.g.) a vax
 * RE		Record End	Unwanted bytes at end of record
 * RL		Record Length	Number of useful bytes per record
 * RS		Record Start	Unwanted bytes at start of record
 * nx,ny	Number X, Y	byte offsets of X and Y sizes of data
 *				within the first record, sizes are ints
 *
 * Most parameters are optional, and will default to 0. If RE or RS is 
 * specified, you must give RL as well. If you specify it as -ve, then 
 * we'll look for it from the O/S.
 * Nx and ny must be present. Note that HH excludes FS, so HH will usually
 * be 2*sizeof(int) irrespective of the value of FS, and nx will usually be 0.
 */
#include <stdio.h>
#include "/users/prieur/smongo/src/mongo.h"
/* #include "/users/prieur/smongo/src/plotsub/image.h" */
typedef struct {
   char name[40];
   int nx,ny;				/* dimensions of image */
   char *space;				/* storage for image */
   float **ptr,				/* pointers to rows of image */
	 xmin,ymin,			/* limits of image */
	 xmax,ymax;
} IMAGE;

IMAGE *get_image();
#ifdef vms
#  include <stat.h>
#else
#  include <sys/types.h>
#  include <sys/stat.h>
#endif

#define SSIZE 40			/* size for small buffers */
extern int interrupt,			/* respond to ^C */
	   verbose;			/* control volubility */
#define CONVERT			/* convert cbuff to float in zz */	\
   { register int jj;							\
     switch(dtype) {							\
       case CHAR:							\
	 for(jj = 0;jj < rsize;jj++)					\
	    *((float *)&zz[zi] + jj) = *(cbuff+jj); break;		\
       case INT:							\
	 for(jj = 0;jj < rsize/sizeof(int);jj++)			\
	    *((float *)&zz[zi] + jj) = *((int *)cbuff + jj); break;	\
       case LONG:							\
	 for(jj = 0;jj < rsize/sizeof(long);jj++)			\
	    *((float *)&zz[zi] + jj) = *((long *)cbuff + jj); break;	\
       case SHORT:							\
	 for(jj = 0;jj < rsize/sizeof(short);jj++)			\
	    *((float *)&zz[zi] + jj) = *((short *)cbuff + jj); break;	\
       default:								\
	 msg("Impossible dtype in contour!\n"); abort();	       	\
     }}
#define BITS_IN_CHAR 8			/* convert FITS BITPIX to sizeof */
#define CHAR 0				/* symbolic names for dtype */
#define FLOAT 1
#define INT 2
#define LONG 3
#define SHORT 4
#define SIZE 10240		/* size of buffer to use in simple cases */

extern int interrupt,			/* respond to ^C */
	   verbose;			/* control volubility */
/**************************************************************
*/
IMAGE *
get_image(filename)
char *filename;
{
   char arr[SIZE],			/* array for header */
   	*cbuff,				/* buffer for converting to floats */
   	*data_type,			/* type of data */
     	filecap[SSIZE],			/* name of filecap file */
	*file_type,			/* name of entry in filecap */
	*get_val(),
	*malloc(),
	*print_var(),
	*ptr,
   	*strncpy(),
	*zz;				/* storage for z */
   static char *get_capstr();
   float bscale =  0.0,			/* if non-zero, */
   	 bzero = 0.0,			/* 	  data = bzero + bscale*data */
   	 xmin,xmax,			/* range of x and */
   	 ymin,ymax,			/* 	  y coordinates */
   	 **z;				/* pointers to image */
   IMAGE *image;			/* the image to create */
   int dsize,				/* size of each datum */
       dtype,				/* type of data in file */
       fd = -1,				/* file descriptor for data */
       fits = 0,			/* is it a FITS image? */
       i,				/* index, esp. to data as read */
       file_start,			/* dead space at start of file */
       head_size,			/* size of header */
       have_xlims = 0,have_ylims = 0,	/* were x/y limits on header? */
       no_swap,				/* don't allow byte swap for FITS */
       nx,ny,				/* dimension of image */
       rec_start,			/* dead space at start of record */
       rec_len,				/* length of data per record */
       rec_end,				/* dead space at end of record */
       rsize,				/* number of bytes to read at a time */
       size,				/* space needed for data */
       zi;				/* index into z; != i if not floats */
   static int find_entry(),
   	      get_recl(),
   	      is_capval(),
   	      read_fits_header();
   long lseek();
   static void swap_2(),
   	       swap_4(),
   	       restore_region(),
   	       save_region();

   if((fd = open(filename,0)) < 0) {
      msg_1s("Can't open %s\n",filename);
      return(NULL);
   }
/*
 * Find the `filecap' file, then the characteristics of file,
 * then read the header and then the data itself
 * Note that the filecap entries will usually be in the graphcap file,
 * and this will be assumed if we can't find filecap.
 */
   if((ptr = get_val("filecap")) == NULL) {
      if((ptr = get_val("graphcap")) == NULL) {
         msg("Can't find filecap/graphcap in environment file\n");
         return(NULL);
      }
   }
   (void)strncpy(filecap,ptr,SSIZE);
   file_type = print_var("file_type");
   if(file_type[0] == '\0') {
      msg("No variable file_type is defined, assuming \"C\"\n");
      find_entry("C",filecap);
   } else {
      find_entry(file_type,filecap);
   }

   file_start = get_capint("FS");
   if((head_size = get_capint("HS")) == 0) {
      head_size = get_capint("HH");	/* old name for it */
   }
   rec_start = get_capint("RS");
   rec_len = get_capint("RL");
   rec_end = get_capint("RE");
   data_type = get_capstr("DA");
   no_swap = is_capval("NS");
   if(*data_type == '\0') data_type = "float";	/* as a default */
/*
 * process values, looking for errors and special values.
 *
 * If HS is -ve, assume that the file has a record structure
 * that even applies to the header. If RL is specified, use it for HS.
 * Otherwise get it from the O/S.
 *
 * If RL is -ve, get it from the O/S
 */
   if(verbose > 0) {
      msg_1s("Data type is %s\n",data_type);
   }
   if(!strcmp(data_type,"char")) {
      dtype = CHAR; dsize = sizeof(char);
   } else if(!strcmp(data_type,"fits")) {
      fits = 1;
      if(rec_len != 0 && rec_len != 2880) {
	 msg("FITS files must have a record length of 2880\n");
      }
      head_size = rec_len = 2880;
   } else if(!strcmp(data_type,"float")) {
      dtype = FLOAT; dsize = sizeof(float);
   } else if(!strcmp(data_type,"int")) {
      dtype = INT; dsize = sizeof(int);
   } else if(!strcmp(data_type,"long")) {
      dtype = LONG; dsize = sizeof(long);
   } else if(!strcmp(data_type,"short")) {
      dtype = SHORT; dsize = sizeof(short);
   } else {
      msg_1s("Data type %s not supported\n",data_type);
      (void)close(fd);
      return(NULL);
   }

   if(head_size == 0) {
      msg_1d("You must specify HS in filecap; Assuming %d\n",2*sizeof(int));
      head_size = 8;
   } else if(head_size < 0) {
      if(rec_len > 0) {
	 head_size = rec_len;
      } else {
	 if((head_size = get_recl(fd)) < 0) {
	    msg_1s("Can't get record length for %s\n",filename);
	    close(fd);
	    return(NULL);
	 }
      }
      file_start += rec_start;		/* fake up the header */
      head_size += rec_end;
   }
   if(rec_start > SSIZE) {
      msg_1d("rec_start is too large, > %d\n",SSIZE);
      (void)close(fd);
      return(NULL);
   }
   if((rec_start || rec_end) && !rec_len) {
      msg("You must specify RL in filecap if you use RS or RE; Assuming -1\n");
      rec_len = -1;
   }
/*
 * Read header
 */
   if(fits) {			/* we already have rec_len */
      if((size = file_start + rec_start + rec_len + rec_end) > SIZE) {
	 msg("Array in get_image is too small to read header\n");
	 (void)close(fd);
         return(NULL);
      }
      if((i = read(fd,arr,size)) < size) {
	 msg("Error reading header\n");
	 (void)close(fd);
         return(NULL);
      }
/*
 * Process the header
 */
      nx = 0;				/* this is the first header record */
      for(;(i = read_fits_header(&arr[file_start + rec_start],&nx,&ny,
		&dtype,&dsize,&bscale,&bzero,&xmin,&xmax,&ymin,&ymax)) != 0;) {
	 if(i < 0) {			/* error on header */
	    (void)close(fd);
	    return(NULL);
	 } else {			/* not seen END card yet */
	    size -= file_start;
	    file_start = 0;		/* we've read file_start already */
	    if((i = read(fd,arr,size)) < size) {
	       msg("Error reading header\n");
	       (void)close(fd);
	       return(NULL);
	    }
	 }
      }
   } else {				/* Not FITS */
      if((size = file_start + head_size) > SIZE) {
	 msg("Array in get_image is too small to read header\n");
	 (void)close(fd);
	 return(NULL);
      }
      if((i = read(fd,arr,size)) < size) {
	 msg("Error reading header\n");
	 (void)close(fd);
	 return(NULL);
      }
      if(!is_capval("nx") || !is_capval("ny")) {
	 msg("Size of image is not present in header\n");
	 (void)close(fd);
	 return(NULL);
      }
/*
 * I hope that the values in filecap won't give an alignment problem
 */
      nx = *(int *)&arr[file_start + get_capint("nx")];
      ny = *(int *)&arr[file_start + get_capint("ny")];
/*
 * check that nx and ny are reasonable. Say, >= 0 && <= 10000
 */
      if(nx <= 0 || ny <= 0) {
	 msg_1d("You have nx = %d, ",nx);
	 msg_1d("ny = %d\n",ny);
	 (void)close(fd);
	 return(NULL);
      }
      if(nx > 10000 || ny > 10000) {	/* probably junk */
	 msg_1d("You have nx = %d, ",nx);
	 msg_1d("ny = %d, is this correct?\n",ny);
	 msg("enter 1 to continue  ");
	 (void)scanf("%d%*c",&i);
	 if(!i) {
	    (void)close(fd);
	    return(NULL);
	 }
      }
   }
/*
 * look for x/y limits of image,
 * and hope that the values in filecap won't give an alignment problem
 */
   if(is_capval("x0")) {
      if(!is_capval("x1")) {
	 msg("You must specify neither or both of x0 and x1 in a file\n");
      } else {
	 xmin = *(float *)&arr[file_start + get_capint("x0")];
	 xmax = *(float *)&arr[file_start + get_capint("x1")];
	 have_xlims = 1;
      }
   }
   if(is_capval("y0")) {
      if(!is_capval("y1")) {
	 msg("You must specify neither or both of y0 and y1 in a file\n");
      } else {
	 ymin = *(float *)&arr[file_start + get_capint("y0")];
	 ymax = *(float *)&arr[file_start + get_capint("y1")];
	 have_ylims = 1;
      }
   }
   
   if(verbose > 0) {
      msg_1s("Image %s's ",filename);
      msg_1d("dimensions are %d*",nx);
      msg_1d("%d\n",ny);
      if(have_xlims) {
	 msg_1f("x: %g - ",xmin);
	 msg_1f("%g\n",xmax);
      }
      if(have_ylims) {
	 msg_1f("y: %g - ",ymin);
	 msg_1f("%g\n",ymax);
      }
   }
/*
 * Now get the record length if we were asked for it. We couldn't get it
 * before because the first record (with header information) can have a
 * different value of rec_len
 */
   if(rec_len < 0) {		/* must get it from the O/S */
      if((rec_len = get_recl(fd)) < 0) {
	 msg_1s("Can't get record length for %s\n",filename);
	 close(fd);
	 return(NULL);
      }
   }
/*
 * Now we can read the real data. We must allow for the record structure
 * when reading it.
 */
   size = nx*ny*dsize;
   rsize = rec_start + rec_len + rec_end;
   if(rsize == 0) {		/* no record structure is specified */
      rsize = (size > SIZE) ? SIZE : size;
   }
   if(size <= 0) {
      msg("Non-positive storage requested for image\n");
      (void)close(fd);
      return(NULL);
   }
/*
 * Now get storage for data, and pointers to data
 * Note that we start reading at &zz[0], but that the first rec_start of
 * the record is junk. Allow for this by moving the z[] pointers up.
 * If we have to convert we convert to a different format we have to
 * allow for the space taken up by converting rec_start of data to float
 */
   if((zz = malloc(rec_start*sizeof(float)/dsize + nx*ny*sizeof(float)))
      								== NULL) {
      msg("Can't allocate image in get_image\n");
      (void)close(fd);
      return(NULL);
   }
   if(dtype != FLOAT) {			/* also need conversion buffer */
      if((cbuff = malloc(rsize)) == NULL) {
	 msg("Can't allocate cbuff in get_image\n");
	 free(zz); zz = NULL;
	 (void)close(fd);
	 return(NULL);
      }
   }
/*
 * allocate and set up the pointers to the image
 */
   if((z = (float **)malloc(ny*sizeof(float *))) == NULL) {
      msg("Can't allocate image pointers in get_image\n");
      free(zz); zz = NULL;
      if(dtype != FLOAT) free(cbuff);
      (void)close(fd);
      return(NULL);
   }
   for(i = 0;i < ny;i++) {
      z[i] = (float *)(zz + rec_start*sizeof(float)/dsize) + i*nx;
   }
/*
 * We'll now simply read the data if rec_start == 0, otherwise
 * save a few bytes in a temp space to avoid being overwritten by it.
 */
   if(rec_start == 0) {
      for(zi = i = 0;!interrupt && i < size;) {
	 if(rsize > size - i) {		/* may be junk at end of file */
	    rsize = size - i;
	 }
	 if(dtype == FLOAT) {
	    if((rsize = read(fd,&zz[zi],rsize)) <= 0) {
	       break;
	    }
	 } else {
	    if((rsize = read(fd,cbuff,rsize)) <= 0) {
	       break;
	    }
#ifdef vax
	    if(!no_swap) {
	       if(dsize == 2) {
		  swap_2(cbuff,rsize);
	       } else if(dsize == 4) {
		  swap_4(cbuff,rsize);
	       }
	    }
#endif
	    CONVERT;
	 }
	 i += rsize - rec_end;
	 zi += (rsize - rec_end)*sizeof(float)/(float)dsize;
      }
      if(i < size) {
	 i += rsize - rec_end;		/* how many bytes we read */
      }
   } else {
/*
 * Note that we start reading at &zz[0], but that the first rec_start of
 * the record is junk. We allowed for this by moving the z[] pointers up.
 */
      for(zi = i = 0;!interrupt && i < size;) {
         save_region(&zz[zi],rec_start);	/* save rec_start region */
	 if(rsize > size - i) {		/* may be junk at end of file */
	    rsize = size - i;
	 }
	 if(dtype == FLOAT) {
	    if((rsize = read(fd,&zz[zi],rsize)) <= 0) {
	       break;
	    }
	 } else {
	    if((rsize = read(fd,cbuff,rsize)) <= 0) {
	       break;
	    }
#ifdef vax
	    if(!no_swap) {
	       if(dsize == 2) {
		  swap_2(&cbuff[rec_start],rsize - rec_start);
	       } else if(dsize == 4) {
		  swap_4(&cbuff[rec_start],rsize - rec_start);
	       }
	    }
#endif
	    CONVERT;
	 }
         restore_region(&zz[zi],rec_start);	/* and restore it */
	 i += rec_len;
	 zi += rec_len*sizeof(float)/(float)dsize;
      }
      if(i < size) {
	 i += rsize - rec_start;	/* how many bytes we read */
      }
   }
   if(i < size) {			/* we didn't find enough data */
      msg_1s("Data in file %s ",filename);
      msg_1d("contained %d ",i);
      msg_1d("not %d bytes\n",size);
   } else {				/* check that file is all read */
      if(read(fd,arr,SIZE) > rec_len) {
	 msg_1d("At least %d bytes remain unread at end of data file\n");
      }
   }

   (void)close(fd);
   if(dtype != FLOAT) {			/* also free conversion buffer */
      free(cbuff);
   }
   if((image = (IMAGE *)malloc(sizeof(IMAGE))) == NULL) {
      fprintf(stderr,"Can't allocate storage for image structure\n");
      return(NULL);
   }
   strncpy(image->name,filename,sizeof(image->name) - 1);
   image->nx = nx;
   image->ny = ny;
   if(have_xlims) {			/* from file header */
      image->xmin = xmin;
      image->xmax = xmax;
   } else {
      image->xmin = 0;
      image->xmax = nx - 1;
   }
   if(have_ylims) {
      image->ymin = ymin;
      image->ymax = ymax;
   } else {
      image->ymin = 0;
      image->ymax = ny - 1;
   }
   image->ptr = z;
   image->space = zz;
/*
 * scale image if neccesary
 */
   if(bscale != 0.0 || bzero != 0.0) {	/* it is */
      register float *zend,*zptr;

      for(zptr = z[0],zend = z[0] + nx*ny;zptr < zend;zptr++) {
	 *zptr = *zptr*bscale + bzero;
      }
   }
   return(image);
}

/******************************************************/
static char temp[SSIZE];		/* save region */

static void save_region(ptr,n)
char *ptr;
int n;
{
   char *end,
	*tptr;

   tptr = temp;
   end = tptr + n;
   
   while(tptr < end) *tptr++ = *ptr++;
}

/***************************************************************
*/
static void restore_region(ptr,n)
char *ptr;
int n;
{
   char *end,
	*tptr;

   tptr = temp;
   end = tptr + n;
   
   while(tptr < end) *ptr++ = *tptr++;
}

/*******************************************************/
/*
 * Get the record length of the current record
 */
static int get_recl(fd)
int fd;					/* current file */
{
   int recl;
#ifndef unix
   struct stat stat_buff;	/* used by fstat to interrogate system */
#endif
   
#ifdef unix
   (void)read(fd,&recl,sizeof(int));	/* first int is record length */
   (void)lseek(fd,-sizeof(int),1);
#else
/*
 * Either we are on vms, when this'll work, or else it won't compile
 */
   if(fstat(fd,&stat_buff) != 0) {
      (void)close(fd);
      return(-1);
   }
   recl = stat_buff.st_fab_mrs;		/* record length for VMS */
#endif
   return(recl);
}

/***************************************************/
/*
 * Now routines to administer `filecap' - actually just interfaces
 * to the general *cap routines used primarily by graphcap.
 */
#include "/users/prieur/smongo/src/tty.h"

static TTY *file_desc;		/* descriptor for filecap */

/***************************************************************
*/
static int find_entry(name,file)
char name[],
     file[];
{
   TTY *ttyopen();

   if((file_desc = ttyopen(file,name,NULL)) == NULL) {
      msg("ttyopen returns NULL in find_entry()\n");
      return(-1);
   }
   tty_index_caps(file_desc,file_desc->t_capcode,file_desc->t_capindex);
   return(0);
}

/***************************************************************
*/
static int get_capint(cap)
char *cap;
{
   int ttygeti();

   if(file_desc == NULL) return(0);	/* file not correctly opened */
   return(ttygeti(file_desc,cap));
}

/***************************************************************
 * Does capability exist?
 */
static int is_capval(cap)
char *cap;
{
   int ttygeti();

   if(file_desc == NULL) return(0);	/* file not correctly opened */
   return(ttygetb(file_desc,cap));
}


/***************************************************************
*/
static char *get_capstr(cap)
char *cap;
{
   static char capstr[21];

   if(file_desc == NULL) return("\0");	/* file not correctly opened */
   if(ttygets(file_desc,cap,capstr,20) == 0) capstr[0] = '\0';
   return(capstr);
}

/***************************************************************
*/
static int read_fits_header(record,nx,ny,dtype,dsize,bscale,bzero,xmin,xmax,ymin,ymax)
char *record;
int *nx,*ny,				/* size of file */
    *dtype,				/* type of data */
    *dsize;				/* length of data */
float *bscale,				/* data = bzero + I*bscale */
      *bzero,
      *xmin,*xmax,			/* range of values on the x axis */
      *ymin,*ymax;			/* and on the y axis */
{
   char buff[81],			/* for reading value (81 plays safe) */
   	*keyword,			/* keyword from a FITS card */
   	*value;				/* value of a FITS keyword */
   double atof();
   int atoi(),
       i,
       naxis;				/* number of axes in data */
   static int read_card();

   i = 0;
   if(*nx == 0) {			/* first record of header */
      if(read_card(record,&keyword,&value) < 0) {
	 return(-1);
      }
      if(strcmp(keyword,"SIMPLE") != 0) {
	 msg("First keyword on FITS header isn't SIMPLE\n");
	 return(-1);
      }
      sscanf(value,"%s",buff);
      if(!strcmp(buff,"T")) {		/* OK, simple FITS. Good */
	 *dtype = INT;			/* for now, pending value of BITPIX */
      } else if(!strcmp(buff,"F")) {	/* Assume it is floating */
	 if(verbose) {
	    msg("FITS file isn't simple, assuming `float'\n");
	 }
	 *dtype = FLOAT;
      } else {
	 msg_1s("Illegal value for keyword SIMPLE : %s\n",value);
	 return(-1);
      }
      i++; record += 80;
      
      if(read_card(record,&keyword,&value) < 0) {
	 return(-1);
      }
      if(strcmp(keyword,"BITPIX") != 0) {
	 msg("Second keyword on FITS header isn't BITPIX\n");
	 return(-1);
      }
      *dsize = atoi(value)/BITS_IN_CHAR;
      if(*dtype == FLOAT && *dsize != 4) {
	 msg_1d("Illegal value of BITPIX for floating data: %d\n",atoi(value));
	 return(-1);
      } else if(*dsize == 1) {
	 *dtype = CHAR;
      } else if(*dsize == 2) {
	 *dtype = SHORT;
      } else if(*dsize == 4) {
	 *dtype = LONG;
      } else {
	 msg_1d("Illegal value of BITPIX: %d\n",atoi(value));
	 return(-1);
      }
      i++; record += 80;
      
      if(read_card(record,&keyword,&value) < 0) {
	 return(-1);
      }
      if(strcmp(keyword,"NAXIS") != 0) {
	 msg("Third keyword on FITS header isn't NAXIS\n");
	 return(-1);
      }
      naxis = atoi(value);
      if(naxis != 1 && naxis != 2) {
	 msg_1d("NAXIS isn't 1 or 2 (it's %d)\n",naxis);
	 return(-1);
      }
      i++; record += 80;
      
      if(read_card(record,&keyword,&value) < 0) {
	 return(-1);
      }
      if(strcmp(keyword,"NAXIS1") != 0) {
	 msg("Fourth keyword on FITS header isn't NAXIS1\n");
	 return(-1);
      }
      *nx = atoi(value);
      i++; record += 80;
      
      if(naxis == 1) {
	 *ny = 1;
      } else {
	 if(read_card(record,&keyword,&value) < 0) {
	    return(-1);
	 }
	 if(strcmp(keyword,"NAXIS2") != 0) {
	    msg("Fifth keyword on FITS header isn't NAXIS2\n");
	    return(-1);
	 }
	 *ny = atoi(value);
	 i++; record += 80;
      }
   }
/*
 * We've read the compulsary header, now look for END
 */
   for(;i < 36;i++) {
      if(read_card(record,&keyword,&value) < 0) {
	 return(-1);
      } else if(!strcmp(keyword,"BSCALE")) {
	 *bscale = atof(value);
      } else if(!strcmp(keyword,"BZERO")) {
	 *bzero = atof(value);
      } else if(!strcmp(keyword,"X0")) {
	 *xmin = atof(value);
      } else if(!strcmp(keyword,"X1")) {
	 *xmax = atof(value);
      } else if(!strcmp(keyword,"Y0")) {
	 *ymin = atof(value);
      } else if(!strcmp(keyword,"Y1")) {
	 *ymax = atof(value);
      } else if(!strcmp(keyword,"END")) {
	 return(0);
      }
      record += 80;
   }
   return(1);
}
/***************************************************************
*/
static int read_card(card,keyword,value)
char *card,
     **keyword,
     **value;
{
   int i;
   
   *keyword = &card[0];

   if(!(!strncmp(*keyword,"COMMENT",7) || !strncmp(*keyword,"END",3) ||
	!strncmp(*keyword,"HISTORY",7) || !strncmp(*keyword,"        ",8))) {
      if(card[8] != '=' || card[9] != ' ') {
	 (*keyword)[8] = '\0';
	 if(verbose) {
	    msg_1s("Missing = for keyword %s\n",*keyword);
	 }
      }
   }

   (*keyword)[8] = '\0';
   for(i = 7;i >= 0;i--) {
      if((*keyword)[i] == ' ') (*keyword)[i] = '\0';
   }
   if((*keyword)[0] == '\0') {
      msg("missing keyword in FITS header\n");
      return(-1);
   }

   *value = &card[10];
   (*value)[30] = '\0';			/* might truncate char values
					   but we don't use any */
   return(0);
}

/*******************************************************************
 * Byte swap ABABAB -> BABABAB in place
 */
static void swap_2(arr,n)
char *arr;				/* array to swap */
int n;					/* number of bytes */
{
   char *end,
   	t;

   if(n%2 != 0) {
      msg_1d("Attempt to byte swap odd number of bytes: %d\n",n);
      n = 2*(int)(n/2);
   }
   
   for(end = arr + n;arr < end;arr += 2) {
      t = arr[0];
      arr[0] = arr[1];
      arr[1] = t;
   }
}

/*******************************************************************
 * Byte swap ABCDABCD -> DCBADCBA in place (e.g. sun <--> vax)
 */
static void swap_4(arr,n)
char *arr;				/* array to swap */
int n;					/* number of bytes */
{
   char *end,
   	t;

   if(n%4 != 0) {
      msg_1d("Attempt to byte swap non-multiple of 4 bytes: %d\n",n);
      n = 4*(int)(n/4);
   }
   
   for(end = arr + n;arr < end;arr += 4) {
      t = arr[0];
      arr[0] = arr[3];
      arr[3] = t;
      t = arr[1];
      arr[1] = arr[2];
      arr[2] = t;
   }
}

