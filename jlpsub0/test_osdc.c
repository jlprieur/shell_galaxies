/*  jlp0_osd.c 
 *  Routines to access to files (Fortran interface to osd.c, in oslib.a)
 */
#include <stdio.h>
#include <time.h>

#include <jlp_ftoc.h>

/*  Main routine used for debugging...*/ 
main()
{
  int no_int,istatus;
  int t[1000],tt[1000];
  int i,istat,imode,fid,nochar;
  char filename[41],string[41];
  printf(" TEST program for jlp0_osd\n");

  i=10;
  jlp_getenv("JLP_PROMPT",&i,string,&istat);
  printf(" string: >%s< \n",string);

  no_int=100;
  for (i=0; i<no_int ; i++) t[i]=i;
  printf(" t(i), i=10, %d \n",t[10]);
  printf(" t(i), i=20, %d \n",t[20]);
  printf(" t(i), i=30, %d \n",t[30]);

  printf(" Input file with binary data (to write) ?");
  scanf("%s",filename);

/* imode: 0=read 1=write 2=read_write 3=append */
  imode=1;
  JLP_OSDOPEN(filename,&imode,&fid,&istat);
  printf(" open (write): istat : %d\n",istat);
  printf(" fid %d\n",fid);

  nochar=no_int*sizeof(int);
  JLP_OSDWRITE(&fid,&t[0],&nochar,&istat);
  printf(" write : istat : %d\n",istat);

  JLP_OSDCLOSE(&fid,&istat);
  printf(" close : istat : %d\n",istat);

  printf(" Input file with binary data (integer) (to read) ?");
  scanf("%s",filename);

  imode=0;
  JLP_OSDOPEN(filename,&imode,&fid,&istat);
  printf(" open (read) : istat : %d\n",istat);
  printf(" fid %d\n",fid);

  printf(" read : number of values (wanted): %d\n",nochar);
  JLP_OSDREAD(&fid,&tt[0],&nochar,&istat);
  printf("      number of values (actually read): %d\n",nochar);
  printf(" tt(i), i=10, %d \n",tt[10]);
  printf(" tt(i), i=20, %d \n",tt[20]);
  printf(" tt(i), i=30, %d \n",tt[30]);

  JLP_OSDCLOSE(&fid,&istat);
  printf(" close : istat : %d\n",istat);
}
/* End of test program */
