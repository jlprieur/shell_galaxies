#include <stdio.h>
#include <string.h>
main()
{
int i, j;
char *t1,*t2;
t1="                      ";
t2="                      ";
t1="test";
t2="test ";
i = strcspn(t1,"x");
printf(" string pos %d\n",i);
j=myprocess(t1,t2);
printf(" %s %s result: %d \n",t1,t2,j);
t1="test.*";
t2="test.bdf ";
j=myprocess(t1,t2);
printf(" %s %s result: %d \n",t1,t2,j);
t1="tes*.bdf";
t2="test.b";
j=myprocess(t1,t2);
printf(" %s %s result: %d \n",t1,t2,j);
t1="*es*bdf";
t2="test.df ";
j=myprocess(t1,t2);
printf(" %s %s result: %d \n",t1,t2,j);
t1="*.bdg";
t2="test.bdf ";
j=myprocess(t1,t2);
printf(" %s %s result: %d \n",t1,t2,j);
t1="*.bdf";
t2="testbdf ";
j=myprocess(t1,t2);
printf(" %s %s result: %d \n",t1,t2,j);
}

/* static */ int myprocess(str1,str2)
 char *str1, *str2;
 {
 int i, j, imax, jmax;

   imax = strlen(str1);
   jmax = strlen(str2);
   j = 0;
   for (i=0; i<imax; ++i)
     if(*(str1+i) != '*')
/* Exit if characters are not the same: */
       {if(*(str1+i) != *(str2+j)) return(-1);
	else j = j+1;}
/* Wild card, look for the index j of the next string in str2: */
      else
/* End of string, successful exit: */
       {if(i+1 >= imax) return(0);
       while(j < jmax && *(str2+j) != *(str1+i+1) ) ++j;
       if(j >= jmax) return(-2);
       printf(" i %d j %d \n",i,j);
       }
 }

