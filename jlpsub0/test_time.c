#include <stdio.h>
#include "jlp_ftoc.h"
main()
{
char date[80];
long status;
JLP_CTIME(date,&status);
printf("date: %s \n",date);
}
