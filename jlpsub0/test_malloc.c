#include <stdio.h>
main()
{
int size;
char buff[81], *pc;
while(1)
{
printf(" size= ");
gets(buff);
sscanf(buff,"%d",&size);
printf(" OK: size = %d \n",size);
size *= sizeof(char);
pc = (char *)malloc(size);
if( pc == NULL) printf(" Error \n");
else printf(" OK \n");
free(pc);
}
}
