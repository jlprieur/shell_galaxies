#include <stdio.h>
int main(int argc, char *argv[])
{
char *pc;
printf(" sizeof(int)=%d sizeof(long)=%d sizeof(short)=%d sizeof(address)=%d sizeof(char)\n",
       sizeof(int), sizeof(long), sizeof(short), sizeof(pc), sizeof(*pc));
printf(" sizeof(float)=%d sizeof(double)=%d sizeof(long double)=%d\n",
       sizeof(float), sizeof(double), sizeof(long double));

return(0);
}
