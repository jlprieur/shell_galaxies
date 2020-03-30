/**********************************************************************
* jlp_system.c
* To substitute "system" routine when not available...
* Remember: system(command) send the command to the underlying shell..
* 
* JLP
* Version 10-02-2003
***********************************************************************/
#include <jlp_ftoc.h>
#if 0

/* Routine to test jlp_system */

#include <stdio.h>
#include <sys/types.h>

main()
{
char *pc, *getenv();
char string[61],symbol[41];
int status;

/*
pc = getenv("JLP_PROMPT");
strcpy(string,pc);
printf(" JLP_PROMPT = >%s< \n",string);

strcpy(symbol,"JLP_PROMPT");
pc = getenv(symbol);
strcpy(string,pc);
printf(" JLP_PROMPT = >%s< \n",string);

system("printenv JLP_PROMPT > /tmp/jlp_symbol.tmp");
*/

strcpy(string,"printenv JLP_PROMPT > tt");
status = system(string);
printf("status = %d \n",status);

strcpy(string,"printenv JLP_FORMAT > ttt");
/*
fork();
execl("/bin/sh", "sh", "-c", string, 0);
*/
status = JLP_SYSTEM(string);
printf("status = %d \n",status);

printf(" OK end \n");
}
#endif

#ifdef ibm
#define HAVE_NOT_SYSTEM
#endif
#ifdef dec 
#define HAVE_NOT_SYSTEM
#endif
#ifdef HAVE_NOT_SYSTEM 
/*
 * From SuperMongo, old version.
 *
 * This routine provides the unix system() call. If HAVE_SYSTEM is not
 * defined, you'll have to provide your own, or try the one provided
 */

/*
 * We don't have a system() call, and haven't faked one. Maybe this'll
 * compile, or at least provide a template
 */
#include <signal.h>
#include <sys/wait.h>

int JLP_SYSTEM(char *str)
{
   int dead_pid,
       pid,
       ret,
       (*sigint)(),
       (*sigquit)();
   int stat;

   if((pid = fork()) == 0) {		/* child */
      execl("/bin/sh","sh","-c",str,0);
      _exit(-1);
   }

/* ignore signals */
/*
   sigint = signal(SIGINT,SIG_IGN);
   sigquit = signal(SIGQUIT,SIG_IGN);
*/

   for(;;) {				/* wait until process finishes */
      dead_pid = wait(&stat);
      if(dead_pid == pid) {		/* that was our one */
	 ret = stat;
	 break;
      } else if(dead_pid == -1) {	/* no children left */
	 ret = -1;
	 break;
      }
   }

/* reinstate signals */
/*
   signal(SIGINT,sigint);
   signal(SIGQUIT,sigquit);
*/

   return(ret);
}
#else
/***************************************************************
* Otherwise use standard system routine:
***************************************************************/

int JLP_SYSTEM(char *string)
{
int status;
status = system(string);
return(status);
}
#endif
