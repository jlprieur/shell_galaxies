#ifndef esoext1_h
#define esoext1_h

#include <f77stat.h>
#include <esoext.h>

extern  int                 section;      /* program section          */
extern  int                   equal;      /* level zero equal sign    */
extern  int                   comma;      /* level zero comma         */
extern  int                  x_flag;      /* extension option flag    */
extern  int                     nlb;      /* present index in 'lbuf'  */
extern  int                  no_lid;      /* no. of line identifiers  */
extern  int                     sno;      /* current no. of labels    */
extern  int                statno[];      /* statement label no.      */
extern  int                do_level;      /* DO stack pointer         */
extern  int                do_label;      /* DO label                 */
extern  int                 id_size;      /* length of identifier     */
extern  int                   nstat;      /* char. index in 'stat'    */
extern  char                 stmt[];      /* present statement        */
extern  char   lbuf[MXLBUF][MXLINE];      /* buffer for input lines   */
extern  LID                   lid[];      /* list of line identifiers */

// f77utl.c
int f77_sect(int *ptype, int sect);
char *find_f77(FSTAT *list, char *id, int *pno, int *ptype);

// lidtbl.c
int chk_io(LID *plid);
int chk_exp(LID *plid);

// getline.c
char get_line(FILE *fp);                          /* add character to buffer  */

// putline.c
int put_line(FILE *ofp, int action, int stype, int labno);

// extutl.c
FILE *push_fp(FILE *fp, char *path, char *name);
char *push_lab(char *label);
char *pop_lab();
char *new_ext(char *name, char *ext);
char *new_file();
char *incl_file(char *line);

#endif
