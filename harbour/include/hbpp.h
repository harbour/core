/*
 * $Id$
 */

/* Definitions shared by  harbour.y and preprocessor */

#ifndef HB_PP_H_
#define HB_PP_H_

#include "hbsetup.h"
#include "hbdefs.h"

typedef struct _PATHNAMES { /* the list of pathnames to search with #include */
  char *szPath;
  struct _PATHNAMES *pNext;
} PATHNAMES;

typedef struct             /* support for filenames */
{
  char _buffer[ _POSIX_PATH_MAX+3 ];
  char *path;
  char *name;
  char *extension;
} FILENAME;

struct _DEFINES;
typedef struct _DEFINES
{
  char *name;
  char *pars;
  int npars;
  char *value;
  struct _DEFINES *last;
} DEFINES;

struct _COMMANDS;
typedef struct _COMMANDS
{
  int com_or_xcom;
  char *name;
  char *mpatt;
  char *value;
  struct _COMMANDS *last;
} COMMANDS;

#define STR_SIZE 8192
#define BUFF_SIZE 2048

#define SKIPTABSPACES(sptr) while ( *sptr == ' ' || *sptr == '\t' ) (sptr)++

#define PATH_DELIMITER  "/\\"
#define IS_PATH_SEP( c ) (strchr(PATH_DELIMITER, (c))!=NULL)
#define OPT_DELIMITER  "/-"
#define IS_OPT_SEP( c ) (strchr(OPT_DELIMITER, (c))!=NULL)

/* HBPP.C exported functions */

extern int ParseDirective( char* );
extern int ParseExpression( char*, char* );
extern int pp_RdStr(FILE*,char *,int,int,char*,int*,int*);
extern int pp_WrStr(FILE*,char *);
extern int strolen ( char* );
extern int strocpy (char*, char* );
extern char* strodup ( char * );
extern DEFINES *AddDefine( char * szDefine, char * szValue ); /* add a new Lex define from the command line */

/* HBPP.C exported variables */

extern int lInclude;
extern int *aCondCompile, nCondCompile;
extern int nline;

extern char * _szPErrors[];
extern char * _szPWarnings[];

/* Needed support modules, but not contained in HBPP.C */

extern FILENAME *SplitFilename( char * );  /* splits filename into a path, a name and an extension */
extern char *MakeFilename( char *, FILENAME *);  /* joins a path, a name an an extension int filename */

extern void *   hb_xalloc( ULONG lSize );   /* allocates memory, returns NULL on failure */
extern void *   hb_xgrab( ULONG lSize );   /* allocates memory, exists on failure */
extern void     hb_xfree( void * pMem );    /* frees memory */
extern void *   hb_xrealloc( void * pMem, ULONG lSize );   /* reallocates memory */

/* Needed support variables, but not contained in HBPP.C */

extern PATHNAMES *_pIncludePath;
extern FILENAME *_pFileName;
extern DEFINES *topDefine;
extern COMMANDS *topCommand;
extern COMMANDS *topTranslate;

#endif /* HB_PP_H_ */
