/*
 * $Id$
 */

/* Definitions shared by  harbour.y and preprocessor */

#ifndef HB_PP_H_
#define HB_PP_H_

#include "hbsetup.h"
#include "hbdefs.h"

typedef struct _PATHNAMES { /* the list of pathnames to search with #include */
  char * szPath;
  struct _PATHNAMES *pNext;
} PATHNAMES;

struct _DEFINES;
typedef struct _DEFINES
{
  char * name;
  char * pars;
  int npars;
  char * value;
  struct _DEFINES * last;
} DEFINES;

struct _COMMANDS;
typedef struct _COMMANDS
{
  int com_or_xcom;
  char * name;
  char * mpatt;
  char * value;
  struct _COMMANDS * last;
} COMMANDS;

#define STR_SIZE 8192
#define BUFF_SIZE 2048

#define SKIPTABSPACES( sptr ) while( *sptr == ' ' || *sptr == '\t' ) ( sptr )++
#define IS_OPT_SEP( c ) ( strchr( OS_OPT_DELIMITER_LIST, ( c ) ) != NULL )

/* HBPP.C exported functions */

extern int ParseDirective( char * );
extern int ParseExpression( char *, char * );
extern int pp_RdStr( FILE *, char *, int, int, char *, int *, int * );
extern int pp_WrStr( FILE *, char * );
extern int strolen( char * );
extern int strocpy( char *, char * );
extern char * strodup( char * );
extern DEFINES * AddDefine( char * szDefine, char * szValue ); /* add a new Lex define from the command line */

/* HBPP.C exported variables */

extern int lInclude;
extern int * aCondCompile, nCondCompile;
extern int nline;

extern char * _szPErrors[];
extern char * _szPWarnings[];

/* Needed support modules, but not contained in HBPP.C */

/* Filename support */
typedef struct
{
  char   szBuffer[ _POSIX_PATH_MAX + 3 ];
  char * szPath;
  char * szName;
  char * szExtension;
} HB_FNAME, * PHB_FNAME, * HB_FNAME_PTR;

extern PHB_FNAME hb_fsFNameSplit( char * szFilename ); /* Split given filename into path, name and extension */
extern char *    hb_fsFNameMerge( char * szFileName, PHB_FNAME pFileName ); /* This function joins path, name and extension into a string with a filename */

extern void *   hb_xgrab( ULONG lSize );   /* allocates memory, exists on failure */
extern void *   hb_xrealloc( void * pMem, ULONG lSize );   /* reallocates memory */
extern void     hb_xfree( void * pMem );    /* frees memory */

/* Needed support variables, but not contained in HBPP.C */

extern PATHNAMES * _pIncludePath;
extern PHB_FNAME _pFileName;
extern DEFINES * topDefine;
extern COMMANDS * topCommand;
extern COMMANDS * topTranslate;

#endif /* HB_PP_H_ */
