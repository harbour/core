/*
 * $Id$
 */

#ifndef HARB_H_
#define HARB_H_

/*  ---------------------------------------------
   Definitions, copied from Harbour.y and types.h    */

/*
 * Note: I don't think it is such a great idea to undef LONG...
 */
#if defined(LONG)
#undef LONG                            /* 4 bytes unsigned */
#endif

typedef long LONG;
typedef unsigned long ULONG;
#define PATH_DELIMITER  "/\\"
#define IS_PATH_SEP( c ) (strchr(PATH_DELIMITER, (c))!=NULL)
#define OPT_DELIMITER  "/-"
#define IS_OPT_SEP( c ) (strchr(OPT_DELIMITER, (c))!=NULL)
typedef struct _PATHNAMES { /* the list of pathnames to search with #include */
  char *szPath;
  struct _PATHNAMES *pNext;
} PATHNAMES;
#ifndef _POSIX_PATH_MAX
   #define _POSIX_PATH_MAX    255
#endif
typedef struct             /* support for filenames */
{
  char _buffer[ _POSIX_PATH_MAX+3 ];
  char *path;
  char *name;
  char *extension;
} FILENAME;

FILENAME *SplitFilename( char * );  /* splits filename into a path, a name and an extension */
char *MakeFilename( char *, FILENAME *);  /* joins a path, a name an an extension int filename */
void * OurMalloc( LONG lSize ); /* our malloc with error control */
void * _xgrab( ULONG );         /* allocates fixed memory */
void * _xrealloc( void *, ULONG );       /* reallocates memory */
void _xfree( void * );            /* frees fixed memory */

/* ------------------------------------------ */
typedef struct
{
  char *name;
  char *pars;
  int npars;
  char *value;
} DEFINES;

typedef struct
{
  int com_or_xcom;
  char *name;
  char *mpatt;
  char *value;
} COMMANDS, TRANSLATES;

#endif  /* HARB_H_ */
