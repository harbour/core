/*
 * $Id$
 */

#ifndef HB_HARB_H_
#define HB_HARB_H_

#include "hbsetup.h"
#include "hbdefs.h"
#include "hbpp.h" /* includes common definitions shared by preprocessor and harbour.y */

#define PATH_DELIMITER  "/\\"
#define IS_PATH_SEP( c ) (strchr(PATH_DELIMITER, (c))!=NULL)
#define OPT_DELIMITER  "/-"
#define IS_OPT_SEP( c ) (strchr(OPT_DELIMITER, (c))!=NULL)

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

FILENAME *SplitFilename( char * );  /* splits filename into a path, a name and an extension */
char *MakeFilename( char *, FILENAME *);  /* joins a path, a name an an extension int filename */
void * OurMalloc( LONG lSize ); /* our malloc with error control */
void * _xgrab( ULONG );         /* allocates fixed memory */
void * _xrealloc( void *, ULONG );       /* reallocates memory */
void _xfree( void * );            /* frees fixed memory */

#endif /* HB_HARB_H_ */
