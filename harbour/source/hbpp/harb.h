/*  ---------------------------------------------
   Definitions, copied from Harbour.y and types.h    */

#undef LONG                            /* 4 bytes unsigned */
typedef long LONG;
typedef unsigned long ULONG;
#define PATH_DELIMITER  "/\\"
#define IS_PATH_SEP( c ) (strchr(PATH_DELIMITER, (c))!=NULL)
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