/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Preprocesor
 *
 * Copyright 1999 Alexander S.Kresin <alex@belacy.belgorod.su>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/* Definitions shared by harbour.y and preprocessor */

#ifndef HB_PP_H_
#define HB_PP_H_

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

extern DEFINES * topDefine;
extern COMMANDS * topCommand;
extern COMMANDS * topTranslate;

#include "compiler.h"

#endif /* HB_PP_H_ */
