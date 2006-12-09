/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Native compiler object module generation from Harbour C output.
 *
 * Copyright 2001 Jos‚ Lal¡n <dezac@corevia.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

#include "hbcomp.h"
#include "hb_io.h"

/* Prototype */
static char * hb_searchpath( const char *, char *, char * );

#define HB_CFG_FILENAME    "harbour.cfg"

/* QUESTION: Allocate buffer dynamically ? */
#define HB_CFG_LINE_LEN    ( _POSIX_PATH_MAX )

/*--------------------------------------------------------------------------*/

/* Builds platform dependant object module from Harbour C output */
void hb_compGenCObj( HB_COMP_DECL, PHB_FNAME pFileName )
{
   char szFileName[ _POSIX_PATH_MAX + 1 ];
   char szLine[ HB_CFG_LINE_LEN ];
   char szCompiler[ HB_CFG_LINE_LEN + 1 ] = "";
   char szOptions[ HB_CFG_LINE_LEN + 1 ] = "";
   char szCommandLine[ HB_CFG_LINE_LEN * 2 + 1 ];
   char szOutPath[ _POSIX_PATH_MAX + 1 ] = "\0";
#if defined( HOST_OS_UNIX_COMPATIBLE )
   char szDefaultUnixPath[ _POSIX_PATH_MAX + 1 ] = "/etc:/usr/local/etc";
   char * pszEnv = szDefaultUnixPath;
   #define HB_NULL_STR " > /dev/null"
   #define HB_ACCESS_FLAG F_OK
#elif defined( OS_DOS_COMPATIBLE )
   char * pszEnv = hb_getenv( "PATH" );
   #define HB_NULL_STR " >nul"      
   #define HB_ACCESS_FLAG 0
#else
   char * pszEnv = NULL;
#endif
   FILE * yyc;
   char * pszCfg;
   BOOL bVerbose = FALSE;   /* Don't show C compiler messages (default). */
   BOOL bDelTmp = TRUE;     /* Delete intermediate C file (default). */
   int iSuccess;

   HB_SYMBOL_UNUSED( HB_COMP_PARAM );

   /* First pass: build the C output */

   /* Force file extension to avoid collisions when called from a make utility */
   pFileName->szExtension = ".c";
   hb_fsFNameMerge( szFileName, pFileName );
   hb_compGenCCode( HB_COMP_PARAM, HB_COMP_PARAM->pFileName );

   /* Begin second pass */

   /* Set up things  */

   /* Grab space */
   pszCfg = ( char * ) hb_xgrab( _POSIX_PATH_MAX + 1 );

   if( pszEnv && pszEnv[ 0 ] != '\0' && *hb_searchpath( HB_CFG_FILENAME, pszEnv, pszCfg ) )
   {

      yyc = fopen( pszCfg, "rt" );
      if( ! yyc )
      {
#if 0
         /* QUESTION: Add a new error to Harbour ?
            hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_OPEN_CFG, szFileName, NULL );
         */
#else
         printf( "\nError: Can't find %s file.\n", HB_CFG_FILENAME );
         return;
#endif
      }

      while( fgets( szLine, HB_CFG_LINE_LEN, yyc ) != NULL )
      {
         ULONG ulLen;
         char * szStr = szLine;
         char * szToken;

         /* Trim left */
         while( HB_ISSPACE( *szStr ) )
            szStr++;

         /* Trim right */
         ulLen = strlen( szStr );
         while( ulLen && HB_ISSPACE( szStr[ ulLen - 1 ] ) )
            ulLen--;

         szStr[ ulLen ] = '\0';
         /* TODO: Check for comments within macros, i.e: CC=bcc32 #comment */

         if( *szStr )
            {
               szToken = strchr( szStr, '=' );
            
               if( szToken )
               {
                  *szToken++ = '\0';
                  if ( *szToken )
                  {
                     /* Checks compiler name */
                     if( ! hb_stricmp( szStr, "CC" ) )
                     {
                        snprintf( szCompiler, sizeof( szCompiler ), "%s", szToken );
                     }
                     /* Checks optional switches */
                     else if( ! hb_stricmp( szStr, "CFLAGS" ) )
                     {
                        snprintf( szOptions, sizeof( szCompiler ), "%s", szToken );
                     }
                     /* Wanna see C compiler output ? */
                     else if( ! hb_stricmp( szStr, "VERBOSE" ) )
                     {
                        if( ! hb_stricmp( szToken, "YES" ) )
                           bVerbose = TRUE;
                     }
                     /* Delete intermediate C file ? */
                     else if( ! hb_stricmp( szStr, "DELTMP" ) )
                     {
                        if( ! hb_stricmp( szToken, "NO" ) )
                           bDelTmp = FALSE;
                     }
                  }
               }
            }
      }

      fclose( yyc );
   }

   #if defined( OS_DOS_COMPATIBLE ) 
   {
      if( pszEnv )
         hb_xfree( ( void * ) pszEnv );
   }
   #endif


   if( ! HB_COMP_PARAM->fQuiet )
   {
      printf( "Building object module output for \'%s\'...", szFileName );
      fflush( stdout );
   }

   /* Check if -o<path> was used */
   if( HB_COMP_PARAM->pOutPath )
   {
      PHB_FNAME pOut = hb_fsFNameSplit( ( char * ) szFileName );
      char pszTemp[ _POSIX_PATH_MAX + 1 ] = "";

      if( HB_COMP_PARAM->pOutPath->szPath )
         pOut->szPath = HB_COMP_PARAM->pOutPath->szPath;

#if defined(__BORLANDC__) || defined(_MSC_VER) || defined(__WATCOMC__)
      pOut->szExtension = ".obj";
#else
      pOut->szExtension = ".o";  /* Don't know if we can hardcode it for Un*x */
#endif
      hb_fsFNameMerge( pszTemp, pOut );

#if defined(_MSC_VER)
      hb_strncat( szOutPath, "-Fo", sizeof( szOutPath ) - 1 );
#elif defined(__WATCOMC__)
      hb_strncat( szOutPath, "-fo=", sizeof( szOutPath ) - 1 );      
#else
      hb_strncat( szOutPath, "-o", sizeof( szOutPath ) - 1 );
#endif

      hb_strncat( szOutPath, pszTemp, sizeof( szOutPath ) - 1 );

      hb_xfree( pOut );
   }

   if( *szCompiler )
   {
      snprintf( szCommandLine, sizeof( szCommandLine ), "%s %s %s %s", szCompiler, szOptions, szOutPath, szFileName );
      
      if( bVerbose )
      {
         printf( "\n%s\n", szCommandLine ) ;
      }
      else
      {
         hb_strncat( szCommandLine, HB_NULL_STR, sizeof( szCommandLine ) );
      }

      /* Compile it! */
      iSuccess = ( system( szCommandLine ) != -1 );

      /* Delete intermediate .c file */
      /* QUESTION: Leave this file if C compiler fails ? */
      if( bDelTmp ) /* && iSuccess ) */
         unlink( ( char * ) szFileName );

      if( ! HB_COMP_PARAM->fQuiet )
      {
         if( iSuccess )
            printf( "Done.\n" );
         else
            printf( "\nFailed to execute: \"%s\"\n", szCommandLine );
      }
   }
   else
   {
      printf( "\nError: No compiler defined in %s\n", HB_CFG_FILENAME );
   }

   if( pszCfg )
      hb_xfree( pszCfg );
}

static char * hb_searchpath( const char * pszFile, char * pszEnv, char * pszCfg )
{
   char * pszPath;
   char pszDelim[2] = { OS_PATH_LIST_SEPARATOR, '\0'};
   BOOL bFound = FALSE;

   /* Check current dir first  */
   if( access( ( const char * ) pszFile, HB_ACCESS_FLAG ) == 0 )
   {
      snprintf( pszCfg, _POSIX_PATH_MAX + 1, "%s", pszFile );
      return ( char * ) pszFile;
   }
   else
   {
      /* Check if pszFile exists somewhere in the path */
      pszPath = strtok( pszEnv, pszDelim );
      if( pszPath )
      {
         while( pszPath )
         {
            snprintf( pszCfg, _POSIX_PATH_MAX + 1, "%s%c%s", pszPath, OS_PATH_DELIMITER, pszFile );
            if( access( ( const char * ) pszCfg, HB_ACCESS_FLAG ) == 0 )
            {
               bFound = TRUE;
               break;
            }
            else
               pszPath = strtok( NULL, pszDelim );
         }
      }
   }

   /* If not found, make sure to return a NULL string */
   if( ! bFound )
      snprintf( pszCfg, _POSIX_PATH_MAX + 1, "%s", "" );

   return ( char * ) pszCfg;
}
