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
#define HB_CFG_LINE_LEN    100

/*--------------------------------------------------------------------------*/

/* Builds platform dependant object module from Harbour C output */
void hb_compGenCObj( PHB_FNAME pFileName )
{
   char szFileName[ _POSIX_PATH_MAX ];
   char szLine[ HB_CFG_LINE_LEN ];
   char szCompiler[ HB_CFG_LINE_LEN ] = "";
   char szOptions[ HB_CFG_LINE_LEN ];
   char szCommandLine[ HB_CFG_LINE_LEN * 2 ];
   char szOutPath[ _POSIX_PATH_MAX ] = "\0";
#if defined( OS_UNIX_COMPATIBLE )
   char szDefaultUnixPath[ _POSIX_PATH_MAX ] = "/etc:/usr/local/etc";
#endif
   FILE * yyc;
   char * pszCfg;
   char * pszEnv;
   BOOL bVerbose = FALSE;   /* Don't show C compiler messages (default). */
   BOOL bDelTmp = TRUE;     /* Delete intermediate C file (default). */
   int iSuccess;

   /* First pass: build the C output */

   /* Force file extension to avoid collisions when called from a make utility */
   pFileName->szExtension = ".c";
   hb_fsFNameMerge( szFileName, pFileName );
   hb_compGenCCode( hb_comp_pFileName );

   /* Begin second pass */

   /* Set up things  */
#if defined(__MSDOS__) || defined(__WIN32__) || defined(_Windows)
   pszEnv = getenv( "PATH" );
#elif defined( OS_UNIX_COMPATIBLE )
   pszEnv = szDefaultUnixPath;
#endif

   /* Grab space */
   pszCfg = ( char * ) hb_xgrab( strlen( pszEnv ) );
   hb_searchpath( HB_CFG_FILENAME, pszEnv, pszCfg );

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

      if( szStr )
      {
         szToken = strtok( szStr, "=" );

         if( szToken )
         {
            /* Checks compiler name */
            if( ! hb_stricmp( szToken, "CC" ) )
            {
               szToken = strtok( NULL, "=" );
               if( szToken ) /* If empty, preserve last value */
                  sprintf( szCompiler, "%s", szToken );
            }

            /* Checks optional switches */
            if( szToken && ! hb_stricmp( szToken, "CFLAGS" ) )
            {
               szToken = strtok( NULL, "=" );
               if( szToken )
                  sprintf( szOptions, "%s", szToken );
            }

            /* Wanna see C compiler output ? */
            if( szToken && ! hb_stricmp( szToken, "VERBOSE" ) )
            {
               szToken = strtok( NULL, "=" );
               if( szToken && ! hb_stricmp( szToken, "YES" ) )
                  bVerbose = TRUE;
            }

            /* Delete intermediate C file ? */
            if( szToken && ! hb_stricmp( szToken, "DELTMP" ) )
            {
               szToken = strtok( NULL, "=" );
               if( szToken && ! hb_stricmp( szToken, "NO" ) )
                  bDelTmp = FALSE;
            }

         }
      }
   }

   fclose( yyc );

   if( ! hb_comp_bQuiet )
   {
      printf( "Building object module output for \'%s\'...", szFileName );
      fflush( stdout );
   }

   /* Check if -o<path> was used */
   if( hb_comp_pOutPath )
   {
      PHB_FNAME pOut = hb_fsFNameSplit( ( char * ) szFileName );
      char * pszTemp = "";

      if( hb_comp_pOutPath->szPath )
         pOut->szPath = hb_comp_pOutPath->szPath;

#if defined(__MSDOS__) || defined(__WIN32__) || defined(_Windows)
      pOut->szExtension = ".obj";
#elif defined( OS_UNIX_COMPATIBLE )
      pOut->szExtension = ".o";  /* Don't know if we can hardcode it for Un*x */
#endif
      hb_fsFNameMerge( pszTemp, pOut );

#if defined(_MSC_VER)
      strcat( szOutPath, "-Fo" );
#else
      strcat( szOutPath, "-o" );
#endif

      strcat( szOutPath, pszTemp );

      hb_xfree( pOut );
   }

   if( *szCompiler )
   {
      if( bVerbose )
      {
         printf( "\n" ) ;
         sprintf( szCommandLine, "%s %s %s %s", szCompiler, szOptions, szOutPath, szFileName );
         printf( "\n" ) ;
      }
      else
      {
#if defined(__MSDOS__) || defined(__WIN32__) || defined(_Windows)
         sprintf( szCommandLine, "%s %s %s %s > nul", szCompiler, szOptions, szOutPath, szFileName );
#elif defined( OS_UNIX_COMPATIBLE )
         sprintf( szCommandLine, "%s %s %s %s > /dev/null", szCompiler, szOptions, szOutPath, szFileName );
#endif
      }

      /* Compile it! */
      iSuccess = ( system( szCommandLine ) != -1 );

      /* Delete intermediate .c file */
      /* QUESTION: Leave this file if C compiler fails ? */
      if( bDelTmp ) /* && iSuccess ) */
         unlink( ( char * ) szFileName );
   }
   else
   {
      printf( "\nError: No compiler defined in %s\n", HB_CFG_FILENAME );
   }

   if( ! hb_comp_bQuiet )
   {
      if( iSuccess )
        printf( "Done.\n" );
      else
        printf( "\nFailed to execute: %s\n", szCommandLine );
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
#if defined( OS_UNIX_COMPATIBLE )
   if( access( ( const char * ) pszFile, F_OK ) == 0 )
#else
   if( access( ( const char * ) pszFile, 0 ) == 0 )
#endif
   {
      sprintf( pszCfg, "%s", pszFile );
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
            sprintf( pszCfg, "%s%c%s", pszPath, OS_PATH_DELIMITER, pszFile );
#if defined( OS_UNIX_COMPATIBLE )
            if( access( ( const char * ) pszCfg, F_OK ) == 0 )
#else
            if( access( ( const char * ) pszCfg, 0 ) == 0 )
#endif
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
      sprintf( pszCfg, "%s", "" );

   return ( char * ) pszCfg;
}
