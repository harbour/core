/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour Version Updater standalone main module
 *
 * Copyright 2003 David G. Holm <dholm@jsd-llc.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * Avoid tracing in preprocessor/compiler.
 */
#if ! defined(HB_TRACE_UTILS)
   #if defined(HB_TRACE_LEVEL)
      #undef HB_TRACE_LEVEL
   #endif
#endif

#define HB_OS_WIN_32_USED

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#include "hbcomp.h"

#if !defined(__MINGW32CE__) && !( defined( _MSC_VER ) && defined( HB_WINCE ) )
#  include <errno.h>
#endif

#define MAX_BUF_LEN 4096

#if defined(HB_WINCE)

#define hb_xgrab(x)     malloc(x)
#define hb_xfree(x)     free(x)

wchar_t *hb_mbtowc( const char * srcA )
{
   DWORD length;
   wchar_t * dstW;

   length = MultiByteToWideChar( CP_ACP, 0, srcA, -1, NULL, 0 );
   dstW = ( wchar_t * ) malloc( ( length + 1 ) * sizeof( wchar_t ) );
   MultiByteToWideChar( CP_ACP, 0, srcA, -1, dstW, length + 1 );

   return dstW;
}

char *hb_wctomb( const wchar_t *srcW )
{
   DWORD length;
   char *dstA;

   length = WideCharToMultiByte( CP_ACP, 0, srcW, -1, NULL, 0, NULL, NULL );
   dstA = ( char * ) hb_xgrab( length + 1 );
   WideCharToMultiByte( CP_ACP, 0, srcW, -1, dstA, length + 1, NULL, NULL );

   return dstA;
}

void hb_wctombget( char *dstA, const wchar_t *srcW, unsigned long ulLen )
{
   WideCharToMultiByte( CP_ACP, 0, srcW, ulLen, dstA, ulLen, NULL, NULL );
}

int remove( const char * path )
{
   wchar_t * wpath;
   int result;

   wpath = hb_mbtowc( path );
   result = DeleteFileW( wpath ) ? 0 : -1;
   free( wpath );

   return result;
}

void perror( const char *szError )
{
   fprintf( stderr, "error: %s\n", szError );
}

int rename( const char * fn1, const char * fn2 )
{
   wchar_t * wfn1, * wfn2;
   int result;

   wfn1 = hb_mbtowc( fn1 );
   wfn2 = hb_mbtowc( fn2 );
   result = MoveFileW( wfn1, wfn2 ) ? 0 : -1;
   free( wfn1 );
   free( wfn2 );

   return result;
}

#endif

static char * szIncrementNumber( char * szBuffer, size_t stSkipOver )
{
   size_t i;
   for( i = stSkipOver; i < strlen( szBuffer ) && isspace( ( BYTE ) szBuffer[ i ] ); i++ ) {}
   if( i > stSkipOver && i < strlen( szBuffer ) )
   {
      int iVersion;
      char szIntBuf[ 8 ];
      size_t stStart = i;
      iVersion = atoi( &szBuffer[ stStart ] ) + 1;
      snprintf( szIntBuf, sizeof( szIntBuf ), "%-7d", iVersion );
      for( i = 0; i < strlen( szIntBuf ); i++ )
      {
         szBuffer[ stStart + i ] = szIntBuf[ i ];
      }
   }
   return szBuffer;
}

static char * szReplaceQuoted( char * szBuffer, const char * new_string )
{
   char szOldBuf[ MAX_BUF_LEN ];
   char * szOpening = strchr( szBuffer, 34 ); /* Locate starting quote */
   if( szOpening )
   {
      char * szClosing = strchr( szOpening + 1, 34 ); /* Locate ending quote */
      if( szClosing )
      {
         strncpy( szOldBuf, szClosing, MAX_BUF_LEN );
         szOldBuf[ MAX_BUF_LEN - 1 ] = '\0';
         strncpy( szOpening + 1, new_string, MAX_BUF_LEN - ( szOpening - szBuffer ) );
         szBuffer[ MAX_BUF_LEN - 1 ] = '\0';
         strncat( szBuffer, szOldBuf, MAX_BUF_LEN - strlen( szBuffer ) );
         szBuffer[ MAX_BUF_LEN - 1 ] = '\0';
         return szBuffer;
      }
   }
   return NULL;
}

int main( int argc, char * argv[] )
{
   BOOL bBadCmdLine = FALSE;
   BOOL bShowHelp = FALSE;
   char cIncrement = ' ';
   char szErrBuf[ MAX_BUF_LEN ];
   int iDebugLevel = 0;
   int iArg = 1;

   while( iArg < argc )
   {
      if( HB_ISOPTSEP(argv[ iArg ][ 0 ]))
      {
         BOOL bInvalid = FALSE;
         switch( argv[ iArg ][ 1 ] )
         {
         case 'd':
         case 'D':
            iDebugLevel = atoi( &argv[ iArg ][ 2 ] );
            break;
         case 'h':
         case 'H':
         case '?':
            bShowHelp = TRUE;
            break;
         case 'i':
         case 'I':
            if( strlen( argv[ iArg ] ) > 2 )
            {
               switch( argv[ iArg ][ 2 ] )
               {
               case 'v':
               case 'V':
               case 'm':
               case 'M':
               case 'r':
               case 'R':
                 cIncrement = tolower( argv[ iArg ][ 2 ] );
                 break;
               default:
                 bInvalid = TRUE;
               }
               break;
            }
            bInvalid = TRUE;
            break;
         default:
            bInvalid = TRUE;
            break;
         }
         if( bInvalid )
         {
            bBadCmdLine = TRUE;
            fprintf( stderr, "\nInvalid command line option: %s\n", &argv[ iArg ][ 1 ] );
         }
      }
      else
      {
         bBadCmdLine = TRUE;
         fprintf( stderr, "\nInvalid command line argument: %s\n", &argv[ iArg ][ 0 ] );
      }
      iArg++;
   }

   if( bShowHelp )
   {
      fprintf( stderr, "\nSyntax:  %s [options]"
              "\n"
              "\nOptions:  -d[level]   set debug level (-1 == none, 0 == minimal*, 1 == verbose)"
              "\n          -iv         increment major version number+"
              "\n          -im         increment minor version number+"
              "\n          -ir         increment revision number+"
              "\n"
              "\nAn * indicates a default option. A + indicates mutually exclusive options"
              "\n(the last one that is present on the command line will be being used)."
              "\n"
              , argv[ 0 ] );

      return 1;
   }
   else if( bBadCmdLine )
   {
      return 2;
   }
   else
   {
      const char * cszChangeLogName = "ChangeLog";
      const char * cszVersionName = "include/hbver.h";
      const char * cszRewriteName = "include/hbver.rw";
      char szInputBuffer[ MAX_BUF_LEN + 16 ];
      char szNewID[ MAX_BUF_LEN + 8 ];
      char szNewLog[ MAX_BUF_LEN + 8 ];
      BOOL bFoundID = FALSE;
      BOOL bFoundLog = FALSE;
      FILE * fhChangeLog;

      snprintf( szErrBuf, sizeof( szErrBuf ), "Opening %s", cszChangeLogName );
      fhChangeLog = fopen( cszChangeLogName, "rt" );
      if( fhChangeLog == NULL )
      {
#if defined(__MINGW32CE__) || ( defined( _MSC_VER ) && defined( HB_WINCE ) )
         perror( szErrBuf );
         return 4;
#else
         switch( errno )
         {
         case ENOENT:
            fprintf( stderr, "\nThis program needs to be run from the directory with the %s file.\n", cszChangeLogName );
            return 3;
         default:
            perror( szErrBuf );
            return 4;
         }
#endif
      }
      while( ! ( bFoundID && bFoundLog ) && ! feof( fhChangeLog ) )
      {
         snprintf( szErrBuf, sizeof( szErrBuf ), "Reading from %s", cszChangeLogName );
         fgets( szInputBuffer, MAX_BUF_LEN, fhChangeLog );
         if( ferror( fhChangeLog ) )
         {
            perror( szErrBuf );
            return 5;
         }
         if( iDebugLevel > 0 ) fprintf( stderr, "\n==> %u <==> %s <==", ( unsigned ) strlen( szInputBuffer ), szInputBuffer );
         if( ! bFoundID )
         {
            char * szID = strstr( szInputBuffer, "$Id: " );
            if( szID )
            {
               size_t stLen;
               szID = &szID[ 5 ]; /* Leave out the id markers... */
               stLen = strcspn( szID, "$" ) - 5; /* ...on both ends */
               szID[ stLen ] = '\0';
               bFoundID = TRUE;
               strncpy( szNewID, szID, sizeof( szNewID ) );
               szNewID[ sizeof( szNewID ) - 1 ] = '\0';
               if( iDebugLevel >= 0 ) fprintf( stderr, "\nID: %s\n", szID );
            }
         }
         else if( szInputBuffer[ 4 ] == '-' && szInputBuffer[ 7 ] == '-' && szInputBuffer[ 10 ] == ' ' && szInputBuffer[ 13 ] == ':' )
         {
            bFoundLog = TRUE;
            strncpy( szNewLog, szInputBuffer, sizeof( szNewLog ) );
            szNewLog[ sizeof( szNewLog ) - 1 ] = '\0';
            szNewLog[ strcspn( szNewLog, "\r\n" ) ] = '\0'; /* Strip newline chars */
            if( iDebugLevel >= 0 ) fprintf( stderr, "\nLOG: %s\n", szInputBuffer );
         }
         else if( iDebugLevel > 0 ) fprintf( stderr, " ===> %c %c %c %c <===", szInputBuffer[ 4 ], szInputBuffer[ 7 ], szInputBuffer[ 10 ], szInputBuffer[ 13 ] );
      }
      fclose( fhChangeLog );
      if( !bFoundID )
      {
         fprintf( stderr, "\nUnable to locate %s $Id record.\n", cszChangeLogName );
         return 6;
      }
      else if( ! bFoundLog )
      {
         fprintf( stderr, "\nUnable to locate a %s entry.\n", cszChangeLogName );
         return 7;
      }
      else
      {
         const char *cszMajorVersion = "#define HB_VER_MAJOR";
         const char *cszMinorVersion = "#define HB_VER_MINOR";
         const char *cszRevision = "#define HB_VER_REVISION";
         const char *cszLastEntry = "#define HB_VER_LENTRY";
         const char *cszLastID = "#define HB_VER_CHLCVS";

         FILE * fhRewrite;
         FILE * fhVersion;

         snprintf( szErrBuf, sizeof( szErrBuf ), "Opening %s", cszVersionName );
         fhVersion = fopen( cszVersionName, "rt" );
         if( fhVersion == NULL )
         {
            perror( szErrBuf );
            return 9;
         }
         snprintf( szErrBuf, sizeof( szErrBuf ), "Creating %s", cszRewriteName );
         fhRewrite = fopen( cszRewriteName, "wt" );
         if( fhRewrite == NULL )
         {
            perror( szErrBuf );
            return 8;
         }
         while( ! feof( fhVersion ) )
         {
            snprintf( szErrBuf, sizeof( szErrBuf ), "Reading from %s", cszVersionName );
            fgets( szInputBuffer, MAX_BUF_LEN, fhVersion );
            if( iDebugLevel > 0 ) fprintf( stderr, "\n==> %u <==> %s <==", ( unsigned ) strlen( szInputBuffer ), szInputBuffer );
            if( ferror( fhVersion ) )
            {
               perror( szErrBuf );
               return 9;
            }
            if( iDebugLevel > 0 ) fprintf( stderr, "\n==> %u <==> %s <==", ( unsigned ) strlen( szInputBuffer ), szInputBuffer );
            if( cIncrement == 'v' && strncmp( szInputBuffer, cszMajorVersion, strlen( cszMajorVersion ) ) == 0 )
            {
               szIncrementNumber( szInputBuffer, strlen( cszMajorVersion ) );
               if( iDebugLevel >= 0 ) fprintf( stderr, "\nMajor version: %s", szInputBuffer );
            }
            if( cIncrement == 'm' && strncmp( szInputBuffer, cszMinorVersion, strlen( cszMinorVersion ) ) == 0 )
            {
               szIncrementNumber( szInputBuffer, strlen( cszMinorVersion ) );
               if( iDebugLevel >= 0 ) fprintf( stderr, "\nMinor version: %s", szInputBuffer );
            }
            if( cIncrement == 'r' && strncmp( szInputBuffer, cszRevision, strlen( cszRevision ) ) == 0 )
            {
               szIncrementNumber( szInputBuffer, strlen( cszRevision ) );
               if( iDebugLevel >= 0 ) fprintf( stderr, "\nRevision level: %s", szInputBuffer );
            }
            if( strncmp( szInputBuffer, cszLastEntry, strlen( cszLastEntry ) ) == 0 )
            {
               szReplaceQuoted( szInputBuffer, szNewLog );
               if( iDebugLevel >= 0 ) fprintf( stderr, "\nNew log: %s", szInputBuffer );
            }
            if( strncmp( szInputBuffer, cszLastID, strlen( cszLastID ) ) == 0 )
            {
               szReplaceQuoted( szInputBuffer, szNewID );
               if( iDebugLevel >= 0 ) fprintf( stderr, "\nNew ID: %s", szInputBuffer );
            }
            if( !feof( fhVersion ) )
            {
               snprintf( szErrBuf, sizeof( szErrBuf ), "Writing to %s", cszRewriteName );
               fputs( szInputBuffer, fhRewrite );
               if( ferror( fhRewrite ) )
               {
                  perror( szErrBuf );
                  return 10;
               }
            }
         }
         fclose( fhVersion );
         fclose( fhRewrite );
         snprintf( szErrBuf, sizeof( szErrBuf ), "Deleting %s", cszVersionName );
         if( remove( cszVersionName ) )
         {
            perror( szErrBuf );
            return 11;
         }
         snprintf( szErrBuf, sizeof( szErrBuf ), "Renaming %s to %s", cszRewriteName, cszVersionName );
         if( rename( cszRewriteName, cszVersionName ) )
         {
            perror( szErrBuf );
            return 11;
         }
      }
   }
   return 0;
}

#if defined( HB_WINCE ) && !defined( __CEGCC__ )
#  include "hbwmain.c"
#endif
