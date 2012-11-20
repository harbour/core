/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Command line and environment argument management
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
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

/* NOTE: Need to have these before Harbour headers,
         because in MT mode, they will automatically #include <os2.h>. */
#define INCL_DOSPROCESS
#define INCL_DOSERRORS
#define INCL_DOSMODULEMGR

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbapicdp.h"
#include "hbvm.h"
#include "hbmemory.ch"
#include "hbstack.h"
#include "hbverbld.h"

#if defined( HB_OS_OS2 )
   #include <os2.h>
#endif

/* Command line argument management */
static int     s_argc = 0;
static char ** s_argv = NULL;

#if ! defined( HB_OS_WIN )

static char    s_szAppName[ HB_PATH_MAX ];

#else

#include "hbwinuni.h"
#include <windows.h>

static LPTSTR * s_lpArgV = NULL;
#ifdef UNICODE
static LPSTR * s_lpArgVStr = NULL;
#endif

static HANDLE  s_hInstance     = 0;
static HANDLE  s_hPrevInstance = 0;
static int     s_iCmdShow      = 0;
static HB_BOOL s_WinMainParam  = HB_FALSE;

void hb_winmainArgVBuild( void )
{
   LPCTSTR lpCmdLine, lpSrc;
   LPTSTR * lpArgV;
   LPTSTR lpDst, lpArg;
   HB_SIZE nSize, nModuleName;
   int iArgC;
   HB_BOOL fQuoted;
   HANDLE hHeap;

   hHeap = GetProcessHeap();
   lpCmdLine = GetCommandLine();
   nModuleName = GetModuleFileName( NULL, NULL, 0 );
   lpArgV = NULL;
   lpDst = NULL;
   nSize = 0;
   iArgC = -1;

   while( lpCmdLine && ! lpArgV && iArgC != 0 )
   {
      if( nSize != 0 )
      {
         lpArgV = ( LPTSTR * ) HeapAlloc( hHeap, 0,
                                          iArgC * sizeof( LPTSTR ) +
                                          nSize * sizeof( TCHAR ) );
         lpDst = ( LPTSTR ) ( lpArgV + iArgC );
         lpArgV[ 0 ] = lpDst;
         lpDst += nModuleName;
      }
      else
      {
         lpDst = ( LPTSTR ) lpCmdLine;
         nSize = nModuleName;
      }

      lpSrc = lpCmdLine;
      lpArg = NULL;
      iArgC = 0;
      fQuoted = HB_FALSE;

      while( *lpSrc != 0 )
      {
         if( *lpSrc == TEXT( '"' ) )
         {
            if( lpArg == NULL )
               lpArg = lpDst;
            fQuoted = ! fQuoted;
         }
         else if( fQuoted || ! HB_ISSPACE( *lpSrc ) )
         {
            if( lpArg == NULL )
               lpArg = lpDst;
            if( iArgC > 0 || nModuleName == 0 )
            {
               if( lpArgV )
                  *lpDst++ = *lpSrc;
               else
                  nSize++;
            }
         }
         else
         {
            if( lpArg )
            {
               if( iArgC > 0 || nModuleName == 0 )
               {
                  if( lpArgV )
                  {
                     *lpDst++ = '\0';
                     lpArgV[ iArgC ] = lpArg;
                  }
                  else
                     nSize++;
               }
               iArgC++;
               lpArg = NULL;
            }
         }
         ++lpSrc;
      }
      if( lpArg )
      {
         if( iArgC > 0 || nModuleName == 0 )
         {
            if( lpArgV )
            {
               *lpDst = '\0';
               lpArgV[ iArgC ] = lpArg;
            }
            else
               nSize++;
         }
         iArgC++;
      }
   }

   if( iArgC <= 0 )
   {
      if( nModuleName != 0 )
      {
         iArgC = 1;
         lpArgV = ( LPTSTR * ) HeapAlloc( hHeap, 0,
                                          iArgC * sizeof( LPTSTR ) +
                                          nSize * sizeof( TCHAR ) );
         lpArgV[ 0 ] = ( LPTSTR ) ( lpArgV + iArgC );
      }
      else
         iArgC = 0;
   }
   if( iArgC > 0 && nModuleName )
   {
      /* NOTE: Manually setup the executable name in Windows,
               because in console apps the name may be truncated
               in some cases, and in GUI apps it's not filled
               at all. [vszakats] */
      if( GetModuleFileName( NULL, lpArgV[ 0 ], nModuleName ) != 0 )
      {
         /* Windows XP does not set trailing 0 if buffer is not large enough [druzus] */
         lpArgV[ 0 ][ nModuleName - 1 ] = 0;
      }
   }

   hb_winmainArgVFree();

   if( iArgC > 0 )
   {
      s_lpArgV = lpArgV;
      s_argc = iArgC;
#ifdef UNICODE
      {
         LPSTR lpStr;

         nSize = 0;
         for( iArgC = 0; iArgC < s_argc; ++iArgC )
            nSize += hb_wctomblen( s_lpArgV[ iArgC ] ) + 1;

         s_lpArgVStr = ( LPSTR * ) HeapAlloc( hHeap, 0,
                                              iArgC * sizeof( LPSTR ) +
                                              nSize * sizeof( char ) );
         lpStr = ( LPSTR ) ( s_lpArgVStr + iArgC );
         for( iArgC = 0; iArgC < s_argc; ++iArgC )
         {
            nSize = hb_wctomblen( s_lpArgV[ iArgC ] ) + 1;
            hb_wcntombcpy( lpStr, s_lpArgV[ iArgC ], nSize );
            s_lpArgVStr[ iArgC ] = lpStr;
            lpStr += nSize;
         }
         s_argv = s_lpArgVStr;
      }
#else
      s_argv = s_lpArgV;
#endif
   }
}

void hb_winmainArgVFree( void )
{
   if( s_lpArgV )
   {
#ifdef UNICODE
      if( s_lpArgVStr )
      {
         if( s_argv == s_lpArgVStr )
            s_argv = NULL;
         HeapFree( GetProcessHeap(), 0, ( void * ) s_lpArgVStr );
         s_lpArgVStr = NULL;
      }
#else
      if( s_argv == s_lpArgV )
         s_argv = NULL;
#endif

      HeapFree( GetProcessHeap(), 0, ( void * ) s_lpArgV );
      s_lpArgV = NULL;
      s_argc = 0;
   }
}

void hb_winmainArgInit( void * hInstance, void * hPrevInstance, int iCmdShow )
{
   s_hInstance = ( HANDLE ) hInstance;
   s_hPrevInstance = ( HANDLE ) hPrevInstance;
   s_iCmdShow = iCmdShow;
   s_WinMainParam = HB_TRUE;
}

HB_BOOL hb_winmainArgGet( void * phInstance, void * phPrevInstance, int * piCmdShow )
{
   if( phInstance )
      *( ( HANDLE * ) phInstance ) = s_hInstance;
   if( phPrevInstance )
      *( ( HANDLE * ) phPrevInstance ) = s_hPrevInstance;
   if( piCmdShow )
      *piCmdShow = s_iCmdShow;

   return s_WinMainParam;
}

#endif

void hb_cmdargInit( int argc, char * argv[] )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_cmdargInit(%d, %p)", argc, argv ) );

#if defined( HB_OS_WIN )
   if( s_lpArgV )
      return;
#endif

   if( argc == 0 || argv == NULL )
   {
      s_argc = 0;
      s_argv = NULL;
   }
   else
   {
      s_argc = argc;
      s_argv = argv;
   }
}

int hb_cmdargARGC( void )
{
   return s_argc;
}

char ** hb_cmdargARGV( void )
{
   return s_argv;
}

const char * hb_cmdargARGVN( int argc )
{
   return argc >= 0 && argc < s_argc ? s_argv[ argc ] : NULL;
}

/* NOTE: Pointer must be freed with hb_xfree() if not NULL */

static char * hb_cmdargDup( int argc )
{
#if defined( HB_OS_WIN )
   if( s_lpArgV )
      return argc >= 0 && argc < s_argc ? HB_OSSTRDUP( s_lpArgV[ argc ] ) : NULL;
#endif
   return argc >= 0 && argc < s_argc ? hb_osStrDecode( s_argv[ argc ] ) : NULL;
}

void hb_cmdargUpdate( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_cmdargUpdate()" ) );

#if ! defined( HB_OS_WIN )
   if( s_argc > 0 )
   {
#  if defined( HB_OS_OS2 )
      {
         PPIB ppib = NULL;
         APIRET ulrc;

         ulrc = DosGetInfoBlocks( NULL, &ppib );
         if( ulrc == NO_ERROR )
         {
            ulrc = DosQueryModuleName( ppib->pib_hmte,
                                       HB_SIZEOFARRAY( s_szAppName ),
                                       s_szAppName );
            if( ulrc == NO_ERROR )
               s_argv[ 0 ] = s_szAppName;
         }
      }
#  else
      /* NOTE: try to create absolute path from s_argv[ 0 ] if necessary */
      {
         PHB_FNAME pFName = hb_fsFNameSplit( s_argv[ 0 ] );
         HB_BOOL fInPath = HB_FALSE;

         if( ! pFName->szPath )
         {
            char * pszPATH = hb_getenv( "PATH" );

            if( pszPATH && *pszPATH )
            {
               HB_PATHNAMES * pSearchPath = NULL, * pNextPath;
               hb_fsAddSearchPath( pszPATH, &pSearchPath );
               pNextPath = pSearchPath;

               while( pNextPath )
               {
                  pFName->szPath = pNextPath->szPath;
                  hb_fsFNameMerge( s_szAppName, pFName );
                  if( hb_fsFileExists( s_szAppName ) )
                  {
                     /* even if the file is located using PATH then it does
                      * not mean we will have absolute path here. It's not
                      * good idea but PATH envvar can also contain relative
                      * directories, f.e. "." or "bin" so we should add
                      * current directory if necessary in code below.
                      */
                     hb_xfree( pFName );
                     pFName = hb_fsFNameSplit( s_szAppName );
                     fInPath = HB_TRUE;
                     break;
                  }
                  pNextPath = pNextPath->pNext;
               }
               hb_fsFreeSearchPath( pSearchPath );
               if( ! fInPath )
                  pFName->szPath = NULL;
            }
            if( pszPATH )
               hb_xfree( pszPATH );
         }
         if( pFName->szPath )
         {
#     if defined( HB_OS_HAS_DRIVE_LETTER )
            if( pFName->szPath[ 0 ] != HB_OS_PATH_DELIM_CHR && ! pFName->szDrive )
#     else
            if( pFName->szPath[ 0 ] != HB_OS_PATH_DELIM_CHR )
#     endif
            {
               if( pFName->szPath[ 0 ] == '.' &&
                   pFName->szPath[ 1 ] == HB_OS_PATH_DELIM_CHR )
                  pFName->szPath += 2;
               s_szAppName[ 0 ] = HB_OS_PATH_DELIM_CHR;
               hb_fsCurDirBuff( 0, s_szAppName + 1, HB_PATH_MAX - 1 );
               if( s_szAppName[ 1 ] != 0 )
               {
                  hb_strncat( s_szAppName, HB_OS_PATH_DELIM_CHR_STRING, HB_PATH_MAX - 1 );
                  hb_strncat( s_szAppName, pFName->szPath, HB_PATH_MAX - 1 );
                  pFName->szPath = hb_strdup( s_szAppName );
                  hb_fsFNameMerge( s_szAppName, pFName );
                  hb_xfree( ( void * ) pFName->szPath );
                  s_argv[ 0 ] = s_szAppName;
               }
            }
            else if( fInPath )
               s_argv[ 0 ] = s_szAppName;
         }
         hb_xfree( pFName );
      }
#  endif
   }
#endif
}

/* places application parameters on the HVM stack */

int hb_cmdargPushArgs( void )
{
   int iArgCount = 0, i;

   for( i = 1; i < s_argc; i++ )
   {
      /* Filter out any parameters beginning with //, like //INFO */
      if( ! hb_cmdargIsInternal( s_argv[ i ], NULL ) )
      {
#if defined( HB_OS_WIN )
         if( s_lpArgV )
            HB_ITEMPUTSTR( hb_stackAllocItem(), s_lpArgV[ i ] );
         else
#endif
            hb_vmPushString( s_argv[ i ], strlen( s_argv[ i ] ) );
         iArgCount++;
      }
   }

   return iArgCount;
}

HB_BOOL hb_cmdargIsInternal( const char * szArg, int * piLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_cmdargIsInternal(%s, %p)", szArg, piLen ) );

   /* NOTE: Not checking for '--' here, as it would filter out
            valid command line options used by applications. [vszakats] */

   if( hb_strnicmp( szArg, "--hb:", 5 ) == 0 ||
       hb_strnicmp( szArg, "//hb:", 5 ) == 0 )
   {
      if( piLen )
         *piLen = 5;

      return HB_TRUE;
   }
   else if( strlen( szArg ) >= 2 &&
            szArg[ 0 ] == '/' &&
            szArg[ 1 ] == '/' )
   {
      if( piLen )
         *piLen = 2;

      return HB_TRUE;
   }

   return HB_FALSE;
}

static char * hb_cmdargGet( const char * pszName, HB_BOOL bRetValue )
{
   char * pszRetVal = NULL;
   char * pszEnvVar;
   int i;
   int iPrefixLen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cmdargGet(%s, %d)", pszName, ( int ) bRetValue ) );

   /* Check the command line first */

   for( i = 1; i < s_argc; i++ )
   {
      if( hb_cmdargIsInternal( s_argv[ i ], &iPrefixLen ) &&
          hb_strnicmp( s_argv[ i ] + iPrefixLen, pszName, strlen( pszName ) ) == 0 )
      {
         if( bRetValue )
         {
#if defined( HB_OS_WIN )
            if( s_lpArgV )
            {
               LPCTSTR lpPos = s_lpArgV[ i ] + iPrefixLen + strlen( pszName );

               if( *lpPos == TEXT( ':' ) )
                  lpPos++;
               return HB_OSSTRDUP( lpPos );
            }
            else
#endif
            {
               char * pszPos = s_argv[ i ] + iPrefixLen + strlen( pszName );

               if( *pszPos == ':' )
                  pszPos++;

               return hb_osStrDecode( pszPos );
            }
         }
         else
            return ( char * ) "";
      }
   }

   /* Check the environment variable */
   pszEnvVar = hb_getenv( "HARBOUR" );
   if( ! pszEnvVar || pszEnvVar[ 0 ] == '\0' )
   {
      if( pszEnvVar )
         hb_xfree( pszEnvVar );

      pszEnvVar = hb_getenv( "CLIPPER" );
   }

   if( pszEnvVar && pszEnvVar[ 0 ] != '\0' )
   {
      char * pszNext = pszEnvVar;

      /* Step through all envvar switches. */

      /* NOTE: CA-Cl*pper doesn't need the switches to be separated by any
               chars at all, Harbour is more strict/standard in this respect,
               it requires the switches to be separated. */

      i = ( int ) strlen( pszName );
      while( *pszNext )
      {
         static const char * s_szSeparator = " ;,\t";
         char * pszEnd;

         /* Skip the separators */
         while( *pszNext && strchr( s_szSeparator, *pszNext ) )
            pszNext++;

         /* The // is optional in the envvar */
         if( hb_cmdargIsInternal( pszNext, &iPrefixLen ) )
            pszNext += iPrefixLen;

         pszEnd = pszNext;
         /* Search for the end of this switch */
         while( *pszEnd && strchr( s_szSeparator, *pszEnd ) == NULL )
            pszEnd++;

         /* Check the switch */
         if( hb_strnicmp( pszNext, pszName, i ) == 0 )
         {
            if( bRetValue )
            {
               HB_SIZE nLen;
               pszNext += i;

               /* Skip value separator colon. */
               if( *pszNext == ':' )
                  pszNext++;

               nLen = pszEnd > pszNext ? pszEnd - pszNext : 0;
               pszRetVal = ( char * ) hb_xgrab( nLen + 1 );
               hb_strncpy( pszRetVal, pszNext, nLen );
            }
            else
               pszRetVal = ( char * ) "";
            break;
         }

         /* Step to the next switch */
         pszNext = pszEnd;
      }
   }

   if( pszEnvVar )
      hb_xfree( pszEnvVar );

   return pszRetVal;
}

HB_BOOL hb_cmdargCheck( const char * pszName )
{
   return hb_cmdargGet( pszName, HB_FALSE ) != NULL;
}

/* NOTE: Pointer must be freed with hb_xfree() if not NULL */

char * hb_cmdargString( const char * pszName )
{
   return hb_cmdargGet( pszName, HB_TRUE );
}

int hb_cmdargNum( const char * pszName )
{
   char * pszValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_cmdargNum(%s)", pszName ) );

   pszValue = hb_cmdargGet( pszName, HB_TRUE );
   if( pszValue )
   {
      int iValue = atoi( pszValue );

      hb_xfree( pszValue );

      return iValue;
   }
   else
      return -1;
}

/* NOTE: Pointer must be freed with hb_xfree() if not NULL */

char * hb_cmdargProgName( void )
{
   return hb_cmdargDup( 0 );
}

/* NOTE: Pointer must be freed with hb_xfree() if not NULL */

char * hb_cmdargBaseProgName( void )
{
   char * pszProgName, * pszBaseProgName;
   PHB_FNAME pFileName;

   pszProgName = hb_cmdargProgName();
   pFileName = hb_fsFNameSplit( pszProgName );
   pszBaseProgName = hb_strdup( pFileName->szName );
   hb_xfree( pFileName );
   hb_xfree( pszProgName );

   return pszBaseProgName;
}

/* Check if an internal switch has been set */

HB_FUNC( HB_ARGCHECK )
{
   hb_retl( HB_ISCHAR( 1 ) ? hb_cmdargCheck( hb_parc( 1 ) ) : HB_FALSE );
}

/* Returns the value of an internal switch */

HB_FUNC( HB_ARGSTRING )
{
   const char * pszName = hb_parc( 1 );

   if( pszName )
   {
      char * pszValue = hb_cmdargString( pszName );

      if( pszValue )
      {
         hb_retc_buffer( pszValue );
         return;
      }
   }

   hb_retc_null();
}

/* Returns the number of command line arguments passed to the application, this
   also includes the internal arguments. */

HB_FUNC( HB_ARGC )
{
   hb_retni( s_argc - 1 );
}

/* Returns a command line argument passed to the application. Calling it with
   the parameter zero or no parameter, it will return the name of the executable,
   as written in the command line. */

HB_FUNC( HB_ARGV )
{
   char * pszArg = hb_cmdargDup( hb_parni( 1 ) );

   if( pszArg )
      hb_retc_buffer( pszArg );
   else
      hb_retc_null();
}

HB_FUNC( HB_ARGSHIFT )
{
   int iArg = 1;

   if( hb_parl( 1 ) )
   {
      while( iArg < s_argc )
      {
         if( ! hb_cmdargIsInternal( s_argv[ iArg ], NULL ) )
         {
            s_argv[ 0 ] = s_argv[ iArg ];
#if defined( HB_OS_WIN )
            if( s_lpArgV )
               s_lpArgV[ 0 ] = s_lpArgV[ iArg ];
#endif
            break;
         }
         ++iArg;
      }
   }
   if( iArg < s_argc )
   {
      --s_argc;
      while( iArg < s_argc )
      {
         s_argv[ iArg ] = s_argv[ iArg + 1 ];
#if defined( HB_OS_WIN )
         if( s_lpArgV )
            s_lpArgV[ iArg ] = s_lpArgV[ iArg + 1 ];
#endif
         ++iArg;
      }
   }
}

HB_FUNC( HB_CMDLINE )
{
   if( s_argc > 1 )
   {
      HB_SIZE nLen = 0;
      int iArg;

#if defined( HB_OS_WIN )
      if( s_lpArgV )
      {
         LPTSTR lpBuffer, ptr;

         for( iArg = 1; iArg < s_argc; iArg++ )
            nLen += HB_STRLEN( s_lpArgV[ iArg ] ) + 1;

         ptr = lpBuffer = ( LPTSTR ) hb_xgrab( nLen * sizeof( TCHAR ) );
         for( iArg = 1; iArg < s_argc; iArg++ )
         {
            nLen = HB_STRLEN( s_lpArgV[ iArg ] );
            memcpy( ptr, s_lpArgV[ iArg ], nLen * sizeof( TCHAR ) );
            ptr += nLen;
            *ptr++ = TEXT( ' ' );
         }
         *--ptr = TEXT( '\0' );

         /* Convert from OS codepage */
#ifdef UNICODE
         HB_RETSTR( lpBuffer );
         hb_xfree( lpBuffer );
#else
         hb_retc_buffer( ( char * ) hb_osDecodeCP( lpBuffer, NULL, NULL ) );
#endif
      }
      else
#endif
      {
         char * pszBuffer, * ptr;

         for( iArg = 1; iArg < s_argc; iArg++ )
            nLen += strlen( s_argv[ iArg ] ) + 1;

         ptr = pszBuffer = ( char * ) hb_xgrab( nLen );
         for( iArg = 1; iArg < s_argc; iArg++ )
         {
            nLen = strlen( s_argv[ iArg ] );
            memcpy( ptr, s_argv[ iArg ], nLen );
            ptr += nLen;
            *ptr++ = ' ';
         }
         *--ptr = '\0';

         /* Convert from OS codepage */
         hb_retc_buffer( ( char * ) hb_osDecodeCP( pszBuffer, NULL, NULL ) );
      }
   }
   else
      hb_retc_null();
}

/* Check for command line internal arguments */
HB_U32 hb_cmdargProcessVM( int * pCancelKey, int * pCancelKeyEx )
{
   char * cFlags;
   HB_U32 ulFlags = HB_VMFLAG_HARBOUR;
   int iHandles, iVal;

   if( hb_cmdargCheck( "INFO" ) )
   {
      {
         char * pszVersion = hb_verHarbour();
         hb_conOutErr( pszVersion, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
         hb_xfree( pszVersion );
      }

      {
         char * pszVersion = hb_verPlatform();
         hb_conOutErr( pszVersion, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
         hb_xfree( pszVersion );
      }

      {
         char buffer[ 128 ];
#if defined( HB_CLP_STRICT )
         hb_snprintf( buffer, sizeof( buffer ), "DS avail=%" HB_PFS "uKB  OS avail=%" HB_PFS "uKB  EMM avail=%" HB_PFS "uKB", hb_xquery( HB_MEM_BLOCK ), hb_xquery( HB_MEM_VM ), hb_xquery( HB_MEM_EMS ) );
#else
         hb_snprintf( buffer, sizeof( buffer ), "DS avail=%" HB_PFS "uKB  OS avail=%" HB_PFS "uKB  EMM avail=%" HB_PFS "uKB  MemStat:%s  MT:%s", hb_xquery( HB_MEM_BLOCK ), hb_xquery( HB_MEM_VM ), hb_xquery( HB_MEM_EMS ), hb_xquery( HB_MEM_USEDMAX ) ? "On" : "Off", hb_vmIsMt() ? "On" : "Off" );
#endif
         hb_conOutErr( buffer, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
      }
   }

   if( hb_cmdargCheck( "BUILD" ) )
      hb_verBuildInfo();

   iHandles = hb_cmdargNum( "F" );
   if( iHandles > 20 )
   {
      #if defined( __WATCOMC__ )
         #if defined( HB_OS_OS2 )
            DosSetMaxFH( iHandles );
         #elif defined( HB_OS_DOS )
            _grow_handles( iHandles );
         #endif
      #endif
   }

   if( ( cFlags = hb_cmdargString( "FLAGS" ) ) != NULL )
   {
      int i = 0;
      while( cFlags[ i ] )
      {
         switch( cFlags[ i++ ] )
         {
            case 'c':
               /* clear all flags - minimal set of features */
               ulFlags = 0;
               break;

            case 'h':
               /* default Harbour mode */
               ulFlags |= HB_VMFLAG_HARBOUR;
               break;
#if 0
            case 'x':
               ulFlags |= HB_VMFLAG_XBASE;
               break;

            case 'r':
               ulFlags |= HB_VMFLAG_RT_MACRO;
               break;
#endif
            case 's':
               ulFlags |= HB_VMFLAG_ARRSTR;
               break;
         }
      }
      hb_xfree( cFlags );
   }

   iVal = hb_cmdargNum( "CANCEL" );
   if( iVal )
      *pCancelKey = iVal;

   iVal = hb_cmdargNum( "CANCELEX" );
   if( iVal )
      *pCancelKeyEx = iVal;

   return ulFlags;
}

/* Source repository revision number */
int hb_verRevision( void )
{
   return HB_VER_REVID;
}

/* ChangeLog ID string */
const char * hb_verChangeLogID( void )
{
   return HB_VER_CHLID;
}

/* ChangeLog last entry string */
const char * hb_verChangeLogLastEntry( void )
{
   return HB_VER_LENTRY;
}

#if defined( HB_LEGACY_LEVEL4 )

/* Source repository revision number */
int hb_verSvnID( void )
{
   return HB_VER_REVID;
}

/* ChangeLog ID string */
const char * hb_verSvnChangeLogID( void )
{
   return HB_VER_CHLID;
}

/* ChangeLog last entry string */
const char * hb_verSvnLastEntry( void )
{
   return HB_VER_LENTRY;
}

#endif

/* build time C compiler flags in HB_USER_CFLAGS envvar */
const char * hb_verFlagsC( void )
{
#ifdef HB_VER_HB_USER_CFLAGS
   return HB_VER_HB_USER_CFLAGS;
#else
   return "";
#endif
}

/* build time linker flags in HB_USER_LDFLAGS envvar */
const char * hb_verFlagsL( void )
{
#ifdef HB_VER_HB_USER_LDFLAGS
   return HB_VER_HB_USER_LDFLAGS;
#else
   return "";
#endif
}

/* build time Harbour compiler flags in HB_USER_PRGFLAGS envvar */
const char * hb_verFlagsPRG( void )
{
#ifdef HB_VER_HB_USER_PRGFLAGS
   return HB_VER_HB_USER_PRGFLAGS;
#else
   return "";
#endif
}

/* build time Harbour platform setting */
const char * hb_verHB_PLAT( void )
{
#ifdef HB_PLATFORM
   return HB_PLATFORM;
#else
   return "";
#endif
}

/* build time Harbour compiler setting */
const char * hb_verHB_COMP( void )
{
#ifdef HB_COMPILER
   return HB_COMPILER;
#else
   return "";
#endif
}
