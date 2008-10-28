/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Command line and environment argument management
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

#define HB_OS_WIN_32_USED

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbmemory.ch"
#include "hbstack.h"
#include "hbverbld.h"

/* Command line argument management */
static int     s_argc = 0;
static char ** s_argv = NULL;

#if defined( HB_OS_WIN_32 )
static char    s_szAppName[ MAX_PATH ];
static TCHAR   s_lpAppName[ MAX_PATH ];
#endif

#if defined( HB_OS_WIN_32 ) && defined( HB_OS_WIN_32_USED )

HB_EXTERN_BEGIN

#ifdef HB_LEGACY_LEVEL
   /* NOTE: 1.0 compatibility stuff. Will be removed in 1.1 [vszakats]. */
   #define s_hInstance     hb_hInstance
   #define s_hPrevInstance hb_hPrevInstance

   HANDLE s_hInstance     = 0;
   HANDLE s_hPrevInstance = 0;
   int    s_iCmdShow      = 0;
   BOOL   s_WinMainParam  = FALSE;
#else
   static HANDLE s_hInstance     = 0;
   static HANDLE s_hPrevInstance = 0;
   static int    s_iCmdShow      = 0;
   static BOOL   s_WinMainParam  = FALSE;
#endif

HB_EXTERN_END

HB_EXPORT void hb_winmainArgInit( HANDLE hInstance, HANDLE hPrevInstance, int iCmdShow )
{
   s_hInstance = hInstance;
   s_hPrevInstance = hPrevInstance;
   s_iCmdShow = iCmdShow;
   s_WinMainParam = TRUE;
}

HB_EXPORT BOOL hb_winmainArgGet( HANDLE * phInstance, HANDLE * phPrevInstance, int * piCmdShow )
{
   if( phInstance )
      *phInstance = s_hInstance;
   if( phPrevInstance )
      *phPrevInstance = s_hPrevInstance;
   if( piCmdShow )
      *piCmdShow = s_iCmdShow;

   return s_WinMainParam;
}

#endif

HB_EXPORT void hb_cmdargInit( int argc, char * argv[] )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cmdargInit(%d, %p)", argc, argv));

   s_argc = argc;
   s_argv = argv;

#if defined( HB_OS_WIN_32 )

   /* NOTE: Manually setup the executable name in Windows, 
            because in console apps the name may be truncated 
            in some cases, and in GUI apps it's not filled 
            at all. [vszakats] */
   if( GetModuleFileName( NULL, s_lpAppName, MAX_PATH ) != 0 )
   {
      HB_TCHAR_GETFROM( s_szAppName, s_lpAppName, MAX_PATH );
      s_argv[ 0 ] = s_szAppName;
   }

#endif
}

int hb_cmdargARGC( void )
{
   return s_argc;
}

char ** hb_cmdargARGV( void )
{
   return s_argv;
}

BOOL hb_cmdargIsInternal( const char * szArg, int * piLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cmdargIsInternal(%s, %p)", szArg, piLen));

   /* NOTE: Not checking for '--' here, as it would filter out 
            valid command line options used by applications. [vszakats] */

   if( hb_strnicmp( szArg, "--hb:", 5 ) == 0 ||
       hb_strnicmp( szArg, "//hb:", 5 ) == 0 )
   {
      if( piLen )
         *piLen = 5;

      return TRUE;
   }
   else if( strlen( szArg ) >= 2 && 
            szArg[ 0 ] == '/' && 
            szArg[ 1 ] == '/' )
   {
      if( piLen )
         *piLen = 2;

      return TRUE;
   }

   return FALSE;
}

static char * hb_cmdargGet( const char * pszName, BOOL bRetValue )
{
   char * pszRetVal = NULL;
   char * pszEnvVar;
   int i;
   int iPrefixLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_cmdargGet(%s, %d)", pszName, (int) bRetValue));

   /* Check the command line first */

   for( i = 1; i < s_argc; i++ )
   {
      if( hb_cmdargIsInternal( s_argv[ i ], &iPrefixLen ) &&
         hb_strnicmp( s_argv[ i ] + iPrefixLen, pszName, strlen( pszName ) ) == 0 )
      {
         if( bRetValue )
         {
            char * pszPos = s_argv[ i ] + iPrefixLen + strlen( pszName );

            if( *pszPos == ':' )
               pszPos++;

            return hb_strdup( pszPos );
         }
         else
            return ( char * ) "";
      }
   }

   /* Check the environment variable */
   pszEnvVar = hb_getenv( "HARBOUR" );
   if( !pszEnvVar || pszEnvVar[ 0 ] == '\0' )
   {
      if( pszEnvVar )
         hb_xfree( ( void * ) pszEnvVar );

      pszEnvVar = hb_getenv( "CLIPPER" );
   }

   if( pszEnvVar && pszEnvVar[ 0 ] != '\0' )
   {
      char * pszNext = pszEnvVar;

      /* Step through all envvar switches. */

      /* NOTE: CA-Cl*pper doesn't need the switches to be separated by any
               chars at all, Harbour is more strict/standard in this respect,
               it requires the switches to be separated. */

      i = strlen( pszName );
      while( *pszNext )
      {
         static const char * szSeparator = " ;,\t";
         char * pszEnd;

         /* Skip the separators */
         while( *pszNext && strchr( szSeparator, *pszNext ) )
            pszNext++;

         /* The // is optional in the envvar */
         if( hb_cmdargIsInternal( pszNext, &iPrefixLen ) )
            pszNext += iPrefixLen;

         pszEnd = pszNext;
         /* Search for the end of this switch */
         while( *pszEnd && strchr( szSeparator, *pszEnd ) == NULL )
            pszEnd++;

         /* Check the switch */
         if( hb_strnicmp( pszNext, pszName, i ) == 0 )
         {
            if( bRetValue )
            {
               ULONG ulLen;
               pszNext += i;

               /* Skip value separator colon. */
               if( *pszNext == ':' )
                  pszNext++;

               ulLen = pszEnd > pszNext ? pszEnd - pszNext : 0;
               pszRetVal = ( char * ) hb_xgrab( ulLen + 1 );
               hb_strncpy( pszRetVal, pszNext, ulLen );
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
      hb_xfree( ( void * ) pszEnvVar );

   return pszRetVal;
}

BOOL hb_cmdargCheck( const char * pszName )
{
   return hb_cmdargGet( pszName, FALSE ) != NULL;
}

/* NOTE: Pointer must be freed with hb_xfree() if not NULL */

char * hb_cmdargString( const char * pszName )
{
   return hb_cmdargGet( pszName, TRUE );
}

int hb_cmdargNum( const char * pszName )
{
   char * pszValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_cmdargNum(%s)", pszName));

   pszValue = hb_cmdargGet( pszName, TRUE );
   if( pszValue )
   {
      int iValue = atoi( pszValue );

      hb_xfree( pszValue );

      return iValue;
   }
   else
      return -1;
}

/* Check if an internal switch has been set */

HB_FUNC( HB_ARGCHECK )
{
   hb_retl( ISCHAR( 1 ) ? hb_cmdargCheck( hb_parc( 1 ) ) : FALSE );
}

/* Returns the value of an internal switch */

HB_FUNC( HB_ARGSTRING )
{
   if( ISCHAR( 1 ) )
   {
      char * pszValue = hb_cmdargString( hb_parc( 1 ) );

      if( pszValue )
      {
         hb_retc( pszValue );
         hb_xfree( pszValue );
         return;
      }
   }

   hb_retc( NULL );
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
   int argc = hb_parni( 1 );

   hb_retc( ( argc >= 0 && argc < s_argc ) ? s_argv[ argc ] : NULL );
}

HB_FUNC( HB_CMDLINE )
{
   char * pszBuffer;

   int nLen;
   int nPos;

   int argc = hb_cmdargARGC();
   char** argv = hb_cmdargARGV();

   nLen = 1;
   for( nPos = 1; nPos < argc; nPos++ )
      nLen += ( int ) strlen( argv[ nPos ] ) + 1;

   pszBuffer = ( char * ) hb_xgrab( nLen + 1 );

   pszBuffer[ 0 ] = '\0';
   for( nPos = 1; nPos < argc; nPos++ )
   {
      hb_strncat( pszBuffer, argv[ nPos ], nLen );
      hb_strncat( pszBuffer, " ", nLen );
   }

   hb_retc_buffer( pszBuffer );
}

/* Check for command line internal arguments */
ULONG hb_cmdargProcessVM( int *pCancelKey, int *pCancelKeyEx )
{
   char * cFlags;
   ULONG ulFlags = HB_VMFLAG_HARBOUR;

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
#if defined(HB_C52_STRICT)
         hb_snprintf( buffer, sizeof( buffer ), "DS avail=%luKB  OS avail=%luKB  EMM avail=%luKB", hb_xquery( HB_MEM_BLOCK ), hb_xquery( HB_MEM_VM ), hb_xquery( HB_MEM_EMS ) );
#else
         hb_snprintf( buffer, sizeof( buffer ), "DS avail=%luKB  OS avail=%luKB  EMM avail=%luKB  MemStat:%s  MT:%s", hb_xquery( HB_MEM_BLOCK ), hb_xquery( HB_MEM_VM ), hb_xquery( HB_MEM_EMS ), hb_xquery( HB_MEM_USEDMAX ) ? "On" : "Off", hb_vmIsMt() ? "On" : "Off" );
#endif
         hb_conOutErr( buffer, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
      }
   }

   if( hb_cmdargCheck( "BUILD" ) )
      hb_verBuildInfo();

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
/*
            case 'x':
               ulFlags |= HB_VMFLAG_XBASE;
               break;

            case 'r':
               ulFlags |= HB_VMFLAG_RT_MACRO;
               break;
*/
            case 's':
               ulFlags |= HB_VMFLAG_ARRSTR;
               break;
         }
      }
      hb_xfree( cFlags );
   }

   if( ( cFlags = hb_cmdargString( "CANCEL" ) ) != NULL ) 
   {
      int iVal = atoi( cFlags );
      if( iVal )
         *pCancelKey = iVal;
      hb_xfree( cFlags );
   }

   if( ( cFlags = hb_cmdargString( "CANCELEX" ) ) != NULL ) 
   {
      int iVal = atoi( cFlags );
      if( iVal )
         *pCancelKeyEx = iVal;
      hb_xfree( cFlags );
   }

   return ulFlags;
}

/* ChangeLog SVN revision number */
int hb_verSvnID( void )
{
   return HB_VER_SVNID;
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

/* build time C compiler flags in C_USR envvar */
const char * hb_verFlagsC( void )
{
#ifdef HB_VER_C_USR
   return HB_VER_C_USR;
#else
   return "";
#endif
}

/* build time linker flags in L_USR envvar */
const char * hb_verFlagsL( void )
{
#ifdef HB_VER_L_USR
   return HB_VER_L_USR;
#else
   return "";
#endif
}

/* build time Harbour compiler flags in PRG_USR envvar */
const char * hb_verFlagsPRG( void )
{
#ifdef HB_VER_PRG_USR
   return HB_VER_PRG_USR;
#else
   return "";
#endif
}
