/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * environment variables access
 *
 * Copyright 2001-2002 Antonio Linares <alinares@fivetech.com>
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

/* NOTE: Notice that this code is needed as ANSI C getenv() crashes
         badly when used from a Windows DLL. */

#include "hbapi.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
   #if defined( HB_OS_WIN_CE )
      #include "hbwince.h"
   #endif
#elif defined( HB_OS_OS2 )
   #define INCL_DOSMISC
   #define INCL_ERRORS
   #include <os2.h>
#endif

/* NOTE: Warning, this function _may_ return NULL as a result if
         the environment variable reading fails form some reason.
         If the return value is not NULL, the caller must free
         the pointer. [vszakats] */

char * hb_getenv( const char * szName )
{
   char * pszBuffer = NULL;

#if defined( HB_OS_WIN )

   {
      LPTSTR lpName = HB_TCHAR_CONVTO( szName );
      DWORD size = GetEnvironmentVariable( lpName, NULL, 0 );

      if( size != 0 )
      {
         LPTSTR lpBuffer = ( LPTSTR ) hb_xgrab( size * sizeof( TCHAR ) );
         GetEnvironmentVariable( lpName, lpBuffer, size );
#if defined( UNICODE )
         pszBuffer = hb_wctomb( lpBuffer );
         hb_xfree( lpBuffer );
#else
         pszBuffer = lpBuffer;
#endif
      }
      HB_TCHAR_FREE( lpName );
   }

#elif defined( HB_OS_OS2 )

   {
      PSZ EnvValue = ( PSZ ) "";

      if( DosScanEnv( ( PCSZ ) szName, &EnvValue ) == NO_ERROR )
         pszBuffer = hb_strdup( ( char * ) EnvValue );
   }

#else

   {
      char * pszTemp = getenv( szName );

      if( pszTemp != NULL )
         pszBuffer = hb_strdup( pszTemp );
   }

#endif

   return pszBuffer;
}


HB_BOOL hb_getenv_buffer( const char * szName, char * szBuffer, int nSize )
{
   HB_BOOL fRetVal;

#if defined( HB_OS_WIN )
   {
#if defined( UNICODE )
      TCHAR name[ 128 ];
      TCHAR buffer[ 128 ];
      LPTSTR lpName, lpBuffer;
      HB_SIZE nLen = strlen( szName ) + 1;

      if( nLen <= HB_SIZEOFARRAY( name ) )
         lpName = name;
      else
         lpName = ( LPTSTR ) hb_xgrab( nLen * sizeof( TCHAR ) );

      hb_mbntowccpy( lpName, szName, nLen - 1 );

      if( szBuffer == NULL )
         lpBuffer = NULL;
      else if( nSize <= ( int ) HB_SIZEOFARRAY( buffer ) )
         lpBuffer = buffer;
      else
         lpBuffer = ( LPTSTR ) hb_xgrab( nSize * sizeof( TCHAR ) );

      fRetVal = GetEnvironmentVariable( lpName, lpBuffer, nSize ) != 0;
      lpBuffer[ nSize - 1 ] = L'\0';

      hb_wcntombcpy( szBuffer, lpBuffer, nSize - 1 );

      if( lpName != name )
         hb_xfree( lpName );
      if( lpBuffer && lpBuffer != buffer )
         hb_xfree( lpBuffer );
#else
      fRetVal = GetEnvironmentVariable( szName, szBuffer, nSize ) != 0;
#endif
   }
#elif defined( HB_OS_OS2 )
   {
      PSZ EnvValue = ( PSZ ) "";

      if( DosScanEnv( ( PCSZ ) szName, &EnvValue ) == NO_ERROR )
      {
         fRetVal = HB_TRUE;
         if( szBuffer != NULL && nSize != 0 )
            hb_strncpy( szBuffer, ( char * ) EnvValue, nSize - 1 );
      }
      else
         fRetVal = HB_FALSE;
   }
#else
   {
      char * pszTemp = getenv( szName );

      if( pszTemp != NULL )
      {
         fRetVal = HB_TRUE;
         if( szBuffer != NULL && nSize != 0 )
            hb_strncpy( szBuffer, pszTemp, nSize - 1 );
      }
      else
         fRetVal = HB_FALSE;
   }
#endif

   if( !fRetVal && szBuffer != NULL && nSize != 0 )
      szBuffer[ 0 ] = '\0';

   return fRetVal;
}

/* set current process environment variable, if szValue is NULL delete
 * environment variable
 */
HB_BOOL hb_setenv( const char * szName, const char * szValue )
{
   if( szName == NULL )
      return HB_FALSE;

#if defined( HB_OS_WIN )
   {
      LPTSTR lpName = HB_TCHAR_CONVTO( szName );
      LPTSTR lpValue = szValue ? HB_TCHAR_CONVTO( szValue ) : NULL;
      HB_BOOL bResult = ( SetEnvironmentVariable( lpName, lpValue ) != 0 );
      if( lpValue )
         HB_TCHAR_FREE( lpValue );
      HB_TCHAR_FREE( lpName );
      return bResult;
   }

#elif defined( _BSD_SOURCE ) || _POSIX_C_SOURCE >= 200112L || \
      _XOPEN_SOURCE >= 600 || \
      defined( __WATCOMC__ ) || defined( __DJGPP__ ) || \
      defined( HB_OS_SUNOS ) || defined( HB_OS_BSD ) || \
      defined( HB_OS_DARWIN ) || defined( HB_OS_BEOS ) || \
      defined( HB_OS_QNX ) || defined( HB_OS_VXWORKS ) || \
      defined( HB_OS_CYGWIN ) || defined( HB_OS_MINIX )

   if( szValue )
      return setenv( szName, szValue, 1 ) == 0;
   else
   {
#  if ( defined( __DJGPP__ ) && \
        ( __DJGPP__ < 2 || ( __DJGPP__ == 2 && __DJGPP_MINOR__ < 4 ) ) ) || \
      defined( __WATCOMC__ )
      szValue = getenv( szName );
      if( szValue && *szValue )
         return setenv( szName, "", 1 ) == 0;
      else
         return HB_TRUE;
#  elif defined( __OpenBSD__ ) || defined( HB_OS_QNX )
      unsetenv( szName );
      return HB_TRUE;
#  else
      return unsetenv( szName ) == 0;
#  endif
   }

#elif defined( _HB_NO_SETENV_ )

   HB_SYMBOL_UNUSED( szName );
   HB_SYMBOL_UNUSED( szValue );

   return HB_FALSE;

#else
   /* please add support for other C compilers
    * if such functionality does not exists for given platform/C compiler
    * then please simply added C compiler with necessary OS/version checking
    * to the above #elif ... to eliminate warning [druzus]
    */

   int iTODO;

   HB_SYMBOL_UNUSED( szName );
   HB_SYMBOL_UNUSED( szValue );

   return HB_FALSE;

#endif
}
