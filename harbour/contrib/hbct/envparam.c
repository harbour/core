/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    CT3 Number and bit manipulation functions:
 *       ENVPARAM()
 *
 * Copyright 2012 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbapi.h"

#if defined( HB_OS_UNIX )
#  include <unistd.h>
#  if defined( HB_OS_DARWIN )
#     include <crt_externs.h>
#     define environ  ( *_NSGetEnviron() )
#  elif ! defined( __WATCOMC__ )
      extern char ** environ;
#  endif
#elif defined( HB_OS_DOS )
#  if defined( __DJGPP__ )
      extern char ** environ;
#  elif ! defined( __WATCOMC__ )
#     define environ _environ
      extern char ** _environ;
#  endif
#elif defined( HB_OS_OS2 )
#  if ! defined( __WATCOMC__ )
      extern char ** environ;
#  endif
#elif defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
#  include "hbwinuni.h"
#  include <windows.h>
#endif

HB_FUNC( ENVPARAM )
{
#if defined( HB_OS_UNIX ) || defined( HB_OS_DOS ) || defined( HB_OS_OS2 )
   char * const * pEnviron = environ, * const * pEnv;
   char * pResult = NULL, * pDst;
   HB_SIZE nSize = 0;

   if( pEnviron )
   {
      for( pEnv = pEnviron; *pEnv; pEnv++ )
         nSize += strlen( *pEnv ) + 2;

      if( nSize > 0 )
      {
         pResult = ( char * ) hb_xgrab( ( nSize + 1 ) * sizeof( char ) );
         for( pEnv = pEnviron, pDst = pResult; *pEnv; pEnv++ )
         {
            HB_SIZE n = strlen( *pEnv );
            memcpy( pDst, *pEnv, n );
            pDst += n;
            *pDst++ = '\r';
            *pDst++ = '\n';
         }
         *pDst++ = '\0';
      }
   }

   if( pResult )
      hb_retc_buffer( ( char * ) hb_osDecodeCP( pResult, NULL, NULL ) );
   else
      hb_retc_null();
#elif defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   LPTCH lpEnviron = GetEnvironmentStrings(), lpEnv;
   LPTSTR lpResult = NULL, lpDst;
   HB_SIZE nSize = 0;

   if( lpEnviron )
   {
      for( lpEnv = lpEnviron; *lpEnv; lpEnv++ )
      {
         while( *++lpEnv )
            ++nSize;
         nSize += 3;
      }
      if( nSize > 0 )
      {
         lpResult = ( LPTSTR ) hb_xgrab( ( nSize + 1 ) * sizeof( TCHAR ) );
         for( lpEnv = lpEnviron, lpDst = lpResult; *lpEnv; lpEnv++ )
         {
            do
            {
               *lpDst++ = *lpEnv++;
            }
            while( *lpEnv );
            *lpDst++ = '\r';
            *lpDst++ = '\n';
         }
      }
   }

   FreeEnvironmentStrings( lpEnviron );

   if( lpResult )
   {
      HB_RETSTRLEN( lpResult, nSize );
      hb_xfree( lpResult );
   }
   else
      hb_retc_null();
#else
   hb_retc_null();
#endif
}
