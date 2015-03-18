/*
 * Windows API functions (security.h)
 *
 * Copyright 2015 Viktor Szakats (vszakats.net/harbour)
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbwapi.h"

#ifndef SECURITY_WIN32
#define SECURITY_WIN32
#endif

/* Workaround for unfixed 'w32api' (as of 3.17.2) bug:
   https://sourceforge.net/p/mingw/bugs/279/#efbc */
#undef _WIN32_WINNT
#define _WIN32_WINNT  0x0500

#include "security.h"

HB_FUNC( WAPI_GETUSERNAMEEX )
{
   HB_BOOL fResult = HB_FALSE;

#if defined( HB_OS_WIN_CE )
   hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
#else
   {
      typedef int ( WINAPI * _HB_GETUSERNAMEEX )( EXTENDED_NAME_FORMAT, LPTSTR, PULONG );

      static _HB_GETUSERNAMEEX s_pGetUserNameEx = NULL;

      if( ! s_pGetUserNameEx )
      {
         HMODULE hModule = hbwapi_LoadLibrarySystem( TEXT( "secur32.dll" ) );
         if( hModule )
            s_pGetUserNameEx = ( _HB_GETUSERNAMEEX ) HB_WINAPI_GETPROCADDRESST( hModule,
               "GetUserNameEx" );
      }

      if( s_pGetUserNameEx )
      {
         EXTENDED_NAME_FORMAT nFormat = ( EXTENDED_NAME_FORMAT ) hb_parnl( 1 );
         ULONG nLen = 256 + 1;
         DWORD dwError;
         LPTSTR pBuffer = ( LPTSTR ) hb_xgrab( nLen * sizeof( TCHAR ) );
         SetLastError( ERROR_SUCCESS );  /* This API call fails to reset the error code on success */
         fResult = ( HB_BOOL ) s_pGetUserNameEx( nFormat, pBuffer, &nLen );
         hbwapi_SetLastError( dwError = GetLastError() );
         if( ! fResult && dwError == ERROR_MORE_DATA )
         {
            pBuffer = ( LPTSTR ) hb_xrealloc( pBuffer, nLen * sizeof( TCHAR ) );
            fResult = ( HB_BOOL ) s_pGetUserNameEx( nFormat, pBuffer, &nLen );
            hbwapi_SetLastError( dwError = GetLastError() );
         }
         if( dwError != ERROR_SUCCESS )
            fResult = HB_FALSE;
         if( fResult )
            HB_STORSTRLEN( pBuffer, nLen, 2 );
         hb_xfree( pBuffer );
      }
      else
         hbwapi_SetLastError( ERROR_INVALID_FUNCTION );
   }
#endif

   if( ! fResult )
      hb_storc( NULL, 2 );

   hb_retl( fResult );
}
