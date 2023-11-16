/*
 * Windows API functions (winuser)
 *
 * Copyright 2009-2014 Viktor Szakats (vszakats.net/harbour)
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

HB_FUNC( WAPI_MESSAGEBOX )
{
   void * hStr1;
   void * hStr2;

   int iResult = MessageBox( hbwapi_par_raw_HWND( 1 ),
                             HB_PARSTR( 2, &hStr1, NULL ),
                             HB_PARSTR( 3, &hStr2, NULL ),
                             hbwapi_par_INT( 4 ) );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_NI( iResult );
   hb_strfree( hStr1 );
   hb_strfree( hStr2 );
}

static int s_MessageBoxTimeout( IN HWND hWnd,
                                IN LPCTSTR lpText, IN LPCTSTR lpCaption,
                                IN UINT uType, IN WORD wLanguageId,
                                IN DWORD dwMilliseconds )
{
   /* undocumented Windows API */
   typedef int ( __stdcall * _HB_MSGBOXTOUT )( IN HWND hWnd,
                                               IN LPCTSTR lpText, IN LPCTSTR lpCaption,
                                               IN UINT uType, IN WORD wLanguageId,
                                               IN DWORD dwMilliseconds );
   static _HB_MSGBOXTOUT s_pMessageBoxTimeout = ( _HB_MSGBOXTOUT ) -1;

   if( s_pMessageBoxTimeout == ( _HB_MSGBOXTOUT ) -1 )
   {
      HMODULE hModule = GetModuleHandle( TEXT( "user32.dll" ) );
      s_pMessageBoxTimeout = hModule == NULL ? NULL : ( _HB_MSGBOXTOUT )
               HB_WINAPI_GETPROCADDRESST( hModule, "MessageBoxTimeout" );
   }

   return s_pMessageBoxTimeout ?
            s_pMessageBoxTimeout( hWnd, lpText, lpCaption, uType,
                                  wLanguageId, dwMilliseconds ) :
#if ! defined( HB_OS_WIN_CE )
            MessageBoxEx( hWnd, lpText, lpCaption, uType, wLanguageId );
#else
            MessageBox( hWnd, lpText, lpCaption, uType );
#endif
}

HB_FUNC( WAPI_MESSAGEBOXTIMEOUT )
{
   void * hStr1;
   void * hStr2;

   int iResult = s_MessageBoxTimeout( hbwapi_par_raw_HWND( 1 ),
                                      HB_PARSTR( 2, &hStr1, NULL ),
                                      HB_PARSTR( 3, &hStr2, NULL ),
                                      hbwapi_par_UINT( 4 ),
                                      hbwapi_par_WORD( 5 ),
                                      hbwapi_par_DWORD( 6 ) );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_NI( iResult );
   hb_strfree( hStr1 );
   hb_strfree( hStr2 );
}
