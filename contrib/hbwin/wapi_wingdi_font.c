/*
 * Windows API function font load/unload (wingdi.h)
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
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

HB_FUNC( WAPI_ADDFONTRESOURCE )
{
   void * hFileName;

   hb_retni( AddFontResource( HB_PARSTRDEF( 1, &hFileName, NULL ) ) );
   hb_strfree( hFileName );
}

HB_FUNC( WAPI_ADDFONTMEMRESOURCEEX )
{
   DWORD dwFonts = 0;
   HANDLE hResult = NULL;

#if ! defined( HB_OS_WIN_CE )
   {
      typedef HANDLE ( WINAPI * _HB_ADDFONTMEMRESOURCEEX )( PVOID, DWORD, PVOID, DWORD * );

      static _HB_ADDFONTMEMRESOURCEEX s_pAddFontMemResourceEx = ( _HB_ADDFONTMEMRESOURCEEX ) -1;

      if( s_pAddFontMemResourceEx == ( _HB_ADDFONTMEMRESOURCEEX ) -1 )
      {
         HMODULE hModule = GetModuleHandle( TEXT( "gdi32.dll" ) );
         if( hModule )
            s_pAddFontMemResourceEx = ( _HB_ADDFONTMEMRESOURCEEX ) HB_WINAPI_GETPROCADDRESST( hModule,
               "AddFontMemResourceEx" );
         else
            s_pAddFontMemResourceEx = NULL;
      }

      if( s_pAddFontMemResourceEx )
         hResult = s_pAddFontMemResourceEx( ( PVOID ) hb_parcx( 1 ), ( DWORD ) hb_parclen( 1 ), NULL, &dwFonts );
   }
#endif

   hb_stornint( dwFonts, 3 );

   hbwapi_ret_raw_HANDLE( hResult );
}

HB_FUNC( WAPI_REMOVEFONTRESOURCE )
{
   void * hFileName;
   hbwapi_ret_L( RemoveFontResource( HB_PARSTRDEF( 1, &hFileName, NULL ) ) );
   hb_strfree( hFileName );
}

HB_FUNC( WAPI_ADDFONTRESOURCEEX )
{
   int iResult = 0;

#if ! defined( HB_OS_WIN_CE )
   {
      typedef int ( WINAPI * _HB_ADDFONTRESOURCEEX )( LPCTSTR, DWORD, PVOID );

      static _HB_ADDFONTRESOURCEEX s_pAddFontResourceEx = ( _HB_ADDFONTRESOURCEEX ) -1;

      if( s_pAddFontResourceEx == ( _HB_ADDFONTRESOURCEEX ) -1 )
      {
         HMODULE hModule = GetModuleHandle( TEXT( "gdi32.dll" ) );
         if( hModule )
            s_pAddFontResourceEx = ( _HB_ADDFONTRESOURCEEX ) HB_WINAPI_GETPROCADDRESST( hModule,
               "AddFontResourceEx" );
         else
            s_pAddFontResourceEx = NULL;
      }

      if( s_pAddFontResourceEx )
      {
         void * hFileName;
         iResult = s_pAddFontResourceEx( HB_PARSTRDEF( 1, &hFileName, NULL ), ( DWORD ) hb_parnl( 2 ), NULL );
         hb_strfree( hFileName );
      }
   }
#endif

   hb_retni( iResult );
}

HB_FUNC( WAPI_REMOVEFONTRESOURCEEX )
{
   BOOL fResult = FALSE;

#if ! defined( HB_OS_WIN_CE )
   {
      typedef BOOL ( WINAPI * _HB_REMOVEFONTRESOURCEEX )( LPCTSTR, DWORD, PVOID );

      static _HB_REMOVEFONTRESOURCEEX s_pRemoveFontResourceEx = ( _HB_REMOVEFONTRESOURCEEX ) -1;

      if( s_pRemoveFontResourceEx == ( _HB_REMOVEFONTRESOURCEEX ) -1 )
      {
         HMODULE hModule = GetModuleHandle( TEXT( "gdi32.dll" ) );
         if( hModule )
            s_pRemoveFontResourceEx = ( _HB_REMOVEFONTRESOURCEEX ) HB_WINAPI_GETPROCADDRESST( hModule,
               "RemoveFontResourceEx" );
         else
            s_pRemoveFontResourceEx = NULL;
      }

      if( s_pRemoveFontResourceEx )
      {
         void * hFileName;
         fResult = s_pRemoveFontResourceEx( HB_PARSTRDEF( 1, &hFileName, NULL ), ( DWORD ) hb_parnl( 2 ), NULL );
         hb_strfree( hFileName );
      }
   }
#endif

   hbwapi_ret_L( fResult );
}

HB_FUNC( WAPI_REMOVEFONTMEMRESOURCEEX )
{
   BOOL fResult = FALSE;

#if ! defined( HB_OS_WIN_CE )
   {
      typedef BOOL ( WINAPI * _HB_REMOVEFONTMEMRESOURCEEX )( HANDLE );

      static _HB_REMOVEFONTMEMRESOURCEEX s_pRemoveFontMemResourceEx = ( _HB_REMOVEFONTMEMRESOURCEEX ) -1;

      if( s_pRemoveFontMemResourceEx == ( _HB_REMOVEFONTMEMRESOURCEEX ) -1 )
      {
         HMODULE hModule = GetModuleHandle( TEXT( "gdi32.dll" ) );
         if( hModule )
            s_pRemoveFontMemResourceEx = ( _HB_REMOVEFONTMEMRESOURCEEX ) HB_WINAPI_GETPROCADDRESST( hModule,
               "RemoveFontMemResourceEx" );
         else
            s_pRemoveFontMemResourceEx = NULL;
      }

      if( s_pRemoveFontMemResourceEx )
         fResult = s_pRemoveFontMemResourceEx( hbwapi_par_raw_HANDLE( 1 ) );
   }
#endif

   hbwapi_ret_L( fResult );
}
