/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API function WAPI_ADDFONTMEMRESOURCEEX() (wingdi.h) (alpha)
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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

#include "hbwapi.h"

HB_FUNC( WAPI_ADDFONTRESOURCE )
{
   void * hFileName;

   hb_retni( AddFontResource( HB_PARSTRDEF( 1, &hFileName, NULL ) ) );
   hb_strfree( hFileName );
}

HB_FUNC( WAPI_REMOVEFONTRESOURCE )
{
   void * hFileName;

   hb_retni( RemoveFontResource( HB_PARSTRDEF( 1, &hFileName, NULL ) ) );
   hb_strfree( hFileName );
}

HB_FUNC( WAPI_ADDFONTRESOURCEEX )
{
   int iResult = 0;

#if ! defined( HB_OS_WIN_CE )
   {
      typedef int ( WINAPI * _HB_ADDFONTRESOURCEEX )( LPCTSTR, DWORD, PVOID );

      static _HB_ADDFONTRESOURCEEX s_pAddFontResourceEx = NULL;

      if( ! s_pAddFontResourceEx )
         s_pAddFontResourceEx = ( _HB_ADDFONTRESOURCEEX ) GetProcAddress( GetModuleHandle( TEXT( "gdi32.dll" ) ),
            HB_WINAPI_FUNCTION_NAME( "AddFontResourceEx" ) );

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
   int iResult = 0;

#if ! defined( HB_OS_WIN_CE )
   {
      typedef int ( WINAPI * _HB_REMOVEFONTRESOURCEEX )( LPCTSTR, DWORD, PVOID );

      static _HB_REMOVEFONTRESOURCEEX s_pRemoveFontResourceEx = NULL;

      if( ! s_pRemoveFontResourceEx )
         s_pRemoveFontResourceEx = ( _HB_REMOVEFONTRESOURCEEX ) GetProcAddress( GetModuleHandle( TEXT( "gdi32.dll" ) ),
            HB_WINAPI_FUNCTION_NAME( "RemoveFontResourceEx" ) );

      if( s_pRemoveFontResourceEx )
      {
         void * hFileName;
         iResult = s_pRemoveFontResourceEx( HB_PARSTRDEF( 1, &hFileName, NULL ), ( DWORD ) hb_parnl( 2 ), NULL );
         hb_strfree( hFileName );
      }
   }
#endif

   hb_retni( iResult );
}
