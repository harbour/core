/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 *     Copyright 2004 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 *
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
*/

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapiitm.h"

static HKEY hb_regkeyconv( ULONG nKey )
{
   switch( nKey )
   {
   case 1:
      return ( HKEY ) HKEY_CLASSES_ROOT;
   case 2:
      return ( HKEY ) HKEY_CURRENT_USER;
#if ! defined( HB_OS_WIN_CE )
   case 3:
      return ( HKEY ) HKEY_CURRENT_CONFIG;
#endif
   case 0:
   case 4:
      return ( HKEY ) HKEY_LOCAL_MACHINE;
   case 5:
      return ( HKEY ) HKEY_USERS;
   }

   return ( HKEY ) nKey;
}

HB_FUNC( WIN_REGCREATEKEYEX )
{
   HKEY hWnd = ( HKEY ) hb_parnl( 8 );
   ULONG nResult = hb_parnl( 9 );
   LPTSTR lpText = HB_TCHAR_CONVTO( hb_parc( 2 ) );

   if( RegCreateKeyEx( hb_regkeyconv( hb_parnl( 1 ) ),
                       lpText,
                       0,
                       NULL,
                       hb_parnl( 5 ),
                       hb_parnl( 6 ),
                       NULL,
                       &hWnd,
                       &nResult ) == ERROR_SUCCESS )
   {
      hb_stornl( ( ULONG ) hWnd, 8 );
      hb_stornl( nResult, 9 );

      hb_retnl( ERROR_SUCCESS );
   }
   else
      hb_retnl( -1 );

   HB_TCHAR_FREE( lpText );
}

HB_FUNC( WIN_REGOPENKEYEX )
{
   HKEY hWnd;
   LPTSTR lpText = HB_TCHAR_CONVTO( hb_parc( 2 ) );

   if( RegOpenKeyEx( hb_regkeyconv( hb_parnl( 1 ) ),
                     lpText,
                     0,
                     hb_parnl( 4 ),
                     &hWnd ) == ERROR_SUCCESS )
   {
      hb_stornl( ( ULONG ) hWnd, 5 );
      hb_retnl( ERROR_SUCCESS );
   }
   else
      hb_retnl( -1 );

   HB_TCHAR_FREE( lpText );
}

HB_FUNC( WIN_REGQUERYVALUEEX )
{
   DWORD nType = 0;
   DWORD nSize = 0;
   LPTSTR lpKey = HB_TCHAR_CONVTO( hb_parc( 2 ) );

   if( RegQueryValueEx( hb_regkeyconv( hb_parnl( 1 ) ),
                        lpKey,
                        NULL,
                        &nType,
                        NULL,
                        &nSize ) == ERROR_SUCCESS )
   {
      if( nSize > 0 )
      {
         BYTE * cValue = ( BYTE * ) hb_xgrab( nSize + 1 );

         RegQueryValueEx( hb_regkeyconv( hb_parnl( 1 ) ),
                          lpKey,
                          NULL,
                          &nType,
                          ( BYTE * ) cValue,
                          &nSize );

         hb_stornl( nType, 4 );

         if( ! hb_storclen_buffer( ( char * ) cValue, nSize, 5 ) )
            hb_xfree( cValue );
      }
   }
   HB_TCHAR_FREE( lpKey );

   hb_retnl( nSize );
}

HB_FUNC( WIN_REGSETVALUEEX )
{
   DWORD nType = hb_parnl( 4 );
   LPTSTR lpKey = HB_TCHAR_CONVTO( hb_parc( 2 ) );

   if( nType != REG_DWORD )
   {
      BYTE * cValue = ( BYTE * ) hb_parc( 5 );
      hb_retni( RegSetValueEx( hb_regkeyconv( hb_parnl( 1 ) ),
                               lpKey,
                               0,
                               nType,
                               ( BYTE * ) cValue,
                               hb_parclen( 5 ) + 1 ) );
   }
   else
   {
      DWORD nSpace = hb_parnl( 5 );
      hb_retni( RegSetValueEx( hb_regkeyconv( hb_parnl( 1 ) ),
                               lpKey,
                               0,
                               nType,
                               ( BYTE * ) &nSpace,
                               sizeof( REG_DWORD ) ) );
   }

   HB_TCHAR_FREE( lpKey );
}

HB_FUNC( WIN_REGCLOSEKEY )
{
   hb_retnl( RegCloseKey( ( HKEY ) hb_parnl( 1 ) ) );
}
