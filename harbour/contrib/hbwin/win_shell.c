/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API functions (shellapi.h - shell32.dll)
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#undef _WIN32_IE
#define _WIN32_IE 0x0500 /* request Windows 2000 features for NOTIFYICONDATA */

#include "hbwapi.h"

/* WIN_ShellNotifyIcon( [<hWnd>], [<nUID>], [<nMessage>], [<hIcon>],
                        [<cTooltip>], [<lAddDel>],
                        [<cInfo>], [<nInfoTimeOut>], [<cInfoTitle>], [<nInfoFlags>] ) -> <lOK> */
HB_FUNC( WIN_SHELLNOTIFYICON )
{
   NOTIFYICONDATA tnid;

   memset( &tnid, 0, sizeof( tnid ) );
   tnid.cbSize = sizeof( tnid );
   tnid.hWnd = wapi_par_HWND( 1 );
   tnid.uID = wapi_par_UINT( 2 );
   tnid.uCallbackMessage = wapi_par_UINT( 3 );
   if( tnid.uCallbackMessage )
      tnid.uFlags = NIF_MESSAGE;
   tnid.hIcon = wapi_par_HICON( 4 );
   if( tnid.hIcon )
      tnid.uFlags |= NIF_ICON;
   if( HB_ITEMCOPYSTR( hb_param( 5, HB_IT_ANY ),
                       tnid.szTip, HB_SIZEOFARRAY( tnid.szTip ) ) > 0 )
      tnid.uFlags |= NIF_TIP;

   #if defined( NIF_INFO ) /* did the headers provide Windows 2000 features? */
   if( hb_iswin2k() ) /* are we running on Windows 2000 or above? */
   {
      if( HB_ITEMCOPYSTR( hb_param( 7, HB_IT_ANY ), tnid.szInfo, HB_SIZEOFARRAY( tnid.szInfo ) ) > 0 )
         tnid.uFlags |= NIF_INFO;
      tnid.uTimeout = ( UINT ) hb_parni( 8 );
      if( HB_ITEMCOPYSTR( hb_param( 9, HB_IT_ANY ), tnid.szInfoTitle, HB_SIZEOFARRAY( tnid.szInfoTitle ) ) > 0 )
         tnid.uFlags |= NIF_INFO;
      tnid.dwInfoFlags = ( DWORD ) hb_parnl( 10 );
   }
   #endif

   wapi_ret_L( Shell_NotifyIcon( HB_ISLOG( 6 ) ?
               ( hb_parl( 6 ) ? NIM_ADD : NIM_DELETE ) : NIM_MODIFY, &tnid ) );
}
