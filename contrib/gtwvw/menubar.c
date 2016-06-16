/*
 * Copyright Peter Rees <peter@rees.co.nz>
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

#include "hbgtwvw.h"

HB_FUNC( WVW_SETMENU )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      SetMenu( wvw_win->hWnd, hbwapi_par_raw_HMENU( 2 ) );

      hb_gt_wvw_ResetWindow( wvw_win );
   }
}

HB_FUNC( WVW_SETPOPUPMENU )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      HMENU hPopup = wvw_win->hPopup;

      wvw_win->hPopup = hbwapi_par_raw_HMENU( 2 );

      hbwapi_ret_raw_HANDLE( hPopup );
   }
   else
      hbwapi_ret_raw_HANDLE( NULL );
}

HB_FUNC( WVW_GETLASTMENUEVENT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   hb_retni( wvw_win ? wvw_win->LastMenuEvent : 0 );
}

HB_FUNC( WVW_SETLASTMENUEVENT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      hb_retni( wvw_win->LastMenuEvent );

      wvw_win->LastMenuEvent = hb_parni( 2 );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( WVW_SETMENUKEYEVENT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      hb_retni( hb_gt_wvw_SetMenuKeyEvent( wvw_win, hb_parni( 2 ) ) );
   else
      hb_retni( 0 );
}

/* wvw_MenuItem_SetBitmaps( hMenu, nIDEnableItem, nPosition, ncBitmapUnchecked, ncBimapChecked ) */
HB_FUNC( WVW_MENUITEM_SETBITMAPS )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      HBITMAP hBitmapUnchecked = NULL;
      HBITMAP hBitmapChecked   = NULL;
      char    szResName[ HB_PATH_MAX + 1 ];
      int     iWidth = 0, iHeight = 0;

      if( HB_ISNUM( 4 ) )
      {
         hb_snprintf( szResName, sizeof( szResName ), "?%u", hb_parni( 4 ) );

         hBitmapUnchecked = hb_gt_wvw_FindBitmapHandle( szResName, &iWidth, &iHeight );

         if( ! hBitmapUnchecked )
         {
            hBitmapUnchecked = ( HBITMAP ) LoadImage( GetModuleHandle( NULL ), MAKEINTRESOURCE( hb_parni( 4 ) ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
            hb_gt_wvw_AddBitmapHandle( szResName, hBitmapUnchecked, iWidth, iHeight );
         }
      }
      else if( HB_ISCHAR( 4 ) )
      {
         hBitmapUnchecked = hb_gt_wvw_FindBitmapHandle( hb_parc( 4 ), &iWidth, &iHeight );

         if( ! hBitmapUnchecked )
         {
            void * hName;
            hBitmapUnchecked = ( HBITMAP ) LoadImage( GetModuleHandle( NULL ), HB_PARSTR( 4, &hName, NULL ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
            hb_strfree( hName );
            hb_gt_wvw_AddBitmapHandle( hb_parc( 4 ), hBitmapUnchecked, iWidth, iHeight );
         }
      }

      if( HB_ISNUM( 5 ) )
      {
         hb_snprintf( szResName, sizeof( szResName ), "?%u", hb_parni( 5 ) );

         hBitmapChecked = hb_gt_wvw_FindBitmapHandle( szResName, &iWidth, &iHeight );

         if( ! hBitmapChecked )
         {
            hBitmapChecked = ( HBITMAP ) LoadImage( GetModuleHandle( NULL ), MAKEINTRESOURCE( hb_parni( 5 ) ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
            hb_gt_wvw_AddBitmapHandle( szResName, hBitmapChecked, iWidth, iHeight );
         }
      }
      else if( HB_ISCHAR( 5 ) )
      {
         hBitmapChecked = hb_gt_wvw_FindBitmapHandle( hb_parc( 5 ), &iWidth, &iHeight );

         if( ! hBitmapChecked )
         {
            void * hName;
            hBitmapChecked = ( HBITMAP ) LoadImage( GetModuleHandle( NULL ), HB_PARSTR( 5, &hName, NULL ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
            hb_strfree( hName );
            hb_gt_wvw_AddBitmapHandle( hb_parc( 5 ), hBitmapChecked, iWidth, iHeight );
         }
      }

      if( HB_ISNUM( 2 ) )
         SetMenuItemBitmaps( hbwapi_par_raw_HMENU( 1 ), hb_parni( 2 ), MF_BYCOMMAND, ( HBITMAP ) hBitmapUnchecked, ( HBITMAP ) hBitmapChecked );
      else
         SetMenuItemBitmaps( hbwapi_par_raw_HMENU( 1 ), hb_parni( 3 ), MF_BYPOSITION, ( HBITMAP ) hBitmapUnchecked, ( HBITMAP ) hBitmapChecked );
   }
}
