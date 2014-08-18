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
      SetMenu( wvw_win->hWnd, ( HMENU ) HB_PARHANDLE( 2 ) );

      hb_gt_wvw_ResetWindow( wvw_win );
   }
}

HB_FUNC( WVW_SETPOPUPMENU )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      HMENU hPopup = wvw_win->hPopup;

      wvw_win->hPopup = ( HMENU ) HB_PARHANDLE( 2 );
      /* if( hPopup ) */
      {
         HB_RETHANDLE( hPopup );
      }
   }
   else
      HB_RETHANDLE( NULL );
}

HB_FUNC( WVW_CREATEMENU )
{
   HB_RETHANDLE( CreateMenu() );
}

HB_FUNC( WVW_CREATEPOPUPMENU )
{
   HB_RETHANDLE( CreatePopupMenu() );
}

/* wvw_AppendMenu( hMenu, nFlags, nMenuItemId, cCaption ) */
HB_FUNC( WVW_APPENDMENU )
{
   TCHAR   ucBuf[ 256 ];
   LPCTSTR szCaption;

   if( ! ( hb_parni( 2 ) & ( MF_SEPARATOR | MF_POPUP ) ) &&
       hb_parni( 3 ) >= WVW_ID_BASE_PUSHBUTTON )
   {
      MessageBox( NULL, TEXT( "Menu command ID too high. Potential conflict with pushbutton" ), hb_gt_wvw_GetAppName(), MB_ICONERROR );
      hb_retl( HB_FALSE );
      return;
   }

   if( HB_ISCHAR( 4 ) )
   {
      void *  hCaption;
      HB_SIZE nLen;

      szCaption = HB_PARSTR( 4, &hCaption, &nLen );

      if( nLen > 0 && nLen < 256 )
      {
         HB_SIZE i;

         for( i = 0; i < nLen; i++ )
            ucBuf[ i ] = szCaption[ i ] == TEXT( '~' ) ? TEXT( '&' ) : szCaption[ i ];

         ucBuf[ nLen ] = TEXT( '\0' );

         szCaption = ucBuf;
      }

      hb_strfree( hCaption );
   }
   else
      szCaption = ( LPCTSTR ) ( HB_PTRDIFF ) hb_parnint( 4 );  /* TOFIX: delete this */

   hb_retl( AppendMenu( ( HMENU ) HB_PARHANDLE( 1 ), ( UINT ) hb_parni( 2 ), ( UINT_PTR ) hb_parnint( 3 ), szCaption ) );
}

HB_FUNC( WVW_DELETEMENU )
{
   hb_retl( DeleteMenu( ( HMENU ) HB_PARHANDLE( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

HB_FUNC( WVW_DESTROYMENU )
{
   hb_retl( DestroyMenu( ( HMENU ) HB_PARHANDLE( 1 ) ) );
}

HB_FUNC( WVW_ENABLEMENUITEM )
{
   hb_retl( EnableMenuItem( ( HMENU ) HB_PARHANDLE( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}

HB_FUNC( WVW_GETLASTMENUEVENT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      hb_retni( wvw_win->LastMenuEvent );
   else
      hb_retni( 0 );
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

/* WVW_MENUITEM_SETBITMAPS( hMenu, nIDEnableItem, nPosition, ncBitmapUnchecked, ncBimapChecked ) */
HB_FUNC( WVW_MENUITEM_SETBITMAPS )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      HBITMAP hBitmapUnchecked = NULL;
      HBITMAP hBitmapChecked   = NULL;
      char    szResname[ HB_PATH_MAX + 1 ];
      int     iWidth, iHeight;

      if( HB_ISNUM( 4 ) )
      {
         hb_snprintf( szResname, sizeof( szResname ), "?%u", hb_parni( 4 ) );

         hBitmapUnchecked = hb_gt_wvw_FindBitmapHandle( szResname, &iWidth, &iHeight );

         if( ! hBitmapUnchecked )
         {
            hBitmapUnchecked = ( HBITMAP ) LoadImage( wvw->hInstance, ( LPCTSTR ) MAKEINTRESOURCE( ( WORD ) hb_parni( 4 ) ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
            hb_gt_wvw_AddBitmapHandle( szResname, hBitmapUnchecked, iWidth, iHeight );
         }
      }
      else if( HB_ISCHAR( 4 ) )
      {
         hBitmapUnchecked = hb_gt_wvw_FindBitmapHandle( hb_parc( 4 ), &iWidth, &iHeight );

         if( ! hBitmapUnchecked )
         {
            void * hName;
            hBitmapUnchecked = ( HBITMAP ) LoadImage( wvw->hInstance, HB_PARSTR( 4, &hName, NULL ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
            hb_strfree( hName );
            hb_gt_wvw_AddBitmapHandle( hb_parc( 4 ), hBitmapUnchecked, iWidth, iHeight );
         }
      }

      if( HB_ISNUM( 5 ) )
      {
         hb_snprintf( szResname, sizeof( szResname ), "?%u", hb_parni( 5 ) );

         hBitmapChecked = hb_gt_wvw_FindBitmapHandle( szResname, &iWidth, &iHeight );

         if( ! hBitmapChecked )
         {
            hBitmapChecked = ( HBITMAP ) LoadImage( wvw->hInstance, ( LPCTSTR ) MAKEINTRESOURCE( ( WORD ) hb_parni( 5 ) ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
            hb_gt_wvw_AddBitmapHandle( szResname, hBitmapChecked, iWidth, iHeight );
         }
      }
      else if( HB_ISCHAR( 5 ) )
      {
         hBitmapChecked = hb_gt_wvw_FindBitmapHandle( hb_parc( 5 ), &iWidth, &iHeight );

         if( ! hBitmapChecked )
         {
            void * hName;
            hBitmapChecked = ( HBITMAP ) LoadImage( wvw->hInstance, HB_PARSTR( 5, &hName, NULL ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
            hb_strfree( hName );
            hb_gt_wvw_AddBitmapHandle( hb_parc( 5 ), hBitmapChecked, iWidth, iHeight );
         }
      }

      if( HB_ISNUM( 2 ) )
         SetMenuItemBitmaps( ( HMENU ) HB_PARHANDLE( 1 ), hb_parni( 2 ), MF_BYCOMMAND, ( HBITMAP ) hBitmapUnchecked, ( HBITMAP ) hBitmapChecked );
      else
         SetMenuItemBitmaps( ( HMENU ) HB_PARHANDLE( 1 ), hb_parni( 3 ), MF_BYPOSITION, ( HBITMAP ) hBitmapUnchecked, ( HBITMAP ) hBitmapChecked );
   }
}

HB_FUNC( WVW_DRAWMENUBAR )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      DrawMenuBar( wvw_win->hWnd );
}

HB_FUNC( WVW_ENDMENU )
{
   hb_retl( EndMenu() );
}

/* wvw_GetMenu([nWinNum]) */
HB_FUNC( WVW_GETMENU )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      HB_RETHANDLE( GetMenu( wvw_win->hWnd ) );
   else
      HB_RETHANDLE( NULL );
}

/* wvw_TrackPopupMenu([nWinNum], n) */
HB_FUNC( WVW_TRACKPOPUPMENU )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      POINT xy;

      memset( &xy, 0, sizeof( xy ) );

      GetCursorPos( &xy );

      hb_retni( TrackPopupMenu( ( HMENU ) HB_PARHANDLE( 2 ),
                                TPM_CENTERALIGN | TPM_RETURNCMD | TPM_RECURSE,
                                xy.x,
                                xy.y,
                                0,
                                wvw_win->hWnd,
                                NULL ) );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( WIN_SETMENU )
{
   SetMenu( ( HWND ) HB_PARHANDLE( 1 ), ( HMENU ) HB_PARHANDLE( 2 ) );
}

/* wvw_NoSysMenu( [nWinNum], lRemoveClose )
 * removes System Menu of a window
 * if lRemoveClose is .T., also removes the 'Close' command and 'X' button
 *
 * no return value
 */
HB_FUNC( WVW_NOSYSMENU )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      HMENU hMenu = GetSystemMenu( wvw_win->hWnd, FALSE );

      if( hMenu )
      {
         DeleteMenu( hMenu, SC_MAXIMIZE, MF_BYCOMMAND );
         DeleteMenu( hMenu, SC_MINIMIZE, MF_BYCOMMAND );
         DeleteMenu( hMenu, SC_SIZE, MF_BYCOMMAND );
         DeleteMenu( hMenu, SC_MOVE, MF_BYCOMMAND );
         DeleteMenu( hMenu, SC_RESTORE, MF_BYCOMMAND );
         DeleteMenu( hMenu, SC_NEXTWINDOW, MF_BYCOMMAND );
         if( hb_parl( 2 ) /* lRemoveClose */ )
         {
            DeleteMenu( hMenu, SC_CLOSE, MF_BYCOMMAND );
            DeleteMenu( hMenu, 0, MF_BYPOSITION );
         }
         DrawMenuBar( wvw_win->hWnd );
      }
   }
}

/* wvw_GetSystemMenu( [nWinNum], lReset )
 * returns the System Menu of a window
 * if lRemoveClose is .T., also removes the 'Close' command and 'X' button
 */
HB_FUNC( WVW_GETSYSTEMMENU )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      HB_RETHANDLE( GetSystemMenu( wvw_win->hWnd, hb_parl( 2 ) /* lReset */ ) );
   else
      HB_RETHANDLE( NULL );
}
