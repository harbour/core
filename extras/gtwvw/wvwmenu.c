/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw menu Functions
 * GTWVW is initially created based on:
 *
 * =Id: gtwvt.c,v 1.60 2004-01-26 08:14:07 vouchcac Exp =
 *
 * Harbour Project source code:
 * Video subsystem for Windows using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 * based on
 *   Bcc ConIO Video subsystem by
 *     Copyright 2002 Marek Paliwoda <paliwoda@inteia.pl>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *   Video subsystem for Windows compilers
 *     Copyright 1999-2000 Paul Tucker <ptucker@sympatico.ca>
 *     Copyright 2002 Przemyslaw Czerpak <druzus@polbox.com>
 *
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_wvw_Tone()
 *
 * See COPYING.txt for licensing terms.
 *
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.   If not, write to
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
 *
 */

#include "hbgtwvw.h"


/*                                                                   */
/*                 Peter Rees <peter@rees.co.nz>                     */
/*                                                                   */


HB_FUNC( WVW_SETMENU )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWinData = hb_gt_wvw_GetWindowsData( usWinNum );

   SetMenu( pWinData->hWnd, ( HMENU ) HB_PARHANDLE( 2 ) );

   hb_gt_wvwResetWindow( usWinNum );
}


HB_FUNC( WVW_SETPOPUPMENU )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA * pWinData = hb_gt_wvw_GetWindowsData( usWinNum );
   HMENU      hPopup   = pWinData->hPopup;

   pWinData->hPopup = ( HMENU ) HB_PARHANDLE( 2 );
   /* if ( hPopup ) */
   {
      HB_RETHANDLE( hPopup );
   }
}


HB_FUNC( WVW_CREATEMENU )
{
   HB_RETHANDLE( CreateMenu() );
}


HB_FUNC( WVW_CREATEPOPUPMENU )
{
   HB_RETHANDLE( CreatePopupMenu() );
}


/* WVW_APPENDMENU( hMenu, nFlags, nMenuItemId, cCaption ) */

HB_FUNC( WVW_APPENDMENU )
{
   char    ucBuf[ 256 ];
   int     i, iLen;
   LPCTSTR lpszCaption;

   if( ! ( hb_parni( 2 ) & ( MF_SEPARATOR | MF_POPUP ) ) &&
       ( hb_parni( 3 ) >= WVW_ID_BASE_PUSHBUTTON ) )
   {
      MessageBox( NULL, TEXT( "Menu Command Id too high. Potential conflict with pushbutton" ),
                  hb_gt_wvw_GetAppName(), MB_ICONERROR );
      hb_retl( FALSE );
      return;
   }

   if( HB_ISCHAR( 4 ) )
   {
      iLen = hb_parclen( 4 );
      if( iLen > 0 && iLen < 256 )
      {
         lpszCaption = hb_parcx( 4 );
         for( i = 0; i < iLen; i++ )
         {
            ucBuf[ i ] = ( *lpszCaption == '~' ) ? '&' : *lpszCaption;
            lpszCaption++;
         }
         ucBuf[ iLen ] = '\0';
         lpszCaption   = ucBuf;
      }
      else
         lpszCaption = hb_parcx( 4 );
   }
   else
      lpszCaption = ( LPCTSTR ) hb_parni( 4 );

   hb_retl( AppendMenu( ( HMENU ) HB_PARHANDLE( 1 ), ( UINT ) hb_parni( 2 ), ( UINT_PTR ) hb_parni( 3 ), ( LPCTSTR ) lpszCaption ) );
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
   hb_retni( EnableMenuItem( ( HMENU ) HB_PARHANDLE( 1 ), ( UINT ) hb_parni( 2 ), ( UINT ) hb_parni( 3 ) ) );
}


HB_FUNC( WVW_GETLASTMENUEVENT )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   hb_retni( hb_gt_wvwGetLastMenuEvent( usWinNum ) );
}


HB_FUNC( WVW_SETLASTMENUEVENT )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   hb_retni( hb_gt_wvwSetLastMenuEvent( usWinNum, hb_parni( 2 ) ) );
}


HB_FUNC( WVW_SETMENUKEYEVENT )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   int  iEvent   = 0;

   if( HB_ISNUM( 2 ) )
      iEvent = hb_parnl( 2 );

   hb_retni( hb_gt_wvwSetMenuKeyEvent( usWinNum, iEvent ) );
}

#if 0
 WVW_MENUITEM_SETBITMAPS(
           hMenu,
           nIDEnableItem,
           nPosition,
           ncBitmapUnchecked,
           ncBimapChecked )
#endif
HB_FUNC( WVW_MENUITEM_SETBITMAPS )
{
   HBITMAP hBitmapUnchecked = NULL;
   HBITMAP hBitmapChecked   = NULL;
   char    szResname[ _MAX_PATH + 1 ];
   int     iWidth, iHeight;

   if( ! HB_ISNIL( 4 ) )
   {
      if( HB_ISNUM( 4 ) )
      {
         sprintf( szResname, "?%u", hb_parni( 4 ) );

         hBitmapUnchecked = FindBitmapHandle( szResname, &iWidth, &iHeight );

         if( ! hBitmapUnchecked )
         {
            hBitmapUnchecked = ( HBITMAP ) LoadImage( hb_getWvwData()->hInstance, ( LPCTSTR ) MAKEINTRESOURCE( ( WORD ) hb_parni( 4 ) ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
            AddBitmapHandle( szResname, hBitmapUnchecked, iWidth, iHeight );
         }
      }
      else
      {
         hBitmapUnchecked = FindBitmapHandle( hb_parcx( 4 ), &iWidth, &iHeight );

         if( ! hBitmapUnchecked )
         {
            hBitmapUnchecked = ( HBITMAP ) LoadImage( hb_getWvwData()->hInstance, hb_parcx( 4 ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
            AddBitmapHandle( hb_parcx( 4 ), hBitmapUnchecked, iWidth, iHeight );
         }
      }
   }

   if( ! HB_ISNIL( 5 ) )
   {
      if( HB_ISNUM( 5 ) )
      {
         sprintf( szResname, "?%u", hb_parni( 5 ) );

         hBitmapChecked = FindBitmapHandle( szResname, &iWidth, &iHeight );

         if( ! hBitmapChecked )
         {
            hBitmapChecked = ( HBITMAP ) LoadImage( hb_getWvwData()->hInstance, ( LPCTSTR ) MAKEINTRESOURCE( ( WORD ) hb_parni( 5 ) ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
            AddBitmapHandle( szResname, hBitmapChecked, iWidth, iHeight );
         }
      }
      else
      {
         hBitmapChecked = FindBitmapHandle( hb_parcx( 5 ), &iWidth, &iHeight );

         if( ! hBitmapChecked )
         {
            hBitmapChecked = ( HBITMAP ) LoadImage( hb_getWvwData()->hInstance, hb_parcx( 5 ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
            AddBitmapHandle( hb_parcx( 5 ), hBitmapChecked, iWidth, iHeight );
         }
      }
   }

   if( ! HB_ISNIL( 2 ) )
      SetMenuItemBitmaps( ( HMENU ) HB_PARHANDLE( 1 ), hb_parni( 2 ), MF_BYCOMMAND, ( HBITMAP ) hBitmapUnchecked, ( HBITMAP ) hBitmapChecked );
   else
      SetMenuItemBitmaps( ( HMENU ) HB_PARHANDLE( 1 ), hb_parni( 3 ), MF_BYPOSITION, ( HBITMAP ) hBitmapUnchecked, ( HBITMAP ) hBitmapChecked );
}


HB_FUNC( WVW_DRAWMENUBAR )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   DrawMenuBar( pWindowData->hWnd );
}


HB_FUNC( WVW_ENDMENU )
{
   hb_retl( EndMenu() );
}

/* WVW_GetMenu([nWinNum]) */
HB_FUNC( WVW_GETMENU )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   HB_RETHANDLE( GetMenu( pWindowData->hWnd ) );
}

/* WVW_TrackPopupMenu([nWinNum], n) */
HB_FUNC( WVW_TRACKPOPUPMENU )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   POINT      xy = { 0 };

   GetCursorPos( &xy );

   hb_retnl( TrackPopupMenu( ( HMENU ) HB_PARHANDLE( 2 ),
                             TPM_CENTERALIGN | TPM_RETURNCMD | TPM_RECURSE,
                             xy.x,
                             xy.y,
                             0,
                             pWindowData->hWnd,
                             NULL ) );
}

HB_FUNC( WIN_SETMENU )
{
   SetMenu( ( HWND ) HB_PARHANDLE( 1 ), ( HMENU ) HB_PARHANDLE( 2 ) );
}

/*
 *  WVW_NOSYSMENU( [nWinNum], lRemoveClose )
 *  removes System Menu of a window
 *  if lRemoveClose is .t., also removes the 'Close' command and 'X' button
 *
 * no return value
 */
HB_FUNC( WVW_NOSYSMENU )
{
   UINT       usWinNum     = WVW_WHICH_WINDOW;
   BOOL       lRemoveClose = HB_ISNIL( 2 ) ? FALSE : hb_parl( 2 );
   WIN_DATA * pWindowData  = hb_gt_wvw_GetWindowsData( usWinNum );
   HMENU      hMenu        = GetSystemMenu( pWindowData->hWnd, FALSE );

   if( hMenu )
   {
      DeleteMenu( hMenu, SC_MAXIMIZE, MF_BYCOMMAND );
      DeleteMenu( hMenu, SC_MINIMIZE, MF_BYCOMMAND );
      DeleteMenu( hMenu, SC_SIZE, MF_BYCOMMAND );
      DeleteMenu( hMenu, SC_MOVE, MF_BYCOMMAND );
      DeleteMenu( hMenu, SC_RESTORE, MF_BYCOMMAND );
      DeleteMenu( hMenu, SC_NEXTWINDOW, MF_BYCOMMAND );
      if( lRemoveClose )
      {
         DeleteMenu( hMenu, SC_CLOSE, MF_BYCOMMAND );
         DeleteMenu( hMenu, 0, MF_BYPOSITION );
      }
      DrawMenuBar( pWindowData->hWnd );
   }
}

/* WVW_GetSystemMenu( [nWinNum], lReset )
   *returns the System Menu of a window
   *if lRemoveClose is .t., also removes the 'Close' command and 'X' button
 */
HB_FUNC( WVW_GETSYSTEMMENU )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   BOOL       lReset      = HB_ISNIL( 2 ) ? FALSE : hb_parl( 2 );

   hb_retnl( ( ULONG ) GetSystemMenu( pWindowData->hWnd, lReset ) );
}
