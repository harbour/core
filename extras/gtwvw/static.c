/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw static control functions
 * GTWVW is initially created based on:
 * =Id: gtwvt.c,v 1.60 2004-01-26 08:14:07 vouchcac Exp =
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/ ).
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

HB_FUNC( WVW_STCREATE )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   HB_UINT    nWin    = WVW_WHICH_WINDOW;
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   HANDLE hInstance  = NULL;
   HWND   hWndParent = wvw_win->hWnd;
   HWND   hWnd;

   POINT   xy;
   int     iTop, iLeft, iBottom, iRight;
   int     iOffTop, iOffLeft, iOffBottom, iOffRight;
   HB_BOOL bBorder = hb_parl( 7 );

   HB_UINT nCtrlId;

   USHORT usWidth  = ( USHORT ) hb_parni( 4 );
   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parnidef( 11, usTop ),
          usRight  = ( USHORT ) hb_parnidef( 12, usLeft + usWidth - 1 );

   int   iStyle = bBorder ? WS_BORDER : 0;
   int   iBox   = hb_parni( 10 );
   HFONT hFont  = NULL;

   if( iBox > 0 )
      iStyle |= iBox;

   if( HB_ISNUM( 8 ) )
      hFont = ( HFONT ) HB_PARHANDLE( 8 );
   else if( wvw_win->hSTfont == NULL )
   {
      wvw_win->hSTfont = CreateFontIndirect( &wvw->lfST );
      if( wvw_win->hSTfont == NULL )
      {
         HB_STOREHANDLE( NULL, 9 );
         hb_retnl( 0 );
         return;
      }
   }

   iOffTop    = HB_ISARRAY( 6 ) ? hb_parvni( 6, 1 ) : 0;
   iOffLeft   = HB_ISARRAY( 6 ) ? hb_parvni( 6, 2 ) : 0;
   iOffBottom = HB_ISARRAY( 6 ) ? hb_parvni( 6, 3 ) : 0;
   iOffRight  = HB_ISARRAY( 6 ) ? hb_parvni( 6, 4 ) : 0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( nWin, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

   xy.y -= wvw_win->iLineSpacing;

   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   nCtrlId = hb_gt_wvw_LastControlId( nWin, WVW_CONTROL_STATIC );
   if( nCtrlId == 0 )
      nCtrlId = WVW_ID_BASE_STATIC;
   else
      nCtrlId++;

   hb_winmainArgGet( &hInstance, NULL, NULL );

   hWnd = CreateWindowEx(
      bBorder ? WS_EX_CLIENTEDGE : 0,
      TEXT( "STATIC" ),
      NULL,
      WS_CHILD | WS_VISIBLE | ( DWORD ) iStyle,
      iLeft,
      iTop,
      iRight - iLeft + 1,
      iBottom - iTop + 1,
      hWndParent,
      ( HMENU ) ( HB_PTRDIFF ) nCtrlId,
      ( HINSTANCE ) hInstance,
      NULL );

   if( hWnd )
   {
      if( HB_ISCHAR( 5 ) )
      {
         void * hText;
         SendMessage( hWnd, WM_SETTEXT, 0, ( LPARAM ) HB_PARSTR( 5, &hText, NULL ) );
         hb_strfree( hText );
      }

      if( hFont )
         SendMessage( hWnd, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );
      else
         SendMessage( hWnd, WM_SETFONT, ( WPARAM ) wvw_win->hSTfont, ( LPARAM ) TRUE );

      HB_STOREHANDLE( hWnd, 9 );
      hb_retnl( nCtrlId );
   }
   else
   {
      HB_STOREHANDLE( NULL, 9 );
      hb_retnl( 0 );
   }
}


HB_FUNC( WVW_STSETTEXT )
{
   HWND hWnd = ( HWND ) HB_PARHANDLE( 2 );

   if( hWnd )
   {
      void * hText;
      SetWindowText( ( HWND ) hWnd, HB_PARSTR( 3, &hText, NULL ) );
      hb_strfree( hText );
      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}


HB_FUNC( WVW_STSETFONT )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   HB_BOOL fResult = HB_TRUE;

   wvw->lfST.lfHeight         = hb_parnldef( 3, wvw_win->fontHeight - 2 );
   wvw->lfST.lfWidth          = hb_parnldef( 4, wvw->lfST.lfWidth );
   wvw->lfST.lfEscapement     = 0;
   wvw->lfST.lfOrientation    = 0;
   wvw->lfST.lfWeight         = hb_parnldef( 5, wvw->lfST.lfWeight );
   wvw->lfST.lfQuality        = ( BYTE ) hb_parnidef( 6, wvw->lfST.lfQuality );
   wvw->lfST.lfItalic         = ( BYTE ) hb_parldef( 7, wvw->lfST.lfItalic );
   wvw->lfST.lfUnderline      = ( BYTE ) hb_parldef( 8, wvw->lfST.lfUnderline );
   wvw->lfST.lfStrikeOut      = ( BYTE ) hb_parldef( 9, wvw->lfST.lfStrikeOut );
   wvw->lfST.lfCharSet        = DEFAULT_CHARSET;
   wvw->lfST.lfPitchAndFamily = FF_DONTCARE;

   if( HB_ISCHAR( 2 ) )
   {
      HB_ITEMCOPYSTR( hb_param( 2, HB_IT_STRING ), wvw->lfST.lfFaceName, HB_SIZEOFARRAY( wvw->lfST.lfFaceName ) );
      wvw_win->fontFace[ HB_SIZEOFARRAY( wvw->lfST.lfFaceName ) - 1 ] = TEXT( '\0' );
   }

   if( wvw_win->hSTfont )
   {
      HFONT hOldFont = wvw_win->hSTfont;
      HFONT hFont    = CreateFontIndirect( &wvw->lfST );
      if( hFont )
      {
         WVW_CTRL * pcd = wvw_win->pcdList;

         while( pcd )
         {
            if( pcd->nClass == WVW_CONTROL_STATIC &&
                ( HFONT ) SendMessage( pcd->hWnd, WM_GETFONT, 0, 0 ) == hOldFont )
               SendMessage( pcd->hWnd, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );

            pcd = pcd->pNext;
         }

         wvw_win->hSTfont = hFont;
         DeleteObject( hOldFont );
      }
      else
         fResult = HB_FALSE;
   }

   hb_retl( fResult );
}
