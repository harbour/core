/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw edit functions
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
   WVW_GLOB * wvw      = hb_gt_wvw_GetWvwData();
   UINT       usWinNum = WVW_WHICH_WINDOW;
   WVW_WIN *  wvw_win  = hb_gt_wvw_GetWindowsData( usWinNum );

   HANDLE hInstance  = NULL;
   HWND   hWndParent = wvw_win->hWnd;
   HWND   hWndCB;

   POINT   xy;
   int     iTop, iLeft, iBottom, iRight;
   int     iOffTop, iOffLeft, iOffBottom, iOffRight;
   HB_BOOL bBorder   = hb_parl( 7 );
   ULONG   ulExStyle = bBorder ? WS_EX_CLIENTEDGE : 0;

   UINT uiCBid;

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
         hb_retnl( 0 );
         return;
      }
   }

   iOffTop    = HB_ISARRAY( 6 ) ? hb_parvni( 6, 1 ) : 0;
   iOffLeft   = HB_ISARRAY( 6 ) ? hb_parvni( 6, 2 ) : 0;
   iOffBottom = HB_ISARRAY( 6 ) ? hb_parvni( 6, 3 ) : 0;
   iOffRight  = HB_ISARRAY( 6 ) ? hb_parvni( 6, 4 ) : 0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

   xy.y -= wvw_win->byLineSpacing;

   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   uiCBid = hb_gt_wvw_LastControlId( usWinNum, WVW_CONTROL_STATIC );
   if( uiCBid == 0 )
      uiCBid = WVW_ID_BASE_STATIC;
   else
      uiCBid++;

   hb_winmainArgGet( &hInstance, NULL, NULL );

   hWndCB = CreateWindowEx(
      ulExStyle,
      "STATIC",
      NULL,
      WS_CHILD | WS_VISIBLE | ( DWORD ) iStyle,
      iLeft,
      iTop,
      iRight - iLeft + 1,
      iBottom - iTop + 1,
      hWndParent,
      ( HMENU ) ( HB_PTRDIFF ) uiCBid,
      ( HINSTANCE ) hInstance,
      NULL );

   if( hWndCB )
   {
      if( HB_ISCHAR( 5 ) )
         SendMessage( hWndCB, WM_SETTEXT, 0, ( LPARAM ) hb_parc( 5 ) );
      if( hFont )
         SendMessage( hWndCB, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );
      else
         SendMessage( hWndCB, WM_SETFONT, ( WPARAM ) wvw_win->hSTfont, ( LPARAM ) TRUE );
      hb_retnl( uiCBid );
      HB_STOREHANDLE( hWndCB, 9 );
   }
   else
      hb_retnl( 0 );
}


HB_FUNC( WVW_STSETTEXT )
{
   HWND hWndCB = ( HWND ) HB_PARHANDLE( 2 );

   if( hWndCB )
   {
      SetWindowText( ( HWND ) hWndCB, ( LPCTSTR ) hb_parc( 3 ) );
      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}


HB_FUNC( WVW_STSETFONT )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   HB_BOOL retval = HB_TRUE;

   wvw->lfST.lfHeight      = hb_parnldef( 3, wvw_win->fontHeight - 2 );
   wvw->lfST.lfWidth       = HB_ISNUM( 4 ) ? hb_parni( 4 ) : wvw->lfST.lfWidth;
   wvw->lfST.lfEscapement  = 0;
   wvw->lfST.lfOrientation = 0;
   wvw->lfST.lfWeight      = HB_ISNUM( 5 ) ? hb_parni( 5 ) : wvw->lfST.lfWeight;
   wvw->lfST.lfItalic      = HB_ISLOG( 7 ) ? ( BYTE ) hb_parl( 7 ) : wvw->lfST.lfItalic;
   wvw->lfST.lfUnderline   = HB_ISLOG( 8 ) ? ( BYTE ) hb_parl( 8 ) : wvw->lfST.lfUnderline;
   wvw->lfST.lfStrikeOut   = HB_ISLOG( 9 ) ? ( BYTE ) hb_parl( 9 ) : wvw->lfST.lfStrikeOut;
   wvw->lfST.lfCharSet     = DEFAULT_CHARSET;

   wvw->lfST.lfQuality        = HB_ISNUM( 6 ) ? ( BYTE ) hb_parni( 6 ) : wvw->lfST.lfQuality;
   wvw->lfST.lfPitchAndFamily = FF_DONTCARE;
   if( HB_ISCHAR( 2 ) )
      hb_strncpy( wvw->lfST.lfFaceName, hb_parc( 2 ), sizeof( wvw->lfPB.lfFaceName ) - 1 );

   if( wvw_win->hSTfont )
   {
      HFONT hOldFont = wvw_win->hSTfont;
      HFONT hFont    = CreateFontIndirect( &wvw->lfST );
      if( hFont )
      {
         WVW_CTRL * pcd = wvw_win->pcdCtrlList;

         while( pcd )
         {
            if( pcd->byCtrlClass == WVW_CONTROL_STATIC &&
                ( HFONT ) SendMessage( pcd->hWndCtrl, WM_GETFONT, 0, 0 ) == hOldFont )
               SendMessage( pcd->hWndCtrl, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );

            pcd = pcd->pNext;
         }

         wvw_win->hSTfont = hFont;
         DeleteObject( hOldFont );
      }
      else
         retval = HB_FALSE;
   }

   hb_retl( retval );
}
