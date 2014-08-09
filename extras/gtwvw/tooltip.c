/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw toolbar and tooltips functions
 * GTWVW is initially created based on:
 * =Id: gtwvt.c,v 1.60 2004-01-26 08:14:07 vouchcac Exp =
 *
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
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_wvw_Tone()
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

#if 0

/* WVW_SetToolTopActive([nWinNum], [lToggle]) */
HB_FUNC( WVW_SETTOOLTIPACTIVE )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   hb_retl( pWindowData->bToolTipActive );

   if( HB_ISLOG( 2 ) )
   {
      if( hb_parl( 2 ) && pWindowData->hWndTT == NULL )
         hb_gt_wvwCreateToolTipWindow( pWindowData );

      pWindowData->bToolTipActive = hb_parl( 2 );
   }
}

/* Wvw_SetToolTip( [nWinNum], nTop, nLeft, nBottom, nRight, cToolText ) */
HB_FUNC( WVW_SETTOOLTIP )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   if( pWindowData->bToolTipActive )
   {
      TOOLINFO ti = { 0 };
      POINT    xy = { 0 };
      int      iTop, iLeft, iBottom, iRight;

      USHORT usTop    = hb_parni( 2 ),
             usLeft   = hb_parni( 3 ),
             usBottom = hb_parni( 4 ),
             usRight  = hb_parni( 5 );

      if( hb_gt_wvw_GetWvwData()->bMainCoordMode )
         hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

      ti.cbSize = sizeof( TOOLINFO );
      ti.hwnd   = pWindowData->hWnd;
      ti.uId    = WVW_ID_BASE_TOOLTIP + usWinNum;

      if( SendMessage( pWindowData->hWndTT, TTM_GETTOOLINFO, 0, ( LPARAM ) &ti ) )
      {
         xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
         iTop  = xy.y;
         iLeft = xy.x;

         xy      = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );
         iBottom = xy.y - 1;
         iRight  = xy.x - 1;

         ti.lpszText    = hb_parc( 6 );
         ti.rect.left   = iLeft;
         ti.rect.top    = iTop;
         ti.rect.right  = iRight;
         ti.rect.bottom = iBottom;

         SendMessage( pWindowData->hWndTT, TTM_SETTOOLINFO, 0, ( LPARAM ) &ti );
      }
   }
}


HB_FUNC( WVW_SETTOOLTIPTEXT )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   TOOLINFO   ti;

   ti.cbSize = sizeof( TOOLINFO );
   ti.hwnd   = pWindowData->hWnd;
   ti.uId    = 100000;

   if( SendMessage( pWindowData->hWndTT, TTM_GETTOOLINFO, 0, ( LPARAM ) &ti ) )
   {
      ti.lpszText = hb_parcx( 2 );
      SendMessage( pWindowData->hWndTT, TTM_UPDATETIPTEXT, 0, ( LPARAM ) &ti );
   }
}

HB_FUNC( WVW_SETTOOLTIPMARGIN )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   RECT       rc;

   rc.left   = hb_parni( 3 );
   rc.top    = hb_parni( 2 );
   rc.right  = hb_parni( 5 );
   rc.bottom = hb_parni( 4 );

   SendMessage( pWindowData->hWndTT, TTM_SETMARGIN, 0, ( LPARAM ) &rc );
}

HB_FUNC( WVW_SETTOOLTIPWIDTH )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   hb_retni( ( int ) SendMessage( pWindowData->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0 ) );

   if( HB_ISNUM( 2 ) )
      SendMessage( pWindowData->hWndTT, TTM_SETMAXTIPWIDTH, 0, ( LPARAM ) ( int ) hb_parni( 2 ) );
}

HB_FUNC( WVW_SETTOOLTIPBKCOLOR )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   hb_retnl( ( COLORREF ) SendMessage( pWindowData->hWndTT, TTM_GETTIPBKCOLOR, 0, 0 ) );

   if( HB_ISNUM( 2 ) )
      SendMessage( pWindowData->hWndTT, TTM_SETTIPBKCOLOR, ( WPARAM ) ( COLORREF ) hb_parnl( 2 ), 0 );
}

HB_FUNC( WVW_SETTOOLTIPTEXTCOLOR )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   hb_retnl( ( COLORREF ) SendMessage( pWindowData->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0 ) );

   if( HB_ISNUM( 2 ) )
      SendMessage( pWindowData->hWndTT, TTM_SETTIPTEXTCOLOR, ( WPARAM ) ( COLORREF ) hb_parnl( 2 ), 0 );
}

HB_FUNC( WVW_SETTOOLTIPTITLE )
{
   if( HB_ISCHAR( 3 ) )
   {
      WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
      int        iIcon       = hb_parni( 2 );

      if( iIcon > 3 )
         iIcon = 0;
      SendMessage( pWindowData->hWndTT, TTM_SETTITLE, ( WPARAM ) iIcon, ( LPARAM ) hb_parc( 3 ) );
   }
}

HB_FUNC( WVW_GETTOOLTIPWIDTH )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   hb_retni( SendMessage( pWindowData->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0 ) );
}

HB_FUNC( WVW_GETTOOLTIPBKCOLOR )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   hb_retnl( ( COLORREF ) SendMessage( pWindowData->hWndTT, TTM_GETTIPBKCOLOR, 0, 0 ) );
}

HB_FUNC( WVW_GETTOOLTIPTEXTCOLOR )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   hb_retnl( ( COLORREF ) SendMessage( pWindowData->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0 ) );
}

#endif
