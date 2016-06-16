/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW scrollbar functions
 * GTWVW is initially created based on:
 * =Id: gtwvt.c,v 1.60 2004-01-26 08:14:07 vouchcac Exp =
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

static LRESULT CALLBACK hb_gt_wvw_XBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   HWND hWndParent = GetParent( hWnd );
   int  nWin;

   int     nCtrlId;
   WNDPROC OldProc;

   PWVW_GLO wvw = hb_gt_wvw();
   PWVW_WIN wvw_win;

   if( wvw == NULL || hWndParent == NULL )
      return DefWindowProc( hWnd, message, wParam, lParam );

   if( message == WM_MOUSEACTIVATE )
      wvw->iScrolling = 1;

   for( nWin = 0; nWin < wvw->iNumWindows; nWin++ )
   {
      if( wvw->pWin[ nWin ]->hWnd == hWndParent )
         break;
   }

   if( nWin >= wvw->iNumWindows )
      return DefWindowProc( hWnd, message, wParam, lParam );

   wvw_win = wvw->pWin[ nWin ];

   nCtrlId = ( int ) GetWindowLong( hWnd, GWL_ID );
   if( nCtrlId == 0 )
   {
      hb_errInternal( 10010, "ScrollBar: Control ID not found with hb_gt_wvw_FindControlId()", NULL, NULL );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   OldProc = hb_gt_wvw_GetControlProc( wvw_win, WVW_CONTROL_SCROLLBAR, hWnd );
   if( OldProc == NULL )
   {
      hb_errInternal( 10011, "ScrollBar: Failed hb_gt_wvw_GetControlProc()", NULL, NULL );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   switch( message )
   {
      case WM_LBUTTONUP:

         CallWindowProc( OldProc, hWnd, message, wParam, lParam );
         if( GetCapture() == hWnd )
         {
            ReleaseCapture();

            InvalidateRect( hWnd, NULL, FALSE );
         }
         return 0;

      case WM_RBUTTONDOWN:

         wvw->iScrolling = 0;

         return 0;
      case WM_RBUTTONUP:

         return 0;
   }

   if( message == WM_CAPTURECHANGED )
      wvw->iScrolling = 0;

   return CallWindowProc( OldProc, hWnd, message, wParam, lParam );
}

/* wvw_xbCreate( [nWinNum], nStyle, nTop, nLeft, nLength, bBlock, aOffset)
 * create scroll bar for window nWinNum
 * nStyle: SBS_HORZ (0)=horizontal, SBS_VERT (1)=vertical
 * nTop: row of top/left corner (in character unit)
 * nLeft: col of top/left corner (in character unit)
 * nLength: length of scrollbar (in character unit)
 * NOTES: width of scrollbar (in character unit)
 *            horiz: defaults to one character height
 *            verti: defaults to one character _height_ too (!)
 *       use aOffset to adjust the dimension
 * aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of scroll bar.
 *         defaults for vertical scroll bar: {0,+3,0,0}
 *         defaults for horiz scroll bar: {+3-linespacing,0,0,0}
 *         NOTES: these defaults are meant to make room for other common
 *                GUI elements like raised/recessed lines.
 *
 * bBlock:  codeblock to execute on every WM_VSCROLL/WM_HSCROLL event.
 *         This codeblock will be evaluated with these parameters:
 *         nWinNum: window number
 *         nXBid  : scrollbar id
 *         nXBmsg : scrollbar message, ie. one of these:
 *         nXBpos : scrollthumb position (only if message==SB_THUMB...)
 *         the "must be handled" messages:
 *             SB_LINEUP/SB_LINELEFT     0: up/left button clicked
 *             SB_LINEDOWN/SB_LINERIGHT  1: down/right button clicked
 *             SB_PAGEUP/SB_PAGELEFT     2: upper/left shaft clicked
 *             SB_PAGEDOWN/SB_PAGERIGHT  3: lower/right shaft clicked
 *         the "may not be handled" messages:
 *             SB_THUMBPOSITION          4: scroll thumb is released at position nXBpos
 *             SB_THUMBTRACK             5: scroll thumb is being dragged at position nXBpos
 *             SB_ENDSCROLL              8
 *
 * returns control id of newly created scroll bar of windows nWinNum
 * returns 0 if failed
 *
 * example:
 * wvw_xbCreate( , 1, 10, 70, 12)
 *  :: creates Vertical scrollbar on current window at (10,70) with length 12
 *     dimensions using default ones.
 *     buttons/parts behaviour using default ones.
 *
 * wvw_xbCreate( , 1, 10, 70, 12, {0, +5, 0, +5} )
 *  :: creates Vertical scrollbar on current window at (10,70) with length 12
 *     left and right coordinate is shifted 5 pixels to the right.
 *     buttons/parts behaviour using default ones.
 *
 * NOTES:
 * ScrollRange is always 0 - 100.
 * Initial ScrollPos is 0
 */
HB_FUNC( WVW_XBCREATE )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop  = hb_parni( 3 ),
          iLeft = hb_parni( 4 ),
          iBottom,
          iRight;

      HWND  hWnd;
      POINT xy;

      int iOffTop, iOffLeft, iOffBottom, iOffRight;
      int iStyle = hb_parnidef( 2, -1 );
      int nCtrlId;

      RECT rXB, rOffXB;

      if( iStyle < SBS_HORZ || iStyle > SBS_VERT || ! HB_ISEVALITEM( 6 ) )
      {
         hb_retni( 0 );
         return;
      }

      if( iStyle == SBS_VERT )
      {
         iBottom = iTop + hb_parni( 5 ) - 1;
         iRight  = iLeft;

         iOffTop    = hb_parvni( 7, 1 );
         iOffLeft   = HB_ISARRAY( 7 ) ? hb_parvni( 7, 2 ) : 3;
         iOffBottom = hb_parvni( 7, 3 );
         iOffRight  = hb_parvni( 7, 4 );
      }
      else
      {
         iRight  = iLeft + hb_parni( 5 ) - 1;
         iBottom = iTop;

         iOffTop    = HB_ISARRAY( 7 ) ? hb_parvni( 7, 1 ) : 3 - wvw_win->iLineSpacing;
         iOffLeft   = hb_parvni( 7, 2 );
         iOffBottom = hb_parvni( 7, 3 );
         iOffRight  = hb_parvni( 7, 4 );
      }

      rXB.top    = iTop;
      rXB.left   = iLeft;
      rXB.bottom = iBottom;
      rXB.right  = iRight;

      rOffXB.top    = iOffTop;
      rOffXB.left   = iOffLeft;
      rOffXB.bottom = iOffBottom;
      rOffXB.right  = iOffRight;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );

      if( iStyle == SBS_VERT )
      {
         iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
         iRight  = iLeft + wvw_win->PTEXTSIZE.y - 1 + iOffRight;
      }
      else
      {
         iRight  = xy.x - 1 + iOffRight;
         iBottom = iTop + wvw_win->PTEXTSIZE.y - 1 + iOffBottom;
      }

      nCtrlId = hb_gt_wvw_LastControlId( wvw_win, WVW_CONTROL_SCROLLBAR );
      if( nCtrlId == 0 )
         nCtrlId = WVW_ID_BASE_SCROLLBAR;
      else
         nCtrlId++;

      hWnd = CreateWindowEx(
         0,                                        /* no extended styles */
         TEXT( "SCROLLBAR" ),                      /* scroll bar control class */
         NULL,                                     /* text for window title bar */
         WS_CHILD | WS_VISIBLE | ( DWORD ) iStyle, /* scroll bar styles */
         iLeft,                                    /* horizontal position */
         iTop,                                     /* vertical position */
         iRight - iLeft + 1,                       /* width of the scroll bar */
         iBottom - iTop + 1,                       /* height */
         wvw_win->hWnd,                            /* handle to main window */
         ( HMENU ) ( HB_PTRUINT ) nCtrlId,         /* id for this scroll bar control */
         GetModuleHandle( NULL ),                  /* instance owning this window */
         NULL );                                   /* pointer not needed */

      if( hWnd )
      {
         SetScrollRange( hWnd, SB_CTL, 0, 99, FALSE );
         SetScrollPos( hWnd, SB_CTL, 0, TRUE );

         hb_gt_wvw_AddControlHandle( wvw_win, WVW_CONTROL_SCROLLBAR, hWnd, nCtrlId, hb_param( 6, HB_IT_EVALITEM ), rXB, rOffXB, iStyle );
         hb_gt_wvw_StoreControlProc( wvw_win, WVW_CONTROL_SCROLLBAR, hWnd,
            ( WNDPROC ) SetWindowLongPtr( hWnd, GWLP_WNDPROC, ( LONG_PTR ) hb_gt_wvw_XBProc ) );

         hb_retni( nCtrlId );
         return;
      }
   }

   hb_retni( 0 );
}

/* wvw_xbDestroy( [nWinNum], nXBid )
   destroy scrollbar nXBid for window nWinNum */
HB_FUNC( WVW_XBDESTROY )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int      nCtrlId     = hb_parni( 2 );
      PWVW_CTL wvw_ctl     = wvw_win->ctlList;
      PWVW_CTL wvw_ctlPrev = NULL;

      while( wvw_ctl )
      {
         if( wvw_ctl->nClass == WVW_CONTROL_SCROLLBAR && wvw_ctl->nId == nCtrlId )
            break;

         wvw_ctlPrev = wvw_ctl;
         wvw_ctl     = wvw_ctl->pNext;
      }

      if( wvw_ctl )
      {
         DestroyWindow( wvw_ctl->hWnd );

         if( wvw_ctlPrev )
            wvw_ctlPrev->pNext = wvw_ctl->pNext;
         else
            wvw_win->ctlList = wvw_ctl->pNext;

         if( wvw_ctl->pBlock )
            hb_itemRelease( wvw_ctl->pBlock );

         hb_xfree( wvw_ctl );
      }
   }
}

/* wvw_xbUpdate(nWinNum, XBid, [nPos], [nPageSize], [nMin], [nMax])
 * update scrollbar data and its display
 * nPos, nPageSize, nMin, nMax are optional.
 * however, both nMin & nMax must be supplied, or not at all.
 * returns current position of scroll thumb.
 * returns -1 if update failed.
 */
HB_FUNC( WVW_XBUPDATE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_SCROLLBAR, hb_parni( 2 ), NULL );

   if( hWnd )
   {
      SCROLLINFO si;

      UINT fMask = SIF_DISABLENOSCROLL;

      if( HB_ISNUM( 3 ) )
         fMask |= SIF_POS;
      if( HB_ISNUM( 4 ) )
         fMask |= SIF_PAGE;
      if( HB_ISNUM( 5 ) || HB_ISNUM( 6 ) )
         fMask |= SIF_RANGE;

      memset( &si, 0, sizeof( si ) );

      si.cbSize = sizeof( si );
      si.fMask  = fMask;
      si.nMin   = hb_parni( 5 );
      si.nMax   = hb_parni( 6 );
      si.nPage  = hbwapi_par_UINT( 4 );
      si.nPos   = hb_parni( 3 );

      hb_retni( SetScrollInfo( hWnd, SB_CTL, &si, TRUE ) );
   }
   else
      hb_retni( -1 );
}

/* wvw_xbInfo( [nWinNum], XBid )
   return an array {nMin, nMax, nPageSize, nPos, nTrackPos }
   return an empty array {} if invalid parameter passed. */
HB_FUNC( WVW_XBINFO )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_SCROLLBAR, hb_parni( 2 ), NULL );

   if( hWnd )
   {
      SCROLLINFO si;

      memset( &si, 0, sizeof( si ) );

      si.cbSize = sizeof( si );
      si.fMask  = SIF_ALL;

      if( GetScrollInfo( hWnd, SB_CTL, &si ) )
      {
         PHB_ITEM aInfo = hb_itemArrayNew( 5 );

         hb_arraySetNI( aInfo, 1, si.nMin );
         hb_arraySetNI( aInfo, 2, si.nMax );
         hb_arraySetNInt( aInfo, 3, si.nPage );
         hb_arraySetNI( aInfo, 4, si.nPos );
         hb_arraySetNI( aInfo, 5, si.nTrackPos );

         hb_itemReturnRelease( aInfo );
         return;
      }
   }

   hb_reta( 0 );
}

/* wvw_xbEnable( [nWinNum], nXBid, nFlags )
 *  enable/disable scrollbar nXBid in window nWinNum (default to topmost window)
 *  nFlags: ESB_ENABLE_BOTH                    0: enable both arrows
 *        ESB_DISABLE_LEFT/ESB_DISABLE_UP    1: disable left/up arrow
 *        ESB_DISABLE_RIGHT/ESB_DISABLE_DOWN 2: disable right/down arrow
 *        ESB_DISABLE_BOTH                   3: disable both arrow
 * returns .T. if successful
 */
HB_FUNC( WVW_XBENABLE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd    = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_SCROLLBAR, hb_parni( 2 ), NULL );
   UINT uiFlags = hbwapi_par_UINT( 3 );

   hb_retl( hWnd && uiFlags <= ESB_DISABLE_BOTH && EnableScrollBar( hWnd, SB_CTL, uiFlags ) );
}

/* wvw_xbVisible( [nWinNum], nXBid, lShow )
 *  show/hide scrollbar nXBid in window nWinNum (default to topmost window)
 *  nXBid is the handle of the scrollbar
 *  lShow: .T. shows the scrollbar (default)
 *         .F. hides the scrollbar
 * returns .T. if successful
 */
HB_FUNC( WVW_XBVISIBLE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_SCROLLBAR, hb_parni( 2 ), NULL );

   hb_retl( hWnd && ShowScrollBar( hWnd, SB_CTL, ( BOOL ) hb_parldef( 3, HB_TRUE ) ) );
}
