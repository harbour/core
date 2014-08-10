/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw statusbar/scrollbar  functions
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

/* STATUS BAR */

/* wvw_sbCreate( [nWinNum] )
 * create status bar for window nWinNum, with one part.
 * returns handle to status bar of windows nWinNum
 * returns 0 if failed, eg. if there is already a status bar for this window
 */
HB_FUNC( WVW_SBCREATE )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   HWND       hWndParent;
   HWND       hWndSB;
   WVW_DATA * pData = hb_gt_wvw_GetWvwData();
   int        ptArray[ WVW_MAX_STATUS_PARTS ];

   if( pWindowData->hStatusBar != NULL )
   {
      hb_retnl( 0 );
      return;
   }

   hWndParent = pWindowData->hWnd;
   hWndSB     = CreateStatusWindow( WS_CHILD | WS_VISIBLE | WS_BORDER | SBT_TOOLTIPS,
                                    NULL,
                                    hWndParent,
                                    WVW_ID_BASE_STATUSBAR + usWinNum );
   if( hWndSB )
   {
      RECT rSB = { 0 };
      if( pWindowData->hSBfont == NULL )
         pWindowData->hSBfont = CreateFontIndirect( &pData->lfSB );
      if( GetClientRect( hWndSB, &rSB ) )
         pWindowData->usSBHeight = ( USHORT ) rSB.bottom;
      pWindowData->hStatusBar = hWndSB;

      hb_gt_wvw_ResetWindow( usWinNum );

      ptArray[ 0 ] = rSB.right;
      SendMessage( hWndSB, WM_SETFONT, ( WPARAM ) pWindowData->hSBfont, ( LPARAM ) TRUE );

      SendMessage( hWndSB, SB_SETPARTS, 1, ( LPARAM ) ( LPINT ) ptArray );
   }

   hb_retnint( ( HB_PTRDIFF ) hWndSB );
}

/* wvw_sbDestroy( [nWinNum] )
 * destroy status bar for window nWinNum
 */
HB_FUNC( WVW_SBDESTROY )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   if( pWindowData->hStatusBar != NULL )
   {
      if( pWindowData->hSBfont )
      {
         DeleteObject( pWindowData->hSBfont );
         pWindowData->hSBfont = NULL;
      }
      DestroyWindow( pWindowData->hStatusBar );
      pWindowData->hStatusBar = NULL;
      pWindowData->bSBPaint   = FALSE;
      pWindowData->usSBHeight = 0;

      hb_gt_wvw_ResetWindow( usWinNum );
   }
}

/* wvw_sbAddPart(nWinNum, cMaxText, nWidth, nStyle, lResetParts, [cIcon , cToolTip])
 *  ps.
 *  lResetParts==.t. :: remove all previously created parts
 *  nStyle: 0 (default), 0x0200 (SBT_POPOUT), 0x0100 (SBT_NOBORDERS)
 *  nWidth: expected width in pixels
 *  NOTE: if cMaxText is passed, nWidth is ignored. width of cMaxText will be used instead
 *  NOTE: the leftmost part will eventually have width of remaining spaces
 *  NOTE: cIcon and cToolTip does not work currently
 *
 * returns number of parts
 * returns 0 if failed
 */
HB_FUNC( WVW_SBADDPART )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   HWND       hWndSB;
   int        ptArray[ WVW_MAX_STATUS_PARTS ];
   int        numOfParts;
   int        n;
   RECT       rSB = { 0 };
   WORD       displayFlags;
   HICON      hIcon;
   BOOL       lResetParts;
   USHORT     usWidth;

   hWndSB = pWindowData->hStatusBar;
   if( hWndSB == NULL )
   {
      hb_retnl( 0 );
      return;
   }

   displayFlags = ( WORD ) hb_parnl( 4 );
   lResetParts  = hb_parl( 5 );
   usWidth      = hb_parni( 3 ) <= 0 ? 5 * WVW_SPACE_BETWEEN_PARTS : ( USHORT ) hb_parni( 3 );

   if( HB_ISCHAR( 2 ) )
   {
      HDC  hDCSB = GetDC( hWndSB );
      SIZE size  = { 0 };

      HFONT hFont    = ( HFONT ) SendMessage( hWndSB, WM_GETFONT, 0, 0 );
      HFONT hOldFont = ( HFONT ) SelectObject( hDCSB, hFont );

      if( GetTextExtentPoint32( hDCSB, hb_parc( 2 ), ( int ) hb_parclen( 2 ) + 1, &size ) )
         usWidth = ( USHORT ) size.cx;

      SelectObject( hDCSB, hOldFont );

      ReleaseDC( hWndSB, hDCSB );
   }

   if( ! lResetParts )
      numOfParts = ( int ) SendMessage( hWndSB, SB_GETPARTS, WVW_MAX_STATUS_PARTS, ( LPARAM ) ( LPINT ) ptArray );
   else
      numOfParts = 0;
   numOfParts++;

   GetClientRect( hWndSB, &rSB );

   ptArray[ numOfParts - 1 ] = rSB.right;
   if( ! lResetParts )
      for( n = 0; n < numOfParts - 1; n++ )
         ptArray[ n ] -= ( usWidth + WVW_SPACE_BETWEEN_PARTS );

   SendMessage( hWndSB, SB_SETPARTS, numOfParts, ( LPARAM ) ( LPINT ) ptArray );

   if( HB_ISCHAR( 6 ) )
   {
      int cy = rSB.bottom - rSB.top - 4;
      int cx = cy;

      hIcon = ( HICON ) LoadImage( 0, hb_parc( 6 ), IMAGE_ICON, cx, cy, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT | LR_DEFAULTSIZE );

      if( hIcon == NULL )
         hIcon = ( HICON ) LoadImage( GetModuleHandle( NULL ), hb_parc( 6 ), IMAGE_ICON, cx, cy, LR_DEFAULTCOLOR | LR_DEFAULTSIZE );

      if( hIcon != NULL )
         SendMessage( hWndSB, SB_SETICON, ( WPARAM ) numOfParts - 1, ( LPARAM ) hIcon );
   }

   SendMessage( hWndSB, SB_SETTEXT, ( numOfParts - 1 ) | displayFlags, 0 );
   if( HB_ISCHAR( 7 ) )
      SendMessage( hWndSB, SB_SETTIPTEXT, ( WPARAM ) ( numOfParts - 1 ), ( LPARAM ) hb_parc( 7 ) );

   hb_retni( numOfParts );
}

/* wvw_sbRefresh(nWinNum)
 * reinitialize StatusBar's parts, eg. after window resize
 * TODO: do it automatically, after hb_gt_wvw_ResetWindowSize()
 * returns number of parts
 * returns 0 if failed
 */
HB_FUNC( WVW_SBREFRESH )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   HWND       hWndSB;
   int        ptArray[ WVW_MAX_STATUS_PARTS ];
   int        numOfParts;
   int        n;
   int        iDiff;
   RECT       rSB = { 0 };

   hWndSB = pWindowData->hStatusBar;
   if( hWndSB == NULL )
   {
      hb_retnl( 0 );
      return;
   }

   numOfParts = ( int ) SendMessage( hWndSB, SB_GETPARTS, WVW_MAX_STATUS_PARTS, ( LPARAM ) ( LPINT ) ptArray );
   if( numOfParts == 0 )
   {
      hb_retnl( 0 );
      return;
   }

   GetClientRect( hWndSB, &rSB );
   iDiff = rSB.right - ptArray[ numOfParts - 1 ];

   for( n = 0; n <= numOfParts - 1; n++ )
      ptArray[ n ] += iDiff;

   SendMessage( hWndSB, SB_SETPARTS, numOfParts, ( LPARAM ) ( LPINT ) ptArray );

   hb_retni( numOfParts );
}

/* wvw_sbSetText([nWinNum], [nPart], cText)
 * Set Text of status bar's part #npart
 */
HB_FUNC( WVW_SBSETTEXT )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   int        iPart       = hb_parnidef( 2, 1 );

   if( HB_ISCHAR( 4 ) )
      pWindowData->cSBColorForeground = strtol( hb_parc( 4 ), NULL, 10 );
   else if( HB_ISNUM( 4 ) )
      pWindowData->cSBColorForeground = hb_parnl( 4 );

   if( HB_ISCHAR( 5 ) )
      pWindowData->cSBColorBackground = strtol( hb_parc( 5 ), NULL, 10 );
   else if( HB_ISNUM( 5 ) )
      pWindowData->cSBColorBackground = hb_parnl( 5 );

   if( iPart == 0 && ( pWindowData->cSBColorForeground || pWindowData->cSBColorBackground ) )
   {
      pWindowData->bSBPaint = TRUE;
      SendMessage( pWindowData->hStatusBar, SB_SETTEXT, SBT_OWNERDRAW, ( LPARAM ) hb_parcx( 3 ) );
      hb_gt_wvw_ProcessMessages( pWindowData );
   }
   else
      SendMessage( pWindowData->hStatusBar, SB_SETTEXT, iPart, ( LPARAM ) hb_parcx( 3 ) );
}

/* wvw_sbGetText([nWinNum], [nPart])
 * Get Text of status bar's part #npart
 */
HB_FUNC( WVW_SBGETTEXT )
{
   WIN_DATA * pWindowData     = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   int        iPart           = hb_parnidef( 2, 1 );
   char       cString[ 1024 ] = "";

   SendMessage( pWindowData->hStatusBar, SB_GETTEXT, ( WPARAM ) iPart, ( LPARAM ) cString );
   hb_retc( cString );
}

/* wvw_sbGetParts([nWinNum])
 * Get number of parts in statusbar of window nWinNum
 */
HB_FUNC( WVW_SBGETPARTS )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   int        numOfParts  = ( int ) SendMessage( pWindowData->hStatusBar, SB_GETPARTS, WVW_MAX_STATUS_PARTS, 0 );

   hb_retni( numOfParts );
}

/* wvw_sbSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality,;
 *                             lItalic, lUnderline, lStrikeout
 */
HB_FUNC( WVW_SBSETFONT )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   WVW_DATA * pData       = hb_gt_wvw_GetWvwData();

   HB_BOOL retval = HB_TRUE;

   pData->lfSB.lfHeight      = HB_ISNUM( 3 ) ? hb_parnl( 3 ) : pWindowData->fontHeight - 2;
   pData->lfSB.lfWidth       = HB_ISNUM( 4 ) ? hb_parni( 4 ) : pData->lfSB.lfWidth;
   pData->lfSB.lfEscapement  = 0;
   pData->lfSB.lfOrientation = 0;
   pData->lfSB.lfWeight      = HB_ISNUM( 5 ) ? hb_parni( 5 ) : pData->lfSB.lfWeight;
   pData->lfSB.lfItalic      = HB_ISLOG( 7 ) ? ( BYTE ) hb_parl( 7 ) : pData->lfSB.lfItalic;
   pData->lfSB.lfUnderline   = HB_ISLOG( 8 ) ? ( BYTE ) hb_parl( 8 ) : pData->lfSB.lfUnderline;
   pData->lfSB.lfStrikeOut   = HB_ISLOG( 9 ) ? ( BYTE ) hb_parl( 9 ) : pData->lfSB.lfStrikeOut;
   pData->lfSB.lfCharSet     = DEFAULT_CHARSET;

   pData->lfSB.lfQuality        = HB_ISNUM( 6 ) ? ( BYTE ) hb_parni( 6 ) : pData->lfSB.lfQuality;
   pData->lfSB.lfPitchAndFamily = FF_DONTCARE;
   if( HB_ISCHAR( 2 ) )
      hb_strncpy( pData->lfSB.lfFaceName, hb_parc( 2 ), sizeof( pData->lfSB.lfFaceName ) - 1 );

   if( pWindowData->hSBfont )
   {
      HFONT hOldFont = pWindowData->hSBfont;
      HFONT hFont    = CreateFontIndirect( &pData->lfSB );
      if( hFont )
      {
         pWindowData->hSBfont = hFont;
         DeleteObject( hOldFont );
      }
      else
         retval = HB_FALSE;
   }

   hb_retl( retval );
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
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   HWND       hWndParent  = pWindowData->hWnd;
   HWND       hWndXB;
   POINT      xy;
   int        iTop, iLeft, iBottom, iRight;
   int        iOffTop, iOffLeft, iOffBottom, iOffRight;
   int        iStyle = ( int ) hb_parnidef( 2, -1 );
   UINT       uiXBid;
   USHORT     usTop  = ( USHORT ) hb_parni( 3 ),
              usLeft = ( USHORT ) hb_parni( 4 ),
              usBottom,
              usRight;

   if( iStyle < SBS_HORZ || iStyle > SBS_VERT || ! HB_ISEVALITEM( 6 ) )
   {
      hb_retnl( 0 );
      return;
   }

   if( iStyle == SBS_VERT )
   {
      usBottom = usTop + ( USHORT ) hb_parni( 5 ) - 1;
      usRight  = usLeft;

      iOffTop    = HB_ISARRAY( 7 ) ? hb_parvni( 7, 1 ) : 0;
      iOffLeft   = HB_ISARRAY( 7 ) ? hb_parvni( 7, 2 ) : 3;
      iOffBottom = HB_ISARRAY( 7 ) ? hb_parvni( 7, 3 ) : 0;
      iOffRight  = HB_ISARRAY( 7 ) ? hb_parvni( 7, 4 ) : 0;
   }
   else
   {
      usRight  = usLeft + ( USHORT ) hb_parni( 5 ) - 1;
      usBottom = usTop;

      iOffTop    = HB_ISARRAY( 7 ) ? hb_parvni( 7, 1 ) : 3 - pWindowData->byLineSpacing;
      iOffLeft   = HB_ISARRAY( 7 ) ? hb_parvni( 7, 2 ) : 0;
      iOffBottom = HB_ISARRAY( 7 ) ? hb_parvni( 7, 3 ) : 0;
      iOffRight  = HB_ISARRAY( 7 ) ? hb_parvni( 7, 4 ) : 0;
   }

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   if( iStyle == SBS_VERT )
   {
      iBottom = xy.y - 1 + iOffBottom;
      iRight  = iLeft + pWindowData->PTEXTSIZE.y - 1 + iOffRight;
   }
   else
   {
      iRight  = xy.x - 1 + iOffRight;
      iBottom = iTop + pWindowData->PTEXTSIZE.y - 1 + iOffBottom;
   }

   uiXBid = hb_gt_wvw_LastControlId( usWinNum, WVW_CONTROL_SCROLLBAR );
   if( uiXBid == 0 )
      uiXBid = WVW_ID_BASE_SCROLLBAR;
   else
      uiXBid++;

   hWndXB = CreateWindowEx(
      0L,                                       /* no extended styles */
      "SCROLLBAR",                              /* scroll bar control class */
      NULL,                                     /* text for window title bar */
      WS_CHILD | WS_VISIBLE | ( DWORD ) iStyle, /* scroll bar styles */
      iLeft,                                    /* horizontal position */
      iTop,                                     /* vertical position */
      iRight - iLeft + 1,                       /* width of the scroll bar */
      iBottom - iTop + 1,                       /* height */
      hWndParent,                               /* handle to main window */
      ( HMENU ) ( HB_PTRDIFF ) uiXBid,          /* id for this scroll bar control */
      hb_gt_wvw_GetWvwData()->hInstance,               /* instance owning this window */
      NULL );                                   /* pointer not needed */

   if( hWndXB )
   {
      RECT rXB, rOffXB;

      WNDPROC OldProc;

      rXB.top    = usTop;
      rXB.left   = usLeft;
      rXB.bottom = usBottom;
      rXB.right  = usRight;

      rOffXB.top    = iOffTop;
      rOffXB.left   = iOffLeft;
      rOffXB.bottom = iOffBottom;
      rOffXB.right  = iOffRight;

      SetScrollRange( hWndXB, SB_CTL, 0, 99, FALSE );
      SetScrollPos( hWndXB, SB_CTL, 0, TRUE );

      hb_gt_wvw_AddControlHandle( usWinNum, WVW_CONTROL_SCROLLBAR, hWndXB, uiXBid, hb_param( 6, HB_IT_EVALITEM ), rXB, rOffXB, ( byte ) iStyle );

      OldProc = ( WNDPROC ) SetWindowLongPtr( hWndXB, GWLP_WNDPROC, ( LONG_PTR ) hb_gt_wvw_XBProc );

      hb_gt_wvw_StoreControlProc( usWinNum, WVW_CONTROL_SCROLLBAR, hWndXB, OldProc );

      hb_retnl( uiXBid );
   }
   else
      hb_retnl( 0 );
}

/* wvw_xbDestroy( [nWinNum], nXBid )
 * destroy scrollbar nXBid for window nWinNum
 */
HB_FUNC( WVW_XBDESTROY )
{
   WIN_DATA *     pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   UINT           uiXBid      = ( UINT ) hb_parnl( 2 );
   CONTROL_DATA * pcd         = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev     = NULL;

   while( pcd )
   {
      if( pcd->byCtrlClass == WVW_CONTROL_SCROLLBAR && pcd->uiCtrlid == uiXBid )
         break;

      pcdPrev = pcd;
      pcd     = pcd->pNext;
   }

   if( pcd )
   {
      DestroyWindow( pcd->hWndCtrl );

      if( pcdPrev )
         pcdPrev->pNext = pcd->pNext;
      else
         pWindowData->pcdCtrlList = pcd->pNext;

      if( pcd->phiCodeBlock )
         hb_itemRelease( pcd->phiCodeBlock );

      hb_xfree( pcd );
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
   byte bStyle;
   HWND hWndXB = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_SCROLLBAR, ( UINT ) hb_parnl( 2 ), &bStyle );
   int  iPage  = hb_parni( 4 );

   if( hWndXB && iPage >= 0 )
   {
      SCROLLINFO si;
      UINT fMask = SIF_DISABLENOSCROLL;

      if( HB_ISNUM( 3 ) )
         fMask |= SIF_POS;
      if( HB_ISNUM( 4 ) )
         fMask |= SIF_PAGE;
      if( HB_ISNUM( 5 ) || HB_ISNUM( 6 ) )
         fMask |= SIF_RANGE;

      si.cbSize = sizeof( si );
      si.fMask  = fMask;
      si.nMin   = hb_parni( 5 );
      si.nMax   = hb_parni( 6 );
      si.nPage  = ( UINT ) iPage;
      si.nPos   = hb_parni( 3 );

      hb_retni( SetScrollInfo( hWndXB, SB_CTL, &si, TRUE ) );
   }
   else
      hb_retni( -1 );
}

/* wvw_xbInfo( [nWinNum], XBid )
 * return an array {nMin, nMax, nPageSize, nPos, nTrackPos }
 * return an empty array {} if invalid parameter passed.
 */
HB_FUNC( WVW_XBINFO )
{
   byte bStyle;
   HWND hWndXB = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_SCROLLBAR, ( UINT ) hb_parnl( 2 ), &bStyle );

   if( hWndXB )
   {
      SCROLLINFO si;

      si.cbSize = sizeof( si );
      si.fMask  = SIF_ALL;

      if( GetScrollInfo( hWndXB, SB_CTL, &si ) )
      {
         PHB_ITEM aInfo = hb_itemArrayNew( 5 );

         hb_arraySetNL( aInfo, 1, si.nMin );
         hb_arraySetNL( aInfo, 2, si.nMax );
         hb_arraySetNL( aInfo, 3, si.nPage );
         hb_arraySetNL( aInfo, 4, si.nPos );
         hb_arraySetNL( aInfo, 5, si.nTrackPos );

         hb_itemReturnRelease( aInfo );
      }
      else
         hb_reta( 0 );
   }
   else
      hb_reta( 0 );
}

/* wvw_xbEnable( [nWinNum], nXBid, nFlags )
 *  enable/disable scrollbar nXBid in window nWinNum (default to topmost window)
 *  nFlags: ESB_ENABLE_BOTH                    0: enable both arrows
 *        ESB_DISABLE_LEFT/ESB_DISABLE_UP    1: disable left/up arrow
 *        ESB_DISABLE_RIGHT/ESB_DISABLE_DOWN 2: disable right/down arrow
 *        ESB_DISABLE_BOTH                   3: disable both arrow
 * returns .t. if successful
 */
HB_FUNC( WVW_XBENABLE )
{
   UINT uiFlags = ( UINT ) hb_parni( 3 );
   byte bStyle;
   HWND hWndXB = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_SCROLLBAR, ( UINT ) hb_parnl( 2 ), &bStyle );

   if( hWndXB && uiFlags <= ESB_DISABLE_BOTH )
      hb_retl( EnableScrollBar( hWndXB, SB_CTL, uiFlags ) );
   else
      hb_retl( HB_FALSE );
}

/* wvw_xbShow( [nWinNum], nXBid, lShow )
 *  show/hide scrollbar nXBid in window nWinNum (default to topmost window)
 *  nXBid is the handle of the scrolbar
 *  lShow: .T. shows the scrolbar (default)
 *       .F. hides the scrolbar
 * returns .t. if successful
 */
HB_FUNC( WVW_XBSHOW )
{
   byte bStyle;
   HWND hWndXB = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_SCROLLBAR, ( UINT ) hb_parnl( 2 ), &bStyle );

   if( hWndXB )
      hb_retl( ShowScrollBar( hWndXB, SB_CTL, ( BOOL ) hb_parldef( 3, HB_TRUE ) ) );
   else
      hb_retl( HB_FALSE );
}


/* SCROLLBAR ends */
