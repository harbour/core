/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw progressbar functions
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

/* wvw_pgCreate( [nWinNum], nTop, nLeft, nBottom, nRight, [aOffset],
 *                         [nBackColor], [nBarColor], [lSmooth], [lVertical])
 * create progress bar for window nWinNum
 * nTop: row of top/left corner (in character unit)
 * nLeft: col of top/left corner (in character unit)
 * nBottom: row of bottom/right corner (in character unit)
 * nRight: col of bottom/right corner (in character unit)
 * aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *        dimension of progress bar. defaults: {0, 0, 0, 0}
 * nBackColor: color of background (as RGB value)
 * nBarColor: color of bar (as RGB value)
 * lSmooth: if .T., draw as smooth bar (default is .F.)
 * lVertical: if .T., draw as vertical progress bar (default is .F.)
 *
 * returns control id of newly created progress bar of windows nWinNum
 * returns 0 if failed
 *
 * example:
 * wvw_pgCreate( , 5, 10, 5, 30)
 *  :: creates horiz progressbar on current window at (5,10) to (5,30)
 *     colors using default ones.
 *
 * wvw_pgCreate( , 5, 10, 5, 30, {-1, 0, +1, 0} )
 *  :: same as above, but the bar is enlarged 1 pixel to the top
 *     and 1 pixel to the bottom
 *
 * NOTES:
 * ProgressRange is initially set as 0 - 100.
 * Initial ProgressPos is 0
 */

HB_FUNC( WVW_PGCREATE )
{
   HANDLE    hInstance  = NULL;
   HB_UINT   nWin       = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win    = hb_gt_wvw_GetWindowsData( nWin );
   HWND      hWndParent = wvw_win->hWnd;
   HWND      hWnd;
   POINT     xy;
   int       iTop, iLeft, iBottom, iRight;
   int       iStyle     = 0;
   HB_BOOL   bBackColor = HB_ISNUM( 7 );
   HB_BOOL   bBarColor  = HB_ISNUM( 8 );
   HB_BOOL   bSmooth    = hb_parl( 9 );
   HB_BOOL   bVertical  = hb_parl( 10 );
   HB_UINT   nCtrlId;
   USHORT    usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

   int iOffTop    = HB_ISARRAY( 6 ) ? hb_parvni( 6, 1 ) : 0;
   int iOffLeft   = HB_ISARRAY( 6 ) ? hb_parvni( 6, 2 ) : 0;
   int iOffBottom = HB_ISARRAY( 6 ) ? hb_parvni( 6, 3 ) : 0;
   int iOffRight  = HB_ISARRAY( 6 ) ? hb_parvni( 6, 4 ) : 0;

   InitCommonControls();

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( nWin, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );
   xy.y   -= wvw_win->iLineSpacing;
   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   nCtrlId = hb_gt_wvw_LastControlId( nWin, WVW_CONTROL_PROGRESSBAR );
   if( nCtrlId == 0 )
      nCtrlId = WVW_ID_BASE_PROGRESSBAR;
   else
      nCtrlId++;

   if( bVertical )
      iStyle |= PBS_VERTICAL;
   if( bSmooth )
      iStyle |= PBS_SMOOTH;

   hb_winmainArgGet( &hInstance, NULL, NULL );

   hWnd = CreateWindowEx(
      0,
      PROGRESS_CLASS,
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
      RECT rXB, rOffXB;

      if( bBackColor )
         SendMessage( hWnd, PBM_SETBKCOLOR, 0, ( LPARAM ) ( COLORREF ) hb_parnl( 7 ) );
      if( bBarColor )
         SendMessage( hWnd, PBM_SETBARCOLOR, 0, ( LPARAM ) ( COLORREF ) hb_parnl( 8 ) );

      SendMessage( hWnd, PBM_SETRANGE, 0, MAKELPARAM( 0, 100 ) );
      SendMessage( hWnd, PBM_SETPOS, 0, 0 );

      rXB.top    = usTop;
      rXB.left   = usLeft;
      rXB.bottom = usBottom;
      rXB.right  = usRight;

      rOffXB.top    = iOffTop;
      rOffXB.left   = iOffLeft;
      rOffXB.bottom = iOffBottom;
      rOffXB.right  = iOffRight;

      hb_gt_wvw_AddControlHandle( nWin, WVW_CONTROL_PROGRESSBAR, hWnd, nCtrlId, NULL, rXB, rOffXB, ( HB_BYTE ) iStyle );

      hb_retnl( nCtrlId );
   }
   else
      hb_retnl( 0 );
}

/* wvw_pgDestroy( [nWinNum], nPGid )
 * destroy progressbar nPGid for window nWinNum
 * This function has no return value.
 */
HB_FUNC( WVW_PGDESTROY )
{
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   HB_UINT    nCtrlId = ( HB_UINT ) hb_parnl( 2 );
   WVW_CTRL * pcd     = wvw_win->pcdList;
   WVW_CTRL * pcdPrev = NULL;

   while( pcd )
   {
      if( pcd->nClass == WVW_CONTROL_PROGRESSBAR && pcd->nId == nCtrlId )
         break;

      pcdPrev = pcd;
      pcd     = pcd->pNext;
   }

   if( pcd )
   {
      DestroyWindow( pcd->hWnd );

      if( pcdPrev )
         pcdPrev->pNext = pcd->pNext;
      else
         wvw_win->pcdList = pcd->pNext;

      if( pcd->pBlock )
         hb_itemRelease( pcd->pBlock );

      hb_xfree( pcd );
   }
}

/* wvw_pgSetRange( nWinNum, PGid, [nMin], [nMax] )
 *  update progressbar data range (default is 0-100)
 *  nMin: a number in range of -32767 to +32767
 *  nMax: a number in range of -32767 to +32767
 *
 * Remark: progress position is reset to nMin
 *
 * returns .T. if operation considered successfull
 */
HB_FUNC( WVW_PGSETRANGE )
{
   HWND hWnd = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_PROGRESSBAR, ( HB_UINT ) hb_parnl( 2 ), NULL );
   int  iMin = hb_parni( 3 );
   int  iMax = hb_parni( 4 );

   if( hWnd && iMin <= iMax )
   {
      SendMessage( hWnd, PBM_SETRANGE, 0, MAKELPARAM( iMin, iMax ) );
      SendMessage( hWnd, PBM_SETPOS, ( WPARAM ) iMin, 0 );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_pgSetPos( nWinNum, PGid, [nPos] )
 * update progressbar position within current range
 * nPos: a number in range of current range
 * returns .T. if operation considered successfull
 */
HB_FUNC( WVW_PGSETPOS )
{
   HWND hWnd = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_PROGRESSBAR, ( HB_UINT ) hb_parnl( 2 ), NULL );

   if( hWnd )
   {
      int     iPos = hb_parni( 3 );
      PBRANGE pbrange;

      SendMessage( hWnd, PBM_GETRANGE, ( WPARAM ) TRUE, ( LPARAM ) &pbrange );

      if( iPos >= pbrange.iLow && iPos <= pbrange.iHigh )
      {
         SendMessage( hWnd, PBM_SETPOS, ( WPARAM ) iPos, 0 );

         hb_retl( HB_TRUE );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_pgGetPos( nWinNum, PGid )
 * get progressbar current position
 * returns 0 if operation failed
 */
HB_FUNC( WVW_PGGETPOS )
{
   HWND hWnd = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_PROGRESSBAR, ( HB_UINT ) hb_parnl( 2 ), NULL );

   if( hWnd )
      hb_retni( ( int ) SendMessage( hWnd, PBM_GETPOS, 0, 0 ) );
   else
      hb_retni( 0 );
}
