/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw checkbox/progressbar  functions
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
 * lSmooth: if .t., draw as smooth bar (default is .f.)
 * lVertical: if .t., draw as vertical progress bar (default is .f.)
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
   HANDLE     hInstance   = NULL;
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   HWND       hWndParent  = pWindowData->hWnd;
   HWND       hWndPG;
   POINT      xy;
   int        iTop, iLeft, iBottom, iRight;
   int        iOffTop, iOffLeft, iOffBottom, iOffRight;
   int        iStyle     = 0;
   HB_BOOL    bBackColor = HB_ISNUM( 7 );
   HB_BOOL    bBarColor  = HB_ISNUM( 8 );
   HB_BOOL    bSmooth    = hb_parl( 9 );
   HB_BOOL    bVertical  = hb_parl( 10 );
   UINT       uiPGid;
   USHORT     usTop    = ( USHORT ) hb_parni( 2 ),
              usLeft   = ( USHORT ) hb_parni( 3 ),
              usBottom = ( USHORT ) hb_parni( 4 ),
              usRight  = ( USHORT ) hb_parni( 5 );

   InitCommonControls();

   iOffTop    = HB_ISARRAY( 6 ) ? hb_parvni( 6, 1 ) : 0;
   iOffLeft   = HB_ISARRAY( 6 ) ? hb_parvni( 6, 2 ) : 0;
   iOffBottom = HB_ISARRAY( 6 ) ? hb_parvni( 6, 3 ) : 0;
   iOffRight  = HB_ISARRAY( 6 ) ? hb_parvni( 6, 4 ) : 0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy      = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );
   xy.y   -= pWindowData->byLineSpacing;
   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   uiPGid = hb_gt_wvw_LastControlId( usWinNum, WVW_CONTROL_PROGRESSBAR );
   if( uiPGid == 0 )
      uiPGid = WVW_ID_BASE_PROGRESSBAR;
   else
      uiPGid++;

   if( bVertical )
      iStyle = iStyle | PBS_VERTICAL;
   if( bSmooth )
      iStyle = iStyle | PBS_SMOOTH;

   hb_winmainArgGet( &hInstance, NULL, NULL );

   hWndPG = CreateWindowEx(
      0L,
      PROGRESS_CLASS,
      NULL,
      WS_CHILD | WS_VISIBLE | ( DWORD ) iStyle,
      iLeft,
      iTop,
      iRight - iLeft + 1,
      iBottom - iTop + 1,
      hWndParent,
      ( HMENU ) ( HB_PTRDIFF ) uiPGid,
      ( HINSTANCE ) hInstance,
      NULL );

   if( hWndPG )
   {
      RECT rXB, rOffXB;

      if( bBackColor )
         SendMessage( hWndPG, PBM_SETBKCOLOR, 0, ( LPARAM ) ( COLORREF ) hb_parnl( 7 ) );
      if( bBarColor )
         SendMessage( hWndPG, PBM_SETBARCOLOR, 0, ( LPARAM ) ( COLORREF ) hb_parnl( 8 ) );

      SendMessage( hWndPG, PBM_SETRANGE, 0, MAKELPARAM( 0, 100 ) );
      SendMessage( hWndPG, PBM_SETPOS, 0, 0 );

      rXB.top    = usTop;
      rXB.left   = usLeft;
      rXB.bottom = usBottom;
      rXB.right  = usRight;

      rOffXB.top    = iOffTop;
      rOffXB.left   = iOffLeft;
      rOffXB.bottom = iOffBottom;
      rOffXB.right  = iOffRight;

      hb_gt_wvw_AddControlHandle( usWinNum, WVW_CONTROL_PROGRESSBAR, hWndPG, uiPGid, NULL, rXB, rOffXB, ( byte ) iStyle );

      hb_retnl( uiPGid );
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
   WIN_DATA *     pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   UINT           uiPGid      = ( UINT ) hb_parnl( 2 );
   CONTROL_DATA * pcd         = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev     = NULL;

   while( pcd )
   {
      if( pcd->byCtrlClass == WVW_CONTROL_PROGRESSBAR && pcd->uiCtrlid == uiPGid )
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

/* wvw_pgSetRange(nWinNum, PGid, [nMin], [nMax])
 *  update progressbar data range (default is 0-100)
 *  nMin: a number in range of -32767 to +32767
 *  nMax: a number in range of -32767 to +32767
 *
 * Remark: progress position is reset to nMin
 *
 * returns .t. if operation considered successfull
 */
HB_FUNC( WVW_PGSETRANGE )
{
   byte bStyle;
   HWND hWndPG = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_PROGRESSBAR, ( UINT ) hb_parnl( 2 ), &bStyle );
   int  iMin   = hb_parni( 3 );
   int  iMax   = hb_parni( 4 );

   if( hWndPG && iMin <= iMax )
   {
      SendMessage( hWndPG, PBM_SETRANGE, 0, MAKELPARAM( iMin, iMax ) );
      SendMessage( hWndPG, PBM_SETPOS, ( WPARAM ) iMin, 0 );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_pgSetPos(nWinNum, PGid, [nPos])
 * update progressbar position within current range
 * nPos: a number in range of current range
 * returns .t. if operation considered successfull
 */
HB_FUNC( WVW_PGSETPOS )
{
   byte    bStyle;
   HWND    hWndPG = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_PROGRESSBAR, ( UINT ) hb_parnl( 2 ), &bStyle );

   if( hWndPG )
   {
      int     iPos = hb_parni( 3 );
      PBRANGE pbrange;

      SendMessage( hWndPG, PBM_GETRANGE, ( WPARAM ) TRUE, ( LPARAM ) &pbrange );

      if( iPos >= pbrange.iLow && iPos <= pbrange.iHigh )
      {
         SendMessage( hWndPG, PBM_SETPOS, ( WPARAM ) iPos, 0 );

         hb_retl( HB_TRUE );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_pgGetPos(nWinNum, PGid)
 * get progressbar current position
 * returns 0 if operation failed
 */
HB_FUNC( WVW_PGGETPOS )
{
   byte bStyle;
   HWND hWndPG = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_PROGRESSBAR, ( UINT ) hb_parnl( 2 ), &bStyle );

   if( hWndPG )
      hb_retni( ( int ) SendMessage( hWndPG, PBM_GETPOS, 0, 0 ) );
   else
      hb_retni( 0 );
}
