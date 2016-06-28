/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW progressbar functions
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
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      HWND  hWnd;
      POINT xy;

      int iStyle = 0;
      int nCtrlId;

      RECT rXB, rOffXB;

      InitCommonControls();

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

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      nCtrlId = hb_gt_wvw_LastControlId( wvw_win, WVW_CONTROL_PROGRESSBAR );
      if( nCtrlId == 0 )
         nCtrlId = WVW_ID_BASE_PROGRESSBAR;
      else
         nCtrlId++;

      if( hb_parl( 9 ) /* fSmooth */ )
         iStyle |= PBS_SMOOTH;
      if( hb_parl( 10 ) /* fVertical */ )
         iStyle |= PBS_VERTICAL;

      hWnd = CreateWindowEx(
         0,
         PROGRESS_CLASS,
         NULL,
         WS_CHILD | WS_VISIBLE | ( DWORD ) iStyle,
         iLeft,
         iTop,
         iRight - iLeft + 1,
         iBottom - iTop + 1,
         wvw_win->hWnd,
         ( HMENU ) ( HB_PTRUINT ) nCtrlId,
         GetModuleHandle( NULL ),
         NULL );

      if( hWnd )
      {
         if( HB_ISNUM( 7 ) )
            SendMessage( hWnd, PBM_SETBKCOLOR, 0, ( LPARAM ) hbwapi_par_COLORREF( 7 ) );
         if( HB_ISNUM( 8 ) )
            SendMessage( hWnd, PBM_SETBARCOLOR, 0, ( LPARAM ) hbwapi_par_COLORREF( 8 ) );

         SendMessage( hWnd, PBM_SETRANGE, 0, MAKELPARAM( 0, 100 ) );
         SendMessage( hWnd, PBM_SETPOS, 0, 0 );

         hb_gt_wvw_AddControlHandle( wvw_win, WVW_CONTROL_PROGRESSBAR, hWnd, nCtrlId, NULL, rXB, rOffXB, iStyle );

         hb_retni( nCtrlId );
         return;
      }
   }

   hb_retni( 0 );
}

/* wvw_pgDestroy( [nWinNum], nPGid )
   destroy progressbar nPGid for window nWinNum
   This function has no return value. */
HB_FUNC( WVW_PGDESTROY )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int      nCtrlId     = hb_parni( 2 );
      PWVW_CTL wvw_ctl     = wvw_win->ctlList;
      PWVW_CTL wvw_ctlPrev = NULL;

      while( wvw_ctl )
      {
         if( wvw_ctl->nClass == WVW_CONTROL_PROGRESSBAR && wvw_ctl->nId == nCtrlId )
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

/* wvw_pgSetRange( nWinNum, PGid, [nMin], [nMax] )
    update progressbar data range (default is 0-100)
    nMin: a number in range of -32767 to +32767
    nMax: a number in range of -32767 to +32767
   Remark: progress position is reset to nMin
   returns .T. if operation considered successful */
HB_FUNC( WVW_PGSETRANGE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_PROGRESSBAR, hb_parni( 2 ), NULL );
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
   update progressbar position within current range
   nPos: a number in range of current range
   returns .T. if operation considered successful */
HB_FUNC( WVW_PGSETPOS )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_PROGRESSBAR, hb_parni( 2 ), NULL );

   if( hWnd )
   {
      int     iPos = hb_parni( 3 );
      PBRANGE pbrange;

      SendMessage( hWnd, PBM_GETRANGE, ( WPARAM ) TRUE, ( LPARAM ) &pbrange );

      if( iPos >= pbrange.iLow && iPos <= pbrange.iHigh )
      {
         SendMessage( hWnd, PBM_SETPOS, ( WPARAM ) iPos, 0 );

         hb_retl( HB_TRUE );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

/* wvw_pgGetPos( nWinNum, PGid )
   get progressbar current position
   returns 0 if operation failed */
HB_FUNC( WVW_PGGETPOS )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_PROGRESSBAR, hb_parni( 2 ), NULL );

   if( hWnd )
      hb_retni( ( int ) SendMessage( hWnd, PBM_GETPOS, 0, 0 ) );
   else
      hb_retni( 0 );
}
