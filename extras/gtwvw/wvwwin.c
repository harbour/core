/*
 * Window Related xHarbour callable functions
 *
 * Budyanto Dj. <budyanto@centrin.net.id>
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

#include "hbapierr.h"
#include "hbmath.h"

static HB_BOOL hb_gt_wvw_SetCentreWindow( PWVW_WIN wvw_win, HB_BOOL fCentre, HB_BOOL fPaint )
{
   HB_BOOL fOldCentre = wvw_win->CentreWindow;

   wvw_win->CentreWindow = fCentre;

   if( fPaint )
   {
      if( IsZoomed( wvw_win->hWnd ) )
         ShowWindow( wvw_win->hWnd, SW_MAXIMIZE );
      else
         ShowWindow( wvw_win->hWnd, SW_RESTORE );

      hb_gt_wvw_ResetWindowSize( wvw_win, wvw_win->hWnd );
   }

   return fOldCentre;
}

/* 2004-07-13 this function was named WVW_lOpenWindow()
 *  now it is wvw_nOpenWindow()
 *  it now returns numeric

 *  wvw_nOpenWindow(cWinName, row1, col1, row2, col2, ;
 *                nStyle, nParentWin)
 * rowx and colx are relative to MAIN WINDOW (not current window!)
 * rowx and colx are used for:
 * (1) positioning window to its initial position,
 * (2) determining the size of the window (new MaxRow() and MaxCol())
 * (3) saved into RowOfs and ColOfs for MainCoord mode
 *
 * nStyle is window style (eg. WS_OVERLAPPEDWINDOW, etc.)
 *       default is: WS_CAPTION|WS_SYSMENU |WS_CLIPCHILDREN
 *       WARNING: you must know what you're doing if you supplied this param
 *       NOTES: if you will use controls such as PUSHBUTTON,
 *              you MUST include WS_CLIPCHILDREN.
 *
 * nParentWin is parent window of the new on we're about to open.
 *       default is: current window (in Standard Mode)
 *                   last window (in MainCoord Mode)
 *       If you want the new window to not have parent,
 *       pass -1 as nParentWin.
 *
 *
 * returns window number if successful
 * returns 0 if failed
 */

HB_FUNC( WVW_NOPENWINDOW )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      LPCTSTR szWinName;
      void *  hWinName = NULL;

      PWVW_WIN wvw_par;

      int iRow1, iCol1, iRow2, iCol2;
      int nWin;

      HWND hWndParent;

      DWORD     dwStyle    = ( DWORD ) hb_parnintdef( 6, WS_POPUP | WS_CAPTION | WS_SYSMENU | WS_CLIPCHILDREN );
      int       iParentWin = hb_gt_wvw_nWin_N( 7 );
      PHB_FNAME pFileName  = NULL;

      if( wvw->iNumWindows <= 0 )
      {
         hb_retni( 0 );
         return;
      }

      if( wvw->iNumWindows == HB_SIZEOFARRAY( wvw->pWin ) )
      {
         MessageBox( NULL, TEXT( "Too many windows to open" ), TEXT( "Error" ), MB_ICONERROR );
         hb_retni( 0 );
         return;
      }

      if( iParentWin > ( wvw->iNumWindows - 1 ) )
      {
         MessageBox( NULL, TEXT( "Invalid parent window" ), TEXT( "Error" ), MB_ICONERROR );
         hb_retni( 0 );
         return;
      }

      if( iParentWin < 0 )
      {
         if( hb_gt_wvw_GetMainCoordMode() )
            wvw_par = hb_gt_wvw_win_top();
         else
            wvw_par = hb_gt_wvw_win( wvw->iCurWindow );

         hWndParent = NULL;
      }
      else
      {
         wvw_par = hb_gt_wvw_win( iParentWin );

         hWndParent = wvw_par->hWnd;
      }

      if( HB_ISCHAR( 1 ) )
      {
         HB_SIZE nLen;

         szWinName = HB_PARSTR( 1, &hWinName, &nLen );

         if( nLen > HB_SIZEOFARRAY( wvw_par->szWinName ) - 1 )
         {
            MessageBox( NULL, TEXT( "Window name too long" ), TEXT( "Error" ), MB_ICONERROR );
            hb_retni( 0 );
            return;
         }
      }
      else
      {
         PHB_ITEM pItem = hb_itemPutCPtr( NULL, hb_cmdargBaseProgName() );
         szWinName = HB_ITEMGETSTR( pItem, &hWinName, NULL );
         hb_itemRelease( pItem );
      }

      iRow1 = hb_parni( 2 );
      iCol1 = hb_parni( 3 );
      iRow2 = hb_parnidef( 4, wvw_par->ROWS - 1 );
      iCol2 = hb_parnidef( 5, wvw_par->COLS - 1 );

      nWin = hb_gt_wvw_OpenWindow( szWinName, iRow1, iCol1, iRow2, iCol2, dwStyle, hWndParent );

      hb_strfree( hWinName );

      if( nWin > 0 )
      {
         PWVW_WIN wvw_win = hb_gt_wvw_win( nWin );

         RECT wi, rcWorkArea;

         memset( &wi, 0, sizeof( wi ) );

         GetWindowRect( wvw_win->hWnd, &wi );

         memset( &rcWorkArea, 0, sizeof( rcWorkArea ) );

         if( SystemParametersInfo( SPI_GETWORKAREA, 0, &rcWorkArea, 0 ) )
         {
            if( wi.right < rcWorkArea.left || wi.left > rcWorkArea.right ||
                wi.top > rcWorkArea.bottom || wi.bottom < rcWorkArea.top )
            {
               hb_gt_wvw_SetCentreWindow( hb_gt_wvw_win_top(), HB_TRUE, HB_TRUE );
               hb_gt_wvw_SetCentreWindow( wvw_win, wvw->fDevCentreWindow, HB_TRUE );
            }
         }

         if( hb_gt_wvw_GetMainCoordMode() )
            wvw->iCurWindow = nWin;

         hb_gtSetMode( wvw_win->ROWS, wvw_win->COLS );

         if( hb_gt_wvw_GetMainCoordMode() )
            hb_gt_wvw_SetCurWindow( 0 );

         SendMessage( wvw_win->hWnd, WM_SETFOCUS, 0, 0 );
      }

      if( pFileName )
         hb_xfree( pFileName );

      hb_retni( nWin );
   }
   else
      hb_retni( 0 );
}

/* wvw_lCloseWindow()
 * closes the last/topmost window
 * returns .T. if successful
 */
HB_FUNC( WVW_LCLOSEWINDOW )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      PWVW_WIN wvw_top;

      if( wvw->iNumWindows <= 1 )
      {
         MessageBox( NULL, TEXT( "No more window to close" ), TEXT( "Error" ), MB_ICONERROR );
         hb_retl( HB_FALSE );
         return;
      }

      hb_gt_wvw_CloseWindow();

      wvw_top = hb_gt_wvw_win_top();

      if( wvw_top )
      {
         if( ! hb_gt_wvw_GetMainCoordMode() )
         {
            wvw->fQuickSetMode = HB_TRUE;
            hb_gtSetMode( wvw_top->ROWS, wvw_top->COLS );
            wvw->fQuickSetMode = HB_FALSE;
         }
         else
            hb_gt_wvw_SetCurWindow( 0 );

         SendMessage( wvw_top->hWnd, WM_SETFOCUS, 0, 0 );

         hb_retl( HB_TRUE );
      }
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_GET_HND_WINDOW )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      HB_RETHANDLE( wvw_win->hWnd );
   else
      HB_RETHANDLE( NULL );
}

/* wvw_nNumWindows()
 * returns number of windows opened (including main window)
 */
HB_FUNC( WVW_NNUMWINDOWS )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
      hb_retni( wvw->iNumWindows );
   else
      hb_retni( 0 );
}

/* wvw_XReposWindow(lAnchored)
 * reposition all windows to their initial position
 *
 * if lAnchored == .T. (default)
 *    all subwindows are positioned according to their respective (row1,col1) coordinate
 * else
 *    all subwindows are positioned according to whatever their "CenterWindow" setting
 *    (see also wvw_CenterWindow())
 */
HB_FUNC( WVW_XREPOSWINDOW )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      int     i;
      HB_BOOL fAnchored = hb_parldef( 1, HB_TRUE );

      /* centerize Main Window, only if not maximized */
      hb_gt_wvw_SetCentreWindow( hb_gt_wvw_win_top(), HB_TRUE, HB_TRUE );

      /* reposition all subwindows */
      for( i = 1; i < wvw->iNumWindows; i++ )
      {
         PWVW_WIN wvw_win = hb_gt_wvw_win( i );

         if( wvw_win )
         {
            if( fAnchored )
               hb_gt_wvw_SetCentreWindow( wvw_win, HB_FALSE, HB_TRUE );
            else
               hb_gt_wvw_SetCentreWindow( wvw_win, wvw_win->CentreWindow, HB_TRUE );
         }
      }
   }
}

/* wvw_nSetCurWindow( nWinNum )   (0==MAIN)
 *  assigns nWinNum as the new current window (wvw->iCurWindow)
 *  returns old current window
 *  example: saved := wvw_nSetCurWindow(0)
 *         ? "This will be displayed in Main Window"
 *         wvw_nSetCurWindow(saved)
 * notes: makes sense only if !wvw->fMainCoordMode
 */
HB_FUNC( WVW_NSETCURWINDOW )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      if( HB_ISNUM( 1 ) )
      {
         int nWin = hb_parni( 1 );

         if( nWin >= 0 && nWin < wvw->iNumWindows )
            hb_retni( hb_gt_wvw_SetCurWindow( nWin ) );
         else
            hb_errRT_TERM( EG_BOUND, 10001, "Window number out of range", "wvw_nSetCurWindow()", 0, 0 );
      }
      else
         hb_retni( wvw->iCurWindow );
   }
   else
      hb_retni( 0 );
}

/* wvw_nRowOfs( [nWinNum] )
 * returns row offset of window #nWinNum (0==MAIN), relative to Main Window
 * nWinNum defaults to current window
 */
HB_FUNC( WVW_NROWOFS )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      hb_retni( hb_gt_wvw_RowOfs( wvw_win ) );
   else
      hb_retni( 0 );
}

/* wvw_nColOfs( [nWinNum] )
   returns col offset of window #nWinNum (0==MAIN), relative to Main Window
   nWinNum defaults to topmost window */
HB_FUNC( WVW_NCOLOFS )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      hb_retni( hb_gt_wvw_ColOfs( wvw_win ) );
   else
      hb_retni( 0 );
}

/* wvw_MaxMaxRow( [nWinNum] )
   returns maximum possible MaxRow() in current screen setting for font used by window nWinNum */
HB_FUNC( WVW_MAXMAXROW )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int maxrows;

      /* rows and cols passed are dummy ones */
      hb_gt_wvw_ValidWindowSize( wvw_win, 10, 10, wvw_win->hFont, wvw_win->fontWidth, &maxrows, NULL );

      hb_retni( maxrows - 1 );
   }
   else
      hb_retni( 0 );
}

/* wvw_MaxMaxCol( [nWinNum] )
   returns maximum possible MaxCol() in current screen setting for font used by window nWinNum */
HB_FUNC( WVW_MAXMAXCOL )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int maxcols;

      /* rows and cols passed are dummy ones */
      hb_gt_wvw_ValidWindowSize( wvw_win, 10, 10, wvw_win->hFont, wvw_win->fontWidth, NULL, &maxcols );

      hb_retni( maxcols - 1 );
   }
   else
      hb_retni( 0 );
}

/* wvw_UnreachedBr( [nWinNum], [nBottomPixels], [nRightPixels] )
 * get unreached pixels
 * below MaxRow() to nBottomPixels
 * and on the right of maxcols() to nRightPixels
 */
HB_FUNC( WVW_UNREACHEDBR )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   int cols = 0, rows = 0;

   if( wvw_win )
   {
      if( IsZoomed( wvw_win->hWnd ) )
      {
         POINT xy = hb_gt_wvw_GetXYFromColRow( wvw_win, wvw_win->COLS, wvw_win->ROWS );
         RECT  ci;

         memset( &ci, 0, sizeof( ci ) );

         GetClientRect( wvw_win->hWnd, &ci );

         rows = ci.bottom - xy.y - wvw_win->iSBHeight;
         cols = ci.right - xy.x;
      }
   }

   hb_storni( rows, 2 );
   hb_storni( cols, 3 );
}

/* wvw_SetMainCoord( [lMainCoord] )
 * returns old setting of wvw->fMainCoordMode,
 * then assigns wvw->fMainCoordMode := lMainCoord (if supplied)
 */
HB_FUNC( WVW_SETMAINCOORD )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      hb_retl( hb_gt_wvw_GetMainCoordMode() );

      if( HB_ISLOG( 1 ) )
      {
         wvw->fMainCoordMode = hb_parl( 1 );

         if( wvw->fMainCoordMode )
            hb_gt_wvw_SetCurWindow( 0 );
         else
            hb_gt_wvw_SetCurWindow( wvw->iNumWindows - 1 );
      }
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_NoClose( [nWinNum] )
 * disable CLOSE 'X' button of a window
 *
 * no return value
 */
HB_FUNC( WVW_NOCLOSE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      HMENU hMenu = GetSystemMenu( wvw_win->hWnd, FALSE );

      if( hMenu )
      {
         DeleteMenu( hMenu, SC_CLOSE, MF_BYCOMMAND );
         DrawMenuBar( wvw_win->hWnd );
      }
   }
}

/* wvw_SetWinStyle( [nWinNum], [nStyle] )
 * Get/Set window style
 * NOTES: if window has controls (eg. pushbutton, scrollbar)
 *      you should include WS_CLIPCHILDREN in nStyle
 *
 * SIDE EFFECT:
 *       if window is hidden, applying nStyle here will cause it to show
 *
 * return Window Style prior to applying the new style
 */
HB_FUNC( WVW_SETWINSTYLE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      LONG_PTR lpStyle;

      if( HB_ISNUM( 2 ) )
      {
         lpStyle = SetWindowLongPtr( wvw_win->hWnd, GWL_STYLE, ( LONG_PTR ) hb_parnint( 2 ) );
         SetWindowPos( wvw_win->hWnd, NULL, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_FRAMECHANGED );
         ShowWindow( wvw_win->hWnd, SW_SHOWNORMAL );
      }
      else
         lpStyle = GetWindowLongPtr( wvw_win->hWnd, GWL_STYLE );

      hb_retnint( lpStyle );
   }
   else
      hb_retnint( 0 );
}

/* wvw_EnableMaximize( [nWinNum], [lEnable] )
 * Get/Set maximize button
 *
 * returns maximize box state prior to applying the new style
 *
 * NOTE: in order to enable MAXIMIZE button, app should have WVW_SIZE() callback function
 */
HB_FUNC( WVW_ENABLEMAXIMIZE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      LONG_PTR lpStyle = GetWindowLongPtr( wvw_win->hWnd, GWL_STYLE );

      HB_BOOL fState = ( lpStyle & ( LONG_PTR ) WS_MAXIMIZEBOX ) != 0;

      hb_retl( fState );

      if( HB_ISLOG( 2 ) )
      {
         if( hb_parl( 2 ) )
         {
            if( fState )
               return;  /* no need */
            lpStyle |= ( LONG_PTR ) WS_MAXIMIZEBOX;
         }
         else
         {
            if( ! fState )
               return;  /* no need */
            lpStyle &= ~( LONG_PTR ) WS_MAXIMIZEBOX;
         }

         SetWindowLongPtr( wvw_win->hWnd, GWL_STYLE, lpStyle );
         SetWindowPos( wvw_win->hWnd, NULL, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_FRAMECHANGED );
         ShowWindow( wvw_win->hWnd, SW_SHOW );
      }
   }
   else
      hb_retl( HB_FALSE );
}

/* GTWVW parameter setting from .prg */
/* Budyanto Dj. <budyanto@centrin.net.id> */

/* wvw_SetPaintRefresh( [nPaintRefresh] )
 * returns old setting of wvw->uiPaintRefresh (millisec between calls to WVW_PAINT)
 * then assigns wvw->uiPaintRefresh:= nPaintRefresh (if supplied)
 * NOTES: nPaintRefresh must be >= 50
 *       or nPaintRefresh == 0, causing Repaint to execute immediately, as GTWVT
 */
HB_FUNC( WVW_SETPAINTREFRESH )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      hb_retni( wvw->uiPaintRefresh );

      if( HB_ISNUM( 1 ) && ( hb_parni( 1 ) >= 50 || hb_parni( 1 ) == 0 ) )
      {
         wvw->uiPaintRefresh = ( HB_UINT ) hb_parni( 1 );

         if( wvw->a.pSymWVW_PAINT )
         {
            int i;
            for( i = 0; i < wvw->iNumWindows; i++ )
            {
               PWVW_WIN wvw_win = hb_gt_wvw_win( i );

               if( wvw_win )
               {
                  if( wvw->uiPaintRefresh > 0 )
                     SetTimer( wvw_win->hWnd, WVW_ID_SYSTEM_TIMER, ( UINT ) wvw->uiPaintRefresh, NULL );
                  else
                     KillTimer( wvw_win->hWnd, WVW_ID_SYSTEM_TIMER );
               }
            }
         }
      }
   }
   else
      hb_retni( 0 );
}

/* wvw_SetVertCaret( [lOn] )
 * if lOn is supplied:
 * lOn == .T.: turn caret into vertical caret
 * lOn == .F.: turn caret into horizontal caret
 * return old setting of wvw->fVertCaret
 */
/* TODO: do you want to make it window selective? */
HB_FUNC( WVW_SETVERTCARET )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      hb_retl( wvw->fVertCaret );

      if( HB_ISLOG( 1 ) )
      {
         PWVW_WIN wvw_win = hb_gt_wvw_win_top();

         wvw->fVertCaret = hb_parl( 1 );

         if( wvw_win )
         {
            /* TODO: we should recalculate width and height of caret! */
            hb_gt_wvw_KillCaret( wvw_win );
            hb_gt_wvw_CreateCaret( wvw_win );
         }
      }
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_SetDefCentreWindow( [lCentre] )
 * returns old setting of wvw->fDevCentreWindow (default CentreWindow setting for newly opened subwindow)
 * then assigns wvw->fDevCentreWindow := lCentre (if supplied)
 * NOTES:
 * - lCentre will be the default CentreWindow for all subwindow opens
 */
HB_FUNC( WVW_SETDEFCENTREWINDOW )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      hb_retl( wvw->fDevCentreWindow );

      if( HB_ISLOG( 1 ) )
         wvw->fDevCentreWindow = hb_parl( 1 );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_SetDefHCentreWindow( [lCentre] )
 * returns old setting of wvw->fDevHCentreWindow (default horizontal CentreWindow setting for newly opened subwindow)
 * then assigns wvw->fDevHCentreWindow := lCentre (if supplied)
 * NOTES:
 * - lCentre will be the default CentreWindow for all subwindow opens
 */
HB_FUNC( WVW_SETDEFHCENTREWINDOW )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      hb_retl( wvw->fDevHCentreWindow );

      if( HB_ISLOG( 1 ) )
         wvw->fDevHCentreWindow = hb_parl( 1 );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_SetDefVCentreWindow( [lCentre] )
 * returns old setting of wvw->fDevVCentreWindow (default horizontal CentreWindow setting for newly opened subwindow)
 * then assigns wvw->fDevVCentreWindow := lCentre (if supplied)
 * NOTES:
 * - lCentre will be the default CentreWindow for all subwindow opens
 */
HB_FUNC( WVW_SETDEFVCENTREWINDOW )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      hb_retl( wvw->fDevVCentreWindow );

      if( HB_ISLOG( 1 ) )
         wvw->fDevVCentreWindow = hb_parl( 1 );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_SetDefLineSpacing( [nLineSpacing] )
 * returns old setting of wvw->iDefLineSpacing (default linespacing between lines)
 * then assigns wvw->iDefLineSpacing:= nLineSpacing (if supplied)
 * NOTES:
 * - nLineSpacing will be the default line spacing for all window opens
 * - nLineSpacing must be even, positive number <= 40
 *   otherwise it will be ignored
 * - to check line spacing being used by a window, use wvw_SetLineSpacing()
 */
HB_FUNC( WVW_SETDEFLINESPACING )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      hb_retni( wvw->iDefLineSpacing );

      if( HB_ISNUM( 1 ) && hb_parni( 1 ) >= 0 && hb_parni( 1 ) <= 40 && /* nobody is crazy enough to use > 40 */
          fmod( hb_parnd( 1 ), 2 ) == 0 )
         wvw->iDefLineSpacing = hb_parni( 1 );
   }
   else
      hb_retni( 0 );
}

/* wvw_SetDefLSpaceColor( [nColorIndex] )
 * returns old setting of wvw->iDefLSpaceColor (color index of spacing between lines)
 * then assigns wvw->iDefLSpaceColor:= nColorIndex (if supplied)
 * NOTES:
 * - nColorIndex will be the default line spacing color for all window opens
 * - nColorIndex must >= 0 and <= 15, or == -1
 *   nCOlorIndex == 0:black, 1:blue, ..., 7:white, ..., 15:bright white
 *   nColorIndex == -1 means line spacing has no color
 * - to check line spacing color being used by a window, use wvw_SetLSpaceColor()
 */
HB_FUNC( WVW_SETDEFLSPACECOLOR )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      hb_retni( wvw->iDefLSpaceColor );

      if( HB_ISNUM( 1 ) && hb_parni( 1 ) >= -1 && hb_parni( 1 ) <= 15 )
         wvw->iDefLSpaceColor = hb_parni( 1 );
   }
   else
      hb_retni( 0 );
}

/* wvw_SetLSpaceColor( [nWinNum], [nColorIndex] )
 * returns old setting of line space color in window nWinNum
 * then set the line spacing color to nColorIndex (if supplied)
 * NOTES:
 * - nColorIndex must be >= 0 and <= 15, or -1
 *   otherwise it will be ignored
 *   nCOlorIndex == 0:black, 1:blue, ..., 7:white, ..., 15:bright white
 * - nColorIndex == -1 means line spacing is not colored
 * - to change default line space color for next window open, use wvw_SetDefLineSpacing()
 */
HB_FUNC( WVW_SETLSPACECOLOR )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iOldValue = wvw_win->iLSpaceColor;

      hb_retni( iOldValue );

      if( HB_ISNUM( 2 ) && hb_parni( 2 ) >= -1 && hb_parni( 2 ) <= 15 )
      {
         wvw_win->iLSpaceColor = hb_parni( 2 );

         if( wvw_win->iLSpaceColor != iOldValue )
            hb_gt_wvw_SetInvalidRect( wvw_win, 0, 0, wvw_win->COLS - 1, wvw_win->ROWS - 1 );
      }
   }
   else
      hb_retni( 0 );
}

/* wvw_AllowNonTopEvent( [lAllow] )
 * returns old setting of wvw->fAllowNonTop
 * and set wvw->fAllowNonTop := lAllow (if this optional param is passed)
 *
 * REMARKS:
 * wvw->fAllowNonTop determines how controls behave on non-topmost window
 * if wvw->fAllowNonTop==.T., control's codeblock will always be executed
 *                         when an event occurs on the control
 * if wvw->fAllowNonTop==.F. (the default)
 *                         control's codeblock will be executed only
 *                         if the control is on the topmost window.
 * IMPORTANT NOTE: KILLFOCUS event will always be executed in all condition
 *
 */
HB_FUNC( WVW_ALLOWNONTOPEVENT )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      hb_retl( wvw->fAllowNonTop );

      if( HB_ISLOG( 1 ) )
         wvw->fAllowNonTop = hb_parl( 1 );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_RecurseCBlock( [lAllow] )
 * returns old setting of wvw->fRecurseCBlock
 * and set wvw->fRecurseCBlock := lAllow (if this optional param is passed)
 *
 * REMARKS:
 * wvw->fRecurseCBlock determines whether gtwvw allow recursion into control's codeblock
 * if wvw->fRecurseCBlock==.T., control's codeblock is allowed to recurse
 * if wvw->fRecurseCBlock==.F. (the default)
 *                         control's codeblock is not allowed to recurse
 * NOTE: if you are using wvw->fRecurseCBlock == .T. make sure your
 *       codeblock is reentrant, otherwise you may have weird result.
 */
HB_FUNC( WVW_RECURSECBLOCK )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      hb_retl( wvw->fRecurseCBlock );

      if( HB_ISLOG( 1 ) )
         wvw->fRecurseCBlock = hb_parl( 1 );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_NoStartupSubWindow( [lOn] )
 * if lOn is supplied:
 * lOn == .T.: when opening window, window will not be displayed
 * lOn == .F.: when opening window, window will be displayed (default)
 * return old setting of s_bNOSTARTUPWINDOW
 */
HB_FUNC( WVW_NOSTARTUPSUBWINDOW )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      hb_retl( wvw->fNOSTARTUPSUBWINDOW );

      if( HB_ISLOG( 1 ) )
         wvw->fNOSTARTUPSUBWINDOW = hb_parl( 1 );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_GETSCREENWIDTH )
{
   hb_retni( GetSystemMetrics( SM_CXSCREEN ) );
}

HB_FUNC( WVW_GETSCREENHEIGHT )
{
   hb_retni( GetSystemMetrics( SM_CYSCREEN ) );
}

/* wvw_SetWindowCentre( nWinNum,   (0==MAIN)
 *                      lCentre,
 *                      lPaintIt)  (if .F. it will just assign lCentre to WVW_WIN)
 */
HB_FUNC( WVW_SETWINDOWCENTRE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      hb_gt_wvw_SetCentreWindow( wvw_win, hb_parl( 2 ), hb_parl( 3 ) );
}

/* wvw_EnableShortcuts( nWinNum, lEnable )
 * lEnable defaults to .T.
 *
 * returns old setting of EnableShortCuts
 */
HB_FUNC( WVW_ENABLESHORTCUTS )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      hb_retl( wvw_win->EnableShortCuts );

      wvw_win->EnableShortCuts = hb_parldef( 2, HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_SETALTF4CLOSE )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw )
   {
      hb_retl( wvw->a.AltF4Close );

      wvw->a.AltF4Close = hb_parl( 1 );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_PROCESSMESSAGES )
{
   hb_gt_wvw_ProcessMessages( NULL );

   hb_retl( HB_TRUE );
}

HB_FUNC( WVW_GETTITLE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      TCHAR ucText[ 1024 ];

      GetWindowText( wvw_win->hWnd, ucText, HB_SIZEOFARRAY( ucText ) - 1 );

      HB_RETSTR( ucText );
   }
   else
      hb_retc_null();
}

HB_FUNC( WVW_INVALIDATERECT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int usTop    = hb_parni( 2 ),
          usLeft   = hb_parni( 3 ),
          usBottom = hb_parni( 4 ),
          usRight  = hb_parni( 5 );

      RECT  rc;
      POINT xy;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy        = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      rc.top    = xy.y;
      rc.left   = xy.x;
      xy        = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );
      rc.bottom = xy.y - 1;
      rc.right  = xy.x - 1;

      InvalidateRect( wvw_win->hWnd, &rc, TRUE );
   }
}

HB_FUNC( WVW_ISLBUTTONPRESSED )
{
   hb_retl( ( GetKeyState( VK_LBUTTON ) & 0x8000 ) != 0 );
}

HB_FUNC( WVW_CLIENTTOSCREEN )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   PHB_ITEM aXY = hb_itemArrayNew( 2 );
   POINT    xy;

   if( wvw_win )
   {
      int usTop  = hb_parni( 2 ),
          usLeft = hb_parni( 3 );

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, NULL, NULL );

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );

      ClientToScreen( wvw_win->hWnd, &xy );
   }
   else
      memset( &xy, 0, sizeof( xy ) );

   hb_arraySetNL( aXY, 1, xy.x );
   hb_arraySetNL( aXY, 2, xy.y );

   hb_itemReturnRelease( aXY );
}
