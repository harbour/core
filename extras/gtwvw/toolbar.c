/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw toolbar functions
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

#include "hbvm.h"

/* add one button to existing Toolbar */
/* uiBitmap is resource id */
static HB_BOOL hb_gt_wvw_AddTBButton( HWND hWndToolbar, const char * szBitmap, HB_UINT uiBitmap, LPCTSTR pszLabel, int iCommand, int iBitmapType, HB_BOOL fMap3Dcolors, PWVW_WIN wvw_win, BOOL fDropdown )
{
   TBBUTTON    tbb;
   TBADDBITMAP tbab;
   TCHAR       szBuffer[ WVW_TB_LABELMAXLENGTH + 2 ];
   int         iNewBitmap, iNewString;
   int         iOffset;

   if( iCommand == 0 )
   {
      tbb.iBitmap   = 0;
      tbb.idCommand = 0;
      tbb.fsState   = TBSTATE_ENABLED;
      tbb.fsStyle   = TBSTYLE_SEP;
      tbb.dwData    = 0;
      tbb.iString   = 0;

      return ( HB_BOOL ) SendMessage( hWndToolbar, TB_ADDBUTTONS, ( WPARAM ) 1, ( LPARAM ) ( LPTBBUTTON ) &tbb );
   }

   switch( iBitmapType )
   {
      case 0:
         iOffset = 0;
         break;
      case 1:
         iOffset = wvw_win->iStartStdBitmap;
         break;
      case 2:
         iOffset = wvw_win->iStartViewBitmap;
         break;
      case 3:
         iOffset = wvw_win->iStartHistBitmap;
         break;
      default:
         iOffset = 0;
   }

   if( iBitmapType == 0 )
   {
      HBITMAP hBitmap = hb_gt_wvw_PrepareBitmap( szBitmap, uiBitmap, wvw_win->iTBImgWidth, wvw_win->iTBImgHeight, fMap3Dcolors, hWndToolbar );

      if( ! hBitmap )
         return HB_FALSE;

      tbab.hInst = NULL;
      tbab.nID   = ( UINT_PTR ) hBitmap;
      iNewBitmap = ( int ) SendMessage( hWndToolbar, TB_ADDBITMAP, ( WPARAM ) 1, ( WPARAM ) &tbab );
   }
   else /* system bitmap */
      iNewBitmap = ( int ) uiBitmap + iOffset;

   HB_STRNCPY( szBuffer, pszLabel, HB_SIZEOFARRAY( szBuffer ) - 1 );

   iNewString = ( int ) SendMessage( hWndToolbar, TB_ADDSTRING, 0, ( LPARAM ) szBuffer );

   tbb.iBitmap   = iNewBitmap;
   tbb.idCommand = iCommand;
   tbb.fsState   = TBSTATE_ENABLED;
   tbb.fsStyle   = TBSTYLE_BUTTON;
   if( fDropdown )
      tbb.fsStyle |= BTNS_WHOLEDROPDOWN;
   tbb.dwData  = 0;
   tbb.iString = iNewString;

   return ( HB_BOOL ) SendMessage( hWndToolbar, TB_ADDBUTTONS, ( WPARAM ) 1, ( LPARAM ) ( LPTBBUTTON ) &tbb );
}

static int hb_gt_wvw_IndexToCommand( HWND hWndTB, int iIndex )
{
   TBBUTTON tbb;

   if( SendMessage( hWndTB, TB_GETBUTTON, ( WPARAM ) iIndex, ( LPARAM ) ( LPTBBUTTON ) &tbb ) )
      return tbb.idCommand;
   else
      return 0;
}

static int hb_gt_wvw_CommandToIndex( HWND hWndTB, int iCommand )
{
   return ( int ) SendMessage( hWndTB, TB_COMMANDTOINDEX, ( WPARAM ) iCommand, 0 );
}

static void hb_gt_wvw_TBinitSize( PWVW_WIN wvw_win, HWND hWndTB )
{
   RECT rTB;

   SendMessage( hWndTB, TB_AUTOSIZE, 0, 0 );

   memset( &rTB, 0, sizeof( rTB ) );

   if( GetClientRect( hWndTB, &rTB ) )
      wvw_win->usTBHeight = rTB.bottom + 2;
}

static POINT hb_gt_wvw_TBGetColRowFromXY( PWVW_WIN wvw_win, int x, int y )
{
   POINT colrow;

   colrow.x = x / wvw_win->PTEXTSIZE.x;
   colrow.y = y / ( wvw_win->PTEXTSIZE.y + wvw_win->iLineSpacing );

   return colrow;
}

static void hb_gt_wvw_TBMouseEvent( PWVW_WIN wvw_win, HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   POINT xy, colrow;
   SHORT keyCode  = 0;
   SHORT keyState = 0;

   PWVW_GLO wvw = hb_gt_wvw();

   HB_SYMBOL_UNUSED( hWnd );
   HB_SYMBOL_UNUSED( wParam );

   if( message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE )
   {
      if( ! wvw_win->MouseMove )
         return;
   }

   xy.x = LOWORD( lParam );
   xy.y = HIWORD( lParam );

   colrow = hb_gt_wvw_TBGetColRowFromXY( wvw_win, xy.x, xy.y );

   hb_gt_wvw_SetMouseX( wvw_win, colrow.x );
   hb_gt_wvw_SetMouseY( wvw_win, colrow.y );

   switch( message )
   {
      case WM_LBUTTONDBLCLK:
         keyCode = K_LDBLCLK;
         break;

      case WM_RBUTTONDBLCLK:
         keyCode = K_RDBLCLK;
         break;

      case WM_LBUTTONDOWN:
      {
         HWND hWndFocus = GetFocus();

         if( hb_gt_wvw_GetControlClass( wvw_win, hWndFocus ) > 0 )
            SetFocus( hWnd );

         keyCode = K_LBUTTONDOWN;
         break;
      }

      case WM_RBUTTONDOWN:
         keyCode = K_RBUTTONDOWN;
         break;

      case WM_LBUTTONUP:
         keyCode = K_LBUTTONUP;
         break;

      case WM_RBUTTONUP:

         if( wvw_win->hPopup )
         {
            int nPopupRet;
            GetCursorPos( &xy );
            nPopupRet = ( int ) TrackPopupMenu( wvw_win->hPopup, TPM_CENTERALIGN + TPM_RETURNCMD, xy.x, xy.y, 0, hWnd, NULL );
            if( nPopupRet )
               hb_gt_wvw_AddCharToInputQueue( nPopupRet );
            return;
         }
         else
         {
            keyCode = K_RBUTTONUP;
            break;
         }

      case WM_MBUTTONDOWN:
         keyCode = K_MBUTTONDOWN;
         break;

      case WM_MBUTTONUP:
         keyCode = K_MBUTTONUP;
         break;

      case WM_MBUTTONDBLCLK:
         keyCode = K_MDBLCLK;
         break;

      case WM_MOUSEMOVE:
         keyState = ( SHORT ) wParam;

         if( keyState == MK_LBUTTON )
            keyCode = K_MMLEFTDOWN;
         else if( keyState == MK_RBUTTON )
            keyCode = K_MMRIGHTDOWN;
         else if( keyState == MK_MBUTTON )
            keyCode = K_MMMIDDLEDOWN;
         else
            keyCode = K_MOUSEMOVE;
         break;

      case WM_MOUSEWHEEL:
         keyState = HIWORD( wParam );

         if( keyState > 0 )
            keyCode = K_MWFORWARD;
         else
            keyCode = K_MWBACKWARD;

         break;

      case WM_NCMOUSEMOVE:
         keyCode = K_NCMOUSEMOVE;
         break;
   }

   if( wvw->a.pSymWVW_TBMOUSE && keyCode != 0 && hb_vmRequestReenter() )
   {
      hb_vmPushDynSym( wvw->a.pSymWVW_TBMOUSE );
      hb_vmPushNil();
      hb_vmPushInteger( wvw_win->nWinId );
      hb_vmPushInteger( keyCode );
      hb_vmPushInteger( colrow.y );
      hb_vmPushInteger( colrow.x );
      hb_vmPushInteger( keyState );
      hb_vmDo( 5 );

      hb_vmRequestRestore();
   }

   hb_gt_wvw_AddCharToInputQueue( keyCode );
}

static LRESULT CALLBACK hb_gt_wvw_TBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   HWND hWndParent = GetParent( hWnd );
   int  nWin;

   PWVW_GLO wvw = hb_gt_wvw();
   PWVW_WIN wvw_win;

   if( wvw == NULL || hWndParent == NULL )
   {
      /* TODO: runtime/internal error is better */
      MessageBox( NULL, TEXT( "hb_gt_wvw_TBProc(): parent of toolbar is missing" ), TEXT( "Error" ), MB_ICONERROR );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   for( nWin = 0; nWin < wvw->iNumWindows; ++nWin )
   {
      if( wvw->pWin[ nWin ]->hWnd == hWndParent )
         break;
   }

   if( nWin >= wvw->iNumWindows )
   {
      /* TODO: runtime/internal error is better */
      MessageBox( NULL, TEXT( "hb_gt_wvw_TBProc(): invalid handle of toolbar's parent" ), hb_gt_wvw_GetAppName(), MB_ICONERROR );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   wvw_win = wvw->pWin[ nWin ];

   switch( message )
   {
      case WM_RBUTTONDOWN:
      case WM_LBUTTONDOWN:
      case WM_RBUTTONUP:
      case WM_LBUTTONUP:
      case WM_RBUTTONDBLCLK:
      case WM_LBUTTONDBLCLK:
      case WM_MBUTTONDOWN:
      case WM_MBUTTONUP:
      case WM_MBUTTONDBLCLK:
      case WM_MOUSEMOVE:
      case WM_MOUSEWHEEL:
      case WM_NCMOUSEMOVE:

         if( ! hb_gt_wvw_AcceptingInput() || ( nWin != wvw->iNumWindows - 1 ) )
            return 0;

         hb_gt_wvw_TBMouseEvent( wvw_win, hWnd, message, wParam, lParam );
#if 0
         return 0;
         TB_ISBUTTONHIGHLIGHTED
#endif
      case WM_PAINT:
      {
         HGDIOBJ hOldObj;
         HDC     hdc;
         RECT    rTB;
         int     iTop, iRight;

         CallWindowProc( wvw_win->tbOldProc, hWnd, message, wParam, lParam );

         memset( &rTB, 0, sizeof( rTB ) );

         GetClientRect( hWnd, &rTB );
         iTop   = rTB.bottom - 3;
         iRight = rTB.right;

         hdc = GetDC( hWnd );

         hOldObj = SelectObject( hdc, wvw->a.penWhite );

         MoveToEx( hdc, 0, iTop, NULL );          /* Top */
         LineTo( hdc, iRight, iTop );

         SelectObject( hdc, wvw->a.penBlack );

         MoveToEx( hdc, 0, iTop + 2, NULL );      /* Bottom */
         LineTo( hdc, iRight, iTop + 2 );

         SelectObject( hdc, wvw->a.penDarkGray );
         MoveToEx( hdc, 0, iTop + 1, NULL );      /* Middle */
         LineTo( hdc, iRight, iTop + 1 );

         SelectObject( wvw_win->hdc, hOldObj );
         ReleaseDC( hWnd, hdc );

         return 0;
      }
   }

   return CallWindowProc( wvw_win->tbOldProc, hWnd, message, wParam, lParam );
}

/* wvw_tbCreate( [nWinNum], lDisplayText, nStyle, nSystemBitmap, nImageWidth, nImageHeight )
 * creates a toolbar at the top (no button initially)
 * lDisplayText==.F. button's string is used as tooltips (default)
 * nStyle: toolbar style, defaults to TBSTYLE_FLAT | TBSTYLE_TOOLTIPS
 * nSystemBitmap: 0:none, 1:small, 2:large (defaults: 1)
 *               small=16x16 large=24x24
 * nImageWidth/Height are in effect only if nSystemBitmap==0
 */
HB_FUNC( WVW_TBCREATE )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win && wvw_win->hToolBar == NULL )
   {
      HWND hWnd;

      #if 0
      DWORD dwStyle = ( DWORD ) hb_parnidef( 3, TBSTYLE_FLAT | TBSTYLE_TOOLTIPS );
      #endif
      DWORD dwStyle = ( DWORD ) hb_parnidef( 3, TBSTYLE_ALTDRAG | TBSTYLE_FLAT | TBSTYLE_TOOLTIPS | TBSTYLE_TRANSPARENT | TBSTYLE_WRAPABLE );

      int iSystemBitmap = hb_parnidef( 4, 1 );
      int iImageWidth   = iSystemBitmap == 0 && HB_ISNUM( 5 ) ? hb_parni( 5 ) : -1;
      int iImageHeight  = iSystemBitmap == 0 && HB_ISNUM( 6 ) ? hb_parni( 6 ) : -1;

      InitCommonControls();

      if( iImageWidth < 0 )
      {
         switch( iSystemBitmap )
         {
            case 1:
               iImageWidth = 16;
               break;
            case 2:
               iImageWidth = 24;
               break;
            default:
               iImageWidth = 16;
         }
      }
      if( iImageHeight < 0 )
      {
         switch( iSystemBitmap )
         {
            case 1:
               iImageHeight = 16;
               break;
            case 2:
               iImageHeight = 24;
               break;
            default:
               iImageHeight = iImageWidth;
         }
      }

      hWnd = CreateToolbarEx( wvw_win->hWnd,
                              WS_CHILD | WS_VISIBLE | dwStyle,
                              WVW_ID_BASE_TOOLBAR + wvw_win->nWinId,
                              0,
                              wvw->hInstance,
                              0,
                              NULL,
                              0,
                              0,
                              0,
                              iImageWidth,
                              iImageHeight,
                              sizeof( TBBUTTON ) );

      if( hWnd )
      {
         wvw_win->tbOldProc = ( WNDPROC ) SetWindowLongPtr( hWnd, GWLP_WNDPROC, ( LONG_PTR ) hb_gt_wvw_TBProc );

         if( iSystemBitmap > 0 )
         {
            TBADDBITMAP tbab;

            memset( &tbab, 0, sizeof( tbab ) );

            tbab.hInst = HINST_COMMCTRL;

            tbab.nID = iSystemBitmap == 1 ? IDB_STD_SMALL_COLOR : IDB_STD_LARGE_COLOR;
            wvw_win->iStartStdBitmap = ( int ) SendMessage( hWnd, TB_ADDBITMAP, 0, ( WPARAM ) &tbab );

            tbab.nID = iSystemBitmap == 1 ? IDB_VIEW_SMALL_COLOR : IDB_VIEW_LARGE_COLOR;
            wvw_win->iStartViewBitmap = ( int ) SendMessage( hWnd, TB_ADDBITMAP, 0, ( WPARAM ) &tbab );

            tbab.nID = iSystemBitmap == 1 ? IDB_HIST_SMALL_COLOR : IDB_HIST_LARGE_COLOR;
            wvw_win->iStartHistBitmap = ( int ) SendMessage( hWnd, TB_ADDBITMAP, 0, ( WPARAM ) &tbab );
         }
         else
         {
            wvw_win->iStartStdBitmap  = 0;
            wvw_win->iStartViewBitmap = 0;
            wvw_win->iStartHistBitmap = 0;
         }

         wvw_win->iTBImgWidth  = iImageWidth;
         wvw_win->iTBImgHeight = iImageHeight;

         SendMessage( hWnd, TB_SETMAXTEXTROWS, ( WPARAM ) ( hb_parl( 2 ) ? 1 : 0 ), 0 );

         hb_stornl( wvw_win->iStartStdBitmap, 7 );
         hb_stornl( wvw_win->iStartViewBitmap, 8 );
         hb_stornl( wvw_win->iStartHistBitmap, 9 );

         hb_gt_wvw_TBinitSize( wvw_win, hWnd );

         wvw_win->hToolBar = hWnd;

         hb_gt_wvw_ResetWindow( wvw_win );

         HB_RETHANDLE( hWnd );
         return;
      }
      else
         MessageBox( NULL, TEXT( "Failed CreateToolbarEx()" ), hb_gt_wvw_GetAppName(), MB_ICONERROR );
   }

   hb_stornl( 0, 7 );
   hb_stornl( 0, 8 );
   hb_stornl( 0, 9 );

   HB_RETHANDLE( NULL );
}

/* wvw_tbAddButton([nWinNum], nCommand, xBitmap, cLabel, nBitmapType, ;
 *                            lMap3Dcolors, lDropdown)
 * adds one button on the right of existing buttons
 * xBitmap:
 * nBitmap is resource id. or use cBitmap as bitmap file name.
 * (bitmap from resources cannot have > 256 colors)
 *
 * cLabel: if lDisplayText, it will be displayed below the bitmap
 *      otherwise it will be used as tooltip
 * nBitmapType: 0:custom, 1:system std bitmap, 2:system view bitmap, 3:system hist bitmap
 * lMap3Dcolors: defaults to .F.
 *         (meaningfull for custom bitmap only)
 *         if .T. the following color mapping will be performed:
 *            RGB( 192, 192, 192 ) --> COLOR_3DFACE   ("transparent")
 *            RGB( 128, 128, 128 ) --> COLOR_3DSHADOW
 *            RGB( 223, 223, 223 ) --> COLOR_3DLIGHT
 *         This might be desirable to have transparent effect.
 *         LIMITATION: this will work on 256 colored bitmaps only
 */
HB_FUNC( WVW_TBADDBUTTON )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int          iCommand = hb_parni( 2 );
      HB_UINT      uiBitmap = ( HB_UINT ) hb_parnint( 3 );
      const char * szBitmap = hb_parc( 3 );
      LPCTSTR      szLabel;
      int          iBitmapType  = hb_parni( 5 );
      HB_BOOL      fMap3Dcolors = hb_parl( 6 );
      HB_BOOL      fDropdown    = hb_parl( 7 );
      int          iOldHeight;

      void *  hLabel;
      HB_SIZE nLen;

      HWND hWnd = wvw_win->hToolBar;

      if( hWnd == NULL )
      {
         hb_retl( HB_FALSE );
         return;
      }

      if( iCommand >= WVW_ID_BASE_PUSHBUTTON )
      {
         MessageBox( NULL, TEXT( "Toolbar button command ID too high. Potential conflict with pushbutton." ), hb_gt_wvw_GetAppName(), MB_ICONERROR );
         hb_retl( HB_FALSE );
         return;
      }

      szLabel = HB_PARSTRDEF( 4, &hLabel, &nLen );

      if( nLen > WVW_TB_LABELMAXLENGTH )
      {
         hb_strfree( hLabel );
         MessageBox( NULL, TEXT( "Cannot addbutton, label too long." ), hb_gt_wvw_GetAppName(), MB_ICONERROR );
         hb_retl( HB_FALSE );
         return;
      }

      iOldHeight = wvw_win->usTBHeight;

      if( ! hb_gt_wvw_AddTBButton( hWnd, szBitmap, uiBitmap, szLabel, iCommand, iBitmapType, fMap3Dcolors, wvw_win, fDropdown ) )
      {
         if( iBitmapType == 0 )
         {
            if( ! hb_gt_wvw_AddTBButton( hWnd, szBitmap, uiBitmap, szLabel, iCommand, 1, fMap3Dcolors, wvw_win, fDropdown ) )
            {
               hb_strfree( hLabel );
               MessageBox( NULL, TEXT( "Failed addbutton." ), hb_gt_wvw_GetAppName(), MB_ICONERROR );
               hb_retl( HB_FALSE );
               return;
            }
         }
         else
         {
            hb_strfree( hLabel );
            MessageBox( NULL, TEXT( "Failed addbutton." ), hb_gt_wvw_GetAppName(), MB_ICONERROR );
            hb_retl( HB_FALSE );
            return;
         }
      }

      hb_strfree( hLabel );

      hb_gt_wvw_TBinitSize( wvw_win, hWnd );

      if( wvw_win->usTBHeight != iOldHeight )
         hb_gt_wvw_ResetWindow( wvw_win );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_tbButtonCount( [nWinNum] )
 * returns number of buttons in toolbar on window nWinNum
 */
HB_FUNC( WVW_TBBUTTONCOUNT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      HWND hWnd = wvw_win->hToolBar;

      hb_retni( hWnd ? ( int ) SendMessage( hWnd, TB_BUTTONCOUNT, 0, 0 ) : 0 );
   }
}

/* wvw_tbDelButton( [nWinNum], nButton )
 * nButton is zero based index of button to delete
 * index=0 is the leftmost button
 * NOTE: button separator is indexed and deleteable too
 */
HB_FUNC( WVW_TBDELBUTTON )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HB_BOOL fResult = HB_FALSE;

   if( wvw_win )
   {
      int  iButton = hb_parnidef( 2, -1 );
      HWND hWnd    = wvw_win->hToolBar;

      if( hWnd && iButton >= 0 )
      {
         int iOldHeight = wvw_win->usTBHeight;

         if( SendMessage( hWnd, TB_DELETEBUTTON, ( WPARAM ) iButton, 0 ) )
         {
            hb_gt_wvw_TBinitSize( wvw_win, hWnd );

            if( wvw_win->usTBHeight != iOldHeight )
               hb_gt_wvw_ResetWindow( wvw_win );

            fResult = HB_TRUE;
         }
      }
   }

   hb_retl( fResult );
}

/* wvw_tbGetButtonRect( [nWinNum], nButton )
 * return an array {nRowStart, nColStart, nRowStop, nColStop}
 */
HB_FUNC( WVW_TBGETBUTTONRECT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int  iButton = hb_parnidef( 2, -1 );
      HWND hWnd    = wvw_win->hToolBar;
      RECT rc;

      if( hWnd && iButton >= 0 && SendMessage( hWnd, TB_GETRECT, ( WPARAM ) iButton, ( LPARAM ) &rc ) )
      {
         PHB_ITEM aXY = hb_itemArrayNew( 4 );

         RECT rcRect = hb_gt_wvw_GetColRowFromXYRect( wvw_win, rc );

         hb_arraySetNL( aXY, 1, HB_MAX( 0, rcRect.top ) );
         hb_arraySetNL( aXY, 2, rcRect.left );
         hb_arraySetNL( aXY, 3, HB_MIN( wvw_win->ROWS - 1, rcRect.bottom ) );
         hb_arraySetNL( aXY, 4, rcRect.right );

         hb_itemReturnRelease( aXY );
      }
   }
}

/* wvw_tbEnableButton( [nWinNum], nButton, [lToggle] )
 * nButton is zero based index of button to enable/disable
 * index=0 is the leftmost button
 * NOTE: button separator is indexed too
 * returns .T. if successful
 */
HB_FUNC( WVW_TBENABLEBUTTON )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HB_BOOL fResult = HB_FALSE;

   if( wvw_win )
   {
      int  iButton = hb_parnidef( 2, -1 );
      HWND hWnd    = wvw_win->hToolBar;

      if( hWnd && iButton >= 0 )
      {
         int iCommand = hb_gt_wvw_IndexToCommand( hWnd, iButton );
         if( iCommand >= 0 )
         {
            int iOldHeight = wvw_win->usTBHeight;

            if( SendMessage( hWnd, TB_ENABLEBUTTON, ( WPARAM ) iCommand, ( LPARAM ) MAKELONG( ( BOOL ) hb_parldef( 3, HB_TRUE ) /* fEnable */, 0 ) ) )
            {
               hb_gt_wvw_TBinitSize( wvw_win, hWnd );

               if( wvw_win->usTBHeight != iOldHeight )
                  hb_gt_wvw_ResetWindow( wvw_win );

               fResult = HB_TRUE;
            }
         }
      }
   }

   hb_retl( fResult );
}

/* wvw_tbDestroy( [nWinNum] )
 * destroy toolbar for window nWinNum
 */
HB_FUNC( WVW_TBDESTROY )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win && wvw_win->hToolBar )
   {
      DestroyWindow( wvw_win->hToolBar );
      wvw_win->hToolBar   = NULL;
      wvw_win->usTBHeight = 0;

      hb_gt_wvw_ResetWindow( wvw_win );
   }
}

/* wvw_tbIndex2Cmd( [nWinNum], nIndex )
 * returns Command Id of button nIndex (0 based)
 * returns -1 if the button does not exist
 */
HB_FUNC( WVW_TBINDEX2CMD )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iCmd = hb_gt_wvw_IndexToCommand( wvw_win->hToolBar, hb_parni( 2 ) );

      if( iCmd > 0 )
      {
         hb_retni( iCmd );
         return;
      }
   }

   hb_retni( -1 );
}

/* wvw_tbCmd2Index( [nWinNum], nCmd )
 * returns Index (0 based) of button whose command id is nCmd
 * returns -1 if the button does not exist
 */
HB_FUNC( WVW_TBCMD2INDEX )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
      hb_retni( hb_gt_wvw_CommandToIndex( wvw_win->hToolBar, hb_parni( 2 ) ) );
}

HB_FUNC( WVW_TOOLBARADDBUTTONS )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win && HB_ISARRAY( 3 ) )
   {
      PHB_ITEM pArray   = hb_param( 3, HB_IT_ARRAY );
      int      iButtons = ( int ) hb_arrayLen( pArray );

      if( iButtons > 0 )
      {
         HWND hWndCtrl = ( HWND ) HB_PARHANDLE( 2 );

         TBBUTTON * tb   = ( TBBUTTON * ) hb_xgrab( iButtons * sizeof( TBBUTTON ) );
         void **    hStr = ( void ** ) hb_xgrab( iButtons * sizeof( void * ) );

         int nCount;
         int iOldHeight = wvw_win->usTBHeight;

         SetWindowLong( hWndCtrl, GWL_STYLE, GetWindowLong( hWndCtrl, GWL_STYLE ) | TBSTYLE_TOOLTIPS | TBSTYLE_FLAT );
         SendMessage( hWndCtrl, TB_BUTTONSTRUCTSIZE, sizeof( TBBUTTON ), 0 );

         for( nCount = 0; nCount < iButtons; ++nCount )
         {
            PHB_ITEM pTemp = hb_arrayGetItemPtr( pArray, nCount + 1 );

            tb[ nCount ].idCommand = hb_arrayGetNI( pTemp, 2 );
            tb[ nCount ].fsState   = ( BYTE ) hb_arrayGetNI( pTemp, 3 );
            tb[ nCount ].fsStyle   = ( BYTE ) hb_arrayGetNI( pTemp, 4 );
            tb[ nCount ].dwData    = hb_arrayGetNI( pTemp, 5 );
            tb[ nCount ].iString   = ( INT_PTR ) HB_ARRAYGETSTR( pTemp, 6, &hStr[ nCount ], NULL );
         }

         SendMessage( hWndCtrl, TB_ADDBUTTONS, ( WPARAM ) iButtons, ( LPARAM ) ( LPTBBUTTON ) tb );
         SendMessage( hWndCtrl, TB_AUTOSIZE, 0, 0 );

         hb_gt_wvw_TBinitSize( wvw_win, hWndCtrl );

         if( wvw_win->usTBHeight != iOldHeight )
            hb_gt_wvw_ResetWindow( wvw_win );

         hb_xfree( tb );

         for( nCount = 0; nCount < iButtons; ++nCount )
            hb_strfree( hStr[ nCount ] );

         hb_xfree( hStr );
      }
   }
}
