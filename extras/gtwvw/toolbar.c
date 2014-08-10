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
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   HWND hWndParent = wvw_win->hWnd;
   HWND hWnd;
   int  iMaxTextRows = hb_parl( 2 ) ? 1 : 0;

#if 0
   DWORD dwStyle = ( DWORD ) hb_parnidef( 3, TBSTYLE_FLAT | TBSTYLE_TOOLTIPS );
#endif
   DWORD dwStyle = ( DWORD ) hb_parnidef( 3, TBSTYLE_ALTDRAG | TBSTYLE_FLAT | TBSTYLE_TOOLTIPS | TBSTYLE_TRANSPARENT | TBSTYLE_WRAPABLE );

   int iSystemBitmap = hb_parnidef( 4, 1 );
   int iImageWidth   = iSystemBitmap == 0 && HB_ISNUM( 5 ) ? hb_parni( 5 ) : -1;
   int iImageHeight  = iSystemBitmap == 0 && HB_ISNUM( 6 ) ? hb_parni( 6 ) : -1;

   InitCommonControls();

   if( wvw_win->hToolBar != NULL )
   {
      hb_retnl( 0 );
      return;
   }

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

   hWnd = CreateToolbarEx( hWndParent,
                           WS_CHILD | WS_VISIBLE | dwStyle,
                           WVW_ID_BASE_TOOLBAR + nWin,
                           0,
                           hb_gt_wvw_GetWvwData()->hInstance,
                           0,
                           NULL,
                           0,
                           0,
                           0,
                           iImageWidth,
                           iImageHeight,
                           sizeof( TBBUTTON ) );

   if( hWnd == NULL )
   {
      MessageBox( NULL, TEXT( "Failed CreateToolbarEx()" ), hb_gt_wvw_GetAppName(), MB_ICONERROR );
      hb_retnl( 0 );
      return;
   }

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

   SendMessage( hWnd, TB_SETMAXTEXTROWS, ( WPARAM ) iMaxTextRows, 0 );

   if( hWnd )
   {
      hb_stornl( wvw_win->iStartStdBitmap, 7 );
      hb_stornl( wvw_win->iStartViewBitmap, 8 );
      hb_stornl( wvw_win->iStartHistBitmap, 9 );

      hb_gt_wvw_TBinitSize( wvw_win, hWnd );

      wvw_win->hToolBar = hWnd;

      hb_gt_wvw_ResetWindow( nWin );
   }
   else
   {
      hb_stornl( 0, 7 );
      hb_stornl( 0, 8 );
      hb_stornl( 0, 9 );
   }

   hb_retnint( ( HB_PTRDIFF ) hWnd );
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
 *            RGB(192,192,192) --> COLOR_3DFACE   ("transparent")
 *            RGB(128,128,128) --> COLOR_3DSHADOW
 *            RGB(223,223,223) --> COLOR_3DLIGHT
 *         This might be desirable to have transparent effect.
 *         LIMITATION: this will work on 256 colored bitmaps only
 */
HB_FUNC( WVW_TBADDBUTTON )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   int          iCommand     = hb_parni( 2 );
   HB_UINT      uiBitmap     = ( HB_UINT ) hb_parni( 3 );
   const char * szBitmap     = hb_parc( 3 );
   const char * szLabel      = hb_parcx( 4 );
   int          iBitmapType  = hb_parni( 5 );
   HB_BOOL      bMap3Dcolors = hb_parl( 6 );
   HB_BOOL      bDropdown    = hb_parl( 7 );
   USHORT       usOldHeight;

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

   if( strlen( szLabel ) > WVW_TB_LABELMAXLENGTH )
   {
      MessageBox( NULL, TEXT( "Cannot addbutton, label too long." ), hb_gt_wvw_GetAppName(), MB_ICONERROR );
      hb_retl( HB_FALSE );
      return;
   }

   usOldHeight = wvw_win->usTBHeight;

   if( ! hb_gt_wvw_AddTBButton( hWnd, szBitmap, uiBitmap, szLabel, iCommand, iBitmapType, bMap3Dcolors, wvw_win, bDropdown ) )
   {
      if( iBitmapType == 0 )
      {
         if( ! hb_gt_wvw_AddTBButton( hWnd, szBitmap, uiBitmap, szLabel, iCommand, 1, bMap3Dcolors, wvw_win, bDropdown ) )
         {
            MessageBox( NULL, TEXT( "Failed addbutton." ), hb_gt_wvw_GetAppName(), MB_ICONERROR );
            hb_retl( HB_FALSE );
            return;
         }
      }
      else
      {
         MessageBox( NULL, TEXT( "Failed addbutton." ), hb_gt_wvw_GetAppName(), MB_ICONERROR );
         hb_retl( HB_FALSE );
         return;
      }
   }

   hb_gt_wvw_TBinitSize( wvw_win, hWnd );

   if( wvw_win->usTBHeight != usOldHeight )
      hb_gt_wvw_ResetWindow( nWin );

   hb_retl( HB_TRUE );
}

/* wvw_tbButtonCount( [nWinNum] )
 * returns number of buttons in toolbar on window nWinNum
 */
HB_FUNC( WVW_TBBUTTONCOUNT )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   HWND      hWnd    = wvw_win->hToolBar;

   if( hWnd )
      hb_retni( ( int ) SendMessage( hWnd, TB_BUTTONCOUNT, 0, 0 ) );
   else
      hb_retni( 0 );
}

/* wvw_tbDelButton( [nWinNum], nButton )
 * nButton is zero based index of button to delete
 * index=0 is the leftmost button
 * NOTE: button separator is indexed and deleteable too
 */
HB_FUNC( WVW_TBDELBUTTON )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );
   int       iButton = hb_parnidef( 2, -1 );
   HWND      hWnd    = wvw_win->hToolBar;

   if( hWnd && iButton >= 0 )
   {
      USHORT usOldHeight = wvw_win->usTBHeight;

      if( SendMessage( hWnd, TB_DELETEBUTTON, ( WPARAM ) iButton, 0 ) )
      {
         hb_gt_wvw_TBinitSize( wvw_win, hWnd );

         if( wvw_win->usTBHeight != usOldHeight )
            hb_gt_wvw_ResetWindow( nWin );

         hb_retl( HB_TRUE );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_tbGetButtonRect( [nWinNum], nButton )
 * return an array {nRowStart, nColStart, nRowStop, nColStop}
 */
HB_FUNC( WVW_TBGETBUTTONRECT )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   int       iButton = hb_parnidef( 2, -1 );
   HWND      hWnd    = wvw_win->hToolBar;
   RECT      rc;

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


/* wvw_tbEnableButton( [nWinNum], nButton, [lToggle] )
 * nButton is zero based index of button to enable/disable
 * index=0 is the leftmost button
 * NOTE: button separator is indexed too
 * returns .T. if successful
 */
HB_FUNC( WVW_TBENABLEBUTTON )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );
   int       iButton = hb_parnidef( 2, -1 );
   HWND      hWnd    = wvw_win->hToolBar;

   if( hWnd && iButton >= 0 )
   {
      int iCommand = hb_gt_wvw_IndexToCommand( hWnd, iButton );
      if( iCommand >= 0 )
      {
         USHORT  usOldHeight = wvw_win->usTBHeight;
         HB_BOOL bEnable     = hb_parldef( 3, HB_TRUE );

         if( SendMessage( hWnd, TB_ENABLEBUTTON, ( WPARAM ) iCommand, ( LPARAM ) MAKELONG( ( BOOL ) bEnable, 0 ) ) )
         {
            hb_gt_wvw_TBinitSize( wvw_win, hWnd );

            if( wvw_win->usTBHeight != usOldHeight )
               hb_gt_wvw_ResetWindow( nWin );

            hb_retl( HB_TRUE );
         }
         else
            hb_retl( HB_FALSE );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_tbDestroy( [nWinNum] )
 * destroy toolbar for window nWinNum
 */
HB_FUNC( WVW_TBDESTROY )
{
   HB_UINT   nWin    = WVW_WHICH_WINDOW;
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   if( wvw_win->hToolBar )
   {
      DestroyWindow( wvw_win->hToolBar );
      wvw_win->hToolBar   = NULL;
      wvw_win->usTBHeight = 0;

      hb_gt_wvw_ResetWindow( nWin );
   }
}

/* wvw_tbIndex2Cmd( [nWinNum], nIndex )
 * returns Command Id of button nIndex (0 based)
 * returns -1 if the button does not exist
 */
HB_FUNC( WVW_TBINDEX2CMD )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   HWND      hWnd    = wvw_win->hToolBar;
   int       iCmd    = hb_gt_wvw_IndexToCommand( hWnd, hb_parni( 2 ) );

   hb_retni( iCmd > 0 ? iCmd : -1 );
}

/* wvw_tbCmd2Index( [nWinNum], nCmd )
 * returns Index (0 based) of button whose command id is nCmd
 * returns -1 if the button does not exist
 */
HB_FUNC( WVW_TBCMD2INDEX )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   HWND      hWnd    = wvw_win->hToolBar;

   hb_retni( hb_gt_wvw_CommandToIndex( hWnd, hb_parni( 2 ) ) );
}
