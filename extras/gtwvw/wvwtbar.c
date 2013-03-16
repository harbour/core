/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw toolbar and tooltips functions
 * GTWVW is initially created based on:
 *
 * =Id: gtwvt.c,v 1.60 2004-01-26 08:14:07 vouchcac Exp =
 *
 * Harbour Project source code:
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
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_gt_wvw_Tone()
 *
 * See COPYING.txt for licensing terms.
 *
 * www - http://harbour-project.org
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
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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

/*WVW_TBCreate([nWinNum], lDisplayText, nStyle, nSystemBitmap, nImageWidth, nImageHeight)
   *creates a toolbar at the top (no button initially)
   *lDisplayText==.f. button's string is used as tooltips (default)
   *nStyle: toolbar style, defaults to TBSTYLE_FLAT | TBSTYLE_TOOLTIPS
   *nSystemBitmap: 0:none, 1:small, 2:large (defaults: 1)
 *               small=16x16 large=24x24
 * nImageWidth/Height are in effect only if nSystemBitmap==0
 */
HB_FUNC( WVW_TBCREATE )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   HWND       hWndParent  = pWindowData->hWnd;
   HWND       hWndTB;
   int        iMaxTextRows = ( int ) ( HB_ISNIL( 2 ) ? 0 : ( hb_parl( 2 ) ? 1 : 0 ) );
//   DWORD dwStyle = (DWORD) ( HB_ISNIL( 3 ) ? TBSTYLE_FLAT | TBSTYLE_TOOLTIPS : hb_parni( 3 ) );
   DWORD dwStyle = ( DWORD ) ( HB_ISNIL( 3 ) ? TBSTYLE_ALTDRAG | TBSTYLE_FLAT | TBSTYLE_TOOLTIPS | TBSTYLE_TRANSPARENT | TBSTYLE_WRAPABLE : hb_parnl( 3 ) );

   int iSystemBitmap = ( int ) ( HB_ISNIL( 4 ) ? 1 : hb_parni( 4 ) );
   int iImageWidth   = ( int ) ( iSystemBitmap == 0 && HB_ISNUM( 5 ) ? hb_parni( 5 ) : -1 );
   int iImageHeight  = ( int ) ( iSystemBitmap == 0 && HB_ISNUM( 6 ) ? hb_parni( 6 ) : -1 );
   TBADDBITMAP tbab  = { 0 };

   InitCommonControls();

   if( pWindowData->hToolBar != NULL )
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
            break;
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
            break;
      }
   }

   hWndTB = CreateToolbarEx( hWndParent,
                             WS_CHILD | WS_VISIBLE | dwStyle,
                             WVW_ID_BASE_TOOLBAR + usWinNum,
                             0,
                             hb_getWvwData()->hInstance,
                             0,
                             NULL,
                             0,
                             0,
                             0,
                             iImageWidth,
                             iImageHeight,
                             sizeof( TBBUTTON ) );

   if( hWndTB == NULL )
   {
      MessageBox( NULL, TEXT( "Failed CreateToolbarEx..." ),
                  hb_gt_wvw_GetAppName(), MB_ICONERROR );
      hb_retnl( 0 );
   }

   pWindowData->tbOldProc = ( WNDPROC ) SetWindowLongPtr( hWndTB,
                                                          GWLP_WNDPROC, ( LONG_PTR ) hb_gt_wvwTBProc );

   if( iSystemBitmap > 0 )
   {
      tbab.hInst = HINST_COMMCTRL;

      tbab.nID = iSystemBitmap == 1 ? IDB_STD_SMALL_COLOR : IDB_STD_LARGE_COLOR;
      pWindowData->iStartStdBitmap = SendMessage( hWndTB, TB_ADDBITMAP, ( WPARAM ) 0, ( WPARAM ) &tbab );

      tbab.nID = iSystemBitmap == 1 ? IDB_VIEW_SMALL_COLOR : IDB_VIEW_LARGE_COLOR;
      pWindowData->iStartViewBitmap = SendMessage( hWndTB, TB_ADDBITMAP, ( WPARAM ) 0, ( WPARAM ) &tbab );

      tbab.nID = iSystemBitmap == 1 ? IDB_HIST_SMALL_COLOR : IDB_HIST_LARGE_COLOR;
      pWindowData->iStartHistBitmap = SendMessage( hWndTB, TB_ADDBITMAP, ( WPARAM ) 0, ( WPARAM ) &tbab );
   }
   else
   {
      pWindowData->iStartStdBitmap  = 0;
      pWindowData->iStartViewBitmap = 0;
      pWindowData->iStartHistBitmap = 0;
   }

   pWindowData->iTBImgWidth  = iImageWidth;
   pWindowData->iTBImgHeight = iImageHeight;

   SendMessage( hWndTB, TB_SETMAXTEXTROWS, ( WPARAM ) iMaxTextRows, ( LPARAM ) 0 );

   if( hWndTB )
   {

      hb_stornl( pWindowData->iStartStdBitmap, 7 );
      hb_stornl( pWindowData->iStartViewBitmap, 8 );
      hb_stornl( pWindowData->iStartHistBitmap, 9 );

      hb_gt_wvwTBinitSize( pWindowData, hWndTB );

      pWindowData->hToolBar = hWndTB;

      hb_gt_wvwResetWindow( usWinNum );
   }

   hb_retnl( ( LONG ) hWndTB );
}

/*WVW_TBAddButton([nWinNum], nCommand, xBitmap, cLabel, nBitmapType,;
 *                           lMap3Dcolors, lDropdown)
 * adds one button on the right of existing buttons
 * xBitmap:
 * nBitmap is resource id. or use cBitmap as bitmap file name.
 * (bitmap from resources cannot have > 256 colors)
 *
 * cLabel: if lDisplayText, it will be displayed below the bitmap
 *      otherwise it will be used as tooltip
 * nBitmapType: 0:custom, 1:system std bitmap, 2:system view bitmap, 3:system hist bitmap
 * lMap3Dcolors: defaults to .f.
 *         (meaningfull for custom bitmap only)
 *         if .t. the following color mapping will be performed:
 *            RGB(192,192,192) --> COLOR_3DFACE   ("transparent")
 *            RGB(128,128,128) --> COLOR_3DSHADOW
 *            RGB(223,223,223) --> COLOR_3DLIGHT
 *         This might be desirable to have transparent effect.
 *         LIMITATION: this will work on 256 colored bitmaps only
 */

HB_FUNC( WVW_TBADDBUTTON )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   int        iCommand    = HB_ISNIL( 2 ) ? 0 : hb_parni( 2 );

   char * szBitmap = HB_ISCHAR( 3 ) ? ( char * ) hb_parcx( 3 ) : NULL;
   UINT   uiBitmap = HB_ISNUM( 3 ) ? ( UINT ) hb_parni( 3 ) : 0;

   char * szLabel      = HB_ISNIL( 4 ) ? ( char * ) "" : ( char * ) hb_parcx( 4 );
   int    iBitmapType  = HB_ISNIL( 5 ) ? 0 : ( int ) hb_parni( 5 );
   BOOL   bMap3Dcolors = HB_ISLOG( 6 ) ? hb_parl( 6 ) : FALSE;
   BOOL   bDropdown    = HB_ISLOG( 7 ) ? hb_parl( 7 ) : FALSE;
   HWND   hWndTB;
   USHORT usOldHeight;

   hWndTB = pWindowData->hToolBar;
   if( hWndTB == NULL )
   {
      hb_retl( FALSE );
      return;
   }

   if( iCommand >= WVW_ID_BASE_PUSHBUTTON )
   {
      MessageBox( NULL, TEXT( "Toolbar button Command Id too high. Potential conflict with pushbutton" ),
                  hb_gt_wvw_GetAppName(), MB_ICONERROR );
      hb_retl( FALSE );
      return;
   }

   if( strlen( szLabel ) > WVW_TB_LABELMAXLENGTH )
   {
      MessageBox( NULL, TEXT( "Cannot addbutton, Label too long..." ),
                  hb_gt_wvw_GetAppName(), MB_ICONERROR );
      hb_retl( FALSE );
      return;
   }

   usOldHeight = pWindowData->usTBHeight;

   if( ! AddTBButton( hWndTB, szBitmap, uiBitmap, szLabel, iCommand, iBitmapType, bMap3Dcolors, pWindowData, bDropdown ) )
   {
      if( iBitmapType == 0 )
      {
         if( ! AddTBButton( hWndTB, szBitmap, uiBitmap, szLabel, iCommand, 1, bMap3Dcolors, pWindowData, bDropdown ) )
         {
            MessageBox( NULL, TEXT( "Failed addbutton..." ),
                        hb_gt_wvw_GetAppName(), MB_ICONERROR );
            hb_retl( FALSE );
            return;
         }
      }
      else
      {
         MessageBox( NULL, TEXT( "Failed addbutton..." ),
                     hb_gt_wvw_GetAppName(), MB_ICONERROR );
         hb_retl( FALSE );
         return;
      }
   }

   hb_gt_wvwTBinitSize( pWindowData, hWndTB );

   if( pWindowData->usTBHeight != usOldHeight )
      hb_gt_wvwResetWindow( usWinNum );

   hb_retl( TRUE );
}

/*WVW_TBButtonCount([nWinNum])
   *returns number of buttons in toolbar on window nWinNum
 */
HB_FUNC( WVW_TBBUTTONCOUNT )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   HWND       hWndTB;

   hWndTB = pWindowData->hToolBar;
   if( hWndTB == NULL )
   {
      hb_retni( 0 );
      return;
   }

   hb_retni( SendMessage( hWndTB, TB_BUTTONCOUNT, ( WPARAM ) 0, ( LPARAM ) 0 ) );
}

/*WVW_TBDelButton([nWinNum], nButton)
   *nButton is zero based index of button to delete
   *index=0 is the leftmost button
   *NOTE: button separator is indexed and deleteable too
 */
HB_FUNC( WVW_TBDELBUTTON )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   int        iButton     = HB_ISNUM( 2 ) ? hb_parni( 2 ) : -1;
   HWND       hWndTB;
   USHORT     usOldHeight;

   hWndTB = pWindowData->hToolBar;
   if( hWndTB == NULL || iButton < 0 )
   {
      hb_retl( FALSE );
      return;
   }

   usOldHeight = pWindowData->usTBHeight;

   if( ! SendMessage( hWndTB, TB_DELETEBUTTON, ( WPARAM ) iButton, ( LPARAM ) 0 ) )
   {
      hb_retl( FALSE );
      return;
   }

   hb_gt_wvwTBinitSize( pWindowData, hWndTB );

   if( pWindowData->usTBHeight != usOldHeight )
      hb_gt_wvwResetWindow( usWinNum );

   hb_retl( TRUE );
}

/* WVW_TBGetButtonRect([nWinNum], nButton)
 * return an array {nRowStart, nColStart, nRowStop, nColStop}
 */
HB_FUNC( WVW_TBGETBUTTONRECT )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   int        iButton     = HB_ISNUM( 2 ) ? hb_parni( 2 ) : -1;
   HWND       hWndTB;
   RECT       rc;
   RECT       rcRect = { 0 };

   PHB_ITEM aXY = hb_itemNew( NULL );
   PHB_ITEM temp;

   hWndTB = pWindowData->hToolBar;
   if( hWndTB == NULL || iButton < 0 || ! SendMessage( hWndTB, TB_GETRECT, ( WPARAM ) iButton, ( LPARAM ) &rc ) )
   {
      hb_itemReturnRelease( aXY );
      return;
   }

   temp = hb_itemNew( NULL );

   hb_arrayNew( aXY, 4 );

   rcRect = hb_gt_wvwGetColRowFromXYRect( pWindowData, rc );
   hb_arraySetForward( aXY, 1, hb_itemPutNL( temp, max( 0, rcRect.top ) ) );
   hb_arraySetForward( aXY, 2, hb_itemPutNL( temp, rcRect.left ) );

   hb_arraySetForward( aXY, 3, hb_itemPutNL( temp, min( pWindowData->ROWS - 1, rcRect.bottom ) ) );
   hb_arraySetForward( aXY, 4, hb_itemPutNL( temp, rcRect.right ) );
   hb_itemRelease( temp );


   hb_itemReturnRelease( aXY );
}


/*WVW_TBEnableButton([nWinNum], nButton, [lToggle])
   *nButton is zero based index of button to enable/disable
   *index=0 is the leftmost button
   *NOTE: button separator is indexed too
   *returns .t. if successful
 */
HB_FUNC( WVW_TBENABLEBUTTON )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   int        iButton     = HB_ISNUM( 2 ) ? hb_parni( 2 ) : -1;
   BOOL       bEnable     = HB_ISLOG( 3 ) ? hb_parl( 3 ) : TRUE;
   int        iCommand;
   HWND       hWndTB;
   USHORT     usOldHeight;

   hWndTB = pWindowData->hToolBar;
   if( hWndTB == NULL || iButton < 0 )
   {
      hb_retl( FALSE );
      return;
   }

   iCommand = IndexToCommand( hWndTB, iButton );
   if( iCommand < 0 )
   {
      hb_retl( FALSE );
      return;
   }

   usOldHeight = pWindowData->usTBHeight;

   if( ! SendMessage( hWndTB, TB_ENABLEBUTTON, ( WPARAM ) iCommand, ( LPARAM ) MAKELONG( bEnable, 0 ) ) )
   {
      hb_retl( FALSE );
      return;
   }

   hb_gt_wvwTBinitSize( pWindowData, hWndTB );

   if( pWindowData->usTBHeight != usOldHeight )
      hb_gt_wvwResetWindow( usWinNum );

   hb_retl( TRUE );
}

/*WVW_TBdestroy( [nWinNum] )
   *destroy toolbar for window nWinNum
 */
HB_FUNC( WVW_TBDESTROY )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   if( ! ( pWindowData->hToolBar == NULL ) )
   {
      DestroyWindow( pWindowData->hToolBar );
      pWindowData->hToolBar   = NULL;
      pWindowData->usTBHeight = 0;

      hb_gt_wvwResetWindow( usWinNum );
   }
}

/*WVW_TBINDEX2CMD([nWinNum], nIndex)
   *returns Command Id of button nIndex (0 based)
   *returns -1 if the button does not exist
 */
HB_FUNC( WVW_TBINDEX2CMD )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   HWND       hWndTB      = pWindowData->hToolBar;
   int        iIndex      = hb_parni( 2 );
   int        iCmd        = IndexToCommand( hWndTB, iIndex );

   hb_retni( ( int ) ( iCmd > 0 ? iCmd : -1 ) );
}

/*WVW_TBCmd2Index([nWinNum], nCmd)
   *returns Index (0 based) of button whose command id is nCmd
   *returns -1 if the button does not exist
 */
HB_FUNC( WVW_TBCMD2INDEX )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   HWND       hWndTB      = pWindowData->hToolBar;
   int        iCmd        = hb_parni( 2 );

   hb_retni( CommandToIndex( hWndTB, iCmd ) );
}


/* TOOLBAR ends                                                      */


#if _WIN32_IE > 0x400


/*                                                                   */
/*                              Tooltips                             */
/*                                                                   */


/*WVW_SetToolTopActive([nWinNum], [lToggle]) */
HB_FUNC( WVW_SETTOOLTIPACTIVE )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   BOOL bActive = pWindowData->bToolTipActive;

   if( ! HB_ISNIL( 2 ) )
   {

      if( hb_parl( 2 ) && ( pWindowData->hWndTT == NULL ) )
         hb_gt_wvwCreateToolTipWindow( pWindowData );

      pWindowData->bToolTipActive = hb_parl( 2 );
   }

   hb_retl( bActive );
}


/*                                                                        */
/*   Wvw_SetToolTip( [nWinNum], nTop, nLeft, nBottom, nRight, cToolText ) */
/*                                                                        */
HB_FUNC( WVW_SETTOOLTIP )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   TOOLINFO ti = { 0 };
   POINT    xy = { 0 };
   int      iTop, iLeft, iBottom, iRight;

   USHORT usTop    = hb_parni( 2 ),
          usLeft   = hb_parni( 3 ),
          usBottom = hb_parni( 4 ),
          usRight  = hb_parni( 5 );

   if( ! pWindowData->bToolTipActive )
      return;

   if( hb_getWvwData()->s_bMainCoordMode )
      hb_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   ti.cbSize = sizeof( TOOLINFO );
   ti.hwnd   = pWindowData->hWnd;
   ti.uId    = WVW_ID_BASE_TOOLTIP + usWinNum;

   if( SendMessage( pWindowData->hWndTT, TTM_GETTOOLINFO, 0, ( LPARAM ) &ti ) )
   {
      xy    = hb_gt_wvwGetXYFromColRow( pWindowData, usLeft, usTop );
      iTop  = xy.y;
      iLeft = xy.x;

      xy      = hb_gt_wvwGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );
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


HB_FUNC( WVW_SETTOOLTIPTEXT )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
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
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   RECT       rc = { 0 };

   rc.left   = hb_parni( 3 );
   rc.top    = hb_parni( 2 );
   rc.right  = hb_parni( 5 );
   rc.bottom = hb_parni( 4 );

   SendMessage( pWindowData->hWndTT, TTM_SETMARGIN, 0, ( LPARAM ) &rc );
}

HB_FUNC( WVW_SETTOOLTIPWIDTH )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   int iTipWidth = SendMessage( pWindowData->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0 );

   if( HB_ISNUM( 2 ) )
      SendMessage( pWindowData->hWndTT, TTM_SETMAXTIPWIDTH, 0, ( LPARAM ) ( int ) hb_parni( 2 ) );

   hb_retni( iTipWidth );
}


HB_FUNC( WVW_SETTOOLTIPBKCOLOR )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   COLORREF cr = SendMessage( pWindowData->hWndTT, TTM_GETTIPBKCOLOR, 0, 0 );

   if( HB_ISNUM( 2 ) )
      SendMessage( pWindowData->hWndTT, TTM_SETTIPBKCOLOR, ( WPARAM ) ( COLORREF ) hb_parnl( 2 ), 0 );
   hb_retnl( ( COLORREF ) cr );
}


HB_FUNC( WVW_SETTOOLTIPTEXTCOLOR )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   COLORREF cr = SendMessage( pWindowData->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0 );

   if( HB_ISNUM( 2 ) )
      SendMessage( pWindowData->hWndTT, TTM_SETTIPTEXTCOLOR, ( WPARAM ) ( COLORREF ) hb_parnl( 2 ), 0 );
   hb_retnl( ( COLORREF ) cr );
}


HB_FUNC( WVW_SETTOOLTIPTITLE )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   int        iIcon;

   if( ! HB_ISNIL( 3 ) )
   {
      iIcon = HB_ISNIL( 2 ) ? 0 : hb_parni( 2 );
      if( iIcon > 3 )
         iIcon = 0;
      SendMessage( pWindowData->hWndTT, TTM_SETTITLE, ( WPARAM ) iIcon, ( LPARAM ) hb_parcx( 3 ) );
   }
}


HB_FUNC( WVW_GETTOOLTIPWIDTH )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   hb_retni( SendMessage( pWindowData->hWndTT, TTM_GETMAXTIPWIDTH, 0, 0 ) );
}


HB_FUNC( WVW_GETTOOLTIPBKCOLOR )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   hb_retnl( ( COLORREF ) SendMessage( pWindowData->hWndTT, TTM_GETTIPBKCOLOR, 0, 0 ) );
}


HB_FUNC( WVW_GETTOOLTIPTEXTCOLOR )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   hb_retnl( ( COLORREF ) SendMessage( pWindowData->hWndTT, TTM_GETTIPTEXTCOLOR, 0, 0 ) );
}

#endif
