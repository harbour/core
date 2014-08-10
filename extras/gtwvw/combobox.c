/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw pushbutton/ combobox functions
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

/* wvw_cbCreate( [nWinNum], nTop, nLeft, nWidth, aText, bBlock, nListLines, ;
 *                          nReserved, nKbdType, aOffset, hControl )
 * create combobox (drop-down list, no editbox) for window nWinNum
 * nTop: row of top/left corner (in character unit)
 * nLeft: col of top/left corner (in character unit)
 * nWidth: width of combobox (in character unit)
 * aText: array of drop-down list members, default = {"empty"}
 *      eg. {"yes","no"}
 * bBlock: codeblock to execute on these events:
 *        event=CBN_SELCHANGE(1): user changes selection
 *                      (not executed if selection
 *                      is changed programmatically)
 *         event=CBN_SETFOCUS
 *         event=CBN_KILLFOCUS
 *         This codeblock will be evaluated with these parameters:
 *         nWinNum: window number
 *         nCBid  : combobox id
 *         nType  : event type (CBN_SELCHANGE/CBN_SETFOCUS/CBN_KILLFOCUS supported)
 *         nIndex : index of the selected list item (0 based)
 * nListLines: number of lines for list items, default = 3
 *            (will be automatically truncated if it's > Len(aText))
 * nReserved: reserved for future (this parm is now ignored)
 *
 * nKbdType: WVW_CB_KBD_STANDARD (0): similar to standard windows convention
 *            ENTER/ESC: will kill focus from combobox
 *          WVW_CB_KBD_CLIPPER (1):
 *            ENTER: drop (show) the list box
 *            UP/DOWN/TAB/SHIFTTAB/ESC: kill focus
 * default is WVW_CB_KBD_STANDARD (0)
 *
 * aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of combobox.
 *         defaults: {-2,-2,+2,+2}
 *         NOTES: the third element (y2) is actually ignored.
 *                height of combobox is always 1 char height
 *                (see also wvw_cbSetFont())
 *
 * returns control id of newly created combobox of windows nWinNum
 * returns 0 if failed
 *
 * example:
 */

HB_FUNC( WVW_CBCREATE )
{
   WVW_GLOB * wvw      = hb_gt_wvw_GetWvwData();
   UINT       usWinNum = WVW_WHICH_WINDOW;
   WVW_WIN *  wvw_win  = hb_gt_wvw_GetWindowsData( usWinNum );

   HWND hWndParent = wvw_win->hWnd;
   HWND hWndCB;
/* LONG cnt; */
   LONG numofchars;
   LONG avgwidth;
   LONG LongComboWidth, NewLongComboWidth;
/* RECT r; */
   HFONT hFont = hb_gt_wvw_GetFont( wvw_win->fontFace, 10, wvw_win->fontWidth, wvw_win->fontWeight, wvw_win->fontQuality, wvw_win->CodePage );

   POINT xy;
   int   iTop, iLeft, iBottom, iRight;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;

   UINT   uiCBid;
   USHORT usWidth      = ( USHORT ) hb_parni( 4 );
   USHORT usTop        = ( USHORT ) hb_parni( 2 ),
          usLeft       = ( USHORT ) hb_parni( 3 ),
          usBottom     = usTop,
          usRight      = usLeft + usWidth - 1;
   USHORT usNumElement = ( USHORT ) ( HB_ISARRAY( 5 ) ? hb_arrayLen( hb_param( 5, HB_IT_ARRAY ) ) : 0 );
   USHORT usListLines  = ( USHORT ) hb_parnidef( 7, 3 );
   BYTE   byCharHeight = hb_gt_wvw_LineHeight( wvw_win );

   /* in the future combobox type might be selectable by 8th parameter */
   int  iStyle   = CBS_DROPDOWNLIST | WS_VSCROLL;
   BYTE bKbdType = ( BYTE ) hb_parnidef( 9, WVW_CB_KBD_STANDARD );

   if( wvw_win->hCBfont == NULL )
   {
      wvw_win->hCBfont = CreateFontIndirect( &wvw->lfCB );
      if( wvw_win->hCBfont == NULL )
      {
         hb_retnl( 0 );
         return;
      }
   }

   LongComboWidth = 0;
   iOffTop        = HB_ISARRAY( 10 ) ? hb_parvni( 10, 1 ) : 0;
   iOffLeft       = HB_ISARRAY( 10 ) ? hb_parvni( 10, 2 ) : 0;

   iOffBottom = usListLines;
   iOffRight  = HB_ISARRAY( 10 ) ? hb_parvni( 10, 4 ) : 0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

   xy.y -= wvw_win->byLineSpacing;

   iBottom = xy.y - 1 + ( iOffBottom * byCharHeight );
   iRight  = xy.x - 1 + iOffRight;

   uiCBid = hb_gt_wvw_LastControlId( usWinNum, WVW_CONTROL_COMBOBOX );
   if( uiCBid == 0 )
      uiCBid = WVW_ID_BASE_COMBOBOX;
   else
      uiCBid++;

   InitCommonControls();

   hWndCB = CreateWindowEx(
      0L,
      "COMBOBOX",
      NULL,
      WS_CHILD | WS_VISIBLE | ( DWORD ) iStyle,
      iLeft,
      iTop,
      iRight - iLeft + 1,
      iBottom - iTop + 1,
      hWndParent,
      ( HMENU ) ( HB_PTRDIFF ) uiCBid,
      wvw->hInstance,
      NULL );

   if( hWndCB )
   {
      RECT    rXB, rOffXB;
      WNDPROC OldProc;
      USHORT  i;
      TCHAR   szDefault[] = TEXT( "empty" );

      SendMessage( hWndCB, WM_SETREDRAW, ( WPARAM ) TRUE, 0 );

      if( usNumElement == 0 )
      {
         if( SendMessage( hWndCB, CB_ADDSTRING, 0, ( LPARAM ) ( LPCTSTR ) szDefault ) < 0 )
         {
            /* ignore failure */
         }
      }
      else
         for( i = 1; i <= usNumElement; i++ )
         {
            if( SendMessage( hWndCB, CB_ADDSTRING, 0, ( LPARAM ) ( LPCTSTR ) hb_parvcx( 5, i ) ) < 0 )
            {
               /* ignore failure */
            }
            else
            {
               numofchars = ( int ) SendMessage( hWndCB, CB_GETLBTEXTLEN, i - 1, 0 );
               if( numofchars > LongComboWidth )
                  LongComboWidth = numofchars;

            }
         }

      SendMessage( hWndCB, CB_SETCURSEL, 0, 0 );
      SendMessage( hWndCB, CB_SETEXTENDEDUI, ( WPARAM ) TRUE, 0 );

      avgwidth = hb_gt_wvw_GetFontDialogUnits( hWndParent, hFont );
      NewLongComboWidth = ( LongComboWidth - 2 ) * avgwidth;
      SendMessage( hWndCB, CB_SETDROPPEDWIDTH, ( WPARAM ) NewLongComboWidth + 100 /* LongComboWidth + 100 */, 0 );

      rXB.top    = usTop;
      rXB.left   = usLeft;
      rXB.bottom = usBottom;
      rXB.right  = usRight;

      rOffXB.top    = iOffTop;
      rOffXB.left   = iOffLeft;
      rOffXB.bottom = iOffBottom;
      rOffXB.right  = iOffRight;

      hb_gt_wvw_AddControlHandle( usWinNum, WVW_CONTROL_COMBOBOX, hWndCB, uiCBid, hb_param( 6, HB_IT_EVALITEM ), rXB, rOffXB, ( byte ) bKbdType );

      OldProc = ( WNDPROC ) SetWindowLongPtr( hWndCB, GWLP_WNDPROC, ( LONG_PTR ) hb_gt_wvw_CBProc );

      hb_gt_wvw_StoreControlProc( usWinNum, WVW_CONTROL_COMBOBOX, hWndCB, OldProc );

      SendMessage( hWndCB, WM_SETFONT, ( WPARAM ) wvw_win->hCBfont, ( LPARAM ) TRUE );
      hb_stornint( ( HB_PTRDIFF ) hWndCB, 11 );

      hb_retnl( uiCBid );
   }
   else
      hb_retnl( 0 );
}

/* wvw_cbDestroy( [nWinNum], nCBid )
 * destroy combobox nCBid for window nWinNum
 */
HB_FUNC( WVW_CBDESTROY )
{
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   UINT       uiCBid  = ( UINT ) hb_parnl( 2 );
   WVW_CTRL * pcd     = wvw_win->pcdCtrlList;
   WVW_CTRL * pcdPrev = NULL;

   while( pcd )
   {
      if( pcd->byCtrlClass == WVW_CONTROL_COMBOBOX && pcd->uiCtrlid == uiCBid )
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
         wvw_win->pcdCtrlList = pcd->pNext;

      if( pcd->phiCodeBlock )
         hb_itemRelease( pcd->phiCodeBlock );

      hb_xfree( pcd );
   }
}

/* wvw_cbSetFocus( [nWinNum], nComboId )
 * set the focus to combobox nComboId in window nWinNum
 */
HB_FUNC( WVW_CBSETFOCUS )
{
   HWND hWndCB = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, ( UINT ) hb_parnl( 2 ), NULL );

   if( hWndCB )
      hb_retl( SetFocus( hWndCB ) != NULL );
   else
      hb_retl( HB_FALSE );
}

/* wvw_cbIsFocused( [nWinNum], nComboId )
 * returns .t. if the focus is on combobox nComboId in window nWinNum
 */
HB_FUNC( WVW_CBISFOCUSED )
{
   HWND hWndCB = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, ( UINT ) hb_parnl( 2 ), NULL );

   hb_retl( GetFocus() == hWndCB );
}

/* wvw_cbEnable( [nWinNum], nComboId, [lEnable] )
 * enable/disable button nComboId on window nWinNum
 *(lEnable defaults to .t., ie. enabling the combobox)
 * return previous state of the combobox (TRUE:enabled FALSE:disabled)
 *(if nComboId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_CBENABLE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   HWND hWndCB   = hb_gt_wvw_FindControlHandle( usWinNum, WVW_CONTROL_COMBOBOX, ( UINT ) hb_parnl( 2 ), NULL );

   if( hWndCB )
   {
      HB_BOOL bEnable = hb_parldef( 3, HB_TRUE );

      hb_retl( EnableWindow( hWndCB, ( BOOL ) bEnable ) == 0 );

      if( ! bEnable )
      {
         WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( usWinNum );
         SetFocus( wvw_win->hWnd );
      }
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_cbSetCodeblock( [nWinNum], nCBid, bBlock )
 * assign (new) codeblock bBlock to combobox nCBid for window nWinNum
 *
 * return .t. if successful
 */
HB_FUNC( WVW_CBSETCODEBLOCK )
{
   WVW_CTRL * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, NULL, ( UINT ) hb_parnl( 2 ) );
   PHB_ITEM   phiCodeBlock = hb_param( 3, HB_IT_EVALITEM );

   if( phiCodeBlock && pcd && ! pcd->bBusy )
   {
      WVW_GLOB * wvw         = hb_gt_wvw_GetWvwData();
      HB_BOOL    bOldSetting = wvw->bRecurseCBlock;

      wvw->bRecurseCBlock = HB_FALSE;
      pcd->bBusy = HB_TRUE;

      if( pcd->phiCodeBlock )
         hb_itemRelease( pcd->phiCodeBlock );

      pcd->phiCodeBlock = hb_itemNew( phiCodeBlock );

      pcd->bBusy = HB_FALSE;
      wvw->bRecurseCBlock = bOldSetting;

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_cbSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality,;
 *                             lItalic, lUnderline, lStrikeout
 *
 * this will initialize font for ALL comboboxes in window nWinNum
 * (including ones created later on)
 *
 * TODO: ? should nHeight be ignored, and always forced to use standard char height?
 */
HB_FUNC( WVW_CBSETFONT )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   HB_BOOL retval = HB_TRUE;

   wvw->lfCB.lfHeight      = hb_parnldef( 3, wvw_win->fontHeight - 2 );
   wvw->lfCB.lfWidth       = HB_ISNUM( 4 ) ? hb_parni( 4 ) : wvw->lfCB.lfWidth;
   wvw->lfCB.lfEscapement  = 0;
   wvw->lfCB.lfOrientation = 0;
   wvw->lfCB.lfWeight      = HB_ISNUM( 5 ) ? hb_parni( 5 )         : wvw->lfCB.lfWeight;
   wvw->lfCB.lfItalic      = HB_ISLOG( 7 ) ? ( BYTE ) hb_parl( 7 ) : wvw->lfCB.lfItalic;
   wvw->lfCB.lfUnderline   = HB_ISLOG( 8 ) ? ( BYTE ) hb_parl( 8 ) : wvw->lfCB.lfUnderline;
   wvw->lfCB.lfStrikeOut   = HB_ISLOG( 9 ) ? ( BYTE ) hb_parl( 9 ) : wvw->lfCB.lfStrikeOut;
   wvw->lfCB.lfCharSet     = DEFAULT_CHARSET;

   wvw->lfCB.lfQuality        = HB_ISNUM( 6 ) ? ( BYTE ) hb_parni( 6 ) : wvw->lfCB.lfQuality;
   wvw->lfCB.lfPitchAndFamily = FF_DONTCARE;
   if( HB_ISCHAR( 2 ) )
      hb_strncpy( wvw->lfCB.lfFaceName, hb_parc( 2 ), sizeof( wvw->lfPB.lfFaceName ) - 1 );

   if( wvw_win->hCBfont )
   {
      HFONT hOldFont = wvw_win->hCBfont;
      HFONT hFont    = CreateFontIndirect( &wvw->lfCB );
      if( hFont )
      {
         WVW_CTRL * pcd = wvw_win->pcdCtrlList;

         while( pcd )
         {
            if( pcd->byCtrlClass == WVW_CONTROL_COMBOBOX &&
                ( HFONT ) SendMessage( pcd->hWndCtrl, WM_GETFONT, 0, 0 ) == hOldFont )
               SendMessage( pcd->hWndCtrl, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );

            pcd = pcd->pNext;
         }

         wvw_win->hCBfont = hFont;
         DeleteObject( hOldFont );
      }
      else
         retval = HB_FALSE;
   }

   hb_retl( retval );
}

/* wvw_cbSetIndex( [nWinNum], nCBid, nIndex )
 *  set current selection of combobox nCBid in window nWinNum to nIndex
 *  (nIndex is 0 based)
 *  returns .t. if successful.
 *
 * NOTE: the better name to this function should be wvw_CBSetCurSel()
 *      but that name is already used.
 *      (wvw_CBSetCurSel() and wvw_cbAddString() is NOT related to other
 *       WVW_CB* functions)
 */
HB_FUNC( WVW_CBSETINDEX )
{
   int        iIndex = hb_parni( 3 );
   WVW_CTRL * pcd    = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, NULL, ( UINT ) hb_parnl( 2 ) );

   if( pcd && iIndex >= 0 )
      hb_retl( SendMessage( pcd->hWndCtrl, CB_SETCURSEL, ( WPARAM ) iIndex, 0 ) == iIndex );
   else
      hb_retl( HB_FALSE );
}

/* wvw_cbGetIndex( [nWinNum], nCBid )
 *  get current selection of combobox nCBid in window nWinNum
 *  return nIndex (0 based)
 *  returns CB_ERR (-1) if none selected
 *
 * NOTE: the better name to this function should be WVW_CBgetCurSel()
 *      but that name is potentially misleading to WVW_CBsetCursel
 *      which is not our family of WVW_CB* functions
 *      (wvw_CBSetCurSel() and wvw_cbAddString() is NOT related to other
 *       WVW_CB* functions)
 */
HB_FUNC( WVW_CBGETINDEX )
{
   WVW_CTRL * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, NULL, ( UINT ) hb_parnl( 2 ) );

   if( pcd )
      hb_retni( ( int ) SendMessage( pcd->hWndCtrl, CB_GETCURSEL, 0, 0 ) );
   else
      hb_retni( CB_ERR );
}

/* wvw_cbFindString( [nWinNum], nCBid, cString )
 *  find index of cString in combobox nCBid in window nWinNum
 *  returns index of cString (0 based)
 *  returns CB_ERR (-1) if string not found
 *
 * NOTE:case insensitive
 */
HB_FUNC( WVW_CBFINDSTRING )
{
   WVW_CTRL * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, NULL, ( UINT ) hb_parnl( 2 ) );

   if( pcd )
      hb_retni( ( int ) SendMessage( pcd->hWndCtrl, CB_FINDSTRING, ( WPARAM ) -1, ( LPARAM ) ( LPCSTR ) hb_parcx( 3 ) ) );
   else
      hb_retni( CB_ERR );
}

/* wvw_cbGetCurText( [nWinNum], nCBid )
 * get current selected cString in combobox nCBid in window nWinNum
 * returns "" if none selected
 *
 */
HB_FUNC( WVW_CBGETCURTEXT )
{
   WVW_CTRL * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, NULL, ( UINT ) hb_parnl( 2 ) );

   if( pcd )
   {
      int iCurSel  = ( int ) SendMessage( pcd->hWndCtrl, CB_GETCURSEL, 0, 0 );
      int iTextLen = ( int ) SendMessage( pcd->hWndCtrl, CB_GETLBTEXTLEN, ( WPARAM ) iCurSel, 0 );
      if( iTextLen == CB_ERR )
         hb_retc_null();
      else
      {
         LPTSTR lptstr = ( char * ) hb_xgrab( iTextLen + 1 );

         if( SendMessage( pcd->hWndCtrl, CB_GETLBTEXT, ( WPARAM ) iCurSel, ( LPARAM ) lptstr ) == CB_ERR )
         {
            hb_retc_null();
            hb_xfree( lptstr );
         }
         else
            hb_retc_buffer( lptstr );
      }
   }
   else
      hb_retc_null();
}

/* wvw_cbIsDropped( [nWinNum], nCBid )
 * get current dropped state of combobox nCBid in window nWinNum
 * returns .t. if listbox is being shown, otherwise .f.
 * Also returns .f. if nCBid not valid
 */
HB_FUNC( WVW_CBISDROPPED )
{
   WVW_CTRL * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, NULL, ( UINT ) hb_parnl( 2 ) );

   if( pcd )
      hb_retl( ( HB_BOOL ) ( BOOL ) SendMessage( pcd->hWndCtrl, CB_GETDROPPEDSTATE, 0, 0 ) );
   else
      hb_retl( HB_FALSE );
}
