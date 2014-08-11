/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw combobox functions
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
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   HB_UINT    nWin    = WVW_WHICH_WINDOW;
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   HWND hWndParent = wvw_win->hWnd;
   HWND hWnd;
/* LONG cnt; */
   LONG numofchars;
   LONG avgwidth;
   LONG LongComboWidth, NewLongComboWidth;
/* RECT r; */
   HFONT hFont = hb_gt_wvw_GetFont( wvw_win->fontFace, 10, wvw_win->fontWidth, wvw_win->fontWeight, wvw_win->fontQuality, wvw_win->CodePage );

   POINT xy;
   int   iTop, iLeft, iBottom, iRight;
   int   iOffTop, iOffLeft, iOffBottom, iOffRight;

   HB_UINT nCtrlId;
   USHORT  usWidth     = ( USHORT ) hb_parni( 4 );
   USHORT  usTop       = ( USHORT ) hb_parni( 2 ),
           usLeft      = ( USHORT ) hb_parni( 3 ),
           usBottom    = usTop,
           usRight     = usLeft + usWidth - 1;
   USHORT usNumElement = ( USHORT ) ( HB_ISARRAY( 5 ) ? hb_arrayLen( hb_param( 5, HB_IT_ARRAY ) ) : 0 );
   USHORT usListLines  = ( USHORT ) hb_parnidef( 7, 3 );
   BYTE   byCharHeight = hb_gt_wvw_LineHeight( wvw_win );

   /* in the future combobox type might be selectable by 8th parameter */
   int     iStyle   = CBS_DROPDOWNLIST | WS_VSCROLL;
   HB_BYTE bKbdType = ( HB_BYTE ) hb_parnidef( 9, WVW_CB_KBD_STANDARD );

   if( wvw_win->hCBfont == NULL )
   {
      wvw_win->hCBfont = CreateFontIndirect( &wvw->lfCB );
      if( wvw_win->hCBfont == NULL )
      {
         HB_STOREHANDLE( NULL, 11 );
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
      hb_gt_wvw_HBFUNCPrologue( nWin, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

   xy.y -= wvw_win->iLineSpacing;

   iBottom = xy.y - 1 + ( iOffBottom * byCharHeight );
   iRight  = xy.x - 1 + iOffRight;

   nCtrlId = hb_gt_wvw_LastControlId( nWin, WVW_CONTROL_COMBOBOX );
   if( nCtrlId == 0 )
      nCtrlId = WVW_ID_BASE_COMBOBOX;
   else
      nCtrlId++;

   InitCommonControls();

   hWnd = CreateWindowEx(
      0,
      TEXT( "COMBOBOX" ),
      NULL,
      WS_CHILD | WS_VISIBLE | ( DWORD ) iStyle,
      iLeft,
      iTop,
      iRight - iLeft + 1,
      iBottom - iTop + 1,
      hWndParent,
      ( HMENU ) ( HB_PTRDIFF ) nCtrlId,
      wvw->hInstance,
      NULL );

   if( hWnd )
   {
      RECT    rXB, rOffXB;
      WNDPROC OldProc;
      USHORT  i;

      SendMessage( hWnd, WM_SETREDRAW, ( WPARAM ) TRUE, 0 );

      if( usNumElement == 0 )
      {
         if( SendMessage( hWnd, CB_ADDSTRING, 0, ( LPARAM ) TEXT( "empty" ) ) < 0 )
         {
            /* ignore failure */
         }
      }
      else
      {
         for( i = 1; i <= usNumElement; i++ )
         {
            void * hText;

            if( SendMessage( hWnd, CB_ADDSTRING, 0, ( LPARAM ) HB_PARASTR( 5, i, &hText, NULL ) ) < 0 )
            {
               /* ignore failure */
            }
            else
            {
               numofchars = ( int ) SendMessage( hWnd, CB_GETLBTEXTLEN, i - 1, 0 );
               if( numofchars > LongComboWidth )
                  LongComboWidth = numofchars;
            }

            hb_strfree( hText );
         }
      }

      SendMessage( hWnd, CB_SETCURSEL, 0, 0 );
      SendMessage( hWnd, CB_SETEXTENDEDUI, ( WPARAM ) TRUE, 0 );

      avgwidth = hb_gt_wvw_GetFontDialogUnits( hWndParent, hFont );
      NewLongComboWidth = ( LongComboWidth - 2 ) * avgwidth;
      SendMessage( hWnd, CB_SETDROPPEDWIDTH, ( WPARAM ) NewLongComboWidth + 100 /* LongComboWidth + 100 */, 0 );

      rXB.top    = usTop;
      rXB.left   = usLeft;
      rXB.bottom = usBottom;
      rXB.right  = usRight;

      rOffXB.top    = iOffTop;
      rOffXB.left   = iOffLeft;
      rOffXB.bottom = iOffBottom;
      rOffXB.right  = iOffRight;

      hb_gt_wvw_AddControlHandle( nWin, WVW_CONTROL_COMBOBOX, hWnd, nCtrlId, hb_param( 6, HB_IT_EVALITEM ), rXB, rOffXB, bKbdType );

      OldProc = ( WNDPROC ) SetWindowLongPtr( hWnd, GWLP_WNDPROC, ( LONG_PTR ) hb_gt_wvw_CBProc );

      hb_gt_wvw_StoreControlProc( nWin, WVW_CONTROL_COMBOBOX, hWnd, OldProc );

      SendMessage( hWnd, WM_SETFONT, ( WPARAM ) wvw_win->hCBfont, ( LPARAM ) TRUE );

      HB_STOREHANDLE( hWnd, 11 );
      hb_retnl( nCtrlId );
   }
   else
   {
      HB_STOREHANDLE( NULL, 11 );
      hb_retnl( 0 );
   }
}

/* wvw_cbDestroy( [nWinNum], nCBid )
 * destroy combobox nCBid for window nWinNum
 */
HB_FUNC( WVW_CBDESTROY )
{
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   HB_UINT    nCtrlId = ( HB_UINT ) hb_parnl( 2 );
   WVW_CTRL * pcd     = wvw_win->pcdList;
   WVW_CTRL * pcdPrev = NULL;

   while( pcd )
   {
      if( pcd->nClass == WVW_CONTROL_COMBOBOX && pcd->nId == nCtrlId )
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

/* wvw_cbSetFocus( [nWinNum], nComboId )
 * set the focus to combobox nComboId in window nWinNum
 */
HB_FUNC( WVW_CBSETFOCUS )
{
   HWND hWnd = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, ( HB_UINT ) hb_parnl( 2 ), NULL );

   hb_retl( hWnd && SetFocus( hWnd ) != NULL );
}

/* wvw_cbIsFocused( [nWinNum], nComboId )
 * returns .T. if the focus is on combobox nComboId in window nWinNum
 */
HB_FUNC( WVW_CBISFOCUSED )
{
   HWND hWnd = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, ( HB_UINT ) hb_parnl( 2 ), NULL );

   hb_retl( GetFocus() == hWnd );
}

/* wvw_cbEnable( [nWinNum], nComboId, [lEnable] )
 * enable/disable button nComboId on window nWinNum
 *(lEnable defaults to .T., ie. enabling the combobox)
 * return previous state of the combobox (TRUE:enabled FALSE:disabled)
 *(if nComboId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_CBENABLE )
{
   HB_UINT nWin = WVW_WHICH_WINDOW;
   HWND    hWnd = hb_gt_wvw_FindControlHandle( nWin, WVW_CONTROL_COMBOBOX, ( HB_UINT ) hb_parnl( 2 ), NULL );

   if( hWnd )
   {
      HB_BOOL bEnable = hb_parldef( 3, HB_TRUE );

      hb_retl( EnableWindow( hWnd, ( BOOL ) bEnable ) == 0 );

      if( ! bEnable )
      {
         WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );
         SetFocus( wvw_win->hWnd );
      }
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_cbSetCodeblock( [nWinNum], nCBid, bBlock )
 * assign (new) codeblock bBlock to combobox nCBid for window nWinNum
 *
 * return .T. if successful
 */
HB_FUNC( WVW_CBSETCODEBLOCK )
{
   WVW_CTRL * pcd    = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, NULL, ( HB_UINT ) hb_parnl( 2 ) );
   PHB_ITEM   pBlock = hb_param( 3, HB_IT_EVALITEM );

   if( pBlock && pcd && ! pcd->fBusy )
   {
      WVW_GLOB * wvw         = hb_gt_wvw_GetWvwData();
      HB_BOOL    fOldSetting = wvw->fRecurseCBlock;

      wvw->fRecurseCBlock = HB_FALSE;
      pcd->fBusy = HB_TRUE;

      if( pcd->pBlock )
         hb_itemRelease( pcd->pBlock );

      pcd->pBlock = hb_itemNew( pBlock );

      pcd->fBusy = HB_FALSE;
      wvw->fRecurseCBlock = fOldSetting;

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_cbSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality, ;
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

   HB_BOOL fResult = HB_TRUE;

   wvw->lfCB.lfHeight         = hb_parnldef( 3, wvw_win->fontHeight - 2 );
   wvw->lfCB.lfWidth          = hb_parnldef( 4, wvw->lfCB.lfWidth );
   wvw->lfCB.lfEscapement     = 0;
   wvw->lfCB.lfOrientation    = 0;
   wvw->lfCB.lfWeight         = hb_parnldef( 5, wvw->lfCB.lfWeight );
   wvw->lfCB.lfQuality        = ( BYTE ) hb_parnidef( 6, wvw->lfCB.lfQuality );
   wvw->lfCB.lfItalic         = ( BYTE ) hb_parldef( 7, wvw->lfCB.lfItalic );
   wvw->lfCB.lfUnderline      = ( BYTE ) hb_parldef( 8, wvw->lfCB.lfUnderline );
   wvw->lfCB.lfStrikeOut      = ( BYTE ) hb_parldef( 9, wvw->lfCB.lfStrikeOut );
   wvw->lfCB.lfCharSet        = DEFAULT_CHARSET;
   wvw->lfCB.lfPitchAndFamily = FF_DONTCARE;

   if( HB_ISCHAR( 2 ) )
   {
      HB_ITEMCOPYSTR( hb_param( 2, HB_IT_STRING ), wvw->lfCB.lfFaceName, HB_SIZEOFARRAY( wvw->lfCB.lfFaceName ) );
      wvw_win->fontFace[ HB_SIZEOFARRAY( wvw->lfCB.lfFaceName ) - 1 ] = TEXT( '\0' );
   }

   if( wvw_win->hCBfont )
   {
      HFONT hOldFont = wvw_win->hCBfont;
      HFONT hFont    = CreateFontIndirect( &wvw->lfCB );
      if( hFont )
      {
         WVW_CTRL * pcd = wvw_win->pcdList;

         while( pcd )
         {
            if( pcd->nClass == WVW_CONTROL_COMBOBOX &&
                ( HFONT ) SendMessage( pcd->hWnd, WM_GETFONT, 0, 0 ) == hOldFont )
               SendMessage( pcd->hWnd, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );

            pcd = pcd->pNext;
         }

         wvw_win->hCBfont = hFont;
         DeleteObject( hOldFont );
      }
      else
         fResult = HB_FALSE;
   }

   hb_retl( fResult );
}

/* wvw_cbSetIndex( [nWinNum], nCBid, nIndex )
 *  set current selection of combobox nCBid in window nWinNum to nIndex
 *  (nIndex is 0 based)
 *  returns .T. if successful.
 *
 * NOTE: the better name to this function should be wvw_CBSetCurSel()
 *      but that name is already used.
 *      (wvw_CBSetCurSel() and wvw_cbAddString() is NOT related to other
 *       WVW_CB* functions)
 */
HB_FUNC( WVW_CBSETINDEX )
{
   int        iIndex = hb_parni( 3 );
   WVW_CTRL * pcd    = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, NULL, ( HB_UINT ) hb_parnl( 2 ) );

   if( pcd && iIndex >= 0 )
      hb_retl( SendMessage( pcd->hWnd, CB_SETCURSEL, ( WPARAM ) iIndex, 0 ) == iIndex );
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
   WVW_CTRL * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, NULL, ( HB_UINT ) hb_parnl( 2 ) );

   if( pcd )
      hb_retni( ( int ) SendMessage( pcd->hWnd, CB_GETCURSEL, 0, 0 ) );
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
   WVW_CTRL * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, NULL, ( HB_UINT ) hb_parnl( 2 ) );

   if( pcd )
   {
      void * hStr;
      hb_retni( ( int ) SendMessage( pcd->hWnd, CB_FINDSTRING, ( WPARAM ) -1, ( LPARAM ) HB_PARSTRDEF( 3, &hStr, NULL ) ) );
      hb_strfree( hStr );
   }
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
   WVW_CTRL * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, NULL, ( HB_UINT ) hb_parnl( 2 ) );

   if( pcd )
   {
      int iCurSel  = ( int ) SendMessage( pcd->hWnd, CB_GETCURSEL, 0, 0 );
      int iTextLen = ( int ) SendMessage( pcd->hWnd, CB_GETLBTEXTLEN, ( WPARAM ) iCurSel, 0 );
      if( iTextLen == CB_ERR )
         hb_retc_null();
      else
      {
         LPTSTR lptstr = ( LPTSTR ) hb_xgrab( ( iTextLen + 1 ) * sizeof( TCHAR ) );

         if( SendMessage( pcd->hWnd, CB_GETLBTEXT, ( WPARAM ) iCurSel, ( LPARAM ) lptstr ) == CB_ERR )
            hb_retc_null();
         else
            HB_RETSTR( lptstr );

         hb_xfree( lptstr );
      }
   }
   else
      hb_retc_null();
}

/* wvw_cbIsDropped( [nWinNum], nCBid )
 * get current dropped state of combobox nCBid in window nWinNum
 * returns .T. if listbox is being shown, otherwise .F.
 * Also returns .F. if nCBid not valid
 */
HB_FUNC( WVW_CBISDROPPED )
{
   WVW_CTRL * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_COMBOBOX, NULL, ( HB_UINT ) hb_parnl( 2 ) );

   if( pcd )
      hb_retl( ( HB_BOOL ) ( BOOL ) SendMessage( pcd->hWnd, CB_GETDROPPEDSTATE, 0, 0 ) );
   else
      hb_retl( HB_FALSE );
}
