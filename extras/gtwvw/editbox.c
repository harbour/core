/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw edit functions
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

/* wvw_ebCreate( [nWinNum], nTop, nLeft, nBottom, nRight, cText, bBlock, ;
 *                         lMultiline, nMoreStyle, nMaxChar, nReserved, aOffset)
 * create editbox for window nWinNum
 * nTop: row of top/left corner (in character unit)
 * nLeft: col of top/left corner (in character unit)
 * nBottom: row of bottom/right corner (in character unit)
 * nRight: col of bottom/right corner (in character unit)
 * cText: initial text to display, default = ""
 *      WARNING!! must be of "C" typed!
 * bBlock: codeblock to execute on these events:
 *         event=EN_SETFOCUS(...): editbox got focus
 *         event=EN_KILLFOCUS(...): editbox lose focus
 *         This codeblock will be evaluated with these parameters:
 *         nWinNum: window number
 *         nEBid  : editbox id
 *         nType  : event type (EN_SETFOCUS/EN_KILLFOCUS supported)
 *
 *
 * lMultiline: .f. :: single line editbox (default)
 *            .t. :: multi line editbox
 * mapped internally into two types of editbox:
 *         WVW_EB_SINGLELINE (1): single line editbox
 *         WVW_EB_MULTILINE (2): multi line editbox
 *         default is WVW_EB_SINGLELINE (1)
 *
 * nMoreStyle: more style that will be added to the predefined style
 *            some examples: ES_PASSWORD, ES_READONLY
 *
 * nMaxChar: (FUTURE FEATURE) maximum number of chars allowed
 *
 * nReserved: reserved for future use
 *
 * aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of editbox.
 *         defaults: {-2,-2,+2,+2}
 *
 * returns control id of newly created editbox of windows nWinNum
 * returns 0 if failed
 *
 * example:
 */

HB_FUNC( WVW_EBCREATE )
{
   HANDLE     hInstance   = NULL;
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   HWND       hWndParent  = pWindowData->hWnd;
   HWND       hWndEB;
   POINT      xy;
   int        iTop, iLeft, iBottom, iRight;
   int        iOffTop, iOffLeft, iOffBottom, iOffRight;
   UINT       uiEBid;
   USHORT     usTop    = ( USHORT ) hb_parni( 2 ),
              usLeft   = ( USHORT ) hb_parni( 3 ),
              usBottom = ( USHORT ) hb_parni( 4 ),
              usRight  = ( USHORT ) hb_parni( 5 );
   LPTSTR lpszText     = ( LPTSTR ) hb_parcx( 6 );

   BOOL bMultiline = hb_parl( 8 );
   BYTE bEBType    = ( BYTE ) ( bMultiline ? WVW_EB_MULTILINE : WVW_EB_SINGLELINE );

   DWORD dwMoreStyle = ( DWORD ) hb_parnl( 9 );

   USHORT usMaxChar = ( USHORT ) ( hb_parni( 10 ) > 0 ? hb_parni( 10 ) : 0 );

   DWORD      dwStyle;
   WVW_DATA * pData = hb_gt_wvw_GetWvwData();

   if( pWindowData->hEBfont == NULL )
   {
      pWindowData->hEBfont = CreateFontIndirect( &pData->lfEB );
      if( pWindowData->hEBfont == NULL )
      {
         hb_retnl( 0 );
         return;
      }
   }

   iOffTop    = HB_ISARRAY( 12 ) ? hb_parvni( 12, 1 ) : 0;
   iOffLeft   = HB_ISARRAY( 12 ) ? hb_parvni( 12, 2 ) : 0;
   iOffBottom = HB_ISARRAY( 12 ) ? hb_parvni( 12, 3 ) : 0;
   iOffRight  = HB_ISARRAY( 12 ) ? hb_parvni( 12, 4 ) : 0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   uiEBid = hb_gt_wvw_LastControlId( usWinNum, WVW_CONTROL_EDITBOX );
   if( uiEBid == 0 )
      uiEBid = WVW_ID_BASE_EDITBOX;
   else
      uiEBid++;

   dwStyle = WS_BORDER | WS_GROUP | WS_TABSTOP | dwMoreStyle;

   if( ( bEBType & WVW_EB_MULTILINE ) == WVW_EB_MULTILINE )
      dwStyle |= ES_AUTOVSCROLL | ES_MULTILINE | ES_WANTRETURN | WS_BORDER | WS_VSCROLL;
   else
      dwStyle |= ES_AUTOHSCROLL;

   if( pWindowData->CodePage == OEM_CHARSET )
      dwStyle |= ES_OEMCONVERT;

   hb_winmainArgGet( &hInstance, NULL, NULL );

   hWndEB = CreateWindowEx(
      0L,
      "EDIT",
      NULL,
      WS_CHILD | WS_VISIBLE | ( DWORD ) dwStyle,
      iLeft,
      iTop,
      iRight - iLeft + 1,
      iBottom - iTop + 1,
      hWndParent,
      ( HMENU ) ( HB_PTRDIFF ) uiEBid,
      ( HINSTANCE ) hInstance,
      NULL );

   if( hWndEB )
   {
      RECT    rXB, rOffXB;
      WNDPROC OldProc;

      BOOL bFromOEM = ( pWindowData->CodePage == OEM_CHARSET );

      if( bFromOEM )
      {
         ULONG  ulLen        = ( ULONG ) strlen( lpszText );
         LPTSTR lpszTextANSI = ( LPTSTR ) hb_xgrab( ulLen + 1 );
         OemToCharBuff( lpszText, lpszTextANSI, ulLen );
         lpszText = lpszTextANSI;
      }

      SendMessage( hWndEB, WM_SETTEXT, 0, ( LPARAM ) lpszText );

      if( bFromOEM )
         hb_xfree( lpszText );

      if( usMaxChar > 0 )
         SendMessage( hWndEB, EM_LIMITTEXT, ( WPARAM ) usMaxChar, 0 );

      rXB.top    = usTop;
      rXB.left   = usLeft;
      rXB.bottom = usBottom;
      rXB.right  = usRight;

      rOffXB.top    = iOffTop;
      rOffXB.left   = iOffLeft;
      rOffXB.bottom = iOffBottom;
      rOffXB.right  = iOffRight;

      hb_gt_wvw_AddControlHandle( usWinNum, WVW_CONTROL_EDITBOX, hWndEB, uiEBid, hb_param( 7, HB_IT_EVALITEM ), rXB, rOffXB, ( byte ) bEBType );

      OldProc = ( WNDPROC ) SetWindowLongPtr( hWndEB, GWLP_WNDPROC, ( LONG_PTR ) hb_gt_wvw_EBProc );

      hb_gt_wvw_StoreControlProc( usWinNum, WVW_CONTROL_EDITBOX, hWndEB, OldProc );

      SendMessage( hWndEB, WM_SETFONT, ( WPARAM ) pWindowData->hEBfont, ( LPARAM ) TRUE );

      hb_retnl( uiEBid );
   }
   else
      hb_retnl( 0 );
}

/* wvw_ebDestroy( [nWinNum], nEBid )
 * destroy editbox nEBid for window nWinNum
 */
HB_FUNC( WVW_EBDESTROY )
{
   WIN_DATA *     pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   UINT           uiEBid      = ( UINT ) hb_parnl( 2 );
   CONTROL_DATA * pcd         = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev     = NULL;

   while( pcd )
   {
      if( pcd->byCtrlClass == WVW_CONTROL_EDITBOX && pcd->uiCtrlid == uiEBid )
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

/* wvw_ebSetFocus( [nWinNum], nEditId )
 * set the focus to editbox nEditId in window nWinNum
 */
HB_FUNC( WVW_EBSETFOCUS )
{
   HWND hWndEB = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, ( UINT ) hb_parnl( 2 ), NULL );

   if( hWndEB )
      hb_retl( SetFocus( hWndEB ) != NULL );
   else
      hb_retl( HB_FALSE );
}

/* wvw_ebIsFocused( [nWinNum], nEditId )
 * returns .t. if the focus is on editbox nEditId in window nWinNum
 */
HB_FUNC( WVW_EBISFOCUSED )
{
   HWND hWndEB = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, ( UINT ) hb_parnl( 2 ), NULL );

   hb_retl( GetFocus() == hWndEB );
}

/* wvw_ebEnable( [nWinNum], nEditId, [lEnable] )
 *  enable/disable editbox nEditId on window nWinNum
 * (lEnable defaults to .t., ie. enabling the editbox)
 *  return previous state of the editbox (TRUE:enabled FALSE:disabled)
 * (if nEditId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_EBENABLE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   HWND hWndEB = hb_gt_wvw_FindControlHandle( usWinNum, WVW_CONTROL_EDITBOX, ( UINT ) hb_parnl( 2 ), NULL );

   if( hWndEB )
   {
      BOOL bEnable = hb_parldef( 3, HB_TRUE );

      hb_retl( EnableWindow( hWndEB, bEnable ) == 0 );

      if( ! bEnable )
      {
         WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
         SetFocus( pWindowData->hWnd );
      }
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_ebEditable( [nWinNum], nEditId, [lEditable] )
 *  get/set editability attribute from editbox nEditId on window nWinNum
 * (if lEditable is not specified, no change to editability)
 *  return previous state of the editbox (TRUE:editable FALSE:not editable)
 * (if nEditId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_EBEDITABLE )
{
   HWND hWndEB = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, ( UINT ) hb_parnl( 2 ), NULL );

   if( hWndEB )
   {
      hb_retl( ( GetWindowLong( hWndEB, GWL_STYLE ) & ES_READONLY ) != ES_READONLY );

      if( HB_ISLOG( 3 ) )
         SendMessage( hWndEB, EM_SETREADONLY, ( WPARAM ) ! hb_parl( 3 ), 0 );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_ebSetCodeblock( [nWinNum], nEBid, bBlock )
 * assign (new) codeblock bBlock to editbox nEBid for window nWinNum
 *
 * return .t. if successful
 */
HB_FUNC( WVW_EBSETCODEBLOCK )
{
   CONTROL_DATA * pcd          = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, NULL, ( UINT ) hb_parnl( 2 ) );
   PHB_ITEM       phiCodeBlock = hb_param( 3, HB_IT_EVALITEM );

   if( phiCodeBlock && pcd && ! pcd->bBusy )
   {
      WVW_DATA * pData = hb_gt_wvw_GetWvwData();
      BOOL bOldSetting = pData->bRecurseCBlock;

      pData->bRecurseCBlock = FALSE;
      pcd->bBusy = TRUE;

      if( pcd->phiCodeBlock )
         hb_itemRelease( pcd->phiCodeBlock );

      pcd->phiCodeBlock = hb_itemNew( phiCodeBlock );

      pcd->bBusy = FALSE;
      pData->bRecurseCBlock = bOldSetting;

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_ebSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality,;
 *                             lItalic, lUnderline, lStrikeout
 *
 * this will initialize font for ALL editboxes in window nWinNum
 * (including ones created later on)
 *
 * TODO: ? should nHeight be ignored, and always forced to use standard char height?
 */
HB_FUNC( WVW_EBSETFONT )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   HB_BOOL    retval      = HB_TRUE;
   WVW_DATA * pData       = hb_gt_wvw_GetWvwData();

   pData->lfEB.lfHeight      = HB_ISNUM( 3 ) ? hb_parnl( 3 ) : pWindowData->fontHeight - 2;
   pData->lfEB.lfWidth       = HB_ISNUM( 4 ) ? hb_parni( 4 ) : pData->lfEB.lfWidth;
   pData->lfEB.lfEscapement  = 0;
   pData->lfEB.lfOrientation = 0;
   pData->lfEB.lfWeight      = HB_ISNUM( 5 ) ? hb_parni( 5 ) : pData->lfEB.lfWeight;
   pData->lfEB.lfItalic      = HB_ISLOG( 7 ) ? ( BYTE ) hb_parl( 7 ) : pData->lfEB.lfItalic;
   pData->lfEB.lfUnderline   = HB_ISLOG( 8 ) ? ( BYTE ) hb_parl( 8 ) : pData->lfEB.lfUnderline;
   pData->lfEB.lfStrikeOut   = HB_ISLOG( 9 ) ? ( BYTE ) hb_parl( 9 ) : pData->lfEB.lfStrikeOut;
   pData->lfEB.lfCharSet     = DEFAULT_CHARSET;

   pData->lfEB.lfQuality        = HB_ISNUM( 6 ) ? ( BYTE ) hb_parni( 6 ) : pData->lfEB.lfQuality;
   pData->lfEB.lfPitchAndFamily = FF_DONTCARE;
   if( HB_ISCHAR( 2 ) )
      hb_strncpy( pData->lfEB.lfFaceName, hb_parc( 2 ), sizeof( pData->lfPB.lfFaceName ) - 1 );

   if( pWindowData->hEBfont )
   {
      HFONT hOldFont = pWindowData->hEBfont;
      HFONT hFont    = CreateFontIndirect( &pData->lfEB );
      if( hFont )
      {
         CONTROL_DATA * pcd = pWindowData->pcdCtrlList;

         while( pcd )
         {
            if( pcd->byCtrlClass == WVW_CONTROL_EDITBOX &&
                ( HFONT ) SendMessage( pcd->hWndCtrl, WM_GETFONT, 0, 0 ) == hOldFont )
               SendMessage( pcd->hWndCtrl, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );

            pcd = pcd->pNext;
         }

         pWindowData->hEBfont = hFont;
         DeleteObject( hOldFont );
      }
      else
         retval = HB_FALSE;
   }

   hb_retl( retval );

}

/* wvw_ebIsMultiline( [nWinNum], nEBid )
 * returns .t. if editbox nEBid in window nWinNum is multiline
 * otherwise .f.
 * Also returns .f. if nEBid not valid
 */
HB_FUNC( WVW_EBISMULTILINE )
{
   CONTROL_DATA * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, NULL, ( UINT ) hb_parnl( 2 ) );

   if( pcd )
      hb_retl( ( pcd->bStyle & WVW_EB_MULTILINE ) == WVW_EB_MULTILINE );
   else
      hb_retl( HB_FALSE );
}

/* wvw_ebGetText( [nWinNum], nEBid,;
 *                          lSoftBreak )
 * returns current text from editbox nEBid in window nWinNum
 * lSoftBreak: Default is FALSE.
 *             insert soft line break character (CR+CR+LF) at wordwrap positions
 *             can be usefull to convert the text to MEMO format
 *             eg. converting editbox's softbreaks into memoline softbreak:
 *                cStr := wvw_ebGetText( NIL, nEBid, .T. )
 *                cStr := StrTran( cStr, CR + CR + LF, Chr( 141 ) + LF )
 *
 * returns "" in case of error (eg. nEBid not valid)
 */
HB_FUNC( WVW_EBGETTEXT )
{
   UINT usWinNum      = WVW_WHICH_WINDOW;
   CONTROL_DATA * pcd = hb_gt_wvw_GetControlData( usWinNum, WVW_CONTROL_EDITBOX, NULL, ( UINT ) hb_parnl( 2 ) );

   if( pcd )
   {
      BOOL bSoftBreak = hb_parl( 3 );
      WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
      BOOL bToOEM = ( pWindowData->CodePage == OEM_CHARSET );

      USHORT usLen;
      LPTSTR lpszTextANSI;

      if( bSoftBreak )
         SendMessage( pcd->hWndCtrl, EM_FMTLINES, ( WPARAM ) TRUE, 0 );

      usLen = ( USHORT ) SendMessage( pcd->hWndCtrl, WM_GETTEXTLENGTH, 0, 0 ) + 1;

      lpszTextANSI = ( LPTSTR ) hb_xgrab( usLen );

      SendMessage( pcd->hWndCtrl, WM_GETTEXT, usLen, ( LPARAM ) lpszTextANSI );

      if( bToOEM )
      {
         ULONG  ulLen    = ( ULONG ) strlen( lpszTextANSI );
         LPTSTR lpszText = ( LPTSTR ) hb_xgrab( ulLen + 1 );
         CharToOem( lpszTextANSI, lpszText );
         hb_retc_buffer( lpszText );
      }
      else
         hb_retc( lpszTextANSI );

      hb_xfree( lpszTextANSI );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_ebSetText( [nWinNum], nEBid, cText )
 * set current text of editbox nEBid in window nWinNum
 * returns .t. if successful, .f. in case of error (eg. nEBid not valid)
 */
HB_FUNC( WVW_EBSETTEXT )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   CONTROL_DATA * pcd = hb_gt_wvw_GetControlData( usWinNum, WVW_CONTROL_EDITBOX, NULL, ( UINT ) hb_parnl( 2 ) );

   if( pcd )
   {
      WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
      BOOL bFromOEM = ( pWindowData->CodePage == OEM_CHARSET );
      LPTSTR lpszText = ( LPTSTR ) hb_parcx( 3 );

      if( bFromOEM )
      {
         ULONG  ulLen        = ( ULONG ) strlen( lpszText );
         LPTSTR lpszTextANSI = ( LPTSTR ) hb_xgrab( ulLen + 1 );
         OemToCharBuff( lpszText, lpszTextANSI, ulLen );
         lpszText = lpszTextANSI;
      }

      hb_retl( ( BOOL ) SendMessage( pcd->hWndCtrl, WM_SETTEXT, 0, ( LPARAM ) lpszText ) );

      if( bFromOEM )
         hb_xfree( lpszText );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_ebGetSel( [nWinNum], nEBid, @nstart, @nend )
 * get selected text editbox nEBid in window nWinNum
 * the start selected text (0-based) is in nstart
 * the end selected text (0-based) is in nend
 * returns .t. if operation successful
 * returns .f. if not (eg. nEBid not valid)
 */
HB_FUNC( WVW_EBGETSEL )
{
   CONTROL_DATA * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, NULL, ( UINT ) hb_parnl( 2 ) );
   DWORD dwStart, dwEnd;

   if( pcd )
   {
      SendMessage( pcd->hWndCtrl, EM_GETSEL, ( WPARAM ) &dwStart, ( LPARAM ) &dwEnd );

      hb_retl( HB_TRUE );
   }
   else
   {
      dwStart = dwEnd = 0;
      hb_retl( HB_FALSE );
   }

   hb_stornl( dwStart, 3 );
   hb_stornl( dwEnd, 4 );
}

/* wvw_ebSetSel( [nWinNum], nEBid, nstart, nend )
 * set selected text editbox nEBid in window nWinNum
 * the start selected text (0-based) is in nstart
 * the end selected text (0-based) is in nend
 * notes: nstart may be > nend (flipped selection)
 * notes: to selet all text: wvw_ebSetSel(nwinnum, nebid, 0, -1)
 * returns .t. if operation successful
 * returns .f. if not (eg. nEBid not valid)
 */
HB_FUNC( WVW_EBSETSEL )
{
   CONTROL_DATA * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, NULL, ( UINT ) hb_parnl( 2 ) );

   if( pcd )
   {
      DWORD dwStart = ( DWORD ) hb_parnl( 3 );
      DWORD dwEnd   = ( DWORD ) hb_parnl( 4 );

      SendMessage( pcd->hWndCtrl, EM_SETSEL, ( WPARAM ) dwStart, ( LPARAM ) dwEnd );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}
