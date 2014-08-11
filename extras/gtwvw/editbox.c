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
 * lMultiline: .F. :: single line editbox (default)
 *            .T. :: multi line editbox
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
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   HB_UINT    nWin    = WVW_WHICH_WINDOW;
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( nWin );

   HANDLE  hInstance  = NULL;
   HWND    hWndParent = wvw_win->hWnd;
   HWND    hWnd;
   POINT   xy;
   int     iTop, iLeft, iBottom, iRight;
   int     iOffTop, iOffLeft, iOffBottom, iOffRight;
   HB_UINT nCtrlId;

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   HB_BOOL bMultiline = hb_parl( 8 );
   HB_BYTE bEBType    = bMultiline ? WVW_EB_MULTILINE : WVW_EB_SINGLELINE;

   DWORD dwMoreStyle = ( DWORD ) hb_parnl( 9 );

   USHORT usMaxChar = ( USHORT ) ( hb_parni( 10 ) > 0 ? hb_parni( 10 ) : 0 );

   DWORD dwStyle;

   if( wvw_win->hEBfont == NULL )
   {
      wvw_win->hEBfont = CreateFontIndirect( &wvw->lfEB );
      if( wvw_win->hEBfont == NULL )
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
      hb_gt_wvw_HBFUNCPrologue( nWin, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

   xy.y -= wvw_win->iLineSpacing;

   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   nCtrlId = hb_gt_wvw_LastControlId( nWin, WVW_CONTROL_EDITBOX );
   if( nCtrlId == 0 )
      nCtrlId = WVW_ID_BASE_EDITBOX;
   else
      nCtrlId++;

   dwStyle = WS_BORDER | WS_GROUP | WS_TABSTOP | dwMoreStyle;

   if( ( bEBType & WVW_EB_MULTILINE ) == WVW_EB_MULTILINE )
      dwStyle |= ES_AUTOVSCROLL | ES_MULTILINE | ES_WANTRETURN | WS_BORDER | WS_VSCROLL;
   else
      dwStyle |= ES_AUTOHSCROLL;

#if ! defined( UNICODE )
   if( wvw_win->CodePage == OEM_CHARSET )
      dwStyle |= ES_OEMCONVERT;
#endif

   hb_winmainArgGet( &hInstance, NULL, NULL );

   hWnd = CreateWindowEx(
      0L,
      TEXT( "EDIT" ),
      NULL,
      WS_CHILD | WS_VISIBLE | ( DWORD ) dwStyle,
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
      RECT    rXB, rOffXB;
      WNDPROC OldProc;

      void * hText;
      LPCTSTR lpszText = HB_PARSTRDEF( 6, &hText, NULL );

#if ! defined( UNICODE )
      if( wvw_win->CodePage == OEM_CHARSET )
      {
         ULONG ulLen        = ( ULONG ) strlen( lpszText );
         LPSTR lpszTextANSI = ( LPSTR ) hb_xgrab( ulLen + 1 );
         OemToCharBuff( lpszText, lpszTextANSI, ulLen );
         lpszText = ( LPCTSTR ) lpszTextANSI;
      }
#endif

      SendMessage( hWnd, WM_SETTEXT, 0, ( LPARAM ) lpszText );

#if ! defined( UNICODE )
      if( wvw_win->CodePage == OEM_CHARSET )
         hb_xfree( lpszText );
#endif

      hb_strfree( hText );

      if( usMaxChar > 0 )
         SendMessage( hWnd, EM_LIMITTEXT, ( WPARAM ) usMaxChar, 0 );

      rXB.top    = usTop;
      rXB.left   = usLeft;
      rXB.bottom = usBottom;
      rXB.right  = usRight;

      rOffXB.top    = iOffTop;
      rOffXB.left   = iOffLeft;
      rOffXB.bottom = iOffBottom;
      rOffXB.right  = iOffRight;

      hb_gt_wvw_AddControlHandle( nWin, WVW_CONTROL_EDITBOX, hWnd, nCtrlId, hb_param( 7, HB_IT_EVALITEM ), rXB, rOffXB, bEBType );

      OldProc = ( WNDPROC ) SetWindowLongPtr( hWnd, GWLP_WNDPROC, ( LONG_PTR ) hb_gt_wvw_EBProc );

      hb_gt_wvw_StoreControlProc( nWin, WVW_CONTROL_EDITBOX, hWnd, OldProc );

      SendMessage( hWnd, WM_SETFONT, ( WPARAM ) wvw_win->hEBfont, ( LPARAM ) TRUE );

      hb_retnl( nCtrlId );
   }
   else
      hb_retnl( 0 );
}

/* wvw_ebDestroy( [nWinNum], nEBid )
 * destroy editbox nEBid for window nWinNum
 */
HB_FUNC( WVW_EBDESTROY )
{
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   HB_UINT    nCtrlId = ( HB_UINT ) hb_parnl( 2 );
   WVW_CTRL * pcd     = wvw_win->pcdList;
   WVW_CTRL * pcdPrev = NULL;

   while( pcd )
   {
      if( pcd->nClass == WVW_CONTROL_EDITBOX && pcd->nId == nCtrlId )
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

/* wvw_ebSetFocus( [nWinNum], nEditId )
 * set the focus to editbox nEditId in window nWinNum
 */
HB_FUNC( WVW_EBSETFOCUS )
{
   HWND hWnd = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, ( HB_UINT ) hb_parnl( 2 ), NULL );

   hb_retl( hWnd && SetFocus( hWnd ) != NULL );
}

/* wvw_ebIsFocused( [nWinNum], nEditId )
 * returns .T. if the focus is on editbox nEditId in window nWinNum
 */
HB_FUNC( WVW_EBISFOCUSED )
{
   HWND hWnd = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, ( HB_UINT ) hb_parnl( 2 ), NULL );

   hb_retl( GetFocus() == hWnd );
}

/* wvw_ebEnable( [nWinNum], nEditId, [lEnable] )
 *  enable/disable editbox nEditId on window nWinNum
 * (lEnable defaults to .T., ie. enabling the editbox)
 *  return previous state of the editbox (TRUE:enabled FALSE:disabled)
 * (if nEditId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_EBENABLE )
{
   HB_UINT nWin = WVW_WHICH_WINDOW;
   HWND    hWnd = hb_gt_wvw_FindControlHandle( nWin, WVW_CONTROL_EDITBOX, ( HB_UINT ) hb_parnl( 2 ), NULL );

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

/* wvw_ebEditable( [nWinNum], nEditId, [lEditable] )
 *  get/set editability attribute from editbox nEditId on window nWinNum
 * (if lEditable is not specified, no change to editability)
 *  return previous state of the editbox (TRUE:editable FALSE:not editable)
 * (if nEditId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_EBEDITABLE )
{
   HWND hWnd = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, ( HB_UINT ) hb_parnl( 2 ), NULL );

   if( hWnd )
   {
      hb_retl( ( GetWindowLong( hWnd, GWL_STYLE ) & ES_READONLY ) != ES_READONLY );

      if( HB_ISLOG( 3 ) )
         SendMessage( hWnd, EM_SETREADONLY, ( WPARAM ) ! hb_parl( 3 ), 0 );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_ebSetCodeblock( [nWinNum], nEBid, bBlock )
 * assign (new) codeblock bBlock to editbox nEBid for window nWinNum
 *
 * return .T. if successful
 */
HB_FUNC( WVW_EBSETCODEBLOCK )
{
   WVW_CTRL * pcd    = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, NULL, ( HB_UINT ) hb_parnl( 2 ) );
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

/* wvw_ebSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality, ;
 *                             lItalic, lUnderline, lStrikeout
 *
 * this will initialize font for ALL editboxes in window nWinNum
 * (including ones created later on)
 *
 * TODO: ? should nHeight be ignored, and always forced to use standard char height?
 */
HB_FUNC( WVW_EBSETFONT )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );

   HB_BOOL fResult = HB_TRUE;

   wvw->lfEB.lfHeight         = hb_parnldef( 3, wvw_win->fontHeight - 2 );
   wvw->lfEB.lfWidth          = hb_parnldef( 4, wvw->lfEB.lfWidth );
   wvw->lfEB.lfEscapement     = 0;
   wvw->lfEB.lfOrientation    = 0;
   wvw->lfEB.lfWeight         = hb_parnldef( 5, wvw->lfEB.lfWeight );
   wvw->lfEB.lfQuality        = ( BYTE ) hb_parnidef( 6, wvw->lfEB.lfQuality );
   wvw->lfEB.lfItalic         = ( BYTE ) hb_parldef( 7, wvw->lfEB.lfItalic );
   wvw->lfEB.lfUnderline      = ( BYTE ) hb_parldef( 8, wvw->lfEB.lfUnderline );
   wvw->lfEB.lfStrikeOut      = ( BYTE ) hb_parldef( 9, wvw->lfEB.lfStrikeOut );
   wvw->lfEB.lfCharSet        = DEFAULT_CHARSET;
   wvw->lfEB.lfPitchAndFamily = FF_DONTCARE;

   if( HB_ISCHAR( 2 ) )
   {
      HB_ITEMCOPYSTR( hb_param( 2, HB_IT_STRING ), wvw->lfEB.lfFaceName, HB_SIZEOFARRAY( wvw->lfEB.lfFaceName ) );
      wvw_win->fontFace[ HB_SIZEOFARRAY( wvw->lfEB.lfFaceName ) - 1 ] = TEXT( '\0' );
   }

   if( wvw_win->hEBfont )
   {
      HFONT hOldFont = wvw_win->hEBfont;
      HFONT hFont    = CreateFontIndirect( &wvw->lfEB );
      if( hFont )
      {
         WVW_CTRL * pcd = wvw_win->pcdList;

         while( pcd )
         {
            if( pcd->nClass == WVW_CONTROL_EDITBOX &&
                ( HFONT ) SendMessage( pcd->hWnd, WM_GETFONT, 0, 0 ) == hOldFont )
               SendMessage( pcd->hWnd, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );

            pcd = pcd->pNext;
         }

         wvw_win->hEBfont = hFont;
         DeleteObject( hOldFont );
      }
      else
         fResult = HB_FALSE;
   }

   hb_retl( fResult );
}

/* wvw_ebIsMultiline( [nWinNum], nEBid )
 * returns .T. if editbox nEBid in window nWinNum is multiline
 * otherwise .F.
 * Also returns .F. if nEBid not valid
 */
HB_FUNC( WVW_EBISMULTILINE )
{
   WVW_CTRL * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, NULL, ( HB_UINT ) hb_parnl( 2 ) );

   if( pcd )
      hb_retl( ( pcd->nStyle & WVW_EB_MULTILINE ) == WVW_EB_MULTILINE );
   else
      hb_retl( HB_FALSE );
}

/* wvw_ebGetText( [nWinNum], nEBid, ;
 *                          lSoftBreak )
 * returns current text from editbox nEBid in window nWinNum
 * lSoftBreak: Default is .F.
 *             insert soft line break character (CR+CR+LF) at wordwrap positions
 *             can be useful to convert the text to MEMO format
 *             eg. converting editbox's softbreaks into memoline softbreak:
 *                cStr := wvw_ebGetText( NIL, nEBid, .T. )
 *                cStr := StrTran( cStr, CR + CR + LF, Chr( 141 ) + LF )
 *
 * returns "" in case of error (eg. nEBid not valid)
 */
HB_FUNC( WVW_EBGETTEXT )
{
   HB_UINT    nWin = WVW_WHICH_WINDOW;
   WVW_CTRL * pcd  = hb_gt_wvw_GetControlData( nWin, WVW_CONTROL_EDITBOX, NULL, ( HB_UINT ) hb_parnl( 2 ) );

   if( pcd )
   {
#if ! defined( UNICODE )
      WVW_WIN * wvw_win    = hb_gt_wvw_GetWindowsData( nWin );
#endif

      USHORT usLen;
      LPTSTR lpszText;

      if( hb_parl( 3 ) /* bSoftBreak */ )
         SendMessage( pcd->hWnd, EM_FMTLINES, ( WPARAM ) TRUE, 0 );

      usLen = ( USHORT ) SendMessage( pcd->hWnd, WM_GETTEXTLENGTH, 0, 0 ) + 1;

      lpszText = ( LPTSTR ) hb_xgrab( usLen * sizeof( TCHAR ) );

      SendMessage( pcd->hWnd, WM_GETTEXT, usLen, ( LPARAM ) lpszText );

#if ! defined( UNICODE )
      if( wvw_win->CodePage == OEM_CHARSET )
      {
         ULONG ulLen       = ( ULONG ) strlen( lpszText );
         LPSTR lpszTextOEM = ( LPSTR ) hb_xgrab( ulLen + 1 );
         CharToOem( lpszText, lpszTextOEM );
         hb_retc_buffer( lpszTextOEM );
      }
      else
         HB_RETSTR( lpszText );
#else
      HB_RETSTR( lpszText );
#endif

      hb_xfree( lpszText );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_ebSetText( [nWinNum], nEBid, cText )
 * set current text of editbox nEBid in window nWinNum
 * returns .T. if successful, .F. in case of error (eg. nEBid not valid)
 */
HB_FUNC( WVW_EBSETTEXT )
{
   HB_UINT    nWin = WVW_WHICH_WINDOW;
   WVW_CTRL * pcd  = hb_gt_wvw_GetControlData( nWin, WVW_CONTROL_EDITBOX, NULL, ( HB_UINT ) hb_parnl( 2 ) );

   if( pcd )
   {
#if ! defined( UNICODE )
      WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( nWin );
#endif

      void * hText;
      LPCTSTR lpszText = HB_PARSTRDEF( 3, &hText, NULL );

#if ! defined( UNICODE )
      if( wvw_win->CodePage == OEM_CHARSET )
      {
         ULONG ulLen        = ( ULONG ) strlen( lpszText );
         LPSTR lpszTextANSI = ( LPSTR ) hb_xgrab( ulLen + 1 );
         OemToCharBuff( lpszText, lpszTextANSI, ulLen );
         lpszText = ( LPCTSTR ) lpszTextANSI;
      }
#endif

      hb_retl( ( HB_BOOL ) ( BOOL ) SendMessage( pcd->hWnd, WM_SETTEXT, 0, ( LPARAM ) lpszText ) );

#if ! defined( UNICODE )
      if( wvw_win->CodePage == OEM_CHARSET )
         hb_xfree( lpszText );
#endif

      hb_strfree( hText );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_ebGetSel( [nWinNum], nEBid, @nstart, @nend )
 * get selected text editbox nEBid in window nWinNum
 * the start selected text (0-based) is in nstart
 * the end selected text (0-based) is in nend
 * returns .T. if operation successful
 * returns .F. if not (eg. nEBid not valid)
 */
HB_FUNC( WVW_EBGETSEL )
{
   WVW_CTRL * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, NULL, ( HB_UINT ) hb_parnl( 2 ) );
   DWORD      dwStart, dwEnd;

   if( pcd )
   {
      SendMessage( pcd->hWnd, EM_GETSEL, ( WPARAM ) &dwStart, ( LPARAM ) &dwEnd );

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
 * returns .T. if operation successful
 * returns .F. if not (eg. nEBid not valid)
 */
HB_FUNC( WVW_EBSETSEL )
{
   WVW_CTRL * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_EDITBOX, NULL, ( HB_UINT ) hb_parnl( 2 ) );

   if( pcd )
   {
      DWORD dwStart = ( DWORD ) hb_parnl( 3 );
      DWORD dwEnd   = ( DWORD ) hb_parnl( 4 );

      SendMessage( pcd->hWnd, EM_SETSEL, ( WPARAM ) dwStart, ( LPARAM ) dwEnd );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}
