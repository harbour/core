/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW combobox functions
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

static LRESULT CALLBACK hb_gt_wvw_CBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   HWND hWndParent = GetParent( hWnd );
   int  nWin;

   int     nCtrlId;
   WNDPROC OldProc;
   int     nKbdType;

   PWVW_GLO wvw = hb_gt_wvw();
   PWVW_WIN wvw_win;

   if( wvw == NULL || hWndParent == NULL )
      return DefWindowProc( hWnd, message, wParam, lParam );

   for( nWin = 0; nWin < wvw->iNumWindows; nWin++ )
   {
      if( wvw->pWin[ nWin ]->hWnd == hWndParent )
         break;
   }

   if( nWin >= wvw->iNumWindows )
      return DefWindowProc( hWnd, message, wParam, lParam );

   wvw_win = wvw->pWin[ nWin ];

   nCtrlId = hb_gt_wvw_FindControlId( wvw_win, WVW_CONTROL_COMBOBOX, hWnd, &nKbdType );
   if( nCtrlId == 0 )
   {
      hb_errInternal( 10010, "ComboBox: Control ID not found with hb_gt_wvw_FindControlId()", NULL, NULL );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   OldProc = hb_gt_wvw_GetControlProc( wvw_win, WVW_CONTROL_COMBOBOX, hWnd );
   if( OldProc == NULL )
   {
      hb_errInternal( 10011, "ComboBox: Failed hb_gt_wvw_GetControlProc()", NULL, NULL );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   switch( message )
   {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      {
         HB_BOOL bAlt   = GetKeyState( VK_MENU ) & 0x8000;
         HB_BOOL bCtrl  = GetKeyState( VK_CONTROL ) & 0x8000;
         HB_BOOL bShift = GetKeyState( VK_SHIFT ) & 0x8000;
         int     c      = ( int ) wParam;
         HB_BOOL fDropped;

         if( ! hb_gt_wvw_BufferedKey( ( int ) wParam ) )
            break;

         fDropped = ( HB_BOOL ) SendMessage( hWnd, CB_GETDROPPEDSTATE, 0, 0 );

         if( nKbdType == WVW_CB_KBD_STANDARD )
         {
            switch( c )
            {
               case VK_F4:
                  if( bAlt )
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }
                  break;

               case VK_ESCAPE:
                  if( ! bCtrl && ! bAlt && ! bShift && ! fDropped )
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }
                  break;

               case VK_TAB:
                  if( ! bCtrl && ! bAlt )
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }
                  break;

               case VK_NEXT:

                  if( fDropped || bAlt || bShift || bCtrl )
                     break;
                  else
                  {
                     SendMessage( hWnd, CB_SHOWDROPDOWN, ( WPARAM ) TRUE, 0 );
                     return 0;
                  }

               case VK_RETURN:
                  if( ! bCtrl && ! bAlt && ! bShift && ! fDropped )
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }
                  break;
            }
            break;

         }     /* WVW_CB_KBD_STANDARD */
         else  /* assume WVW_CB_KBD_CLIPPER */
         {
            switch( c )
            {
               case VK_F4:
                  if( bAlt )
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }
                  break;

               case VK_RETURN:

                  if( fDropped || bAlt || bShift || bCtrl )
                     break;
                  else
                  {
                     SendMessage( hWnd, CB_SHOWDROPDOWN, ( WPARAM ) TRUE, 0 );
                     return 0;
                  }

               case VK_ESCAPE:
                  if( fDropped || bAlt || bShift || bCtrl )
                     break;
                  else
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }

               case VK_UP:
               case VK_DOWN:
               case VK_RIGHT:
               case VK_LEFT:
               case VK_HOME:
               case VK_END:
               case VK_PRIOR:
               case VK_NEXT:
                  if( fDropped )
                     break;
                  else
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }

               case VK_TAB:
                  if( ! bCtrl && ! bAlt )
                  {
                     SetFocus( hWndParent );
                     PostMessage( hWndParent, message, wParam, lParam );
                     return 0;
                  }
                  break;
            }
            break;
         }
      }
   }

   return CallWindowProc( OldProc, hWnd, message, wParam, lParam );
}

static int hb_gt_wvw_GetFontDialogUnits( HWND hWnd, HFONT hFont )
{
   const TCHAR tmp[] = TEXT( "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" );

   SIZE sz;

   HDC hDC = GetDC( hWnd );  /* get the DC to the window */

   SelectObject( hDC, hFont );  /* with the current font attributes, select the font */
   GetTextExtentPoint32( hDC, tmp, HB_SIZEOFARRAY( tmp ), &sz );  /* get its length */

   ReleaseDC( hWnd, hDC );

   return sz.cx / HB_SIZEOFARRAY( tmp );  /* calculate the average character width */
}

/* wvw_cbCreate( [nWinNum], nTop, nLeft, nWidth, aText, bBlock, nListLines, ;
 *                          nReserved, nKbdType, aOffset, @hControl, nStyle )
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
 *           WVW_CB_KBD_CLIPPER (1):
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
 */
HB_FUNC( WVW_CBCREATE )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      HWND hWnd;

      POINT xy;

      int iOffTop, iOffLeft, iOffBottom, iOffRight;
      int nCtrlId;

      int iWidth  = hb_parni( 4 );
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = iTop,
          iRight  = iLeft + iWidth - 1;

      int iNumElement = HB_ISARRAY( 5 ) ? ( int ) hb_arrayLen( hb_param( 5, HB_IT_ARRAY ) ) : 0;

      RECT rXB, rOffXB;

      if( wvw_win->hCBfont == NULL )
      {
         wvw_win->hCBfont = CreateFontIndirect( &wvw->lfCB );
         if( wvw_win->hCBfont == NULL )
         {
            hbwapi_stor_HANDLE( NULL, 11 );
            hb_retni( 0 );
            return;
         }
      }

      iOffTop    = hb_parvni( 10, 1 );
      iOffLeft   = hb_parvni( 10, 2 );
      iOffBottom = hb_parnidef( 7, 3 );  /* nListLines */
      iOffRight  = hb_parvni( 10, 4 );

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
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + ( iOffBottom * hb_gt_wvw_LineHeight( wvw_win ) );
      iRight  = xy.x - 1 + iOffRight;

      nCtrlId = hb_gt_wvw_LastControlId( wvw_win, WVW_CONTROL_COMBOBOX );
      if( nCtrlId == 0 )
         nCtrlId = WVW_ID_BASE_COMBOBOX;
      else
         nCtrlId++;

      InitCommonControls();

      hWnd = CreateWindowEx(
         0,
         TEXT( "COMBOBOX" ),
         NULL,
         WS_CHILD | WS_VISIBLE | CBS_DROPDOWNLIST | WS_VSCROLL | hb_parni( 12 ) /* nStyle */,
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
         int LongComboWidth = 0, NewLongComboWidth;

         SendMessage( hWnd, WM_SETREDRAW, ( WPARAM ) TRUE, 0 );

         if( iNumElement == 0 )
         {
            if( SendMessage( hWnd, CB_ADDSTRING, 0, ( LPARAM ) TEXT( "empty" ) ) < 0 )
            {
               /* ignore failure */
            }
         }
         else
         {
            int i;

            for( i = 1; i <= iNumElement; i++ )
            {
               void * hText;

               if( SendMessage( hWnd, CB_ADDSTRING, 0, ( LPARAM ) HB_PARASTR( 5, i, &hText, NULL ) ) < 0 )
               {
                  /* ignore failure */
               }
               else
               {
                  int numofchars = ( int ) SendMessage( hWnd, CB_GETLBTEXTLEN, i - 1, 0 );
                  if( numofchars > LongComboWidth )
                     LongComboWidth = numofchars;
               }

               hb_strfree( hText );
            }
         }

         SendMessage( hWnd, CB_SETCURSEL, 0, 0 );
         SendMessage( hWnd, CB_SETEXTENDEDUI, ( WPARAM ) TRUE, 0 );

         {
            HFONT hFont = hb_gt_wvw_GetFont( wvw_win->fontFace, 10, wvw_win->fontWidth, wvw_win->fontWeight, wvw_win->fontQuality, wvw_win->CodePage );
            NewLongComboWidth = ( LongComboWidth - 2 ) * hb_gt_wvw_GetFontDialogUnits( wvw_win->hWnd, hFont );
            DeleteObject( hFont );
         }
         SendMessage( hWnd, CB_SETDROPPEDWIDTH, ( WPARAM ) NewLongComboWidth + 100 /* LongComboWidth + 100 */, 0 );

         hb_gt_wvw_AddControlHandle( wvw_win, WVW_CONTROL_COMBOBOX, hWnd, nCtrlId, hb_param( 6, HB_IT_EVALITEM ),
                                     rXB, rOffXB, hb_parnidef( 9, WVW_CB_KBD_STANDARD ) );
         hb_gt_wvw_StoreControlProc( wvw_win, WVW_CONTROL_COMBOBOX, hWnd,
            ( WNDPROC ) SetWindowLongPtr( hWnd, GWLP_WNDPROC, ( LONG_PTR ) hb_gt_wvw_CBProc ) );

         SendMessage( hWnd, WM_SETFONT, ( WPARAM ) wvw_win->hCBfont, ( LPARAM ) TRUE );

         hbwapi_stor_HANDLE( hWnd, 11 );
         hb_retni( nCtrlId );
         return;
      }
   }

   hbwapi_stor_HANDLE( NULL, 11 );
   hb_retni( 0 );
}

/* wvw_cbDestroy( [nWinNum], nCBid )
   destroy combobox nCBid for window nWinNum */
HB_FUNC( WVW_CBDESTROY )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int      nCtrlId     = hb_parni( 2 );
      PWVW_CTL wvw_ctl     = wvw_win->ctlList;
      PWVW_CTL wvw_ctlPrev = NULL;

      while( wvw_ctl )
      {
         if( wvw_ctl->nClass == WVW_CONTROL_COMBOBOX && wvw_ctl->nId == nCtrlId )
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

/* wvw_cbSetFocus( [nWinNum], nComboId )
   set the focus to combobox nComboId in window nWinNum */
HB_FUNC( WVW_CBSETFOCUS )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_COMBOBOX, hb_parni( 2 ), NULL );

   hb_retl( hWnd && SetFocus( hWnd ) != NULL );
}

/* wvw_cbIsFocused( [nWinNum], nComboId )
   returns .T. if the focus is on combobox nComboId in window nWinNum */
HB_FUNC( WVW_CBISFOCUSED )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_COMBOBOX, hb_parni( 2 ), NULL );

   hb_retl( hWnd && GetFocus() == hWnd );
}

/* wvw_cbEnable( [nWinNum], nComboId, [lEnable] )
 * enable/disable button nComboId on window nWinNum
 *(lEnable defaults to .T., ie. enabling the combobox)
 * return previous state of the combobox (TRUE:enabled FALSE:disabled)
 *(if nComboId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_CBENABLE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_COMBOBOX, hb_parni( 2 ), NULL );

   if( hWnd )
   {
      HB_BOOL fEnable = hb_parldef( 3, HB_TRUE );

      hb_retl( EnableWindow( hWnd, ( BOOL ) fEnable ) == 0 );

      if( ! fEnable )
         SetFocus( wvw_win->hWnd );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_cbSetCodeblock( [nWinNum], nCBid, bBlock )
   assign (new) codeblock bBlock to combobox nCBid for window nWinNum
   return .T. if successful */
HB_FUNC( WVW_CBSETCODEBLOCK )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_COMBOBOX, NULL, hb_parni( 2 ) );
   PHB_ITEM pBlock  = hb_param( 3, HB_IT_EVALITEM );

   if( pBlock && wvw_ctl && ! wvw_ctl->fBusy )
   {
      PWVW_GLO wvw         = hb_gt_wvw();
      HB_BOOL  fOldSetting = wvw->fRecurseCBlock;

      wvw->fRecurseCBlock = HB_FALSE;
      wvw_ctl->fBusy      = HB_TRUE;

      if( wvw_ctl->pBlock )
         hb_itemRelease( wvw_ctl->pBlock );

      wvw_ctl->pBlock = hb_itemNew( pBlock );

      wvw_ctl->fBusy      = HB_FALSE;
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
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
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
            PWVW_CTL wvw_ctl = wvw_win->ctlList;

            while( wvw_ctl )
            {
               if( wvw_ctl->nClass == WVW_CONTROL_COMBOBOX &&
                   ( HFONT ) SendMessage( wvw_ctl->hWnd, WM_GETFONT, 0, 0 ) == hOldFont )
                  SendMessage( wvw_ctl->hWnd, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );

               wvw_ctl = wvw_ctl->pNext;
            }

            wvw_win->hCBfont = hFont;
            DeleteObject( hOldFont );
         }
         else
            fResult = HB_FALSE;
      }

      hb_retl( fResult );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_cbSetIndex( [nWinNum], nCBid, nIndex )
 *  set current selection of combobox nCBid in window nWinNum to nIndex
 *  (nIndex is 0 based)
 *  returns .T. if successful.
 *
 * NOTE: the better name to this function should be wvw_cbSetCurSel()
 *      but that name is already used.
 *      (wvw_cbSetCurSel() and wvw_cbAddString() is NOT related to other
 *       WVW_CB* functions)
 */
HB_FUNC( WVW_CBSETINDEX )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_COMBOBOX, NULL, hb_parni( 2 ) );
   int      iIndex  = hb_parni( 3 );

   if( wvw_ctl && iIndex >= 0 )
      hb_retl( ( HB_BOOL ) SendMessage( wvw_ctl->hWnd, CB_SETCURSEL, ( WPARAM ) iIndex, 0 ) == iIndex );
   else
      hb_retl( HB_FALSE );
}

/* wvw_cbGetIndex( [nWinNum], nCBid )
 *  get current selection of combobox nCBid in window nWinNum
 *  return nIndex (0 based)
 *  returns CB_ERR (-1) if none selected
 *
 * NOTE: the better name to this function should be WVW_CBgetCurSel()
 *      but that name is potentially misleading to wvw_cbSetCurSel
 *      which is not our family of wvw_cb*() functions
 *      (wvw_cbSetCurSel() and wvw_cbAddString() is NOT related to other
 *       WVW_CB* functions)
 */
HB_FUNC( WVW_CBGETINDEX )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_COMBOBOX, NULL, hb_parni( 2 ) );

   if( wvw_ctl )
      hb_retni( ( int ) SendMessage( wvw_ctl->hWnd, CB_GETCURSEL, 0, 0 ) );
   else
      hb_retni( CB_ERR );
}

/* wvw_cbFindString( [nWinNum], nCBid, cString )
    find index of cString in combobox nCBid in window nWinNum
    returns index of cString (0 based)
    returns CB_ERR (-1) if string not found
   NOTE:case insensitive */
HB_FUNC( WVW_CBFINDSTRING )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_COMBOBOX, NULL, hb_parni( 2 ) );

   if( wvw_ctl )
   {
      void * hStr;
      hb_retni( ( int ) SendMessage( wvw_ctl->hWnd, CB_FINDSTRING, ( WPARAM ) -1, ( LPARAM ) HB_PARSTRDEF( 3, &hStr, NULL ) ) );
      hb_strfree( hStr );
   }
   else
      hb_retni( CB_ERR );
}

/* wvw_cbGetCurText( [nWinNum], nCBid )
   get current selected cString in combobox nCBid in window nWinNum
   returns "" if none selected */
HB_FUNC( WVW_CBGETCURTEXT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_COMBOBOX, NULL, hb_parni( 2 ) );

   if( wvw_ctl )
   {
      int iCurSel  = ( int ) SendMessage( wvw_ctl->hWnd, CB_GETCURSEL, 0, 0 );
      int iTextLen = ( int ) SendMessage( wvw_ctl->hWnd, CB_GETLBTEXTLEN, ( WPARAM ) iCurSel, 0 );
      if( iTextLen == CB_ERR )
         hb_retc_null();
      else
      {
         LPTSTR lptstr = ( LPTSTR ) hb_xgrab( ( iTextLen + 1 ) * sizeof( TCHAR ) );

         if( SendMessage( wvw_ctl->hWnd, CB_GETLBTEXT, ( WPARAM ) iCurSel, ( LPARAM ) lptstr ) == CB_ERR )
            hb_retc_null();
         else
            HB_RETSTRLEN( lptstr, iTextLen );

         hb_xfree( lptstr );
      }
   }
   else
      hb_retc_null();
}

/* wvw_cbIsDropped( [nWinNum], nCBid )
   get current dropped state of combobox nCBid in window nWinNum
   returns .T. if listbox is being shown, otherwise .F.
   Also returns .F. if nCBid not valid */
HB_FUNC( WVW_CBISDROPPED )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_COMBOBOX, NULL, hb_parni( 2 ) );

   if( wvw_ctl )
      hb_retl( ( HB_BOOL ) SendMessage( wvw_ctl->hWnd, CB_GETDROPPEDSTATE, 0, 0 ) );
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WVW_CBVISIBLE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_COMBOBOX, hb_parni( 2 ), NULL );

   hb_retl( hWnd && ShowWindow( hWnd, hb_parldef( 3, HB_TRUE ) ? SW_SHOW : SW_HIDE ) == 0 );
}
