/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW edit functions
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

/* EDITBOX begins (experimental) */

static const int s_K_Ctrl[] =
{
   K_CTRL_A, K_CTRL_B, K_CTRL_C, K_CTRL_D, K_CTRL_E, K_CTRL_F, K_CTRL_G, K_CTRL_H,
   K_CTRL_I, K_CTRL_J, K_CTRL_K, K_CTRL_L, K_CTRL_M, K_CTRL_N, K_CTRL_O, K_CTRL_P,
   K_CTRL_Q, K_CTRL_R, K_CTRL_S, K_CTRL_T, K_CTRL_U, K_CTRL_V, K_CTRL_W, K_CTRL_X,
   K_CTRL_Y, K_CTRL_Z
};

static LRESULT CALLBACK hb_gt_wvw_EBProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   HWND hWndParent = GetParent( hWnd );
   int  nWin;

   int     nCtrlId;
   WNDPROC OldProc;
   int     nEBType;
   int     iKey;

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

   nCtrlId = hb_gt_wvw_FindControlId( wvw_win, WVW_CONTROL_EDITBOX, hWnd, &nEBType );
   if( nCtrlId == 0 )
   {
      hb_errInternal( 10010, "EditBox: Control ID not found with hb_gt_wvw_FindControlId()", NULL, NULL );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   OldProc = hb_gt_wvw_GetControlProc( wvw_win, WVW_CONTROL_EDITBOX, hWnd );
   if( OldProc == NULL )
   {
      hb_errInternal( 10011, "EditBox: Failed hb_gt_wvw_GetControlProc()", NULL, NULL );

      return DefWindowProc( hWnd, message, wParam, lParam );
   }

   iKey = 0;
   switch( message )
   {
      case WM_KEYDOWN:
      case WM_SYSKEYDOWN:
      {
         HB_BOOL bAlt = GetKeyState( VK_MENU ) & 0x8000;
         int     c    = ( int ) wParam;
         switch( c )
         {
            case VK_F1:
               iKey = hb_gt_wvw_JustTranslateKey( K_F1, K_SH_F1, K_ALT_F1, K_CTRL_F1 );
               break;
            case VK_F2:
               iKey = hb_gt_wvw_JustTranslateKey( K_F2, K_SH_F2, K_ALT_F2, K_CTRL_F2 );
               break;
            case VK_F3:
               iKey = hb_gt_wvw_JustTranslateKey( K_F3, K_SH_F3, K_ALT_F3, K_CTRL_F3 );
               break;
            case VK_F4:
               if( bAlt )
               {
                  SetFocus( hWndParent );
                  PostMessage( hWndParent, message, wParam, lParam );
                  return 0;
               }
               else
                  iKey = hb_gt_wvw_JustTranslateKey( K_F4, K_SH_F4, K_ALT_F4, K_CTRL_F4 );
               break;
            case VK_F5:
               iKey = hb_gt_wvw_JustTranslateKey( K_F5, K_SH_F5, K_ALT_F5, K_CTRL_F5 );
               break;
            case VK_F6:
               iKey = hb_gt_wvw_JustTranslateKey( K_F6, K_SH_F6, K_ALT_F6, K_CTRL_F6 );
               break;
            case VK_F7:
               iKey = hb_gt_wvw_JustTranslateKey( K_F7, K_SH_F7, K_ALT_F7, K_CTRL_F7 );
               break;
            case VK_F8:
               iKey = hb_gt_wvw_JustTranslateKey( K_F8, K_SH_F8, K_ALT_F8, K_CTRL_F8 );
               break;
            case VK_F9:
               iKey = hb_gt_wvw_JustTranslateKey( K_F9, K_SH_F9, K_ALT_F9, K_CTRL_F9 );
               break;
            case VK_F10:
               iKey = hb_gt_wvw_JustTranslateKey( K_F10, K_SH_F10, K_ALT_F10, K_CTRL_F10 );
               break;
            case VK_F11:
               iKey = hb_gt_wvw_JustTranslateKey( K_F11, K_SH_F11, K_ALT_F11, K_CTRL_F11 );
               break;
            case VK_F12:
               iKey = hb_gt_wvw_JustTranslateKey( K_F12, K_SH_F12, K_ALT_F12, K_CTRL_F12 );
               break;
         }
         break;
      }

      case WM_CHAR:
      {
         HB_BOOL bCtrl     = GetKeyState( VK_CONTROL ) & 0x8000;
         int     iScanCode = HB_LOBYTE( HIWORD( lParam ) );
         int     c         = ( int ) wParam;

         if( bCtrl && iScanCode == 28 )
            iKey = K_CTRL_RETURN;
         else if( bCtrl && ( c >= 1 && c <= 26 ) )
            iKey = s_K_Ctrl[ c - 1 ];
         else
         {
            switch( c )
            {
               case VK_BACK:
                  iKey = hb_gt_wvw_JustTranslateKey( K_BS, K_SH_BS, K_ALT_BS, K_CTRL_BS );
                  break;
               case VK_TAB:
                  iKey = hb_gt_wvw_JustTranslateKey( K_TAB, K_SH_TAB, K_ALT_TAB, K_CTRL_TAB );
                  break;
               case VK_RETURN:
                  iKey = hb_gt_wvw_JustTranslateKey( K_RETURN, K_SH_RETURN, K_ALT_RETURN, K_CTRL_RETURN );
                  break;
               case VK_ESCAPE:
                  iKey = K_ESC;
                  break;
               default:
#if ! defined( UNICODE )
                  if( wvw_win->CodePage == OEM_CHARSET )
                     c = hb_gt_wvw_key_ansi_to_oem( c );
#endif
                  iKey = c;
            }
         }
         break;
      }

      case WM_SYSCHAR:
      {
         int c, iScanCode = HB_LOBYTE( HIWORD( lParam ) );
         switch( iScanCode )
         {
            case  2:
               c = K_ALT_1;
               break;
            case  3:
               c = K_ALT_2;
               break;
            case  4:
               c = K_ALT_3;
               break;
            case  5:
               c = K_ALT_4;
               break;
            case  6:
               c = K_ALT_5;
               break;
            case  7:
               c = K_ALT_6;
               break;
            case  8:
               c = K_ALT_7;
               break;
            case  9:
               c = K_ALT_8;
               break;
            case 10:
               c = K_ALT_9;
               break;
            case 11:
               c = K_ALT_0;
               break;
            case 13:
               c = K_ALT_EQUALS;
               break;
            case 14:
               c = K_ALT_BS;
               break;
            case 16:
               c = K_ALT_Q;
               break;
            case 17:
               c = K_ALT_W;
               break;
            case 18:
               c = K_ALT_E;
               break;
            case 19:
               c = K_ALT_R;
               break;
            case 20:
               c = K_ALT_T;
               break;
            case 21:
               c = K_ALT_Y;
               break;
            case 22:
               c = K_ALT_U;
               break;
            case 23:
               c = K_ALT_I;
               break;
            case 24:
               c = K_ALT_O;
               break;
            case 25:
               c = K_ALT_P;
               break;
            case 30:
               c = K_ALT_A;
               break;
            case 31:
               c = K_ALT_S;
               break;
            case 32:
               c = K_ALT_D;
               break;
            case 33:
               c = K_ALT_F;
               break;
            case 34:
               c = K_ALT_G;
               break;
            case 35:
               c = K_ALT_H;
               break;
            case 36:
               c = K_ALT_J;
               break;
            case 37:
               c = K_ALT_K;
               break;
            case 38:
               c = K_ALT_L;
               break;
            case 44:
               c = K_ALT_Z;
               break;
            case 45:
               c = K_ALT_X;
               break;
            case 46:
               c = K_ALT_C;
               break;
            case 47:
               c = K_ALT_V;
               break;
            case 48:
               c = K_ALT_B;
               break;
            case 49:
               c = K_ALT_N;
               break;
            case 50:
               c = K_ALT_M;
               break;
            default:
               c = ( int ) wParam;
         }
         iKey = c;
         break;
      }
   }

   if( iKey != 0 )
   {
      HB_BOOL  fCodeExec  = HB_FALSE;
      PHB_ITEM pKey       = hb_itemPutNI( NULL, iKey );
      PHB_ITEM pCodeblock = hb_itemDoC( "SETKEY", 1, pKey );
      if( HB_IS_EVALITEM( pCodeblock ) )
      {
         PHB_ITEM pReturn;
         SetFocus( hWndParent );
         pReturn = hb_itemDo( pCodeblock, 0 );
         hb_itemRelease( pReturn );
         SetFocus( hWnd );
         fCodeExec = HB_TRUE;
      }
      hb_itemRelease( pCodeblock );
      hb_itemRelease( pKey );
      if( fCodeExec )
         return 0;
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
         HB_BOOL fMultiline;

         if( ! hb_gt_wvw_BufferedKey( ( int ) wParam ) )
            break;

         fMultiline = ( ( nEBType & WVW_EB_MULTILINE ) == WVW_EB_MULTILINE );

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
               if( fMultiline || bAlt || bShift || bCtrl )
                  break;
               else if( ! fMultiline )
               {
                  SetFocus( hWndParent );
                  PostMessage( hWndParent, message, wParam, lParam );
                  return 0;
               }

            case VK_ESCAPE:

               if( bAlt || bShift || bCtrl )
                  break;
               else
               {
                  SetFocus( hWndParent );
                  PostMessage( hWndParent, message, wParam, lParam );
                  return 0;
               }

            case VK_UP:
            case VK_DOWN:

            case VK_PRIOR:
            case VK_NEXT:
               if( fMultiline )
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

            case VK_BACK:
               if( ! bAlt )
                  break;
               if( SendMessage( hWnd, EM_CANUNDO, 0, 0 ) )
               {
                  SendMessage( hWnd, EM_UNDO, 0, 0 );
                  return 0;
               }
               break;
         }
         break;
      }

      case WM_CHAR:
      {
         HB_BOOL bCtrl = GetKeyState( VK_CONTROL ) & 0x8000;
         switch( ( int ) wParam )
         {
            case VK_TAB:
               return 0;

            case 1:
               if( bCtrl )
               {
                  SendMessage( hWnd, EM_SETSEL, 0, ( LPARAM ) -1 );
                  return 0;
               }
               break;
         }
         break;
      }
   }

   return CallWindowProc( OldProc, hWnd, message, wParam, lParam );
}

/* wvw_ebCreate( [nWinNum], nTop, nLeft, nBottom, nRight, cText, bBlock, ;
 *                          lMultiline, nMoreStyle, nMaxChar, nReserved, aOffset )
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
 *             .T. :: multi line editbox
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
 */
HB_FUNC( WVW_EBCREATE )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      HWND  hWnd;
      POINT xy;

      int iOffTop, iOffLeft, iOffBottom, iOffRight;
      int nCtrlId;

      int nEBType = hb_parl( 8 ) ? WVW_EB_MULTILINE : WVW_EB_SINGLELINE;

      DWORD dwStyle  = ( DWORD ) hb_parnl( 9 );
      int   iMaxChar = hb_parni( 10 ) > 0 ? hb_parni( 10 ) : 0;

      RECT rXB, rOffXB;

      if( wvw_win->hEBfont == NULL )
      {
         wvw_win->hEBfont = CreateFontIndirect( &wvw->lfEB );
         if( wvw_win->hEBfont == NULL )
         {
            hb_retni( 0 );
            return;
         }
      }

      iOffTop    = hb_parvni( 12, 1 );
      iOffLeft   = hb_parvni( 12, 2 );
      iOffBottom = hb_parvni( 12, 3 );
      iOffRight  = hb_parvni( 12, 4 );

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

      nCtrlId = hb_gt_wvw_LastControlId( wvw_win, WVW_CONTROL_EDITBOX );
      if( nCtrlId == 0 )
         nCtrlId = WVW_ID_BASE_EDITBOX;
      else
         nCtrlId++;

      dwStyle |= WS_BORDER | WS_GROUP | WS_TABSTOP;

      if( ( nEBType & WVW_EB_MULTILINE ) == WVW_EB_MULTILINE )
         dwStyle |= ES_AUTOVSCROLL | ES_MULTILINE | ES_WANTRETURN | WS_BORDER | WS_VSCROLL;
      else
         dwStyle |= ES_AUTOHSCROLL;

      hWnd = CreateWindowEx(
         0,
         TEXT( "EDIT" ),
         NULL,
         WS_CHILD | WS_VISIBLE | ( DWORD ) dwStyle,
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
         void *  hText;
         LPCTSTR szText = HB_PARSTRDEF( 6, &hText, NULL );

         SendMessage( hWnd, WM_SETTEXT, 0, ( LPARAM ) szText );

         hb_strfree( hText );

         if( iMaxChar > 0 )
            SendMessage( hWnd, EM_LIMITTEXT, ( WPARAM ) iMaxChar, 0 );

         hb_gt_wvw_AddControlHandle( wvw_win, WVW_CONTROL_EDITBOX, hWnd, nCtrlId, hb_param( 7, HB_IT_EVALITEM ), rXB, rOffXB, nEBType );
         hb_gt_wvw_StoreControlProc( wvw_win, WVW_CONTROL_EDITBOX, hWnd,
            ( WNDPROC ) SetWindowLongPtr( hWnd, GWLP_WNDPROC, ( LONG_PTR ) hb_gt_wvw_EBProc ) );

         SendMessage( hWnd, WM_SETFONT, ( WPARAM ) wvw_win->hEBfont, ( LPARAM ) TRUE );

         hb_retni( nCtrlId );
         return;
      }
   }

   hb_retni( 0 );
}

/* wvw_ebDestroy( [nWinNum], nEBid )
   destroy editbox nEBid for window nWinNum */
HB_FUNC( WVW_EBDESTROY )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int      nCtrlId     = hb_parni( 2 );
      PWVW_CTL wvw_ctl     = wvw_win->ctlList;
      PWVW_CTL wvw_ctlPrev = NULL;

      while( wvw_ctl )
      {
         if( wvw_ctl->nClass == WVW_CONTROL_EDITBOX && wvw_ctl->nId == nCtrlId )
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

/* wvw_ebSetFocus( [nWinNum], nEditId )
   set the focus to editbox nEditId in window nWinNum */
HB_FUNC( WVW_EBSETFOCUS )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_EDITBOX, hb_parni( 2 ), NULL );

   hb_retl( hWnd && SetFocus( hWnd ) != NULL );
}

/* wvw_ebIsFocused( [nWinNum], nEditId )
   returns .T. if the focus is on editbox nEditId in window nWinNum */
HB_FUNC( WVW_EBISFOCUSED )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_EDITBOX, hb_parni( 2 ), NULL );

   hb_retl( hWnd && GetFocus() == hWnd );
}

/* wvw_ebEnable( [nWinNum], nEditId, [lEnable] )
 *  enable/disable editbox nEditId on window nWinNum
 * (lEnable defaults to .T., ie. enabling the editbox)
 *  return previous state of the editbox (TRUE:enabled FALSE:disabled)
 * (if nEditId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_EBENABLE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_EDITBOX, hb_parni( 2 ), NULL );

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

/* wvw_ebEditable( [nWinNum], nEditId, [lEditable] )
 *  get/set editability attribute from editbox nEditId on window nWinNum
 * (if lEditable is not specified, no change to editability)
 *  return previous state of the editbox (TRUE:editable FALSE:not editable)
 * (if nEditId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_EBEDITABLE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_EDITBOX, hb_parni( 2 ), NULL );

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
   assign (new) codeblock bBlock to editbox nEBid for window nWinNum
   return .T. if successful */
HB_FUNC( WVW_EBSETCODEBLOCK )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_EDITBOX, NULL, hb_parni( 2 ) );
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

/* wvw_ebSetFont( [nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality, ;
 *                           lItalic, lUnderline, lStrikeout )
 *
 * this will initialize font for ALL editboxes in window nWinNum
 * (including ones created later on)
 *
 * TODO: ? should nHeight be ignored, and always forced to use standard char height?
 */
HB_FUNC( WVW_EBSETFONT )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
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
            PWVW_CTL wvw_ctl = wvw_win->ctlList;

            while( wvw_ctl )
            {
               if( wvw_ctl->nClass == WVW_CONTROL_EDITBOX &&
                   ( HFONT ) SendMessage( wvw_ctl->hWnd, WM_GETFONT, 0, 0 ) == hOldFont )
                  SendMessage( wvw_ctl->hWnd, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );

               wvw_ctl = wvw_ctl->pNext;
            }

            wvw_win->hEBfont = hFont;
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

/* wvw_ebIsMultiline( [nWinNum], nEBid )
   returns .T. if editbox nEBid in window nWinNum is multiline
   otherwise .F.
   Also returns .F. if nEBid not valid */
HB_FUNC( WVW_EBISMULTILINE )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_EDITBOX, NULL, hb_parni( 2 ) );

   if( wvw_ctl )
      hb_retl( ( wvw_ctl->nStyle & WVW_EB_MULTILINE ) == WVW_EB_MULTILINE );
   else
      hb_retl( HB_FALSE );
}

/* wvw_ebGetText( [nWinNum], nEBid, lSoftBreak )
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
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_EDITBOX, NULL, hb_parni( 2 ) );

   if( wvw_ctl )
   {
      int    iLen;
      LPTSTR szText;

      if( hb_parl( 3 ) /* fSoftBreak */ )
         SendMessage( wvw_ctl->hWnd, EM_FMTLINES, ( WPARAM ) TRUE, 0 );

      iLen = ( int ) SendMessage( wvw_ctl->hWnd, WM_GETTEXTLENGTH, 0, 0 ) + 1;

      szText = ( LPTSTR ) hb_xgrab( ( iLen + 1 ) * sizeof( TCHAR ) );

      SendMessage( wvw_ctl->hWnd, WM_GETTEXT, iLen, ( LPARAM ) szText );

      HB_RETSTRLEN( szText, iLen );

      hb_xfree( szText );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_ebSetText( [nWinNum], nEBid, cText )
   set current text of editbox nEBid in window nWinNum
   returns .T. if successful, .F. in case of error (eg. nEBid not valid) */
HB_FUNC( WVW_EBSETTEXT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_EDITBOX, NULL, hb_parni( 2 ) );

   if( wvw_ctl )
   {
      void *  hText;
      LPCTSTR szText = HB_PARSTRDEF( 3, &hText, NULL );

      hb_retl( ( HB_BOOL ) SendMessage( wvw_ctl->hWnd, WM_SETTEXT, 0, ( LPARAM ) szText ) );

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
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_EDITBOX, NULL, hb_parni( 2 ) );
   DWORD    dwStart, dwEnd;

   if( wvw_ctl )
   {
      SendMessage( wvw_ctl->hWnd, EM_GETSEL, ( WPARAM ) &dwStart, ( LPARAM ) &dwEnd );

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
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_CTL wvw_ctl = hb_gt_wvw_ctl( wvw_win, WVW_CONTROL_EDITBOX, NULL, hb_parni( 2 ) );

   if( wvw_ctl )
   {
      DWORD dwStart = ( DWORD ) hb_parnl( 3 );
      DWORD dwEnd   = ( DWORD ) hb_parnl( 4 );

      SendMessage( wvw_ctl->hWnd, EM_SETSEL, ( WPARAM ) dwStart, ( LPARAM ) dwEnd );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}
