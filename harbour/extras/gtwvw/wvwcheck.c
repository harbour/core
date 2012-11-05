/*
 * $Id$
 */

/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw checkbox/progressbar  functions
 * GTWVW is initially created based on:
 *
 * =Id: gtwvt.c,v 1.60 2004/01/26 08:14:07 vouchcac Exp =
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
 * See doc/license.txt for licensing terms.
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
 * along with this software; see the file COPYING.   If not, write to
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


/* CHECKBOX begins                                                   */


/*WVW_CXcreate( [nWinNum], nTop, nLeft, nBottom, nRight, cText, cImage/nImage, bBlock, aOffset,;
 *              nStretchBitmap, lMap3Dcolors)
 * create CHECKBOX for window nWinNum
 * nTop: row of top/left corner (in character unit)
 * nLeft: col of top/left corner (in character unit)
 * nBottom: row of bottom/right corner (in character unit) defaults==nTop
 * nRight: col of bottom/right corner (in character unit) defaults==??
 * cText: caption, default == ""
 *
 * cImage: bitmap file name, can be supplied as nImage: bitmap resource id
 * nStretchBitmap: a number between 0 and 1 (inclusive) as a factor to
 *                stretch the bitmap.
 *                1.0: bitmap covers the whole button
 *                0.5: bitmap covers 50% of button
 *                0: bitmap is not stretch
 *               (default is 1)
 * lMap3Dcolors: defaults to .f.
 *           if .t. the following color mapping will be performed:
 *              RGB(192,192,192) --> COLOR_3DFACE   ("transparent")
 *              RGB(128,128,128) --> COLOR_3DSHADOW
 *              RGB(223,223,223) --> COLOR_3DLIGHT
 *           This might be desirable to have transparent effect.
 *           LIMITATION: this will work on 256 colored bitmaps only
 *
 * aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *         dimension of CHECKBOX.
 *         defaults for CHECKBOX: {-2,-2,+2,+2}
 *
 * bBlock:  codeblock to execute on every BN_CLICK event.
 *         This codeblock will be evaluated with these parameters:
 *         nWinNum: window number
 *         nCXid  : CHECKBOX id
 *
 * returns control id of newly created CHECKBOX of windows nWinNum
 * returns 0 if failed
 *
 * example:
 */

HB_FUNC( WVW_CXCREATE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   int  iOffTop, iOffLeft, iOffBottom, iOffRight;
   // int   iStyle;
   UINT   uiPBid;
   USHORT usTop         = ( BYTE ) hb_parni( 2 ),
          usLeft        = ( BYTE ) hb_parni( 3 ),
          usBottom      = ( BYTE ) hb_parni( 4 ),
          usRight       = ( BYTE ) hb_parni( 5 );
   LPCTSTR lpszCaption  = HB_ISCHAR( 6 ) ? hb_parcx( 6 ) : NULL;
   char *  szBitmap     = HB_ISCHAR( 7 ) ? ( char * ) hb_parcx( 7 ) : NULL;
   UINT    uiBitmap     = HB_ISNUM( 7 ) ? ( UINT ) hb_parni( 7 ) : 0;
   double  dStretch     = ! HB_ISNIL( 10 ) ? hb_parnd( 10 ) : 1;
   BOOL    bMap3Dcolors = HB_ISLOG( 11 ) ? ( BOOL ) hb_parl( 11 ) : FALSE;

   if( ! HB_ISBLOCK( 8 ) )
   {
      hb_retnl( 0 );
      return;
   }

   iOffTop    = ! HB_ISNIL( 9 ) ? hb_parvni( 9, 1 ) : -2;
   iOffLeft   = ! HB_ISNIL( 9 ) ? hb_parvni( 9, 2 ) : -2;
   iOffBottom = ! HB_ISNIL( 9 ) ? hb_parvni( 9, 3 ) : +2;
   iOffRight  = ! HB_ISNIL( 9 ) ? hb_parvni( 9, 4 ) : +2;

   uiPBid = ButtonCreate( usWinNum, usTop, usLeft, usBottom, usRight, lpszCaption,
                          szBitmap, uiBitmap, hb_param( 8, HB_IT_BLOCK ),
                          iOffTop, iOffLeft, iOffBottom, iOffRight,
                          dStretch, bMap3Dcolors,
                          BS_AUTOCHECKBOX );
   hb_retnl( ( LONG ) uiPBid );
}

/*WVW_CXdestroy( [nWinNum], nCXid )
   *destroy checkbox nCXid for window nWinNum
 */
HB_FUNC( WVW_CXDESTROY )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA *     pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   UINT           uiCXid      = ( UINT ) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   CONTROL_DATA * pcd         = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev     = NULL;

   while( pcd )
   {
      if( pcd->byCtrlClass == WVW_CONTROL_CHECKBOX && pcd->uiCtrlid == uiCXid )
      {
         break;
      }
      pcdPrev = pcd;
      pcd     = pcd->pNext;
   }

   if( pcd == NULL )
   {
      return;
   }

   DestroyWindow( pcd->hWndCtrl );

   if( pcdPrev == NULL )
   {
      pWindowData->pcdCtrlList = pcd->pNext;
   }
   else
   {
      pcdPrev->pNext = pcd->pNext;
   }

   if( pcd->phiCodeBlock )
   {
      hb_itemRelease( pcd->phiCodeBlock );

   }

   hb_xfree( pcd );
}

/*WVW_CXsetFocus( [nWinNum], nButtonId )
   *set the focus to checkbox nButtonId in window nWinNum
 */
HB_FUNC( WVW_CXSETFOCUS )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   UINT uiCtrlId = HB_ISNIL( 2 ) ? 0 : hb_parni( 2 );
   byte bStyle;
   HWND hWndCX = FindControlHandle( usWinNum, WVW_CONTROL_CHECKBOX, uiCtrlId, &bStyle );

   if( hWndCX )
   {

      hb_retl( SetFocus( hWndCX ) != NULL );
   }
   else
   {
      hb_retl( FALSE );
   }
}

/*WVW_CXenable( [nWinNum], nButtonId, [lToggle] )
   *enable/disable checkbox nButtonId on window nWinNum
   *(lToggle defaults to .t., ie. enabling the checkbox)
   *return previous state of the checkbox (TRUE:enabled FALSE:disabled)
   *(if nButtonId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_CXENABLE )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   UINT       uiCtrlId = HB_ISNIL( 2 ) ? 0 : hb_parni( 2 );
   BOOL       bEnable  = HB_ISNIL( 3 ) ? TRUE : hb_parl( 3 );
   byte       bStyle;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   HWND       hWndCX      = FindControlHandle( usWinNum, WVW_CONTROL_CHECKBOX, uiCtrlId, &bStyle );

   if( hWndCX )
   {
      hb_retl( EnableWindow( hWndCX, bEnable ) == 0 );

      if( ! bEnable )
      {
         SetFocus( pWindowData->hWnd );
      }
   }
   else
   {
      hb_retl( FALSE );
   }
}

/*WVW_CXsetcodeblock( [nWinNum], nCXid, bBlock )
   *assign (new) codeblock bBlock to button nCXid for window nWinNum
 *
 * return .t. if successful
 */
HB_FUNC( WVW_CXSETCODEBLOCK )
{
   UINT usWinNum               = WVW_WHICH_WINDOW;
   WVW_DATA *     pData        = hb_getWvwData();
   UINT           uiCXid       = ( UINT ) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   CONTROL_DATA * pcd          = GetControlData( usWinNum, WVW_CONTROL_CHECKBOX, NULL, uiCXid );
   PHB_ITEM       phiCodeBlock = hb_param( 3, HB_IT_BLOCK );
   BOOL           bOldSetting  = pData->s_bRecurseCBlock;

   if( ! phiCodeBlock || pcd == NULL || pcd->bBusy )
   {
      hb_retl( FALSE );
      return;
   }

   pData->s_bRecurseCBlock = FALSE;
   pcd->bBusy = TRUE;

   if( pcd->phiCodeBlock )
   {
      hb_itemRelease( pcd->phiCodeBlock );

   }

   pcd->phiCodeBlock = hb_itemNew( phiCodeBlock );

   pcd->bBusy = FALSE;
   pData->s_bRecurseCBlock = bOldSetting;

   hb_retl( TRUE );
}

/* WVW_CXsetcheck( [nWinNum], nCXid, nCheckState )
 * assigns check-state of checkbox nCXid
 *           0==unchecked    BST_UNCHECKED
 *           1==checked      BST_CHECKED
 *           2==indeterminate BST_INDETERMINATE
 * this function always returns .t.
 */
HB_FUNC( WVW_CXSETCHECK )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   UINT  uiCXid       = ( UINT ) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   ULONG ulCheck      = ( ULONG ) ( HB_ISNIL( 3 ) ? BST_CHECKED  : hb_parni( 3 ) );
   CONTROL_DATA * pcd = GetControlData( usWinNum, WVW_CONTROL_CHECKBOX, NULL, uiCXid );

   if( pcd->hWndCtrl )
   {
      SendMessage( pcd->hWndCtrl,
                   BM_SETCHECK, ( WPARAM ) ulCheck, ( LPARAM ) 0 );
   }

   hb_retl( TRUE );
}

/*WVW_CXgetcheck( [nWinNum], nCXid )
   *returns check-state of checkbox nCXid
 *           0==unchecked    BST_UNCHECKED
 *           1==checked      BST_CHECKED
 *           2==indeterminate BST_INDETERMINATE
 */
HB_FUNC( WVW_CXGETCHECK )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   UINT  uiCXid       = ( UINT ) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   ULONG ulCheck      = 0;
   CONTROL_DATA * pcd = GetControlData( usWinNum, WVW_CONTROL_CHECKBOX, NULL, uiCXid );

   if( pcd->hWndCtrl )
   {
      ulCheck = SendMessage( pcd->hWndCtrl,
                             BM_GETCHECK, ( WPARAM ) 0, ( LPARAM ) 0 );
   }

   hb_retnl( ulCheck );
}

/*WVW_CXSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality,;
 *                             lItalic, lUnderline, lStrikeout
 *
 */
HB_FUNC( WVW_CXSETFONT )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   WVW_DATA * pData       = hb_getWvwData();

   BOOL retval = TRUE;

   pData->s_lfCX.lfHeight      = HB_ISNIL( 3 ) ? pWindowData->fontHeight - 2 : hb_parnl( 3 );
   pData->s_lfCX.lfWidth       = HB_ISNIL( 4 ) ? pData->s_lfCX.lfWidth : hb_parni( 4 );
   pData->s_lfCX.lfEscapement  = 0;
   pData->s_lfCX.lfOrientation = 0;
   pData->s_lfCX.lfWeight      = HB_ISNIL( 5 ) ? pData->s_lfCX.lfWeight : hb_parni( 5 );
   pData->s_lfCX.lfItalic      = HB_ISNIL( 7 ) ? pData->s_lfCX.lfItalic    : ( BYTE ) hb_parl( 7 );
   pData->s_lfCX.lfUnderline   = HB_ISNIL( 8 ) ? pData->s_lfCX.lfUnderline : ( BYTE ) hb_parl( 8 );
   pData->s_lfCX.lfStrikeOut   = HB_ISNIL( 9 ) ? pData->s_lfCX.lfStrikeOut : ( BYTE ) hb_parl( 9 );
   pData->s_lfCX.lfCharSet     = DEFAULT_CHARSET;

   pData->s_lfCX.lfQuality        = HB_ISNIL( 6 ) ? pData->s_lfCX.lfQuality : ( BYTE ) hb_parni( 6 );
   pData->s_lfCX.lfPitchAndFamily = FF_DONTCARE;
   if( HB_ISCHAR( 2 ) )
   {
      strcpy( pData->s_lfCX.lfFaceName, hb_parcx( 2 ) );
   }

   if( pWindowData->hCXfont )
   {
      HFONT hOldFont = pWindowData->hCXfont;
      HFONT hFont    = CreateFontIndirect( &pData->s_lfCX );
      if( hFont )
      {
         /*CONTROL_DATA * pcd = pWindowData->pcdCtrlList;

            while (pcd)
            {
            if ((pcd->byCtrlClass == WVW_CONTROL_PUSHBUTTON) &&
               ((HFONT) SendMessage( pcd->hWndCtrl, WM_GETFONT, (WPARAM) 0, (LPARAM) 0) == hOldFont)
              )
            {
              SendMessage( pcd->hWndCtrl, WM_SETFONT, (WPARAM) hFont, (LPARAM) TRUE);
            }

            pcd = pcd->pNext;
            } */

         pWindowData->hCXfont = hFont;
         DeleteObject( ( HFONT ) hOldFont );

      }
      else
      {
         retval = FALSE;
      }
   }

   hb_retl( retval );
}

HB_FUNC( WVW_CXSTATUSFONT )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   UINT uiPBid        = ( UINT ) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   BOOL bFocus        = HB_ISNIL( 3 ) ? TRUE : hb_parl( 3 );
   CONTROL_DATA * pcd = GetControlData( usWinNum, WVW_CONTROL_PUSHBUTTON, NULL, uiPBid );

   if( pcd->hWndCtrl )
   {
      if( bFocus )
      {
         SendMessage( pcd->hWndCtrl, WM_SETFONT, ( WPARAM ) pWindowData->hCXfont, ( LPARAM ) TRUE );
      }
      else
      {
         SendMessage( pcd->hWndCtrl, WM_SETFONT, ( WPARAM ) pWindowData->hPBfont, ( LPARAM ) TRUE );
      }
   }

   hb_retl( TRUE );
}



/* CHECKBOX ends                                                     */





/* PROGRESSBAR begins                                                 */


/* WVW_PGcreate( [nWinNum], nTop, nLeft, nBottom, nRight, [aOffset],
 *                         [nBackColor], [nBarColor], [lSmooth], [lVertical])
 * create progress bar for window nWinNum
 * nTop: row of top/left corner (in character unit)
 * nLeft: col of top/left corner (in character unit)
 * nBottom: row of bottom/right corner (in character unit)
 * nRight: col of bottom/right corner (in character unit)
 * aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *        dimension of progress bar. defaults: {0, 0, 0, 0}
 * nBackColor: color of background (as RGB value)
 * nBarColor: color of bar (as RGB value)
 * lSmooth: if .t., draw as smooth bar (default is .f.)
 * lVertical: if .t., draw as vertical progress bar (default is .f.)
 *
 * returns control id of newly created progress bar of windows nWinNum
 * returns 0 if failed
 *
 * example:
 * WVW_PGcreate( , 5, 10, 5, 30)
 *  :: creates horiz progressbar on current window at (5,10) to (5,30)
 *     colors using default ones.
 *
 * WVW_PGcreate( , 5, 10, 5, 30, {-1, 0, +1, 0} )
 *  :: same as above, but the bar is enlarged 1 pixel to the top
 *     and 1 pixel to the bottom
 *
 * NOTES:
 * ProgressRange is initially set as 0 - 100.
 * Initial ProgressPos is 0
 */

HB_FUNC( WVW_PGCREATE )
{
   HANDLE     hInstance   = NULL;
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   HWND       hWndParent  = pWindowData->hWnd;
   HWND       hWndPG;
   POINT      xy = { 0 };
   int        iTop, iLeft, iBottom, iRight;
   int        iOffTop, iOffLeft, iOffBottom, iOffRight;
   int        iStyle     = 0;
   BOOL       bBackColor = ! HB_ISNIL( 7 );
   BOOL       bBarColor  = ! HB_ISNIL( 8 );
   BOOL       bSmooth    = ( ! HB_ISLOG( 9 ) ? FALSE : hb_parl( 9 ) );
   BOOL       bVertical  = ( ! HB_ISLOG( 10 ) ? FALSE : hb_parl( 10 ) );
   UINT       uiPGid;
   USHORT     usTop    = ( USHORT ) hb_parni( 2 ),
              usLeft   = ( USHORT ) hb_parni( 3 ),
              usBottom = ( USHORT ) hb_parni( 4 ),
              usRight  = ( USHORT ) hb_parni( 5 );

   InitCommonControls();

   iOffTop    = ! HB_ISNIL( 6 ) ? hb_parvni( 6, 1 ) : 0;
   iOffLeft   = ! HB_ISNIL( 6 ) ? hb_parvni( 6, 2 ) : 0;
   iOffBottom = ! HB_ISNIL( 6 ) ? hb_parvni( 6, 3 ) : 0;
   iOffRight  = ! HB_ISNIL( 6 ) ? hb_parvni( 6, 4 ) : 0;

   if( hb_gt_wvw_GetMainCoordMode() )
   {
      hb_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );
   }

   xy    = hb_gt_wvwGetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy      = hb_gt_wvwGetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );
   xy.y   -= pWindowData->byLineSpacing;
   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   uiPGid = LastControlId( usWinNum, WVW_CONTROL_PROGRESSBAR );
   if( uiPGid == 0 )
   {
      uiPGid = WVW_ID_BASE_PROGRESSBAR;
   }
   else
   {
      uiPGid++;
   }

   if( bVertical )
   {

      iStyle = iStyle | PBS_VERTICAL;
   }
   if( bSmooth )
   {

      iStyle = iStyle | PBS_SMOOTH;
   }

   hb_winmainArgGet( &hInstance, NULL, NULL );

   hWndPG = CreateWindowEx(
      0L,
      PROGRESS_CLASS,
      ( LPSTR ) NULL,
      WS_CHILD | WS_VISIBLE | ( DWORD ) iStyle,
      iLeft,
      iTop,
      iRight - iLeft + 1,
      iBottom - iTop + 1,
      hWndParent,
      ( HMENU ) uiPGid,
      ( HINSTANCE ) hInstance,
      ( LPVOID ) NULL
      );

   if( hWndPG )
   {
      RECT rXB = { 0 }, rOffXB = { 0 };

      if( bBackColor )
      {
         SendMessage( hWndPG, PBM_SETBKCOLOR, 0, ( LPARAM ) ( COLORREF ) hb_parnl( 7 ) );
      }
      if( bBarColor )
      {
         SendMessage( hWndPG, PBM_SETBARCOLOR, 0, ( LPARAM ) ( COLORREF ) hb_parnl( 8 ) );
      }

      SendMessage( hWndPG, PBM_SETRANGE, 0, MAKELPARAM( 0, 100 ) );
      SendMessage( hWndPG, PBM_SETPOS, ( WPARAM ) 0, 0 );

      rXB.top       = usTop;     rXB.left = usLeft;
      rXB.bottom    = usBottom; rXB.right = usRight;
      rOffXB.top    = iOffTop;     rOffXB.left = iOffLeft;
      rOffXB.bottom = iOffBottom; rOffXB.right = iOffRight;

      AddControlHandle( usWinNum, WVW_CONTROL_PROGRESSBAR, hWndPG, uiPGid, ( PHB_ITEM ) NULL, rXB, rOffXB, ( byte ) iStyle );

      hb_retnl( ( LONG ) uiPGid );
   }
   else
   {

      hb_retnl( ( LONG ) 0 );
   }
}

/*WVW_PGdestroy( [nWinNum], nPGid )
   *destroy progressbar nPGid for window nWinNum
   *This function has no return value.
 */
HB_FUNC( WVW_PGDESTROY )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   WIN_DATA *     pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   UINT           uiPGid      = ( UINT ) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   CONTROL_DATA * pcd         = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev     = NULL;

   while( pcd )
   {
      if( pcd->byCtrlClass == WVW_CONTROL_PROGRESSBAR && pcd->uiCtrlid == uiPGid )
      {
         break;
      }

      pcdPrev = pcd;
      pcd     = pcd->pNext;
   }
   if( pcd == NULL )
   {
      return;
   }

   DestroyWindow( pcd->hWndCtrl );

   if( pcdPrev == NULL )
   {
      pWindowData->pcdCtrlList = pcd->pNext;
   }
   else
   {
      pcdPrev->pNext = pcd->pNext;
   }

   if( pcd->phiCodeBlock )
   {
      hb_itemRelease( pcd->phiCodeBlock );

   }

   hb_xfree( pcd );
}

/* WVW_PGsetrange(nWinNum, PGid, [nMin], [nMax])
 *  update progressbar data range (default is 0-100)
 *  nMin: a number in range of -32767 to +32767
 *  nMax: a number in range of -32767 to +32767
 *
 * Remark: progress position is reset to nMin
 *
 * returns .t. if operation considered successfull
 */
HB_FUNC( WVW_PGSETRANGE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   UINT uiPGid = ( UINT ) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   byte bStyle;
   HWND hWndPG = FindControlHandle( usWinNum, WVW_CONTROL_PROGRESSBAR, uiPGid, &bStyle );
   int  iMin   = ( int ) ( HB_ISNIL( 3 ) ? 0 : hb_parni( 3 ) );
   int  iMax   = ( int ) ( HB_ISNIL( 4 ) ? 0 : hb_parni( 4 ) );

   if( uiPGid == 0 || hWndPG == NULL || ( iMin > iMax ) )
   {
      hb_retl( FALSE );
      return;
   }

   SendMessage( hWndPG, PBM_SETRANGE, 0, MAKELPARAM( iMin, iMax ) );
   SendMessage( hWndPG, PBM_SETPOS, ( WPARAM ) iMin, 0 );

   hb_retl( TRUE );
}

/*WVW_PGsetpos(nWinNum, PGid, [nPos])
   *update progressbar position within current range
   *nPos: a number in range of current range
   *returns .t. if operation considered successfull
 */
HB_FUNC( WVW_PGSETPOS )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   UINT    uiPGid = ( UINT ) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   byte    bStyle;
   HWND    hWndPG = FindControlHandle( usWinNum, WVW_CONTROL_PROGRESSBAR, uiPGid, &bStyle );
   int     iPos   = ( int ) ( HB_ISNIL( 3 ) ? 0 : hb_parni( 3 ) );
   PBRANGE pbrange;

   if( uiPGid == 0 || hWndPG == NULL )
   {
      hb_retl( FALSE );
      return;
   }

   SendMessage( hWndPG, PBM_GETRANGE, ( WPARAM ) TRUE, ( LPARAM ) &pbrange );

   if( iPos < pbrange.iLow || iPos > pbrange.iHigh )
   {
      hb_retl( FALSE );
      return;
   }

   SendMessage( hWndPG, PBM_SETPOS, ( WPARAM ) iPos, 0 );

   hb_retl( TRUE );
}

/*WVW_PGgetpos(nWinNum, PGid)
   *get progressbar current position
   *returns 0 if operation failed
 */
HB_FUNC( WVW_PGGETPOS )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   UINT uiPGid = ( UINT ) ( HB_ISNIL( 2 ) ? 0  : hb_parni( 2 ) );
   byte bStyle;
   HWND hWndPG = FindControlHandle( usWinNum, WVW_CONTROL_PROGRESSBAR, uiPGid, &bStyle );

   if( uiPGid == 0 || hWndPG == NULL )
   {
      hb_retni( 0 );
      return;
   }

   hb_retni( ( int ) SendMessage( hWndPG, PBM_GETPOS, ( WPARAM ) 0, ( LPARAM ) 0 ) );

}


/* PROGRESSBAR ends                                                   */
