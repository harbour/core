/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw checkbox/progressbar  functions
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

/* wvw_cxCreate( [nWinNum], nTop, nLeft, nBottom, nRight, cText, cImage/nImage, bBlock, aOffset,;
 *               nStretchBitmap, lMap3Dcolors)
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
 */

HB_FUNC( WVW_CXCREATE )
{
   if( HB_ISEVALITEM( 8 ) )
   {
      USHORT usTop          = ( USHORT ) hb_parni( 2 ),
             usLeft         = ( USHORT ) hb_parni( 3 ),
             usBottom       = ( USHORT ) hb_parni( 4 ),
             usRight        = ( USHORT ) hb_parni( 5 );
      LPCTSTR lpszCaption   = hb_parc( 6 );
      const char * szBitmap = hb_parc( 7 );
      UINT    uiBitmap      = ( UINT ) hb_parni( 7 );
      double  dStretch      = HB_ISNUM( 10 ) ? hb_parnd( 10 ) : 1;
      BOOL    bMap3Dcolors  = ( BOOL ) hb_parl( 11 );

      int iOffTop    = HB_ISARRAY( 9 ) ? hb_parvni( 9, 1 ) : -2;
      int iOffLeft   = HB_ISARRAY( 9 ) ? hb_parvni( 9, 2 ) : -2;
      int iOffBottom = HB_ISARRAY( 9 ) ? hb_parvni( 9, 3 ) : 2;
      int iOffRight  = HB_ISARRAY( 9 ) ? hb_parvni( 9, 4 ) : 2;

      hb_retnl( hb_gt_wvw_ButtonCreate( WVW_WHICH_WINDOW, usTop, usLeft, usBottom, usRight, lpszCaption,
                             szBitmap, uiBitmap, hb_param( 8, HB_IT_EVALITEM ),
                             iOffTop, iOffLeft, iOffBottom, iOffRight,
                             dStretch, bMap3Dcolors,
                             BS_AUTOCHECKBOX ) );
   }
   else
      hb_retnl( 0 );
}

/* wvw_cxDestroy( [nWinNum], nCXid )
 * destroy checkbox nCXid for window nWinNum
 */
HB_FUNC( WVW_CXDESTROY )
{
   WIN_DATA *     pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   UINT           uiCXid      = ( UINT ) hb_parnl( 2 );
   CONTROL_DATA * pcd         = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev     = NULL;

   while( pcd )
   {
      if( pcd->byCtrlClass == WVW_CONTROL_CHECKBOX && pcd->uiCtrlid == uiCXid )
         break;
      pcdPrev = pcd;
      pcd     = pcd->pNext;
   }

   if( pcd )
   {
      DestroyWindow( pcd->hWndCtrl );

      if( pcdPrev == NULL )
         pWindowData->pcdCtrlList = pcd->pNext;
      else
         pcdPrev->pNext = pcd->pNext;

      if( pcd->phiCodeBlock )
         hb_itemRelease( pcd->phiCodeBlock );

      hb_xfree( pcd );
   }
}

/* wvw_cxSetFocus( [nWinNum], nButtonId )
 * set the focus to checkbox nButtonId in window nWinNum
 */
HB_FUNC( WVW_CXSETFOCUS )
{
   HWND hWndCX = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_CHECKBOX, ( UINT ) hb_parnl( 2 ), NULL );

   if( hWndCX )
      hb_retl( SetFocus( hWndCX ) != NULL );
   else
      hb_retl( HB_FALSE );
}

/* wvw_cxEnable( [nWinNum], nButtonId, [lToggle] )
 * enable/disable checkbox nButtonId on window nWinNum
 * (lToggle defaults to .t., ie. enabling the checkbox)
 * return previous state of the checkbox (TRUE:enabled FALSE:disabled)
 * (if nButtonId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_CXENABLE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;
   HWND hWndCX = hb_gt_wvw_FindControlHandle( usWinNum, WVW_CONTROL_CHECKBOX, ( UINT ) hb_parnl( 2 ), NULL );

   if( hWndCX )
   {
      BOOL bEnable = hb_parldef( 3, HB_TRUE );

      hb_retl( EnableWindow( hWndCX, bEnable ) == 0 );

      if( ! bEnable )
      {
         WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
         SetFocus( pWindowData->hWnd );
      }
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_cxSetCodeblock( [nWinNum], nCXid, bBlock )
 * assign (new) codeblock bBlock to button nCXid for window nWinNum
 *
 * return .t. if successful
 */
HB_FUNC( WVW_CXSETCODEBLOCK )
{
   WVW_DATA *     pData        = hb_gt_wvw_GetWvwData();
   CONTROL_DATA * pcd          = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_CHECKBOX, NULL, ( UINT ) hb_parnl( 2 ) );
   PHB_ITEM       phiCodeBlock = hb_param( 3, HB_IT_EVALITEM );
   BOOL           bOldSetting  = pData->bRecurseCBlock;

   if( phiCodeBlock && pcd && ! pcd->bBusy )
   {
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

/* wvw_cxSetCheck( [nWinNum], nCXid, nCheckState )
 * assigns check-state of checkbox nCXid
 *           0==unchecked    BST_UNCHECKED
 *           1==checked      BST_CHECKED
 *           2==indeterminate BST_INDETERMINATE
 * this function always returns .t.
 */
HB_FUNC( WVW_CXSETCHECK )
{
   CONTROL_DATA * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_CHECKBOX, NULL, ( UINT ) hb_parnl( 2 ) );

   if( pcd->hWndCtrl )
      SendMessage( pcd->hWndCtrl, BM_SETCHECK, ( WPARAM ) ( ULONG ) hb_parnidef( 3, BST_CHECKED ), 0 );

   hb_retl( HB_TRUE );
}

/* wvw_cxGetCheck( [nWinNum], nCXid )
 * returns check-state of checkbox nCXid
 *           0==unchecked    BST_UNCHECKED
 *           1==checked      BST_CHECKED
 *           2==indeterminate BST_INDETERMINATE
 */
HB_FUNC( WVW_CXGETCHECK )
{
   ULONG ulCheck;
   CONTROL_DATA * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_CHECKBOX, NULL, ( UINT ) hb_parnl( 2 ) );

   if( pcd->hWndCtrl )
      ulCheck = ( int ) SendMessage( pcd->hWndCtrl, BM_GETCHECK, 0, 0 );
   else
      ulCheck = 0;

   hb_retnl( ulCheck );
}

/* wvw_cxSetFont( [nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality, lItalic, lUnderline, lStrikeout ) */
HB_FUNC( WVW_CXSETFONT )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   WVW_DATA * pData       = hb_gt_wvw_GetWvwData();

   HB_BOOL retval = HB_TRUE;

   pData->lfCX.lfHeight      = HB_ISNUM( 3 ) ? hb_parnl( 3 ) : pWindowData->fontHeight - 2;
   pData->lfCX.lfWidth       = HB_ISNUM( 4 ) ? hb_parni( 4 ) : pData->lfCX.lfWidth;
   pData->lfCX.lfEscapement  = 0;
   pData->lfCX.lfOrientation = 0;
   pData->lfCX.lfWeight      = HB_ISNUM( 5 ) ? hb_parni( 5 ) : pData->lfCX.lfWeight;
   pData->lfCX.lfItalic      = HB_ISLOG( 7 ) ? ( BYTE ) hb_parl( 7 ) : pData->lfCX.lfItalic;
   pData->lfCX.lfUnderline   = HB_ISLOG( 8 ) ? ( BYTE ) hb_parl( 8 ) : pData->lfCX.lfUnderline;
   pData->lfCX.lfStrikeOut   = HB_ISLOG( 9 ) ? ( BYTE ) hb_parl( 9 ) : pData->lfCX.lfStrikeOut;
   pData->lfCX.lfCharSet     = DEFAULT_CHARSET;

   pData->lfCX.lfQuality        = HB_ISNUM( 6 ) ? ( BYTE ) hb_parni( 6 ) : pData->lfCX.lfQuality;
   pData->lfCX.lfPitchAndFamily = FF_DONTCARE;
   if( HB_ISCHAR( 2 ) )
      hb_strncpy( pData->lfCX.lfFaceName, hb_parc( 2 ), sizeof( pData->lfPB.lfFaceName ) - 1 );

   if( pWindowData->hCXfont )
   {
      HFONT hOldFont = pWindowData->hCXfont;
      HFONT hFont    = CreateFontIndirect( &pData->lfCX );
      if( hFont )
      {
#if 0
         CONTROL_DATA * pcd = pWindowData->pcdCtrlList;

         while( pcd )
         {
            if( pcd->byCtrlClass == WVW_CONTROL_PUSHBUTTON &&
                ( HFONT ) SendMessage( pcd->hWndCtrl, WM_GETFONT, 0, 0 ) == hOldFont )
               SendMessage( pcd->hWndCtrl, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );

            pcd = pcd->pNext;
         }
#endif
         pWindowData->hCXfont = hFont;
         DeleteObject( hOldFont );
      }
      else
         retval = HB_FALSE;
   }

   hb_retl( retval );
}

HB_FUNC( WVW_CXSTATUSFONT )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   CONTROL_DATA * pcd = hb_gt_wvw_GetControlData( usWinNum, WVW_CONTROL_PUSHBUTTON, NULL, ( UINT ) hb_parnl( 2 ) );

   if( pcd->hWndCtrl )
   {
      if( hb_parldef( 3, HB_TRUE ) /* lFocus */ )
         SendMessage( pcd->hWndCtrl, WM_SETFONT, ( WPARAM ) pWindowData->hCXfont, ( LPARAM ) TRUE );
      else
         SendMessage( pcd->hWndCtrl, WM_SETFONT, ( WPARAM ) pWindowData->hPBfont, ( LPARAM ) TRUE );
   }

   hb_retl( HB_TRUE );
}
