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

/* wvw_pbCreate( [nWinNum], nTop, nLeft, nBottom, nRight, cText, cImage/nImage, bBlock, aOffset,;
 *               nStretchBitmap, lMap3Dcolors)
 * create pushbutton for window nWinNum
 * nTop: row of top/left corner (in character unit)
 * nLeft: col of top/left corner (in character unit)
 * nBottom: row of bottom/right corner (in character unit) defaults==nTop
 * nRight: col of bottom/right corner (in character unit) defaults==??
 * cText: caption, default == ""
 *
 *
 * cImage: bitmap file name, can be supplied as nImage: bitmap resource id
 *
 * nStretchBitmap: a number between 0 and 1 (inclusive) as a factor to
 *               stretch the bitmap.
 *               1.0: bitmap covers the whole button
 *               0.5: bitmap covers 50% of button
 *               0: bitmap is not stretch
 *              (default is 1)
 *
 * lMap3Dcolors: defaults to .f.
 *          if .t. the following color mapping will be performed:
 *             RGB(192,192,192) --> COLOR_3DFACE   ("transparent")
 *             RGB(128,128,128) --> COLOR_3DSHADOW
 *             RGB(223,223,223) --> COLOR_3DLIGHT
 *          This might be desirable to have transparent effect.
 *          LIMITATION: this will work on 256 colored bitmaps only
 *
 * aOffset: array {y1,x1,y2,x2} of offsets to corner pixels, to adjust
 *        dimension of pushbutton.
 *        defaults for pushbutton: {-2,-2,+2,+2}
 *
 * bBlock:  codeblock to execute on every BN_CLICK event.
 *        This codeblock will be evaluated with these parameters:
 *        nWinNum: window number
 *        nPBid  : pushbutton id
 *
 * returns control id of newly created pushbutton of windows nWinNum
 * returns 0 if failed
 *
 * example:
 */

HB_FUNC( WVW_PBCREATE )
{
   if( HB_ISEVALITEM( 8 ) )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );
      LPCTSTR      lpszCaption  = hb_parc( 6 );
      const char * szBitmap     = hb_parc( 7 );
      UINT         uiBitmap     = ( UINT ) hb_parni( 7 );
      double       dStretch     = HB_ISNUM( 10 ) ? hb_parnd( 10 ) : 1;
      HB_BOOL      bMap3Dcolors = hb_parl( 11 );

      int iOffTop    = HB_ISARRAY( 9 ) ? hb_parvni( 9, 1 ) : -2;
      int iOffLeft   = HB_ISARRAY( 9 ) ? hb_parvni( 9, 2 ) : -2;
      int iOffBottom = HB_ISARRAY( 9 ) ? hb_parvni( 9, 3 ) : 2;
      int iOffRight  = HB_ISARRAY( 9 ) ? hb_parvni( 9, 4 ) : 2;

      hb_retnl( hb_gt_wvw_ButtonCreate( WVW_WHICH_WINDOW, usTop, usLeft, usBottom, usRight, lpszCaption,
                                        szBitmap, uiBitmap, hb_param( 8, HB_IT_EVALITEM ),
                                        iOffTop, iOffLeft, iOffBottom, iOffRight,
                                        dStretch, bMap3Dcolors,
                                        BS_PUSHBUTTON ) );
   }
   else
      hb_retnl( 0 );

}

/* wvw_pbDestroy( [nWinNum], nPBid )
 * destroy button nPBid for window nWinNum
 */
HB_FUNC( WVW_PBDESTROY )
{
   WIN_DATA *     pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   UINT           uiPBid      = ( UINT ) hb_parnl( 2 );
   CONTROL_DATA * pcd         = pWindowData->pcdCtrlList;
   CONTROL_DATA * pcdPrev     = NULL;

   while( pcd )
   {
      if( pcd->byCtrlClass == WVW_CONTROL_PUSHBUTTON && pcd->uiCtrlid == uiPBid )
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

/* wvw_pbSetFocus( [nWinNum], nButtonId )
 * set the focus to button nButtonId in window nWinNum
 */
HB_FUNC( WVW_PBSETFOCUS )
{
   HWND hWndPB = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_PUSHBUTTON, ( UINT ) hb_parnl( 2 ), NULL );

   if( hWndPB )
      hb_retl( SetFocus( hWndPB ) != NULL );
   else
      hb_retl( HB_FALSE );
}

/* wvw_pbIsFocused( [nWinNum], nPBid )
 * returns .t. if the focus is on button nPBid in window nWinNum
 */
HB_FUNC( WVW_PBISFOCUSED )
{
   HWND hWndPB = hb_gt_wvw_FindControlHandle( WVW_WHICH_WINDOW, WVW_CONTROL_PUSHBUTTON, ( UINT ) hb_parnl( 2 ), NULL );

   hb_retl( GetFocus() == hWndPB );
}

/* wvw_pbEnable( [nWinNum], nButtonId, [lToggle] )
 *  enable/disable button nButtonId on window nWinNum
 * (lToggle defaults to .t., ie. enabling the button)
 *  return previous state of the button (TRUE:enabled FALSE:disabled)
 * (if nButtonId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_PBENABLE )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   HWND       hWndPB      = hb_gt_wvw_FindControlHandle( usWinNum, WVW_CONTROL_PUSHBUTTON, ( UINT ) hb_parnl( 2 ), NULL );

   if( hWndPB )
   {
      HB_BOOL bEnable = hb_parldef( 3, HB_TRUE );

      hb_retl( EnableWindow( hWndPB, ( BOOL ) bEnable ) == 0 );

      if( ! bEnable )
         SetFocus( pWindowData->hWnd );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_pbSetCodeblock( [nWinNum], nPBid, bBlock )
 * assign (new) codeblock bBlock to button nPBid for window nWinNum
 *
 * return .t. if successful
 */
HB_FUNC( WVW_PBSETCODEBLOCK )
{
   WVW_DATA *     pData        = hb_gt_wvw_GetWvwData();
   CONTROL_DATA * pcd          = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_PUSHBUTTON, NULL, ( UINT ) hb_parnl( 2 ) );
   PHB_ITEM       phiCodeBlock = hb_param( 3, HB_IT_EVALITEM );

   if( phiCodeBlock && pcd && ! pcd->bBusy )
   {
      HB_BOOL bOldSetting = pData->bRecurseCBlock;

      pData->bRecurseCBlock = HB_FALSE;
      pcd->bBusy = HB_TRUE;

      if( pcd->phiCodeBlock )
         hb_itemRelease( pcd->phiCodeBlock );

      pcd->phiCodeBlock = hb_itemNew( phiCodeBlock );

      pcd->bBusy = HB_FALSE;
      pData->bRecurseCBlock = bOldSetting;

      hb_retl( HB_TRUE );
   }
   else
   {
#if 0
      if( ! HB_ISEVALITEM( 3 ) )
         MessageBox( NULL, TEXT( "Codeblock expected" ), pData->szAppName, MB_ICONERROR );

      if( pcd == NULL )
         MessageBox( NULL, TEXT( "Control data not found" ), pData->szAppName, MB_ICONERROR );

      if( pcd->bBusy )
         MessageBox( NULL, TEXT( "Codeblock is busy" ), pData->szAppName, MB_ICONERROR );
#endif
      hb_retl( HB_FALSE );
   }
}

/* wvw_pbSetStyle( [nWinNum], nPBid, nStyle )
 * assign new style nStyle to button nPBid for window nWinNum
 * typical usage: nStyle==BS_DEFPUSHBUTTON (==01) to turn the button
 *                                               into default push button
 *                                               (thick border)
 *                       BS_PUSHBUTTON    (==00) to turn the button
 *                                               into regular push button
 *
 * using other styles like BS_MULTILINE may also be usefull,
 * but I haven't tried that
 *
 * this function always return .t.
 */
HB_FUNC( WVW_PBSETSTYLE )
{
   CONTROL_DATA * pcd = hb_gt_wvw_GetControlData( WVW_WHICH_WINDOW, WVW_CONTROL_PUSHBUTTON, NULL, ( UINT ) hb_parnl( 2 ) );

   if( pcd->hWndCtrl )
      SendMessage( pcd->hWndCtrl, BM_SETSTYLE, ( WPARAM ) ( ULONG ) hb_parni( 3 ), ( LPARAM ) TRUE );

   hb_retl( HB_TRUE );
}

/* wvw_pbSetFont([nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality,;
 *                             lItalic, lUnderline, lStrikeout
 *
 * this will initialize font for ALL pushbuttons in window nWinNum
 * (including ones created later on)
 */
HB_FUNC( WVW_PBSETFONT )
{
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( WVW_WHICH_WINDOW );
   WVW_DATA * pData       = hb_gt_wvw_GetWvwData();
   HB_BOOL    retval      = HB_TRUE;

   pData->lfPB.lfHeight      = HB_ISNUM( 3 ) ? hb_parnl( 3 ) : pWindowData->fontHeight - 2;
   pData->lfPB.lfWidth       = HB_ISNUM( 4 ) ? hb_parni( 4 ) : pData->lfPB.lfWidth;
   pData->lfPB.lfEscapement  = 0;
   pData->lfPB.lfOrientation = 0;
   pData->lfPB.lfWeight      = HB_ISNUM( 5 ) ? hb_parni( 5 )         : pData->lfPB.lfWeight;
   pData->lfPB.lfItalic      = HB_ISLOG( 7 ) ? ( BYTE ) hb_parl( 7 ) : pData->lfPB.lfItalic;
   pData->lfPB.lfUnderline   = HB_ISLOG( 8 ) ? ( BYTE ) hb_parl( 8 ) : pData->lfPB.lfUnderline;
   pData->lfPB.lfStrikeOut   = HB_ISLOG( 9 ) ? ( BYTE ) hb_parl( 9 ) : pData->lfPB.lfStrikeOut;
   pData->lfPB.lfCharSet     = DEFAULT_CHARSET;

   pData->lfPB.lfQuality        = HB_ISNUM( 6 ) ? ( BYTE ) hb_parni( 6 ) : pData->lfPB.lfQuality;
   pData->lfPB.lfPitchAndFamily = FF_DONTCARE;
   if( HB_ISCHAR( 2 ) )
      hb_strncpy( pData->lfPB.lfFaceName, hb_parc( 2 ), sizeof( pData->lfPB.lfFaceName ) - 1 );

   if( pWindowData->hPBfont )
   {
      HFONT hOldFont = pWindowData->hPBfont;
      HFONT hFont    = CreateFontIndirect( &pData->lfPB );
      if( hFont )
      {
         CONTROL_DATA * pcd = pWindowData->pcdCtrlList;

         while( pcd )
         {
            if( pcd->byCtrlClass == WVW_CONTROL_PUSHBUTTON &&
                ( HFONT ) SendMessage( pcd->hWndCtrl, WM_GETFONT, 0, 0 ) == hOldFont )
               SendMessage( pcd->hWndCtrl, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );

            pcd = pcd->pNext;
         }

         pWindowData->hPBfont = hFont;
         DeleteObject( hOldFont );
      }
      else
         retval = HB_FALSE;
   }

   hb_retl( retval );
}
