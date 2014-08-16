/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw pushbutton functions
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

/* wvw_pbCreate( [nWinNum], nTop, nLeft, nBottom, nRight, cText, cImage/nImage, bBlock, aOffset, ;
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
 * lMap3Dcolors: defaults to .F.
 *          if .T. the following color mapping will be performed:
 *             RGB( 192, 192, 192 ) --> COLOR_3DFACE   ("transparent")
 *             RGB( 128, 128, 128 ) --> COLOR_3DSHADOW
 *             RGB( 223, 223, 223 ) --> COLOR_3DLIGHT
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
   WVW_WIN * wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = NULL;

   if( wvw_win && HB_ISEVALITEM( 8 ) )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      void * hCaption;

      int iOffTop    = HB_ISARRAY( 9 ) ? hb_parvni( 9, 1 ) : -2;
      int iOffLeft   = HB_ISARRAY( 9 ) ? hb_parvni( 9, 2 ) : -2;
      int iOffBottom = HB_ISARRAY( 9 ) ? hb_parvni( 9, 3 ) : 2;
      int iOffRight  = HB_ISARRAY( 9 ) ? hb_parvni( 9, 4 ) : 2;

      hb_retnl( hb_gt_wvw_ButtonCreate( wvw_win, usTop, usLeft, usBottom, usRight,
                                        HB_PARSTR( 6, &hCaption, NULL ),
                                        hb_parc( 7 ),
                                        ( HB_UINT ) hb_parnl( 7 ),
                                        hb_param( 8, HB_IT_EVALITEM ),
                                        iOffTop, iOffLeft, iOffBottom, iOffRight,
                                        HB_ISNUM( 10 ) ? hb_parnd( 10 ) : 1 /* dStretch */,
                                        hb_parl( 11 ) /* bMap3Dcolors */,
                                        BS_PUSHBUTTON, &hWnd ) );

      hb_strfree( hCaption );
   }
   else
      hb_retnl( 0 );

   HB_STOREHANDLE( hWnd, 12 );
}

/* wvw_pbDestroy( [nWinNum], nPBid )
 * destroy button nPBid for window nWinNum
 */
HB_FUNC( WVW_PBDESTROY )
{
   WVW_WIN * wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int        nCtrlId = hb_parni( 2 );
      WVW_CTRL * pcd     = wvw_win->pcdList;
      WVW_CTRL * pcdPrev = NULL;

      while( pcd )
      {
         if( pcd->nClass == WVW_CONTROL_PUSHBUTTON && pcd->nId == nCtrlId )
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
}

/* wvw_pbSetFocus( [nWinNum], nButtonId )
 * set the focus to button nButtonId in window nWinNum
 */
HB_FUNC( WVW_PBSETFOCUS )
{
   WVW_WIN * wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_PUSHBUTTON, hb_parni( 2 ), NULL );

   hb_retl( hWnd && SetFocus( hWnd ) != NULL );
}

/* wvw_pbIsFocused( [nWinNum], nPBid )
 * returns .T. if the focus is on button nPBid in window nWinNum
 */
HB_FUNC( WVW_PBISFOCUSED )
{
   WVW_WIN * wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_PUSHBUTTON, hb_parni( 2 ), NULL );

   hb_retl( hWnd && GetFocus() == hWnd );
}

/* wvw_pbEnable( [nWinNum], nButtonId, [lToggle] )
 *  enable/disable button nButtonId on window nWinNum
 * (lToggle defaults to .T., ie. enabling the button)
 *  return previous state of the button (TRUE:enabled FALSE:disabled)
 * (if nButtonId is invalid, this function returns FALSE too)
 */
HB_FUNC( WVW_PBENABLE )
{
   WVW_WIN * wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_PUSHBUTTON, hb_parni( 2 ), NULL );

   if( hWnd )
   {
      HB_BOOL bEnable = hb_parldef( 3, HB_TRUE );

      hb_retl( EnableWindow( hWnd, ( BOOL ) bEnable ) == 0 );

      if( ! bEnable )
         SetFocus( wvw_win->hWnd );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_pbSetCodeblock( [nWinNum], nPBid, bBlock )
 * assign (new) codeblock bBlock to button nPBid for window nWinNum
 *
 * return .T. if successful
 */
HB_FUNC( WVW_PBSETCODEBLOCK )
{
   WVW_GLOB * wvw     = hb_gt_wvw();
   WVW_WIN *  wvw_win = hb_gt_wvw_win_par();
   WVW_CTRL * pcd     = hb_gt_wvw_GetControlData( wvw_win, WVW_CONTROL_PUSHBUTTON, NULL, hb_parni( 2 ) );
   PHB_ITEM   pBlock  = hb_param( 3, HB_IT_EVALITEM );

   if( pBlock && pcd && ! pcd->fBusy )
   {
      HB_BOOL fOldSetting = wvw->fRecurseCBlock;

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
   {
#if 0
      if( ! HB_ISEVALITEM( 3 ) )
         MessageBox( NULL, TEXT( "Codeblock expected" ), wvw->szAppName, MB_ICONERROR );

      if( pcd == NULL )
         MessageBox( NULL, TEXT( "Control data not found" ), wvw->szAppName, MB_ICONERROR );

      if( pcd->fBusy )
         MessageBox( NULL, TEXT( "Codeblock is busy" ), wvw->szAppName, MB_ICONERROR );
#endif
      hb_retl( HB_FALSE );
   }
}

/* wvw_pbSetStyle( [nWinNum], nPBid, nStyle )
 * assign new style nStyle to button nPBid for window nWinNum
 * typical usage: nStyle==BS_DEFPUSHBUTTON (==01) to turn the button
 *                                                into default push button
 *                                                (thick border)
 *                        BS_PUSHBUTTON    (==00) to turn the button
 *                                                into regular push button
 *
 * using other styles like BS_MULTILINE may also be useful,
 * but I haven't tried that
 *
 * this function always return .T.
 */
HB_FUNC( WVW_PBSETSTYLE )
{
   WVW_WIN *  wvw_win = hb_gt_wvw_win_par();
   WVW_CTRL * pcd     = hb_gt_wvw_GetControlData( wvw_win, WVW_CONTROL_PUSHBUTTON, NULL, hb_parni( 2 ) );

   if( pcd->hWnd )
      SendMessage( pcd->hWnd, BM_SETSTYLE, ( WPARAM ) hb_parni( 3 ), ( LPARAM ) TRUE );

   hb_retl( HB_TRUE );
}

/* wvw_pbSetFont( [nWinNum], cFontFace, nHeight, nWidth, nWeight, nQUality, ;
 *                             lItalic, lUnderline, lStrikeout )
 * this will initialize font for ALL pushbuttons in window nWinNum
 * (including ones created later on)
 */
HB_FUNC( WVW_PBSETFONT )
{
   WVW_GLOB * wvw     = hb_gt_wvw();
   WVW_WIN *  wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      HB_BOOL fResult = HB_TRUE;

      wvw->lfPB.lfHeight         = hb_parnldef( 3, wvw_win->fontHeight - 2 );
      wvw->lfPB.lfWidth          = hb_parnldef( 4, wvw->lfPB.lfWidth );
      wvw->lfPB.lfEscapement     = 0;
      wvw->lfPB.lfOrientation    = 0;
      wvw->lfPB.lfWeight         = hb_parnldef( 5, wvw->lfPB.lfWeight );
      wvw->lfPB.lfQuality        = ( BYTE ) hb_parnidef( 6, wvw->lfPB.lfQuality );
      wvw->lfPB.lfItalic         = ( BYTE ) hb_parldef( 7, wvw->lfPB.lfItalic );
      wvw->lfPB.lfUnderline      = ( BYTE ) hb_parldef( 8, wvw->lfPB.lfUnderline );
      wvw->lfPB.lfStrikeOut      = ( BYTE ) hb_parldef( 9, wvw->lfPB.lfStrikeOut );
      wvw->lfPB.lfCharSet        = DEFAULT_CHARSET;
      wvw->lfPB.lfPitchAndFamily = FF_DONTCARE;

      if( HB_ISCHAR( 2 ) )
      {
         HB_ITEMCOPYSTR( hb_param( 2, HB_IT_STRING ), wvw->lfPB.lfFaceName, HB_SIZEOFARRAY( wvw->lfPB.lfFaceName ) );
         wvw_win->fontFace[ HB_SIZEOFARRAY( wvw->lfPB.lfFaceName ) - 1 ] = TEXT( '\0' );
      }

      if( wvw_win->hPBfont )
      {
         HFONT hOldFont = wvw_win->hPBfont;
         HFONT hFont    = CreateFontIndirect( &wvw->lfPB );
         if( hFont )
         {
            WVW_CTRL * pcd = wvw_win->pcdList;

            while( pcd )
            {
               if( pcd->nClass == WVW_CONTROL_PUSHBUTTON &&
                   ( HFONT ) SendMessage( pcd->hWnd, WM_GETFONT, 0, 0 ) == hOldFont )
                  SendMessage( pcd->hWnd, WM_SETFONT, ( WPARAM ) hFont, ( LPARAM ) TRUE );

               pcd = pcd->pNext;
            }

            wvw_win->hPBfont = hFont;
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

HB_FUNC( WVW_PBVISIBLE )
{
   WVW_WIN * wvw_win = hb_gt_wvw_win_par();

   HWND hWnd = hb_gt_wvw_FindControlHandle( wvw_win, WVW_CONTROL_PUSHBUTTON, hb_parni( 2 ), NULL );

   hb_retl( hWnd && ShowWindow( hWnd, hb_parldef( 3, HB_TRUE ) ? SW_SHOW : SW_HIDE ) == 0 );
}
