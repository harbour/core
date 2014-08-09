/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * gtwvw draw functions
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

static const COLORREF s_COLORS[] = {
   BLACK,
   BLUE,
   GREEN,
   CYAN,
   RED,
   MAGENTA,
   BROWN,
   WHITE,
   LIGHT_GRAY,
   BRIGHT_BLUE,
   BRIGHT_GREEN,
   BRIGHT_CYAN,
   BRIGHT_RED,
   BRIGHT_MAGENTA,
   YELLOW,
   BRIGHT_WHITE
};

/* wvw_DrawLabelObj( [nWinNum], nTop, nLeft, nBottom, nRight, cLabel, nAlignHorz, nAlignVert, nTextColor, nBkColor, hFont,
                     aOffset ) */

HB_FUNC( WVW_DRAWLABELOBJ )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   POINT      xy = { 0 };
   int        iTop, iLeft, iBottom, iRight, x, y;
   int        iOffTop, iOffLeft, iOffBottom, iOffRight;
   RECT       rect = { 0 };
   HFONT      oldFont;
   int        oldTextAlign, iAlignHorz, iAlignVert, iAlignH = 0, iAlignV;
   COLORREF   oldBkColor, oldTextColor;
   UINT       uiOptions;
   SIZE       sz = { 0 };

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   iOffTop    = HB_ISARRAY( 12 ) ? hb_parvni( 12, 1 ) :  0;
   iOffLeft   = HB_ISARRAY( 12 ) ? hb_parvni( 12, 2 ) :  0;
   iOffBottom = HB_ISARRAY( 12 ) ? hb_parvni( 12, 3 ) :  0;
   iOffRight  = HB_ISARRAY( 12 ) ? hb_parvni( 12, 4 ) :  0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + 1 + iOffBottom;
   iRight  = xy.x - 1 + 1 + iOffRight;

   iAlignHorz = hb_parni( 7 );
   iAlignVert = hb_parni( 8 );

   oldTextColor = SetTextColor( pWindowData->hdc, HB_ISNUM( 9 ) ? ( COLORREF ) hb_parnl( 9 ) : pWindowData->foreground );
   oldBkColor   = SetBkColor( pWindowData->hdc, HB_ISNUM( 10 ) ? ( COLORREF ) hb_parnl( 10 ) : pWindowData->background );
   oldFont      = ( HFONT ) SelectObject( pWindowData->hdc, ( HFONT ) HB_PARHANDLE( 11 ) );

   GetTextExtentPoint32( pWindowData->hdc, hb_parcx( 6 ), strlen( hb_parcx( 6 ) ), &sz );

   x = iLeft;
   y = iTop;

   switch( iAlignHorz )
   {
      case 0:
         iAlignH = TA_LEFT;
         break;
      case 1:
         iAlignH = TA_RIGHT;
         x       = iRight;
         break;
      case 2:
         iAlignH = TA_CENTER;
         x       = iLeft + ( ( iRight - iLeft + 1 ) / 2 );
         break;
   }

   iAlignV = TA_TOP;

   switch( iAlignVert )
   {
      case 1:
         y = iBottom - sz.cy;
         break;
      case 2:
         y = iTop + ( ( iBottom - iTop + 1 - sz.cy ) / 2 );
         break;
   }

   oldTextAlign = SetTextAlign( pWindowData->hdc, iAlignH | iAlignV );

   rect.top    = iTop;
   rect.left   = iLeft;
   rect.bottom = iBottom;
   rect.right  = iRight;

   uiOptions = ETO_CLIPPED | ETO_OPAQUE;

   ExtTextOut( pWindowData->hdc, x, y, uiOptions, &rect, hb_parcx( 6 ), strlen( hb_parcx( 6 ) ), NULL );

   SelectObject( pWindowData->hdc, oldFont );
   SetTextAlign( pWindowData->hdc, oldTextAlign );
   SetBkColor( pWindowData->hdc, oldBkColor );
   SetTextColor( pWindowData->hdc, oldTextColor );

   hb_retl( HB_TRUE );
}


/* wvw_DrawToolButtonState( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOff, nState ) */
HB_FUNC( WVW_DRAWTOOLBUTTONSTATE )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   APP_DATA * s_sApp      = hb_gt_wvw_GetAppData();
   POINT      xy = { 0 };
   int        iTop, iLeft, iBottom, iRight;
   int        iState = hb_parni( 7 );

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + hb_parvni( 6, 1 );
   iLeft = xy.x + hb_parvni( 6, 2 );

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + hb_parvni( 6, 3 );
   iRight  = xy.x - 1 + hb_parvni( 6, 4 );

   switch( iState )
   {
      case 0:
         SelectObject( pWindowData->hdc, s_sApp->penGray );

         MoveToEx( pWindowData->hdc, iRight, iTop, NULL );           /* Right  */
         LineTo( pWindowData->hdc, iRight, iBottom + 1 );

         MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );         /* Bottom */
         LineTo( pWindowData->hdc, iRight, iBottom );

         MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Left   */
         LineTo( pWindowData->hdc, iLeft, iBottom );

         MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Top    */
         LineTo( pWindowData->hdc, iRight, iTop );
         break;

      case 1:
         SelectObject( pWindowData->hdc, s_sApp->penBlack );

         MoveToEx( pWindowData->hdc, iRight, iTop, NULL );           /* Right  */
         LineTo( pWindowData->hdc, iRight, iBottom + 1 );

         MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );         /* Bottom */
         LineTo( pWindowData->hdc, iRight, iBottom );

         SelectObject( pWindowData->hdc, s_sApp->penWhite );

         MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Left   */
         LineTo( pWindowData->hdc, iLeft, iBottom );

         MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Top    */
         LineTo( pWindowData->hdc, iRight, iTop );
         break;

      case 2:
         SelectObject( pWindowData->hdc, s_sApp->penWhite );

         MoveToEx( pWindowData->hdc, iRight, iTop, NULL );           /* Right  */
         LineTo( pWindowData->hdc, iRight, iBottom + 1 );

         MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );         /* Bottom */
         LineTo( pWindowData->hdc, iRight, iBottom );

         SelectObject( pWindowData->hdc, s_sApp->penBlack );

         MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Left   */
         LineTo( pWindowData->hdc, iLeft, iBottom );

         MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Top    */
         LineTo( pWindowData->hdc, iRight, iTop );
         break;

   }
   hb_retl( HB_TRUE );
}


/* wvw_DrawScrollButton( [nWinNum], nTop, nLeft, nBottom, nRight, aOffPixels, nTLBR, lDepressed ) */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be usefull */
HB_FUNC( WVW_DRAWSCROLLBUTTON )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   APP_DATA * s_sApp      = hb_gt_wvw_GetAppData();
   int        iTop, iLeft, iBottom, iRight;
   POINT *    Point;
   POINT      xy = { 0 };
   int        iHeight, iOff;
   BOOL       bDepressed = hb_parl( 8 );

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + hb_parvni( 6, 1 );
   iLeft = xy.x + hb_parvni( 6, 2 );

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + hb_parvni( 6, 3 );
   iRight  = xy.x - 1 + hb_parvni( 6, 4 );

   Point = ( POINT * ) hb_xgrab( 3 * sizeof( POINT ) );
   iOff  = 6;

   iHeight = iBottom - iTop + 1;

   if( bDepressed )
      hb_gt_wvw_DrawBoxRecessed( usWinNum, iTop + 1, iLeft + 1, iBottom - 2, iRight - 2, FALSE );
   else
      hb_gt_wvw_DrawBoxRaised( usWinNum, iTop + 1, iLeft + 1, iBottom - 2, iRight - 2, FALSE );
   SelectObject( pWindowData->hdc, s_sApp->solidBrush );

   switch( hb_parni( 7 ) )
   {
      case 1:
         xy.y       = iTop + iOff - 1;
         xy.x       = iLeft + ( ( iRight - iLeft + 1 ) / 2 );
         Point[ 0 ] = xy;
         xy.y       = iBottom - iOff - 1;
         xy.x       = iLeft + iOff - 1;
         Point[ 1 ] = xy;
         xy.x       = iRight - iOff + 1;
         Point[ 2 ] = xy;
         break;

      case 2:
         xy.y       = iTop + ( ( iBottom - iTop + 1 ) / 2 );
         xy.x       = iLeft + iOff;
         Point[ 0 ] = xy;
         xy.x       = iRight - iOff - 1;
         xy.y       = iTop + iOff - 1;
         Point[ 1 ] = xy;
         xy.y       = iBottom - iOff + 1;
         Point[ 2 ] = xy;
         break;

      case 3:
         xy.x       = iLeft + ( ( iRight - iLeft + 1 ) / 2 );
         xy.y       = iBottom - iOff;
         Point[ 0 ] = xy;
         xy.x       = iLeft + iOff - 1;
         xy.y       = iBottom - iHeight + iOff + 1;
         Point[ 1 ] = xy;
         xy.x       = iRight - iOff + 1;
         Point[ 2 ] = xy;
         break;

      case 4:
         xy.x       = iRight - iOff - 1;
         xy.y       = iTop + ( ( iBottom - iTop + 1 ) / 2 );
         Point[ 0 ] = xy;
         xy.x       = iLeft + iOff + 1;
         xy.y       = iTop + iOff - 1;
         Point[ 1 ] = xy;
         xy.y       = iBottom - iOff + 1;
         Point[ 2 ] = xy;
         break;
   }

   Polygon( pWindowData->hdc, Point, 3 );

   hb_xfree( Point );
}


/* Wvw_DrawScrollbarThumbVert( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlScroll, nThumbPos ) */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be usefull */
HB_FUNC( WVW_DRAWSCROLLTHUMBVERT )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   APP_DATA * s_sApp      = hb_gt_wvw_GetAppData();
   POINT      xy = { 0 };
   int        iTop, iLeft, iBottom, iRight;
   int        iTabTop, iTabLft, iTabBtm, iTabRgt;

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   USHORT usTabTop = ( USHORT ) hb_parni( 7 );

   if( hb_gt_wvw_GetMainCoordMode() )
   {
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTabTop, NULL, NULL, NULL );
   }

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + hb_parvni( 6, 1 );
   iLeft = xy.x + hb_parvni( 6, 2 );

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + hb_parvni( 6, 3 );
   iRight  = xy.x - 1 + hb_parvni( 6, 4 );

   SetBkColor( pWindowData->hdc, RGB( 230, 230, 230 ) );
   SelectObject( pWindowData->hdc, s_sApp->diagonalBrush );

   SelectObject( pWindowData->hdc, s_sApp->penNull );
   Rectangle( pWindowData->hdc, iLeft, iTop, iRight + 1, iBottom + 1 );

   xy      = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft /* dummy */, usTabTop );
   iTabTop = xy.y;

   iTabLft = iLeft;
   iTabBtm = iTabTop + pWindowData->PTEXTSIZE.y - 1;
   iTabRgt = iRight;

   SelectObject( pWindowData->hdc, s_sApp->wvwWhiteBrush );
   SelectObject( pWindowData->hdc, s_sApp->penGray );
   Rectangle( pWindowData->hdc, iTabLft, iTabTop, iTabRgt + 1, iTabBtm );

   hb_gt_wvw_DrawBoxRaised( usWinNum, iTabTop + 1, iTabLft + 1, iTabBtm - 2, iTabRgt - 2, FALSE );
}


/* Wvw_DrawScrollbarThumbHorz( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffset, nThumbPos ) */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be usefull */
HB_FUNC( WVW_DRAWSCROLLTHUMBHORZ )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   APP_DATA * s_sApp      = hb_gt_wvw_GetAppData();
   POINT      xy = { 0 };
   int        iThumbLeft, iThumbRight;
   int        iTop, iLeft, iBottom, iRight;

   USHORT usTop       = ( USHORT ) hb_parni( 2 ),
          usLeft      = ( USHORT ) hb_parni( 3 ),
          usBottom    = ( USHORT ) hb_parni( 4 ),
          usRight     = ( USHORT ) hb_parni( 5 );
   USHORT usThumbLeft = ( USHORT ) hb_parni( 7 );

   if( hb_gt_wvw_GetMainCoordMode() )
   {
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );
      hb_gt_wvw_HBFUNCPrologue( usWinNum, NULL, &usThumbLeft, NULL, NULL );
   }

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + hb_parvni( 6, 1 );
   iLeft = xy.x + hb_parvni( 6, 2 );

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + hb_parvni( 6, 3 );
   iRight  = xy.x - 1 + hb_parvni( 6, 4 );

   xy         = hb_gt_wvw_GetXYFromColRow( pWindowData, usThumbLeft, usTop /* dummy */ );
   iThumbLeft = xy.x;

   iThumbRight = iThumbLeft + ( pWindowData->PTEXTSIZE.x * 2 ) - 1;

   SetBkColor( pWindowData->hdc, RGB( 230, 230, 230 ) );
   SelectObject( pWindowData->hdc, s_sApp->diagonalBrush );
   SelectObject( pWindowData->hdc, s_sApp->penNull );
   Rectangle( pWindowData->hdc, iLeft, iTop, iRight + 1, iBottom + 1 );

   SelectObject( pWindowData->hdc, s_sApp->wvwWhiteBrush );
   SelectObject( pWindowData->hdc, s_sApp->penGray );
   Rectangle( pWindowData->hdc, iThumbLeft, iTop, iThumbRight, iBottom );
   hb_gt_wvw_DrawBoxRaised( usWinNum, iTop + 1, iThumbLeft + 1, iBottom - 2, iThumbRight - 2, FALSE );
}


/* wvw_DrawShadedRect( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffSet, nHorVert, aRGBb, aRGBe ) */
HB_FUNC( WVW_DRAWSHADEDRECT )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   APP_DATA * s_sApp      = hb_gt_wvw_GetAppData();
   HB_BOOL    bGF         = HB_FALSE;

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   if( s_sApp->hMSImg32 )
   {
      TRIVERTEX     vert[ 2 ];
      GRADIENT_RECT gRect = { 0 };

      int   iMode = hb_parnidef( 7, GRADIENT_FILL_RECT_H );
      POINT xy    = { 0 };
      int   iTop, iLeft, iBottom, iRight;

      xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
      iTop  = xy.y + hb_parvni( 6, 1 );
      iLeft = xy.x + hb_parvni( 6, 2 );

      xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

      xy.y -= pWindowData->byLineSpacing;

      iBottom = xy.y - 1 + hb_parvni( 6, 3 );
      iRight  = xy.x - 1 + hb_parvni( 6, 4 );

      vert[ 0 ].x     = iLeft;
      vert[ 0 ].y     = iTop;
      vert[ 0 ].Red   = ( COLOR16 ) hb_parvni( 8, 1 );
      vert[ 0 ].Green = ( COLOR16 ) hb_parvni( 8, 2 );
      vert[ 0 ].Blue  = ( COLOR16 ) hb_parvni( 8, 3 );
      vert[ 0 ].Alpha = ( COLOR16 ) hb_parvni( 8, 4 );

      vert[ 1 ].x     = iRight;
      vert[ 1 ].y     = iBottom;
      vert[ 1 ].Red   = ( COLOR16 ) hb_parvni( 9, 1 );
      vert[ 1 ].Green = ( COLOR16 ) hb_parvni( 9, 2 );
      vert[ 1 ].Blue  = ( COLOR16 ) hb_parvni( 9, 3 );
      vert[ 1 ].Alpha = ( COLOR16 ) hb_parvni( 9, 4 );

      gRect.UpperLeft  = 0;
      gRect.LowerRight = 1;

      bGF = ( HB_BOOL ) s_sApp->pfnGF( pWindowData->hdc, vert, 2, &gRect, 1, iMode );
   }

   hb_retl( bGF );
}


/* wvw_DrawTextBox( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffSet, cText, ;
                    nAlignHorz, nAlignVert, nTextColor, nBackColor, ;
                    nBackMode, hFont ) */
HB_FUNC( WVW_DRAWTEXTBOX )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   POINT      xy = { 0 };
   int        iTop, iLeft, iBottom, iRight;

   int iAlignHorz = hb_parni( 8 );

   int iAlignH = 0;

   COLORREF oldTextColor, oldBkColor;
   HFONT    oldFont;
   int      oldTextAlign, oldBkMode;
   RECT     rc = { 0 };

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + hb_parvni( 6, 1 );
   iLeft = xy.x + hb_parvni( 6, 2 );

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + hb_parvni( 6, 3 );
   iRight  = xy.x - 1 + hb_parvni( 6, 4 );

   switch( iAlignHorz )
   {
      case 0:
         iAlignH = DT_LEFT;
         break;
      case 1:
         iAlignH = DT_RIGHT;
         break;
      case 2:
         iAlignH = DT_CENTER;
         break;
   }

   rc.top    = iTop;
   rc.left   = iLeft;
   rc.bottom = iBottom;
   rc.right  = iRight;

   oldTextAlign = SetTextAlign( pWindowData->hdc, TA_TOP | TA_LEFT | TA_NOUPDATECP );
   oldTextColor = SetTextColor( pWindowData->hdc, HB_ISNUM( 10 ) ? ( COLORREF ) hb_parnl( 10 ) : pWindowData->foreground );
   oldBkColor   = SetBkColor( pWindowData->hdc, HB_ISNUM( 11 ) ? ( COLORREF ) hb_parnl( 11 ) : pWindowData->background );
   oldBkMode    = SetBkMode( pWindowData->hdc, hb_parnidef( 12, OPAQUE ) );
   oldFont      = ( HFONT ) SelectObject( pWindowData->hdc, ( HFONT ) HB_PARHANDLE( 13 ) );

   DrawText( pWindowData->hdc, hb_parcx( 7 ), strlen( hb_parcx( 7 ) ), &rc, iAlignH | DT_WORDBREAK | DT_TOP );

   SetTextColor( pWindowData->hdc, oldTextColor );
   SetBkColor( pWindowData->hdc, oldBkColor );
   SetBkMode( pWindowData->hdc, oldBkMode );
   SetTextAlign( pWindowData->hdc, oldTextAlign );
   SelectObject( pWindowData->hdc, oldFont );
}

/* wvw_DrawProgressBar( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlTLBR, nPercent,;
                        nBackColor, nBarColor, cImage, lVertical, nDirection ) */
HB_FUNC( WVW_DRAWPROGRESSBAR )
{
   UINT       usWinNum     = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData  = hb_gt_wvw_GetWindowsData( usWinNum );
   WIN_DATA * pWinMainData = hb_gt_wvw_GetWindowsData( 0 );
   APP_DATA * s_sApp       = hb_gt_wvw_GetAppData();
   USHORT     usTop        = ( USHORT ) hb_parni( 2 ),
              usLeft   = ( USHORT ) hb_parni( 3 ),
              usBottom = ( USHORT ) hb_parni( 4 ),
              usRight  = ( USHORT ) hb_parni( 5 );
   int  iTop;
   int  iLeft;
   int  iBottom;
   int  iRight;
   int  iPercent, iBarUpto, iDirection;
   BOOL bVertical, bImage;

   COLORREF crBarColor;
   HBRUSH   hBrush;
   LOGBRUSH lb = { 0 };
   RECT     rc = { 0 };
   POINT    xy = { 0 };

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + hb_parvni( 6, 1 );
   iLeft = xy.x + hb_parvni( 6, 2 );

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + hb_parvni( 6, 3 );
   iRight  = xy.x - 1 + hb_parvni( 6, 4 );

   iPercent   = hb_parni( 7 );
   bImage     = HB_ISCHAR( 10 );
   bVertical  = hb_parl( 11 );
   iDirection = hb_parni( 12 );

   if( bVertical )
   {
      if( iDirection == 0 )
      {
         iBarUpto  = iTop + ( ( iBottom - iTop ) * iPercent / 100 );
         rc.top    = iTop;
         rc.left   = iLeft;
         rc.bottom = iBarUpto;
         rc.right  = iRight;
      }
      else
      {
         iBarUpto  = iBottom - ( ( iBottom - iTop ) * iPercent / 100 );
         rc.top    = iBarUpto;
         rc.left   = iLeft;
         rc.bottom = iBottom;
         rc.right  = iRight;
      }
   }
   else
   {
      if( iDirection == 0 )
      {
         iBarUpto  = iLeft + ( ( iRight - iLeft ) * iPercent / 100 );
         rc.top    = iTop;
         rc.left   = iLeft;
         rc.bottom = iBottom;
         rc.right  = iBarUpto;
      }
      else
      {
         iBarUpto  = iRight - ( ( iRight - iLeft ) * iPercent / 100 );
         rc.top    = iTop;
         rc.left   = iBarUpto;
         rc.bottom = iBottom;
         rc.right  = iRight;
      }
   }

   if( bImage )
      hb_gt_wvw_DrawImage( usWinNum, rc.left, rc.top, rc.right - rc.left + 1, rc.bottom - rc.top + 1, hb_parc( 10 ),
                          FALSE );
   else
   {
      crBarColor = HB_ISNUM( 9 ) ? ( COLORREF ) hb_parnl( 9 ) : hb_gt_wvw_GetColorData(  0 );

      lb.lbStyle = BS_SOLID;
      lb.lbColor = crBarColor;
      lb.lbHatch = 0;

      hBrush = CreateBrushIndirect( &lb );

      rc.bottom++;
      rc.right++;
      FillRect( pWindowData->hdc, &rc, hBrush );

      SelectObject( pWinMainData->hdc, ( HBRUSH ) s_sApp->OriginalBrush );
      DeleteObject( hBrush );
   }
}


/* wvw_DrawBoxGet( [nWinNum], nRow, nCol, nWidth,;
 *                 aOffset )   <-- additional parm, not exist in GTWVT
 *
 * NOTES: unlike GTWVT, GTWVW draw white lines on outer right and outer bottom
 *       Besides, scope is the same as DRAWBOXRECESSED, ie.
 *       two pixel out of char boundary
 */
HB_FUNC( WVW_DRAWBOXGET )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   POINT      xy       = { 0 };
   POINT      yz       = { 0 };
   WIN_DATA * pWindowData;
   APP_DATA * s_sApp = hb_gt_wvw_GetAppData();
   int        iTop, iLeft, iBottom, iRight;
   int        iOffTop, iOffLeft, iOffBottom, iOffRight;
   USHORT     usRow, usCol, usLen;

   pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   usRow = ( USHORT ) hb_parni( 2 );
   usCol = ( USHORT ) hb_parni( 3 );
   usLen = ( USHORT ) hb_parni( 4 );

   iOffTop    = HB_ISARRAY( 5 ) ? hb_parvni( 5, 1 ) :  0;
   iOffLeft   = HB_ISARRAY( 5 ) ? hb_parvni( 5, 2 ) :  0;
   iOffBottom = HB_ISARRAY( 5 ) ? hb_parvni( 5, 3 ) :  0;
   iOffRight  = HB_ISARRAY( 5 ) ? hb_parvni( 5, 4 ) :  0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usRow, &usCol, NULL, NULL );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usCol, usRow );
   iTop  = xy.y - 1 + iOffTop;
   iLeft = xy.x - 1 + iOffLeft;

   yz = hb_gt_wvw_GetXYFromColRow( pWindowData, usCol + usLen, usRow + 1 );

   yz.y -= pWindowData->byLineSpacing;

   iBottom = yz.y + iOffBottom;
   iRight  = yz.x + iOffRight;

   SelectObject( pWindowData->hdc, s_sApp->penBlack );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );

   LineTo( pWindowData->hdc, iRight, iTop );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );

   LineTo( pWindowData->hdc, iLeft, iBottom );

   SelectObject( pWindowData->hdc, s_sApp->penDarkGray );

   MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );

   LineTo( pWindowData->hdc, iRight + 1, iTop - 1 );

   MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );

   LineTo( pWindowData->hdc, iLeft - 1, iBottom + 1 );

   /* GTWVW also draws right and bottom outer with single white line */
   SelectObject( pWindowData->hdc, s_sApp->penWhite );

   MoveToEx( pWindowData->hdc, iRight + 1, iTop - 1, NULL );
   LineTo( pWindowData->hdc, iRight + 1, iBottom + 1 + 1 );

   MoveToEx( pWindowData->hdc, iLeft - 1, iBottom + 1, NULL );
   LineTo( pWindowData->hdc, iRight + 1, iBottom + 1 );

   MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iRight, iTop, NULL );
   LineTo( pWindowData->hdc, iRight, iBottom + 1 );

   hb_retl( HB_TRUE );
}

/* wvw_DrawBoxGet_XP( [nWinNum], nRow, nCol, nWidth,;
 *                    aOffset )   <-- additional parm, not exist in GTWVT
 *
 * NOTES: unlike GTWVT, GTWVW draw white lines on outer right and outer bottom
 *       Besides, scope is the same as DRAWBOXRECESSED, ie.
 *       two pixel out of char boundary
 */
HB_FUNC( WVW_DRAWBOXGET_XP )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   POINT      xy       = { 0 };
   POINT      yz       = { 0 };
   WIN_DATA * pWindowData;
   APP_DATA * s_sApp = hb_gt_wvw_GetAppData();
   int        iTop, iLeft, iBottom, iRight;
   int        iOffTop, iOffLeft, iOffBottom, iOffRight;
   USHORT     usRow, usCol, usLen;

   pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   usRow = ( USHORT ) hb_parni( 2 );
   usCol = ( USHORT ) hb_parni( 3 );
   usLen = ( USHORT ) hb_parni( 4 );

   iOffTop    = HB_ISARRAY( 5 ) ? hb_parvni( 5, 1 ) :  0;
   iOffLeft   = HB_ISARRAY( 5 ) ? hb_parvni( 5, 2 ) :  0;
   iOffBottom = HB_ISARRAY( 5 ) ? hb_parvni( 5, 3 ) :  0;
   iOffRight  = HB_ISARRAY( 5 ) ? hb_parvni( 5, 4 ) :  0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usRow, &usCol, NULL, NULL );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usCol, usRow );
   iTop  = xy.y - 1 + iOffTop;
   iLeft = xy.x - 1 + iOffLeft;

   yz = hb_gt_wvw_GetXYFromColRow( pWindowData, usCol + usLen, usRow + 1 );

   yz.y -= pWindowData->byLineSpacing;

   iBottom = yz.y + iOffBottom;
   iRight  = yz.x + iOffRight;

   SelectObject( pWindowData->hdc, s_sApp->penGray );

   MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );
   LineTo( pWindowData->hdc, iRight + 1, iTop - 1 );

   MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );
   LineTo( pWindowData->hdc, iLeft - 1, iBottom + 1 );

   MoveToEx( pWindowData->hdc, iLeft - 1, iBottom + 1, NULL );
   LineTo( pWindowData->hdc, iRight + 1, iBottom + 1 );

   MoveToEx( pWindowData->hdc, iRight + 1, iTop - 1, NULL );
   LineTo( pWindowData->hdc, iRight + 1, iBottom + 1 );

   hb_retl( HB_TRUE );
}


/* wvw_DrawBoxRaised( nWinNum, nTop, nLeft, nBottom, nRight, lTight/aOffset) */

/* if lTight, box is drawn inside the character region
   AND top and left lines are lower two pixel down to make room for above/left object
   WARNING: gui object of this type subject to be overwritten by chars
   NOTE that these lines are to be overwritten by displayed char,
        we are depending on the fact that gui object will be painted last

   lTight may be replaced with aOffset parm {top,left,bottom,right}
     ie. offset in pixel unit */
HB_FUNC( WVW_DRAWBOXRAISED )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   POINT      xy       = { 0 };
   int        iTop, iLeft, iBottom, iRight;
   WIN_DATA * pWindowData;
   USHORT     usTop    = ( USHORT ) hb_parni( 2 ),
              usLeft   = ( USHORT ) hb_parni( 3 ),
              usBottom = ( USHORT ) hb_parni( 4 ),
              usRight  = ( USHORT ) hb_parni( 5 );
   BOOL bUseArray      = HB_ISARRAY( 6 );

   BOOL bTight = hb_parl( 6 );
   int  iOLeft, iOTop, iORight, iOBottom;

   pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   if( bTight )
   {

      iOTop    = 2;
      iOLeft   = 2;
      iOBottom = -1;
      iORight  = -1;
   }
   else if( bUseArray )
   {

      iOTop    = hb_parvni( 6, 1 ) - 1;
      iOLeft   = hb_parvni( 6, 2 ) - 1;
      iOBottom = hb_parvni( 6, 3 );
      iORight  = hb_parvni( 6, 4 );
   }
   else
   {

      iOTop    = -1;
      iOLeft   = -1;
      iOBottom = 0;
      iORight  = 0;
   }

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + iOTop;
   iLeft = xy.x + iOLeft;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y + iOBottom;
   iRight  = xy.x + iORight;

   hb_gt_wvw_DrawBoxRaised( pWindowData->byWinId, iTop, iLeft, iBottom, iRight,
                           bTight );

   hb_retl( HB_TRUE );
}


/* wvw_DrawBoxRecessed( nWinNum, nTop, nLeft, nBottom, nRight, lTight/aOffset) <--none in gtwvt */

/* if lTight, box is drawn inside the character region
   AND top and left lines are lower two pixel down to make room for above/left object
   WARNING: gui object of this type subject to be overwritten by chars
   NOTE that these lines are to be overwritten by displayed char,
        we are depending on the fact that gui object will be painted last

   lTight may be replaced with aOffset parm {top,left,bottom,right}
     ie. offset in pixel unit */
HB_FUNC( WVW_DRAWBOXRECESSED )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   POINT      xy       = { 0 };
   int        iTop, iLeft, iBottom, iRight;
   WIN_DATA * pWindowData;
   USHORT     usTop    = ( USHORT ) hb_parni( 2 ),
              usLeft   = ( USHORT ) hb_parni( 3 ),
              usBottom = ( USHORT ) hb_parni( 4 ),
              usRight  = ( USHORT ) hb_parni( 5 );
   BOOL bUseArray      = HB_ISARRAY( 6 );

   BOOL bTight = hb_parl( 6 );
   int  iOLeft, iOTop, iORight, iOBottom;

   pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   if( bTight )
   {

      iOTop    = 2;
      iOLeft   = 2;
      iOBottom = -1;
      iORight  = -1;
   }
   else if( bUseArray )
   {
      iOTop    = hb_parvni( 6, 1 ) - 1;
      iOLeft   = hb_parvni( 6, 2 ) - 1;
      iOBottom = hb_parvni( 6, 3 );
      iORight  = hb_parvni( 6, 4 );
   }
   else
   {
      iOTop    = -1;
      iOLeft   = -1;
      iOBottom = 0;
      iORight  = 0;
   }

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + iOTop;
   iLeft = xy.x + iOLeft;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y + iOBottom;
   iRight  = xy.x + iORight;

   hb_gt_wvw_DrawBoxRecessed( pWindowData->byWinId, iTop, iLeft, iBottom, iRight,
                             bTight );

   hb_retl( HB_TRUE );
}

/* wvw_DrawBoxGroup( nWinNum, nTop, nLeft, nBottom, nRight, [aOffset] )
 *
 * NOTE: aOffset is TLBR offset in pixel. none in GTWVT
 */
HB_FUNC( WVW_DRAWBOXGROUP )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   POINT      xy       = { 0 };
   int        iTop, iLeft, iBottom, iRight;
   int        iOffTop, iOffLeft, iOffBottom, iOffRight;
   WIN_DATA * pWindowData;
   APP_DATA * s_sApp   = hb_gt_wvw_GetAppData();
   USHORT     usTop    = ( USHORT ) hb_parni( 2 ),
              usLeft   = ( USHORT ) hb_parni( 3 ),
              usBottom = ( USHORT ) hb_parni( 4 ),
              usRight  = ( USHORT ) hb_parni( 5 );

   pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   iOffTop    = HB_ISARRAY( 6 ) ? hb_parvni( 6, 1 ) : 0;
   iOffLeft   = HB_ISARRAY( 6 ) ? hb_parvni( 6, 2 ) : 0;
   iOffBottom = HB_ISARRAY( 6 ) ? hb_parvni( 6, 3 ) : 0;
   iOffRight  = HB_ISARRAY( 6 ) ? hb_parvni( 6, 4 ) : 0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y - 1 + iOffTop;
   iLeft = xy.x - 1 + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y + iOffBottom;
   iRight  = xy.x + iOffRight;

   SelectObject( pWindowData->hdc, s_sApp->penDarkGray );

   MoveToEx( pWindowData->hdc, iRight, iTop, NULL );          /* Right Inner   */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );        /* Bottom Inner  */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );   /* Left Outer    */
   LineTo( pWindowData->hdc, iLeft - 1, iBottom + 1 );

   MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );   /* Top Outer     */
   LineTo( pWindowData->hdc, iRight + 1, iTop - 1 );

   SelectObject( pWindowData->hdc, s_sApp->penWhite );

   MoveToEx( pWindowData->hdc, iRight + 1, iTop, NULL );       /* Right Outer   */
   LineTo( pWindowData->hdc, iRight + 1, iBottom + 1 );

   MoveToEx( pWindowData->hdc, iLeft - 1, iBottom + 1, NULL );  /* Bottom Outer  */
   LineTo( pWindowData->hdc, iRight + 1 + 1, iBottom + 1 );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Left Inner    */
   LineTo( pWindowData->hdc, iLeft, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Top Inner     */
   LineTo( pWindowData->hdc, iRight, iTop );

   hb_retl( HB_TRUE );
}


/* wvw_DrawBoxRaised( nWinNum, nTop, nLeft, nBottom, nRight ) */
HB_FUNC( WVW_DRAWBOXGROUPRAISED )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   POINT      xy       = { 0 };
   int        iTop, iLeft, iBottom, iRight;
   WIN_DATA * pWindowData;
   APP_DATA * s_sApp   = hb_gt_wvw_GetAppData();
   USHORT     usTop    = ( USHORT ) hb_parni( 2 ),
              usLeft   = ( USHORT ) hb_parni( 3 ),
              usBottom = ( USHORT ) hb_parni( 4 ),
              usRight  = ( USHORT ) hb_parni( 5 );

   pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y - 1;
   iLeft = xy.x - 1;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y;
   iRight  = xy.x;

   SelectObject( pWindowData->hdc, s_sApp->penWhite );

   MoveToEx( pWindowData->hdc, iRight, iTop, NULL );           /* Right Inner   */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );         /* Bottom Inner  */
   LineTo( pWindowData->hdc, iRight, iBottom );

   MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );    /* Left Outer    */
   LineTo( pWindowData->hdc, iLeft - 1, iBottom + 1 );

   MoveToEx( pWindowData->hdc, iLeft - 1, iTop - 1, NULL );    /* Top Outer     */
   LineTo( pWindowData->hdc, iRight + 1, iTop - 1 );

   SelectObject( pWindowData->hdc, s_sApp->penDarkGray );

   MoveToEx( pWindowData->hdc, iRight + 1, iTop, NULL );       /* Right Outer   */
   LineTo( pWindowData->hdc, iRight + 1, iBottom + 1 );

   MoveToEx( pWindowData->hdc, iLeft - 1, iBottom + 1, NULL );  /* Bottom Outer  */
   LineTo( pWindowData->hdc, iRight + 1 + 1, iBottom + 1 );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Left Inner    */
   LineTo( pWindowData->hdc, iLeft, iBottom );

   MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );            /* Top Inner     */
   LineTo( pWindowData->hdc, iRight, iTop );

   hb_retl( HB_TRUE );
}


/* wvw_DrawImage( nWinNum, ;
                  nTop, nLeft, nBottom, nRight, cImage/nPictureSlot, ;
                  lTight/aOffset,;
                  lTransparent) <--none in gtwvt */

/* 2006-07-24 Notes:
   (1) Image dimension
   if nBottom is NIL, then image height will be proportional to image width.
   if nRight  is NIL, then image width will be proportional to image height.
   if nBottom and nRight are BOTH NIL then original image dimension is used

   (2) Transparency
   if lTransparent is .t., top-left pixel is used as the transparent color,

   (3) Caching
   Image will always be cached. See the WARNING in hb_gt_wvw_DrawImage().
   Do not use this function to draw a large number of images.
   TODO: make it an option.
 */

HB_FUNC( WVW_DRAWIMAGE )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   APP_DATA * s_sApp   = hb_gt_wvw_GetAppData();
   POINT      xy       = { 0 };
   int        iLeft, iTop, iRight = 0, iBottom = 0;
   WIN_DATA * pWindowData;

   BOOL bActBottom = ! HB_ISNUM( 4 ),
        bActRight  = ! HB_ISNUM( 5 );
   int iImgWidth   = 0, iImgHeight = 0;

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   BOOL bTight       = hb_parl( 7 );
   BOOL bUseArray    = HB_ISARRAY( 7 );
   BOOL bTransparent = hb_parl( 8 );
   int  iOLeft, iOTop, iORight, iOBottom;
   HB_BOOL bResult;

   pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   if( bTight )
   {
      iOTop    = 2 + 1;
      iOLeft   = 2 + 1;
      iOBottom = -1;
      iORight  = -1;
   }
   else if( bUseArray )
   {
      iOTop    = hb_parvni( 7, 1 );
      iOLeft   = hb_parvni( 7, 2 );
      iOBottom = hb_parvni( 7, 3 );
      iORight  = hb_parvni( 7, 4 );
   }
   else
      iOTop = iOLeft = iOBottom = iORight = 0;

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + iOTop;
   iLeft = xy.x + iOLeft;

   if( bActRight || bActBottom )
   {
      if( ( ! HB_ISNUM( 6 ) && ! hb_gt_wvw_GetImageDimension( hb_parcx( 6 ), &iImgWidth, &iImgHeight ) ) ||
          ( HB_ISNUM( 6 ) && ! hb_gt_wvw_GetIPictDimension( s_sApp->iPicture[ hb_parni( 6 ) - 1 ], &iImgWidth, &iImgHeight ) )
          )
      {
         bActRight  = FALSE;
         bActBottom = FALSE;
      }
      else if( bActRight && bActBottom )
      {
         iRight  = iLeft + iImgWidth - 1;
         iBottom = iTop + iImgHeight - 1;
      }
   }

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   if( ! bActBottom )
      iBottom = xy.y - 1 + iOBottom;

   if( ! bActRight )
      iRight = xy.x - 1 + iORight;

   if( ( bActBottom || bActRight ) && ! ( bActBottom && bActRight ) )
   {
      int iDispWidth, iDispHeight;
      if( bActRight )
      {
         /* right corner (width) must be proportional to height */

         iDispHeight = iBottom - iTop + 1;
         iDispWidth  = ( int ) ( ( float ) iImgWidth / iImgHeight * iDispHeight );
         iRight      = iLeft + iDispWidth - 1;
      }
      else
      {
         /* bottom corner (height) must be proportional to width */

         iDispWidth  = iRight - iLeft + 1;
         iDispHeight = ( int ) ( ( float ) iImgHeight / iImgWidth * iDispWidth );
         iBottom     = iTop + iDispHeight - 1;
      }
   }

   if( HB_ISNUM( 6 ) )
      bResult = hb_gt_wvw_RenderPicture( usWinNum, iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, s_sApp->iPicture[ hb_parni( 6 ) - 1 ],
                                        bTransparent );
   else
      bResult = hb_gt_wvw_DrawImage( usWinNum, iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, hb_parcx( 6 ),
                                    bTransparent );

   hb_retl( bResult );
}

/* wvw_DrawImage_Resource( nWinNum, ;
                  nTop, nLeft, nBottom, nRight, nPictureResource/cPictureResource, ;
                  lTight/aOffset,;
                  lTransparent) <--none in gtwvt */

/* 2006-07-24 Notes:
   (1) Image dimension
   if nBottom is NIL, then image height will be proportional to image width.
   if nRight  is NIL, then image width will be proportional to image height.
   if nBottom and nRight are BOTH NIL then original image dimension is used

   (2) Transparency
   if lTransparent is .t., top-left pixel is used as the transparent color,

   (3) Caching
   Image will always be cached. See the WARNING in hb_gt_wvw_DrawImage().
   Do not use this function to draw a large number of images.
   TODO: make it an option.
 */

HB_FUNC( WVW_DRAWIMAGE_RESOURCE )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   POINT      xy       = { 0 };
   int        iLeft, iTop, iRight = 0, iBottom = 0;
   WIN_DATA * pWindowData;

   BOOL bActBottom = ! HB_ISNUM( 4 ),
        bActRight  = ! HB_ISNUM( 5 );
   int  iImgWidth, iImgHeight;
   LONG lImgWidth, lImgHeight;

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   BOOL       bTight       = hb_parl( 7 );
   BOOL       bUseArray    = HB_ISARRAY( 7 );
   BOOL       bTransparent = hb_parl( 8 );
   int        iOLeft, iOTop, iORight, iOBottom;
   IPicture * pPic;

   pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   if( bTight )
   {
      iOTop    = 2 + 1;
      iOLeft   = 2 + 1;
      iOBottom = -1;
      iORight  = -1;
   }
   else if( bUseArray )
   {
      iOTop    = hb_parvni( 7, 1 );
      iOLeft   = hb_parvni( 7, 2 );
      iOBottom = hb_parvni( 7, 3 );
      iORight  = hb_parvni( 7, 4 );
   }
   else
      iOTop = iOLeft = iOBottom = iORight = 0;

   xy         = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop       = xy.y + iOTop;
   iLeft      = xy.x + iOLeft;
   lImgWidth  = 0;
   lImgHeight = 0;
   iImgWidth  = 0;
   iImgHeight = 0;

   if( ! HB_ISNUM( 6 ) )
   {
      pPic = hb_gt_wvw_hb_gt_wvw_rr_LoadPictureFromResource( hb_parcx( 6 ), 0, &lImgWidth, &lImgHeight );

      if( pPic == NULL )
         pPic = hb_gt_wvw_rr_LoadPicture( hb_parcx( 6 ), &lImgWidth, &lImgHeight );
   }
   else
      pPic = hb_gt_wvw_hb_gt_wvw_rr_LoadPictureFromResource( NULL, hb_parni( 6 ), &lImgWidth, &lImgHeight );

#if 0
   lImgWidth  = iImgWidth;
   lImgHeight = iImgHeight;
#endif

   if( pPic == NULL )
   {
      hb_retl( HB_FALSE );
      return;
   }

   if( bActRight || bActBottom )
   {
      if( ! hb_gt_wvw_GetIPictDimension( pPic, &iImgWidth, &iImgHeight ) )
      {
         bActRight  = FALSE;
         bActBottom = FALSE;
      }
      else if( bActRight && bActBottom )
      {
         iRight  = iLeft + iImgWidth;
         iBottom = iTop + iImgHeight;
      }
   }

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   if( ! bActBottom )
      iBottom = xy.y - 1 + iOBottom;

   if( ! bActRight )
      iRight = xy.x - 1 + iORight;

   if( ( bActBottom || bActRight ) && ! ( bActBottom && bActRight ) )
   {
      int iDispWidth, iDispHeight;
      if( bActRight )
      {
         /* right corner (width) must be proportional to height */

         iDispHeight = iBottom - iTop + 1;
         iDispWidth  = ( int ) ( ( float ) iImgWidth / iImgHeight * iDispHeight );
         iRight      = iLeft + iDispWidth - 1;
      }
      else
      {
         /* bottom corner (height) must be proportional to width */

         iDispWidth  = iRight - iLeft + 1;
         iDispHeight = ( int ) ( ( float ) iImgHeight / iImgWidth * iDispWidth );
         iBottom     = iTop + iDispHeight - 1;
      }
   }

   hb_retl( hb_gt_wvw_RenderPicture( usWinNum, iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, pPic, bTransparent ) );
}


/* wvw_DrawLabel( nWinNum, ;
                  nRow, nCol, cLabel, nAlign, nEscapement, nTextColor, ;
                  nBkColor, cFontFace,nHeight, nWidth, nWeight, ;
                  nQuality, nCharSet, lItalic, lUnderline, lStrikeOut ) */
HB_FUNC( WVW_DRAWLABEL )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   POINT      xy       = { 0 };
   HFONT      hFont, oldFont;
   LOGFONT    logfont = { 0 };
   int        oldTextAlign;
   COLORREF   oldBkColor, oldTextColor;
   WIN_DATA * pWindowData;
   USHORT     usRow = ( USHORT ) hb_parni( 2 ),
              usCol = ( USHORT ) hb_parni( 3 );

   pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usRow, &usCol, NULL, NULL );

   logfont.lfEscapement     = hb_parni( 6 ) * 10;
   logfont.lfOrientation    = 0;
   logfont.lfWeight         = hb_parni( 12 );
   logfont.lfItalic         = ( BYTE ) hb_parl( 15 );
   logfont.lfUnderline      = ( BYTE ) hb_parl( 16 );
   logfont.lfStrikeOut      = ( BYTE ) hb_parl( 17 );
   logfont.lfCharSet        = HB_ISNUM( 14 ) ? ( BYTE ) hb_parni( 14 ) : ( BYTE ) pWindowData->CodePage;
   logfont.lfOutPrecision   = 0;
   logfont.lfClipPrecision  = 0;
   logfont.lfQuality        = ( BYTE ) hb_parnidef( 13, DEFAULT_QUALITY );
   logfont.lfPitchAndFamily = FF_DONTCARE;
   logfont.lfHeight         = HB_ISNUM( 10 ) ? hb_parni( 10 ) : pWindowData->fontHeight;
   logfont.lfWidth = HB_ISNUM( 11 ) ? hb_parni( 11 ) : ( pWindowData->fontWidth < 0 ? -pWindowData->fontWidth : pWindowData->fontWidth );

   hb_strncpy( logfont.lfFaceName, HB_ISCHAR( 9 ) ? hb_parc( 9 ) : pWindowData->fontFace, sizeof( logfont.lfFaceName ) - 1 );

   hFont = CreateFontIndirect( &logfont );
   if( hFont )
   {
      xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usCol, usRow );

      oldBkColor   = SetBkColor( pWindowData->hdc, HB_ISNUM( 8 ) ? ( COLORREF ) hb_parnl( 8 ) : pWindowData->background );
      oldTextColor = SetTextColor( pWindowData->hdc, HB_ISNUM( 7 ) ? ( COLORREF ) hb_parnl( 7 ) : pWindowData->foreground );
      oldTextAlign = SetTextAlign( pWindowData->hdc, hb_parnidef( 5, TA_LEFT ) );
      oldFont      = ( HFONT ) SelectObject( pWindowData->hdc, hFont );

      ExtTextOut( pWindowData->hdc, xy.x, xy.y, 0, NULL, hb_parcx( 4 ), strlen( hb_parcx( 4 ) ), NULL );

      SelectObject( pWindowData->hdc, oldFont );
      DeleteObject( hFont );
      SetTextAlign( pWindowData->hdc, oldTextAlign );
      SetBkColor( pWindowData->hdc, oldBkColor );
      SetTextColor( pWindowData->hdc, oldTextColor );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}


/* wvw_DrawOutline( nWinNum, nTop, nLeft, nBottom, nRight, nThick, nShape, nRGBColor ) */
HB_FUNC( WVW_DRAWOUTLINE )
{
   UINT  usWinNum = WVW_WHICH_WINDOW;
   HPEN  hPen     = 0, hOldPen = 0;
   POINT xy       = { 0 };
   int   iTop, iLeft, iBottom, iRight;

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   APP_DATA * s_sApp = hb_gt_wvw_GetAppData();

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y - 1;
   iLeft = xy.x - 1;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y;
   iRight  = xy.x;

   if( HB_ISNUM( 6 ) )
   {
      hPen = CreatePen( hb_parni( 6 ), 0, ( COLORREF ) hb_parnl( 8 ) );
      if( hPen )
         hOldPen = ( HPEN ) SelectObject( pWindowData->hdc, hPen );
   }
   else
      /* hPen = 0; */
      SelectObject( pWindowData->hdc, s_sApp->penBlack );

   hb_gt_wvw_DrawOutline( usWinNum, iTop, iLeft, iBottom, iRight );

   if( hPen )
   {
      SelectObject( pWindowData->hdc, hOldPen );
      DeleteObject( hPen );
   }

   hb_retl( HB_TRUE );
}


/*                1       2      3       4       5        6        7       8       9      10      11      12
   wvw_DrawLine( nWinNum, nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nStyle, nThick, nColor, aOffset) */

HB_FUNC( WVW_DRAWLINE )
{
   UINT     usWinNum = WVW_WHICH_WINDOW;
   POINT    xy       = { 0 };
   int      iTop, iLeft, iBottom, iRight, iOffset;
   int      iOffTop, iOffLeft, iOffBottom, iOffRight;
   int      iOrient, iFormat, iAlign, iStyle, iThick;
   int      x, y;
   COLORREF cr;
   HPEN     hPen, hOldPen;
   USHORT   usTop    = ( USHORT ) hb_parni( 2 ),
            usLeft   = ( USHORT ) hb_parni( 3 ),
            usBottom = ( USHORT ) hb_parni( 4 ),
            usRight  = ( USHORT ) hb_parni( 5 );

   WIN_DATA * pWindowData;
   APP_DATA * s_sApp = hb_gt_wvw_GetAppData();

   pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

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

   iOrient = hb_parni( 6 );
   iFormat = hb_parni( 7 );
   iAlign  = hb_parni( 8 );
   iStyle  = hb_parni( 9 );
   iThick  = hb_parni( 10 );
   cr      = ( COLORREF ) hb_parnl( 11 );

   x = iLeft;
   y = iTop;

   switch( iAlign )
   {
      case 0:                    /* Center      */
         if( iOrient == 0 )      /* Horizontal  */
         {
            iOffset = ( ( iBottom - iTop ) / 2 );
            y       = iTop + iOffset;
         }
         else
         {
            iOffset = ( ( iRight - iLeft ) / 2 );
            x       = iLeft + iOffset;
         }
         break;

      case 1:                  /* Top      */
         break;

      case 2:                  /* bottom   */
         if( iFormat == 0 || iFormat == 1 )
            y = iBottom - 1;
         else
            y = iBottom;
         break;

      case 3:                  /* Left     */
         break;

      case 4:                  /* Right    */
         if( iFormat == 0 || iFormat == 1 )
            x = iRight - 1;
         else
            x = iRight;
         break;
   }

   hPen    = CreatePen( iStyle, iThick, cr );
   hOldPen = ( HPEN ) SelectObject( pWindowData->hdc, hPen );

   switch( iFormat )
   {
      case 0:                                         /* Raised        */
         if( iOrient == 0 )                           /*  Horizontal   */
         {
            SelectObject( pWindowData->hdc, s_sApp->penWhite );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, iRight, y );
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y + 1, NULL );
            LineTo( pWindowData->hdc, iRight, y + 1 );
         }
         else                                       /*  Vertical     */
         {
            SelectObject( pWindowData->hdc, s_sApp->penWhite );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, x, iBottom );
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x + 1, y, NULL );
            LineTo( pWindowData->hdc, x + 1, iBottom );
         }
         break;

      case 1:                                      /* Recessed       */
         if( iOrient == 0 )                        /* Horizontal     */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, iRight, y );
            SelectObject( pWindowData->hdc, s_sApp->penWhite );
            MoveToEx( pWindowData->hdc, x, y + 1, NULL );
            LineTo( pWindowData->hdc, iRight, y + 1 );
         }
         else                                      /*  Vertical      */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, x, iBottom );
            SelectObject( pWindowData->hdc, s_sApp->penWhite );
            MoveToEx( pWindowData->hdc, x + 1, y, NULL );
            LineTo( pWindowData->hdc, x + 1, iBottom );
         }
         break;

      case 2:                                      /* Plain          */
         if( iOrient == 0 )                        /* Horizontal     */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, iRight, y );
         }
         else                                      /*  Vertical      */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, x, iBottom );
         }
         break;
   }

   SelectObject( pWindowData->hdc, hOldPen );
   DeleteObject( hPen );
   hb_retl( HB_TRUE );
}


/* Inside the area requested! */
/* wvw_DrawEllipse( nWinNum, nTop, nLeft, nBottom, nRight, aOffset) */
HB_FUNC( WVW_DRAWELLIPSE )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   POINT      xy          = { 0 };
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   APP_DATA * s_sApp      = hb_gt_wvw_GetAppData();
   int        iTop, iLeft, iBottom, iRight;
   int        iOffTop, iOffLeft, iOffBottom, iOffRight;
   USHORT     usTop    = ( USHORT ) hb_parni( 2 ),
              usLeft   = ( USHORT ) hb_parni( 3 ),
              usBottom = ( USHORT ) hb_parni( 4 ),
              usRight  = ( USHORT ) hb_parni( 5 );

   iOffTop    = HB_ISARRAY( 6 ) ? hb_parvni( 6, 1 ) : 0;
   iOffLeft   = HB_ISARRAY( 6 ) ? hb_parvni( 6, 2 ) : 0;
   iOffBottom = HB_ISARRAY( 6 ) ? hb_parvni( 6, 3 ) : 0;
   iOffRight  = HB_ISARRAY( 6 ) ? hb_parvni( 6, 4 ) : 0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   SelectObject( pWindowData->hdc, s_sApp->currentBrush );
   SelectObject( pWindowData->hdc, s_sApp->currentPen );

   hb_retl( Ellipse( pWindowData->hdc, iLeft, iTop, iRight, iBottom ) );
}


/* wvw_DrawRectangle( nWinNum, nTop, nLeft, nBottom, nRight, aOffset, lUsaCurrentPen ) */
HB_FUNC( WVW_DRAWRECTANGLE )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   POINT      xy          = { 0 };
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData(  usWinNum );
   APP_DATA * s_sApp      = hb_gt_wvw_GetAppData();
   int        iTop, iLeft, iBottom, iRight;
   int        iOffTop, iOffLeft, iOffBottom, iOffRight;
   USHORT     usTop    = ( USHORT ) hb_parni( 2 ),
              usLeft   = ( USHORT ) hb_parni( 3 ),
              usBottom = ( USHORT ) hb_parni( 4 ),
              usRight  = ( USHORT ) hb_parni( 5 );
   /* Ref.: 28454 - Marson de Paula - 2007-11-27 */
   BOOL bUsaCurrentPen = hb_parldef( 7, HB_TRUE );

   iOffTop    = HB_ISARRAY( 6 ) ? hb_parvni( 6, 1 ) : 0;
   iOffLeft   = HB_ISARRAY( 6 ) ? hb_parvni( 6, 2 ) : 0;
   iOffBottom = HB_ISARRAY( 6 ) ? hb_parvni( 6, 3 ) : 0;
   iOffRight  = HB_ISARRAY( 6 ) ? hb_parvni( 6, 4 ) : 0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   SelectObject( pWindowData->hdc, s_sApp->currentBrush );
   /* Ref.: 28454 - Marson de Paula - 2007-11-27 */
   if( bUsaCurrentPen )
      SelectObject( pWindowData->hdc, s_sApp->currentPen );
   else
      SelectObject( pWindowData->hdc, s_sApp->penBlack );

   hb_retl( Rectangle( pWindowData->hdc, iLeft, iTop, iRight, iBottom ) );
}

/* wvw_DrawRoundRect( nWinNum, nTop, nLeft, nBottom, nRight, ;
                      aOffset, ; <-- new parm
                      nRoundHeight, nRoundWidth */

/* WARNING!!!
   unlike previous release of GTWVW, 6th parameter is now aOffset
   This placement of new parameter is made in line with gtwvt's way of doing it */

HB_FUNC( WVW_DRAWROUNDRECT )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   POINT      xy          = { 0 };
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   APP_DATA * s_sApp      = hb_gt_wvw_GetAppData();
   int        iTop, iLeft, iBottom, iRight, iWd, iHt;
   USHORT     usTop    = ( USHORT ) hb_parni( 2 ),
              usLeft   = ( USHORT ) hb_parni( 3 ),
              usBottom = ( USHORT ) hb_parni( 4 ),
              usRight  = ( USHORT ) hb_parni( 5 );
   int iOffTop, iOffLeft, iOffBottom, iOffRight;

   iOffTop    = HB_ISARRAY( 6 ) ? hb_parvni( 6, 1 ) : 0;
   iOffLeft   = HB_ISARRAY( 6 ) ? hb_parvni( 6, 2 ) : 0;
   iOffBottom = HB_ISARRAY( 6 ) ? hb_parvni( 6, 3 ) : 0;
   iOffRight  = HB_ISARRAY( 6 ) ? hb_parvni( 6, 4 ) : 0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y + iOffTop;
   iLeft = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1 + iOffBottom;
   iRight  = xy.x - 1 + iOffRight;

   iWd = hb_parni( 8 );
   iHt = hb_parni( 7 );

   SelectObject( pWindowData->hdc, s_sApp->currentBrush );
   SelectObject( pWindowData->hdc, s_sApp->currentPen );

   hb_retl( RoundRect( pWindowData->hdc, iLeft, iTop, iRight, iBottom, iWd, iHt ) );
}


/* wvw_DrawFocusRect( nWinNum, nTop, nLeft, nBottom, nRight, aOffset ) */
HB_FUNC( WVW_DRAWFOCUSRECT )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   RECT       rc          = { 0 };
   POINT      xy          = { 0 };
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   USHORT     usTop       = ( USHORT ) hb_parni( 2 ),
              usLeft      = ( USHORT ) hb_parni( 3 ),
              usBottom    = ( USHORT ) hb_parni( 4 ),
              usRight     = ( USHORT ) hb_parni( 5 );
   int iOffTop, iOffLeft, iOffBottom, iOffRight;

   iOffTop    = HB_ISARRAY( 6 ) ? hb_parvni( 6, 1 ) : 0;
   iOffLeft   = HB_ISARRAY( 6 ) ? hb_parvni( 6, 2 ) : 0;
   iOffBottom = HB_ISARRAY( 6 ) ? hb_parvni( 6, 3 ) : 0;
   iOffRight  = HB_ISARRAY( 6 ) ? hb_parvni( 6, 4 ) : 0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy      = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   rc.top  = xy.y + iOffTop;
   rc.left = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   rc.bottom = xy.y - 1 + iOffBottom;
   rc.right  = xy.x - 1 + iOffRight;

   hb_retl( DrawFocusRect( pWindowData->hdc, &rc ) );
}

/* NOTE: this is compatibility function with GTWVT similar with wvw_FillRectangle() */

/* wvw_DrawColorRect( nWinNum, nTop, nLeft, nBottom, nRight, aPxlOff, nRGB ) */
HB_FUNC( WVW_DRAWCOLORRECT )
{
   UINT       usWinNum        = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData     = hb_gt_wvw_GetWindowsData( usWinNum );
   WIN_DATA * pWindowMainData = hb_gt_wvw_GetWindowsData( 0 );
   APP_DATA * s_sApp   = hb_gt_wvw_GetAppData();
   USHORT     usTop    = ( USHORT ) hb_parni( 2 ),
              usLeft   = ( USHORT ) hb_parni( 3 ),
              usBottom = ( USHORT ) hb_parni( 4 ),
              usRight  = ( USHORT ) hb_parni( 5 );
   int    iOffTop, iOffLeft, iOffBottom, iOffRight;
   RECT   rc = { 0 };
   POINT  xy = { 0 };
   HBRUSH hBrush;

   iOffTop    = HB_ISARRAY( 6 ) ? hb_parvni( 6, 1 ) : 0;
   iOffLeft   = HB_ISARRAY( 6 ) ? hb_parvni( 6, 2 ) : 0;
   iOffBottom = HB_ISARRAY( 6 ) ? hb_parvni( 6, 3 ) : 0;
   iOffRight  = HB_ISARRAY( 6 ) ? hb_parvni( 6, 4 ) : 0;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy      = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   rc.top  = xy.y + iOffTop;
   rc.left = xy.x + iOffLeft;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   rc.bottom = xy.y - 1 + iOffBottom;
   rc.right  = xy.x - 1 + iOffRight;

   hBrush = CreateSolidBrush( ( COLORREF ) hb_parnl( 7 ) );

   if( hBrush )
   {
      hb_retl( FillRect( pWindowData->hdc, &rc, hBrush ) );

      SelectObject( pWindowMainData->hdc, ( HBRUSH ) s_sApp->OriginalBrush );
      DeleteObject( hBrush );
   }
   else
      hb_retl( HB_FALSE );
}


/* wvw_DrawGridHorz( nWinNum, nTop, nLeft, nRight, nRows ) */
HB_FUNC( WVW_DRAWGRIDHORZ )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   USHORT     usAtRow  = ( USHORT ) hb_parni( 2 );
   int        iRows    = hb_parni( 5 );
   int        i, y;
   int        iLeft, iRight;
   WIN_DATA * pWindowData;
   APP_DATA * s_sApp = hb_gt_wvw_GetAppData();
   USHORT     usLeft, usRight;

   pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   usLeft  = ( USHORT ) hb_parni( 3 );
   usRight = ( USHORT ) hb_parni( 4 );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usAtRow, &usLeft, NULL, &usRight );

   iLeft  = usLeft * pWindowData->PTEXTSIZE.x;
   iRight = ( ( usRight + 1 ) * pWindowData->PTEXTSIZE.x ) - 1;

   if( s_sApp->gridPen == NULL )
      s_sApp->gridPen = CreatePen( 0, 0, GetSysColor( COLOR_BTNFACE ) );

   SelectObject( pWindowData->hdc, s_sApp->gridPen );

   for( i = 0; i < iRows; i++ )
   {
      y = usAtRow * hb_gt_wvw_LineHeight( pWindowData );
      y += pWindowData->usTBHeight;

      MoveToEx( pWindowData->hdc, iLeft, y, NULL );
      LineTo( pWindowData->hdc, iRight, y );

      usAtRow++;
   }

   hb_retl( HB_TRUE );
}

/* wvw_DrawGridVert( nWinNum, nTop, nBottom, aCols, nCols, [aOffset] )
 *
 * NOTE: aOffset is TLBR offset in pixel. none in GTWVT
 *       actually aOffset[ 4 ] (Right Offset) is not used here
 */
HB_FUNC( WVW_DRAWGRIDVERT )
{
   UINT       usWinNum = WVW_WHICH_WINDOW;
   int        iTop, iBottom, x;
   int        iOffTop, iOffLeft, iOffBottom, iOffRight;
   int        i;
   int        iCharHeight, iCharWidth;
   int        iTabs = hb_parni( 5 );
   WIN_DATA * pWindowData;
   USHORT     usTop, usBottom, usCol;
   APP_DATA * s_sApp = hb_gt_wvw_GetAppData();

   pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   if( ! iTabs )
      hb_retl( HB_FALSE );

   iOffTop    = HB_ISARRAY( 6 ) ? hb_parvni( 6, 1 ) : 0;
   iOffLeft   = HB_ISARRAY( 6 ) ? hb_parvni( 6, 2 ) : 0;
   iOffBottom = HB_ISARRAY( 6 ) ? hb_parvni( 6, 3 ) : 0;
   iOffRight  = HB_ISARRAY( 6 ) ? hb_parvni( 6, 4 ) : 0;  /* is not actually used */

   HB_SYMBOL_UNUSED( iOffRight );

   usTop    = ( USHORT ) hb_parni( 2 );
   usBottom = ( USHORT ) hb_parni( 3 );
   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, NULL, &usBottom, NULL );

   iCharWidth = pWindowData->PTEXTSIZE.x;

   iCharHeight = hb_gt_wvw_LineHeight( pWindowData );

   iTop    = ( usTop * iCharHeight ) + pWindowData->usTBHeight + iOffTop;
   iBottom = ( ( usBottom + 1 ) * iCharHeight ) - 1 + pWindowData->usTBHeight + iOffBottom;

   if( s_sApp->gridPen == NULL )
      s_sApp->gridPen = CreatePen( 0, 0, GetSysColor( COLOR_BTNFACE ) );

   SelectObject( pWindowData->hdc, s_sApp->gridPen );

   for( i = 1; i <= iTabs; i++ )
   {
      usCol = ( USHORT ) hb_parvni( 4, i );
      if( hb_gt_wvw_GetMainCoordMode() )
         usCol -= pWindowData->usColOfs;

      x = ( usCol * iCharWidth ) + iOffLeft;

      MoveToEx( pWindowData->hdc, x, iTop, NULL );
      LineTo( pWindowData->hdc, x, iBottom );
   }

   hb_retl( HB_TRUE );
}


/* wvw_DrawButton( nWinNum, ;
                   nTop, nLeft, nBottom, nRight, cText, cImage/nImage, nFormat, ;
                   nTextColor, nBkColor, nImageAt ) */
HB_FUNC( WVW_DRAWBUTTON )
{
   UINT  usWinNum = WVW_WHICH_WINDOW;
   SIZE  sz       = { 0 };
   POINT xy       = { 0 };
   RECT  rc       = { 0 };
   int   iTop, iLeft, iBottom, iRight;
   int   iAlign, oldTextAlign, oldBkMode;
   int   iTextHeight /*, iTextWidth */;
   int   iImageWidth, iImageHeight;
   COLORREF /* oldBkColor, */ oldTextColor;
   LOGBRUSH   lb = { 0 };
   HBRUSH     hBrush;
   IPicture * iPicture;

   BOOL bText   = HB_ISCHAR( 6 );
   BOOL bImage  = HB_ISNUM( 7 ) || HB_ISCHAR( 7 );
   int  iFormat = hb_parni( 8 );

   COLORREF textColor = ( COLORREF ) hb_parnldef(  9, s_COLORS[ 0 ] );
   COLORREF bkColor   = ( COLORREF ) hb_parnldef( 10, s_COLORS[ 7 ] );

   WIN_DATA * pWindowData;
   WIN_DATA * pWindowMainData = hb_gt_wvw_GetWindowsData( 0 );
   APP_DATA * s_sApp   = hb_gt_wvw_GetAppData();
   USHORT     usTop    = ( USHORT ) hb_parni( 2 ),
              usLeft   = ( USHORT ) hb_parni( 3 ),
              usBottom = ( USHORT ) hb_parni( 4 ),
              usRight  = ( USHORT ) hb_parni( 5 );

   pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y;
   iLeft = xy.x;

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iBottom = xy.y - 1;
   iRight  = xy.x - 1;

   lb.lbStyle = BS_SOLID;
   lb.lbColor = bkColor;
   lb.lbHatch = 0;

   hBrush = CreateBrushIndirect( &lb );

   rc.left   = iLeft;
   rc.top    = iTop;
   rc.right  = iRight + 1;
   rc.bottom = iBottom + 1;

   FillRect( pWindowData->hdc, &rc, hBrush );

   SelectObject( pWindowMainData->hdc, ( HBRUSH ) s_sApp->OriginalBrush );
   DeleteObject( hBrush );

   switch( iFormat )
   {
      case 1:
         hb_gt_wvw_DrawBoxRecessed( pWindowData->byWinId, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1,
                                   FALSE );
         break;
      case 2:
         break;
      case 3:
         hb_gt_wvw_DrawOutline( pWindowData->byWinId, iTop, iLeft, iBottom, iRight );
         break;

      case 4:
         break;

      default:
         hb_gt_wvw_DrawBoxRaised( pWindowData->byWinId, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1,
                                 FALSE );
         break;
   }

   if( bText )
   {
      SelectObject( pWindowData->hdc, GetStockObject( DEFAULT_GUI_FONT ) );

      GetTextExtentPoint32( pWindowData->hdc, hb_parcx( 6 ), strlen( hb_parcx( 6 ) ), &sz );

      iTextHeight = sz.cy;

      xy.x = iLeft + ( ( iRight - iLeft + 1 ) / 2 );

      if( bImage )
         xy.y = iBottom - 2 - iTextHeight;
      else
         xy.y = iTop + ( ( iBottom - iTop + 1 - iTextHeight ) / 2 );

      if( iFormat == 1 )
      {
         xy.x += 2;
         xy.y += 2;
      }

      iAlign = TA_CENTER + TA_TOP;

      oldTextAlign = SetTextAlign( pWindowData->hdc, iAlign );
      oldBkMode    = SetBkMode( pWindowData->hdc, TRANSPARENT );
      oldTextColor = SetTextColor( pWindowData->hdc, textColor );

      ExtTextOut( pWindowData->hdc, xy.x, xy.y, 0, NULL, hb_parcx( 6 ), strlen( hb_parcx( 6 ) ), NULL );

      SetTextColor( pWindowData->hdc, oldTextColor );
      SetBkMode( pWindowData->hdc, oldBkMode );
      SetTextAlign( pWindowData->hdc, oldTextAlign );
   }
   else
      iTextHeight = -1;

   if( bImage )
   {
      iImageWidth = iRight - iLeft + 1 - 8;

      iImageHeight = iBottom - iTop + 1 - 8 - iTextHeight;

      if( HB_ISNUM( 7 ) )
      {
         iPicture = s_sApp->iPicture[ hb_parni( 7 ) - 1 ];

         hb_gt_wvw_RenderPicture( usWinNum, iLeft + 4, iTop + 4, iImageWidth, iImageHeight, iPicture, FALSE );
      }
      else
         hb_gt_wvw_DrawImage( usWinNum, iLeft + 4, iTop + 4, iImageWidth, iImageHeight, hb_parcx( 7 ), FALSE );
   }

   hb_retl( HB_TRUE );
}


/* wvw_DrawStatusBar() is meant for WVT compatibility only.
   WVW_SBxxxx() functions are recommended instead.
 */
HB_FUNC( WVW_DRAWSTATUSBAR )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   APP_DATA * s_sApp      = hb_gt_wvw_GetAppData();
   int        iPanels     = hb_parni( 2 );
   int        i, iNext;
   int        iTop, iLeft, iBottom, iRight;
   POINT      xy = { 0 };
   USHORT     usTop,
              usLeft,
              usBottom,
              usRight;

   iNext = 0;

   for( i = 0; i < iPanels; i++ )
   {
      usTop    = ( USHORT ) hb_parvni( 3, iNext + 1 );
      usLeft   = ( USHORT ) hb_parvni( 3, iNext + 2 );
      usBottom = ( USHORT ) hb_parvni( 3, iNext + 3 );
      usRight  = ( USHORT ) hb_parvni( 3, iNext + 4 );

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
      iTop  = xy.y;
      iLeft = xy.x + 1;

      xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight, usBottom + 1 );

      xy.y -= pWindowData->byLineSpacing;

      iBottom = xy.y - 1;
      iRight  = xy.x - 2;

      SelectObject( pWindowData->hdc, s_sApp->penWhite );

      MoveToEx( pWindowData->hdc, iRight, iTop, NULL );            /* Right  */
      LineTo( pWindowData->hdc, iRight, iBottom );

      MoveToEx( pWindowData->hdc, iLeft, iBottom, NULL );          /* Bottom */
      LineTo( pWindowData->hdc, iRight, iBottom );

      SelectObject( pWindowData->hdc, s_sApp->penDarkGray );

      MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );             /* Left   */
      LineTo( pWindowData->hdc, iLeft, iBottom );

      MoveToEx( pWindowData->hdc, iLeft, iTop, NULL );             /* Top    */
      LineTo( pWindowData->hdc, iRight, iTop );

      iNext = iNext + 4;
   }

   usTop  = ( USHORT ) hb_parvni( 3, ( 4 * iPanels ) - 1 );
   usLeft = ( USHORT ) hb_parvni( 3, 4 * iPanels );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, NULL, NULL );

   xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop + 1 );

   xy.y -= pWindowData->byLineSpacing;

   iTop    = xy.y - 2;
   iLeft   = xy.x - 2;
   iBottom = iTop;
   iRight  = iLeft;

   SelectObject( pWindowData->hdc, s_sApp->penBlack );

   MoveToEx( pWindowData->hdc, iLeft - 4, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iTop - 4 );
   MoveToEx( pWindowData->hdc, iLeft - 7, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iTop - 7 );
   MoveToEx( pWindowData->hdc, iLeft - 10, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iTop - 10 );

   SelectObject( pWindowData->hdc, s_sApp->penWhite );

   MoveToEx( pWindowData->hdc, iLeft - 5, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iTop - 5 );
   MoveToEx( pWindowData->hdc, iLeft - 8, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iTop - 8 );
   MoveToEx( pWindowData->hdc, iLeft - 11, iBottom, NULL );
   LineTo( pWindowData->hdc, iRight, iTop - 11 );
}


/* wvw_DrawPicture( [nWinNum], nTop, nLeft, nBottom, nRight, nSlot, lTight/aAdj ) -> lOk */
/* nSlot <= 20  aAdj == { 0,0,-2,-2 } To Adjust the pixels for { Top,Left,Bottom,Right } */
HB_FUNC( WVW_DRAWPICTURE )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   POINT      xy          = { 0 };
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   APP_DATA * s_sApp      = hb_gt_wvw_GetAppData();
   int        iTop, iLeft, iBottom, iRight;

   int iSlot = hb_parni( 6 ) - 1;

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   BOOL bTight    = hb_parl( 7 );
   BOOL bUseArray = HB_ISARRAY( 7 );
   int  iOLeft, iOTop, iORight, iOBottom;

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   if( bTight )
   {
      iOTop    = 2 + 1;
      iOLeft   = 2 + 1;
      iOBottom = -1;
      iORight  = -1;
   }
   else if( bUseArray )
   {
      iOTop    = hb_parvni( 7, 1 );
      iOLeft   = hb_parvni( 7, 2 );
      iOBottom = hb_parvni( 7, 3 );
      iORight  = hb_parvni( 7, 4 );
   }
   else
      iOTop = iOLeft = iOBottom = iORight = 0;

   if( iSlot < WVW_PICTURES_MAX && s_sApp->iPicture[ iSlot ] )
   {
      xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
      iTop  = xy.y + iOTop;
      iLeft = xy.x + iOLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );
      iBottom = xy.y - 1 + iOBottom;
      iRight  = xy.x - 1 + iORight;

      hb_retl( hb_gt_wvw_RenderPicture( usWinNum, iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1, s_sApp->iPicture[ iSlot ],
                                       FALSE ) );
   }
   else
      hb_retl( HB_FALSE );
}


/* wvw_DrawLabelEx( [nWinNum], nRow, nCol, cLabel, nAlign, nTextColor, nBkColor, nSlotFont ) */
HB_FUNC( WVW_DRAWLABELEX )
{
   UINT       usWinNum    = WVW_WHICH_WINDOW;
   APP_DATA * s_sApp      = hb_gt_wvw_GetAppData();
   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   POINT      xy = { 0 };
   HFONT      oldFont;
   int        oldTextAlign;
   COLORREF   oldBkColor, oldTextColor;
   int        iSlot = hb_parni( 8 ) - 1;

   USHORT usTop  = ( USHORT ) hb_parni( 2 ),
          usLeft = ( USHORT ) hb_parni( 3 );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, NULL, NULL );

   if( s_sApp->hUserFonts[ iSlot ] )
   {
      xy = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );

      oldBkColor   = SetBkColor( pWindowData->hdc, HB_ISNUM( 7 ) ? ( COLORREF ) hb_parnl( 7 ) : pWindowData->background );
      oldTextColor = SetTextColor( pWindowData->hdc, HB_ISNUM( 6 ) ? ( COLORREF ) hb_parnl( 6 ) : pWindowData->foreground );
      oldTextAlign = SetTextAlign( pWindowData->hdc, hb_parnidef( 5, TA_LEFT ) );
      oldFont      = ( HFONT ) SelectObject( pWindowData->hdc, s_sApp->hUserFonts[ iSlot ] );

      ExtTextOut( pWindowData->hdc, xy.x, xy.y, 0, NULL, hb_parcx( 4 ), strlen( hb_parcx( 4 ) ), NULL );

      SelectObject( pWindowData->hdc, oldFont );
      SetTextAlign( pWindowData->hdc, oldTextAlign );
      SetBkColor( pWindowData->hdc, oldBkColor );
      SetTextColor( pWindowData->hdc, oldTextColor );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}


/*                      1      2       3       4        5        6       7       8      9        */
/* wvw_DrawLineEx( [nWinNum], nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nSlotPen ) */
HB_FUNC( WVW_DRAWLINEEX )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   APP_DATA * s_sApp      = hb_gt_wvw_GetAppData();
   POINT      xy = { 0 };
   int        iTop, iLeft, iBottom, iRight, iOffset;
   int        iOrient, iFormat, iAlign;
   int        x, y;
   HPEN       hPen;
   int        iSlot = hb_parni( 9 ) - 1;

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y;
   iLeft = xy.x;

   xy      = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );
   iBottom = xy.y - 1;
   iRight  = xy.x - 1;

   iOrient = hb_parni( 6 );
   iFormat = hb_parni( 7 );
   iAlign  = hb_parni( 8 );

   x = iLeft;
   y = iTop;

   switch( iAlign )
   {
      case 0:                    /* Center       */
         if( iOrient == 0 )      /* Horizontal   */
         {
            iOffset = ( iBottom - iTop ) / 2;
            y       = iTop + iOffset;
         }
         else
         {
            iOffset = ( iRight - iLeft ) / 2;
            x       = iLeft + iOffset;
         }
         break;

      case 1:                  /* Top     */
         break;

      case 2:                               /* bottom  */
         if( iFormat == 0 || iFormat == 1 ) /* Raised/Recessd */
            y = iBottom - 1;
         else
            y = iBottom;
         break;

      case 3:                  /* Left    */
         break;

      case 4:                               /* Right   */
         if( iFormat == 0 || iFormat == 1 ) /* Raised/Recessd */
            x = iRight - 1;
         else
            x = iRight;
         break;
   }

   hPen = s_sApp->hUserPens[ iSlot ];

   switch( iFormat )
   {
      case 0:                                         /* Raised       */
         if( iOrient == 0 )                           /*  Horizontal  */
         {
            SelectObject( pWindowData->hdc, s_sApp->penWhite );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, iRight, y );
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y + 1, NULL );
            LineTo( pWindowData->hdc, iRight, y + 1 );
         }
         else                                       /*  Vertical    */
         {
            SelectObject( pWindowData->hdc, s_sApp->penWhite );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, x, iBottom );
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x + 1, y, NULL );
            LineTo( pWindowData->hdc, x + 1, iBottom );
         }
         break;

      case 1:                                      /* Recessed    */
         if( iOrient == 0 )                        /* Horizontal  */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, iRight, y );
            SelectObject( pWindowData->hdc, s_sApp->penWhite );
            MoveToEx( pWindowData->hdc, x, y + 1, NULL );
            LineTo( pWindowData->hdc, iRight, y + 1 );
         }
         else                                      /*  Vertical   */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, x, iBottom );
            SelectObject( pWindowData->hdc, s_sApp->penWhite );
            MoveToEx( pWindowData->hdc, x + 1, y, NULL );
            LineTo( pWindowData->hdc, x + 1, iBottom );
         }
         break;

      case 2:                                      /* Plain      */
         if( iOrient == 0 )                        /* Horizontal */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, iRight, y );
         }
         else                                      /*  Vertical  */
         {
            SelectObject( pWindowData->hdc, hPen );
            MoveToEx( pWindowData->hdc, x, y, NULL );
            LineTo( pWindowData->hdc, x, iBottom );
         }
         break;
   }

   hb_retl( HB_TRUE );
}


/* wvw_DrawOutlineEx( [nWinNum], nTop, nLeft, nBottom, nRight, nSlotPen ) */
HB_FUNC( WVW_DRAWOUTLINEEX )
{
   UINT usWinNum = WVW_WHICH_WINDOW;

   WIN_DATA * pWindowData = hb_gt_wvw_GetWindowsData( usWinNum );
   APP_DATA * s_sApp      = hb_gt_wvw_GetAppData();
   POINT      xy = { 0 };
   int        iTop, iLeft, iBottom, iRight;
   int        iSlot = hb_parni( 6 ) - 1;

   USHORT usTop    = ( USHORT ) hb_parni( 2 ),
          usLeft   = ( USHORT ) hb_parni( 3 ),
          usBottom = ( USHORT ) hb_parni( 4 ),
          usRight  = ( USHORT ) hb_parni( 5 );

   if( hb_gt_wvw_GetMainCoordMode() )
      hb_gt_wvw_HBFUNCPrologue( usWinNum, &usTop, &usLeft, &usBottom, &usRight );

   xy    = hb_gt_wvw_GetXYFromColRow( pWindowData, usLeft, usTop );
   iTop  = xy.y - 1;
   iLeft = xy.x - 1;

   xy      = hb_gt_wvw_GetXYFromColRow( pWindowData, usRight + 1, usBottom + 1 );
   iBottom = xy.y;
   iRight  = xy.x;

   if( s_sApp->hUserPens[ iSlot ] )
      SelectObject( pWindowData->hdc, s_sApp->hUserPens[ iSlot ] );
   else
      SelectObject( pWindowData->hdc, s_sApp->penBlack );

   hb_gt_wvw_DrawOutline( usWinNum, iTop, iLeft, iBottom, iRight );
}
