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

/* difference with gtwvt's:
   here we have an option bTight.
   if it is true, only one pixel lines used (outer lines are not drawn)

   TODO: combine it with aOffset like DrawImage ?
 */
static void hb_gt_wvw_DrawBoxRecessed( WVW_WIN * wvw_win, int iTop, int iLeft, int iBottom, int iRight, HB_BOOL bTight )
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   if( ! bTight )
      SelectObject( wvw_win->hdc, wvw->a.penWhiteDim );
   else
      SelectObject( wvw_win->hdc, wvw->a.penWhite );

   MoveToEx( wvw_win->hdc, iRight, iTop, NULL );            /* Right Inner  */
   LineTo( wvw_win->hdc, iRight, iBottom );

   MoveToEx( wvw_win->hdc, iLeft, iBottom, NULL );          /* Bottom Inner */
   LineTo( wvw_win->hdc, iRight, iBottom );

   if( ! bTight )
   {
      SelectObject( wvw_win->hdc, wvw->a.penWhite );

      MoveToEx( wvw_win->hdc, iRight + 1, iTop - 1, NULL );   /* Right Outer  */
      LineTo( wvw_win->hdc, iRight + 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iBottom + 1, NULL ); /* Bottom Outer */
      LineTo( wvw_win->hdc, iRight + 2, iBottom + 1 );
   }

   SelectObject( wvw_win->hdc, wvw->a.penGray );

   MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );             /* Left Inner */
   LineTo( wvw_win->hdc, iLeft, iBottom );

   MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );             /* Top Inner  */
   LineTo( wvw_win->hdc, iRight, iTop );

   if( ! bTight )
   {
      SelectObject( wvw_win->hdc, wvw->a.penDarkGray );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );    /* Left Outer */
      LineTo( wvw_win->hdc, iLeft - 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );    /* Top Outer  */
      LineTo( wvw_win->hdc, iRight + 1, iTop - 1 );
   }
}

/* difference with gtwvt's:
   here we have an option bTight.
   if it is true, only one pixel lines used (outer lines are not drawn)

   TODO: combine it with aOffset like DrawImage ?
 */
static void hb_gt_wvw_DrawBoxRaised( WVW_WIN * wvw_win, int iTop, int iLeft, int iBottom, int iRight, HB_BOOL bTight ) /* <-- none in gtwvt */
{
   WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

   if( ! bTight )
      SelectObject( wvw_win->hdc, wvw->a.penWhiteDim );
   else
      SelectObject( wvw_win->hdc, wvw->a.penWhite );

   MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );  /* Top Inner */
   LineTo( wvw_win->hdc, iRight, iTop );

   MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );  /* Left Inner */
   LineTo( wvw_win->hdc, iLeft, iBottom );

   if( ! bTight )
   {
      SelectObject( wvw_win->hdc, wvw->a.penWhite );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );  /* Top Outer */
      LineTo( wvw_win->hdc, iRight + 1, iTop - 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );  /* Left Outer */
      LineTo( wvw_win->hdc, iLeft - 1, iBottom + 1 );
   }

   if( ! bTight )
      SelectObject( wvw_win->hdc, wvw->a.penDarkGray );
   else
      SelectObject( wvw_win->hdc, wvw->a.penBlack );

   MoveToEx( wvw_win->hdc, iLeft, iBottom, NULL );   /* Bottom Inner */
   LineTo( wvw_win->hdc, iRight, iBottom );

   MoveToEx( wvw_win->hdc, iRight, iBottom, NULL );  /* Right Inner */
   LineTo( wvw_win->hdc, iRight, iTop );

   if( ! bTight )
   {
      SelectObject( wvw_win->hdc, wvw->a.penBlack );

      MoveToEx( wvw_win->hdc, iLeft - 1, iBottom + 1, NULL );  /* Bottom Outer */
      LineTo( wvw_win->hdc, iRight + 1 + 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iRight + 1, iTop - 1, NULL );  /* Right Outer */
      LineTo( wvw_win->hdc, iRight + 1, iBottom + 1 );
   }
}

static void hb_gt_wvw_DrawOutline( WVW_WIN * wvw_win, int iTop, int iLeft, int iBottom, int iRight )
{
   MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );     /* Top    */
   LineTo( wvw_win->hdc, iRight, iTop );

   MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );     /* Left   */
   LineTo( wvw_win->hdc, iLeft, iBottom );

   MoveToEx( wvw_win->hdc, iLeft, iBottom, NULL );  /* Bottom */
   LineTo( wvw_win->hdc, iRight, iBottom );

   MoveToEx( wvw_win->hdc, iRight, iTop, NULL );    /* Right  */
   LineTo( wvw_win->hdc, iRight, iBottom + 1 );
}

static void s_DrawTransparentBitmap( HDC hdc, HBITMAP hBitmap, int xStart, int yStart, int iDestWidth, int iDestHeight )
{
   BITMAP   bm;
   COLORREF cColor;
   HBITMAP  bmAndBack, bmAndObject, bmAndMem;
   HBITMAP  bmBackOld, bmObjectOld, bmMemOld;
   HDC      hdcMem, hdcBack, hdcObject, hdcTemp;
   HBITMAP  bmStretch, bmStretchOld;
   POINT    ptSize;
   COLORREF cTransparentColor;

   HDC hdcCopy = CreateCompatibleDC( hdc );
   SelectObject( hdcCopy, hBitmap );

   cTransparentColor = GetPixel( hdcCopy, 0, 0 );

   GetObject( hBitmap, sizeof( bm ), ( LPVOID ) &bm );
   ptSize.x = bm.bmWidth;
   ptSize.y = bm.bmHeight;
   DPtoLP( hdcCopy, &ptSize, 1 );

   bmStretch    = CreateCompatibleBitmap( hdc, iDestWidth, iDestHeight );
   hdcTemp      = CreateCompatibleDC( hdc );
   bmStretchOld = ( HBITMAP ) SelectObject( hdcTemp, bmStretch );

   StretchBlt( hdcTemp, 0, 0,
               iDestWidth, iDestHeight,
               hdcCopy, 0, 0,
               ptSize.x, ptSize.y,
               SRCCOPY );

   hdcBack   = CreateCompatibleDC( hdc );
   hdcObject = CreateCompatibleDC( hdc );
   hdcMem    = CreateCompatibleDC( hdc );

   bmAndBack = CreateBitmap( iDestWidth, iDestHeight, 1, 1, NULL );

   bmAndObject = CreateBitmap( iDestWidth, iDestHeight, 1, 1, NULL );

   bmAndMem = CreateCompatibleBitmap( hdc, iDestWidth, iDestHeight );

   bmBackOld   = ( HBITMAP ) SelectObject( hdcBack, bmAndBack );
   bmObjectOld = ( HBITMAP ) SelectObject( hdcObject, bmAndObject );
   bmMemOld    = ( HBITMAP ) SelectObject( hdcMem, bmAndMem );

   SetMapMode( hdcTemp, GetMapMode( hdc ) );

   cColor = SetBkColor( hdcTemp, cTransparentColor );

   BitBlt( hdcObject, 0, 0, iDestWidth, iDestHeight, hdcTemp, 0, 0, SRCCOPY );

   SetBkColor( hdcTemp, cColor );

   BitBlt( hdcBack, 0, 0, iDestWidth, iDestHeight, hdcObject, 0, 0, NOTSRCCOPY );
   BitBlt( hdcMem, 0, 0, iDestWidth, iDestHeight, hdc, xStart, yStart, SRCCOPY );
   BitBlt( hdcMem, 0, 0, iDestWidth, iDestHeight, hdcObject, 0, 0, SRCAND );
   BitBlt( hdcTemp, 0, 0, iDestWidth, iDestHeight, hdcBack, 0, 0, SRCAND );
   BitBlt( hdcMem, 0, 0, iDestWidth, iDestHeight, hdcTemp, 0, 0, SRCPAINT );
   BitBlt( hdc, xStart, yStart, iDestWidth, iDestHeight, hdcMem, 0, 0, SRCCOPY );

   DeleteObject( SelectObject( hdcBack, bmBackOld ) );
   DeleteObject( SelectObject( hdcObject, bmObjectOld ) );
   DeleteObject( SelectObject( hdcMem, bmMemOld ) );
   DeleteObject( SelectObject( hdcTemp, bmStretchOld ) );

   DeleteDC( hdcMem );
   DeleteDC( hdcBack );
   DeleteDC( hdcObject );
   DeleteDC( hdcTemp );
   DeleteDC( hdcCopy );
}

/* 2006-07-24 Notes:
   (1) Transparency
   if bTransparent is .T., top-left pixel is used as the transparent color,

   (2) Caching
   WARNING this function will always CACHE the image.
   Do not use it to draw large number of images, because image handle
   is never closed.
   TODO: make it an option.
 */
static HB_BOOL hb_gt_wvw_DrawImage( WVW_WIN * wvw_win, int x1, int y1, int wd, int ht, const char * image, HB_BOOL bTransparent )
{
   HB_BOOL fResult;
   int     iWidth = 0;
   int     iHeight = 0;
   HDC     hdc;

   HBITMAP hBitmap = hb_gt_wvw_FindUserBitmapHandle( image, &iWidth, &iHeight );

   if( ! hBitmap )
   {
      OLE_HANDLE oHtemp;
      BITMAP     bmTemp;

      IPicture * pPic = hb_gt_wvw_LoadPicture( image );

      if( ! pPic )
         return HB_FALSE;

      #if 0
      /* 2006-07-24 canNOT do it this way: */
      HB_VTBL( pPic )->get_Width( HB_THIS_( pPic ) &lWidth );
      HB_VTBL( pPic )->get_Height( HB_THIS_( pPic ) &lHeight );
      iWidth = ( int ) lWidth;
      iHeight = ( int ) lHeight;
      #endif

      if( HB_VTBL( pPic )->get_Handle( HB_THIS_( pPic ) &oHtemp ) == S_OK )
         hBitmap = ( HBITMAP ) CopyImage( ( HBITMAP ) ( HB_PTRDIFF ) oHtemp, IMAGE_BITMAP, 0, 0, LR_COPYRETURNORG );

      hb_gt_wvw_DestroyPicture( pPic );

      if( ! hBitmap )
         return HB_FALSE;

      GetObject( hBitmap, sizeof( bmTemp ), ( LPVOID ) &bmTemp );
      iWidth  = bmTemp.bmWidth;
      iHeight = bmTemp.bmHeight;

      hb_gt_wvw_AddUserBitmapHandle( image, hBitmap, iWidth, iHeight );
   }

   hdc = GetDC( wvw_win->hWnd );

   if( bTransparent )
   {
      s_DrawTransparentBitmap( hdc, hBitmap, x1, y1, wd, ht );
      fResult = HB_TRUE;
   }
   else
   {
      int iOldMode;

      HDC hdcMem = CreateCompatibleDC( hdc );

      SelectObject( hdcMem, hBitmap );

      iOldMode = SetStretchBltMode( hdc, COLORONCOLOR );

      fResult = ( HB_BOOL ) StretchBlt(
         hdc,        /* handle to destination DC */
         x1,         /* x-coord of destination upper-left corner */
         y1,         /* y-coord of destination upper-left corner */
         wd,         /* width of destination rectangle */
         ht,         /* height of destination rectangle */
         hdcMem,     /* handle to source DC */
         0,          /* x-coord of source upper-left corner */
         0,          /* y-coord of source upper-left corner */
         iWidth,     /* width of source rectangle */
         iHeight,    /* height of source rectangle */
         SRCCOPY );  /* raster operation code */

      SetStretchBltMode( hdc, iOldMode );

      DeleteDC( hdcMem );
   }

   ReleaseDC( wvw_win->hWnd, hdc );

   return fResult;
}

static HB_BOOL hb_gt_wvw_RenderPicture( WVW_WIN * wvw_win, int x1, int y1, int wd, int ht, IPicture * iPicture, HB_BOOL bTransp )
{
   HB_BOOL fResult = HB_FALSE;

   if( iPicture )
   {
      OLE_XSIZE_HIMETRIC lWidth;
      OLE_YSIZE_HIMETRIC lHeight;

      int x, y, xe, ye;
      int c   = x1;
      int r   = y1;
      int dc  = wd;
      int dr  = ht;
      int tor = 0;
      int toc = 0;

      HRGN  hrgn1;
      POINT lpp;

      RECT rc_dummy;

      memset( &rc_dummy, 0, sizeof( rc_dummy ) );

      /* if bTransp, we use different method */
      if( bTransp )
      {
         OLE_HANDLE oHtemp;
         HDC        hdc;

         HB_VTBL( iPicture )->get_Handle( HB_THIS_( iPicture ) &oHtemp );

         if( oHtemp )
         {
            hdc = GetDC( wvw_win->hWnd );
            s_DrawTransparentBitmap( hdc, ( HBITMAP ) ( HB_PTRDIFF ) oHtemp, x1, y1, wd, ht );
            ReleaseDC( wvw_win->hWnd, hdc );

            fResult = HB_TRUE;
         }
         else
            fResult = HB_FALSE;
         return fResult;
      }
      /* endif bTransp, we use different method */

      if( HB_VTBL( iPicture )->get_Width( HB_THIS_( iPicture ) &lWidth ) != S_OK )
         lWidth = 0;
      if( HB_VTBL( iPicture )->get_Height( HB_THIS_( iPicture ) &lHeight ) != S_OK )
         lHeight = 0;

      if( dc == 0 )
         dc = ( int ) ( ( float ) dr * lWidth / lHeight );
      if( dr == 0 )
         dr = ( int ) ( ( float ) dc * lHeight / lWidth );
      if( tor == 0 )
         tor = dr;
      if( toc == 0 )
         toc = dc;
      x  = c;
      y  = r;
      xe = c + toc - 1;
      ye = r + tor - 1;

      memset( &lpp, 0, sizeof( lpp ) );

      GetViewportOrgEx( wvw_win->hdc, &lpp );

      hrgn1 = CreateRectRgn( c + lpp.x, r + lpp.y, xe + lpp.x, ye + lpp.y );
      SelectClipRgn( wvw_win->hdc, hrgn1 );

      while( x < xe )
      {
         while( y < ye )
         {
            HB_VTBL( iPicture )->Render( HB_THIS_( iPicture ) wvw_win->hdc, x, y, dc, dr, 0,
                                         lHeight, lWidth, -lHeight, &rc_dummy );
            y += dr;
         }
         y  = r;
         x += dc;
      }

      SelectClipRgn( wvw_win->hdc, NULL );
      DeleteObject( hrgn1 );

      fResult = HB_TRUE;
   }

   return fResult;
}

/* wvw_SetPen( nPenStyle, nWidth, nColor ) */
/* IMPORTANT: in prev release this functions has nWinNum parameter
              PENs are now application-wide. */
HB_FUNC( WVW_SETPEN )
{
   if( HB_ISNUM( 1 ) )
   {
      HPEN hPen = CreatePen( hb_parni( 1 ), hb_parni( 2 ), ( COLORREF ) hb_parnint( 3 ) );

      if( hPen )
      {
         WVW_GLOB * wvw = hb_gt_wvw_GetWvwData();

         if( wvw )
         {
            if( wvw->a.currentPen )
               DeleteObject( wvw->a.currentPen );

            wvw->a.currentPen = hPen;

            hb_retl( HB_TRUE );
            return;
         }
      }
   }

   hb_retl( HB_FALSE );
}

/* wvw_SetBrush( nStyle, nColor, [ nHatch ] ) */
/* IMPORTANT: in prev release this functions has nWinNum parameter
              BRUSHes are now application-wide. */
HB_FUNC( WVW_SETBRUSH )
{
   if( HB_ISNUM( 1 ) )
   {
      HBRUSH   hBrush;
      LOGBRUSH lb;

      memset( &lb, 0, sizeof( lb ) );

      lb.lbStyle = hb_parnl( 1 );
      lb.lbColor = ( COLORREF ) hb_parnint( 2 );
      lb.lbHatch = hb_parnl( 3 );

      hBrush = CreateBrushIndirect( &lb );

      if( hBrush )
      {
         WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
         WVW_WIN *  wvw_zer = hb_gt_wvw_GetWindowsData( 0 );

         if( wvw && wvw_zer )
         {
            if( wvw->a.currentBrush )
            {
               SelectObject( wvw_zer->hdc, wvw->a.OriginalBrush );
               DeleteObject( wvw->a.currentBrush );
            }
            wvw->a.currentBrush = hBrush;

            hb_retl( HB_TRUE );
            return;
         }
      }
   }

   hb_retl( HB_FALSE );
}

/* wvw_DrawBoxGet( [nWinNum], nRow, nCol, nWidth, ;
                   aOffset )   <-- additional parm, not exist in GTWVT */
/* NOTES: unlike GTWVT, GTWVW draw white lines on outer right and outer bottom
          Besides, scope is the same as DRAWBOXRECESSED, ie.
          two pixel out of char boundary */
HB_FUNC( WVW_DRAWBOXGET )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usRow = ( USHORT ) hb_parni( 2 );
      USHORT usCol = ( USHORT ) hb_parni( 3 );
      USHORT usLen = ( USHORT ) hb_parni( 4 );

      int iOffTop    = hb_parvni( 5, 1 );
      int iOffLeft   = hb_parvni( 5, 2 );
      int iOffBottom = hb_parvni( 5, 3 );
      int iOffRight  = hb_parvni( 5, 4 );

      POINT xy;
      POINT yz;
      int   iTop, iLeft, iBottom, iRight;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usRow, &usCol, NULL, NULL );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usCol, usRow );
      iTop  = xy.y - 1 + iOffTop;
      iLeft = xy.x - 1 + iOffLeft;

      yz = hb_gt_wvw_GetXYFromColRow( wvw_win, usCol + usLen, usRow + 1 );

      yz.y -= wvw_win->iLineSpacing;

      iBottom = yz.y + iOffBottom;
      iRight  = yz.x + iOffRight;

      SelectObject( wvw_win->hdc, wvw->a.penBlack );

      MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );
      LineTo( wvw_win->hdc, iRight, iTop );

      MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );
      LineTo( wvw_win->hdc, iLeft, iBottom );

      SelectObject( wvw_win->hdc, wvw->a.penDarkGray );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );
      LineTo( wvw_win->hdc, iRight + 1, iTop - 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );
      LineTo( wvw_win->hdc, iLeft - 1, iBottom + 1 );

      /* GTWVW also draws right and bottom outer with single white line */
      SelectObject( wvw_win->hdc, wvw->a.penWhite );

      MoveToEx( wvw_win->hdc, iRight + 1, iTop - 1, NULL );
      LineTo( wvw_win->hdc, iRight + 1, iBottom + 1 + 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iBottom + 1, NULL );
      LineTo( wvw_win->hdc, iRight + 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iLeft, iBottom, NULL );
      LineTo( wvw_win->hdc, iRight, iBottom );

      MoveToEx( wvw_win->hdc, iRight, iTop, NULL );
      LineTo( wvw_win->hdc, iRight, iBottom + 1 );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawBoxGet_XP( [nWinNum], nRow, nCol, nWidth, ;
                      aOffset )   <-- additional parm, not exist in GTWVT */
/* NOTES: unlike GTWVT, GTWVW draw white lines on outer right and outer bottom
          Besides, scope is the same as DRAWBOXRECESSED, ie.
          two pixel out of char boundary */
HB_FUNC( WVW_DRAWBOXGET_XP )  /* Not in WVT */
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usRow = ( USHORT ) hb_parni( 2 );
      USHORT usCol = ( USHORT ) hb_parni( 3 );
      USHORT usLen = ( USHORT ) hb_parni( 4 );

      int iOffTop    = hb_parvni( 5, 1 );
      int iOffLeft   = hb_parvni( 5, 2 );
      int iOffBottom = hb_parvni( 5, 3 );
      int iOffRight  = hb_parvni( 5, 4 );

      POINT xy;
      POINT yz;
      int   iTop, iLeft, iBottom, iRight;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usRow, &usCol, NULL, NULL );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usCol, usRow );
      iTop  = xy.y - 1 + iOffTop;
      iLeft = xy.x - 1 + iOffLeft;

      yz = hb_gt_wvw_GetXYFromColRow( wvw_win, usCol + usLen, usRow + 1 );

      yz.y -= wvw_win->iLineSpacing;

      iBottom = yz.y + iOffBottom;
      iRight  = yz.x + iOffRight;

      SelectObject( wvw_win->hdc, wvw->a.penGray );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );
      LineTo( wvw_win->hdc, iRight + 1, iTop - 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );
      LineTo( wvw_win->hdc, iLeft - 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iBottom + 1, NULL );
      LineTo( wvw_win->hdc, iRight + 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iRight + 1, iTop - 1, NULL );
      LineTo( wvw_win->hdc, iRight + 1, iBottom + 1 );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
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
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      HB_BOOL bTight = hb_parl( 6 );

      POINT xy;
      int   iTop, iLeft, iBottom, iRight;
      int   iOLeft, iOTop, iORight, iOBottom;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      if( bTight )
      {
         iOTop    = 2;
         iOLeft   = 2;
         iOBottom = -1;
         iORight  = -1;
      }
      else
      {
         iOTop    = hb_parvni( 6, 1 ) - 1;
         iOLeft   = hb_parvni( 6, 2 ) - 1;
         iOBottom = hb_parvni( 6, 3 );
         iORight  = hb_parvni( 6, 4 );
      }

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + iOTop;
      iLeft = xy.x + iOLeft;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y + iOBottom;
      iRight  = xy.x + iORight;

      hb_gt_wvw_DrawBoxRaised( wvw_win, iTop, iLeft, iBottom, iRight, bTight );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
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
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      HB_BOOL bTight = hb_parl( 6 );

      POINT xy;
      int   iTop, iLeft, iBottom, iRight;
      int   iOLeft, iOTop, iORight, iOBottom;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      if( bTight )
      {
         iOTop    = 2;
         iOLeft   = 2;
         iOBottom = -1;
         iORight  = -1;
      }
      else
      {
         iOTop    = hb_parvni( 6, 1 ) - 1;
         iOLeft   = hb_parvni( 6, 2 ) - 1;
         iOBottom = hb_parvni( 6, 3 );
         iORight  = hb_parvni( 6, 4 );
      }

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + iOTop;
      iLeft = xy.x + iOLeft;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y + iOBottom;
      iRight  = xy.x + iORight;

      hb_gt_wvw_DrawBoxRecessed( wvw_win, iTop, iLeft, iBottom, iRight, bTight );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawBoxGroup( nWinNum, nTop, nLeft, nBottom, nRight, [aOffset] ) */
/* NOTE: aOffset is TLBR offset in pixel. none in GTWVT */
HB_FUNC( WVW_DRAWBOXGROUP )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      POINT xy;
      int   iTop, iLeft, iBottom, iRight;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y - 1 + iOffTop;
      iLeft = xy.x - 1 + iOffLeft;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y + iOffBottom;
      iRight  = xy.x + iOffRight;

      SelectObject( wvw_win->hdc, wvw->a.penDarkGray );

      MoveToEx( wvw_win->hdc, iRight, iTop, NULL );            /* Right Inner  */
      LineTo( wvw_win->hdc, iRight, iBottom );

      MoveToEx( wvw_win->hdc, iLeft, iBottom, NULL );          /* Bottom Inner */
      LineTo( wvw_win->hdc, iRight, iBottom );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );     /* Left Outer   */
      LineTo( wvw_win->hdc, iLeft - 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );     /* Top Outer    */
      LineTo( wvw_win->hdc, iRight + 1, iTop - 1 );

      SelectObject( wvw_win->hdc, wvw->a.penWhite );

      MoveToEx( wvw_win->hdc, iRight + 1, iTop, NULL );        /* Right Outer  */
      LineTo( wvw_win->hdc, iRight + 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iBottom + 1, NULL );  /* Bottom Outer */
      LineTo( wvw_win->hdc, iRight + 1 + 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );             /* Left Inner   */
      LineTo( wvw_win->hdc, iLeft, iBottom );

      MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );             /* Top Inner    */
      LineTo( wvw_win->hdc, iRight, iTop );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawBoxRaised( nWinNum, nTop, nLeft, nBottom, nRight ) */
HB_FUNC( WVW_DRAWBOXGROUPRAISED )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      POINT xy;
      int   iTop, iLeft, iBottom, iRight;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y - 1;
      iLeft = xy.x - 1;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y;
      iRight  = xy.x;

      SelectObject( wvw_win->hdc, wvw->a.penWhite );

      MoveToEx( wvw_win->hdc, iRight, iTop, NULL );            /* Right Inner  */
      LineTo( wvw_win->hdc, iRight, iBottom );

      MoveToEx( wvw_win->hdc, iLeft, iBottom, NULL );          /* Bottom Inner */
      LineTo( wvw_win->hdc, iRight, iBottom );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );     /* Left Outer   */
      LineTo( wvw_win->hdc, iLeft - 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iTop - 1, NULL );     /* Top Outer    */
      LineTo( wvw_win->hdc, iRight + 1, iTop - 1 );

      SelectObject( wvw_win->hdc, wvw->a.penDarkGray );

      MoveToEx( wvw_win->hdc, iRight + 1, iTop, NULL );        /* Right Outer  */
      LineTo( wvw_win->hdc, iRight + 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iLeft - 1, iBottom + 1, NULL );  /* Bottom Outer */
      LineTo( wvw_win->hdc, iRight + 1 + 1, iBottom + 1 );

      MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );             /* Left Inner   */
      LineTo( wvw_win->hdc, iLeft, iBottom );

      MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );             /* Top Inner    */
      LineTo( wvw_win->hdc, iRight, iTop );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawImage( nWinNum, ;
                  nTop, nLeft, nBottom, nRight, cImage/nPictureSlot, ;
                  lTight/aOffset, ;
                  lTransparent ) <-- none in gtwvt */

/* 2006-07-24 Notes:
   (1) Image dimension
   if nBottom is NIL, then image height will be proportional to image width.
   if nRight  is NIL, then image width will be proportional to image height.
   if nBottom and nRight are BOTH NIL then original image dimension is used

   (2) Transparency
   if lTransparent is .T., top-left pixel is used as the transparent color,

   (3) Caching
   Image will always be cached. See the WARNING in hb_gt_wvw_DrawImage().
   Do not use this function to draw a large number of images.
   TODO: make it an option. */

HB_FUNC( WVW_DRAWIMAGE )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      POINT xy;
      int   iLeft, iTop, iRight = 0, iBottom = 0;

      HB_BOOL fResult;

      HB_BOOL bActBottom   = ! HB_ISNUM( 4 );
      HB_BOOL bActRight    = ! HB_ISNUM( 5 );
      HB_BOOL bTight       = hb_parl( 7 );
      HB_BOOL bTransparent = hb_parl( 8 );

      int iImgWidth = 0, iImgHeight = 0;
      int iOLeft, iOTop, iORight, iOBottom;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      if( bTight )
      {
         iOTop    = 2 + 1;
         iOLeft   = 2 + 1;
         iOBottom = -1;
         iORight  = -1;
      }
      else
      {
         iOTop    = hb_parvni( 7, 1 );
         iOLeft   = hb_parvni( 7, 2 );
         iOBottom = hb_parvni( 7, 3 );
         iORight  = hb_parvni( 7, 4 );
      }

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + iOTop;
      iLeft = xy.x + iOLeft;

      if( bActRight || bActBottom )
      {
         HB_BOOL fSuccess = HB_FALSE;

         if( HB_ISNUM( 6 ) )
         {
            int iSlot = hb_parni( 6 ) - 1;

            if( iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.iPicture ) )
               fSuccess = hb_gt_wvw_GetIPictDimension( wvw->a.iPicture[ iSlot ], &iImgWidth, &iImgHeight );
         }
         else
            fSuccess = hb_gt_wvw_GetImageDimension( hb_parcx( 6 ), &iImgWidth, &iImgHeight );

         if( ! fSuccess )
         {
            bActRight  = HB_FALSE;
            bActBottom = HB_FALSE;
         }
         else if( bActRight && bActBottom )
         {
            iRight  = iLeft + iImgWidth - 1;
            iBottom = iTop + iImgHeight - 1;
         }
      }

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

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
      {
         int iSlot = hb_parni( 6 ) - 1;

         if( iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.iPicture ) )
            fResult = hb_gt_wvw_RenderPicture( wvw_win, iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, wvw->a.iPicture[ iSlot ], bTransparent );
         else
            fResult = HB_FALSE;
      }
      else
         fResult = hb_gt_wvw_DrawImage( wvw_win, iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, hb_parcx( 6 ), bTransparent );

      hb_retl( fResult );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawImage_Resource( nWinNum, ;
                  nTop, nLeft, nBottom, nRight, nPictureResource/cPictureResource, ;
                  lTight/aOffset, ;
                  lTransparent ) <-- none in gtwvt */

/* 2006-07-24 Notes:
   (1) Image dimension
   if nBottom is NIL, then image height will be proportional to image width.
   if nRight  is NIL, then image width will be proportional to image height.
   if nBottom and nRight are BOTH NIL then original image dimension is used

   (2) Transparency
   if lTransparent is .T., top-left pixel is used as the transparent color,

   (3) Caching
   Image will always be cached. See the WARNING in hb_gt_wvw_DrawImage().
   Do not use this function to draw a large number of images.
   TODO: make it an option. */

HB_FUNC( WVW_DRAWIMAGE_RESOURCE )  /* Not in WVT */
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      long       lImgWidth = 0, lImgHeight = 0;
      IPicture * pPic;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      if( HB_ISNUM( 6 ) )
         pPic = hb_gt_wvw_rr_LoadPictureFromResource( NULL, hb_parni( 6 ), &lImgWidth, &lImgHeight );
      else
      {
         pPic = hb_gt_wvw_rr_LoadPictureFromResource( hb_parcx( 6 ), 0, &lImgWidth, &lImgHeight );

         if( pPic == NULL )
            pPic = hb_gt_wvw_rr_LoadPicture( hb_parcx( 6 ), &lImgWidth, &lImgHeight );
      }

      if( pPic )
      {
         HB_BOOL bActBottom   = ! HB_ISNUM( 4 );
         HB_BOOL bActRight    = ! HB_ISNUM( 5 );
         HB_BOOL bTight       = hb_parl( 7 );
         HB_BOOL bTransparent = hb_parl( 8 );

         int iImgWidth = 0, iImgHeight = 0;
         int iLeft, iTop, iRight = 0, iBottom = 0;
         int iOLeft, iOTop, iORight, iOBottom;

         POINT xy;

         if( bTight )
         {
            iOTop    = 2 + 1;
            iOLeft   = 2 + 1;
            iOBottom = -1;
            iORight  = -1;
         }
         else
         {
            iOTop    = hb_parvni( 7, 1 );
            iOLeft   = hb_parvni( 7, 2 );
            iOBottom = hb_parvni( 7, 3 );
            iORight  = hb_parvni( 7, 4 );
         }

         xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
         iTop  = xy.y + iOTop;
         iLeft = xy.x + iOLeft;

         if( bActRight || bActBottom )
         {
            if( ! hb_gt_wvw_GetIPictDimension( pPic, &iImgWidth, &iImgHeight ) )
            {
               bActRight  = HB_FALSE;
               bActBottom = HB_FALSE;
            }
            else if( bActRight && bActBottom )
            {
               iRight  = iLeft + iImgWidth;
               iBottom = iTop + iImgHeight;
            }
         }

         xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

         xy.y -= wvw_win->iLineSpacing;

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

         hb_retl( hb_gt_wvw_RenderPicture( wvw_win, iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, pPic, bTransparent ) );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawLabel( nWinNum, ;
                  nRow, nCol, cLabel, nAlign, nEscapement, nTextColor, ;
                  nBkColor, cFontFace,nHeight, nWidth, nWeight, ;
                  nQuality, nCharSet, lItalic, lUnderline, lStrikeOut ) */
HB_FUNC( WVW_DRAWLABEL )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw_win )
   {
      USHORT usRow = ( USHORT ) hb_parni( 2 ),
             usCol = ( USHORT ) hb_parni( 3 );

      HFONT    hFont, oldFont;
      LOGFONT  lf;
      int      oldTextAlign;
      COLORREF oldBkColor, oldTextColor;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usRow, &usCol, NULL, NULL );

      memset( &lf, 0, sizeof( lf ) );

      lf.lfEscapement     = hb_parni( 6 ) * 10;
      lf.lfOrientation    = 0;
      lf.lfWeight         = hb_parni( 12 );
      lf.lfItalic         = ( BYTE ) hb_parl( 15 );
      lf.lfUnderline      = ( BYTE ) hb_parl( 16 );
      lf.lfStrikeOut      = ( BYTE ) hb_parl( 17 );
      lf.lfCharSet        = ( BYTE ) hb_parnidef( 14, wvw_win->CodePage );
      lf.lfOutPrecision   = 0;
      lf.lfClipPrecision  = 0;
      lf.lfQuality        = ( BYTE ) hb_parnidef( 13, DEFAULT_QUALITY );
      lf.lfPitchAndFamily = FF_DONTCARE;
      lf.lfHeight         = hb_parnidef( 10, wvw_win->fontHeight );
      lf.lfWidth = hb_parnidef( 11, wvw_win->fontWidth < 0 ? -wvw_win->fontWidth : wvw_win->fontWidth );

      if( HB_ISCHAR( 9 ) )
      {
         HB_ITEMCOPYSTR( hb_param( 9, HB_IT_STRING ), lf.lfFaceName, HB_SIZEOFARRAY( lf.lfFaceName ) );
         wvw_win->fontFace[ HB_SIZEOFARRAY( lf.lfFaceName ) - 1 ] = TEXT( '\0' );
      }
      else
         HB_STRNCPY( lf.lfFaceName, wvw_win->fontFace, HB_SIZEOFARRAY( lf.lfFaceName ) - 1 );

      hFont = CreateFontIndirect( &lf );
      if( hFont )
      {
         HB_SIZE nLen;
         void *  hText;
         LPCTSTR szText = HB_PARSTRDEF( 4, &hText, &nLen );

         POINT xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usCol, usRow );

         oldBkColor   = SetBkColor( wvw_win->hdc, ( COLORREF ) hb_parnintdef( 8, wvw_win->background ) );
         oldTextColor = SetTextColor( wvw_win->hdc, ( COLORREF ) hb_parnintdef( 7, wvw_win->foreground ) );
         oldTextAlign = SetTextAlign( wvw_win->hdc, hb_parnidef( 5, TA_LEFT ) );
         oldFont      = ( HFONT ) SelectObject( wvw_win->hdc, hFont );

         ExtTextOut( wvw_win->hdc, xy.x, xy.y, 0, NULL, szText, ( UINT ) nLen, NULL );

         hb_strfree( hText );

         SelectObject( wvw_win->hdc, oldFont );
         DeleteObject( hFont );
         SetTextAlign( wvw_win->hdc, oldTextAlign );
         SetBkColor( wvw_win->hdc, oldBkColor );
         SetTextColor( wvw_win->hdc, oldTextColor );

         hb_retl( HB_TRUE );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawLabelEx( [nWinNum], nRow, nCol, cLabel, nAlign, nTextColor, nBkColor, nSlotFont ) */
HB_FUNC( WVW_DRAWLABELEX )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      int iSlot = hb_parni( 8 ) - 1;

      if( iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.hUserFonts ) && wvw->a.hUserFonts[ iSlot ] )
      {
         USHORT usTop  = ( USHORT ) hb_parni( 2 );
         USHORT usLeft = ( USHORT ) hb_parni( 3 );

         HFONT    oldFont;
         int      oldTextAlign;
         COLORREF oldBkColor, oldTextColor;

         HB_SIZE nLen;
         void *  hText;
         LPCTSTR szText = HB_PARSTRDEF( 4, &hText, &nLen );

         POINT xy;

         if( hb_gt_wvw_GetMainCoordMode() )
            hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, NULL, NULL );

         xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );

         oldBkColor   = SetBkColor( wvw_win->hdc, ( COLORREF ) hb_parnintdef( 7, wvw_win->background ) );
         oldTextColor = SetTextColor( wvw_win->hdc, ( COLORREF ) hb_parnintdef( 6, wvw_win->foreground ) );
         oldTextAlign = SetTextAlign( wvw_win->hdc, hb_parnidef( 5, TA_LEFT ) );
         oldFont      = ( HFONT ) SelectObject( wvw_win->hdc, wvw->a.hUserFonts[ iSlot ] );

         ExtTextOut( wvw_win->hdc, xy.x, xy.y, 0, NULL, szText, ( UINT ) nLen, NULL );

         hb_strfree( hText );

         SelectObject( wvw_win->hdc, oldFont );
         SetTextAlign( wvw_win->hdc, oldTextAlign );
         SetBkColor( wvw_win->hdc, oldBkColor );
         SetTextColor( wvw_win->hdc, oldTextColor );

         hb_retl( HB_TRUE );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawLabelObj( [nWinNum], nTop, nLeft, nBottom, nRight, cLabel, nAlignHorz, nAlignVert, nTextColor, nBkColor, hFont, aOffset ) */
HB_FUNC( WVW_DRAWLABELOBJ )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      int iOffTop    = hb_parvni( 12, 1 );
      int iOffLeft   = hb_parvni( 12, 2 );
      int iOffBottom = hb_parvni( 12, 3 );
      int iOffRight  = hb_parvni( 12, 4 );

      POINT    xy;
      int      iTop, iLeft, iBottom, iRight, x, y;
      RECT     rc;
      HFONT    oldFont;
      int      oldTextAlign, iAlignHorz, iAlignVert, iAlignH = 0, iAlignV;
      COLORREF oldBkColor, oldTextColor;
      UINT     uiOptions;
      SIZE     sz;

      HB_SIZE nLen;
      void *  hText;
      LPCTSTR szText = HB_PARSTRDEF( 6, &hText, &nLen );

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y - 1 + 1 + iOffBottom;
      iRight  = xy.x - 1 + 1 + iOffRight;

      iAlignHorz = hb_parni( 7 );
      iAlignVert = hb_parni( 8 );

      oldTextColor = SetTextColor( wvw_win->hdc, ( COLORREF ) hb_parnintdef( 9, wvw_win->foreground ) );
      oldBkColor   = SetBkColor( wvw_win->hdc, ( COLORREF ) hb_parnintdef( 10, wvw_win->background ) );
      oldFont      = ( HFONT ) SelectObject( wvw_win->hdc, ( HFONT ) HB_PARHANDLE( 11 ) );

      memset( &sz, 0, sizeof( sz ) );

      GetTextExtentPoint32( wvw_win->hdc, szText, ( int ) nLen, &sz );

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

      oldTextAlign = SetTextAlign( wvw_win->hdc, iAlignH | iAlignV );

      rc.top    = iTop;
      rc.left   = iLeft;
      rc.bottom = iBottom;
      rc.right  = iRight;

      uiOptions = ETO_CLIPPED | ETO_OPAQUE;

      ExtTextOut( wvw_win->hdc, x, y, uiOptions, &rc, szText, ( UINT ) nLen, NULL );

      hb_strfree( hText );

      SelectObject( wvw_win->hdc, oldFont );
      SetTextAlign( wvw_win->hdc, oldTextAlign );
      SetBkColor( wvw_win->hdc, oldBkColor );
      SetTextColor( wvw_win->hdc, oldTextColor );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawOutline( nWinNum, nTop, nLeft, nBottom, nRight, nThick, nShape, nRGBColor ) */
HB_FUNC( WVW_DRAWOUTLINE )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      HPEN  hPen = 0, hOldPen = 0;
      POINT xy;
      int   iTop, iLeft, iBottom, iRight;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y - 1;
      iLeft = xy.x - 1;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y;
      iRight  = xy.x;

      if( HB_ISNUM( 6 ) )
      {
         hPen = CreatePen( hb_parni( 6 ), 0, ( COLORREF ) hb_parnint( 8 ) );
         if( hPen )
            hOldPen = ( HPEN ) SelectObject( wvw_win->hdc, hPen );
      }
      else
         /* hPen = NULL; */
         SelectObject( wvw_win->hdc, wvw->a.penBlack );

      hb_gt_wvw_DrawOutline( wvw_win, iTop, iLeft, iBottom, iRight );

      if( hPen )
      {
         SelectObject( wvw_win->hdc, hOldPen );
         DeleteObject( hPen );
      }

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawOutlineEx( [nWinNum], nTop, nLeft, nBottom, nRight, nSlotPen ) */
HB_FUNC( WVW_DRAWOUTLINEEX )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      int iSlot = hb_parni( 6 ) - 1;

      POINT xy;
      int   iTop, iLeft, iBottom, iRight;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y - 1;
      iLeft = xy.x - 1;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );
      iBottom = xy.y;
      iRight  = xy.x;

      if( iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.hUserPens ) && wvw->a.hUserPens[ iSlot ] )
         SelectObject( wvw_win->hdc, wvw->a.hUserPens[ iSlot ] );
      else
         SelectObject( wvw_win->hdc, wvw->a.penBlack );

      hb_gt_wvw_DrawOutline( wvw_win, iTop, iLeft, iBottom, iRight );
   }
   else
      hb_retl( HB_FALSE );
}

/*                1       2      3       4       5        6        7       8       9      10      11      12
   wvw_DrawLine( nWinNum, nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nStyle, nThick, nColor, aOffset) */
HB_FUNC( WVW_DRAWLINE )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      int iOffTop    = hb_parvni( 12, 1 );
      int iOffLeft   = hb_parvni( 12, 2 );
      int iOffBottom = hb_parvni( 12, 3 );
      int iOffRight  = hb_parvni( 12, 4 );

      int   iTop, iLeft, iBottom, iRight, iOffset;
      int   x, y;
      POINT xy;
      HPEN  hPen, hOldPen;

      int      iOrient = hb_parni( 6 );
      int      iFormat = hb_parni( 7 );
      int      iAlign  = hb_parni( 8 );
      int      iStyle  = hb_parni( 9 );
      int      iThick  = hb_parni( 10 );
      COLORREF cr      = ( COLORREF ) hb_parnint( 11 );

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      x = iLeft;
      y = iTop;

      switch( iAlign )
      {
         case 0:               /* Center */
            if( iOrient == 0 ) /* Horizontal */
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

         case 1:  /* Top */
            break;

         case 2:  /* Bottom */
            if( iFormat == 0 || iFormat == 1 )
               y = iBottom - 1;
            else
               y = iBottom;
            break;

         case 3:  /* Left */
            break;

         case 4:  /* Right */
            if( iFormat == 0 || iFormat == 1 )
               x = iRight - 1;
            else
               x = iRight;
            break;
      }

      hPen    = CreatePen( iStyle, iThick, cr );
      hOldPen = ( HPEN ) SelectObject( wvw_win->hdc, hPen );

      switch( iFormat )
      {
         case 0:               /* Raised */
            if( iOrient == 0 ) /* Horizontal */
            {
               SelectObject( wvw_win->hdc, wvw->a.penWhite );
               MoveToEx( wvw_win->hdc, x, y, NULL );
               LineTo( wvw_win->hdc, iRight, y );
               SelectObject( wvw_win->hdc, hPen );
               MoveToEx( wvw_win->hdc, x, y + 1, NULL );
               LineTo( wvw_win->hdc, iRight, y + 1 );
            }
            else  /* Vertical */
            {
               SelectObject( wvw_win->hdc, wvw->a.penWhite );
               MoveToEx( wvw_win->hdc, x, y, NULL );
               LineTo( wvw_win->hdc, x, iBottom );
               SelectObject( wvw_win->hdc, hPen );
               MoveToEx( wvw_win->hdc, x + 1, y, NULL );
               LineTo( wvw_win->hdc, x + 1, iBottom );
            }
            break;

         case 1:               /* Recessed */
            if( iOrient == 0 ) /* Horizontal */
            {
               SelectObject( wvw_win->hdc, hPen );
               MoveToEx( wvw_win->hdc, x, y, NULL );
               LineTo( wvw_win->hdc, iRight, y );
               SelectObject( wvw_win->hdc, wvw->a.penWhite );
               MoveToEx( wvw_win->hdc, x, y + 1, NULL );
               LineTo( wvw_win->hdc, iRight, y + 1 );
            }
            else  /* Vertical */
            {
               SelectObject( wvw_win->hdc, hPen );
               MoveToEx( wvw_win->hdc, x, y, NULL );
               LineTo( wvw_win->hdc, x, iBottom );
               SelectObject( wvw_win->hdc, wvw->a.penWhite );
               MoveToEx( wvw_win->hdc, x + 1, y, NULL );
               LineTo( wvw_win->hdc, x + 1, iBottom );
            }
            break;

         case 2:               /* Plain */
            if( iOrient == 0 ) /* Horizontal */
            {
               SelectObject( wvw_win->hdc, hPen );
               MoveToEx( wvw_win->hdc, x, y, NULL );
               LineTo( wvw_win->hdc, iRight, y );
            }
            else  /* Vertical */
            {
               SelectObject( wvw_win->hdc, hPen );
               MoveToEx( wvw_win->hdc, x, y, NULL );
               LineTo( wvw_win->hdc, x, iBottom );
            }
            break;
      }

      SelectObject( wvw_win->hdc, hOldPen );
      DeleteObject( hPen );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/*                      1      2       3       4        5        6       7       8      9        */
/* wvw_DrawLineEx( [nWinNum], nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nSlotPen ) */
HB_FUNC( WVW_DRAWLINEEX )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      int iSlot = hb_parni( 9 ) - 1;

      if( iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.hUserPens ) )
      {
         USHORT usTop    = ( USHORT ) hb_parni( 2 ),
                usLeft   = ( USHORT ) hb_parni( 3 ),
                usBottom = ( USHORT ) hb_parni( 4 ),
                usRight  = ( USHORT ) hb_parni( 5 );

         HPEN hPen = wvw->a.hUserPens[ iSlot ];

         POINT xy;
         int   iTop, iLeft, iBottom, iRight, iOffset;
         int   iOrient, iFormat, iAlign;
         int   x, y;

         if( hb_gt_wvw_GetMainCoordMode() )
            hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

         xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
         iTop  = xy.y;
         iLeft = xy.x;

         xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );
         iBottom = xy.y - 1;
         iRight  = xy.x - 1;

         iOrient = hb_parni( 6 );
         iFormat = hb_parni( 7 );
         iAlign  = hb_parni( 8 );

         x = iLeft;
         y = iTop;

         switch( iAlign )
         {
            case 0:               /* Center */
               if( iOrient == 0 ) /* Horizontal */
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

            case 1: /* Top */
               break;

            case 2:                               /* Bottom */
               if( iFormat == 0 || iFormat == 1 ) /* Raised/Recessed */
                  y = iBottom - 1;
               else
                  y = iBottom;
               break;

            case 3: /* Left */
               break;

            case 4:                               /* Right */
               if( iFormat == 0 || iFormat == 1 ) /* Raised/Recessed */
                  x = iRight - 1;
               else
                  x = iRight;
               break;
         }

         switch( iFormat )
         {
            case 0:               /* Raised */
               if( iOrient == 0 ) /* Horizontal */
               {
                  SelectObject( wvw_win->hdc, wvw->a.penWhite );
                  MoveToEx( wvw_win->hdc, x, y, NULL );
                  LineTo( wvw_win->hdc, iRight, y );
                  SelectObject( wvw_win->hdc, hPen );
                  MoveToEx( wvw_win->hdc, x, y + 1, NULL );
                  LineTo( wvw_win->hdc, iRight, y + 1 );
               }
               else  /* Vertical */
               {
                  SelectObject( wvw_win->hdc, wvw->a.penWhite );
                  MoveToEx( wvw_win->hdc, x, y, NULL );
                  LineTo( wvw_win->hdc, x, iBottom );
                  SelectObject( wvw_win->hdc, hPen );
                  MoveToEx( wvw_win->hdc, x + 1, y, NULL );
                  LineTo( wvw_win->hdc, x + 1, iBottom );
               }
               break;

            case 1:               /* Recessed */
               if( iOrient == 0 ) /* Horizontal */
               {
                  SelectObject( wvw_win->hdc, hPen );
                  MoveToEx( wvw_win->hdc, x, y, NULL );
                  LineTo( wvw_win->hdc, iRight, y );
                  SelectObject( wvw_win->hdc, wvw->a.penWhite );
                  MoveToEx( wvw_win->hdc, x, y + 1, NULL );
                  LineTo( wvw_win->hdc, iRight, y + 1 );
               }
               else  /* Vertical */
               {
                  SelectObject( wvw_win->hdc, hPen );
                  MoveToEx( wvw_win->hdc, x, y, NULL );
                  LineTo( wvw_win->hdc, x, iBottom );
                  SelectObject( wvw_win->hdc, wvw->a.penWhite );
                  MoveToEx( wvw_win->hdc, x + 1, y, NULL );
                  LineTo( wvw_win->hdc, x + 1, iBottom );
               }
               break;

            case 2:               /* Plain */
               if( iOrient == 0 ) /* Horizontal */
               {
                  SelectObject( wvw_win->hdc, hPen );
                  MoveToEx( wvw_win->hdc, x, y, NULL );
                  LineTo( wvw_win->hdc, iRight, y );
               }
               else  /* Vertical */
               {
                  SelectObject( wvw_win->hdc, hPen );
                  MoveToEx( wvw_win->hdc, x, y, NULL );
                  LineTo( wvw_win->hdc, x, iBottom );
               }
               break;
         }

         hb_retl( HB_TRUE );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

/* Inside the area requested! */
/* wvw_DrawEllipse( nWinNum, nTop, nLeft, nBottom, nRight, aOffset) */
HB_FUNC( WVW_DRAWELLIPSE )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      POINT xy;
      int   iTop, iLeft, iBottom, iRight;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      SelectObject( wvw_win->hdc, wvw->a.currentBrush );
      SelectObject( wvw_win->hdc, wvw->a.currentPen );

      hb_retl( Ellipse( wvw_win->hdc, iLeft, iTop, iRight, iBottom ) );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawRectangle( nWinNum, nTop, nLeft, nBottom, nRight, aOffset, lUsaCurrentPen ) */
HB_FUNC( WVW_DRAWRECTANGLE )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      POINT   xy;
      int     iTop, iLeft, iBottom, iRight;
      HB_BOOL bUseCurrentPen = hb_parldef( 7, HB_TRUE );  /* Ref.: 28454 - Marson de Paula - 2007-11-27 */

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      SelectObject( wvw_win->hdc, wvw->a.currentBrush );
      /* Ref.: 28454 - Marson de Paula - 2007-11-27 */
      if( bUseCurrentPen )
         SelectObject( wvw_win->hdc, wvw->a.currentPen );
      else
         SelectObject( wvw_win->hdc, wvw->a.penBlack );

      hb_retl( Rectangle( wvw_win->hdc, iLeft, iTop, iRight, iBottom ) );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawRoundRect( nWinNum, nTop, nLeft, nBottom, nRight, ;
                      aOffset, ; <-- new parm
                      nRoundHeight, nRoundWidth */

/* WARNING!!!
   unlike previous release of GTWVW, 6th parameter is now aOffset
   This placement of new parameter is made in line with gtwvt's way of doing it */

HB_FUNC( WVW_DRAWROUNDRECT )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      POINT xy;
      int   iTop, iLeft, iBottom, iRight, iWd, iHt;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      iWd = hb_parni( 8 );
      iHt = hb_parni( 7 );

      SelectObject( wvw_win->hdc, wvw->a.currentBrush );
      SelectObject( wvw_win->hdc, wvw->a.currentPen );

      hb_retl( RoundRect( wvw_win->hdc, iLeft, iTop, iRight, iBottom, iWd, iHt ) );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawFocusRect( nWinNum, nTop, nLeft, nBottom, nRight, aOffset ) */
HB_FUNC( WVW_DRAWFOCUSRECT )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      RECT  rc;
      POINT xy;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      rc.top  = xy.y + iOffTop;
      rc.left = xy.x + iOffLeft;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      rc.bottom = xy.y - 1 + iOffBottom;
      rc.right  = xy.x - 1 + iOffRight;

      hb_retl( DrawFocusRect( wvw_win->hdc, &rc ) );
   }
   else
      hb_retl( HB_FALSE );
}

/* NOTE: this is compatibility function with GTWVT similar with wvw_FillRectangle() */

/* wvw_DrawColorRect( nWinNum, nTop, nLeft, nBottom, nRight, aPxlOff, nRGB ) */
HB_FUNC( WVW_DRAWCOLORRECT )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );
   WVW_WIN *  wvw_zer = hb_gt_wvw_GetWindowsData( 0 );

   if( wvw && wvw_win && wvw_zer )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      RECT   rc;
      POINT  xy;
      HBRUSH hBrush;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      rc.top  = xy.y + iOffTop;
      rc.left = xy.x + iOffLeft;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      rc.bottom = xy.y - 1 + iOffBottom;
      rc.right  = xy.x - 1 + iOffRight;

      hBrush = CreateSolidBrush( ( COLORREF ) hb_parnint( 7 ) );

      if( hBrush )
      {
         hb_retl( FillRect( wvw_win->hdc, &rc, hBrush ) );

         SelectObject( wvw_zer->hdc, wvw->a.OriginalBrush );
         DeleteObject( hBrush );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawGridHorz( nWinNum, nTop, nLeft, nRight, nRows ) */
HB_FUNC( WVW_DRAWGRIDHORZ )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usAtRow = ( USHORT ) hb_parni( 2 );
      USHORT usLeft  = ( USHORT ) hb_parni( 3 );
      USHORT usRight = ( USHORT ) hb_parni( 4 );

      int iRows = hb_parni( 5 );
      int i, y;
      int iLeft, iRight;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usAtRow, &usLeft, NULL, &usRight );

      iLeft  = usLeft * wvw_win->PTEXTSIZE.x;
      iRight = ( ( usRight + 1 ) * wvw_win->PTEXTSIZE.x ) - 1;

      if( wvw->a.gridPen == NULL )
         wvw->a.gridPen = CreatePen( 0, 0, GetSysColor( COLOR_BTNFACE ) );

      SelectObject( wvw_win->hdc, wvw->a.gridPen );

      for( i = 0; i < iRows; i++ )
      {
         y  = usAtRow * hb_gt_wvw_LineHeight( wvw_win );
         y += wvw_win->usTBHeight;

         MoveToEx( wvw_win->hdc, iLeft, y, NULL );
         LineTo( wvw_win->hdc, iRight, y );

         usAtRow++;
      }

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawGridVert( nWinNum, nTop, nBottom, aCols, nCols, [aOffset] ) */
/* NOTE: aOffset is TLBR offset in pixel. none in GTWVT
         actually aOffset[ 4 ] (Right Offset) is not used here */
HB_FUNC( WVW_DRAWGRIDVERT )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 );
      USHORT usBottom = ( USHORT ) hb_parni( 3 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );  /* is not actually used */

      int iTop, iBottom;
      int i;
      int iCharHeight, iCharWidth;
      int iTabs = hb_parni( 5 );

      if( ! iTabs )
      {
         hb_retl( HB_FALSE );
         return;
      }

      HB_SYMBOL_UNUSED( iOffRight );

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, NULL, &usBottom, NULL );

      iCharWidth = wvw_win->PTEXTSIZE.x;

      iCharHeight = hb_gt_wvw_LineHeight( wvw_win );

      iTop    = ( usTop * iCharHeight ) + wvw_win->usTBHeight + iOffTop;
      iBottom = ( ( usBottom + 1 ) * iCharHeight ) - 1 + wvw_win->usTBHeight + iOffBottom;

      if( wvw->a.gridPen == NULL )
         wvw->a.gridPen = CreatePen( 0, 0, GetSysColor( COLOR_BTNFACE ) );

      SelectObject( wvw_win->hdc, wvw->a.gridPen );

      for( i = 1; i <= iTabs; i++ )
      {
         USHORT usCol = ( USHORT ) hb_parvni( 4, i );
         int    x;

         if( hb_gt_wvw_GetMainCoordMode() )
            usCol -= wvw_win->usColOfs;

         x = ( usCol * iCharWidth ) + iOffLeft;

         MoveToEx( wvw_win->hdc, x, iTop, NULL );
         LineTo( wvw_win->hdc, x, iBottom );
      }

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawButton( nWinNum, ;
                   nTop, nLeft, nBottom, nRight, cText, cImage/nImage, nFormat, ;
                   nTextColor, nBkColor, nImageAt ) */
HB_FUNC( WVW_DRAWBUTTON )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );
   WVW_WIN *  wvw_zer = hb_gt_wvw_GetWindowsData( 0 );

   if( wvw && wvw_win && wvw_zer )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      POINT      xy;
      RECT       rc;
      int        iTop, iLeft, iBottom, iRight;
      int        iAlign;
      int        iTextHeight;
      LOGBRUSH   lb;
      HBRUSH     hBrush;
      IPicture * iPicture;

      HB_BOOL bText   = HB_ISCHAR( 6 );
      HB_BOOL bImage  = HB_ISNUM( 7 ) || HB_ISCHAR( 7 );
      int     iFormat = hb_parni( 8 );

      COLORREF textColor = ( COLORREF ) hb_parnintdef(  9, hb_gt_wvw_GetColorData( 0 ) );
      COLORREF bkColor   = ( COLORREF ) hb_parnintdef( 10, hb_gt_wvw_GetColorData( 7 ) );

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y;
      iLeft = xy.x;

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y - 1;
      iRight  = xy.x - 1;

      memset( &lb, 0, sizeof( lb ) );

      lb.lbStyle = BS_SOLID;
      lb.lbColor = bkColor;
      lb.lbHatch = 0;

      hBrush = CreateBrushIndirect( &lb );

      rc.left   = iLeft;
      rc.top    = iTop;
      rc.right  = iRight + 1;
      rc.bottom = iBottom + 1;

      FillRect( wvw_win->hdc, &rc, hBrush );

      SelectObject( wvw_zer->hdc, wvw->a.OriginalBrush );
      DeleteObject( hBrush );

      switch( iFormat )
      {
         case 1:
            hb_gt_wvw_DrawBoxRecessed( wvw_win, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1, HB_FALSE );
            break;
         case 2:
            break;
         case 3:
            hb_gt_wvw_DrawOutline( wvw_win, iTop, iLeft, iBottom, iRight );
            break;
         case 4:
            break;
         default:
            hb_gt_wvw_DrawBoxRaised( wvw_win, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1, HB_FALSE );
      }

      if( bText )
      {
         int      oldTextAlign;
         int      oldBkMode;
         COLORREF oldTextColor;

         HB_SIZE nLen;
         void *  hText;
         LPCTSTR szText = HB_PARSTRDEF( 6, &hText, &nLen );

         SIZE sz;

         SelectObject( wvw_win->hdc, GetStockObject( DEFAULT_GUI_FONT ) );

         memset( &sz, 0, sizeof( sz ) );

         GetTextExtentPoint32( wvw_win->hdc, szText, ( int ) nLen, &sz );

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

         iAlign = TA_CENTER | TA_TOP;

         oldTextAlign = SetTextAlign( wvw_win->hdc, iAlign );
         oldBkMode    = SetBkMode( wvw_win->hdc, TRANSPARENT );
         oldTextColor = SetTextColor( wvw_win->hdc, textColor );

         ExtTextOut( wvw_win->hdc, xy.x, xy.y, 0, NULL, szText, ( UINT ) nLen, NULL );

         hb_strfree( hText );

         SetTextColor( wvw_win->hdc, oldTextColor );
         SetBkMode( wvw_win->hdc, oldBkMode );
         SetTextAlign( wvw_win->hdc, oldTextAlign );
      }
      else
         iTextHeight = -1;

      if( bImage )
      {
         int iImageWidth  = iRight - iLeft + 1 - 8;
         int iImageHeight = iBottom - iTop + 1 - 8 - iTextHeight;

         if( HB_ISNUM( 7 ) )
         {
            int iSlot = hb_parni( 7 ) - 1;

            if( iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.iPicture ) )
            {
               iPicture = wvw->a.iPicture[ iSlot ];

               hb_gt_wvw_RenderPicture( wvw_win, iLeft + 4, iTop + 4, iImageWidth, iImageHeight, iPicture, HB_FALSE );
            }
         }
         else
            hb_gt_wvw_DrawImage( wvw_win, iLeft + 4, iTop + 4, iImageWidth, iImageHeight, hb_parcx( 7 ), HB_FALSE );
      }

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawStatusBar() is meant for WVT compatibility only.
   WVW_SBxxxx() functions are recommended instead. */
HB_FUNC( WVW_DRAWSTATUSBAR )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop;
      USHORT usLeft;
      USHORT usBottom;
      USHORT usRight;

      int   iPanels = hb_parni( 2 );
      int   i, iNext = 0;
      int   iTop, iLeft, iBottom, iRight;
      POINT xy;

      for( i = 0; i < iPanels; i++ )
      {
         usTop    = ( USHORT ) hb_parvni( 3, iNext + 1 );
         usLeft   = ( USHORT ) hb_parvni( 3, iNext + 2 );
         usBottom = ( USHORT ) hb_parvni( 3, iNext + 3 );
         usRight  = ( USHORT ) hb_parvni( 3, iNext + 4 );

         if( hb_gt_wvw_GetMainCoordMode() )
            hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

         xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
         iTop  = xy.y;
         iLeft = xy.x + 1;

         xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight, usBottom + 1 );

         xy.y -= wvw_win->iLineSpacing;

         iBottom = xy.y - 1;
         iRight  = xy.x - 2;

         SelectObject( wvw_win->hdc, wvw->a.penWhite );

         MoveToEx( wvw_win->hdc, iRight, iTop, NULL );    /* Right  */
         LineTo( wvw_win->hdc, iRight, iBottom );

         MoveToEx( wvw_win->hdc, iLeft, iBottom, NULL );  /* Bottom */
         LineTo( wvw_win->hdc, iRight, iBottom );

         SelectObject( wvw_win->hdc, wvw->a.penDarkGray );

         MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );     /* Left   */
         LineTo( wvw_win->hdc, iLeft, iBottom );

         MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );     /* Top    */
         LineTo( wvw_win->hdc, iRight, iTop );

         iNext += 4;
      }

      usTop  = ( USHORT ) hb_parvni( 3, ( 4 * iPanels ) - 1 );
      usLeft = ( USHORT ) hb_parvni( 3, 4 * iPanels );

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, NULL, NULL );

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iTop    = xy.y - 2;
      iLeft   = xy.x - 2;
      iBottom = iTop;
      iRight  = iLeft;

      SelectObject( wvw_win->hdc, wvw->a.penBlack );

      MoveToEx( wvw_win->hdc, iLeft - 4, iBottom, NULL );
      LineTo( wvw_win->hdc, iRight, iTop - 4 );
      MoveToEx( wvw_win->hdc, iLeft - 7, iBottom, NULL );
      LineTo( wvw_win->hdc, iRight, iTop - 7 );
      MoveToEx( wvw_win->hdc, iLeft - 10, iBottom, NULL );
      LineTo( wvw_win->hdc, iRight, iTop - 10 );

      SelectObject( wvw_win->hdc, wvw->a.penWhite );

      MoveToEx( wvw_win->hdc, iLeft - 5, iBottom, NULL );
      LineTo( wvw_win->hdc, iRight, iTop - 5 );
      MoveToEx( wvw_win->hdc, iLeft - 8, iBottom, NULL );
      LineTo( wvw_win->hdc, iRight, iTop - 8 );
      MoveToEx( wvw_win->hdc, iLeft - 11, iBottom, NULL );
      LineTo( wvw_win->hdc, iRight, iTop - 11 );
   }
}

/* wvw_DrawPicture( [nWinNum], nTop, nLeft, nBottom, nRight, nSlot, lTight/aAdj ) -> lOk */
/* nSlot <= 20  aAdj == { 0,0,-2,-2 } To Adjust the pixels for { Top,Left,Bottom,Right } */
HB_FUNC( WVW_DRAWPICTURE )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      int iSlot = hb_parni( 6 ) - 1;

      if( iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.iPicture ) && wvw->a.iPicture[ iSlot ] )
      {
         USHORT usTop    = ( USHORT ) hb_parni( 2 ),
                usLeft   = ( USHORT ) hb_parni( 3 ),
                usBottom = ( USHORT ) hb_parni( 4 ),
                usRight  = ( USHORT ) hb_parni( 5 );

         HB_BOOL bTight = hb_parl( 7 );

         int iTop, iLeft, iBottom, iRight;
         int iOLeft, iOTop, iORight, iOBottom;

         POINT xy;

         if( hb_gt_wvw_GetMainCoordMode() )
            hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

         if( bTight )
         {
            iOTop    = 2 + 1;
            iOLeft   = 2 + 1;
            iOBottom = -1;
            iORight  = -1;
         }
         else
         {
            iOTop    = hb_parvni( 7, 1 );
            iOLeft   = hb_parvni( 7, 2 );
            iOBottom = hb_parvni( 7, 3 );
            iORight  = hb_parvni( 7, 4 );
         }

         xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
         iTop  = xy.y + iOTop;
         iLeft = xy.x + iOLeft;

         xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );
         iBottom = xy.y - 1 + iOBottom;
         iRight  = xy.x - 1 + iORight;

         hb_retl( hb_gt_wvw_RenderPicture( wvw_win, iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1, wvw->a.iPicture[ iSlot ], HB_FALSE ) );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawToolButtonState( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOff, nState ) */
HB_FUNC( WVW_DRAWTOOLBUTTONSTATE )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      POINT xy;
      int   iTop, iLeft, iBottom, iRight;
      int   iState = hb_parni( 7 );

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + hb_parvni( 6, 1 );
      iLeft = xy.x + hb_parvni( 6, 2 );

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y - 1 + hb_parvni( 6, 3 );
      iRight  = xy.x - 1 + hb_parvni( 6, 4 );

      switch( iState )
      {
         case 0:
            SelectObject( wvw_win->hdc, wvw->a.penGray );

            MoveToEx( wvw_win->hdc, iRight, iTop, NULL );    /* Right  */
            LineTo( wvw_win->hdc, iRight, iBottom + 1 );

            MoveToEx( wvw_win->hdc, iLeft, iBottom, NULL );  /* Bottom */
            LineTo( wvw_win->hdc, iRight, iBottom );

            MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );     /* Left   */
            LineTo( wvw_win->hdc, iLeft, iBottom );

            MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );     /* Top    */
            LineTo( wvw_win->hdc, iRight, iTop );
            break;

         case 1:
            SelectObject( wvw_win->hdc, wvw->a.penBlack );

            MoveToEx( wvw_win->hdc, iRight, iTop, NULL );    /* Right  */
            LineTo( wvw_win->hdc, iRight, iBottom + 1 );

            MoveToEx( wvw_win->hdc, iLeft, iBottom, NULL );  /* Bottom */
            LineTo( wvw_win->hdc, iRight, iBottom );

            SelectObject( wvw_win->hdc, wvw->a.penWhite );

            MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );     /* Left   */
            LineTo( wvw_win->hdc, iLeft, iBottom );

            MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );     /* Top    */
            LineTo( wvw_win->hdc, iRight, iTop );
            break;

         case 2:
            SelectObject( wvw_win->hdc, wvw->a.penWhite );

            MoveToEx( wvw_win->hdc, iRight, iTop, NULL );    /* Right  */
            LineTo( wvw_win->hdc, iRight, iBottom + 1 );

            MoveToEx( wvw_win->hdc, iLeft, iBottom, NULL );  /* Bottom */
            LineTo( wvw_win->hdc, iRight, iBottom );

            SelectObject( wvw_win->hdc, wvw->a.penBlack );

            MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );     /* Left   */
            LineTo( wvw_win->hdc, iLeft, iBottom );

            MoveToEx( wvw_win->hdc, iLeft, iTop, NULL );     /* Top    */
            LineTo( wvw_win->hdc, iRight, iTop );
            break;
      }

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawScrollButton( [nWinNum], nTop, nLeft, nBottom, nRight, aOffPixels, nTLBR, lDepressed ) */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be useful */
HB_FUNC( WVW_DRAWSCROLLBUTTON )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      int   iTop, iLeft, iBottom, iRight;
      POINT Point[ 3 ];
      POINT xy;
      int   iHeight, iOff;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + hb_parvni( 6, 1 );
      iLeft = xy.x + hb_parvni( 6, 2 );

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y - 1 + hb_parvni( 6, 3 );
      iRight  = xy.x - 1 + hb_parvni( 6, 4 );

      iOff = 6;

      iHeight = iBottom - iTop + 1;

      if( hb_parl( 8 ) /* bDepressed */ )
         hb_gt_wvw_DrawBoxRecessed( wvw_win, iTop + 1, iLeft + 1, iBottom - 2, iRight - 2, HB_FALSE );
      else
         hb_gt_wvw_DrawBoxRaised( wvw_win, iTop + 1, iLeft + 1, iBottom - 2, iRight - 2, HB_FALSE );
      SelectObject( wvw_win->hdc, wvw->a.solidBrush );

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

         default:
            memset( &Point, 0, sizeof( Point ) );
      }

      Polygon( wvw_win->hdc, Point, HB_SIZEOFARRAY( Point ) );
   }
}

/* Wvw_DrawScrollbarThumbVert( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlScroll, nThumbPos ) */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be useful */
HB_FUNC( WVW_DRAWSCROLLTHUMBVERT )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      USHORT usTabTop = ( USHORT ) hb_parni( 7 );

      POINT xy;
      int   iTop, iLeft, iBottom, iRight;
      int   iTabTop, iTabLft, iTabBtm, iTabRgt;

      if( hb_gt_wvw_GetMainCoordMode() )
      {
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTabTop, NULL, NULL, NULL );
      }

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + hb_parvni( 6, 1 );
      iLeft = xy.x + hb_parvni( 6, 2 );

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y - 1 + hb_parvni( 6, 3 );
      iRight  = xy.x - 1 + hb_parvni( 6, 4 );

      SetBkColor( wvw_win->hdc, RGB( 230, 230, 230 ) );
      SelectObject( wvw_win->hdc, wvw->a.diagonalBrush );

      SelectObject( wvw_win->hdc, wvw->a.penNull );
      Rectangle( wvw_win->hdc, iLeft, iTop, iRight + 1, iBottom + 1 );

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft /* dummy */, usTabTop );
      iTabTop = xy.y;

      iTabLft = iLeft;
      iTabBtm = iTabTop + wvw_win->PTEXTSIZE.y - 1;
      iTabRgt = iRight;

      SelectObject( wvw_win->hdc, wvw->a.wvwWhiteBrush );
      SelectObject( wvw_win->hdc, wvw->a.penGray );
      Rectangle( wvw_win->hdc, iTabLft, iTabTop, iTabRgt + 1, iTabBtm );

      hb_gt_wvw_DrawBoxRaised( wvw_win, iTabTop + 1, iTabLft + 1, iTabBtm - 2, iTabRgt - 2, HB_FALSE );
   }
}

/* Wvw_DrawScrollbarThumbHorz( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffset, nThumbPos ) */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be useful */
HB_FUNC( WVW_DRAWSCROLLTHUMBHORZ )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      USHORT usThumbLeft = ( USHORT ) hb_parni( 7 );

      POINT xy;
      int   iThumbLeft, iThumbRight;
      int   iTop, iLeft, iBottom, iRight;

      if( hb_gt_wvw_GetMainCoordMode() )
      {
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );
         hb_gt_wvw_HBFUNCPrologue( wvw_win, NULL, &usThumbLeft, NULL, NULL );
      }

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + hb_parvni( 6, 1 );
      iLeft = xy.x + hb_parvni( 6, 2 );

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y - 1 + hb_parvni( 6, 3 );
      iRight  = xy.x - 1 + hb_parvni( 6, 4 );

      xy         = hb_gt_wvw_GetXYFromColRow( wvw_win, usThumbLeft, usTop /* dummy */ );
      iThumbLeft = xy.x;

      iThumbRight = iThumbLeft + ( wvw_win->PTEXTSIZE.x * 2 ) - 1;

      SetBkColor( wvw_win->hdc, RGB( 230, 230, 230 ) );
      SelectObject( wvw_win->hdc, wvw->a.diagonalBrush );
      SelectObject( wvw_win->hdc, wvw->a.penNull );
      Rectangle( wvw_win->hdc, iLeft, iTop, iRight + 1, iBottom + 1 );

      SelectObject( wvw_win->hdc, wvw->a.wvwWhiteBrush );
      SelectObject( wvw_win->hdc, wvw->a.penGray );
      Rectangle( wvw_win->hdc, iThumbLeft, iTop, iThumbRight, iBottom );

      hb_gt_wvw_DrawBoxRaised( wvw_win, iTop + 1, iThumbLeft + 1, iBottom - 2, iThumbRight - 2, HB_FALSE );
   }
}

/* wvw_DrawShadedRect( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffSet, nHorVert, aRGBb, aRGBe ) */
HB_FUNC( WVW_DRAWSHADEDRECT )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw && wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      HB_BOOL fResult = HB_FALSE;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      if( wvw->a.hMSImg32 )
      {
         TRIVERTEX     vert[ 2 ];
         GRADIENT_RECT gRect;

         int   iMode = hb_parnidef( 7, GRADIENT_FILL_RECT_H );
         POINT xy;
         int   iTop, iLeft, iBottom, iRight;

         xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
         iTop  = xy.y + hb_parvni( 6, 1 );
         iLeft = xy.x + hb_parvni( 6, 2 );

         xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

         xy.y -= wvw_win->iLineSpacing;

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

         fResult = ( HB_BOOL ) wvw->a.pfnGF( wvw_win->hdc, vert, HB_SIZEOFARRAY( vert ), &gRect, 1, iMode );
      }

      hb_retl( fResult );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawTextBox( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffSet, cText, ;
                    nAlignHorz, nAlignVert, nTextColor, nBackColor, ;
                    nBackMode, hFont ) */
HB_FUNC( WVW_DRAWTEXTBOX )
{
   WVW_WIN * wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );

   if( wvw_win )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      POINT xy;
      int   iTop, iLeft, iBottom, iRight;

      int iAlignH = 0;

      COLORREF oldTextColor, oldBkColor;
      HFONT    oldFont;
      int      oldTextAlign, oldBkMode;
      RECT     rc;

      HB_SIZE nLen;
      void *  hText;
      LPCTSTR szText = HB_PARSTRDEF( 7, &hText, &nLen );

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + hb_parvni( 6, 1 );
      iLeft = xy.x + hb_parvni( 6, 2 );

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y - 1 + hb_parvni( 6, 3 );
      iRight  = xy.x - 1 + hb_parvni( 6, 4 );

      switch( hb_parni( 8 ) )
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

      oldTextAlign = SetTextAlign( wvw_win->hdc, TA_TOP | TA_LEFT | TA_NOUPDATECP );
      oldTextColor = SetTextColor( wvw_win->hdc, ( COLORREF ) hb_parnintdef( 10, wvw_win->foreground ) );
      oldBkColor   = SetBkColor( wvw_win->hdc, ( COLORREF ) hb_parnintdef( 11, wvw_win->background ) );
      oldBkMode    = SetBkMode( wvw_win->hdc, hb_parnidef( 12, OPAQUE ) );
      oldFont      = ( HFONT ) SelectObject( wvw_win->hdc, ( HFONT ) HB_PARHANDLE( 13 ) );

      DrawText( wvw_win->hdc, szText, ( int ) nLen, &rc, iAlignH | DT_WORDBREAK | DT_TOP );

      hb_strfree( hText );

      SetTextColor( wvw_win->hdc, oldTextColor );
      SetBkColor( wvw_win->hdc, oldBkColor );
      SetBkMode( wvw_win->hdc, oldBkMode );
      SetTextAlign( wvw_win->hdc, oldTextAlign );
      SelectObject( wvw_win->hdc, oldFont );
   }
}

/* wvw_DrawProgressBar( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlTLBR, nPercent, ;
                        nBackColor, nBarColor, cImage, lVertical, nDirection ) */
HB_FUNC( WVW_DRAWPROGRESSBAR )
{
   WVW_GLOB * wvw     = hb_gt_wvw_GetWvwData();
   WVW_WIN *  wvw_win = hb_gt_wvw_GetWindowsData( hb_gt_wvw_nWin() );
   WVW_WIN *  wvw_zer = hb_gt_wvw_GetWindowsData( 0 );

   if( wvw && wvw_win && wvw_zer )
   {
      USHORT usTop    = ( USHORT ) hb_parni( 2 ),
             usLeft   = ( USHORT ) hb_parni( 3 ),
             usBottom = ( USHORT ) hb_parni( 4 ),
             usRight  = ( USHORT ) hb_parni( 5 );

      int iPercent   = hb_parni( 7 );
      int iDirection = hb_parni( 12 );

      int iTop;
      int iLeft;
      int iBottom;
      int iRight;
      int iBarUpto;

      RECT  rc;
      POINT xy;

      if( hb_gt_wvw_GetMainCoordMode() )
         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + hb_parvni( 6, 1 );
      iLeft = xy.x + hb_parvni( 6, 2 );

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      xy.y -= wvw_win->iLineSpacing;

      iBottom = xy.y - 1 + hb_parvni( 6, 3 );
      iRight  = xy.x - 1 + hb_parvni( 6, 4 );

      if( hb_parl( 11 ) /* bVertical */ )
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

      if( HB_ISCHAR( 10 ) )
         hb_gt_wvw_DrawImage( wvw_win, rc.left, rc.top, rc.right - rc.left + 1, rc.bottom - rc.top + 1, hb_parc( 10 ), HB_FALSE );
      else
      {
         HBRUSH   hBrush;
         LOGBRUSH lb;

         memset( &lb, 0, sizeof( lb ) );

         lb.lbStyle = BS_SOLID;
         lb.lbColor = ( COLORREF ) hb_parnintdef( 9, hb_gt_wvw_GetColorData( 0 ) );
         lb.lbHatch = 0;

         hBrush = CreateBrushIndirect( &lb );

         rc.bottom++;
         rc.right++;
         FillRect( wvw_win->hdc, &rc, hBrush );

         SelectObject( wvw_zer->hdc, wvw->a.OriginalBrush );
         DeleteObject( hBrush );
      }
   }
}
