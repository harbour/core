/*
 * Video subsystem for Windows using GUI windows instead of Console
 * with multiple windows support
 *   Copyright 2004 Budyanto Dj. <budyanto@centrin.net.id>
 * GTWVW draw functions
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

/* difference with GTWVT's:
   here we have an option fTight.
   if it is true, only one pixel lines used (outer lines are not drawn)
   TODO: combine it with aOffset like DrawImage ? */
static void hb_gt_wvw_DrawBoxRecessed( HDC hDC, int iTop, int iLeft, int iBottom, int iRight, HB_BOOL fTight )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( ! fTight )
      SelectObject( hDC, wvw->a.penWhiteDim );
   else
      SelectObject( hDC, wvw->a.penWhite );

   MoveToEx( hDC, iRight, iTop, NULL );            /* Right Inner  */
   LineTo( hDC, iRight, iBottom );

   MoveToEx( hDC, iLeft, iBottom, NULL );          /* Bottom Inner */
   LineTo( hDC, iRight, iBottom );

   if( ! fTight )
   {
      SelectObject( hDC, wvw->a.penWhite );

      MoveToEx( hDC, iRight + 1, iTop - 1, NULL );   /* Right Outer  */
      LineTo( hDC, iRight + 1, iBottom + 1 );

      MoveToEx( hDC, iLeft - 1, iBottom + 1, NULL ); /* Bottom Outer */
      LineTo( hDC, iRight + 2, iBottom + 1 );
   }

   SelectObject( hDC, wvw->a.penGray );

   MoveToEx( hDC, iLeft, iTop, NULL );             /* Left Inner */
   LineTo( hDC, iLeft, iBottom );

   MoveToEx( hDC, iLeft, iTop, NULL );             /* Top Inner  */
   LineTo( hDC, iRight, iTop );

   if( ! fTight )
   {
      SelectObject( hDC, wvw->a.penDarkGray );

      MoveToEx( hDC, iLeft - 1, iTop - 1, NULL );    /* Left Outer */
      LineTo( hDC, iLeft - 1, iBottom + 1 );

      MoveToEx( hDC, iLeft - 1, iTop - 1, NULL );    /* Top Outer  */
      LineTo( hDC, iRight + 1, iTop - 1 );
   }
}

/* difference with GTWVT's:
   here we have an option fTight.
   if it is true, only one pixel lines used (outer lines are not drawn)
   TODO: combine it with aOffset like DrawImage ? */
static void hb_gt_wvw_DrawBoxRaised( HDC hDC, int iTop, int iLeft, int iBottom, int iRight, HB_BOOL fTight ) /* <-- none in GTWVT */
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( ! fTight )
      SelectObject( hDC, wvw->a.penWhiteDim );
   else
      SelectObject( hDC, wvw->a.penWhite );

   MoveToEx( hDC, iLeft, iTop, NULL );  /* Top Inner */
   LineTo( hDC, iRight, iTop );

   MoveToEx( hDC, iLeft, iTop, NULL );  /* Left Inner */
   LineTo( hDC, iLeft, iBottom );

   if( ! fTight )
   {
      SelectObject( hDC, wvw->a.penWhite );

      MoveToEx( hDC, iLeft - 1, iTop - 1, NULL );  /* Top Outer */
      LineTo( hDC, iRight + 1, iTop - 1 );

      MoveToEx( hDC, iLeft - 1, iTop - 1, NULL );  /* Left Outer */
      LineTo( hDC, iLeft - 1, iBottom + 1 );
   }

   if( ! fTight )
      SelectObject( hDC, wvw->a.penDarkGray );
   else
      SelectObject( hDC, wvw->a.penBlack );

   MoveToEx( hDC, iLeft, iBottom, NULL );   /* Bottom Inner */
   LineTo( hDC, iRight, iBottom );

   MoveToEx( hDC, iRight, iBottom, NULL );  /* Right Inner */
   LineTo( hDC, iRight, iTop );

   if( ! fTight )
   {
      SelectObject( hDC, wvw->a.penBlack );

      MoveToEx( hDC, iLeft - 1, iBottom + 1, NULL );  /* Bottom Outer */
      LineTo( hDC, iRight + 1 + 1, iBottom + 1 );

      MoveToEx( hDC, iRight + 1, iTop - 1, NULL );  /* Right Outer */
      LineTo( hDC, iRight + 1, iBottom + 1 );
   }
}

static void hb_gt_wvw_DrawOutline( HDC hDC, int iTop, int iLeft, int iBottom, int iRight )
{
   MoveToEx( hDC, iLeft, iTop, NULL );     /* Top    */
   LineTo( hDC, iRight, iTop );

   MoveToEx( hDC, iLeft, iTop, NULL );     /* Left   */
   LineTo( hDC, iLeft, iBottom );

   MoveToEx( hDC, iLeft, iBottom, NULL );  /* Bottom */
   LineTo( hDC, iRight, iBottom );

   MoveToEx( hDC, iRight, iTop, NULL );    /* Right  */
   LineTo( hDC, iRight, iBottom + 1 );
}

static void s_DrawTransparentBitmap( HDC hDC, HBITMAP hBitmap, int xStart, int yStart, int iDestWidth, int iDestHeight )
{
   BITMAP   bm;
   COLORREF cColor;
   HBITMAP  bmAndBack, bmAndObject, bmAndMem;
   HBITMAP  bmBackOld, bmObjectOld, bmMemOld;
   HDC      hdcMem, hdcBack, hdcObject, hdcTemp;
   HBITMAP  bmStretch, bmStretchOld;
   POINT    ptSize;
   COLORREF cTransparentColor;

   HDC hdcCopy = CreateCompatibleDC( hDC );

   SelectObject( hdcCopy, hBitmap );

   cTransparentColor = GetPixel( hdcCopy, 0, 0 );

   GetObject( hBitmap, sizeof( bm ), ( LPVOID ) &bm );
   ptSize.x = bm.bmWidth;
   ptSize.y = bm.bmHeight;
   DPtoLP( hdcCopy, &ptSize, 1 );

   bmStretch    = CreateCompatibleBitmap( hDC, iDestWidth, iDestHeight );
   hdcTemp      = CreateCompatibleDC( hDC );
   bmStretchOld = ( HBITMAP ) SelectObject( hdcTemp, bmStretch );

   StretchBlt( hdcTemp, 0, 0,
               iDestWidth, iDestHeight,
               hdcCopy, 0, 0,
               ptSize.x, ptSize.y,
               SRCCOPY );

   hdcBack   = CreateCompatibleDC( hDC );
   hdcObject = CreateCompatibleDC( hDC );
   hdcMem    = CreateCompatibleDC( hDC );

   bmAndBack = CreateBitmap( iDestWidth, iDestHeight, 1, 1, NULL );

   bmAndObject = CreateBitmap( iDestWidth, iDestHeight, 1, 1, NULL );

   bmAndMem = CreateCompatibleBitmap( hDC, iDestWidth, iDestHeight );

   bmBackOld   = ( HBITMAP ) SelectObject( hdcBack, bmAndBack );
   bmObjectOld = ( HBITMAP ) SelectObject( hdcObject, bmAndObject );
   bmMemOld    = ( HBITMAP ) SelectObject( hdcMem, bmAndMem );

   SetMapMode( hdcTemp, GetMapMode( hDC ) );

   cColor = SetBkColor( hdcTemp, cTransparentColor );

   BitBlt( hdcObject, 0, 0, iDestWidth, iDestHeight, hdcTemp, 0, 0, SRCCOPY );

   SetBkColor( hdcTemp, cColor );

   BitBlt( hdcBack, 0, 0, iDestWidth, iDestHeight, hdcObject, 0, 0, NOTSRCCOPY );
   BitBlt( hdcMem, 0, 0, iDestWidth, iDestHeight, hDC, xStart, yStart, SRCCOPY );
   BitBlt( hdcMem, 0, 0, iDestWidth, iDestHeight, hdcObject, 0, 0, SRCAND );
   BitBlt( hdcTemp, 0, 0, iDestWidth, iDestHeight, hdcBack, 0, 0, SRCAND );
   BitBlt( hdcMem, 0, 0, iDestWidth, iDestHeight, hdcTemp, 0, 0, SRCPAINT );
   BitBlt( hDC, xStart, yStart, iDestWidth, iDestHeight, hdcMem, 0, 0, SRCCOPY );

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
   if fTransparent is .T., top-left pixel is used as the transparent color,

   (2) Caching
   WARNING this function will always CACHE the image.
   Do not use it to draw large number of images, because image handle
   is never closed.
   TODO: make it an option. */
static HB_BOOL hb_gt_wvw_DrawImage( HWND hWnd, int x1, int y1, int wd, int ht, const char * image, HB_BOOL fTransparent )
{
   HB_BOOL fResult;
   int     iWidth  = 0;
   int     iHeight = 0;

   HBITMAP hBitmap = hb_gt_wvw_FindUserBitmapHandle( image, &iWidth, &iHeight );

   HDC hDC = GetDC( hWnd );  /* QUESTION: does this function need a new DC, or s_DrawTransparentBitmap()? */

   if( ! hBitmap )
   {
      HBITMAP hBitmapTemp;
      BITMAP  bmTemp;

      IPicture * pPicture = hb_gt_wvw_LoadPicture( image );

      if( ! pPicture )
         return HB_FALSE;

      #if 0
      /* 2006-07-24 canNOT do it this way: */
      HB_VTBL( pPicture )->get_Width( HB_THIS_( pPicture ) & lWidth );
      HB_VTBL( pPicture )->get_Height( HB_THIS_( pPicture ) & lHeight );
      iWidth  = ( int ) lWidth;
      iHeight = ( int ) lHeight;
      #endif

      if( HB_VTBL( pPicture )->get_Handle( HB_THIS_ ( pPicture ) ( OLE_HANDLE * ) & hBitmapTemp ) == S_OK )
         hBitmap = ( HBITMAP ) CopyImage( hBitmapTemp, IMAGE_BITMAP, 0, 0, LR_COPYRETURNORG );

      hb_gt_wvw_DestroyPicture( pPicture );

      if( ! hBitmap )
         return HB_FALSE;

      GetObject( hBitmap, sizeof( bmTemp ), ( LPVOID ) &bmTemp );
      iWidth  = bmTemp.bmWidth;
      iHeight = bmTemp.bmHeight;

      hb_gt_wvw_AddUserBitmapHandle( image, hBitmap, iWidth, iHeight );
   }

   if( fTransparent )
   {
      s_DrawTransparentBitmap( hDC, hBitmap, x1, y1, wd, ht );
      fResult = HB_TRUE;
   }
   else
   {
      int iOldMode;

      HDC hdcMem = CreateCompatibleDC( hDC );

      SelectObject( hdcMem, hBitmap );

      iOldMode = SetStretchBltMode( hDC, COLORONCOLOR );

      fResult = ( HB_BOOL ) StretchBlt(
         hDC,        /* handle to destination DC */
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

      SetStretchBltMode( hDC, iOldMode );

      DeleteDC( hdcMem );
   }

   ReleaseDC( hWnd, hDC );

   return fResult;
}

static HB_BOOL hb_gt_wvw_RenderPicture( PWVW_WIN wvw_win, int x1, int y1, int wd, int ht, IPicture * pPicture, HB_BOOL bTransp )
{
   HB_BOOL fResult = HB_FALSE;

   if( pPicture )
   {
      OLE_XSIZE_HIMETRIC nWidth;
      OLE_YSIZE_HIMETRIC nHeight;

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
         HBITMAP hBitmap;

         HB_VTBL( pPicture )->get_Handle( HB_THIS_ ( pPicture ) ( OLE_HANDLE * ) & hBitmap );

         if( hBitmap )
         {
            HDC hDCTemp = GetDC( wvw_win->hWnd );
            s_DrawTransparentBitmap( hDCTemp, hBitmap, x1, y1, wd, ht );
            ReleaseDC( wvw_win->hWnd, hDCTemp );

            fResult = HB_TRUE;
         }
         else
            fResult = HB_FALSE;

         return fResult;
      }
      /* endif bTransp, we use different method */

      if( HB_VTBL( pPicture )->get_Width( HB_THIS_( pPicture ) & nWidth ) != S_OK )
         nWidth = 0;
      if( HB_VTBL( pPicture )->get_Height( HB_THIS_( pPicture ) & nHeight ) != S_OK )
         nHeight = 0;

      if( dc == 0 )
         dc = ( int ) ( ( float ) dr * nWidth / nHeight );
      if( dr == 0 )
         dr = ( int ) ( ( float ) dc * nHeight / nWidth );
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
            HB_VTBL( pPicture )->Render( HB_THIS_( pPicture ) wvw_win->hdc, x, y, dc, dr, 0, ( OLE_YPOS_HIMETRIC ) nHeight, nWidth, -nHeight, &rc_dummy );
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

/* s_FindPictureHandle() and s_AddPictureHandle() are for bitmaps associated with
   Windows controls such as toolbar, pushbutton, checkbox, etc */
static IPicture * s_FindPictureHandle( PWVW_GLO wvw, const char * szFileName, int * piWidth, int * piHeight )
{
   WVW_IPIC * pph = wvw->a.pphPictureList;

   HB_BOOL fStrictDimension = ! ( *piWidth == 0 && *piHeight == 0 );

   while( pph )
   {
      if( strcmp( szFileName, pph->szFilename ) == 0 &&
          ( ! fStrictDimension ||
            ( *piWidth == pph->iWidth &&
              *piHeight == pph->iHeight
            )
          ) )
      {
         if( ! fStrictDimension )
         {
            *piWidth  = pph->iWidth;
            *piHeight = pph->iHeight;
         }
         return pph->pPicture;
      }

      pph = pph->pNext;
   }

   return NULL;
}

static void s_AddPictureHandle( PWVW_GLO wvw, const char * szFileName, IPicture * pPicture, int iWidth, int iHeight )
{
   WVW_IPIC * pphNew = ( WVW_IPIC * ) hb_xgrabz( sizeof( WVW_IPIC ) );

   hb_strncpy( pphNew->szFilename, szFileName, sizeof( pphNew->szFilename ) - 1 );
   pphNew->pPicture = pPicture;
   pphNew->iWidth   = iWidth;
   pphNew->iHeight  = iHeight;
   pphNew->pNext    = wvw->a.pphPictureList;

   wvw->a.pphPictureList = pphNew;
}

static IPicture * hb_gt_wvw_rr_LoadPictureFromResource( PWVW_GLO wvw, const char * resname, HB_UINT iresimage, int * piWidth, int * piHeight )
{
   HBITMAP    hbmpx;
   IPicture * pPicture = NULL;

   int iWidth  = *piWidth;
   int iHeight = *piHeight;

   if( resname )
   {
      hbmpx = hb_gt_wvw_FindBitmapHandle( resname, &iWidth, &iHeight );

      if( ! hbmpx )
      {
         LPTSTR lpFree;
         hbmpx = ( HBITMAP ) LoadImage( wvw->hInstance, HB_FSNAMECONV( resname, &lpFree ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
         if( lpFree )
            hb_xfree( lpFree );
         hb_gt_wvw_AddBitmapHandle( resname, hbmpx, iWidth, iHeight );
      }
   }
   else
   {
      char szResname[ HB_PATH_MAX + 1 ];

      hb_snprintf( szResname, sizeof( szResname ), "?%u", iresimage );

      hbmpx = hb_gt_wvw_FindBitmapHandle( szResname, &iWidth, &iHeight );

      if( ! hbmpx )
      {
         hbmpx = ( HBITMAP ) LoadImage( wvw->hInstance, ( LPCTSTR ) MAKEINTRESOURCE( ( WORD ) iresimage ), IMAGE_BITMAP, 0, 0, LR_DEFAULTCOLOR );
         hb_gt_wvw_AddBitmapHandle( szResname, hbmpx, iWidth, iHeight );
      }

      resname = ( const char * ) szResname;
   }

   *piWidth  = iWidth;
   *piHeight = iHeight;

   if( hbmpx )
   {
      pPicture = s_FindPictureHandle( wvw, resname, &iWidth, &iHeight );

      if( pPicture )
      {
         OLE_XSIZE_HIMETRIC nWidth;
         OLE_YSIZE_HIMETRIC nHeight;

         if( HB_VTBL( pPicture )->get_Width( HB_THIS_( pPicture ) & nWidth ) != S_OK )
            nWidth = 0;
         if( HB_VTBL( pPicture )->get_Height( HB_THIS_( pPicture ) & nHeight ) != S_OK )
            nHeight = 0;

         *piWidth  = ( int ) nWidth;
         *piHeight = ( int ) nHeight;
      }
      else
      {
         PICTDESC picd;

         picd.cbSizeofstruct = sizeof( picd );
         picd.picType        = PICTYPE_BITMAP;
         picd.bmp.hbitmap    = hbmpx;

         OleCreatePictureIndirect( &picd, HB_ID_REF( IID_IPicture ), TRUE, ( LPVOID * ) &pPicture );

         s_AddPictureHandle( wvw, resname, pPicture, iWidth, iHeight );
      }
   }

   return pPicture;
}

static IPicture * hb_gt_wvw_rr_LoadPicture( const char * filename, int * piWidth, int * piHeight )
{
   IPicture * pPicture = NULL;

   HB_FHANDLE fhnd = hb_fsOpen( filename, FO_READ | FO_SHARED );

   if( fhnd != FS_ERROR )
   {
      DWORD   nFileSize = ( DWORD ) hb_fsSeek( fhnd, 0, FS_END );
      HGLOBAL hGlobal   = GlobalAlloc( GMEM_MOVEABLE, nFileSize + 4096 );

      if( hGlobal )
      {
         void * pGlobal = GlobalLock( hGlobal );

         if( pGlobal )
         {
            IStream * iStream = NULL;

            memset( pGlobal, 0, nFileSize );

            hb_fsSeek( fhnd, 0, FS_SET );
            hb_fsReadLarge( fhnd, pGlobal, nFileSize );

            if( CreateStreamOnHGlobal( hGlobal, TRUE, &iStream ) == S_OK && iStream )
            {
               OleLoadPicture( iStream, nFileSize, TRUE, HB_ID_REF( IID_IPicture ), ( LPVOID * ) &pPicture );
               HB_VTBL( iStream )->Release( HB_THIS( iStream ) );
            }
            else
               pPicture = NULL;

            GlobalUnlock( hGlobal );
            GlobalFree( hGlobal );

            if( pPicture )
            {
               OLE_XSIZE_HIMETRIC nWidth;
               OLE_YSIZE_HIMETRIC nHeight;

               if( HB_VTBL( pPicture )->get_Width( HB_THIS_( pPicture ) & nWidth ) != S_OK )
                  nWidth = 0;
               if( HB_VTBL( pPicture )->get_Height( HB_THIS_( pPicture ) & nHeight ) != S_OK )
                  nHeight = 0;

               *piWidth  = ( int ) nWidth;
               *piHeight = ( int ) nHeight;
            }
         }

         hb_fsClose( fhnd );
      }
   }

   return pPicture;
}

/* wvw_SetPen( nPenStyle, nWidth, nColor ) */
/* IMPORTANT: in prev release this functions has nWinNum parameter
              PENs are now application-wide. */
HB_FUNC( WVW_SETPEN )
{
   PWVW_GLO wvw = hb_gt_wvw();

   if( wvw && HB_ISNUM( 1 ) )
   {
      HPEN hPen = CreatePen( hb_parni( 1 ), hb_parni( 2 ), ( COLORREF ) hb_parnint( 3 ) );

      if( hPen )
      {
         if( wvw->a.currentPen )
            DeleteObject( wvw->a.currentPen );

         wvw->a.currentPen = hPen;

         hb_retl( HB_TRUE );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

/* wvw_SetBrush( nStyle, nColor, [ nHatch ] ) */
/* IMPORTANT: in prev release this functions has nWinNum parameter
              BRUSHes are now application-wide. */
HB_FUNC( WVW_SETBRUSH )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_zer = hb_gt_wvw_win( 0 );

   if( wvw && wvw_zer && HB_ISNUM( 1 ) )
   {
      HBRUSH   hBrush;
      LOGBRUSH lb;

      memset( &lb, 0, sizeof( lb ) );

      lb.lbStyle = ( UINT ) hb_parnint( 1 );
      lb.lbColor = ( COLORREF ) hb_parnint( 2 );
      lb.lbHatch = ( ULONG_PTR ) hb_parnint( 3 );

      hBrush = CreateBrushIndirect( &lb );

      if( hBrush )
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

   hb_retl( HB_FALSE );
}

/* wvw_DrawBoxGet( [nWinNum], nRow, nCol, nWidth, ;
                   aOffset )   <-- additional parm, not exist in GTWVT */
/* NOTES: unlike GTWVT, GTWVW draw white lines on outer right and outer bottom
          Besides, scope is the same as WVW_DRAWBOXRECESSED(), ie.
          two pixel out of char boundary */
HB_FUNC( WVW_DRAWBOXGET )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iRow = hb_parni( 2 );
      int iCol = hb_parni( 3 );
      int iLen = hb_parni( 4 );

      int iOffTop    = hb_parvni( 5, 1 );
      int iOffLeft   = hb_parvni( 5, 2 );
      int iOffBottom = hb_parvni( 5, 3 );
      int iOffRight  = hb_parvni( 5, 4 );

      POINT xy;
      int   iTop, iLeft, iBottom, iRight;
      HDC   hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iRow, &iCol, NULL, NULL );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iCol, iRow );
      iTop  = xy.y - 1 + iOffTop;
      iLeft = xy.x - 1 + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iCol + iLen, iRow + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing + iOffBottom;
      iRight  = xy.x + iOffRight;

      SelectObject( hDC, wvw->a.penBlack );

      MoveToEx( hDC, iLeft, iTop, NULL );
      LineTo( hDC, iRight, iTop );

      MoveToEx( hDC, iLeft, iTop, NULL );
      LineTo( hDC, iLeft, iBottom );

      SelectObject( hDC, wvw->a.penDarkGray );

      MoveToEx( hDC, iLeft - 1, iTop - 1, NULL );
      LineTo( hDC, iRight + 1, iTop - 1 );

      MoveToEx( hDC, iLeft - 1, iTop - 1, NULL );
      LineTo( hDC, iLeft - 1, iBottom + 1 );

      /* GTWVW also draws right and bottom outer with single white line */
      SelectObject( hDC, wvw->a.penWhite );

      MoveToEx( hDC, iRight + 1, iTop - 1, NULL );
      LineTo( hDC, iRight + 1, iBottom + 1 + 1 );

      MoveToEx( hDC, iLeft - 1, iBottom + 1, NULL );
      LineTo( hDC, iRight + 1, iBottom + 1 );

      MoveToEx( hDC, iLeft, iBottom, NULL );
      LineTo( hDC, iRight, iBottom );

      MoveToEx( hDC, iRight, iTop, NULL );
      LineTo( hDC, iRight, iBottom + 1 );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawBoxGet_XP( [nWinNum], nRow, nCol, nWidth, ;
                      aOffset )   <-- additional parm, not exist in GTWVT */
/* NOTES: unlike GTWVT, GTWVW draw white lines on outer right and outer bottom
          Besides, scope is the same as WVW_DRAWBOXRECESSED(), ie.
          two pixel out of char boundary */
HB_FUNC( WVW_DRAWBOXGET_XP )  /* Not in WVT */
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iRow = hb_parni( 2 );
      int iCol = hb_parni( 3 );
      int iLen = hb_parni( 4 );

      int iOffTop    = hb_parvni( 5, 1 );
      int iOffLeft   = hb_parvni( 5, 2 );
      int iOffBottom = hb_parvni( 5, 3 );
      int iOffRight  = hb_parvni( 5, 4 );

      POINT xy;
      int   iTop, iLeft, iBottom, iRight;
      HDC   hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iRow, &iCol, NULL, NULL );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iCol, iRow );
      iTop  = xy.y - 1 + iOffTop;
      iLeft = xy.x - 1 + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iCol + iLen, iRow + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing + iOffBottom;
      iRight  = xy.x + iOffRight;

      SelectObject( hDC, wvw->a.penGray );

      MoveToEx( hDC, iLeft - 1, iTop - 1, NULL );
      LineTo( hDC, iRight + 1, iTop - 1 );

      MoveToEx( hDC, iLeft - 1, iTop - 1, NULL );
      LineTo( hDC, iLeft - 1, iBottom + 1 );

      MoveToEx( hDC, iLeft - 1, iBottom + 1, NULL );
      LineTo( hDC, iRight + 1, iBottom + 1 );

      MoveToEx( hDC, iRight + 1, iTop - 1, NULL );
      LineTo( hDC, iRight + 1, iBottom + 1 );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawBoxRaised( nWinNum, nTop, nLeft, nBottom, nRight, lTight/aOffset ) */

/* if lTight, box is drawn inside the character region
   AND top and left lines are lower two pixel down to make room for above/left object
   WARNING: gui object of this type subject to be overwritten by chars
   NOTE that these lines are to be overwritten by displayed char,
        we are depending on the fact that gui object will be painted last

   lTight may be replaced with aOffset parm {top,left,bottom,right}
     ie. offset in pixel unit */
HB_FUNC( WVW_DRAWBOXRAISED )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      HB_BOOL fTight = hb_parl( 6 );

      POINT xy;
      int   iOffLeft, iOffTop, iOffRight, iOffBottom;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      if( fTight )
      {
         iOffTop    = 2;
         iOffLeft   = 2;
         iOffBottom = -1;
         iOffRight  = -1;
      }
      else
      {
         iOffTop    = hb_parvni( 6, 1 ) - 1;
         iOffLeft   = hb_parvni( 6, 2 ) - 1;
         iOffBottom = hb_parvni( 6, 3 );
         iOffRight  = hb_parvni( 6, 4 );
      }

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing + iOffBottom;
      iRight  = xy.x + iOffRight;

      hb_gt_wvw_DrawBoxRaised( wvw_win->hdc, iTop, iLeft, iBottom, iRight, fTight );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawBoxRecessed( nWinNum, nTop, nLeft, nBottom, nRight, lTight/aOffset ) <--none in GTWVT */

/* if lTight, box is drawn inside the character region
   AND top and left lines are lower two pixel down to make room for above/left object
   WARNING: gui object of this type subject to be overwritten by chars
   NOTE that these lines are to be overwritten by displayed char,
        we are depending on the fact that gui object will be painted last

   lTight may be replaced with aOffset parm {top,left,bottom,right}
     ie. offset in pixel unit */
HB_FUNC( WVW_DRAWBOXRECESSED )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      HB_BOOL fTight = hb_parl( 6 );

      POINT xy;
      int   iOffLeft, iOffTop, iOffRight, iOffBottom;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      if( fTight )
      {
         iOffTop    = 2;
         iOffLeft   = 2;
         iOffBottom = -1;
         iOffRight  = -1;
      }
      else
      {
         iOffTop    = hb_parvni( 6, 1 ) - 1;
         iOffLeft   = hb_parvni( 6, 2 ) - 1;
         iOffBottom = hb_parvni( 6, 3 );
         iOffRight  = hb_parvni( 6, 4 );
      }

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing + iOffBottom;
      iRight  = xy.x + iOffRight;

      hb_gt_wvw_DrawBoxRecessed( wvw_win->hdc, iTop, iLeft, iBottom, iRight, fTight );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawBoxGroup( nWinNum, nTop, nLeft, nBottom, nRight, [aOffset] ) */
/* NOTE: aOffset is TLBR offset in pixel. none in GTWVT */
HB_FUNC( WVW_DRAWBOXGROUP )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop, iLeft, iBottom, iRight;
      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_GetCoord( wvw_win, 2, 6, &iTop, &iLeft, &iBottom, &iRight );

      SelectObject( hDC, wvw->a.penDarkGray );

      MoveToEx( hDC, iRight, iTop, NULL );            /* Right Inner  */
      LineTo( hDC, iRight, iBottom );

      MoveToEx( hDC, iLeft, iBottom, NULL );          /* Bottom Inner */
      LineTo( hDC, iRight, iBottom );

      MoveToEx( hDC, iLeft - 1, iTop - 1, NULL );     /* Left Outer   */
      LineTo( hDC, iLeft - 1, iBottom + 1 );

      MoveToEx( hDC, iLeft - 1, iTop - 1, NULL );     /* Top Outer    */
      LineTo( hDC, iRight + 1, iTop - 1 );

      SelectObject( hDC, wvw->a.penWhite );

      MoveToEx( hDC, iRight + 1, iTop, NULL );        /* Right Outer  */
      LineTo( hDC, iRight + 1, iBottom + 1 );

      MoveToEx( hDC, iLeft - 1, iBottom + 1, NULL );  /* Bottom Outer */
      LineTo( hDC, iRight + 1 + 1, iBottom + 1 );

      MoveToEx( hDC, iLeft, iTop, NULL );             /* Left Inner   */
      LineTo( hDC, iLeft, iBottom );

      MoveToEx( hDC, iLeft, iTop, NULL );             /* Top Inner    */
      LineTo( hDC, iRight, iTop );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawBoxRaised( nWinNum, nTop, nLeft, nBottom, nRight ) */
HB_FUNC( WVW_DRAWBOXGROUPRAISED )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop, iLeft, iBottom, iRight;
      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_GetCoord( wvw_win, 2, 0, &iTop, &iLeft, &iBottom, &iRight );

      SelectObject( hDC, wvw->a.penWhite );

      MoveToEx( hDC, iRight, iTop, NULL );            /* Right Inner  */
      LineTo( hDC, iRight, iBottom );

      MoveToEx( hDC, iLeft, iBottom, NULL );          /* Bottom Inner */
      LineTo( hDC, iRight, iBottom );

      MoveToEx( hDC, iLeft - 1, iTop - 1, NULL );     /* Left Outer   */
      LineTo( hDC, iLeft - 1, iBottom + 1 );

      MoveToEx( hDC, iLeft - 1, iTop - 1, NULL );     /* Top Outer    */
      LineTo( hDC, iRight + 1, iTop - 1 );

      SelectObject( hDC, wvw->a.penDarkGray );

      MoveToEx( hDC, iRight + 1, iTop, NULL );        /* Right Outer  */
      LineTo( hDC, iRight + 1, iBottom + 1 );

      MoveToEx( hDC, iLeft - 1, iBottom + 1, NULL );  /* Bottom Outer */
      LineTo( hDC, iRight + 1 + 1, iBottom + 1 );

      MoveToEx( hDC, iLeft, iTop, NULL );             /* Left Inner   */
      LineTo( hDC, iLeft, iBottom );

      MoveToEx( hDC, iLeft, iTop, NULL );             /* Top Inner    */
      LineTo( hDC, iRight, iTop );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawImage( nWinNum, ;
      nTop, nLeft, nBottom, nRight, cImage/nPictureSlot, ;
      lTight/aOffset, ;
      lTransparent ) <-- none in GTWVT */

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
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int usTop    = hb_parni( 2 ),
          usLeft   = hb_parni( 3 ),
          usBottom = hb_parni( 4 ),
          usRight  = hb_parni( 5 );

      POINT xy;
      int   iLeft, iTop, iRight = 0, iBottom = 0;

      HB_BOOL fResult;

      HB_BOOL fActBottom   = ! HB_ISNUM( 4 );
      HB_BOOL fActRight    = ! HB_ISNUM( 5 );
      HB_BOOL fTransparent = hb_parl( 8 );

      int iImgWidth = 0, iImgHeight = 0;
      int iOffLeft, iOffTop, iOffRight, iOffBottom;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

      if( hb_parl( 7 ) /* fTight */ )
      {
         iOffTop    = 2 + 1;
         iOffLeft   = 2 + 1;
         iOffBottom = -1;
         iOffRight  = -1;
      }
      else
      {
         iOffTop    = hb_parvni( 7, 1 );
         iOffLeft   = hb_parvni( 7, 2 );
         iOffBottom = hb_parvni( 7, 3 );
         iOffRight  = hb_parvni( 7, 4 );
      }

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      if( fActRight || fActBottom )
      {
         HB_BOOL fSuccess = HB_FALSE;

         if( HB_ISNUM( 6 ) )
         {
            int iSlot = hb_parni( 6 ) - 1;

            if( iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.pPicture ) )
               fSuccess = hb_gt_wvw_GetIPictDimension( wvw->a.pPicture[ iSlot ], &iImgWidth, &iImgHeight );
         }
         else
            fSuccess = hb_gt_wvw_GetImageDimension( hb_parcx( 6 ), &iImgWidth, &iImgHeight );

         if( ! fSuccess )
         {
            fActRight  = HB_FALSE;
            fActBottom = HB_FALSE;
         }
         else if( fActRight && fActBottom )
         {
            iRight  = iLeft + iImgWidth - 1;
            iBottom = iTop + iImgHeight - 1;
         }
      }

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

      if( ! fActBottom )
         iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;

      if( ! fActRight )
         iRight = xy.x - 1 + iOffRight;

      if( ( fActBottom || fActRight ) && ! ( fActBottom && fActRight ) )
      {
         if( fActRight )
            iRight = iLeft + ( int ) ( ( float ) iImgWidth / iImgHeight * ( iBottom - iTop + 1) ) - 1;  /* right corner (width) must be proportional to height */
         else
            iBottom = iTop + ( int ) ( ( float ) iImgHeight / iImgWidth * ( iRight - iLeft + 1 ) ) - 1;  /* bottom corner (height) must be proportional to width */
      }

      if( HB_ISNUM( 6 ) )
      {
         int iSlot = hb_parni( 6 ) - 1;

         if( iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.pPicture ) )
            fResult = hb_gt_wvw_RenderPicture( wvw_win, iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, wvw->a.pPicture[ iSlot ], fTransparent );
         else
            fResult = HB_FALSE;
      }
      else
         fResult = hb_gt_wvw_DrawImage( wvw_win->hWnd, iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, hb_parcx( 6 ), fTransparent );

      hb_retl( fResult );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawImage_Resource( nWinNum, ;
      nTop, nLeft, nBottom, nRight, nPictureResource/cPictureResource, ;
      lTight/aOffset, ;
      lTransparent ) <-- none in GTWVT */

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
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int        iImgWidth = 0, iImgHeight = 0;
      IPicture * pPicture;

      if( HB_ISNUM( 6 ) )
         pPicture = hb_gt_wvw_rr_LoadPictureFromResource( wvw, NULL, hb_parni( 6 ), &iImgWidth, &iImgHeight );
      else
      {
         pPicture = hb_gt_wvw_rr_LoadPictureFromResource( wvw, hb_parcx( 6 ), 0, &iImgWidth, &iImgHeight );

         if( pPicture == NULL )
            pPicture = hb_gt_wvw_rr_LoadPicture( hb_parcx( 6 ), &iImgWidth, &iImgHeight );
      }

      if( pPicture )
      {
         int usTop    = hb_parni( 2 ),
             usLeft   = hb_parni( 3 ),
             usBottom = hb_parni( 4 ),
             usRight  = hb_parni( 5 );

         HB_BOOL fActBottom = ! HB_ISNUM( 4 );
         HB_BOOL fActRight  = ! HB_ISNUM( 5 );

         int iLeft, iTop, iRight = 0, iBottom = 0;
         int iOffLeft, iOffTop, iOffRight, iOffBottom;

         POINT xy;

         hb_gt_wvw_HBFUNCPrologue( wvw_win, &usTop, &usLeft, &usBottom, &usRight );

         if( hb_parl( 7 ) /* fTight */ )
         {
            iOffTop    = 2 + 1;
            iOffLeft   = 2 + 1;
            iOffBottom = -1;
            iOffRight  = -1;
         }
         else
         {
            iOffTop    = hb_parvni( 7, 1 );
            iOffLeft   = hb_parvni( 7, 2 );
            iOffBottom = hb_parvni( 7, 3 );
            iOffRight  = hb_parvni( 7, 4 );
         }

         xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, usLeft, usTop );
         iTop  = xy.y + iOffTop;
         iLeft = xy.x + iOffLeft;

         if( fActRight || fActBottom )
         {
            if( ! hb_gt_wvw_GetIPictDimension( pPicture, &iImgWidth, &iImgHeight ) )
            {
               fActRight  = HB_FALSE;
               fActBottom = HB_FALSE;
            }
            else if( fActRight && fActBottom )
            {
               iRight  = iLeft + iImgWidth;
               iBottom = iTop + iImgHeight;
            }
         }

         xy = hb_gt_wvw_GetXYFromColRow( wvw_win, usRight + 1, usBottom + 1 );

         if( ! fActBottom )
            iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;

         if( ! fActRight )
            iRight = xy.x - 1 + iOffRight;

         if( ( fActBottom || fActRight ) && ! ( fActBottom && fActRight ) )
         {
            if( fActRight )
               iRight = iLeft + ( int ) ( ( float ) iImgWidth / iImgHeight * ( iBottom - iTop + 1 ) ) - 1;  /* right corner (width) must be proportional to height */
            else
               iBottom = iTop + ( int ) ( ( float ) iImgHeight / iImgWidth * ( iRight - iLeft + 1 ) ) - 1;  /* bottom corner (height) must be proportional to width */
         }

         hb_retl( hb_gt_wvw_RenderPicture( wvw_win, iLeft, iTop, ( iRight - iLeft ) + 1, ( iBottom - iTop ) + 1, pPicture, hb_parl( 8 ) /* fTransparent */ ) );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

/* wvw_DrawLabel( nWinNum, ;
      nRow, nCol, cLabel, nAlign, nEscapement, nTextColor, ;
      nBkColor, cFontFace,nHeight, nWidth, nWeight, ;
      nQuality, nCharSet, lItalic, lUnderline, lStrikeOut ) */
HB_FUNC( WVW_DRAWLABEL )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iRow = hb_parni( 2 ),
          iCol = hb_parni( 3 );

      HFONT   hFont;
      LOGFONT lf;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iRow, &iCol, NULL, NULL );

      memset( &lf, 0, sizeof( lf ) );

      lf.lfEscapement     = hb_parnl( 6 ) * 10;
      lf.lfOrientation    = 0;
      lf.lfWeight         = hb_parnl( 12 );
      lf.lfItalic         = ( BYTE ) hb_parl( 15 );
      lf.lfUnderline      = ( BYTE ) hb_parl( 16 );
      lf.lfStrikeOut      = ( BYTE ) hb_parl( 17 );
      lf.lfCharSet        = ( BYTE ) hb_parnidef( 14, wvw_win->CodePage );
      lf.lfOutPrecision   = 0;
      lf.lfClipPrecision  = 0;
      lf.lfQuality        = ( BYTE ) hb_parnidef( 13, DEFAULT_QUALITY );
      lf.lfPitchAndFamily = FF_DONTCARE;
      lf.lfHeight         = hb_parnldef( 10, wvw_win->fontHeight );
      lf.lfWidth = hb_parnldef( 11, wvw_win->fontWidth < 0 ? -wvw_win->fontWidth : wvw_win->fontWidth );

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
         HDC hDC = wvw_win->hdc;

         HFONT    oldFont      = ( HFONT ) SelectObject( hDC, hFont );
         int      oldTextAlign = SetTextAlign( hDC, hb_parnidef( 5, TA_LEFT ) );
         COLORREF oldBkColor   = SetBkColor( hDC, ( COLORREF ) hb_parnintdef( 8, wvw_win->background ) );
         COLORREF oldTextColor = SetTextColor( hDC, ( COLORREF ) hb_parnintdef( 7, wvw_win->foreground ) );

         HB_SIZE nLen;
         void *  hText;
         LPCTSTR szText = HB_PARSTRDEF( 4, &hText, &nLen );

         POINT xy = hb_gt_wvw_GetXYFromColRow( wvw_win, iCol, iRow );

         ExtTextOut( hDC, xy.x, xy.y, 0, NULL, szText, ( UINT ) nLen, NULL );

         hb_strfree( hText );

         SelectObject( hDC, oldFont );
         DeleteObject( hFont );
         SetTextAlign( hDC, oldTextAlign );
         SetBkColor( hDC, oldBkColor );
         SetTextColor( hDC, oldTextColor );

         hb_retl( HB_TRUE );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

/* wvw_DrawLabelEx( [nWinNum], nRow, nCol, cLabel, nAlign, nTextColor, nBkColor, nSlotFont ) */
HB_FUNC( WVW_DRAWLABELEX )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   int iSlot = hb_parni( 8 ) - 1;

   if( wvw && wvw_win && iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.hUserFonts ) && wvw->a.hUserFonts[ iSlot ] )
   {
      int iTop  = hb_parni( 2 );
      int iLeft = hb_parni( 3 );

      HB_SIZE nLen;
      void *  hText;
      LPCTSTR szText = HB_PARSTRDEF( 4, &hText, &nLen );

      HDC hDC = wvw_win->hdc;

      int      oldTextAlign = SetTextAlign( hDC, hb_parnidef( 5, TA_LEFT ) );
      COLORREF oldTextColor = SetTextColor( hDC, ( COLORREF ) hb_parnintdef( 6, wvw_win->foreground ) );
      COLORREF oldBkColor   = SetBkColor( hDC, ( COLORREF ) hb_parnintdef( 7, wvw_win->background ) );
      HFONT    oldFont      = ( HFONT ) SelectObject( hDC, wvw->a.hUserFonts[ iSlot ] );

      POINT xy;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, NULL, NULL );

      xy = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );

      ExtTextOut( hDC, xy.x, xy.y, 0, NULL, szText, ( UINT ) nLen, NULL );

      hb_strfree( hText );

      SelectObject( hDC, oldFont );
      SetTextAlign( hDC, oldTextAlign );
      SetBkColor( hDC, oldBkColor );
      SetTextColor( hDC, oldTextColor );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawLabelObj( [nWinNum], nTop, nLeft, nBottom, nRight, cLabel, nAlignHorz, nAlignVert, nTextColor, nBkColor, hFont, aOffset ) */
HB_FUNC( WVW_DRAWLABELOBJ )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 12, 1 );
      int iOffLeft   = hb_parvni( 12, 2 );
      int iOffBottom = hb_parvni( 12, 3 );
      int iOffRight  = hb_parvni( 12, 4 );

      POINT xy;
      int   x, y;
      RECT  rc;
      int   oldTextAlign, iAlignHorz, iAlignVert, iAlignH = 0, iAlignV;
      UINT  uiOptions;
      SIZE  sz;

      HDC hDC = wvw_win->hdc;

      COLORREF oldTextColor = SetTextColor( hDC, ( COLORREF ) hb_parnintdef( 9, wvw_win->foreground ) );
      COLORREF oldBkColor   = SetBkColor( hDC, ( COLORREF ) hb_parnintdef( 10, wvw_win->background ) );
      HFONT    oldFont      = ( HFONT ) SelectObject( hDC, ( HFONT ) HB_PARHANDLE( 11 ) );

      HB_SIZE nLen;
      void *  hText;
      LPCTSTR szText = HB_PARSTRDEF( 6, &hText, &nLen );

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + 1 + iOffBottom;
      iRight  = xy.x - 1 + 1 + iOffRight;

      iAlignHorz = hb_parni( 7 );
      iAlignVert = hb_parni( 8 );

      memset( &sz, 0, sizeof( sz ) );

      GetTextExtentPoint32( hDC, szText, ( int ) nLen, &sz );

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

      oldTextAlign = SetTextAlign( hDC, iAlignH | iAlignV );

      rc.top    = iTop;
      rc.left   = iLeft;
      rc.bottom = iBottom;
      rc.right  = iRight;

      uiOptions = ETO_CLIPPED | ETO_OPAQUE;

      ExtTextOut( hDC, x, y, uiOptions, &rc, szText, ( UINT ) nLen, NULL );

      hb_strfree( hText );

      SelectObject( hDC, oldFont );
      SetTextAlign( hDC, oldTextAlign );
      SetBkColor( hDC, oldBkColor );
      SetTextColor( hDC, oldTextColor );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawOutline( nWinNum, nTop, nLeft, nBottom, nRight, nThick, nShape, nRGBColor ) */
HB_FUNC( WVW_DRAWOUTLINE )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      HPEN  hPen = 0, hOldPen = 0;
      POINT xy;

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y - 1;
      iLeft = xy.x - 1;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing;
      iRight  = xy.x;

      if( HB_ISNUM( 6 ) )
      {
         hPen = CreatePen( hb_parni( 6 ), 0, ( COLORREF ) hb_parnint( 8 ) );
         if( hPen )
            hOldPen = ( HPEN ) SelectObject( hDC, hPen );
      }
      else
         /* hPen = NULL; */
         SelectObject( hDC, wvw->a.penBlack );

      hb_gt_wvw_DrawOutline( hDC, iTop, iLeft, iBottom, iRight );

      if( hPen )
      {
         SelectObject( hDC, hOldPen );
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
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iSlot = hb_parni( 6 ) - 1;

      POINT xy;

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y - 1;
      iLeft = xy.x - 1;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y;
      iRight  = xy.x;

      if( iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.hUserPens ) && wvw->a.hUserPens[ iSlot ] )
         SelectObject( hDC, wvw->a.hUserPens[ iSlot ] );
      else
         SelectObject( hDC, wvw->a.penBlack );

      hb_gt_wvw_DrawOutline( hDC, iTop, iLeft, iBottom, iRight );
   }
}

/*                1       2      3       4       5        6        7       8       9      10      11      12
   wvw_DrawLine( nWinNum, nTop, nLeft, nBottom, nRight, nOrient, nFormat, nAlign, nStyle, nThick, nColor, aOffset) */
HB_FUNC( WVW_DRAWLINE )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 12, 1 );
      int iOffLeft   = hb_parvni( 12, 2 );
      int iOffBottom = hb_parvni( 12, 3 );
      int iOffRight  = hb_parvni( 12, 4 );

      int   iOffset;
      int   x, y;
      POINT xy;
      HPEN  hPen, hOldPen;

      int iOrient = hb_parni( 6 );
      int iFormat = hb_parni( 7 );
      int iAlign  = hb_parni( 8 );
      int iStyle  = hb_parni( 9 );
      int iThick  = hb_parni( 10 );

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
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

      hPen    = CreatePen( iStyle, iThick, ( COLORREF ) hb_parnint( 11 ) );
      hOldPen = ( HPEN ) SelectObject( hDC, hPen );

      switch( iFormat )
      {
         case 0:               /* Raised */
            if( iOrient == 0 ) /* Horizontal */
            {
               SelectObject( hDC, wvw->a.penWhite );
               MoveToEx( hDC, x, y, NULL );
               LineTo( hDC, iRight, y );
               SelectObject( hDC, hPen );
               MoveToEx( hDC, x, y + 1, NULL );
               LineTo( hDC, iRight, y + 1 );
            }
            else  /* Vertical */
            {
               SelectObject( hDC, wvw->a.penWhite );
               MoveToEx( hDC, x, y, NULL );
               LineTo( hDC, x, iBottom );
               SelectObject( hDC, hPen );
               MoveToEx( hDC, x + 1, y, NULL );
               LineTo( hDC, x + 1, iBottom );
            }
            break;

         case 1:               /* Recessed */
            if( iOrient == 0 ) /* Horizontal */
            {
               SelectObject( hDC, hPen );
               MoveToEx( hDC, x, y, NULL );
               LineTo( hDC, iRight, y );
               SelectObject( hDC, wvw->a.penWhite );
               MoveToEx( hDC, x, y + 1, NULL );
               LineTo( hDC, iRight, y + 1 );
            }
            else  /* Vertical */
            {
               SelectObject( hDC, hPen );
               MoveToEx( hDC, x, y, NULL );
               LineTo( hDC, x, iBottom );
               SelectObject( hDC, wvw->a.penWhite );
               MoveToEx( hDC, x + 1, y, NULL );
               LineTo( hDC, x + 1, iBottom );
            }
            break;

         case 2:               /* Plain */
            if( iOrient == 0 ) /* Horizontal */
            {
               SelectObject( hDC, hPen );
               MoveToEx( hDC, x, y, NULL );
               LineTo( hDC, iRight, y );
            }
            else  /* Vertical */
            {
               SelectObject( hDC, hPen );
               MoveToEx( hDC, x, y, NULL );
               LineTo( hDC, x, iBottom );
            }
            break;
      }

      SelectObject( hDC, hOldPen );
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
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   int iSlot = hb_parni( 9 ) - 1;

   if( wvw && wvw_win && iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.hUserPens ) )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      HPEN hPen = wvw->a.hUserPens[ iSlot ];

      POINT xy;
      int   iOffset;
      int   x, y;

      int iOrient = hb_parni( 6 );
      int iFormat = hb_parni( 7 );
      int iAlign  = hb_parni( 8 );

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y;
      iLeft = xy.x;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - 1;
      iRight  = xy.x - 1;

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
               SelectObject( hDC, wvw->a.penWhite );
               MoveToEx( hDC, x, y, NULL );
               LineTo( hDC, iRight, y );
               SelectObject( hDC, hPen );
               MoveToEx( hDC, x, y + 1, NULL );
               LineTo( hDC, iRight, y + 1 );
            }
            else  /* Vertical */
            {
               SelectObject( hDC, wvw->a.penWhite );
               MoveToEx( hDC, x, y, NULL );
               LineTo( hDC, x, iBottom );
               SelectObject( hDC, hPen );
               MoveToEx( hDC, x + 1, y, NULL );
               LineTo( hDC, x + 1, iBottom );
            }
            break;

         case 1:               /* Recessed */
            if( iOrient == 0 ) /* Horizontal */
            {
               SelectObject( hDC, hPen );
               MoveToEx( hDC, x, y, NULL );
               LineTo( hDC, iRight, y );
               SelectObject( hDC, wvw->a.penWhite );
               MoveToEx( hDC, x, y + 1, NULL );
               LineTo( hDC, iRight, y + 1 );
            }
            else  /* Vertical */
            {
               SelectObject( hDC, hPen );
               MoveToEx( hDC, x, y, NULL );
               LineTo( hDC, x, iBottom );
               SelectObject( hDC, wvw->a.penWhite );
               MoveToEx( hDC, x + 1, y, NULL );
               LineTo( hDC, x + 1, iBottom );
            }
            break;

         case 2:               /* Plain */
            if( iOrient == 0 ) /* Horizontal */
            {
               SelectObject( hDC, hPen );
               MoveToEx( hDC, x, y, NULL );
               LineTo( hDC, iRight, y );
            }
            else  /* Vertical */
            {
               SelectObject( hDC, hPen );
               MoveToEx( hDC, x, y, NULL );
               LineTo( hDC, x, iBottom );
            }
            break;
      }

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* Inside the area requested! */
/* wvw_DrawEllipse( nWinNum, nTop, nLeft, nBottom, nRight, aOffset) */
HB_FUNC( WVW_DRAWELLIPSE )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      POINT xy;

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      SelectObject( hDC, wvw->a.currentBrush );
      SelectObject( hDC, wvw->a.currentPen );

      hb_retl( Ellipse( hDC, iLeft, iTop, iRight, iBottom ) );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawRectangle( nWinNum, nTop, nLeft, nBottom, nRight, aOffset, lUsaCurrentPen ) */
HB_FUNC( WVW_DRAWRECTANGLE )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      POINT xy;

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      SelectObject( hDC, wvw->a.currentBrush );
      SelectObject( hDC, hb_parldef( 7, HB_TRUE ) /* fUseCurrentPen */ ? wvw->a.currentPen : wvw->a.penBlack );

      hb_retl( Rectangle( hDC, iLeft, iTop, iRight, iBottom ) );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawRoundRect( nWinNum, nTop, nLeft, nBottom, nRight, ;
                      aOffset, ; <-- new parm
                      nRoundHeight, nRoundWidth */

/* WARNING!!!
   unlike previous release of GTWVW, 6th parameter is now aOffset
   This placement of new parameter is made in line with GTWVT's way of doing it */

HB_FUNC( WVW_DRAWROUNDRECT )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      POINT xy;

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      SelectObject( hDC, wvw->a.currentBrush );
      SelectObject( hDC, wvw->a.currentPen );

      hb_retl( RoundRect( hDC, iLeft, iTop, iRight, iBottom, hb_parni( 8 ), hb_parni( 7 ) ) );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawFocusRect( nWinNum, nTop, nLeft, nBottom, nRight, aOffset ) */
HB_FUNC( WVW_DRAWFOCUSRECT )
{
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      RECT  rc;
      POINT xy;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      rc.top  = xy.y + iOffTop;
      rc.left = xy.x + iOffLeft;

      xy        = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      rc.bottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
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
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_WIN wvw_zer = hb_gt_wvw_win( 0 );

   if( wvw && wvw_win && wvw_zer )
   {
      HBRUSH hBrush = CreateSolidBrush( ( COLORREF ) hb_parnint( 7 ) );

      if( hBrush )
      {
         int iTop    = hb_parni( 2 ),
             iLeft   = hb_parni( 3 ),
             iBottom = hb_parni( 4 ),
             iRight  = hb_parni( 5 );

         int iOffTop    = hb_parvni( 6, 1 );
         int iOffLeft   = hb_parvni( 6, 2 );
         int iOffBottom = hb_parvni( 6, 3 );
         int iOffRight  = hb_parvni( 6, 4 );

         POINT xy;
         RECT  rc;

         hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

         xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
         rc.top  = xy.y + iOffTop;
         rc.left = xy.x + iOffLeft;

         xy        = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
         rc.bottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
         rc.right  = xy.x - 1 + iOffRight;

         hb_retl( FillRect( wvw_win->hdc, &rc, hBrush ) );

         SelectObject( wvw_zer->hdc, wvw->a.OriginalBrush );
         DeleteObject( hBrush );
         return;
      }
   }

   hb_retl( HB_FALSE );
}

/* wvw_DrawGridHorz( nWinNum, nTop, nLeft, nRight, nRows ) */
HB_FUNC( WVW_DRAWGRIDHORZ )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iAtRow = hb_parni( 2 );
      int iLeft  = hb_parni( 3 );
      int iRight = hb_parni( 4 );

      int iRows = hb_parni( 5 );
      int i;

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iAtRow, &iLeft, NULL, &iRight );

      iLeft  = iLeft * wvw_win->PTEXTSIZE.x;
      iRight = ( ( iRight + 1 ) * wvw_win->PTEXTSIZE.x ) - 1;

      if( wvw->a.gridPen == NULL )
         wvw->a.gridPen = CreatePen( 0, 0, GetSysColor( COLOR_BTNFACE ) );

      SelectObject( hDC, wvw->a.gridPen );

      for( i = 0; i < iRows; i++ )
      {
         int y = ( iAtRow * hb_gt_wvw_LineHeight( wvw_win ) ) + wvw_win->iTBHeight;

         MoveToEx( hDC, iLeft, y, NULL );
         LineTo( hDC, iRight, y );

         iAtRow++;
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
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   int iTabs = hb_parni( 5 );

   if( wvw && wvw_win && iTabs > 0 )
   {
      int iTop    = hb_parni( 2 );
      int iBottom = hb_parni( 3 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );

      int i;

      int iCharWidth  = wvw_win->PTEXTSIZE.x;
      int iCharHeight = hb_gt_wvw_LineHeight( wvw_win );

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, NULL, &iBottom, NULL );

      iTop    = ( iTop * iCharHeight ) + wvw_win->iTBHeight + iOffTop;
      iBottom = ( ( iBottom + 1 ) * iCharHeight ) - 1 + wvw_win->iTBHeight + iOffBottom;

      if( wvw->a.gridPen == NULL )
         wvw->a.gridPen = CreatePen( 0, 0, GetSysColor( COLOR_BTNFACE ) );

      SelectObject( hDC, wvw->a.gridPen );

      for( i = 1; i <= iTabs; i++ )
      {
         int iCol = hb_parvni( 4, i );
         int x;

         if( hb_gt_wvw_GetMainCoordMode() )
            iCol -= wvw_win->iColOfs;

         x = ( iCol * iCharWidth ) + iOffLeft;

         MoveToEx( hDC, x, iTop, NULL );
         LineTo( hDC, x, iBottom );
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
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_WIN wvw_zer = hb_gt_wvw_win( 0 );

   if( wvw && wvw_win && wvw_zer )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      POINT    xy;
      RECT     rc;
      int      iTextHeight;
      LOGBRUSH lb;
      HBRUSH   hBrush;

      HB_BOOL fImage  = HB_ISNUM( 7 ) || HB_ISCHAR( 7 );
      int     iFormat = hb_parni( 8 );

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y;
      iLeft = xy.x;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1;
      iRight  = xy.x - 1;

      memset( &lb, 0, sizeof( lb ) );

      lb.lbStyle = BS_SOLID;
      lb.lbColor = ( COLORREF ) hb_parnintdef( 10, hb_gt_wvw_GetColorData( 7 ) );
      lb.lbHatch = 0;

      hBrush = CreateBrushIndirect( &lb );

      rc.left   = iLeft;
      rc.top    = iTop;
      rc.right  = iRight + 1;
      rc.bottom = iBottom + 1;

      FillRect( hDC, &rc, hBrush );

      SelectObject( wvw_zer->hdc, wvw->a.OriginalBrush );
      DeleteObject( hBrush );

      switch( iFormat )
      {
         case 1:
            hb_gt_wvw_DrawBoxRecessed( hDC, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1, HB_FALSE );
            break;
         case 2:
            break;
         case 3:
            hb_gt_wvw_DrawOutline( hDC, iTop, iLeft, iBottom, iRight );
            break;
         case 4:
            break;
         default:
            hb_gt_wvw_DrawBoxRaised( hDC, iTop + 1, iLeft + 1, iBottom - 1, iRight - 1, HB_FALSE );
      }

      if( HB_ISCHAR( 6 ) )
      {
         int      oldTextAlign = SetTextAlign( hDC, TA_CENTER | TA_TOP );
         int      oldBkMode    = SetBkMode( hDC, TRANSPARENT );
         COLORREF oldTextColor = SetTextColor( hDC, ( COLORREF ) hb_parnintdef(  9, hb_gt_wvw_GetColorData( 0 ) ) );

         HB_SIZE nLen;
         void *  hText;
         LPCTSTR szText = HB_PARSTRDEF( 6, &hText, &nLen );

         SIZE sz;

         SelectObject( hDC, GetStockObject( DEFAULT_GUI_FONT ) );

         memset( &sz, 0, sizeof( sz ) );

         GetTextExtentPoint32( hDC, szText, ( int ) nLen, &sz );

         iTextHeight = sz.cy;

         xy.x = iLeft + ( ( iRight - iLeft + 1 ) / 2 );

         if( fImage )
            xy.y = iBottom - 2 - iTextHeight;
         else
            xy.y = iTop + ( ( iBottom - iTop + 1 - iTextHeight ) / 2 );

         if( iFormat == 1 )
         {
            xy.x += 2;
            xy.y += 2;
         }

         ExtTextOut( hDC, xy.x, xy.y, 0, NULL, szText, ( UINT ) nLen, NULL );

         hb_strfree( hText );

         SetTextColor( hDC, oldTextColor );
         SetBkMode( hDC, oldBkMode );
         SetTextAlign( hDC, oldTextAlign );
      }
      else
         iTextHeight = -1;

      if( fImage )
      {
         int iImageWidth  = iRight - iLeft + 1 - 8;
         int iImageHeight = iBottom - iTop + 1 - 8 - iTextHeight;

         if( HB_ISNUM( 7 ) )
         {
            int iSlot = hb_parni( 7 ) - 1;

            if( iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.pPicture ) )
               hb_gt_wvw_RenderPicture( wvw_win, iLeft + 4, iTop + 4, iImageWidth, iImageHeight, wvw->a.pPicture[ iSlot ], HB_FALSE );
         }
         else
            hb_gt_wvw_DrawImage( wvw_win->hWnd, iLeft + 4, iTop + 4, iImageWidth, iImageHeight, hb_parcx( 7 ), HB_FALSE );
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
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop;
      int iLeft;
      int iBottom;
      int iRight;

      int   iPanels = hb_parni( 2 );
      int   i, iNext = 0;
      POINT xy;

      HDC hDC = wvw_win->hdc;

      for( i = 0; i < iPanels; i++ )
      {
         iTop    = hb_parvni( 3, iNext + 1 );
         iLeft   = hb_parvni( 3, iNext + 2 );
         iBottom = hb_parvni( 3, iNext + 3 );
         iRight  = hb_parvni( 3, iNext + 4 );

         hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

         xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
         iTop  = xy.y;
         iLeft = xy.x + 1;

         xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight, iBottom + 1 );
         iBottom = xy.y - wvw_win->iLineSpacing - 1;
         iRight  = xy.x - 2;

         SelectObject( hDC, wvw->a.penWhite );

         MoveToEx( hDC, iRight, iTop, NULL );    /* Right  */
         LineTo( hDC, iRight, iBottom );

         MoveToEx( hDC, iLeft, iBottom, NULL );  /* Bottom */
         LineTo( hDC, iRight, iBottom );

         SelectObject( hDC, wvw->a.penDarkGray );

         MoveToEx( hDC, iLeft, iTop, NULL );     /* Left   */
         LineTo( hDC, iLeft, iBottom );

         MoveToEx( hDC, iLeft, iTop, NULL );     /* Top    */
         LineTo( hDC, iRight, iTop );

         iNext += 4;
      }

      iTop  = hb_parvni( 3, ( 4 * iPanels ) - 1 );
      iLeft = hb_parvni( 3, 4 * iPanels );

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, NULL, NULL );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop + 1 );
      iTop  = iBottom = xy.y - wvw_win->iLineSpacing - 2;
      iLeft = iRight = xy.x - 2;

      SelectObject( hDC, wvw->a.penBlack );

      MoveToEx( hDC, iLeft - 4, iBottom, NULL );
      LineTo( hDC, iRight, iTop - 4 );
      MoveToEx( hDC, iLeft - 7, iBottom, NULL );
      LineTo( hDC, iRight, iTop - 7 );
      MoveToEx( hDC, iLeft - 10, iBottom, NULL );
      LineTo( hDC, iRight, iTop - 10 );

      SelectObject( hDC, wvw->a.penWhite );

      MoveToEx( hDC, iLeft - 5, iBottom, NULL );
      LineTo( hDC, iRight, iTop - 5 );
      MoveToEx( hDC, iLeft - 8, iBottom, NULL );
      LineTo( hDC, iRight, iTop - 8 );
      MoveToEx( hDC, iLeft - 11, iBottom, NULL );
      LineTo( hDC, iRight, iTop - 11 );
   }
}

/* wvw_DrawPicture( [nWinNum], nTop, nLeft, nBottom, nRight, nSlot, lTight/aAdj ) -> lOk */
/* nSlot <= 20  aAdj == { 0,0,-2,-2 } To Adjust the pixels for { Top,Left,Bottom,Right } */
HB_FUNC( WVW_DRAWPICTURE )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   int iSlot = hb_parni( 6 ) - 1;

   if( wvw && wvw_win && iSlot >= 0 && iSlot < ( int ) HB_SIZEOFARRAY( wvw->a.pPicture ) && wvw->a.pPicture[ iSlot ] )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffLeft, iOffTop, iOffRight, iOffBottom;

      POINT xy;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      if( hb_parl( 7 ) /* fTight */ )
      {
         iOffTop    = 2 + 1;
         iOffLeft   = 2 + 1;
         iOffBottom = -1;
         iOffRight  = -1;
      }
      else
      {
         iOffTop    = hb_parvni( 7, 1 );
         iOffLeft   = hb_parvni( 7, 2 );
         iOffBottom = hb_parvni( 7, 3 );
         iOffRight  = hb_parvni( 7, 4 );
      }

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      hb_retl( hb_gt_wvw_RenderPicture( wvw_win, iLeft, iTop, iRight - iLeft + 1, iBottom - iTop + 1, wvw->a.pPicture[ iSlot ], HB_FALSE ) );
   }
   else
      hb_retl( HB_FALSE );
}

/* wvw_DrawToolButtonState( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOff, nState ) */
HB_FUNC( WVW_DRAWTOOLBUTTONSTATE )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      POINT xy;
      int   iState = hb_parni( 7 );

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      switch( iState )
      {
         case 0:
            SelectObject( hDC, wvw->a.penGray );

            MoveToEx( hDC, iRight, iTop, NULL );    /* Right  */
            LineTo( hDC, iRight, iBottom + 1 );

            MoveToEx( hDC, iLeft, iBottom, NULL );  /* Bottom */
            LineTo( hDC, iRight, iBottom );

            MoveToEx( hDC, iLeft, iTop, NULL );     /* Left   */
            LineTo( hDC, iLeft, iBottom );

            MoveToEx( hDC, iLeft, iTop, NULL );     /* Top    */
            LineTo( hDC, iRight, iTop );
            break;

         case 1:
            SelectObject( hDC, wvw->a.penBlack );

            MoveToEx( hDC, iRight, iTop, NULL );    /* Right  */
            LineTo( hDC, iRight, iBottom + 1 );

            MoveToEx( hDC, iLeft, iBottom, NULL );  /* Bottom */
            LineTo( hDC, iRight, iBottom );

            SelectObject( hDC, wvw->a.penWhite );

            MoveToEx( hDC, iLeft, iTop, NULL );     /* Left   */
            LineTo( hDC, iLeft, iBottom );

            MoveToEx( hDC, iLeft, iTop, NULL );     /* Top    */
            LineTo( hDC, iRight, iTop );
            break;

         case 2:
            SelectObject( hDC, wvw->a.penWhite );

            MoveToEx( hDC, iRight, iTop, NULL );    /* Right  */
            LineTo( hDC, iRight, iBottom + 1 );

            MoveToEx( hDC, iLeft, iBottom, NULL );  /* Bottom */
            LineTo( hDC, iRight, iBottom );

            SelectObject( hDC, wvw->a.penBlack );

            MoveToEx( hDC, iLeft, iTop, NULL );     /* Left   */
            LineTo( hDC, iLeft, iBottom );

            MoveToEx( hDC, iLeft, iTop, NULL );     /* Top    */
            LineTo( hDC, iRight, iTop );
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
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      POINT Point[ 3 ];
      POINT xy;
      int   iHeight, iOff;

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      iOff = 6;

      iHeight = iBottom - iTop + 1;

      if( hb_parl( 8 ) /* fDepressed */ )
         hb_gt_wvw_DrawBoxRecessed( hDC, iTop + 1, iLeft + 1, iBottom - 2, iRight - 2, HB_FALSE );
      else
         hb_gt_wvw_DrawBoxRaised( hDC, iTop + 1, iLeft + 1, iBottom - 2, iRight - 2, HB_FALSE );
      SelectObject( hDC, wvw->a.solidBrush );

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

      Polygon( hDC, Point, HB_SIZEOFARRAY( Point ) );
   }
}

/* Wvw_DrawScrollbarThumbVert( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlScroll, nThumbPos ) */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be useful */
HB_FUNC( WVW_DRAWSCROLLTHUMBVERT )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      int iTabTop = hb_parni( 7 ), iTabLft, iTabBtm, iTabRgt;

      POINT xy;

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );
      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTabTop, NULL, NULL, NULL );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      SetBkColor( hDC, RGB( 230, 230, 230 ) );
      SelectObject( hDC, wvw->a.diagonalBrush );

      SelectObject( hDC, wvw->a.penNull );
      Rectangle( hDC, iLeft, iTop, iRight + 1, iBottom + 1 );

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft /* dummy */, iTabTop );
      iTabTop = xy.y;

      iTabLft = iLeft;
      iTabBtm = iTabTop + wvw_win->PTEXTSIZE.y - 1;
      iTabRgt = iRight;

      SelectObject( hDC, wvw->a.wvwWhiteBrush );
      SelectObject( hDC, wvw->a.penGray );
      Rectangle( hDC, iTabLft, iTabTop, iTabRgt + 1, iTabBtm );

      hb_gt_wvw_DrawBoxRaised( hDC, iTabTop + 1, iTabLft + 1, iTabBtm - 2, iTabRgt - 2, HB_FALSE );
   }
}

/* Wvw_DrawScrollbarThumbHorz( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffset, nThumbPos ) */
/* NOTE: with WVW_XB (scrollbar) this function does not seem to be useful */
HB_FUNC( WVW_DRAWSCROLLTHUMBHORZ )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      int iThumbLeft = hb_parni( 7 ), iThumbRight;

      POINT xy;

      HDC hDC = wvw_win->hdc;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );
      hb_gt_wvw_HBFUNCPrologue( wvw_win, NULL, &iThumbLeft, NULL, NULL );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      xy         = hb_gt_wvw_GetXYFromColRow( wvw_win, iThumbLeft, iTop /* dummy */ );
      iThumbLeft = xy.x;

      iThumbRight = iThumbLeft + ( wvw_win->PTEXTSIZE.x * 2 ) - 1;

      SetBkColor( hDC, RGB( 230, 230, 230 ) );
      SelectObject( hDC, wvw->a.diagonalBrush );
      SelectObject( hDC, wvw->a.penNull );
      Rectangle( hDC, iLeft, iTop, iRight + 1, iBottom + 1 );

      SelectObject( hDC, wvw->a.wvwWhiteBrush );
      SelectObject( hDC, wvw->a.penGray );
      Rectangle( hDC, iThumbLeft, iTop, iThumbRight, iBottom );

      hb_gt_wvw_DrawBoxRaised( hDC, iTop + 1, iThumbLeft + 1, iBottom - 2, iThumbRight - 2, HB_FALSE );
   }
}

/* wvw_DrawShadedRect( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlOffSet, nHorVert, aRGBb, aRGBe ) */
HB_FUNC( WVW_DRAWSHADEDRECT )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw && wvw_win )
   {
      HB_BOOL fResult = HB_FALSE;

      if( wvw->a.hMSImg32 )
      {
         int iTop    = hb_parni( 2 ),
             iLeft   = hb_parni( 3 ),
             iBottom = hb_parni( 4 ),
             iRight  = hb_parni( 5 );

         int iOffTop    = hb_parvni( 6, 1 );
         int iOffLeft   = hb_parvni( 6, 2 );
         int iOffBottom = hb_parvni( 6, 3 );
         int iOffRight  = hb_parvni( 6, 4 );

         TRIVERTEX     vert[ 2 ];
         GRADIENT_RECT gRect;

         int   iMode = hb_parnidef( 7, GRADIENT_FILL_RECT_H );
         POINT xy;

         hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

         xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
         iTop  = xy.y + iOffTop;
         iLeft = xy.x + iOffLeft;

         xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
         iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
         iRight  = xy.x - 1 + iOffRight;

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
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();

   if( wvw_win )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      POINT xy;
      RECT  rc;

      HDC hDC = wvw_win->hdc;

      int iAlignH = 0;

      int      oldTextAlign = SetTextAlign( hDC, TA_TOP | TA_LEFT | TA_NOUPDATECP );
      COLORREF oldTextColor = SetTextColor( hDC, ( COLORREF ) hb_parnintdef( 10, wvw_win->foreground ) );
      COLORREF oldBkColor   = SetBkColor( hDC, ( COLORREF ) hb_parnintdef( 11, wvw_win->background ) );
      int      oldBkMode    = SetBkMode( hDC, hb_parnidef( 12, OPAQUE ) );
      HFONT    oldFont      = ( HFONT ) SelectObject( hDC, ( HFONT ) HB_PARHANDLE( 13 ) );

      HB_SIZE nLen;
      void *  hText;
      LPCTSTR szText = HB_PARSTRDEF( 7, &hText, &nLen );

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

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

      DrawText( hDC, szText, ( int ) nLen, &rc, iAlignH | DT_WORDBREAK | DT_TOP );

      hb_strfree( hText );

      SetTextColor( hDC, oldTextColor );
      SetBkColor( hDC, oldBkColor );
      SetBkMode( hDC, oldBkMode );
      SetTextAlign( hDC, oldTextAlign );
      SelectObject( hDC, oldFont );
   }
}

/* wvw_DrawProgressBar( [nWinNum], nTop, nLeft, nBottom, nRight, aPxlTLBR, nPercent, ;
                        nBackColor, nBarColor, cImage, lVertical, nDirection ) */
HB_FUNC( WVW_DRAWPROGRESSBAR )
{
   PWVW_GLO wvw     = hb_gt_wvw();
   PWVW_WIN wvw_win = hb_gt_wvw_win_par();
   PWVW_WIN wvw_zer = hb_gt_wvw_win( 0 );

   if( wvw && wvw_win && wvw_zer )
   {
      int iTop    = hb_parni( 2 ),
          iLeft   = hb_parni( 3 ),
          iBottom = hb_parni( 4 ),
          iRight  = hb_parni( 5 );

      int iOffTop    = hb_parvni( 6, 1 );
      int iOffLeft   = hb_parvni( 6, 2 );
      int iOffBottom = hb_parvni( 6, 3 );
      int iOffRight  = hb_parvni( 6, 4 );

      int iPercent   = hb_parni( 7 );
      int iDirection = hb_parni( 12 );

      RECT  rc;
      POINT xy;

      hb_gt_wvw_HBFUNCPrologue( wvw_win, &iTop, &iLeft, &iBottom, &iRight );

      xy    = hb_gt_wvw_GetXYFromColRow( wvw_win, iLeft, iTop );
      iTop  = xy.y + iOffTop;
      iLeft = xy.x + iOffLeft;

      xy      = hb_gt_wvw_GetXYFromColRow( wvw_win, iRight + 1, iBottom + 1 );
      iBottom = xy.y - wvw_win->iLineSpacing - 1 + iOffBottom;
      iRight  = xy.x - 1 + iOffRight;

      if( hb_parl( 11 ) /* fVertical */ )
      {
         if( iDirection == 0 )
         {
            rc.top    = iTop;
            rc.left   = iLeft;
            rc.bottom = iTop + ( ( iBottom - iTop ) * iPercent / 100 );
            rc.right  = iRight;
         }
         else
         {
            rc.top    = iBottom - ( ( iBottom - iTop ) * iPercent / 100 );
            rc.left   = iLeft;
            rc.bottom = iBottom;
            rc.right  = iRight;
         }
      }
      else
      {
         if( iDirection == 0 )
         {
            rc.top    = iTop;
            rc.left   = iLeft;
            rc.bottom = iBottom;
            rc.right  = iLeft + ( ( iRight - iLeft ) * iPercent / 100 );
         }
         else
         {
            rc.top    = iTop;
            rc.left   = iRight - ( ( iRight - iLeft ) * iPercent / 100 );
            rc.bottom = iBottom;
            rc.right  = iRight;
         }
      }

      if( HB_ISCHAR( 10 ) )
         hb_gt_wvw_DrawImage( wvw_win->hWnd, rc.left, rc.top, rc.right - rc.left + 1, rc.bottom - rc.top + 1, hb_parc( 10 ), HB_FALSE );
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
