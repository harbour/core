/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Printing subsystem for Windows using GUI printing
 *
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software & Systems Ltd
 * www - http://www.harbour-project.org
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

/*

  TPRINT() was designed to make it easy to emulate Clipper Dot Matrix printing.
  Dot Matrix printing was in CPI ( Characters per inch & Lines per inch ).
  Even though "Mapping Mode" for TPRINT() is MM_TEXT, ::SetFont() accepts the
  nWidth parameter in CPI not Pixels. Also the default ::LineHeight is for
  6 lines per inch so ::NewLine() works as per "LineFeed" on Dot Matrix printers.
  If you do not like this then inherit from the class and override anything you want

  Simple example

  TO DO:    Colour printing
            etc....

  Peter Rees 21 January 2004 <peter@rees.co.nz>

*/

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbwinuni.h"

#if defined( HB_OS_WIN ) && !defined( HB_OS_WIN_CE )

#include <winspool.h>

#ifndef INVALID_FILE_SIZE
   #define INVALID_FILE_SIZE ( DWORD ) 0xFFFFFFFF
#endif

static HB_GARBAGE_FUNC( win_HDC_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      DeleteDC( ( HDC ) *ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gc_HDC_funcs =
{
   win_HDC_release,
   hb_gcDummyMark
};

static HDC win_HDC_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gc_HDC_funcs, iParam );

   return ph ? ( HDC ) * ph : ( HDC ) hb_parptr( iParam );
}

static HB_GARBAGE_FUNC( win_HPEN_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      DeleteObject( ( HPEN ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      * ph = NULL;
   }
}

static const HB_GC_FUNCS s_gc_HPEN_funcs =
{
   win_HPEN_release,
   hb_gcDummyMark
};

static HPEN win_HPEN_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gc_HPEN_funcs, iParam );

   return ph ? ( HPEN ) * ph : ( HPEN ) hb_parptr( iParam );
}

static HB_GARBAGE_FUNC( win_HFONT_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      DeleteObject( ( HFONT ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      * ph = NULL;
   }
}

static const HB_GC_FUNCS s_gc_HFONT_funcs =
{
   win_HFONT_release,
   hb_gcDummyMark
};

HB_FUNC( WIN_CREATEDC )
{
   if( HB_ISCHAR( 1 ) )
   {
      void * hDevice;

      HDC hDC = CreateDC( TEXT( "" ),
                          HB_PARSTR( 1, &hDevice, NULL ),
                          NULL,
                          NULL );

      if( hDC )
      {
         void ** ph = ( void ** ) hb_gcAllocate( sizeof( HDC * ), &s_gc_HDC_funcs );

         *ph = hDC;
         hb_retptrGC( ph );
      }
      else
         hb_retptr( NULL );

      hb_strfree( hDevice );
   }
   else
      hb_retptr( NULL );
}

HB_FUNC( WIN_STARTDOC )
{
   HDC hDC = win_HDC_par( 1 );
   DOCINFO sDoc;
   HB_BOOL bResult = HB_FALSE;

   if( hDC )
   {
      void * hDocName;

      sDoc.cbSize = sizeof( DOCINFO );
      sDoc.lpszDocName = HB_PARSTR( 2, &hDocName, NULL );
      sDoc.lpszOutput = NULL;
      sDoc.lpszDatatype = NULL;
      sDoc.fwType = 0;
      bResult = ( StartDoc( hDC, &sDoc ) > 0 );

      hb_strfree( hDocName );
   }

   hb_retl( bResult );
}

HB_FUNC( WIN_ENDDOC )
{
   HB_BOOL bResult = HB_FALSE;
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      if( hb_parl( 2 ) )
         bResult = ( AbortDoc( hDC ) > 0 );
      else
         bResult = ( EndDoc( hDC ) > 0 );
   }

   hb_retl( bResult );
}

HB_FUNC( WIN_ABORTDOC )
{
   HDC hDC = win_HDC_par( 1 );

   hb_retl( hDC && ( AbortDoc( hDC ) > 0 ) );
}

HB_FUNC( WIN_DELETEDC )
{
   win_HDC_release( hb_parptrGC( &s_gc_HDC_funcs, 1 ) );

   hb_retni( 0 );               /* Return zero as a new handle even if fails */
}

HB_FUNC( WIN_STARTPAGE )
{
   HDC hDC = win_HDC_par( 1 );

   hb_retl( hDC && StartPage( hDC ) > 0 );
}

HB_FUNC( WIN_ENDPAGE )
{
   HDC hDC = win_HDC_par( 1 );

   hb_retl( hDC && EndPage( hDC ) > 0 );
}

HB_FUNC( WIN_TEXTOUT )
{
   long lResult = 0;
   HDC hDC = win_HDC_par( 1 );

   if( hDC && HB_ISCHAR( 4 ) )
   {
      HB_SIZE nLen = hb_parnl( 5 );

      void * hData;
      HB_SIZE nDataLen;
      LPCTSTR lpData = HB_PARSTR( 4, &hData, &nDataLen );

      if( nLen > nDataLen )
         nLen = nDataLen;

      if( nLen > 0 )
      {
         SIZE sSize;

         int iRow = hb_parni( 2 );
         int iCol = hb_parni( 3 );
         int iWidth = hb_parni( 6 ); /* defaults to 0 */
         int iAlign = hb_parni( 7 ); /* defaults to 0 */

         if( iAlign == 1 )
            SetTextAlign( ( HDC ) hDC, TA_NOUPDATECP | TA_BOTTOM | TA_RIGHT );
         else if( iAlign == 2 )
            SetTextAlign( ( HDC ) hDC, TA_NOUPDATECP | TA_BOTTOM | TA_CENTER );
         else
            SetTextAlign( ( HDC ) hDC, TA_NOUPDATECP | TA_BOTTOM | TA_LEFT );

         if( iWidth < 0 && nLen < 1024 )
         {
            int n = nLen, aFixed[ 1024 ];

            iWidth = -iWidth;

            while( n )
               aFixed[ --n ] = iWidth;

            if( ExtTextOut( hDC, iRow, iCol, 0, NULL, lpData, ( UINT ) nLen, aFixed ) )
               lResult = ( long ) ( nLen * iWidth );
         }
         else if( TextOut( hDC, iRow, iCol, lpData, nLen ) )
         {
            GetTextExtentPoint32( hDC, lpData, nLen, &sSize ); /* Get the length of the text in device size */
            lResult = ( long ) sSize.cx; /* return the width so we can update the current pen position (::PosY) */
         }
      }

      hb_strfree( hData );
   }

   hb_retnl( lResult );
}

HB_FUNC( WIN_GETTEXTSIZE )
{
   long lResult = 0;
   HDC hDC = win_HDC_par( 1 );

   if( hDC && HB_ISCHAR( 2 ) )
   {
      HB_SIZE nLen = hb_parnl( 3 );

      void * hData;
      HB_SIZE nDataLen;
      LPCTSTR lpData = HB_PARSTR( 2, &hData, &nDataLen );

      if( nLen > nDataLen )
         nLen = nDataLen;

      if( nLen > 0 )
      {
         SIZE sSize;

         GetTextExtentPoint32( hDC, lpData, ( int ) nLen, &sSize );     /* Get the length of the text in device size */

         if( ! hb_parldef( 4, 1 ) )
            lResult = ( long ) sSize.cy;    /* return the height */
         else
            lResult = ( long ) sSize.cx;    /* return the width */
      }

      hb_strfree( hData );
   }

   hb_retnl( lResult );
}

HB_FUNC( WIN_GETCHARSIZE )
{
   long lResult = 0;
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      TEXTMETRIC tm;

      GetTextMetrics( hDC, &tm );
      if( hb_parl( 2 ) )
         lResult = ( long ) tm.tmHeight;
      else
         lResult = ( long ) tm.tmAveCharWidth;
   }

   hb_retnl( lResult );
}

HB_FUNC( WIN_GETDEVICECAPS )
{
   HDC hDC = win_HDC_par( 1 );

   hb_retnl( hDC && HB_ISNUM( 2 ) ? ( long ) GetDeviceCaps( hDC, hb_parni( 2 ) ) : 0 );
}

HB_FUNC( WIN_SETMAPMODE )
{
   HDC hDC = win_HDC_par( 1 );

   hb_retnl( hDC && HB_ISNUM( 2 ) ? SetMapMode( hDC, hb_parni( 2 ) ) : 0 );
}

HB_FUNC( WIN_MULDIV )
{
   hb_retnl( MulDiv( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

HB_FUNC( WIN_CREATEFONT )
{
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      HFONT hFont;
      void * hFontFace;
      int iHeight = hb_parni( 3 );
      int iMul = hb_parni( 4 );
      int iDiv = hb_parni( 5 );
      int iWidth;
      int iWeight = hb_parni( 6 );

      iWeight = iWeight > 0 ? iWeight : FW_NORMAL;
      iHeight = -MulDiv( iHeight, GetDeviceCaps( hDC, LOGPIXELSY ), 72 );
      if( iDiv )
         iWidth = MulDiv( abs( iMul ), GetDeviceCaps( hDC, LOGPIXELSX ), abs( iDiv ) );
      else
         iWidth = 0;  /* Use the default font width */

      hFont = CreateFont( iHeight,
                          iWidth,
                          0,
                          0,
                          iWeight,
                          ( DWORD ) hb_parl( 8 ) /* dwItalic */,
                          ( DWORD ) hb_parl( 7 ) /* dwUnderLine */,
                          0,
                          ( DWORD ) hb_parnl( 9 ) /* dwCharSet */,
                          OUT_DEVICE_PRECIS,
                          CLIP_DEFAULT_PRECIS,
                          DRAFT_QUALITY,
                          DEFAULT_PITCH | FF_DONTCARE,
                          HB_PARSTR( 2, &hFontFace, NULL ) );

      hb_strfree( hFontFace );

      if( hFont )
      {
         void ** ph = ( void ** ) hb_gcAllocate( sizeof( HFONT * ), &s_gc_HFONT_funcs );

         *ph = hFont;
         SelectObject( hDC, hFont );
         hb_retptrGC( ph );
      }
      else
         hb_retptr( NULL );
   }
   else
      hb_retptr( NULL );
}

HB_FUNC( WIN_GETPRINTERFONTNAME )
{
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      TCHAR tszFontName[ 128 ];

      GetTextFace( hDC, HB_SIZEOFARRAY( tszFontName ) - 1, tszFontName );

      HB_RETSTR( tszFontName );
   }
   else
      hb_retc_null();
}

HB_FUNC( WIN_BITMAPSOK )
{
   HDC hDC = win_HDC_par( 1 );

   hb_retl( hDC && ( GetDeviceCaps( hDC, RASTERCAPS ) & RC_STRETCHDIB ) );
}

HB_FUNC( WIN_SETDOCUMENTPROPERTIES )
{
   HB_BOOL bResult = HB_FALSE;
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      HANDLE hPrinter;
      void * hDeviceName;
      LPCTSTR lpDeviceName = HB_PARSTR( 2, &hDeviceName, NULL );

      if( OpenPrinter( ( LPTSTR ) lpDeviceName, &hPrinter, NULL ) )
      {
         LONG lSize = DocumentProperties( 0, hPrinter, ( LPTSTR ) lpDeviceName, NULL, NULL, 0 );

         if( lSize > 0 )
         {
            PDEVMODE pDevMode = ( PDEVMODE ) hb_xgrab( lSize );

            DocumentProperties( 0, hPrinter, ( LPTSTR ) lpDeviceName, pDevMode, pDevMode, DM_OUT_BUFFER );

            if( HB_ISNUM( 3 ) && hb_parni( 3 ) )        /* [2007-02-22] don't change if 0 */
               pDevMode->dmPaperSize = ( short ) hb_parni( 3 );

            if( HB_ISLOG( 4 ) )
               pDevMode->dmOrientation = ( short ) ( hb_parl( 4 ) ? 2 : 1 );

            if( HB_ISNUM( 5 ) && hb_parni( 5 ) > 0 )
               pDevMode->dmCopies = ( short ) hb_parni( 5 );

            if( HB_ISNUM( 6 ) && hb_parni( 6 ) )        /* [2007-02-22] don't change if 0 */
               pDevMode->dmDefaultSource = ( short ) hb_parni( 6 );

            if( HB_ISNUM( 7 ) && hb_parni( 7 ) )        /* [2007-02-22] don't change if 0 */
               pDevMode->dmDuplex = ( short ) hb_parni( 7 );

            if( HB_ISNUM( 8 ) && hb_parni( 8 ) )        /* [2007-02-22] don't change if 0 */
               pDevMode->dmPrintQuality = ( short ) hb_parni( 8 );

            bResult = ( ResetDC( hDC, pDevMode ) != NULL );

            hb_xfree( pDevMode );
         }

         ClosePrinter( hPrinter );
      }

      hb_strfree( hDeviceName );
   }

   hb_retl( bResult );
}

HB_FUNC( WIN_GETDOCUMENTPROPERTIES )
{
   HB_BOOL bResult = HB_FALSE;
   HANDLE hPrinter;
   void * hDeviceName;
   LPCTSTR lpDeviceName = HB_PARSTR( 1, &hDeviceName, NULL );

   if( OpenPrinter( ( LPTSTR ) lpDeviceName, &hPrinter, NULL ) )
   {
      LONG lSize = DocumentProperties( 0, hPrinter, ( LPTSTR ) lpDeviceName, NULL, NULL, 0 );

      if( lSize > 0 )
      {
         PDEVMODE pDevMode = ( PDEVMODE ) hb_xgrab( lSize );

         DocumentProperties( 0, hPrinter, ( LPTSTR ) lpDeviceName, pDevMode, pDevMode, DM_OUT_BUFFER );

         hb_storni( pDevMode->dmPaperSize, 2 );
         hb_storl( pDevMode->dmOrientation == 2, 3 );
         hb_storni( pDevMode->dmCopies, 4 );
         hb_storni( pDevMode->dmDefaultSource, 5 );
         hb_storni( pDevMode->dmDuplex, 6 );
         hb_storni( pDevMode->dmPrintQuality, 7 );
         bResult = HB_TRUE;

         hb_xfree( pDevMode );
      }

      ClosePrinter( hPrinter );
   }

   hb_strfree( hDeviceName );

   hb_retl( bResult );
}

/* Functions for loading & printing bitmaps */

HB_FUNC( WIN_LOADBITMAPFILE )
{
   HB_FHANDLE fhnd = hb_fsOpen( hb_parcx( 1 ), FO_READ | FO_SHARED );

   if( fhnd != FS_ERROR )
   {
      ULONG ulSize = hb_fsSeek( fhnd, 0, FS_END );

      if( ulSize > 2 && ulSize <= ( 32 * 1024 * 1024 ) )
      {
         BITMAPFILEHEADER * pbmfh = ( BITMAPFILEHEADER * ) hb_xgrab( ulSize );

         hb_fsSeek( fhnd, 0, FS_SET );

         if( hb_fsReadLarge( fhnd, pbmfh, ulSize ) == ulSize && pbmfh->bfType == *( WORD * ) "BM" )
            hb_retclen( ( char * ) pbmfh, ( HB_SIZE ) ulSize );

         hb_xfree( pbmfh );
      }
      else
         hb_retc_null();

      hb_fsClose( fhnd );
   }
   else
      hb_retc_null();
}

HB_FUNC( WIN_DRAWBITMAP )
{
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      BITMAPFILEHEADER * pbmfh = ( BITMAPFILEHEADER * ) hb_parc( 2 );
      BITMAPINFO * pbmi;
      BYTE * pBits;
      int cxDib, cyDib;

      pbmi = ( BITMAPINFO * ) ( pbmfh + 1 );
      pBits = ( BYTE * ) pbmfh + pbmfh->bfOffBits;

      if( pbmi->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      {                            /* Remember there are 2 types of BitMap File */
         cxDib = ( ( BITMAPCOREHEADER * ) pbmi )->bcWidth;
         cyDib = ( ( BITMAPCOREHEADER * ) pbmi )->bcHeight;
      }
      else
      {
         cxDib = pbmi->bmiHeader.biWidth;
         cyDib = abs( pbmi->bmiHeader.biHeight );
      }

      SetStretchBltMode( hDC, COLORONCOLOR );

      hb_retl( StretchDIBits( hDC, hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ),
                              0, 0, cxDib, cyDib, pBits, pbmi,
                              DIB_RGB_COLORS, SRCCOPY ) != ( int ) GDI_ERROR );
   }
   else
      hb_retl( HB_FALSE );
}

static int CALLBACK FontEnumCallBack( LOGFONT * lplf, TEXTMETRIC * lpntm, DWORD dwFontType,
                                      LPVOID pArray )
{
   PHB_ITEM pSubItems = hb_itemArrayNew( 4 );

   HB_ARRAYSETSTR( pSubItems, 1, lplf->lfFaceName );
   hb_arraySetL( pSubItems, 2, lplf->lfPitchAndFamily & FIXED_PITCH );
   hb_arraySetL( pSubItems, 3, dwFontType & TRUETYPE_FONTTYPE );
   hb_arraySetNL( pSubItems, 4, lpntm->tmCharSet );
   hb_arrayAddForward( ( PHB_ITEM ) pArray, pSubItems );

   hb_itemRelease( pSubItems );

   return HB_TRUE;
}

HB_FUNC( WIN_ENUMFONTS )
{
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      PHB_ITEM pArray = hb_itemArrayNew( 0 );

      EnumFonts( hDC, ( LPCTSTR ) NULL, ( FONTENUMPROC ) FontEnumCallBack, ( LPARAM ) pArray );

      hb_itemReturnRelease( pArray );
   }
}

HB_FUNC( WIN_SETCOLOR )
{
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      if( HB_ISNUM( 2 ) )
         hb_retnl( ( long ) SetTextColor( hDC, ( COLORREF ) hb_parnl( 2 ) ) );
      else
         hb_retnl( ( long ) GetTextColor( hDC ) );

      if( HB_ISNUM( 3 ) )
         SetBkColor( hDC, ( COLORREF ) hb_parnl( 3 ) );

      if( HB_ISNUM( 4 ) )
         SetTextAlign( hDC, hb_parni( 4 ) );
   }
   else
      hb_retnl( ( long ) CLR_INVALID );
}

HB_FUNC( WIN_SETPEN )
{
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      HPEN hPen;

      if( HB_ISPOINTER( 2 ) )
         hPen = win_HPEN_par( 1 );
      else
         hPen = CreatePen( hb_parni( 2 ),             /* pen style */
                           hb_parni( 3 ),             /* pen width */
                           ( COLORREF ) hb_parnl( 4 ) /* pen color */
                         );

      if( hPen )
      {
         void ** ph = ( void ** ) hb_gcAllocate( sizeof( HPEN * ), &s_gc_HPEN_funcs );

         *ph = hPen;
         SelectObject( hDC, hPen );
         hb_retptrGC( ph );
      }
      else
         hb_retptr( NULL );
   }
   else
      hb_retptr( NULL );
}

HB_FUNC( WIN_FILLRECT )
{
   HDC hDC = win_HDC_par( 1 );
   HB_BOOL fResult = HB_FALSE;

   if( hDC )
   {
      HBRUSH hBrush = CreateSolidBrush( ( COLORREF ) hb_parnl( 6 ) );
      RECT rct;

      rct.left = hb_parnl( 2 );
      rct.top = hb_parnl( 3 );
      rct.right = hb_parnl( 4 );
      rct.bottom = hb_parnl( 5 );

      if( FillRect( hDC, &rct, hBrush ) )
         fResult = HB_TRUE;

      DeleteObject( hBrush );
   }
   hb_retl( fResult );
}

HB_FUNC( WIN_LINETO )
{
   HDC hDC = win_HDC_par( 1 );

   hb_retl( hDC ? MoveToEx( hDC, hb_parni( 2 ) /* x1 */,
                                 hb_parni( 3 ) /* y1 */, NULL ) &&
                  LineTo( hDC, hb_parni( 4 ) /* x2 */,
                               hb_parni( 5 ) /* y2 */ ) : HB_FALSE );
}

HB_FUNC( WIN_RECTANGLE )
{
   HDC hDC = win_HDC_par( 1 );
   int x1 = hb_parni( 2 );
   int y1 = hb_parni( 3 );
   int x2 = hb_parni( 4 );
   int y2 = hb_parni( 5 );
   int iWidth = hb_parni( 6 );
   int iHeight = hb_parni( 7 );

   if( iWidth && iHeight )
      hb_retl( hDC ? RoundRect( hDC, x1, y1, x2, y2, iWidth, iHeight ) : HB_FALSE );
   else
      hb_retl( hDC ? Rectangle( hDC, x1, y1, x2, y2 ) : HB_FALSE );
}

HB_FUNC( WIN_ARC )
{
   HDC hDC = win_HDC_par( 1 );

   hb_retl( hDC ? Arc( hDC /* hDC */,
                       hb_parni( 2 ) /* x1 */,
                       hb_parni( 3 ) /* y1 */,
                       hb_parni( 4 ) /* x2 */,
                       hb_parni( 5 ) /* y2 */,
                       0,
                       0,
                       0,
                       0 ) : HB_FALSE );
}

HB_FUNC( WIN_ELLIPSE )
{
   HDC hDC = win_HDC_par( 1 );

   hb_retl( hDC ? Ellipse( hDC /* hDC */,
                           hb_parni( 2 ) /* x1 */,
                           hb_parni( 3 ) /* y1 */,
                           hb_parni( 4 ) /* x2 */,
                           hb_parni( 5 ) /* y2 */ ) : HB_FALSE );
}

HB_FUNC( WIN_SETBKMODE )
{
   HDC hDC = win_HDC_par( 1 );

   hb_retni( hDC ? SetBkMode( win_HDC_par( 1 ), hb_parni( 2 ) ) : 0 );
}

#endif
