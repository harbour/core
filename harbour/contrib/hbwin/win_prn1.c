/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Printing subsystem for Windows using GUI printing
 *     Copyright 2004 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 *
 * See COPYING for licensing terms.
 *
 * www - http://www.harbour-project.org
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
#include "hbapiitm.h"

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
      DeleteObject( ( HDC ) * ph );

      /* set pointer to NULL to avoid multiple freeing */
      * ph = NULL;
   }
}

static const HB_GC_FUNCS s_gc_HPEN_funcs =
{
   win_HPEN_release,
   hb_gcDummyMark
};

HB_FUNC( WIN_CREATEDC )
{
   if( HB_ISCHAR( 1 ) )
   {
      LPTSTR lpText = HB_TCHAR_CONVTO( hb_parc( 1 ) );
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( HDC * ), &s_gc_HDC_funcs );
      *ph = ( void * ) CreateDC( TEXT( "" ), lpText, NULL, NULL );
      hb_retptrGC( ph );
      HB_TCHAR_FREE( lpText );
   }
   else
      hb_retptr( NULL );
}

HB_FUNC( WIN_STARTDOC )
{
   HDC hDC = win_HDC_par( 1 );
   DOCINFO sDoc;
   BOOL Result = FALSE;

   if( hDC )
   {
      const char * szDocName = hb_parc( 2 );
      LPTSTR lpDocName = szDocName ? HB_TCHAR_CONVTO( szDocName ) : NULL;
      sDoc.cbSize = sizeof( DOCINFO );
      sDoc.lpszDocName = lpDocName;
      sDoc.lpszOutput = NULL;
      sDoc.lpszDatatype = NULL;
      sDoc.fwType = 0;
      Result = ( BOOL ) ( StartDoc( hDC, &sDoc ) > 0 );

      if( lpDocName )
         HB_TCHAR_FREE( lpDocName );
   }

   hb_retl( Result );
}

HB_FUNC( WIN_ENDDOC )
{
   BOOL Result = FALSE;
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      if( hb_parl( 2 ) )
         Result = ( AbortDoc( hDC ) > 0 );
      else
         Result = ( EndDoc( hDC ) > 0 );
   }

   hb_retl( Result );
}

HB_FUNC( WIN_ABORTDOC )
{
   HDC hDC = win_HDC_par( 1 );

   hb_retl( hDC && ( AbortDoc( hDC ) > 0 ) );
}

HB_FUNC( WIN_DELETEDC )
{
   void ** phDC = ( void ** ) hb_parptrGC( &s_gc_HDC_funcs, 1 );

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( phDC && * phDC )
   {
      /* Destroy the object */
      DeleteDC( ( HDC ) * phDC );

      /* set pointer to NULL to avoid multiple freeing */
      * phDC = NULL;
   }

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
   LONG Result = 0;
   HDC hDC = win_HDC_par( 1 );
   ULONG ulLen = hb_parclen( 4 );
   SIZE sSize;

   if( hDC && ulLen )
   {
      int iLen = hb_parni( 5 );

      if( iLen > ( int ) ulLen )
         iLen = ( int ) ulLen;

      if( iLen > 0 )
      {
         int iRow = hb_parni( 2 );
         int iCol = hb_parni( 3 );
         int iWidth = hb_parni( 6 ); /* defaults to 0 */
         LPTSTR lpData = HB_TCHAR_CONVNTO( hb_parc( 4 ), iLen );

         if( hb_parnl( 7 ) == 1 )
            SetTextAlign( ( HDC ) hDC, TA_NOUPDATECP | TA_BOTTOM | TA_RIGHT );
         else if( hb_parnl( 7 ) == 2 )
            SetTextAlign( ( HDC ) hDC, TA_NOUPDATECP | TA_BOTTOM | TA_CENTER );
         else
            SetTextAlign( ( HDC ) hDC, TA_NOUPDATECP | TA_BOTTOM | TA_LEFT );

         if( iWidth < 0 && iLen < 1024 )
         {
            int n = iLen, aFixed[1024];

            iWidth = -iWidth;

            while( n )
               aFixed[--n] = iWidth;

            if( ExtTextOut( hDC, iRow, iCol, 0, NULL, lpData, iLen, aFixed ) )
               Result = ( LONG ) ( iLen * iWidth );
         }
         else if( TextOut( hDC, iRow, iCol, lpData, iLen ) )
         {
            GetTextExtentPoint32( hDC, lpData, iLen, &sSize ); /* Get the length of the text in device size */
            Result = ( LONG ) sSize.cx; /* return the width so we can update the current pen position (::PosY) */
         }

         HB_TCHAR_FREE( lpData );
      }
   }

   hb_retnl( Result );
}

HB_FUNC( WIN_GETTEXTSIZE )
{
   LONG Result = 0;
   HDC hDC = win_HDC_par( 1 );
   ULONG ulLen = hb_parclen( 2 );

   if( hDC && ulLen )
   {
      int iLen = hb_parni( 3 );
      LPTSTR lpData;
      SIZE sSize;

      if( ( ULONG ) iLen > ulLen )
         iLen = ulLen;

      lpData = HB_TCHAR_CONVNTO( hb_parc( 2 ), iLen );

      GetTextExtentPoint32( hDC, lpData, iLen, &sSize );       /* Get the length of the text in device size */

      if( !hb_parldef( 4, 1 ) )
         Result = ( LONG ) sSize.cy;    /* return the height */
      else
         Result = ( LONG ) sSize.cx;    /* return the width */

      HB_TCHAR_FREE( lpData );
   }

   hb_retnl( Result );
}


HB_FUNC( WIN_GETCHARSIZE )
{
   LONG Result = 0;
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      TEXTMETRIC tm;

      GetTextMetrics( hDC, &tm );
      if( hb_parl( 2 ) )
         Result = ( LONG ) tm.tmHeight;
      else
         Result = ( LONG ) tm.tmAveCharWidth;
   }

   hb_retnl( Result );
}

HB_FUNC( WIN_GETDEVICECAPS )
{
   HDC hDC = win_HDC_par( 1 );

   hb_retnl( hDC && HB_ISNUM( 2 ) ? ( long ) GetDeviceCaps( hDC, hb_parnl( 2 ) ) : 0 );
}

HB_FUNC( WIN_SETMAPMODE )
{
   HDC hDC = win_HDC_par( 1 );

   hb_retnl( hDC && HB_ISNUM( 2 ) ? SetMapMode( hDC, hb_parnl( 2 ) ) : 0 );
}

HB_FUNC( WIN_MULDIV )
{
   hb_retnl( MulDiv( hb_parnl( 1 ), hb_parnl( 2 ), hb_parnl( 3 ) ) );
}

HB_FUNC( WIN_CREATEFONT )
{
   BOOL Result = FALSE;
   HDC hDC = win_HDC_par( 1 );
   HFONT hFont;
   const char * pszFont = hb_parc( 2 );
   LPTSTR lpFont = pszFont ? HB_TCHAR_CONVTO( pszFont ) : NULL;
   int iHeight = hb_parni( 3 );
   int iMul = hb_parni( 4 );
   int iDiv = hb_parni( 5 );
   int iWidth;
   int iWeight = hb_parni( 6 );
   DWORD dwUnderLine = ( DWORD ) hb_parl( 7 );
   DWORD dwItalic = ( DWORD ) hb_parl( 8 );
   DWORD dwCharSet = ( DWORD ) hb_parnl( 9 );

   iWeight = iWeight > 0 ? iWeight : FW_NORMAL;
   iHeight = -MulDiv( iHeight, GetDeviceCaps( hDC, LOGPIXELSY ), 72 );
   if( iDiv )
      iWidth = MulDiv( abs( iMul ), GetDeviceCaps( hDC, LOGPIXELSX ), abs( iDiv ) );
   else
      iWidth = 0;               /* Use the default font width */

   hFont = CreateFont( iHeight, iWidth, 0, 0, iWeight, dwItalic, dwUnderLine, 0,
                       dwCharSet, OUT_DEVICE_PRECIS, CLIP_DEFAULT_PRECIS, DRAFT_QUALITY,
                       DEFAULT_PITCH | FF_DONTCARE, lpFont );
   if( lpFont )
      HB_TCHAR_FREE( lpFont );

   if( hFont )
   {
      HFONT hOldFont = ( HFONT ) SelectObject( hDC, hFont );

      if( hOldFont )
         DeleteObject( hOldFont );

      Result = TRUE;
   }

   hb_retl( Result );
}

HB_FUNC( WIN_GETPRINTERFONTNAME )
{
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      unsigned char cFont[ 128 ];

      GetTextFace( hDC, sizeof( cFont ) - 1, ( LPTSTR ) cFont );

      hb_retc( ( char * ) cFont );
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
   BOOL Result = FALSE;
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      HANDLE hPrinter;
      const char * pszPrinterName = hb_parc( 2 );
      LPTSTR lpPrinterName = pszPrinterName ? HB_TCHAR_CONVTO( pszPrinterName ) : NULL;

      if( OpenPrinter( lpPrinterName, &hPrinter, NULL ) )
      {
         PDEVMODE pDevMode = NULL;
         LONG lSize = DocumentProperties( 0, hPrinter, lpPrinterName, pDevMode, pDevMode, 0 );

         if( lSize > 0 )
         {
            pDevMode = ( PDEVMODE ) hb_xgrab( lSize );

            if( pDevMode )
            {
               DocumentProperties( 0, hPrinter, lpPrinterName, pDevMode, pDevMode, DM_OUT_BUFFER );

               if( HB_ISNUM( 3 ) && hb_parnl( 3 ) )        /* 22/02/2007 don't change if 0 */
                  pDevMode->dmPaperSize = ( short ) hb_parnl( 3 );

               if( HB_ISLOG( 4 ) )
                  pDevMode->dmOrientation = ( short ) ( hb_parl( 4 ) ? 2 : 1 );

               if( HB_ISNUM( 5 ) && hb_parnl( 5 ) > 0 )
                  pDevMode->dmCopies = ( short ) hb_parnl( 5 );

               if( HB_ISNUM( 6 ) && hb_parnl( 6 ) )        /* 22/02/2007 don't change if 0 */
                  pDevMode->dmDefaultSource = ( short ) hb_parnl( 6 );

               if( HB_ISNUM( 7 ) && hb_parnl( 7 ) )        /* 22/02/2007 don't change if 0 */
                  pDevMode->dmDuplex = ( short ) hb_parnl( 7 );

               if( HB_ISNUM( 8 ) && hb_parnl( 8 ) )        /* 22/02/2007 don't change if 0 */
                  pDevMode->dmPrintQuality = ( short ) hb_parnl( 8 );

               Result = ( ResetDC( hDC, pDevMode ) != NULL );

               hb_xfree( pDevMode );
            }
         }

         ClosePrinter( hPrinter );
      }

      if( lpPrinterName )
         HB_TCHAR_FREE( lpPrinterName );
   }

   hb_retl( Result );
}

/* Functions for Loading & Printing bitmaps */

HB_FUNC( WIN_LOADBITMAPFILE )
{
   LPTSTR lpFileName = HB_TCHAR_CONVTO( hb_parcx( 1 ) );
   BOOL bSuccess = FALSE;
   DWORD dwFileSize = 0, dwHighSize, dwBytesRead;
   HANDLE hFile;
   BITMAPFILEHEADER * pbmfh = NULL;

   hFile = CreateFile( ( LPCTSTR ) lpFileName, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING,
                       FILE_FLAG_SEQUENTIAL_SCAN, NULL );

   HB_TCHAR_FREE( lpFileName );

   if( hFile != INVALID_HANDLE_VALUE )
   {
      dwFileSize = GetFileSize( hFile, &dwHighSize );

      if( ( dwFileSize != INVALID_FILE_SIZE ) && ! dwHighSize )  /* Do not continue if File size error or TOO big for memory */
      {
         pbmfh = ( BITMAPFILEHEADER * ) hb_xgrab( dwFileSize );

         if( pbmfh )
         {
            bSuccess = ReadFile( hFile, pbmfh, dwFileSize, &dwBytesRead, NULL );
            bSuccess = bSuccess && ( dwBytesRead == dwFileSize ) && ( pbmfh->bfType == *( WORD * ) "BM" );      /*&& (pbmfh->bfSize == dwFileSize) ;*/
         }
      }

      CloseHandle( hFile );
   }

   if( bSuccess )
   {
      hb_retclen( ( char * ) pbmfh, dwFileSize );

      if( pbmfh )
         hb_xfree( pbmfh );
   }
   else
   {
      hb_retc_null();

      if( pbmfh )
         hb_xfree( pbmfh );
   }
}

HB_FUNC( WIN_DRAWBITMAP )
{
   HDC hDC = win_HDC_par( 1 );
   BITMAPFILEHEADER *pbmfh = ( BITMAPFILEHEADER * ) hb_parc( 2 );
   BITMAPINFO *pbmi;
   BYTE *pBits;
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

static int CALLBACK FontEnumCallBack( LOGFONT * lplf, TEXTMETRIC * lpntm, DWORD FontType,
                                      LPVOID pArray )
{
   PHB_ITEM SubItems = hb_itemNew( NULL );
   char * pszFaceName = HB_TCHAR_CONVFROM( lplf->lfFaceName );

   hb_arrayNew( SubItems, 4 );
   hb_itemPutC( hb_arrayGetItemPtr( SubItems, 1 ), pszFaceName );
   hb_itemPutL( hb_arrayGetItemPtr( SubItems, 2 ), lplf->lfPitchAndFamily & FIXED_PITCH );
   hb_itemPutL( hb_arrayGetItemPtr( SubItems, 3 ), FontType & TRUETYPE_FONTTYPE );
   hb_itemPutNL( hb_arrayGetItemPtr( SubItems, 4 ), lpntm->tmCharSet );
   hb_arrayAddForward( ( PHB_ITEM ) pArray, SubItems );
   hb_itemRelease( SubItems );
   HB_TCHAR_FREE( pszFaceName );

   return TRUE;
}

HB_FUNC( WIN_ENUMFONTS )
{
   HDC hDC = win_HDC_par( 1 );

   if( hDC )
   {
      PHB_ITEM pArray = hb_itemNew( NULL );

      hb_arrayNew( pArray, 0 );

      EnumFonts( hDC, ( LPCTSTR ) NULL, ( FONTENUMPROC ) FontEnumCallBack, ( LPARAM ) pArray );

      hb_itemReturnRelease( pArray );
   }
}

HB_FUNC( WIN_GETEXEFILENAME )
{
   unsigned char pBuf[ 1024 ];

   GetModuleFileName( NULL, ( LPTSTR ) pBuf, 1023 );

   hb_retc( ( char * ) pBuf );
}

HB_FUNC( WIN_SETCOLOR )
{
   HDC hDC = win_HDC_par( 1 );

   SetTextColor( hDC, ( COLORREF ) hb_parnl( 2 ) );

   if( HB_ISNUM( 3 ) )
      SetBkColor( hDC, ( COLORREF ) hb_parnl( 3 ) );

   if( HB_ISNUM( 4 ) )
      SetTextAlign( hDC, hb_parni( 4 ) );
}

HB_FUNC( WIN_SETPEN )
{
   HDC hDC = win_HDC_par( 1 );
   HPEN hOldPen;

   void ** ph = ( void ** ) hb_gcAllocate( sizeof( HPEN * ), &s_gc_HPEN_funcs );

   * ph = ( void * ) CreatePen( hb_parni( 2 ),                /* pen style */
                                hb_parni( 3 ),                /* pen width */
                                ( COLORREF ) hb_parnl( 4 )    /* pen color */
                              );

   hOldPen = ( HPEN ) SelectObject( hDC, ( HPEN ) * ph );

   if( hOldPen )
      DeleteObject( hOldPen );

   hb_retptrGC( ph );
}

HB_FUNC( WIN_FILLRECT )
{
   HDC hDC = win_HDC_par( 1 );
   int x1 = hb_parni( 2 );
   int y1 = hb_parni( 3 );
   int x2 = hb_parni( 4 );
   int y2 = hb_parni( 5 );
   HBRUSH hBrush = CreateSolidBrush( ( COLORREF ) hb_parnl( 6 ) );
   RECT rct;

   rct.top = y1;
   rct.left = x1;
   rct.bottom = y2;
   rct.right = x2;

   FillRect( hDC, &rct, hBrush );

   DeleteObject( hBrush );
}

HB_FUNC( WIN_LINETO )
{
   HDC hDC = win_HDC_par( 1 );
   int x1 = hb_parni( 2 );
   int y1 = hb_parni( 3 );
   int x2 = hb_parni( 4 );
   int y2 = hb_parni( 5 );

   MoveToEx( hDC, x1, y1, NULL );

   hb_retl( LineTo( hDC, x2, y2 ) );
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
      hb_retl( RoundRect( hDC, x1, y1, x2, y2, iWidth, iHeight ) );
   else
      hb_retl( Rectangle( hDC, x1, y1, x2, y2 ) );
}

HB_FUNC( WIN_ARC )
{
   HDC hDC = win_HDC_par( 1 );
   int x1 = hb_parni( 2 );
   int y1 = hb_parni( 3 );
   int x2 = hb_parni( 4 );
   int y2 = hb_parni( 5 );

   hb_retl( Arc( hDC, x1, y1, x2, y2, 0, 0, 0, 0 ) );
}

HB_FUNC( WIN_ELLIPSE )
{
   HDC hDC = win_HDC_par( 1 );
   int x1 = hb_parni( 2 );
   int y1 = hb_parni( 3 );
   int x2 = hb_parni( 4 );
   int y2 = hb_parni( 5 );

   hb_retl( Ellipse( hDC, x1, y1, x2, y2 ) );
}

HB_FUNC( WIN_SETBKMODE )
{
   hb_retnl( SetBkMode( win_HDC_par( 1 ), hb_parnl( 2 ) ) );
}

#endif
