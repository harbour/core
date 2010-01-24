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
   WIN_PRN() was designed to make it easy to emulate Clipper Dot Matrix printing.
   Dot Matrix printing was in CPI ( Characters per inch & Lines per inch ).
   Even though "Mapping Mode" for WIN_PRN() is WIN_MM_TEXT, ::SetFont() accepts the
   nWidth parameter in CPI not Pixels. Also the default ::LineHeight is for
   6 lines per inch so ::NewLine() works as per "LineFeed" on Dot Matrix printers.
   If you do not like this then inherit from the class and override anything you want

   Simple example

   TODO: Colour printing
         etc....

   Peter Rees 21 January 2004 <peter@rees.co.nz>
*/

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbwapi.h"
#include "hbwinuni.h"

#if ! defined( HB_OS_WIN_CE )

HB_FUNC( WIN_CREATEDC )
{
   if( HB_ISCHAR( 1 ) )
   {
      void * hDevice;

      HDC hDC = CreateDC( TEXT( "" ),
                          HB_PARSTR( 1, &hDevice, NULL ),
                          NULL,
                          NULL );

      hbwapi_ret_HDC( hDC );

      hb_strfree( hDevice );
   }
   else
      hb_retptr( NULL );
}

HB_FUNC( WIN_STARTDOC )
{
   HDC hDC = hbwapi_par_HDC( 1 );
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
   HDC hDC = hbwapi_par_HDC( 1 );

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
   HDC hDC = hbwapi_par_HDC( 1 );

   hb_retl( hDC && ( AbortDoc( hDC ) > 0 ) );
}

/* Compatibility dummy */
HB_FUNC( WIN_DELETEDC )
{
   hb_retni( 0 );
}

HB_FUNC( WIN_STARTPAGE )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   hb_retl( hDC && StartPage( hDC ) > 0 );
}

HB_FUNC( WIN_ENDPAGE )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   hb_retl( hDC && EndPage( hDC ) > 0 );
}

HB_FUNC( WIN_TEXTOUT )
{
   long lResult = 0;
   HDC hDC = hbwapi_par_HDC( 1 );

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

         if( HB_ISNUM( 7 ) )
            SetTextAlign( ( HDC ) hDC, TA_NOUPDATECP | hb_parni( 7 ) );

         if( iWidth < 0 && nLen < 1024 )
         {
            int n = ( int ) nLen, aFixed[ 1024 ];

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
   HDC hDC = hbwapi_par_HDC( 1 );

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
   HDC hDC = hbwapi_par_HDC( 1 );

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
   HDC hDC = hbwapi_par_HDC( 1 );

   hb_retni( hDC && HB_ISNUM( 2 ) ? ( long ) GetDeviceCaps( hDC, hb_parni( 2 ) ) : 0 );
}

HB_FUNC( WIN_SETMAPMODE )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   hb_retni( hDC && HB_ISNUM( 2 ) ? SetMapMode( hDC, hb_parni( 2 ) ) : 0 );
}

HB_FUNC( WIN_CREATEFONT )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
   {
      HFONT hFont;
      void * hFontFace;
      int iHeight;
      int iWidth;
      int iWeight = hb_parni( 6 );

      iWeight = iWeight > 0 ? iWeight : FW_NORMAL;

      if( hb_parl( 10 ) ) /* Ugly hack to enable full control for caller */
      {
         iHeight = hb_parni( 3 );
         iWidth = hb_parni( 5 );
      }
      else
      {
         int iMul = hb_parni( 4 );
         int iDiv = hb_parni( 5 );

         iHeight = -MulDiv( hb_parni( 3 ), GetDeviceCaps( hDC, LOGPIXELSY ), 72 );

         if( iDiv )
            iWidth = MulDiv( abs( iMul ), GetDeviceCaps( hDC, LOGPIXELSX ), abs( iDiv ) );
         else
            iWidth = 0;  /* Use the default font width */
      }

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

      hbwapi_ret_HFONT( hFont );

      hb_strfree( hFontFace );

      if( hFont )
         SelectObject( hDC, hFont );
   }
   else
      hb_retptr( NULL );
}

HB_FUNC( WIN_GETPRINTERFONTNAME )
{
   HDC hDC = hbwapi_par_HDC( 1 );

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
   HDC hDC = hbwapi_par_HDC( 1 );

   hb_retl( hDC && ( GetDeviceCaps( hDC, RASTERCAPS ) & RC_STRETCHDIB ) );
}

HB_FUNC( WIN_SETDOCUMENTPROPERTIES )
{
   HB_BOOL bResult = HB_FALSE;
   HDC hDC = hbwapi_par_HDC( 1 );

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

            if( DocumentProperties( 0, hPrinter, ( LPTSTR ) lpDeviceName, pDevMode, pDevMode, DM_OUT_BUFFER ) == IDOK )
            {
               DWORD dmFields = 0, fMode;
               HB_BOOL fUserDialog;
               int iProp, iProp2;

               fUserDialog = HB_ISBYREF( 3 ) || HB_ISBYREF( 4 ) ||
                             HB_ISBYREF( 5 ) || HB_ISBYREF( 6 ) ||
                             HB_ISBYREF( 7 ) || HB_ISBYREF( 8 ) ||
                             HB_ISBYREF( 9 ) || HB_ISBYREF( 10 );

               if( ( iProp = hb_parni( 3 ) ) != 0 )      /* [2007-02-22] don't change if 0 */
               {
                  pDevMode->dmPaperSize = ( short ) iProp;
                  dmFields |= DM_PAPERSIZE;
               }

               if( HB_ISLOG( 4 ) )
               {
                  pDevMode->dmOrientation = ( short ) ( hb_parl( 4 ) ? DMORIENT_LANDSCAPE : DMORIENT_PORTRAIT );
                  dmFields |= DM_ORIENTATION;
               }

               if( ( iProp = hb_parni( 5 ) ) > 0 )
               {
                  pDevMode->dmCopies = ( short ) iProp;
                  dmFields |= DM_COPIES;
               }

               if( ( iProp = hb_parni( 6 ) ) != 0 )      /* [2007-02-22] don't change if 0 */
               {
                  pDevMode->dmDefaultSource = ( short ) iProp;
                  dmFields |= DM_DEFAULTSOURCE;
               }

               if( ( iProp = hb_parni( 7 ) ) != 0 )      /* [2007-02-22] don't change if 0 */
               {
                  pDevMode->dmDuplex = ( short ) iProp;
                  dmFields |= DM_DUPLEX;
               }

               if( ( iProp = hb_parni( 8 ) ) != 0 )      /* [2007-02-22] don't change if 0 */
               {
                  pDevMode->dmPrintQuality = ( short ) iProp;
                  dmFields |= DM_PRINTQUALITY;
               }

               if( pDevMode->dmPaperSize == DMPAPER_USER &&
                   ( iProp = hb_parni( 9 ) ) > 0 &&
                   ( iProp2 = hb_parni( 10 ) ) > 0 )
               {
                  pDevMode->dmPaperLength = ( short ) iProp;
                  pDevMode->dmPaperWidth = ( short ) iProp2;
                  dmFields |= DM_PAPERLENGTH | DM_PAPERWIDTH;
               }

               pDevMode->dmFields = dmFields;

               fMode = DM_IN_BUFFER | DM_OUT_BUFFER;
               if( fUserDialog )
                  fMode |= DM_IN_PROMPT;

               if( DocumentProperties( 0, hPrinter, ( LPTSTR ) lpDeviceName, pDevMode, pDevMode, fMode ) == IDOK )
               {
                  hb_storni( pDevMode->dmPaperSize, 3 );
                  hb_storl( pDevMode->dmOrientation == DMORIENT_LANDSCAPE, 4 );
                  hb_storni( pDevMode->dmCopies, 5 );
                  hb_storni( pDevMode->dmDefaultSource, 6 );
                  hb_storni( pDevMode->dmDuplex, 7 );
                  hb_storni( pDevMode->dmPrintQuality, 8 );
                  hb_storni( pDevMode->dmPaperLength, 9 );
                  hb_storni( pDevMode->dmPaperWidth, 10 );

                  bResult = ( ResetDC( hDC, pDevMode ) != NULL );
               }
            }

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

         if( DocumentProperties( 0, hPrinter, ( LPTSTR ) lpDeviceName, pDevMode, pDevMode, DM_OUT_BUFFER ) == IDOK )
         {
            hb_storni( pDevMode->dmPaperSize, 2 );
            hb_storl( pDevMode->dmOrientation == DMORIENT_LANDSCAPE, 3 );
            hb_storni( pDevMode->dmCopies, 4 );
            hb_storni( pDevMode->dmDefaultSource, 5 );
            hb_storni( pDevMode->dmDuplex, 6 );
            hb_storni( pDevMode->dmPrintQuality, 7 );
            hb_storni( pDevMode->dmPaperLength, 8 );
            hb_storni( pDevMode->dmPaperWidth, 9 );
            bResult = HB_TRUE;
         }

         hb_xfree( pDevMode );
      }

      ClosePrinter( hPrinter );
   }

   hb_strfree( hDeviceName );

   hb_retl( bResult );
}

/* Functions for loading & printing bitmaps */

#define HB_WIN_BITMAP_UNKNOWN       0
#define HB_WIN_BITMAP_BMP           1
#define HB_WIN_BITMAP_JPEG          2
#define HB_WIN_BITMAP_PNG           3

static int hbwin_BitmapType( const void * pImgBuf )
{
   int iType = HB_WIN_BITMAP_UNKNOWN;

   if( pImgBuf )
   {
      if( memcmp( pImgBuf, "BM", 2 ) == 0 )
         iType = HB_WIN_BITMAP_BMP;
      else if( memcmp( pImgBuf, "\377\330\377", 3 ) == 0 )
         iType = HB_WIN_BITMAP_JPEG;
      else if( memcmp( pImgBuf, "\211PNG", 4 ) == 0 )
         iType = HB_WIN_BITMAP_PNG;
   }

   return iType;
}

HB_FUNC( WIN_BITMAPTYPE )
{
   hb_retni( hbwin_BitmapType( hb_parc( 1 ) ) );
}

HB_FUNC( WIN_LOADBITMAPFILE )
{
   HB_FHANDLE fhnd = hb_fsOpen( hb_parcx( 1 ), FO_READ | FO_SHARED );

   /* Set default return value */
   hb_retc_null();

   if( fhnd != FS_ERROR )
   {
      ULONG ulSize = hb_fsSeek( fhnd, 0, FS_END );

      /* TOFIX: No check is done on read data from disk which is a large security hole
                and may cause GPF even in simple error cases, like invalid file content.
                [vszakats] */
      if( ulSize > 2 && ulSize <= ( 32 * 1024 * 1024 ) )
      {
         void * pbmfh = hb_xgrab( ulSize );

         hb_fsSeek( fhnd, 0, FS_SET );

         if( hb_fsReadLarge( fhnd, pbmfh, ulSize ) == ulSize && hbwin_BitmapType( pbmfh ) != HB_WIN_BITMAP_UNKNOWN )
            hb_retclen_buffer( ( char * ) pbmfh, ( HB_SIZE ) ulSize );
         else
            hb_xfree( pbmfh );
      }

      hb_fsClose( fhnd );
   }
}

/* Some compilers don't implement these define [jarabal] */
#ifndef CHECKJPEGFORMAT
#define CHECKJPEGFORMAT    4119
#endif
#ifndef CHECKPNGFORMAT
#define CHECKPNGFORMAT     4120
#endif

static HB_BOOL hbwin_CheckPrnDrvFormat( HDC hDC, int iType, const void * pImgBuf, ULONG ulSize, PHB_ITEM pItmErrMsg )
{
   if( hDC && iType && pImgBuf && ulSize >= sizeof( BITMAPCOREHEADER ) )
   {
      if( iType == HB_WIN_BITMAP_BMP )
         return HB_TRUE;
      else
      {
         int iRes = iType = ( iType == HB_WIN_BITMAP_JPEG ? CHECKJPEGFORMAT : CHECKPNGFORMAT );

         iRes = ExtEscape( hDC, QUERYESCSUPPORT, sizeof(iRes), ( LPCSTR ) &iRes, 0, 0 );
         if( iRes > 0 )
         {
            if( ExtEscape( hDC, iType, ulSize, ( LPCSTR ) pImgBuf, sizeof(iRes), ( LPSTR ) &iRes ) > 0 )
            {
               if( pItmErrMsg && iRes != 1 )
                  hb_itemPutC( pItmErrMsg, "CHECKFORMAT failure" );

               return iRes == 1;
            }
            else
            {
               if( pItmErrMsg )
                  hb_itemPutC( pItmErrMsg, "Invalid source devmode for ESCSUPPORT" );

               return HB_FALSE;
            }
         }
         else
         {
            if( pItmErrMsg )
               hb_itemPutC( pItmErrMsg, "QUERYESCSUPPORT Not Implemented" );

            return HB_FALSE;
         }
      }
   }
   else
      return HB_FALSE;
}

HB_FUNC( WIN_CHECKPRNDRVFORMAT )
{
   const char * pImgBuf = hb_parc( 2 );

   hb_retl( hbwin_CheckPrnDrvFormat( hbwapi_par_HDC( 1 ), hbwin_BitmapType( pImgBuf ), pImgBuf, hb_parclen( 2 ), hb_param( 3, HB_IT_BYREF ) ) );
}

HB_FUNC( WIN_DRAWBITMAP )
{
   BITMAPINFO * pbmi;
   BYTE * pBits = NULL;
   HDC hDC = hbwapi_par_HDC( 1 );
   ULONG ulSize = hb_parclen( 2 );
   BITMAPFILEHEADER * pbmfh = ( BITMAPFILEHEADER * ) hb_parc( 2 );
   int iType = hbwin_BitmapType( pbmfh );

   /* TOFIX: No check is done on 2nd parameter which is a large security hole
             and may cause GPF in simple error cases.
             [vszakats] */
   if( hbwin_CheckPrnDrvFormat( hDC, iType, pbmfh, ulSize, NULL ) )
   {
      int cxDib = hb_parni( 7 );
      int cyDib = hb_parni( 8 );

      if( iType == HB_WIN_BITMAP_BMP )
      {
         pbmi = ( BITMAPINFO * ) ( pbmfh + 1 );
         pBits = ( BYTE * ) pbmfh + pbmfh->bfOffBits;

         /* Remember there are 2 types of BitMap File */
         if( pbmi->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
         {
            cxDib = ( ( BITMAPCOREHEADER * ) pbmi )->bcWidth;
            cyDib = ( ( BITMAPCOREHEADER * ) pbmi )->bcHeight;
         }
         else
         {
            cxDib = pbmi->bmiHeader.biWidth;
            cyDib = abs( pbmi->bmiHeader.biHeight );
         }
      }
      else if( cxDib && cyDib )
      {
         BITMAPINFO bmi;

         memset( &bmi, 0, sizeof( bmi ) );
         bmi.bmiHeader.biSize        = sizeof( BITMAPINFO );
         bmi.bmiHeader.biWidth       = cxDib;
         bmi.bmiHeader.biHeight      = -cyDib; /* top-down image */
         bmi.bmiHeader.biPlanes      = 1;
         bmi.bmiHeader.biBitCount    = 0;
         bmi.bmiHeader.biCompression = ( iType == HB_WIN_BITMAP_JPEG ? BI_JPEG : BI_PNG );
         bmi.bmiHeader.biSizeImage   = ulSize;
         pbmi = &bmi;
         pBits = ( BYTE * ) pbmfh;
      }

      if( pbmi && pBits )
      {
         SetStretchBltMode( hDC, COLORONCOLOR );
         hb_retl( StretchDIBits( hDC, hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ),
                                 0, 0, cxDib, cyDib, pBits, pbmi,
                                 DIB_RGB_COLORS, SRCCOPY ) != ( int ) GDI_ERROR );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( WIN_BITMAPDIMENSIONS )
{
   BITMAPFILEHEADER * pbmfh = ( BITMAPFILEHEADER * ) hb_parc( 1 );

   if( hb_parclen( 1 ) >= sizeof( BITMAPCOREHEADER ) && hbwin_BitmapType( pbmfh ) == HB_WIN_BITMAP_BMP )
   {
      int cxDib, cyDib;
      BITMAPINFO * pbmi = ( BITMAPINFO * ) ( pbmfh + 1 );

      /* Remember there are 2 types of BitMap File */
      if( pbmi->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
      {
         cxDib = ( ( BITMAPCOREHEADER * ) pbmi )->bcWidth;
         cyDib = ( ( BITMAPCOREHEADER * ) pbmi )->bcHeight;
      }
      else
      {
         cxDib = pbmi->bmiHeader.biWidth;
         cyDib = abs( pbmi->bmiHeader.biHeight );
      }

      hb_storni( cxDib, 2 );
      hb_storni( cyDib, 3 );

      hb_retl( HB_TRUE );
   }
   else
   {
      hb_storni( 0, 2 );
      hb_storni( 0, 3 );

      hb_retl( HB_FALSE );
   }
}

static int CALLBACK FontEnumCallBack( LOGFONT * lplf, TEXTMETRIC * lpntm,
                                      DWORD dwFontType, LPVOID pArray )
{
   PHB_ITEM pSubItems = hb_itemArrayNew( 4 );

   HB_ARRAYSETSTR( pSubItems, 1, lplf->lfFaceName );
   hb_arraySetL( pSubItems, 2, ( lplf->lfPitchAndFamily & FIXED_PITCH ) != 0 );
   hb_arraySetL( pSubItems, 3, ( dwFontType & TRUETYPE_FONTTYPE ) != 0 );
   hb_arraySetNL( pSubItems, 4, lpntm->tmCharSet );
   hb_arrayAddForward( ( PHB_ITEM ) pArray, pSubItems );

   hb_itemRelease( pSubItems );

   return HB_TRUE;
}

HB_FUNC( WIN_ENUMFONTS )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
   {
      PHB_ITEM pArray = hb_itemArrayNew( 0 );

      EnumFonts( hDC, ( LPCTSTR ) NULL, ( FONTENUMPROC ) FontEnumCallBack, ( LPARAM ) pArray );

      hb_itemReturnRelease( pArray );
   }
   else
      hb_reta( 0 );
}

HB_FUNC( WIN_SETCOLOR )
{
   HDC hDC = hbwapi_par_HDC( 1 );

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
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
   {
      HPEN hPen;

      if( HB_ISPOINTER( 2 ) )
         hPen = hbwapi_par_HPEN( 2 );
      else
      {
         hPen = CreatePen( hb_parni( 2 ),                /* pen style */
                           hb_parni( 3 ),                /* pen width */
                           ( COLORREF ) hb_parnl( 4 ) ); /* pen color */

         hbwapi_ret_HPEN( hPen );
      }

      if( hPen )
         SelectObject( hDC, hPen );
   }
   else
      hb_retptr( NULL );
}

HB_FUNC( WIN_FILLRECT )
{
   HDC hDC = hbwapi_par_HDC( 1 );
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
   HDC hDC = hbwapi_par_HDC( 1 );

   hb_retl( hDC ? MoveToEx( hDC, hb_parni( 2 ) /* x1 */,
                                 hb_parni( 3 ) /* y1 */, NULL ) &&
                  LineTo( hDC, hb_parni( 4 ) /* x2 */,
                               hb_parni( 5 ) /* y2 */ ) : HB_FALSE );
}

HB_FUNC( WIN_RECTANGLE )
{
   HDC hDC = hbwapi_par_HDC( 1 );
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
   HDC hDC = hbwapi_par_HDC( 1 );

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
   HDC hDC = hbwapi_par_HDC( 1 );

   hb_retl( hDC ? Ellipse( hDC /* hDC */,
                           hb_parni( 2 ) /* x1 */,
                           hb_parni( 3 ) /* y1 */,
                           hb_parni( 4 ) /* x2 */,
                           hb_parni( 5 ) /* y2 */ ) : HB_FALSE );
}

HB_FUNC( WIN_SETBKMODE )
{
   HDC hDC = hbwapi_par_HDC( 1 );
   int iMode = 0;

   if( hDC )
   {
      if( HB_ISNUM( 2 ) )
         iMode = SetBkMode( hDC, hb_parni( 2 ) );
      else
         iMode = GetBkMode( hDC );
   }
   hb_retni( iMode );
}

#endif
