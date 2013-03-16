/*
 * Harbour Project source code:
 * Printing subsystem for Windows using GUI printing
 *
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software and Systems Ltd
 * www - http://harbour-project.org
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

#include "hbwin.h"
#include "hbwapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"

#if defined( HB_OS_WIN_CE )
   /* For Arc() */
   #include "hbwince.h"

   #if defined( __POCC__ )
      #ifndef FONTENUMPROC
      #define FONTENUMPROC  FONTENUMPROCW
      #endif
   #endif
#else
   #include <winspool.h>
#endif

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
         else if( ExtTextOut( hDC, iRow, iCol, 0, NULL, lpData, ( UINT ) nLen, NULL ) )
         {
            GetTextExtentPoint32( hDC, lpData, ( int ) nLen, &sSize ); /* Get the length of the text in device size */
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
#if ! defined( HB_OS_WIN_CE )
   HDC hDC = hbwapi_par_HDC( 1 );

   hb_retni( hDC && HB_ISNUM( 2 ) ? SetMapMode( hDC, hb_parni( 2 ) ) : 0 );
#else
   hb_retni( 0 );
#endif
}

HB_FUNC( WIN_CREATEFONT )
{
   HDC hDC = hbwapi_par_HDC( 1 );

   if( hDC )
   {
      LOGFONT lf;
      HFONT hFont;
      int iHeight;
      int iWidth;
      int iWeight = hb_parni( 6 );

      void * hfFaceName;
      LPCTSTR pfFaceName;
      HB_SIZE nLen;

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

      lf.lfHeight         = ( LONG ) iHeight;
      lf.lfWidth          = ( LONG ) iWidth;
      lf.lfEscapement     = 0;
      lf.lfOrientation    = 0;
      lf.lfWeight         = ( LONG ) iWeight;
      lf.lfItalic         = ( BYTE ) hb_parl( 8 );
      lf.lfUnderline      = ( BYTE ) hb_parl( 7 );
      lf.lfStrikeOut      = ( BYTE ) 0;
      lf.lfCharSet        = ( BYTE ) hb_parnl( 9 );
#if defined( HB_OS_WIN_CE )
      lf.lfOutPrecision   = ( BYTE ) OUT_DEFAULT_PRECIS;
#else
      lf.lfOutPrecision   = ( BYTE ) OUT_DEVICE_PRECIS;
#endif
      lf.lfClipPrecision  = ( BYTE ) CLIP_DEFAULT_PRECIS;
      lf.lfQuality        = ( BYTE ) DRAFT_QUALITY;
      lf.lfPitchAndFamily = ( BYTE ) DEFAULT_PITCH | FF_DONTCARE;

      pfFaceName = HB_PARSTR( 2, &hfFaceName, &nLen );

      if( nLen > ( LF_FACESIZE - 1 ) )
         nLen = LF_FACESIZE - 1;

      memcpy( lf.lfFaceName, pfFaceName, nLen * sizeof( TCHAR ) );
      lf.lfFaceName[ nLen ] = TEXT( '\0' );

      hb_strfree( hfFaceName );

      hFont = CreateFontIndirect( &lf );

      hbwapi_ret_HFONT( hFont );

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

#if ! defined( HB_OS_WIN_CE )
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
#endif

   hb_retl( bResult );
}

HB_FUNC( WIN_GETDOCUMENTPROPERTIES )
{
   HB_BOOL bResult = HB_FALSE;

#if ! defined( HB_OS_WIN_CE )
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
#endif

   hb_retl( bResult );
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

   return 1;
}

HB_FUNC( WIN_ENUMFONTS )
{
   HDC hDC = hbwapi_par_HDC( 1 );
   HB_BOOL fNullDC = ( ! hDC );
   PHB_ITEM pArray = hb_itemArrayNew( 0 );

   if( fNullDC )
      hDC = GetDC( NULL );

   EnumFonts( hDC, ( LPCTSTR ) NULL, ( FONTENUMPROC ) FontEnumCallBack, ( LPARAM ) pArray );

   if( fNullDC )
      ReleaseDC( NULL, hDC );

   hb_itemReturnRelease( pArray );
}

HB_FUNC( WIN_ENUMFONTFAMILIES )
{
   PHB_ITEM pArray = hb_itemArrayNew( 0 );
#if ! defined( HB_OS_WIN_CE )
   HDC hDC = hbwapi_par_HDC( 1 );
   HB_BOOL fNullDC = ( ! hDC );
   LOGFONT Logfont;

   memset( &Logfont, 0, sizeof( Logfont ) );

   Logfont.lfCharSet = ( BYTE ) hb_parnidef( 1, DEFAULT_CHARSET );
   if( HB_ISCHAR( 2 ) )
   {
      void * hText;
      HB_STRNCPY( Logfont.lfFaceName, HB_PARSTR( 2, &hText, NULL ), HB_SIZEOFARRAY( Logfont.lfFaceName ) - 1 );
      hb_strfree( hText );
   }

   if( fNullDC )
      hDC = GetDC( NULL );

   EnumFontFamiliesEx( hDC, &Logfont, ( FONTENUMPROC ) FontEnumCallBack, ( LPARAM ) pArray, 0 );

   if( fNullDC )
      ReleaseDC( NULL, hDC );
#endif

   hb_itemReturnRelease( pArray );
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

      rct.left   = hb_parnl( 2 );
      rct.top    = hb_parnl( 3 );
      rct.right  = hb_parnl( 4 );
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
   HDC hDC     = hbwapi_par_HDC( 1 );
   int x1      = hb_parni( 2 );
   int y1      = hb_parni( 3 );
   int x2      = hb_parni( 4 );
   int y2      = hb_parni( 5 );
   int iWidth  = hb_parni( 6 );
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
