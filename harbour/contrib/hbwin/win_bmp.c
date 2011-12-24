/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Printing subsystem for Windows using GUI printing
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.hu)
 * Copyright 2010 Xavi <jarabal/at/gmail.com>
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software & Systems Ltd
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

#include "hbwin.h"
#include "hbwapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"

#ifndef QUERYESCSUPPORT
#define QUERYESCSUPPORT 8
#endif
#ifndef BI_JPEG
#define BI_JPEG         4
#endif
#ifndef BI_PNG
#define BI_PNG          5
#endif

/* Functions for loading & printing bitmaps */

int hbwin_bitmapType( const void * pImgBuf, HB_SIZE size )
{
   int iType = HB_WIN_BITMAP_UNKNOWN;

   if( pImgBuf )
   {
      if(      size > 2 && memcmp( pImgBuf, "BM", 2 ) == 0 )
         iType = HB_WIN_BITMAP_BMP;
      else if( size > 3 && memcmp( pImgBuf, "\xFF\xD8\xFF", 3 ) == 0 )
         iType = HB_WIN_BITMAP_JPEG;
      else if( size > 4 && memcmp( pImgBuf, "\x89PNG", 4 ) == 0 )
         iType = HB_WIN_BITMAP_PNG;
   }

   return iType;
}

HB_FUNC( WIN_BITMAPTYPE )
{
   hb_retni( hbwin_bitmapType( hb_parc( 1 ), hb_parclen( 1 ) ) );
}

HB_FUNC( WIN_LOADBITMAPFILE )
{
   HB_FHANDLE fhnd = hb_fsOpen( hb_parcx( 1 ), FO_READ | FO_SHARED );

   /* Set default return value */
   hb_retc_null();

   if( fhnd != FS_ERROR )
   {
      HB_SIZE nSize = hb_fsSeek( fhnd, 0, FS_END );

      /* TOFIX: No check is done on read data from disk which is a large security hole
                and may cause GPF even in simple error cases, like invalid file content.
                [vszakats] */
      if( nSize > 2 && nSize <= ( 32 * 1024 * 1024 ) )
      {
         void * pbmfh = hb_xgrab( nSize + 1 );

         hb_fsSeek( fhnd, 0, FS_SET );

         if( hb_fsReadLarge( fhnd, pbmfh, nSize ) == nSize && hbwin_bitmapType( pbmfh, nSize ) != HB_WIN_BITMAP_UNKNOWN )
            hb_retclen_buffer( ( char * ) pbmfh, nSize );
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

static int hbwin_bitmapIsSupported( HDC hDC, int iType, const void * pImgBuf, HB_SIZE nSize )
{
   if( hDC &&
       iType != HB_WIN_BITMAP_UNKNOWN &&
       pImgBuf &&
       nSize >= sizeof( BITMAPCOREHEADER ) )
   {
      if( iType == HB_WIN_BITMAP_BMP )
         return 0;
      else
      {
         int iRes = iType = ( iType == HB_WIN_BITMAP_JPEG ? CHECKJPEGFORMAT : CHECKPNGFORMAT );

         iRes = ExtEscape( hDC, QUERYESCSUPPORT, sizeof( iRes ), ( LPCSTR ) &iRes, 0, 0 );
         if( iRes > 0 )
         {
            if( ExtEscape( hDC, iType, ( int ) nSize, ( LPCSTR ) pImgBuf, sizeof( iRes ), ( LPSTR ) &iRes ) > 0 )
            {
               if( iRes == 1 )
                  return 0;
               else
                  return -4;
            }
            else
               return -3;
         }
         else
            return -2;
      }
   }
   else
      return -1;
}

HB_FUNC( WIN_BITMAPISSUPPORTED )
{
   const char * pImgBuf = hb_parc( 2 );
   HB_SIZE nSize = hb_parclen( 2 );

   hb_retni( hbwin_bitmapIsSupported( hbwapi_par_HDC( 1 ), hbwin_bitmapType( pImgBuf, nSize ), pImgBuf, nSize ) );
}

HB_FUNC( WIN_DRAWBITMAP )
{
   BITMAPINFO * pbmi = NULL;
   BYTE * pBits = NULL;
   HDC hDC = hbwapi_par_HDC( 1 );
   HB_SIZE nSize = hb_parclen( 2 );
   BITMAPFILEHEADER * pbmfh = ( BITMAPFILEHEADER * ) hb_parc( 2 );
   int iType = hbwin_bitmapType( pbmfh, nSize );

   /* TOFIX: No check is done on 2nd parameter which is a large security hole
             and may cause GPF in simple error cases.
             [vszakats] */
   if( hbwin_bitmapIsSupported( hDC, iType, pbmfh, nSize ) == 0 )
   {
      int iWidth = hb_parni( 7 );
      int iHeight = hb_parni( 8 );

      if( iType == HB_WIN_BITMAP_BMP )
      {
         pbmi = ( BITMAPINFO * ) ( pbmfh + 1 );
         pBits = ( BYTE * ) pbmfh + pbmfh->bfOffBits;

         /* Remember there are 2 types of BitMap File */
         if( pbmi->bmiHeader.biSize == sizeof( BITMAPCOREHEADER ) )
         {
            iWidth = ( ( BITMAPCOREHEADER * ) pbmi )->bcWidth;
            iHeight = ( ( BITMAPCOREHEADER * ) pbmi )->bcHeight;
         }
         else
         {
            iWidth = pbmi->bmiHeader.biWidth;
            iHeight = abs( pbmi->bmiHeader.biHeight );
         }
      }
      else if( iWidth && iHeight )
      {
         BITMAPINFO bmi;

         memset( &bmi, 0, sizeof( bmi ) );
         bmi.bmiHeader.biSize        = sizeof( BITMAPINFO );
         bmi.bmiHeader.biWidth       = iWidth;
         bmi.bmiHeader.biHeight      = -iHeight; /* top-down image */
         bmi.bmiHeader.biPlanes      = 1;
         bmi.bmiHeader.biBitCount    = 0;
         bmi.bmiHeader.biCompression = ( iType == HB_WIN_BITMAP_JPEG ? BI_JPEG : BI_PNG );
         bmi.bmiHeader.biSizeImage   = ( DWORD ) nSize;
         pbmi = &bmi;
         pBits = ( BYTE * ) pbmfh;
      }

      if( pbmi && pBits )
      {
#if ! defined( HB_OS_WIN_CE )
         SetStretchBltMode( hDC, COLORONCOLOR );
#endif
         hb_retl( StretchDIBits( hDC, hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ),
                                 0, 0, iWidth, iHeight, pBits, pbmi,
                                 DIB_RGB_COLORS, SRCCOPY ) != ( int ) GDI_ERROR );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_retl( HB_FALSE );
}
