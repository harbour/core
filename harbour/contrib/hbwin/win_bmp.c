/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Printing subsystem for Windows using GUI printing
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2010 Xavi <jarabal/at/gmail.com>
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

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbwapi.h"
#include "hbwinuni.h"

#if defined( HB_HAS_PNG ) && defined( HB_HAS_ZLIB )
   #include "png.h"
#endif

#if ! defined( HB_OS_WIN_CE )

/* Functions for loading & printing bitmaps */

#define HB_WIN_BITMAP_UNKNOWN       0
#define HB_WIN_BITMAP_BMP           1
#define HB_WIN_BITMAP_JPEG          2
#define HB_WIN_BITMAP_PNG           3

static int hbwin_BitmapType( const void * pImgBuf, HB_SIZE size )
{
   int iType = HB_WIN_BITMAP_UNKNOWN;

   if( pImgBuf )
   {
      if(      size > 2 && memcmp( pImgBuf, "BM", 2 ) == 0 )
         iType = HB_WIN_BITMAP_BMP;
      else if( size > 3 && memcmp( pImgBuf, "\377\330\377", 3 ) == 0 )
         iType = HB_WIN_BITMAP_JPEG;
      else if( size > 4 && memcmp( pImgBuf, "\211PNG", 4 ) == 0 )
         iType = HB_WIN_BITMAP_PNG;
   }

   return iType;
}

HB_FUNC( WIN_BITMAPTYPE )
{
   hb_retni( hbwin_BitmapType( hb_parc( 1 ), hb_parclen( 1 ) ) );
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

         if( hb_fsReadLarge( fhnd, pbmfh, ulSize ) == ulSize && hbwin_BitmapType( pbmfh, ulSize ) != HB_WIN_BITMAP_UNKNOWN )
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

static int hbwin_BitmapIsSupported( HDC hDC, int iType, const void * pImgBuf, HB_SIZE nSize )
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
            if( ExtEscape( hDC, iType, nSize, ( LPCSTR ) pImgBuf, sizeof( iRes ), ( LPSTR ) &iRes ) > 0 )
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

   hb_retni( hbwin_BitmapIsSupported( hbwapi_par_HDC( 1 ), hbwin_BitmapType( pImgBuf, nSize ), pImgBuf, nSize ) );
}

HB_FUNC( WIN_DRAWBITMAP )
{
   BITMAPINFO * pbmi = NULL;
   BYTE * pBits = NULL;
   HDC hDC = hbwapi_par_HDC( 1 );
   HB_SIZE nSize = hb_parclen( 2 );
   BITMAPFILEHEADER * pbmfh = ( BITMAPFILEHEADER * ) hb_parc( 2 );
   int iType = hbwin_BitmapType( pbmfh, nSize );

   /* TOFIX: No check is done on 2nd parameter which is a large security hole
             and may cause GPF in simple error cases.
             [vszakats] */
   if( hbwin_BitmapIsSupported( hDC, iType, pbmfh, nSize ) == 0 )
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
         SetStretchBltMode( hDC, COLORONCOLOR );
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

/* .jpeg size detection code. [vszakats] */

#define _JPEG_RET_OK                0
#define _JPEG_RET_OVERRUN           1
#define _JPEG_RET_INVALID           2
#define _JPEG_RET_UNSUPPORTED       3

#define _JPEG_CS_GRAY               1
#define _JPEG_CS_RGB                2
#define _JPEG_CS_CMYK               3

static int hb_jpeg_get_param( const HB_BYTE * buffer, HB_SIZE nBufferSize, int * piHeight, int * piWidth, int * piColorSpace, int * piBPC )
{
   HB_SIZE nPos = 0;

   HB_U16 tag;
   HB_U16 height = 0;
   HB_U16 width = 0;
   HB_BYTE colorspace = 0;
   HB_BYTE bpc = 0;

   if( piHeight )
      *piHeight = 0;
   if( piWidth )
      *piWidth = 0;
   if( piColorSpace )
      *piColorSpace = 0;
   if( piBPC )
      *piBPC = 0;

   if( nPos >= nBufferSize )
      return _JPEG_RET_OVERRUN;

   tag = HB_SWAP_UINT16( ( HB_U16 ) HB_GET_LE_UINT16( buffer + nPos ) ); nPos += 2;

   /* SOI marker */
   if( tag != 0xFFD8 )
      return _JPEG_RET_INVALID;

   for( ;; )
   {
      HB_U16 size;

      if( nPos >= nBufferSize )
         return _JPEG_RET_OVERRUN;

      tag = HB_SWAP_UINT16( ( HB_U16 ) HB_GET_LE_UINT16( buffer + nPos ) ); nPos += 2;

      if( nPos >= nBufferSize )
         return _JPEG_RET_OVERRUN;

      size = HB_SWAP_UINT16( ( HB_U16 ) HB_GET_LE_UINT16( buffer + nPos ) ); nPos += 2;

      /* SOF markers */
      if( tag == 0xFFC0 ||
          tag == 0xFFC1 ||
          tag == 0xFFC2 ||
          tag == 0xFFC9 )
      {
         if( nPos >= nBufferSize )
            return _JPEG_RET_OVERRUN;

         colorspace = *( buffer + nPos ); nPos += 1;

         if( nPos >= nBufferSize )
            return _JPEG_RET_OVERRUN;

         height = HB_SWAP_UINT16( ( HB_U16 ) HB_GET_LE_UINT16( buffer + nPos ) ); nPos += 2;

         if( nPos >= nBufferSize )
            return _JPEG_RET_OVERRUN;

         width = HB_SWAP_UINT16( ( HB_U16 ) HB_GET_LE_UINT16( buffer + nPos ) ); nPos += 2;

         if( nPos >= nBufferSize )
            return _JPEG_RET_OVERRUN;

         bpc = *( buffer + nPos ); nPos += 1;

         break;
      }
      else if( ( tag | 0x00FF ) != 0xFFFF ) /* lost marker */
         return _JPEG_RET_UNSUPPORTED;

      nPos += size - 2;

      if( nPos >= nBufferSize )
         return _JPEG_RET_OVERRUN;
   }

   if( piHeight )
      *piHeight = ( int ) height;
   if( piWidth )
      *piWidth = ( int ) width;
   if( piBPC )
      *piBPC = ( int ) bpc;
   if( piColorSpace )
   {
      switch( colorspace )
      {
         case 1: *piColorSpace = _JPEG_CS_GRAY; break;
         case 3: *piColorSpace = _JPEG_CS_RGB; break;
         case 4: *piColorSpace = _JPEG_CS_CMYK; break;
      }
   }

   return _JPEG_RET_OK;
}

/* .png size detection code. [vszakats] */

#if defined( HB_HAS_PNG ) && defined( HB_HAS_ZLIB )

#define _PNG_RET_OK                 0
#define _PNG_RET_ERR_INVALID1       1
#define _PNG_RET_ERR_INVALID2       2
#define _PNG_RET_ERR_INIT1          3
#define _PNG_RET_ERR_INIT2          4
#define _PNG_RET_ERR_INIT3          5
#define _PNG_RET_ERR_READ           6

typedef struct
{
   const HB_BYTE * buffer;
   HB_SIZE nLen;
   HB_SIZE nPos;
   HB_BOOL bOk;
} HB_PNG_READ;

static void hb_png_read_func( png_structp png_ptr, png_bytep data, png_uint_32 length )
{
   HB_PNG_READ * hb_png_read_data = ( HB_PNG_READ * ) png_get_io_ptr( png_ptr );
   png_uint_32 pos;

   for( pos = 0; pos < length && hb_png_read_data->nPos < hb_png_read_data->nLen; )
      data[ pos++ ] = hb_png_read_data->buffer[ hb_png_read_data->nPos++ ];

   hb_png_read_data->bOk = ( length == pos );
}

static int hb_png_get_param( const HB_BYTE * buffer, HB_SIZE nBufferSize, int * piHeight, int * piWidth, int * piColorSpace, int * piBPC )
{
   png_structp png_ptr;
   png_infop info_ptr;
   png_byte header[ 8 ];

   HB_PNG_READ hb_png_read_data;
   int iResult;

   if( piHeight )
      *piHeight = 0;
   if( piWidth )
      *piWidth = 0;
   if( piColorSpace )
      *piColorSpace = 0;
   if( piBPC )
      *piBPC = 0;

   if( nBufferSize < sizeof( header ) )
      return _PNG_RET_ERR_INVALID1;

   memcpy( header, buffer, sizeof( header ) );

   if( png_sig_cmp( header, ( png_size_t ) 0, sizeof( header ) ) )
      return _PNG_RET_ERR_INVALID2;

   png_ptr = png_create_read_struct( PNG_LIBPNG_VER_STRING, NULL, NULL, NULL );
   if( ! png_ptr )
      return _PNG_RET_ERR_INIT1;

   info_ptr = png_create_info_struct( png_ptr );
   if( ! info_ptr )
   {
      png_destroy_read_struct( &png_ptr, ( png_infopp ) NULL, ( png_infopp ) NULL );
      return _PNG_RET_ERR_INIT2;
   }

   hb_png_read_data.buffer = buffer;
   hb_png_read_data.nLen = nBufferSize;
   hb_png_read_data.nPos = sizeof( header );
   hb_png_read_data.bOk = HB_TRUE;

   png_set_sig_bytes( png_ptr, sizeof( header ) );
   png_set_read_fn( png_ptr, ( void * ) &hb_png_read_data, ( png_rw_ptr ) &hb_png_read_func );

   png_read_info( png_ptr, info_ptr );

   if( hb_png_read_data.bOk )
   {
      png_uint_32 width;
      png_uint_32 height;
      int bit_depth;
      int color_type;

      png_get_IHDR( png_ptr, info_ptr, &width, &height, &bit_depth, &color_type, NULL, NULL, NULL );

      if( piHeight )
         *piHeight = ( int ) height;
      if( piWidth )
         *piWidth = ( int ) width;
      if( piBPC )
         *piBPC = bit_depth;
      if( piColorSpace )
         *piColorSpace = color_type;

      iResult = _PNG_RET_OK;
   }
   else
      iResult = _PNG_RET_ERR_READ;

   png_destroy_read_struct( &png_ptr, &info_ptr, ( png_infopp ) NULL );

   return iResult;
}

#endif

HB_FUNC( WIN_BITMAPDIMENSIONS )
{
   const void * buffer = hb_parc( 1 );
   HB_SIZE nSize = hb_parclen( 1 );

   int iType = hbwin_BitmapType( buffer, nSize );

   int iHeight = 0;
   int iWidth = 0;
   HB_BOOL bRetVal = HB_FALSE;

   if( iType == HB_WIN_BITMAP_BMP && nSize >= sizeof( BITMAPCOREHEADER ) )
   {
      BITMAPFILEHEADER * pbmfh = ( BITMAPFILEHEADER * ) buffer;
      BITMAPINFO * pbmi = ( BITMAPINFO * ) ( pbmfh + 1 );

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

      bRetVal = HB_TRUE;
   }
   else if( iType == HB_WIN_BITMAP_JPEG )
   {
      bRetVal = ( hb_jpeg_get_param( ( const HB_BYTE * ) buffer, nSize, &iHeight, &iWidth, NULL, NULL ) == _JPEG_RET_OK );
   }
#if defined( HB_HAS_PNG ) && defined( HB_HAS_ZLIB )
   else if( iType == HB_WIN_BITMAP_PNG )
   {
      bRetVal = ( hb_png_get_param( ( const HB_BYTE * ) buffer, nSize, &iHeight, &iWidth, NULL, NULL ) == _PNG_RET_OK );
   }
#endif

   hb_storni( iWidth, 2 );
   hb_storni( iHeight, 3 );

   hb_retl( bRetVal );
}

#endif
