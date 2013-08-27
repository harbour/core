/*
 * Harbour Project source code:
 * Bitmap dimension detection
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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

#include "hbwin.h"
#include "hbapiitm.h"

#if defined( HB_HAS_PNG ) && defined( HB_HAS_ZLIB )
   #include "png.h"
#endif

/* .jpeg size detection code. [vszakats] */

#define _JPEG_RET_OK           0
#define _JPEG_RET_OVERRUN      1
#define _JPEG_RET_INVALID      2
#define _JPEG_RET_UNSUPPORTED  3

#define _JPEG_CS_GRAY          1
#define _JPEG_CS_RGB           2
#define _JPEG_CS_CMYK          3

static int hb_jpeg_get_param( const HB_BYTE * buffer, HB_SIZE nBufferSize, int * piHeight, int * piWidth, int * piColorSpace, int * piBPC )
{
   HB_SIZE nPos = 0;

   HB_U16 tag;
   HB_U16 height = 0;
   HB_U16 width = 0;
   HB_BYTE colorspace = 0;
   HB_BYTE bpc = 0;

   if( piHeight )
      *piHeight = ( int ) height;
   if( piWidth )
      *piWidth = ( int ) width;
   if( piColorSpace )
      *piColorSpace = ( int ) colorspace;
   if( piBPC )
      *piBPC = ( int ) bpc;

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

         bpc = *( buffer + nPos ); /* nPos += 1; */

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

#define _PNG_RET_OK            0
#define _PNG_RET_ERR_INVALID1  1
#define _PNG_RET_ERR_INVALID2  2
#define _PNG_RET_ERR_INIT1     3
#define _PNG_RET_ERR_INIT2     4
#define _PNG_RET_ERR_INIT3     5
#define _PNG_RET_ERR_READ      6

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
   png_set_read_fn( png_ptr, ( void * ) &hb_png_read_data, ( png_rw_ptr ) hb_png_read_func );

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

   int iType = hbwin_bitmapType( buffer, nSize );

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
