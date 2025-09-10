/*
 * BMP image library for Harbour
 *
 * Copyright 2025 Przemyslaw Czerpak <druzus /at/ priv.onet.pl>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#include "hbbmp.h"
#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"

void hb_bmp_free( PHB_BMPINFO pBMP )
{
   if( pBMP->data )
      hb_xfree( pBMP->data );
   hb_xfree( pBMP );
}

PHB_BMPINFO hb_bmp_new( int width, int height, int depth, int dpi, int * piError )
{
   PHB_BMPINFO pBMP = NULL;
   HB_BOOL fromtop = height < 0;

   if( piError )
      *piError = 0;

   if( fromtop )
   {
      height = -height;
      if( height == 1 )
         fromtop = HB_FALSE;
   }
   if( dpi == 0 )
      dpi = HB_BMP_DPI_DEFAULT;

   if( width < 1 || height < 1 || width > 0x10000 || height > 0x10000 )
   {
      if( piError )
         *piError = HB_BMP_ERROR_SIZE;
   }
   else
   {
      int rowlen;

      switch( depth )
      {
         case 0:
            depth = HB_BMP_DEPTH_DEFAULT;
            /* fallthrough */
         case 1:
         /* 2 color depth BMP is not officially supported,
            uncomment line below if you need it */
         /* case 2: */
         case 4:
         case 8:
         case 16:
         case 24:
         case 32:
            break;
         default:
            if( piError )
               *piError = HB_BMP_ERROR_DEPTH;
            return NULL;
      }
      pBMP = ( PHB_BMPINFO ) hb_xgrabz( sizeof( HB_BMPINFO ) );
      rowlen = ( ( width * depth + 31 ) >> 5 ) << 2;

      pBMP->error    = 0;
      pBMP->width    = width;
      pBMP->height   = height;
      pBMP->dpi      = dpi;
      pBMP->depth    = depth;
      pBMP->clrused  = 0;
      pBMP->rowlen   = rowlen;
      pBMP->fromtop  = fromtop;
      pBMP->data     = ( HB_BYTE * ) hb_xgrabz( rowlen * height );
   }
   return pBMP;
}

PHB_BMPINFO hb_bmp_frombitmap( const HB_BYTE * bitmap, int align,
                               int width, int height, int depth, int dpi,
                               const int * palette, int colors, int * piError )
{
   static const int mono_palette[ 2 ] = { 0x00FFFFFF, 0x00000000 };
   PHB_BMPINFO pBMP = NULL;

   if( bitmap && ( align >= 8 || align >= depth ) && ( align & ( align - 1 ) ) == 0 )
      pBMP = hb_bmp_new( width, height, depth, dpi, piError );
   else if ( piError )
      *piError = HB_BMP_ERROR_CORRUPT;

   if( pBMP )
   {
      if( bitmap )
      {
         int rowbits = ( ( pBMP->width * depth ) + align - 1 ) & ~( align - 1 );
         int rowlen = ( rowbits + 0x07 ) >> 3, row, col;

         if( align == 32 )
            memcpy( pBMP->data, bitmap, pBMP->height * pBMP->rowlen );
         else if( pBMP->height == 1 )
            memcpy( pBMP->data, bitmap, HB_MIN( rowlen, pBMP->rowlen ) );
         else if( align >= 8 )
         {
            for( row = 0; row < height; ++row )
               memcpy( pBMP->data + pBMP->rowlen * row, bitmap + rowlen * row,
                       HB_MIN( rowlen, pBMP->rowlen ) );
         }
         else
         {
            int maskb, shift;

            shift = depth == 4 ? 1 : ( depth == 2 ? 2 : 3 );
            maskb = ( 0x01 << shift ) - 1;
            for( row = 0; row < height; ++row )
            {
               HB_BYTE * rowdst = pBMP->data + pBMP->rowlen * row;
               int offset = row * rowbits;
               for( col = 0; col < width; ++col, offset += depth )
                  rowdst[ col >> shift ] |= ( ( bitmap[ offset >> shift ] >>
                                  ( ( maskb - ( offset & maskb ) ) * depth ) ) &
                                maskb ) << ( maskb - ( col & maskb ) ) * depth;
            }
         }
      }
      if( depth == 1 && ( palette == NULL || colors <= 0 ) )
      {
         palette = mono_palette;
         colors = 2;
      }
      if( palette && colors > 0 && depth <= 8 )
      {
         int i;
         if( colors > 256 )
            colors = 256;
         for( i = 0; i < colors; ++i )
         {
            int clr = palette[ i ];
            hb_bmp_color( pBMP, ( clr >> 16 ) & 0xFF, ( clr >> 8 ) & 0xFF, clr & 0xFF, ( clr >> 24 ) & 0xFF );
         }
      }
   }
   return pBMP;
}

PHB_BMPINFO hb_bmp_copy( PHB_BMPINFO pBMP )
{
   PHB_BMPINFO pBMPnew = ( PHB_BMPINFO ) hb_xgrab( sizeof( HB_BMPINFO ) );

   memcpy( pBMPnew, pBMP, sizeof( HB_BMPINFO ) );
   pBMPnew->data = ( HB_BYTE * ) hb_xgrab( pBMP->rowlen * pBMP->height );
   memcpy( pBMPnew->data, pBMP->data, pBMP->rowlen * pBMP->height );

   pBMPnew->error = 0;

   return pBMPnew;
}

HB_BYTE * hb_bmp_bitmapptr( PHB_BMPINFO pBMP, HB_SIZE * pnSize )
{
   if( pnSize )
      *pnSize = pBMP->rowlen * pBMP->height;
   return pBMP->data;
}

void hb_bmp_seterror( PHB_BMPINFO pBMP, int error )
{
   pBMP->error = error;
}

int hb_bmp_error( PHB_BMPINFO pBMP )
{
   return pBMP->error;
}

int hb_bmp_width( PHB_BMPINFO pBMP )
{
   return pBMP->width;
}

int hb_bmp_height( PHB_BMPINFO pBMP )
{
   return pBMP->height;
}

int hb_bmp_depth( PHB_BMPINFO pBMP )
{
   return pBMP->depth;
}

void hb_bmp_colorreset( PHB_BMPINFO pBMP )
{
   memset( pBMP->palette, 0, sizeof( pBMP->palette ) );
   pBMP->clrused = 0;
}

HB_MAXINT hb_bmp_color( PHB_BMPINFO pBMP, int r, int g, int b, int a )
{
   HB_MAXINT iColor = -1;

   pBMP->error = 0;

   if( ( r & 0xFF ) != r || ( g & 0xFF ) != g ||
       ( b & 0xFF ) != b || ( a & 0xFF ) != a )
      pBMP->error = HB_BMP_ERROR_COLORVALUE;
   else if( pBMP->depth == 32 )
      iColor = b | ( g << 8 ) | ( r << 16 ) | ( a << 24 );
   else if( pBMP->depth == 24 )
      iColor = b | ( g << 8 ) | ( r << 16 );
   /* 16 color depth does not have RGB mapping in BMP format
    * color indexes have to be used directly and their RGB
    * values defined outside BMP structure
    */
   else if( pBMP->depth <= 8 )
   {
      int i;
      for( i = 0; i < pBMP->clrused; ++i )
      {
         if( pBMP->palette[ i ].blue  == b &&
             pBMP->palette[ i ].green == g &&
             pBMP->palette[ i ].red   == r &&
             pBMP->palette[ i ].alpha == a )
         {
            iColor = i;
            break;
         }
      }
      if( iColor == -1 )
      {
         if( pBMP->clrused < ( 1 << pBMP->depth ) )
         {
            iColor = pBMP->clrused++;
            pBMP->palette[ ( unsigned int ) iColor ].blue  = ( HB_BYTE ) b;
            pBMP->palette[ ( unsigned int ) iColor ].green = ( HB_BYTE ) g;
            pBMP->palette[ ( unsigned int ) iColor ].red   = ( HB_BYTE ) r;
            pBMP->palette[ ( unsigned int ) iColor ].alpha = ( HB_BYTE ) a;
         }
         else
            pBMP->error = HB_BMP_ERROR_PALETTEFULL;
      }
   }
   else
      pBMP->error = HB_BMP_ERROR_COLORINDEX;

   return iColor;
}

HB_BOOL hb_bmp_color2rgb( PHB_BMPINFO pBMP, HB_MAXINT clr, int * r, int * g, int * b, int * a )
{
   pBMP->error = HB_BMP_ERROR_COLORINDEX;
   * b = * g = * r = * a = -1;

   if( clr >= 0 && clr <= HB_LL( 0xFFFFFFFF ) )
   {
      switch( pBMP->depth )
      {
         case 1:
         case 2:
         case 4:
         case 8:
            if( clr < ( HB_MAXINT ) pBMP->clrused )
            {
               * b = pBMP->palette[ ( unsigned int ) clr ].blue;
               * g = pBMP->palette[ ( unsigned int ) clr ].green;
               * r = pBMP->palette[ ( unsigned int ) clr ].red;
               * a = pBMP->palette[ ( unsigned int ) clr ].alpha;
               pBMP->error = 0;
            }
            break;
         /* 16 color depth does not have RGB mapping in BMP format
          * color indexes have to be used directly and their RGB
          * values defined outside BMP structure
          */
         case 24:
            /* ignore alpha channel */
            clr &= 0xFFFFFF;
            /* fallthrough */
         case 32:
            * b = clr & 0xFF;
            * g = ( clr >> 8 ) & 0xFF;
            * r = ( clr >> 16 ) & 0xFF;
            * a = ( clr >> 24 ) & 0xFF;
            pBMP->error = 0;
            break;
      }
   }
   return pBMP->error == 0;
}

HB_BOOL hb_bmp_putpixel( PHB_BMPINFO pBMP, int x, int y, HB_MAXINT clr )
{
   if( x < 0 || x >= pBMP->width || y < 0 || y >= pBMP->height )
      pBMP->error = HB_BMP_ERROR_RANGE;
   else if( clr < 0 || ( clr > ( pBMP->depth <= 8  ? pBMP->clrused :
                               ( pBMP->depth == 16 ? 0xFFFF :
                                                     HB_LL( 0xFFFFFFFF ) ) ) ) )
      pBMP->error = HB_BMP_ERROR_COLORINDEX;
   else
   {
      int index = ( ( ( pBMP->fromtop ? y : pBMP->height - y - 1 ) *
                      pBMP->rowlen ) << 3 ) + ( x * pBMP->depth );
      HB_BYTE * ptr = &pBMP->data[ index >> 3 ];
      int shift = -1;

      switch( pBMP->depth )
      {
         case 1:
            shift = 0x07 ^ ( x & 0x07 );
            break;
         case 2:
            shift = ( 0x03 ^ ( x & 0x03 ) ) << 1;
            break;
         case 4:
            shift = ( 0x01 ^ ( x & 0x01 ) ) << 2;
            break;
         case 8:
            *ptr = ( HB_BYTE ) clr;
            break;
         case 16:
            HB_PUT_LE_UINT16( ptr, clr );
            break;
         case 24:
            HB_PUT_LE_UINT24( ptr, clr );
            break;
         case 32:
            HB_PUT_LE_UINT32( ptr, clr );
            break;
      }
      if( shift >= 0 )
         *ptr = ( HB_BYTE ) ( ( *ptr & ~( ( ( 0x01 << pBMP->depth ) - 1 ) << shift ) ) | ( clr << shift ) );
      pBMP->error = 0;
   }
   return pBMP->error == 0;
}

HB_MAXINT hb_bmp_getpixel( PHB_BMPINFO pBMP, int x, int y )
{
   HB_MAXINT clr = -1;

   if( x < 0 || x >= pBMP->width || y < 0 || y >= pBMP->height )
      pBMP->error = HB_BMP_ERROR_RANGE;
   else
   {
      int index = ( ( ( pBMP->fromtop ? y : pBMP->height - y - 1 ) *
                      pBMP->rowlen ) << 3 ) + ( x * pBMP->depth );
      HB_BYTE * ptr = &pBMP->data[ index >> 3 ];
      int shift = -1;

      switch( pBMP->depth )
      {
         case 1:
            shift = 0x07 ^ ( x & 0x07 );
            break;
         case 2:
            shift = ( 0x03 ^ ( x & 0x03 ) ) << 1;
            break;
         case 4:
            shift = ( 0x01 ^ ( x & 0x01 ) ) << 2;
            break;
         case 8:
            clr = *ptr;
            break;
         case 16:
            clr = HB_GET_LE_UINT16( ptr );
            break;
         case 24:
            clr = HB_GET_LE_UINT24( ptr );
            break;
         case 32:
            clr = HB_GET_LE_UINT32( ptr );
            break;
      }
      if( shift >= 0 )
         clr = ( *ptr & ( ( ( 0x01 << pBMP->depth ) - 1 ) << shift ) ) >> shift;
      pBMP->error = 0;
   }
   return clr;
}

void hb_bmp_line( PHB_BMPINFO pBMP, int x1, int y1, int x2, int y2, HB_MAXINT clr )
{
   if( clr < 0 || ( clr > ( pBMP->depth <= 8  ? pBMP->clrused :
                          ( pBMP->depth == 16 ? 0xFFFF :
                                                HB_LL( 0xFFFFFFFF ) ) ) ) )
      pBMP->error = HB_BMP_ERROR_COLORINDEX;
   else
   {
      double dd;
      int dx, dy, x, y;

      dx = x1 >= x2 ? x1 - x2 : x2 - x1;
      dy = y1 >= y2 ? y1 - y2 : y2 - y1;

      if( dx >= dy || dy == 0 ? x1 > x2 : y1 > y2 || dx == 0 )
      {
         int nn = x1;
         x1 = x2;
         x2 = nn;
         nn = y1;
         y1 = y2;
         y2 = nn;
      }

      if( dy == 0 )
      {
         for( x = HB_MAX( x1, 0 ); x <= x2 && x < pBMP->width; ++x )
            hb_bmp_putpixel( pBMP, x, y1, clr );
      }
      else if( dx == 0 )
      {
         for( y = HB_MAX( y1, 0 ); y <= y2 && y < pBMP->height; ++y )
            hb_bmp_putpixel( pBMP, x1, y, clr );
      }
      else if( dx >= dy )
      {
         dd = ( double ) ( y2 - y1 ) / ( x2 - x1 );
         for( x = HB_MAX( x1, 0 ); x <= x2 && x < pBMP->width; ++x )
            hb_bmp_putpixel( pBMP, x, ( int ) ( dd * ( x - x1 ) + 0.50001 ) + y1, clr );
      }
      else
      {
         dd = ( double ) ( x2 - x1 ) / ( y2 - y1 );
         for( y = HB_MAX( y1, 0 ); y <= y2 && y < pBMP->height; ++y )
            hb_bmp_putpixel( pBMP, ( int ) ( dd * ( y - y1 ) + 0.50001 ) + x1, y, clr );
      }
      pBMP->error = 0;
   }
}

void hb_bmp_rect( PHB_BMPINFO pBMP, int x, int y, int width, int height, HB_MAXINT clr, HB_BOOL fFill )
{
   if( width < 0 )
   {
      x += width;
      width = - width;
   }
   if( height < 0 )
   {
      y += height;
      height = - height;
   }
   if( clr < 0 || ( clr > ( pBMP->depth <= 8  ? pBMP->clrused :
                          ( pBMP->depth == 16 ? 0xFFFF :
                                                HB_LL( 0xFFFFFFFF ) ) ) ) )
      pBMP->error = HB_BMP_ERROR_COLORINDEX;
   else if( width < 0 || height < 0 || 
            x >= pBMP->width || y >= pBMP->height ||
            x + width < 0 || y + height < 0 )
      pBMP->error = HB_BMP_ERROR_RANGE;
   else
   {
      int x1, y1, x2, y2, xx, yy;

      x1 = HB_MAX( x, 0 );
      y1 = HB_MAX( y, 0 );
      x2 = x1 + width >= pBMP->width ? pBMP->width : x1 + width;
      y2 = y1 + height >= pBMP->height ? pBMP->height : y1 + height;
      if( fFill )
      {
         for( xx = x1; xx < x2; ++xx )
         {
            for( yy = y1; yy < y2; ++yy )
               hb_bmp_putpixel( pBMP, xx, yy, clr );
         }
      }
      else
      {
         if( y >= 0 )
         {
            for( xx = x1; xx < x2; ++xx )
               hb_bmp_putpixel( pBMP, xx, y, clr );
         }
         if( x >= 0 )
         {
            for( yy = y1; yy < y2; ++yy )
               hb_bmp_putpixel( pBMP, x, yy, clr );
         }
         if( y2 < pBMP->height )
         {
            for( xx = x1; xx < x2; ++xx )
               hb_bmp_putpixel( pBMP, xx, y2 - 1, clr );
         }
         if( x2 < pBMP->width )
         {
            for( yy = y1; yy < y2; ++yy )
               hb_bmp_putpixel( pBMP, x2 - 1, yy, clr );
         }
      }
      pBMP->error = 0;
   }
}

PHB_BMPINFO hb_bmp_decode( const HB_BYTE * data, HB_SIZE size, int * piError )
{
   PHB_BMPINFO pBMP = NULL;
   int iError = 0;

   if( ! data || size > 0x10000000 )
      iError = HB_BMP_ERROR_PARAM;
   else if( size < HB_BMP_FILEHEADER_SIZE + HB_BMP_INFOHEADER_MINSIZE )
      iError = HB_BMP_ERROR_CORRUPT;
   else
   {
      PHB_BMPHEADER header = ( PHB_BMPHEADER ) data;
      if( header->signature[ 0 ] != 'B' || header->signature[ 1 ] != 'M' )
         iError = HB_BMP_ERROR_CORRUPT;
      else
      {
         /* file_size in BMP headers is often wrong so I do not check it
          * int file_size = HB_GET_LE_INT32( header->file_size );
          */
         int bmpoffset = HB_GET_LE_INT32( header->bmpoffset ),
             headersize = HB_GET_LE_INT32( header->headersize ),
             width = 0, height = 0, depth = 0, rowlen, dpi = 0,
             clrused = 0, palette_bytes = 0;
         HB_BOOL fromtop;

         if( size < ( HB_SIZE ) (  HB_BMP_FILEHEADER_SIZE +
                                   HB_BMP_INFOHEADER_MINSIZE + headersize ) )
            iError = HB_BMP_ERROR_CORRUPT;
         else if( headersize == 12 || headersize == 16 )
         {
            PHB_BMPOS2HEADER header_os2 = ( PHB_BMPOS2HEADER ) header->file_size;
            width = HB_GET_LE_UINT16( header_os2->width );
            height = HB_GET_LE_UINT16( header_os2->height );
            depth = HB_GET_LE_UINT16( header_os2->bitsperpixel );
            if( depth <= 8 )
               clrused = 0x01 << depth;
            dpi = HB_BMP_DPI_DEFAULT;
            palette_bytes = 3;
            if( depth != 1 && depth != 2 && depth != 4 && depth != 8 && depth != 24 )
               iError = HB_BMP_ERROR_CORRUPT;
         }
         else if( headersize == 40 || headersize == 52 || headersize == 56 ||
                  headersize == 108 || headersize == 124 )
         {
            width = HB_GET_LE_INT32( header->width );
            height = HB_GET_LE_INT32( header->height );
            dpi = ( int ) ( ( double ) HB_GET_LE_INT32( header->hresolution ) / 39.3701 + 0.5 );
            depth = HB_GET_LE_UINT16( header->bitsperpixel );
            if( depth <= 8 )
            {
               clrused = HB_GET_LE_INT32( header->clrused );
               if( clrused == 0 )
                  clrused = 0x01 << depth;
            }
            palette_bytes = 4;
            if( ( depth != 1 && depth != 2 && depth != 4 && depth != 8 &&
                  depth != 16 && depth != 24 && depth != 32 ) )
               iError = HB_BMP_ERROR_CORRUPT;
            else if( HB_GET_LE_INT32( header->compression ) != 0 )
               iError = HB_BMP_ERROR_UNSUPPORTED;
         }
         else
            iError = HB_BMP_ERROR_CORRUPT;

         fromtop = height < 0;
         if( fromtop )
            height = -height;

         rowlen = ( ( width * depth + 31 ) >> 5 ) << 2;
         if( iError == 0 &&
             ( HB_BMP_FILEHEADER_SIZE + headersize + clrused * palette_bytes > bmpoffset ||
               ( HB_SIZE ) bmpoffset + rowlen * height > size ) )
            iError = HB_BMP_ERROR_CORRUPT;
         else if( iError == 0 )
         {
            pBMP = hb_bmp_new( width, fromtop ? -height : height, depth, dpi, &iError );
            if( pBMP )
            {
               const HB_BYTE * ptr = data + HB_BMP_FILEHEADER_SIZE + headersize;
               int idx, unused = 0;
               for( idx = 0; idx < clrused; ++idx )
               {
                  pBMP->palette[ idx ].blue  = *ptr++;
                  pBMP->palette[ idx ].green = *ptr++;
                  pBMP->palette[ idx ].red   = *ptr++;
                  if( palette_bytes == 4 )
                     pBMP->palette[ idx ].alpha = *ptr++;
                  if( pBMP->palette[ idx ].blue  == 0 &&
                      pBMP->palette[ idx ].green == 0 &&
                      pBMP->palette[ idx ].red   == 0 &&
                      pBMP->palette[ idx ].alpha == 0 )
                     ++unused;
                  else if( unused > 1 )
                     unused = 1;
               }
               if( unused > 1 )
                  clrused -= unused - 1;
               pBMP->clrused = clrused;
               memcpy( pBMP->data, data + bmpoffset, rowlen * height );
            }
         }
      }
   }
   if( piError )
      *piError = iError;

   return pBMP;
}

HB_BYTE * hb_bmp_encode( PHB_BMPINFO pBMP, HB_SIZE * pnSize )
{
   int clrused, bmpoffset, bmpsize, file_size, idx, height, dpi;
   PHB_BMPHEADER header;
   HB_BYTE * data, * ptr;

   /* always allocate area for whole palette for programs which
    * do not check number of used colors in BMP header
    */
   clrused = pBMP->depth > 8 ? 0 : 0x01 << pBMP->depth;
   /* 4 byte alignment is not required inside file */
   /* bmpoffset = ( sizeof( HB_BMPHEADER ) + clrused * 4 + 3 ) & ~0x03; */
   bmpoffset = sizeof( HB_BMPHEADER ) + clrused * 4;
   bmpsize = pBMP->rowlen * pBMP->height;
   file_size = bmpoffset + bmpsize;
   height = pBMP->fromtop ? -pBMP->height : pBMP->height;
   dpi = ( int ) ( HB_BMP_INCHES_PER_METRE * pBMP->dpi + 0.5 );

   /* when clrused is set in BMP header to value different then 0 or
    * palette size some BMP viewers cannot show BMP correctly so
    * I decided to comment code below and always store 0
    */
   /*
   if( clrused > 0  )
      clrused = clrused > pBMP->clrused ? pBMP->clrused : 0;
   */
   clrused = 0;

   data = ( HB_BYTE * ) hb_xgrab( file_size + 1 );
   memset( data, 0, sizeof( HB_BMPHEADER ) );

   header = ( PHB_BMPHEADER ) data;
   header->signature[ 0 ] = 'B';
   header->signature[ 1 ] = 'M';
   HB_PUT_LE_UINT32( header->file_size, file_size );
   HB_PUT_LE_UINT16( header->reserved1, 0 );
   HB_PUT_LE_UINT16( header->reserved2, 0 );
   HB_PUT_LE_UINT32( header->bmpoffset, bmpoffset );
   HB_PUT_LE_UINT32( header->headersize, sizeof( HB_BMPHEADER ) - HB_BMP_FILEHEADER_SIZE );
   HB_PUT_LE_UINT32( header->width, pBMP->width );
   HB_PUT_LE_UINT32( header->height, height );
   HB_PUT_LE_UINT16( header->planes, 1 );
   HB_PUT_LE_UINT16( header->bitsperpixel, pBMP->depth );
   HB_PUT_LE_UINT32( header->compression, 0 );
   HB_PUT_LE_UINT32( header->bitmapsize, bmpsize );
   HB_PUT_LE_UINT32( header->hresolution, dpi );
   HB_PUT_LE_UINT32( header->vresolution, dpi );
   HB_PUT_LE_UINT32( header->clrused, clrused );
   HB_PUT_LE_UINT32( header->clrimportant, 0 );

   ptr = data + sizeof( HB_BMPHEADER );
   for( idx = 0; idx < pBMP->clrused; ++idx )
   {
      *ptr++ = pBMP->palette[ idx ].blue;
      *ptr++ = pBMP->palette[ idx ].green;
      *ptr++ = pBMP->palette[ idx ].red;
      *ptr++ = pBMP->palette[ idx ].alpha;
   }
   while( ptr < data + bmpoffset )
      *ptr++ = 0;
   memcpy( data + bmpoffset, pBMP->data, pBMP->rowlen * pBMP->height );

   data[ file_size ] = 0;
   *pnSize = file_size;

   pBMP->error = 0;

   return data;
}

/* Collectable pointer support */

static HB_GARBAGE_FUNC( hb_bmp_destructor )
{
   PHB_BMPINFO * pBMPptr = ( PHB_BMPINFO * ) Cargo;

   if( *pBMPptr )
   {
      hb_bmp_free( *pBMPptr );
      *pBMPptr = NULL;
   }
}

static const HB_GC_FUNCS s_gcBMPfuncs =
{
   hb_bmp_destructor,
   hb_gcDummyMark
};

void hb_bmpParamFree( int iParam )
{
   PHB_BMPINFO * pBMPPtr = ( PHB_BMPINFO * ) hb_parptrGC( &s_gcBMPfuncs, iParam );

   if( pBMPPtr && *pBMPPtr )
   {
      hb_bmp_free( *pBMPPtr );
      *pBMPPtr = NULL;
   }
}

PHB_BMPINFO hb_bmpParam( int iParam, HB_BOOL fError )
{
   PHB_BMPINFO * pBMPPtr = ( PHB_BMPINFO * ) hb_parptrGC( &s_gcBMPfuncs, iParam );

   if( pBMPPtr && *pBMPPtr )
      return *pBMPPtr;
   else if( fError )
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

void hb_bmpReturn( PHB_BMPINFO pBMP )
{
   if( pBMP )
   {
      PHB_BMPINFO * pBMPPtr = ( PHB_BMPINFO * )
                        hb_gcAllocate( sizeof( PHB_BMPINFO ), &s_gcBMPfuncs );
      *pBMPPtr = pBMP;
      hb_retptrGC( pBMPPtr );
   }
   else
      hb_ret();
}

/* PRG functions */

HB_FUNC( HB_BMP_NEW )
{
   int iError = 0;
   PHB_BMPINFO pBMP = hb_bmp_new( hb_parni( 1 ), hb_parni( 2 ),
                                  hb_parnidef( 3, 1 ),
                                  hb_parnidef( 4, HB_BMP_DPI_DEFAULT ),
                                  &iError );
   hb_storni( iError, 5 );
   hb_bmpReturn( pBMP );
}

/* hb_bmp_frombitmap( <cBitMap>, <nAlign>, <nWidth>, <nHeight>, [<nDepth>=1], [<nDPI>=72], [<aPalette>], [@<nError>] ) -> <pBMP> | NIL */
HB_FUNC( HB_BMP_FROMBITMAP )
{
   const HB_BYTE * bitmap = ( const HB_BYTE * ) hb_parc( 1 );
   int align = hb_parni( 2 ), width = hb_parni( 3 ), height = hb_parni( 4 ),
       depth = hb_parni( 5 ), dpi = hb_parni( 6 ),
       iError = 0;
   PHB_ITEM pColors = hb_param( 7, HB_IT_ARRAY );
   HB_BOOL fromtop = height < 0;
   PHB_BMPINFO pBMP = NULL;

   if( fromtop )
   {
      height = -height;
      if( height == 1 )
         fromtop = HB_FALSE;
   }
   if( ! bitmap || ( align & ( align - 1 ) ) != 0 ||
       ( HB_SIZE ) ( ( ( ( width * depth + align - 1 ) & ~( align - 1 ) ) *
                       height + 0x07 ) >> 3 ) > hb_parclen( 1 ) )
      iError = HB_BMP_ERROR_PARAM;
   else
   {
      pBMP = hb_bmp_frombitmap( bitmap, align, width, fromtop ? -height : height, depth, dpi,
                                NULL, 0, &iError );
      if( pBMP && pColors && depth <= 8 )
      {
         HB_SIZE nLen = hb_arrayLen( pColors ), nAt;

         if( nLen > ( HB_SIZE ) ( 1 << depth ) )
            nLen = ( HB_SIZE ) ( 1 << depth );
         hb_bmp_colorreset( pBMP );
         for( nAt = 1; nAt <= nLen; ++nAt )
         {
            int clr = hb_arrayGetNI( pColors, nAt );
            hb_bmp_color( pBMP, ( clr >> 16 ) & 0xFF, ( clr >> 8 ) & 0xFF,
                                clr * 0xFF, ( clr >> 24 ) & 0xFF );
         }
      }
   }
   hb_storni( iError, 8 );
   hb_bmpReturn( pBMP );
}

HB_FUNC( HB_BMP_COPY )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
      pBMP = hb_bmp_copy( pBMP );

   hb_bmpReturn( pBMP );
}

HB_FUNC( HB_BMP_DECODE )
{
   const char * data = hb_parc( 1 );
   if( data )
   {
      int iError = 0;
      PHB_BMPINFO pBMP = hb_bmp_decode( ( const HB_BYTE * ) data, hb_parclen( 1 ), &iError );
      hb_storni( iError, 2 );
      hb_bmpReturn( pBMP );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_BMP_ENCODE )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
   {
      HB_SIZE size;
      HB_BYTE * data = hb_bmp_encode( pBMP, &size );

      hb_retclen_buffer( ( char * ) data, size );
   }
}

HB_FUNC( HB_BMP_FREE )
{
   hb_bmpParamFree( 1 );
}

HB_FUNC( HB_BMP_ERROR )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
      hb_retni( hb_bmp_error( pBMP ) );
}

HB_FUNC( HB_BMP_WIDTH )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
      hb_retni( hb_bmp_width( pBMP ) );
}

HB_FUNC( HB_BMP_HEIGHT )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
      hb_retni( hb_bmp_height( pBMP ) );
}

HB_FUNC( HB_BMP_DEPTH )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
      hb_retni( hb_bmp_depth( pBMP ) );
}

HB_FUNC( HB_BMP_COLORRESET )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
      hb_bmp_colorreset( pBMP );
}

HB_FUNC( HB_BMP_COLOR )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
      hb_retnint( hb_bmp_color( pBMP, hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ) );
}

HB_FUNC( HB_BMP_COLOR2RGB )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
   {
      int r, g, b, a;
      hb_retl( hb_bmp_color2rgb( pBMP, hb_parnint( 2 ), &r, &g, &b, &a ) );
      hb_storni( r, 3 );
      hb_storni( g, 4 );
      hb_storni( b, 5 );
      hb_storni( a, 6 );
   }
}

HB_FUNC( HB_BMP_PUTPIXEL )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
      hb_retl( hb_bmp_putpixel( pBMP, hb_parni( 2 ), hb_parni( 3 ), hb_parnint( 4 ) ) );
}

HB_FUNC( HB_BMP_GETPIXEL )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
      hb_retnint( hb_bmp_getpixel( pBMP, hb_parni( 2 ), hb_parni( 3 ) ) );
}

HB_FUNC( HB_BMP_LINE )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
      hb_bmp_line( pBMP, hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parnint( 6 ) );
}

HB_FUNC( HB_BMP_RECT )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
      hb_bmp_rect( pBMP, hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parnint( 6 ), hb_parldef( 7, HB_TRUE ) );
}

HB_FUNC( HB_BMP_LOAD )
{
   const char * pszFileName = hb_parc( 1 );

   if( pszFileName )
   {
      int iError = 0;
      HB_SIZE size;
      HB_BYTE * data = hb_fileLoad( pszFileName, 0x1000000, &size );
      if( data )
      {
         PHB_BMPINFO pBMP = hb_bmp_decode( data, size, &iError );
         hb_bmpReturn( pBMP );
         hb_xfree( data );
      }
      else
         iError = HB_BMP_ERROR_FILEREAD;
      hb_storni( iError, 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_BMP_SAVE )
{
   PHB_BMPINFO pBMP = hb_bmpParam( 1, HB_TRUE );

   if( pBMP )
   {
      const char * pszFileName = hb_parc( 2 );

      if( pszFileName )
      {
         HB_SIZE size;
         HB_BYTE * data = hb_bmp_encode( pBMP, &size );
         HB_BOOL fResult = hb_fileSave( pszFileName, data, size );
         if( ! fResult )
            hb_bmp_seterror( pBMP, HB_BMP_ERROR_FILEWRITE );
         hb_retl( fResult );
         hb_xfree( data );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}
