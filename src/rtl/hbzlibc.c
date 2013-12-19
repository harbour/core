/*
 * Harbour Project source code:
 *    dynamic reference to ZLIB functions
 *
 * Copyright 2013 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#define _HB_ZLIB_INTERNAL_

#include "hbzlib.h"

static HB_SIZE s_zlibCompressBound( HB_SIZE nLen )
{
   HB_SYMBOL_UNUSED( nLen );
   return 0;
}

static HB_SIZE s_zlibUncompressedSize( const char * pSrc, HB_SIZE nLen, int * piResult )
{
   HB_SYMBOL_UNUSED( pSrc );
   HB_SYMBOL_UNUSED( nLen );
   HB_SYMBOL_UNUSED( piResult );
   return 0;
}

static int s_zlibCompress( char * pDst, HB_SIZE * pnDst, const char * pSrc, HB_SIZE nLen, int iLevel )
{
   HB_SYMBOL_UNUSED( pDst );
   HB_SYMBOL_UNUSED( pnDst );
   HB_SYMBOL_UNUSED( pSrc );
   HB_SYMBOL_UNUSED( nLen );
   HB_SYMBOL_UNUSED( iLevel );

   return HB_ZLIB_RES_UNSUPPORTED;
}

static int s_zlibUncompress( char * pDst, HB_SIZE * pnDst, const char * pSrc, HB_SIZE nLen )
{
   HB_SYMBOL_UNUSED( pDst );
   HB_SYMBOL_UNUSED( pnDst );
   HB_SYMBOL_UNUSED( pSrc );
   HB_SYMBOL_UNUSED( nLen );

   return HB_ZLIB_RES_UNSUPPORTED;
}

static HB_ZLIB_CBOUND s_compressBound    = s_zlibCompressBound;
static HB_ZLIB_UNSIZE s_uncompressedSize = s_zlibUncompressedSize;
static HB_ZLIB_COMPRS s_compress         = s_zlibCompress;
static HB_ZLIB_UNCMPS s_uncompress       = s_zlibUncompress;

void hb_zlibInit( HB_ZLIB_CBOUND pBound, HB_ZLIB_UNSIZE pUnSize,
                  HB_ZLIB_COMPRS pCompress, HB_ZLIB_UNCMPS pUncompress )
{
   s_compressBound    = pBound;
   s_uncompressedSize = pUnSize;
   s_compress         = pCompress;
   s_uncompress       = pUncompress;
}

HB_SIZE hb_zlibCompressBound( HB_SIZE nLen )
{
   return s_compressBound( nLen );
}

HB_SIZE hb_zlibUncompressedSize( const char * pSrc, HB_SIZE nLen, int * piResult )
{
   return s_uncompressedSize( pSrc, nLen, piResult );
}

int hb_zlibCompress( char * pDst, HB_SIZE * pnDst, const char * pSrc, HB_SIZE nLen, int iLevel )
{
   return s_compress( pDst, pnDst, pSrc, nLen, iLevel );
}

int hb_zlibUncompress( char * pDst, HB_SIZE * pnDst, const char * pSrc, HB_SIZE nLen )
{
   return s_uncompress( pDst, pnDst, pSrc, nLen );
}
