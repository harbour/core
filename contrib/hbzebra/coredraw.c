/*
 * Zebra barcode library
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

#include "hbzebra.h"
#include "hbvm.h"

typedef void ( *HB_ZEBRA_CALLBACK )( void * cargo, double dX, double dY, double dWidth, double dHeight );

int hb_zebra_draw( PHB_ZEBRA pZebra, HB_ZEBRA_CALLBACK pCallback, void * cargo, double dX, double dY, double dWidth, double dHeight, int iFlags )
{
   double  dLast;
   HB_SIZE n, nLen, nCount;
   HB_BOOL fLastBit;
   int     i, iCol = pZebra->iCol;

   HB_SYMBOL_UNUSED( iFlags );

   if( pZebra->iError != 0 )
      return HB_ZEBRA_ERROR_INVALIDZEBRA;

   nLen = hb_bitbuffer_len( pZebra->pBits );
   fLastBit = hb_bitbuffer_get( pZebra->pBits, 0 );
   dLast = dX;
   nCount = 0;
   i = 0;
   for( n = 0; n < nLen; n++ )
   {
      HB_BOOL fBit = hb_bitbuffer_get( pZebra->pBits, n );
      if( fBit != fLastBit )
      {
         if( fLastBit && pCallback )
            pCallback( cargo, dLast, dY, dWidth * nCount, dHeight );

         dLast += dWidth * nCount;
         nCount = 0;
         fLastBit = fBit;
      }
      nCount++;
      if( ++i == iCol )
      {
         if( nCount )
         {
            if( fBit && pCallback )
               pCallback( cargo, dLast, dY, dWidth * nCount, dHeight );
            nCount = 0;
         }
         i = 0;
         dY += dHeight;
         dLast = dX;
         if( n + 1 < nLen )
            fLastBit = hb_bitbuffer_get( pZebra->pBits, n + 1 );
      }
   }
   if( fLastBit && nCount && pCallback )
      pCallback( cargo, dLast, dY, dWidth * nCount, dHeight );

   return 0;
}

static void hb_zebra_draw_codeblock_callback( void * pDrawBlock, double dX, double dY, double dWidth, double dHeight )
{
   if( pDrawBlock && HB_IS_BLOCK( pDrawBlock ) && hb_vmRequestReenter() )
   {
      hb_vmPushEvalSym();
      hb_vmPush( ( PHB_ITEM ) pDrawBlock );
      hb_vmPushDouble( dX, HB_DEFAULT_DECIMALS );
      hb_vmPushDouble( dY, HB_DEFAULT_DECIMALS );
      hb_vmPushDouble( dWidth, HB_DEFAULT_DECIMALS );
      hb_vmPushDouble( dHeight, HB_DEFAULT_DECIMALS );
      hb_vmSend( 4 );
      hb_vmRequestRestore();
   }
}

int hb_zebra_draw_codeblock( PHB_ZEBRA pZebra, PHB_ITEM pDrawBlock, double dX, double dY, double dWidth, double dHeight, int iFlags )
{
   return hb_zebra_draw( pZebra, hb_zebra_draw_codeblock_callback, pDrawBlock, dX, dY, dWidth, dHeight, iFlags );
}

HB_FUNC( HB_ZEBRA_DRAW )
{
   PHB_ZEBRA pZebra = hb_zebra_param( 1 );

   if( pZebra )
   {
      PHB_ITEM pDrawBlock = hb_param( 2, HB_IT_BLOCK );
      if( pDrawBlock )
         hb_retni( hb_zebra_draw_codeblock( pZebra, pDrawBlock, hb_parnd( 3 ), hb_parnd( 4 ), hb_parnd( 5 ), hb_parnd( 6 ), hb_parni( 7 ) ) );
      else
         hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

int hb_zebra_getsize( PHB_ZEBRA pZebra, int * piWidth, int * piHeight )
{
   HB_SIZE n, nLen;
   int     iRow, iCol, iMaxCol = pZebra->iCol, iWidth, iHeight;

   if( pZebra->iError != 0 )
   {
      *piWidth = *piHeight = 0;
      return HB_ZEBRA_ERROR_INVALIDZEBRA;
   }

   nLen = hb_bitbuffer_len( pZebra->pBits );
   iWidth = iHeight = 0;
   iCol = iRow = 1;
   for( n = 0; n < nLen; n++ )
   {
      if( hb_bitbuffer_get( pZebra->pBits, n ) )
      {
         if( iCol > iWidth )
            iWidth = iCol;
         if( iRow > iHeight )
            iHeight = iRow;
      }
      if( iCol++ == iMaxCol )
      {
         ++iRow;
         iCol = 1;
      }
   }

   *piWidth = iWidth;
   *piHeight = iHeight;

   return 0;
}

/* hb_zebra_getsize( <hZebra>, @<nWidth>, @<nHeight> ) -> <nError> */
HB_FUNC( HB_ZEBRA_GETSIZE )
{
   PHB_ZEBRA pZebra = hb_zebra_param( 1 );

   if( pZebra )
   {
      int iWidth, iHeight;

      hb_retni( hb_zebra_getsize( pZebra, &iWidth, &iHeight ) );
      hb_storni( iWidth, 2 );
      hb_storni( iHeight, 3 );
   }
}

/* NOTE: caller must free the returned bitmap pointer if not NULL */
unsigned char * hb_zebra_getbitmap( PHB_ZEBRA pZebra, int iAlign, HB_BOOL fBottomUp,
                                    HB_SIZE * pnSize, int * piWidth, int * piHeight,
                                    int iScaleX, int iScaleY, int iBorder )
{
   unsigned char * pBitMap = NULL;
   HB_SIZE nSize = 0;
   int iWidth, iHeight;

   if( hb_zebra_getsize( pZebra, &iWidth, &iHeight ) == 0 &&
                         iWidth != 0 && iHeight != 0 )
   {
      HB_SIZE nLen = hb_bitbuffer_len( pZebra->pBits ), n;
      int iLineBits, iLineOffset, iMaxCol, iCol;

      if( iAlign < 1 || iAlign > 64 || ( iAlign & ( iAlign - 1 ) ) != 0 )
         iAlign = 8;
      if( iScaleX < 1 )
         iScaleX = 1;
      if( iScaleY < 1 )
         iScaleY = 1;
      if( iBorder < 0 )
         iBorder = 0;
      iWidth = iWidth * iScaleX + ( iBorder << 1 );
      iHeight = iHeight * iScaleY + ( iBorder << 1 );
      iLineBits = ( iWidth + ( iAlign - 1 ) ) & ~( iAlign - 1 );
      nSize = ( iLineBits * iHeight + 0x07 ) >> 3;
      if( nLen > ( nSize << 3 ) )
         nLen = nSize << 3;
      pBitMap = ( unsigned char * ) hb_xgrab( nSize + 1 );

      iMaxCol = pZebra->iCol;
      iCol = 0;
      if( fBottomUp )
         iLineOffset = iLineBits * ( iHeight - iBorder - 1 );
      else
         iLineOffset = iLineBits * iBorder;

      memset( pBitMap, 0, nSize );
      for( n = 0; n < nLen; n++ )
      {
         if( hb_bitbuffer_get( pZebra->pBits, n ) )
         {
            int iBitPos = iLineOffset + iCol * iScaleX + iBorder, iX, iY;
            for( iY = 0; iY < iScaleY; ++iY )
            {
               for( iX = 0; iX < iScaleX; ++iX )
               {
                  unsigned char * ptr = pBitMap + ( ( iBitPos + iX ) >> 3 );
                  *ptr |= 0x80 >> ( ( iBitPos + iX ) & 0x07 );
               }
               iBitPos += fBottomUp ? - iLineBits : iLineBits;
            }
         }
         if( ++iCol == iMaxCol )
         {
            iCol = 0;
            iLineOffset += ( fBottomUp ? - iLineBits : iLineBits ) * iScaleY;
         }
      }
   }

   *pnSize = nSize;
   *piWidth = iWidth;
   *piHeight = iHeight;

   return pBitMap;
}

/* hb_zebra_getbitmap( <hZebra>, <nAlign>=8, <lBottomUp>=.F., @<nWidth>, @<nHeight>, <nScaleX>, <nScaleY>, <nBorder> ) -> <cBitMap> | NIL */
HB_FUNC( HB_ZEBRA_GETBITMAP )
{
   PHB_ZEBRA pZebra = hb_zebra_param( 1 );

   if( pZebra )
   {
      HB_SIZE nSize;
      int iWidth, iHeight;
      unsigned char * pBitMap = hb_zebra_getbitmap( pZebra, hb_parni( 2 ), hb_parl( 3 ),
                                                    &nSize, &iWidth, &iHeight,
                                                    hb_parni( 6 ), hb_parni( 7 ), hb_parni( 8 ) );

      hb_storni( iWidth, 4 );
      hb_storni( iHeight, 5 );
      if( pBitMap )
         hb_retclen_buffer( ( char * ) pBitMap, nSize );
      else
         hb_retc_null();
   }
}
