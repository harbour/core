/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Zebra barcode library
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

/*
   DataMatrix is ISO/IEC 16022:2006

   Some info links:
     http://www.gs1.org/docs/barcodes/GS1_DataMatrix_Introduction_and_technical_overview.pdf
     http://www.aipsys.com/dmintro.htm

   Open source projects, that implements DataMatrix:
     http://www.datenfreihafen.org/projects/iec16022.html
     http://www.libdmtx.org/
     http://www.codeproject.com/Articles/66495/DataMatrixNet-ported-to-Compact-Framework.aspx

   Online encoder:
     http://www.bcgen.com/datamatrix-barcode-creator.html
     http://www.bcmaker.com/demos/datamatrix.php

   Online decoder:
     http://www.datasymbol.com/barcode-recognition-sdk/barcode-reader/online-barcode-decoder.html

 */

#include "hbzebra.h"
#include "hbapiitm.h"
#include "hbapierr.h"


/* Special CodeWords */
#define PADDING               129
#define PAIR_OF_DIGITS        130  /* 00..99 encoded as 130..229 */
#define SHIFT_EXTENDED_ASCII  235  /* Shift to extended ASCII for 1 character */

#define SIZE_COUNT            30

typedef struct
{
   int iRow;
   int iCol;
   int iRegionRow;
   int iRegionCol;
   int iDataSize;
   int iBlockSize;
   int iBlockErrorSize;
} DATAMATRIX_SIZE, * PDATAMATRIX_SIZE;


static const DATAMATRIX_SIZE s_size[ SIZE_COUNT ] = {
   { 10,  10,  10,  10,    3,    3,    5 },
   { 12,  12,  12,  12,    5,    5,    7 },
   {  8,  18,   8,  18,    5,    5,    7 },
   { 14,  14,  14,  14,    8,    8,   10 },
   {  8,  32,   8,  16,   10,   10,   11 },
   { 16,  16,  16,  16,   12,   12,   12 },
   { 12,  26,  12,  26,   16,   16,   14 },
   { 18,  18,  18,  18,   18,   18,   14 },
   { 20,  20,  20,  20,   22,   22,   18 },
   { 12,  36,  12,  18,   22,   22,   18 },
   { 22,  22,  22,  22,   30,   30,   20 },
   { 16,  36,  16,  18,   32,   32,   24 },
   { 24,  24,  24,  24,   36,   36,   24 },
   { 26,  26,  26,  26,   44,   44,   28 },
   { 16,  48,  16,  24,   49,   49,   28 },
   { 32,  32,  16,  16,   62,   62,   36 },
   { 36,  36,  18,  18,   86,   86,   42 },
   { 40,  40,  20,  20,  114,  114,   48 },
   { 44,  44,  22,  22,  144,  144,   56 },
   { 48,  48,  24,  24,  174,  174,   68 },
   { 52,  52,  26,  26,  204,  102,   42 },
   { 64,  64,  16,  16,  280,  140,   56 },
   { 72,  72,  18,  18,  368,   92,   36 },
   { 80,  80,  20,  20,  456,  114,   48 },
   { 88,  88,  22,  22,  576,  144,   56 },
   { 96,  96,  24,  24,  696,  174,   68 },
   {104, 104,  26,  26,  816,  136,   56 },
   {120, 120,  20,  20, 1050,  175,   68 },
   {132, 132,  22,  22, 1304,  163,   62 },
   {144, 144,  24,  24, 1558,  156,   62 }};


static int _datamatrix_isdigit( char ch )
{
   return '0' <= ch && ch <= '9';
}

static int _datamatrix_encode( const char * szCode, int iLen, unsigned char * pCW )
{
   int i, iPos = 0;

   for( i = 0; i < iLen; i++ )
   {
      if( _datamatrix_isdigit( szCode[ i ] ) && i < iLen - 1 && _datamatrix_isdigit( szCode[ i + 1 ] ) )
      {
         pCW[ iPos++ ] = ( unsigned char ) ( ( szCode[ i ] - '0' ) * 10 + szCode[ i + 1 ] - '0' + PAIR_OF_DIGITS );
         i++;
      }
      else if( ( unsigned char ) szCode[ i ] <= 127 )
      {
         pCW[ iPos++ ] = ( unsigned char ) szCode[ i ] + 1;
      }
      else
      {
         pCW[ iPos++ ] = SHIFT_EXTENDED_ASCII;
         pCW[ iPos++ ] = ( unsigned char ) szCode[ i ] - 127;
      }
   }
   return iPos;
}

static void _reed_solomon_encode( unsigned char * pData, int iDataLen, unsigned char * pEC, int iECLen, int * pPoly, int * pExp, int * pLog, int iMod )
{
   int i, j;
   unsigned char iM;

   for( i = 0; i < iECLen; i++ )
      pEC[ i ] = 0;

   for( i = 0; i < iDataLen; i++ )
   {
      iM = pData[ i ] ^ pEC[ iECLen - 1 ];
      for( j = iECLen - 1; j > 0; j-- )
      {
         if( iM && pPoly[ j ] )
            pEC[ j ] = ( unsigned char ) ( pEC[ j - 1 ] ^ pExp[ ( pLog[ iM ] + pLog[ pPoly[ j ] ] ) % iMod ] );
         else
            pEC[ j ] = pEC[ j - 1 ];
      }
      if( iM && pPoly[ 0 ] )
         pEC[ 0 ] = ( unsigned char ) ( pExp[ ( pLog[ iM ] + pLog[ pPoly[ 0 ] ] ) % iMod ] );
      else
         pEC[ 0 ] = 0;
   }
}

static void _datamatrix_reed_solomon( unsigned char * pData, const DATAMATRIX_SIZE * pSize )
{
   int * pPoly, * pExp, * pLog;
   int i, j, iBits, iMod, iPoly, iECLen, iIndex, iBlocks;

   /* Init Galois field. Parameters: iPoly */
   iPoly = 0x12D;

   j = iPoly;
   for( iBits = 0; j > 1; iBits++ )
      j >>= 1;

   iMod = ( 1 << iBits ) - 1;
   pExp = ( int * ) hb_xgrab( sizeof( int ) * iMod );          /* exponent function */
   pLog = ( int * ) hb_xgrab( sizeof( int ) * ( iMod + 1 ) );  /* logarithm function */
   j = 1;
   for( i = 0; i < iMod; i++ )
   {
      pExp[ i ] = j;
      pLog[ j ] = i;
      j <<= 1;
      if( j & ( 1 << iBits ) )
         j ^= iPoly;
   }

   /* Init Reed-Solomonn encode. Parameters: iECLen, iIndex */
   iECLen = pSize->iBlockErrorSize;
   iIndex = 1;

   pPoly = ( int * ) hb_xgrab( sizeof( int ) * ( iECLen + 1 ) );
   pPoly[ 0 ] = 1;
   for( i = 1; i <= iECLen; i++ )
   {
      pPoly[ i ] = 1;
      for( j = i - 1; j > 0; j-- )
      {
         if( pPoly[ j ] )
            pPoly[ j ] = pExp[ ( pLog[ pPoly[ j ] ] + iIndex ) % iMod ];

         pPoly[ j ] ^= pPoly[ j - 1 ];
      }
      pPoly[ 0 ] = pExp[ ( pLog[ pPoly[ 0 ] ] + iIndex ) % iMod ];
      iIndex++;
   }

   /* Divide data into blocks and do Reed-Solomon encoding for each block */

   iBlocks = ( pSize->iDataSize + 2 ) / pSize->iBlockSize;
   for( i = 0; i < iBlocks; i++ )
   {
      unsigned char  data[ 256 ], ecc[ 80 ];
      int   k = 0;

      /* Copy to temporary buffer */
      for( j = i; j < pSize->iDataSize; j += iBlocks )
         data[ k++ ] = pData[ j ];

      /* Calculate Reed-Solomon ECC for one block */
      _reed_solomon_encode( data, k, ecc, pSize->iBlockErrorSize, pPoly, pExp, pLog, iMod );

      /* Copy ECC to codeword array */
      k = pSize->iBlockErrorSize;
      for( j = i; j < pSize->iBlockErrorSize * iBlocks; j += iBlocks )
         pData[ pSize->iDataSize + j ] = ecc[ --k ];
   }

   hb_xfree( pExp );
   hb_xfree( pLog );
   hb_xfree( pPoly );
}


static void _datamatrix_place_bit( int * pArr, int iPRow, int iPCol, int iR, int iC, int iValue )
{
   if( iR < 0 )
   {
      iR += iPRow;
      iC += 4 - ( ( iPRow + 4 ) % 8 );
   }
   if( iC < 0 )
   {
      iC += iPCol;
      iR += 4 - ( ( iPCol + 4 ) % 8 );
   }
   pArr[ iR * iPCol + iC ] = iValue;
}

static void _datamatrix_place( int * pArr, int iPRow, int iPCol, int iR, int iC, int iIndex )
{
   _datamatrix_place_bit( pArr, iPRow, iPCol, iR - 2, iC - 2, ( iIndex << 3 ) + 7 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iR - 2, iC - 1, ( iIndex << 3 ) + 6 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iR - 1, iC - 2, ( iIndex << 3 ) + 5 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iR - 1, iC - 1, ( iIndex << 3 ) + 4 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iR - 1, iC - 0, ( iIndex << 3 ) + 3 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iR - 0, iC - 2, ( iIndex << 3 ) + 2 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iR - 0, iC - 1, ( iIndex << 3 ) + 1 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iR - 0, iC - 0, ( iIndex << 3 ) + 0 );
}

static void _datamatrix_place_a( int * pArr, int iPRow, int iPCol, int iIndex )
{
   _datamatrix_place_bit( pArr, iPRow, iPCol, iPRow - 1,         0, ( iIndex << 3 ) + 7 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iPRow - 1,         1, ( iIndex << 3 ) + 6 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iPRow - 1,         2, ( iIndex << 3 ) + 5 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         0, iPCol - 2, ( iIndex << 3 ) + 4 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         0, iPCol - 1, ( iIndex << 3 ) + 3 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         1, iPCol - 1, ( iIndex << 3 ) + 2 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         2, iPCol - 1, ( iIndex << 3 ) + 1 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         3, iPCol - 1, ( iIndex << 3 ) + 0 );
}

static void _datamatrix_place_b( int * pArr, int iPRow, int iPCol, int iIndex )
{
   _datamatrix_place_bit( pArr, iPRow, iPCol, iPRow - 3,         0, ( iIndex << 3 ) + 7 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iPRow - 2,         0, ( iIndex << 3 ) + 6 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iPRow - 1,         0, ( iIndex << 3 ) + 5 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         0, iPCol - 4, ( iIndex << 3 ) + 4 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         0, iPCol - 3, ( iIndex << 3 ) + 3 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         0, iPCol - 2, ( iIndex << 3 ) + 2 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         0, iPCol - 1, ( iIndex << 3 ) + 1 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         1, iPCol - 1, ( iIndex << 3 ) + 0 );
}

static void _datamatrix_place_c( int * pArr, int iPRow, int iPCol, int iIndex )
{
   _datamatrix_place_bit( pArr, iPRow, iPCol, iPRow - 3,         0, ( iIndex << 3 ) + 7 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iPRow - 2,         0, ( iIndex << 3 ) + 6 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iPRow - 1,         0, ( iIndex << 3 ) + 5 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         0, iPCol - 2, ( iIndex << 3 ) + 4 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         0, iPCol - 1, ( iIndex << 3 ) + 3 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         1, iPCol - 1, ( iIndex << 3 ) + 2 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         2, iPCol - 1, ( iIndex << 3 ) + 1 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         3, iPCol - 1, ( iIndex << 3 ) + 0 );
}

static void _datamatrix_place_d( int * pArr, int iPRow, int iPCol, int iIndex )
{
   _datamatrix_place_bit( pArr, iPRow, iPCol, iPRow - 1,         0, ( iIndex << 3 ) + 7 );
   _datamatrix_place_bit( pArr, iPRow, iPCol, iPRow - 1, iPCol - 1, ( iIndex << 3 ) + 6 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         0, iPCol - 3, ( iIndex << 3 ) + 5 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         0, iPCol - 2, ( iIndex << 3 ) + 4 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         0, iPCol - 1, ( iIndex << 3 ) + 3 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         1, iPCol - 3, ( iIndex << 3 ) + 2 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         1, iPCol - 2, ( iIndex << 3 ) + 1 );
   _datamatrix_place_bit( pArr, iPRow, iPCol,         1, iPCol - 1, ( iIndex << 3 ) + 0 );
}

static void _datamatrix_do_placement( PHB_BITBUFFER pBits, unsigned char * pCW, const DATAMATRIX_SIZE * pSize )
{
   int * pArr;
   int i, iR, iC, iPRow, iPCol;


   /* Calculate placement size without L-patterns and clock tracks */
   iPRow = pSize->iRow - 2 * ( pSize->iRow / pSize->iRegionRow );
   iPCol = pSize->iCol - 2 * ( pSize->iCol / pSize->iRegionCol );

   pArr = ( int * ) hb_xgrab( sizeof( int ) * iPCol * iPRow );
   hb_xmemset( pArr, 0, sizeof( int ) * iPCol * iPRow );

   /* Generate placement index array */

   i = 1;
   iR = 4;
   iC = 0;
   do
   {
      if( iR == iPRow && iC == 0 )
         _datamatrix_place_a( pArr, iPRow, iPCol, i++ );
      if( iR == iPRow - 2 && iC == 0 && iPCol % 4 )
         _datamatrix_place_b( pArr, iPRow, iPCol, i++ );
      if( iR == iPRow - 2 && iC == 0 && ( iPCol % 8 ) == 4 )
         _datamatrix_place_c( pArr, iPRow, iPCol, i++ );
      if( iR == iPRow + 4 && iC == 2 && ( iPCol % 8 ) == 0  )
         _datamatrix_place_d( pArr, iPRow, iPCol, i++ );

      do
      {
         if( iR < iPRow && iC >= 0 && pArr[ iR * iPCol + iC ] == 0 )
            _datamatrix_place( pArr, iPRow, iPCol, iR, iC, i++ );
         iR -= 2;
         iC += 2;
      }
      while( iR >= 0 && iC < iPCol );

      iR++;
      iC += 3;

      do
      {
         if( iR >= 0 && iC < iPCol && pArr[ iR * iPCol + iC ] == 0 )
            _datamatrix_place( pArr, iPRow, iPCol, iR, iC, i++ );
         iR += 2;
         iC -= 2;
      }
      while( iR < iPRow && iC >= 0 );

      iR += 3;
      iC++;
   }
   while( iR < iPRow || iC < iPCol );

   if( pArr[ iPRow * iPCol - 1 ] == 0 )
      pArr[ iPRow * iPCol - 1 ] = pArr[ iPRow * iPCol - iPCol - 2 ] = 1;


   /* Place codewords */

   for( iR = 0; iR < iPRow; iR++ )
   {
      for( iC = 0; iC < iPCol; iC++ )
      {
         i = pArr[ iR * iPCol + iC ];
         if( i == 1 ||
             ( i > 7 && ( pCW[ ( i >> 3 ) - 1 ] & ( 1 << ( i & 7 ) ) ) ) )
         {
            hb_bitbuffer_set( pBits,
                              ( 1 + iR + 2 * ( iR / ( pSize->iRegionRow - 2 ) ) ) * pSize->iCol +
                              ( 1 + iC + 2 * ( iC / ( pSize->iRegionCol - 2 ) ) ), 1 );
         }
      }
   }
   hb_xfree( pArr );
}

PHB_ZEBRA hb_zebra_create_datamatrix( const char * szCode, HB_SIZE nLen, int iFlags )
{
   PHB_ZEBRA pZebra;
   const DATAMATRIX_SIZE * pSize;
   unsigned char * pCW;
   int        i, j, iDataCount, iErrorSize, iLen = ( int ) nLen;

   pZebra = hb_zebra_create();
   pZebra->iType = HB_ZEBRA_TYPE_DATAMATRIX;

   if( iLen > 3116 )
   {
      pZebra->iError = HB_ZEBRA_ERROR_TOOLARGE;
      return pZebra;
   }

   pCW = ( unsigned char * ) hb_xgrab( sizeof( char ) * iLen * 2 );
   iDataCount = _datamatrix_encode( szCode, iLen, pCW );

   if( iDataCount > 3116 )
   {
      pZebra->iError = HB_ZEBRA_ERROR_TOOLARGE;
      return pZebra;
   }

   pSize = NULL;
   for( i = 0; i < SIZE_COUNT; i++ )
   {
      if( s_size[ i ].iDataSize >= iDataCount )
      {
         if( ( ( iFlags & HB_ZEBRA_FLAG_DATAMATRIX_SQUARE )    && s_size[ i ].iRow == s_size[ i ].iCol ) ||
             ( ( iFlags & HB_ZEBRA_FLAG_DATAMATRIX_RECTANGLE ) && s_size[ i ].iRow != s_size[ i ].iCol ) ||
             ( iFlags & ( HB_ZEBRA_FLAG_DATAMATRIX_SQUARE | HB_ZEBRA_FLAG_DATAMATRIX_RECTANGLE ) ) == 0 )
         {
            pSize = s_size + i;
            break;
         }
      }
   }
   if( ! pSize )
   {
      hb_xfree( pCW );
      pZebra->iError = HB_ZEBRA_ERROR_INVALIDCODE;
      return pZebra;
   }

   iErrorSize = ( pSize->iDataSize + 2 ) / pSize->iBlockSize * pSize->iBlockErrorSize;

   pCW = ( unsigned char * ) hb_xrealloc( pCW, pSize->iDataSize + iErrorSize );
   for( i = iDataCount; i < pSize->iDataSize; i++ )
      pCW[ i ] = PADDING;

   /* Reed-Solomon error correction */
   _datamatrix_reed_solomon( pCW, pSize );

#if 0
   for( i = 0; i < pSize->iDataSize + iErrorSize; i++ )
      HB_TRACE( HB_TR_ALWAYS, ( "cw=%d", pCW[ i ] ) );
#endif

   pZebra->iCol = pSize->iCol;

   pZebra->szCode = hb_strdup( szCode );
   pZebra->pBits = hb_bitbuffer_create();

   /* allocate bitbuffer */
   hb_bitbuffer_set( pZebra->pBits, pSize->iCol * pSize->iRow - 1, 0 );

   /* Draw L-finder pattern and clock track */
   for( j = 0; j < pSize->iRow; j += pSize->iRegionRow )
   {
      for( i = 0; i < pSize->iCol; i++ )
         hb_bitbuffer_set( pZebra->pBits, ( j + pSize->iRegionRow - 1 ) * pSize->iCol + i, 1 );
      for( i = 0; i < pSize->iCol; i += 2 )
         hb_bitbuffer_set( pZebra->pBits, j * pSize->iCol + i, 1 );
   }
   for( i = 0; i < pSize->iCol; i += pSize->iRegionCol )
   {
      for( j = 1; j < pSize->iRow; j++ )
         hb_bitbuffer_set( pZebra->pBits, j * pSize->iCol + i, 1 );
      for( j = 1; j < pSize->iRow; j += 2 )
         hb_bitbuffer_set( pZebra->pBits, j * pSize->iCol + i + pSize->iRegionCol - 1, 1 );
   }

   /* And now the most crazy part - placement */
   _datamatrix_do_placement( pZebra->pBits, pCW, pSize );

   hb_xfree( pCW );
   return pZebra;
}

HB_FUNC( HB_ZEBRA_CREATE_DATAMATRIX )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_STRING );

   if( pItem )
      hb_zebra_ret( hb_zebra_create_datamatrix( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ), hb_parni( 2 ) ) );
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
