/*
 * BLAKE2b function wrapper (fast secure hash)
 *
 * Copyright 2015 Viktor Szakats
 * (Harbour wrapper and low-level code adaptation)
 *
 * Low-level BLAKE2 C code courtesy of
 *   2015-01-25  Markku-Juhani O. Saarinen <mjos@iki.fi>
 *   A simple BLAKE2b Reference Implementation
 *   as of
 *      https://github.com/mjosaarinen/blake2_mjosref/commit/861edb3c33b5d7eaa2bddea5aaa13f6754bea10a
 *   License: CC0 1.0 Universal <https://creativecommons.org/publicdomain/zero/1.0/>
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

/* https://tools.ietf.org/html/draft-saarinen-blake2-03 */

#include "hbapi.h"

/* state context */
typedef struct
{
   HB_U8  b[ 128 ];                     /* input buffer */
   HB_U64 h[ 8 ];                       /* chained state */
   HB_U64 t[ 2 ];                       /* total number of bytes */
   HB_SIZE c;                           /* pointer for b[] */
   HB_SIZE outlen;                      /* digest size */
} blake2b_ctx;

/* cyclic right rotation */
#define _HB_ROTR64( x, y )  ( ( ( x ) >> ( y ) ) ^ ( ( x ) << ( 64 - ( y ) ) ) )

/* G Mixing function */
#define B2B_G( a, b, c, d, x, y )  {           \
   v[ a ] = v[ a ] + v[ b ] + x;               \
   v[ d ] = _HB_ROTR64( v[ d ] ^ v[ a ], 32 ); \
   v[ c ] = v[ c ] + v[ d ];                   \
   v[ b ] = _HB_ROTR64( v[ b ] ^ v[ c ], 24 ); \
   v[ a ] = v[ a ] + v[ b ] + y;               \
   v[ d ] = _HB_ROTR64( v[ d ] ^ v[ a ], 16 ); \
   v[ c ] = v[ c ] + v[ d ];                   \
   v[ b ] = _HB_ROTR64( v[ b ] ^ v[ c ], 63 ); }

/* Initialization Vector */
static const HB_U64 blake2b_iv[ 8 ] =
{
   0x6A09E667F3BCC908, 0xBB67AE8584CAA73B,
   0x3C6EF372FE94F82B, 0xA54FF53A5F1D36F1,
   0x510E527FADE682D1, 0x9B05688C2B3E6C1F,
   0x1F83D9ABFB41BD6B, 0x5BE0CD19137E2179
};

/* Compression function. "last" flag indicates last block. */
static void blake2b_compress( blake2b_ctx * ctx, int last )
{
   static const HB_U8 sigma[ 12 ][ 16 ] = {
      { 0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15 },
      { 14, 10, 4,  8,  9,  15, 13, 6,  1,  12, 0,  2,  11, 7,  5,  3  },
      { 11, 8,  12, 0,  5,  2,  15, 13, 10, 14, 3,  6,  7,  1,  9,  4  },
      { 7,  9,  3,  1,  13, 12, 11, 14, 2,  6,  5,  10, 4,  0,  15, 8  },
      { 9,  0,  5,  7,  2,  4,  10, 15, 14, 1,  11, 12, 6,  8,  3,  13 },
      { 2,  12, 6,  10, 0,  11, 8,  3,  4,  13, 7,  5,  15, 14, 1,  9  },
      { 12, 5,  1,  15, 14, 13, 4,  10, 0,  7,  6,  3,  9,  2,  8,  11 },
      { 13, 11, 7,  14, 12, 1,  3,  9,  5,  0,  15, 4,  8,  6,  2,  10 },
      { 6,  15, 14, 9,  11, 3,  0,  8,  12, 2,  13, 7,  1,  4,  10, 5  },
      { 10, 2,  8,  4,  7,  6,  1,  5,  15, 11, 9,  14, 3,  12, 13, 0  },
      { 0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15 },
      { 14, 10, 4,  8,  9,  15, 13, 6,  1,  12, 0,  2,  11, 7,  5,  3  }
   };

   int    i;
   HB_U64 v[ 16 ], m[ 16 ];

   for( i = 0; i < 8; i++ )             /* init work variables */
   {
      v[ i ]     = ctx->h[ i ];
      v[ i + 8 ] = blake2b_iv[ i ];
   }

   v[ 12 ] ^= ctx->t[ 0 ];              /* low 64 bits of offset */
   v[ 13 ] ^= ctx->t[ 1 ];              /* high 64 bits */
   if( last )                           /* last block flag set ? */
      v[ 14 ] = ~v[ 14 ];

   for( i = 0; i < 16; i++ )            /* get little-endian words */
      m[ i ] = HB_GET_UINT64( &ctx->b[ 8 * i ] );

   for( i = 0; i < 12; i++ )            /* twelve rounds */
   {
      B2B_G( 0, 4,  8, 12, m[ sigma[ i ][  0 ] ], m[ sigma[ i ][  1 ] ] );
      B2B_G( 1, 5,  9, 13, m[ sigma[ i ][  2 ] ], m[ sigma[ i ][  3 ] ] );
      B2B_G( 2, 6, 10, 14, m[ sigma[ i ][  4 ] ], m[ sigma[ i ][  5 ] ] );
      B2B_G( 3, 7, 11, 15, m[ sigma[ i ][  6 ] ], m[ sigma[ i ][  7 ] ] );
      B2B_G( 0, 5, 10, 15, m[ sigma[ i ][  8 ] ], m[ sigma[ i ][  9 ] ] );
      B2B_G( 1, 6, 11, 12, m[ sigma[ i ][ 10 ] ], m[ sigma[ i ][ 11 ] ] );
      B2B_G( 2, 7,  8, 13, m[ sigma[ i ][ 12 ] ], m[ sigma[ i ][ 13 ] ] );
      B2B_G( 3, 4,  9, 14, m[ sigma[ i ][ 14 ] ], m[ sigma[ i ][ 15 ] ] );
   }

   for( i = 0; i < 8; ++i )
      ctx->h[ i ] ^= v[ i ] ^ v[ i + 8 ];
}

/* update with new data */
static void blake2b_update( blake2b_ctx * ctx,
                            const void * in, HB_SIZE inlen ) /* data bytes */
{
   HB_SIZE i;

   for( i = 0; i < inlen; i++ )
   {
      if( ctx->c == 128 )               /* buffer full ? */
      {
         ctx->t[ 0 ] += ctx->c;         /* add counters */
         if( ctx->t[ 0 ] < ctx->c )     /* carry overflow ? */
            ctx->t[ 1 ]++;              /* high word */
         blake2b_compress( ctx, 0 );    /* compress (not last) */
         ctx->c = 0;                    /* counter to zero */
      }
      ctx->b[ ctx->c++ ] = ( ( const HB_U8 * ) in )[ i ];
   }
}

/* Initialize the state. key is optional */
static int blake2b_init( blake2b_ctx * ctx, HB_SIZE outlen,
                         const void * key, HB_SIZE keylen ) /* (keylen=0: no key) */
{
   HB_SIZE i;

   if( outlen == 0 || outlen > 64 || keylen > 64 )
      return -1;                        /* illegal parameters */

   for( i = 0; i < 8; i++ )             /* state, "param block" */
      ctx->h[ i ] = blake2b_iv[ i ];
   ctx->h[ 0 ] ^= 0x01010000 ^ ( keylen << 8 ) ^ outlen;

   ctx->t[ 0 ] = 0;                     /* input count low word */
   ctx->t[ 1 ] = 0;                     /* input count high word */
   ctx->c      = 0;                     /* pointer within buffer */
   ctx->outlen = outlen;

   for( i = keylen; i < 128; i++ )      /* zero input block */
      ctx->b[ i ] = 0;
   if( keylen > 0 )
   {
      blake2b_update( ctx, key, keylen );
      ctx->c = 128;                     /* at the end */
   }

   return 0;
}

/* finalize */
static void blake2b_final( blake2b_ctx * ctx, void * out )
{
   HB_SIZE i;

   ctx->t[ 0 ] += ctx->c;               /* mark last block offset */
   if( ctx->t[ 0 ] < ctx->c )           /* carry overflow */
      ctx->t[ 1 ]++;                    /* high word */

   while( ctx->c < 128 )                /* fill up with zeros */
      ctx->b[ ctx->c++ ] = 0;
   blake2b_compress( ctx, 1 );          /* final block flag = 1 */

   /* little endian convert and store */
   for( i = 0; i < ctx->outlen; i++ )
   {
      ( ( HB_U8 * ) out )[ i ] =
         ( ctx->h[ i >> 3 ] >> ( 8 * ( i & 7 ) ) ) & 0xFF;
   }
}

/* convenience function for all-in-one computation */
static int blake2b( void * out, HB_SIZE outlen,
                    const void * key, HB_SIZE keylen,
                    const void * in, HB_SIZE inlen )
{
   blake2b_ctx ctx;

   if( blake2b_init( &ctx, outlen, key, keylen ) )
      return -1;
   blake2b_update( &ctx, in, inlen );
   blake2b_final( &ctx, out );

   return 0;
}

#define BLAKE2B_OUTBYTES  64

/* hb_BLAKE2b( <cIn>, [<cKey>], [<nSize=64>], [<lBinary=.F.>] ) --> <cHash> */
HB_FUNC( HB_BLAKE2B )
{
   const char * pszStr = hb_parc( 1 );

   if( pszStr )
   {
      int   iHashLen = hb_parnidef( 3, BLAKE2B_OUTBYTES );
      HB_U8 out[ BLAKE2B_OUTBYTES ];

      if( iHashLen < 1 )
         iHashLen = 1;
      else if( iHashLen > BLAKE2B_OUTBYTES )
         iHashLen = BLAKE2B_OUTBYTES;

      if( blake2b( out, iHashLen, hb_parc( 2 ), hb_parclen( 2 ), pszStr, hb_parclen( 1 ) ) == 0 )
      {
         if( ! hb_parl( 4 ) )
         {
            char digest[ ( sizeof( out ) * 2 ) + 1 ];
            hb_strtohex( ( char * ) out, iHashLen, digest );
            hb_retclen( digest, iHashLen * 2 );
         }
         else
            hb_retclen( ( char * ) out, iHashLen );

         return;
      }
   }

   hb_retc_null();
}
