/*
 * SHA3 function wrappers (secure hash)
 *
 * Copyright 2015 Viktor Szakats
 * (Harbour wrapper and low-level code adaptation)
 *
 * Low-level SHA3 C code:
 *   https://github.com/gvanas/KeccakCodePackage/blob/4d212774e78bd46758636f6297924a2a1947d04f/Standalone/CompactFIPS202/Keccak-readable-and-compact.c
 * by courtesy of:
 *   Implementation by the Keccak, Keyak and Ketje Teams, namely, Guido Bertoni,
 *   Joan Daemen, Michaël Peeters, Gilles Van Assche and Ronny Van Keer, hereby
 *   denoted as "the implementer".
 *
 *   For more information, feedback or questions, please refer to our websites:
 *   https://keccak.noekeon.org/
 *   https://keyak.noekeon.org/
 *   https://ketje.noekeon.org/
 *
 *   To the extent possible under law, the implementer has waived all copyright
 *   and related or neighboring rights to the source code in this file.
 *   https://creativecommons.org/publicdomain/zero/1.0/
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

#include "hbapi.h"

/* The purpose of this source file is to demonstrate a readable and compact
   implementation of all the Keccak instances approved in the FIPS 202 standard,
   including the hash functions and the extendable-output functions (XOFs).

   We focused on clarity and on source-code compactness,
   rather than on the performance.

   The advantages of this implementation are:
    + The source code is compact, after removing the comments, that is. :-)
    + There are no tables with arbitrary constants.
    + For clarity, the comments link the operations to the specifications using
           the same notation as much as possible.
    + There is no restriction in cryptographic features. In particular,
           the SHAKE128 and SHAKE256 XOFs can produce any output length.
    + The code does not use much RAM, as all operations are done in place.

   The drawbacks of this implementation are:
    - There is no message queue. The whole message must be ready in a buffer.
    - It is not optimized for performance.

   For a more complete set of implementations, please refer to
   the Keccak Code Package at https://github.com/gvanas/KeccakCodePackage

   For more information, please refer to:
    * [Keccak Reference] https://keccak.team/files/Keccak-reference-3.0.pdf
    * [Keccak Specifications Summary] https://keccak.team/keccak_specs_summary.html

   This file uses UTF-8 encoding, as some comments use Greek letters. */

/* Technicalities */

typedef HB_U64 tKeccakLane;

#if ! defined( HB_LITTLE_ENDIAN )

/* Function to load a 64-bit value using the little-endian (LE) convention.
   On a LE platform, this could be greatly simplified using a cast. */
static HB_U64 load64( const HB_U8 * x )
{
   int    i;
   HB_U64 u = 0;

   for( i = 7; i >= 0; --i )
   {
      u <<= 8;
      u  |= x[ i ];
   }
   return u;
}

/* Function to store a 64-bit value using the little-endian (LE) convention.
   On a LE platform, this could be greatly simplified using a cast. */
static void store64( HB_U8 * x, HB_U64 u )
{
   unsigned int i;

   for( i = 0; i < 8; ++i )
   {
      x[ i ] = u;
      u    >>= 8;
   }
}

/* Function to XOR into a 64-bit value using the little-endian (LE) convention.
   On a LE platform, this could be greatly simplified using a cast. */
static void xor64( HB_U8 * x, HB_U64 u )
{
   unsigned int i;

   for( i = 0; i < 8; ++i )
   {
      x[ i ] ^= u;
      u     >>= 8;
   }
}
#endif

/* A readable and compact implementation of the Keccak-f[1600] permutation. */

#define ROL64( a, offset )          ( ( ( ( HB_U64 ) a ) << offset ) ^ ( ( ( HB_U64 ) a ) >> ( 64 - offset ) ) )
#define i( x, y )                   ( ( x ) + 5 * ( y ) )

#if defined( HB_LITTLE_ENDIAN )
   #define readLane( x, y )         ( ( ( tKeccakLane * ) state )[ i( x, y ) ] )
   #define writeLane( x, y, lane )  ( ( ( tKeccakLane * ) state )[ i( x, y ) ] )  = ( lane )
   #define XORLane( x, y, lane )    ( ( ( tKeccakLane * ) state )[ i( x, y ) ] ) ^= ( lane )
#else
   #define readLane( x, y )         load64( ( HB_U8 * ) state + sizeof( tKeccakLane ) * i( x, y ) )
   #define writeLane( x, y, lane )  store64( ( HB_U8 * ) state + sizeof( tKeccakLane ) * i( x, y ), lane )
   #define XORLane( x, y, lane )    xor64( ( HB_U8 * ) state + sizeof( tKeccakLane ) * i( x, y ), lane )
#endif

/* Function that computes the linear feedback shift register (LFSR) used to
   define the round constants (see [Keccak Reference, Section 1.2]). */
static int LFSR86540( HB_U8 * LFSR )
{
   int result = ( ( *LFSR ) & 0x01 ) != 0;

   if( ( ( *LFSR ) & 0x80 ) != 0 )
      /* Primitive polynomial over GF(2): x^8+x^6+x^5+x^4+1 */
      ( *LFSR ) = ( ( *LFSR ) << 1 ) ^ 0x71;
   else
      ( *LFSR ) <<= 1;

   return result;
}

/* Function that computes the Keccak-f[1600] permutation on the given state. */
static void KeccakF1600_StatePermute( void * state )
{
   unsigned int round, x, y, j, t;
   HB_U8        LFSRstate = 0x01;

   for( round = 0; round < 24; round++ )
   {
      {
         /* === θ step (see [Keccak Reference, Section 2.3.2]) === */
         tKeccakLane C[ 5 ], D;

         /* Compute the parity of the columns */
         for( x = 0; x < 5; ++x )
            C[ x ] = readLane( x, 0 ) ^ readLane( x, 1 ) ^ readLane( x, 2 ) ^ readLane( x, 3 ) ^ readLane( x, 4 );
         for( x = 0; x < 5; ++x )
         {
            /* Compute the θ effect for a given column */
            D = C[ ( x + 4 ) % 5 ] ^ ROL64( C[ ( x + 1 ) % 5 ], 1 );
            /* Add the θ effect to the whole column */
            for( y = 0; y < 5; y++ )
               XORLane( x, y, D );
         }
      }

      {
         /* === ρ and π steps (see [Keccak Reference, Sections 2.3.3 and 2.3.4]) === */
         tKeccakLane current, temp;
         /* Start at coordinates (1 0) */
         x       = 1; y = 0;
         current = readLane( x, y );
         /* Iterate over ((0 1)(2 3))^t * (1 0) for 0 ≤ t ≤ 23 */
         for( t = 0; t < 24; t++ )
         {
            /* Compute the rotation constant r = (t+1)(t+2)/2 */
            unsigned int r = ( ( t + 1 ) * ( t + 2 ) / 2 ) % 64;
            /* Compute ((0 1)(2 3)) * (x y) */
            unsigned int Y = ( 2 * x + 3 * y ) % 5; x = y; y = Y;
            /* Swap current and state(x,y), and rotate */
            temp = readLane( x, y );
            writeLane( x, y, ROL64( current, r ) );
            current = temp;
         }
      }

      {
         /* === χ step (see [Keccak Reference, Section 2.3.1]) === */
         tKeccakLane temp[ 5 ];
         for( y = 0; y < 5; y++ )
         {
            /* Take a copy of the plane */
            for( x = 0; x < 5; ++x )
               temp[ x ] = readLane( x, y );
            /* Compute χ on the plane */
            for( x = 0; x < 5; ++x )
               writeLane( x, y, temp[ x ] ^ ( ( ~temp[ ( x + 1 ) % 5 ] ) & temp[ ( x + 2 ) % 5 ] ) );
         }
      }

      {
         /* === ι step (see [Keccak Reference, Section 2.3.5]) === */
         for( j = 0; j < 7; ++j )
         {
            unsigned int bitPosition = ( 1 << j ) - 1;  /* 2^j-1 */
            if( LFSR86540( &LFSRstate ) )
               XORLane( 0, 0, ( tKeccakLane ) 1 << bitPosition );
         }
      }
   }
}

/* A readable and compact implementation of the Keccak sponge functions
   that use the Keccak-f[1600] permutation. */

/* Function to compute the Keccak[r, c] sponge function over a given input.
 * @param  rate            The value of the rate r.
 * @param  capacity        The value of the capacity c.
 * @param  input           Pointer to the input message.
 * @param  inputByteLen    The number of input bytes provided in the input message.
 * @param  delimitedSuffix Bits that will be automatically appended to the end
 *                         of the input message, as in domain separation.
 *                         This is a byte containing from 0 to 7 bits
 *                         These <i>n</i> bits must be in the least significant bit positions
 *                         and must be delimited with a bit 1 at position <i>n</i>
 *                         (counting from 0=LSB to 7=MSB) and followed by bits 0
 *                         from position <i>n</i>+1 to position 7.
 *                         Some examples:
 *                             - If no bits are to be appended, then @a delimitedSuffix must be 0x01.
 *                             - If the 2-bit sequence 0,1 is to be appended (as for SHA3-*), @a delimitedSuffix must be 0x06.
 *                             - If the 4-bit sequence 1,1,1,1 is to be appended (as for SHAKE*), @a delimitedSuffix must be 0x1F.
 *                             - If the 7-bit sequence 1,1,0,1,0,0,0 is to be absorbed, @a delimitedSuffix must be 0x8B.
 * @param  output          Pointer to the buffer where to store the output.
 * @param  outputByteLen   The number of output bytes desired.
 * @pre    One must have r+c=1600 and the rate a multiple of 8 bits in this implementation.
 */

static void Keccak( unsigned int rate, unsigned int capacity, const unsigned char * input, HB_SIZE inputByteLen, unsigned char delimitedSuffix, unsigned char * output, HB_SIZE outputByteLen )
{
   HB_U8        state[ 200 ];
   unsigned int rateInBytes = rate / 8;
   unsigned int blockSize   = 0;
   unsigned int i;

   if( ( ( rate + capacity ) != 1600 ) || ( ( rate % 8 ) != 0 ) )
      return;

   /* === Initialize the state === */
   memset( state, 0, sizeof( state ) );

   /* === Absorb all the input blocks === */
   while( inputByteLen > 0 )
   {
      blockSize = HB_MIN( ( unsigned int ) inputByteLen, rateInBytes );
      for( i = 0; i < blockSize; i++ )
         state[ i ] ^= input[ i ];
      input        += blockSize;
      inputByteLen -= blockSize;

      if( blockSize == rateInBytes )
      {
         KeccakF1600_StatePermute( state );
         blockSize = 0;
      }
   }

   /* === Do the padding and switch to the squeezing phase === */
   /* Absorb the last few bits and add the first bit of padding (which coincides with the delimiter in delimitedSuffix) */
   state[ blockSize ] ^= delimitedSuffix;
   /* If the first bit of padding is at position rate-1, we need a whole new block for the second bit of padding */
   if( ( ( delimitedSuffix & 0x80 ) != 0 ) && ( blockSize == ( rateInBytes - 1 ) ) )
      KeccakF1600_StatePermute( state );
   /* Add the second bit of padding */
   state[ rateInBytes - 1 ] ^= 0x80;
   /* Switch to the squeezing phase */
   KeccakF1600_StatePermute( state );

   /* === Squeeze out all the output blocks === */
   while( outputByteLen > 0 )
   {
      blockSize = HB_MIN( ( unsigned int ) outputByteLen, rateInBytes );
      memcpy( output, state, blockSize );
      output        += blockSize;
      outputByteLen -= blockSize;

      if( outputByteLen > 0 )
         KeccakF1600_StatePermute( state );
   }
}

/* Function to compute SHAKE128 on the input message with any output length. */
static void FIPS202_SHAKE128( const unsigned char * input, HB_SIZE inputByteLen, unsigned char * output, HB_SIZE outputByteLen )
{
   Keccak( 1344, 256, input, inputByteLen, 0x1F, output, outputByteLen );
}

/* Function to compute SHAKE256 on the input message with any output length. */
static void FIPS202_SHAKE256( const unsigned char * input, HB_SIZE inputByteLen, unsigned char * output, HB_SIZE outputByteLen )
{
   Keccak( 1088, 512, input, inputByteLen, 0x1F, output, outputByteLen );
}

/* Function to compute SHA3-224 on the input message. The output length is fixed to 28 bytes. */
static void FIPS202_SHA3_224( const unsigned char * input, HB_SIZE inputByteLen, unsigned char * output )
{
   Keccak( 1152, 448, input, inputByteLen, 0x06, output, 28 );
}

/* Function to compute SHA3-256 on the input message. The output length is fixed to 32 bytes. */
static void FIPS202_SHA3_256( const unsigned char * input, HB_SIZE inputByteLen, unsigned char * output )
{
   Keccak( 1088, 512, input, inputByteLen, 0x06, output, 32 );
}

/* Function to compute SHA3-384 on the input message. The output length is fixed to 48 bytes. */
static void FIPS202_SHA3_384( const unsigned char * input, HB_SIZE inputByteLen, unsigned char * output )
{
   Keccak( 832, 768, input, inputByteLen, 0x06, output, 48 );
}

/* Function to compute SHA3-512 on the input message. The output length is fixed to 64 bytes. */
static void FIPS202_SHA3_512( const unsigned char * input, HB_SIZE inputByteLen, unsigned char * output )
{
   Keccak( 576, 1024, input, inputByteLen, 0x06, output, 64 );
}

HB_FUNC( HB_SHAKE128 )
{
   HB_SIZE nOutSize = hb_parnsdef( 2, 256 ) / 8;

   if( nOutSize > 0 )
   {
      HB_U8 * out = ( HB_U8 * ) hb_xgrab( nOutSize + 1 );

      FIPS202_SHAKE128( ( const HB_U8 * ) hb_parcx( 1 ), hb_parclen( 1 ), out, nOutSize );

      if( hb_parl( 3 ) )
         hb_retclen_buffer( ( char * ) out, sizeof( out ) );
      else
      {
         char * digest = ( char * ) hb_xgrab( ( nOutSize * 2 ) + 1 );
         hb_strtohex( ( char * ) out, nOutSize, digest );
         hb_xfree( out );
         hb_retclen_buffer( digest, nOutSize * 2 );
      }
   }
}

HB_FUNC( HB_SHAKE256 )
{
   HB_SIZE nOutSize = hb_parnsdef( 2, 512 ) / 8;

   if( nOutSize > 0 )
   {
      HB_U8 * out = ( HB_U8 * ) hb_xgrab( nOutSize + 1 );

      FIPS202_SHAKE256( ( const HB_U8 * ) hb_parcx( 1 ), hb_parclen( 1 ), out, nOutSize );

      if( hb_parl( 3 ) )
         hb_retclen_buffer( ( char * ) out, sizeof( out ) );
      else
      {
         char * digest = ( char * ) hb_xgrab( ( nOutSize * 2 ) + 1 );
         hb_strtohex( ( char * ) out, nOutSize, digest );
         hb_xfree( out );
         hb_retclen_buffer( digest, nOutSize * 2 );
      }
   }
}

HB_FUNC( HB_SHA3224 )
{
   HB_U8 out[ 224 / 8 + 1 ];

   FIPS202_SHA3_224( ( const HB_U8 * ) hb_parcx( 1 ), hb_parclen( 1 ), out );

   if( hb_parl( 2 ) )
      hb_retclen( ( char * ) out, sizeof( out ) - 1 );
   else
   {
      char digest[ ( ( sizeof( out ) - 1 ) * 2 ) + 1 ];
      hb_strtohex( ( char * ) out, sizeof( out ) - 1, digest );
      hb_retclen( digest, ( sizeof( out ) - 1 ) * 2 );
   }
}

HB_FUNC( HB_SHA3256 )
{
   HB_U8 out[ 256 / 8 + 1 ];

   FIPS202_SHA3_256( ( const HB_U8 * ) hb_parcx( 1 ), hb_parclen( 1 ), out );

   if( hb_parl( 2 ) )
      hb_retclen( ( char * ) out, sizeof( out ) - 1 );
   else
   {
      char digest[ ( ( sizeof( out ) - 1 ) * 2 ) + 1 ];
      hb_strtohex( ( char * ) out, sizeof( out ) - 1, digest );
      hb_retclen( digest, ( sizeof( out ) - 1 ) * 2 );
   }
}

HB_FUNC( HB_SHA3384 )
{
   HB_U8 out[ 384 / 8 + 1 ];

   FIPS202_SHA3_384( ( const HB_U8 * ) hb_parcx( 1 ), hb_parclen( 1 ), out );

   if( hb_parl( 2 ) )
      hb_retclen( ( char * ) out, sizeof( out ) - 1 );
   else
   {
      char digest[ ( ( sizeof( out ) - 1 ) * 2 ) + 1 ];
      hb_strtohex( ( char * ) out, sizeof( out ) - 1, digest );
      hb_retclen( digest, ( sizeof( out ) - 1 ) * 2 );
   }
}

HB_FUNC( HB_SHA3512 )
{
   HB_U8 out[ 512 / 8 + 1 ];

   FIPS202_SHA3_512( ( const HB_U8 * ) hb_parcx( 1 ), hb_parclen( 1 ), out );

   if( hb_parl( 2 ) )
      hb_retclen( ( char * ) out, sizeof( out ) - 1 );
   else
   {
      char digest[ ( ( sizeof( out ) - 1 ) * 2 ) + 1 ];
      hb_strtohex( ( char * ) out, sizeof( out ) - 1, digest );
      hb_retclen( digest, ( sizeof( out ) - 1 ) * 2 );
   }
}
