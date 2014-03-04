/*
 * Harbour Project source code:
 *    Harbour MD5 Support
 *
 * Copyright 2004 Dmitry V. Korzhov <dk@april26.spb.ru>
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    updated for current Harbour code, other then x86@32 machines,
 *    files and buffers longer then 2^32 and some fixes
 *
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

/*
   MD5 digest (based on RFC 1321 only). Harbour implementation

   PRG functions:

   hb_MD5( <cString> ) -> <cMD5>
         Calculates RFC 1321 MD5 digest (128-bit checksum)
      Parameters:
         <cString>   - string variable to calculate MD5
      Returns:
         ASCII hex MD5 digest as 32-byte string
         empty string on error

   hb_MD5File( <cFileName> ) -> <cMD5>
         Calculates RFC 1321 MD5 digest (128-bit checksum) of a file contents
         (file size is limited by OS limits only)
      Parameters:
         <cFileName> - file name
      Returns:
         ASCII hex MD5 digest as 32-byte string
         empty string on error

 */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbchksum.h"

/* MD5 buffer */
typedef struct
{
   HB_U32  accum[ 4 ];
   HB_BYTE buf[ 64 ];
} MD5_BUF;

/*
   Pseudofunctions;
   A[x] - accumulators[4]
   T[x] - Value table[64]
   X[x] - buffer[16]
*/
#define ROTL(x,n) ((x << n) | (x >> (32 - n)))
#define PF1(a,b,c,d,k,s,i) \
   A[a] = A[a] + ((A[b] & A[c]) | ((~A[b]) & A[d])) + X[k] + T[i]; \
   A[a] = ROTL(A[a], s) + A[b]
#define PF2(a,b,c,d,k,s,i) \
   A[a] = A[a] + ((A[b] & A[d]) | (A[c] & (~A[d]))) + X[k] + T[i]; \
   A[a] = ROTL(A[a], s) + A[b]
#define PF3(a,b,c,d,k,s,i) \
   A[a] = A[a] + (A[b] ^ A[c] ^ A[d]) + X[k] + T[i]; \
   A[a] = ROTL(A[a], s) + A[b]
#define PF4(a,b,c,d,k,s,i) \
   A[a] = A[a] + (A[c] ^(A[b] | (~A[d]))) + X[k] + T[i]; \
   A[a] = ROTL(A[a], s) + A[b]

/* Defines for file ops */
#define MAX_FBUF  0x20000 /* file read buffer size, MUST be 64*n */

/* Static data */
static const HB_U32 T[ 64 ] = {
   0xD76AA478, 0xE8C7B756, 0x242070DB, 0xC1BDCEEE,
   0xF57C0FAF, 0x4787C62A, 0xA8304613, 0xFD469501,
   0x698098D8, 0x8B44F7AF, 0xFFFF5BB1, 0x895CD7BE,
   0x6B901122, 0xFD987193, 0xA679438E, 0x49B40821,
   0xF61E2562, 0xC040B340, 0x265E5A51, 0xE9B6C7AA,
   0xD62F105D, 0x02441453, 0xD8A1E681, 0xE7D3FBC8,
   0x21E1CDE6, 0xC33707D6, 0xF4D50D87, 0x455A14ED,
   0xA9E3E905, 0xFCEFA3F8, 0x676F02D9, 0x8D2A4C8A,
   0xFFFA3942, 0x8771F681, 0x6D9D6122, 0xFDE5380C,
   0xA4BEEA44, 0x4BDECFA9, 0xF6BB4B60, 0xBEBFBC70,
   0x289b7EC6, 0xEAA127FA, 0xD4EF3085, 0x04881D05,
   0xD9D4D039, 0xE6DB99E5, 0x1FA27CF8, 0xC4AC5665,
   0xF4292244, 0x432AFF97, 0xAB9423A7, 0xFC93A039,
   0x655b59C3, 0x8F0CCC92, 0xFFEFF47D, 0x85845DD1,
   0x6Fa87E4F, 0xFE2CE6e0, 0xA3014314, 0x4E0811A1,
   0xF7537E82, 0xBD3AF235, 0x2AD7D2BB, 0xEB86D391
};

static void hb_md5go( MD5_BUF * md5 )
{
   HB_U32 X[ 16 ], A[ 4 ];
   HB_BYTE * ptr;
   int i;

   /* copy accumulators first */
   memcpy( A, md5->accum, sizeof( A ) );

   /* fill buffer */
   for( i = 0, ptr = md5->buf; i < 16; i++, ptr += 4 )
      X[ i ] = HB_GET_LE_UINT32( ptr );

   /* process buffer */
   PF1( 0, 1, 2, 3,  0,  7,  0 );
   PF1( 3, 0, 1, 2,  1, 12,  1 );
   PF1( 2, 3, 0, 1,  2, 17,  2 );
   PF1( 1, 2, 3, 0,  3, 22,  3 );
   PF1( 0, 1, 2, 3,  4,  7,  4 );
   PF1( 3, 0, 1, 2,  5, 12,  5 );
   PF1( 2, 3, 0, 1,  6, 17,  6 );
   PF1( 1, 2, 3, 0,  7, 22,  7 );
   PF1( 0, 1, 2, 3,  8,  7,  8 );
   PF1( 3, 0, 1, 2,  9, 12,  9 );
   PF1( 2, 3, 0, 1, 10, 17, 10 );
   PF1( 1, 2, 3, 0, 11, 22, 11 );
   PF1( 0, 1, 2, 3, 12,  7, 12 );
   PF1( 3, 0, 1, 2, 13, 12, 13 );
   PF1( 2, 3, 0, 1, 14, 17, 14 );
   PF1( 1, 2, 3, 0, 15, 22, 15 );
   PF2( 0, 1, 2, 3,  1,  5, 16 );
   PF2( 3, 0, 1, 2,  6,  9, 17 );
   PF2( 2, 3, 0, 1, 11, 14, 18 );
   PF2( 1, 2, 3, 0,  0, 20, 19 );
   PF2( 0, 1, 2, 3,  5,  5, 20 );
   PF2( 3, 0, 1, 2, 10,  9, 21 );
   PF2( 2, 3, 0, 1, 15, 14, 22 );
   PF2( 1, 2, 3, 0,  4, 20, 23 );
   PF2( 0, 1, 2, 3,  9,  5, 24 );
   PF2( 3, 0, 1, 2, 14,  9, 25 );
   PF2( 2, 3, 0, 1,  3, 14, 26 );
   PF2( 1, 2, 3, 0,  8, 20, 27 );
   PF2( 0, 1, 2, 3, 13,  5, 28 );
   PF2( 3, 0, 1, 2,  2,  9, 29 );
   PF2( 2, 3, 0, 1,  7, 14, 30 );
   PF2( 1, 2, 3, 0, 12, 20, 31 );
   PF3( 0, 1, 2, 3,  5,  4, 32 );
   PF3( 3, 0, 1, 2,  8, 11, 33 );
   PF3( 2, 3, 0, 1, 11, 16, 34 );
   PF3( 1, 2, 3, 0, 14, 23, 35 );
   PF3( 0, 1, 2, 3,  1,  4, 36 );
   PF3( 3, 0, 1, 2,  4, 11, 37 );
   PF3( 2, 3, 0, 1,  7, 16, 38 );
   PF3( 1, 2, 3, 0, 10, 23, 39 );
   PF3( 0, 1, 2, 3, 13,  4, 40 );
   PF3( 3, 0, 1, 2,  0, 11, 41 );
   PF3( 2, 3, 0, 1,  3, 16, 42 );
   PF3( 1, 2, 3, 0,  6, 23, 43 );
   PF3( 0, 1, 2, 3,  9,  4, 44 );
   PF3( 3, 0, 1, 2, 12, 11, 45 );
   PF3( 2, 3, 0, 1, 15, 16, 46 );
   PF3( 1, 2, 3, 0,  2, 23, 47 );
   PF4( 0, 1, 2, 3,  0,  6, 48 );
   PF4( 3, 0, 1, 2,  7, 10, 49 );
   PF4( 2, 3, 0, 1, 14, 15, 50 );
   PF4( 1, 2, 3, 0,  5, 21, 51 );
   PF4( 0, 1, 2, 3, 12,  6, 52 );
   PF4( 3, 0, 1, 2,  3, 10, 53 );
   PF4( 2, 3, 0, 1, 10, 15, 54 );
   PF4( 1, 2, 3, 0,  1, 21, 55 );
   PF4( 0, 1, 2, 3,  8,  6, 56 );
   PF4( 3, 0, 1, 2, 15, 10, 57 );
   PF4( 2, 3, 0, 1,  6, 15, 58 );
   PF4( 1, 2, 3, 0, 13, 21, 59 );
   PF4( 0, 1, 2, 3,  4,  6, 60 );
   PF4( 3, 0, 1, 2, 11, 10, 61 );
   PF4( 2, 3, 0, 1,  2, 15, 62 );
   PF4( 1, 2, 3, 0,  9, 21, 63 );

   /* Update accumulators */
   md5->accum[ 0 ] += A[ 0 ];
   md5->accum[ 1 ] += A[ 1 ];
   md5->accum[ 2 ] += A[ 2 ];
   md5->accum[ 3 ] += A[ 3 ];
}

static void hb_md5accinit( HB_U32 accum[] )
{
   /* fill initial accumulator state */
   accum[ 0 ] = 0x67452301;
   accum[ 1 ] = 0xEFCDAB89;
   accum[ 2 ] = 0x98BADCFE;
   accum[ 3 ] = 0x10325476;
}

static void hb_md5val( HB_U32 accum[], char * md5val )
{
   int i, n;

   for( i = 0; i < 4; i++ )
   {
      for( n = 0; n < 4; n++ )
         *md5val++ = ( char ) ( ( accum[ i ] >> ( n << 3 ) ) & 0xFF );
   }
}

/*
   Parameters:
      data     - input byte stream
      datalen  - input stream length
      digest   - raw (unformatted) MD5 digest buffer
                 (at least 16 bytes long)
 */
void hb_md5( const void * data, HB_SIZE nLen, char * digest )
{
   const unsigned char * ucdata = ( const unsigned char * ) data;
   HB_UCHAR buf[ 128 ];
   MD5_BUF md5;
   HB_ISIZ i, n;

   /* perform startup procedures */
   hb_md5accinit( md5.accum );
   /* count full 512bit blocks in data*/
   n = nLen >> 6;
   /* process full blocks */
   for( i = 0; i < n; i++, ucdata += 64 )
   {
      memcpy( md5.buf, ucdata, 64 );
      hb_md5go( &md5 );
   }
   /* prepare additional block(s) */
   memset( buf, 0, sizeof( buf ) );
   n = nLen & 63;
   if( n )
      memcpy( buf, ucdata, n );
   buf[ n ] = 0x80;
   /* count bits length */
   i = 56;
   if( n >= 56 )
   {
      i += 64;
      memcpy( md5.buf, buf, 64 );
      hb_md5go( &md5 );
   }
   buf[ i++ ] = ( HB_UCHAR ) ( ( nLen << 3 ) & 0xF8 );
   nLen >>= 5;
   for( n = 7; n; --n )
   {
      buf[ i++ ] = ( HB_UCHAR ) ( nLen & 0xFF );
      nLen >>= 8;
   }
   memcpy( md5.buf, buf + i - 64, 64 );
   hb_md5go( &md5 );
   /* write digest */
   hb_md5val( md5.accum, digest );
}

/*
   Parameters:
      pFile    - file handle
      digest   - raw (unformatted) MD5 digest buffer
                 (at least 16 bytes long)
 */
HB_BOOL hb_md5file( const char * pszFileName, char * digest )
{
   PHB_FILE pFile = hb_fileExtOpen( pszFileName, NULL,
                                    FO_READ | FO_SHARED | FO_PRIVATE |
                                    FXO_SHARELOCK | FXO_NOSEEKPOS,
                                    NULL, NULL );
   if( pFile != NULL )
   {
      MD5_BUF md5;
      HB_SIZE n;
      int i;
      HB_FOFFSET flen = 0;
      HB_UCHAR buf[ 128 ];
      HB_BYTE * readbuf = ( HB_BYTE * ) hb_xgrab( MAX_FBUF );

      hb_md5accinit( md5.accum );
      n = hb_fileRead( pFile, readbuf, MAX_FBUF, -1 );
      flen += n;
      while( n == MAX_FBUF )
      {
         for( i = 0; i < ( MAX_FBUF >> 6 ); i++ )
         {
            memcpy( md5.buf, readbuf + ( i << 6 ), 64 );
            hb_md5go( &md5 );
         }
         n = hb_fileRead( pFile, readbuf, MAX_FBUF, -1 );
         flen += n;
      }
      hb_fileClose( pFile );
      i = 0;
      while( n > 64 )
      {
         memcpy( md5.buf, readbuf + i, 64 );
         hb_md5go( &md5 );
         i += 64;
         n -= 64;
      }
      memset( buf, 0, sizeof( buf ) );
      if( n )
         memcpy( buf, readbuf + i, n );
      buf[ n ] = 0x80;
      i = 56;
      if( n >= 56 )
      {
         i += 64;
         memcpy( md5.buf, buf, 64 );
         hb_md5go( &md5 );
      }
      buf[ i++ ] = ( HB_UCHAR ) ( ( flen << 3 ) & 0xF8 );
      flen >>= 5;
      for( n = 7; n; --n )
      {
         buf[ i++ ] = ( HB_UCHAR ) ( flen & 0xFF );
         flen >>= 8;
      }
      memcpy( md5.buf, buf + i - 64, 64 );
      hb_md5go( &md5 );
      hb_md5val( md5.accum, digest );
      hb_xfree( readbuf );

      return HB_TRUE;
   }
   return HB_FALSE;
}

HB_FUNC( HB_MD5 )
{
   const char * pszStr = hb_parc( 1 );

   if( pszStr )
   {
      HB_SIZE nLen = hb_parclen( 1 );
      char dststr[ 16 ];

      hb_md5( pszStr, nLen, dststr );

      if( ! hb_parl( 2 ) )
      {
         char digest[ ( sizeof( dststr ) * 2 ) + 1 ];
         hb_strtohex( dststr, sizeof( dststr ), digest );
         hb_retclen( digest, HB_SIZEOFARRAY( digest ) - 1 );
      }
      else
         hb_retclen( dststr, HB_SIZEOFARRAY( dststr ) );
   }
   else
      hb_retc_null();  /* return empty string on wrong call */
}

HB_FUNC( HB_MD5FILE )
{
   const char * pszFileName = hb_parc( 1 );
   char dststr[ 16 ];

   if( pszFileName && hb_md5file( pszFileName, dststr ) )
   {
      if( ! hb_parl( 2 ) )
      {
         char digest[ ( sizeof( dststr ) * 2 ) + 1 ];
         hb_strtohex( dststr, sizeof( dststr ), digest );
         hb_retclen( digest, HB_SIZEOFARRAY( digest ) - 1 );
      }
      else
         hb_retclen( dststr, HB_SIZEOFARRAY( dststr ) );
   }
   else
      hb_retc_null(); /* return empty string on wrong call */
}
