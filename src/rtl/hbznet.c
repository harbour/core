/*
 * Harbour Project source code:
 *    ZLIB compression for Harbour stream sockets
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#define _HB_ZNET_INTERNAL_

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbsocket.h"
#include "hbbfish.h"
#include "hbznet.h"
#include "hbzlib.ch"

#include <zlib.h>

typedef struct _HB_ZNETSTREAM
{
   z_stream rd;         /* input stream */
   z_stream wr;         /* output stream */
   int      err;        /* error code for last stream operation */
   int      crypt;      /* encryption */
   uInt     crypt_in;   /* number of encrypted characters in buffer  */
   uInt     crypt_size; /* size of encrypted block */
   uInt     skip_in;    /* encryption block padding bytes */
   uInt     skip_out;   /* room for block size */
   Bytef *  crypt_out;  /* block size offset for encrypted blocks */
   Bytef *  inbuf;      /* input buffer */
   Bytef *  outbuf;     /* output buffer */
   HB_BLOWFISH * bf;
}
HB_ZNETSTREAM;

#define HB_ZNET_BUFSIZE 16384

#if MAX_MEM_LEVEL >= 8
#  define HB_ZNET_MEM_LEVEL   8
#else
#  define HB_ZNET_MEM_LEVEL   MAX_MEM_LEVEL
#endif

/* return status of last compression/decompression operation */
int hb_znetError( PHB_ZNETSTREAM pStream )
{
   return pStream->err;
}

/* release stream structure
 */
void hb_znetClose( PHB_ZNETSTREAM pStream )
{
   if( pStream->inbuf )
      hb_xfree( pStream->inbuf );

   if( pStream->outbuf )
      hb_xfree( pStream->outbuf );

   if( pStream->bf )
      hb_xfree( pStream->bf );

   deflateEnd( &pStream->wr );
   inflateEnd( &pStream->rd );

   hb_xfree( pStream );
}

/* create new stream structure
 */
PHB_ZNETSTREAM hb_znetOpen( int level, int strategy )
{
   PHB_ZNETSTREAM pStream = ( PHB_ZNETSTREAM ) hb_xgrab( sizeof( HB_ZNETSTREAM ) );

   memset( pStream, 0, sizeof( HB_ZNETSTREAM ) );

   if( level != Z_DEFAULT_COMPRESSION &&
       !( level >= Z_NO_COMPRESSION && level <= Z_BEST_COMPRESSION ) )
      level = Z_DEFAULT_COMPRESSION;

   if( strategy != Z_FILTERED     &&
#if defined( Z_RLE )
       strategy != Z_RLE          &&
#endif
#if defined( Z_FIXED )
       strategy != Z_FIXED        &&
#endif
       strategy != Z_HUFFMAN_ONLY )
      strategy = Z_DEFAULT_STRATEGY;

   if( deflateInit2( &pStream->wr, level,
                     Z_DEFLATED, -MAX_WBITS, HB_ZNET_MEM_LEVEL, strategy ) == Z_OK )
   {
      pStream->wr.next_out = pStream->outbuf = ( Bytef * ) hb_xgrab( HB_ZNET_BUFSIZE );
      pStream->wr.avail_out = HB_ZNET_BUFSIZE;

      pStream->rd.next_in = pStream->inbuf = ( Bytef * ) hb_xgrab( HB_ZNET_BUFSIZE );
      if( inflateInit2( &pStream->rd, -MAX_WBITS ) == Z_OK )
         return pStream;
   }

   hb_znetClose( pStream );
   return NULL;
}

/* set encryption key
 */
void hb_znetEncryptKey( PHB_ZNETSTREAM pStream, const void * keydata, int keylen )
{
   if( pStream->crypt == 0 )
   {
      pStream->crypt = 1;

      /* initialize encryption key */
      pStream->bf = ( HB_BLOWFISH * ) hb_xgrab( sizeof( HB_BLOWFISH ) );
      hb_blowfishInit( pStream->bf, keydata, keylen );

      /* initialize input buffer */
      pStream->skip_in = 0;
      pStream->crypt_size = 0;
      pStream->crypt_in = pStream->rd.avail_in;
      pStream->rd.avail_in = 0;

      /* initialize output buffer */
      pStream->crypt_out = pStream->wr.next_out;
      pStream->wr.next_out += 2;
      if( pStream->wr.avail_out < 2 )
         pStream->skip_out = 2 - pStream->wr.avail_out;
      else
         pStream->skip_out = 0;
      pStream->wr.avail_out -= 2 - pStream->skip_out;
   }
}

static void hb_znetDecrypt( PHB_ZNETSTREAM pStream, Bytef * data )
{
   HB_U32 xl, xr;

   xl = HB_GET_BE_UINT32( data );
   xr = HB_GET_BE_UINT32( data + 4 );
   hb_blowfishDecrypt( pStream->bf, &xl, &xr );
   HB_PUT_BE_UINT32( data, xl );
   HB_PUT_BE_UINT32( data + 4, xr );
}

static void hb_znetEncrypt( PHB_ZNETSTREAM pStream, Bytef * data )
{
   HB_U32 xl, xr;

   xl = HB_GET_BE_UINT32( data );
   xr = HB_GET_BE_UINT32( data + 4 );
   hb_blowfishEncrypt( pStream->bf, &xl, &xr );
   HB_PUT_BE_UINT32( data, xl );
   HB_PUT_BE_UINT32( data + 4, xr );
}

/* read data using stream structure
 */
long hb_znetRead( PHB_ZNETSTREAM pStream, HB_SOCKET sd, void * buffer, long len, HB_MAXINT timeout )
{
   long rec = 0;

   pStream->rd.next_out = ( Bytef * ) buffer;
   pStream->rd.avail_out = ( uInt ) len;
   pStream->err = Z_OK;

   while( pStream->rd.avail_out )
   {
      if( pStream->rd.avail_in == 0 && pStream->err == Z_BUF_ERROR )
      {
         if( pStream->skip_in )
         {
            pStream->rd.next_in += pStream->skip_in;
            pStream->skip_in = 0;
         }
         if( pStream->crypt_in && pStream->rd.next_in > pStream->inbuf )
            memmove( pStream->inbuf, pStream->rd.next_in, pStream->crypt_in );
         pStream->rd.next_in = pStream->inbuf;

         if( ! pStream->crypt || pStream->crypt_in < 8 )
         {
            if( pStream->rd.avail_out != ( uInt ) len )
               timeout = 0;
            rec = hb_socketRecv( sd, pStream->inbuf + pStream->crypt_in, HB_ZNET_BUFSIZE - pStream->crypt_in, 0, timeout );
            if( rec <= 0 )
               break;
         }

         if( pStream->crypt )
         {
            pStream->crypt_in += rec;
            if( pStream->crypt_size == 0 )
            {
               if( pStream->crypt_in >= 8 )
               {
                  hb_znetDecrypt( pStream, pStream->rd.next_in );
                  pStream->crypt_size = HB_GET_BE_UINT16( pStream->rd.next_in );
                  pStream->rd.next_in += 2;
                  pStream->crypt_in -= 8;
                  rec = HB_MIN( pStream->crypt_size, 6 );
                  pStream->crypt_size -= ( uInt ) rec;
                  pStream->rd.avail_in += ( uInt ) rec;
                  pStream->skip_in = ( uInt ) ( 6 - rec );
                  rec = 0;
               }
            }
            if( pStream->skip_in == 0 )
            {
               long l = ( pStream->crypt_size + 0x07 ) & ~0x07;
               rec = pStream->crypt_in & ~0x07;
               if( rec > l )
                  rec = l;
               /* decrypt the buffer */
               for( l = 0; l < rec; l += 8 )
                  hb_znetDecrypt( pStream, pStream->rd.next_in + pStream->rd.avail_in + l );
               pStream->crypt_in -= rec;
               if( ( uInt ) rec > pStream->crypt_size )
               {
                  pStream->skip_in = rec - pStream->crypt_size;
                  rec = pStream->crypt_size;
               }
               pStream->crypt_size -= rec;
            }
         }
         pStream->rd.avail_in += ( uInt ) rec;
         if( pStream->rd.avail_in == 0 )
            break;
         rec = 0;
      }
      pStream->err = inflate( &pStream->rd, Z_SYNC_FLUSH );
/*
      if( pStream->err == Z_STREAM_END && pStream->rd.avail_in == 0 )
         pStream->err = Z_BUF_ERROR;
 */
      if( pStream->err != Z_OK &&
          ! ( pStream->err == Z_BUF_ERROR && pStream->rd.avail_in == 0 ) )
         break;
   }

   len -= pStream->rd.avail_out;

   return len == 0 ? rec : len;
}

static long hb_znetStreamWrite( PHB_ZNETSTREAM pStream, HB_SOCKET sd, HB_MAXINT timeout )
{
   long tosnd = HB_ZNET_BUFSIZE - pStream->wr.avail_out;
   long snd = 0, rest =  0;

   if( pStream->crypt )
   {
      long size = ( long ) ( pStream->wr.next_out - pStream->crypt_out );

      if( size > 2 )
      {
         HB_U16 uiLen = ( HB_U16 ) ( size - 2 );
         HB_PUT_BE_UINT16( pStream->crypt_out, uiLen );
         uiLen = ( HB_U16 ) ( ( ( size + 7 ) ^ 0x07 ) & 0x07 );
         if( ( uInt ) uiLen > pStream->wr.avail_out )
         {
            /* it may happen only if encryption was enabled in non empty
             * buffer and the unencrypted part has not been flushed yet.
             */
            rest = size;
         }
         else
         {
            while( uiLen-- )
            {
               *pStream->wr.next_out++ = ( Byte ) 0; /* TODO: use better hashing data */
               pStream->wr.avail_out--;
               size++;
            }
            /* encrypt the buffer */
            for( tosnd = 0; tosnd < size; tosnd += 8 )
               hb_znetEncrypt( pStream, pStream->crypt_out + tosnd );
            pStream->crypt_out = pStream->wr.next_out;
            pStream->wr.next_out += 2;
            if( pStream->wr.avail_out < 2 )
               pStream->skip_out = 2 - pStream->wr.avail_out;
            pStream->wr.avail_out -= 2 - pStream->skip_out;
         }
         tosnd = ( long ) ( pStream->crypt_out - pStream->outbuf );
      }
      else
         tosnd -= 2;
   }

   if( tosnd > 0 )
   {
      snd = hb_socketSend( sd, pStream->outbuf, tosnd, 0, timeout );
      if( snd > 0 )
      {
         tosnd += rest - snd;
         if( tosnd > 0 )
            memmove( pStream->outbuf, pStream->outbuf + snd, tosnd );
         pStream->wr.avail_out += ( uInt ) snd;
         pStream->wr.next_out -= snd;
         pStream->crypt_out -= snd;
         if( pStream->skip_out )
         {
            if( pStream->skip_out <= pStream->wr.avail_out )
            {
               pStream->wr.avail_out -= pStream->skip_out;
               pStream->skip_out = 0;
            }
            else
            {
               pStream->skip_out -= pStream->wr.avail_out;
               pStream->wr.avail_out = 0;
            }
         }
      }
   }
   return snd;
}

/* flush data in stream structure - return number of bytes left in the
 * buffer which were not sent
 */
long hb_znetFlush( PHB_ZNETSTREAM pStream, HB_SOCKET sd, HB_MAXINT timeout )
{
   uInt uiSize = HB_ZNET_BUFSIZE - ( pStream->crypt ? 2 : 0 );

   if( pStream->wr.avail_out > 0 )
      pStream->err = deflate( &pStream->wr, Z_PARTIAL_FLUSH );
   else
      pStream->err = Z_OK;

   while( pStream->wr.avail_out < uiSize )
   {
      if( hb_znetStreamWrite( pStream, sd, timeout ) <= 0 )
         break;

      if( pStream->err == Z_OK )
         pStream->err = deflate( &pStream->wr, Z_PARTIAL_FLUSH );
   }

   return uiSize - pStream->wr.avail_out;
}

/* write data using stream structure
 */
long hb_znetWrite( PHB_ZNETSTREAM pStream, HB_SOCKET sd, const void * buffer, long len, HB_MAXINT timeout, long * plast )
{
   long snd = 0;

   pStream->wr.next_in = ( Bytef * ) buffer;
   pStream->wr.avail_in = ( uInt ) len;
   pStream->err = Z_OK;

   while( pStream->wr.avail_in )
   {
      if( pStream->wr.avail_out == 0 )
      {
         snd = hb_znetStreamWrite( pStream, sd, timeout );
         if( plast )
            *plast = snd;
         if( snd <= 0 )
            break;
         snd = 0;
      }
      pStream->err = deflate( &pStream->wr, Z_NO_FLUSH );
      if( pStream->err != Z_OK )
         break;
   }

   len -= pStream->wr.avail_in;

   return len == 0 ? snd : len;
}
