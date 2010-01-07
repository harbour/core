/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    ZLIB compression for Harbour stream sockets
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#define _HB_ZNET_INTERNAL_

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbsocket.h"
#include "hbznet.h"
#include "hbzlib.ch"

#include <zlib.h>

typedef struct _HB_ZNETSTREAM
{
   z_stream rd;         /* input stream */
   z_stream wr;         /* output stream */
   int      err;        /* error code for last stream operation */
   Bytef *  inbuf;      /* input buffer */
   Bytef *  outbuf;     /* output buffer */
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

/* read data using stream structure
 */
long hb_znetRead( PHB_ZNETSTREAM pStream, HB_SOCKET sd, void * buffer, long len, HB_LONG timeout )
{
   long rec = 0;

   pStream->rd.next_out = ( Bytef * ) buffer;
   pStream->rd.avail_out = ( uInt ) len;
   pStream->err = Z_OK;

   while( pStream->rd.avail_out )
   {
      if( pStream->rd.avail_in == 0 && pStream->err == Z_STREAM_END )
      {
         if( pStream->rd.avail_out != ( uInt ) len )
            timeout = 0;
         rec = hb_socketRecv( sd, pStream->inbuf, HB_ZNET_BUFSIZE, 0, timeout );
         if( rec <= 0 )
            break;
         pStream->rd.avail_in = ( uInt ) rec;
         pStream->rd.next_in = pStream->inbuf;
         rec = 0;
      }
      pStream->err = inflate( &pStream->rd, Z_SYNC_FLUSH );
      if( pStream->err == Z_BUF_ERROR )
         pStream->err = Z_STREAM_END;
      if( pStream->err != Z_OK && pStream->err != Z_STREAM_END )
         break;
   }

   len -= pStream->rd.avail_out;

   return len == 0 ? rec : len;
}

static long hb_znetStreamWrite( PHB_ZNETSTREAM pStream, HB_SOCKET sd, HB_LONG timeout )
{
   long tosnd = HB_ZNET_BUFSIZE - pStream->wr.avail_out;
   long snd = 0;

   if( tosnd > 0 )
   {
      snd = hb_socketSend( sd, pStream->outbuf, tosnd, 0, timeout );
      if( snd > 0 )
      {
         if( snd < tosnd )
            memmove( pStream->outbuf, pStream->outbuf + snd, tosnd - snd );
         pStream->wr.avail_out += ( uInt ) snd;
         pStream->wr.next_out = pStream->outbuf + tosnd - snd;
      }
   }
   return snd;
}

/* flush data in stream structure - return number of bytes left in the
 * buffer which were not sent
 */
long hb_znetFlush( PHB_ZNETSTREAM pStream, HB_SOCKET sd, HB_LONG timeout )
{
   if( pStream->wr.avail_out > 0 )
      pStream->err = deflate( &pStream->wr, Z_SYNC_FLUSH );
   else
      pStream->err = Z_OK;

   while( pStream->wr.avail_out < HB_ZNET_BUFSIZE )
   {
      if( hb_znetStreamWrite( pStream, sd, timeout ) <= 0 )
         break;

      if( pStream->err == Z_OK )
         pStream->err = deflate( &pStream->wr, Z_SYNC_FLUSH );
   }

   return HB_ZNET_BUFSIZE - pStream->wr.avail_out;
}

/* write data using stream structure
 */
long hb_znetWrite( PHB_ZNETSTREAM pStream, HB_SOCKET sd, const void * buffer, long len, HB_LONG timeout, long * plast )
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

/* this function is intentionally not in hbinet.c to not create binding
 * to ZLIB if user does not use it
 */
HB_FUNC( HB_INETCOMPRESS )
{
   PHB_ITEM pItem = hb_param( 1, HB_IT_POINTER );
   int iLevel = Z_DEFAULT_COMPRESSION, iStrategy = Z_DEFAULT_STRATEGY, i;

   if( HB_ISNUM( 2 ) )
   {
      i = hb_parni( 2 );
      if( i >= Z_NO_COMPRESSION && i <= Z_BEST_COMPRESSION )
         iLevel = i;
   }

   if( HB_ISNUM( 3 ) )
   {
      i = hb_parni( 3 );
      if( i == Z_FILTERED     ||
          i == Z_HUFFMAN_ONLY ||
          i == Z_RLE          ||
          i == Z_FIXED )
         iStrategy = i;
   }

   if( iLevel == HB_ZLIB_COMPRESSION_DISABLE )
      hb_znetInetInitialize( pItem, NULL, NULL, NULL, NULL, NULL );
   else
   {
      PHB_ZNETSTREAM pStream = hb_znetOpen( iLevel, iStrategy );
      if( pStream == NULL )
         pItem = NULL; /* to force RTE */
      if( ! hb_znetInetInitialize( pItem, pStream, hb_znetRead, hb_znetWrite,
                                   hb_znetFlush, hb_znetClose ) )
      {
         if( pStream )
            hb_znetClose( pStream );
      }
   }
}
