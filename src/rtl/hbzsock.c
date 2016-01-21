/*
 * Harbour extended socket filter with ZLIB compression
 *
 * Copyright 2015 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* this has to be declared before hbsocket.h is included */
#define _HB_SOCKEX_IMPLEMENTATION_

#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbsocket.h"
#include "hbinit.h"
#include "hbzlib.ch"

#include <zlib.h>

#define HB_ZSOCK_ERROR_BASE   100

#define HB_ZSOCK_READAHEAD    0x40
#define HB_ZSOCK_RDBUFSIZE    4096
#define HB_ZSOCK_WRBUFSIZE    4096

#if MAX_MEM_LEVEL >= 8
#  define HB_ZSOCK_MEM_LEVEL  8
#else
#  define HB_ZSOCK_MEM_LEVEL  MAX_MEM_LEVEL
#endif

#define HB_ZSOCK_GET( p )     ( ( PHB_SOCKEX_Z ) p->cargo )

typedef struct
{
   PHB_SOCKEX     sock;

   HB_BOOL        fDecompressIn;
   HB_BOOL        fCompressOut;

   z_stream       z_read;
   z_stream       z_write;

   HB_BYTE *      rdbuf;
   HB_BYTE *      wrbuf;
}
HB_SOCKEX_Z, * PHB_SOCKEX_Z;

static voidpf s_zsock_zalloc( voidpf opaque, uInt items, uInt size )
{
   HB_SYMBOL_UNUSED( opaque );
   return hb_xalloc( ( HB_SIZE ) items * size );
}

static void s_zsock_zfree( voidpf opaque, voidpf address )
{
   HB_SYMBOL_UNUSED( opaque );
   hb_xfree( address );
}

static long s_zsock_write( PHB_SOCKEX_Z pZ, HB_MAXINT timeout )
{
   long lSent = 0, len = HB_ZSOCK_WRBUFSIZE - pZ->z_write.avail_out;

   while( lSent < len )
   {
      long l = hb_sockexWrite( pZ->sock, pZ->wrbuf + lSent, len - lSent, timeout );
      if( l <= 0 )
      {
         switch( hb_socketGetError() )
         {
            case HB_SOCKET_ERR_TIMEOUT:
            case HB_SOCKET_ERR_AGAIN:
            case HB_SOCKET_ERR_TRYAGAIN:
               break;
            default:
               lSent = -1;
               break;
         }
         break;
      }

      lSent += l;
      if( timeout > 0 )
         timeout = 0;
   }

   if( lSent > 0 )
   {
      if( lSent < len )
         memmove( pZ->wrbuf, pZ->wrbuf + lSent, len - lSent );
      pZ->z_write.avail_out += lSent;
      pZ->z_write.next_out -= lSent;
   }

   return lSent;
}

static int s_zsock_inbuffer( PHB_SOCKEX pSock )
{
   PHB_SOCKEX_Z pZ = HB_ZSOCK_GET( pSock );

   if( pSock->inbuffer == 0 && pZ->fDecompressIn )
   {
      int err;

      if( pSock->buffer == NULL )
      {
         if( pSock->readahead <= 0 )
            pSock->readahead = HB_ZSOCK_READAHEAD;
         pSock->buffer = ( HB_BYTE * ) hb_xgrab( pSock->readahead );
      }

      pZ->z_read.next_out  = ( Bytef * ) pSock->buffer;
      pZ->z_read.avail_out = ( uInt ) pSock->readahead;

      err = inflate( &pZ->z_read, Z_SYNC_FLUSH );
      if( err != Z_OK && err != Z_BUF_ERROR )
         hb_socketSetError( HB_ZSOCK_ERROR_BASE - err );
      pSock->inbuffer = pSock->readahead - pZ->z_read.avail_out;
   }
   return pSock->inbuffer > 0 ? 1 : 0;
}


/* socket filter */

static long s_sockexRead( PHB_SOCKEX pSock, void * data, long len, HB_MAXINT timeout )
{
   PHB_SOCKEX_Z pZ = HB_ZSOCK_GET( pSock );
   long lRecv = 0;

   if( pSock->inbuffer > 0 && len > 0 )
   {
      lRecv = HB_MIN( pSock->inbuffer, len );
      memcpy( data, pSock->buffer + pSock->posbuffer, lRecv );
      if( ( pSock->inbuffer -= lRecv ) > 0 )
         pSock->posbuffer += lRecv;
      else
         pSock->posbuffer = 0;
      return lRecv;
   }
   else if( pZ->fDecompressIn )
   {
      int err = Z_OK;

      pZ->z_read.next_out  = ( Bytef * ) data;
      pZ->z_read.avail_out = ( uInt ) len;
      pZ->z_read.total_out = 0;

      while( pZ->z_read.avail_out )
      {
         if( err == Z_BUF_ERROR && pZ->z_read.avail_in == 0 )
         {
            lRecv = hb_sockexRead( pZ->sock, pZ->rdbuf, HB_ZSOCK_RDBUFSIZE,
                                   pZ->z_read.total_out == 0 ? timeout : 0 );
            if( lRecv <= 0 )
               break;
            pZ->z_read.next_in = ( Bytef * ) pZ->rdbuf;
            pZ->z_read.avail_in = ( uInt ) lRecv;
         }
         else if( err != Z_OK )
         {
            hb_socketSetError( HB_ZSOCK_ERROR_BASE - err );
            lRecv = -1;
            break;
         }
         err = inflate( &pZ->z_read, Z_SYNC_FLUSH );
      }

      if( pZ->z_read.total_out != 0 )
         lRecv = ( long ) pZ->z_read.total_out;

      return lRecv;
   }
   else
      return hb_sockexRead( pZ->sock, data, len, timeout );
}

static long s_sockexWrite( PHB_SOCKEX pSock, const void * data, long len, HB_MAXINT timeout )
{
   PHB_SOCKEX_Z pZ = HB_ZSOCK_GET( pSock );

   if( pZ->fCompressOut )
   {
      long lWritten = 0;

      pZ->z_write.next_in  = ( Bytef * ) HB_UNCONST( data );
      pZ->z_write.avail_in = ( uInt ) len;

      while( pZ->z_write.avail_in )
      {
         int err;

         if( pZ->z_write.avail_out == 0 )
         {
            lWritten = s_zsock_write( pZ, timeout );
            if( lWritten <= 0 )
               break;
            timeout = 0;
         }
         err = deflate( &pZ->z_write, Z_NO_FLUSH );
         if( err != Z_OK )
         {
            if( err != Z_BUF_ERROR )
            {
               hb_socketSetError( HB_ZSOCK_ERROR_BASE - err );
               lWritten = -1;
            }
            break;
         }
      }

      return lWritten >= 0 ? ( long ) ( len - pZ->z_write.avail_in ) : lWritten;
   }
   else
      return hb_sockexWrite( pZ->sock, data, len, timeout );
}

static long s_sockexFlush( PHB_SOCKEX pSock, HB_MAXINT timeout, HB_BOOL fSync )
{
   PHB_SOCKEX_Z pZ = HB_ZSOCK_GET( pSock );
   long lResult = 0;

   if( pZ->fCompressOut &&
       ( ! fSync || pZ->z_write.avail_out != HB_ZSOCK_WRBUFSIZE ||
         pZ->z_write.total_in != 0 || pZ->z_write.total_out != 0 ) )
   {
      int err;

      if( pZ->z_write.avail_out > 0 )
         err = deflate( &pZ->z_write, fSync ? Z_FULL_FLUSH : Z_PARTIAL_FLUSH );
      else
         err = Z_OK;

      while( pZ->z_write.avail_out < HB_ZSOCK_WRBUFSIZE )
      {
         if( s_zsock_write( pZ, timeout ) <= 0 )
            break;
         if( err == Z_OK || err == Z_BUF_ERROR )
            err = deflate( &pZ->z_write, fSync ? Z_FULL_FLUSH : Z_PARTIAL_FLUSH );
      }
      if( err != Z_OK && err != Z_BUF_ERROR )
         hb_socketSetError( HB_ZSOCK_ERROR_BASE - err );
      lResult = HB_ZSOCK_WRBUFSIZE - pZ->z_write.avail_out;
   }
   return lResult + hb_sockexFlush( pZ->sock, timeout, fSync );
}

static int s_sockexCanRead( PHB_SOCKEX pSock, HB_BOOL fBuffer, HB_MAXINT timeout )
{
   return s_zsock_inbuffer( pSock ) ? 1 :
          hb_sockexCanRead( HB_ZSOCK_GET( pSock )->sock, fBuffer, timeout );
}

static int s_sockexCanWrite( PHB_SOCKEX pSock, HB_BOOL fBuffer, HB_MAXINT timeout )
{
   return hb_sockexCanWrite( HB_ZSOCK_GET( pSock )->sock, fBuffer, timeout );
}

static char * s_sockexName( PHB_SOCKEX pSock )
{
   char * pszName = hb_sockexIsRaw( HB_ZSOCK_GET( pSock )->sock ) ? NULL :
                    hb_sockexName( HB_ZSOCK_GET( pSock )->sock );
   if( pszName )
   {
      char * pszFree = pszName;
      pszName = hb_xstrcpy( NULL, pSock->pFilter->pszName, "|", pszName, NULL );
      hb_xfree( pszFree );
   }
   else
      pszName = hb_strdup( pSock->pFilter->pszName );

   return pszName;
}

static const char * s_sockexErrorStr( PHB_SOCKEX pSock, int iError )
{
   switch( HB_ZSOCK_ERROR_BASE - iError )
   {
      case Z_STREAM_END:
         return "Z_STREAM_END";
      case Z_NEED_DICT:
         return "Z_NEED_DICT";
      case Z_ERRNO:
         return "Z_ERRNO";
      case Z_STREAM_ERROR:
         return "Z_STREAM_ERROR";
      case Z_DATA_ERROR:
         return "Z_DATA_ERROR";
      case Z_MEM_ERROR:
         return "Z_MEM_ERROR";
      case Z_BUF_ERROR:
         return "Z_BUF_ERROR";
      case Z_VERSION_ERROR:
         return "Z_VERSION_ERROR";
   }

   return hb_sockexErrorStr( HB_ZSOCK_GET( pSock )->sock, iError );
}

static int s_sockexClose( PHB_SOCKEX pSock, HB_BOOL fClose )
{
   PHB_SOCKEX_Z pZ = HB_ZSOCK_GET( pSock );
   int iResult = 0;

   if( pZ )
   {
      if( pZ->sock )
         s_sockexFlush( pSock, HB_MAX( 15000, pSock->iAutoFlush ), HB_TRUE );

      if( pZ->fDecompressIn )
         inflateEnd( &pZ->z_read );
      if( pZ->rdbuf )
         hb_xfree( pZ->rdbuf );
      if( pZ->fCompressOut )
         deflateEnd( &pZ->z_write );
      if( pZ->wrbuf )
         hb_xfree( pZ->wrbuf );

      if( pZ->sock )
      {
         if( pSock->fShutDown )
            pZ->sock->fShutDown = HB_TRUE;
         if( pSock->iAutoFlush != 0 && pZ->sock->iAutoFlush == 0 )
            pZ->sock->iAutoFlush = pSock->iAutoFlush;
         iResult = hb_sockexClose( pZ->sock, fClose );
      }

      hb_xfree( pZ );
   }
   /* call hb_sockexRawClear() with fClose = HB_FALSE because
      hb_sockexClose() already closed real socket */
   hb_sockexRawClear( pSock, HB_FALSE );
   hb_xfree( pSock );

   return iResult;
}

static PHB_SOCKEX s_sockexNext( PHB_SOCKEX pSock, PHB_ITEM pParams );

static PHB_SOCKEX s_sockexNew( HB_SOCKET sd, PHB_ITEM pParams )
{
   PHB_SOCKEX pSock, pSockNew = NULL;

   pSock = hb_sockexNew( sd, NULL, pParams );
   if( pSock )
   {
      pSockNew = s_sockexNext( pSock, pParams );
      if( pSockNew == NULL )
         hb_sockexClose( pSock, HB_FALSE );
   }

   return pSockNew;
}

static const HB_SOCKET_FILTER s_sockFilter =
{
   "ZSOCK",
   s_sockexNew,
   s_sockexNext,
   s_sockexClose,
   s_sockexRead,
   s_sockexWrite,
   s_sockexFlush,
   s_sockexCanRead,
   s_sockexCanWrite,
   s_sockexName,
   s_sockexErrorStr
};

static PHB_SOCKEX s_sockexNext( PHB_SOCKEX pSock, PHB_ITEM pParams )
{
   PHB_SOCKEX pSockNew = NULL;

   if( pSock )
   {
      HB_BOOL fDecompressIn = HB_TRUE, fCompressOut = HB_TRUE;
      int level = HB_ZLIB_COMPRESSION_DEFAULT,
          strategy = HB_ZLIB_STRATEGY_DEFAULT,
          windowBitsIn = MAX_WBITS, windowBitsOut = MAX_WBITS;

      if( pParams && HB_IS_HASH( pParams ) )
      {
         PHB_ITEM pItem;

         if( ( pItem = hb_hashGetCItemPtr( pParams, "zlib" ) ) != NULL &&
             HB_IS_NUMERIC( pItem ) )
            level = hb_itemGetNI( pItem );
         if( ( pItem = hb_hashGetCItemPtr( pParams, "zs" ) ) != NULL &&
             HB_IS_NUMERIC( pItem ) )
            strategy = hb_itemGetNI( pItem );

         if( ( pItem = hb_hashGetCItemPtr( pParams, "gzin" ) ) != NULL &&
             HB_IS_LOGICAL( pItem ) )
         {
            fDecompressIn = hb_itemGetL( pItem );
            if( fDecompressIn )
               windowBitsIn += 16;
         }
         if( ( pItem = hb_hashGetCItemPtr( pParams, "zin" ) ) != NULL &&
             HB_IS_LOGICAL( pItem ) )
         {
            if( windowBitsIn == MAX_WBITS )
               fDecompressIn = hb_itemGetL( pItem );
            else if( hb_itemGetL( pItem ) )
               windowBitsIn += 16;
         }

         if( ( pItem = hb_hashGetCItemPtr( pParams, "gzout" ) ) != NULL &&
             HB_IS_LOGICAL( pItem ) )
         {
            fCompressOut = hb_itemGetL( pItem );
            if( fCompressOut )
               windowBitsOut += 16;
         }
         if( ( pItem = hb_hashGetCItemPtr( pParams, "zout" ) ) != NULL &&
             HB_IS_LOGICAL( pItem ) && windowBitsOut == MAX_WBITS )
            fCompressOut = hb_itemGetL( pItem );
      }

      if( level != HB_ZLIB_COMPRESSION_DISABLE &&
          ( fDecompressIn || fCompressOut ) )
      {
         PHB_SOCKEX_Z pZ = ( PHB_SOCKEX_Z ) hb_xgrabz( sizeof( HB_SOCKEX_Z ) );

         pSockNew = ( PHB_SOCKEX ) hb_xgrabz( sizeof( HB_SOCKEX ) );
         pSockNew->sd = HB_NO_SOCKET;
         pSockNew->fRedirAll = HB_TRUE;
         pSockNew->pFilter = &s_sockFilter;

         pSockNew->cargo = ( void * ) pZ;
         pZ->z_read.zalloc = s_zsock_zalloc;
         pZ->z_read.zfree  = s_zsock_zfree;
         pZ->z_read.opaque = Z_NULL;

         pZ->z_write.zalloc = s_zsock_zalloc;
         pZ->z_write.zfree  = s_zsock_zfree;
         pZ->z_write.opaque = Z_NULL;

         pZ->z_read.next_in  = NULL;
         pZ->z_read.avail_in = 0;

         if( level != Z_DEFAULT_COMPRESSION &&
             !( level >= Z_NO_COMPRESSION && level <= Z_BEST_COMPRESSION ) )
            level = Z_DEFAULT_COMPRESSION;

         if( strategy != Z_FILTERED    &&
#if defined( Z_RLE )
             strategy != Z_RLE         &&
#endif
#if defined( Z_FIXED )
             strategy != Z_FIXED       &&
#endif
             strategy != Z_HUFFMAN_ONLY )
            strategy = Z_DEFAULT_STRATEGY;

         if( fDecompressIn && level != HB_ZLIB_COMPRESSION_DISABLE )
         {
            /* MAX_WBITS=15, decompression - support for formats:
             * -15: raw, 15: ZLIB, 31: GZIP, 47: ZLIB+GZIP
             */
            if( inflateInit2( &pZ->z_read, windowBitsIn ) == Z_OK )
            {
               pZ->fDecompressIn = HB_TRUE;
               pZ->rdbuf = ( HB_BYTE * ) hb_xgrab( HB_ZSOCK_RDBUFSIZE );
            }
            else
               level = HB_ZLIB_COMPRESSION_DISABLE;
         }

         if( fCompressOut && level != HB_ZLIB_COMPRESSION_DISABLE )
         {
            /* MAX_WBITS=15, compression format:
             * -15: raw, 15: ZLIB (+6 bytes), 31: GZIP(+18 bytes)
             */
            if( deflateInit2( &pZ->z_write, level,
                              Z_DEFLATED, windowBitsOut, HB_ZSOCK_MEM_LEVEL,
                              strategy ) == Z_OK )
            {
               pZ->fCompressOut = HB_TRUE;
               pZ->wrbuf = ( HB_BYTE * ) hb_xgrab( HB_ZSOCK_WRBUFSIZE );
               pZ->z_write.next_out  = ( Bytef * ) pZ->wrbuf;
               pZ->z_write.avail_out = HB_ZSOCK_WRBUFSIZE;
            }
            else
               level = HB_ZLIB_COMPRESSION_DISABLE;
         }

         if( level != HB_ZLIB_COMPRESSION_DISABLE )
         {
            pSockNew->sd = pSock->sd;
            pSockNew->fShutDown = pSock->fShutDown;
            pSockNew->iAutoFlush = pSock->iAutoFlush;
            pZ->sock = pSock;
            hb_socekxParamsInit( pSockNew, pParams );
         }
         else
         {
            s_sockexClose( pSockNew, HB_FALSE );
            pSockNew = NULL;
         }
      }
   }

   return pSockNew;
}

/* hb_socketNewZSock( <pSocket>, [<hParams>] ) -> <pSocket> */
HB_FUNC( HB_SOCKETNEWZSOCK )
{
   PHB_SOCKEX pSock = hb_sockexParam( 1 );

   if( pSock )
   {
      pSock = s_sockexNext( pSock, hb_param( 2, HB_IT_HASH ) );
      if( pSock )
      {
         hb_sockexItemClear( hb_param( 1, HB_IT_POINTER ) );
         hb_sockexItemPut( hb_param( -1, HB_IT_ANY ), pSock );
      }
   }
}


HB_CALL_ON_STARTUP_BEGIN( _hb_zsock_init_ )
   hb_sockexRegister( &s_sockFilter );
HB_CALL_ON_STARTUP_END( _hb_zsock_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_zsock_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC( _hb_zsock_init_ )
   #include "hbiniseg.h"
#endif
