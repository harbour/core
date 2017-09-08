/*
 * ZLIB functions wrapper
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#define _HB_ZLIB_INTERNAL_

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbinit.h"
#include "hbzlib.h"

#include <zlib.h>

/* Try to figure if we have this function. Z_RLE was introduced in 1.2.0.1,
   while compressBound() and deflateBound() were added in 1.2.0. This means
   we have to miss compressBound() when using zlib 1.2.0. [vszakats] */
/* ZLIB_VERNUM were added in version 1.2.0.2 so it cannot be used for older
   zlib libraries */
#if defined( Z_RLE ) && ! defined( Z_SOLO )
#define _HB_Z_COMPRESSBOUND
#endif

#if ! defined( _HB_Z_COMPRESSBOUND )
/* additional 12 bytes is for GZIP compression which uses bigger header */
#define deflateBound( s, n )  ( hb_zlibCompressBound( n ) + ( fGZip ? 12 : 0 ) )
#endif

static HB_SIZE s_zlibCompressBound( HB_SIZE nLen )
{
#if ! defined( _HB_Z_COMPRESSBOUND )
   return nLen + ( nLen >> 12 ) + ( nLen >> 14 ) + ( nLen >> 25 ) + 13;
#else
   return compressBound( ( uLong ) nLen );
#endif
}

static void * s_zlib_alloc( void * cargo, uInt items, uInt size )
{
   HB_SYMBOL_UNUSED( cargo );

   return ( items > 0 && size > 0 ) ? hb_xalloc( ( HB_SIZE ) items * size ) : NULL;
}

static void s_zlib_free( void * cargo, void * address )
{
   HB_SYMBOL_UNUSED( cargo );

   if( address )
      hb_xfree( address );
}

static int s_zlibCompress2( char ** pDstPtr, HB_SIZE * pnDst,
                            const char * pSrc, HB_SIZE nSrc,
                            HB_BOOL fGZip, int level )
{
   z_stream stream;
   int iResult;

   memset( &stream, 0, sizeof( stream ) );
   stream.zalloc    = s_zlib_alloc;
   stream.zfree     = s_zlib_free;
   stream.opaque    = NULL;
   stream.next_in   = ( Bytef* ) HB_UNCONST( pSrc );
   stream.avail_in  = ( uInt ) nSrc;
   iResult = deflateInit2( &stream, level, Z_DEFLATED,
                           15 + ( fGZip ? 16 : 0 ), 8,
                           Z_DEFAULT_STRATEGY );
   if( iResult == Z_OK )
   {
      if( *pDstPtr == NULL )
      {
         if( *pnDst == 0 )
            *pnDst = deflateBound( &stream, ( uLong ) nSrc );
         *pDstPtr = ( char * ) hb_xalloc( *pnDst + 1 );
         if( *pDstPtr == NULL )
            iResult = Z_MEM_ERROR;
      }
   }

   if( iResult == Z_OK )
   {
      stream.next_out  = ( Bytef* ) *pDstPtr;
      stream.avail_out = ( uInt ) *pnDst;

      do
      {
         iResult = deflate( &stream, Z_FINISH );
      }
      while( iResult == Z_OK );

      if( iResult == Z_STREAM_END )
      {
         *pnDst = stream.total_out;
         iResult = Z_OK;
      }
      deflateEnd( &stream );
   }

   return iResult;
}

static int s_zlibCompress( char * pDst, HB_SIZE * pnDst,
                           const char * pSrc, HB_SIZE nSrc, int level )
{
   return s_zlibCompress2( &pDst, pnDst, pSrc, nSrc, HB_FALSE, level );
}

static HB_SIZE s_zlibUncompressedSize( const char * szSrc, HB_SIZE nLen,
                                       int * piResult )
{
   Byte buffer[ 1024 ];
   z_stream stream;
   HB_SIZE nDest = 0;

   memset( &stream, 0, sizeof( stream ) );
   stream.zalloc    = s_zlib_alloc;
   stream.zfree     = s_zlib_free;
   stream.opaque    = NULL;
   stream.next_in   = ( Bytef * ) HB_UNCONST( szSrc );
   stream.avail_in  = ( uInt ) nLen;

   *piResult = inflateInit2( &stream, 15 + 32 );
   if( *piResult == Z_OK )
   {
      do
      {
         stream.next_out  = buffer;
         stream.avail_out = sizeof( buffer );
         *piResult = inflate( &stream, Z_NO_FLUSH );
      }
      while( *piResult == Z_OK );

      if( *piResult == Z_STREAM_END )
      {
         nDest = stream.total_out;
         *piResult = Z_OK;
      }
      inflateEnd( &stream );
   }

   return nDest;
}

static int s_zlibUncompress( char * pDst, HB_SIZE * pnDst,
                             const char * pSrc, HB_SIZE nSrc )
{
   z_stream stream;
   int iResult;

   memset( &stream, 0, sizeof( stream ) );
   stream.zalloc    = s_zlib_alloc;
   stream.zfree     = s_zlib_free;
   stream.opaque    = NULL;
   stream.next_in   = ( Bytef* ) HB_UNCONST( pSrc );
   stream.avail_in  = ( uInt ) nSrc;
   iResult = inflateInit2( &stream, 15 + 32 );

   if( iResult == Z_OK )
   {
      stream.next_out  = ( Bytef* ) pDst;
      stream.avail_out = ( uInt ) *pnDst;

      do
      {
         iResult = inflate( &stream, Z_FINISH );
      }
      while( iResult == Z_OK );

      if( iResult == Z_STREAM_END )
      {
         *pnDst = stream.total_out;
         iResult = Z_OK;
      }
      inflateEnd( &stream );
   }

   return iResult;
}

/*
 * hb_ZLibVersion( [<nType>] ) --> <cZlibVersion>
 */
HB_FUNC( HB_ZLIBVERSION )
{
   if( hb_parni( 1 ) == 1 )
      hb_retc_const( ZLIB_VERSION );
   else
#if defined( HB_OS_QNX )
      /* NOTE: Hack to avoid "undefined reference to 'zlibVersion' when linking hbrun on QNX 6.2.1. */
      hb_retc_null();
#else
      hb_retc( zlibVersion() );
#endif
}

/*
 * hb_ZCompressBound( <cData> | <nDataLen> ) --> <nMaxCompressLen>
 */
HB_FUNC( HB_ZCOMPRESSBOUND )
{
   if( HB_ISCHAR( 1 ) )
      hb_retnint( s_zlibCompressBound( hb_parclen( 1 ) ) );
   else if( HB_ISNUM( 1 ) )
      hb_retnint( s_zlibCompressBound( hb_parns( 1 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * hb_ZUncompressLen( <cCompressedData>, [<@nResult>] )
 *          --> <nUnCompressedDataLen> or -1 on error
 */
HB_FUNC( HB_ZUNCOMPRESSLEN )
{
   const char * szData = hb_parc( 1 );

   if( szData )
   {
      HB_SIZE nLen = hb_parclen( 1 );
      int iResult = Z_OK;

      if( nLen )
         nLen = s_zlibUncompressedSize( szData, nLen, &iResult );

      if( iResult == Z_OK )
         hb_retnint( nLen );
      else
         hb_retni( -1 );

      hb_storni( iResult, 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * hb_ZCompress( <cData>, [<nDstBufLen>|<@cBuffer>], [<@nResult>], [<nLevel>] )
 *    => <cCompressedData> or NIL on Error
 */
HB_FUNC( HB_ZCOMPRESS )
{
   const char * szData = hb_parc( 1 );

   if( szData )
   {
      HB_SIZE nLen = hb_parclen( 1 );

      if( nLen )
      {
         PHB_ITEM pBuffer = HB_ISBYREF( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL;
         HB_BOOL fAlloc = HB_FALSE;
         HB_SIZE nDstLen;
         char * pDest;
         int iResult;

         if( pBuffer )
         {
            if( ! hb_itemGetWriteCL( pBuffer, &pDest, &nDstLen ) )
               pDest = NULL;
         }
         else
         {
            if( HB_ISNUM( 2 ) )
            {
               nDstLen = hb_parns( 2 );
               pDest = ( char * ) hb_xalloc( nDstLen + 1 );
            }
            else
            {
               pDest = NULL;
               nDstLen = 0;
               fAlloc = HB_TRUE;
            }
         }

         if( pDest || fAlloc )
         {
            iResult = s_zlibCompress2( &pDest, &nDstLen, szData, nLen, HB_FALSE,
                                       hb_parnidef( 4, Z_DEFAULT_COMPRESSION ) );
            if( ! pBuffer )
            {
               if( iResult == Z_OK )
                  hb_retclen_buffer( pDest, nDstLen );
               else if( pDest )
                  hb_xfree( pDest );
            }
            else if( iResult == Z_OK )
               hb_retclen( pDest, nDstLen );
         }
         else
            iResult = Z_MEM_ERROR;

         hb_storni( iResult, 3 );
      }
      else
      {
         hb_retc_null();
         hb_storni( Z_OK, 3 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * hb_ZUncompress( <cCompressedData>, [<nDstBufLen>|<@cBuffer>], [<@nResult>] )
 *    => <cUnCompressedData> or NIL on Error
 */
HB_FUNC( HB_ZUNCOMPRESS )
{
   PHB_ITEM pBuffer = HB_ISBYREF( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL;
   const char * szData = hb_parc( 1 );

   if( szData )
   {
      HB_SIZE nLen = hb_parclen( 1 );

      if( nLen )
      {
         HB_SIZE nDstLen;
         char * pDest = NULL;
         int iResult = Z_OK;

         if( pBuffer )
         {
            if( ! hb_itemGetWriteCL( pBuffer, &pDest, &nDstLen ) )
               iResult = Z_MEM_ERROR;
         }
         else
         {
            nDstLen = HB_ISNUM( 2 ) ? ( HB_SIZE ) hb_parns( 2 ) :
                           s_zlibUncompressedSize( szData, nLen, &iResult );
            if( iResult == Z_OK )
            {
               pDest = ( char * ) hb_xalloc( nDstLen + 1 );
               if( ! pDest )
                  iResult = Z_MEM_ERROR;
            }
         }

         if( iResult == Z_OK )
         {
            iResult = s_zlibUncompress( pDest, &nDstLen, szData, nLen );

            if( ! pBuffer )
            {
               if( iResult == Z_OK )
                  hb_retclen_buffer( pDest, nDstLen );
               else
                  hb_xfree( pDest );
            }
            else if( iResult == Z_OK )
               hb_retclen( pDest, nDstLen );
         }
         hb_storni( iResult, 3 );
      }
      else
      {
         hb_retc_null();
         hb_storni( Z_OK, 3 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * hb_gzCompressBound( <cData> | <nDataLen> ) --> <nMaxCompressLen>
 */
HB_FUNC( HB_GZCOMPRESSBOUND )
{
   if( HB_ISCHAR( 1 ) )
      hb_retnint( s_zlibCompressBound( ( uLong ) hb_parclen( 1 ) ) + 12 );
   else if( HB_ISNUM( 1 ) )
      hb_retnint( s_zlibCompressBound( ( uLong ) hb_parns( 1 ) ) + 12 );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * hb_gzCompress( <cData>, [<nDstBufLen>|<@cBuffer>], [<@nResult>], [<nLevel>] )
 *    => <cCompressedData> or NIL on Error
 *
 * Note: this function does not create any references to gz* ZLIB functions
 *       so it's intentionally here not in hbzlibgz.c file.
 */
HB_FUNC( HB_GZCOMPRESS )
{
   const char * szData = hb_parc( 1 );

   if( szData )
   {
      HB_SIZE nLen = hb_parclen( 1 );

      if( nLen )
      {
         PHB_ITEM pBuffer = HB_ISBYREF( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL;
         HB_BOOL fAlloc = HB_FALSE;
         HB_SIZE nDstLen;
         char * pDest;
         int iResult;

         if( pBuffer )
         {
            if( ! hb_itemGetWriteCL( pBuffer, &pDest, &nDstLen ) )
               pDest = NULL;
         }
         else
         {
            if( HB_ISNUM( 2 ) )
            {
               nDstLen = hb_parns( 2 );
               pDest = ( char * ) hb_xalloc( nDstLen + 1 );
            }
            else
            {
               pDest = NULL;
               nDstLen = 0;
               fAlloc = HB_TRUE;
            }
         }

         if( pDest || fAlloc )
         {
            iResult = s_zlibCompress2( &pDest, &nDstLen, szData, nLen, HB_TRUE,
                                       hb_parnidef( 4, Z_DEFAULT_COMPRESSION ) );
            if( ! pBuffer )
            {
               if( iResult == Z_OK )
                  hb_retclen_buffer( pDest, nDstLen );
               else if( pDest )
                  hb_xfree( pDest );
            }
            else if( iResult == Z_OK )
               hb_retclen( pDest, nDstLen );
         }
         else
            iResult = Z_MEM_ERROR;

         hb_storni( iResult, 3 );
      }
      else
      {
         hb_retc_null();
         hb_storni( Z_OK, 3 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * hb_ZError( <nError> ) => <cErrorDescription>
 */
HB_FUNC( HB_ZERROR )
{
#if defined( HB_OS_QNX )
   /* NOTE: Hack to avoid "undefined reference to 'zlibVersion' when linking hbrun on QNX 6.2.1. */
   hb_retc_null();
#else
   hb_retc( zError( hb_parni( 1 ) ) );
#endif
}

HB_CALL_ON_STARTUP_BEGIN( _hb_zlib_init_ )
   hb_zlibInit( s_zlibCompressBound, s_zlibUncompressedSize,
                s_zlibCompress, s_zlibUncompress );
HB_CALL_ON_STARTUP_END( _hb_zlib_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_zlib_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_zlib_init_ )
   #include "hbiniseg.h"
#endif
