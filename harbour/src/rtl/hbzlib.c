/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    ZLIB functions wrapper
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

#include <zlib.h>

/* Try to figure if we have this function. Z_RLE was introduced in 1.2.0.1,
   while compressBound() was added in 1.2.0. This means we have to miss
   compressBound() when using zlib 1.2.0. [vszakats] */
#if defined( Z_RLE )
   #define _HB_Z_COMPRESSBOUND
#endif

static HB_SIZE hb_zlibUncompressedSize( const char * szSrc, HB_SIZE nLen,
                                        int * piResult )
{
   Byte buffer[ 1024 ];
   z_stream stream;
   HB_SIZE nDest = 0;

   memset( &stream, 0, sizeof( z_stream ) );

   stream.next_in   = ( Bytef * ) szSrc;
   stream.avail_in  = ( uInt ) nLen;
/*
   stream.zalloc    = Z_NULL;
   stream.zfree     = Z_NULL;
   stream.opaque    = NULL;
*/

   *piResult = inflateInit( &stream );
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

/*
 * HB_ZLIBVERSION( [<nType>] ) -> <cZlibVersion>
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
 * HB_ZCOMPRESSBOUND( <cData> | <nDataLen> ) -> <nMaxCompressLen>
 */
HB_FUNC( HB_ZCOMPRESSBOUND )
{
   if( HB_ISCHAR( 1 ) )
#if defined( _HB_Z_COMPRESSBOUND )
      hb_retnint( compressBound( ( uLong ) hb_parclen( 1 ) ) );
#else
      hb_retnint( 0 );
#endif
   else if( HB_ISNUM( 1 ) )
#if defined( _HB_Z_COMPRESSBOUND )
      hb_retnint( compressBound( ( uLong ) hb_parns( 1 ) ) );
#else
      hb_retnint( 0 );
#endif
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_ZUNCOMPRESSLEN( <cCompressedData>, [<@nResult>] )
 *          -> <nUnCompressedDataLen> or -1 on error
 */
HB_FUNC( HB_ZUNCOMPRESSLEN )
{
   const char * szData = hb_parc( 1 );

   if( szData )
   {
      HB_SIZE nLen = hb_parclen( 1 );
      int iResult = Z_OK;

      if( nLen )
         nLen = hb_zlibUncompressedSize( szData, nLen, &iResult );

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
 * HB_ZCOMPRESS( <cData>, [<nDstBufLen>|<@cBuffer>], [<@nResult>], [<nLevel>] )
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
         uLong ulDstLen;
         char * pDest;
         int iResult;

         if( pBuffer )
         {
            HB_SIZE nDstLen;
            if( !hb_itemGetWriteCL( pBuffer, &pDest, &nDstLen ) )
               pDest = NULL;
            ulDstLen = ( uLong ) nDstLen;
         }
         else
         {
            ulDstLen = HB_ISNUM( 2 ) ? ( uLong ) hb_parns( 2 ) :
#if defined( _HB_Z_COMPRESSBOUND )
                                    compressBound( ( uLong ) nLen );
#else
                                    0;
#endif
            pDest = ( char * ) hb_xalloc( ulDstLen + 1 );
         }

         if( pDest )
         {
            if( HB_ISNUM( 4 ) )
               iResult = compress2( ( Bytef * ) pDest, &ulDstLen, ( Bytef * ) szData, ( uLong ) nLen, hb_parni( 4 ) );
            else
               iResult = compress( ( Bytef * ) pDest, &ulDstLen, ( Bytef * ) szData, ( uLong ) nLen );

            if( !pBuffer )
            {
               if( iResult == Z_OK )
                  hb_retclen_buffer( pDest, ulDstLen );
               else
                  hb_xfree( pDest );
            }
            else if( iResult == Z_OK )
               hb_retclen( pDest, ulDstLen );
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
 * HB_ZUNCOMPRESS( <cCompressedData>, [<nDstBufLen>|<@cBuffer>], [<@nResult>] )
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
         uLong ulDstLen;
         char * pDest = NULL;
         int iResult = Z_OK;

         if( pBuffer )
         {
            HB_SIZE nDstLen;
            if( !hb_itemGetWriteCL( pBuffer, &pDest, &nDstLen ) )
               iResult = Z_MEM_ERROR;
            ulDstLen = ( uLong ) nDstLen;
         }
         else
         {
            ulDstLen = HB_ISNUM( 2 ) ? ( uLong ) hb_parns( 2 ) :
                          ( uLong ) hb_zlibUncompressedSize( szData, nLen, &iResult );
            if( iResult == Z_OK )
            {
               pDest = ( char * ) hb_xalloc( ulDstLen + 1 );
               if( !pDest )
                  iResult = Z_MEM_ERROR;
            }
         }

         if( iResult == Z_OK )
         {
            iResult = uncompress( ( Bytef * ) pDest, &ulDstLen, ( Bytef * ) szData, ( uLong ) nLen );

            if( !pBuffer )
            {
               if( iResult == Z_OK )
                  hb_retclen_buffer( pDest, ulDstLen );
               else
                  hb_xfree( pDest );
            }
            else if( iResult == Z_OK )
               hb_retclen( pDest, ulDstLen );
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
 * HB_ZERROR( <nError> ) => <cErrorDescription>
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
