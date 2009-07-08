/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    ZLIB functions wrapper
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbzlib.h"

static ULONG hb_zlibUncompressedSize( const char * szSrc, ULONG ulLen )
{
   Byte buffer[ 1024 ];
   z_stream stream;
   ULONG ulDest = 0;

   memset( &stream, 0, sizeof( z_stream ) );

   stream.next_in   = ( Bytef * ) szSrc;
   stream.avail_in  = ( uInt ) ulLen;
/*
   stream.zalloc    = Z_NULL;
   stream.zfree     = Z_NULL;
   stream.opaque    = NULL;
*/
   if( inflateInit( &stream ) == Z_OK )
   {
      int iStatus;
      do
      {
         stream.next_out  = buffer;
         stream.avail_out = sizeof( buffer );
         iStatus = inflate( &stream, Z_NO_FLUSH );
      }
      while( iStatus == Z_OK );
      if( iStatus == Z_STREAM_END )
         ulDest = stream.total_out;
      inflateEnd( &stream );
   }

   return ulDest;
}

/*
 * HB_ZLIBVERSION( [<nType>] ) -> <cZlibVersion>
 */
HB_FUNC( HB_ZLIBVERSION )
{
   if( hb_parni( 1 ) == 1 )
      hb_retc_const( ZLIB_VERSION );
   else
      hb_retc( zlibVersion() );
}

/*
 * HB_ZCOMPRESSBOUND( <cData> | <nDataLen> ) -> <nMaxCompressLen>
 */
HB_FUNC( HB_ZCOMPRESSBOUND )
{
   if( HB_ISCHAR( 1 ) )
      hb_retnint( compressBound( hb_parclen( 1 ) ) );
   else if( HB_ISNUM( 1 ) )
      hb_retnint( compressBound( ( uLong ) hb_parnint( 1 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * HB_ZUNCOMPRESSLEN( <cCompressedData> ) -> <nUnCompressedDataLen> or 0 on error
 */
HB_FUNC( HB_ZUNCOMPRESSLEN )
{
   ULONG ulLen = hb_parclen( 1 );

   hb_retnint( ulLen ? hb_zlibUncompressedSize( hb_parc( 1 ), ulLen ) : 0 );
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
      ULONG ulLen = hb_parclen( 1 );

      if( ulLen )
      {
         PHB_ITEM pBuffer = HB_ISBYREF( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL;
         uLong ulDstLen;
         char * pDest;
         int iResult;

         if( pBuffer )
         {
            if( !hb_itemGetWriteCL( pBuffer, &pDest, &ulDstLen ) )
               pDest = NULL;
         }
         else
         {
            ulDstLen = HB_ISNUM( 2 ) ? ( uLong ) hb_parnint( 2 ) :
                                    compressBound( ulLen );
            pDest = ( char * ) hb_xalloc( ulDstLen + 1 );
         }

         if( pDest )
         {
            if( HB_ISNUM( 4 ) )
               iResult = compress2( ( Bytef * ) pDest, &ulDstLen, ( Bytef * ) szData, ulLen, hb_parni( 4 ) );
            else
               iResult = compress( ( Bytef * ) pDest, &ulDstLen, ( Bytef * ) szData, ulLen );

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
      ULONG ulLen = hb_parclen( 1 );

      if( ulLen )
      {
         uLong ulDstLen;
         char * pDest;
         int iResult;

         if( pBuffer )
         {
            if( !hb_itemGetWriteCL( pBuffer, &pDest, &ulDstLen ) )
               pDest = NULL;
         }
         else
         {
            ulDstLen = HB_ISNUM( 2 ) ? ( uLong ) hb_parnint( 2 ) :
                                    hb_zlibUncompressedSize( szData, ulLen );
            pDest = ( char * ) hb_xalloc( ulDstLen + 1 );
         }

         if( pDest )
         {
            iResult = uncompress( ( Bytef * ) pDest, &ulDstLen, ( Bytef * ) szData, ulLen );

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
