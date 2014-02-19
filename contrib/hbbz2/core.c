/*
 * Harbour Project source code:
 *    BZIP2 functions wrapper
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

#include "hbapi.h"

#include "hbapiitm.h"
#include "hbapierr.h"

#include <bzlib.h>

#include "hbbz2.ch"

/* Required if bz2 lib was built with BZ_NO_STDIO [vszakats] */
void bz_internal_error( int errcode )
{
   hb_errInternal( ( HB_ERRCODE ) errcode, "libbzip2", NULL, NULL );
}

static void * hb_bz2Alloc( void * cargo, int nmemb, int size )
{
   HB_SYMBOL_UNUSED( cargo );
   return hb_xalloc( ( HB_SIZE ) nmemb * size );
}

static void hb_bz2Free( void * cargo, void * ptr )
{
   HB_SYMBOL_UNUSED( cargo );
   hb_xfree( ptr );
}

static HB_SIZE hb_bz2CompressBound( HB_SIZE nLen )
{
   HB_SIZE nSize = nLen + nLen / 100 + 600;

   return nSize < nLen ? HB_SIZE_MAX - 1 : nSize;
}

static int hb_bz2Compress( const char * szSrc, HB_SIZE nSrc,
                           char * szDst, HB_SIZE * pnDst, int iBlockSize )
{
   bz_stream stream;
   int       iResult;

   memset( &stream, 0, sizeof( stream ) );

   stream.next_in  = ( char * ) szSrc;
   stream.avail_in = ( unsigned int ) nSrc;

   stream.next_out  = szDst;
   stream.avail_out = ( unsigned int ) *pnDst;

   stream.bzalloc = hb_bz2Alloc;
   stream.bzfree  = hb_bz2Free;
/* stream.opaque  = NULL; */

   iResult = BZ2_bzCompressInit( &stream, iBlockSize, 0, 0 );
   if( iResult == BZ_OK )
   {
      do
      {
         iResult = BZ2_bzCompress( &stream, BZ_FINISH );
      }
      while( iResult == BZ_FINISH_OK );

      if( iResult == BZ_STREAM_END )
      {
#if HB_SIZE_MAX <= UINT_MAX
         *pnDst = ( HB_SIZE ) stream.total_out_lo32;
#else
         *pnDst = ( ( HB_SIZE ) stream.total_out_hi32 << 32 ) |
                  stream.total_out_lo32;
#endif
         iResult = BZ_OK;
      }

      BZ2_bzCompressEnd( &stream );
   }

   return iResult;
}

static HB_SIZE hb_bz2UncompressedSize( const char * szSrc, HB_SIZE nLen,
                                       int * piResult )
{
   char      buffer[ 1024 ];
   bz_stream stream;
   HB_SIZE   nDest = 0;

   memset( &stream, 0, sizeof( stream ) );

   stream.next_in  = ( char * ) szSrc;
   stream.avail_in = ( unsigned int ) nLen;

   stream.bzalloc = hb_bz2Alloc;
   stream.bzfree  = hb_bz2Free;
/* stream.opaque  = NULL; */

   *piResult = BZ2_bzDecompressInit( &stream, 0, 0 );
   if( *piResult == BZ_OK )
   {
      do
      {
         stream.next_out  = buffer;
         stream.avail_out = sizeof( buffer );
         *piResult        = BZ2_bzDecompress( &stream );
      }
      while( *piResult == BZ_OK );

      if( *piResult == BZ_STREAM_END )
      {
         *piResult = BZ_OK;
#if HB_SIZE_MAX <= UINT_MAX
         if( stream.total_out_hi32 != 0 )
            *piResult = BZ_MEM_ERROR;
         else
            nDest = ( HB_SIZE ) stream.total_out_lo32;
#else
         nDest = ( ( HB_SIZE ) stream.total_out_hi32 << 32 ) |
                 stream.total_out_lo32;
#endif
      }
      BZ2_bzDecompressEnd( &stream );
   }

   return nDest;
}

static int hb_bz2Uncompress( const char * szSrc, HB_SIZE nSrc,
                             char * szDst, HB_SIZE * pnDst )
{
   bz_stream stream;
   int       iResult;

   memset( &stream, 0, sizeof( stream ) );

   stream.next_in  = ( char * ) szSrc;
   stream.avail_in = ( unsigned int ) nSrc;

   stream.next_out  = szDst;
   stream.avail_out = ( unsigned int ) *pnDst;

   stream.bzalloc = hb_bz2Alloc;
   stream.bzfree  = hb_bz2Free;
/* stream.opaque  = NULL; */

   iResult = BZ2_bzDecompressInit( &stream, 0, 0 );
   if( iResult == BZ_OK )
   {
      do
      {
         iResult = BZ2_bzDecompress( &stream );
      }
      while( iResult == BZ_OK );

      if( iResult == BZ_STREAM_END )
      {
#if HB_SIZE_MAX <= UINT_MAX
         *pnDst = ( HB_SIZE ) stream.total_out_lo32;
#else
         *pnDst = ( ( HB_SIZE ) stream.total_out_hi32 << 32 ) |
                  stream.total_out_lo32;
#endif
         iResult = BZ_OK;
      }
      BZ2_bzDecompressEnd( &stream );
   }

   return iResult;
}

/*
 * hb_bz2_Version() -> <cBZlibVersion>
 */
HB_FUNC( HB_BZ2_VERSION )
{
   hb_retc( BZ2_bzlibVersion() );
}

/*
 * hb_bz2_CompressBound( <cData> | <nDataLen> ) -> <nMaxCompressLen>
 */
HB_FUNC( HB_BZ2_COMPRESSBOUND )
{
   if( HB_ISCHAR( 1 ) )
      hb_retnint( hb_bz2CompressBound( hb_parclen( 1 ) ) );
   else if( HB_ISNUM( 1 ) )
      hb_retnint( hb_bz2CompressBound( ( HB_SIZE ) hb_parnint( 1 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * hb_bz2_UncompressLen( <cCompressedData>, [<@nResult>] )
 *          -> <nUnCompressedDataLen> or -1 on error
 */
HB_FUNC( HB_BZ2_UNCOMPRESSLEN )
{
   const char * szData = hb_parc( 1 );

   if( szData )
   {
      HB_SIZE nLen    = hb_parclen( 1 );
      int     iResult = BZ_OK;

      if( nLen )
         nLen = hb_bz2UncompressedSize( szData, nLen, &iResult );

      if( iResult == BZ_OK )
         hb_retnint( nLen );
      else
         hb_retni( -1 );

      hb_storni( iResult, 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * hb_bz2_Compress( <cData>, [<nDstBufLen>|<@cBuffer>], [<@nResult>], [<nLevel>] )
 *    => <cCompressedData> or NIL on Error
 */
HB_FUNC( HB_BZ2_COMPRESS )
{
   const char * szData = hb_parc( 1 );

   if( szData )
   {
      HB_SIZE nLen = hb_parclen( 1 );

      if( nLen )
      {
         PHB_ITEM pBuffer = HB_ISBYREF( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL;
         HB_SIZE  nDstLen;
         char *   pDest;
         int      iResult;

         if( pBuffer )
         {
            if( ! hb_itemGetWriteCL( pBuffer, &pDest, &nDstLen ) )
               pDest = NULL;
         }
         else
         {
            nDstLen = HB_ISNUM( 2 ) ? ( HB_SIZE ) hb_parnint( 2 ) :
                      ( HB_SIZE ) hb_bz2CompressBound( nLen );
            pDest = ( char * ) hb_xalloc( nDstLen + 1 );
         }

         if( pDest )
         {
            iResult = hb_bz2Compress( szData, nLen, pDest, &nDstLen,
                                      hb_parnidef( 4, HB_BZ_COMPRESSION_DEFAULT ) );
            if( ! pBuffer )
            {
               if( iResult == BZ_OK )
                  hb_retclen_buffer( pDest, nDstLen );
               else
                  hb_xfree( pDest );
            }
            else if( iResult == BZ_OK )
               hb_retclen( pDest, nDstLen );
         }
         else
            iResult = BZ_MEM_ERROR;

         hb_storni( iResult, 3 );
      }
      else
      {
         hb_retc_null();
         hb_storni( BZ_OK, 3 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * hb_bz2_Uncompress( <cCompressedData>, [<nDstBufLen>|<@cBuffer>], [<@nResult>] )
 *    => <cUnCompressedData> or NIL on Error
 */
HB_FUNC( HB_BZ2_UNCOMPRESS )
{
   PHB_ITEM     pBuffer = HB_ISBYREF( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL;
   const char * szData  = hb_parc( 1 );

   if( szData )
   {
      HB_SIZE nLen = hb_parclen( 1 );

      if( nLen )
      {
         HB_SIZE nDstLen;
         char *  pDest   = NULL;
         int     iResult = BZ_OK;

         if( pBuffer )
         {
            if( ! hb_itemGetWriteCL( pBuffer, &pDest, &nDstLen ) )
               iResult = BZ_MEM_ERROR;
         }
         else
         {
            nDstLen = HB_ISNUM( 2 ) ? ( HB_SIZE ) hb_parnint( 2 ) :
                      hb_bz2UncompressedSize( szData, nLen, &iResult );
            if( iResult == BZ_OK )
            {
               pDest = ( char * ) hb_xalloc( nDstLen + 1 );
               if( ! pDest )
                  iResult = BZ_MEM_ERROR;
            }
         }

         if( iResult == BZ_OK )
         {
            iResult = hb_bz2Uncompress( szData, nLen, pDest, &nDstLen );

            if( ! pBuffer )
            {
               if( iResult == BZ_OK )
                  hb_retclen_buffer( pDest, nDstLen );
               else
                  hb_xfree( pDest );
            }
            else if( iResult == BZ_OK )
               hb_retclen( pDest, nDstLen );
         }
         hb_storni( iResult, 3 );
      }
      else
      {
         hb_retc_null();
         hb_storni( BZ_OK, 3 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
