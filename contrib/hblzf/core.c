/*
 * Harbour Project source code:
 *    LZF functions wrapper
 *
 * Copyright 2010 Petr Chornyj <myorg63@mail.ru>
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
#include "hbapierr.h"
#include "hbapiitm.h"

#include "lzf.h"
#include "lzfP.h"

#if ! AVOID_ERRNO
#  include <errno.h>
#endif

#include "hblzf.ch"

static HB_SIZE hb_lzf_compressbound( HB_SIZE nLen )
{
   HB_SIZE nBuffSize = ( HB_SIZE ) ( nLen * 1.04 + 1 );

   return ( nBuffSize >= 32 ) ? nBuffSize : 32;
}

HB_FUNC( HB_LZF_COMPRESSBOUND )
{
   if( HB_ISCHAR( 1 ) || HB_ISNUM( 1 ) )
   {
      HB_SIZE nLen = HB_ISCHAR( 1 ) ? hb_parclen( 1 ) : ( HB_SIZE ) hb_parns( 1 );
      hb_retns( hb_lzf_compressbound( nLen ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Return a LZF_VERSION, API version */
HB_FUNC( HB_LZF_VERSION )
{
   hb_retni( LZF_VERSION );
}

/* Return if lzf was optimized for speed (or for compression) */
HB_FUNC( HB_LZF_OPTIMIZED_FOR_SPEED )
{
#if ULTRA_FAST
   hb_retl( HB_TRUE );
#else
   hb_retl( HB_FALSE );
#endif
}

/* Return a string compressed with LZF */
HB_FUNC( HB_LZF_COMPRESS )
{
   PHB_ITEM pArg = hb_param( 1, HB_IT_STRING );

   if( pArg )
   {
      HB_SIZE in_len = hb_itemGetCLen( pArg );

      if( in_len )
      {
         PHB_ITEM     pBuffer = HB_ISBYREF( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL;
         const char * in_data = hb_itemGetCPtr( pArg );
         char *       out_data;
         HB_SIZE      out_len;

         if( pBuffer )
         {
            if( ! hb_itemGetWriteCL( pBuffer, &out_data, &out_len ) )
               out_data = NULL;
         }
         else
         {
            out_len = ( HB_ISNUM( 2 ) && hb_parns( 2 ) >= 0 ) ?
                      ( HB_SIZE ) hb_parns( 2 ) :
                      hb_lzf_compressbound( in_len );

            out_data = ( char * ) hb_xalloc( out_len + 1 );
         }

         if( out_data )
         {
            unsigned int uiResult = lzf_compress( in_data, ( unsigned int ) in_len, out_data, ( unsigned int ) out_len );

            if( uiResult != 0 )
            {
               if( pBuffer )
                  hb_retclen( out_data, uiResult );
               else
                  hb_retclen_buffer( out_data, uiResult );

               hb_storni( HB_LZF_OK, 3 );
            }
            else
            {
               if( ! pBuffer )
                  hb_xfree( out_data );

               hb_storni( HB_LZF_BUF_ERROR, 3 );
            }
         }
         else
            hb_storni( HB_LZF_MEM_ERROR, 3 );
      }
      else
      {
         hb_retc_null();
         hb_storni( HB_LZF_OK, 3 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Return a string decompressed with LZF */
HB_FUNC( HB_LZF_DECOMPRESS )
{
   PHB_ITEM pArg = hb_param( 1, HB_IT_STRING );

   if( pArg )
   {
      HB_SIZE in_len = hb_itemGetCLen( pArg );

      if( in_len )
      {
         PHB_ITEM     pBuffer = HB_ISBYREF( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL;
         const char * in_data = hb_itemGetCPtr( pArg );
         char *       buffer;
         HB_SIZE      buffer_size;

         if( pBuffer )
         {
            if( ! hb_itemGetWriteCL( pBuffer, &buffer, &buffer_size ) )
               buffer = NULL;
         }
         else
         {
            buffer_size = ( HB_ISNUM( 2 ) && hb_parns( 2 ) >= 0 ) ?
                          hb_parns( 2 ) : HB_LZF_DEFAULT_BUFFSIZE;

            buffer = ( char * ) hb_xalloc( buffer_size + 1 );
         }

         if( buffer && buffer_size )
         {
            unsigned int uiResult;

            do
            {
               buffer_size <<= 1;
               buffer        = ( char * ) hb_xrealloc( buffer, buffer_size + 1 );

               uiResult = lzf_decompress( in_data, ( unsigned int ) in_len, buffer, ( unsigned int ) buffer_size );
            }
            while( uiResult == 0
#if ! AVOID_ERRNO
                   && errno == E2BIG
#endif
                   );

            if( uiResult == 0 )
            {
#if ! AVOID_ERRNO
               if( errno == EINVAL )
                  hb_storni( HB_LZF_DATA_CORRUPTED, 3 );
               else
#endif
               hb_storni( HB_LZF_OK, 3 );

               if( ! pBuffer )
                  hb_xfree( buffer );
            }
            else
            {
               if( pBuffer )
                  hb_retclen( buffer, uiResult );
               else
                  hb_retclen_buffer( buffer, uiResult );

               hb_storni( HB_LZF_OK, 3 );
            }
         }
         else
            hb_storni( buffer_size ? HB_LZF_BUF_ERROR : HB_LZF_MEM_ERROR, 3 );
      }
      else
      {
         hb_retc_null();
         hb_storni( HB_LZF_OK, 3 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
