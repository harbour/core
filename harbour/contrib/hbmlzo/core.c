/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    hbmLZO (miniLZO functions wrapper).
 *    miniLZO -- mini subset of the LZO real-time data compression library
 *               http://www.oberhumer.com/opensource/lzo/
 *
 * Copyright 2011 Petr Chornyj <myorg63@mail.ru>
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
#include "hbapierr.h"
#include "hbinit.h"
#include "hbvm.h"

#include "minilzo.h"

static lzo_uint hb_lzo_compressbound( lzo_uint nLen )
{
   return ( lzo_uint ) ( nLen + ( nLen / 16 ) + 64 + 3 );
}

HB_FUNC( HB_LZO_COMPRESSBOUND )
{
   if( HB_ISCHAR( 1 ) || HB_ISNUM( 1 ) )
   {
      HB_SIZE nLen = HB_ISCHAR( 1 ) ? hb_parclen( 1 ) : ( HB_SIZE ) hb_parns( 1 );

      hb_retns( hb_lzo_compressbound( nLen ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Library initialization */

static void hb_mlzo_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   if( lzo_init() != LZO_E_OK )
      hb_errInternal( HB_EI_VMBADSTARTUP /* TOFIX: ? */, NULL, NULL, NULL );
}

HB_CALL_ON_STARTUP_BEGIN( _hb_mlzo_init_ )
hb_vmAtInit( hb_mlzo_init, NULL );
HB_CALL_ON_STARTUP_END( _hb_mlzo_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_mlzo_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY HB_DATASEG_FUNC( _hb_mlzo_init_ )
   #include "hbiniseg.h"
#endif

/* Compression */

/*
 * int lzo1x_1_compress ( const lzo_bytep src,
 *                        lzo_uint  src_len,
 *                        lzo_bytep dst,
 *                        lzo_uintp dst_len,
 *                        lzo_voidp wrkmem );
 *
 * Memory requirements: LZO1X_1_MEM_COMPRESS (64 kB on 32-bit machines)
 *
 * Return value:
 *    Always returns LZO_E_OK (this function can never fail).
 */

HB_FUNC( HB_LZO1X_1_COMPRESS )
{
   const char * src = hb_parcx( 1 );

   if( src )
   {
      lzo_uint    src_len = ( lzo_uint ) hb_parclen( 1 );
      lzo_bytep   dst     = NULL;

      if( src_len > 0 )
         dst = ( lzo_bytep ) hb_xalloc( HB_MAX( hb_lzo_compressbound( src_len ),
                                                ( HB_SIZE ) hb_parns( 2 ) ) );
      if( dst == NULL )
         hb_storni( LZO_E_OUT_OF_MEMORY, 3 );  /* out of memory */
      else
      {
         lzo_uint    dst_len;
         lzo_voidp   wrkmem  = ( lzo_voidp ) hb_xalloc( LZO1X_1_MEM_COMPRESS );
         int         r;

         if( wrkmem == NULL )
            r = LZO_E_OUT_OF_MEMORY; /* out of memory */
         else
         {
            r = lzo1x_1_compress( ( lzo_bytep ) src, src_len, dst, &dst_len, wrkmem );
            hb_xfree( wrkmem );
         }

         hb_storni( r, 3 );

         if( r == LZO_E_OK )
         {
            hb_storni( dst_len, 2 );

            if( dst_len >= src_len )
               hb_storni( LZO_E_NOT_COMPRESSIBLE, 3 );  /* incompressible data */
            else
            {
               hb_retclen_buffer( ( char * ) dst, dst_len );
               return;
            }
         }
         hb_xfree( dst );
      }
      hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Decompression functions */

/*
   lzo1x_decompress()
   lzo1x_decompress_safe()
 */

HB_FUNC( HB_LZO1X_DECOMPRESS )
{
   const char * src = hb_parcx( 1 );

   if( src )
   {
      lzo_uint    src_len = ( lzo_uint ) hb_parclen( 1 );
      lzo_bytep   dst     = NULL;
      lzo_uint    dst_len;

      if( hb_parns( 2 ) > 0 )
         dst = ( lzo_bytep ) hb_xalloc( hb_parns( 2 ) );

      if( dst == NULL )
         hb_storni( LZO_E_OUT_OF_MEMORY, 3 );  /* out of memory */
      else
      {
         int r = lzo1x_decompress( ( lzo_bytep ) src, src_len, dst, &dst_len, NULL );

         hb_storni( r, 3 );

         if( r == LZO_E_OK )
         {
            hb_storni( dst_len, 2 );
            hb_retclen_buffer( ( char * ) dst, dst_len );
            return;
         }
         hb_xfree( dst );
      }
      hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_LZO1X_DECOMPRESS_SAFE )
{
   const char * src = hb_parcx( 1 );

   if( src )
   {
      lzo_uint    src_len = ( lzo_uint ) hb_parclen( 1 );
      lzo_bytep   dst     = NULL;
      lzo_uint    dst_len = ( lzo_uint ) hb_parns( 2 );

      if( dst_len > 0 )
         dst = ( lzo_bytep ) hb_xalloc( dst_len );

      if( dst == NULL )
         hb_storni( LZO_E_OUT_OF_MEMORY, 3 );  /* out of memory */
      else
      {
         int r = lzo1x_decompress_safe( ( lzo_bytep ) src, src_len, dst, &dst_len, NULL );

         hb_storni( r, 3 );

         if( r == LZO_E_OK )
         {
            hb_storni( dst_len, 2 );
            hb_retclen_buffer( ( char * ) dst, dst_len );
            return;
         }
         hb_xfree( dst );
      }
      hb_retc_null();
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Version functions */

HB_FUNC( LZO_VERSION )
{
   hb_retni( lzo_version() );
}

HB_FUNC( LZO_VERSION_STRING )
{
   hb_retc( lzo_version_string() );
}

HB_FUNC( LZO_VERSION_DATE )
{
   hb_retc( lzo_version_date() );
}

/* Checksum functions */

HB_FUNC( LZO_ADLER32 )
{
   const char * src = hb_parc( 1 );

   if( src )
   {
      lzo_uint32 checksum = lzo_adler32( 0, NULL, 0 );

      hb_retnint( ( HB_MAXINT ) lzo_adler32( checksum, ( lzo_bytep ) src, hb_parclen( 1 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
