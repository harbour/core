/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OpenSSL API (EVP MD) - Harbour interface.
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
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
#include "hbapiitm.h"

#include "hbssl.h"

#include <openssl/evp.h>

HB_FUNC( OPENSSL_ADD_ALL_DIGESTS )
{
   OpenSSL_add_all_digests();
}

static HB_GARBAGE_FUNC( EVP_MD_CTX_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      EVP_MD_CTX_destroy( ( EVP_MD_CTX * ) *ph );

      /* set pointer to NULL just in case */
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcEVP_MD_CTX_funcs =
{
   EVP_MD_CTX_release,
   hb_gcDummyMark
};

static void * hb_EVP_MD_CTX_is( int iParam )
{
   return hb_parptrGC( &s_gcEVP_MD_CTX_funcs, iParam );
}

static EVP_MD_CTX * hb_EVP_MD_CTX_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcEVP_MD_CTX_funcs, iParam );

   return ph ? ( EVP_MD_CTX * ) *ph : NULL;
}

int hb_EVP_MD_is( int iParam )
{
   return HB_ISCHAR( iParam ) || HB_ISNUM( iParam );
}

const EVP_MD * hb_EVP_MD_par( int iParam )
{
   const EVP_MD * p;

   if( HB_ISCHAR( iParam ) )
      return EVP_get_digestbyname( hb_parc( iParam ) );

   switch( hb_parni( iParam ) )
   {
      case HB_EVP_MD_MD_NULL:    p = EVP_md_null();   break;
#if ! defined( OPENSSL_NO_MD2 ) && OPENSSL_VERSION_NUMBER < 0x10000000L
      case HB_EVP_MD_MD2:        p = EVP_md2();       break;
#endif
#ifndef OPENSSL_NO_MD4
      case HB_EVP_MD_MD4:        p = EVP_md4();       break;
#endif
#ifndef OPENSSL_NO_MD5
      case HB_EVP_MD_MD5:        p = EVP_md5();       break;
#endif
#ifndef OPENSSL_NO_SHA
      case HB_EVP_MD_SHA:        p = EVP_sha();       break;
      case HB_EVP_MD_SHA1:       p = EVP_sha1();      break;
      case HB_EVP_MD_DSS:        p = EVP_dss();       break;
      case HB_EVP_MD_DSS1:       p = EVP_dss1();      break;
#if ! defined( HB_OPENSSL_OLD_OSX_ )
      case HB_EVP_MD_ECDSA:      p = EVP_ecdsa();     break;
#endif
#endif
#ifndef OPENSSL_NO_SHA256
      case HB_EVP_MD_SHA224:     p = EVP_sha224();    break;
      case HB_EVP_MD_SHA256:     p = EVP_sha256();    break;
#endif
#ifndef OPENSSL_NO_SHA512
      case HB_EVP_MD_SHA384:     p = EVP_sha384();    break;
      case HB_EVP_MD_SHA512:     p = EVP_sha512();    break;
#endif
#ifndef OPENSSL_NO_MDC2
      case HB_EVP_MD_MDC2:       p = EVP_mdc2();      break;
#endif
#ifndef OPENSSL_NO_RIPEMD
      case HB_EVP_MD_RIPEMD160:  p = EVP_ripemd160(); break;
#endif
      default:                   p = NULL;
   }

   return p;
}

static int hb_EVP_MD_ptr_to_id( const EVP_MD * p )
{
   int n;

   if(      p == EVP_md_null()   ) n = HB_EVP_MD_MD_NULL;
#ifndef OPENSSL_NO_MD2
   else if( p == EVP_md2()       ) n = HB_EVP_MD_MD2;
#endif
#ifndef OPENSSL_NO_MD4
   else if( p == EVP_md4()       ) n = HB_EVP_MD_MD4;
#endif
#ifndef OPENSSL_NO_MD5
   else if( p == EVP_md5()       ) n = HB_EVP_MD_MD5;
#endif
#ifndef OPENSSL_NO_SHA
   else if( p == EVP_sha()       ) n = HB_EVP_MD_SHA;
   else if( p == EVP_sha1()      ) n = HB_EVP_MD_SHA1;
   else if( p == EVP_dss()       ) n = HB_EVP_MD_DSS;
   else if( p == EVP_dss1()      ) n = HB_EVP_MD_DSS1;
#if ! defined( HB_OPENSSL_OLD_OSX_ )
   else if( p == EVP_ecdsa()     ) n = HB_EVP_MD_ECDSA;
#endif
#endif
#ifndef OPENSSL_NO_SHA256
   else if( p == EVP_sha224()    ) n = HB_EVP_MD_SHA224;
   else if( p == EVP_sha256()    ) n = HB_EVP_MD_SHA256;
#endif
#ifndef OPENSSL_NO_SHA512
   else if( p == EVP_sha384()    ) n = HB_EVP_MD_SHA384;
   else if( p == EVP_sha512()    ) n = HB_EVP_MD_SHA512;
#endif
#ifndef OPENSSL_NO_MDC2
   else if( p == EVP_mdc2()      ) n = HB_EVP_MD_MDC2;
#endif
#ifndef OPENSSL_NO_RIPEMD
   else if( p == EVP_ripemd160() ) n = HB_EVP_MD_RIPEMD160;
#endif
   else                            n = HB_EVP_MD_UNSUPPORTED;

   return n;
}

HB_FUNC( EVP_GET_DIGESTBYNAME )
{
   if( HB_ISCHAR( 1 ) )
      hb_retni( hb_EVP_MD_ptr_to_id( EVP_get_digestbyname( hb_parc( 1 ) ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_GET_DIGESTBYNID )
{
   if( HB_ISNUM( 1 ) )
      hb_retni( hb_EVP_MD_ptr_to_id( EVP_get_digestbynid( hb_parni( 1 ) ) ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_MD_TYPE )
{
   const EVP_MD * md = hb_EVP_MD_par( 1 );

   hb_retni( md ? EVP_MD_type( md ) : 0 );
}

HB_FUNC( EVP_MD_NID )
{
   const EVP_MD * md = hb_EVP_MD_par( 1 );

   hb_retni( md ? EVP_MD_nid( md ) : 0 );
}

HB_FUNC( EVP_MD_PKEY_TYPE )
{
   const EVP_MD * md = hb_EVP_MD_par( 1 );

   hb_retni( md ? EVP_MD_pkey_type( md ) : 0 );
}

HB_FUNC( EVP_MD_SIZE )
{
   const EVP_MD * md = hb_EVP_MD_par( 1 );

   hb_retni( md ? EVP_MD_size( md ) : 0 );
}

HB_FUNC( EVP_MD_BLOCK_SIZE )
{
   const EVP_MD * md = hb_EVP_MD_par( 1 );

   hb_retni( md ? EVP_MD_block_size( md ) : 0 );
}

HB_FUNC( EVP_MD_CTX_CREATE )
{
   void ** ph = ( void ** ) hb_gcAllocate( sizeof( EVP_MD_CTX * ), &s_gcEVP_MD_CTX_funcs );

   EVP_MD_CTX * ctx = EVP_MD_CTX_create();

   *ph = ctx;

   hb_retptrGC( ph );
}

HB_FUNC( EVP_MD_CTX_INIT )
{
   if( hb_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
         EVP_MD_CTX_init( ctx );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_MD_CTX_CLEANUP )
{
   if( hb_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_MD_CTX_cleanup( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_MD_CTX_MD )
{
   if( hb_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
         hb_retni( hb_EVP_MD_ptr_to_id( EVP_MD_CTX_md( ctx ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_MD_CTX_COPY )
{
   if( hb_EVP_MD_CTX_is( 1 ) && hb_EVP_MD_CTX_is( 2 ) )
   {
      EVP_MD_CTX * ctx_out = hb_EVP_MD_CTX_par( 1 );
      EVP_MD_CTX * ctx_in  = hb_EVP_MD_CTX_par( 2 );

      if( ctx_out && ctx_in )
         hb_retni( EVP_MD_CTX_copy( ctx_out, ctx_in ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_MD_CTX_COPY_EX )
{
   if( hb_EVP_MD_CTX_is( 1 ) && hb_EVP_MD_CTX_is( 2 ) )
   {
      EVP_MD_CTX * ctx_out = hb_EVP_MD_CTX_par( 1 );
      EVP_MD_CTX * ctx_in  = hb_EVP_MD_CTX_par( 2 );

      if( ctx_out && ctx_in )
         hb_retni( EVP_MD_CTX_copy_ex( ctx_out, ctx_in ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_DIGESTINIT )
{
   const EVP_MD * md = hb_EVP_MD_par( 2 );

   if( hb_EVP_MD_CTX_is( 1 ) && md )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_DigestInit( ctx, md ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_DIGESTINIT_EX )
{
   const EVP_MD * md = hb_EVP_MD_par( 2 );

   if( hb_EVP_MD_CTX_is( 1 ) && md )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_DigestInit_ex( ctx, md, ( ENGINE * ) hb_parptr( 3 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_DIGESTUPDATE )
{
   if( hb_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_DigestUpdate( ctx, hb_parcx( 2 ), ( size_t ) hb_parclen( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_DIGESTFINAL )
{
   if( hb_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) hb_xgrab( EVP_MAX_MD_SIZE + 1 );
         unsigned int    size   = 0;

         hb_retni( EVP_DigestFinal( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! hb_storclen_buffer( ( char * ) buffer, ( HB_SIZE ) size, 2 ) )
               hb_xfree( buffer );
         }
         else
         {
            hb_xfree( buffer );
            hb_storc( NULL, 2 );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_DIGESTFINAL_EX )
{
   if( hb_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) hb_xgrab( EVP_MAX_MD_SIZE + 1 );
         unsigned int    size   = 0;

         hb_retni( EVP_DigestFinal_ex( ctx, buffer, &size ) );

         if( size > 0 )
         {
            if( ! hb_storclen_buffer( ( char * ) buffer, ( HB_SIZE ) size, 2 ) )
               hb_xfree( buffer );
         }
         else
         {
            hb_xfree( buffer );
            hb_storc( NULL, 2 );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_SIGNINIT )
{
   const EVP_MD * md = hb_EVP_MD_par( 2 );

   if( hb_EVP_MD_CTX_is( 1 ) && md )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
         EVP_SignInit( ctx, md );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_SIGNINIT_EX )
{
   const EVP_MD * md = hb_EVP_MD_par( 2 );

   if( hb_EVP_MD_CTX_is( 1 ) && md )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_SignInit_ex( ctx, md, ( ENGINE * ) hb_parptr( 3 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_SIGNUPDATE )
{
   if( hb_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_SignUpdate( ctx, hb_parcx( 2 ), ( size_t ) hb_parclen( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_SIGNFINAL )
{
   if( hb_EVP_MD_CTX_is( 1 ) && hb_EVP_PKEY_is( 3 ) )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
      {
         unsigned char * buffer = ( unsigned char * ) hb_xgrab( EVP_PKEY_size( hb_EVP_PKEY_par( 3 ) ) + 1 );
         unsigned int    size   = 0;

         hb_retni( EVP_SignFinal( ctx, buffer, &size, hb_EVP_PKEY_par( 3 ) ) );

         if( size > 0 )
         {
            if( ! hb_storclen_buffer( ( char * ) buffer, ( HB_SIZE ) size, 2 ) )
               hb_xfree( buffer );
         }
         else
         {
            hb_xfree( buffer );
            hb_storc( NULL, 2 );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_VERIFYINIT )
{
   const EVP_MD * md = hb_EVP_MD_par( 2 );

   if( hb_EVP_MD_CTX_is( 1 ) && md )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_VerifyInit( ctx, md ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_VERIFYINIT_EX )
{
   const EVP_MD * md = hb_EVP_MD_par( 2 );

   if( hb_EVP_MD_CTX_is( 1 ) && md )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_VerifyInit_ex( ctx, md, ( ENGINE * ) hb_parptr( 3 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_VERIFYUPDATE )
{
   if( hb_EVP_MD_CTX_is( 1 ) )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_VerifyUpdate( ctx, hb_parcx( 2 ), ( size_t ) hb_parclen( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_VERIFYFINAL )
{
   if( hb_EVP_MD_CTX_is( 1 ) && hb_EVP_PKEY_is( 3 ) )
   {
      EVP_MD_CTX * ctx = hb_EVP_MD_CTX_par( 1 );

      if( ctx )
         hb_retni( EVP_VerifyFinal( ctx, ( const unsigned char * ) hb_parcx( 2 ), ( unsigned int ) hb_parclen( 2 ), hb_EVP_PKEY_par( 3 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
