/*
 * Harbour Project source code:
 * OpenSSL API (EVP PKEY) - Harbour interface.
 *
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour)
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

#include "hbssl.h"

#include <openssl/evp.h>

static HB_GARBAGE_FUNC( EVP_PKEY_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      EVP_PKEY_free( ( EVP_PKEY * ) *ph );

      /* set pointer to NULL just in case */
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcEVP_PKEY_funcs =
{
   EVP_PKEY_release,
   hb_gcDummyMark
};

void * hb_EVP_PKEY_is( int iParam )
{
   return hb_parptrGC( &s_gcEVP_PKEY_funcs, iParam );
}

EVP_PKEY * hb_EVP_PKEY_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcEVP_PKEY_funcs, iParam );

   return ph ? ( EVP_PKEY * ) *ph : NULL;
}

HB_FUNC( EVP_PKEY_NEW )
{
   void ** ph = ( void ** ) hb_gcAllocate( sizeof( EVP_PKEY * ), &s_gcEVP_PKEY_funcs );

   EVP_PKEY * ctx = EVP_PKEY_new();

   *ph = ctx;

   hb_retptrGC( ph );
}

HB_FUNC( EVP_PKEY_TYPE )
{
   hb_retni( EVP_PKEY_type( hb_parni( 1 ) ) );
}

HB_FUNC( EVP_PKEY_SIZE )
{
   if( hb_EVP_PKEY_is( 1 ) )
   {
      EVP_PKEY * pkey = hb_EVP_PKEY_par( 1 );

      if( pkey )
         hb_retni( EVP_PKEY_size( pkey ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_PKEY_BITS )
{
   if( hb_EVP_PKEY_is( 1 ) )
   {
      EVP_PKEY * pkey = hb_EVP_PKEY_par( 1 );

      if( pkey )
         hb_retni( EVP_PKEY_bits( pkey ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_PKEY_ASSIGN )
{
   if( hb_EVP_PKEY_is( 1 ) )
   {
      EVP_PKEY * pkey = hb_EVP_PKEY_par( 1 );

      if( pkey )
         /* QUESTION: Is hb_openssl_strdup() okay here? [vszakats] */
         hb_retni( EVP_PKEY_assign( pkey, hb_parni( 2 ), hb_openssl_strdup( hb_parcx( 3 ) ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_PKEY_ASSIGN_RSA )
{
#ifndef OPENSSL_NO_RSA
   if( hb_EVP_PKEY_is( 1 ) && HB_ISPOINTER( 2 ) )
   {
      EVP_PKEY * pkey = hb_EVP_PKEY_par( 1 );
      RSA *      key  = ( RSA * ) hb_parptr( 2 );

      if( pkey && key )
         hb_retni( EVP_PKEY_assign_RSA( pkey, key ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_errRT_BASE( EG_NOFUNC, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}

HB_FUNC( EVP_PKEY_ASSIGN_DSA )
{
#ifndef OPENSSL_NO_DSA
   if( hb_EVP_PKEY_is( 1 ) && HB_ISPOINTER( 2 ) )
   {
      EVP_PKEY * pkey = hb_EVP_PKEY_par( 1 );
      DSA *      key  = ( DSA * ) hb_parptr( 2 );

      if( pkey && key )
         hb_retni( EVP_PKEY_assign_DSA( pkey, key ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_errRT_BASE( EG_NOFUNC, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}

HB_FUNC( EVP_PKEY_ASSIGN_DH )
{
#ifndef OPENSSL_NO_RSA
   if( hb_EVP_PKEY_is( 1 ) && HB_ISPOINTER( 2 ) )
   {
      EVP_PKEY * pkey = hb_EVP_PKEY_par( 1 );
      DH *       key  = ( DH * ) hb_parptr( 2 );

      if( pkey && key )
         hb_retni( EVP_PKEY_assign_DH( pkey, key ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_errRT_BASE( EG_NOFUNC, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}

#if 0

int EVP_PKEY_set1_RSA( EVP_PKEY * pkey, RSA * key );
int EVP_PKEY_set1_DSA( EVP_PKEY * pkey, DSA * key );
int EVP_PKEY_set1_DH( EVP_PKEY * pkey, DH * key );
int EVP_PKEY_set1_EC_KEY( EVP_PKEY * pkey, EC_KEY * key );

RSA *    EVP_PKEY_get1_RSA( EVP_PKEY * pkey );
DSA *    EVP_PKEY_get1_DSA( EVP_PKEY * pkey );
DH *     EVP_PKEY_get1_DH( EVP_PKEY * pkey );
EC_KEY * EVP_PKEY_get1_EC_KEY( EVP_PKEY * pkey );

/* These changed in 0.9.9 to something different, they weren't probably documented before. */
int EVP_PKEY_decrypt( unsigned char * dec_key, const unsigned char * enc_key, int enc_key_len, EVP_PKEY * private_key );
int EVP_PKEY_encrypt( unsigned char * enc_key, const unsigned char * key, int key_len, EVP_PKEY * pub_key     );

/* 1.0.0 */
int EVP_PKEY_sign_init( EVP_PKEY_CTX * ctx );
int EVP_PKEY_sign( EVP_PKEY_CTX * ctx,
                   unsigned char * sig, size_t * siglen,
                   const unsigned char * tbs, size_t tbslen );

int EVP_PKEY_verify_init( EVP_PKEY_CTX * ctx );
int EVP_PKEY_verify( EVP_PKEY_CTX * ctx,
                     const unsigned char * sig, size_t siglen,
                     const unsigned char * tbs, size_t tbslen );

int EVP_PKEY_verify_recover_init( EVP_PKEY_CTX * ctx );
int EVP_PKEY_verify_recover( EVP_PKEY_CTX * ctx,
                             unsigned char * rout, size_t * routlen,
                             const unsigned char * sig, size_t siglen );

int EVP_PKEY_encrypt_init( EVP_PKEY_CTX * ctx );
int EVP_PKEY_encrypt( EVP_PKEY_CTX * ctx,
                      unsigned char * out, size_t * outlen,
                      const unsigned char * in, size_t inlen );

int EVP_PKEY_decrypt_init( EVP_PKEY_CTX * ctx );
int EVP_PKEY_decrypt( EVP_PKEY_CTX * ctx,
                      unsigned char * out, size_t * outlen,
                      const unsigned char * in, size_t inlen );

#endif
