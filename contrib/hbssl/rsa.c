/*
 * OpenSSL API (RSA) - Harbour interface.
 *
 * Copyright 2016 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbssl.h"

#include <openssl/rsa.h>

static HB_GARBAGE_FUNC( s_RSA_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      RSA_free( ( RSA * ) *ph );

      /* set pointer to NULL just in case */
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcRSA_funcs =
{
   s_RSA_release,
   hb_gcDummyMark
};

HB_BOOL hb_RSA_is( int iParam )
{
   return hb_parptrGC( &s_gcRSA_funcs, iParam ) != NULL;
}

RSA * hb_RSA_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcRSA_funcs, iParam );

   return ph ? ( RSA * ) *ph : NULL;
}

void hb_RSA_ret( RSA * rsa )
{
   void ** ph = ( void ** ) hb_gcAllocate( sizeof( RSA * ), &s_gcRSA_funcs );

   *ph = rsa;

   hb_retptrGC( ph );
}

HB_FUNC( RSA_SIZE )
{
   RSA * rsa = hb_RSA_par( 1 );

   if( rsa )
      hb_retni( RSA_size( rsa ) );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( RSA_PUBLIC_ENCRYPT )
{
   RSA * rsa = hb_RSA_par( 1 );

   if( rsa )
   {
      const unsigned char * from = ( const unsigned char * ) hb_parcx( 2 );
      int flen = ( int ) hb_parclen( 2 ),
          padding = hb_parnidef( 4, RSA_PKCS1_PADDING );
      unsigned char * buffer;
      int ret;

      buffer = ( unsigned char * ) hb_xgrab( RSA_size( rsa ) + 1 );

      ret = RSA_public_encrypt( flen, HB_SSL_CONST_BYTE( from ), buffer, rsa, padding );
      if( ret > 0 )
      {
         if( ! hb_storclen_buffer( ( char * ) buffer, ret, 3 ) )
            ret = 0;
      }
      if( ret <= 0 )
      {
         if( buffer )
            hb_xfree( buffer );
         hb_storc( NULL, 3 );
      }
      hb_retni( ret );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( RSA_PRIVATE_DECRYPT )
{
   RSA * rsa = hb_RSA_par( 1 );

   if( rsa )
   {
      const unsigned char * from = ( const unsigned char * ) hb_parcx( 2 );
      int flen = ( int ) hb_parclen( 2 ),
          padding = hb_parnidef( 4, RSA_PKCS1_PADDING );
      unsigned char * buffer;
      int ret;

      buffer = ( unsigned char * ) hb_xgrab( RSA_size( rsa ) + 1 );

      ret = RSA_private_decrypt( flen, HB_SSL_CONST_BYTE( from ), buffer, rsa, padding );
      if( ret > 0 )
      {
         buffer = ( unsigned char * ) hb_xrealloc( buffer, ret + 1 );
         if( ! hb_storclen_buffer( ( char * ) buffer, ret, 3 ) )
            ret = 0;
      }
      if( ret <= 0 )
      {
         if( buffer )
            hb_xfree( buffer );
         hb_storc( NULL, 3 );
      }
      hb_retni( ret );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( RSA_PRIVATE_ENCRYPT )
{
   RSA * rsa = hb_RSA_par( 1 );

   if( rsa )
   {
      const unsigned char * from = ( const unsigned char * ) hb_parcx( 2 );
      int flen = ( int ) hb_parclen( 2 ),
          padding = hb_parnidef( 4, RSA_PKCS1_PADDING );
      unsigned char * buffer;
      int ret;

      buffer = ( unsigned char * ) hb_xgrab( RSA_size( rsa ) + 1 );

      ret = RSA_private_encrypt( flen, HB_SSL_CONST_BYTE( from ), buffer, rsa, padding );
      if( ret > 0 )
      {
         if( ! hb_storclen_buffer( ( char * ) buffer, ret, 3 ) )
            ret = 0;
      }
      if( ret <= 0 )
      {
         if( buffer )
            hb_xfree( buffer );
         hb_storc( NULL, 3 );
      }
      hb_retni( ret );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( RSA_PUBLIC_DECRYPT )
{
   RSA * rsa = hb_RSA_par( 1 );

   if( rsa )
   {
      const unsigned char * from = ( const unsigned char * ) hb_parcx( 2 );
      int flen = ( int ) hb_parclen( 2 ),
          padding = hb_parnidef( 4, RSA_PKCS1_PADDING );
      unsigned char * buffer;
      int ret;

      buffer = ( unsigned char * ) hb_xgrab( RSA_size( rsa ) + 1 );

      ret = RSA_public_decrypt( flen, HB_SSL_CONST_BYTE( from ), buffer, rsa, padding );
      if( ret > 0 )
      {
         buffer = ( unsigned char * ) hb_xrealloc( buffer, ret + 1 );
         if( ! hb_storclen_buffer( ( char * ) buffer, ret, 3 ) )
            ret = 0;
      }
      if( ret <= 0 )
      {
         if( buffer )
            hb_xfree( buffer );
         hb_storc( NULL, 3 );
      }
      hb_retni( ret );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
