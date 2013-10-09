/*
 * Harbour Project source code:
 * OpenSSL API (SSL) - Harbour interface.
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

/* for applink.c */
#if ! defined( HB_OPENSSL_STATIC )
#  if defined( _MSC_VER )
#     ifndef _CRT_SECURE_NO_WARNINGS
#        define _CRT_SECURE_NO_WARNINGS
#     endif
#  endif
#endif

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"

#if defined( HB_OS_WIN )
#  include <windows.h>
#  include <wincrypt.h>
#endif

#include "hbssl.h"

/* NOTE: See: https://www.openssl.org/support/faq.html#PROG2
         Application must call SSL_init(), so that this module gets linked.
         [vszakats] */
#if defined( HB_OS_WIN ) && ! defined( HB_OPENSSL_STATIC ) && OPENSSL_VERSION_NUMBER >= 0x00908000L
   /* NOTE: It doesn't build in bcc55:
            Warning W8065 openssl/applink.c 40: Call to function '_setmode' with no prototype in function app_fsetmod
            Error E2451 openssl/applink.c 82: Undefined symbol '_lseek' in function OPENSSL_Applink
    */
#  if ! defined( __BORLANDC__ )
#     include "openssl/applink.c"
#  endif
#endif

HB_FUNC( SSL_INIT )
{
   SSL_library_init();
   SSL_load_error_strings();
}

HB_FUNC( SSLEAY_VERSION )
{
   int value = hb_parni( 1 );

   switch( value )
   {
      case HB_SSLEAY_VERSION:   value = SSLEAY_VERSION;  break;
      case HB_SSLEAY_CFLAGS:    value = SSLEAY_CFLAGS;   break;
      case HB_SSLEAY_BUILT_ON:  value = SSLEAY_BUILT_ON; break;
      case HB_SSLEAY_PLATFORM:  value = SSLEAY_PLATFORM; break;
      case HB_SSLEAY_DIR:       value = SSLEAY_DIR;      break;
   }

   hb_retc( SSLeay_version( value ) );
}

HB_FUNC( OPENSSL_VERSION )
{
   hb_retnint( OPENSSL_VERSION_NUMBER );
}

HB_FUNC( SSLEAY )
{
   hb_retnint( SSLeay() );
}

static HB_GARBAGE_FUNC( SSL_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      SSL_free( ( SSL * ) *ph );

      /* set pointer to NULL just in case */
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcSSL_funcs =
{
   SSL_release,
   hb_gcDummyMark
};

void * hb_SSL_is( int iParam )
{
   return hb_parptrGC( &s_gcSSL_funcs, iParam );
}

SSL * hb_SSL_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcSSL_funcs, iParam );

   return ph ? ( SSL * ) *ph : NULL;
}

HB_FUNC( SSL_NEW )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
      {
         void ** ph = ( void ** ) hb_gcAllocate( sizeof( SSL * ), &s_gcSSL_funcs );

         SSL * ssl = SSL_new( ctx );

         *ph = ssl;

         hb_retptrGC( ph );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_DUP )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl_par = hb_SSL_par( 1 );

      if( ssl_par )
      {
         void ** ph = ( void ** ) hb_gcAllocate( sizeof( SSL * ), &s_gcSSL_funcs );

         SSL * ssl = SSL_dup( ssl_par );

         *ph = ssl;

         hb_retptrGC( ph );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_ACCEPT )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_accept( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CLEAR )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         SSL_clear( ssl );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_STATE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_state( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_PENDING )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_pending( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_BIO )
{
   BIO * rbio = ( BIO * ) hb_parptr( 2 );
   BIO * wbio = ( BIO * ) hb_parptr( 2 );

   if( hb_SSL_is( 1 ) && rbio && wbio )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         SSL_set_bio( ssl, rbio, wbio );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_RBIO )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retptr( SSL_get_rbio( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_WBIO )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retptr( SSL_get_wbio( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CONNECT )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_connect( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SHUTDOWN )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_shutdown( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_VERSION )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_version( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_VERSION )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retc( SSL_get_version( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_CIPHER )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retc( SSL_get_cipher( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_DO_HANDSHAKE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_do_handshake( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_RENEGOTIATE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_renegotiate( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_TOTAL_RENEGOTIATIONS )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retnl( SSL_total_renegotiations( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_FD )
{
   int iSD;

   if( hb_SSL_is( 1 ) && ( iSD = hb_parnidef( 2, -1 ) ) != -1 )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_set_fd( ssl, iSD ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_RFD )
{
   int iSD;

   if( hb_SSL_is( 1 ) && ( iSD = hb_parnidef( 2, -1 ) ) != -1 )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_set_rfd( ssl, iSD ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_WFD )
{
   int iSD;

   if( hb_SSL_is( 1 ) && ( iSD = hb_parnidef( 2, -1 ) ) != -1 )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_set_wfd( ssl, iSD ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_WANT )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_want( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_WANT_NOTHING )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_want_nothing( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_WANT_X509_LOOKUP )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_want_x509_lookup( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_WANT_READ )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_want_read( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_READ )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
      {
         PHB_ITEM pItem = hb_param( 2, HB_IT_STRING );
         char *   pBuffer;
         HB_SIZE  nLen;
         int      nRead = 0;

         if( pItem && HB_ISBYREF( 2 ) &&
             hb_itemGetWriteCL( pItem, &pBuffer, &nLen ) )
         {
            if( HB_ISNUM( 3 ) )
            {
               nRead = hb_parni( 3 );
               if( nRead >= 0 && nRead < ( int ) nLen )
                  nLen = nRead;
            }
            nRead = nLen >= INT_MAX ? INT_MAX : ( int ) nLen;

            nRead = SSL_read( ssl, pBuffer, nRead );
         }

         hb_retni( nRead );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_PEEK )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
      {
         PHB_ITEM pItem = hb_param( 2, HB_IT_STRING );
         char *   pBuffer;
         HB_SIZE  nLen;
         int      nRead = 0;

         if( pItem && HB_ISBYREF( 2 ) &&
             hb_itemGetWriteCL( pItem, &pBuffer, &nLen ) )
         {
            if( HB_ISNUM( 3 ) )
            {
               nRead = hb_parni( 3 );
               if( nRead >= 0 && nRead < ( int ) nLen )
                  nLen = nRead;
            }
            nRead = nLen >= INT_MAX ? INT_MAX : ( int ) nLen;

            nRead = SSL_peek( ssl, pBuffer, nRead );
         }

         hb_retni( nRead );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_WANT_WRITE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_want_write( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_WRITE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
      {
         PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
         HB_SIZE  nLen    = hb_itemGetCLen( pBuffer );

         if( HB_ISNUM( 3 ) )
         {
            HB_SIZE nWrite = ( HB_SIZE ) hb_parnl( 3 );
            if( nWrite < nLen )
               nLen = nWrite;
         }

         hb_retni( SSL_write( ssl, hb_itemGetCPtr( pBuffer ), ( int ) nLen ) );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_SSL_METHOD )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
#if OPENSSL_VERSION_NUMBER < 0x10000000L
         hb_retni( SSL_set_ssl_method( ssl, ( SSL_METHOD * ) hb_ssl_method_id_to_ptr( hb_parni( 2 ) ) ) );
#else
         hb_retni( SSL_set_ssl_method( ssl, hb_ssl_method_id_to_ptr( hb_parni( 2 ) ) ) );
#endif
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_SSL_METHOD )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
      {
#if OPENSSL_VERSION_NUMBER < 0x10000000L
         SSL_METHOD * p = SSL_get_ssl_method( ssl );
#else
         const SSL_METHOD * p = SSL_get_ssl_method( ssl );
#endif
         int n;

         if(      p == SSLv3_method()         ) n = HB_SSL_CTX_NEW_METHOD_SSLV3;
         else if( p == SSLv3_server_method()  ) n = HB_SSL_CTX_NEW_METHOD_SSLV3_SERVER;
         else if( p == SSLv3_client_method()  ) n = HB_SSL_CTX_NEW_METHOD_SSLV3_CLIENT;
#if OPENSSL_VERSION_NUMBER < 0x10000000L
         else if( p == SSLv2_method()         ) n = HB_SSL_CTX_NEW_METHOD_SSLV2;
         else if( p == SSLv2_server_method()  ) n = HB_SSL_CTX_NEW_METHOD_SSLV2_SERVER;
         else if( p == SSLv2_client_method()  ) n = HB_SSL_CTX_NEW_METHOD_SSLV2_CLIENT;
#endif
         else if( p == TLSv1_method()         ) n = HB_SSL_CTX_NEW_METHOD_TLSV1;
         else if( p == TLSv1_server_method()  ) n = HB_SSL_CTX_NEW_METHOD_TLSV1_SERVER;
         else if( p == TLSv1_client_method()  ) n = HB_SSL_CTX_NEW_METHOD_TLSV1_CLIENT;
         else if( p == SSLv23_method()        ) n = HB_SSL_CTX_NEW_METHOD_SSLV23;
         else if( p == SSLv23_server_method() ) n = HB_SSL_CTX_NEW_METHOD_SSLV23_SERVER;
         else if( p == SSLv23_client_method() ) n = HB_SSL_CTX_NEW_METHOD_SSLV23_CLIENT;
         else                                   n = HB_SSL_CTX_NEW_METHOD_UNKNOWN;

         hb_retni( n );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_CURRENT_CIPHER )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retptr( ( void * ) SSL_get_current_cipher( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_CIPHER_BITS )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
      {
         int alg_bits = 0;

         hb_retni( SSL_get_cipher_bits( ssl, &alg_bits ) );

         hb_storni( alg_bits, 2 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_CIPHER_LIST )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retc( SSL_get_cipher_list( ssl, hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_CIPHER_LIST )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl && hb_parclen( 2 ) <= 255 )
         hb_retni( SSL_set_cipher_list( ssl, hb_parcx( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_CIPHER_NAME )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retc( SSL_get_cipher_name( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_CIPHER_VERSION )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retc( SSL_get_cipher_version( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_COPY_SESSION_ID )
{
   if( hb_SSL_is( 1 ) && hb_SSL_is( 2 ) )
   {
      SSL * ssl1 = hb_SSL_par( 1 );
      SSL * ssl2 = hb_SSL_par( 2 );

      if( ssl1 && ssl2 )
         SSL_copy_session_id( ssl1, ssl2 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_SHARED_CIPHERS )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
      {
         char buffer[ 128 + 1 ]; /* See: CVE-2006-3738 */

         buffer[ 0 ] = '\0';

         hb_retc( SSL_get_shared_ciphers( ssl, buffer, sizeof( buffer ) - 1 ) );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_ALERT_DESC_STRING )
{
   hb_retc( SSL_alert_desc_string( hb_parni( 1 ) ) );
}

HB_FUNC( SSL_ALERT_DESC_STRING_LONG )
{
   hb_retc( SSL_alert_desc_string_long( hb_parni( 1 ) ) );
}

HB_FUNC( SSL_ALERT_TYPE_STRING )
{
   hb_retc( SSL_alert_type_string( hb_parni( 1 ) ) );
}

HB_FUNC( SSL_ALERT_TYPE_STRING_LONG )
{
   hb_retc( SSL_alert_type_string_long( hb_parni( 1 ) ) );
}

HB_FUNC( SSL_RSTATE_STRING )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retc( SSL_rstate_string( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_RSTATE_STRING_LONG )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retc( SSL_rstate_string( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_STATE_STRING )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retc( SSL_rstate_string( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_STATE_STRING_LONG )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retc( SSL_rstate_string( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#if 0

HB_FUNC( SSL_GET_PSK_IDENTITY_HINT )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retc( SSL_get_psk_identity_hint( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_PSK_IDENTITY )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retc( SSL_get_psk_identity( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#endif

HB_FUNC( SSL_CHECK_PRIVATE_KEY )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_check_private_key( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_ERROR )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_get_error( ssl, hb_parni( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_FD )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_get_fd( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_RFD )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_get_rfd( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_WFD )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_get_wfd( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_QUIET_SHUTDOWN )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_get_quiet_shutdown( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_SHUTDOWN )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_get_shutdown( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_READ_AHEAD )
{
   if( hb_SSL_is( 1 ) )
   {
#if defined( __BORLANDC__ ) /* TOFIX: SSL_get_read_ahead is an unresolved external when trying to link with BCC */
      hb_retni( 0 );
#else
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_get_read_ahead( ssl ) );
#endif
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_STATE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_get_state( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_VERIFY_MODE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_get_verify_mode( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_IN_ACCEPT_INIT )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_in_accept_init( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_IN_BEFORE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_in_before( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_IN_CONNECT_INIT )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_in_connect_init( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_IN_INIT )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_in_init( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_IS_INIT_FINISHED )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_is_init_finished( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_NUM_RENEGOTIATIONS )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retnl( SSL_num_renegotiations( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CLEAR_NUM_RENEGOTIATIONS )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retnl( SSL_clear_num_renegotiations( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_DEFAULT_TIMEOUT )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retnl( SSL_get_default_timeout( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_VERIFY_RESULT )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retnl( SSL_get_verify_result( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SESSION_REUSED )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retnl( SSL_session_reused( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_ACCEPT_STATE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         SSL_set_accept_state( ssl );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_CONNECT_STATE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         SSL_set_connect_state( ssl );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_OPTIONS )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retnl( SSL_get_options( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_OPTIONS )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         SSL_set_options( ssl, ( unsigned long ) hb_parnl( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_QUIET_SHUTDOWN )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         SSL_set_quiet_shutdown( ssl, hb_parni( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_READ_AHEAD )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         SSL_set_read_ahead( ssl, hb_parni( 2 ) /* yes */ );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_SHUTDOWN )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         SSL_set_shutdown( ssl, hb_parni( 2 ) /* mode */ );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_VERIFY_RESULT )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         SSL_set_verify_result( ssl, hb_parnl( 2 ) /* arg */ );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_MODE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         SSL_set_mode( ssl, hb_parnl( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_MODE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retnl( SSL_get_mode( ssl ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SET_MTU )
{
   if( hb_SSL_is( 1 ) )
   {
#if ! defined( HB_OPENSSL_OLD_OSX_ )
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         SSL_set_mtu( ssl, hb_parnl( 2 ) );
#endif
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_CERTIFICATE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_X509_ret( SSL_get_certificate( ssl ), HB_FALSE );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_PEER_CERTIFICATE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_X509_ret( SSL_get_peer_certificate( ssl ), HB_TRUE );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_USE_CERTIFICATE )
{
   if( hb_SSL_is( 1 ) && hb_X509_is( 2 ) )
   {
      SSL *  ssl  = hb_SSL_par( 1 );
      X509 * x509 = hb_X509_par( 2 );

      if( ssl && x509 )
         hb_retni( SSL_use_certificate( ssl, x509 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_ADD_CLIENT_CA )
{
   if( hb_SSL_is( 1 ) && hb_X509_is( 2 ) )
   {
      SSL *  ssl  = hb_SSL_par( 1 );
      X509 * x509 = hb_X509_par( 2 );

      if( ssl && x509 )
         hb_retni( SSL_add_client_CA( ssl, x509 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_USE_CERTIFICATE_FILE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_use_certificate_file( ssl, hb_parc( 2 ), hb_parni( 3 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_USE_PRIVATEKEY_FILE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_use_PrivateKey_file( ssl, hb_parc( 2 ), hb_parni( 3 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_USE_RSAPRIVATEKEY_FILE )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_use_RSAPrivateKey_file( ssl, hb_parc( 2 ), hb_parni( 3 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_CIPHERS )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
      {
         STACK_OF( SSL_CIPHER ) * stack = SSL_get_ciphers( ssl );
         int len = sk_SSL_CIPHER_num( stack );

         if( len > 0 )
         {
            PHB_ITEM pArray = hb_itemArrayNew( len );
            int      tmp;

            for( tmp = 0; tmp < len; tmp++ )
               hb_arraySetPtr( pArray, tmp + 1, sk_SSL_CIPHER_value( stack, tmp ) );

            hb_itemReturnRelease( pArray );
         }
         else
            hb_reta( 0 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_GET_CLIENT_CA_LIST )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
      {
         STACK_OF( X509_NAME ) * stack = SSL_get_client_CA_list( ssl );
         int len = sk_X509_NAME_num( stack );

         if( len > 0 )
         {
            PHB_ITEM pArray = hb_itemArrayNew( len );
            int      tmp;

            for( tmp = 0; tmp < len; tmp++ )
               hb_arraySetPtr( pArray, tmp + 1, sk_X509_NAME_value( stack, tmp ) );

            hb_itemReturnRelease( pArray );
         }
         else
            hb_reta( 0 );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_LOAD_CLIENT_CA_FILE )
{
   if( HB_ISCHAR( 1 ) )
   {
      STACK_OF( X509_NAME ) * stack = SSL_load_client_CA_file( hb_parc( 1 ) );
      int len = sk_X509_NAME_num( stack );

      if( len > 0 )
      {
         PHB_ITEM pArray = hb_itemArrayNew( len );
         int      tmp;

         for( tmp = 0; tmp < len; tmp++ )
            hb_arraySetPtr( pArray, tmp + 1, sk_X509_NAME_value( stack, tmp ) );

         hb_itemReturnRelease( pArray );
      }
      else
         hb_reta( 0 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_USE_RSAPRIVATEKEY_ASN1 )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         /* 'const' not used in 2nd param because ssh.h misses it, too.
             Bug report sent: #1988
             [vszakats] */
         hb_retni( SSL_use_RSAPrivateKey_ASN1( ssl, ( unsigned char * ) hb_parc( 2 ), ( int ) hb_parclen( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_USE_PRIVATEKEY_ASN1 )
{
   if( hb_SSL_is( 2 ) )
   {
      SSL * ssl = hb_SSL_par( 2 );

      if( ssl )
         hb_retni( SSL_use_PrivateKey_ASN1( hb_parni( 1 ), ssl, ( const unsigned char * ) hb_parc( 3 ), ( int ) hb_parclen( 3 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_USE_CERTIFICATE_ASN1 )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_use_certificate_ASN1( ssl, ( const unsigned char * ) hb_parc( 2 ), ( int ) hb_parclen( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_USE_PRIVATEKEY )
{
   if( hb_SSL_is( 1 ) && hb_EVP_PKEY_is( 2 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         /* QUESTION: It's unclear whether we should pass a copy here,
                      and who should free such passed EVP_PKEY object.
                      [vszakats] */
         hb_retni( SSL_use_PrivateKey( ssl, hb_EVP_PKEY_par( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* Callback */
/* -------- */

static void hb_ssl_msg_callback( int write_p, int version, int content_type, const void * buf, size_t len, SSL * ssl, void * userdata )
{
   HB_SYMBOL_UNUSED( ssl );

   if( userdata && hb_vmRequestReenter() )
   {
      hb_vmPushEvalSym();
      hb_vmPush( ( PHB_ITEM ) userdata );
      hb_vmPushLogical( write_p );
      hb_vmPushInteger( version );
      hb_vmPushInteger( content_type );
      hb_vmPushString( ( const char * ) buf, ( HB_SIZE ) len );
      hb_vmSend( 4 );

      hb_vmRequestRestore();
   }
}

HB_FUNC( SSL_SET_MSG_CALLBACK )
{
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
      {
         PHB_ITEM pCallback = hb_param( 2, HB_IT_BLOCK | HB_IT_SYMBOL );

         if( pCallback )
         {
            PHB_ITEM pPassCallback = hb_itemNew( pCallback );
            SSL_set_msg_callback_arg( ssl, pPassCallback );
            SSL_set_msg_callback( ssl, hb_ssl_msg_callback );
         }
         else
         {
            /* NOTE: WARNING: Direct access to OpenSSL internals. [vszakats] */
            hb_itemRelease( ( PHB_ITEM ) ssl->msg_callback_arg );
            SSL_set_msg_callback_arg( ssl, NULL );
            SSL_set_msg_callback( ssl, NULL );
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*

   void         SSL_set_psk_client_callback(SSL *ssl, unsigned int (*callback)(SSL *ssl, const char *hint, char *identity, unsigned int max_identity_len, unsigned char *psk, unsigned int max_psk_len));
   void         SSL_set_psk_server_callback(SSL *ssl, unsigned int (*callback)(SSL *ssl, const char *identity, unsigned char *psk, int max_psk_len));

   EVP_PKEY *   SSL_get_privatekey(SSL *ssl);

   STACK *      SSL_get_peer_cert_chain(const SSL *ssl);
   int          SSL_use_RSAPrivateKey(SSL *ssl, RSA *rsa);
   void         SSL_set_app_data(SSL *ssl, char *arg);
   int          SSL_set_ex_data(SSL *ssl, int idx, char *arg);
   char *       SSL_get_app_data(SSL *ssl);
   char *       SSL_get_ex_data( ssl, int );
   int          SSL_add_dir_cert_subjects_to_stack(STACK *stack, const char *dir);
   int          SSL_add_file_cert_subjects_to_stack(STACK *stack, const char *file);
   STACK *      SSL_dup_CA_list(STACK *sk);
   SSL_CTX *    SSL_get_SSL_CTX(const SSL *ssl);
   int          SSL_get_ex_data_X509_STORE_CTX_idx(void);
   int          SSL_get_ex_new_index(long argl, char *argp, int (*new_func);(void), int (*dup_func)(void), void (*free_func)(void))
   void (*SSL_get_info_callback(const SSL *ssl);)()
   SSL_SESSION *SSL_get_session(const SSL *ssl);
   int (*SSL_get_verify_callback(const SSL *ssl))(int,X509_STORE_CTX *)
   void         SSL_set_client_CA_list(SSL *ssl, STACK *list);
   void         SSL_set_info_callback(SSL *ssl, void (*cb);(void))
   void         SSL_set_verify(SSL *ssl, int mode, int (*callback);(void))
 */
