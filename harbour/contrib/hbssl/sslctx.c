/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OpenSSL API (SSL_CTX) - Harbour interface.
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu)
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
#include "hbapierr.h"

#include "hbssl.h"

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
   case HB_SSLEAY_VERSION  : value = SSLEAY_VERSION;  break;
   case HB_SSLEAY_CFLAGS   : value = SSLEAY_CFLAGS;   break;
   case HB_SSLEAY_BUILT_ON : value = SSLEAY_BUILT_ON; break;
   case HB_SSLEAY_PLATFORM : value = SSLEAY_PLATFORM; break;
   case HB_SSLEAY_DIR      : value = SSLEAY_DIR;      break;
   }

   hb_retc( SSLeay_version( value ) );
}

static HB_GARBAGE_FUNC( SSL_CTX_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      SSL_CTX_free( ( SSL_CTX * ) * ph );

      /* set pointer to NULL just in case */
      * ph = NULL;
   }
}

void * hb_SSL_CTX_is( int iParam )
{
   return hb_parptrGC( SSL_CTX_release, iParam );
}

SSL_CTX * hb_SSL_CTX_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( SSL_CTX_release, iParam );

   return ph ? ( SSL_CTX * ) * ph : NULL;
}

SSL_METHOD * hb_ssl_method_id_to_ptr( int n )
{
   SSL_METHOD * method;

   switch( n )
   {
   case HB_SSL_CTX_NEW_METHOD_SSLV2         : method = SSLv2_method();         break;
   case HB_SSL_CTX_NEW_METHOD_SSLV2_SERVER  : method = SSLv2_server_method();  break;
   case HB_SSL_CTX_NEW_METHOD_SSLV2_CLIENT  : method = SSLv2_client_method();  break;
   case HB_SSL_CTX_NEW_METHOD_SSLV3         : method = SSLv3_method();         break;
   case HB_SSL_CTX_NEW_METHOD_SSLV3_SERVER  : method = SSLv3_server_method();  break;
   case HB_SSL_CTX_NEW_METHOD_SSLV3_CLIENT  : method = SSLv3_client_method();  break;
   case HB_SSL_CTX_NEW_METHOD_TLSV1         : method = TLSv1_method();         break;
   case HB_SSL_CTX_NEW_METHOD_TLSV1_SERVER  : method = TLSv1_server_method();  break;
   case HB_SSL_CTX_NEW_METHOD_TLSV1_CLIENT  : method = TLSv1_client_method();  break;
   case HB_SSL_CTX_NEW_METHOD_SSLV23        : method = SSLv23_method();        break;
   case HB_SSL_CTX_NEW_METHOD_SSLV23_SERVER : method = SSLv23_server_method(); break;
   case HB_SSL_CTX_NEW_METHOD_SSLV23_CLIENT : method = SSLv23_client_method(); break;
   default                                  : method = SSLv23_method();
   }

   return method;
}

HB_FUNC( SSL_CTX_NEW )
{
   void ** ph = ( void ** ) hb_gcAlloc( sizeof( SSL_CTX * ), SSL_CTX_release );

   SSL_CTX * ctx = SSL_CTX_new( hb_ssl_method_id_to_ptr( HB_ISNUM( 1 ) ? hb_parni( 1 ) : HB_SSL_CTX_NEW_METHOD_DEFAULT ) );

   * ph = ( void * ) ctx;

   hb_retptrGC( ph );
}

HB_FUNC( SSL_CTX_SET_SSL_VERSION )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_set_ssl_version( ctx, hb_ssl_method_id_to_ptr( hb_parni( 2 ) ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_GET_TIMEOUT )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retnl( SSL_CTX_get_timeout( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SET_TIMEOUT )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_set_timeout( ctx, hb_parnl( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SET_CIPHER_LIST )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx && hb_parclen( 2 ) <= 255 )
         SSL_CTX_set_cipher_list( ctx, hb_parcx( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_ADD_SESSION )
{
   if( hb_SSL_CTX_is( 1 ) && hb_SSL_SESSION_is( 2 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );
      SSL_SESSION * session = hb_SSL_SESSION_par( 2 );

      if( ctx && session )
         hb_retni( SSL_CTX_add_session( ctx, session ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_REMOVE_SESSION )
{
   if( hb_SSL_CTX_is( 1 ) && hb_SSL_SESSION_is( 2 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );
      SSL_SESSION * session = hb_SSL_SESSION_par( 2 );

      if( ctx && session )
         hb_retni( SSL_CTX_remove_session( ctx, session ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_FLUSH_SESSIONS )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_flush_sessions( ctx, hb_parnl( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_GET_SESSION_CACHE_MODE )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_get_session_cache_mode( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SET_SESSION_CACHE_MODE )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_set_session_cache_mode( ctx, hb_parni( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_CHECK_PRIVATE_KEY )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_check_private_key( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_GET_QUIET_SHUTDOWN )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_get_quiet_shutdown( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_GET_VERIFY_MODE )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_get_verify_mode( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_ACCEPT )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_sess_accept( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_ACCEPT_GOOD )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_sess_accept_good( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_ACCEPT_RENEGOTIATE )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_sess_accept_renegotiate( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_CACHE_FULL )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_sess_cache_full( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_CB_HITS )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_sess_cb_hits( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_CONNECT )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_sess_connect( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_CONNECT_GOOD )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_sess_connect_good( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_CONNECT_RENEGOTIATE )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_sess_connect_renegotiate( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_GET_CACHE_SIZE )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_sess_get_cache_size( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_HITS )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_sess_hits( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_MISSES )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_sess_misses( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_NUMBER )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_sess_number( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_TIMEOUTS )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_sess_timeouts( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SET_DEFAULT_VERIFY_PATHS )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_set_default_verify_paths( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_NEED_TMP_RSA )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retnl( SSL_CTX_need_tmp_RSA( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SESS_SET_CACHE_SIZE )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_sess_set_cache_size( ctx, hb_parni( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SET_DEFAULT_READ_AHEAD )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_set_default_read_ahead( ctx, hb_parni( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SET_OPTIONS )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_set_options( ctx, ( unsigned long ) hb_parnl( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SET_QUIET_SHUTDOWN )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_set_quiet_shutdown( ctx, hb_parni( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_SET_MODE )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         SSL_CTX_set_mode( ctx, hb_parnl( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_GET_MODE )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_parnl( SSL_CTX_get_mode( ctx ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_USE_CERTIFICATE )
{
   if( hb_SSL_CTX_is( 1 ) && hb_X509_is( 2 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );
      X509 * x509 = hb_X509_par( 2 );

      if( ctx && x509 )
         hb_retni( SSL_CTX_use_certificate( ctx, x509 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_ADD_CLIENT_CA )
{
   if( hb_SSL_CTX_is( 1 ) && hb_X509_is( 2 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );
      X509 * x509 = hb_X509_par( 2 );

      if( ctx && x509 )
         hb_retni( SSL_CTX_add_client_CA( ctx, x509 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_ADD_EXTRA_CHAIN_CERT )
{
   if( hb_SSL_CTX_is( 1 ) && hb_X509_is( 2 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );
      X509 * x509 = hb_X509_par( 2 );

      if( ctx && x509 )
         hb_retnl( SSL_CTX_add_extra_chain_cert( ctx, x509 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_USE_CERTIFICATE_FILE )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_use_certificate_file( ctx, hb_parc( 2 ), hb_parni( 3 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_USE_CERTIFICATE_CHAIN_FILE )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_use_certificate_chain_file( ctx, hb_parc( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_USE_PRIVATEKEY_FILE )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_use_PrivateKey_file( ctx, hb_parc( 2 ), hb_parni( 3 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CTX_USE_RSAPRIVATEKEY_FILE )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
         hb_retni( SSL_CTX_use_RSAPrivateKey_file( ctx, hb_parc( 2 ), hb_parni( 3 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
int SSL_CTX_use_certificate_ASN1(SSL_CTX *ctx, int len, unsigned char *d);

X509_STORE *SSL_CTX_get_cert_store(const SSL_CTX *);
void SSL_CTX_set_cert_store(SSL_CTX *,X509_STORE *);
void SSL_CTX_set_cert_store(SSL_CTX *ctx, X509_STORE *cs);
int  SSL_CTX_use_PrivateKey(SSL_CTX *ctx, EVP_PKEY *pkey);
int  SSL_CTX_use_PrivateKey_ASN1(int type, SSL_CTX *ctx, unsigned char *d, long len);
int  SSL_CTX_use_RSAPrivateKey(SSL_CTX *ctx, RSA *rsa);
int  SSL_CTX_use_RSAPrivateKey_ASN1(SSL_CTX *ctx, unsigned char *d, long len);
long SSL_CTX_ctrl(SSL_CTX *ctx, int cmd, long larg, char *parg);
STACK *SSL_CTX_get_client_CA_list(const SSL_CTX *ctx);

void SSL_CTX_set_app_data(SSL_CTX *ctx, void *arg);
int SSL_CTX_set_ex_data(SSL_CTX *s, int idx, char *arg);
char * SSL_CTX_get_app_data( ctx );
char * SSL_CTX_get_ex_data( ctx, int );

int (*SSL_CTX_get_client_cert_cb(SSL_CTX *ctx))(SSL *ssl, X509 **x509, EVP_PKEY **pkey);
int SSL_CTX_get_ex_new_index(long argl, char *argp, int (*new_func);(void), int (*dup_func)(void), void (*free_func)(void))
void (*SSL_CTX_get_info_callback(SSL_CTX *ctx))(SSL *ssl, int cb, int ret);
int (*SSL_CTX_get_verify_callback(const SSL_CTX *ctx))(int ok, X509_STORE_CTX *ctx);
int SSL_CTX_load_verify_locations(SSL_CTX *ctx, char *CAfile, char *CApath);
SSL_SESSION *(*SSL_CTX_sess_get_get_cb(SSL_CTX *ctx))(SSL *ssl, unsigned char *data, int len, int *copy);
int (*SSL_CTX_sess_get_new_cb(SSL_CTX *ctx)(SSL *ssl, SSL_SESSION *sess);
void (*SSL_CTX_sess_get_remove_cb(SSL_CTX *ctx)(SSL_CTX *ctx, SSL_SESSION *sess);
void SSL_CTX_sess_set_get_cb(SSL_CTX *ctx, SSL_SESSION *(*cb)(SSL *ssl, unsigned char *data, int len, int *copy));
void SSL_CTX_sess_set_new_cb(SSL_CTX *ctx, int (*cb)(SSL *ssl, SSL_SESSION *sess));
void SSL_CTX_sess_set_remove_cb(SSL_CTX *ctx, void (*cb)(SSL_CTX *ctx, SSL_SESSION *sess));
LHASH *SSL_CTX_sessions(SSL_CTX *ctx);
void SSL_CTX_set_cert_verify_cb(SSL_CTX *ctx, int (*cb)(), char *arg)
void SSL_CTX_set_client_CA_list(SSL_CTX *ctx, STACK *list);
void SSL_CTX_set_client_cert_cb(SSL_CTX *ctx, int (*cb)(SSL *ssl, X509 **x509, EVP_PKEY **pkey));
void SSL_CTX_set_default_passwd_cb(SSL_CTX *ctx, int (*cb);(void))
void SSL_CTX_set_info_callback(SSL_CTX *ctx, void (*cb)(SSL *ssl, int cb, int ret));
void SSL_CTX_set_msg_callback(SSL_CTX *ctx, void (*cb)(int write_p, int version, int content_type, const void *buf, size_t len, SSL *ssl, void *arg));
void SSL_CTX_set_msg_callback_arg(SSL_CTX *ctx, void *arg);
long SSL_CTX_set_tmp_dh(SSL_CTX* ctx, DH *dh);
long SSL_CTX_set_tmp_dh_callback(SSL_CTX *ctx, DH *(*cb)(void));
long SSL_CTX_set_tmp_rsa(SSL_CTX *ctx, RSA *rsa);
SSL_CTX_set_tmp_rsa_callback
long <STRONG>SSL_CTX_set_tmp_rsa_callback</STRONG>(SSL_CTX *<STRONG>ctx</STRONG>, RSA *(*<STRONG>cb</STRONG>)(SSL *<STRONG>ssl</STRONG>, int <STRONG>export</STRONG>, int <STRONG>keylength</STRONG>));
Sets the callback which will be called when a temporary private key is required. The export flag will be set if the reason for needing a temp key is that an export ciphersuite is in use, in which case, keylength will contain the required keylength in bits. Generate a key of appropriate size (using ???) and return it.
SSL_set_tmp_rsa_callback
long SSL_set_tmp_rsa_callback(SSL *ssl, RSA *(*cb)(SSL *ssl, int export, int keylength));
The same as SSL_CTX_set_tmp_rsa_callback, except it operates on an SSL session instead of a context.
void SSL_CTX_set_verify(SSL_CTX *ctx, int mode, int (*cb);(void))
void SSL_CTX_set_psk_client_callback(SSL_CTX *ctx, unsigned int (*callback)(SSL *ssl, const char *hint, char *identity, unsigned int max_identity_len, unsigned char *psk, unsigned int max_psk_len));
void SSL_CTX_set_psk_server_callback(SSL_CTX *ctx, unsigned int (*callback)(SSL *ssl, const char *identity, unsigned char *psk, int max_psk_len));
*/
