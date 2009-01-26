/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OpenSSL API (SSL) - Harbour interface.
 *
 * Copyright 2009 Viktor Szakats <harbour 01 syenar hu>
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
#include "hbapiitm.h"

#include "hbssl.h"

static HB_GARBAGE_FUNC( SSL_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      SSL_free( ( SSL * ) * ph );

      /* set pointer to NULL just in case */
      * ph = NULL;
   }
}

void * hb_SSL_is( int iParam )
{
   return hb_parptrGC( SSL_release, iParam );
}

SSL * hb_SSL_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( SSL_release, iParam );

   return ph ? ( SSL * ) * ph : NULL;
}

HB_FUNC( SSL_NEW )
{
   if( hb_SSL_CTX_is( 1 ) )
   {
      SSL_CTX * ctx = hb_SSL_CTX_par( 1 );

      if( ctx )
      {
         void ** ph = ( void ** ) hb_gcAlloc( sizeof( SSL * ), SSL_release );

         SSL * ssl = SSL_new( ctx );

         * ph = ( void * ) ssl;

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
         void ** ph = ( void ** ) hb_gcAlloc( sizeof( SSL * ), SSL_release );

         SSL * ssl = SSL_dup( ssl_par );

         * ph = ( void * ) ssl;

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
         hb_retc_const( SSL_get_cipher( ssl ) );
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
   if( hb_SSL_is( 1 ) )
   {
      SSL * ssl = hb_SSL_par( 1 );

      if( ssl )
         hb_retni( SSL_set_fd( ssl, hb_parni( 2 ) ) );
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
         PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
         int nRead;

         if( pBuffer && ISBYREF( 2 ) && ISNUM( 3 ) )
         {
            nRead = hb_parni( 3 );

            if( ( ULONG ) nRead <= hb_parcsiz( 2 ) )
            {
               pBuffer = hb_itemUnShareString( pBuffer );

               nRead = SSL_read( ssl, ( void * ) hb_itemGetCPtr( pBuffer ), nRead );
            }
            else
               nRead = 0;
         }
         else
            nRead = 0;

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
         PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
         int nRead;

         if( pBuffer && ISBYREF( 2 ) && ISNUM( 3 ) )
         {
            nRead = hb_parni( 3 );

            if( ( ULONG ) nRead <= hb_parcsiz( 2 ) )
            {
               pBuffer = hb_itemUnShareString( pBuffer );

               nRead = SSL_peek( ssl, ( void * ) hb_itemGetCPtr( pBuffer ), nRead );
            }
            else
               nRead = 0;
         }
         else
            nRead = 0;

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
         ULONG nLen = hb_itemGetCLen( pBuffer );

         if( ISNUM( 3 ) )
         {
            ULONG nWrite = ( ULONG ) hb_parnl( 3 );
            if( nWrite < nLen )
               nLen = nWrite;
         }

         hb_retni( SSL_read( ssl, ( void * ) hb_itemGetCPtr( pBuffer ), ( int ) nLen ) );
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
         hb_retni( SSL_set_ssl_method( ssl, hb_ssl_method_id_to_ptr( hb_parni( 2 ) ) ) );
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

/*
int SSL_add_dir_cert_subjects_to_stack(STACK *stack, const char *dir);
int SSL_add_file_cert_subjects_to_stack(STACK *stack, const char *file);
int SSL_add_client_CA(SSL *ssl, X509 *x);
char *SSL_alert_desc_string(int value);
char *SSL_alert_desc_string_long(int value);
char *SSL_alert_type_string(int value);
char *SSL_alert_type_string_long(int value);
int SSL_check_private_key(const SSL *ssl);
long SSL_clear_num_renegotiations(SSL *ssl);
void SSL_copy_session_id(SSL *t, const SSL *f);
long SSL_ctrl(SSL *ssl, int cmd, long larg, char *parg);
STACK *SSL_dup_CA_list(STACK *sk);
SSL_CTX *SSL_get_SSL_CTX(const SSL *ssl);
char *SSL_get_app_data(SSL *ssl);
X509 *SSL_get_certificate(const SSL *ssl);
int SSL_get_cipher_bits(const SSL *ssl, int *alg_bits);
char *SSL_get_cipher_list(const SSL *ssl, int n);
char *SSL_get_cipher_name(const SSL *ssl);
char *SSL_get_cipher_version(const SSL *ssl);
STACK *SSL_get_ciphers(const SSL *ssl);
STACK *SSL_get_client_CA_list(const SSL *ssl);
long SSL_get_default_timeout(const SSL *ssl);
int SSL_get_error(const SSL *ssl, int i);
char *SSL_get_ex_data(const SSL *ssl, int idx);
int SSL_get_ex_data_X509_STORE_CTX_idx(void);
int SSL_get_ex_new_index(long argl, char *argp, int (*new_func);(void), int (*dup_func)(void), void (*free_func)(void))
int SSL_get_fd(const SSL *ssl);
void (*SSL_get_info_callback(const SSL *ssl);)()
STACK *      SSL_get_peer_cert_chain(const SSL *ssl);
X509 *       SSL_get_peer_certificate(const SSL *ssl);
EVP_PKEY *   SSL_get_privatekey(SSL *ssl);
int          SSL_get_quiet_shutdown(const SSL *ssl);
BIO *        SSL_get_rbio(const SSL *ssl);
int          SSL_get_read_ahead(const SSL *ssl);
SSL_SESSION *SSL_get_session(const SSL *ssl);
char *      SSL_get_shared_ciphers(const SSL *ssl, char *buf, int len);
int         SSL_get_shutdown(const SSL *ssl);
const SSL_METHOD *SSL_get_ssl_method(SSL *ssl);
int         SSL_get_state(const SSL *ssl);
int (*SSL_get_verify_callback(const SSL *ssl))(int,X509_STORE_CTX *)
int         SSL_get_verify_mode(const SSL *ssl);
long        SSL_get_verify_result(const SSL *ssl);
BIO *       SSL_get_wbio(const SSL *ssl);
int         SSL_in_accept_init(SSL *ssl);
int         SSL_in_before(SSL *ssl);
int         SSL_in_connect_init(SSL *ssl);
int         SSL_in_init(SSL *ssl);
int         SSL_is_init_finished(SSL *ssl);
STACK *     SSL_load_client_CA_file(char *file);
void        SSL_load_error_strings(void);
long        SSL_num_renegotiations(SSL *ssl);
char *      SSL_rstate_string(SSL *ssl);
char *      SSL_rstate_string_long(SSL *ssl);
long        SSL_session_reused(SSL *ssl);
void        SSL_set_accept_state(SSL *ssl);
void        SSL_set_app_data(SSL *ssl, char *arg);
void        SSL_set_bio(SSL *ssl, BIO *rbio, BIO *wbio);
int         SSL_set_cipher_list(SSL *ssl, char *str);
void        SSL_set_client_CA_list(SSL *ssl, STACK *list);
void        SSL_set_connect_state(SSL *ssl);
int         SSL_set_ex_data(SSL *ssl, int idx, char *arg);
int         SSL_set_fd(SSL *ssl, int fd);
void        SSL_set_info_callback(SSL *ssl, void (*cb);(void))
void        SSL_set_msg_callback(SSL *ctx, void (*cb)(int write_p, int version, int content_type, const void *buf, size_t len, SSL *ssl, void *arg));
void        SSL_set_msg_callback_arg(SSL *ctx, void *arg);
void        SSL_set_options(SSL *ssl, unsigned long op);
void        SSL_set_quiet_shutdown(SSL *ssl, int mode);
void        SSL_set_read_ahead(SSL *ssl, int yes);
int         SSL_set_rfd(SSL *ssl, int fd);
int         SSL_set_session(SSL *ssl, SSL_SESSION *session);
void        SSL_set_shutdown(SSL *ssl, int mode);
void        SSL_set_verify(SSL *ssl, int mode, int (*callback);(void))
void        SSL_set_verify_result(SSL *ssl, long arg);
int         SSL_set_wfd(SSL *ssl, int fd);
char *      SSL_state_string(const SSL *ssl);
char *      SSL_state_string_long(const SSL *ssl);
int         SSL_use_PrivateKey(SSL *ssl, EVP_PKEY *pkey);
int         SSL_use_PrivateKey_ASN1(int type, SSL *ssl, unsigned char *d, long len);
int         SSL_use_PrivateKey_file(SSL *ssl, char *file, int type);
int         SSL_use_RSAPrivateKey(SSL *ssl, RSA *rsa);
int         SSL_use_RSAPrivateKey_ASN1(SSL *ssl, unsigned char *d, long len);
int         SSL_use_RSAPrivateKey_file(SSL *ssl, char *file, int type);
int         SSL_use_certificate(SSL *ssl, X509 *x);
int         SSL_use_certificate_ASN1(SSL *ssl, int len, unsigned char *d);
int         SSL_use_certificate_file(SSL *ssl, char *file, int type);
void        SSL_set_psk_client_callback(SSL *ssl, unsigned int (*callback)(SSL *ssl, const char *hint, char *identity, unsigned int max_identity_len, unsigned char *psk, unsigned int max_psk_len));
int         SSL_use_psk_identity_hint(SSL *ssl, const char *hint);
void        SSL_set_psk_server_callback(SSL *ssl, unsigned int (*callback)(SSL *ssl, const char *identity, unsigned char *psk, int max_psk_len));
const char *SSL_get_psk_identity_hint(SSL *ssl);
const char *SSL_get_psk_identity(SSL *ssl);
*/
