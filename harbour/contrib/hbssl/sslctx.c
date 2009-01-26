/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OpenSSL API (SSL_CTX) - Harbour interface.
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

#include "hbssl.h"

HB_FUNC( SSL_INIT )
{
   SSL_load_error_strings();
   SSL_library_init();
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

   hb_retc_const( SSLeay_version( value ) );
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

HB_FUNC( SSL_CTX_NEW )
{
   void ** ph = ( void ** ) hb_gcAlloc( sizeof( SSL_CTX * ), SSL_CTX_release );

   SSL_CTX * ctx;
   SSL_METHOD * method;

   switch( hb_parni( 1 ) )
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

   ctx = SSL_CTX_new( method );

   * ph = ( void * ) ctx;

   hb_retptrGC( ph );
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

      if( ctx )
         SSL_CTX_set_cipher_list( ctx, hb_parcx( 2 ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
X509_STORE *SSL_CTX_get_cert_store(const SSL_CTX *);
void SSL_CTX_set_cert_store(SSL_CTX *,X509_STORE *);
*/
