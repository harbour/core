/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OpenSSL API (SSL_SESSION) - Harbour interface.
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

#include "hbssl.h"

static HB_GARBAGE_FUNC( SSL_SESSION_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && * ph )
   {
      /* Destroy the object */
      SSL_SESSION_free( ( SSL_SESSION * ) * ph );

      /* set pointer to NULL just in case */
      * ph = NULL;
   }
}

static const HB_GC_FUNCS s_gcSSL_SESSION_funcs =
{
   SSL_SESSION_release,
   hb_gcDummyMark
};

void * hb_SSL_SESSION_is( int iParam )
{
   return hb_parptrGC( &s_gcSSL_SESSION_funcs, iParam );
}

SSL_SESSION * hb_SSL_SESSION_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gcSSL_SESSION_funcs, iParam );

   return ph ? ( SSL_SESSION * ) * ph : NULL;
}

HB_FUNC( SSL_SESSION_NEW )
{
   void ** ph = ( void ** ) hb_gcAllocate( sizeof( SSL_SESSION * ), &s_gcSSL_SESSION_funcs );

   SSL_SESSION * session = SSL_SESSION_new();

   * ph = ( void * ) session;

   hb_retptrGC( ph );
}

HB_FUNC( SSL_SESSION_CMP )
{
   if( hb_SSL_SESSION_is( 1 ) && hb_SSL_SESSION_is( 2 ) )
   {
#if OPENSSL_VERSION_NUMBER < 0x10000000L
      SSL_SESSION * session1 = hb_SSL_SESSION_par( 1 );
      SSL_SESSION * session2 = hb_SSL_SESSION_par( 2 );

      if( session1 && session2 )
         hb_retni( SSL_SESSION_cmp( session1, session2 ) );
#endif
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SESSION_SET_TIME )
{
   if( hb_SSL_SESSION_is( 1 ) )
   {
      SSL_SESSION * session = hb_SSL_SESSION_par( 1 );

      if( session )
         hb_retnl( SSL_SESSION_set_time( session, hb_parnl( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SESSION_SET_TIMEOUT )
{
   if( hb_SSL_SESSION_is( 1 ) )
   {
      SSL_SESSION * session = hb_SSL_SESSION_par( 1 );

      if( session )
         hb_retnl( SSL_SESSION_set_timeout( session, hb_parnl( 2 ) ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SESSION_GET_TIME )
{
   if( hb_SSL_SESSION_is( 1 ) )
   {
      SSL_SESSION * session = hb_SSL_SESSION_par( 1 );

      if( session )
         hb_retnl( SSL_SESSION_get_time( session ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SESSION_GET_TIMEOUT )
{
   if( hb_SSL_SESSION_is( 1 ) )
   {
      SSL_SESSION * session = hb_SSL_SESSION_par( 1 );

      if( session )
         hb_retnl( SSL_SESSION_get_timeout( session ) );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_SESSION_HASH )
{
   if( hb_SSL_SESSION_is( 1 ) )
   {
#if OPENSSL_VERSION_NUMBER < 0x10000000L
      SSL_SESSION * session = hb_SSL_SESSION_par( 1 );

      if( session )
         hb_retnl( SSL_SESSION_hash( session ) );
#endif
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
char *SSL_SESSION_get_app_data(SSL_SESSION *s);
char *SSL_SESSION_get_ex_data(const SSL_SESSION *s, int idx);
void SSL_SESSION_set_app_data(SSL_SESSION *s, char *a);
int SSL_SESSION_set_ex_data(SSL_SESSION *s, int idx, char *arg);

int SSL_SESSION_get_ex_new_index(long argl, char *argp, int (*new_func)(void), int (*dup_func)(void), void (*free_func)(void))
int SSL_SESSION_print(BIO *bp, const SSL_SESSION *x);
int SSL_SESSION_print_fp(FILE *fp, const SSL_SESSION *x);
*/
