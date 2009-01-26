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
