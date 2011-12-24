/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OpenSSL API (EVP) - Harbour interface.
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.hu)
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

#include <openssl/evp.h>

char * hb_openssl_strdup( const char * pszText )
{
   char * pszDup;
   size_t len = strlen( pszText ) + 1;

   pszDup = ( char * ) OPENSSL_malloc( len );
   memcpy( pszDup, pszText, len );

   return pszDup;
}

HB_FUNC( OPENSSL_ADD_ALL_ALGORITHMS )
{
   OpenSSL_add_all_algorithms();
}

HB_FUNC( EVP_CLEANUP )
{
   EVP_cleanup();
}

HB_FUNC( ERR_LOAD_EVP_STRINGS )
{
   ERR_load_EVP_strings();
}

HB_FUNC( EVP_PKEY_FREE )
{
   EVP_PKEY * key = ( EVP_PKEY * ) hb_parptr( 1 );

   if( key )
      EVP_PKEY_free( key );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( EVP_BYTESTOKEY )
{
   const EVP_CIPHER * cipher = hb_EVP_CIPHER_par( 1 );
   const EVP_MD * md = hb_EVP_MD_par( 2 );

   if( cipher && md && ( ! HB_ISCHAR( 3 ) || hb_parclen( 3 ) == 8 ) )
   {
      unsigned char key[ EVP_MAX_KEY_LENGTH ];
      unsigned char iv[ EVP_MAX_IV_LENGTH ];

      hb_retni( EVP_BytesToKey( cipher,
                                md,
                                ( const unsigned char * ) hb_parc( 3 ) /* salt */,
                                ( const unsigned char * ) hb_parcx( 4 ) /* data */,
                                ( int ) hb_parclen( 4 ),
                                hb_parni( 5 ) /* count */,
                                key,
                                iv ) );

      hb_storc( ( char * ) key, 6 );
      hb_storc( ( char * ) iv, 7 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
