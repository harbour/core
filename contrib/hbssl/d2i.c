/*
 * OpenSSL API (d2i) - Harbour interface.
 *
 * Copyright 2025 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbvm.h"

#include <openssl/x509.h>

HB_FUNC( D2I_PUBKEY )
{
   HB_SSL_CONST unsigned char * pszKeyDer = ( HB_SSL_CONST unsigned char * ) hb_parc( 1 );

   if( pszKeyDer )
   {
      EVP_PKEY * pKey = d2i_PUBKEY( NULL, &pszKeyDer, ( long ) hb_parclen( 1 ) );
      if( pKey )
         hb_EVP_PKEY_ret( pKey );
      else
         hb_retptr( NULL );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( D2I_RSAPUBLICKEY )
{
   HB_SSL_CONST unsigned char * pszKeyDer = ( HB_SSL_CONST unsigned char * ) hb_parc( 1 );

   if( pszKeyDer )
   {
      RSA * pKey = d2i_RSAPublicKey( NULL, &pszKeyDer, ( long ) hb_parclen( 1 ) );
      if( pKey )
         hb_RSA_ret( pKey );
      else
         hb_retptr( NULL );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( D2I_X509 )
{
   HB_SSL_CONST unsigned char * pszCrtDer = ( HB_SSL_CONST unsigned char * ) hb_parc( 1 );

   if( pszCrtDer )
   {
      X509 * x509 = d2i_X509( NULL, &pszCrtDer, ( long ) hb_parclen( 1 ) );
      if( x509 )
         hb_X509_ret( x509 );
      else
         hb_retptr( NULL );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
