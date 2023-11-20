/*
 * ED25519 wrappers
 *
 * Copyright 2015 Viktor Szakats
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

#include "hbapi.h"

#include "hbapiitm.h"
#include "hbapierr.h"

#include "ed25519.h"

/* hb_ed25519_create_keypair( @<public_key>, @<secret_key> ) --> NIL */
HB_FUNC( HB_ED25519_CREATE_KEYPAIR )
{
   unsigned char seed[ 32 ], public_key[ 32 ], secret_key[ 64 ];

   hb_random_block( seed, sizeof( seed ) );

   ed25519_create_keypair( public_key, secret_key, seed );

   hb_storclen( ( char * ) public_key, sizeof( public_key ), 1 );
   hb_storclen( ( char * ) secret_key, sizeof( secret_key ), 2 );

   hb_ret();
}

/* hb_ed25519_get_pubkey( <secret_key> ) --> <public_key> */
HB_FUNC( HB_ED25519_GET_PUBKEY )
{
   if( hb_parclen( 1 ) == 64 )
   {
      unsigned char public_key[ 32 ];

      ed25519_get_pubkey( public_key, ( const unsigned char * ) hb_parc( 1 ) );

      hb_retclen( ( char * ) public_key, sizeof( public_key ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3013, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* hb_ed25519_sign( <message>[, <public_key>], <secret_key> ) --> <signature>
   NOTE: <public_key> is always ignored. This argument is deprecated. */
HB_FUNC( HB_ED25519_SIGN )
{
   int secret_key_arg = hb_pcount() == 3 ? 3 : 2;  /* COMPATIBILITY */

   if( HB_ISCHAR( 1 ) &&
       hb_parclen( secret_key_arg ) == 64 )
   {
      unsigned char signature[ 64 ];

      ed25519_sign( signature,
                    ( const unsigned char * ) hb_parc( 1 ), ( size_t ) hb_parclen( 1 ),
                    NULL,
                    ( const unsigned char * ) hb_parc( secret_key_arg ) );

      hb_retclen( ( char * ) signature, sizeof( signature ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3013, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* hb_ed25519_verify( <signature>, <message>, <public_key> ) --> <lOK> */
HB_FUNC( HB_ED25519_VERIFY )
{
   if( hb_parclen( 1 ) == 64 &&
       HB_ISCHAR( 2 ) &&
       hb_parclen( 3 ) == 32 )
      hb_retl( ed25519_verify( ( const unsigned char * ) hb_parc( 1 ),
                               ( const unsigned char * ) hb_parc( 2 ), ( size_t ) hb_parclen( 2 ),
                               ( const unsigned char * ) hb_parc( 3 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3013, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#if 0

/* hb_ed25519_key_exchange( <public_key>, <secret_key> ) --> <shared_secret> */
HB_FUNC( HB_ED25519_KEY_EXCHANGE )
{
   if( hb_parclen( 1 ) == 32 &&
       hb_parclen( 2 ) == 64 )
   {
      unsigned char shared_secret[ 32 ];

      ed25519_key_exchange( shared_secret,
                            ( const unsigned char * ) hb_parc( 1 ),
                            ( const unsigned char * ) hb_parc( 2 ) );

      hb_retclen( ( char * ) shared_secret, sizeof( shared_secret ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3013, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

#endif
