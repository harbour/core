/*
 * Harbour Project source code:
 * OpenSSL API (SSL_CIPHER) - Harbour interface.
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

HB_FUNC( SSL_CIPHER_DESCRIPTION )
{
   if( HB_ISPOINTER( 1 ) )
   {
      char buffer[ 128 ];

      buffer[ 0 ] = '\0';

      hb_retc( hb_parptr( 1 ) ? SSL_CIPHER_description( ( SSL_CIPHER * ) hb_parptr( 1 ), buffer, sizeof( buffer ) ) : NULL );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CIPHER_GET_BITS )
{
   if( HB_ISPOINTER( 1 ) )
   {
      int alg_bits = 0;

      hb_retni( hb_parptr( 1 ) ? SSL_CIPHER_get_bits( ( SSL_CIPHER * ) hb_parptr( 1 ), &alg_bits ) : 0 );

      hb_storni( alg_bits, 2 );
   }
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CIPHER_GET_NAME )
{
   if( HB_ISPOINTER( 1 ) )
      hb_retc( hb_parptr( 1 ) ? SSL_CIPHER_get_name( ( SSL_CIPHER * ) hb_parptr( 1 ) ) : NULL );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( SSL_CIPHER_GET_VERSION )
{
   if( HB_ISPOINTER( 1 ) )
      hb_retc( hb_parptr( 1 ) ? SSL_CIPHER_get_version( ( SSL_CIPHER * ) hb_parptr( 1 ) ) : NULL );
   else
      hb_errRT_BASE( EG_ARG, 2010, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
