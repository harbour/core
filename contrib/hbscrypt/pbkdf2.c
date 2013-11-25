/*
 * Harbour Project source code:
 * Harbour interface to PBKDF2 password hashing
 *
 * Copyright 2013 Viktor Szakats (vszakats.net/harbour)
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

#include "hbapi.h"
#include "hbapierr.h"

#include "sha256.h"

/* hb_pbkdf2_sha256( <cPassword>, <cSalt>, <nCount>, <nKeyLen> ) -> <cKey> */
HB_FUNC( HB_PBKDF2_SHA256 )
{
   HB_U64 nC = hb_parnintdef( 3, 1 );
   HB_ISIZ keylen = hb_parns( 4 );

   if( HB_ISCHAR( 1 ) &&
       HB_ISCHAR( 2 ) &&
       keylen >= 16 &&
       nC >= 1 )
   {
      uint8_t * buf = ( uint8_t * ) hb_xgrab( keylen + 1 );

      buf[ 0 ] = '\0';

      PBKDF2_SHA256(
         ( uint8_t * ) hb_parc( 1 ), ( size_t ) hb_parclen( 1 ),
         ( uint8_t * ) hb_parc( 2 ), ( size_t ) hb_parclen( 2 ),
         nC,
         buf, keylen );

      hb_retclen_buffer( ( char * ) buf, keylen );
   }
   else
      hb_errRT_BASE( EG_ARG, 2101, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
