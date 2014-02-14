/*
 * Harbour Project source code:
 * Harbour interface to CityHash string hashing
 *
 * Copyright 2014 Viktor Szakats (vszakats.net/harbour)
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

#include "city.h"

HB_FUNC( CITYHASH32 )
{
   hb_retnint( CityHash32( hb_parcx( 1 ), ( size_t ) hb_parclen( 1 ) ) );
}

HB_FUNC( CITYHASH64 )
{
   int p = hb_pcount();

   if( p == 1 )
      hb_retnint( CityHash64( hb_parcx( 1 ), ( size_t ) hb_parclen( 1 ) ) );
   else if( p == 2 )
      hb_retnint( CityHash64WithSeed( hb_parcx( 1 ), ( size_t ) hb_parclen( 1 ), ( uint64 ) hb_parnint( 2 ) ) );
   else if( p >= 3 )
      hb_retnint( CityHash64WithSeeds( hb_parcx( 1 ), ( size_t ) hb_parclen( 1 ), ( uint64 ) hb_parnint( 2 ), ( uint64 ) hb_parnint( 3 ) ) );
}

HB_FUNC( CITYHASH128 )
{
#if defined( HB_GET_LE_UINT128 )
   int p = hb_pcount();

   if( p == 1 )
      hb_retnint( CityHash128( hb_parcx( 1 ), ( size_t ) hb_parclen( 1 ) ) );
   else if( p >= 2 )
   hb_retnint( CityHash128WithSeed( hb_parcx( 1 ), ( size_t ) hb_parclen( 1 ), ( uint128 ) hb_parnint( 2 ) ) );
#else
   hb_errRT_BASE( EG_UNSUPPORTED, 2001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}
