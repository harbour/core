/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ROUND(), INT() functions
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
 *    INT()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <math.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

HB_FUNC( INT )
{
   PHB_ITEM pNumber = hb_param( 1, IT_NUMERIC );

   if( pNumber )
   {
      double dNumber = hb_itemGetND( pNumber );
      int iWidth;

      hb_itemGetNLen( pNumber, &iWidth, NULL );

      hb_retndlen( dNumber >= 0 ? floor( dNumber ) : ceil( dNumber ), iWidth, 0 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1090, NULL, "INT" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

double hb_numRound( double dResult, int iDec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_numRound(%lf, %d)", dResult, iDec));

   if( dResult != 0.0 )
   {
      if( iDec == 0 )
      {
         if( dResult < 0.0 )
            dResult = ceil( dResult - 0.5 );
         else
            dResult = floor( dResult + 0.5 );
      }
      else if( iDec < 0 )
      {
         double dAdjust = pow( 10, -iDec );

         if( dResult < 0.0 )
            dResult = ceil( ( dResult / dAdjust ) - 0.5 );
         else
            dResult = floor( ( dResult / dAdjust ) + 0.5 );

         dResult *= dAdjust;
      }
      else
      {
         double dAdjust = pow( 10, iDec );

         if( dResult < 0.0 )
            dResult = ceil( ( dResult * dAdjust ) - 0.5 );
         else
            dResult = floor( ( dResult * dAdjust ) + 0.5 );

         dResult /= dAdjust;
      }
   }

   return dResult;
}

HB_FUNC( ROUND )
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
   {
      int iDec = hb_parni( 2 );

      hb_retndlen( hb_numRound( hb_parnd( 1 ), iDec ), 0, HB_MAX( iDec, 0 ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1094, NULL, "ROUND" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}
