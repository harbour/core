/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MIN(), MAX() functions
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* returns the maximum of two date or numerics */
HB_FUNC( MAX )
{
   PHB_ITEM p1 = hb_param( 1, HB_IT_ANY );
   PHB_ITEM p2 = hb_param( 2, HB_IT_ANY );

   if( HB_IS_NUMERIC( p1 ) && HB_IS_NUMERIC( p2 ) )
   {
      /* NOTE: The order of these if() branches is significant,
               please, don't change it. [vszakats] */

      if( HB_IS_DOUBLE( p1 ) || HB_IS_DOUBLE( p2 ) )
      {
         double d1 = hb_itemGetND( p1 );
         double d2 = hb_itemGetND( p2 );

         int iDec1;
         int iDec2;

         hb_itemGetNLen( p1, NULL, &iDec1 );
         hb_itemGetNLen( p2, NULL, &iDec2 );

         if( d1 >= d2 )
            hb_retndlen( d1, 0, iDec1 );
         else
            hb_retndlen( d2, 0, iDec2 );
      }
      else if( HB_IS_LONG( p1 ) || HB_IS_LONG( p2 ) )
      {
         long l1 = hb_itemGetNL( p1 );
         long l2 = hb_itemGetNL( p2 );

         hb_retnl( l1 >= l2 ? l1 : l2 );
      }
      else
      {
         int i1 = hb_itemGetNI( p1 );
         int i2 = hb_itemGetNI( p2 );

         hb_retni( i1 >= i2 ? i1 : i2 );
      }
   }
   else if( HB_IS_DATE( p1 ) && HB_IS_DATE( p2 ) )
   {
      char szDate[ 9 ];

      hb_retds( hb_itemGetDL( p1 ) >= hb_itemGetDL( p2 ) ? hb_pardsbuff( szDate, 1 ) : hb_pardsbuff( szDate, 2 ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1093, NULL, "MAX", 2, p1, p2 );
}

/* returns the minimum of two date or numerics */
HB_FUNC( MIN )
{
   PHB_ITEM p1 = hb_param( 1, HB_IT_ANY );
   PHB_ITEM p2 = hb_param( 2, HB_IT_ANY );

   if( HB_IS_NUMERIC( p1 ) && HB_IS_NUMERIC( p2 ) )
   {
      /* NOTE: The order of these if() branches is significant,
               please, don't change it. [vszakats] */

      if( HB_IS_DOUBLE( p1 ) || HB_IS_DOUBLE( p2 ) )
      {
         double d1 = hb_itemGetND( p1 );
         double d2 = hb_itemGetND( p2 );

         int iDec1;
         int iDec2;

         hb_itemGetNLen( p1, NULL, &iDec1 );
         hb_itemGetNLen( p2, NULL, &iDec2 );

         if( d1 <= d2 )
            hb_retndlen( d1, 0, iDec1 );
         else
            hb_retndlen( d2, 0, iDec2 );
      }
      else if( HB_IS_LONG( p1 ) || HB_IS_LONG( p2 ) )
      {
         long l1 = hb_itemGetNL( p1 );
         long l2 = hb_itemGetNL( p2 );

         hb_retnl( l1 <= l2 ? l1 : l2 );
      }
      else
      {
         int i1 = hb_itemGetNI( p1 );
         int i2 = hb_itemGetNI( p2 );

         hb_retni( i1 <= i2 ? i1 : i2 );
      }
   }
   else if( HB_IS_DATE( p1 ) && HB_IS_DATE( p2 ) )
   {
      char szDate[ 9 ];

      hb_retds( hb_itemGetDL( p1 ) <= hb_itemGetDL( p2 ) ? hb_pardsbuff( szDate, 1 ) : hb_pardsbuff( szDate, 2 ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1092, NULL, "MIN", 2, p1, p2 );
}

