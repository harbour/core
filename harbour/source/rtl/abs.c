/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ABS() function
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

HB_FUNC( ABS )
{
   PHB_ITEM pNumber = hb_param( 1, HB_IT_NUMERIC );

   if( pNumber )
   {
      int iWidth;
      int iDec;

      hb_itemGetNLen( pNumber, &iWidth, &iDec );

      if( HB_IS_INTEGER( pNumber ) )
      {
         int iNumber = hb_itemGetNI( pNumber );

         if( iNumber >= 0 )
            hb_retnilen( iNumber, iWidth );
         else
            hb_retni( -iNumber );
      }
      else if( HB_IS_LONG( pNumber ) )
      {
         long lNumber = hb_itemGetNL( pNumber );

         if( lNumber >= 0 )
            hb_retnllen( lNumber, iWidth );
         else
            hb_retnl( -lNumber );
      }
      else if( HB_IS_DOUBLE( pNumber ) )
      {
         double dNumber = hb_itemGetND( pNumber );

         hb_retndlen( dNumber >= 0.0 ? dNumber : -dNumber, 0, iDec );
      }
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1089, NULL, "ABS" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

