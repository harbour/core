/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MOD() function
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

/* NOTE: In CA-Cl*pper this is written in Clipper, see the source below,
         and the error handling is NOT made here, but in the % operator.
         [vszakats] */

/* NOTE: CA-Cl*pper is buggy since it relies on the fact that the errorhandler
         will silently handle zero division errors. [vszakats] */

/* NOTE: This C version fully emulates the behaviour of the original
         CA-Cl*pper version, including bugs/side-effects. [vszakats] */

HB_FUNC( MOD )
{
   PHB_ITEM pNumber = hb_param( 1, HB_IT_NUMERIC );

   if( pNumber && ISNUM( 2 ) )
   {
      double dNumber = hb_itemGetND( pNumber );
      double dBase = hb_parnd( 2 ); /* dBase! Cool! */

      if( dBase )
      {
         double dResult = dNumber - ( ( long ) ( dNumber / dBase ) * dBase );

         if( dResult * dBase < 0 )
            hb_retnd( dResult + dBase );
         else
            hb_retnd( dResult );
      }
      else
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%" );

         if( pResult )
         {
            int iDec;

            hb_itemRelease( pResult );

            hb_itemGetNLen( pNumber, NULL, &iDec );
  
            hb_retndlen( dNumber, 0, iDec );
         }
      }
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1085, NULL, "%" );

      if( pResult )
         hb_itemRelease( hb_itemReturn( pResult ) );
   }
}

/*
FUNCTION MOD( cl_num, cl_base )
   LOCAL cl_result := cl_num % cl_base

   RETURN iif( cl_base = 0, cl_num, iif( cl_result * cl_base < 0, cl_result + cl_base, cl_result ) )
*/
