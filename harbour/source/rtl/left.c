/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * LEFT() function
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/* returns the left-most n characters in string */

HB_FUNC( LEFT )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText && ISNUM( 2 ) )
   {
      long lLen = hb_parnl( 2 );

      if( lLen > ( long ) hb_itemGetCLen( pText ) )
         lLen = ( long ) hb_itemGetCLen( pText );
      else if( lLen < 0 )
         lLen = 0;

      hb_retclen( hb_itemGetCPtr( pText ), lLen );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1124, NULL, "LEFT" );
}

