/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * VAL() function
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

/* returns the numeric value of a character string representation of a number  */
double hb_strVal( const char * szText )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strVal(%s)", szText));

   return atof( szText );
}

/* returns the numeric value of a character string representation of a number  */
HB_FUNC( VAL )
{
   PHB_ITEM pText = hb_param( 1, IT_STRING );

   if( pText )
   {
      int iWidth;
      int iDec;
      char * ptr = strchr( hb_itemGetCPtr( pText ), '.' );

      if( ptr )
      {
         iWidth = ptr - hb_itemGetCPtr( pText );
         iDec = strlen( ptr + 1 );
      }
      else
      {
         iWidth = strlen( hb_itemGetCPtr( pText ) );
         iDec = 0;
      }

      hb_retndlen( hb_strVal( hb_itemGetCPtr( pText ) ), iWidth, iDec );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1098, NULL, "VAL" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

