/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CHR(), ASC() functions
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

#include <ctype.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* converts an ASCII code to a character value */
HB_FUNC( CHR )
{
   if( ISNUM( 1 ) )
   {
      char szChar[ 2 ];

      /* NOTE: CA-Cl*pper's compiler optimizer will be wrong for those
               CHR() cases where the passed parameter is a constant which
               can be divided by 256 but it's not zero, in this case it
               will return an empty string instead of a Chr(0). [vszakats] */

      /* Believe it or not, clipper does this! */
      szChar[ 0 ] = hb_parnl( 1 ) % 256;
      szChar[ 1 ] = '\0';

      hb_retclen( szChar, 1 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1104, NULL, "CHR" );

      if( pResult )
         hb_itemRelease( hb_itemReturn( pResult ) );
   }
}

/* converts a character value to an ASCII code */
HB_FUNC( ASC )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      if( hb_itemGetCLen( pText ) > 0 )
         hb_retni( ( BYTE ) * ( hb_itemGetCPtr( pText ) ) );
      else
         hb_retni( 0 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1107, NULL, "ASC" );

      if( pResult )
         hb_itemRelease( hb_itemReturn( pResult ) );
   }
}

