/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DISPBOX() function
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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
#include "hbapigt.h"
#include "hbapiitm.h"

HB_FUNC( DISPBOX )
{
   PHB_ITEM pTop    = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pLeft   = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pBottom = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pRight  = hb_param( 4, HB_IT_NUMERIC );

   if( pTop && pLeft && pBottom && pRight )
   {
      char * pszColor = ISCHAR( 6 ) ? hb_parc( 6 ) : NULL;
      char szOldColor[ CLR_STRLEN ];

      if( pszColor )
      {
         hb_gtGetColorStr( szOldColor );
         hb_gtSetColorStr( pszColor );
      }

      if( ISCHAR( 5 ) )
         hb_gtBox( hb_itemGetNI( pTop ),
                   hb_itemGetNI( pLeft), 
                   hb_itemGetNI( pBottom ), 
                   hb_itemGetNI( pRight ), 
                   ( BYTE * ) hb_parc( 5 ) );

      else if( ISNUM( 5 ) && hb_parni( 5 ) == 2 )
         hb_gtBoxD( hb_itemGetNI( pTop ),
                    hb_itemGetNI( pLeft), 
                    hb_itemGetNI( pBottom ), 
                    hb_itemGetNI( pRight ) );

      else
         hb_gtBoxS( hb_itemGetNI( pTop ),
                    hb_itemGetNI( pLeft), 
                    hb_itemGetNI( pBottom ), 
                    hb_itemGetNI( pRight ) );

      if( pszColor )
         hb_gtSetColorStr( szOldColor );
   }
}

