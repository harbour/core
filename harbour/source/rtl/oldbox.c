/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __BOX(), __BOXS(), __BOXD() undocumented box drawing functions
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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

#ifdef HB_C52_UNDOC

HB_FUNC( __BOX )
{
   PHB_ITEM pTop    = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pLeft   = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pBottom = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pRight  = hb_param( 4, HB_IT_NUMERIC );

   if( pTop && pLeft && pBottom && pRight )
         hb_gtBox( hb_itemGetNI( pTop ),
                   hb_itemGetNI( pLeft), 
                   hb_itemGetNI( pBottom ), 
                   hb_itemGetNI( pRight ),
                   ( BYTE * ) ( ISCHAR( 5 ) ? hb_parc( 5 ) : " " ) );
}

HB_FUNC( __BOXD )
{
   PHB_ITEM pTop    = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pLeft   = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pBottom = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pRight  = hb_param( 4, HB_IT_NUMERIC );

   if( pTop && pLeft && pBottom && pRight )
         hb_gtBoxD( hb_itemGetNI( pTop ),
                   hb_itemGetNI( pLeft), 
                   hb_itemGetNI( pBottom ), 
                   hb_itemGetNI( pRight ) );
}

HB_FUNC( __BOXS )
{
   PHB_ITEM pTop    = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pLeft   = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pBottom = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pRight  = hb_param( 4, HB_IT_NUMERIC );

  if( pTop && pLeft && pBottom && pRight )
         hb_gtBoxS( hb_itemGetNI( pTop ),
                   hb_itemGetNI( pLeft), 
                   hb_itemGetNI( pBottom ), 
                   hb_itemGetNI( pRight ) );
}

#endif

