/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_SHADOW(), HB_CLRAREA(), DBGSHADOW() functions
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
#include "hbapigt.h"

HB_FUNC( HB_SHADOW )
{
   if( hb_pcount() >= 4 )
      hb_gtDrawShadow( hb_parni( 1 ),
                       hb_parni( 2 ),
                       hb_parni( 3 ),
                       hb_parni( 4 ),
                       ISNUM( 5 ) ? hb_parni( 5 ) : 7 );
}

HB_FUNC( HB_CLRAREA )
{
   if( hb_pcount() > 4 )
      hb_gt_SetAttribute( hb_parni( 1 ),
                          hb_parni( 2 ),
                          hb_parni( 3 ),
                          hb_parni( 4 ),
                          hb_parni( 5 ) );
}

#ifdef HB_C52_UNDOC

HB_FUNC( DBGSHADOW )
{
   HB_FUNCNAME( HB_SHADOW )();
}

#endif
