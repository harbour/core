/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SETPOS(), ROW(), COL() functions
 *
 * Copyright 1999 Bil Simser <bsimser@home.com>
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
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    SETPOS()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC( SETPOS ) /* Sets the screen position */
{
   if( ISNUM( 1 ) && ISNUM( 2 ) )
      hb_gtSetPos( hb_parni( 1 ), hb_parni( 2 ) );
}

HB_FUNC( ROW ) /* Return the current screen row position (zero origin) */
{
   SHORT iRow;
   SHORT iCol;

   hb_gtGetPos( &iRow, &iCol );

   hb_retni( iRow );
}

HB_FUNC( COL ) /* Return the current screen column position (zero origin) */
{
   SHORT iRow;
   SHORT iCol;

   hb_gtGetPos( &iRow, &iCol );

   hb_retni( iCol );
}

