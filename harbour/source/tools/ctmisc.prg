/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Misc CA-Tools functions
 *
 * Copyright 2000 Victor Szakats <info@szelvesz.hu>
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

#include "color.ch"
#include "common.ch"
#include "setcurs.ch"

MEMVAR GetList

FUNCTION CT_CENTER( c, n, p )
   RETURN PadC( AllTrim( c ), n, p )

FUNCTION CT_CSETCURS( l )

   IF PCount() == 0
      RETURN SetCursor() != SC_NONE
   ENDIF

   RETURN SetCursor( iif( l, SC_NORMAL, SC_NONE ) ) != SC_NONE

FUNCTION CT_CSETKEY( n )
   RETURN SetKey( n )

FUNCTION CT_ENHANCED()

   ColorSelect( CLR_ENHANCED )

   RETURN ""

FUNCTION CT_STANDARD()

   ColorSelect( CLR_STANDARD )

   RETURN ""

FUNCTION CT_LTOC( l )
   RETURN iif( l, "T", "F" )

FUNCTION CT_RESTGETS( aGetList )

   GetList := aGetList

   RETURN .T.

FUNCTION CT_SAVEGETS()
   RETURN GetList

FUNCTION CT_SCREENMIX( c, a, row, col )

   DEFAULT row TO Row()
   DEFAULT col TO Col()

   RestScreen( row, col, row, col + Len( a ) - 1, CT_CHARMIX( c, a ) )

   RETURN ""

