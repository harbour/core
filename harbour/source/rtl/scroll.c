/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SCROLL() function
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

/* Scrolls a screen region */

HB_FUNC( SCROLL )
{
   int iMaxRow = hb_gtMaxRow();
   int iMaxCol = hb_gtMaxCol();

   int iTop    = ISNUM( 1 ) ? hb_parni( 1 ) : 0;      
   int iLeft   = ISNUM( 2 ) ? hb_parni( 2 ) : 0;      
   int iBottom = ISNUM( 3 ) ? hb_parni( 3 ) : iMaxRow;
   int iRight  = ISNUM( 4 ) ? hb_parni( 4 ) : iMaxCol;

   /* Enforce limits of (0,0) to (MAXROW(),MAXCOL()) */

   if( iTop < 0 ) iTop = 0;
   else if( iTop > iMaxRow ) iTop = iMaxRow;

   if( iLeft < 0 ) iLeft = 0;
   else if( iLeft > iMaxCol ) iLeft = iMaxCol;

   if( iBottom < 0 ) iBottom = 0;
   else if( iBottom > iMaxRow ) iBottom = iMaxRow;

   if( iRight < 0 ) iRight = 0;
   else if( iRight > iMaxCol ) iRight = iMaxCol;

   hb_gtScroll( ( USHORT ) iTop, 
                ( USHORT ) iLeft, 
                ( USHORT ) iBottom, 
                ( USHORT ) iRight, 
                ISNUM( 5 ) ? hb_parni( 5 ) : 0, 
                ISNUM( 6 ) ? hb_parni( 6 ) : 0 );
}

