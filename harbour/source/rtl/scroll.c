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

   int iTop;
   int iLeft;
   int iBottom;
   int iRight;

   /* Enforce limits of (0,0) to (MAXROW(),MAXCOL()) */

   iTop = hb_parni( 1 );
   if( iTop < 0 ) iTop = 0;
   else if( iTop > iMaxRow ) iTop = iMaxRow;

   iLeft = hb_parni( 2 );
   if( iLeft < 0 ) iLeft = 0;
   else if( iLeft > iMaxCol ) iLeft = iMaxCol;

   if( ISNUM( 3 ) )
   {
      iBottom = hb_parni( 3 );
      if( iBottom < 0 ) iBottom = 0;
      else if( iBottom > iMaxRow ) iBottom = iMaxRow;
   }
   else
      iBottom = iMaxRow;

   if( ISNUM( 4 ) )
   {
      iRight = hb_parni( 4 );
      if( iRight < 0 ) iRight = 0;
      else if( iRight > iMaxCol ) iRight = iMaxCol;
   }
   else
      iRight = iMaxCol;

   hb_gtScroll( ( USHORT ) iTop, 
                ( USHORT ) iLeft, 
                ( USHORT ) iBottom, 
                ( USHORT ) iRight, 
                hb_parni( 5 ), /* Defaults to zero on bad type */
                hb_parni( 6 ) ); /* Defaults to zero on bad type */
}

