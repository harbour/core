/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Terminal API (undocumented part)
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

#include "hbapigt.h"

void hb_gtWCreate( HB_GT_RECT * rect, HB_GT_WND ** wnd )
{
   HB_SYMBOL_UNUSED( rect );
   HB_SYMBOL_UNUSED( wnd );
}

void hb_gtWDestroy( HB_GT_WND * wnd )
{
   HB_SYMBOL_UNUSED( wnd );
}

BOOL hb_gtWFlash( void )
{
   return FALSE;
}

void hb_gtWApp( HB_GT_WND ** wnd )
{
   HB_SYMBOL_UNUSED( wnd );
}

void hb_gtWCurrent( HB_GT_WND * wnd )
{
   HB_SYMBOL_UNUSED( wnd );
}

void hb_gtWPos( HB_GT_WND * wnd, HB_GT_RECT * rect )
{
   HB_SYMBOL_UNUSED( wnd );
   HB_SYMBOL_UNUSED( rect );
}

BOOL hb_gtWVis( HB_GT_WND * wnd, USHORT uiStatus )
{
   HB_SYMBOL_UNUSED( wnd );
   HB_SYMBOL_UNUSED( uiStatus );

   return FALSE;
}

USHORT hb_gtSLR( HB_GT_SLR * pSLR ) /* System Level Request */
{
   /* Do nothing in Harbour, since it the low-level GT API is 
      implemented with a different method. */
   
   HB_SYMBOL_UNUSED( pSLR );

   return 1;
}

USHORT hb_gtModalRead( void * dummy )
{
   HB_SYMBOL_UNUSED( dummy );

   return 1;
}

USHORT hb_gtBeginWrite( void )
{
   return 1;
}

USHORT hb_gtEndWrite( void )
{
   return 1;
}

USHORT hb_gtFlushCursor( void )
{
   return 1;
}

USHORT hb_gtSetColor( HB_GT_RGB * color )
{
   HB_SYMBOL_UNUSED( color );

   return 1;
}

USHORT hb_gtGetColor( HB_GT_RGB * color )
{
   HB_SYMBOL_UNUSED( color );

   return 1;
}

USHORT hb_gtSetBorder( HB_GT_RGB * color )
{
   HB_SYMBOL_UNUSED( color );

   return 1;
}

