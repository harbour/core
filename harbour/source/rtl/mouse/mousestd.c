/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Mouse subsystem for plain ANSI C stream IO (stub)
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
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

#include "mouseapi.h"

/* NOTE: This file is a simple stub for those platforms which don't have
         any kind of mouse support. */

/* C callable low-level interface */

void hb_mouse_Init( void )
{
   ;
}

void hb_mouse_Exit( void )
{
   ;
}

BOOL hb_mouse_IsPresent( void )
{
   return FALSE;
}

void hb_mouse_Show( void )
{
   ;
}

void hb_mouse_Hide( void )
{
   ;
}

int hb_mouse_Col( void )
{
   return 0;
}

int hb_mouse_Row( void )
{
   return 0;
}

void hb_mouse_SetPos( int iRow, int iCol )
{
   HB_SYMBOL_UNUSED( iRow );
   HB_SYMBOL_UNUSED( iCol );
}

BOOL hb_mouse_IsButtonPressed( int iButton )
{
   HB_SYMBOL_UNUSED( iButton );

   return FALSE;
}

int hb_mouse_CountButton( void )
{
   return 0;
}

void hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight )
{
   HB_SYMBOL_UNUSED( iTop );
   HB_SYMBOL_UNUSED( iLeft );
   HB_SYMBOL_UNUSED( iBottom );
   HB_SYMBOL_UNUSED( iRight );
}

void hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight )
{
   HB_SYMBOL_UNUSED( piTop );
   HB_SYMBOL_UNUSED( piLeft );
   HB_SYMBOL_UNUSED( piBottom );
   HB_SYMBOL_UNUSED( piRight );
}

