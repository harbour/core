/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Mouse API
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

#ifndef HB_MOUSEAPI_H_
#define HB_MOUSEAPI_H_

#include "extend.h"

/* Public interface. These should never change, only be added to. */

extern void   hb_mouseInit( void );
extern void   hb_mouseExit( void );
extern BOOL   hb_mouseIsPresent( void );
extern BOOL   hb_mouseGetCursor( void );
extern void   hb_mouseSetCursor( BOOL bVisible );
extern int    hb_mouseCol( void );
extern int    hb_mouseRow( void );
extern void   hb_mouseSetPos( int iRow, int iCol );
extern BOOL   hb_mouseIsButtonPressed( int iButton );
extern int    hb_mouseCountButton( void );
extern void   hb_mouseSetBounds( int iTop, int iLeft, int iBottom, int iRight );
extern void   hb_mouseGetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight );

/* Private interface listed below. these are common to all platforms */

extern void   hb_mouse_Init( void );
extern void   hb_mouse_Exit( void );
extern BOOL   hb_mouse_IsPresent( void );
extern void   hb_mouse_Show( void );
extern void   hb_mouse_Hide( void );
extern int    hb_mouse_Col( void );
extern int    hb_mouse_Row( void );
extern void   hb_mouse_SetPos( int iRow, int iCol );
extern BOOL   hb_mouse_IsButtonPressed( int iButton );
extern int    hb_mouse_CountButton( void );
extern void   hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight );
extern void   hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight );

#endif /* HB_MOUSEAPI_H_ */
