/*
 * $Id$

   Copyright(C) 1999 by Paul Tucker

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: ptucker@sympatico.ca
*/

#include "pcode.h"
#include <init.h>

int hb_gtSetColorStr(char * fpColorString);
int hb_gtGetColorStr(char * fpColorString);
void hb_gtexit(void);

char *hb_SetColor( char *sColor )
{
    char *color;

    color = (char *)hb_xgrab( 256 );
    hb_gtGetColorStr( color );

    if( sColor )
       hb_gtSetColorStr( sColor );

    /* The caller is responsible for releasing this */
    return (char *)hb_xrealloc( color, strlen( color )+1 );
}

HARBOUR HB_SETCOLOR( void );
HARBOUR HB_GTEXIT( void );

HB_INIT_SYMBOLS_BEGIN( SETCOLOR__InitSymbols )
{ "SETCOLOR", FS_PUBLIC, HB_SETCOLOR, 0 },
{ "GTEXIT", FS_PUBLIC, HB_GTEXIT, 0 }
HB_INIT_SYMBOLS_END( SETCOLOR__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup SETCOLOR__InitSymbols
#endif

HARBOUR HB_SETCOLOR( void )
{
    char *color;

    hb_retc( color = hb_SetColor( hb_pcount() == 1 ? hb_parc(1) : NULL ) );
    hb_xfree( color );
}

/* TODO: This is a temporary fix  - Call it on exit if you use SetColor() */

HARBOUR HB_GTEXIT( void )
{
   hb_gtexit();
}
