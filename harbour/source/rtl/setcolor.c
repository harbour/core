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
void hb_gtInit(void);

HARBOUR HB_SETCOLOR( void );

HB_INIT_SYMBOLS_BEGIN( SETCOLOR__InitSymbols )
{ "SETCOLOR", FS_PUBLIC, HB_SETCOLOR, 0 }
HB_INIT_SYMBOLS_END( SETCOLOR__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup SETCOLOR__InitSymbols
#endif

HARBOUR HB_SETCOLOR( void )
{
    char *color;

    color = (char *)hb_xgrab(200);
    hb_gtGetColorStr( color );
    hb_retc( color );
    hb_xfree( color );
    if( hb_pcount() == 1 )
       hb_gtSetColorStr( hb_parc(1) );
}
