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
#include <set.h>
#ifdef HARBOUR_USE_GTAPI
 #include <gtapi.h>
#else
 static char old_string[ sizeof( hb_set.HB_SET_COLOR ) ];
#endif

/* From strings.c */
char *hb_strUpper(char *szText, long lLen);

char *hb_setColor( char *sColor )
{
#ifdef HARBOUR_USE_GTAPI
   hb_gtGetColorStr( hb_set.HB_SET_COLOR );
#else
   strncpy (old_string, hb_set.HB_SET_COLOR, sizeof( hb_set.HB_SET_COLOR ) );
   old_string[ sizeof( hb_set.HB_SET_COLOR ) - 1 ] = 0;
#endif

   if( sColor )
   {
   #ifdef HARBOUR_USE_GTAPI  
      hb_gtSetColorStr( sColor );
   #else
      strncpy (hb_set.HB_SET_COLOR, sColor, sizeof( hb_set.HB_SET_COLOR ) );
      hb_set.HB_SET_COLOR[ sizeof( hb_set.HB_SET_COLOR ) - 1 ] = 0;
      hb_strUpper( hb_set.HB_SET_COLOR, strlen( hb_set.HB_SET_COLOR ) );
   #endif
  }
#ifdef HARBOUR_USE_GTAPI  
   return hb_set.HB_SET_COLOR;
#else
   return old_string;
#endif
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
    hb_retc( hb_setColor( hb_pcount() ? hb_parc(1) : NULL ) );
}

/* TODO: This is a temporary fix  - Call it on exit if you use SetColor() */

HARBOUR HB_GTEXIT( void )
{
#ifdef HARBOUR_USE_GTAPI
   hb_gtExit();
#endif
}
