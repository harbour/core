/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Color functions
 *
 * Copyright 1999 Paul Tucker <ptucker@sympatico.ca>
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

#include "extend.h"
#include "set.h"
#ifdef HARBOUR_USE_GTAPI
   #include "gtapi.h"
#else
   static char s_old_string[ sizeof( hb_set.HB_SET_COLOR ) ];
#endif

char * hb_setColor( char * sColor )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_setColor(%s)", sColor));

#ifdef HARBOUR_USE_GTAPI
   hb_gtGetColorStr( hb_set.HB_SET_COLOR );
#else
   strncpy( s_old_string, hb_set.HB_SET_COLOR, sizeof( hb_set.HB_SET_COLOR ) );
   s_old_string[ sizeof( hb_set.HB_SET_COLOR ) - 1 ] = '\0';
#endif

   if( sColor != ( char * ) NULL )
   {
   #ifdef HARBOUR_USE_GTAPI
      hb_gtSetColorStr( sColor );
   #else
      strncpy( hb_set.HB_SET_COLOR, sColor, sizeof( hb_set.HB_SET_COLOR ) );
      hb_set.HB_SET_COLOR[ sizeof( hb_set.HB_SET_COLOR ) - 1 ] = 0;
      hb_strUpper( hb_set.HB_SET_COLOR, strlen( hb_set.HB_SET_COLOR ) );
   #endif
  }
#ifdef HARBOUR_USE_GTAPI
   return hb_set.HB_SET_COLOR;
#else
   return s_old_string;
#endif
}

HARBOUR HB_SETCOLOR( void )
{
   hb_retc( hb_setColor( ISCHAR( 1 ) ? hb_parc( 1 ) : NULL ) );
}

HARBOUR HB_COLORSELECT( void )
{
#ifdef HARBOUR_USE_GTAPI
   if( ISNUM( 1 ) )
      hb_gtColorSelect( hb_parni( 1 ) );
#endif
}
