/*
 * $Id$
 *
   Harbour Project source code

   Copyright(C) 1999 by Jose Lalin.
   http://www.Harbour-Project.org/

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

   You can contact me at: dezac@corevia.com
 */

#include "extend.h"
#include "itemapi.h"
#include <ctype.h>
#include "init.h"

extern STACK stack;

HARBOUR HB_DESCEND(void);

HB_INIT_SYMBOLS_BEGIN( Descend__InitSymbols )
{ "DESCEND", FS_PUBLIC, HB_DESCEND, 0 }
HB_INIT_SYMBOLS_END( Descend__InitSymbols );
#if ! defined(__GNUC__)
#pragma Descend__InitSymbols
#endif

char *hb_strdescend( char *string )
{
   char *s;

   if( string )
   {
      for( s = string; *s; ++s )
         *s = 256 - *s;
   }
   return string;
}

HARBOUR HB_DESCEND( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pItem = hb_param(1, IT_ANY );

      if( pItem )
      {
         if( IS_STRING( pItem ) )
            hb_retc( hb_strdescend( pItem->item.asString.value ) );
         else if( IS_DATE( pItem ) )
            hb_retnl( 5231808 - pItem->item.asDate.value );
         else if( IS_INTEGER( pItem ) )
            hb_retni( -1 * pItem->item.asInteger.value );
         else if( IS_LONG( pItem ) )
            hb_retnl( -1 * pItem->item.asLong.value );
         else if( IS_DOUBLE( pItem ) )
         {
            PHB_ITEM pReturn;

            pReturn = hb_itemPutND( NULL, -1 * pItem->item.asDouble.value );
            hb_itemReturn( pReturn );
            hb_itemRelease( pReturn );

/* It is dengerous to operate on the stack directly
            stack.Return.wDec = pItem->wDec;
*/
         }
         else if( IS_LOGICAL( pItem ) )
            hb_retl( !pItem->item.asLogical.value );
         else
            hb_retc( "NIL" );
      }
   }
}
