/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * VALTYPE() function
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

char * hb_valtypeGet( HB_ITEM_PTR pItem )
{
   char * szType;

   switch( pItem->type & ~IT_BYREF )
   {
      case IT_ARRAY:
         szType = ( hb_arrayIsObject( pItem ) ? "O" : "A" );
         break;

      case IT_BLOCK:
         szType = "B";
         break;

      case IT_DATE:
         szType = "D";
         break;

      case IT_LOGICAL:
         szType = "L";
         break;

      case IT_INTEGER:
      case IT_LONG:
      case IT_DOUBLE:
         szType = "N";
         break;

      case IT_STRING:
         szType = "C";
         break;

      case IT_MEMO:
         szType = "M";
         break;

      default:
         szType = "U";
         break;
   }
   return szType;
}

HARBOUR HB_VALTYPE( void )
{
   PHB_ITEM pItem = hb_param( 1, IT_ANY );

   /* NOTE: Double safety to ensure that a parameter was really passed,
            compiler checks this, but a direct hb_vmDo() call
            may not do so. [vszakats] */

   if( pItem )
      hb_retc( hb_valtypeGet( pItem ) );
   else
      hb_retc( "U" );
}

