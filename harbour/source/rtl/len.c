/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * LEN() function
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

#include "extend.h"
#include "errorapi.h"
#include "itemapi.h"

/*  $DOC$
 *  $FUNCNAME$
 *      LEN
 *  $CATEGORY$
 *
 *  $ONELINER$
 *      Returns size of a string or size of an array.
 *  $SYNTAX$
 *      LEN( <acString> ) --> <nLength>
 *  $ARGUMENTS$
 *      <acString> is a character string or the array to check.
 *  $RETURNS$
 *      The length of the string or the number of elements that contains
 *      an array.
 *  $DESCRIPTION$
 *      This function returns the string length or the size of an array. If
 *      it is used with a multidimensional array it returns the sizee of the
 *      first dimension.
 *  $EXAMPLES$
 *      ? Len( "Harbour" ) --> 7
 *  $TESTS$
 *      function Test()
 *         LOCAL cName := ""
 *         ACCEPT "Enter your name: " TO cName
 *         ? Len( cName )
 *      return nil
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      LEN() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      EMPTY, RTRIM, LTRIM
 *  $END$
 */

HARBOUR HB_LEN( void )
{
   PHB_ITEM pItem = hb_param( 1, IT_ANY );

   /* NOTE: pItem cannot be NULL here */

   switch( pItem->type )
   {
      case IT_ARRAY:
         hb_retnl( hb_arrayLen( pItem ) );
         break;

      case IT_STRING:
      case IT_MEMO:
         hb_retnl( hb_itemGetCLen( pItem ) );
         break;

      default:
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1111, NULL, "LEN" );

         if( pResult )
         {
            hb_itemReturn( pResult );
            hb_itemRelease( pResult );
         }
      }
   }
}
