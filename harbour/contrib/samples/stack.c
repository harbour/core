/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Stack structure
 *
 * Copyright 2000 Jose Lalin <dezac@corevia.com>
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
#include "hbapiitm.h"

/* StackNew() --> <aStack>
*/
HB_FUNC( STACKNEW )
{
   PHB_ITEM pReturn = hb_itemArrayNew( 0 );   /* Create array */

   hb_itemReturn( pReturn );
   hb_itemRelease( pReturn );
}

/*  StackPush( <aStack>, <xValue> ) --> <aStack>
*/
HB_FUNC( STACKPUSH )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pAny = hb_param( 2, HB_IT_ANY );

   hb_arrayAdd( pArray, pAny );
}

/* StackPop( <aStack> ) --> <xValue>
   Returns NIL if the stack is empty
*/
HB_FUNC( STACKPOP )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   long ulLen = hb_arrayLen( pArray );
   PHB_ITEM pLast = hb_itemNew( NULL );

   if( ulLen )
   {
      hb_arrayLast( pArray, pLast );
      hb_arrayDel( pArray, ulLen );
      --ulLen;
      hb_arraySize( pArray, HB_MAX( ulLen, 0 ) );
   }

   hb_itemReturn( pLast );
   hb_itemRelease( pLast );
}

/* StackIsEmpty( <aStack> ) --> <lEmpty>
*/
HB_FUNC( STACKISEMPTY )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   hb_retl( hb_arrayLen( pArray ) == 0 );
}

/* StackTop( <aStack> ) --> <xValue>
   Returns the top item
*/
HB_FUNC( STACKTOP )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pLast = hb_itemNew( NULL );

   hb_arrayLast( pArray, pLast );

   hb_itemReturn( pLast );
   hb_itemRelease( pLast );
}
