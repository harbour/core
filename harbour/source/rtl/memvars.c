/*
 * $Id$
 *
   Harbour Project source code

   This file is a part of Harbour Runtime Library and it contains code
   that handles memory variables.

   Copyright (C) 1999 Ryszard Glab
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

*/

#include <extend.h>
#include <string.h>

/* static HB_VALUE_PTR _privateTable = NULL; */

static ULONG _globalTableSize = 0;
static ULONG _globalFirstFree = 0;
static ULONG _globalLastFree  = 0;
static ULONG _globalFreeCnt   = 0;
static HB_VALUE_PTR _globalTable = NULL;

#define TABLE_INITHB_VALUE   10
#define TABLE_EXPANDHB_VALUE  5

/* Uncomment this to trace codeblocks activity
#define MEMVARDEBUG
*/


void hb_MemvarInit( void )
{
}


void hb_MemvarRelease( void )
{
   if( _globalTable )
      hb_xfree( _globalTable );
}


HB_VALUE_PTR *hb_GlobalValueBaseAddress( void )
{
   return &_globalTable;
}


/*
 * This function creates new global value.
 *
 * pSource = item value that have to be stored or NULL
 * pOwner = the name of PUBLIC variable or NULL
 *
 * Returns:
 *  handle to variable memory or fails
 *
*/
HANDLE hb_GlobalValueNew( HB_ITEM_PTR pSource )
{
   HB_VALUE_PTR pValue;
   HANDLE hValue;

   if( ! _globalTable )
   {
      _globalTable = (HB_VALUE_PTR) hb_xgrab( sizeof(HB_VALUE) * TABLE_INITHB_VALUE );
      _globalTableSize = TABLE_INITHB_VALUE;
      _globalFreeCnt   = 0;
      _globalFirstFree = _globalLastFree = 1;
      hValue = 0;
   }
   else
   {
      if( _globalFreeCnt )
      {
         /* There are holes in the table
          * Get a first available hole
          */
         hValue =_globalFirstFree;
         --_globalFreeCnt;

         /* Now find the next hole
          */
         if( _globalFreeCnt )
         {
            while( _globalTable[ ++_globalFirstFree ].counter );
         }
         else
            /* No more holes
             */
            _globalFirstFree =_globalLastFree;
      }
      else
      {
         /* Allocate the value from the end of table
          */
         if( _globalFirstFree < _globalTableSize )
         {
            hValue =_globalFirstFree;
            _globalFirstFree = ++_globalLastFree;
         }
         else
         {
            /* No more free values in the table - expand the table
             */
            hValue = _globalTableSize;
            _globalFirstFree =_globalLastFree = _globalTableSize +1;
            _globalTableSize += TABLE_EXPANDHB_VALUE;
            _globalTable =(HB_VALUE_PTR) hb_xrealloc( _globalTable, sizeof(HB_VALUE) * _globalTableSize );
         }

      }
   }

   pValue =_globalTable + hValue;
   if( pSource )
      memcpy( &pValue->item, pSource, sizeof(HB_ITEM) );
   else
      pValue->item.type =IT_NIL;
   pValue->counter =1;

   #ifdef MEMVARDEBUG
      printf( "\n>>>>>Global item created with handle =%i", hValue );
   #endif

   return hValue;
}

/*
 * This function increases the number of references to passed global value
 *
 */
void hb_GlobalValueIncRef( HANDLE hValue )
{
   #ifdef MEMVARDEBUG
   if( hValue < 0 || hValue > _globalTableSize )
   {
      printf( "\nInvalid MEMVAR handle %i (max %li)\n", hValue, _globalTableSize );
      exit( 1 );
   }
   #endif

   _globalTable[ hValue ].counter++;

   #ifdef MEMVARDEBUG
      printf( "\n+++Global item (%i) increment refCounter=%li", hValue, _globalTable[ hValue ].counter );
   #endif
}

/*
 * This function decreases the number of references to passed global value.
 * If it is the last reference then this value is deleted.
 *
 */
void hb_GlobalValueDecRef( HANDLE hValue )
{
   HB_VALUE_PTR pValue;

   #ifdef MEMVARDEBUG
   if( hValue < 0 || hValue > _globalTableSize )
   {
      printf( "\nInvalid MEMVAR handle %i (max %li)\n", hValue, _globalTableSize );
      exit( 1 );
   }
   #endif

   pValue =_globalTable + hValue;
   #ifdef MEMVARDEBUG
      printf( "\n---Global item (%i) decrement refCounter=%li", hValue, pValue->counter-1 );
   #endif
   if( --pValue->counter == 0 )
   {
      ItemRelease( &pValue->item );
      if( _globalFirstFree > hValue )
         _globalFirstFree = hValue;
      if( (_globalLastFree - hValue) == 1 )
         _globalLastFree =hValue;
      if( _globalFirstFree != _globalLastFree )
         ++_globalFreeCnt;

   #ifdef MEMVARDEBUG
      printf( "\n<<<<<Global item (%i) deleted", hValue );
   #endif
   }
}
