/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour common hash table implementation
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

#include "hbhash.h"

static HB_HASH_ITEM_PTR hb_hashItemNew( ULONG ulKey, void * pValue )
{
   HB_HASH_ITEM_PTR pItem = (HB_HASH_ITEM_PTR) hb_xgrab( sizeof( HB_HASH_ITEM ) );
   
   pItem->key = ulKey;
   pItem->cargo = pValue;
   pItem->next = NULL;

   return pItem;
}

/* create a new  hash table 
* ulSize = initial numer of items in the table
* pHashTable = a function that calculates a hash key value
*       (first parameter is a value to add)
* pDelete = a function that clears item's value before item's releasing
*       (first parameter is a value to clear)
* pComp = a function for comparing a values
*       (first and second are values to compare, function have to return
*        zero if values match or nonzero if they don't match)
*/
HB_HASH_TABLE_PTR hb_hashTableCreate( ULONG ulSize, 
                                   HB_HASH_FUNC_PTR pHashFunc, 
                                   HB_HASH_FUNC_PTR pDelete,
                                   HB_HASH_FUNC_PTR pComp )
{
   HB_HASH_TABLE_PTR pTable = ( HB_HASH_TABLE_PTR ) hb_xgrab( sizeof( HB_HASH_TABLE ) );
   
   pTable->ulTableSize = ulSize;
   pTable->pKeyFunc = pHashFunc;
   pTable->pDeleteItemFunc = pDelete;
   pTable->pCompFunc = pComp;
   
   pTable->pItems = ( HB_HASH_ITEM_PTR * ) hb_xgrab( sizeof( HB_HASH_ITEM_PTR ) * ulSize );
   memset( pTable->pItems, 0, sizeof( HB_HASH_ITEM_PTR ) * ulSize );
   
   return pTable;
}

/* Delete all items in the hash table and next delete the table 
*/
void hb_hashTableKill( HB_HASH_TABLE_PTR pTable )
{
   ULONG ulSize = 0;
   
   while( ulSize < pTable->ulTableSize )
   {
      if( pTable->pItems[ ulSize ] )
      {
         HB_HASH_ITEM_PTR pItem;
         HB_HASH_ITEM_PTR pNext;
         
         pItem = pTable->pItems[ ulSize ];
         while( pItem )
         {
            if( pTable->pDeleteItemFunc )
               ( pTable->pDeleteItemFunc )( pItem->cargo, NULL );
            pNext = pItem->next;
            hb_xfree( ( void * ) pItem );
            pItem = pNext;
         }
      }
      ++ulSize;
   }
   hb_xfree( ( void * ) pTable );
}

/* add a new value into th ehash table */
BOOL hb_hashTableAdd( HB_HASH_TABLE_PTR pTable, void *pValue )
{
   ULONG ulKey;
   HB_HASH_ITEM_PTR pItem;
   
   ulKey = ( pTable->pKeyFunc )( pValue, NULL );
   pItem = pTable->pItems[ ulKey ];
   if( pItem )
   {
      while( pItem->next )
         pItem = pItem->next;
      pItem->next = hb_hashItemNew( ulKey, pValue );
   }
   else
      pTable->pItems[ ulKey ] = hb_hashItemNew( ulKey, pValue );
      
   return TRUE;
}

/* return the pointer to item's value or NULL if not found 
*/
void * hb_hashTableFind( HB_HASH_TABLE_PTR pTable, void *pValue )
{
   ULONG ulKey;
   HB_HASH_ITEM_PTR pItem;
   void * pFound = NULL;
   
   ulKey = ( pTable->pKeyFunc )( pValue, NULL );
   pItem = pTable->pItems[ ulKey ];
   if( pItem )
   {
      while( pItem && (( pTable->pCompFunc )( pItem->cargo, pValue ) != 0) )
         pItem = pItem->next;
         
      if( pItem )
         pFound =  pItem->cargo;
   }

   return pFound;
}

/* return the hash table size */
ULONG hb_hashTableBucket( HB_HASH_TABLE_PTR pTable )
{
   return pTable->ulTableSize;
}
