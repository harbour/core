/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour simple hash table implementation
 *
 * Copyright 1999-2002 Ryszard Glab <rglab@imid.med.pl>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbhash.h"

static HB_HASH_ITEM_PTR hb_hashItemNew( ULONG ulKey, const void * pKey, const void * pValue )
{
   HB_HASH_ITEM_PTR pItem = (HB_HASH_ITEM_PTR) hb_xgrab( sizeof( HB_HASH_ITEM ) );

   pItem->key = ulKey;
   pItem->KeyPtr = pKey;
   pItem->ValPtr = pValue;
   pItem->next = NULL;

   return pItem;
}

static void hb_hashItemDelete( HB_HASH_TABLE_PTR pTable, HB_HASH_ITEM_PTR pItem )
{
   if( pTable->pDeleteItemFunc )
      ( pTable->pDeleteItemFunc )( pTable, pItem->KeyPtr, pItem->ValPtr );
   hb_xfree( ( void * ) pItem );
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
   pTable->ulCount = pTable->ulUsed = 0;

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
         HB_HASH_ITEM_PTR pItem, pFree;
         pItem = pTable->pItems[ ulSize ];
         while( pItem )
         {
            pFree = pItem;
            pItem = pItem->next;
            hb_hashItemDelete( pTable, pFree );
         }
      }
      ++ulSize;
   }
   hb_xfree( ( void * ) pTable->pItems );
   hb_xfree( ( void * ) pTable );
}

/* resize table */
HB_HASH_TABLE_PTR hb_hashTableResize( HB_HASH_TABLE_PTR pTable, ULONG ulNewSize )
{
   HB_HASH_TABLE_PTR pNew;
   ULONG ulSize = 0;

   if( ulNewSize == 0 )
      ulNewSize = 2 * pTable->ulTableSize + 1;
   pNew = hb_hashTableCreate( ulNewSize,
                              pTable->pKeyFunc,
                              pTable->pDeleteItemFunc,
                              pTable->pCompFunc );

   while( ulSize < pTable->ulTableSize )
   {
      if( pTable->pItems[ ulSize ] )
      {
         HB_HASH_ITEM_PTR pItem;

         pItem = pTable->pItems[ ulSize ];
         while( pItem )
         {
            ULONG ulKey;
            HB_HASH_ITEM_PTR pNewItem, pNext;

            pNext = pItem->next;
            ulKey = ( pTable->pKeyFunc )( pNew, pItem->KeyPtr, pItem->ValPtr );
            pNewItem = pNew->pItems[ ulKey ];
            if( pNewItem )
            {
               while( pNewItem->next )
                  pNewItem = pNewItem->next;
               pNewItem->next = pItem;
            }
            else
            {
               pNew->pItems[ ulKey ] = pItem;
               ++pNew->ulUsed;
            }
            pItem->key = ulKey;
            pItem->next = NULL;
            ++pNew->ulCount;
            pItem = pNext;
         }
      }
      ++ulSize;
   }
   hb_xfree( ( void * ) pTable->pItems );
   hb_xfree( ( void * ) pTable );

   return pNew;
}

/* add a new value into th ehash table */
BOOL hb_hashTableAdd( HB_HASH_TABLE_PTR pTable, void * pKey, void * pValue )
{
   ULONG ulKey;
   HB_HASH_ITEM_PTR pItem;

   ulKey = ( pTable->pKeyFunc )( pTable, pKey, pValue );
   pItem = pTable->pItems[ ulKey ];
   if( pItem )
   {
      while( pItem->next )
         pItem = pItem->next;
      pItem->next = hb_hashItemNew( ulKey, pKey, pValue );
   }
   else
   {
      pTable->pItems[ ulKey ] = hb_hashItemNew( ulKey, pKey, pValue );
      ++pTable->ulUsed;
   }
   ++pTable->ulCount;

   return TRUE;
}

/* return the pointer to item's value or NULL if not found
*/
const void * hb_hashTableFind( HB_HASH_TABLE_PTR pTable, const void * pKey )
{
   ULONG ulKey;
   HB_HASH_ITEM_PTR pItem;
   const void * pFound = NULL;

   ulKey = ( pTable->pKeyFunc )( pTable, pKey, NULL );
   pItem = pTable->pItems[ ulKey ];
   if( pItem )
   {
      while( pItem && ( ( pTable->pCompFunc )( pTable, pItem->KeyPtr, pKey ) != 0 ) )
         pItem = pItem->next;

      if( pItem )
         pFound = pItem->ValPtr;
   }

   return pFound;
}

/* Delete an item from the table
* Returns TRUE if item was found and returns FALSE when passed item
* is not stored in the table
*/
BOOL hb_hashTableDel( HB_HASH_TABLE_PTR pTable, const void *pKey )
{
   ULONG ulKey;
   HB_HASH_ITEM_PTR pItem;
   HB_HASH_ITEM_PTR pPrev = NULL;
   BOOL bFound = FALSE;

   ulKey = ( pTable->pKeyFunc )( pTable, pKey, NULL );
   if( ulKey > pTable->ulTableSize )
      return FALSE;

   pItem = pTable->pItems[ ulKey ];
   while( pItem && ! bFound )
   {
      if( ( pTable->pCompFunc )( pTable, pItem->KeyPtr, pKey ) == 0 )
      {
         if( pPrev )
         {
            pPrev->next = pItem->next;
         }
         else
         {
            pTable->pItems[ ulKey ] = pItem->next;
            if( !pItem->next )
            {
               --pTable->ulUsed;
               pTable->pItems[ ulKey ] = NULL;
            }
         }
         --pTable->ulCount;
         hb_hashItemDelete( pTable, pItem );
         bFound = TRUE;
      }
      else
      {
         pPrev = pItem;
         pItem = pItem->next;
      }
   }

   return bFound;
}

/* return the hash table size */
ULONG hb_hashTableSize( HB_HASH_TABLE_PTR pTable )
{
   return pTable->ulTableSize;
}
