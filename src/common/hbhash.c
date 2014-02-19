/*
 * Harbour Project source code:
 * Harbour simple hash table implementation
 *
 * Copyright 1999-2002 Ryszard Glab <rglab@imid.med.pl>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

static PHB_HASH_ITEM hb_hashItemNew( HB_SIZE nKey, const void * pKey, const void * pValue )
{
   PHB_HASH_ITEM pItem = ( PHB_HASH_ITEM ) hb_xgrab( sizeof( HB_HASH_ITEM ) );

   pItem->key = nKey;
   pItem->KeyPtr = pKey;
   pItem->ValPtr = pValue;
   pItem->next = NULL;

   return pItem;
}

static void hb_hashItemDelete( PHB_HASH_TABLE pTable, PHB_HASH_ITEM pItem )
{
   if( pTable->pDeleteItemFunc )
      ( pTable->pDeleteItemFunc )( pTable, pItem->KeyPtr, pItem->ValPtr );
   hb_xfree( pItem );
}

/* create a new  hash table
 * nSize = initial numer of items in the table
 * pHashTable = a function that calculates a hash key value
 *       (first parameter is a value to add)
 * pDelete = a function that clears item's value before item's releasing
 *       (first parameter is a value to clear)
 * pComp = a function for comparing a values
 *       (first and second are values to compare, function have to return
 *        zero if values match or nonzero if they don't match)
 */
PHB_HASH_TABLE hb_hashTableCreate( HB_SIZE nSize,
                                   PHB_HASH_FUNC pHashFunc,
                                   PHB_HASH_FUNC pDelete,
                                   PHB_HASH_FUNC pComp )
{
   PHB_HASH_TABLE pTable = ( PHB_HASH_TABLE ) hb_xgrab( sizeof( HB_HASH_TABLE ) );

   pTable->nTableSize = nSize;
   pTable->pKeyFunc = pHashFunc;
   pTable->pDeleteItemFunc = pDelete;
   pTable->pCompFunc = pComp;
   pTable->nCount = pTable->nUsed = 0;

   pTable->pItems = ( PHB_HASH_ITEM * ) hb_xgrab( sizeof( PHB_HASH_ITEM ) * nSize );
   memset( pTable->pItems, 0, sizeof( PHB_HASH_ITEM ) * nSize );

   return pTable;
}

/* Delete all items in the hash table and next delete the table
 */
void hb_hashTableKill( PHB_HASH_TABLE pTable )
{
   HB_SIZE nSize = 0;

   while( nSize < pTable->nTableSize )
   {
      if( pTable->pItems[ nSize ] )
      {
         PHB_HASH_ITEM pItem, pFree;
         pItem = pTable->pItems[ nSize ];
         while( pItem )
         {
            pFree = pItem;
            pItem = pItem->next;
            hb_hashItemDelete( pTable, pFree );
         }
      }
      ++nSize;
   }
   hb_xfree( pTable->pItems );
   hb_xfree( pTable );
}

/* resize table */
PHB_HASH_TABLE hb_hashTableResize( PHB_HASH_TABLE pTable, HB_SIZE nNewSize )
{
   PHB_HASH_TABLE pNew;
   HB_SIZE nSize = 0;

   if( nNewSize == 0 )
      nNewSize = 2 * pTable->nTableSize + 1;
   pNew = hb_hashTableCreate( nNewSize,
                              pTable->pKeyFunc,
                              pTable->pDeleteItemFunc,
                              pTable->pCompFunc );

   while( nSize < pTable->nTableSize )
   {
      if( pTable->pItems[ nSize ] )
      {
         PHB_HASH_ITEM pItem;

         pItem = pTable->pItems[ nSize ];
         while( pItem )
         {
            HB_SIZE nKey;
            PHB_HASH_ITEM pNewItem, pNext;

            pNext = pItem->next;
            nKey = ( pTable->pKeyFunc )( pNew, pItem->KeyPtr, pItem->ValPtr );
            pNewItem = pNew->pItems[ nKey ];
            if( pNewItem )
            {
               while( pNewItem->next )
                  pNewItem = pNewItem->next;
               pNewItem->next = pItem;
            }
            else
            {
               pNew->pItems[ nKey ] = pItem;
               ++pNew->nUsed;
            }
            pItem->key = nKey;
            pItem->next = NULL;
            ++pNew->nCount;
            pItem = pNext;
         }
      }
      ++nSize;
   }
   hb_xfree( pTable->pItems );
   hb_xfree( pTable );

   return pNew;
}

/* add a new value into th ehash table */
HB_BOOL hb_hashTableAdd( PHB_HASH_TABLE pTable, const void * pKey, const void * pValue )
{
   HB_SIZE nKey;
   PHB_HASH_ITEM pItem;

   nKey = ( pTable->pKeyFunc )( pTable, pKey, pValue );
   pItem = pTable->pItems[ nKey ];
   if( pItem )
   {
      while( pItem->next )
         pItem = pItem->next;
      pItem->next = hb_hashItemNew( nKey, pKey, pValue );
   }
   else
   {
      pTable->pItems[ nKey ] = hb_hashItemNew( nKey, pKey, pValue );
      ++pTable->nUsed;
   }
   ++pTable->nCount;

   return HB_TRUE;
}

/* return the pointer to item's value or NULL if not found
 */
const void * hb_hashTableFind( PHB_HASH_TABLE pTable, const void * pKey )
{
   HB_SIZE nKey;
   PHB_HASH_ITEM pItem;
   const void * pFound = NULL;

   nKey = ( pTable->pKeyFunc )( pTable, pKey, NULL );
   pItem = pTable->pItems[ nKey ];
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
 * Returns HB_TRUE if item was found and returns HB_FALSE when passed item
 * is not stored in the table
 */
HB_BOOL hb_hashTableDel( PHB_HASH_TABLE pTable, const void * pKey )
{
   HB_SIZE nKey;
   PHB_HASH_ITEM pItem;
   PHB_HASH_ITEM pPrev = NULL;
   HB_BOOL bFound = HB_FALSE;

   nKey = ( pTable->pKeyFunc )( pTable, pKey, NULL );
   if( nKey > pTable->nTableSize )
      return HB_FALSE;

   pItem = pTable->pItems[ nKey ];
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
            pTable->pItems[ nKey ] = pItem->next;
            if( ! pItem->next )
            {
               --pTable->nUsed;
               pTable->pItems[ nKey ] = NULL;
            }
         }
         --pTable->nCount;
         hb_hashItemDelete( pTable, pItem );
         bFound = HB_TRUE;
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
HB_SIZE hb_hashTableSize( PHB_HASH_TABLE pTable )
{
   return pTable->nTableSize;
}
