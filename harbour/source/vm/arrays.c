/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Array API (C level)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_arrayIsObject()
 *    hb_arrayCopyC()
 *    hb_arrayGetC()
 *
 * Copyright 2001 Ron Pinkas <ron@profit-master.com>
 *    hb_arrayClone()
 *    hb_arrayFromStack()
 *    hb_arrayFromParams()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbvm.h"
#include "hbstack.h"

static void hb_arrayReleaseItems( PHB_BASEARRAY pBaseArray )
{
   if( pBaseArray->ulLen )
   {
      HB_ITEM_PTR pItems = pBaseArray->pItems;
      ULONG ulLen = pBaseArray->ulLen;

      /*
       * clear the pBaseArray->pItems to avoid infinite loop in cross
       * referenced items when pBaseArray is not freed due to buggy
       * object destructor [druzus]
       */
      pBaseArray->pItems = NULL;
      pBaseArray->ulLen  = 0;

      while( ulLen-- )
      {
         if( HB_IS_COMPLEX( pItems + ulLen ) )
            hb_itemClear( pItems + ulLen );
      }
      hb_xfree( pItems );
   }
}

void hb_arrayPushBase( PHB_BASEARRAY pBaseArray )
{
   PHB_ITEM pItem = hb_stackAllocItem();

   pItem->type = HB_IT_ARRAY;
   pItem->item.asArray.value = pBaseArray;
   hb_gcRefInc( pBaseArray );
}

/* This releases array when called from the garbage collector */
static HB_GARBAGE_FUNC( hb_arrayReleaseGarbage )
{
   PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) Cargo;

   if( pBaseArray->uiClass )
   {
      /*
       * do not execute destructor for supercasted objects [druzus]
       */
      if( pBaseArray->uiPrevCls == 0 &&
          hb_clsHasDestructor( pBaseArray->uiClass ) )
      {
         hb_arrayPushBase( pBaseArray );
         hb_objDestructorCall( hb_stackItemFromTop( -1 ) );

         /* Clear object properities before hb_stackPop(), [druzus] */
         pBaseArray->uiClass = 0;
         hb_stackPop();

         /*
          * release array items before hb_gcRefCheck() to avoid double
          * pBaseArray freeing when it will have cross references to
          * self after executing buggy destructor [druzus]
          */
         hb_arrayReleaseItems( pBaseArray );
         hb_gcRefCheck( pBaseArray );
         return;
      }

      /*
       * This is only some additional protection for buggy code
       * which can store reference to this object in other class
       * destructor when executed from GC and it will only cause
       * RT error when user will try to send any message to this
       * object [druzus]
       */
      pBaseArray->uiClass = 0;
   }

   hb_arrayReleaseItems( pBaseArray );
}

BOOL hb_arrayNew( PHB_ITEM pItem, ULONG ulLen ) /* creates a new array */
{
   PHB_BASEARRAY pBaseArray;
   PHB_ITEM pItems;
   ULONG ulPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayNew(%p, %lu)", pItem, ulLen));

   if( HB_IS_COMPLEX( pItem ) )
      hb_itemClear( pItem );

   /*
    * allocate memory for items before hb_gcAlloc() to be
    * safe for automatic GC activation in hb_xgrab() without
    * calling hb_gcLock()/hb_gcUnlock(). [druzus]
    */
   if( ulLen > 0 )
   {
      pItems = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) * ulLen );
      for( ulPos = 0; ulPos < ulLen; ++ulPos )
         ( pItems + ulPos )->type = HB_IT_NIL;
   }
   else
      pItems = NULL;

   pBaseArray = ( PHB_BASEARRAY ) hb_gcAlloc( sizeof( HB_BASEARRAY ), hb_arrayReleaseGarbage );
   pBaseArray->pItems     = pItems;
   pBaseArray->ulLen      = ulLen;
   pBaseArray->uiClass    = 0;
   pBaseArray->uiPrevCls  = 0;
   pBaseArray->ulAllocated= ulLen;
   pItem->type = HB_IT_ARRAY;
   pItem->item.asArray.value = pBaseArray;

   return TRUE;
}

BOOL hb_arraySize( PHB_ITEM pArray, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySize(%p, %lu)", pArray, ulLen));

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;

      if( ulLen != pBaseArray->ulLen )
      {
         ULONG ulPos;

         if( pBaseArray->ulLen == 0 )
         {
            pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( ulLen * sizeof( HB_ITEM ) );
            pBaseArray->ulAllocated = ulLen;

            for( ulPos = 0; ulPos < ulLen; ulPos++ )
               ( pBaseArray->pItems + ulPos )->type = HB_IT_NIL;
         }
         else
         {
            if( pBaseArray->ulLen < ulLen )
            {

               if( pBaseArray->ulAllocated < ulLen )
               {
                  /* 
                     A common practice is to double allocation buffer size. Thus, making
                     reallocation count logarithmic to total number of added numbers.
                     I've used here a little different formula. ulAllocated is divided by 
                     factor 2 ( >> 1 ) and 1 is added to requested size. This algorithm 
                     has properties:
                       - reallocation count remains asymptoticaly logarithmic;
                       - saves memory for large arrays, because reallocation buffer 
                         size is not doubled, but multiplied by 1.5;
                       - adding of 1, allows reduce reallocation count for small arrays.
                   */
                  pBaseArray->ulAllocated = ( pBaseArray->ulAllocated >> 1 ) + 1 + ulLen;
                  pBaseArray->pItems = ( PHB_ITEM ) hb_xrealloc( pBaseArray->pItems, sizeof( HB_ITEM ) * pBaseArray->ulAllocated );
               }

               /* set value for new items */
               for( ulPos = pBaseArray->ulLen; ulPos < ulLen; ulPos++ )
                  ( pBaseArray->pItems + ulPos )->type = HB_IT_NIL;
            }
            else if( pBaseArray->ulLen > ulLen )
            {
               /* release old items */
               for( ulPos = ulLen; ulPos < pBaseArray->ulLen; ulPos++ )
               {
                  if( HB_IS_COMPLEX( pBaseArray->pItems + ulPos ) )
                     hb_itemClear( pBaseArray->pItems + ulPos );
               }

               if( ulLen == 0 )
               {
                  hb_xfree( pBaseArray->pItems );
                  pBaseArray->pItems = NULL;
               }
               else if( ulLen < ( pBaseArray->ulAllocated >> 1 ) )
               {
                  pBaseArray->ulAllocated = ulLen;
                  pBaseArray->pItems = ( PHB_ITEM ) hb_xrealloc( pBaseArray->pItems, sizeof( HB_ITEM ) * ulLen );
               }
            }
         }

         pBaseArray->ulLen = ulLen;
      }

      return TRUE;
   }
   else
      return FALSE;
}

ULONG hb_arrayLen( PHB_ITEM pArray )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayLen(%p)", pArray));

   if( HB_IS_ARRAY( pArray ) )
      return pArray->item.asArray.value->ulLen;
   else
      return 0;
}

BOOL hb_arrayIsObject( PHB_ITEM pArray )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayIsObject(%p)", pArray));

   if( HB_IS_ARRAY( pArray ) )
      return pArray->item.asArray.value->uiClass != 0;
   else
      return FALSE;
}

/* retrives the array unique ID */
void * hb_arrayId( PHB_ITEM pArray )
{
   if( HB_IS_ARRAY( pArray ) )
      return ( void * ) pArray->item.asArray.value;
   else
      return NULL;
}

BOOL hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayAdd(%p, %p)", pArray, pValue));

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;

      if( pBaseArray->ulLen < ULONG_MAX )
      {
         hb_arraySize( pArray, pBaseArray->ulLen + 1 );
         pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;
         hb_itemCopy( pBaseArray->pItems + ( pBaseArray->ulLen - 1 ), pValue );

         return TRUE;
      }
   }

   return FALSE;
}

BOOL hb_arrayAddForward( PHB_ITEM pArray, PHB_ITEM pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayAddForward(%p, %p)", pArray, pValue));

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;

      if( pBaseArray->ulLen < ULONG_MAX )
      {
         hb_arraySize( pArray, pBaseArray->ulLen + 1 );
         pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;
         hb_itemForwardValue( pBaseArray->pItems + ( pBaseArray->ulLen - 1 ), pValue );

         return TRUE;
      }
   }

   return FALSE;
}

BOOL hb_arrayDel( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayDel(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) )
   {
      ULONG ulLen = pArray->item.asArray.value->ulLen;

      if( ulIndex > 0 && ulIndex <= ulLen )
      {
         PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;

         if( ulIndex == ulLen )
         {
            hb_itemSetNil( pBaseArray->pItems + ulIndex - 1 );
         }
         else
         {
            for( ; ulIndex < ulLen; ++ulIndex )       /* move items */
               hb_itemMoveRef( pBaseArray->pItems + ulIndex - 1,
                               pBaseArray->pItems + ulIndex );
         }

         return TRUE;
      }
   }

   return FALSE;
}

BOOL hb_arrayIns( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayIns(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) )
   {
      ULONG ulLen = pArray->item.asArray.value->ulLen;

      if( ulIndex > 0 && ulIndex <= ulLen )
      {
         PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;

         if( ulIndex == ulLen )
         {
            hb_itemSetNil( pBaseArray->pItems + ulIndex - 1 );
         }
         else
         {
            while( --ulLen >= ulIndex )                     /* move items */
               hb_itemMoveRef( pBaseArray->pItems + ulLen,
                               pBaseArray->pItems + ulLen - 1 );
         }

         return TRUE;
      }
   }

   return FALSE;
}

BOOL hb_arraySet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySet(%p, %lu, %p)", pArray, ulIndex, pItem));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemCopy( pArray->item.asArray.value->pItems + ( ulIndex - 1 ), pItem );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetForward( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetForward(%p, %lu, %p)", pArray, ulIndex, pItem));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemMove( pArray->item.asArray.value->pItems + ( ulIndex - 1 ), pItem );
      return TRUE;
   }
   else
   {
      hb_itemClear( pItem );
      return FALSE;
   }
}

BOOL hb_arrayGet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGet(%p, %lu, %p)", pArray, ulIndex, pItem));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemCopy( pItem, pArray->item.asArray.value->pItems + ( ulIndex - 1 ) );
      return TRUE;
   }
   else
   {
      hb_itemSetNil( pItem );
      return FALSE;
   }
}

BOOL hb_arrayGetItemRef( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetItemRef(%p, %lu, %p)", pArray, ulIndex, pItem));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      if( pArray != pItem )
      {
         if( HB_IS_COMPLEX( pItem ) )
            hb_itemClear( pItem );
         hb_gcRefInc( pArray->item.asArray.value );
      }
      pItem->type = HB_IT_BYREF;
      pItem->item.asRefer.BasePtr.array = pArray->item.asArray.value;
      pItem->item.asRefer.value = ulIndex - 1;
      pItem->item.asRefer.offset = 0;
      return TRUE;
   }
   else
   {
      hb_itemSetNil( pItem );
      return FALSE;
   }
}

/*
 * This function returns a pointer to an item occupied by the specified
 * array element - it doesn't return an item's value
 */
PHB_ITEM hb_arrayGetItemPtr( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetItemPtr(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return pArray->item.asArray.value->pItems + ulIndex - 1;
   else
      return NULL;
}

char * hb_arrayGetDS( PHB_ITEM pArray, ULONG ulIndex, char * szDate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetDS(%p, %lu, %s)", pArray, ulIndex, szDate));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetDS( pArray->item.asArray.value->pItems + ulIndex - 1, szDate );
   else
      /* NOTE: Intentionally calling it with a bad parameter in order to get
               the default value from hb_itemGetDS(). [vszakats] */
      return hb_itemGetDS( NULL, szDate );
}

long hb_arrayGetDL( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetDL(%p, %lu)", pArray, ulIndex ));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetDL( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      /* NOTE: Intentionally calling it with a bad parameter in order to get
               the default value from hb_itemGetDL(). [vszakats] */
      return hb_itemGetDL( NULL );
}

double hb_arrayGetTD( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetTD(%p, %lu)", pArray, ulIndex ));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetTD( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

BOOL hb_arrayGetTDT( PHB_ITEM pArray, ULONG ulIndex, LONG * plJulian, LONG * plMilliSec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetTDT(%p, %lu, %p, %p)", pArray, ulIndex, plJulian, plMilliSec));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetTDT( pArray->item.asArray.value->pItems + ulIndex - 1, plJulian, plMilliSec );
   else
   {
      *plJulian = *plMilliSec = 0;
      return FALSE;
   }
}

BOOL hb_arrayGetL( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetL(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetL( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return FALSE;
}

int hb_arrayGetNI( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNI(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetNI( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

long hb_arrayGetNL( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNL(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetNL( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

#ifndef HB_LONG_LONG_OFF
LONGLONG hb_arrayGetNLL( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNLL(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetNLL( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}
#endif

HB_LONG hb_arrayGetNInt( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNLL(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetNInt( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

double hb_arrayGetND( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetND(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetND( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

ULONG hb_arrayCopyC( PHB_ITEM pArray, ULONG ulIndex, char * szBuffer, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayCopyC(%p, %lu, %s, %lu)", pArray, ulIndex, szBuffer, ulLen));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemCopyC( pArray->item.asArray.value->pItems + ulIndex - 1, szBuffer, ulLen );
   else
      return 0;
}

char * hb_arrayGetC( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetC(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetC( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return NULL;
}

char * hb_arrayGetCPtr( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetCPtr(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetCPtr( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return ( char * ) "";
}

ULONG hb_arrayGetCLen( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetCLen(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetCLen( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

void * hb_arrayGetPtr( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetPtr(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetPtr( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return NULL;
}

void * hb_arrayGetPtrGC( PHB_ITEM pArray, ULONG ulIndex, HB_GARBAGE_FUNC_PTR pFunc )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetPtrGC(%p, %lu, %p)", pArray, ulIndex, pFunc));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetPtrGC( pArray->item.asArray.value->pItems + ulIndex - 1, pFunc );
   else
      return NULL;
}

PHB_SYMB hb_arrayGetSymbol( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetSymbol(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetSymbol( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return NULL;
}


HB_TYPE hb_arrayGetType( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetType(%p, %lu)", pArray, ulIndex));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemType( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

BOOL hb_arraySetDS( PHB_ITEM pArray, ULONG ulIndex, const char * szDate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetDS(%p, %lu, %s)", pArray, ulIndex, szDate));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutDS( pArray->item.asArray.value->pItems + ulIndex - 1, szDate );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetDL( PHB_ITEM pArray, ULONG ulIndex, LONG lDate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetDL(%p, %lu, %ld)", pArray, ulIndex, lDate));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutDL( pArray->item.asArray.value->pItems + ulIndex - 1, lDate );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetTD( PHB_ITEM pArray, ULONG ulIndex, double dTimeStamp )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetTD(%p, %lu, %lf)", pArray, ulIndex, dTimeStamp));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutTD( pArray->item.asArray.value->pItems + ulIndex - 1, dTimeStamp );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetTDT( PHB_ITEM pArray, ULONG ulIndex, LONG lJulian, LONG lMilliSec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetTDT(%p, %lu, %lf)", pArray, ulIndex, lJulian, lMilliSec));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutTDT( pArray->item.asArray.value->pItems + ulIndex - 1, lJulian, lMilliSec );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetL( PHB_ITEM pArray, ULONG ulIndex, BOOL fValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetL(%p, %lu, %d)", pArray, ulIndex, fValue));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutL( pArray->item.asArray.value->pItems + ulIndex - 1, fValue );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetNI( PHB_ITEM pArray, ULONG ulIndex, int iNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetNI(%p, %lu, %d)", pArray, ulIndex, iNumber));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutNI( pArray->item.asArray.value->pItems + ulIndex - 1, iNumber );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetNL( PHB_ITEM pArray, ULONG ulIndex, LONG lNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetNL(%p, %lu, %lu)", pArray, ulIndex, lNumber));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutNL( pArray->item.asArray.value->pItems + ulIndex - 1, lNumber );
      return TRUE;
   }
   else
      return FALSE;
}

#ifndef HB_LONG_LONG_OFF
BOOL hb_arraySetNLL( PHB_ITEM pArray, ULONG ulIndex, LONGLONG llNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetNLL(%p, %lu, %" PFLL "d)", pArray, ulIndex, llNumber));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutNLL( pArray->item.asArray.value->pItems + ulIndex - 1, llNumber );
      return TRUE;
   }
   else
      return FALSE;
}
#endif

BOOL hb_arraySetNInt( PHB_ITEM pArray, ULONG ulIndex, HB_LONG lNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetNInt(%p, %lu, %" PFHL "d)", pArray, ulIndex, lNumber));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutNInt( pArray->item.asArray.value->pItems + ulIndex - 1, lNumber );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetND( PHB_ITEM pArray, ULONG ulIndex, double dNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetND(%p, %lu, %lf)", pArray, ulIndex, dNumber));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutND( pArray->item.asArray.value->pItems + ulIndex - 1, dNumber );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetC( PHB_ITEM pArray, ULONG ulIndex, const char * szText )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetC(%p, %lu, %p)", pArray, ulIndex, szText));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutC( pArray->item.asArray.value->pItems + ulIndex - 1, szText );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetCL( PHB_ITEM pArray, ULONG ulIndex, const char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetC(%p, %lu, %p, %lu)", pArray, ulIndex, szText, ulLen));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutCL( pArray->item.asArray.value->pItems + ulIndex - 1, szText, ulLen );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetCPtr( PHB_ITEM pArray, ULONG ulIndex, char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetCPtr(%p, %lu, %p, %lu)", pArray, ulIndex, szText, ulLen));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutCLPtr( pArray->item.asArray.value->pItems + ulIndex - 1, szText, ulLen );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetPtr( PHB_ITEM pArray, ULONG ulIndex, void * pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetPtr(%p, %lu, %p)", pArray, ulIndex, pValue));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutPtr( pArray->item.asArray.value->pItems + ulIndex - 1, pValue );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetPtrGC( PHB_ITEM pArray, ULONG ulIndex, void * pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetPtrGC(%p, %lu, %p)", pArray, ulIndex, pValue));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutPtrGC( pArray->item.asArray.value->pItems + ulIndex - 1, pValue );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arraySetSymbol( PHB_ITEM pArray, ULONG ulIndex, PHB_SYMB pSymbol )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySetSymbol(%p, %lu, %p)", pArray, ulIndex, pSymbol));

   if( HB_IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemPutSymbol( pArray->item.asArray.value->pItems + ulIndex - 1, pSymbol );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayLast(%p, %p)", pArray, pResult));

   if( HB_IS_ARRAY( pArray ) )
   {
      if( pArray->item.asArray.value->ulLen > 0 )
         hb_itemCopy( pResult, pArray->item.asArray.value->pItems +
                             ( pArray->item.asArray.value->ulLen - 1 ) );
      else
         hb_itemSetNil( pResult );

      return TRUE;
   }

   hb_itemSetNil( pResult );

   return FALSE;
}

BOOL hb_arrayFill( PHB_ITEM pArray, PHB_ITEM pValue, ULONG * pulStart, ULONG * pulCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayFill(%p, %p, %p, %p)", pArray, pValue, pulStart, pulCount));

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG ulLen = pBaseArray->ulLen;
      ULONG ulStart;
      ULONG ulCount;

      if( pulStart && *pulStart )
         ulStart = *pulStart - 1;
      else
         ulStart = 0;

      if( ulStart < ulLen )
      {
         ulCount = ulLen - ulStart;
         if( pulCount && *pulCount < ulCount )
            ulCount = *pulCount;

         if( ulCount > 0 )
         {
            do
            {
               hb_itemCopy( pBaseArray->pItems + ulStart++, pValue );
            }
            while( --ulCount > 0 );
         }
      }

      return TRUE;
   }
   else
      return FALSE;
}

ULONG hb_arrayScan( PHB_ITEM pArray, PHB_ITEM pValue, ULONG * pulStart, ULONG * pulCount, BOOL fExact )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayScan(%p, %p, %p, %p, %d)", pArray, pValue, pulStart, pulCount, (int) fExact));

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG ulLen = pBaseArray->ulLen;
      ULONG ulStart;
      ULONG ulCount;

      if( pulStart && *pulStart )
         ulStart = *pulStart - 1;
      else
         ulStart = 0;

      if( ulStart < ulLen )
      {
         ulCount = ulLen - ulStart;
         if( pulCount && *pulCount < ulCount )
            ulCount = *pulCount;

         if( ulCount > 0 )
         {
            /* Make separate search loops for different types to find, so that
               the loop can be faster. */

            if( HB_IS_BLOCK( pValue ) )
            {
               do
               {
                  hb_vmPushSymbol( &hb_symEval );
                  hb_vmPush( pValue );
                  hb_vmPush( pBaseArray->pItems + ulStart );
                  hb_vmPushLong( ++ulStart );
                  hb_vmDo( 2 );

                  if( HB_IS_LOGICAL( hb_stackReturnItem() ) && hb_stackReturnItem()->item.asLogical.value )
                     return ulStart;
               }
               while( --ulCount > 0 && ulStart < pBaseArray->ulLen );
            }
            else if( HB_IS_STRING( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart++;

                  /* NOTE: The order of the pItem and pValue parameters passed to
                           hb_itemStrCmp() is significant, please don't change it. [vszakats] */
                  if( HB_IS_STRING( pItem ) && hb_itemStrCmp( pItem, pValue, fExact ) == 0 )
                     return ulStart;
               }
               while( --ulCount > 0 );
            }
            else if( HB_IS_NUMERIC( pValue ) )
            {
               double dValue = hb_itemGetND( pValue );

               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart++;

                  if( HB_IS_NUMERIC( pItem ) && hb_itemGetND( pItem ) == dValue )
                     return ulStart;
               }
               while( --ulCount > 0 );
            }
            else if( HB_IS_DATETIME( pValue ) )
            {
               if( fExact )
               {
                  do
                  {
                     PHB_ITEM pItem = pBaseArray->pItems + ulStart++;

                     if( HB_IS_DATETIME( pItem ) &&
                         pItem->item.asDateTime.julian == pValue->item.asDateTime.julian &&
                         pItem->item.asDateTime.time == pValue->item.asDateTime.time )
                        return ulStart;
                  }
                  while( --ulCount > 0 );
               }
               else
               {
                  do
                  {
                     PHB_ITEM pItem = pBaseArray->pItems + ulStart++;

                     if( HB_IS_DATETIME( pItem ) &&
                         pItem->item.asDateTime.julian == pValue->item.asDateTime.julian )
                        return ulStart;
                  }
                  while( --ulCount > 0 );
               }
            }
            else if( HB_IS_LOGICAL( pValue ) )
            {
               BOOL bValue = hb_itemGetL( pValue );

               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart++;

                  if( HB_IS_LOGICAL( pItem ) && hb_itemGetL( pItem ) == bValue )
                     return ulStart;
               }
               while( --ulCount > 0 );
            }
            else if( HB_IS_NIL( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart++;

                  if( HB_IS_NIL( pItem ) )
                     return ulStart;
               }
               while( --ulCount > 0 );
            }
            else if( HB_IS_POINTER( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart++;

                  if( HB_IS_POINTER( pItem ) &&
                      pItem->item.asPointer.value == pValue->item.asPointer.value )
                     return ulStart;
               }
               while( --ulCount > 0 );
            }
            else if( fExact && HB_IS_ARRAY( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart++;

                  if( HB_IS_ARRAY( pItem ) &&
                      pItem->item.asArray.value == pValue->item.asArray.value )
                     return ulStart;
               }
               while( --ulCount > 0 );
            }
            else if( fExact && HB_IS_HASH( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart++;

                  if( HB_IS_HASH( pItem ) &&
                      pItem->item.asHash.value == pValue->item.asHash.value )
                     return ulStart;
               }
               while( --ulCount > 0 );
            }
         }
      }
   }

   return 0;
}

ULONG hb_arrayRevScan( PHB_ITEM pArray, PHB_ITEM pValue, ULONG * pulStart, ULONG * pulCount, BOOL fExact )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayRevScan(%p, %p, %p, %p, %d)", pArray, pValue, pulStart, pulCount, (int) fExact));

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG ulLen = pBaseArray->ulLen;
      ULONG ulStart;
      ULONG ulCount;

      if( pulStart && *pulStart )
         ulStart = *pulStart - 1;
      else
         ulStart = ulLen - 1;

      if( ulStart < ulLen )
      {
         ulCount = ulStart + 1;
         if( pulCount && *pulCount < ulCount )
            ulCount = *pulCount;

         if( ulCount > 0 )
         {
            /* Make separate search loops for different types to find, so that
               the loop can be faster. */

            if( HB_IS_BLOCK( pValue ) )
            {
               do
               {
                  hb_vmPushSymbol( &hb_symEval );
                  hb_vmPush( pValue );
                  if( ulStart < pBaseArray->ulLen )
                     hb_vmPush( pBaseArray->pItems + ulStart );
                  else
                     hb_vmPushNil();
                  hb_vmPushLong( ulStart + 1 );
                  hb_vmDo( 2 );

                  if( HB_IS_LOGICAL( hb_stackReturnItem() ) && hb_stackReturnItem()->item.asLogical.value )
                     return ulStart + 1;
               }
               while( --ulCount && ulStart-- );
            }
            else if( HB_IS_STRING( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart;

                  /* NOTE: The order of the pItem and pValue parameters passed to
                           hb_itemStrCmp() is significant, please don't change it. [vszakats] */
                  if( HB_IS_STRING( pItem ) && hb_itemStrCmp( pItem, pValue, fExact ) == 0 )
                     return ulStart + 1;
               }
               while( --ulCount && ulStart-- );
            }
            else if( HB_IS_NUMERIC( pValue ) )
            {
               double dValue = hb_itemGetND( pValue );

               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart;

                  if( HB_IS_NUMERIC( pItem ) && hb_itemGetND( pItem ) == dValue )
                     return ulStart + 1;
               }
               while( --ulCount && ulStart-- );
            }
            else if( HB_IS_DATETIME( pValue ) )
            {
               if( fExact )
               {
                  do
                  {
                     PHB_ITEM pItem = pBaseArray->pItems + ulStart;

                     if( HB_IS_DATETIME( pItem ) &&
                         pItem->item.asDateTime.julian == pValue->item.asDateTime.julian &&
                         pItem->item.asDateTime.time == pValue->item.asDateTime.time )
                        return ulStart + 1;
                  }
                  while( --ulCount && ulStart-- );
               }
               else
               {
                  do
                  {
                     PHB_ITEM pItem = pBaseArray->pItems + ulStart;

                     if( HB_IS_DATETIME( pItem ) &&
                         pItem->item.asDateTime.julian == pValue->item.asDateTime.julian )
                        return ulStart + 1;
                  }
                  while( --ulCount && ulStart-- );
               }
            }
            else if( HB_IS_LOGICAL( pValue ) )
            {
               BOOL bValue = hb_itemGetL( pValue );

               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart;

                  if( HB_IS_LOGICAL( pItem ) && hb_itemGetL( pItem ) == bValue )
                     return ulStart + 1;
               }
               while( --ulCount && ulStart-- );
            }
            else if( HB_IS_NIL( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart;

                  if( HB_IS_NIL( pItem ) )
                     return ulStart + 1;
               }
               while( --ulCount && ulStart-- );
            }
            else if( HB_IS_POINTER( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart;

                  if( HB_IS_POINTER( pItem ) &&
                      pItem->item.asPointer.value == pValue->item.asPointer.value )
                     return ulStart + 1;
               }
               while( --ulCount && ulStart-- );
            }
            else if( fExact && HB_IS_ARRAY( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart;

                  if( HB_IS_ARRAY( pItem ) &&
                      pItem->item.asArray.value == pValue->item.asArray.value )
                     return ulStart + 1;
               }
               while( --ulCount && ulStart-- );
            }
            else if( fExact && HB_IS_HASH( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + ulStart;

                  if( HB_IS_HASH( pItem ) &&
                      pItem->item.asHash.value == pValue->item.asHash.value )
                     return ulStart + 1;
               }
               while( --ulCount && ulStart-- );
            }
         }
      }
   }

   return 0;
}

BOOL hb_arrayEval( PHB_ITEM pArray, PHB_ITEM bBlock, ULONG * pulStart, ULONG * pulCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayEval(%p, %p, %p, %p)", pArray, bBlock, pulStart, pulCount));

   if( HB_IS_ARRAY( pArray ) && HB_IS_BLOCK( bBlock ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG ulLen = pBaseArray->ulLen;
      ULONG ulStart;
      ULONG ulCount;

      if( pulStart && *pulStart )
         ulStart = *pulStart - 1;
      else
         ulStart = 0;

      if( ulStart < ulLen )
      {
         ulCount = ulLen - ulStart;
         if( pulCount && *pulCount < ulCount )
            ulCount = *pulCount;

         if( ulCount > 0 )
         {
            do
            {
               hb_vmPushSymbol( &hb_symEval );
               hb_vmPush( bBlock );
               hb_vmPush( pBaseArray->pItems + ulStart );
               hb_vmPushLong( ulStart + 1 );
               hb_vmDo( 2 );
            }
            while( --ulCount > 0 && ++ulStart < pBaseArray->ulLen );
            /*
             * checking for ulStart < pBaseArray->ulLen is fix for
             * possible GPF when codeblock decrease array size
             */
         }
      }

      return TRUE;
   }
   else
      return FALSE;
}

/* NOTE: CA-Cl*pper 5.3a has a fix for the case when the starting position
         is greater than the length of the array. [vszakats] */

BOOL hb_arrayCopy( PHB_ITEM pSrcArray, PHB_ITEM pDstArray, ULONG * pulStart,
                   ULONG * pulCount, ULONG * pulTarget )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayCopy(%p, %p, %p, %p, %p)", pSrcArray, pDstArray, pulStart, pulCount, pulTarget));

   if( HB_IS_ARRAY( pSrcArray ) && HB_IS_ARRAY( pDstArray ) )
   {
      PHB_BASEARRAY pSrcBaseArray = pSrcArray->item.asArray.value;
      PHB_BASEARRAY pDstBaseArray = pDstArray->item.asArray.value;
      ULONG ulSrcLen = pSrcBaseArray->ulLen;
      ULONG ulDstLen = pDstBaseArray->ulLen;
      ULONG ulStart;
      ULONG ulCount;
      ULONG ulTarget;

      if( pulStart && ( *pulStart >= 1 ) )
         ulStart = *pulStart;
      else
         ulStart = 1;

      if( pulTarget && ( *pulTarget >= 1 ) )
         ulTarget = *pulTarget;
      else
         ulTarget = 1;

#ifdef HB_COMPAT_C53 /* From CA-Cl*pper 5.3a */
      if( ulStart <= ulSrcLen )
#else
      if( ulSrcLen > 0 )
#endif
      {
#ifndef HB_COMPAT_C53 /* From CA-Cl*pper 5.3a */
         if( ulStart > ulSrcLen )
            ulStart = ulSrcLen;
#endif
         if( pulCount && ( *pulCount <= ulSrcLen - ulStart ) )
            ulCount = *pulCount;
         else
            ulCount = ulSrcLen - ulStart + 1;

/* This is probably a bug, present in all versions of CA-Cl*pper. */
#if defined( HB_C52_STRICT ) || 1
         if( ulDstLen > 0 )
         {
            if( ulTarget > ulDstLen )
               ulTarget = ulDstLen;
#else
         if( ulTarget <= ulDstLen )
         {
#endif

            if( ulCount > ulDstLen - ulTarget )
               ulCount = ulDstLen - ulTarget + 1;

            for( ulTarget--, ulStart--; ulCount > 0; ulCount--, ulStart++, ulTarget++ )
               hb_itemCopy( pDstBaseArray->pItems + ulTarget, pSrcBaseArray->pItems + ulStart );
         }
      }

      return TRUE;
   }
   else
      return FALSE;
}

static void hb_arrayCloneBody( PHB_BASEARRAY pSrcBaseArray, PHB_BASEARRAY pDstBaseArray, PHB_NESTED_CLONED pClonedList )
{
   PHB_ITEM pSrcItem, pDstItem;
   ULONG ulLen;

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayCloneBody(%p, %p, %p)", pSrcBaseArray, pDstBaseArray, pClonedList));

   pSrcItem = pSrcBaseArray->pItems;
   pDstItem = pDstBaseArray->pItems;

   pDstBaseArray->uiClass = pSrcBaseArray->uiClass;

   for( ulLen = pSrcBaseArray->ulLen; ulLen; --ulLen, ++pSrcItem, ++pDstItem )
      hb_cloneNested( pDstItem, pSrcItem, pClonedList );
}

void hb_cloneNested( PHB_ITEM pDstItem, PHB_ITEM pSrcItem, PHB_NESTED_CLONED pClonedList )
{
   /* Clipper clones nested array ONLY if NOT an Object!!! */
   if( HB_IS_ARRAY( pSrcItem ) && pSrcItem->item.asArray.value->uiClass == 0 )
   {
      PHB_NESTED_CLONED pCloned = pClonedList;
      PHB_BASEARRAY pBaseArray = pSrcItem->item.asArray.value;

      do
      {
         if( pCloned->value == ( void * ) pBaseArray )
            break;
         pCloned = pCloned->pNext;
      }
      while( pCloned );

      if( pCloned )
         hb_itemCopy( pDstItem, pCloned->pDest );
      else
      {
         hb_arrayNew( pDstItem, pBaseArray->ulLen );

         pCloned = ( PHB_NESTED_CLONED ) hb_xgrab( sizeof( HB_NESTED_CLONED ) );
         pCloned->value     = ( void * ) pBaseArray;
         pCloned->pDest     = pDstItem;
         pCloned->pNext     = pClonedList->pNext;
         pClonedList->pNext = pCloned;

         hb_arrayCloneBody( pBaseArray, pDstItem->item.asArray.value, pClonedList );
      }
   }
   else if( HB_IS_HASH( pSrcItem ) )
   {
      PHB_NESTED_CLONED pCloned = pClonedList;
      PHB_BASEHASH pBaseHash = pSrcItem->item.asHash.value;

      do
      {
         if( pCloned->value == ( void * ) pBaseHash )
            break;
         pCloned = pCloned->pNext;
      }
      while( pCloned );

      if( pCloned )
         hb_itemCopy( pDstItem, pCloned->pDest );
      else
      {
         pCloned = ( PHB_NESTED_CLONED ) hb_xgrab( sizeof( HB_NESTED_CLONED ) );
         pCloned->value     = ( void * ) pBaseHash;
         pCloned->pDest     = pDstItem;
         pCloned->pNext     = pClonedList->pNext;
         pClonedList->pNext = pCloned;

         hb_hashCloneBody( pSrcItem, pDstItem, pClonedList );
      }
   }
   else
      hb_itemCopy( pDstItem, pSrcItem );
}

PHB_ITEM hb_arrayClone( PHB_ITEM pSrcArray )
{
   PHB_ITEM pDstArray;

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayClone(%p)", pSrcArray));

   pDstArray = hb_itemNew( NULL );
   if( HB_IS_ARRAY( pSrcArray ) )
   {
      PHB_NESTED_CLONED pClonedList, pCloned;
      PHB_BASEARRAY pSrcBaseArray = pSrcArray->item.asArray.value;
      ULONG ulSrcLen = pSrcBaseArray->ulLen;

      hb_arrayNew( pDstArray, ulSrcLen );
      pClonedList = ( PHB_NESTED_CLONED ) hb_xgrab( sizeof( HB_NESTED_CLONED ) );
      pClonedList->value = ( void * ) pSrcBaseArray;
      pClonedList->pDest = pDstArray;
      pClonedList->pNext = NULL;

      hb_arrayCloneBody( pSrcBaseArray, pDstArray->item.asArray.value, pClonedList );

      do
      {
         pCloned = pClonedList;
         pClonedList = pClonedList->pNext;
         hb_xfree( pCloned );
      }
      while( pClonedList );
   }
   return pDstArray;
}

PHB_ITEM hb_arrayFromStack( USHORT uiLen )
{
   PHB_ITEM pArray = hb_itemNew( NULL );
   USHORT uiPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayFromStack(%hu)", uiLen));

   hb_arrayNew( pArray, uiLen );

   for( uiPos = 1; uiPos <= uiLen; uiPos++ )
   {
      hb_arraySet( pArray, uiPos, hb_stackItemFromTop( uiPos - uiLen - 1 ) );
   }

   return pArray;
}

PHB_ITEM hb_arrayFromParams( int iLevel )
{
   PHB_ITEM pArray;
   USHORT uiPos, uiPCount;
   LONG lBaseOffset;

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayFromParams(%d)", iLevel));

   lBaseOffset = hb_stackBaseProcOffset( iLevel );
   if( lBaseOffset > 0 )
      uiPCount = hb_stackItem( lBaseOffset )->item.asSymbol.paramcnt;
   else
      uiPCount = 0;

   pArray = hb_itemArrayNew( uiPCount );
   for( uiPos = 1; uiPos <= uiPCount; uiPos++ )
   {
      hb_arraySet( pArray, uiPos, hb_stackItem( lBaseOffset + uiPos + 1 ) );
   }

   return pArray;
}

PHB_ITEM hb_arrayBaseParams( void )
{
   PHB_ITEM pArray;
   USHORT uiPos, uiPCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayBaseParams()"));

   pArray = hb_itemNew( NULL );
   uiPCount = hb_stackBaseItem()->item.asSymbol.paramcnt;

   hb_arrayNew( pArray, uiPCount );

   for( uiPos = 1; uiPos <= uiPCount; uiPos++ )
   {
      hb_arraySet( pArray, uiPos, hb_stackItemFromBase( uiPos ) );
   }

   return pArray;
}

PHB_ITEM hb_arraySelfParams( void )
{
   PHB_ITEM pArray;
   USHORT uiPos, uiPCount;

   HB_TRACE(HB_TR_DEBUG, ("hb_arraySelfParams()"));

   pArray = hb_itemNew( NULL );
   uiPCount = hb_stackBaseItem()->item.asSymbol.paramcnt;

   hb_arrayNew( pArray, uiPCount + 1 );

   for( uiPos = 0; uiPos <= uiPCount; uiPos++ )
   {
      hb_arraySet( pArray, uiPos + 1, hb_stackItemFromBase( uiPos ) );
   }

   return pArray;
}
