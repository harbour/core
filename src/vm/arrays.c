/*
 * Harbour Project source code:
 * The Array API (C level)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * www - http://harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 *    hb_arrayIsObject()
 *    hb_arrayCopyC()
 *    hb_arrayGetC()
 *
 * Copyright 2001 Ron Pinkas <ron@profit-master.com>
 *    hb_arrayClone()
 *    hb_arrayFromStack()
 *    hb_arrayFromParams()
 *
 * See COPYING.txt for licensing terms.
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
   if( pBaseArray->nLen )
   {
      PHB_ITEM pItems = pBaseArray->pItems;
      HB_SIZE nLen = pBaseArray->nLen;

      /*
       * clear the pBaseArray->pItems to avoid infinite loop in cross
       * referenced items when pBaseArray is not freed due to buggy
       * object destructor [druzus]
       */
      pBaseArray->pItems = NULL;
      pBaseArray->nLen = 0;

      while( nLen-- )
      {
         if( HB_IS_COMPLEX( pItems + nLen ) )
            hb_itemClear( pItems + nLen );
      }
      hb_xfree( pItems );
   }
}

void hb_arrayPushBase( PHB_BASEARRAY pBaseArray )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   pItem->type = HB_IT_ARRAY;
   pItem->item.asArray.value = pBaseArray;
   hb_gcRefInc( pBaseArray );
}

/* This releases array when called from the garbage collector */
static HB_GARBAGE_FUNC( hb_arrayGarbageRelease )
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
         HB_STACK_TLS_PRELOAD
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

static HB_GARBAGE_FUNC( hb_arrayGarbageMark )
{
   PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) Cargo;

   if( pBaseArray->nLen )
   {
      HB_SIZE nLen = pBaseArray->nLen;
      PHB_ITEM pItems = pBaseArray->pItems;

      while( nLen-- )
      {
         if( HB_IS_GCITEM( pItems + nLen ) )
            hb_gcItemRef( pItems + nLen );
      }
   }
}

static const HB_GC_FUNCS s_gcArrayFuncs =
{
   hb_arrayGarbageRelease,
   hb_arrayGarbageMark
};


HB_BOOL hb_arrayNew( PHB_ITEM pItem, HB_SIZE nLen ) /* creates a new array */
{
   PHB_BASEARRAY pBaseArray;
   PHB_ITEM pItems;
   HB_SIZE nPos;

   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayNew(%p, %" HB_PFS "u)", pItem, nLen ) );

   if( HB_IS_COMPLEX( pItem ) )
      hb_itemClear( pItem );

   /*
    * allocate memory for items before hb_gcAllocRaw() to be
    * safe for automatic GC activation in hb_xgrab() without
    * calling hb_gcLock()/hb_gcUnlock(). [druzus]
    */
   if( nLen > 0 )
   {
      pItems = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) * nLen );
      for( nPos = 0; nPos < nLen; ++nPos )
         ( pItems + nPos )->type = HB_IT_NIL;
   }
   else
      pItems = NULL;

   pBaseArray = ( PHB_BASEARRAY ) hb_gcAllocRaw( sizeof( HB_BASEARRAY ), &s_gcArrayFuncs );
   pBaseArray->pItems     = pItems;
   pBaseArray->nLen       = nLen;
   pBaseArray->uiClass    = 0;
   pBaseArray->uiPrevCls  = 0;
   pBaseArray->nAllocated = nLen;
   pItem->type = HB_IT_ARRAY;
   pItem->item.asArray.value = pBaseArray;

   return HB_TRUE;
}

void hb_arraySwap( PHB_ITEM pArray1, PHB_ITEM pArray2 )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySwap(%p, %p)", pArray1, pArray2 ) );

   if( HB_IS_ARRAY( pArray1 ) && HB_IS_ARRAY( pArray2 ) )
   {
      HB_BASEARRAY tmpBaseArray;

      tmpBaseArray = * pArray1->item.asArray.value;
      * pArray1->item.asArray.value = * pArray2->item.asArray.value;
      * pArray2->item.asArray.value = tmpBaseArray;
   }
}

HB_BOOL hb_arraySize( PHB_ITEM pArray, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySize(%p, %" HB_PFS "u)", pArray, nLen ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;

      if( nLen != pBaseArray->nLen )
      {
         HB_SIZE nPos;

         if( pBaseArray->nLen == 0 )
         {
            pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( nLen * sizeof( HB_ITEM ) );
            pBaseArray->nAllocated = nLen;

            for( nPos = 0; nPos < nLen; nPos++ )
               ( pBaseArray->pItems + nPos )->type = HB_IT_NIL;
         }
         else
         {
            if( pBaseArray->nLen < nLen )
            {
               if( pBaseArray->nAllocated < nLen )
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
                  pBaseArray->nAllocated = ( pBaseArray->nAllocated >> 1 ) + 1 + nLen;
                  pBaseArray->pItems = ( PHB_ITEM ) hb_xrealloc( pBaseArray->pItems, sizeof( HB_ITEM ) * pBaseArray->nAllocated );
               }

               /* set value for new items */
               for( nPos = pBaseArray->nLen; nPos < nLen; nPos++ )
                  ( pBaseArray->pItems + nPos )->type = HB_IT_NIL;
            }
            else if( pBaseArray->nLen > nLen )
            {
               /* release old items */
               for( nPos = nLen; nPos < pBaseArray->nLen; nPos++ )
               {
                  if( HB_IS_COMPLEX( pBaseArray->pItems + nPos ) )
                     hb_itemClear( pBaseArray->pItems + nPos );
               }

               if( nLen == 0 )
               {
                  hb_xfree( pBaseArray->pItems );
                  pBaseArray->pItems = NULL;
               }
               else if( nLen < ( pBaseArray->nAllocated >> 1 ) )
               {
                  pBaseArray->pItems = ( PHB_ITEM ) hb_xrealloc( pBaseArray->pItems, sizeof( HB_ITEM ) * nLen );
                  pBaseArray->nAllocated = nLen;
               }
            }
         }

         pBaseArray->nLen = nLen;
      }

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_SIZE hb_arrayLen( PHB_ITEM pArray )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayLen(%p)", pArray ) );

   if( HB_IS_ARRAY( pArray ) )
      return pArray->item.asArray.value->nLen;
   else
      return 0;
}

HB_BOOL hb_arrayIsObject( PHB_ITEM pArray )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayIsObject(%p)", pArray ) );

   if( HB_IS_ARRAY( pArray ) )
      return pArray->item.asArray.value->uiClass != 0;
   else
      return HB_FALSE;
}

/* retrives the array unique ID */
void * hb_arrayId( PHB_ITEM pArray )
{
   if( pArray && HB_IS_ARRAY( pArray ) )
      return ( void * ) pArray->item.asArray.value;
   else
      return NULL;
}

PHB_ITEM hb_arrayFromId( PHB_ITEM pItem, void * pArrayId )
{
   HB_STACK_TLS_PRELOAD

   hb_arrayPushBase( ( PHB_BASEARRAY ) pArrayId );
   if( pItem == NULL )
      pItem = hb_itemNew( NULL );
   hb_itemMove( pItem, hb_stackItemFromTop( -1 ) );
   hb_stackPop();

   return pItem;
}

HB_BOOL hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayAdd(%p, %p)", pArray, pValue ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;

      if( pBaseArray->nLen < HB_SIZE_MAX )
      {
         hb_arraySize( pArray, pBaseArray->nLen + 1 );
         pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;
         hb_itemCopy( pBaseArray->pItems + ( pBaseArray->nLen - 1 ), pValue );

         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

HB_BOOL hb_arrayAddForward( PHB_ITEM pArray, PHB_ITEM pValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayAddForward(%p, %p)", pArray, pValue ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;

      if( pBaseArray->nLen < HB_SIZE_MAX )
      {
         hb_arraySize( pArray, pBaseArray->nLen + 1 );
         pBaseArray = ( PHB_BASEARRAY ) pArray->item.asArray.value;
         hb_itemMove( pBaseArray->pItems + ( pBaseArray->nLen - 1 ), pValue );

         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

HB_BOOL hb_arrayDel( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayDel(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      HB_SIZE nLen = pArray->item.asArray.value->nLen;

      if( nIndex > 0 && nIndex <= nLen )
      {
         PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;

         if( nIndex == nLen )
         {
            hb_itemSetNil( pBaseArray->pItems + nIndex - 1 );
         }
         else
         {
            for(; nIndex < nLen; ++nIndex )        /* move items */
               hb_itemMoveRef( pBaseArray->pItems + nIndex - 1,
                               pBaseArray->pItems + nIndex );
         }

         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

HB_BOOL hb_arrayIns( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayIns(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      HB_SIZE nLen = pArray->item.asArray.value->nLen;

      if( nIndex > 0 && nIndex <= nLen )
      {
         PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;

         if( nIndex == nLen )
         {
            hb_itemSetNil( pBaseArray->pItems + nIndex - 1 );
         }
         else
         {
            while( --nLen >= nIndex )                     /* move items */
               hb_itemMoveRef( pBaseArray->pItems + nLen,
                               pBaseArray->pItems + nLen - 1 );
         }

         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

HB_BOOL hb_arraySet( PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySet(%p, %" HB_PFS "u, %p)", pArray, nIndex, pItem ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemCopy( pArray->item.asArray.value->pItems + ( nIndex - 1 ), pItem );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetForward( PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetForward(%p, %" HB_PFS "u, %p)", pArray, nIndex, pItem ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemMove( pArray->item.asArray.value->pItems + ( nIndex - 1 ), pItem );
      return HB_TRUE;
   }
   else
   {
      hb_itemClear( pItem );
      return HB_FALSE;
   }
}

HB_BOOL hb_arrayGet( PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGet(%p, %" HB_PFS "u, %p)", pArray, nIndex, pItem ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemCopy( pItem, pArray->item.asArray.value->pItems + ( nIndex - 1 ) );
      return HB_TRUE;
   }
   else
   {
      hb_itemSetNil( pItem );
      return HB_FALSE;
   }
}

HB_BOOL hb_arrayGetItemRef( PHB_ITEM pArray, HB_SIZE nIndex, PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetItemRef(%p, %" HB_PFS "u, %p)", pArray, nIndex, pItem ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      if( pArray != pItem )
      {
         if( HB_IS_COMPLEX( pItem ) )
            hb_itemClear( pItem );
         hb_gcRefInc( pArray->item.asArray.value );
      }
      pItem->type = HB_IT_BYREF;
      pItem->item.asRefer.BasePtr.array = pArray->item.asArray.value;
      pItem->item.asRefer.value = nIndex - 1;
      pItem->item.asRefer.offset = 0;
      return HB_TRUE;
   }
   else
   {
      hb_itemSetNil( pItem );
      return HB_FALSE;
   }
}

/*
 * This function returns a pointer to an item occupied by the specified
 * array element - it doesn't return an item's value
 */
PHB_ITEM hb_arrayGetItemPtr( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetItemPtr(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return pArray->item.asArray.value->pItems + nIndex - 1;
   else
      return NULL;
}

char * hb_arrayGetDS( PHB_ITEM pArray, HB_SIZE nIndex, char * szDate )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetDS(%p, %" HB_PFS "u, %s)", pArray, nIndex, szDate ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetDS( pArray->item.asArray.value->pItems + nIndex - 1, szDate );
   else
      /* NOTE: Intentionally calling it with a bad parameter in order to get
               the default value from hb_itemGetDS(). [vszakats] */
      return hb_itemGetDS( NULL, szDate );
}

long hb_arrayGetDL( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetDL(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetDL( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      /* NOTE: Intentionally calling it with a bad parameter in order to get
               the default value from hb_itemGetDL(). [vszakats] */
      return hb_itemGetDL( NULL );
}

double hb_arrayGetTD( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetTD(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetTD( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

HB_BOOL hb_arrayGetTDT( PHB_ITEM pArray, HB_SIZE nIndex, long * plJulian, long * plMilliSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetTDT(%p, %" HB_PFS "u, %p, %p)", pArray, nIndex, plJulian, plMilliSec ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetTDT( pArray->item.asArray.value->pItems + nIndex - 1, plJulian, plMilliSec );
   else
   {
      *plJulian = *plMilliSec = 0;
      return HB_FALSE;
   }
}

HB_BOOL hb_arrayGetL( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetL(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetL( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return HB_FALSE;
}

int hb_arrayGetNI( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetNI(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetNI( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

long hb_arrayGetNL( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetNL(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetNL( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

HB_ISIZ hb_arrayGetNS( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetNS(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetNS( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

#ifndef HB_LONG_LONG_OFF
HB_LONGLONG hb_arrayGetNLL( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetNLL(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetNLL( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}
#endif

HB_MAXINT hb_arrayGetNInt( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetNInt(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetNInt( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

double hb_arrayGetND( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetND(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetND( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

HB_SIZE hb_arrayCopyC( PHB_ITEM pArray, HB_SIZE nIndex, char * szBuffer, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayCopyC(%p, %" HB_PFS "u, %s, %" HB_PFS "u)", pArray, nIndex, szBuffer, nLen ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemCopyC( pArray->item.asArray.value->pItems + nIndex - 1, szBuffer, nLen );
   else
      return 0;
}

char * hb_arrayGetC( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetC(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetC( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return NULL;
}

const char * hb_arrayGetCPtr( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetCPtr(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetCPtr( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return "";
}

HB_SIZE hb_arrayGetCLen( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetCLen(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetCLen( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

void * hb_arrayGetPtr( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetPtr(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetPtr( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return NULL;
}

void * hb_arrayGetPtrGC( PHB_ITEM pArray, HB_SIZE nIndex, const HB_GC_FUNCS * pFuncs )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetPtrGC(%p, %" HB_PFS "u, %p)", pArray, nIndex, pFuncs ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetPtrGC( pArray->item.asArray.value->pItems + nIndex - 1, pFuncs );
   else
      return NULL;
}

PHB_SYMB hb_arrayGetSymbol( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetSymbol(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemGetSymbol( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return NULL;
}


HB_TYPE hb_arrayGetType( PHB_ITEM pArray, HB_SIZE nIndex )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayGetType(%p, %" HB_PFS "u)", pArray, nIndex ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
      return hb_itemType( pArray->item.asArray.value->pItems + nIndex - 1 );
   else
      return 0;
}

HB_BOOL hb_arraySetDS( PHB_ITEM pArray, HB_SIZE nIndex, const char * szDate )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetDS(%p, %" HB_PFS "u, %s)", pArray, nIndex, szDate ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutDS( pArray->item.asArray.value->pItems + nIndex - 1, szDate );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetDL( PHB_ITEM pArray, HB_SIZE nIndex, long lDate )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetDL(%p, %" HB_PFS "u, %ld)", pArray, nIndex, lDate ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutDL( pArray->item.asArray.value->pItems + nIndex - 1, lDate );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetTD( PHB_ITEM pArray, HB_SIZE nIndex, double dTimeStamp )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetTD(%p, %" HB_PFS "u, %lf)", pArray, nIndex, dTimeStamp ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutTD( pArray->item.asArray.value->pItems + nIndex - 1, dTimeStamp );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetTDT( PHB_ITEM pArray, HB_SIZE nIndex, long lJulian, long lMilliSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetTDT(%p, %" HB_PFS "u, %lu, %lu)", pArray, nIndex, lJulian, lMilliSec ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutTDT( pArray->item.asArray.value->pItems + nIndex - 1, lJulian, lMilliSec );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetL( PHB_ITEM pArray, HB_SIZE nIndex, HB_BOOL fValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetL(%p, %" HB_PFS "u, %d)", pArray, nIndex, fValue ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutL( pArray->item.asArray.value->pItems + nIndex - 1, fValue );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetNI( PHB_ITEM pArray, HB_SIZE nIndex, int iNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetNI(%p, %" HB_PFS "u, %d)", pArray, nIndex, iNumber ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutNI( pArray->item.asArray.value->pItems + nIndex - 1, iNumber );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetNL( PHB_ITEM pArray, HB_SIZE nIndex, long lNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetNL(%p, %" HB_PFS "u, %lu)", pArray, nIndex, lNumber ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutNL( pArray->item.asArray.value->pItems + nIndex - 1, lNumber );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetNS( PHB_ITEM pArray, HB_SIZE nIndex, HB_ISIZ nNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetNS(%p, %" HB_PFS "u, %" HB_PFS "d)", pArray, nIndex, nNumber ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutNS( pArray->item.asArray.value->pItems + nIndex - 1, nNumber );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

#ifndef HB_LONG_LONG_OFF
HB_BOOL hb_arraySetNLL( PHB_ITEM pArray, HB_SIZE nIndex, HB_LONGLONG llNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetNLL(%p, %" HB_PFS "u, %" PFLL "d)", pArray, nIndex, llNumber ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutNLL( pArray->item.asArray.value->pItems + nIndex - 1, llNumber );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}
#endif

HB_BOOL hb_arraySetNInt( PHB_ITEM pArray, HB_SIZE nIndex, HB_MAXINT nNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetNInt(%p, %" HB_PFS "u, %" PFHL "d)", pArray, nIndex, nNumber ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutNInt( pArray->item.asArray.value->pItems + nIndex - 1, nNumber );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetND( PHB_ITEM pArray, HB_SIZE nIndex, double dNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetND(%p, %" HB_PFS "u, %lf)", pArray, nIndex, dNumber ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutND( pArray->item.asArray.value->pItems + nIndex - 1, dNumber );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetC( PHB_ITEM pArray, HB_SIZE nIndex, const char * szText )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetC(%p, %" HB_PFS "u, %p)", pArray, nIndex, szText ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutC( pArray->item.asArray.value->pItems + nIndex - 1, szText );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetCL( PHB_ITEM pArray, HB_SIZE nIndex, const char * szText, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetC(%p, %" HB_PFS "u, %p, %" HB_PFS "u)", pArray, nIndex, szText, nLen ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutCL( pArray->item.asArray.value->pItems + nIndex - 1, szText, nLen );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetCPtr( PHB_ITEM pArray, HB_SIZE nIndex, char * szText )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetCPtr(%p, %" HB_PFS "u, %p)", pArray, nIndex, szText ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutCPtr( pArray->item.asArray.value->pItems + nIndex - 1, szText );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetCLPtr( PHB_ITEM pArray, HB_SIZE nIndex, char * szText, HB_SIZE nLen )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetCLPtr(%p, %" HB_PFS "u, %p, %" HB_PFS "u)", pArray, nIndex, szText, nLen ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutCLPtr( pArray->item.asArray.value->pItems + nIndex - 1, szText, nLen );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetCConst( PHB_ITEM pArray, HB_SIZE nIndex, const char * szText )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetCConst(%p, %" HB_PFS "u, %p)", pArray, nIndex, szText ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutCConst( pArray->item.asArray.value->pItems + nIndex - 1, szText );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetPtr( PHB_ITEM pArray, HB_SIZE nIndex, void * pValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetPtr(%p, %" HB_PFS "u, %p)", pArray, nIndex, pValue ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutPtr( pArray->item.asArray.value->pItems + nIndex - 1, pValue );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetPtrGC( PHB_ITEM pArray, HB_SIZE nIndex, void * pValue )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetPtrGC(%p, %" HB_PFS "u, %p)", pArray, nIndex, pValue ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutPtrGC( pArray->item.asArray.value->pItems + nIndex - 1, pValue );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arraySetSymbol( PHB_ITEM pArray, HB_SIZE nIndex, PHB_SYMB pSymbol )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySetSymbol(%p, %" HB_PFS "u, %p)", pArray, nIndex, pSymbol ) );

   if( HB_IS_ARRAY( pArray ) && nIndex > 0 && nIndex <= pArray->item.asArray.value->nLen )
   {
      hb_itemPutSymbol( pArray->item.asArray.value->pItems + nIndex - 1, pSymbol );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayLast(%p, %p)", pArray, pResult ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      if( pArray->item.asArray.value->nLen > 0 )
         hb_itemCopy( pResult, pArray->item.asArray.value->pItems +
                             ( pArray->item.asArray.value->nLen - 1 ) );
      else
         hb_itemSetNil( pResult );

      return HB_TRUE;
   }

   hb_itemSetNil( pResult );

   return HB_FALSE;
}

HB_BOOL hb_arrayFill( PHB_ITEM pArray, PHB_ITEM pValue, HB_SIZE * pnStart, HB_SIZE * pnCount )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayFill(%p, %p, %p, %p)", pArray, pValue, pnStart, pnCount ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      HB_SIZE nLen = pBaseArray->nLen;
      HB_SIZE nStart;
      HB_SIZE nCount;

      if( pnStart && *pnStart )
         nStart = *pnStart - 1;
      else
         nStart = 0;

      if( nStart < nLen )
      {
         nCount = nLen - nStart;
         if( pnCount && *pnCount < nCount )
            nCount = *pnCount;

         if( nCount > 0 )
         {
            do
            {
               hb_itemCopy( pBaseArray->pItems + nStart++, pValue );
            }
            while( --nCount > 0 );
         }
      }

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_SIZE hb_arrayScan( PHB_ITEM pArray, PHB_ITEM pValue, HB_SIZE * pnStart, HB_SIZE * pnCount, HB_BOOL fExact )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayScan(%p, %p, %p, %p, %d)", pArray, pValue, pnStart, pnCount, ( int ) fExact ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      HB_SIZE nLen = pBaseArray->nLen;
      HB_SIZE nStart;
      HB_SIZE nCount;

      if( pnStart && *pnStart )
         nStart = *pnStart - 1;
      else
         nStart = 0;

      if( nStart < nLen )
      {
         nCount = nLen - nStart;
         if( pnCount && *pnCount < nCount )
            nCount = *pnCount;

         if( nCount > 0 )
         {
            /* Make separate search loops for different types to find, so that
               the loop can be faster. */

            if( HB_IS_BLOCK( pValue ) )
            {
               HB_STACK_TLS_PRELOAD
               do
               {
                  hb_vmPushEvalSym();
                  hb_vmPush( pValue );
                  hb_vmPush( pBaseArray->pItems + nStart );
                  hb_vmPushSize( ++nStart );
                  hb_vmEval( 2 );

                  if( HB_IS_LOGICAL( hb_stackReturnItem() ) && hb_stackReturnItem()->item.asLogical.value )
                     return nStart;
               }
               while( --nCount > 0 && nStart < pBaseArray->nLen );
            }
            else if( HB_IS_STRING( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart++;

                  /* NOTE: The order of the pItem and pValue parameters passed to
                           hb_itemStrCmp() is significant, please don't change it. [vszakats] */
                  if( HB_IS_STRING( pItem ) && hb_itemStrCmp( pItem, pValue, fExact ) == 0 )
                     return nStart;
               }
               while( --nCount > 0 );
            }
            else if( HB_IS_NUMERIC( pValue ) )
            {
               double dValue = hb_itemGetND( pValue );

               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart++;

                  if( HB_IS_NUMERIC( pItem ) && hb_itemGetND( pItem ) == dValue )
                     return nStart;
               }
               while( --nCount > 0 );
            }
            else if( HB_IS_DATETIME( pValue ) )
            {
               if( fExact )
               {
                  do
                  {
                     PHB_ITEM pItem = pBaseArray->pItems + nStart++;

                     if( HB_IS_DATETIME( pItem ) &&
                         pItem->item.asDateTime.julian == pValue->item.asDateTime.julian &&
                         pItem->item.asDateTime.time == pValue->item.asDateTime.time )
                        return nStart;
                  }
                  while( --nCount > 0 );
               }
               else
               {
                  do
                  {
                     PHB_ITEM pItem = pBaseArray->pItems + nStart++;

                     if( HB_IS_DATETIME( pItem ) &&
                         pItem->item.asDateTime.julian == pValue->item.asDateTime.julian )
                        return nStart;
                  }
                  while( --nCount > 0 );
               }
            }
            else if( HB_IS_LOGICAL( pValue ) )
            {
               HB_BOOL bValue = hb_itemGetL( pValue );

               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart++;

                  if( HB_IS_LOGICAL( pItem ) && hb_itemGetL( pItem ) == bValue )
                     return nStart;
               }
               while( --nCount > 0 );
            }
            else if( HB_IS_NIL( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart++;

                  if( HB_IS_NIL( pItem ) )
                     return nStart;
               }
               while( --nCount > 0 );
            }
            else if( HB_IS_POINTER( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart++;

                  if( HB_IS_POINTER( pItem ) &&
                      pItem->item.asPointer.value == pValue->item.asPointer.value )
                     return nStart;
               }
               while( --nCount > 0 );
            }
            else if( fExact && HB_IS_ARRAY( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart++;

                  if( HB_IS_ARRAY( pItem ) &&
                      pItem->item.asArray.value == pValue->item.asArray.value )
                     return nStart;
               }
               while( --nCount > 0 );
            }
            else if( fExact && HB_IS_HASH( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart++;

                  if( HB_IS_HASH( pItem ) &&
                      pItem->item.asHash.value == pValue->item.asHash.value )
                     return nStart;
               }
               while( --nCount > 0 );
            }
         }
      }
   }

   return 0;
}

HB_SIZE hb_arrayRevScan( PHB_ITEM pArray, PHB_ITEM pValue, HB_SIZE * pnStart, HB_SIZE * pnCount, HB_BOOL fExact )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayRevScan(%p, %p, %p, %p, %d)", pArray, pValue, pnStart, pnCount, ( int ) fExact ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      HB_SIZE nLen = pBaseArray->nLen;
      HB_SIZE nStart;
      HB_SIZE nCount;

      if( pnStart && *pnStart )
         nStart = *pnStart - 1;
      else
         nStart = nLen - 1;

      if( nStart < nLen )
      {
         nCount = nStart + 1;
         if( pnCount && *pnCount < nCount )
            nCount = *pnCount;

         if( nCount > 0 )
         {
            /* Make separate search loops for different types to find, so that
               the loop can be faster. */

            if( HB_IS_BLOCK( pValue ) )
            {
               HB_STACK_TLS_PRELOAD
               do
               {
                  hb_vmPushEvalSym();
                  hb_vmPush( pValue );
                  if( nStart < pBaseArray->nLen )
                     hb_vmPush( pBaseArray->pItems + nStart );
                  else
                     hb_vmPushNil();
                  hb_vmPushSize( nStart + 1 );
                  hb_vmEval( 2 );

                  if( HB_IS_LOGICAL( hb_stackReturnItem() ) && hb_stackReturnItem()->item.asLogical.value )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( HB_IS_STRING( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart;

                  /* NOTE: The order of the pItem and pValue parameters passed to
                           hb_itemStrCmp() is significant, please don't change it. [vszakats] */
                  if( HB_IS_STRING( pItem ) && hb_itemStrCmp( pItem, pValue, fExact ) == 0 )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( HB_IS_NUMERIC( pValue ) )
            {
               double dValue = hb_itemGetND( pValue );

               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart;

                  if( HB_IS_NUMERIC( pItem ) && hb_itemGetND( pItem ) == dValue )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( HB_IS_DATETIME( pValue ) )
            {
               if( fExact )
               {
                  do
                  {
                     PHB_ITEM pItem = pBaseArray->pItems + nStart;

                     if( HB_IS_DATETIME( pItem ) &&
                         pItem->item.asDateTime.julian == pValue->item.asDateTime.julian &&
                         pItem->item.asDateTime.time == pValue->item.asDateTime.time )
                        return nStart + 1;
                  }
                  while( --nCount && nStart-- );
               }
               else
               {
                  do
                  {
                     PHB_ITEM pItem = pBaseArray->pItems + nStart;

                     if( HB_IS_DATETIME( pItem ) &&
                         pItem->item.asDateTime.julian == pValue->item.asDateTime.julian )
                        return nStart + 1;
                  }
                  while( --nCount && nStart-- );
               }
            }
            else if( HB_IS_LOGICAL( pValue ) )
            {
               HB_BOOL bValue = hb_itemGetL( pValue );

               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart;

                  if( HB_IS_LOGICAL( pItem ) && hb_itemGetL( pItem ) == bValue )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( HB_IS_NIL( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart;

                  if( HB_IS_NIL( pItem ) )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( HB_IS_POINTER( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart;

                  if( HB_IS_POINTER( pItem ) &&
                      pItem->item.asPointer.value == pValue->item.asPointer.value )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( fExact && HB_IS_ARRAY( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart;

                  if( HB_IS_ARRAY( pItem ) &&
                      pItem->item.asArray.value == pValue->item.asArray.value )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
            else if( fExact && HB_IS_HASH( pValue ) )
            {
               do
               {
                  PHB_ITEM pItem = pBaseArray->pItems + nStart;

                  if( HB_IS_HASH( pItem ) &&
                      pItem->item.asHash.value == pValue->item.asHash.value )
                     return nStart + 1;
               }
               while( --nCount && nStart-- );
            }
         }
      }
   }

   return 0;
}

HB_BOOL hb_arrayEval( PHB_ITEM pArray, PHB_ITEM bBlock, HB_SIZE * pnStart, HB_SIZE * pnCount )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayEval(%p, %p, %p, %p)", pArray, bBlock, pnStart, pnCount ) );

   if( HB_IS_ARRAY( pArray ) && HB_IS_BLOCK( bBlock ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      HB_SIZE nLen = pBaseArray->nLen;
      HB_SIZE nStart;
      HB_SIZE nCount;

      if( pnStart && *pnStart )
         nStart = *pnStart - 1;
      else
         nStart = 0;

      if( nStart < nLen )
      {
         nCount = nLen - nStart;
         if( pnCount && *pnCount < nCount )
            nCount = *pnCount;

         if( nCount > 0 )
         {
            do
            {
               hb_vmPushEvalSym();
               hb_vmPush( bBlock );
               hb_vmPush( pBaseArray->pItems + nStart );
               hb_vmPushSize( nStart + 1 );
               hb_vmEval( 2 );
            }
            while( --nCount > 0 && ++nStart < pBaseArray->nLen );
            /*
             * checking for nStart < pBaseArray->nLen is fix for
             * possible GPF when codeblock decrease array size
             */
         }
      }

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

/* NOTE: CA-Cl*pper 5.3a has a fix for the case when the starting position
         is greater than the length of the array. [vszakats] */

HB_BOOL hb_arrayCopy( PHB_ITEM pSrcArray, PHB_ITEM pDstArray, HB_SIZE * pnStart,
                      HB_SIZE * pnCount, HB_SIZE * pnTarget )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayCopy(%p, %p, %p, %p, %p)", pSrcArray, pDstArray, pnStart, pnCount, pnTarget ) );

   if( HB_IS_ARRAY( pSrcArray ) && HB_IS_ARRAY( pDstArray ) )
   {
      PHB_BASEARRAY pSrcBaseArray = pSrcArray->item.asArray.value;
      PHB_BASEARRAY pDstBaseArray = pDstArray->item.asArray.value;
      HB_SIZE nSrcLen = pSrcBaseArray->nLen;
      HB_SIZE nDstLen = pDstBaseArray->nLen;
      HB_SIZE nStart;
      HB_SIZE nCount;
      HB_SIZE nTarget;

      if( pnStart && ( *pnStart >= 1 ) )
         nStart = *pnStart;
      else
         nStart = 1;

      if( pnTarget && ( *pnTarget >= 1 ) )
         nTarget = *pnTarget;
      else
         nTarget = 1;

#ifdef HB_COMPAT_C53 /* From CA-Cl*pper 5.3a */
      if( nStart <= nSrcLen )
#else
      if( nSrcLen > 0 )
#endif
      {
#ifndef HB_COMPAT_C53 /* From CA-Cl*pper 5.3a */
         if( nStart > nSrcLen )
            nStart = nSrcLen;
#endif
         if( pnCount && ( *pnCount <= nSrcLen - nStart ) )
            nCount = *pnCount;
         else
            nCount = nSrcLen - nStart + 1;

/* This is probably a bug, present in all versions of CA-Cl*pper. */
#if defined( HB_CLP_STRICT ) || 1
         if( nDstLen > 0 )
         {
            if( nTarget > nDstLen )
               nTarget = nDstLen;
#else
         if( nTarget <= nDstLen )
         {
#endif

            if( nCount > nDstLen - nTarget )
               nCount = nDstLen - nTarget + 1;

            for( nTarget--, nStart--; nCount > 0; nCount--, nStart++, nTarget++ )
               hb_itemCopy( pDstBaseArray->pItems + nTarget, pSrcBaseArray->pItems + nStart );
         }
      }

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

static void hb_arrayCloneBody( PHB_BASEARRAY pSrcBaseArray, PHB_BASEARRAY pDstBaseArray, PHB_NESTED_CLONED pClonedList )
{
   PHB_ITEM pSrcItem, pDstItem;
   HB_SIZE nLen;

   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayCloneBody(%p, %p, %p)", pSrcBaseArray, pDstBaseArray, pClonedList ) );

   pSrcItem = pSrcBaseArray->pItems;
   pDstItem = pDstBaseArray->pItems;

   pDstBaseArray->uiClass = pSrcBaseArray->uiClass;

   for( nLen = pSrcBaseArray->nLen; nLen; --nLen, ++pSrcItem, ++pDstItem )
      hb_cloneNested( pDstItem, pSrcItem, pClonedList );
}

void hb_cloneNested( PHB_ITEM pDstItem, PHB_ITEM pSrcItem, PHB_NESTED_CLONED pClonedList )
{
   /* Clipper clones nested array ONLY if NOT an Object!!! */
   if( HB_IS_ARRAY( pSrcItem ) )
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
      else if( pSrcItem->item.asArray.value->uiClass != 0 )
         hb_objCloneTo( pDstItem, pSrcItem, pClonedList );
      else
      {
         hb_arrayNew( pDstItem, pBaseArray->nLen );

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

PHB_ITEM hb_arrayCloneTo( PHB_ITEM pDstArray, PHB_ITEM pSrcArray )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayCloneTo(%p,%p)", pDstArray, pSrcArray ) );

   if( HB_IS_ARRAY( pSrcArray ) )
   {
      PHB_NESTED_CLONED pClonedList, pCloned;
      PHB_BASEARRAY pSrcBaseArray = pSrcArray->item.asArray.value;
      HB_SIZE nSrcLen = pSrcBaseArray->nLen;

      hb_arrayNew( pDstArray, nSrcLen );
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

PHB_ITEM hb_arrayClone( PHB_ITEM pArray )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayClone(%p)", pArray ) );

   return hb_arrayCloneTo( hb_itemNew( NULL ), pArray );
}

PHB_ITEM hb_arrayFromStack( HB_USHORT uiLen )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pArray = hb_itemNew( NULL );
   HB_USHORT uiPos;

   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayFromStack(%hu)", uiLen ) );

   hb_arrayNew( pArray, uiLen );

   for( uiPos = 1; uiPos <= uiLen; uiPos++ )
      hb_arraySet( pArray, uiPos, hb_stackItemFromTop( uiPos - uiLen - 1 ) );

   return pArray;
}

PHB_ITEM hb_arrayFromParams( int iLevel )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pArray;
   HB_USHORT uiPos, uiPCount;
   HB_ISIZ nBaseOffset;

   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayFromParams(%d)", iLevel ) );

   nBaseOffset = hb_stackBaseProcOffset( iLevel );
   if( nBaseOffset > 0 )
      uiPCount = hb_stackItem( nBaseOffset )->item.asSymbol.paramcnt;
   else
      uiPCount = 0;

   pArray = hb_itemArrayNew( uiPCount );
   for( uiPos = 1; uiPos <= uiPCount; uiPos++ )
      hb_arraySet( pArray, uiPos, hb_stackItem( nBaseOffset + uiPos + 1 ) );

   return pArray;
}

PHB_ITEM hb_arrayBaseParams( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pArray;
   HB_USHORT uiPos, uiPCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayBaseParams()" ) );

   pArray = hb_itemNew( NULL );
   uiPCount = hb_stackBaseItem()->item.asSymbol.paramcnt;

   hb_arrayNew( pArray, uiPCount );

   for( uiPos = 1; uiPos <= uiPCount; uiPos++ )
      hb_arraySet( pArray, uiPos, hb_stackItemFromBase( uiPos ) );

   return pArray;
}

PHB_ITEM hb_arraySelfParams( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pArray;
   HB_USHORT uiPos, uiPCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySelfParams()" ) );

   pArray = hb_itemNew( NULL );
   uiPCount = hb_stackBaseItem()->item.asSymbol.paramcnt;

   hb_arrayNew( pArray, uiPCount + 1 );

   for( uiPos = 0; uiPos <= uiPCount; uiPos++ )
      hb_arraySet( pArray, uiPos + 1, hb_stackItemFromBase( uiPos ) );

   return pArray;
}
