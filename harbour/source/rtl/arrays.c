/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Array API
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 *    hb_arrayIsObject()
 *    hb_arrayError()
 *    hb_arrayCopyC()
 *    hb_arrayGetC()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "extend.h"
#include "itemapi.h"
#include "errorapi.h"
#include "langapi.h"
#include "ctoharb.h"

/*
 * Internal
 */

BOOL hb_arrayNew( PHB_ITEM pItem, ULONG ulLen ) /* creates a new array */
{
   PHB_BASEARRAY pBaseArray = ( PHB_BASEARRAY ) hb_xgrab( sizeof( HB_BASEARRAY ) );
   ULONG ulPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayNew(%p, %lu)", pItem, ulLen));

   hb_itemClear( pItem );

   pItem->type = IT_ARRAY;

   if( ulLen > 0 )
      pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) * ulLen );
   else
      pBaseArray->pItems = NULL;

   pBaseArray->ulLen      = ulLen;
   pBaseArray->uiHolders  = 1;
   pBaseArray->uiClass    = 0;
   pBaseArray->uiPrevCls  = 0;

   for( ulPos = 0; ulPos < ulLen; ulPos++ )
      ( pBaseArray->pItems + ulPos )->type = IT_NIL;

   pItem->item.asArray.value = pBaseArray;

   return TRUE;
}

BOOL hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayAdd(%p, %p)", pArray, pValue));

   if( IS_ARRAY( pArray ) )
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

ULONG hb_arrayLen( PHB_ITEM pArray )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayLen(%p)", pArray));

   if( IS_ARRAY( pArray ) )
      return pArray->item.asArray.value->ulLen;
   else
      return 0;
}

BOOL hb_arrayIsObject( PHB_ITEM pArray )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayIsObject(%p)", pArray));

   if( IS_ARRAY( pArray ) )
      return pArray->item.asArray.value->uiClass != 0;
   else
      return FALSE;
}

BOOL hb_arraySize( PHB_ITEM pArray, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySize(%p, %lu)", pArray, ulLen));

   if( IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG ulPos;

      if( ! pBaseArray->ulLen )
      {
         pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( ulLen * sizeof( HB_ITEM ) );

         for( ulPos = 0; ulPos < ulLen; ulPos++ )
            ( pBaseArray->pItems + ulPos )->type = IT_NIL;
      }
      else
      {
         if( pBaseArray->ulLen < ulLen )
         {
            pBaseArray->pItems = ( PHB_ITEM ) hb_xrealloc( pBaseArray->pItems, sizeof( HB_ITEM ) * ulLen );

            /* set value for new items */
            for( ulPos = pBaseArray->ulLen; ulPos < ulLen; ulPos++ )
               ( pBaseArray->pItems + ulPos )->type = IT_NIL;
         }
         else if( pBaseArray->ulLen > ulLen )
         {
            /* release old items */
            for( ulPos = ulLen; ulPos < pBaseArray->ulLen; ulPos++ )
               hb_itemClear( pBaseArray->pItems + ulPos );

            if( ulLen == 0 )
            {
               hb_xfree( pBaseArray->pItems );
               pBaseArray->pItems = NULL;
            }
            else
               pBaseArray->pItems = ( PHB_ITEM ) hb_xrealloc( pBaseArray->pItems, sizeof( HB_ITEM ) * ulLen );
         }
      }

      pBaseArray->ulLen = ulLen;

      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arrayDel( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayDel(%p, %lu)", pArray, ulIndex));

   if( IS_ARRAY( pArray ) )
   {
      ULONG ulLen = pArray->item.asArray.value->ulLen;

      if( ulIndex > 0 && ulIndex <= ulLen )
      {
         PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;

         hb_itemClear( pBaseArray->pItems + ( ulIndex - 1 ) );

         for( ulIndex--; ulIndex < ulLen; ulIndex++ )       /* move items */
            hb_itemCopy( pBaseArray->pItems + ulIndex, pBaseArray->pItems + ( ulIndex + 1 ) );

         hb_itemClear( pBaseArray->pItems + ( ulLen - 1 ) );
      }

      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arrayIns( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayIns(%p, %lu)", pArray, ulIndex));

   if( IS_ARRAY( pArray ) )
   {
      ULONG ulLen = pArray->item.asArray.value->ulLen;

      if( ulIndex > 0 && ulIndex <= ulLen )
      {
         PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;

         hb_itemClear( pBaseArray->pItems + ( ulLen - 1 ) );

         for( ulLen--; ulLen >= ulIndex; ulLen-- )          /* move items */
            hb_itemCopy( pBaseArray->pItems + ulLen, pBaseArray->pItems + ( ulLen - 1 ) );

         hb_itemClear( pBaseArray->pItems + ulLen );
      }

      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arrayError( PHB_ITEM pArray, ULONG ulIndex, BOOL bAssign )
{
   BOOL bError;

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayError(%p, %lu, %d)", pArray, ulIndex, (int) bAssign));

   if( IS_ARRAY( pArray ) )
   {
      if( ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
         bError = FALSE;
      else
      {
         bError = TRUE;
         if( bAssign )
            hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ) );
         else
            hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ) );
      }
   }
   else
   {
      bError = TRUE;
      if( bAssign )
         hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ) );
      else
         hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ) );
   }

   return bError;
}

BOOL hb_arraySet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayError(%p, %lu, %p)", pArray, ulIndex, pItem));

   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemCopy( pArray->item.asArray.value->pItems + ( ulIndex - 1 ), pItem );
      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arrayGet( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGet(%p, %lu, %p)", pArray, ulIndex, pItem));

   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemCopy( pItem, pArray->item.asArray.value->pItems + ( ulIndex - 1 ) );
      return TRUE;
   }
   else
   {
      hb_itemClear( pItem );
      return FALSE;
   }
}

char * hb_arrayGetDS( PHB_ITEM pArray, ULONG ulIndex, char * szDate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetDS(%p, %lu, %s)", pArray, ulIndex, szDate));

   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      hb_itemGetDS( pArray->item.asArray.value->pItems + ulIndex - 1, szDate );
   else
      /* NOTE: Intentionally calling it with a bad parameter in order to get
               the default value from hb_itemGetDS(). */
      hb_itemGetDS( NULL, szDate );

   return szDate;
}

BOOL hb_arrayGetBool( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetBool(%p, %lu)", pArray, ulIndex));

   if( IS_ARRAY( pArray ) )
   {
      if( ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
         return hb_itemGetL( pArray->item.asArray.value->pItems + ulIndex - 1 );
   }

   return FALSE;
}

/*
 * This function returns a pointer to an item occupied by the specified
 * array element - it doesn't return an item's value
 */
PHB_ITEM hb_arrayGetItemPtr( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetItemPtr(%p, %lu)", pArray, ulIndex));

   if( IS_ARRAY( pArray ) )
   {
      if( ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
         return pArray->item.asArray.value->pItems + ( ulIndex - 1 );
   }

   return NULL;
}

BOOL hb_arrayGetL( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetL(%p, %lu)", pArray, ulIndex));

   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetL( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return FALSE;
}

int hb_arrayGetNI( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNI(%p, %lu)", pArray, ulIndex));

   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetNI( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

long hb_arrayGetNL( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetNL(%p, %lu)", pArray, ulIndex));

   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetNL( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

double hb_arrayGetND( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetND(%p, %lu)", pArray, ulIndex));

   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetND( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

ULONG hb_arrayCopyC( PHB_ITEM pArray, ULONG ulIndex, char * szBuffer, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayCopyC(%p, %lu, %s, %lu)", pArray, ulIndex, szBuffer, ulLen));

   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemCopyC( pArray->item.asArray.value->pItems + ulIndex - 1, szBuffer, ulLen );
   else
      return 0;
}

char * hb_arrayGetC( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetC(%p, %lu)", pArray, ulIndex));

   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetC( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return NULL;
}

char * hb_arrayGetCPtr( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetCPtr(%p, %lu)", pArray, ulIndex));

   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetCPtr( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return "";
}

ULONG hb_arrayGetCLen( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetCLen(%p, %lu)", pArray, ulIndex));

   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetCLen( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

USHORT hb_arrayGetType( PHB_ITEM pArray, ULONG ulIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayGetType(%p, %lu)", pArray, ulIndex));

   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemType( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

BOOL hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayLast(%p, %p)", pArray, pResult));

   if( IS_ARRAY( pArray ) )
   {
      if( pArray->item.asArray.value->ulLen > 0 )
         hb_itemCopy( pResult, pArray->item.asArray.value->pItems +
                             ( pArray->item.asArray.value->ulLen - 1 ) );
      else
         hb_itemClear( pResult );

      return TRUE;
   }

   hb_itemClear( pResult );

   return FALSE;
}

BOOL hb_arrayFill( PHB_ITEM pArray, PHB_ITEM pValue, ULONG * pulStart, ULONG * pulCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayFill(%p, %p, %p, %p)", pArray, pValue, pulStart, pulCount));

   if( IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG ulLen = pBaseArray->ulLen;
      ULONG ulStart;
      ULONG ulCount;

      if( pulStart && ( *pulStart >= 1 ) )
         ulStart = *pulStart;
      else
         ulStart = 1;

      if( ulStart <= ulLen )
      {
         if( pulCount && ( *pulCount <= ulLen - ulStart ) )
            ulCount = *pulCount;
         else
            ulCount = ulLen - ulStart + 1;

         if( ulStart + ulCount > ulLen )             /* check range */
            ulCount = ulLen - ulStart + 1;

         for( ; ulCount > 0; ulCount--, ulStart++ )     /* set value items */
            hb_itemCopy( pBaseArray->pItems + ( ulStart - 1 ), pValue );
      }

      return TRUE;
   }
   else
      return FALSE;
}

ULONG hb_arrayScan( PHB_ITEM pArray, PHB_ITEM pValue, ULONG * pulStart, ULONG * pulCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayScan(%p, %p, %p, %p)", pArray, pValue, pulStart, pulCount));

   if( IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG ulLen = pBaseArray->ulLen;
      ULONG ulStart;
      ULONG ulCount;

      if( pulStart && ( *pulStart >= 1 ) )
         ulStart = *pulStart;
      else
         ulStart = 1;

      if( ulStart <= ulLen )
      {
         if( pulCount && ( *pulCount <= ulLen - ulStart ) )
            ulCount = *pulCount;
         else
            ulCount = ulLen - ulStart + 1;

         if( ulStart + ulCount > ulLen )             /* check range */
            ulCount = ulLen - ulStart + 1;

         /* Make separate search loops for different types to find, so that
            the loop can be faster. */

         if( IS_BLOCK( pValue ) )
         {
            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               hb_vmPushSymbol( &hb_symEval );
               hb_vmPush( pValue );
               hb_vmPush( pBaseArray->pItems + ulStart );
               hb_vmDo( 1 );

               if( IS_LOGICAL( &hb_stack.Return ) && hb_stack.Return.item.asLogical.value )
                  return ulStart + 1;                  /* arrays start from 1 */
            }
         }
         else if( IS_STRING( pValue ) )
         {
            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               PHB_ITEM pItem = pBaseArray->pItems + ulStart;

               /* NOTE: The order of the pItem and pValue parameters passed to
                        hb_itemStrCmp() is significant, please don't change it. */
               if( IS_STRING( pItem ) && hb_itemStrCmp( pItem, pValue, FALSE ) == 0 )
                  return ulStart + 1;
            }
         }
         else if( IS_NUMERIC( pValue ) )
         {
            double dValue = hb_itemGetND( pValue );

            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               PHB_ITEM pItem = pBaseArray->pItems + ulStart;

               if( IS_NUMERIC( pItem ) && hb_itemGetND( pItem ) == dValue )
                  return ulStart + 1;
            }
         }
         else if( IS_DATE( pValue ) )
         {
            /* NOTE: This is correct: Get the date as a long value. */
            LONG lValue = hb_itemGetNL( pValue );

            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               PHB_ITEM pItem = pBaseArray->pItems + ulStart;

               if( IS_DATE( pItem ) && hb_itemGetNL( pItem ) == lValue )
                  return ulStart + 1;
            }
         }
         else if( IS_LOGICAL( pValue ) )
         {
            BOOL bValue = hb_itemGetL( pValue ); /* NOTE: This is correct: Get the date as a long value. */

            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               PHB_ITEM pItem = pBaseArray->pItems + ulStart;

               if( IS_LOGICAL( pItem ) && hb_itemGetL( pItem ) == bValue )
                  return ulStart + 1;
            }
         }
         else if( IS_NIL( pValue ) )
         {
            for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
            {
               if( IS_NIL( pBaseArray->pItems + ulStart ) )
                  return ulStart + 1;
            }
         }
      }
   }

   return 0;
}

BOOL hb_arrayEval( PHB_ITEM pArray, PHB_ITEM bBlock, ULONG * pulStart, ULONG * pulCount )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayEval(%p, %p, %p, %p)", pArray, bBlock, pulStart, pulCount));

   if( IS_ARRAY( pArray ) && IS_BLOCK( bBlock ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG ulLen = pBaseArray->ulLen;
      ULONG ulStart;
      ULONG ulCount;

      if( pulStart && ( *pulStart >= 1 ) )
         ulStart = *pulStart;
      else
         ulStart = 1;

      if( ulStart <= ulLen )
      {
         if( pulCount && ( *pulCount <= ulLen - ulStart ) )
            ulCount = *pulCount;
         else
            ulCount = ulLen - ulStart + 1;

         if( ulStart + ulCount > ulLen )             /* check range */
            ulCount = ulLen - ulStart + 1;

         for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
         {
            PHB_ITEM pItem = pBaseArray->pItems + ulStart;

            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( bBlock );
            hb_vmPush( pItem );
            hb_vmPushNumber( ( double ) ( ulStart + 1 ), 0 );
            hb_vmDo( 2 );
         }
      }

      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arrayRelease( PHB_ITEM pArray )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayRelease(%p)", pArray));

   if( IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG ulLen = pBaseArray->ulLen;
      ULONG ulPos;

      for( ulPos = 0; ulPos < ulLen; ulPos++ )
         hb_itemClear( pBaseArray->pItems + ulPos );

      if( pBaseArray->pItems )
         hb_xfree( pBaseArray->pItems );

      hb_xfree( pBaseArray );

      pArray->type = IT_NIL;
      pArray->item.asArray.value = NULL;

      return TRUE;
   }
   else
      return FALSE;
}

/* NOTE: CA-Cl*pper 5.3a has a fix for the case when the starting position
         is greater than the length of the array. */

BOOL hb_arrayCopy( PHB_ITEM pSrcArray, PHB_ITEM pDstArray, ULONG * pulStart,
                   ULONG * pulCount, ULONG * pulTarget )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arrayCopy(%p, %p, %p, %p, %p)", pSrcArray, pDstArray, pulStart, pulCount, pulTarget));

   if( IS_ARRAY( pSrcArray ) && IS_ARRAY( pDstArray ) )
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
#ifdef HB_FIX_ACOPY_BUG
         if( ulTarget <= ulDstLen )
         {
#else
         if( ulDstLen > 0 )
         {
            if( ulTarget > ulDstLen )
               ulTarget = ulDstLen;
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

PHB_ITEM hb_arrayClone( PHB_ITEM pSrcArray )
{
   PHB_ITEM pDstArray = hb_itemNew( NULL );

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayClone(%p)", pSrcArray));

   if( IS_ARRAY( pSrcArray ) )
   {
      PHB_BASEARRAY pSrcBaseArray = pSrcArray->item.asArray.value;
      PHB_BASEARRAY pDstBaseArray;
      ULONG ulSrcLen = pSrcBaseArray->ulLen;
      ULONG ulCount;

      hb_arrayNew( pDstArray, ulSrcLen );

      pDstBaseArray = pDstArray->item.asArray.value;
      pDstBaseArray->uiClass = pSrcBaseArray->uiClass;

      for( ulCount = 0; ulCount < ulSrcLen; ulCount++ )
      {
         PHB_ITEM pSrcItem = pSrcBaseArray->pItems + ulCount;

         if( pSrcItem->type == IT_ARRAY )
         {
            PHB_ITEM pClone = hb_arrayClone( pSrcItem );

            hb_itemArrayPut( pDstArray, ulCount + 1, pClone );
            hb_itemRelease( pClone );
         }
         else
            hb_itemArrayPut( pDstArray, ulCount + 1, pSrcItem );
      }
   }

   return pDstArray;
}

/*
 * HARBOUR
 */

/* This function creates an array item using 'iDimension' as an index
 * to retrieve the number of elements from the parameter list.
 */
static void hb_arrayNewRagged( PHB_ITEM pArray, int iDimension )
{
   ULONG ulElements;

   HB_TRACE(HB_TR_DEBUG, ("hb_arrayNewRagged(%p, %d)", pArray, iDimension));

   ulElements = ( ULONG ) hb_parnl( iDimension );

   /* create an array */
   hb_arrayNew( pArray, ulElements );

   if( ++iDimension <= hb_pcount() )
   {
      /* call self recursively to create next dimensions
       */
      while( ulElements )
         hb_arrayNewRagged( hb_arrayGetItemPtr( pArray, ulElements-- ), iDimension );
   }
}

HARBOUR HB_ARRAY( void )
{
   int iPCount = hb_pcount();

   if( iPCount > 0 )
   {
      BOOL bError = FALSE;
      int iParam;

      for( iParam = 1; iParam <= iPCount; iParam++ )
      {
         if( ! ISNUM( iParam ) )
         {
            bError = TRUE;
            break;
         }

         if( hb_parnl( iParam ) < 0 ) /* || hb_parnl( iParam ) <= 4096 */
         {
            hb_errRT_BASE( EG_BOUND, 1131, NULL, hb_langDGetErrorDesc( EG_ARRDIMENSION ) );
            bError = TRUE;
            break;
         }
      }

      if( ! bError )
         hb_arrayNewRagged( &hb_stack.Return, 1 );
   }
}

HARBOUR HB_AADD( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

   if( pArray )
   {
      PHB_ITEM pValue = hb_param( 2, IT_ANY );

      if( pValue && hb_arrayAdd( pArray, pValue ) )
         hb_itemReturn( pValue );
      else
         hb_errRT_BASE( EG_BOUND, 1187, NULL, "AADD" );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1123, NULL, "AADD" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

/* NOTE: CA-Cl*pper 5.3 and older will return NIL on bad parameter, 5.3a,b
         will throw a runtime error. */

HARBOUR HB_ASIZE( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

   if( pArray && ISNUM( 2 ) )
   {
      LONG lSize = hb_parnl( 2 );

      hb_arraySize( pArray, HB_MAX_( lSize, 0 ) );

      hb_itemReturn( pArray ); /* ASize() returns the array itself */
   }
#ifdef HB_COMPAT_C53 /* From CA-Cl*pper 5.3a */
   else
      hb_errRT_BASE( EG_ARG, 2023, NULL, "ASIZE" );
#endif
}

HARBOUR HB_ATAIL( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

   if( pArray )
      hb_arrayLast( pArray, &hb_stack.Return );
}

HARBOUR HB_AINS( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

   if( pArray )
   {
      if( ISNUM( 2 ) )
         hb_arrayIns( pArray, hb_parnl( 2 ) );

      hb_itemReturn( pArray ); /* AIns() returns the array itself */
   }
}

HARBOUR HB_ADEL( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

   if( pArray )
   {
      if( ISNUM( 2 ) )
         hb_arrayDel( pArray, hb_parnl( 2 ) );

      hb_itemReturn( pArray ); /* ADel() returns the array itself */
   }
}

HARBOUR HB_AFILL( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

   if( pArray )
   {
      PHB_ITEM pValue = hb_param( 2, IT_ANY );

      if( pValue )
      {
         ULONG ulStart = hb_parnl( 3 );
         ULONG ulCount = hb_parnl( 4 );

         hb_arrayFill( pArray,
                       pValue,
                       ISNUM( 3 ) ? &ulStart : NULL,
                       ISNUM( 4 ) ? &ulCount : NULL );
      }

      hb_itemReturn( pArray ); /* AFill() returns the array itself */
   }
}

HARBOUR HB_ASCAN( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );
   PHB_ITEM pValue = hb_param( 2, IT_ANY );

   if( pArray && pValue )
   {
      ULONG ulStart = hb_parnl( 3 );
      ULONG ulCount = hb_parnl( 4 );

      hb_retnl( hb_arrayScan( pArray,
                              pValue,
                              ISNUM( 3 ) ? &ulStart : NULL,
                              ISNUM( 4 ) ? &ulCount : NULL ) );
   }
   else
      hb_retnl( 0 );
}

HARBOUR HB_AEVAL( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );
   PHB_ITEM pBlock = hb_param( 2, IT_BLOCK );

   if( pArray && pBlock )
   {
      ULONG ulStart = hb_parnl( 3 );
      ULONG ulCount = hb_parnl( 4 );

      hb_arrayEval( pArray,
                    pBlock,
                    ISNUM( 3 ) ? &ulStart : NULL,
                    ISNUM( 4 ) ? &ulCount : NULL );

      hb_itemReturn( pArray ); /* AEval() returns the array itself */
   }
   else
      hb_errRT_BASE( EG_ARG, 2017, NULL, "AEVAL" );
}

HARBOUR HB_ACOPY( void )
{
   PHB_ITEM pSrcArray = hb_param( 1, IT_ARRAY );
   PHB_ITEM pDstArray = hb_param( 2, IT_ARRAY );

   if( pSrcArray && pDstArray )
   {
      /* CA-Cl*pper works this way. */
      if( ! hb_arrayIsObject( pSrcArray ) && ! hb_arrayIsObject( pDstArray ) )
      {
         ULONG ulStart = hb_parnl( 3 );
         ULONG ulCount = hb_parnl( 4 );
         ULONG ulTarget = hb_parnl( 5 );

         hb_arrayCopy( pSrcArray,
                       pDstArray,
                       ISNUM( 3 ) ? &ulStart : NULL,
                       ISNUM( 4 ) ? &ulCount : NULL,
                       ISNUM( 5 ) ? &ulTarget : NULL );
      }

      hb_itemReturn( pDstArray ); /* ACopy() returns the target array */
   }
}

/* NOTE: Clipper will return NIL if the parameter is not an array */

HARBOUR HB_ACLONE( void )
{
   PHB_ITEM pSrcArray = hb_param( 1, IT_ARRAY );

   if( pSrcArray && ! hb_arrayIsObject( pSrcArray ) )
   {
      PHB_ITEM pDstArray = hb_arrayClone( pSrcArray );
      hb_itemReturn( pDstArray ); /* AClone() returns the new array */
      hb_itemRelease( pDstArray );
   }
}

