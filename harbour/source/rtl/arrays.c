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
               the default value from hb_itemGetDS(). [vszel] */
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
                        hb_itemStrCmp() is significant, please don't change it. [vszel] */
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
            /* NOTE: This is correct: Get the date as a long value. [vszel] */
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
            BOOL bValue = hb_itemGetL( pValue ); /* NOTE: This is correct: Get the date as a long value. [vszel] */

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
         is greater than the length of the array. [vszel] */

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
/*  $DOC$
 *  $FUNCNAME$
 *     ARRAY()
 *  $CATEGORY$
 *     ARRAY
 *  $ONELINER$
 *      Create an uninitialized array of specified length
 *  $SYNTAX$
 *     ARRAY(<nElements> [, <nElements>...]) --> aArray
 *  $ARGUMENTS$
     <nElements> is the number of elements in the specified dimension.
     The maximum number of elements in a dimension is 4096.  Arrays in
     HARBOUR can have an unlimited number of dimensions.
 *     
 *  $RETURNS$
 *   ARRAY() returns an array of specified dimensions.
 *  $DESCRIPTION$
     ARRAY() is an array function that returns an uninitialized array with
     the specified number of elements and dimensions.  If more than one
     <nElements> argument is specified, a multidimensional array is created
     with the number of dimensions equal to the number of <nElements>
     arguments specified.  Any <nElements> that is itself an array creates a
     nested array.

     In HARBOUR, there are several ways to create an array.  You can
     declare an array using a declaration statement such as LOCAL or STATIC;
     you can create an array using a PRIVATE or PUBLIC statement; you can
     assign a literal array to an existing variable; or you can use the
     ARRAY() function.  ARRAY() has the advantage that it can create arrays
     within expressions or code blocks.
 *     
 *  $EXAMPLES$
 *
       This example creates a one-dimensional array of five elements
        using the ARRAY() function, and then shows the equivalent action by
        assigning a literal array of NIL values:

        aArray := ARRAY(5)
        aArray := { NIL, NIL, NIL, NIL, NIL }

      This example shows three different statements which create the
        same multidimensional array:

        aArray := ARRAY(3, 2)
        aArray := { {NIL, NIL}, {NIL, NIL}, {NIL, NIL} }
        aArray := { ARRAY(2), ARRAY(2), ARRAY(2) }

      This example creates a nested, multidimensional array:

        aArray := ARRAY(3, {NIL,NIL})

 *  $SEEALSO$
 *     AADD(),ADEL(),AFILL(),AINS()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *  AADD()   
 *  $CATEGORY$
 *     ARRAY
 *  $ONELINER$
 *     Add a new element to the end of an array  
 *  $SYNTAX$
 *     AADD(<aTarget>, <expValue>) --> Value
 *  $ARGUMENTS$
 *
 *    <aTarget> is the array to add a new element to.
 *
 *    <expValue> is the value assigned to the new element.
 *
 *  $RETURNS$
 *
 *    AADD() evaluates <expValue> and returns its value.  If <expValue> is not
 *    specified, AADD() returns NIL.
 *     
 *  $DESCRIPTION$
     AADD() is an array function that increases the actual length of the
     target array by one.  The newly created array element is assigned the
     value specified by <expValue>.

     AADD() is used to dynamically grow an array.  It is useful for building
     dynamic lists or queues.  A good example of this is the GetList array
     used by the GET system to hold Get objects.  After a READ or CLEAR GETS,
     GetList becomes an empty array.  Each time you execute an @...GET
     command, the GET system uses AADD() to add a new element to the end of
     the GetList array, and then assigns a new Get object to the new element.

     AADD() is similar to ASIZE() but only adds one element at a time;
     ASIZE() can grow or shrink an array to a specified size.  AADD(),
     however, has the advantage that it can assign a value to the new
     element, while ASIZE() cannot.  AADD() may also seem similar to AINS(),
     but they are different:  AINS() moves elements within an array, but it
     does not change the array's length.

     Note:  If <expValue> is another array, the new element in the target
     array will contain a reference to the array specified by <expValue>.
 *     
 *  $EXAMPLES$
        These examples demonstrate the effects of multiple invocations
        of AADD() to an array:

        aArray := {}               // Result: aArray is an empty array
        AADD(aArray, 5)            // Result: aArray is { 5 }
        AADD(aArray, 10)           // Result: aArray is { 5, 10 }
        AADD(aArray, { 12, 10 })   // Result: aArray is
                                   // { 5, 10, { 12, 10 } }
 *     
 *  $SEEALSO$
 *     AINS(),ASIZE()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
         will throw a runtime error. [vszel] */
/*  $DOC$
 *  $FUNCNAME$
 *     ASIZE()
 *  $CATEGORY$
 *     ARRAY
 *  $ONELINER$
 *     Grow or shrink an array
 *  $SYNTAX$
 *     ASIZE(<aTarget>, <nLength>) --> aTarget
 *  $ARGUMENTS$
       <aTarget> is the array to grow or shrink.

       <nLength> is the new size of the array.
 *     
 *  $RETURNS$
 *     ASIZE() returns a reference to the target array, <aTarget>.
 *  $DESCRIPTION$
       ASIZE() is an array function that changes the actual length of the
       <aTarget> array.  The array is shortened or lengthened to match the
       specified length.  If the array is shortened, elements at the end of the
       array are lost.  If the array is lengthened, new elements are added to
       the end of the array and assigned NIL.

       ASIZE() is similar to AADD() which adds a single new element to the end
       of an array and optionally assigns a new value at the same time.  Note
       that ASIZE() is different from AINS() and ADEL(), which do not actually
       change the array's length.
 *     
 *  $EXAMPLES$
     ^CFE  These examples demonstrate adding new elements and deleting
        existing elements:

        aArray := { 1 }          // Result: aArray is { 1 }
        ASIZE(aArray, 3)         // Result: aArray is { 1, NIL, NIL }
        ASIZE(aArray, 1)         // Result: aArray is { 1 }
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     AADD(),ADEL(),AFILL(),AINS()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     ATAIL()
 *  $CATEGORY$
 *     ARRAY
 *  $ONELINER$
 *     Return the highest numbered element of an array
 *  $SYNTAX$
 *     ATAIL(<aArray>) --> Element
 *  $ARGUMENTS$
 *     <aArray> is the array.
 *  $RETURNS$
       ATAIL() returns either a value or a reference to an array or object.
       The array is not changed.
 *     
 *  $DESCRIPTION$
     ATAIL() is an array function that returns the highest numbered element
     of an array.  It can be used in applications as shorthand for
     <aArray>[LEN(<aArray>)] when you need to obtain the last element of an
     array.
 *     
 *  $EXAMPLES$
     ^CFE  The following example creates a literal array and returns that
        last element of the array:

        aArray := {"a", "b", "c", "d"}
        ? ATAIL(aArray)                     // Result: d
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     
 *  $INCLUDE$
 *     
 *  $END$
 */

HARBOUR HB_ATAIL( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

   if( pArray )
      hb_arrayLast( pArray, &hb_stack.Return );
}
/*  $DOC$
 *  $FUNCNAME$
 *     AINS()
 *  $CATEGORY$
 *     ARRAY
 *  $ONELINER$
 *     Insert a NIL element into an array
 *  $SYNTAX$
 *     AINS(<aTarget>, <nPosition>) --> aTarget 
 *  $ARGUMENTS$
       <aTarget> is the array into which a new element will be inserted.

       <nPosition> is the position at which the new element will be
       inserted.
 *     
 *  $RETURNS$
 *     AINS() returns a reference to the target array, <aTarget>.
 *  $DESCRIPTION$
       AINS() is an array function that inserts a new element into a specified
     array.  The newly inserted element is NIL data type until a new value is
     assigned to it.  After the insertion, the last element in the array is
     discarded, and all elements after the new element are shifted down one
     position.

       Warning!  AINS() must be used carefully with multidimensional
     arrays.  Multidimensional arrays in HARBOUR are implemented by
     nesting arrays within other arrays.  Using AINS() in a multidimensional
     array discards the last element in the specified target array which, if
     it is an array element, will cause one or more dimensions to be lost.
     To insert a new dimension into an array, first add a new element to the
     end of the array using AADD() or ASIZE() before using AINS().
 *     
 *  $EXAMPLES$
     ^CFE  This example demonstrates the effect of using AINS() on an
        array:

        LOCAL aArray
        aArray := { 1, 2, 3 }      // Result: aArray is
                                   // now { 1, 2, 3 }
        AINS(aArray, 2)            // Result: aArray is
                                   // now { 1, NIL, 2 }
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     AADD(),ACOPY(),ADEL(),AEVAL(),AFILL(),ASIZE()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     ADEL()
 *  $CATEGORY$
 *     ARRAY
 *  $ONELINER$
 *     Delete an array element
 *  $SYNTAX$
 *     ADEL(<aTarget>, <nPosition>) --> aTarget
 *  $ARGUMENTS$
       <aTarget> is the array to delete an element from.

       <nPosition> is the position of the target array element to delete.
 *     
 *  $RETURNS$
 *     ADEL() returns a reference to the target array, <aTarget>.
 *  $DESCRIPTION$
       ADEL() is an array function that deletes an element from an array.  The
     contents of the specified array element is lost, and all elements from
     that position to the end of the array are shifted up one element.  The
     last element in the array becomes NIL.

       Warning!  HARBOUR implements multidimensional arrays by nesting
     arrays within other arrays.  If the <aTarget> array is a
     multidimensional array, ADEL() can delete an entire subarray specified
     by <nPosition>, causing <aTarget> to describe an array with a different
     structure than the original.
 *     
 *  $EXAMPLES$
     ^CFE  This example creates a constant array of three elements, and
        then deletes the second element.  The third element is moved up one
        position, and the new third element is assigned a NIL:

        LOCAL aArray
        aArray := { 1, 2, 3 }      // Result: aArray is
                                   // now { 1, 2, 3 }
        ADEL(aArray, 2)            // Result: aArray is
                                   // now { 1, 3, NIL }
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     ACOPY(),AINS(),AFILL() 
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     AFILL()
 *  $CATEGORY$
 *     ARRAY
 *  $ONELINER$
 *     Fill an array with a specified value
 *  $SYNTAX$
       AFILL(<aTarget>, <expValue>,
        [<nStart>], [<nCount>]) --> aTarget
 *     
 *  $ARGUMENTS$
       <aTarget> is the array to fill.

       <expValue> is the value to place in each array element.  It can be
     an expression of any valid data type.

       <nStart> is the position of the first element to fill.  If this
     argument is omitted, the default value is one.

       <nCount> is the number of elements to fill starting with element
     <nStart>.  If this argument is omitted, elements are filled from the
     starting element position to the end of the array.
 *     
 *  $RETURNS$
 *     AFILL() returns a reference to <aTarget>. 
 *  $DESCRIPTION$
       AFILL() is an array function that fills the specified array with a
     single value of any data type (including an array, code block, or NIL)
     by assigning <expValue> to each array element in the specified range.

       Warning!  AFILL() cannot be used to fill multidimensional arrays.
     HARBOUR implements multidimensional arrays by nesting arrays within
     other arrays.  Using AFILL() with a multidimensional array will
     overwrite subarrays used for the other dimensions of the array.
 *     
 *  $EXAMPLES$
     ^CFE  This example, creates a three-element array.  The array is
        then filled with the logical value, (.F.).  Finally, elements in
        positions two and three are assigned the new value of true (.T.):

        LOCAL aLogic[3]
        // Result: aLogic is { NIL, NIL, NIL }

        AFILL(aLogic, .F.)
        // Result: aLogic is { .F., .F., .F. }

        AFILL(aLogic, .T., 2, 2)
        // Result: aLogic is { .F., .T., .T. }
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     AADD(),AEVAL(),DBSTRUCT(),DIRECTORY()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     ASCAN()
 *  $CATEGORY$
 *     ARRAY
 *  $ONELINER$
 *     Scan an array for a value or until a block returns true (.T.)
 *  $SYNTAX$
 *     ASCAN(<aTarget>, <expSearch>,
 *      [<nStart>], [<nCount>]) --> nStoppedAt
 *     
 *  $ARGUMENTS$
 *     <aTarget>   Name of array to be scaned.
 *
 *     <expSearch> Expression to search for withing <aTarget>
 *
 *     <nStart>    Beggining subscript position at witch to start the
 *     search.
 *
 *     <nCount> Number of elements to scan with <aTarget>.
 *   position.  If this argument is not specified, all elements from the
 *    
 *  $RETURNS$
 *     <nStoppedAt> A numeric value of subscript position where <expSearch>
 *     was found.
 *     
 *  $DESCRIPTION$
 *     ASCAN() is an array function that scans an array for a specified value
 *   and operates like SEEK when searching for a simple value.  The
 *   <expSearch> value is compared to the target array element beginning with
 *   the leftmost character in the target element and proceeding until there
 *   are no more characters left in <expSearch>.  If there is no match,
 *   ASCAN() proceeds to the next element in the array.
 *
 *      Since ASCAN() uses the equal operator (=) for comparisons, it is
 *   sensitive to the status of EXACT.  If EXACT is ON, the target array
 *   element must be exactly equal to the result of <expSearch> to match.
 *
 *     If the <expSearch> argument is a code block, ASCAN() scans the <aTarget>
 *   array executing the block for each element accessed.  As each element is
 *   encountered, ASCAN() passes the element's value as an argument to the
 *   code block, and then performs an EVAL() on the block.  The scanning
 *   operation stops when the code block returns true (.T.), or ASCAN()
 *   reaches the last element in the array.
 *     
 *  $EXAMPLES$
 *   ^CFE  This example demonstrates scanning a three-element array using
 *      simple values and a code block as search criteria.  The code block
 *      criteria shows how to perform a case-insensitive search:
 *
 *      aArray := { "Tom", "Mary", "Sue" }
 *      ? ASCAN(aArray, "Mary")             // Result: 2
 *      ? ASCAN(aArray, "mary")             // Result: 0
 *      //
 *      ? ASCAN(aArray, { |x| UPPER(x) ;
 *            == "MARY" })                  // Result: 2
 *
 *   ^CFE  This example demonstrates scanning for multiple instances of a
 *      search argument after a match is found:
 *
 *      LOCAL aArray := { "Tom", "Mary", "Sue",;
 *                         "Mary" }, nStart := 1
 *      //
 *      // Get last array element position
 *      nAtEnd := LEN(aArray)
 *      DO WHILE (nPos := ASCAN(aArray, "Mary", ;
 *                   nStart)) > 0
 *         ? nPos, aArray[nPos]
 *         //
 *         // Get new starting position and test
 *         // boundary condition
 *         IF (nStart := ++nPos) > nAtEnd
 *            EXIT
 *         ENDIF
 *      ENDDO
 *
 *   ^CFE  This example scans a two dimensional array using a code block.
 *      Note that the parameter aVal in the code block is an array:
 *
 *      LOCAL aArr:={}
 *      CLS
 *      AADD(aArr,{"one","two"})
 *      AADD(aArr,{"three","four"})
 *      AADD(aArr,{"five","six"})
 *      ? ASCAN(aArr, {|aVal| aVal[2] == "four"})         // Returns 2
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *     This functions is not CA-Clipper compatible. Clipper ASCAN() is
 *     affected by the SET EXACT ON/OFF Condition
 *  $SEEALSO$
 *     ACOMP(),AEVAL()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     AEVAL()
 *  $CATEGORY$
 *     ARRAY
 *  $ONELINER$
 *     Evaluated the subscript element of an array
 *  $SYNTAX$
 *     AEVAL(<aArray>, <bBlock>,
 *      [<nStart>], [<nCount>]) --> aArray
 *     
 *  $ARGUMENTS$
 *     <aArray> Is the array to be evaluated.
 *
 *     <bBlock> Is a code block to evaluate for each element processed.
 *
 *     <nStart> The beggining array element to evaluate.
 *
 *     <nCount> The number of elements to process. 
 *     
 *  $RETURNS$
 *     AEVAL() returns an array pointer reference.
 *     
 *  $DESCRIPTION$
 *     This function will evaluate and process the subscript elements
 *     in <aArray>. A code block passed as <bBlock> defines the 
 *     operation to be executed on each element of the array. All
 *     elements in <aArray> will be evaluated unless specified by a
 *     beggining subscript position in <nStart> for <nCount> elements.
 *
 *     Two parameters are passed to the code block <bBlock>. The
 *     individual elements in an array are the first parameter and the
 *     subscript position is the second.
 *
 *     AEVAL() does not replace a FOR...NEXT loop for processing arrays.
 *     If a array is an autonomous unit,AEVAL() is appropriate.If the
 *     array is to be altered or if elements are to be reevalueted, a
 *     FOR...NEXT loop is more appropriate.
 *  $EXAMPLES$
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     EVAL(),DBEVAL()
 *  $INCLUDE$
 *     
 *  $END$
 */

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
/*  $DOC$
 *  $FUNCNAME$
 *     ACOPY()
 *  $CATEGORY$
 *     ARRAY
 *  $ONELINER$
 *     Copy elements from one array to another
 *  $SYNTAX$
 *     ACOPY(<aSource>, <aTarget>,
 *      [<nStart>], [<nCount>], [<nTargetPos>]) --> aTarget
 *     
 *  $ARGUMENTS$
 *     <aSource> is the array to copy elements from.
 *
 *     <aTarget> is the array to copy elements to.
 *
 *     <nStart>  is the beggining subscript position to copy from <aSource>
 *
 *     <nCount>  the number of subscript elements to copy from <aSource>
 *
 *     <nTargetPos> the starting subscript position in <aTarget> to copy
 *  elements to
 *     
 *  $RETURNS$
 *     ACOPY() returns an array pointer reference 
 *  $DESCRIPTION$
 *     This function copies array elements from <aSource> to <aTarget>.
 *     <nStart> is the beggining element to be copied from <aSource>;the
 *   default is 1.
 *     <nCount> is the number of element to be copied from <aSource>;the
 *   default is the entire array.
 *     <nTargetPos> is the subscript number in the target array,<aTarget>,
 *   to witch array elements are to be copied;the default is 1
 *     This function will copy all data types in <aSource> to <aTarget>.
 *   If an array element in <aSource> is a pointer reference to another
 *   array, that array pointer will be copied to <aTarget>; not all
 *   subdimensions will be copied from one array to the next. This must
 *   be accomplished via the ACLONE() function.
 *
 *    ^b note
 *     If array <aSource> is larger then <aTarget>, array elements will
 *   start copying at <nTargetPos> and continue copying until the end of
 *   array <aTarget> is reached. The ACOPY() function doesn't append
 *   subscript positions to the target array, the size of the target
 *   array <aTarget> remains constant.
 *  $EXAMPLES$
 *   ^CFE  This example creates two arrays, each filled with a value.
 *      The first two elements from the source array are then copied into the
 *      target array:
 *
 *      LOCAL nCount := 2, nStart := 1, aOne, aTwo
 *      aOne := { 1, 1, 1 }
 *      aTwo := { 2, 2, 2 }
 *      ACOPY(aOne, aTwo, nStart, nCount)
 *      // Result: aTwo is now { 1, 1, 2 }
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     ACLONE(),ADEL(),AEVAL(),AFILL(),AINS(),ASORT()
 *  $INCLUDE$
 *     
 *  $END$
 */

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

/* NOTE: Clipper will return NIL if the parameter is not an array. [vszel] */

/*  $DOC$
 *  $FUNCNAME$
 *     ACLONE()
 *  $CATEGORY$
 *     ARRAY
 *  $ONELINER$
 *     Duplicate a  multidimensional array
 *  $SYNTAX$
 *     ACLONE(<aSource>) --> aDuplicate
 *  $ARGUMENTS$
 *     <aSource> Name of the array to be cloned.
 *  $RETURNS$
 *     ACLONE() A new array pointer reference complete with nested array
 *   values.
 *  $DESCRIPTION$
 *     This function makes a complete copy of the array expressed as
 *  <aSource> and return a cloned set of array values.This provides
 *  a complete 
 *  $EXAMPLES$
 *   ^CFE  This example creates an array then duplicates it using
 *      ACLONE().  The first array is then altered, but the duplicate copy is
 *      unaffected:
 *
 *      LOCAL aOne, aTwo
 *      aOne := { 1, 2, 3 }        // Result: aOne is {1, 2, 3}
 *      aTwo := ACLONE(aOne)       // Result: aTwo is {1, 2, 3}
 *      aOne[1] := 99              // Result: aOne is {99, 2, 3}
 *                                 // aTwo is still {1, 2, 3}
 *
 *  $TESTS$
 *
 *  $STATUS$
 *     R
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *     ACOPY(),ADEL(),AINS(),ASIZE()
 *  $INCLUDE$
 *     
 *  $END$
 */

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

