/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ASORT() function
 *
 * Copyright 2000 Victor Szakats <info@szelvesz.hu>
 *                Jose Lalin <dezac@corevia.com>
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

/* TOFIX: The sorting engine requires signed indexes to work, this means
          that arrays larger than 2^31 elements cannot be sorted. [vszakats] */

/* NOTE: Based on PD code found in
         SORTING AND SEARCHING ALGORITHMS: A COOKBOOK, BY THOMAS NIEMANN
         http://members.xoom.com/_XMCM/thomasn/s_man.htm */

#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"

/* NOTE: If this is defined the item copying is optimized, in a way that 
         instead of calling the official hb_itemCopy(), the item structures
         will be directly copied with memcpy(), this means that the related 
         data areas (string space for example) will never be moved. This can be 
         safely done here, because it's guaranteed by the nature of sorting 
         that the set of items doesn't change (there're no deleted or new 
         items, just swapping) in this functions. 
         Using this option makes sorting *much* faster, but if you have a 
         problem, or the low level stuff changes, turn it off. [vszakats] */
#define HB_ASORT_OPT_ITEMCOPY

static BOOL hb_itemIsLess( PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
      return hb_itemStrCmp( pItem1, pItem2, FALSE ) < 0;
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
      return hb_itemGetND( pItem1 ) < hb_itemGetND( pItem2 );
   else if( HB_IS_DATE( pItem1 ) && HB_IS_DATE( pItem2 ) )
      return hb_itemGetDL( pItem1 ) < hb_itemGetDL( pItem2 );
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
      return hb_itemGetL( pItem1 ) < hb_itemGetL( pItem2 );
   else
   {
      /* NOTE: For non-matching types CA-Cl*pper sorts always like this:
               Array/Object Block String Logical Date Numeric NIL [jlalin] */

      int iWeight1;
      int iWeight2;

      if( HB_IS_ARRAY( pItem1 ) ) iWeight1 = 1;
      else if( HB_IS_BLOCK( pItem1 ) ) iWeight1 = 2;
      else if( HB_IS_STRING( pItem1 ) ) iWeight1 = 3;
      else if( HB_IS_LOGICAL( pItem1 ) ) iWeight1 = 4;
      else if( HB_IS_DATE( pItem1 ) ) iWeight1 = 5;
      else if( HB_IS_NUMERIC( pItem1 ) ) iWeight1 = 6;
      else iWeight1 = 7;

      if( HB_IS_ARRAY( pItem2 ) ) iWeight2 = 1;
      else if( HB_IS_BLOCK( pItem2 ) ) iWeight2 = 2;
      else if( HB_IS_STRING( pItem2 ) ) iWeight2 = 3;
      else if( HB_IS_LOGICAL( pItem2 ) ) iWeight2 = 4;
      else if( HB_IS_DATE( pItem2 ) ) iWeight2 = 5;
      else if( HB_IS_NUMERIC( pItem2 ) ) iWeight2 = 6;
      else iWeight2 = 7;

      return iWeight1 < iWeight2;
   }
}

/* partition array pItems[lb..ub] */

static LONG hb_arraySortQuickPartition( PHB_ITEM pItems, LONG lb, LONG ub, PHB_ITEM pBlock )
{
   LONG i;
   LONG j;
   LONG p;

   HB_ITEM pivot;
   
   /* select pivot and exchange with 1st element */
   p = lb + ( ( ub - lb ) / 2 );
   
#ifdef HB_ASORT_OPT_ITEMCOPY
   memcpy( &pivot, pItems + p, sizeof( HB_ITEM ) );
   if( p != lb )
      memcpy( pItems + p, pItems + lb, sizeof( HB_ITEM ) );
#else
   hb_itemInit( &pivot );
   hb_itemCopy( &pivot, pItems + p );
   if( p != lb )
      hb_itemCopy( pItems + p, pItems + lb );
#endif

   /* sort lb+1..ub based on pivot */
   i = lb + 1;
   j = ub;

   while( TRUE )
   {
      if( pBlock )
      {
         /* Call the codeblock to compare the items */
         while( i < j )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pBlock );
            hb_vmPush( pItems + i );
            hb_vmPush( &pivot );
            hb_vmDo( 2 );
            
            if( ( HB_IS_LOGICAL( &hb_stack.Return ) ? hb_stack.Return.item.asLogical.value : hb_itemIsLess( pItems + i, &pivot ) ) )
               i++;
            else
               break;
         }
         
         while( j >= i )
         {
            hb_vmPushSymbol( &hb_symEval );
            hb_vmPush( pBlock );
            hb_vmPush( &pivot );
            hb_vmPush( pItems + j );
            hb_vmDo( 2 );
            
            if( ( HB_IS_LOGICAL( &hb_stack.Return ) ? hb_stack.Return.item.asLogical.value : hb_itemIsLess( &pivot, pItems + j ) ) )
               j--;
            else
               break;
         }
      }
      else
      {
         /* Do native compare when no codeblock is supplied */
         while( i < j && hb_itemIsLess( pItems + i, &pivot ) )
            i++;
         
         while( j >= i && hb_itemIsLess( &pivot, pItems + j ) )
            j--;
      }

      if( i >= j )
         break;

      /* Swap the items */
      {
         HB_ITEM temp;
      
#ifdef HB_ASORT_OPT_ITEMCOPY
         memcpy( &temp, pItems + j, sizeof( HB_ITEM ) );
         memcpy( pItems + j, pItems + i, sizeof( HB_ITEM ) );
         memcpy( pItems + i, &temp, sizeof( HB_ITEM ) );
#else
         hb_itemInit( &temp );
         hb_itemCopy( &temp, pItems + j );
         hb_itemCopy( pItems + j, pItems + i );
         hb_itemCopy( pItems + i, &temp );
         hb_itemClear( &temp );
#endif
      }

      j--;
      i++;
   }

   /* pivot belongs in pItems[j] */
#ifdef HB_ASORT_OPT_ITEMCOPY
   if( lb != j )
      memcpy( pItems + lb, pItems + j, sizeof( HB_ITEM ) );
   memcpy( pItems + j, &pivot, sizeof( HB_ITEM ) );
#else
   if( lb != j )
      hb_itemCopy( pItems + lb, pItems + j );
   hb_itemCopy( pItems + j, &pivot );
   hb_itemClear( &pivot );
#endif

   return j;
}

/* sort array pItems[lb..ub] */

static void hb_arraySortQuick( PHB_ITEM pItems, LONG lb, LONG ub, PHB_ITEM pBlock )
{
   while( lb < ub )
   {
      /* partition into two segments */
      LONG m = hb_arraySortQuickPartition( pItems, lb, ub, pBlock );

      /* sort the smallest partition to minimize stack requirements */
      if( m - lb <= ub - m )
      {
         hb_arraySortQuick( pItems, lb, m - 1, pBlock );
         lb = m + 1;
      }
      else
      {
         hb_arraySortQuick( pItems, m + 1, ub, pBlock );
         ub = m - 1;
      }
   }
}

BOOL hb_arraySort( PHB_ITEM pArray, ULONG * pulStart, ULONG * pulCount, PHB_ITEM pBlock )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_arraySort(%p, %p, %p, %p)", pArray, pulStart, pulCount, pBlock));

   if( HB_IS_ARRAY( pArray ) )
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
         if( pulCount && *pulCount >= 1 && ( *pulCount <= ulLen - ulStart ) )
            ulCount = *pulCount;
         else
            ulCount = ulLen - ulStart + 1;
      
         if( ulStart + ulCount > ulLen )             /* check range */
            ulCount = ulLen - ulStart + 1;
      
         ulCount += ulStart - 2;
      
         /* Optimize when only one or no element is to be sorted */
         if( ulCount > 1 )
            hb_arraySortQuick( pBaseArray->pItems, ulStart - 1, ulCount, pBlock );
      }

      return TRUE;
   }
   else
      return FALSE;
}

HB_FUNC( ASORT )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray && ! hb_arrayIsObject( pArray )  )
   {
      ULONG ulStart = hb_parnl( 2 );
      ULONG ulCount = hb_parnl( 3 );
      
      hb_arraySort( pArray,
                    ISNUM( 2 ) ? &ulStart : NULL,
                    ISNUM( 3 ) ? &ulCount : NULL,
                    hb_param( 4, HB_IT_BLOCK ) );
         
      hb_itemReturn( pArray ); /* ASort() returns the array itself */
   }
}

