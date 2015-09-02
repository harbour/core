/*
 * ASort() function
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
 * Copyright 1999-2001 Jose Lalin <dezac@corevia.com>
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

/* TOFIX: The sorting engine requires signed indexes to work, this means
          that arrays larger than 2^31 elements cannot be sorted. [vszakats] */

/* NOTE: Based on PD code found in
         SORTING AND SEARCHING ALGORITHMS: A COOKBOOK, BY THOMAS NIEMANN
         https://www.cs.auckland.ac.nz/~jmor159/PLDS210/niemann/s_man.htm */

#include "hbvmint.h"
#include "hbapiitm.h"
#include "hbvm.h"

static HB_BOOL hb_itemIsLess( PHB_BASEARRAY pBaseArray, PHB_ITEM pBlock,
                              HB_SIZE nItem1, HB_SIZE nItem2 )
{
   PHB_ITEM pItem1 = pBaseArray->pItems + nItem1,
            pItem2 = pBaseArray->pItems + nItem2;

   if( pBlock )
   {
      PHB_ITEM pRet;

      /* protection against array resizing by user codeblock */
      if( pBaseArray->nLen <= nItem1 || pBaseArray->nLen <= nItem2 )
         return HB_FALSE;

      hb_vmPushEvalSym();
      hb_vmPush( pBlock );
      hb_vmPush( pItem1 );
      hb_vmPush( pItem2 );
      hb_vmSend( 2 );

      pRet = hb_param( -1, HB_IT_ANY );

      /* CA-Cl*pper always takes return value as logical item
       * accepting 0, 1 as numeric representation of HB_FALSE/HB_TRUE
       */
      return ( HB_IS_LOGICAL( pRet ) || HB_IS_NUMERIC( pRet ) ) ?
             hb_itemGetL( pRet ) : HB_TRUE;
   }

   /* Do native compare when no codeblock is supplied */

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
      return hb_itemStrCmp( pItem1, pItem2, HB_FALSE ) < 0;
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
      /* intentionally separate comparison for integer numbers
         to avoid precision lose in 64bit integer to double conversion */
      return hb_itemGetNInt( pItem1 ) < hb_itemGetNInt( pItem2 );
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
      return hb_itemGetND( pItem1 ) < hb_itemGetND( pItem2 );
   else if( HB_IS_TIMESTAMP( pItem1 ) && HB_IS_TIMESTAMP( pItem2 ) )
   {
      long lDate1, lTime1, lDate2, lTime2;

      hb_itemGetTDT( pItem1, &lDate1, &lTime1 );
      hb_itemGetTDT( pItem2, &lDate2, &lTime2 );
      return lDate1 == lDate2 ? lTime1 < lTime2 : lDate1 < lDate2;
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
      /* it's not exact comparison, compare only julian date */
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
      else if( HB_IS_DATETIME( pItem1 ) ) iWeight1 = 5;
      else if( HB_IS_NUMERIC( pItem1 ) ) iWeight1 = 6;
      else iWeight1 = 7;

      if( HB_IS_ARRAY( pItem2 ) ) iWeight2 = 1;
      else if( HB_IS_BLOCK( pItem2 ) ) iWeight2 = 2;
      else if( HB_IS_STRING( pItem2 ) ) iWeight2 = 3;
      else if( HB_IS_LOGICAL( pItem2 ) ) iWeight2 = 4;
      else if( HB_IS_DATETIME( pItem2 ) ) iWeight2 = 5;
      else if( HB_IS_NUMERIC( pItem2 ) ) iWeight2 = 6;
      else iWeight2 = 7;

      return iWeight1 < iWeight2;
   }
}

#ifdef HB_CLP_STRICT

/* partition array pItems[lb..ub] */

static HB_ISIZ hb_arraySortQuickPartition( PHB_BASEARRAY pBaseArray, HB_ISIZ lb, HB_ISIZ ub, PHB_ITEM pBlock )
{
   HB_ISIZ i, j;

   /* select pivot and exchange with 1st element */
   i = lb + ( ( ub - lb ) >> 1 );
   if( i != lb )
      hb_itemRawSwap( pBaseArray->pItems + lb, pBaseArray->pItems + i );

   /* sort lb+1..ub based on pivot */
   i = lb + 1;
   j = ub;

   for( ;; )
   {
      while( i < j && hb_itemIsLess( pBaseArray, pBlock, i, lb ) )
         i++;

      while( j >= i && hb_itemIsLess( pBaseArray, pBlock, lb, j ) )
         j--;

      if( i >= j )
         break;

      /* Swap the items */
      hb_itemRawSwap( pBaseArray->pItems + i, pBaseArray->pItems + j );
      j--;
      i++;
   }

   /* pivot belongs in pBaseArray->pItems[ j ] */
   if( j > lb && pBaseArray->nLen > ( HB_SIZE ) j )
      hb_itemRawSwap( pBaseArray->pItems + lb, pBaseArray->pItems + j );

   return j;
}

/* sort array pBaseArray->pItems[lb..ub] */

static void hb_arraySortQuick( PHB_BASEARRAY pBaseArray, HB_ISIZ lb, HB_ISIZ ub, PHB_ITEM pBlock )
{
   while( lb < ub )
   {
      HB_ISIZ m;

      if( ( HB_SIZE ) ub >= pBaseArray->nLen )
      {
         ub = pBaseArray->nLen - 1;
         if( lb >= ub )
            break;
      }

      /* partition into two segments */
      m = hb_arraySortQuickPartition( pBaseArray, lb, ub, pBlock );

      /* sort the smallest partition to minimize stack requirements */
      if( m - lb <= ub - m )
      {
         hb_arraySortQuick( pBaseArray, lb, m - 1, pBlock );
         lb = m + 1;
      }
      else
      {
         hb_arraySortQuick( pBaseArray, m + 1, ub, pBlock );
         ub = m - 1;
      }
   }
}

static void hb_arraySortStart( PHB_BASEARRAY pBaseArray, PHB_ITEM pBlock,
                               HB_SIZE nStart, HB_SIZE nCount )
{
   hb_arraySortQuick( pBaseArray, nStart, nStart + nCount - 1, pBlock );
}

#else

static HB_BOOL hb_arraySortDO( PHB_BASEARRAY pBaseArray, PHB_ITEM pBlock,
                               HB_SIZE * pSrc, HB_SIZE * pBuf, HB_SIZE nCount )
{
   if( nCount > 1 )
   {
      HB_SIZE nCnt1, nCnt2, * pPtr1, * pPtr2, * pDst;
      HB_BOOL fBuf1, fBuf2;

      nCnt1 = nCount >> 1;
      nCnt2 = nCount - nCnt1;
      pPtr1 = &pSrc[ 0 ];
      pPtr2 = &pSrc[ nCnt1 ];

      fBuf1 = hb_arraySortDO( pBaseArray, pBlock, pPtr1, &pBuf[ 0 ], nCnt1 );
      fBuf2 = hb_arraySortDO( pBaseArray, pBlock, pPtr2, &pBuf[ nCnt1 ], nCnt2 );
      if( fBuf1 )
         pDst = pBuf;
      else
      {
         pDst = pSrc;
         pPtr1 = &pBuf[ 0 ];
      }
      if( ! fBuf2 )
         pPtr2 = &pBuf[ nCnt1 ];

      while( nCnt1 > 0 && nCnt2 > 0 )
      {
         if( hb_itemIsLess( pBaseArray, pBlock, *pPtr2, *pPtr1 ) )
         {
            *pDst++ = *pPtr2++;
            nCnt2--;
         }
         else
         {
            *pDst++ = *pPtr1++;
            nCnt1--;
         }
      }
      if( nCnt1 > 0 )
      {
         do
            *pDst++ = *pPtr1++;
         while( --nCnt1 );
      }
      else if( nCnt2 > 0 && fBuf1 == fBuf2 )
      {
         do
            *pDst++ = *pPtr2++;
         while( --nCnt2 );
      }
      return ! fBuf1;
   }
   return HB_TRUE;
}

static void hb_arraySortStart( PHB_BASEARRAY pBaseArray, PHB_ITEM pBlock,
                               HB_SIZE nStart, HB_SIZE nCount )
{
   HB_SIZE * pBuffer, * pDest, * pPos, nPos, nTo;

   pBuffer = ( HB_SIZE * ) hb_xgrab( sizeof( HB_SIZE ) * 2 * nCount );
   for( nPos = 0; nPos < nCount; ++nPos )
      pBuffer[ nPos ] = nStart + nPos;

   if( hb_arraySortDO( pBaseArray, pBlock, pBuffer, &pBuffer[ nCount ], nCount ) )
      pPos = ( pDest = pBuffer ) + nCount;
   else
      pDest = ( pPos = pBuffer ) + nCount;

   /* protection against array resizing by user codeblock */
   if( nStart + nCount > pBaseArray->nLen )
   {
      if( pBaseArray->nLen > nStart )
      {
         for( nPos = nTo = 0; nPos < nCount; ++nPos )
         {
            if( pDest[ nPos ] < pBaseArray->nLen )
               pDest[ nTo++ ] = pDest[ nPos ];
         }
         nCount = nTo;
      }
      else
         nCount = 0;
   }

   for( nPos = 0; nPos < nCount; ++nPos )
      pPos[ pDest[ nPos ] - nStart ] = nPos;

   for( nPos = 0; nPos < nCount; ++nPos )
   {
      if( nPos + nStart != pDest[ nPos ] )
      {
         hb_itemRawSwap( pBaseArray->pItems + nPos + nStart,
                         pBaseArray->pItems + pDest[ nPos ] );
         pDest[ pPos[ nPos ] ] = pDest[ nPos ];
         pPos[ pDest[ nPos ] - nStart ] = pPos[ nPos ];
      }
   }

   hb_xfree( pBuffer );
}
#endif /* HB_CLP_STRICT */

HB_BOOL hb_arraySort( PHB_ITEM pArray, HB_SIZE * pnStart, HB_SIZE * pnCount, PHB_ITEM pBlock )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_arraySort(%p, %p, %p, %p)", pArray, pnStart, pnCount, pBlock ) );

   if( HB_IS_ARRAY( pArray ) )
   {
      PHB_BASEARRAY pBaseArray = pArray->item.asArray.value;
      HB_SIZE nLen = pBaseArray->nLen;
      HB_SIZE nStart;

      if( pnStart && *pnStart >= 1 )
         nStart = *pnStart;
      else
         nStart = 1;

      if( nStart <= nLen )
      {
         HB_SIZE nCount;

         if( pnCount && *pnCount >= 1 && ( *pnCount <= nLen - nStart ) )
            nCount = *pnCount;
         else
            nCount = nLen - nStart + 1;

         if( nStart + nCount > nLen )             /* check range */
            nCount = nLen - nStart + 1;

         /* Optimize when only one or no element is to be sorted */
         if( nCount > 1 )
            hb_arraySortStart( pBaseArray, pBlock, nStart - 1, nCount );
      }

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_FUNC( ASORT )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray && ! hb_arrayIsObject( pArray ) )
   {
      HB_SIZE nStart = hb_parns( 2 );
      HB_SIZE nCount = hb_parns( 3 );

      hb_arraySort( pArray,
                    HB_ISNUM( 2 ) ? &nStart : NULL,
                    HB_ISNUM( 3 ) ? &nCount : NULL,
                    hb_param( 4, HB_IT_EVALITEM ) );

      hb_itemReturn( pArray ); /* ASort() returns the array itself */
   }
}
