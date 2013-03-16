/*
 * Harbour Project source code:
 * The Array API (Harbour level)
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

#include "hbapi.h"
#include "hbstack.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"

/* This function creates an array item using 'iDimension' as an index
 * to retrieve the number of elements from the parameter list.
 */
static void hb_arrayNewRagged( PHB_ITEM pArray, int iDimension )
{
   HB_SIZE nElements;

   HB_TRACE( HB_TR_DEBUG, ( "hb_arrayNewRagged(%p, %d)", pArray, iDimension ) );

   nElements = hb_parns( iDimension );

   /* create an array */
   hb_arrayNew( pArray, nElements );

   if( ++iDimension <= hb_pcount() )
   {
      /* call self recursively to create next dimensions
       */
      while( nElements )
         hb_arrayNewRagged( hb_arrayGetItemPtr( pArray, nElements-- ), iDimension );
   }
}

HB_FUNC( ARRAY )
{
   int iPCount = hb_pcount();

   if( iPCount > 0 )
   {
      HB_BOOL bError = HB_FALSE;
      int iParam;

      for( iParam = 1; iParam <= iPCount; iParam++ )
      {
         if( ! HB_ISNUM( iParam ) )
         {
            bError = HB_TRUE;
            break;
         }

         if( hb_parns( iParam ) < 0 ) /* || hb_parns( iParam ) <= 4096 */
         {
#ifdef HB_CLP_STRICT
            hb_errRT_BASE( EG_BOUND, 1131, NULL, hb_langDGetErrorDesc( EG_ARRDIMENSION ), 0 );
#else
            hb_errRT_BASE( EG_BOUND, 1131, NULL, hb_langDGetErrorDesc( EG_ARRDIMENSION ), HB_ERR_ARGS_BASEPARAMS );
#endif
            bError = HB_TRUE;
            break;
         }
      }

      if( ! bError )
         hb_arrayNewRagged( hb_stackReturnItem(), 1 );
   }
}

HB_FUNC( AADD )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

      if( pValue && hb_arrayAdd( pArray, pValue ) )
         hb_itemReturn( pValue );
      else
         hb_errRT_BASE( EG_BOUND, 1187, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* NOTE: CA-Cl*pper 5.3 and older will return NIL on bad parameter, 5.3a,b
         will throw a runtime error. [vszakats] */

HB_FUNC( ASIZE )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray && HB_ISNUM( 2 ) )
   {
      HB_ISIZ nSize = hb_parns( 2 );

      hb_arraySize( pArray, HB_MAX( nSize, 0 ) );

      hb_itemReturn( pArray ); /* ASize() returns the array itself */
   }
#ifdef HB_COMPAT_C53 /* From CA-Cl*pper 5.3a */
   else
#ifdef HB_CLP_STRICT
      hb_errRT_BASE( EG_ARG, 2023, NULL, HB_ERR_FUNCNAME, 0 );
#else
      hb_errRT_BASE( EG_ARG, 2023, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
#endif
}

HB_FUNC( ATAIL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
      hb_arrayLast( pArray, hb_stackReturnItem() );
}

HB_FUNC( AINS )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      HB_ISIZ nPos = hb_parns( 2 );

      if( nPos == 0 )
         nPos = 1;

      hb_arrayIns( pArray, nPos );

      hb_itemReturn( pArray ); /* AIns() returns the array itself */
   }
}

HB_FUNC( ADEL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      HB_ISIZ nPos = hb_parns( 2 );

      if( nPos == 0 )
         nPos = 1;

      hb_arrayDel( pArray, nPos );

      hb_itemReturn( pArray ); /* ADel() returns the array itself */
   }
}

HB_FUNC( AFILL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

      hb_itemReturn( pArray ); /* AFill() returns the array itself */

      if( pValue )
      {
         HB_SIZE nStart, nCount;
         HB_ISIZ lStart = hb_parns( 3 ), lCount = hb_parns( 4 );

         /* Explicy lCount of 0 - Nothing to do! */
         if( HB_ISNUM( 4 ) && lCount == 0 )
            return;
         /* Clipper aborts if negative start. */
         else if( lStart < 0 )
            return;
         /* Clipper allows Start to be of wrong type, or 0, and corrects it to 1. */
         else if( lStart == 0 )
            lStart = 1;
         if( lCount < 0 )
         {
            /* Clipper allows the Count to be negative, if start is 1, and corrects it to maximum elements. */
            if( lStart == 1 )
               nCount = 0;
            /* Clipper aborts if negative count and start is not at 1. */
            else
               return;
         }
         nStart = ( HB_SIZE ) lStart;
         nCount = ( HB_SIZE ) lCount;
         hb_arrayFill( pArray,
                       pValue,
                       HB_ISNUM( 3 ) ? &nStart : NULL,
                       HB_ISNUM( 4 ) ? &nCount : NULL );
      }
   }
   else
#ifdef HB_CLP_STRICT
      /* NOTE: In CA-Cl*pper AFILL() is written in a manner that it will
               call AEVAL() to do the job, so the error (if any) will also be
               thrown by AEVAL().  [vszakats] */
      hb_errRT_BASE( EG_ARG, 2017, NULL, "AEVAL", 0 );
#else
      hb_errRT_BASE( EG_ARG, 6004, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}

HB_FUNC( ASCAN )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

   if( pArray && pValue )
   {
      HB_SIZE nStart = hb_parns( 3 );
      HB_SIZE nCount = hb_parns( 4 );

      hb_retns( hb_arrayScan( pArray, pValue,
                              HB_ISNUM( 3 ) ? &nStart : NULL,
                              HB_ISNUM( 4 ) ? &nCount : NULL,
                              HB_FALSE ) );
   }
   else
      hb_retni( 0 );
}

/* Same as ASCAN() but has an additional parameter to force exact comparison. */
HB_FUNC( HB_ASCAN )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

   if( pArray && pValue )
   {
      HB_SIZE nStart = hb_parns( 3 );
      HB_SIZE nCount = hb_parns( 4 );

      hb_retns( hb_arrayScan( pArray, pValue,
                              HB_ISNUM( 3 ) ? &nStart : NULL,
                              HB_ISNUM( 4 ) ? &nCount : NULL,
                              hb_parl( 5 ) ) );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( HB_RASCAN )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

   if( pArray && pValue )
   {
      HB_SIZE nStart = hb_parns( 3 );
      HB_SIZE nCount = hb_parns( 4 );

      hb_retns( hb_arrayRevScan( pArray, pValue,
                                 HB_ISNUM( 3 ) ? &nStart : NULL,
                                 HB_ISNUM( 4 ) ? &nCount : NULL,
                                 hb_parl( 5 ) ) );
   }
   else
      hb_retni( 0 );
}

HB_FUNC( HB_AINS )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      HB_ISIZ nPos = hb_parns( 2 );

      if( nPos == 0 )
         nPos = 1;

      if( hb_parl( 4 ) )
      {
         HB_SIZE nLen = hb_arrayLen( pArray ) + 1;
         if( nPos >= 1 && ( HB_SIZE ) nPos <= nLen )
            hb_arraySize( pArray, nLen );
      }

      if( hb_arrayIns( pArray, nPos ) )
      {
         if( ! HB_ISNIL( 3 ) )
            hb_arraySet( pArray, nPos, hb_param( 3, HB_IT_ANY ) );
      }

      hb_itemReturn( pArray ); /* AIns() returns the array itself */
   }
}

HB_FUNC( HB_ADEL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      HB_ISIZ nPos = hb_parns( 2 );

      if( nPos == 0 )
         nPos = 1;

      if( hb_arrayDel( pArray, nPos ) )
      {
         if( hb_parl( 3 ) )
            hb_arraySize( pArray, hb_arrayLen( pArray ) - 1 );
      }

      hb_itemReturn( pArray ); /* ADel() returns the array itself */
   }
}

/* TODO: In Xbase++ fifth parameter determines whether array elements
         are passed by reference to the code block. [vszakats] */

HB_FUNC( AEVAL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pBlock = hb_param( 2, HB_IT_BLOCK );

   if( pArray && pBlock )
   {
      HB_SIZE nStart = hb_parns( 3 );
      HB_SIZE nCount = hb_parns( 4 );

      hb_arrayEval( pArray,
                    pBlock,
                    HB_ISNUM( 3 ) ? &nStart : NULL,
                    HB_ISNUM( 4 ) ? &nCount : NULL );

      hb_itemReturn( pArray ); /* AEval() returns the array itself */
   }
   else
      hb_errRT_BASE( EG_ARG, 2017, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( ACOPY )
{
   PHB_ITEM pSrcArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pDstArray = hb_param( 2, HB_IT_ARRAY );

   if( pSrcArray && pDstArray )
   {
      /* CA-Cl*pper works this way. */
      if( ! hb_arrayIsObject( pSrcArray ) && ! hb_arrayIsObject( pDstArray ) )
      {
         HB_SIZE nStart = hb_parns( 3 );
         HB_SIZE nCount = hb_parns( 4 );
         HB_SIZE nTarget = hb_parns( 5 );

         hb_arrayCopy( pSrcArray,
                       pDstArray,
                       HB_ISNUM( 3 ) ? &nStart : NULL,
                       HB_ISNUM( 4 ) ? &nCount : NULL,
                       HB_ISNUM( 5 ) ? &nTarget : NULL );
      }

      hb_itemReturn( pDstArray ); /* ACopy() returns the target array */
   }
}

/* NOTE: Clipper will return NIL if the parameter is not an array. [vszakats] */

HB_FUNC( ACLONE )
{
   PHB_ITEM pSrcArray = hb_param( 1, HB_IT_ARRAY );

   if( pSrcArray && ! hb_arrayIsObject( pSrcArray ) )
      hb_arrayCloneTo( hb_stackReturnItem(), pSrcArray ); /* AClone() returns the new array */
}

HB_FUNC( HB_APARAMS )
{
   hb_itemReturnRelease( hb_arrayFromParams( hb_parni( 1 ) + 1 ) );
}
