/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Array API (Harbour level)
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

HB_FUNC( ARRAY )
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
            hb_errRT_BASE( EG_BOUND, 1131, NULL, hb_langDGetErrorDesc( EG_ARRDIMENSION ), 1, hb_paramError( 1 ) );
            bError = TRUE;
            break;
         }
      }

      if( ! bError )
         hb_arrayNewRagged( &hb_stack.Return, 1 );
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
         hb_errRT_BASE( EG_BOUND, 1187, NULL, "AADD", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1123, NULL, "AADD", 2, hb_paramError(1), hb_paramError( 2 ) );
}

HB_FUNC( HB_ARRAYID )  /* for debugging: returns the array's "address" so dual references to same array can be seen */
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( HB_IS_ARRAY(pArray) )
      hb_retnl( (long) pArray->item.asArray.value );
   else
      hb_retnl( -1 );
}


/* NOTE: CA-Cl*pper 5.3 and older will return NIL on bad parameter, 5.3a,b
         will throw a runtime error. [vszakats] */

HB_FUNC( ASIZE )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray && ISNUM( 2 ) )
   {
      long lSize = hb_parnl( 2 );

      hb_arraySize( pArray, HB_MAX( lSize, 0 ) );

      hb_itemReturn( pArray ); /* ASize() returns the array itself */
   }
#ifdef HB_COMPAT_C53 /* From CA-Cl*pper 5.3a */
   else
      hb_errRT_BASE( EG_ARG, 2023, NULL, "ASIZE", 2, hb_paramError( 1 ), hb_paramError( 2 ) );
#endif
}

HB_FUNC( ATAIL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
      hb_arrayLast( pArray, &hb_stack.Return );
}

HB_FUNC( AINS )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      if( ISNUM( 2 ) )
         hb_arrayIns( pArray, hb_parnl( 2 ) );

      hb_itemReturn( pArray ); /* AIns() returns the array itself */
   }
}

HB_FUNC( ADEL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      if( ISNUM( 2 ) )
         hb_arrayDel( pArray, hb_parnl( 2 ) );

      hb_itemReturn( pArray ); /* ADel() returns the array itself */
   }
}

HB_FUNC( AFILL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray )
   {
      PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

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
   else
#ifdef HB_C52_STRICT
      /* NOTE: In CA-Cl*pper AFILL() is written in a manner that it will
               call AEVAL() to do the job, so the error (if any) will also be
               thrown by AEVAL().  [vszakats] */
      hb_errRT_BASE( EG_ARG, 2017, NULL, "AEVAL", 4, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
#else
      hb_errRT_BASE( EG_ARG, 9999, NULL, "AFILL", 4, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
#endif
}

HB_FUNC( ASCAN )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pValue = hb_param( 2, HB_IT_ANY );

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

/* TODO: In Xbase++ fifth parameter determines whether array elements
         are passed by reference to the code block. [vszakats] */

HB_FUNC( AEVAL )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   PHB_ITEM pBlock = hb_param( 2, HB_IT_BLOCK );

   if( pArray && pBlock )
   {
      ULONG ulStart = hb_parnl( 3 );
      ULONG ulCount = hb_parnl( 4 );

      hb_arrayEval( pArray,
                    pBlock,
                    ISNUM( 3 ) ? &ulStart : NULL,
                    ISNUM( 4 ) ? &ulCount : NULL );

      hb_itemReturn( hb_stackItemFromBase( 1 ) ); /* AEval() returns the array itself */
   }
   else
      hb_errRT_BASE( EG_ARG, 2017, NULL, "AEVAL", 4, hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
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
         ULONG ulStart = hb_parnl( 3 );
         ULONG ulCount = hb_parnl( 4 );
         ULONG ulTarget = hb_parnl( 5 );

         hb_arrayCopy( pSrcArray,
                       pDstArray,
                       ISNUM( 3 ) ? &ulStart : NULL,
                       ISNUM( 4 ) ? &ulCount : NULL,
                       ISNUM( 5 ) ? &ulTarget : NULL );
      }

      hb_itemReturn( hb_stackItemFromBase( 2 ) ); /* ACopy() returns the target array */
   }
}

/* NOTE: Clipper will return NIL if the parameter is not an array. [vszakats] */

HB_FUNC( ACLONE )
{
   PHB_ITEM pSrcArray = hb_param( 1, HB_IT_ARRAY );

   if( pSrcArray && ! hb_arrayIsObject( pSrcArray ) )
      hb_itemRelease( hb_itemReturn( hb_arrayClone( pSrcArray, NULL ) ) ); /* AClone() returns the new array */
}

