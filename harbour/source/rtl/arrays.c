/*
 * $Id$

   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
*/

/* Harbour Project source code
   http://www.Harbour-Project.org/
   The following functions are Copyright 1999 Victor Szel <info@szelvesz.hu>:
      hb_arrayIsObject()
      hb_arrayError()
      hb_arrayCopyC()
      hb_arrayGetC()
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

#include "extend.h"
#include "itemapi.h"
#include "errorapi.h"
#include "langapi.h"
#include "ctoharb.h"
#include "dates.h"

/*
 * Internal
 */

BOOL hb_arrayNew( PHB_ITEM pItem, ULONG ulLen ) /* creates a new array */
{
   PBASEARRAY pBaseArray = ( PBASEARRAY ) hb_xgrab( sizeof( BASEARRAY ) );
   ULONG ulPos;

   hb_itemClear( pItem );

   pItem->type = IT_ARRAY;

   if( ulLen > 0 )
      pBaseArray->pItems = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) * ulLen );
   else
      pBaseArray->pItems = NULL;

   pBaseArray->ulLen      = ulLen;
   pBaseArray->wHolders   = 1;
   pBaseArray->wClass     = 0;
   pBaseArray->bSuperCast = FALSE;

   for( ulPos = 0; ulPos < ulLen; ulPos++ )
      ( pBaseArray->pItems + ulPos )->type = IT_NIL;

   pItem->item.asArray.value = pBaseArray;

   return TRUE;
}

BOOL hb_arrayAdd( PHB_ITEM pArray, PHB_ITEM pValue )
{
   if( IS_ARRAY( pArray ) )
   {
      PBASEARRAY pBaseArray = ( PBASEARRAY ) pArray->item.asArray.value;

      if( pBaseArray->ulLen < ULONG_MAX )
      {
         hb_arraySize( pArray, pBaseArray->ulLen + 1 );
         pBaseArray = ( PBASEARRAY ) pArray->item.asArray.value;
         hb_itemCopy( pBaseArray->pItems + ( pBaseArray->ulLen - 1 ), pValue );

         return TRUE;
      }
   }

   return FALSE;
}

ULONG hb_arrayLen( PHB_ITEM pArray )
{
   if( IS_ARRAY( pArray ) )
      return pArray->item.asArray.value->ulLen;
   else
      return 0;
}

BOOL hb_arrayIsObject( PHB_ITEM pArray )
{
   if( IS_ARRAY( pArray ) )
      return pArray->item.asArray.value->wClass != 0;
   else
      return FALSE;
}

BOOL hb_arraySize( PHB_ITEM pArray, ULONG ulLen )
{
   if( IS_ARRAY( pArray ) )
   {
      PBASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG      ulPos;

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
   if( IS_ARRAY( pArray ) )
   {
      ULONG ulLen = pArray->item.asArray.value->ulLen;

      if( ulIndex > 0 && ulIndex <= ulLen )
      {
         PBASEARRAY pBaseArray = pArray->item.asArray.value;

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
   if( IS_ARRAY( pArray ) )
   {
      ULONG ulLen = pArray->item.asArray.value->ulLen;

      if( ulIndex > 0 && ulIndex <= ulLen )
      {
         PBASEARRAY pBaseArray = pArray->item.asArray.value;

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
   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
   {
      hb_itemGetDS( pArray->item.asArray.value->pItems + ulIndex - 1, szDate );
   }
   else
   {
      memset( szDate, ' ', 8 );
      szDate[ 8 ] = '\0';
   }

   return szDate;
}

BOOL hb_arrayGetL( PHB_ITEM pArray, ULONG ulIndex )
{
   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetL( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return FALSE;
}

int hb_arrayGetNI( PHB_ITEM pArray, ULONG ulIndex )
{
   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetNI( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

long hb_arrayGetNL( PHB_ITEM pArray, ULONG ulIndex )
{
   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetNL( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

double hb_arrayGetND( PHB_ITEM pArray, ULONG ulIndex )
{
   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetND( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

ULONG hb_arrayCopyC( PHB_ITEM pArray, ULONG ulIndex, char * szBuffer, ULONG ulLen )
{
   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemCopyC( pArray->item.asArray.value->pItems + ulIndex - 1, szBuffer, ulLen );
   else
      return 0;
}

char * hb_arrayGetC( PHB_ITEM pArray, ULONG ulIndex )
{
   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetC( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return NULL;
}

char * hb_arrayGetCPtr( PHB_ITEM pArray, ULONG ulIndex )
{
   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetCPtr( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return "";
}

ULONG hb_arrayGetCLen( PHB_ITEM pArray, ULONG ulIndex )
{
   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemGetCLen( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

USHORT hb_arrayGetType( PHB_ITEM pArray, ULONG ulIndex )
{
   if( IS_ARRAY( pArray ) && ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      return hb_itemType( pArray->item.asArray.value->pItems + ulIndex - 1 );
   else
      return 0;
}

BOOL hb_arrayLast( PHB_ITEM pArray, PHB_ITEM pResult )
{
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

BOOL hb_arrayFill( PHB_ITEM pArray, PHB_ITEM pValue, ULONG ulStart, ULONG ulCount )
{
   if( IS_ARRAY( pArray ) )
   {
      PBASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG      ulLen = pBaseArray->ulLen;

      if( ulStart == 0 )                          /* if parameter is missing */
         ulStart = 1;

      if( ulCount == 0 )                          /* if parameter is missing */
         ulCount = ulLen - ulStart + 1;

      if( ulStart + ulCount > ulLen )             /* check range */
         ulCount = ulLen - ulStart + 1;

      for( ; ulCount > 0; ulCount--, ulStart++ )     /* set value items */
         hb_itemCopy( pBaseArray->pItems + ( ulStart - 1 ), pValue );

      return TRUE;
   }
   else
      return FALSE;
}

ULONG hb_arrayScan( PHB_ITEM pArray, PHB_ITEM pValue, ULONG ulStart, ULONG ulCount )
{
   if( IS_ARRAY( pArray ) )
   {
      PBASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG      ulLen = pBaseArray->ulLen;

      if( ulStart == 0 )                       /* if parameter is missing */
         ulStart = 1;

      if( ulCount == 0 )                       /* if parameter is missing */
         ulCount = ulLen - ulStart + 1;

      if( ulStart + ulCount > ulLen )          /* check range */
         ulCount = ulLen - ulStart + 1;

      /* Make separate search loops for different types to find, so that
         the loop can be faster. */

      if( IS_BLOCK( pValue ) )
      {
         for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
         {
            hb_vmPushSymbol( &symEval );
            hb_vmPush( pValue );
            hb_vmPush( pBaseArray->pItems + ulStart );
            hb_vmDo( 1 );

            if( IS_LOGICAL( &stack.Return ) && stack.Return.item.asLogical.value )
               return ulStart + 1;                  /* arrays start from 1 */
         }
      }
      else if( IS_STRING( pValue ) )
      {
         for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
         {
            PHB_ITEM pItem = pBaseArray->pItems + ulStart;

            if( IS_STRING( pItem ) && hb_itemStrCmp( pValue, pItem, FALSE ) == 0 )
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

   return 0;
}

BOOL hb_arrayEval( PHB_ITEM pArray, PHB_ITEM bBlock, ULONG ulStart, ULONG ulCount )
{
   if( IS_ARRAY( pArray ) && IS_BLOCK( bBlock ) )
   {
      PBASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG      ulLen = pBaseArray->ulLen;

      if( ulStart == 0 )                          /* if parameter is missing */
         ulStart = 1;

      if( ulCount == 0 )                          /* if parameter is missing */
         ulCount = ulLen - ulStart + 1;

      if( ulStart + ulCount > ulLen )             /* check range */
         ulCount = ulLen - ulStart + 1;

      for( ulStart--; ulCount > 0; ulCount--, ulStart++ )
      {
         PHB_ITEM pItem = pBaseArray->pItems + ulStart;

         hb_vmPushSymbol( &symEval );
         hb_vmPush( bBlock );
         hb_vmPush( pItem );
         hb_vmPushNumber( ( double ) ( ulStart + 1 ), 0 );
         hb_vmDo( 2 );
      }

      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arrayRelease( PHB_ITEM pArray )
{
   if( IS_ARRAY( pArray ) )
   {
      PBASEARRAY pBaseArray = pArray->item.asArray.value;
      ULONG      ulLen = pBaseArray->ulLen;
      ULONG      ulPos;

      if( !pBaseArray->bSuperCast )
      {
         for( ulPos = 0; ulPos < ulLen; ulPos++ )
            hb_itemClear( pBaseArray->pItems + ulPos );

         if( pBaseArray->pItems )
            hb_xfree( pBaseArray->pItems );
      }
      hb_xfree( pBaseArray );

      pArray->type = IT_NIL;
      pArray->item.asArray.value = NULL;

      return TRUE;
   }
   else
      return FALSE;
}

BOOL hb_arrayCopy( PHB_ITEM pSrcArray, PHB_ITEM pDstArray, ULONG ulStart,
                   ULONG ulCount, ULONG ulTarget )
{
   if( IS_ARRAY( pSrcArray ) && IS_ARRAY( pDstArray ) )
   {
      PBASEARRAY pSrcBaseArray = pSrcArray->item.asArray.value;
      PBASEARRAY pDstBaseArray = pDstArray->item.asArray.value;
      ULONG      ulSrcLen = pSrcBaseArray->ulLen;
      ULONG      ulDstLen = pDstBaseArray->ulLen;

      if( ulStart == 0 )                          /* if parameter is missing */
         ulStart = 1;

      if( ulTarget == 0 )                         /* if parameter is missing */
         ulTarget = 1;

      if( ulCount == 0 )                          /* if parameter is missing */
         ulCount = ulSrcLen - ulStart + 1;

      if( ulStart + ulCount > ulSrcLen )          /* check range */
         ulCount = ulSrcLen - ulStart + 1;

      if( ulCount > ulDstLen )
         ulCount = ulDstLen;

      for( ulTarget--, ulStart--; ulCount > 0; ulCount--, ulStart++ )
         hb_itemCopy( pDstBaseArray->pItems + ( ulTarget + ulStart ), pSrcBaseArray->pItems + ulStart );

      return TRUE;
   }
   else
      return FALSE;
}

PHB_ITEM hb_arrayClone( PHB_ITEM pSrcArray )
{
   PHB_ITEM pDstArray = hb_itemNew( NULL );

   if( IS_ARRAY( pSrcArray ) )
   {
      PBASEARRAY pSrcBaseArray = pSrcArray->item.asArray.value;
      PBASEARRAY pDstBaseArray;
      ULONG      ulSrcLen = pSrcBaseArray->ulLen;
      ULONG      ulCount;

      hb_arrayNew( pDstArray, ulSrcLen );

      pDstBaseArray = pDstArray->item.asArray.value;
      pDstBaseArray->wClass = pSrcBaseArray->wClass;

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

/* TOFIX: Clipper will not work with OBJECT type in these functions. */

/* TODO: Support multiple dimensions */

HARBOUR HB_ARRAY( void )
{
   int iParCount = hb_pcount();

   if( iParCount > 0 )
   {
      int tmp;
      BOOL bError = FALSE;

      for( tmp = 1; tmp <= iParCount; tmp++ )
      {
         if( ! ISNUM( tmp ) )
         {
            bError = TRUE;
            break;
         }

         if( hb_parnl( tmp ) < 0 )
            hb_errRT_BASE( EG_BOUND, 1131, NULL, hb_langDGetErrorDesc( EG_ARRDIMENSION ) );
      }

      if( ! bError )
         hb_arrayNew( &stack.Return, hb_parnl( 1 ) );
   }
}

HARBOUR HB_AADD( void )
{
   if( hb_pcount() == 2 )
   {
      PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

      if( pArray )
      {
         PHB_ITEM pValue = hb_param( 2, IT_ANY );

         if( hb_arrayAdd( pArray, pValue ) )
            hb_itemReturn( pValue );
         else
            hb_errRT_BASE( EG_BOUND, 1187, NULL, "AADD" );
      }
      else
         hb_errRT_BASE( EG_ARG, 1123, NULL, "AADD" );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "AADD" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_ASIZE( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

   if( pArray && ISNUM( 2 ) )
   {
      LONG lSize = hb_parnl( 2 );

      hb_arraySize( pArray, MAX( lSize, 0 ) );

      hb_itemReturn( pArray ); /* ASize() returns the array itself */
   }
}

HARBOUR HB_ATAIL( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

   if( pArray )
      hb_arrayLast( pArray, &stack.Return );
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

/* TOFIX: nCount parameter == zero is incompatible. */

HARBOUR HB_AFILL( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );

   if( pArray )
   {
      hb_arrayFill( pArray, hb_param( 2, IT_ANY ), hb_parnl( 3 ), hb_parnl( 4 ) );
      hb_itemReturn( pArray ); /* AFill() returns the array itself */
   }
}

/* TOFIX: nCount parameter == zero is incompatible. */

HARBOUR HB_ASCAN( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );
   PHB_ITEM pValue = hb_param( 2, IT_ANY );

   if( pArray && pValue )
      hb_retnl( hb_arrayScan( pArray, pValue, hb_parnl( 3 ), hb_parnl( 4 ) ) );
   else
      hb_retnl( 0 );
}

/* TOFIX: nCount parameter == zero is incompatible. */

HARBOUR HB_AEVAL( void )
{
   PHB_ITEM pArray = hb_param( 1, IT_ARRAY );
   PHB_ITEM pBlock = hb_param( 2, IT_BLOCK );

   if( pArray && pBlock )
   {
      hb_arrayEval( pArray, pBlock, hb_parnl( 3 ), hb_parnl( 4 ) );
      hb_itemReturn( pArray ); /* AEval() returns the array itself */
   }
   else
      hb_errRT_BASE( EG_ARG, 2017, NULL, "AEVAL" );
}

/* TOFIX: nCount parameter == zero is incompatible. */

HARBOUR HB_ACOPY( void )
{
   PHB_ITEM pSrcArray = hb_param( 1, IT_ARRAY );
   PHB_ITEM pDstArray = hb_param( 2, IT_ARRAY );

   if( pSrcArray && pDstArray )
   {
      hb_arrayCopy( pSrcArray, pDstArray, hb_parnl( 3 ), hb_parnl( 4 ), hb_parnl( 5 ) );
      hb_itemReturn( pDstArray ); /* ACopy() returns the target array */
   }
}

/* NOTE: Clipper will return NIL if the parameter is not an array */

HARBOUR HB_ACLONE( void )
{
   PHB_ITEM pSrcArray = hb_param( 1, IT_ARRAY );

   if( pSrcArray )
   {
      PHB_ITEM pDstArray = hb_arrayClone( pSrcArray );
      hb_itemReturn( pDstArray ); /* AClone() returns the new array */
      hb_itemRelease( pDstArray );
   }
}

