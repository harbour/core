/*
 * $Id$
 */

#include <extend.h>
#include <itemapi.h>
#include <ctoharb.h>
#include <set.h>

/* TODO: Remove these when the extend system has been modified */
#define hb_xgrab _xgrab
#define hb_xfree _xfree
#define hb_param _param

extern STACK stack;
extern SYMBOL symEval;

/* TODO: Someone make a dates.h so this isn't necessary! */
//long greg2julian( long lDay, long lMonth, long lYear );
//extern void julian2greg( long julian, long * plDay, long * plMonth, long * plYear );

BOOL hb_evalNew( PEVALINFO pEvalInfo, PHB_ITEM pItem )
{
   BOOL bResult = FALSE;

   if( pEvalInfo )
   {
      memset( pEvalInfo, 0, sizeof( EVALINFO ) );
      pEvalInfo->pItems[ 0 ] = hb_itemNew( 0 );
      ItemCopy( pEvalInfo->pItems[ 0 ], pItem );
      bResult = TRUE;
   }
   return bResult;
}

BOOL hb_evalPutParam( PEVALINFO pEvalInfo, PHB_ITEM pItem )
{
   BOOL bResult = FALSE;
   WORD w;

   if( pEvalInfo )
   {
      for( w = 1; w < 10; w++ ) /* note that 0 position is used by the codeblock or function name item */
      {
         if( ! pEvalInfo->pItems[ w ] )
         {
            pEvalInfo->pItems[ w ] = hb_itemNew( 0 );
            ItemCopy( pEvalInfo->pItems[ w ], pItem );
            bResult = TRUE;
            break;
         }
      }
   }
   return bResult;
}

BOOL hb_evalRelease( PEVALINFO pEvalInfo )
{
   BOOL bResult = FALSE;
   WORD w;

   if( pEvalInfo )
   {
      for( w = 0; w < 10; w++ )
         hb_itemRelease( pEvalInfo->pItems[ w ] );
      bResult = TRUE;
   }
   return bResult;
}

PHB_ITEM hb_evalLaunch( PEVALINFO pEvalInfo )
{
   WORD w = 1;
   PHB_ITEM pResult = 0;

   if( pEvalInfo )
   {
      if( IS_STRING( pEvalInfo->pItems[ 0 ] ) )
      {
         PushSymbol( GetDynSym( hb_itemGetC( pEvalInfo->pItems[ 0 ] ) )->pSymbol );
         PushNil();
         while( w < 10 && pEvalInfo->pItems[ w ] )
            Push( pEvalInfo->pItems[ w++ ] );
         Do( w - 1 );
         pResult = hb_itemNew( 0 );
         ItemCopy( pResult, &stack.Return );
      }
      else if( IS_BLOCK( pEvalInfo->pItems[ 0 ] ) )
      {
         PushSymbol( &symEval );
         Push( pEvalInfo->pItems[ 0 ] );
         while( w < 10 && pEvalInfo->pItems[ w ] )
            Push( pEvalInfo->pItems[ w++ ] );
         Do( w - 1 );
         pResult = hb_itemNew( 0 );
         ItemCopy( pResult, &stack.Return );
      }
   }
   return pResult;
}

PHB_ITEM hb_itemNew( PHB_ITEM pNull )
{
   PHB_ITEM pItem = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) );

   if( pNull )       /* keep the C compiler silent */
      pNull->wType = 0;   /* keep the C compiler silent */

   memset( pItem, 0, sizeof( HB_ITEM ) );
   pItem->wType = IT_NIL;

   return pItem;
}

PHB_ITEM hb_itemParam( WORD wParam )
{
   PHB_ITEM pNew = hb_itemNew( 0 );

   if( hb_param( wParam, IT_ANY ) )
      ItemCopy(pNew, hb_param( wParam, IT_ANY ));

   return pNew;
}

BOOL hb_itemRelease( PHB_ITEM pItem )
{
   BOOL bResult = FALSE;

   if( pItem )
   {
      ItemRelease(pItem);
      hb_xfree( pItem );
      bResult = TRUE;
   }
   return bResult;
}

PHB_ITEM hb_itemArrayNew( ULONG ulLen )
{
   PHB_ITEM pItem = hb_itemNew(0);

   hb_arrayNew(pItem, ulLen);

   return pItem;
}

PHB_ITEM hb_itemArrayGet( PHB_ITEM pArray, ULONG ulIndex )
{
   PHB_ITEM pItem = hb_itemNew(0);

   hb_arrayGet(pArray, ulIndex, pItem);

   return pItem;
}

PHB_ITEM hb_itemArrayPut( PHB_ITEM pArray, ULONG ulIndex, PHB_ITEM pItem )
{
   hb_arraySet(pArray, ulIndex, pItem);
   return pArray;
}

PHB_ITEM hb_itemPutC( PHB_ITEM pItem, char * szText )
{
   if( pItem )
      ItemRelease( pItem );  /* warning: this is hvm.c one not this one */
   else
      pItem = hb_itemNew(0);

   pItem->wType = IT_STRING;
   pItem->wLength = strlen( szText );
   pItem->value.szText = ( char * ) hb_xgrab( pItem->wLength + 1 );
   strcpy( pItem->value.szText, szText );
   return pItem;
}

PHB_ITEM hb_itemPutCL( PHB_ITEM pItem, char * nszText, ULONG ulLen )
{
   if( pItem )
      ItemRelease( pItem );  /* warning: this is hvm.c one not this one */
   else
      pItem = hb_itemNew(0);

   pItem->wType = IT_STRING;
   pItem->wLength = ulLen;
   pItem->value.szText = ( char * ) hb_xgrab( ulLen + 1 );
   memcpy( pItem->value.szText, nszText, ulLen );
   pItem->value.szText[ ulLen ] = 0;

   return pItem;
}

char *hb_itemGetC( PHB_ITEM pItem )
{
   if( pItem && IS_STRING( pItem ) )
   {
      char *szResult = (char *)_xgrab(pItem->wLength + 1);
      memcpy(szResult, pItem->value.szText, pItem->wLength);
      szResult[pItem->wLength] = 0;

      return szResult;
   }
   else
      return NULL;
}

ULONG hb_itemCopyC( PHB_ITEM pItem, char *szBuffer, ULONG ulLen )
{
   if( pItem && IS_STRING(pItem) )
   {
      if( !ulLen )
         ulLen = pItem->wLength;

      memcpy(szBuffer, pItem->value.szText, ulLen);
      return ulLen;
   }
   else
      return 0;
}

BOOL hb_itemFreeC( char *szText )
{
   BOOL bResult = FALSE;

   if( szText )
   {
      hb_xfree(szText);
      bResult = TRUE;
   }
   return bResult;
}

char *hb_itemGetDS( PHB_ITEM pItem, char *szDate )
{
   if( pItem && IS_DATE(pItem) )
   {
      long lDay, lMonth, lYear;
      hb_dateDecode(pItem->value.lDate, &lDay, &lMonth, &lYear);

      szDate[ 0 ] = ( lYear / 1000 ) + '0';
      szDate[ 1 ] = ( ( lYear % 1000 ) / 100 ) + '0';
      szDate[ 2 ] = ( ( lYear % 100 ) / 10 ) + '0';
      szDate[ 3 ] = ( lYear % 10 ) + '0';

      szDate[ 4 ] = ( lMonth / 10 ) + '0';
      szDate[ 5 ] = ( lMonth % 10 ) + '0';

      szDate[ 6 ] = ( lDay / 10 ) + '0';
      szDate[ 7 ] = ( lDay % 10 ) + '0';
      szDate[ 8 ] = 0;

      return szDate;
   }
   else
      return "00000000";
}

BOOL hb_itemGetL( PHB_ITEM pItem )
{
   if( pItem && IS_LOGICAL(pItem) )
   {
      return pItem->value.iLogical;
   }
   else
      return FALSE;
}

double hb_itemGetND( PHB_ITEM pItem )
{
   if( pItem )
   {
      switch( pItem->wType )
      {
         case IT_INTEGER:  return pItem->value.iNumber;
         case IT_DOUBLE:   return pItem->value.dNumber;
         case IT_LONG:     return pItem->value.lNumber;
         default:          return 0;
      }
   }
   else
      return 0;
}

long hb_itemGetNL( PHB_ITEM pItem )
{
   if( pItem )
   {
      switch( pItem->wType )
      {
         case IT_INTEGER:  return pItem->value.iNumber;
         case IT_DOUBLE:   return pItem->value.dNumber;
         case IT_LONG:     return pItem->value.lNumber;
         default:          return 0;
      }
   }
   else
      return 0;
}

PHB_ITEM hb_itemReturn( PHB_ITEM pItem )
{
   if( pItem )
      ItemCopy(&stack.Return, pItem);

   return pItem;
}

PHB_ITEM hb_itemPutDS( PHB_ITEM pItem, char *szDate )
{
   long lDay, lMonth, lYear;

   if( pItem )
      ItemRelease( pItem );  /* warning: this is hvm.c one not this one */
   else
      pItem = hb_itemNew(0);

   lDay   = ((szDate[ 6 ] - '0') * 10) + (szDate[ 7 ] - '0');
   lMonth = ((szDate[ 4 ] - '0') * 10) + (szDate[ 5 ] - '0');
   lYear  = ((szDate[ 0 ] - '0') * 1000) + ((szDate[ 1 ] - '0') * 100)
      + ((szDate[ 2 ] - '0') * 10) + (szDate[ 3 ] - '0');

   ItemRelease( pItem );
   pItem->wType   = IT_DATE;
   pItem->wLength = 8;
   /* QUESTION: Is this ok ? we are going to use a long to store the date */
   /* QUESTION: What happens if we use sizeof( LONG ) instead ? */
   /* QUESTION: Would it break Clipper language code ? */
   pItem->value.lDate = hb_dateEncode(lDay, lMonth, lYear);

   return pItem;
}

PHB_ITEM hb_itemPutL( PHB_ITEM pItem, BOOL bValue )
{
   if( pItem )
      ItemRelease( pItem );  /* warning: this is hvm.c one not this one */
   else
      pItem = hb_itemNew(0);

   pItem->wType = IT_LOGICAL;
   pItem->wLength = 3;
   pItem->value.iLogical = bValue;
   return pItem;
}

PHB_ITEM hb_itemPutND( PHB_ITEM pItem, double dNumber )
{
   if( pItem )
      ItemRelease( pItem );  /* warning: this is hvm.c one not this one */
   else
      pItem = hb_itemNew(0);

   pItem->wType = IT_DOUBLE;
   if( dNumber > 10000000000.0 ) pItem->wLength = 20;
   else pItem->wLength = 10;
   pItem->wDec    = hb_set.HB_SET_DECIMALS;
   pItem->value.dNumber = dNumber;
   return pItem;
}

PHB_ITEM hb_itemPutNL( PHB_ITEM pItem, long lNumber )
{
   if( pItem )
      ItemRelease( pItem );  /* warning: this is hvm.c one not this one */
   else
      pItem = hb_itemNew(0);

   pItem->wType = IT_DOUBLE;
   pItem->wLength = 10;
   pItem->wDec    = hb_set.HB_SET_DECIMALS;
   pItem->value.lNumber = lNumber;
   return pItem;
}

ULONG hb_itemSize( PHB_ITEM pItem )
{
   if( pItem )
   {
      switch( pItem->wType )
      {
         case IT_ARRAY:    return hb_arrayLen(pItem);
         case IT_STRING:   return pItem->wLength;
      }
   }
   return 0;
}

WORD hb_itemType( PHB_ITEM pItem )
{
   return pItem->wType;
}
