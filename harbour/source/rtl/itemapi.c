#include <extend.h>
#include <itemapi.h>
#include <ctoharb.h>

extern STACK stack;
extern PSYMBOL symEval;

/* TODO: Someone make a dates.h so this isn't necessary! */
long greg2julian( long lDay, long lMonth, long lYear );
extern void julian2greg( long julian, long * plDay, long * plMonth, long * plYear );

BOOL _evalNew( PEVALINFO pEvalInfo, PITEM pItem )
{
   BOOL bResult = FALSE;

   if( pEvalInfo )
   {
      memset( pEvalInfo, 0, sizeof( EVALINFO ) );
      pEvalInfo->pItems[ 0 ] = _itemNew( 0 );
      ItemCopy( pEvalInfo->pItems[ 0 ], pItem );
      bResult = TRUE;
   }
   return bResult;
}

BOOL _evalPutParam( PEVALINFO pEvalInfo, PITEM pItem )
{
   BOOL bResult = FALSE;
   WORD w;

   if( pEvalInfo )
   {
      for( w = 1; w < 10; w++ ) /* note that 0 position is used by the codeblock or function name item */
      {
         if( ! pEvalInfo->pItems[ w ] )
         {
            pEvalInfo->pItems[ w ] = _itemNew( 0 );
            ItemCopy( pEvalInfo->pItems[ w ], pItem );
            bResult = TRUE;
            break;
         }
      }
   }
   return bResult;
}

BOOL _evalRelease( PEVALINFO pEvalInfo )
{
   BOOL bResult = FALSE;
   WORD w;

   if( pEvalInfo )
   {
      for( w = 0; w < 10; w++ )
         _itemRelease( pEvalInfo->pItems[ w ] );
      bResult = TRUE;
   }
   return bResult;
}

PITEM _evalLaunch( PEVALINFO pEvalInfo )
{
   WORD w = 1;
   PITEM pResult = 0;

   if( pEvalInfo )
   {
      if( IS_STRING( pEvalInfo->pItems[ 0 ] ) )
      {
         PushSymbol( GetDynSym( _itemGetC( pEvalInfo->pItems[ 0 ] ) )->pSymbol );
         PushNil();
         while( w < 10 && pEvalInfo->pItems[ w ] )
            Push( pEvalInfo->pItems[ w++ ] );
         Do( w - 1 );
         pResult = _itemNew( 0 );
         ItemCopy( pResult, &stack.Return );
      }
      else if( IS_BLOCK( pEvalInfo->pItems[ 0 ] ) )
      {
         PushSymbol( symEval );
         Push( pEvalInfo->pItems[ 0 ] );
         while( w < 10 && pEvalInfo->pItems[ w ] )
            Push( pEvalInfo->pItems[ w++ ] );
         Do( w - 1 );
         pResult = _itemNew( 0 );
         ItemCopy( pResult, &stack.Return );
      }
   }
   return pResult;
}

PITEM _itemNew( PITEM pNull )
{
   PITEM pItem = ( PITEM ) _xgrab( sizeof( ITEM ) );

   if( pNull )       /* keep the C compiler silent */
      pNull->wType = 0;   /* keep the C compiler silent */

   memset( pItem, 0, sizeof( ITEM ) );
   pItem->wType = IT_NIL;

   return pItem;
}

PITEM _itemParam( WORD wParam )
{
   PITEM pNew = _itemNew( 0 );

   if( _param( wParam, IT_ANY ) )
      ItemCopy( pNew, _param( wParam, IT_ANY ) );

   return pNew;
}

BOOL _itemRelease( PITEM pItem )
{
   BOOL bResult = FALSE;

   if( pItem )
   {
      _xfree( pItem );
      bResult = TRUE;
   }
   return bResult;
}

PITEM _itemArrayNew( ULONG ulLen )
{
   PITEM pItem = _itemNew(0);

   Array(pItem, ulLen);

   return pItem;
}

PITEM _itemArrayGet( PITEM pArray, ULONG ulIndex )
{
   PITEM pItem = _itemNew(0);

   ArrayGet(pArray, ulIndex, pItem);

   return pItem;
}

PITEM _itemArrayPut( PITEM pArray, ULONG ulIndex, PITEM pItem )
{
   ArraySet(pArray, ulIndex, pItem);
   return pArray;
}

PITEM _itemPutC( PITEM pItem, char * szText )
{
   if( pItem )
   {
      ItemRelease( pItem );  /* warning: this is hvm.c one not this one */
      pItem->wType = IT_STRING;
      pItem->wLength = strlen( szText );
      pItem->value.szText = ( char * ) _xgrab( pItem->wLength + 1 );
      strcpy( pItem->value.szText, szText );
   }
   return pItem;
}

PITEM _itemPutCL( PITEM pItem, char * nszText, ULONG ulLen )
{
   if( pItem )
   {
      ItemRelease( pItem );  /* warning: this is hvm.c one not this one */
      pItem->wType = IT_STRING;
      pItem->wLength = ulLen;
      pItem->value.szText = ( char * ) _xgrab( ulLen + 1 );
      memcpy( pItem->value.szText, nszText, ulLen );
      pItem->value.szText[ ulLen ] = 0;
   }
   return pItem;
}

char *_itemGetC( PITEM pItem )
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

ULONG _itemCopyC( PITEM pItem, char *szBuffer, ULONG ulLen )
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

BOOL _itemFreeC( char *szText )
{
   BOOL bResult = FALSE;

   if( szText )
   {
      _xfree(szText);
      bResult = TRUE;
   }
   return bResult;
}

char *_itemGetDS( PITEM pItem, char *szDate )
{
   if( pItem && IS_DATE(pItem) )
   {
      long lDay, lMonth, lYear;
      julian2greg(pItem->value.lDate, &lDay, &lMonth, &lYear);

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

BOOL _itemGetL( PITEM pItem )
{
   if( pItem && IS_LOGICAL(pItem) )
   {
      return pItem->value.iLogical;
   }
   else
      return FALSE;
}

double _itemGetND( PITEM pItem )
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

long _itemGetNL( PITEM pItem )
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

PITEM _itemReturn( PITEM pItem )
{
   if( pItem )
      ItemCopy(&stack.Return, pItem);

   return pItem;
}

PITEM _itemPutDS( PITEM pItem, char *szDate )
{
   if( pItem )
   {
      long lDay, lMonth, lYear;

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
      pItem->value.lDate = greg2julian(lDay, lMonth, lYear);
   }
   return pItem;
}

PITEM _itemPutL( PITEM pItem, BOOL bValue )
{
   if( pItem )
   {
      ItemRelease( pItem );  /* warning: this is hvm.c one not this one */
      pItem->wType = IT_LOGICAL;
      pItem->wLength = 1;
      pItem->value.iLogical = bValue;
   }
   return pItem;
}

PITEM _itemPutND( PITEM pItem, double dNumber )
{
   if( pItem )
   {
      ItemRelease( pItem );  /* warning: this is hvm.c one not this one */
      pItem->wType = IT_DOUBLE;
      pItem->wLength = sizeof( double );
      pItem->value.dNumber = dNumber;
   }
   return pItem;
}

PITEM _itemPutNL( PITEM pItem, long lNumber )
{
   if( pItem )
   {
      ItemRelease( pItem );  /* warning: this is hvm.c one not this one */
      pItem->wType = IT_DOUBLE;
      pItem->wLength = sizeof( double );
      pItem->value.lNumber = lNumber;
   }
   return pItem;
}


ULONG _itemSize( PITEM pItem )
{
   return pItem->wLength;
}

WORD _itemType( PITEM pItem )
{
   return pItem->wType;
}
