#include <extend.h>
#include <ctoharb.h>

extern STACK  stack;
extern SYMBOL symEval;

// -------------
// Internal
// -------------
void Array( PITEM pItem, ULONG ulLen ) /* creates a new array */
{
   PBASEARRAY pBaseArray = ( PBASEARRAY ) _xgrab( sizeof( BASEARRAY ) );
   ULONG ul;

   ItemRelease( pItem );

   pItem->wType = IT_ARRAY;

   if( ulLen )
	  pBaseArray->pItems = ( PITEM ) _xgrab( sizeof( ITEM ) * ulLen );
   else
	  pBaseArray->pItems = 0;

   pBaseArray->ulLen    = ulLen;
   pBaseArray->wHolders = 1;
   pBaseArray->wClass   = 0;

   for( ul = 0; ul < ulLen; ul++ )
	  ( pBaseArray->pItems + ul )->wType = IT_NIL;

   pItem->value.pBaseArray = pBaseArray;
}

void ArrayAdd( PITEM pArray, PITEM pValue )
{
   PBASEARRAY pBaseArray = ( PBASEARRAY ) pArray->value.pBaseArray;
   ArraySize( pArray, pBaseArray->ulLen + 1 );
   ItemCopy( pBaseArray->pItems + ( pBaseArray->ulLen - 1 ), pValue );
}

void ArrayGet( PITEM pArray, ULONG ulIndex, PITEM pItem )
{
   if( IS_ARRAY( pArray ) )
   {
	  if( ulIndex <= ( unsigned ) ArrayLen( pArray ) )
		 ItemCopy( pItem,
		  ( ( PBASEARRAY ) pArray->value.pBaseArray )->pItems + ( ulIndex - 1 ) );
	  else
	  {
		 printf( "Error: Array access out of bounds\n" );
		 exit( 1 );
	  }
   }
   /* QUESTION: Should we raise an error here ? */
}

char * ArrayGetString( PITEM pArray, ULONG ulIndex )
{
   PITEM pItem, pError;

   if( IS_ARRAY( pArray ) )
   {
	  if( ulIndex <= ( unsigned ) ArrayLen( pArray ) )
      {
         pItem = ( ( PBASEARRAY ) pArray->value.pBaseArray )->pItems + ulIndex - 1;

         if( IS_STRING( pItem ) )
            return pItem->value.szText;
         else
            return "";
      }
      else
	  {
         pError = _errNew();
         _errPutDescription( pError, "Bound error: Array access" );
         _errLaunch( pError );
         _errRelease( pError );
      }
   }
   return "";
}

ULONG ArrayGetStringLen( PITEM pArray, ULONG ulIndex )
{
   PITEM pItem, pError;

   if( IS_ARRAY( pArray ) )
   {
	  if( ulIndex <= ( unsigned ) ArrayLen( pArray ) )
      {
         pItem = ( ( PBASEARRAY ) pArray->value.pBaseArray )->pItems + ulIndex - 1;

         if( IS_STRING( pItem ) )
            return pItem->wLength;
         else
            return 0;
      }
      else
	  {
         pError = _errNew();
         _errPutDescription( pError, "Bound error: Array access" );
         _errLaunch( pError );
         _errRelease( pError );
      }
   }
   return 0;
}

void ArrayLast( PITEM pArray, PITEM pResult )
{
   if( ( ( PBASEARRAY ) pArray->value.pBaseArray )->ulLen )
	  ItemCopy( pResult, ( ( PBASEARRAY ) pArray->value.pBaseArray )->pItems +
		( ( ( PBASEARRAY ) pArray->value.pBaseArray )->ulLen - 1 ) );
   /* QUESTION: Should we raise an error here ? */
}

int ArrayLen( PITEM pArray )
{
   if( IS_ARRAY( pArray ) )
	  return ( ( PBASEARRAY ) pArray->value.pBaseArray )->ulLen;

   /* QUESTION: Should we raise an error here ? */
   return 0;
}

void ArraySet( PITEM pArray, ULONG ulIndex, PITEM pItem )
{
   if( IS_ARRAY( pArray ) )
   {
	  if( ulIndex <= ( unsigned ) ArrayLen( pArray ) )
		 ItemCopy( ( ( PBASEARRAY ) pArray->value.pBaseArray )->pItems + ( ulIndex - 1 ),
				 pItem );
	  else
	  {
		 printf( "Error: Array access out of bounds\n" );
		 exit( 1 );
	  }
   }
   /* QUESTION: Should we raise an error here ? */
}

void ArraySize( PITEM pArray, ULONG ulLen )
{
   if ( IS_ARRAY( pArray ) )
   {
	  PBASEARRAY pBaseArray = ( PBASEARRAY ) pArray->value.pBaseArray;
	  ULONG      ul;

	  if( ! pBaseArray->ulLen )
	  {
		 pBaseArray->pItems = ( PITEM ) _xgrab( ulLen * sizeof( ITEM ) );
		 for ( ul = 0; ul < ulLen; ul ++ )
			( pBaseArray->pItems + ul )->wType = IT_NIL;
	  }
	  else
	  {
		 if( pBaseArray->ulLen < ulLen )
		 {
			pBaseArray->pItems = ( PITEM )_xrealloc( pBaseArray->pItems,
													 sizeof( ITEM ) * ulLen );

			/* set value for new items */
			for( ul = pBaseArray->ulLen; ul < ulLen; ul++ )
			   ( pBaseArray->pItems + ul )->wType = IT_NIL;
		 }
		 else if( pBaseArray->ulLen > ulLen )
		 {
			/* release old items */
			for( ul = ulLen; ul < pBaseArray->ulLen; ul++ )
			   ItemRelease( pBaseArray->pItems + ul );

			pBaseArray->pItems = ( PITEM )_xrealloc( pBaseArray->pItems,
													 sizeof( ITEM ) * ulLen );
		 }
	  }
	  pBaseArray->ulLen = ulLen;
   }
   /* QUESTION: Should we raise an error here ? */
}

void ArrayFill( PITEM pArray, PITEM pValue, ULONG ulStart, ULONG ulCount )
{
   if ( IS_ARRAY( pArray ) )
   {
	  PBASEARRAY pBaseArray;
	  ULONG      ulLen = ArrayLen( pArray );

	  if ( ulStart == 0 )                         /* if parameter is missing */
		 ulStart = 1;

	  if ( ulCount == 0 )                         /* if parameter is missing */
		 ulCount = ulLen - ulStart + 1;

	  if ( ulStart + ulCount > ulLen )            /* check range */
		 ulCount = ulLen - ulStart + 1;

	  pBaseArray = ( PBASEARRAY )pArray->value.pBaseArray;

	  for ( ; ulCount > 0; ulCount --, ulStart ++ )     /* set value items */
		 ItemCopy( pBaseArray->pItems + ( ulStart - 1 ), pValue );
   }
   /* QUESTION: Should we raise an error here ? */
}

void ArrayDel( PITEM pArray, ULONG ulIndex )
{
   if ( IS_ARRAY( pArray ) )
   {
	  ULONG ulLen = ArrayLen( pArray );

	  if ( ulIndex > 0 && ulIndex <= ulLen )
	  {
		 PBASEARRAY pBaseArray = ( PBASEARRAY )pArray->value.pBaseArray;

		 ItemRelease( pBaseArray->pItems + ( ulIndex - 1 ) );

		 for ( ulIndex --; ulIndex < ulLen; ulIndex ++ )       /* move items */
			ItemCopy( pBaseArray->pItems + ulIndex,
				  pBaseArray->pItems + ( ulIndex + 1 ) );

		 ItemRelease( pBaseArray->pItems + ( ulLen - 1 ) );
//         ( pBaseArray->pItems + ( ulLen - 1 ) )->wType = IT_NIL;
	  }
   }
   /* QUESTION: Should we raise an error here ? */
}

void ArrayIns( PITEM pArray, ULONG ulIndex )
{
   if ( IS_ARRAY( pArray ) )
   {
	  ULONG ulLen = ArrayLen( pArray );

	  if ( ulIndex > 0 && ulIndex <= ulLen )
	  {
		 PBASEARRAY pBaseArray = ( PBASEARRAY )pArray->value.pBaseArray;

		 ItemRelease( pBaseArray->pItems + ( ulLen - 1 ) );

		 for ( ulLen --; ulLen >= ulIndex; ulLen -- )          /* move items */
			ItemCopy( pBaseArray->pItems + ulLen,
					  pBaseArray->pItems + ( ulLen - 1 ) );

		 ItemRelease( pBaseArray->pItems + ulLen );
//         ( pBaseArray->pItems + ulLen )->wType = IT_NIL;    /* set nil value */
	  }
   }
}

int ArrayScan( PITEM pArray, PITEM pValue, ULONG ulStart, ULONG ulCount )
{
   if ( IS_ARRAY( pArray ) && pValue->wType != IT_NIL )
   {
	  int        iRet = 0;
	  PBASEARRAY pBaseArray;
	  ULONG      ulLen = ArrayLen( pArray );

	  if ( ulStart == 0 )                         /* if parameter is missing */
		 ulStart = 1;

	  if ( ulCount == 0 )                         /* if parameter is missing */
		 ulCount = ulLen - ulStart + 1;

	  if ( ulStart + ulCount > ulLen )            /* check range */
		 ulCount = ulLen - ulStart + 1;

	  pBaseArray = ( PBASEARRAY )pArray->value.pBaseArray;

	  for ( ulStart --; ulCount > 0; ulCount --, ulStart ++ )
	  {
		 PITEM pItem = pBaseArray->pItems + ulStart;

		 if ( pValue->wType == IT_BLOCK )
		 {
			PushSymbol( &symEval );
			Push( pValue );
			Push( pItem );
			Do( 1 );
			if ( stack.Return.value.iLogical )
			   iRet = 1;
		 }
		 else
		 {
		   if ( pValue->wType == pItem->wType )
		   {
			  switch( pItem->wType )
			  {
				 case IT_INTEGER :
					iRet = ( pValue->value.iNumber == pItem->value.iNumber );
					break;

				 case IT_LONG :
					iRet = ( pValue->value.lNumber == pItem->value.lNumber );
					break;

				 case IT_DOUBLE :
					iRet = ( pValue->value.dNumber == pItem->value.dNumber );
					break;

				 case IT_DATE :
					iRet = ( pValue->value.lDate == pItem->value.lDate );
					break;

				 case IT_LOGICAL :
					iRet = ( pValue->value.iLogical == pItem->value.iLogical );
					break;

				 case IT_STRING :
					iRet = ( OurStrCmp( pValue, pItem ) == 0 );
					break;
			  }
		   }
		 }
		 if ( iRet )
			return ulStart + 1;                  /* arrays start from 1 */
	  }
   }
   return 0;
}

void ArrayEval( PITEM pArray, PITEM bBlock, ULONG ulStart, ULONG ulCount )
{
   if ( IS_ARRAY( pArray ) && IS_BLOCK( bBlock ) )
   {
	  PBASEARRAY pBaseArray;
	  ULONG      ulLen = ArrayLen( pArray );

	  if ( ulStart == 0 )                         /* if parameter is missing */
		 ulStart = 1;

	  if ( ulCount == 0 )                         /* if parameter is missing */
		 ulCount = ulLen - ulStart + 1;

	  if ( ulStart + ulCount > ulLen )            /* check range */
		 ulCount = ulLen - ulStart + 1;

	  pBaseArray = ( PBASEARRAY )pArray->value.pBaseArray;

	  for ( ulStart --; ulCount > 0; ulCount --, ulStart ++ )
	  {
		 PITEM pItem = pBaseArray->pItems + ulStart;

		 PushSymbol( &symEval );
		 Push( bBlock );
		 Push( pItem );
		 Do( 1 );
	  }
   }
}

void ArrayRelease( PITEM pArray )
{
   if ( IS_ARRAY( pArray ) )
   {
	  ULONG      ul, ulLen  = ArrayLen( pArray );
	  PBASEARRAY pBaseArray = ( PBASEARRAY )pArray->value.pBaseArray;

	  for ( ul = 0; ul < ulLen; ul ++ )
		 ItemRelease( pBaseArray->pItems + ul );

      if( pBaseArray->pItems )
         _xfree( pBaseArray->pItems );
	  _xfree( pBaseArray );

	  pArray->wType = IT_NIL;
	  pArray->value.pBaseArray = NULL;
   }
}

// -------------
// HARBOUR
// -------------
HARBOUR ARRAY( void )
{
   Array( &stack.Return, _parnl( 1 ) );
}

HARBOUR AADD( void )
{
   PITEM pArray = _param( 1, IT_ARRAY );
   PITEM pValue = _param( 2, 0xFFFF ); /* any type */

   if ( pArray )
	  ArrayAdd( pArray, pValue );

   ItemCopy( &stack.Return, pValue );
}

HARBOUR ASIZE( void )
{
   PITEM pArray = _param( 1, IT_ARRAY );

   if ( pArray )
   {
	  ArraySize( pArray, _parnl( 2 ) );
	  ItemCopy( &stack.Return, pArray );  /* ASize() returns the array itself */
   }
   else
	  _ret();    /* QUESTION: Should we raise an error here ? */
}

HARBOUR ATAIL( void )
{
   PITEM pArray = _param( 1, IT_ARRAY );

   if ( pArray )
	  ArrayLast( pArray, &stack.Return );
   else
	  _ret();  /* QUESTION: Should we raise an error here ? */
}

HARBOUR AINS( void )
{
   PITEM pArray  = _param( 1, IT_ARRAY );

   if ( pArray )
   {
	  ArrayIns( pArray, _parnl( 2 ) );
	  ItemCopy( &stack.Return, pArray );  /* AIns() returns the array itself */
   }
   else
	  _ret();
}

HARBOUR ADEL( void )
{
   PITEM pArray  = _param( 1, IT_ARRAY );

   if ( pArray )
   {
	  ArrayDel( pArray, _parnl( 2 ) );
	  ItemCopy( &stack.Return, pArray ); /* ADel() returns the array itself */
   }
   else
	  _ret();
}

HARBOUR AFILL( void )
{
   PITEM pArray  = _param( 1, IT_ARRAY );

   if ( pArray )
   {
	  ArrayFill( pArray, _param( 2, IT_ANY ), _parnl( 3 ), _parnl( 4 ) );
	  ItemCopy( &stack.Return, pArray ); /* AFill() returns the array itself */
   }
   else
	  _ret();
}

HARBOUR ASCAN( void )
{
   PITEM pArray = _param( 1, IT_ARRAY );

   if ( pArray )
	  _retnl( ArrayScan( pArray, _param( 2, IT_ANY ), _parnl( 3 ), _parnl( 4 ) ) );
   else
	  _retnl( 0 );
}

HARBOUR AEVAL( void )
{
   PITEM pArray = _param( 1, IT_ARRAY );
   PITEM bBlock = _param( 2, IT_BLOCK );

   if ( pArray )
   {
	  ArrayEval( pArray, bBlock, _parnl( 3 ), _parnl( 4 ) );
	  ItemCopy( &stack.Return, pArray ); /* AEval() returns the array itself */
   }
   else
	  _ret();
}
