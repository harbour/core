/*
 * $Id$
 */

#include <malloc.h>
#include <set.h>
#include <stdlib.h>
#include <extend.h>
#include <dates.h>

extern STACK stack;

ULONG ulMemoryBlocks = 0;
ULONG ulMemoryMaxBlocks = 0;
ULONG ulMemoryMaxConsumed = 0;
ULONG ulMemoryConsumed = 0;

PHB_ITEM hb_param( WORD wParam, WORD wMask )
{
   WORD wType;
   PHB_ITEM pLocal;

   if( wParam <= hb_pcount() )
   {
      wType = ( stack.pBase + 1 + wParam )->type;

      if( ( wType & wMask ) || ( wType == IT_NIL && wMask == IT_ANY ) )
      {
         pLocal = stack.pBase + 1 + wParam;
         if( wType & IT_BYREF )
            return stack.pItems + pLocal->item.asRefer.value;
         else
            return pLocal;
      }
      else
         return 0;
   }
   return 0;
}

char * hb_parc( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->type & IT_BYREF )
         pItem = stack.pItems + pItem->item.asRefer.value;

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return hb_arrayGetString( pItem, wArrayIndex );
         else
            return "";
      }
      else if( IS_STRING( pItem ) )
         return pItem->item.asString.value;

      else
         return "";
   }
   return "";
}

ULONG hb_parclen( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->type & IT_BYREF )
         pItem = stack.pItems + pItem->item.asRefer.value;

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return hb_arrayGetStringLen( pItem, wArrayIndex );
         else
            return 0;
      }
      else if( IS_STRING( pItem ) )
         return pItem->item.asString.length;

      else
         return 0;
   }
   return 0;
}

char * hb_pards( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;
   long lDay, lMonth, lYear;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->type & IT_BYREF )
         pItem = stack.pItems + pItem->item.asRefer.value;
         
      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return strcpy( stack.szDate, hb_arrayGetDate( pItem, wArrayIndex ) );
         else
            return "        ";
      }         

        else if( IS_DATE( pItem ) && pItem->item.asDate.value > 0 )
      {
         hb_dateDecode( pItem->item.asDate.value, &lDay, &lMonth, &lYear );

         stack.szDate[ 0 ] = ( lYear / 1000 ) + '0';
         stack.szDate[ 1 ] = ( ( lYear % 1000 ) / 100 ) + '0';
         stack.szDate[ 2 ] = ( ( lYear % 100 ) / 10 ) + '0';
         stack.szDate[ 3 ] = ( lYear % 10 ) + '0';

         stack.szDate[ 4 ] = ( lMonth / 10 ) + '0';
         stack.szDate[ 5 ] = ( lMonth % 10 ) + '0';

         stack.szDate[ 6 ] = ( lDay / 10 ) + '0';
         stack.szDate[ 7 ] = ( lDay % 10 ) + '0';
         stack.szDate[ 8 ] = 0;

         return stack.szDate; /* this guaranties good behavior when multithreading */
      }
      else
         return "        ";
   }
   return "        ";
}

int hb_parl( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->type & IT_BYREF )
         pItem = stack.pItems + pItem->item.asRefer.value;

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return hb_arrayGetBool( pItem, wArrayIndex );
         else
            return 0;
      }
      
      else if( IS_LOGICAL( pItem ) )
         return pItem->item.asLogical.value;

      else
         return 0;
   }
   return 0;
}

double hb_parnd( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->type & IT_BYREF )
         pItem = stack.pItems + pItem->item.asRefer.value;

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return hb_arrayGetDouble( pItem, wArrayIndex );
         else
            return 0;
      }
      else if( IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value;

      else if( IS_LONG( pItem ) )
         return pItem->item.asLong.value;

      else if( IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value;

      else
         return 0;
   }
   return 0;
}

int hb_parni( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->type & IT_BYREF )
         pItem = stack.pItems + pItem->item.asRefer.value;
         
      if( IS_ARRAY( pItem ) )
      {
	 if( wArrayIndex )
	    return (long) hb_arrayGetDouble( pItem, wArrayIndex );
	 else
	    return 0;
      }

      else if( IS_INTEGER( pItem ) )
         return pItem->item.asInteger.value;

      else if( IS_LONG( pItem ) )
         return pItem->item.asLong.value;

      else if( IS_DOUBLE( pItem ) )
         return pItem->item.asDouble.value;

      else
         return 0;
   }
   return 0;
}

long hb_parnl( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->type & IT_BYREF )
         pItem = stack.pItems + pItem->item.asRefer.value;

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
	    return (long) hb_arrayGetDouble( pItem, wArrayIndex );
         else
            return 0;
      }

      else if( IS_INTEGER( pItem ) )
         return (long) pItem->item.asInteger.value;

      else if( IS_LONG( pItem ) )
         return pItem->item.asLong.value;

      else if( IS_DOUBLE( pItem ) )
         return (long) pItem->item.asDouble.value;

      else
         return 0;
   }
   return 0;
}

int hb_parinfa( int iParamNum, ULONG uiArrayIndex )
{
   PHB_ITEM pArray = hb_param( iParamNum, IT_ARRAY );

   if( pArray )
   {
      if( ! uiArrayIndex )
         return hb_arrayLen( pArray );
      else
         return hb_arrayGetType( pArray, uiArrayIndex );
   }
   else
      return 0; /* QUESTION: should we raise an error here ? */
}

WORD hb_parinfo( WORD wParam )
{
   if( ! wParam )
      return stack.pBase->item.asSymbol.paramcnt;
   else
   {
      if( wParam <= hb_pcount() )
         return ( stack.pBase + 1 + wParam )->type;
      else
         return 0;
   }
}

WORD hb_pcount( void )
{
   return stack.pBase->item.asSymbol.paramcnt;
}

void hb_ret( void )
{
   ItemRelease( &stack.Return );
}

void hb_reta( ULONG ulLen )  /* undocumented hb_reta() */
{
   hb_arrayNew( &stack.Return, ulLen );
}

void hb_retc( char * szText )
{
   ULONG ulLen = strlen( szText );

   ItemRelease( &stack.Return );
   stack.Return.type   = IT_STRING;
   stack.Return.item.asString.length = ulLen;
   stack.Return.item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
   strcpy( stack.Return.item.asString.value, szText );
}

void hb_retclen( char * szText, ULONG ulLen )
{
   ItemRelease( &stack.Return );
   stack.Return.type   = IT_STRING;
   stack.Return.item.asString.length = ulLen;
   stack.Return.item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
   memcpy( stack.Return.item.asString.value, szText, ulLen );
   stack.Return.item.asString.value[ ulLen ] = 0;
}

void hb_retds( char * szDate ) /* szDate must have yyyymmdd format */
{
   long lDay, lMonth, lYear;

   if( szDate && strlen( szDate ) == 8 )
   {
      /* Date string has correct length, so attempt to convert */
      lDay   = ( ( szDate[ 6 ] - '0' ) * 10 ) + ( szDate[ 7 ] - '0' );
      lMonth = ( ( szDate[ 4 ] - '0' ) * 10 ) + ( szDate[ 5 ] - '0' );
      lYear  = ( ( szDate[ 0 ] - '0' ) * 1000 ) + ( ( szDate[ 1 ] - '0' ) * 100 ) +
               ( ( szDate[ 2 ] - '0' ) * 10 ) + ( szDate[ 3 ] - '0' );
   }
   else lDay = lMonth = lYear = 0; /* Date string missing or bad length,
                                      so force an empty date */

   ItemRelease( &stack.Return );

   stack.Return.type   = IT_DATE;
   stack.Return.item.asDate.length = 8;
   /* QUESTION: Is this ok ? we are going to use a long to store the date */
   /* QUESTION: What happens if we use sizeof( LONG ) instead ? */
   /* QUESTION: Would it break Clipper language code ? */
   stack.Return.item.asDate.value = hb_dateEncode( lDay, lMonth, lYear );
}

void hb_retnd( double dNumber )
{
   ItemRelease( &stack.Return );
   stack.Return.type   = IT_DOUBLE;
   if( dNumber > 10000000000.0 )
      stack.Return.item.asDouble.length = 20;
   else
      stack.Return.item.asDouble.length = 10;
   stack.Return.item.asDouble.decimal   = hb_set.HB_SET_DECIMALS;
   stack.Return.item.asDouble.value     = dNumber;
}

void hb_retni( int iNumber )
{
   ItemRelease( &stack.Return );
   stack.Return.type                   = IT_INTEGER;
   stack.Return.item.asInteger.length  = 10;
   stack.Return.item.asInteger.decimal = 0;
   stack.Return.item.asInteger.value   = iNumber;
}

void hb_retl( int iTrueFalse )
{
   ItemRelease( &stack.Return );
   stack.Return.type                  = IT_LOGICAL;
   stack.Return.item.asLogical.length = 3;
   stack.Return.item.asLogical.value  = iTrueFalse;
}

void hb_retnl( long lNumber )
{
   ItemRelease( &stack.Return );
   stack.Return.type                = IT_LONG;
   stack.Return.item.asLong.length  = 10;
   stack.Return.item.asLong.decimal = 0;
   stack.Return.item.asLong.value   = lNumber;
}

void hb_storc( char * szText, WORD wParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;
   ULONG ulLen;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, long );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         ulLen = strlen( szText );
         pItemRef = stack.pItems + pItem->item.asRefer.value;
         ItemRelease( pItemRef );
         pItemRef->type = IT_STRING;
         pItemRef->item.asString.length = ulLen;
         pItemRef->item.asString.value = ( char * ) hb_xgrab( ulLen + 1 );
         strcpy( pItemRef->item.asString.value, szText );
      }
   }
}

void hb_storclen( char * fixText, WORD wLength, WORD wParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, long );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = stack.pItems + pItem->item.asRefer.value;
         ItemRelease( pItemRef );
         pItemRef->type = IT_STRING;
         pItemRef->item.asString.length = wLength;
         pItemRef->item.asString.value = ( char * ) hb_xgrab( wLength + 1 );
         memcpy( pItemRef->item.asString.value, fixText, wLength );
         pItemRef->item.asString.value[ wLength ] = '\0';
      }
   }
}

void hb_stords( char * szDate, WORD wParam, ... ) /* szDate must have yyyymmdd format */
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;
   long lDay, lMonth, lYear;

   if( szDate && strlen( szDate ) == 8 )
   {
      /* Date string is valid length, so attempt conversion */
      lDay   = ( ( szDate[ 6 ] - '0' ) * 10 ) + ( szDate[ 7 ] - '0' );
      lMonth = ( ( szDate[ 4 ] - '0' ) * 10 ) + ( szDate[ 5 ] - '0' );
      lYear  = ( ( szDate[ 0 ] - '0' ) * 1000 ) + ( ( szDate[ 1 ] - '0' ) * 100 ) +
               ( ( szDate[ 2 ] - '0' ) * 10 ) + ( szDate[ 3 ] - '0' );
   }
   else lDay = lMonth = lYear = 0; /* Date string missing or bad length,
                                      so force an empty date */

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = stack.pItems + pItem->item.asRefer.value;
         ItemRelease( pItemRef );
         pItemRef->type               = IT_DATE;
         pItemRef->item.asDate.length = 8;
         pItemRef->item.asDate.value  = hb_dateEncode( lDay, lMonth, lYear );
      }
   }
}

void hb_storl( int iLogical, WORD wParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = stack.pItems + pItem->item.asRefer.value;
         ItemRelease( pItemRef );
         pItemRef->type                  = IT_LOGICAL;
         pItemRef->item.asLogical.length = 3;
         pItemRef->item.asLogical.value  = iLogical;
      }
   }
}

void hb_storni( int iValue, WORD wParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = stack.pItems + pItem->item.asRefer.value;
         ItemRelease( pItemRef );
         pItemRef->type                   = IT_INTEGER;
         pItemRef->item.asInteger.length  = 10;
         pItemRef->item.asInteger.decimal = 0;
         pItemRef->item.asInteger.value   = iValue;
      }
   }
}

void hb_stornl( long lValue, WORD wParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, long );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = stack.pItems + pItem->item.asRefer.value;
         ItemRelease( pItemRef );
         pItemRef->type                = IT_LONG;
         pItemRef->item.asLong.length  = 10;
         pItemRef->item.asLong.decimal = 0;
         pItemRef->item.asLong.value   = lValue;
      }
   }
}

void hb_stornd( double dValue, WORD wParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, long );
   va_end( va );

   if( wParam <= hb_pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = stack.pItems + pItem->item.asRefer.value;
         ItemRelease( pItemRef );
         pItemRef->type   = IT_DOUBLE;
         if( dValue > 10000000000.0 )
            pItemRef->item.asDouble.length = 20;
         else
            pItemRef->item.asDouble.length = 10;
         pItemRef->item.asDouble.decimal   = hb_set.HB_SET_DECIMALS;
         pItemRef->item.asDouble.value     = dValue;
      }
   }
}

void * hb_xgrab( ULONG ulSize )         /* allocates fixed memory */
{
   void * pMem = malloc( ulSize + sizeof( ULONG ) );

   if( ! pMem )
   {
      printf( "\n_xgrab error: can't allocate memory!\n" );
      exit( 1 );
   }

   * ( ( ULONG * ) pMem ) = ulSize;  /* we store the block size into it */

   ulMemoryConsumed    += ulSize;
   ulMemoryMaxConsumed += ulSize;
   ulMemoryBlocks++;
   ulMemoryMaxBlocks++;

   return ( char * ) pMem + sizeof( ULONG );
}

void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
   ULONG ulMemSize = * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );
   void * pResult = realloc( ( char * ) pMem - sizeof( ULONG ), ulSize + sizeof( ULONG ) );

   if( ! pResult )
   {
      printf( "\n_xrealloc error: can't reallocate memory!\n" );
      exit( 1 );
   }

   * ( ( ULONG * ) pResult ) = ulSize;  /* we store the block size into it */

   if( ! ulSize )
      ulMemoryBlocks--;

   ulMemoryConsumed += ( ulSize - ulMemSize );
   if( ulSize > ulMemSize )
      ulMemoryMaxConsumed += ulSize - ulMemSize;

   return ( char * ) pResult + sizeof( ULONG );
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{
   ULONG ulMemSize = * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );

   if( pMem )
      free( ( char * ) pMem - sizeof( ULONG ) );
   else
      printf( "\nCalling hb_xfree() with a null pointer!\n" );

   ulMemoryConsumed -= ulMemSize;
   ulMemoryBlocks--;
}
