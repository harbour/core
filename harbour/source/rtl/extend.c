/*
 * $Id$
 */

#include <malloc.h>
#include <set.h>
#include <stdlib.h>
#include <extend.h>

extern STACK stack;

ULONG ulMemoryBlocks = 0;
ULONG ulMemoryMaxBlocks = 0;
ULONG ulMemoryMaxConsumed = 0;
ULONG ulMemoryConsumed = 0;

PHB_ITEM _param( WORD wParam, WORD wMask )
{
   WORD wType;
   PHB_ITEM pLocal;

   if( wParam <= _pcount() )
   {
      wType = ( stack.pBase + 1 + wParam )->wType;

      if( ( wType & wMask ) || ( wType == IT_NIL && wMask == IT_ANY ) )
      {
         pLocal = stack.pBase + 1 + wParam;
         if( wType & IT_BYREF )
            return stack.pItems + pLocal->value.wItem;
         else
            return pLocal;
      }
      else
         return 0;
   }
   return 0;
}

char * _parc( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->wType & IT_BYREF )
         pItem = stack.pItems + pItem->value.wItem;

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return hb_arrayGetString( pItem, wArrayIndex );
         else
            return "";
      }
      else if( IS_STRING( pItem ) )
         return pItem->value.szText;

      else
         return "";
   }
   return "";
}

ULONG _parclen( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->wType & IT_BYREF )
         pItem = stack.pItems + pItem->value.wItem;

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return hb_arrayGetStringLen( pItem, wArrayIndex );
         else
            return 0;
      }
      else if( IS_STRING( pItem ) )
         return pItem->wLength;

      else
         return 0;
   }
   return 0;
}

char * _pards( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;
   long lDay, lMonth, lYear;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->wType & IT_BYREF )
         pItem = stack.pItems + pItem->value.wItem;

      if( IS_ARRAY( pItem ) && wArrayIndex )
         /* TODO: implement wArrayIndex use when retrieving an array element */
         return "        ";

      else if( IS_DATE( pItem ) && pItem->value.lDate > 0 )
      {
         hb_dateDecode( pItem->value.lDate, &lDay, &lMonth, &lYear );

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

int _parl( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->wType & IT_BYREF )
         pItem = stack.pItems + pItem->value.wItem;

      if( IS_ARRAY( pItem ) && wArrayIndex )
         /* TODO: implement wArrayIndex use when retrieving an array element */
         return 0;

      else if( IS_LOGICAL( pItem ) )
         return pItem->value.iLogical;

      else
         return 0;
   }
   return 0;
}

double _parnd( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->wType & IT_BYREF )
         pItem = stack.pItems + pItem->value.wItem;

      if( IS_ARRAY( pItem ) && wArrayIndex )
         /* TODO: implement wArrayIndex use when retrieving an array element */
         return 0;

      else if( IS_INTEGER( pItem ) )
         return pItem->value.iNumber;

      else if( IS_LONG( pItem ) )
         return pItem->value.lNumber;

      else if( IS_DOUBLE( pItem ) )
         return pItem->value.dNumber;

      else
         return 0;
   }
   return 0;
}

int _parni( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->wType & IT_BYREF )
         pItem = stack.pItems + pItem->value.wItem;

      if( IS_ARRAY( pItem ) && wArrayIndex )
         /* TODO: implement wArrayIndex use when retrieving an array element */
         return 0;

      else if( IS_INTEGER( pItem ) )
         return pItem->value.iNumber;

      else if( IS_LONG( pItem ) )
         return pItem->value.lNumber;

      else if( IS_DOUBLE( pItem ) )
         return pItem->value.dNumber;

      else
         return 0;
   }
   return 0;
}

long _parnl( WORD wParam, ... )
{
   PHB_ITEM pItem;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;
      if( pItem->wType & IT_BYREF )
         pItem = stack.pItems + pItem->value.wItem;

      if( IS_ARRAY( pItem ) && wArrayIndex )
         /* TODO: implement wArrayIndex use when retrieving an array element */
         return 0;

      else if( IS_INTEGER( pItem ) )
         return (long) pItem->value.iNumber;

      else if( IS_LONG( pItem ) )
         return pItem->value.lNumber;

      else if( IS_DOUBLE( pItem ) )
         return (long) pItem->value.dNumber;

      else
         return 0;
   }
   return 0;
}

int _parinfa( int iParamNum, ULONG uiArrayIndex )
{
   PHB_ITEM pArray = _param( iParamNum, IT_ARRAY );

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

WORD _parinfo( WORD wParam )
{
   if( ! wParam )
      return stack.pBase->wParams;
   else
   {
      if( wParam <= _pcount() )
         return ( stack.pBase + 1 + wParam )->wType;
      else
         return 0;
   }
}

WORD _pcount( void )
{
   return stack.pBase->wParams;
}

void _ret( void )
{
   ItemRelease( &stack.Return );
}

void _reta( ULONG ulLen )  /* undocumented _reta() */
{
   hb_arrayNew( &stack.Return, ulLen );
}

void _retc( char * szText )
{
   ULONG ulLen = strlen( szText );

   ItemRelease( &stack.Return );
   stack.Return.wType   = IT_STRING;
   stack.Return.wLength = ulLen;
   stack.Return.value.szText = ( char * ) _xgrab( ulLen + 1 );
   strcpy( stack.Return.value.szText, szText );
}

void _retclen( char * szText, ULONG ulLen )
{
   ItemRelease( &stack.Return );
   stack.Return.wType   = IT_STRING;
   stack.Return.wLength = ulLen;
   stack.Return.value.szText = ( char * ) _xgrab( ulLen + 1 );
   memcpy( stack.Return.value.szText, szText, ulLen );
   stack.Return.value.szText[ ulLen ] = 0;
}

void _retds( char * szDate ) /* szDate must have yyyymmdd format */
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

   stack.Return.wType   = IT_DATE;
   stack.Return.wLength = 8;
   /* QUESTION: Is this ok ? we are going to use a long to store the date */
   /* QUESTION: What happens if we use sizeof( LONG ) instead ? */
   /* QUESTION: Would it break Clipper language code ? */
   stack.Return.value.lDate = hb_dateEncode( lDay, lMonth, lYear );
}

void _retnd( double dNumber )
{
   ItemRelease( &stack.Return );
   stack.Return.wType   = IT_DOUBLE;
   if( dNumber > 10000000000.0 ) stack.Return.wLength = 20;
   else stack.Return.wLength = 10;
   stack.Return.wDec    = hb_set.HB_SET_DECIMALS;
   stack.Return.value.dNumber = dNumber;
}

void _retni( int iNumber )
{
   ItemRelease( &stack.Return );
   stack.Return.wType   = IT_INTEGER;
   stack.Return.wLength = 10;
   stack.Return.wDec    = 0;
   stack.Return.value.iNumber = iNumber;
}

void _retl( int iTrueFalse )
{
   ItemRelease( &stack.Return );
   stack.Return.wType   = IT_LOGICAL;
   stack.Return.wLength = 3;
   stack.Return.value.iLogical = iTrueFalse;
}

void _retnl( long lNumber )
{
   ItemRelease( &stack.Return );
   stack.Return.wType   = IT_LONG;
   stack.Return.wLength = 10;
   stack.Return.wDec    = 0;
   stack.Return.value.lNumber = lNumber;
}

void _storc( char * szText, WORD wParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;
   ULONG ulLen;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, long );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         ulLen = strlen( szText );
         pItemRef = stack.pItems + pItem->value.wItem;
         ItemRelease( pItemRef );
         pItemRef->wType = IT_STRING;
         pItemRef->wLength = ulLen;
         pItemRef->value.szText = ( char * ) _xgrab( ulLen + 1 );
         strcpy( pItemRef->value.szText, szText );
      }
   }
}

void _storclen( char * fixText, WORD wLength, WORD wParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, long );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = stack.pItems + pItem->value.wItem;
         ItemRelease( pItemRef );
         pItemRef->wType = IT_STRING;
         pItemRef->wLength = wLength;
         pItemRef->value.szText = ( char * ) _xgrab( wLength + 1 );
         memcpy( pItemRef->value.szText, fixText, wLength );
         pItemRef->value.szText[ wLength ] = '\0';
      }
   }
}

void _stords( char * szDate, WORD wParam, ... ) /* szDate must have yyyymmdd format */
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

   if( wParam <= _pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = stack.pItems + pItem->value.wItem;
         ItemRelease( pItemRef );
         pItemRef->wType   = IT_DATE;
         pItemRef->wLength = 8;
         pItemRef->value.lDate = hb_dateEncode( lDay, lMonth, lYear );
      }
   }
}

void _storl( int iLogical, WORD wParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = stack.pItems + pItem->value.wItem;
         ItemRelease( pItemRef );
         pItemRef->wType = IT_LOGICAL;
         pItemRef->wLength = 3;
         pItemRef->value.iLogical = iLogical;
      }
   }
}

void _storni( int iValue, WORD wParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = stack.pItems + pItem->value.wItem;
         ItemRelease( pItemRef );
         pItemRef->wType   = IT_INTEGER;
         pItemRef->wLength = 10;
         pItemRef->wDec    = 0;
         pItemRef->value.iNumber = iValue;
      }
   }
}

void _stornl( long lValue, WORD wParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, long );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = stack.pItems + pItem->value.wItem;
         ItemRelease( pItemRef );
         pItemRef->wType   = IT_LONG;
         pItemRef->wLength = 10;
         pItemRef->wDec    = 0;
         pItemRef->value.lNumber = lValue;
      }
   }
}

void _stornd( double dValue, WORD wParam, ... )
{
   PHB_ITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, long );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = ( stack.pBase + 1 + wParam );

      if( IS_ARRAY( pItem ) && wArrayIndex )
      /* TODO: implement wArrayIndex use when storing to an array element */
         return;

      if( IS_BYREF( pItem ) )
      {
         pItemRef = stack.pItems + pItem->value.wItem;
         ItemRelease( pItemRef );
         pItemRef->wType   = IT_DOUBLE;
         if( dValue > 10000000000.0 ) pItemRef->wLength = 20;
         else pItemRef->wLength = 10;
         pItemRef->wDec    = hb_set.HB_SET_DECIMALS;
         pItemRef->value.dNumber = dValue;
      }
   }
}

void * _xgrab( ULONG ulSize )         /* allocates fixed memory */
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

void * _xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
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

void _xfree( void * pMem )            /* frees fixed memory */
{
   ULONG ulMemSize = * ( ULONG * ) ( ( char * ) pMem - sizeof( ULONG ) );

   if( pMem )
      free( ( char * ) pMem - sizeof( ULONG ) );
   else
      printf( "\nCalling _xfree() with a null pointer!\n" );

   ulMemoryConsumed -= ulMemSize;
   ulMemoryBlocks--;
}
