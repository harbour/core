/*
 * $Id$
 */

#include <malloc.h>
#include <stdlib.h>
#include <extend.h>

extern STACK stack;

ULONG ulMemoryBlocks = 0;
ULONG ulMemoryMaxBlocks = 0;
ULONG ulMemoryMaxConsumed = 0;
ULONG ulMemoryConsumed = 0;

PITEM _param( WORD wParam, WORD wMask )
{
   WORD wType;

   if( wParam <= _pcount() )
   {
      wType = ( stack.pBase + 1 + wParam )->wType;

      if( ( wType & wMask ) || ( wType == IT_NIL && wMask == IT_ANY ) )
         return stack.pBase + 1 + wParam;
      else
         return 0;
   }
   return 0;
}

char * _parc( WORD wParam, ... )
{
   PITEM pItem;
   va_list va;
   WORD wArrayIndex = 0;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return ArrayGetString( pItem, wArrayIndex );
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
   PITEM pItem;
   va_list va;
   WORD wArrayIndex = 0;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;

      if( IS_ARRAY( pItem ) )
      {
         if( wArrayIndex )
            return ArrayGetStringLen( pItem, wArrayIndex );
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
   PITEM pItem;
   va_list va;
   WORD wArrayIndex = 0;
   long lDay, lMonth, lYear;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;

      if( IS_ARRAY( pItem ) && wArrayIndex )
         /* TODO: implement wArrayIndex use when retrieving an array element */
         return "";

      else if( IS_DATE( pItem ) )
      {
         hb_julian2greg( pItem->value.lDate, &lDay, &lMonth, &lYear );

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
         return "00000000";
   }
   return "00000000";
}

int _parl( WORD wParam, ... )
{
   PITEM pItem;
   va_list va;
   WORD wArrayIndex = 0;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;

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
   PITEM pItem;
   va_list va;
   WORD wArrayIndex = 0;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;

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
   PITEM pItem;
   va_list va;
   WORD wArrayIndex = 0;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;

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
   PITEM pItem;
   va_list va;
   WORD wArrayIndex = 0;

   va_start( va, wParam );
   wArrayIndex = va_arg( va, int );
   va_end( va );

   if( wParam <= _pcount() )
   {
      pItem = stack.pBase + 1 + wParam;

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
   Array( &stack.Return, ulLen );
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

   lDay   = ( ( szDate[ 6 ] - '0' ) * 10 ) + ( szDate[ 7 ] - '0' );
   lMonth = ( ( szDate[ 4 ] - '0' ) * 10 ) + ( szDate[ 5 ] - '0' );
   lYear  = ( ( szDate[ 0 ] - '0' ) * 1000 ) + ( ( szDate[ 1 ] - '0' ) * 100 ) +
            ( ( szDate[ 2 ] - '0' ) * 10 ) + ( szDate[ 3 ] - '0' );

   ItemRelease( &stack.Return );

   stack.Return.wType   = IT_DATE;
   stack.Return.wLength = 8;
   /* QUESTION: Is this ok ? we are going to use a long to store the date */
   /* QUESTION: What happens if we use sizeof( LONG ) instead ? */
   /* QUESTION: Would it break Clipper language code ? */
   stack.Return.value.lDate = hb_greg2julian( lDay, lMonth, lYear );
}

void _retnd( double dNumber )
{
   ItemRelease( &stack.Return );
   stack.Return.wType   = IT_DOUBLE;
   stack.Return.wLength = sizeof( double ); /* QUESTION: Is this correct ? */
   stack.Return.value.dNumber = dNumber;
}

void _retni( int iNumber )
{
   ItemRelease( &stack.Return );
   stack.Return.wType   = IT_INTEGER;
   stack.Return.wLength = sizeof( int ); /* QUESTION: Is this correct ? */
   stack.Return.value.iNumber = iNumber;
}

void _retl( int iTrueFalse )
{
   ItemRelease( &stack.Return );
   stack.Return.wType   = IT_LOGICAL;
   stack.Return.wLength = 1; /* QUESTION: Is this correct ? */
   stack.Return.value.iLogical = iTrueFalse;
}

void _retnl( long lNumber )
{
   ItemRelease( &stack.Return );
   stack.Return.wType   = IT_LONG;
   stack.Return.wLength = sizeof( LONG ); /* QUESTION: Is this correct ? */
   stack.Return.value.lNumber = lNumber;
}

void _storc( char * szText, WORD wParam, ... )
{
   PITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex = 0;
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
   PITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex = 0;

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
   PITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex = 0;
   long lDay, lMonth, lYear;

   lDay   = ( ( szDate[ 6 ] - '0' ) * 10 ) + ( szDate[ 7 ] - '0' );
   lMonth = ( ( szDate[ 4 ] - '0' ) * 10 ) + ( szDate[ 5 ] - '0' );
   lYear  = ( ( szDate[ 0 ] - '0' ) * 1000 ) + ( ( szDate[ 1 ] - '0' ) * 100 ) +
            ( ( szDate[ 2 ] - '0' ) * 10 ) + ( szDate[ 3 ] - '0' );

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
         pItemRef->wType = IT_DATE;
         pItemRef->value.lDate = hb_greg2julian( lDay, lMonth, lYear );
      }
   }
}

void _storl( int iLogical, WORD wParam, ... )
{
   PITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex = 0;

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
         pItemRef->value.iLogical = iLogical;
      }
   }
}

void _storni( int iValue, WORD wParam, ... )
{
   PITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex = 0;

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
         pItemRef->wType = IT_INTEGER;
         pItemRef->value.iNumber = iValue;
      }
   }
}

void _stornl( long lValue, WORD wParam, ... )
{
   PITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex = 0;

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
         pItemRef->wType = IT_LONG;
         pItemRef->value.lNumber = lValue;
      }
   }
}

void _stornd( double dValue, WORD wParam, ... )
{
   PITEM pItem, pItemRef;
   va_list va;
   WORD wArrayIndex = 0;

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
         pItemRef->wType = IT_DOUBLE;
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
