/* $Doc$
 * $Description$  Debug functions.
 * $Requirement$  source\rtl\itemapi.c (1999/05/04)
 * $Date$         1999/05/06
 * $End$ */

#include <extend.h>
#include <ctoharb.h>
#include <ctype.h>

extern STACK stack;                             /* External data used       */
extern ITEM  aStatics;
PITEM _itemReturn( PITEM );                     /* External functions used  */
PITEM _itemArrayNew( ULONG );
PITEM _itemArrayPut( PITEM, ULONG, PITEM );
PITEM _itemNew( PITEM );
PITEM ArrayClone( PITEM );

/* $Doc$
 * $FuncName$     <aStat> __aStatic()
 * $Description$  Return the statics array
 * $End$ */
HARBOUR __ASTATIC()
{
   _itemReturn( &aStatics );
}


/* $Doc$
 * $FuncName$     <xStat> __Static(<nStatic>)
 * $Description$  Return a specified statics
 * $End$ */
HARBOUR __STATIC()
{
   PITEM pStatic;
   WORD  wStatic;

   wStatic = _parni(1);
   pStatic = ( ( PBASEARRAY ) aStatics.value.pBaseArray )->pItems + stack.iStatics +
             wStatic - 1;
   _itemReturn( pStatic );
}


/* $Doc$
 * $FuncName$     <nVars> __StackLen()
 * $Description$  Returns the length of the stack
 * $End$ */
WORD StackLen()
{
   WORD  nCount = 0;
   PITEM p;

   for( p = stack.pItems; p <= stack.pPos; p++, nCount++ );
   return( nCount );
}
HARBOUR __STACKLEN()
{
   _retni( StackLen() );
}


/* $Doc$
 * $FuncName$     <aStack> __aStack()
 * $Description$  Returns the stack
 * $End$ */
HARBOUR __STACK()
{
   PITEM pReturn;
   PITEM p;
   PITEM pTemp;

   WORD  wLen;
   WORD  wPos = 1;

   pReturn = _itemArrayNew( wLen=StackLen() );  /* Create a transfer array  */
   for( p = stack.pItems; p <= stack.pPos ; wPos++, p++ )
   {
      switch( p->wType )
      {
         case IT_SYMBOL:                        /* Symbol is pushed as text */
         {
            pTemp = _itemNew(NULL);             /* Create temporary string  */
            pTemp->wType   = IT_STRING;
            pTemp->wLength = strlen( p->value.pSymbol->szName )+2;
            pTemp->value.szText = (char *) _xgrab( pTemp->wLength+1 );

            sprintf( pTemp->value.szText,
                     "[%s]", p->value.pSymbol->szName );

            _itemArrayPut( pReturn, wPos, pTemp );
            ItemRelease( pTemp );               /* Get rid of temporary str.*/
            _xfree( pTemp );
            break;
         }

         case IT_NIL:                           /* Normal types             */
         case IT_ARRAY:
         case IT_BLOCK:
         case IT_DATE:
         case IT_DOUBLE:
         case IT_LOGICAL:
         case IT_LONG:
         case IT_INTEGER:
         case IT_STRING:
         {
            _itemArrayPut( pReturn, wPos, p );
            break;
         }
         default:
         {
            pTemp = _itemNew(NULL);             /* Create temporary string  */
            pTemp->wType   = IT_STRING;
            pTemp->wLength = strlen( p->value.pSymbol->szName )+2;
            pTemp->value.szText = (char *) _xgrab( pTemp->wLength+1 );

            sprintf( pTemp->value.szText, "?type=%i?", p->wType );

            _itemArrayPut( pReturn, wPos, pTemp );
            ItemRelease( pTemp );               /* Get rid of temporary str.*/
            _xfree( pTemp );
         }
      }
   }
   _itemReturn( pReturn );
   ItemRelease( pReturn );
   _xfree( pReturn );
}
