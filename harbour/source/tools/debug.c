/* $Doc$
 * $Description$  Debug functions.
 * $Requirement$  source\rtl\itemapi.c (1999/05/04)
 * $Date$         1999/05/06
 * $End$ */

#include <extend.h>
#include <ctoharb.h>
#include <ctype.h>
#include <itemapi.h>

extern STACK stack;                             /* External data used       */
extern HB_ITEM  aStatics;

PHB_ITEM ArrayClone( PHB_ITEM );

/* $Doc$
 * $FuncName$     <aStat> __aStatic()
 * $Description$  Return the statics array
 *
 *                Please aClone before assignments
 * $End$ */
HARBOUR __ASTATIC()
{
   hb_itemReturn( &aStatics );
}


/* $Doc$
 * $FuncName$     <xStat> __Static(<nStatic>)
 * $Description$  Return a specified statics
 * $End$ */
HARBOUR __STATIC()
{
   PHB_ITEM pStatic;
   WORD  wStatic;

   wStatic = _parni(1);
   pStatic = ( ( PBASEARRAY ) aStatics.value.pBaseArray )->pItems +
             stack.iStatics + wStatic - 1;
   hb_itemReturn( pStatic );
}


/* $Doc$
 * $FuncName$     AddToArray( <pItem>, <pReturn>, <wPos> )
 * $Description$  Add <pItem> to array <pReturn> at pos <wPos>
 * $End$ */
void AddToArray( PHB_ITEM pItem, PHB_ITEM pReturn, WORD wPos )
{
   PHB_ITEM pTemp;

   if( pItem->wType == IT_SYMBOL)
   {                                            /* Symbol is pushed as text */
      pTemp = hb_itemNew(NULL);                 /* Create temporary string  */
      pTemp->wType   = IT_STRING;
      pTemp->wLength = strlen( pItem->value.pSymbol->szName )+2;
      pTemp->value.szText = (char *) _xgrab( pTemp->wLength+1 );

      sprintf( pTemp->value.szText, "[%s]", pItem->value.pSymbol->szName );

      hb_itemArrayPut( pReturn, wPos, pTemp );
      ItemRelease( pTemp );                     /* Get rid of temporary str.*/
      _xfree( pTemp );
   }
   else                                         /* Normal types             */
      hb_itemArrayPut( pReturn, wPos, pItem );
}


/* $Doc$
 * $FuncName$     <nVars> __GlobalStackLen()
 * $Description$  Returns the length of the global stack
 * $End$ */
WORD GlobalStackLen( void )
{
   PHB_ITEM pItem;
   WORD  nCount = 0;

   for( pItem = stack.pItems; pItem++ <= stack.pPos; nCount++ );
   return( nCount );
}
HARBOUR __GLOBALSTACKLEN()
{
   _retni( GlobalStackLen() );
}


/* $Doc$
 * $FuncName$     <aStack> __aGlobalStack()
 * $Description$  Returns the global stack
 * $End$ */
HARBOUR __AGLOBALSTACK()
{
   PHB_ITEM pReturn;
   PHB_ITEM pItem;

   WORD  wLen = GlobalStackLen();
   WORD  wPos = 1;

   pReturn = hb_itemArrayNew( wLen );           /* Create a transfer array  */
   for( pItem = stack.pItems; pItem <= stack.pPos; pItem++ )
      AddToArray( pItem, pReturn, wPos++ );
   hb_itemReturn( pReturn );
   ItemRelease( pReturn );
   _xfree( pReturn );
}


/* $Doc$
 * $FuncName$     <nVars> __StackLen()
 * $Description$  Returns the length of the stack of the calling function
 * $End$ */
WORD StackLen( void )
{
   PHB_ITEM pItem;
   PHB_ITEM pBase = stack.pItems + stack.pBase->wBase;

   WORD  nCount = 0;

   for( pItem = pBase; pItem < stack.pBase; pItem++, nCount++ );
   return( nCount );
}
HARBOUR __STACKLEN()
{
   _retni( StackLen() );
}


/* $Doc$
 * $FuncName$     <aStack> __aStack()
 * $Description$  Returns the stack of the calling function
 *                "[<symbol>]"  Means symbol.
 *
 *                [1]        Symbol of current function
 *                [2]        Self | NIL
 *                [3 .. x]   Parameters
 *                [x+1 .. y] Locals
 *                [y+1 ..]   Pushed data
 * $End$ */
HARBOUR __ASTACK()
{
   PHB_ITEM pReturn;
   PHB_ITEM pItem;
   PHB_ITEM pBase = stack.pItems + stack.pBase->wBase;

   WORD  wLen  = StackLen();
   WORD  wPos  = 1;

   pReturn = hb_itemArrayNew( wLen );           /* Create a transfer array  */
   for( pItem = pBase; pItem < stack.pBase; pItem++ )
      AddToArray( pItem, pReturn, wPos++ );
   hb_itemReturn( pReturn );
   ItemRelease( pReturn );
   _xfree( pReturn );
}


/* $Doc$
 * $FuncName$     <aParam> __aParam()
 * $Description$  Returns the passed parameters of the calling function
 * $End$ */
               /* TODO : put bLocals / bParams      */
               /* somewhere for declared parameters */
               /* and locals                        */
HARBOUR __APARAM()
{
   PHB_ITEM pReturn;
   PHB_ITEM pItem;
   PHB_ITEM pBase = stack.pItems + stack.pBase->wBase;
                                                /* Skip function + self     */
   WORD  wLen  = pBase->wParams;
   WORD  wPos  = 1;

   pReturn = hb_itemArrayNew( wLen );           /* Create a transfer array  */
   for( pItem = pBase+2; wLen--; pItem++ )
      AddToArray( pItem, pReturn, wPos++ );
   hb_itemReturn( pReturn );
   ItemRelease( pReturn );
   _xfree( pReturn );
}


