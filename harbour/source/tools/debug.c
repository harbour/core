/*
 * $Id$
 */

/*
 * Debug - Contains debugging functions
 *
 * Copyright (C) 1999 Eddie Runia (eddie@runia.com)
 * Part of the Harbour Project www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 */

/* $Doc$
 * $Description$  Debug functions.
 * $Requirement$  source\rtl\itemapi.c (1999/05/04)
 * $Date$         1999/05/06
 * $End$ */

#include "extend.h"
#include "itemapi.h"

/* $Doc$
 * $FuncName$     <aStat> __aStatic()
 * $Description$  Return the statics array
 *
 *                Please aClone before assignments
 * $End$ */
HARBOUR HB___ASTATIC(void)
{
   PHB_ITEM pStatics = hb_arrayClone( &aStatics );

   hb_itemCopy( &stack.Return, pStatics );
   hb_itemRelease( pStatics );
}


/* $Doc$
 * $FuncName$     <xStat> __Static(<nStatic>)
 * $Description$  Return a specified statics
 * $End$ */
HARBOUR HB___STATIC(void)
{
   PHB_ITEM pStatic;
   WORD  wStatic;

   wStatic = hb_parni(1);
   pStatic = aStatics.item.asArray.value->pItems +
             stack.iStatics + wStatic - 1;
   hb_itemReturn( pStatic );
}


/* $Doc$
 * $FuncName$     AddToArray( <pItem>, <pReturn>, <wPos> )
 * $Description$  Add <pItem> to array <pReturn> at pos <wPos>
 * $End$ */
static void AddToArray( PHB_ITEM pItem, PHB_ITEM pReturn, WORD wPos )
{
   PHB_ITEM pTemp;

   if( pItem->type == IT_SYMBOL)
   {                                            /* Symbol is pushed as text */
      pTemp = hb_itemNew(NULL);                 /* Create temporary string  */
      pTemp->type   = IT_STRING;
      pTemp->item.asString.length = strlen( pItem->item.asSymbol.value->szName )+2;
      pTemp->item.asString.value = (char *) hb_xgrab( pTemp->item.asString.length+1 );

      sprintf( pTemp->item.asString.value, "[%s]", pItem->item.asSymbol.value->szName );

      hb_itemArrayPut( pReturn, wPos, pTemp );
      hb_itemClear( pTemp );                     /* Get rid of temporary str.*/
      hb_xfree( pTemp );
   }
   else                                         /* Normal types             */
      hb_itemArrayPut( pReturn, wPos, pItem );
}


/* $Doc$
 * $FuncName$     <nVars> __GlobalStackLen()
 * $Description$  Returns the length of the global stack
 * $End$ */
static WORD GlobalStackLen( void )
{
   PHB_ITEM pItem;
   WORD  nCount = 0;

   for( pItem = stack.pItems; pItem++ <= stack.pPos; nCount++ );
   return( nCount );
}
HARBOUR HB___GLOBALSTACKLEN(void)
{
   hb_retni( GlobalStackLen() );
}


/* $Doc$
 * $FuncName$     <aStack> __aGlobalStack()
 * $Description$  Returns the global stack
 * $End$ */
HARBOUR HB___AGLOBALSTACK(void)
{
   PHB_ITEM pReturn;
   PHB_ITEM pItem;

   WORD  wLen = GlobalStackLen();
   WORD  wPos = 1;

   pReturn = hb_itemArrayNew( wLen );           /* Create a transfer array  */
   for( pItem = stack.pItems; pItem <= stack.pPos; pItem++ )
      AddToArray( pItem, pReturn, wPos++ );
   hb_itemReturn( pReturn );
   hb_itemClear( pReturn );
   hb_xfree( pReturn );
}


/* $Doc$
 * $FuncName$     <nVars> __StackLen()
 * $Description$  Returns the length of the stack of the calling function
 * $End$ */
static WORD StackLen( void )
{
   PHB_ITEM pItem;
   PHB_ITEM pBase = stack.pItems + stack.pBase->item.asSymbol.stackbase;

   WORD  nCount = 0;

   for( pItem = pBase; pItem < stack.pBase; pItem++, nCount++ );
   return( nCount );
}
HARBOUR HB___STACKLEN(void)
{
   hb_retni( StackLen() );
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
HARBOUR HB___ASTACK(void)
{
   PHB_ITEM pReturn;
   PHB_ITEM pItem;
   PHB_ITEM pBase = stack.pItems + stack.pBase->item.asSymbol.stackbase;

   WORD  wLen  = StackLen();
   WORD  wPos  = 1;

   pReturn = hb_itemArrayNew( wLen );           /* Create a transfer array  */
   for( pItem = pBase; pItem < stack.pBase; pItem++ )
      AddToArray( pItem, pReturn, wPos++ );
   hb_itemReturn( pReturn );
   hb_itemClear( pReturn );
   hb_xfree( pReturn );
}


/* $Doc$
 * $FuncName$     <aParam> __aParam()
 * $Description$  Returns the passed parameters of the calling function
 * $End$ */
               /* TODO : put bLocals / bParams      */
               /* somewhere for declared parameters */
               /* and locals                        */
HARBOUR HB___APARAM(void)
{
   PHB_ITEM pReturn;
   PHB_ITEM pItem;
   PHB_ITEM pBase = stack.pItems + stack.pBase->item.asSymbol.stackbase;
                                                /* Skip function + self     */
   WORD  wLen  = pBase->item.asSymbol.paramcnt;
   WORD  wPos  = 1;

   pReturn = hb_itemArrayNew( wLen );           /* Create a transfer array  */
   for( pItem = pBase+2; wLen--; pItem++ )
      AddToArray( pItem, pReturn, wPos++ );
   hb_itemReturn( pReturn );
   hb_itemClear( pReturn );
   hb_xfree( pReturn );
}


