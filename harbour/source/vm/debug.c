/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Debugging functions for LOCAL, STATIC variables and the stack
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 * www - http://www.harbour-project.org
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
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"

/* $Doc$
 * $FuncName$     AddToArray( <pItem>, <pReturn>, <uiPos> )
 * $Description$  Add <pItem> to array <pReturn> at pos <uiPos>
 * $End$ */
static void AddToArray( PHB_ITEM pItem, PHB_ITEM pReturn, ULONG ulPos )
{
   PHB_ITEM pTemp;

   HB_TRACE(HB_TR_DEBUG, ("AddToArray(%p, %p, %lu)", pItem, pReturn, ulPos));

   if( pItem->type == IT_SYMBOL )
   {                                            /* Symbol is pushed as text */
      pTemp = hb_itemNew( NULL );               /* Create temporary string */
      pTemp->type = IT_STRING;
      pTemp->item.asString.length = strlen( pItem->item.asSymbol.value->szName ) + 2;
      pTemp->item.asString.value = ( char * ) hb_xgrab( pTemp->item.asString.length + 1 );

      sprintf( pTemp->item.asString.value, "[%s]", pItem->item.asSymbol.value->szName );

      hb_itemArrayPut( pReturn, ulPos, pTemp );
      hb_itemRelease( pTemp );                  /* Get rid of temporary str.*/
   }
   else                                         /* Normal types             */
      hb_itemArrayPut( pReturn, ulPos, pItem );
}


/* $Doc$
 * $FuncName$     <nVars> __vmStkGCount()
 * $Description$  Returns the length of the global stack
 * $End$ */
static USHORT hb_stackLenGlobal( void )
{
   PHB_ITEM pItem;
   USHORT uiCount = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackLenGlobal()"));

   for( pItem = hb_stack.pItems; pItem++ <= hb_stack.pPos; uiCount++ );

   return uiCount;
}

HARBOUR HB___VMSTKGCOUNT( void )
{
   hb_retni( hb_stackLenGlobal() );
}


/* $Doc$
 * $FuncName$     <aStack> __vmStkGList()
 * $Description$  Returns the global stack
 * $End$ */
HARBOUR HB___VMSTKGLIST( void )
{
   PHB_ITEM pReturn;
   PHB_ITEM pItem;

   USHORT uiLen = hb_stackLenGlobal();
   USHORT uiPos = 1;

   pReturn = hb_itemArrayNew( uiLen );           /* Create a transfer array  */

   for( pItem = hb_stack.pItems; pItem <= hb_stack.pPos; pItem++ )
      AddToArray( pItem, pReturn, uiPos++ );

   hb_itemReturn( pReturn );
   hb_itemRelease( pReturn );
}


/* $Doc$
 * $FuncName$     <nVars> __vmStkLCount()
 * $Description$  Returns the length of the stack of the calling function
 * $End$ */
static USHORT StackLen( void )
{
   PHB_ITEM pItem;
   PHB_ITEM pBase;
   USHORT uiCount = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_StackLen()"));

   pBase = hb_stack.pItems + hb_stack.pBase->item.asSymbol.stackbase;
   for( pItem = pBase; pItem < hb_stack.pBase; pItem++, uiCount++ );

   return uiCount;
}
HARBOUR HB___VMSTKLCOUNT( void )
{
   hb_retni( StackLen() );
}


/* $Doc$
 * $FuncName$     <aStack> __vmStkLList()
 * $Description$  Returns the stack of the calling function
 *                "[<symbol>]"  Means symbol.
 *
 *                [1]        Symbol of current function
 *                [2]        Self | NIL
 *                [3 .. x]   Parameters
 *                [x+1 .. y] Locals
 *                [y+1 ..]   Pushed data
 * $End$ */
HARBOUR HB___VMSTKLLIST( void )
{
   PHB_ITEM pReturn;
   PHB_ITEM pItem;
   PHB_ITEM pBase = hb_stack.pItems + hb_stack.pBase->item.asSymbol.stackbase;

   USHORT uiLen = StackLen();
   USHORT uiPos = 1;

   pReturn = hb_itemArrayNew( uiLen );           /* Create a transfer array  */
   for( pItem = pBase; pItem < hb_stack.pBase; pItem++ )
      AddToArray( pItem, pReturn, uiPos++ );
   hb_itemReturn( pReturn );
   hb_itemRelease( pReturn );
}


/* $Doc$
 * $FuncName$     <aParam> __vmParLGet()
 * $Description$  Returns the passed parameters of the calling function
 * $End$ */
               /* TODO : put bLocals / bParams      */
               /* somewhere for declared parameters */
               /* and locals                        */
HARBOUR HB___VMPARLLIST( void )
{
   PHB_ITEM pReturn;
   PHB_ITEM pItem;
   PHB_ITEM pBase = hb_stack.pItems + hb_stack.pBase->item.asSymbol.stackbase;
                                                /* Skip function + self     */
   USHORT uiLen = pBase->item.asSymbol.paramcnt;
   USHORT uiPos = 1;

   pReturn = hb_itemArrayNew( uiLen );           /* Create a transfer array  */

   for( pItem = pBase + 2; uiLen--; pItem++ )
      AddToArray( pItem, pReturn, uiPos++ );

   hb_itemReturn( pReturn );
   hb_itemRelease( pReturn );
}

/* -------------------------------------------------------------------------- */
/*  $DOC$
 *  $FUNCNAME$
 *    __VMVARLGET()
 *  $CATEGORY$
 *    Variable management
 *  $ONELINER$
 *  $SYNTAX$
 *    __VMVARLGET( <nProcLevel>, <nLocal> )
 *  $ARGUMENTS$
 *    <nProcLevel> Is the procedure level, same as used in ProcName()
 *    and ProcLine(), from which a local variable containts is going to
 *    be retrieved.
 *    <nLocal> Is the index of the local variable to retrieve.
 *  $RETURNS$
 *    The containts of a specific local variable
 *  $DESCRIPTION$
 *    This function is used from the debugger
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *
 *  $END$
 */

HARBOUR HB___VMVARLGET( void )
{
   int iLevel = hb_parni( 1 ) + 1;
   PHB_ITEM pBase = hb_stack.pBase;

   while( ( iLevel-- > 0 ) && pBase != hb_stack.pItems )
      pBase = hb_stack.pItems + pBase->item.asSymbol.stackbase;

   hb_itemReturn( pBase + 1 + hb_parni( 2 ) );
}

