/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Memvar (PRIVATE/PUBLIC) runtime support
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    __MVSAVE()
 *    __MVRESTORE() (Thanks to Dave Pearson and Jo French for the original
 *                   Clipper function (FReadMem()) to read .mem files)
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h> /* for toupper() function */

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h" /* for __MVSAVE()/__MVRESTORE() */
#include "hbdate.h" /* for __MVSAVE()/__MVRESTORE() */
#include "hbcomp.h" /* for VS_* macros */
#include "error.ch"
#include "hbmemvar.ch"
#include "hbset.h"
#include "hbstack.h"

static PHB_DYNS * s_privateStack  = NULL;
static ULONG s_privateStackSize = 0;
static ULONG s_privateStackCnt  = 0;
static ULONG s_privateStackBase = 0;

static ULONG s_globalTableSize = 0;
static ULONG s_globalFirstFree = 0;
static ULONG s_globalLastFree  = 0;
static HB_VALUE_PTR s_globalTable = NULL;

#define TABLE_INITHB_VALUE   100
#define TABLE_EXPANDHB_VALUE  50

struct mv_PUBLIC_var_info
{
   int iPos;
   BOOL bFound;
   HB_DYNS_PTR pDynSym;
};

static void hb_memvarCreateFromDynSymbol( PHB_DYNS, BYTE, PHB_ITEM );

void hb_memvarsInit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarsInit()"));

   s_globalTable = ( HB_VALUE_PTR ) hb_xgrab( sizeof( HB_VALUE ) * TABLE_INITHB_VALUE );
   s_globalTableSize = TABLE_INITHB_VALUE;
   s_globalFirstFree = 0;
   s_globalLastFree  = 1;

   s_privateStack = ( PHB_DYNS * ) hb_xgrab( sizeof( PHB_DYNS ) * TABLE_INITHB_VALUE );
   s_privateStackSize = TABLE_INITHB_VALUE;
   s_privateStackCnt  = s_privateStackBase = 0;
}

void hb_memvarsFree( void )
{
   if( s_globalTable )
      hb_xfree( s_globalTable );

   if( s_privateStack )
      hb_xfree( s_privateStack );
}

/*
 * This function base address of values table
 */
HB_VALUE_PTR *hb_memvarValueBaseAddress( void )
{
   return &s_globalTable;
}

/*
 * This function creates new global value.
 *
 * pSource = item value that have to be stored or NULL
 * hPrevMemvar
 *    -1 if function is called to create memvar variable for a codeblock
 *       (to store detached local variable) - in this case we have to do
 *       exact copy of passed item (without duplicating its value and
 *       without reference decrementing)
 *    else we are creating regular memvar variable (PUBLIC or PRIVATE)
 *       In this case we have to do normal item coping.
 *       hPrevMemvar > 0 is old memvar value which will be hidden by
 *       new PRIVATE variable
 *
 * Returns:
 *  handle to variable memory or fails
 *
*/
static HB_HANDLE hb_memvarValueNew( HB_ITEM_PTR pSource, HB_HANDLE hPrevMemvar )
{
   HB_VALUE_PTR pValue;
   HB_HANDLE hValue;   /* handle 0 is reserved */
                       /* = 1 removed, since it's initialized in all branches. Caused a warning with Borland C++ */

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueNew(%p, %lu)", pSource, hPrevMemvar));

   if( s_globalFirstFree )
   {
      /* There are holes in the table - get a first available one
       */
      hValue = s_globalFirstFree;
      s_globalFirstFree = s_globalTable[ hValue ].hPrevMemvar;
   }
   else
   {
      /* Allocate the value from the end of table
       */
      if( s_globalLastFree < s_globalTableSize )
      {
         hValue = s_globalLastFree++;
      }
      else
      {
         /* No more free values in the table - expand the table
          */
         hValue = s_globalTableSize;
         s_globalLastFree = s_globalTableSize + 1;
         s_globalTableSize += TABLE_EXPANDHB_VALUE;
         s_globalTable = ( HB_VALUE_PTR ) hb_xrealloc( s_globalTable, sizeof( HB_VALUE ) * s_globalTableSize );
      }
   }

   pValue = s_globalTable + hValue;
   pValue->pVarItem = ( HB_ITEM_PTR ) hb_xgrab( sizeof( HB_ITEM ) );
   pValue->pVarItem->type = HB_IT_NIL;
   pValue->hPrevMemvar = hPrevMemvar;
   pValue->counter = 1;
   if( pSource )
   {
      if( hPrevMemvar == ( HB_HANDLE ) -1 ) /* detached local - copy its body only */
         memcpy( pValue->pVarItem, pSource, sizeof( HB_ITEM ) );
      else
         hb_itemCopy( pValue->pVarItem, pSource );
   }

   HB_TRACE(HB_TR_INFO, ("hb_memvarValueNew: memvar item created with handle %i", hValue));

   return hValue;
}

static void hb_memvarRecycle( HB_HANDLE hValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarRecycle(%lu)", hValue));
   
   s_globalTable[ hValue ].hPrevMemvar = s_globalFirstFree;
   s_globalFirstFree = hValue;
}

/*
 * This function increases the number of references to passed global value
 */
void hb_memvarValueIncRef( HB_HANDLE hValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueIncRef(%lu)", hValue));

   s_globalTable[ hValue ].counter++;

   HB_TRACE(HB_TR_INFO, ("Memvar item (%i) increment refCounter=%li", hValue, s_globalTable[ hValue ].counter));
}

/*
 * This function decreases the number of references to passed global value.
 * If it is the last reference then this value is deleted.
 */
void hb_memvarValueDecRef( HB_HANDLE hValue )
{
   HB_VALUE_PTR pValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueDecRef(%lu)", hValue));

   pValue = s_globalTable + hValue;

   HB_TRACE(HB_TR_INFO, ("Memvar item (%i) decrement refCounter=%li", hValue, pValue->counter-1));

   if( --pValue->counter == 0 )
   {
      if( HB_IS_COMPLEX( pValue->pVarItem ) )
         hb_itemClear( pValue->pVarItem );
      hb_xfree( pValue->pVarItem );
      hb_memvarRecycle( hValue );

      HB_TRACE(HB_TR_INFO, ("Memvar item (%i) deleted", hValue));
   }
}

/*
 * Detach local variable (swap current value with a memvar handle)
 */
static void hb_memvarDetachDynSym( PHB_DYNS pDynSym, BOOL fRestore )
{
   HB_VALUE_PTR pValue;
   HB_HANDLE hValue;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarDetachDynSym(%p, %d)", pDynSym, fRestore));

   hValue = pDynSym->hMemvar;
   pValue = s_globalTable + hValue;
   pDynSym->hMemvar = fRestore ? pValue->hPrevMemvar : 0;

   if( --pValue->counter == 0 )
   {
      if( HB_IS_COMPLEX( pValue->pVarItem ) )
         hb_itemClear( pValue->pVarItem );
      hb_xfree( pValue->pVarItem );
      hb_memvarRecycle( hValue );

      HB_TRACE(HB_TR_INFO, ("Memvar item (%i) deleted", hValue));
   }
   else
   {
      /* memvar is still accessible by active references on HVM stack
       * and/or as detached local in codeblocks - so we have to mark it
       * as detached or it will be scanned in GC mark pass and if some
       * cross references exists it will never be freed.
       */
      pValue->hPrevMemvar = ( HB_HANDLE ) -1;
   }
}

/*
 * Detach local variable (swap current value with a memvar handle)
 */
HB_ITEM_PTR hb_memvarDetachLocal( HB_ITEM_PTR pLocal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarDetachLocal(%p, %d)", pLocal, pLocal->type ));

   if( HB_IS_BYREF( pLocal ) )
   {
      do
      {
         if( HB_IS_MEMVAR( pLocal ) )
            break;
         else if( HB_IS_ENUM( pLocal ) && !pLocal->item.asEnum.valuePtr )
         {
            PHB_ITEM pBase = HB_IS_BYREF( pLocal->item.asEnum.basePtr ) ?
                            hb_itemUnRef( pLocal->item.asEnum.basePtr ) :
                                          pLocal->item.asEnum.basePtr;
            if( HB_IS_ARRAY( pBase ) )
            {
               PHB_ITEM pItem = hb_itemNew( NULL );
               hb_arrayGetItemRef( pBase, pLocal->item.asEnum.offset, pItem );
               pLocal->item.asEnum.valuePtr = pItem;
               pLocal = pItem;
               break;
            }
         }
         else if( pLocal->item.asRefer.value >= 0 &&
                  pLocal->item.asRefer.offset == 0 )
            break;
         pLocal = hb_itemUnRefOnce( pLocal );
      }
      while( HB_IS_BYREF( pLocal ) );
   }

   /* Change the value only if this variable is not referenced
    * by another codeblock yet.
    * In this case we have to copy the current value to a global memory
    * pool so it can be shared by codeblocks
    */
   if( ! HB_IS_MEMVAR( pLocal ) )
   {
      HB_HANDLE hMemvar = hb_memvarValueNew( pLocal, ( HB_HANDLE ) -1 );

      pLocal->type = HB_IT_BYREF | HB_IT_MEMVAR;
      pLocal->item.asMemvar.itemsbase = &s_globalTable;
      pLocal->item.asMemvar.value     = hMemvar;
   }
   return pLocal;
}


/*
 * This function pushes passed dynamic symbol that belongs to PRIVATE variable
 * into the stack. The value will be popped from it if the variable falls
 * outside the scope (either by using RELEASE, CLEAR ALL, CLEAR MEMORY or by
 * an exit from the function/procedure)
 *
 */
static void hb_memvarAddPrivate( PHB_DYNS pDynSym )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarAddPrivate(%p)", pDynSym));

   /* Allocate the value from the end of table
    */
   if( s_privateStackCnt == s_privateStackSize )
   {
      /* No more free values in the table - expand the table
       */
      s_privateStackSize += TABLE_EXPANDHB_VALUE;
      s_privateStack = ( PHB_DYNS * ) hb_xrealloc( s_privateStack, sizeof( PHB_DYNS ) * s_privateStackSize );
   }

   s_privateStack[ s_privateStackCnt++ ] = pDynSym;
}

/*
 * This function returns current PRIVATE variables stack base
 */
ULONG hb_memvarGetPrivatesBase( void )
{
   ULONG ulBase = s_privateStackBase;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetPrivatesBase()"));

   s_privateStackBase = s_privateStackCnt;
   return ulBase;
}

/*
 * This function releases PRIVATE variables created after passed base
 */
void hb_memvarSetPrivatesBase( ULONG ulBase )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarSetPrivatesBase(%lu)", ulBase));

   while( s_privateStackCnt > s_privateStackBase )
   {
      if( s_privateStack[ --s_privateStackCnt ]->hMemvar )
      {
         /* Restore previous value for variables that were overridden
          */
         hb_memvarDetachDynSym( s_privateStack[ s_privateStackCnt ], TRUE );
      }
   }
   s_privateStackBase = ulBase;
}

/*
 * Update PRIVATE base ofsset so they will not be removed
 * when function return
 */
void hb_memvarUpdatePrivatesBase( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarUpdatePrivatesBase()"));

   s_privateStackBase = s_privateStackCnt;
}

/*
 * This functions copies passed item value into the memvar pointed
 * by symbol
 *
 * pMemvar - symbol associated with a variable
 * pItem   - value to store in memvar
 *
 */
void hb_memvarSetValue( PHB_SYMB pMemvarSymb, HB_ITEM_PTR pItem )
{
   PHB_DYNS pDyn;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarSetValue(%p, %p)", pMemvarSymb, pItem));

   pDyn = ( PHB_DYNS ) pMemvarSymb->pDynSym;
   if( pDyn )
   {
      HB_TRACE(HB_TR_INFO, ("Memvar item (%i)(%s) assigned", pDyn->hMemvar, pMemvarSymb->szName));

      if( pDyn->hMemvar )
      {
         /* value is already created */
         hb_itemCopyToRef( s_globalTable[ pDyn->hMemvar ].pVarItem, pItem );
      }
      else
      {
         /* assignment to undeclared memvar - PRIVATE is assumed */
         hb_memvarCreateFromDynSymbol( pDyn, VS_PRIVATE, pItem );
      }

      /* Remove MEMOFLAG if exists (assignment from field). */
      s_globalTable[ pDyn->hMemvar ].pVarItem->type &= ~HB_IT_MEMOFLAG;
   }
   else
      hb_errInternal( HB_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );
}

ERRCODE hb_memvarGet( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb )
{
   PHB_DYNS pDyn;
   ERRCODE bSuccess = FAILURE;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGet(%p, %p)", pItem, pMemvarSymb));

   pDyn = ( PHB_DYNS ) pMemvarSymb->pDynSym;
   if( pDyn )
   {
      HB_TRACE(HB_TR_INFO, ("Memvar item (%i)(%s) queried", pDyn->hMemvar, pMemvarSymb->szName));

      if( pDyn->hMemvar )
      {
         /* value is already created
          */
         HB_ITEM_PTR pGetItem = s_globalTable[ pDyn->hMemvar ].pVarItem;
         if( HB_IS_BYREF( pGetItem ) )
            hb_itemCopy( pItem, hb_itemUnRef( pGetItem ) );
         else
            hb_itemCopy( pItem, pGetItem );
         bSuccess = SUCCESS;
      }
   }
   else
      hb_errInternal( HB_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );

   return bSuccess;
}

void hb_memvarGetValue( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetValue(%p, %p)", pItem, pMemvarSymb));

   if( hb_memvarGet( pItem, pMemvarSymb ) == FAILURE )
   {
      /* Generate an error with retry possibility
       * (user created error handler can create this variable)
       */
      HB_ITEM_PTR pError;

      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                             NULL, pMemvarSymb->szName, 0, EF_CANRETRY );

      while( hb_errLaunch( pError ) == E_RETRY )
      {
         if( hb_memvarGet( pItem, pMemvarSymb ) == SUCCESS )
            break;
      }

      hb_errRelease( pError );
   }
}

void hb_memvarGetRefer( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb )
{
   PHB_DYNS pDyn;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetRefer(%p, %p)", pItem, pMemvarSymb));

   pDyn = ( PHB_DYNS ) pMemvarSymb->pDynSym;
   if( pDyn )
   {
      HB_TRACE(HB_TR_INFO, ("Memvar item (%i)(%s) referenced", pDyn->hMemvar, pMemvarSymb->szName));

      if( pDyn->hMemvar )
      {
         PHB_ITEM pMemvar = s_globalTable[ pDyn->hMemvar ].pVarItem;
         if( HB_IS_BYREF( pMemvar ) )
            hb_itemCopy( pItem, pMemvar );
         else
         {
            /* value is already created */
            pItem->type = HB_IT_BYREF | HB_IT_MEMVAR;
            pItem->item.asMemvar.value = pDyn->hMemvar;
            pItem->item.asMemvar.itemsbase = &s_globalTable;
            ++s_globalTable[ pDyn->hMemvar ].counter;
         }
      }
      else
      {
         /* Generate an error with retry possibility
          * (user created error handler can make this variable accessible)
          */
         HB_ITEM_PTR pError;

         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                                NULL, pMemvarSymb->szName, 0, EF_CANRETRY );

         while( hb_errLaunch( pError ) == E_RETRY )
         {
            if( pDyn->hMemvar )
            {
               PHB_ITEM pMemvar = s_globalTable[ pDyn->hMemvar ].pVarItem;
               if( HB_IS_BYREF( pMemvar ) )
                  hb_itemCopy( pItem, pMemvar );
               else
               {
                  /* value is already created */
                  pItem->type = HB_IT_BYREF | HB_IT_MEMVAR;
                  pItem->item.asMemvar.value = pDyn->hMemvar;
                  pItem->item.asMemvar.itemsbase = &s_globalTable;
                  ++s_globalTable[ pDyn->hMemvar ].counter;
               }
               break;
            }
         }
         hb_errRelease( pError );
      }
   }
   else
      hb_errInternal( HB_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );
}

PHB_ITEM hb_memvarGetItem( PHB_SYMB pMemvarSymb )
{
   PHB_DYNS pDyn;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetItem(%p)", pMemvarSymb));

   pDyn = ( PHB_DYNS ) pMemvarSymb->pDynSym;
   if( pDyn && pDyn->hMemvar )
   {
      HB_ITEM_PTR pItem = s_globalTable[ pDyn->hMemvar ].pVarItem;
      if( HB_IS_BYREF( pItem ) )
         return hb_itemUnRef( pItem );
      else
         return pItem;
   }
   return NULL;
}

/*
 */
void hb_memvarNewParameter( PHB_SYMB pSymbol, PHB_ITEM pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarNewParameter(%p, %p)", pSymbol, pValue));

   hb_memvarCreateFromDynSymbol( pSymbol->pDynSym, VS_PRIVATE, pValue );
}

static HB_DYNS_PTR hb_memvarFindSymbol( char * szArg, ULONG ulLen )
{
   HB_DYNS_PTR pDynSym = NULL;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarFindSymbol(%p,%lu)", szArg, ulLen));

   if( ulLen && szArg && *szArg )
   {
      char szUprName[ HB_SYMBOL_NAME_LEN + 1 ];
      int iSize = 0;

      do
      {
         char cChar = *szArg++;

         if( cChar >= 'a' && cChar <= 'z' )
         {
            szUprName[ iSize++ ] = cChar - ( 'a' - 'A' );
         }
         else if( cChar == ' ' || cChar == '\t' || cChar == '\n' )
         {
            if( iSize )
               break;
         }
         else if( !cChar )
         {
            break;
         }
         else
         {
            szUprName[ iSize++ ] = cChar;
         }
      }
      while( --ulLen && iSize < HB_SYMBOL_NAME_LEN );

      if( iSize )
      {
         szUprName[ iSize ] = '\0';
         pDynSym = hb_dynsymFind( szUprName );
      }
   }
   return pDynSym;
}

char * hb_memvarGetStrValuePtr( char * szVarName, ULONG *pulLen )
{
   HB_DYNS_PTR pDynVar;
   char * szValue = NULL;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetStrValuePtr(%s, %li)", szVarName, pulLen));

   pDynVar = hb_memvarFindSymbol( szVarName, *pulLen );

   if( pDynVar )
   {
      /* there is dynamic symbol with the requested name - check if it is
       * a memvar variable
       */
      if( pDynVar->hMemvar )
      {
         /* variable contains some data
          */
         HB_ITEM_PTR pItem = s_globalTable[ pDynVar->hMemvar ].pVarItem;
         if( HB_IS_BYREF( pItem ) )
            pItem = hb_itemUnRef( pItem );   /* it is a PARAMETER variable */
         if( HB_IS_STRING( pItem ) )
         {
            szValue = pItem->item.asString.value;
            *pulLen = pItem->item.asString.length;
         }
      }
   }

   return szValue;
}

/*
 * This function creates a value for memvar variable
 *
 * pMemvar - an item that stores the name of variable - it can be either
 *          the HB_IT_SYMBOL (if created by PUBLIC statement) or HB_IT_STRING
 *          (if created by direct call to __MVPUBLIC function)
 * bScope - the scope of created variable - if a variable with the same name
 *          exists already then it's value is hidden by new variable with
 *          passed scope
 * pValue - optional item used to initialize the value of created variable
 *          or NULL
 *
 */
void hb_memvarCreateFromItem( PHB_ITEM pMemvar, BYTE bScope, PHB_ITEM pValue )
{
   PHB_DYNS pDynVar = NULL;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarCreateFromItem(%p, %d, %p)", pMemvar, bScope, pValue));

   /* find dynamic symbol or creeate one */
   if( HB_IS_SYMBOL( pMemvar ) )
      /* pDynVar = hb_dynsymGet( pMemvar->item.asSymbol.value->szName ); */
      pDynVar = pMemvar->item.asSymbol.value->pDynSym;
   else if( HB_IS_STRING( pMemvar ) )
      pDynVar = hb_dynsymGet( pMemvar->item.asString.value );

   if( pDynVar )
      hb_memvarCreateFromDynSymbol( pDynVar, bScope, pValue );
   else
      hb_errRT_BASE( EG_ARG, 3008, NULL, "&", HB_ERR_ARGS_BASEPARAMS );
}

static void hb_memvarCreateFromDynSymbol( PHB_DYNS pDynVar, BYTE bScope, PHB_ITEM pValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarCreateFromDynSymbol(%p, %d, %p)", pDynVar, bScope, pValue));

   if( bScope & VS_PUBLIC )
   {
      /* If the variable with the same name exists already
       * then the current value have to be unchanged
       */
      if( ! pDynVar->hMemvar )
      {
         pDynVar->hMemvar = hb_memvarValueNew( pValue, 0 );
         if( !pValue )
         {
            /* new PUBLIC variable - initialize it to .F.
             */

            s_globalTable[ pDynVar->hMemvar ].pVarItem->type = HB_IT_LOGICAL;

            /* NOTE: PUBLIC variables named CLIPPER and HARBOUR are initialized */
            /*       to .T., this is normal Clipper behaviour. [vszakats] */

            if( strcmp( pDynVar->pSymbol->szName, "HARBOUR" ) == 0 ||
                 strcmp( pDynVar->pSymbol->szName, "CLIPPER" ) == 0 )
               s_globalTable[ pDynVar->hMemvar ].pVarItem->item.asLogical.value = TRUE;
            else
               s_globalTable[ pDynVar->hMemvar ].pVarItem->item.asLogical.value = FALSE;
         }
      }
   }
   else
   {
      /* We need to store the handle to the value of variable that is
       * visible at this moment so later we can restore this value when
       * the new variable will be released
       */
      pDynVar->hMemvar = hb_memvarValueNew( pValue, pDynVar->hMemvar );

      /* Add this variable to the PRIVATE variables stack
       */
      hb_memvarAddPrivate( pDynVar );
   }
}

/* This function releases all memory occupied by a memvar variable
 * It also restores the value that was hidden if there is another
 * PRIVATE variable with the same name.
 */
static void hb_memvarRelease( HB_ITEM_PTR pMemvar )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarRelease(%p)", pMemvar));

   if( HB_IS_STRING( pMemvar ) )
   {
      PHB_DYNS pDynSymbol = hb_memvarFindSymbol( pMemvar->item.asString.value,
                                                 pMemvar->item.asString.length );

      if( pDynSymbol && pDynSymbol->hMemvar )
      {
         ULONG ulBase = s_privateStackCnt;

         /* Find the variable with a requested name that is currently visible
          * Start from the top of the stack.
          */
         while( ulBase > 0 )
         {
            if( pDynSymbol == s_privateStack[ --ulBase ] )
            {
               /* reset current value to NIL - the overriden variables will be
                * visible after exit from current procedure
                */
               hb_itemClear( s_globalTable[ pDynSymbol->hMemvar ].pVarItem );
               return;
            }
         }

         /* No match found for PRIVATEs - it's PUBLIC so let's remove it.
          */
         hb_memvarDetachDynSym( pDynSymbol, FALSE );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 3008, NULL, "RELEASE", HB_ERR_ARGS_BASEPARAMS );
}


/* This function releases all memory occupied by a memvar variable and
 * assigns NIL value - it releases variables created in current
 * procedure only.
 * The scope of released variables are specified using passed name's mask
 */
static void hb_memvarReleaseWithMask( char *szMask, BOOL bInclude )
{
   ULONG ulBase = s_privateStackCnt;
   PHB_DYNS pDynVar;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarReleaseWithMask(%s, %d)", szMask, (int) bInclude));

   while( ulBase > s_privateStackBase )
   {
      --ulBase;
      pDynVar = s_privateStack[ ulBase ];
      /* reset current value to NIL - the overriden variables will be
       * visible after exit from current procedure
       */
      if( pDynVar->hMemvar )
      {
         BOOL fMatch = hb_strMatchCaseWildExact( pDynVar->pSymbol->szName, szMask );
         if( bInclude ? fMatch : !fMatch )
            hb_itemClear( s_globalTable[ pDynVar->hMemvar ].pVarItem );
      }
   }
}

/* Checks if passed dynamic symbol is a variable and returns its scope
 */
static int hb_memvarScopeGet( PHB_DYNS pDynVar )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarScopeGet(%p)", pDynVar));

   if( pDynVar->hMemvar == 0 )
      return HB_MV_UNKNOWN;
   else
   {
      ULONG ulBase = s_privateStackCnt;    /* start from the top of the stack */

      while( ulBase )
      {
         if( pDynVar == s_privateStack[ --ulBase ] )
         {
            if( ulBase >= s_privateStackBase )
               return HB_MV_PRIVATE_LOCAL;
            else
               return HB_MV_PRIVATE_GLOBAL;
         }
      }
      return HB_MV_PUBLIC;
   }
}

/* This function checks the scope of passed variable name
 */
int hb_memvarScope( char * szVarName, ULONG ulLength )
{
   PHB_DYNS pDynVar;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarScope(%s, %lu)", szVarName, ulLength));

   pDynVar = hb_memvarFindSymbol( szVarName, ulLength );

   if( pDynVar )
      return hb_memvarScopeGet( pDynVar );
   else
      return HB_MV_NOT_FOUND;
}

/* Releases memory occupied by a variable
 */
static HB_DYNS_FUNC( hb_memvarClear )
{
   HB_SYMBOL_UNUSED( Cargo );

   if( pDynSymbol->hMemvar )
      hb_memvarDetachDynSym( pDynSymbol, FALSE );

   return TRUE;
}

/* Clear all memvar variables */
void hb_memvarsClear( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarsClear()"));

   hb_stackClearMevarsBase();
   s_privateStackBase = 0;
   hb_memvarSetPrivatesBase( 0 );
   hb_dynsymEval( hb_memvarClear, NULL );
}

/* Checks passed dynamic symbol if it is a PUBLIC variable and
 * increments the counter eventually
 */
static HB_DYNS_FUNC( hb_memvarCountPublics )
{
   if( hb_memvarScopeGet( pDynSymbol ) == HB_MV_PUBLIC )
      ( * ( ( int * )Cargo ) )++;

   return TRUE;
}

/* Count the number of variables with given scope
 */
static int hb_memvarCount( int iScope )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarCount(%d)", iScope));

   if( iScope == HB_MV_PUBLIC )
   {
      int iPublicCnt = 0;

      hb_dynsymEval( hb_memvarCountPublics, ( void * ) &iPublicCnt );
      return iPublicCnt;
   }
   else
      return s_privateStackCnt;  /* number of PRIVATE variables */
}

/* Checks passed dynamic symbol if it is a PUBLIC variable and returns
 * a pointer to its dynamic symbol
 */
static HB_DYNS_FUNC( hb_memvarFindPublicByPos )
{
   BOOL bCont = TRUE;

   if( hb_memvarScopeGet( pDynSymbol ) == HB_MV_PUBLIC )
   {
      struct mv_PUBLIC_var_info *pStruPub = (struct mv_PUBLIC_var_info *) Cargo;
      if( pStruPub->iPos-- == 0 )
      {
         pStruPub->bFound  = TRUE;
         pStruPub->pDynSym = pDynSymbol;
         bCont =FALSE;
      }
   }

   return bCont;
}

/* Returns the pointer to item that holds a value of variable (or NULL if
 * not found). It fills also the pointer to the variable name
 * Both pointers points to existing and used data - they shouldn't be
 * deallocated.
 */
static HB_ITEM_PTR hb_memvarDebugVariable( int iScope, int iPos, const char * * pszName )
{
   HB_ITEM_PTR pValue = NULL;
   *pszName = NULL;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarDebugVariable(%d, %d, %p)", iScope, iPos, pszName));

   if( iPos > 0 )
   {
      --iPos;
      if( iScope == HB_MV_PUBLIC )
      {
         struct mv_PUBLIC_var_info struPub;

         struPub.iPos   = iPos;
         struPub.bFound = FALSE;
         /* enumerate existing dynamic symbols and fill this structure
          * with info for requested PUBLIC variable
          */
         hb_dynsymEval( hb_memvarFindPublicByPos, ( void * ) &struPub );
         if( struPub.bFound )
         {
            pValue = s_globalTable[ struPub.pDynSym->hMemvar ].pVarItem;
            *pszName = struPub.pDynSym->pSymbol->szName;
         }
      }
      else
      {
         if( ( ULONG ) iPos < s_privateStackCnt )
         {
            HB_DYNS_PTR pDynSym = s_privateStack[ iPos ];

            pValue =s_globalTable[ pDynSym->hMemvar ].pVarItem;
            *pszName = pDynSym->pSymbol->szName;
         }
      }
   }

   return pValue;
}

/* ************************************************************************** */

static char * hb_memvarGetMask( int iParam )
{
   char * pszMask = hb_parc( iParam );
   if( !pszMask || pszMask[ 0 ] == '*' )
      pszMask = "*";
   return pszMask;
}

HB_FUNC( __MVPUBLIC )
{
   int iCount = hb_pcount();

   if( iCount )
   {
      int i;

      for( i = 1; i <= iCount; i++ )
      {
         PHB_ITEM pMemvar = hb_param( i, HB_IT_ANY );

         if( pMemvar )
         {
            if( HB_IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ULONG j, ulLen = hb_arrayLen( pMemvar );

               for( j = 1; j <= ulLen; j++ )
               {
                  hb_memvarCreateFromItem( hb_arrayGetItemPtr( pMemvar, j ), VS_PUBLIC, NULL );
               }
            }
            else
               hb_memvarCreateFromItem( pMemvar, VS_PUBLIC, NULL );
         }
      }
   }
}

HB_FUNC( __MVPRIVATE )
{
   int iCount = hb_pcount();

   if( iCount )
   {
      int i;

      for( i = 1; i <= iCount; i++ )
      {
         PHB_ITEM pMemvar = hb_param( i, HB_IT_ANY );

         if( pMemvar )
         {
            if( HB_IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ULONG j, ulLen = hb_arrayLen( pMemvar );

               for( j = 1; j <= ulLen; j++ )
               {
                  hb_memvarCreateFromItem( hb_arrayGetItemPtr( pMemvar, j ), VS_PRIVATE, NULL );
               }
            }
            else
               hb_memvarCreateFromItem( pMemvar, VS_PRIVATE, NULL );
         }
      }
      hb_memvarUpdatePrivatesBase();
   }
}

HB_FUNC( __MVXRELEASE )
{
   int iCount = hb_pcount();

   if( iCount )
   {
      int i;

      for( i = 1; i <= iCount; i++ )
      {
         PHB_ITEM pMemvar = hb_param( i, HB_IT_ANY );

         if( pMemvar )
         {
            if( HB_IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ULONG j, ulLen = hb_arrayLen( pMemvar );

               for( j = 1; j <= ulLen; j++ )
               {
                  hb_memvarRelease( hb_arrayGetItemPtr( pMemvar, j ) );
               }
            }
            else
               hb_memvarRelease( pMemvar );
         }
      }
   }
}

HB_FUNC( __MVRELEASE )
{
   int iCount = hb_pcount();

   if( iCount && ISCHAR( 1 ) )
   {
      BOOL bIncludeVar;
      char * pszMask;

      pszMask = hb_memvarGetMask( 1 );
      bIncludeVar = ( pszMask[ 0 ] == '*' && !pszMask[ 1 ] ) ||
                    iCount < 2 || hb_parl( 2 );
      hb_memvarReleaseWithMask( pszMask, bIncludeVar );
   }
}

HB_FUNC( __MVSCOPE )
{
   int iMemvar = HB_MV_ERROR;

   if( hb_pcount() )
   {
      PHB_ITEM pVarName = hb_param( 1, HB_IT_STRING );

      if( pVarName )
         iMemvar = hb_memvarScope( pVarName->item.asString.value,
                                   pVarName->item.asString.length );
   }

   hb_retni( iMemvar );
}

HB_FUNC( __MVCLEAR )
{
   hb_memvarsClear();
}

HB_FUNC( __MVDBGINFO )
{
   int iCount = hb_pcount();

   if( iCount == 1 )          /* request for a number of variables */
      hb_retni( hb_memvarCount( hb_parni( 1 ) ) );

   else if( iCount >= 2 )     /* request for a value of variable */
   {
      HB_ITEM_PTR pValue;
      const char * szName;

      pValue = hb_memvarDebugVariable( hb_parni( 1 ), hb_parni( 2 ), &szName );

      if( pValue )
      {
         /*the requested variable was found
          */
         if( iCount >= 3 && ISBYREF( 3 ) )
         {
            /* we have to use this variable regardless of its current value
             */
            HB_ITEM_PTR pName = hb_param( 3, HB_IT_ANY );

            hb_itemPutC( pName, szName ); /* clear an old value and copy a new one */
            /* szName points directly to a symbol name - it cannot be released
             */
         }
         hb_itemReturn( pValue );
         /* pValue points directly to the item structure used by this variable
          * this item cannot be released
          */
      }
      else
      {
         hb_ret(); /* return NIL value */

         if( iCount >= 3 && ISBYREF( 3 ) )
         {
            /* we have to use this variable regardless of its current value
             */
            HB_ITEM_PTR pName = hb_param( 3, HB_IT_ANY );

            hb_itemPutC( pName, "?" ); /* clear an old value and copy a new one */
         }
      }
   }
}

HB_FUNC( __MVEXIST )
{
   PHB_DYNS pDyn;
   pDyn = hb_memvarFindSymbol( hb_parc( 1 ), hb_parclen( 1 ) );
   hb_retl( pDyn && pDyn->hMemvar );
}

HB_FUNC( __MVGET )
{
   HB_ITEM_PTR pName = hb_param( 1, HB_IT_STRING );

   if( pName )
   {
      HB_DYNS_PTR pDynVar = hb_memvarFindSymbol( pName->item.asString.value,
                                                 pName->item.asString.length );

      if( pDynVar )
      {
         PHB_ITEM pValue = hb_stackAllocItem();

         hb_memvarGetValue( pValue, pDynVar->pSymbol );
         hb_itemReturnForward( pValue );
         hb_stackDec();
      }
      else
      {
         /* Generate an error with retry possibility
          * (user created error handler can create this variable)
          */
         HB_ITEM_PTR pError;

         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                                 NULL, pName->item.asString.value, 0, EF_CANRETRY );

         while( hb_errLaunch( pError ) == E_RETRY )
         {
            pDynVar = hb_memvarFindSymbol( hb_itemGetCPtr( pName ),
                                           hb_itemGetCLen( pName ) );
            if( pDynVar )
            {
               PHB_ITEM pValue = hb_stackAllocItem();

               hb_memvarGetValue( pValue, pDynVar->pSymbol );
               hb_itemReturnForward( pValue );
               hb_stackDec();
               break;
            }
         }
         hb_errRelease( pError );
      }
   }
   else
   {
      /* either the first parameter is not specified or it has a wrong type
       * (it must be a string)
       * This is not a critical error - we can continue normal processing
       */
      hb_errRT_BASE_SubstR( EG_ARG, 3009, NULL, NULL, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( __MVPUT )
{
   HB_ITEM_PTR pName = hb_param( 1, HB_IT_STRING );
   HB_ITEM_PTR pValue = hb_paramError( 2 );

   if( pName )
   {
      /* the first parameter is a string with not empty variable name
       */
      HB_DYNS_PTR pDynVar = hb_memvarFindSymbol( pName->item.asString.value,
                                                 pName->item.asString.length );
      if( pDynVar )
      {
         /* variable was declared somwhere - assign a new value
          */
         hb_memvarSetValue( pDynVar->pSymbol, pValue );
      }
      else
      {
         /* attempt to assign a value to undeclared variable
          * create the PRIVATE one
          */
         hb_memvarCreateFromDynSymbol( hb_dynsymGet( pName->item.asString.value ), VS_PRIVATE, pValue );
      }
      hb_memvarUpdatePrivatesBase();
      hb_itemReturn( pValue );
   }
   else
   {
      /* either the first parameter is not specified or it has a wrong type
       * (it must be a string)
       * This is not a critical error - we can continue normal processing
       */
      HB_ITEM_PTR pRetValue = hb_errRT_BASE_Subst( EG_ARG, 3010, NULL, NULL, HB_ERR_ARGS_BASEPARAMS );

      if( pRetValue )
         hb_itemRelease( pRetValue );
      hb_itemReturn( pValue );
   }
}

#define HB_MEM_REC_LEN          32
#define HB_MEM_NUM_LEN          8

typedef struct
{
   char * pszMask;
   BOOL bIncludeMask;
   BYTE * buffer;
   FHANDLE fhnd;
} MEMVARSAVE_CARGO;

/* saves a variable to a mem file already open */

static HB_DYNS_FUNC( hb_memvarSave )
{
   char * pszMask    = ( ( MEMVARSAVE_CARGO * ) Cargo )->pszMask;
   BOOL bIncludeMask = ( ( MEMVARSAVE_CARGO * ) Cargo )->bIncludeMask;
   BYTE * buffer     = ( ( MEMVARSAVE_CARGO * ) Cargo )->buffer;
   FHANDLE fhnd      = ( ( MEMVARSAVE_CARGO * ) Cargo )->fhnd;

   /* NOTE: Harbour name lengths are not limited, but the .mem file
            structure is not flexible enough to allow for it.
            [vszakats] */

   if( pDynSymbol->hMemvar )
   {
      BOOL bMatch = hb_strMatchCaseWildExact( pDynSymbol->pSymbol->szName, pszMask );

      PHB_ITEM pItem = s_globalTable[ pDynSymbol->hMemvar ].pVarItem;

      /* Process it if it matches the passed mask */
      if( bIncludeMask ? bMatch : ! bMatch )
      {
         /* NOTE: Clipper will not initialize the record buffer with
                  zeros, so they will look trashed. [vszakats] */
         memset( buffer, 0, HB_MEM_REC_LEN );

         /* NOTE: Save only the first 10 characters of the name */
         strncpy( ( char * ) buffer, pDynSymbol->pSymbol->szName, 10 );
         buffer[ 10 ] = '\0';

         if( HB_IS_STRING( pItem ) && ( hb_itemGetCLen( pItem ) + 1 ) <= SHRT_MAX )
         {
            /* Store the closing zero byte, too */
            USHORT uiLength = ( USHORT ) ( hb_itemGetCLen( pItem ) + 1 );

            buffer[ 11 ] = 'C' + 128;
            buffer[ 16 ] = HB_LOBYTE( uiLength );
            buffer[ 17 ] = HB_HIBYTE( uiLength );

            hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN );
            hb_fsWrite( fhnd, ( BYTE * ) hb_itemGetCPtr( pItem ), uiLength );
         }
         else if( HB_IS_NUMERIC( pItem ) )
         {
            BYTE byNum[ sizeof( double ) ];
            int iWidth;
            int iDec;

            hb_itemGetNLen( pItem, &iWidth, &iDec );

            buffer[ 11 ] = 'N' + 128;
#ifdef HB_C52_STRICT
/* NOTE: This is the buggy, but fully CA-Cl*pper compatible method. [vszakats] */
            buffer[ 16 ] = ( BYTE ) iWidth + ( HB_IS_DOUBLE( pItem ) ? iDec + 1 : 0 );
#else
/* NOTE: This would be the correct method, but Clipper is buggy here. [vszakats] */
            buffer[ 16 ] = ( BYTE ) iWidth + ( iDec == 0 ? 0 : iDec + 1 );
#endif
            buffer[ 17 ] = ( BYTE ) iDec;

            HB_PUT_LE_DOUBLE( byNum, hb_itemGetND( pItem ) );

            hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN );
            hb_fsWrite( fhnd, byNum, sizeof( byNum ) );
         }
         else if( HB_IS_DATE( pItem ) )
         {
            BYTE byNum[ sizeof( double ) ];
            double dNumber = ( double ) hb_itemGetDL( pItem );

            buffer[ 11 ] = 'D' + 128;
            buffer[ 16 ] = 1;
            buffer[ 17 ] = 0;

            HB_PUT_LE_DOUBLE( byNum, dNumber );

            hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN );
            hb_fsWrite( fhnd, byNum, sizeof( byNum ) );
         }
         else if( HB_IS_LOGICAL( pItem ) )
         {
            BYTE byLogical[ 1 ];

            buffer[ 11 ] = 'L' + 128;
            buffer[ 16 ] = sizeof( BYTE );
            buffer[ 17 ] = 0;

            byLogical[ 0 ] = hb_itemGetL( pItem ) ? 1 : 0;

            hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN );
            hb_fsWrite( fhnd, byLogical, sizeof( BYTE ) );
         }
      }
   }
   return TRUE;
}

HB_FUNC( __MVSAVE )
{
   /* Clipper also checks for the number of arguments here */
   if( hb_pcount() == 3 && ISCHAR( 1 ) && ISCHAR( 2 ) && ISLOG( 3 ) )
   {
      PHB_FNAME pFileName;
      char szFileName[ _POSIX_PATH_MAX + 1 ];
      FHANDLE fhnd;

      /* Generate filename */

      pFileName = hb_fsFNameSplit( hb_parc( 1 ) );

      if( hb_set.HB_SET_DEFEXTENSIONS && pFileName->szExtension == NULL )
         pFileName->szExtension = ".mem";

      if( pFileName->szPath == NULL )
         pFileName->szPath = hb_set.HB_SET_DEFAULT;

      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );

      /* Create .mem file */

      do
      {
         fhnd = hb_fsCreate( ( BYTE * ) szFileName, FC_NORMAL );
      }
      while( fhnd == FS_ERROR &&
             hb_errRT_BASE_Ext1( EG_CREATE, 2006, NULL, szFileName,
                                 hb_fsError(), EF_CANDEFAULT | EF_CANRETRY,
                                 HB_ERR_ARGS_BASEPARAMS ) == E_RETRY );

      if( fhnd != FS_ERROR )
      {
         BYTE buffer[ HB_MEM_REC_LEN ];
         MEMVARSAVE_CARGO msc;

         msc.pszMask      = hb_memvarGetMask( 2 );
         msc.bIncludeMask = hb_parl( 3 );
         msc.buffer       = buffer;
         msc.fhnd         = fhnd;

         /* Walk through all visible memory variables and save each one */

         hb_dynsymEval( hb_memvarSave, ( void * ) &msc );

         buffer[ 0 ] = '\x1A';
         hb_fsWrite( fhnd, buffer, 1 );

         hb_fsClose( fhnd );
      }
   }
   else
      /* NOTE: Undocumented error message in CA-Cl*pper 5.2e and 5.3x. [ckedem] */
      hb_errRT_BASE( EG_ARG, 2008, NULL, "__MSAVE", HB_ERR_ARGS_BASEPARAMS );
}

/* NOTE: There's an extension in Harbour, which makes it possible to only
         load (or not load) variable names with a specific name mask.
         [vszakats] */

HB_FUNC( __MVRESTORE )
{
   /* Clipper checks for the number of arguments here here, but we cannot
      in Harbour since we have two optional parameters as an extension. */
   if( ISCHAR( 1 ) && ISLOG( 2 ) )
   {
      PHB_FNAME pFileName;
      char szFileName[ _POSIX_PATH_MAX + 1 ];
      FHANDLE fhnd;

      BOOL bAdditive = hb_parl( 2 );

      /* Clear all memory variables if not ADDITIVE */

      if( ! bAdditive )
         hb_memvarsClear();

      /* Generate filename */

      pFileName = hb_fsFNameSplit( hb_parc( 1 ) );

      if( hb_set.HB_SET_DEFEXTENSIONS && pFileName->szExtension == NULL )
         pFileName->szExtension = ".mem";

      if( pFileName->szPath == NULL )
         pFileName->szPath = hb_set.HB_SET_DEFAULT;

      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );

      /* Open .mem file */

      do
      {
         fhnd = hb_fsOpen( ( BYTE * ) szFileName, FO_READ | FO_DENYWRITE | FO_PRIVATE );
      }
      while( fhnd == FS_ERROR &&
             hb_errRT_BASE_Ext1( EG_OPEN, 2005, NULL, szFileName,
                                 hb_fsError(), EF_CANDEFAULT | EF_CANRETRY,
                                 HB_ERR_ARGS_BASEPARAMS ) == E_RETRY );

      if( fhnd != FS_ERROR )
      {
         BOOL bIncludeMask;
         BYTE buffer[ HB_MEM_REC_LEN ];
         char * pszMask;

         pszMask = hb_memvarGetMask( 3 );
         bIncludeMask = !ISLOG( 4 ) || hb_parl( 4 );

         while( hb_fsRead( fhnd, buffer, HB_MEM_REC_LEN ) == HB_MEM_REC_LEN )
         {
            char *szName = hb_strdup( ( char * ) buffer );
            USHORT uiType = ( USHORT ) ( buffer[ 11 ] - 128 );
            USHORT uiWidth = ( USHORT ) buffer[ 16 ];
            USHORT uiDec = ( USHORT ) buffer[ 17 ];
            PHB_ITEM pItem = NULL;

            switch( uiType )
            {
               case 'C':
               {
                  BYTE * pbyString;

                  uiWidth += uiDec * 256;
                  pbyString = ( BYTE * ) hb_xgrab( uiWidth );

                  if( hb_fsRead( fhnd, pbyString, uiWidth ) == uiWidth )
                     pItem = hb_itemPutCL( NULL, ( char * ) pbyString, uiWidth - 1 );

                  hb_xfree( pbyString );

                  break;
               }

               case 'N':
               {
                  BYTE pbyNumber[ HB_MEM_NUM_LEN ];

                  if( hb_fsRead( fhnd, pbyNumber, HB_MEM_NUM_LEN ) == HB_MEM_NUM_LEN )
                     pItem = hb_itemPutNLen( NULL, HB_GET_LE_DOUBLE( pbyNumber ), uiWidth - ( uiDec ? ( uiDec + 1 ) : 0 ), uiDec );

                  break;
               }

               case 'D':
               {
                  BYTE pbyNumber[ HB_MEM_NUM_LEN ];

                  if( hb_fsRead( fhnd, pbyNumber, HB_MEM_NUM_LEN ) == HB_MEM_NUM_LEN )
                     pItem = hb_itemPutDL( NULL, ( long ) HB_GET_LE_DOUBLE( pbyNumber ) );

                  break;
               }

               case 'L':
               {
                  BYTE pbyLogical[ 1 ];

                  if( hb_fsRead( fhnd, pbyLogical, 1 ) == 1 )
                     pItem = hb_itemPutL( NULL, pbyLogical[ 0 ] != 0 );

                  break;
               }
            }

            if( pItem )
            {
               BOOL bMatch = hb_strMatchCaseWildExact( szName, pszMask );

               /* Process it if it matches the passed mask */
               if( bIncludeMask ? bMatch : ! bMatch )
               {
                  /* the first parameter is a string with not empty variable name */
                  HB_DYNS_PTR pDynVar = hb_memvarFindSymbol( szName, strlen( szName ) );

                  if( pDynVar )
                     /* variable was declared somwhere - assign a new value */
                     hb_memvarSetValue( pDynVar->pSymbol, pItem );
                  else
                     /* attempt to assign a value to undeclared variable create the PRIVATE one */
                     hb_memvarCreateFromDynSymbol( hb_dynsymGet( szName ), VS_PRIVATE, pItem );

                  hb_itemReturn( pItem );
               }

               hb_itemRelease( pItem );
            }

            hb_xfree( szName );
         }

         hb_fsClose( fhnd );
         hb_memvarUpdatePrivatesBase();
      }
      else
         hb_retl( FALSE );
   }
   else
      /* NOTE: Undocumented error message in CA-Cl*pper 5.2e and 5.3x. [ckedem] */
      hb_errRT_BASE( EG_ARG, 2007, NULL, "__MRESTORE", HB_ERR_ARGS_BASEPARAMS );
}

/*
 * This is a hacking function which changes base private offset so
 * PRIVATE variables created in function which calls __MVSETBASE()
 * will not be released when the function exit but will be inherited
 * by its caller. [druzus]
 */
HB_FUNC( __MVSETBASE )
{
   long lOffset = hb_stackBaseProcOffset( 0 );

   if( lOffset > 0 )
      hb_stackItem( lOffset )->item.asSymbol.stackstate->ulPrivateBase =
                                                hb_memvarGetPrivatesBase();
}


/* ----------------------------------------------------------------------- */
/* The garbage collector interface */
/* ----------------------------------------------------------------------- */


/* Mark all memvars as used so they will not be released by the
 * garbage collector
 */
void hb_memvarsIsMemvarRef( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarsIsMemvarRef()"));

   if( s_globalTable )
   {
      ULONG ulCnt = s_globalLastFree;

      while( --ulCnt )
      {
         /* do not check detached variables - for these variables only
          * references from the eval stack are meaningfull for the GC
         */
         if( s_globalTable[ ulCnt ].counter && s_globalTable[ ulCnt ].hPrevMemvar != ( HB_HANDLE )-1 )
         {
            if( HB_IS_GCITEM( s_globalTable[ ulCnt ].pVarItem ) )
               hb_gcItemRef( s_globalTable[ ulCnt ].pVarItem );
         }
      }
   }
}

HB_HANDLE hb_memvarGetVarHandle( char *szName )
{
   PHB_DYNS pDyn;

   if( ( pDyn = hb_dynsymFind( szName ) ) != NULL )
      return  pDyn->hMemvar;
   else
      return 0; /* invalid handle */
}

PHB_ITEM hb_memvarGetValueByHandle( HB_HANDLE hMemvar )
{
   if( hMemvar && hMemvar < s_globalTableSize )
      return  s_globalTable[ hMemvar ].pVarItem;
   else
      return NULL;
}
