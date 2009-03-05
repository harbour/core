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

#if !defined( HB_MT_VM )

#  define hb_dynsymGetMemvar( p )         ( ( PHB_ITEM ) (p)->pMemvar )
#  define hb_dynsymSetMemvar( p, h )      do { (p)->pMemvar = (h); } while(0)

#endif

#define TABLE_INITHB_VALUE   100
#define TABLE_EXPANDHB_VALUE  50

struct mv_PUBLIC_var_info
{
   int iPos;
   BOOL bFound;
   HB_DYNS_PTR pDynSym;
};

struct mv_memvarArray_info
{
   PHB_ITEM pArray;
   PHB_DYNS * pDyns;
   ULONG ulCount;
   int iScope;
};

static void hb_memvarCreateFromDynSymbol( PHB_DYNS pDynVar, BYTE bScope, PHB_ITEM pValue );

static PHB_ITEM hb_memvarValueNew( void )
{
   PHB_ITEM pMemvar;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueNew()"));

   pMemvar = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) );
   pMemvar->type = HB_IT_NIL;

   return pMemvar;
}

/*
 * This function increases the number of references to passed global value
 */
#undef hb_memvarValueIncRef
void hb_memvarValueIncRef( PHB_ITEM pMemvar )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueIncRef(%p)", pMemvar));

   hb_xRefInc( pMemvar );
}

/*
 * This function decreases the number of references to passed global value.
 * If it is the last reference then this value is deleted.
 */
void hb_memvarValueDecRef( PHB_ITEM pMemvar )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarValueDecRef(%p)", pMemvar));

   if( hb_xRefDec( pMemvar ) )
   {
      if( HB_IS_COMPLEX( pMemvar ) )
         hb_itemClear( pMemvar );
      hb_xfree( pMemvar );
   }
}

/*
 * Detach public or private variable (swap current value with a memvar handle)
 */
static void hb_memvarDetachDynSym( PHB_DYNS pDynSym, PHB_ITEM pPrevMemvar )
{
   PHB_ITEM pMemvar;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarDetachDynSym(%p,%p)", pDynSym, pPrevMemvar));

   pMemvar = hb_dynsymGetMemvar( pDynSym );
   hb_dynsymSetMemvar( pDynSym, pPrevMemvar );

   hb_memvarValueDecRef( pMemvar );
}

/*
 * Detach local variable (swap current value with a memvar handle)
 */
HB_ITEM_PTR hb_memvarDetachLocal( PHB_ITEM pLocal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarDetachLocal(%p)", pLocal));

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
      PHB_ITEM pMemvar = hb_memvarValueNew();

      hb_itemRawCpy( pMemvar, pLocal );
      pMemvar->type &= ~HB_IT_DEFAULT;

      pLocal->type = HB_IT_BYREF | HB_IT_MEMVAR;
      pLocal->item.asMemvar.value = pMemvar;
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
static void hb_memvarAddPrivate( PHB_DYNS pDynSym, PHB_ITEM pValue )
{
   PHB_PRIVATE_STACK pPrivateStack;
   PHB_ITEM pMemvar;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarAddPrivate(%p,%p)", pDynSym, pValue));

   pPrivateStack = hb_stackGetPrivateStack();

   pMemvar = hb_dynsymGetMemvar( pDynSym );
   /* If the variable with the same name exists already
    * and it's PRIVATE variable declared in this function then
    * do not push new memvar on PRIVATEs stack
    */
   if( pMemvar )
   {
      ULONG ulCount = pPrivateStack->count;
      while( ulCount > pPrivateStack->base )
      {
         if( pDynSym == pPrivateStack->stack[ ulCount - 1 ].pDynSym )
            break;
         --ulCount;
      }
      if( ulCount <= pPrivateStack->base )
         pMemvar = NULL;
   }

   if( ! pMemvar )
   {
      /* Allocate the value from the end of table
       */
      if( pPrivateStack->count == pPrivateStack->size )
      {
         /* No more free values in the table - expand the table
          */
         if( pPrivateStack->size == 0 )
         {
            pPrivateStack->stack = ( PHB_PRIVATE_ITEM )
                  hb_xgrab( sizeof( HB_PRIVATE_ITEM ) * TABLE_INITHB_VALUE );
            pPrivateStack->size  = TABLE_INITHB_VALUE;
            pPrivateStack->count = pPrivateStack->base = 0;
         }
         else
         {
            pPrivateStack->size += TABLE_EXPANDHB_VALUE;
            pPrivateStack->stack = ( PHB_PRIVATE_ITEM )
                  hb_xrealloc( pPrivateStack->stack,
                               sizeof( HB_PRIVATE_ITEM ) * pPrivateStack->size );
         }
      }

      pPrivateStack->stack[ pPrivateStack->count ].pDynSym = pDynSym;
      pPrivateStack->stack[ pPrivateStack->count++ ].pPrevMemvar = hb_dynsymGetMemvar( pDynSym );

      pMemvar = hb_memvarValueNew();
      hb_dynsymSetMemvar( pDynSym, pMemvar );
   }

   if( pValue )
   {
      hb_itemCopy( pMemvar, pValue );
      /* Remove MEMOFLAG if exists (assignment from field). */
      pMemvar->type &= ~HB_IT_MEMOFLAG;
   }
}

/*
 * This function returns current PRIVATE variables stack base
 */
ULONG hb_memvarGetPrivatesBase( void )
{
   ULONG ulBase;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetPrivatesBase()"));

   ulBase = hb_stackGetPrivateStack()->base;
   hb_stackGetPrivateStack()->base = hb_stackGetPrivateStack()->count;
   return ulBase;
}

/*
 * This function releases PRIVATE variables created after passed base
 */
void hb_memvarSetPrivatesBase( ULONG ulBase )
{
   PHB_PRIVATE_STACK pPrivateStack;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarSetPrivatesBase(%lu)", ulBase));

   pPrivateStack = hb_stackGetPrivateStack();

   while( pPrivateStack->count > pPrivateStack->base )
   {
      PHB_DYNS pDynSym = pPrivateStack->stack[ --pPrivateStack->count ].pDynSym;

      if( hb_dynsymGetMemvar( pDynSym ) )
      {
         /* Restore previous value for variables that were overridden
          */
         hb_memvarDetachDynSym( pDynSym, pPrivateStack->stack[ pPrivateStack->count ].pPrevMemvar );
      }
   }
   pPrivateStack->base = ulBase;
}

/*
 * Update PRIVATE base ofsset so they will not be removed
 * when function return
 */
void hb_memvarUpdatePrivatesBase( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarUpdatePrivatesBase()"));

   hb_stackGetPrivateStack()->base = hb_stackGetPrivateStack()->count;
}

/*
 * Reset PRIVATE base offset to the level of previous function
 */
static void hb_memvarResetPrivatesBase( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarResetPrivatesBase()"));

   hb_stackGetPrivateStack()->base = hb_stackBaseItem()->item.asSymbol.stackstate->ulPrivateBase;
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

   pDyn = pMemvarSymb->pDynSym;
   if( pDyn )
   {
      PHB_ITEM pMemvar;

      pMemvar = hb_dynsymGetMemvar( pDyn );

      HB_TRACE(HB_TR_INFO, ("Memvar item (%p)(%s) assigned", pMemvar, pMemvarSymb->szName));

      if( pMemvar )
      {
         /* value is already created */
         hb_itemCopyToRef( pMemvar, pItem );
         /* Remove MEMOFLAG if exists (assignment from field). */
         pMemvar->type &= ~HB_IT_MEMOFLAG;
      }
      else
      {
         /* assignment to undeclared memvar - PRIVATE is assumed */
         hb_memvarCreateFromDynSymbol( pDyn, VS_PRIVATE, pItem );
      }

   }
   else
      hb_errInternal( HB_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );
}

HB_ERRCODE hb_memvarGet( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb )
{
   PHB_DYNS pDyn;
   HB_ERRCODE bSuccess = HB_FAILURE;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGet(%p, %p)", pItem, pMemvarSymb));

   pDyn = pMemvarSymb->pDynSym;
   if( pDyn )
   {
      PHB_ITEM pMemvar;


      pMemvar = hb_dynsymGetMemvar( pDyn );

      HB_TRACE(HB_TR_INFO, ("Memvar item (%p)(%s) queried", pMemvar, pMemvarSymb->szName));

      if( pMemvar )
      {
         /* value is already created
          */
         if( HB_IS_BYREF( pMemvar ) )
            hb_itemCopy( pItem, hb_itemUnRef( pMemvar ) );
         else
            hb_itemCopy( pItem, pMemvar );
         bSuccess = HB_SUCCESS;
      }
   }
   else
      hb_errInternal( HB_EI_MVBADSYMBOL, NULL, pMemvarSymb->szName, NULL );

   return bSuccess;
}

void hb_memvarGetValue( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetValue(%p, %p)", pItem, pMemvarSymb));

   if( hb_memvarGet( pItem, pMemvarSymb ) == HB_FAILURE )
   {
      /* Generate an error with retry possibility
       * (user created error handler can create this variable)
       */
      HB_ITEM_PTR pError;

      pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                             NULL, pMemvarSymb->szName, 0, EF_CANRETRY );

      while( hb_errLaunch( pError ) == E_RETRY )
      {
         if( hb_memvarGet( pItem, pMemvarSymb ) == HB_SUCCESS )
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
      PHB_ITEM pMemvar;

      pMemvar = hb_dynsymGetMemvar( pDyn );

      HB_TRACE(HB_TR_INFO, ("Memvar item (%p)(%s) referenced", pMemvar, pMemvarSymb->szName));

      if( pMemvar )
      {
         if( HB_IS_BYREF( pMemvar ) && !HB_IS_ENUM( pMemvar ) )
            hb_itemCopy( pItem, pMemvar );
         else
         {
            /* value is already created */
            pItem->type = HB_IT_BYREF | HB_IT_MEMVAR;
            pItem->item.asMemvar.value = pMemvar;
            hb_xRefInc( pMemvar );
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
            pMemvar = hb_dynsymGetMemvar( pDyn );
            if( pMemvar )
            {
               if( HB_IS_BYREF( pMemvar ) && !HB_IS_ENUM( pMemvar ) )
                  hb_itemCopy( pItem, pMemvar );
               else
               {
                  /* value is already created */
                  pItem->type = HB_IT_BYREF | HB_IT_MEMVAR;
                  pItem->item.asMemvar.value = pMemvar;
                  hb_xRefInc( pMemvar );
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
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetItem(%p)", pMemvarSymb));

   if( pMemvarSymb->pDynSym )
   {
      PHB_ITEM pMemvar = hb_dynsymGetMemvar( pMemvarSymb->pDynSym );

      if( pMemvar )
      {
         if( HB_IS_BYREF( pMemvar ) )
            return hb_itemUnRef( pMemvar );
         else
            return pMemvar;
      }
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

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarGetStrValuePtr(%s, %p)", szVarName, pulLen));

   pDynVar = hb_memvarFindSymbol( szVarName, *pulLen );

   if( pDynVar )
   {
      /* there is dynamic symbol with the requested name - check if it is
       * a memvar variable
       */
      PHB_ITEM pMemvar = hb_dynsymGetMemvar( pDynVar );

      if( pMemvar )
      {
         /* variable contains some data
          */
         if( HB_IS_BYREF( pMemvar ) )
            pMemvar = hb_itemUnRef( pMemvar );

         if( HB_IS_STRING( pMemvar ) )
         {
            szValue = pMemvar->item.asString.value;
            *pulLen = pMemvar->item.asString.length;
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
      if( ! hb_dynsymGetMemvar( pDynVar ) )
      {
         PHB_ITEM pMemvar = hb_memvarValueNew();

         hb_dynsymSetMemvar( pDynVar, pMemvar );

         if( pValue )
         {
            hb_itemCopy( pMemvar, pValue );
            /* Remove MEMOFLAG if exists (assignment from field). */
            pMemvar->type &= ~HB_IT_MEMOFLAG;
         }
         else
         {
            /* new PUBLIC variable - initialize it to .F.
             */
            pMemvar->type = HB_IT_LOGICAL;

            /* NOTE: PUBLIC variables named CLIPPER and HARBOUR are initialized */
            /*       to .T., this is normal Clipper behaviour. [vszakats] */

            pMemvar->item.asLogical.value =
                        ( strcmp( pDynVar->pSymbol->szName, "HARBOUR" ) == 0 ||
                          strcmp( pDynVar->pSymbol->szName, "CLIPPER" ) == 0 );
         }
      }
   }
   else
   {
      /* Create new PRIVATE var and add it to the PRIVATE variables stack
       */
      hb_memvarAddPrivate( pDynVar, pValue );
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

      if( pDynSymbol && hb_dynsymGetMemvar( pDynSymbol ) )
      {
         ULONG ulBase = hb_stackGetPrivateStack()->count;

         /* Find the variable with a requested name that is currently visible
          * Start from the top of the stack.
          */
         while( ulBase > 0 )
         {
            if( pDynSymbol == hb_stackGetPrivateStack()->stack[ --ulBase ].pDynSym )
            {
               /* reset current value to NIL - the overriden variables will be
                * visible after exit from current procedure
                */
               PHB_ITEM pMemvar = hb_dynsymGetMemvar( pDynSymbol );
               if( pMemvar )
                  hb_itemClear( pMemvar );
               return;
            }
         }

         /* No match found for PRIVATEs - it's PUBLIC so let's remove it.
          */
         hb_memvarDetachDynSym( pDynSymbol, NULL );
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
static void hb_memvarReleaseWithMask( const char *szMask, BOOL bInclude )
{
   ULONG ulBase = hb_stackGetPrivateStack()->count;
   PHB_DYNS pDynVar;
   PHB_ITEM pMemvar;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarReleaseWithMask(%s, %d)", szMask, (int) bInclude));

   while( ulBase > hb_stackGetPrivateStack()->base )
   {
      --ulBase;
      pDynVar = hb_stackGetPrivateStack()->stack[ ulBase ].pDynSym;
      /* reset current value to NIL - the overriden variables will be
       * visible after exit from current procedure
       */
      pMemvar = hb_dynsymGetMemvar( pDynVar );
      if( pMemvar )
      {
         BOOL fMatch = hb_strMatchCaseWildExact( pDynVar->pSymbol->szName, szMask );
         if( bInclude ? fMatch : !fMatch )
            hb_itemClear( pMemvar );
      }
   }
}

/* Checks if passed dynamic symbol is a variable and returns its scope
 */
static int hb_memvarScopeGet( PHB_DYNS pDynVar )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_memvarScopeGet(%p)", pDynVar));

   if( hb_dynsymGetMemvar( pDynVar ) == 0 )
      return HB_MV_UNKNOWN;
   else
   {
      ULONG ulBase = hb_stackGetPrivateStack()->count;    /* start from the top of the stack */

      while( ulBase )
      {
         if( pDynVar == hb_stackGetPrivateStack()->stack[ --ulBase ].pDynSym )
         {
            if( ulBase >= hb_stackGetPrivateStack()->base )
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

#if !defined( HB_MT_VM )
/* Releases memory occupied by a variable
 */
static HB_DYNS_FUNC( hb_memvarClear )
{
   if( pDynSymbol != ( PHB_DYNS ) Cargo &&
       hb_dynsymGetMemvar( pDynSymbol ) )
      hb_memvarDetachDynSym( pDynSymbol, NULL );

   return TRUE;
}
#endif

/* Clear all memvar variables optionally without GetList PUBLIC variable */
void hb_memvarsClear( BOOL fAll )
{
   PHB_DYNS pGetList;

   HB_TRACE(HB_TR_DEBUG, ("hb_memvarsClear(%d)", ( int ) fAll));

   pGetList = fAll ? NULL : hb_dynsymFind( "GETLIST" );

   hb_stackClearMemvarsBase();
   hb_stackGetPrivateStack()->base = 0;
   hb_memvarSetPrivatesBase( 0 );
#if !defined( HB_MT_VM )
   hb_dynsymEval( hb_memvarClear, ( void * ) pGetList );
#else
   /* this is a little bit hacked but many times faster version
    * of memvars clearing because it scans only given thread stack
    * not global dynamic symbol table. It noticeable reduce the cost
    * of HVM thread releasing.
    */
   hb_stackClearMemvars( pGetList ? ( int ) pGetList->uiSymNum : -1 );
#endif
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

      hb_dynsymProtectEval( hb_memvarCountPublics, ( void * ) &iPublicCnt );
      return iPublicCnt;
   }
   else
      return hb_stackGetPrivateStack()->count;  /* number of PRIVATE variables */
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
         bCont = FALSE;
      }
   }

   return bCont;
}

/* Returns the pointer to item that holds a value of variable (or NULL if
 * not found). It fills also the pointer to the variable name
 * Both pointers points to existing and used data - they shouldn't be
 * deallocated.
 */
static HB_ITEM_PTR hb_memvarDebugVariable( int iScope, int iPos, const char ** pszName )
{
   PHB_ITEM pValue = NULL;
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
         hb_dynsymProtectEval( hb_memvarFindPublicByPos, ( void * ) &struPub );
         if( struPub.bFound )
         {
            pValue = hb_dynsymGetMemvar( struPub.pDynSym );
            *pszName = struPub.pDynSym->pSymbol->szName;
         }
      }
      else
      {
         if( ( ULONG ) iPos < hb_stackGetPrivateStack()->count )
         {
            HB_DYNS_PTR pDynSym = hb_stackGetPrivateStack()->stack[ iPos ].pDynSym;

            pValue = hb_dynsymGetMemvar( pDynSym );
            *pszName = pDynSym->pSymbol->szName;
         }
      }
   }

   return pValue;
}

static HB_DYNS_FUNC( hb_memvarCountVisible )
{
   PHB_ITEM pMemvar = hb_dynsymGetMemvar( pDynSymbol );

   if( pMemvar )
   {
      struct mv_memvarArray_info *pMVInfo = ( struct mv_memvarArray_info * ) Cargo;
      if( !pMVInfo->iScope ||
          ( hb_memvarScopeGet( pDynSymbol ) & pMVInfo->iScope ) != 0 )
      {
         pMVInfo->pDyns[ pMVInfo->ulCount++ ] = pDynSymbol;
      }
   }
   return TRUE;
}

PHB_ITEM hb_memvarSaveInArray( int iScope, BOOL fCopy )
{
   struct mv_memvarArray_info MVInfo;
   PHB_ITEM pArray, pItem, pMemvar;
   PHB_DYNS pDynSymbol;

   pArray = NULL;

   iScope &= HB_MV_PUBLIC | HB_MV_PRIVATE;
   if( iScope == ( HB_MV_PUBLIC | HB_MV_PRIVATE ) )
      iScope = 0;

#if !defined( HB_MT_VM )
   MVInfo.pDyns = ( PHB_DYNS * ) hb_xgrab( hb_dynsymCount() *
                                           sizeof( PHB_DYNS ) );
#else
   MVInfo.pDyns = ( PHB_DYNS * ) hb_xgrab( hb_stackDynHandlesCount() *
                                           sizeof( PHB_DYNS ) );
#endif
   MVInfo.ulCount = 0;
   MVInfo.iScope = iScope;

   hb_dynsymProtectEval( hb_memvarCountVisible, ( void * ) &MVInfo );
   if( MVInfo.ulCount > 0 )
   {
      pArray = hb_itemArrayNew( MVInfo.ulCount );
      do
      {
         pItem = hb_arrayGetItemPtr( pArray, MVInfo.ulCount );
         pDynSymbol = MVInfo.pDyns[ --MVInfo.ulCount ];
         pMemvar = hb_dynsymGetMemvar( pDynSymbol ),

         hb_arrayNew( pItem, 2 );
         hb_arraySetSymbol( pItem, 1, pDynSymbol->pSymbol );
         pItem = hb_arrayGetItemPtr( pItem, 2 );
         if( fCopy )
         {
            hb_itemCopy( pItem, pMemvar );
            hb_memvarDetachLocal( pItem );
         }
         else
         {
            pItem->type = HB_IT_BYREF | HB_IT_MEMVAR;
            pItem->item.asMemvar.value = pMemvar;
            hb_xRefInc( pMemvar );
         }
      }
      while( MVInfo.ulCount );
   }
   hb_xfree( MVInfo.pDyns );

   return pArray;
}

void hb_memvarRestoreFromArray( PHB_ITEM pArray )
{
   ULONG ulCount, ulPos;

   ulCount = hb_arrayLen( pArray );
   for( ulPos = 1; ulPos <= ulCount; ++ulPos )
   {
      PHB_ITEM pItem = hb_arrayGetItemPtr( pArray, ulPos );
      PHB_DYNS pDynSym = hb_arrayGetSymbol( pItem, 1 )->pDynSym;
      PHB_ITEM pMemvar = hb_arrayGetItemPtr( pItem, 2 )->item.asMemvar.value;
      hb_memvarValueIncRef( pMemvar );
      if( hb_dynsymGetMemvar( pDynSym ) )
         hb_memvarDetachDynSym( pDynSym, pMemvar );
      else
         hb_dynsymSetMemvar( pDynSym, pMemvar );
   }
}

/* ************************************************************************** */

static const char * hb_memvarGetMask( int iParam )
{
   const char * pszMask = hb_parc( iParam );
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

      hb_memvarResetPrivatesBase();
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
      const char * pszMask;

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
   hb_memvarsClear( FALSE );
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
   hb_retl( pDyn && hb_dynsymGetMemvar( pDyn ) );
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
   const char * pszMask;
   BOOL bIncludeMask;
   BYTE * buffer;
   HB_FHANDLE fhnd;
} MEMVARSAVE_CARGO;

/* saves a variable to a mem file already open */

static HB_DYNS_FUNC( hb_memvarSave )
{
   const char * pszMask    = ( ( MEMVARSAVE_CARGO * ) Cargo )->pszMask;
   BOOL bIncludeMask = ( ( MEMVARSAVE_CARGO * ) Cargo )->bIncludeMask;
   BYTE * buffer     = ( ( MEMVARSAVE_CARGO * ) Cargo )->buffer;
   HB_FHANDLE fhnd   = ( ( MEMVARSAVE_CARGO * ) Cargo )->fhnd;
   PHB_ITEM pMemvar;

   /* NOTE: Harbour name lengths are not limited, but the .mem file
            structure is not flexible enough to allow for it.
            [vszakats] */

   pMemvar = hb_dynsymGetMemvar( pDynSymbol );
   if( pMemvar )
   {
      BOOL bMatch = hb_strMatchCaseWildExact( pDynSymbol->pSymbol->szName, pszMask );

      /* Process it if it matches the passed mask */
      if( bIncludeMask ? bMatch : ! bMatch )
      {
         /* NOTE: Clipper will not initialize the record buffer with
                  zeros, so they will look trashed. [vszakats] */
         memset( buffer, 0, HB_MEM_REC_LEN );

         /* NOTE: Save only the first 10 characters of the name */
         hb_strncpy( ( char * ) buffer, pDynSymbol->pSymbol->szName, 10 );

         if( HB_IS_STRING( pMemvar ) )
         {
            /* Store the closing zero byte, too */
            ULONG ulLen = hb_itemGetCLen( pMemvar ) + 1;
            int iOverFlow = 0;

            /* Clipper support only 64KB strings */
            if( ulLen > USHRT_MAX )
            {
               ulLen = USHRT_MAX;
               iOverFlow = 1;
            }
            buffer[ 11 ] = 'C' + 128;
            HB_PUT_LE_UINT16( &buffer[ 16 ], ulLen );
            hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN );
            hb_fsWriteLarge( fhnd, ( BYTE * ) hb_itemGetCPtr( pMemvar ), ulLen - iOverFlow );
            if( iOverFlow )
               hb_fsWrite( fhnd, ( const BYTE * ) "\0", 1 );
         }
         else if( HB_IS_NUMERIC( pMemvar ) )
         {
            double dNumber;
            int iWidth;
            int iDec;

            dNumber = hb_itemGetND( pMemvar );
            hb_itemGetNLen( pMemvar, &iWidth, &iDec );
            buffer[ 11 ] = 'N' + 128;
#ifdef HB_C52_STRICT
/* NOTE: This is the buggy, but fully CA-Cl*pper compatible method. [vszakats] */
            buffer[ 16 ] = ( BYTE ) iWidth + ( HB_IS_DOUBLE( pMemvar ) ? ( BYTE ) ( iDec + 1 ) : 0 );
#else
/* NOTE: This would be the correct method, but Clipper is buggy here. [vszakats] */
            buffer[ 16 ] = ( BYTE ) iWidth + ( iDec == 0 ? 0 : ( BYTE ) ( iDec + 1 ) );
#endif
            buffer[ 17 ] = ( BYTE ) iDec;
            HB_PUT_LE_DOUBLE( &buffer[ HB_MEM_REC_LEN ], dNumber );
            hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN + HB_MEM_NUM_LEN );
         }
         else if( HB_IS_DATE( pMemvar ) )
         {
            double dNumber = ( double ) hb_itemGetDL( pMemvar );

            buffer[ 11 ] = 'D' + 128;
            buffer[ 16 ] = 1;
            buffer[ 17 ] = 0;
            HB_PUT_LE_DOUBLE( &buffer[ HB_MEM_REC_LEN ], dNumber );
            hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN + HB_MEM_NUM_LEN );
         }
         else if( HB_IS_LOGICAL( pMemvar ) )
         {
            buffer[ 11 ] = 'L' + 128;
            buffer[ 16 ] = 1;
            buffer[ 17 ] = 0;
            buffer[ HB_MEM_REC_LEN ] = hb_itemGetL( pMemvar ) ? 1 : 0;
            hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN + 1 );
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
      PHB_ITEM pError = NULL;
      PHB_FNAME pFileName;
      char szFileName[ _POSIX_PATH_MAX + 1 ];
      HB_FHANDLE fhnd;

      /* Generate filename */

      pFileName = hb_fsFNameSplit( hb_parc( 1 ) );

      if( pFileName->szExtension == NULL && hb_stackSetStruct()->HB_SET_DEFEXTENSIONS )
         pFileName->szExtension = ".mem";

      if( ! pFileName->szPath )
         pFileName->szPath = hb_stackSetStruct()->HB_SET_DEFAULT;

      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );

      /* Create .mem file */
      do
      {
         fhnd = hb_fsExtOpen( ( BYTE * ) szFileName, NULL,
                              FXO_TRUNCATE | FO_READWRITE | FO_EXCLUSIVE |
                              FXO_DEFAULTS | FXO_SHARELOCK,
                              NULL, pError );
         if( fhnd == FS_ERROR )
         {
            pError = hb_errRT_FileError( pError, NULL, EG_CREATE, 2006, szFileName );
            if( hb_errLaunch( pError ) != E_RETRY )
               break;
         }
      }
      while( fhnd == FS_ERROR );

      if( fhnd != FS_ERROR )
      {
         BYTE buffer[ HB_MEM_REC_LEN + HB_MEM_NUM_LEN ];
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

      if( pError )
         hb_itemRelease( pError );
   }
   else
      /* NOTE: Undocumented error message in CA-Cl*pper 5.2e and 5.3x. [ckedem] */
      hb_errRT_BASE( EG_ARG, 2008, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* NOTE: There's an extension in Harbour, which makes it possible to only
         load (or not load) variable names with a specific name mask.
         [vszakats] */

HB_FUNC( __MVRESTORE )
{
   /* Clipper checks for the number of arguments here here, but we cannot
      in Harbour since we have two optional parameters as an extension. */
#ifdef HB_C52_STRICT
   if( hb_pcount() == 2 && ISCHAR( 1 ) && ISLOG( 2 ) )
#else
   if( ISCHAR( 1 ) && ISLOG( 2 ) )
#endif
   {
      PHB_ITEM pError = NULL;
      PHB_FNAME pFileName;
      char szFileName[ _POSIX_PATH_MAX + 1 ];
      HB_FHANDLE fhnd;

      BOOL bAdditive = hb_parl( 2 );

      /* Clear all memory variables if not ADDITIVE */

      if( ! bAdditive )
         hb_memvarsClear( FALSE );

      /* Generate filename */

      pFileName = hb_fsFNameSplit( hb_parc( 1 ) );

      if( pFileName->szExtension == NULL && hb_stackSetStruct()->HB_SET_DEFEXTENSIONS )
         pFileName->szExtension = ".mem";

      if( ! pFileName->szPath )
         pFileName->szPath = hb_stackSetStruct()->HB_SET_DEFAULT;

      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );

      /* Open .mem file */
      do
      {
         fhnd = hb_fsExtOpen( ( BYTE * ) szFileName, NULL,
                              FO_READ | FXO_DEFAULTS | FXO_SHARELOCK,
                              NULL, pError );
         if( fhnd == FS_ERROR )
         {
            pError = hb_errRT_FileError( pError, NULL, EG_OPEN, 2005, szFileName );
            if( hb_errLaunch( pError ) != E_RETRY )
               break;
         }
      }
      while( fhnd == FS_ERROR );

      if( fhnd != FS_ERROR )
      {
         BOOL bIncludeMask;
         BYTE buffer[ HB_MEM_REC_LEN ];
         const char * pszMask;
         char *szName;
         PHB_ITEM pItem = NULL;

#ifdef HB_C52_STRICT
         pszMask = "*";
         bIncludeMask = TRUE;
#else
         pszMask = hb_memvarGetMask( 3 );
         bIncludeMask = !ISLOG( 4 ) || hb_parl( 4 );
#endif

         while( hb_fsRead( fhnd, buffer, HB_MEM_REC_LEN ) == HB_MEM_REC_LEN )
         {
            /* FoxPro does not add 128 to item type: 'N', 'C', 'D', 'L'
             * CA-Cl*pper respects it and read such files so we also should.
             */
            USHORT uiType = ( USHORT ) ( buffer[ 11 ] & 0x7f );
            USHORT uiWidth = ( USHORT ) buffer[ 16 ];
            USHORT uiDec = ( USHORT ) buffer[ 17 ];

            /* protect against corrupted files */
            buffer[ 10 ] = '\0';
            szName = ( char * ) buffer;

            switch( uiType )
            {
               case 'C':
               {
                  BYTE * pbyString;

                  uiWidth += uiDec * 256;
                  pbyString = ( BYTE * ) hb_xgrab( uiWidth );

                  if( hb_fsRead( fhnd, pbyString, uiWidth ) == uiWidth )
                     pItem = hb_itemPutCLPtr( pItem, ( char * ) pbyString, uiWidth - 1 );
                  else
                  {
                     hb_xfree( pbyString );
                     szName = NULL;
                  }

                  break;
               }

               case 'N':
               {
                  BYTE pbyNumber[ HB_MEM_NUM_LEN ];

                  if( hb_fsRead( fhnd, pbyNumber, HB_MEM_NUM_LEN ) == HB_MEM_NUM_LEN )
                     pItem = hb_itemPutNLen( pItem, HB_GET_LE_DOUBLE( pbyNumber ), uiWidth - ( uiDec ? ( uiDec + 1 ) : 0 ), uiDec );
                  else
                     szName = NULL;

                  break;
               }

               case 'D':
               {
                  BYTE pbyNumber[ HB_MEM_NUM_LEN ];

                  if( hb_fsRead( fhnd, pbyNumber, HB_MEM_NUM_LEN ) == HB_MEM_NUM_LEN )
                     pItem = hb_itemPutDL( pItem, ( long ) HB_GET_LE_DOUBLE( pbyNumber ) );
                  else
                     szName = NULL;

                  break;
               }

               case 'L':
               {
                  BYTE pbyLogical[ 1 ];

                  if( hb_fsRead( fhnd, pbyLogical, 1 ) == 1 )
                     pItem = hb_itemPutL( pItem, pbyLogical[ 0 ] != 0 );
                  else
                     szName = NULL;

                  break;
               }

               default:
                  szName = NULL;
            }

            if( szName )
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
               }
            }
         }

         hb_fsClose( fhnd );
         hb_memvarUpdatePrivatesBase();
         hb_itemReturnRelease( pItem );
      }
      else
         hb_retl( FALSE );

      if( pError )
         hb_itemRelease( pError );
   }
   else
      /* NOTE: Undocumented error message in CA-Cl*pper 5.2e and 5.3x. [ckedem] */
      hb_errRT_BASE( EG_ARG, 2007, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
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

/* debugger function */
PHB_ITEM hb_memvarGetValueBySym( PHB_DYNS pDynSym )
{
   return hb_dynsymGetMemvar( pDynSym );
}
