/*
 * $Id$
 */

/*
   Harbour Project source code

   This file is a part of Harbour Runtime Library and it contains code
   that handles memory variables.

   Copyright (C) 1999 Ryszard Glab
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

*/

/* Harbour Project source code
   http://www.Harbour-Project.org/

   The following functions are Copyright 1999 Victor Szel <info@szelvesz.hu>:
      HB___MVSAVE()
      HB___MVRESTORE() (Thanks to Dave Pearson and Jo French for the original
                        Clipper function (FReadMem()) to read .MEM files)
      HB___MSAVE()
      HB___MRESTORE()

   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

#include <ctype.h> /* for toupper() function */

#include "extend.h"
#include "itemapi.h"
#include "errorapi.h"
#include "dates.h" /* for __MVSAVE()/__MVRESTORE() */
#include "filesys.h" /* for __MVSAVE()/__MVRESTORE() */
#include "error.ch"
#include "memvars.ch"

#define VS_PRIVATE     64
#define VS_PUBLIC     128

static PHB_DYNS * s_privateStack  = NULL;
static ULONG s_privateStackSize = 0;
static ULONG s_privateStackCnt  = 0;
static ULONG s_privateStackBase = 0;

static ULONG s_globalTableSize = 0;
static ULONG s_globalFirstFree = 0;
static ULONG s_globalLastFree  = 0;
static ULONG s_globalFreeCnt   = 0;
static HB_VALUE_PTR s_globalTable = NULL;

#define TABLE_INITHB_VALUE   100
#define TABLE_EXPANDHB_VALUE  50

struct mv_PUBLIC_var_info
{
   int iPos;
   BOOL bFound;
   HB_DYNS_PTR pDynSym;
};


/* Uncomment this to trace memvars activity
#define MEMVARDEBUG
*/

static void hb_memvarCreateFromItem( PHB_ITEM, BYTE, PHB_ITEM );
static void hb_memvarCreateFromDynSymbol( PHB_DYNS, BYTE, PHB_ITEM );
static void hb_memvarAddPrivate( PHB_DYNS );

void hb_memvarsInit( void )
{
   s_globalTable = ( HB_VALUE_PTR ) hb_xgrab( sizeof( HB_VALUE ) * TABLE_INITHB_VALUE );
   s_globalTableSize = TABLE_INITHB_VALUE;
   s_globalFreeCnt   = 0;
   s_globalFirstFree = s_globalLastFree = 1;

   s_privateStack = ( PHB_DYNS * ) hb_xgrab( sizeof( PHB_DYNS ) * TABLE_INITHB_VALUE );
   s_privateStackSize = TABLE_INITHB_VALUE;
   s_privateStackCnt  = s_privateStackBase = 0;
}


void hb_memvarsRelease( void )
{
   ULONG ulCnt = s_globalLastFree;

   if( s_globalTable )
   {
      while( ulCnt )
      {
         if( s_globalTable[ ulCnt ].counter )
         {
            hb_itemClear( &s_globalTable[ ulCnt ].item );
            s_globalTable[ ulCnt ].counter = 0;
         }
         --ulCnt;
      }

      hb_xfree( s_globalTable );
   }

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
 * bTrueMemvar = TRUE | FALSE
 *    FALSE if function is called to create memvar variable for a codeblock
 *       (to store detached local variable) - in this case we have to do
 *       exact copy of passed item (without duplicating its value and
 *       without reference decrementing)
 *    TRUE if we are creating regular memvar variable (PUBLI or PRIVATE)
 *       In this case we have to do normal item coping.
 *
 * Returns:
 *  handle to variable memory or fails
 *
*/
HB_HANDLE hb_memvarValueNew( HB_ITEM_PTR pSource, BOOL bTrueMemvar )
{
   HB_VALUE_PTR pValue;
   HB_HANDLE hValue = 1;   /* handle 0 is reserved */

   if( s_globalFreeCnt )
   {
      /* There are holes in the table
         * Get a first available hole
         */
      hValue = s_globalFirstFree;
      --s_globalFreeCnt;

      /* Now find the next hole
         */
      if( s_globalFreeCnt )
      {
         ++s_globalFirstFree;
         while( s_globalTable[ s_globalFirstFree ].counter )
            ++s_globalFirstFree;
      }
      else
         /* No more holes
            */
         s_globalFirstFree = s_globalLastFree;
   }
   else
   {
      /* Allocate the value from the end of table
         */
      if( s_globalFirstFree < s_globalTableSize )
      {
         hValue = s_globalFirstFree;
         s_globalFirstFree = ++s_globalLastFree;
      }
      else
      {
         /* No more free values in the table - expand the table
            */
         hValue = s_globalTableSize;
         s_globalFirstFree = s_globalLastFree = s_globalTableSize + 1;
         s_globalTableSize += TABLE_EXPANDHB_VALUE;
         s_globalTable = ( HB_VALUE_PTR ) hb_xrealloc( s_globalTable, sizeof( HB_VALUE ) * s_globalTableSize );
      }

   }

   pValue = s_globalTable + hValue;
   pValue->counter = 1;
   if( pSource )
   {
      if( bTrueMemvar )
         hb_itemCopy( &pValue->item, pSource );
      else
         memcpy( &pValue->item, pSource, sizeof( HB_ITEM ) );
   }
   else
      pValue->item.type = IT_NIL;

   #ifdef MEMVARDEBUG
      printf( "\n>>>>>Memvar item created with handle =%i", hValue );
   #endif

   return hValue;
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
   s_privateStackBase = s_privateStackCnt;
   return ulBase;
}

/*
 * This function releases PRIVATE variables created after passed base
 *
 */
void hb_memvarSetPrivatesBase( ULONG ulBase )
{
   HB_HANDLE hVar, hOldValue;

   while( s_privateStackCnt > s_privateStackBase )
   {
      --s_privateStackCnt;
      hVar = s_privateStack[ s_privateStackCnt ]->hMemvar;
      hOldValue = s_globalTable[ hVar ].hPrevMemvar;
      hb_memvarValueDecRef( hVar );
      /*
      * Restore previous value for variables that were overridden
      */
      s_privateStack[ s_privateStackCnt ]->hMemvar = hOldValue;
   }
   s_privateStackBase = ulBase;
}

/*
 * This function increases the number of references to passed global value
 *
 */
void hb_memvarValueIncRef( HB_HANDLE hValue )
{
   #ifdef MEMVARDEBUG
   if( hValue < 1 || hValue > s_globalTableSize )
   {
      printf( "\nInvalid MEMVAR handle %i (max %li)\n", hValue, s_globalTableSize );
      exit( 1 );
   }
   #endif

   s_globalTable[ hValue ].counter++;

   #ifdef MEMVARDEBUG
      printf( "\n+++Memvar item (%i) increment refCounter=%li", hValue, s_globalTable[ hValue ].counter );
   #endif
}

/*
 * This function decreases the number of references to passed global value.
 * If it is the last reference then this value is deleted.
 *
 */
void hb_memvarValueDecRef( HB_HANDLE hValue )
{
   HB_VALUE_PTR pValue;

   #ifdef MEMVARDEBUG
   if( hValue < 1 || hValue > s_globalTableSize )
   {
      printf( "\nInvalid MEMVAR handle %i (max %li)\n", hValue, s_globalTableSize );
      exit( 1 );
   }
   #endif

   pValue = s_globalTable + hValue;
   #ifdef MEMVARDEBUG
      printf( "\n---Memvar item (%i) decrement refCounter=%li", hValue, pValue->counter-1 );
   #endif
   if( pValue->counter > 0 )
   {
      if( --pValue->counter == 0 )
      {
         hb_itemClear( &pValue->item );
         if( s_globalFirstFree > hValue )
         {
            if( ( s_globalLastFree - hValue ) == 1 )
               s_globalFirstFree = s_globalLastFree = hValue;     /* last item */
            else
            {
               s_globalFirstFree = hValue;
               ++s_globalFreeCnt;             /* middle item */
            }
         }
         else if( ( s_globalLastFree - hValue ) == 1 )
         {
            s_globalLastFree = hValue;         /* last item */
            if( s_globalLastFree == s_globalFirstFree )
               s_globalFreeCnt = 0;
         }
         else
            ++s_globalFreeCnt;

      #ifdef MEMVARDEBUG
         printf( "\n<<<<<Memvar item (%i) deleted", hValue );
      #endif
      }
   }
/* This can happen if for example PUBLIC variable holds a codeblock with
 * detached variable. When hb_memvarsRelease() is called then detached
 * variable can be released before the codeblock. So if the codeblock
 * will be released later then it will try to release again this detached
 * variable.
   else
      printf( "\n Attempt to release released item " );
 */
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
   PHB_DYNS pDyn = ( PHB_DYNS ) pMemvarSymb->pDynSym;

   if( pDyn )
   {
      #ifdef MEMVARDEBUG
         printf( "\n::::Memvar item (%i)(%s) assigned", pDyn->hMemvar, pMemvarSymb->szName );
      #endif
      if( pDyn->hMemvar )
      {
         /* value is already created */
         HB_ITEM_PTR pSetItem = &s_globalTable[ pDyn->hMemvar ].item;
         if( IS_BYREF( pSetItem ) )
            hb_itemCopy( hb_itemUnRef( pSetItem ), pItem );
         else
            hb_itemCopy( pSetItem, pItem );
      }
      else
      {
         /* assignment to undeclared memvar - PRIVATE is assumed
          */
         hb_memvarCreateFromDynSymbol( pDyn, VS_PRIVATE, pItem );
      }
   }
   else
      hb_errRT_BASE( EG_NOVAR, 1003, NULL, pMemvarSymb->szName );
}

void hb_memvarGetValue( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb )
{
   PHB_DYNS pDyn = ( PHB_DYNS ) pMemvarSymb->pDynSym;

   if( pDyn )
   {
      #ifdef MEMVARDEBUG
         printf( "\n????Memvar item (%i)(%s) queried", pDyn->hMemvar, pMemvarSymb->szName );
      #endif
      if( pDyn->hMemvar )
      {
         /* value is already created
          */
         HB_ITEM_PTR pGetItem = &s_globalTable[ pDyn->hMemvar ].item;
         if( IS_BYREF( pGetItem ) )
            hb_itemCopy( pItem, hb_itemUnRef( pGetItem ) );
         else
            hb_itemCopy( pItem, pGetItem );
      }
      else /* variable is not initialized */
      {
         /* Generate an error with retry possibility
          * (user created error handler can create this variable)
          */
         WORD wAction = E_RETRY;
         HB_ITEM_PTR pError;

         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                                 NULL, pMemvarSymb->szName, 0, EF_CANRETRY );

         while( wAction == E_RETRY )
         {
            wAction = hb_errLaunch( pError );
            if( wAction == E_RETRY )
            {
               if( pDyn->hMemvar )
               {
                  /* value is already created
                  */
                  HB_ITEM_PTR pGetItem = &s_globalTable[ pDyn->hMemvar ].item;
                  if( IS_BYREF( pGetItem ) )
                     hb_itemCopy( pItem, hb_itemUnRef( pGetItem ) );
                  else
                     hb_itemCopy( pItem, pGetItem );
                  wAction = E_DEFAULT;
               }
            }
         }
         hb_errRelease( pError );
      }
   }
   else
      hb_errRT_BASE( EG_NOVAR, 1003, NULL, pMemvarSymb->szName );
}

void hb_memvarGetRefer( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb )
{
   PHB_DYNS pDyn = ( PHB_DYNS ) pMemvarSymb->pDynSym;

   if( pDyn )
   {
      #ifdef MEMVARDEBUG
         printf( "\n@@@@Memvar item (%i)(%s) referenced", pDyn->hMemvar, pMemvarSymb->szName );
      #endif
      if( pDyn->hMemvar )
      {
         /* value is already created */
         pItem->type = IT_BYREF | IT_MEMVAR;
         pItem->item.asMemvar.offset = 0;
         pItem->item.asMemvar.value = pDyn->hMemvar;
         pItem->item.asMemvar.itemsbase = &s_globalTable;
         ++s_globalTable[ pDyn->hMemvar ].counter;
      }
      else
      {
         /* Generate an error with retry possibility
          * (user created error handler can make this variable accessible)
          */
         WORD wAction = E_RETRY;
         HB_ITEM_PTR pError;

         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                                 NULL, pMemvarSymb->szName, 0, EF_CANRETRY );

         while( wAction == E_RETRY )
         {
            wAction = hb_errLaunch( pError );
            if( wAction == E_RETRY )
            {
               if( pDyn->hMemvar )
               {
                  /* value is already created */
                  pItem->type = IT_BYREF | IT_MEMVAR;
                  pItem->item.asMemvar.offset = 0;
                  pItem->item.asMemvar.value = pDyn->hMemvar;
                  pItem->item.asMemvar.itemsbase = &s_globalTable;
                  ++s_globalTable[ pDyn->hMemvar ].counter;
                  wAction = E_DEFAULT;
               }
            }
         }
         hb_errRelease( pError );
      }
   }
   else
      hb_errRT_BASE( EG_NOVAR, 1003, NULL, pMemvarSymb->szName );
}

/*
 * This function creates a value for memvar variable
 *
 * pMemvar - an item that stores the name of variable - it can be either
 *          the IT_SYMBOL (if created by PUBLIC statement) or IT_STRING
 *          (if created by direct call to __PUBLIC function)
 * bScope - the scope of created variable - if a variable with the same name
 *          exists already then it's value is hidden by new variable with
 *          passed scope
 * pValue - optional item used to initialize the value of created variable
 *          or NULL
 *
 */
static void hb_memvarCreateFromItem( PHB_ITEM pMemvar, BYTE bScope, PHB_ITEM pValue )
{
   PHB_DYNS pDynVar;

   /* find dynamic symbol or creeate one */
   if( IS_SYMBOL( pMemvar ) )
      pDynVar = hb_dynsymGet( pMemvar->item.asSymbol.value->szName );
   else if( IS_STRING( pMemvar ) )
      pDynVar = hb_dynsymGet( pMemvar->item.asString.value );
   else
      hb_errRT_BASE( EG_ARG, 3008, NULL, "&" );

   if( pDynVar )
      hb_memvarCreateFromDynSymbol( pDynVar, bScope, pValue );
}

static void hb_memvarCreateFromDynSymbol( PHB_DYNS pDynVar, BYTE bScope, PHB_ITEM pValue )
{
   if( bScope & VS_PUBLIC )
   {
      /* If the variable with the same name exists already
       * then the current value have to be unchanged
       */
      if( ! pDynVar->hMemvar )
      {
         pDynVar->hMemvar = hb_memvarValueNew( pValue, TRUE );
         if( !pValue )
         {
            /* new PUBLIC variable - initialize it to .F.
             */

            s_globalTable[ pDynVar->hMemvar ].item.type = IT_LOGICAL;

            /* NOTE: PUBLIC variables named CLIPPER and HARBOUR are initialized */
            /*       to .T., this is normal Clipper behaviour */

            if( strcmp( pDynVar->pSymbol->szName, "HARBOUR" ) == 0 ||
                 strcmp( pDynVar->pSymbol->szName, "CLIPPER" ) == 0 )
               s_globalTable[ pDynVar->hMemvar ].item.item.asLogical.value = TRUE;
            else
               s_globalTable[ pDynVar->hMemvar ].item.item.asLogical.value = FALSE;
         }
      }
   }
   else
   {
      /* We need to store the handle to the value of variable that is
       * visible at this moment so later we can restore this value when
       * the new variable will be released
       */
      HB_HANDLE hCurrentValue = pDynVar->hMemvar;

      pDynVar->hMemvar = hb_memvarValueNew( pValue, TRUE );
      s_globalTable[ pDynVar->hMemvar ].hPrevMemvar = hCurrentValue;

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
   ULONG ulBase = s_privateStackCnt;
   PHB_DYNS pDynVar;

   if( IS_STRING( pMemvar ) )
   {
      /* Find the variable with a requested name that is currently visible
       * Start from the top of the stack.
       */
      while( ulBase > 0 )
      {
         --ulBase;
         pDynVar = s_privateStack[ ulBase ];
         /* reset current value to NIL - the overriden variables will be
         * visible after exit from current procedure
         */
         if( pDynVar->hMemvar )
         {
            if( hb_stricmp( pDynVar->pSymbol->szName, pMemvar->item.asString.value ) == 0 )
            {
               hb_itemClear( &s_globalTable[ pDynVar->hMemvar ].item );
               ulBase = 0;
            }
         }
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 3008, NULL, "RELEASE" );
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

   while( ulBase > s_privateStackBase )
   {
      --ulBase;
      pDynVar = s_privateStack[ ulBase ];
      /* reset current value to NIL - the overriden variables will be
       * visible after exit from current procedure
       */
      if( pDynVar->hMemvar )
      {
         if( bInclude )
         {
            if( ( szMask[ 0 ] == '*') || hb_strMatchRegExp( pDynVar->pSymbol->szName, szMask ) )
               hb_itemClear( &s_globalTable[ pDynVar->hMemvar ].item );
         }
         else if( ! hb_strMatchRegExp( pDynVar->pSymbol->szName, szMask ) )
            hb_itemClear( &s_globalTable[ pDynVar->hMemvar ].item );
      }
   }
}

/* Checks if passed dynamic symbol is a variable and returns its scope
 */
static int hb_memvarScopeGet( PHB_DYNS pDynVar )
{
   if( pDynVar->hMemvar == 0 )
      return MV_UNKNOWN;
   else
   {
      ULONG ulBase = s_privateStackCnt;    /* start from the top of the stack */
      int iMemvar = MV_PUBLIC;

      while( ulBase )
      {
         --ulBase;
         if( pDynVar == s_privateStack[ ulBase ] )
         {
            if( ulBase >= s_privateStackBase )
               iMemvar = MV_PRIVATE_LOCAL;
            else
               iMemvar = MV_PRIVATE_GLOBAL;
            ulBase = 0;
         }
      }
      return iMemvar;
   }
}

/* This function checks the scope of passed variable name
 */
static int hb_memvarScope( char * szVarName, ULONG ulLength )
{
   int iMemvar = MV_ERROR;
   char * szName;

   szName = ( char * ) hb_xalloc( ulLength );
   if( szName )
   {
      PHB_DYNS pDynVar;

      memcpy( szName, szVarName, ulLength );
      pDynVar = hb_dynsymFind( hb_strUpper( szName, ulLength - 1 ) );
      if( pDynVar )
         iMemvar = hb_memvarScopeGet( pDynVar );
      else
         iMemvar = MV_NOT_FOUND;
      hb_xfree( szName );
   }

   return iMemvar;
}

/* Releases memory occupied by a variable
 */
static HB_DYNS_FUNC( hb_memvarClear )
{
   HB_SYMBOL_UNUSED( Cargo );

   if( pDynSymbol->hMemvar )
   {
      s_globalTable[ pDynSymbol->hMemvar ].counter = 1;
      hb_memvarValueDecRef( pDynSymbol->hMemvar );
      pDynSymbol->hMemvar = 0;
   }

   return TRUE;
}

/* Checks passed dynamic symbol if it is a PUBLIC variable and
 * increments the counter eventually
 */
static HB_DYNS_FUNC( hb_memvarCountPublics )
{
   if( hb_memvarScopeGet( pDynSymbol ) == MV_PUBLIC )
      ( * ( ( int * )Cargo ) )++;

   return TRUE;
}

/* Count the number of variables with given scope
 */
static int hb_memvarCount( int iScope )
{
   if( iScope == MV_PUBLIC )
   {
      int iPublicCnt = 0;

      hb_dynsymEval( hb_memvarCountPublics, (void *) &iPublicCnt );
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

   if( hb_memvarScopeGet( pDynSymbol ) == MV_PUBLIC )
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
static HB_ITEM_PTR hb_memvarDebugVariable( int iScope, int iPos, char * *pszName )
{
   HB_ITEM_PTR pValue = NULL;
   *pszName = NULL;

   if( iPos > 0 )
   {
      --iPos;
      if( iScope == MV_PUBLIC )
      {
         struct mv_PUBLIC_var_info struPub;

         struPub.iPos   = iPos;
         struPub.bFound = FALSE;
         /* enumerate existing dynamic symbols and fill this structure
          * with info for requested PUBLIC variable
          */
         hb_dynsymEval( hb_memvarFindPublicByPos, (void *) &struPub );
         if( struPub.bFound )
         {
            pValue =&s_globalTable[ struPub.pDynSym->hMemvar ].item;
            *pszName =struPub.pDynSym->pSymbol->szName;
         }
      }
      else
      {
         if( iPos < s_privateStackCnt )
         {
            HB_DYNS_PTR pDynSym = s_privateStack[ iPos ];

            pValue =&s_globalTable[ pDynSym->hMemvar ].item;
            *pszName = pDynSym->pSymbol->szName;
         }
      }
   }

   return pValue;
}

static HB_DYNS_PTR hb_memvarFindSymbol( HB_ITEM_PTR pName )
{
   HB_DYNS_PTR pDynSym = NULL;

   if( pName )
   {
      ULONG ulLen = pName->item.asString.length;

      if( ulLen )
      {
         char * szName = ( char * ) hb_xgrab( ulLen + 1 );
         char * szArg  = pName->item.asString.value;

         szName[ ulLen ] = '\0';
         do
         {
            --ulLen;
            szName[ ulLen ] = toupper( szArg[ ulLen ] );
         } while( ulLen );

         pDynSym = hb_dynsymFind( szName );
         hb_xfree( szName );
      }
   }
   return pDynSym;
}


/* ************************************************************************** */

/* -------------------------------------------------------------------------- */
/*  $DOC$
 *  $FUNCNAME$
 *    __MVPUBLIC()
 *  $CATEGORY$
 *    Variable management
 *  $ONELINER$
 *    This function creates a PUBLIC variable
 *  $SYNTAX$
 *    __MVPUBLIC( <variable_name> )
 *  $ARGUMENTS$
 *    <variable_name> = either a string that contains the variable's name or
 *       an one-dimensional array of strings with variable names
 *       No skeleton are allowed here.
 *  $RETURNS$
 *    Nothing
 *  $DESCRIPTION$
 *    This function can be called either by the harbour compiler or by user.
 *    The compiler always passes the item of IT_SYMBOL type that stores the
 *    name of variable.
 *      If a variable with the same name exists already then the new
 *    variable is not created - the previous value remains unchanged.
 *      If it is first variable with this name then the  variable is
 *       initialized with .T. value.
 *
 *  $EXAMPLES$
 *
 *  $TESTS$
 *
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *
 *  $END$
 */
HARBOUR HB___MVPUBLIC( void )
{
   int i, iCount = hb_pcount();
   PHB_ITEM pMemvar;

   if( iCount )
   {
      for( i = 1; i <= iCount; i++ )
      {
         pMemvar = hb_param( i, IT_ANY );
         if( pMemvar )
         {
            if( IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ULONG j, ulLen = hb_arrayLen( pMemvar );
               HB_ITEM VarItem;

               for( j = 1; j <= ulLen; j++ )
               {
                  hb_arrayGet( pMemvar, j, &VarItem );
                  hb_memvarCreateFromItem( &VarItem, VS_PUBLIC, NULL );
                  hb_itemClear( &VarItem );
               }
            }
            else
               hb_memvarCreateFromItem( pMemvar, VS_PUBLIC, NULL );
         }
      }
   }
}

/* -------------------------------------------------------------------------- */
/*  $DOC$
 *  $FUNCNAME$
 *    __MVPRIVATE()
 *  $CATEGORY$
 *    Variable management
 *  $ONELINER$
 *    This function creates a PRIVATE variable
 *  $SYNTAX$
 *    __MVPRIVATE( <variable_name> )
 *  $ARGUMENTS$
 *    <variable_name> = either a string that contains the variable's name or
 *       an one-dimensional array of strings with variable names
 *       No skeleton are allowed here.
 *  $RETURNS$
 *    Nothing
 *  $DESCRIPTION$
 *    This function can be called either by the harbour compiler or by user.
 *    The compiler always passes the item of IT_SYMBOL type that stores the
 *    name of variable.
 *    If a variable with the same name exists already then the value of old
 *    variable is hidden until the new variable is  released. The new variable
 *    is always initialized to NIL value.
 *
 *  $EXAMPLES$
 *
 *  $TESTS$
 *
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *
 *  $END$
 */
HARBOUR HB___MVPRIVATE( void )
{
   int i, iCount = hb_pcount();
   PHB_ITEM pMemvar;

   if( iCount )
   {
      for( i = 1; i <= iCount; i++ )
      {
         pMemvar = hb_param( i, IT_ANY );
         if( pMemvar )
         {
            if( IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ULONG j, ulLen = hb_arrayLen( pMemvar );
               HB_ITEM VarItem;

               for( j = 1; j <= ulLen; j++ )
               {
                  hb_arrayGet( pMemvar, j, &VarItem );
                  hb_memvarCreateFromItem( &VarItem, VS_PRIVATE, NULL );
                  hb_itemClear( &VarItem );
               }
            }
            else
               hb_memvarCreateFromItem( pMemvar, VS_PRIVATE, NULL );
         }
      }
   }
}

/* -------------------------------------------------------------------------- */
/*  $DOC$
 *  $FUNCNAME$
 *    __MVXRELEASE()
 *  $CATEGORY$
 *    Variable management
 *  $ONELINER$
 *    This function releases value stored in PRIVATE or PUBLIC variable
 *  $SYNTAX$
 *    __MVXRELEASE( <variable_name> )
 *  $ARGUMENTS$
 *    <variable_name> = either a string that contains the variable's name or
 *       an one-dimensional array of strings with variable names
 *       No skeleton are allowed here.
 *  $RETURNS$
 *    Nothing
 *  $DESCRIPTION$
 *    This function releases values stored in memory variable. It shouldn't
 *    be called directly, rather it should be placed into RELEASE command.
 *      If the released variable is a PRIVATE variable then previously hidden
 *    variable with the same name becomes visible after exit from the
 *    procedure where released variable was created. If you access
 *    the released variable in the same function/procedure where it
 *    was created the the NIL value is returned. You can however assign
 *    a new value to released variable without any side effects.
 *
 *    It releases variable even if this variable was created in different
 *    procedure
 *  $EXAMPLES$
 *
 *    PROCEDURE MAIN()
 *    PRIVATE mPrivate
 *
 *       mPrivate :="PRIVATE from MAIN()"
 *       ? mPrivate     //PRIVATE from MAIN()
 *       Test()
 *       ? mPrivate     //PRIVATE from MAIN()
 *
 *    RETURN
 *
 *    PROCEDURE Test()
 *    PRIVATE mPrivate
 *
 *       mPrivate :="PRIVATE from Test()"
 *       ? mPrivate           //PRIVATE from TEST()
 *       RELEASE mPrivate
 *       ? mPrivate           //NIL
 *       mPrivate :="Again in Test()"
 *
 *    RETURN
 *
 *  $TESTS$
 *
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *
 *  $END$
 */
HARBOUR HB___MVXRELEASE( void )
{
   int i, iCount = hb_pcount();
   PHB_ITEM pMemvar;

   if( iCount )
   {
      for( i = 1; i <= iCount; i++ )
      {
         pMemvar = hb_param( i, IT_ANY );
         if( pMemvar )
         {
            if( IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ULONG j, ulLen = hb_arrayLen( pMemvar );
               HB_ITEM VarItem;

               for( j = 1; j <= ulLen; j++ )
               {
                  hb_arrayGet( pMemvar, j, &VarItem );
                  hb_memvarRelease( &VarItem );
                  hb_itemClear( &VarItem );
               }
            }
            else
               hb_memvarRelease( pMemvar );
         }
      }
   }
}

/* -------------------------------------------------------------------------- */
/*  $DOC$
 *  $FUNCNAME$
 *    __MVRELEASE()
 *  $CATEGORY$
 *    Variable management
 *  $ONELINER$
 * This function releases PRIVATE variables created in current procedure
 *  $SYNTAX$
 *    __MVRELEASE( <skeleton>, <include_exclude_flag> )
 *  $ARGUMENTS$
 *    <skeleton> = string that contains the wildcard mask for variables' names
 *       that will be released. Supported wildcards: '*' and '?'
 *    <include_exclude_flag> = logical value that specifies if variables
 *       that match passed skeleton should be either included in deletion
 *       (if .T.) or excluded from deletion (if .F.)
 *  $RETURNS$
 *    Nothing
 *  $DESCRIPTION$
 *    This function releases values stored in memory variables. It shouldn't
 *    be called directly, it should be placed into RELEASE ALL command.
 *      If the released variable is a PRIVATE variable then previously hidden
 *    variable with the same name becomes visible after exit from the
 *    procedure where released variable was created. If you access
 *    the released variable in the same function/procedure where it
 *    was created the the NIL value is returned. You can however assign
 *    a new value to released variable without any side effects.
 *      PUBLIC variables are not changed by this function.
 *  $EXAMPLES$
 *
 *  $TESTS$
 *
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *
 *  $END$
 */
HARBOUR HB___MVRELEASE( void )
{
   int iCount = hb_pcount();
   PHB_ITEM pMask;

   if( iCount )
   {
      pMask = hb_param( 1, IT_STRING );
      if( pMask )
      {
         BOOL bIncludeVar;

         if( iCount > 1 )
            bIncludeVar = hb_parl( 2 );
         else
            bIncludeVar = TRUE;

         if( pMask->item.asString.value[ 0 ] == '*' )
            bIncludeVar = TRUE;   /* delete all memvar variables */
         hb_memvarReleaseWithMask( pMask->item.asString.value, bIncludeVar );
      }
   }
}

/* -------------------------------------------------------------------------- */
/*  $DOC$
 *  $FUNCNAME$
 *    __MVSCOPE()
 *  $CATEGORY$
 *    Variable management
 *  $ONELINER$
 *    If variable exists then returns its scope.
 *  $SYNTAX$
 *    __MVSCOPE( <cVarName> )
 *  $ARGUMENTS$
 *    <cVarName> = a string with a variable name to check
 *  $RETURNS$
 *    The symbolic values are defined in include/memvars.ch
 *    MV_NOT_FOUND      =variable is not declared (not found in symbol table)
 *    MV_UNKNOWN        =if variable doesn't exist (but found in symbol table)
 *    MV_ERROR          =if information cannot be obtained (memory error or argument error)
 *    MV_PUBLIC         =for public variables
 *    MV_PRIVATE_GLOBAL =for private variables declared outside of current function/procedure
 *    MV_PRIVATE_LOCAL  =for private variables declared in current function/procedure
 *  $DESCRIPTION$
 *
 *  $EXAMPLES$
 *
 *    PROCEDURE MAIN()
 *    PUBLIC mPublic
 *    PRIVATE mPrivateGlobal
 *
 *      CallProc()
 *       ? __mvScope( "mPrivateLocal" )      //MV_UNKNOWN
 *
 *    RETURN
 *
 *    PROCEDURE CallProc()
 *    PRIVATE mPrivateLocal
 *
 *       ? __mvScope( "mPublic" )            //MV_PUBLIC
 *       ? __mvScope( "mPrivateGlobal" )     //MV_PRIVATE_GLOBAL
 *       ? __mvScope( "mPrivateLocal" )      //MV_PRIVATE_LOCAL
 *       ? __mvScope( "mFindMe" )            //MV_NOT_FOUND
 *
 *       IF( __mvScope( "mPublic" ) > MV_ERROR )
 *          ? "Variable exists"
 *       ELSE
 *          ? "Variable not created yet"
 *       ENDIF
 *
 *    RETURN
 *
 *  $TESTS$
 *
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *    include/memvars.ch
 *  $END$
 */
HARBOUR HB___MVSCOPE( void )
{
   int iMemvar = MV_ERROR;

   if( hb_pcount() )
   {
      PHB_ITEM pVarName;

      pVarName = hb_param( 1, IT_STRING );
      if( pVarName )
      {
         iMemvar = hb_memvarScope( pVarName->item.asString.value, pVarName->item.asString.length + 1 );
      }
   }
   hb_retni( iMemvar );
}

/* -------------------------------------------------------------------------- */
/*  $DOC$
 *  $FUNCNAME$
 *    __MVCLEAR()
 *  $CATEGORY$
 *    Variable management
 *  $ONELINER$
 *    This function releases all PRIVATE and PUBLIC variables
 *  $SYNTAX$
 *    __MVCLEAR()
 *  $ARGUMENTS$
 *
 *  $RETURNS$
 *    Nothing
 *  $DESCRIPTION$
 *      This function releases all PRIVATE and PUBLIC variables.
 *    It is used to implement CLEAR MEMORY statement.
 *    The memory occupied by all visible variables are released - any
 *    attempt to access the variable will result in a runtime error.
 *    You have to reuse PRIVATE or PUBLIC statement to create again
 *    the variable that was cleared by this function.
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *
 *  $END$
 */
HARBOUR HB___MVCLEAR( void )
{
   hb_dynsymEval( hb_memvarClear, NULL );
}

/* -------------------------------------------------------------------------- */
/*  $DOC$
 *  $FUNCNAME$
 *    __MVDBGINFO()
 *  $CATEGORY$
 *    Variable management
 *  $ONELINER$
 *    This function returns the information about the variables for debugger
 *  $SYNTAX$
 *    __MVDBGINFO( <nScope> [, <nPosition> [, @<cVarName>] ] )
 *  $ARGUMENTS$
 *     <nScope> = the scope of variables for which an information is asked
 *           Supported values (defined in memvars.ch)
 *           MV_PUBLIC
 *           MV_PRIVATE (or any other value)
 *     <nPosition> = the position of asked variable on the list of variables
 *        with specified scope - it should start from position 1
 *     <cVarName> = the value is filled with a variable name if passed by
 *        reference and <nPosition> is specified
 *  $RETURNS$
 *     The return value depends on the number of arguments passed
 *  $DESCRIPTION$
 *      This function retrieves the information about memvar variables.
 *      It returns either the number of variables with given scope (when the
 *    first argument is passed only) or a value of variable identified by its
 *    position in the variables' list (when second argument is passed).
 *      It also returns the name of a variable if optional third argument
 *    is passed by reference.
 *
 *      If requested variable doesn't exist (requested position is greater
 *    then the number of defined variables) then NIL value is returned
 *    and variable name is set to "?"
 *
 *      The dynamic symbols table is used to find a PUBLIC variable then
 *    the PUBLIC variables are always sorted alphabetically. The PRIVATE
 *    variables are sorted in the creation order.
 *
 *    Note:
 *    Due to dynamic nature of memvar variables there is no guarantee that
 *    successive calls to retrieve the value of <Nth> PUBLIC variable will
 *    return the value of the same variable.
 *  $EXAMPLES$
 *
 *  #include <memvars.ch>
 *
 *  LOCAL nCount, i, xValue, cName
 *
 *  nCount =_mvDBGINFO( MV_PUBLIC )
 *  FOR i:=1 TO nCount
 *     xValue =__mvDBGINFO( MV_PUBLIC, i, @cName )
 *     ? i, cName, xValue
 *  NEXT
 *
 *  $TESTS$
 *
 *  #include <memvars.ch>
 *  PROCEDURE MAIN()
 *
 *    ? 'PUBLIC=', __mvDBGINFO( MV_PUBLIC )
 *    ? 'PRIVATE=', __mvDBGINFO( MV_PRIVATE )
 *
 *    PUBLIC cPublic:='cPublic in MAIN'
 *
 *    ? 'PUBLIC=', __mvDBGINFO( MV_PUBLIC )
 *    ? 'PRIVATE=', __mvDBGINFO( MV_PRIVATE )
 *
 *    PRIVATE cPrivate:='cPrivate in MAIN'
 *
 *    ? 'PUBLIC=', __mvDBGINFO( MV_PUBLIC )
 *    ? 'PRIVATE=', __mvDBGINFO( MV_PRIVATE )
 *
 *    CountMemvars()
 *
 *    ? 'Back in Main'
 *    ? 'PUBLIC=', __mvDBGINFO( MV_PUBLIC )
 *    ? 'PRIVATE=', __mvDBGINFO( MV_PRIVATE )
 *
 *
 *  RETURN
 *
 *  PROCEDURE CountMemvars()
 *  LOCAL i, nCnt, xVal, cName
 *  PUBLIC ccPublic:='ccPublic'
 *  PRIVATE ccPrivate:='ccPrivate'
 *
 *    ? 'In CountMemvars'
 *    ? 'PUBLIC=', __mvDBGINFO( MV_PUBLIC )
 *    ? 'PRIVATE=', __mvDBGINFO( MV_PRIVATE )
 *
 *    PRIVATE cPublic:='cPublic'
 *
 *    ? 'PUBLIC=', __mvDBGINFO( MV_PUBLIC )
 *    ? 'PRIVATE=', __mvDBGINFO( MV_PRIVATE )
 *
 *    nCnt =__mvDBGINFO( MV_PRIVATE ) +1
 *    FOR i:=1 TO nCnt
 *        xVal =__mvDBGINFO( MV_PRIVATE, i, @cName )
 *        ? i, '=', cName, xVal
 *    NEXT
 *
 *    nCnt =__mvDBGINFO( MV_PUBLIC ) +1
 *    FOR i:=1 TO nCnt
 *        xVal =__mvDBGINFO( MV_PUBLIC, i, @cName )
 *        ? i, '=', cName, xVal
 *    NEXT
 *
 *  RETURN
 *
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *     This function should be called from the debugger only.
 *  $SEEALSO$
 *
 *  $END$
 */
HARBOUR HB___MVDBGINFO( void )
{
   int iCount = hb_pcount();

   if( iCount == 1 )          /* request for a number of variables */
      hb_retni( hb_memvarCount( hb_parni( 1 ) ) );

   else if( iCount >= 2 )     /* request for a value of variable */
   {
      HB_ITEM_PTR pValue;
      char * szName;

      pValue = hb_memvarDebugVariable( hb_parni( 1 ), hb_parni( 2 ), &szName );

      if( pValue )
      {
         /*the requested variable was found
          */
         if( iCount >= 3 && ISBYREF( 3 ) )
         {
            /* we have to use this variable regardless of its current value
             */
            HB_ITEM_PTR pName = hb_param( 3, IT_ANY );

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
         pValue = hb_itemNew( NULL );
         hb_itemReturn( pValue );      /* return NIL value */
         hb_itemRelease( pValue );

         if( iCount >= 3 && ISBYREF( 3 ) )
         {
            /* we have to use this variable regardless of its current value
             */
            HB_ITEM_PTR pName = hb_param( 3, IT_ANY );

            hb_itemPutC( pName, "?" ); /* clear an old value and copy a new one */
         }
      }
   }
}

/* -------------------------------------------------------------------------- */
/*  $DOC$
 *  $FUNCNAME$
 *    __MVGET()
 *  $CATEGORY$
 *    Variable management
 *  $ONELINER$
 *    This function returns value of memory variable
 *  $SYNTAX$
 *    __MVGET( <cVarName> )
 *  $ARGUMENTS$
 *    <cVarName> - string that specifies the name of variable
 *  $RETURNS$
 *    The value of variable
 *  $DESCRIPTION$
 *      This function returns the value of PRIVATE or PUBLIC variable if
 *    this variable exists otherwise it generates a runtime error.
 *    The variable is specified by its name passed as the function parameter.
 *  $EXAMPLES$
 *    FUNCTION MEMVARBLOCK( cMemvar )
 *    RETURN {|x| IIF( PCOUNT()==0, __MVGET( cMemvar ), __MVPUT( cMemvar, x ) ) }
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *    MVPUT()
 *  $END$
 */
HARBOUR HB___MVGET( void )
{
   HB_ITEM_PTR pName = hb_param( 1, IT_STRING );

   if( pName )
   {
      HB_DYNS_PTR pDynVar = hb_memvarFindSymbol( pName );

      if( pDynVar )
      {
         HB_ITEM retValue;
         hb_memvarGetValue( &retValue, pDynVar->pSymbol );
         hb_itemReturn( &retValue );
         hb_itemClear( &retValue );
      }
      else
      {
         /* Generate an error with retry possibility
          * (user created error handler can create this variable)
          */
         WORD wAction = E_RETRY;
         HB_ITEM_PTR pError;

         pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                                 NULL, pName->item.asString.value, 0, EF_CANRETRY );

         while( wAction == E_RETRY )
         {
            wAction = hb_errLaunch( pError );
            if( wAction == E_RETRY )
            {
               pDynVar = hb_memvarFindSymbol( pName );
               if( pDynVar )
               {
                  HB_ITEM retValue;
                  hb_memvarGetValue( &retValue, pDynVar->pSymbol );
                  hb_itemReturn( &retValue );
                  hb_itemClear( &retValue );
                  wAction =E_DEFAULT;
               }
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
      /* TODO: This should be expanded a little to report a passed incorrect
       * value to the error handler
       */
      HB_ITEM_PTR pRetValue = hb_errRT_BASE_Subst( EG_ARG, 3009, NULL, NULL );

      if( pRetValue )
      {
         hb_itemReturn( pRetValue );
         hb_itemRelease( pRetValue );
      }
   }
}

/* -------------------------------------------------------------------------- */
/*  $DOC$
 *  $FUNCNAME$
 *    __MVPUT()
 *  $CATEGORY$
 *    Variable management
 *  $ONELINER$
 *    This function set the value of memory variable
 *  $SYNTAX$
 *    __MVGET( <cVarName> [, <xValue>] )
 *  $ARGUMENTS$
 *    <cVarName> - string that specifies the name of variable
 *    <xValue>   - a value of any type that will be set - if it is not
 *      specified then NIL is assumed
 *  $RETURNS$
 *    A value assigned to the given variable.
 *  $DESCRIPTION$
 *      This function sets the value of PRIVATE or PUBLIC variable if
 *    this variable exists otherwise it generates a runtime error.
 *    The variable is specified by its name passed as the function
 *    parameter.
 *    If a value is not specified then the NIL is assumed
 *  $EXAMPLES$
 *    FUNCTION MEMVARBLOCK( cMemvar )
 *    RETURN {|x| IIF( PCOUNT()==0, __MVGET( cMemvar ), __MVPUT( cMemvar, x ) ) }
 *  $STATUS$
 *
 *  $COMPLIANCE$
 *
 *  $SEEALSO$
 *    MVPUT()
 *  $END$
 */
HARBOUR HB___MVPUT( void )
{
   HB_ITEM_PTR pName = hb_param( 1, IT_STRING );
   HB_ITEM nil;
   HB_ITEM_PTR pValue = &nil;

   nil.type = IT_NIL;
   if( hb_pcount() >= 2 )
      pValue = hb_param( 2, IT_ANY );

   if( pName )
   {
      /* the first parameter is a string with not empty variable name
       */
      HB_DYNS_PTR pDynVar = hb_memvarFindSymbol( pName );

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
      hb_itemReturn( pValue );
   }
   else
   {
      /* either the first parameter is not specified or it has a wrong type
       * (it must be a string)
       * This is not a critical error - we can continue normal processing
       */
      /* TODO: This should be expanded a little to report a passed incorrect
       * value to the error handler
       */
      HB_ITEM_PTR pRetValue = hb_errRT_BASE_Subst( EG_ARG, 3010, NULL, NULL );

      if( pRetValue )
         hb_itemRelease( pRetValue );
      hb_itemReturn( pValue );
   }
}

#define HB_MEM_REC_LEN          32
#define HB_MEM_NUM_LEN          8

HARBOUR HB___MVSAVE( void )
{
   /* Clipper also checks for the number of arguments here */
   if( hb_pcount() == 3 && ISCHAR( 1 ) && ISCHAR( 2 ) && ISLOG( 3 ) )
   {
      PHB_FNAME pFileName;
      char szFileName[ _POSIX_PATH_MAX + 1 ];
      FHANDLE fhnd;

      /* Generate filename */

      pFileName = hb_fsFNameSplit( hb_parc( 1 ) );

      if( pFileName->szExtension == NULL )
         pFileName->szExtension = ".mem";

      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );

      /* Create .MEM file */

      while( ( fhnd = hb_fsCreate( ( BYTE * ) szFileName, FC_NORMAL ) ) == FS_ERROR )
      {
         WORD wResult = hb_errRT_BASE_Ext1( EG_CREATE, 2006, NULL, szFileName, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY );

         if( wResult == E_DEFAULT || wResult == E_BREAK )
            break;
      }

      if( fhnd != FS_ERROR )
      {
         char * pszMask = hb_parc( 2 );
         BOOL bIncludeMask = hb_parl( 3 );
         BYTE buffer[ HB_MEM_REC_LEN ];

         /* Walk through all visible memory variables */

         ULONG ulBase = s_privateStackCnt;
         PHB_DYNS pDynVar;

         while( ulBase > s_privateStackBase )
         {
            --ulBase;
            pDynVar = s_privateStack[ ulBase ];

            /* Save memvars with a maximum name length of 10 characters */
            /* NOTE: Harbour name lengths are not limited, but the .MEM file
                     structure is not flexible enough to allow for it.
                     So we are ignoring any variables with more then 10 chars
                     long name. */
            if( pDynVar->hMemvar && strlen( pDynVar->pSymbol->szName ) <= 10 )
            {
               BOOL bMatch = ( pszMask[ 0 ] == '*' || hb_strMatchRegExp( pDynVar->pSymbol->szName, pszMask ) );

               /* Process it if it matches the passed mask */
               if( bIncludeMask ? bMatch : ! bMatch )
               {
                  PHB_ITEM pItem = &s_globalTable[ pDynVar->hMemvar ].item;

                  memset( buffer, 0, HB_MEM_REC_LEN );
                  strcpy( ( char * ) buffer, pDynVar->pSymbol->szName );

                  if( IS_STRING( pItem ) && ( hb_itemGetCLen( pItem ) + 1 ) <= SHRT_MAX )
                  {
                     /* Store the closing zero byte, too */
                     USHORT uiLength = hb_itemGetCLen( pItem ) + 1;

                     buffer[ 11 ] = 'C' + 128;
                     buffer[ 16 ] = LOBYTE( uiLength );
                     buffer[ 17 ] = HIBYTE( uiLength );

                     hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN );
                     hb_fsWrite( fhnd, ( BYTE * ) hb_itemGetCPtr( pItem ), uiLength );
                  }
                  else if( IS_NUMERIC( pItem ) )
                  {
                     double dNumber = hb_itemGetND( pItem );
                     int iWidth;
                     int iDec;

                     hb_itemGetNLen( pItem, &iWidth, &iDec );

                     buffer[ 11 ] = 'N' + 128;
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
/* NOTE: This is the buggy, but fully CA-Cl*pper compatible method. */
                     buffer[ 16 ] = ( BYTE ) iWidth + ( IS_DOUBLE( pItem ) ? iDec + 1 : 0 );
#else
/* NOTE: This would be the correct method, but Clipper is buggy here. */
                     buffer[ 16 ] = ( BYTE ) iWidth + ( iDec == 0 ? 0 : iDec + 1 );
#endif
                     buffer[ 17 ] = ( BYTE ) iDec;

                     hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN );
                     hb_fsWrite( fhnd, ( BYTE * ) &dNumber, sizeof( dNumber ) );
                  }
                  else if( IS_DATE( pItem ) )
                  {
                     double dNumber = ( double ) hb_itemGetNL( pItem );

                     buffer[ 11 ] = 'D' + 128;
                     buffer[ 16 ] = 1;
                     buffer[ 17 ] = 0;

                     hb_fsWrite( fhnd, buffer, HB_MEM_REC_LEN );
                     hb_fsWrite( fhnd, ( BYTE * ) &dNumber, sizeof( dNumber ) );
                  }
                  else if( IS_LOGICAL( pItem ) )
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
         }

         buffer[ 0 ] = '\x1A';
         hb_fsWrite( fhnd, buffer, 1 );

         hb_fsClose( fhnd );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 2008, NULL, "__MSAVE" );
}

/* NOTE: There's an extension in Harbour, which makes it possible to only
         load (or not load) variable names with a specific name mask. */

HARBOUR HB___MVRESTORE( void )
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
         hb_dynsymEval( hb_memvarClear, NULL );

      /* Generate filename */

      pFileName = hb_fsFNameSplit( hb_parc( 1 ) );

      if( pFileName->szExtension == NULL )
         pFileName->szExtension = ".mem";

      hb_fsFNameMerge( szFileName, pFileName );
      hb_xfree( pFileName );

      /* Open .MEM file */

      while( ( fhnd = hb_fsOpen( ( BYTE * ) szFileName, FO_READ | FO_SHARED ) ) == FS_ERROR )
      {
         WORD wResult = hb_errRT_BASE_Ext1( EG_OPEN, 2005, NULL, szFileName, hb_fsError(), EF_CANDEFAULT | EF_CANRETRY );

         if( wResult == E_DEFAULT || wResult == E_BREAK )
            break;
      }

      if( fhnd != FS_ERROR )
      {
         char * pszMask = ISCHAR( 3 ) ? hb_parc( 3 ) : "*";
         BOOL bIncludeMask = ISCHAR( 4 ) ? hb_parl( 4 ) : TRUE;
         BYTE buffer[ HB_MEM_REC_LEN ];

         while( hb_fsRead( fhnd, buffer, HB_MEM_REC_LEN ) == HB_MEM_REC_LEN )
         {
            PHB_ITEM pName = hb_itemPutC( NULL, ( char * ) buffer );
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
                     pItem = hb_itemPutNLen( NULL, * ( double * ) &pbyNumber, uiWidth - ( uiDec ? ( uiDec + 1 ) : 0 ), uiDec );

                  break;
               }

               case 'D':
               {
                  BYTE pbyNumber[ HB_MEM_NUM_LEN ];

                  if( hb_fsRead( fhnd, pbyNumber, HB_MEM_NUM_LEN ) == HB_MEM_NUM_LEN )
                     pItem = hb_itemPutDL( NULL, ( long ) ( * ( double * ) &pbyNumber ) );

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
               BOOL bMatch = ( pszMask[ 0 ] == '*' || hb_strMatchRegExp( hb_itemGetCPtr( pName ), pszMask ) );

               /* Process it if it matches the passed mask */
               if( bIncludeMask ? bMatch : ! bMatch )
               {
                  /* the first parameter is a string with not empty variable name */
                  HB_DYNS_PTR pDynVar = hb_memvarFindSymbol( pName );

                  if( pDynVar )
                     /* variable was declared somwhere - assign a new value */
                     hb_memvarSetValue( pDynVar->pSymbol, pItem );
                  else
                     /* attempt to assign a value to undeclared variable create the PRIVATE one */
                     hb_memvarCreateFromDynSymbol( hb_dynsymGet( pName->item.asString.value ), VS_PRIVATE, pItem );

                  hb_itemReturn( pItem );
               }

               hb_itemRelease( pItem );
            }

            hb_itemRelease( pName );
         }

         hb_fsClose( fhnd );
      }
      else
         hb_retl( FALSE );
   }
   else
      hb_errRT_BASE( EG_ARG, 2007, NULL, "__MRESTORE" );
}

/* CA-Clipper 5.2e compatibility functions. */

HARBOUR HB___MCLEAR( void )
{
   HB___MVCLEAR();
}

HARBOUR HB___MRELEASE( void )
{
   HB___MVRELEASE();
}

HARBOUR HB___MXRELEASE( void )
{
   HB___MVXRELEASE();
}

HARBOUR HB___MSAVE( void )
{
   HB___MVSAVE();
}

HARBOUR HB___MRESTORE( void )
{
   HB___MVRESTORE();
}
