/*
 * $Id$
 *
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

#include <string.h>
#include "extend.h"
#include "itemapi.h"
#include "errorapi.h"
#include "error.ch"

#define VS_PRIVATE     64
#define VS_PUBLIC     128

static PDYNSYM *_privateStack  = NULL;
static ULONG _privateStackSize = 0;
static ULONG _privateStackCnt  = 0;
static ULONG _privateStackBase = 0;

static ULONG _globalTableSize = 0;
static ULONG _globalFirstFree = 0;
static ULONG _globalLastFree  = 0;
static ULONG _globalFreeCnt   = 0;
static HB_VALUE_PTR _globalTable = NULL;

#define TABLE_INITHB_VALUE   100
#define TABLE_EXPANDHB_VALUE  50

/* Uncomment this to trace memvars activity
#define MEMVARDEBUG
*/

static void hb_MemvarCreateFromItem( PHB_ITEM, BYTE, PHB_ITEM );
static void hb_MemvarCreateFromDynSymbol( PDYNSYM, BYTE, PHB_ITEM );
static void hb_MemvarAddPrivate( PDYNSYM );

void hb_MemvarsInit( void )
{
   _globalTable = (HB_VALUE_PTR) hb_xgrab( sizeof(HB_VALUE) * TABLE_INITHB_VALUE );
   _globalTableSize = TABLE_INITHB_VALUE;
   _globalFreeCnt   = 0;
   _globalFirstFree = _globalLastFree = 1;

   _privateStack = (PDYNSYM *) hb_xgrab( sizeof(PDYNSYM) * TABLE_INITHB_VALUE );
   _privateStackSize = TABLE_INITHB_VALUE;
   _privateStackCnt  = _privateStackBase = 0;
}


void hb_MemvarsRelease( void )
{
   ULONG ulCnt = _globalLastFree;

   if( _globalTable )
   {
      while( ulCnt )
      {
         if( _globalTable[ ulCnt ].counter )
         {
            hb_itemClear( &_globalTable[ ulCnt ].item );
            _globalTable[ ulCnt ].counter =0;
         }
         --ulCnt;
      }

      hb_xfree( _globalTable );
   }

   if( _privateStack )
      hb_xfree( _privateStack );
}

/*
 * This function base address of values table
 */
HB_VALUE_PTR *hb_MemvarValueBaseAddress( void )
{
   return &_globalTable;
}


/*
 * This function creates new global value.
 *
 * pSource = item value that have to be stored or NULL
 * iTrueMemvar = TRUE | FALSE
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
HB_HANDLE hb_MemvarValueNew( HB_ITEM_PTR pSource, int iTrueMemvar )
{
   HB_VALUE_PTR pValue;
   HB_HANDLE hValue = 1;   /* handle 0 is reserved */

   if( _globalFreeCnt )
   {
      /* There are holes in the table
         * Get a first available hole
         */
      hValue =_globalFirstFree;
      --_globalFreeCnt;

      /* Now find the next hole
         */
      if( _globalFreeCnt )
      {
         ++_globalFirstFree;
         while( _globalTable[ _globalFirstFree ].counter )
            ++_globalFirstFree;
      }
      else
         /* No more holes
            */
         _globalFirstFree =_globalLastFree;
   }
   else
   {
      /* Allocate the value from the end of table
         */
      if( _globalFirstFree < _globalTableSize )
      {
         hValue =_globalFirstFree;
         _globalFirstFree = ++_globalLastFree;
      }
      else
      {
         /* No more free values in the table - expand the table
            */
         hValue = _globalTableSize;
         _globalFirstFree =_globalLastFree = _globalTableSize +1;
         _globalTableSize += TABLE_EXPANDHB_VALUE;
         _globalTable =(HB_VALUE_PTR) hb_xrealloc( _globalTable, sizeof(HB_VALUE) * _globalTableSize );
      }

   }

   pValue =_globalTable + hValue;
   pValue->counter =1;
   if( pSource )
   {
      if( iTrueMemvar )
         hb_itemCopy( &pValue->item, pSource );
      else
         memcpy( &pValue->item, pSource, sizeof(HB_ITEM) );
   }
   else
      pValue->item.type =IT_NIL;

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
static void hb_MemvarAddPrivate( PDYNSYM pDynSym )
{
   /* Allocate the value from the end of table
    */
   if( _privateStackCnt == _privateStackSize )
   {
      /* No more free values in the table - expand the table
       */
      _privateStackSize += TABLE_EXPANDHB_VALUE;
      _privateStack =(PDYNSYM *) hb_xrealloc( _privateStack, sizeof(PDYNSYM) * _privateStackSize );
   }

   _privateStack[ _privateStackCnt++ ] =pDynSym;
}

/*
 * This function returns current PRIVATE variables stack base
 */
ULONG hb_MemvarGetPrivatesBase( void )
{
   ULONG ulBase = _privateStackBase;
   _privateStackBase =_privateStackCnt;
   return ulBase;
}

/*
 * This function releases PRIVATE variables created after passed base
 *
 */
void hb_MemvarSetPrivatesBase( ULONG ulBase )
{
   HB_HANDLE hVar, hOldValue;

   while( _privateStackCnt > _privateStackBase )
   {
      --_privateStackCnt;
      hVar =_privateStack[ _privateStackCnt ]->hMemvar;
      hOldValue =_globalTable[ hVar ].hPrevMemvar;
      hb_MemvarValueDecRef( hVar );
      /*
      * Restore previous value for variables that were overridden
      */
      _privateStack[ _privateStackCnt ]->hMemvar =hOldValue;
   }
   _privateStackBase =ulBase;
}

/*
 * This function increases the number of references to passed global value
 *
 */
void hb_MemvarValueIncRef( HB_HANDLE hValue )
{
   #ifdef MEMVARDEBUG
   if( hValue < 1 || hValue > _globalTableSize )
   {
      printf( "\nInvalid MEMVAR handle %i (max %li)\n", hValue, _globalTableSize );
      exit( 1 );
   }
   #endif

   _globalTable[ hValue ].counter++;

   #ifdef MEMVARDEBUG
      printf( "\n+++Memvar item (%i) increment refCounter=%li", hValue, _globalTable[ hValue ].counter );
   #endif
}

/*
 * This function decreases the number of references to passed global value.
 * If it is the last reference then this value is deleted.
 *
 */
void hb_MemvarValueDecRef( HB_HANDLE hValue )
{
   HB_VALUE_PTR pValue;

   #ifdef MEMVARDEBUG
   if( hValue < 1 || hValue > _globalTableSize )
   {
      printf( "\nInvalid MEMVAR handle %i (max %li)\n", hValue, _globalTableSize );
      exit( 1 );
   }
   #endif

   pValue =_globalTable + hValue;
   #ifdef MEMVARDEBUG
      printf( "\n---Memvar item (%i) decrement refCounter=%li", hValue, pValue->counter-1 );
   #endif
   if( pValue->counter > 0 )
   {
      if( --pValue->counter == 0 )
      {
         hb_itemClear( &pValue->item );
         if( _globalFirstFree > hValue )
         {
            if( (_globalLastFree - hValue) == 1 )
               _globalFirstFree = _globalLastFree =hValue;     /* last item */
            else
            {
               _globalFirstFree =hValue;
               ++_globalFreeCnt;             /* middle item */
            }
         }
         else if( (_globalLastFree - hValue) == 1 )
         {
            _globalLastFree =hValue;         /* last item */
            if( _globalLastFree == _globalFirstFree )
               _globalFreeCnt =0;
         }
         else
            ++_globalFreeCnt;

      #ifdef MEMVARDEBUG
         printf( "\n<<<<<Memvar item (%i) deleted", hValue );
      #endif
      }
   }
/* This can happen if for example PUBLIC variable holds a codeblock with
 * detached variable. When hb_MemvarsRelease() is called then detached
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
void hb_MemvarSetValue( PHB_SYMB pMemvarSymb, HB_ITEM_PTR pItem )
{
   PDYNSYM pDyn;

   pDyn = (PDYNSYM) pMemvarSymb->pDynSym;

   if( pDyn )
   {
      #ifdef MEMVARDEBUG
         printf( "\n::::Memvar item (%i)(%s) assigned", pDyn->hMemvar, pMemvarSymb->szName );
      #endif
      if( pDyn->hMemvar )
      {
         /* value is already created */
         HB_ITEM_PTR pSetItem = &_globalTable[ pDyn->hMemvar ].item;
         if( IS_BYREF(pSetItem) )
            hb_itemCopy( hb_itemUnRef(pSetItem), pItem );
         else
            hb_itemCopy( pSetItem, pItem );
      }
      else
      {
         /* assignment to undeclared memvar - PRIVATE is assumed
          */
         hb_MemvarCreateFromDynSymbol( pDyn, VS_PRIVATE, pItem );
      }
   }
   else
      hb_errorRT_BASE( EG_NOVAR, 1003, NULL, pMemvarSymb->szName );
}

void hb_MemvarGetValue( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb )
{
   PDYNSYM pDyn;

   pDyn = (PDYNSYM) pMemvarSymb->pDynSym;

   if( pDyn )
   {
      #ifdef MEMVARDEBUG
         printf( "\n????Memvar item (%i)(%s) queried", pDyn->hMemvar, pMemvarSymb->szName );
      #endif
      if( pDyn->hMemvar )
      {
         /* value is already created
          */
         HB_ITEM_PTR pGetItem = &_globalTable[ pDyn->hMemvar ].item;
         if( IS_BYREF(pGetItem) )
            hb_itemCopy( pItem, hb_itemUnRef(pGetItem) );
         else
            hb_itemCopy( pItem, pGetItem );
      }
      else /* variable is not initialized */
         hb_errorRT_BASE( EG_NOVAR, 1003, NULL, pMemvarSymb->szName );
   }
   else
      hb_errorRT_BASE( EG_NOVAR, 1003, NULL, pMemvarSymb->szName );
}

void hb_MemvarGetRefer( HB_ITEM_PTR pItem, PHB_SYMB pMemvarSymb )
{
   PDYNSYM pDyn;

   pDyn = (PDYNSYM) pMemvarSymb->pDynSym;

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
         pItem->item.asMemvar.itemsbase = &_globalTable;
         ++_globalTable[ pDyn->hMemvar ].counter;
      }
      else
         hb_errorRT_BASE( EG_NOVAR, 1003, NULL, pMemvarSymb->szName );
   }
   else
      hb_errorRT_BASE( EG_NOVAR, 1003, NULL, pMemvarSymb->szName );
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
static void hb_MemvarCreateFromItem( PHB_ITEM pMemvar, BYTE bScope, PHB_ITEM pValue )
{
   PDYNSYM pDynVar;

   /* find dynamic symbol or creeate one */
   if( IS_SYMBOL( pMemvar ) )
      pDynVar =hb_GetDynSym( pMemvar->item.asSymbol.value->szName );
   else if( IS_STRING( pMemvar ) )
      pDynVar =hb_GetDynSym( pMemvar->item.asString.value );
   else
      hb_errorRT_BASE( EG_ARG, 3008, NULL, "&" );

   if( pDynVar )
      hb_MemvarCreateFromDynSymbol( pDynVar, bScope, pValue );
}

static void hb_MemvarCreateFromDynSymbol( PDYNSYM pDynVar, BYTE bScope, PHB_ITEM pValue )
{
   if( bScope & VS_PUBLIC )
   {
      /* If the variable with the same name exists already
       * then the current value have to be unchanged
       */
      if( ! pDynVar->hMemvar )
      {
         pDynVar->hMemvar = hb_MemvarValueNew( pValue, TRUE );
         if( !pValue )
         {
            /* new PUBLIC variable - initialize it to .F.
             */
            _globalTable[ pDynVar->hMemvar ].item.type =IT_LOGICAL;
            _globalTable[ pDynVar->hMemvar ].item.item.asLogical.value =0;
         }
      }
   }
   else
   {
      /* We need to store the handle to the value of variable that is
       * visible at this moment so later we can restore this value when
       * the new variable will be released
       */
      HB_HANDLE hCurrentValue =pDynVar->hMemvar;

      pDynVar->hMemvar = hb_MemvarValueNew( pValue, TRUE );
      _globalTable[ pDynVar->hMemvar ].hPrevMemvar =hCurrentValue;

      /* Add this variable to the PRIVATE variables stack
       */
      hb_MemvarAddPrivate( pDynVar );
   }
}

/* This function releases all memory occupied by a memvar variable
 * It also restores the value that was hidden if there is another
 * PRIVATE variable with the same name.
 */
void hb_MemvarRelease( HB_ITEM_PTR pMemvar )
{
   ULONG ulBase = _privateStackCnt;
   PDYNSYM pDynVar;

   if( IS_STRING(pMemvar) )
   {
      /* Find the variable with a requested name that is currently visible
       * Start from the top of the stack.
       */
      while( ulBase > 0 )
      {
         --ulBase;
         pDynVar =_privateStack[ ulBase ];
         /* reset current value to NIL - the overriden variables will be
         * visible after exit from current procedure
         */
         if( pDynVar->hMemvar )
         {
            if( hb_stricmp( pDynVar->pSymbol->szName, pMemvar->item.asString.value ) == 0 )
            {
               hb_itemClear( &_globalTable[ pDynVar->hMemvar ].item );
               ulBase =0;
            }
         }
      }
   }
   else
      hb_errorRT_BASE( EG_ARG, 3008, NULL, "RELEASE" );
}


/* This function releases all memory occupied by a memvar variable and
 * assigns NIL value - it releases variables created in current
 * procedure only.
 * The scope of released variables are specified using passed name's mask
 */
void hb_MemvarReleaseWithMask( char *szMask, BOOL bInclude )
{
   ULONG ulBase = _privateStackCnt;
   PDYNSYM pDynVar;

   while( ulBase > _privateStackBase )
   {
      --ulBase;
      pDynVar =_privateStack[ ulBase ];
      /* reset current value to NIL - the overriden variables will be
       * visible after exit from current procedure
       */
      if( pDynVar->hMemvar )
      {
         if( bInclude )
         {
            if( (szMask[ 0 ] == '*') || hb_strMatchRegExp( pDynVar->pSymbol->szName, szMask ) )
               hb_itemClear( &_globalTable[ pDynVar->hMemvar ].item );
         }
         else if( ! hb_strMatchRegExp( pDynVar->pSymbol->szName, szMask ) )
            hb_itemClear( &_globalTable[ pDynVar->hMemvar ].item );
      }
   }
}


/* ************************************************************************** */

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
      for( i=1; i<=iCount; i++ )
      {
         pMemvar =hb_param( i, IT_ANY );
         if( pMemvar )
         {
            if( IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ULONG j, ulLen = hb_arrayLen( pMemvar );
               HB_ITEM VarItem;

               for( j=1; j<=ulLen; j++ )
               {
                  hb_arrayGet( pMemvar, j, &VarItem );
                  hb_MemvarCreateFromItem( &VarItem, VS_PUBLIC, NULL );
                  hb_itemClear( &VarItem );
               }
            }
            else
               hb_MemvarCreateFromItem( pMemvar, VS_PUBLIC, NULL );
         }
      }
   }
}

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
      for( i=1; i<=iCount; i++ )
      {
         pMemvar =hb_param( i, IT_ANY );
         if( pMemvar )
         {
            if( IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ULONG j, ulLen = hb_arrayLen( pMemvar );
               HB_ITEM VarItem;

               for( j=1; j<=ulLen; j++ )
               {
                  hb_arrayGet( pMemvar, j, &VarItem );
                  hb_MemvarCreateFromItem( &VarItem, VS_PRIVATE, NULL );
                  hb_itemClear( &VarItem );
               }
            }
            else
               hb_MemvarCreateFromItem( pMemvar, VS_PRIVATE, NULL );
         }
      }
   }
}

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
 *    If the released variable is a PRIVATE variable then previously hidden
 *    variable with the same name becomes visible (after exit from the
 *    procedure where released variable was created).
 *
 *    It releases variable even if this variable was created in different
 *    procedure
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
HARBOUR HB___MVXRELEASE( void )
{
   int i, iCount = hb_pcount();
   PHB_ITEM pMemvar;

   if( iCount )
   {
      for( i=1; i<=iCount; i++ )
      {
         pMemvar =hb_param( i, IT_ANY );
         if( pMemvar )
         {
            if( IS_ARRAY( pMemvar ) )
            {
               /* we are accepting an one-dimensional array of strings only
                */
               ULONG j, ulLen = hb_arrayLen( pMemvar );
               HB_ITEM VarItem;

               for( j=1; j<=ulLen; j++ )
               {
                  hb_arrayGet( pMemvar, j, &VarItem );
                  hb_MemvarRelease( &VarItem );
                  hb_itemClear( &VarItem );
               }
            }
            else
               hb_MemvarRelease( pMemvar );
         }
      }
   }
}


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
 *    If released variable is a PRIVATE variable then the NIL is assigned.
 *    PUBLIC variables are not changed.
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
      pMask =hb_param( 1, IT_STRING );
      if( pMask )
      {
         BOOL bIncludeVar;

         if( iCount > 1 )
            bIncludeVar =hb_parl( 2 );
         else
            bIncludeVar =TRUE;

         if( pMask->item.asString.value[ 0 ] == '*' )
            bIncludeVar =TRUE;   /* delete all memvar variables */
         hb_MemvarReleaseWithMask( pMask->item.asString.value, bIncludeVar );
      }
   }
}
