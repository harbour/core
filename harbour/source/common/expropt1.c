/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Expression Optimizer - common expressions
 *
 * Copyright 1999 Ryszard Glab
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

/* TODO:
 *    - Correct post- and pre- operations to correctly handle the following code
 *    a[ i++ ]++
 *    Notice: in current implementation (an in Clipper too) 'i++' is evaluated
 *    two times! This causes that the new value (after incrementation) is
 *    stored in next element of the array.
 */

/* NOTE: This must be the first definition
 *    This is a common code shared by macro and standalone compiler
 */
#define  HB_MACRO_SUPPORT

#include <math.h>
#include "hbmacro.h"
#include "hbcomp.h"

/* memory allocation
 */
#define  HB_XGRAB( size )  hb_xgrab( (size) )
#define  HB_XFREE( pPtr )  hb_xfree( (void *)(pPtr) )

static char * s_OperTable[] = {
   "",
   "NIL",
   "Numeric",
   "String",
   "Codeblock",
   "Logical",
   "SELF",
   "Array",
   "@",
   "@",
   "IIF",
   ",",
   ",",
   "[",
   "&",
   "()",
   "->",
   "->",
   ":",
   "",      /* symbol */
   "",      /* alias */
   "",      /* RunTime variable */
   "",      /* variable */
   "++",      /* post-operators -> lowest precedence */
   "--",
   ":=",       /* assigments */
   "+=",
   "-=",
   "*=",
   "/=",
   "%=",
   "^=",
   ".OR.",           /* logical operators */
   ".AND.",
   ".NOT.",
   "=",        /* relational operators */
   "==",
   "<",
   ">",
   "<=",
   ">=",
   "!=",
   "$",
   "+",      /* addition */
   "-",
   "*",      /* multiple */
   "/",
   "%",
   "^",
   "-",    /* sign operator */
   "++",
   "--"
};

/* ************************************************************************* */

HB_EXPR_PTR hb_compExprNew( int iType )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNew(%i)", iType));

   pExpr = ( HB_EXPR_PTR ) HB_XGRAB( sizeof( HB_EXPR ) );

   pExpr->ExprType = iType;
   pExpr->pNext    = NULL;
   pExpr->ValType  = HB_EV_UNKNOWN;
   pExpr->Counter  = 1;

   return pExpr;
}

/* Delete self - all components will be deleted somewhere else
 */
void hb_compExprClear( HB_EXPR_PTR pExpr )
{
   if( --pExpr->Counter == 0 )
      HB_XFREE( pExpr );
}

/* Increase a reference counter (this allows to share the same expression
 * in more then one context)
 */
HB_EXPR_PTR hb_compExprClone( HB_EXPR_PTR pSrc )
{
   pSrc->Counter++;
   return pSrc;
}

char * hb_compExprDescription( HB_EXPR_PTR pExpr )
{
   if( pExpr )
      return s_OperTable[ pExpr->ExprType ];
   else
      return s_OperTable[ 0 ];
}

int hb_compExprType( HB_EXPR_PTR pExpr )
{
   return ( int ) pExpr->ExprType;
}

/* ************************************************************************* */

HB_EXPR_PTR hb_compExprNewEmpty( void )
{
   return hb_compExprNew( HB_ET_NONE );
}

HB_EXPR_PTR hb_compExprNewDouble( double dValue, BYTE ucWidth, BYTE ucDec )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewDouble(%f, %i)", dValue, ucDec));

   pExpr =hb_compExprNew( HB_ET_NUMERIC );

   pExpr->value.asNum.dVal    = dValue;
   pExpr->value.asNum.bWidth  = ucWidth;
   pExpr->value.asNum.bDec    = ucDec;
   pExpr->value.asNum.NumType = HB_ET_DOUBLE;
   pExpr->ValType = HB_EV_NUMERIC;

   return pExpr;
}

HB_EXPR_PTR hb_compExprNewLong( long lValue )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewLong(%li)", lValue));

   pExpr =hb_compExprNew( HB_ET_NUMERIC );

   pExpr->value.asNum.lVal    = lValue;
   pExpr->value.asNum.bDec    = 0;
   pExpr->value.asNum.NumType = HB_ET_LONG;
   pExpr->ValType = HB_EV_NUMERIC;

   return pExpr;
}


HB_EXPR_PTR hb_compExprNewCodeBlock( void )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewCodeBlock()"));

   pExpr =hb_compExprNew( HB_ET_CODEBLOCK );

   pExpr->value.asList.pExprList = NULL;
   pExpr->value.asList.pIndex    = NULL;  /* this will hold local variables declarations */
   pExpr->ValType = HB_EV_CODEBLOCK;

   return pExpr;
}

HB_EXPR_PTR hb_compExprNewLogical( int iValue )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewLogical(%i)", iValue));

   pExpr =hb_compExprNew( HB_ET_LOGICAL );

   pExpr->value.asLogical = iValue;
   pExpr->ValType = HB_EV_LOGICAL;

   return pExpr;
}


HB_EXPR_PTR hb_compExprNewNil( void )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewNil()"));

   pExpr = hb_compExprNew( HB_ET_NIL );

   pExpr->ValType = HB_EV_NIL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewSelf( void )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewSelf()"));

   pExpr =hb_compExprNew( HB_ET_SELF );

   pExpr->ValType = HB_EV_OBJECT;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewVarRef( char * szVarName )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewVarRef(%s)", szVarName));

   pExpr =hb_compExprNew( HB_ET_VARREF );

   pExpr->value.asSymbol = szVarName;
   pExpr->ValType = HB_EV_VARREF;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewFunRef( char * szFunName )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewFunRef(%s)", szFunName));

   pExpr =hb_compExprNew( HB_ET_FUNREF );

   pExpr->value.asSymbol = szFunName;
   pExpr->ValType = HB_EV_FUNREF;
   return pExpr;
}

/* Creates a new literal array { item1, item2, ... itemN }
 *    'pArrList' is a list of array elements
 */
HB_EXPR_PTR hb_compExprNewArray( HB_EXPR_PTR pArrList )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewArray()"));

   pArrList->ExprType = HB_ET_ARRAY;   /* change type from ET_LIST */
   pArrList->ValType  = HB_EV_ARRAY;
   pArrList->ulLength = 0;

   pExpr = pArrList->value.asList.pExprList;   /* get first element on the list */
   /* Now we need to replace all EO_NONE expressions with ET_NIL expressions
    * If EO_NONE is the first expression and there is no more expressions
    * then it is an empty array {} and ET_NIL cannot be used
    */
   if( pExpr->ExprType == HB_ET_NONE && pExpr->pNext == NULL )
   {
      pArrList->value.asList.pExprList = NULL;
   }
   else
   {
      /* there are at least one non-empty element specified
       */
      while( pExpr )
      {
         /* if empty element was specified replace it with NIL value */
         if( pExpr->ExprType == HB_ET_NONE )
            pExpr->ExprType = HB_ET_NIL;
         pExpr = pExpr->pNext;
         ++pArrList->ulLength;
      }
   }
   pArrList->value.asList.pIndex = NULL;
   return pArrList;
}

/* Creates new macro expression
 */
HB_EXPR_PTR hb_compExprNewMacro( HB_EXPR_PTR pMacroExpr, unsigned char cMacroOp, char * szName )
{
   HB_EXPR_PTR pExpr;

   if( szName )
   {
      HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewMacro(%s)", szName));

      /* Macro variable is used:  &identifier
       * or macro text: [text]&variable[more_macro_text]
       */
      /*
       * NOTE: Clipper assumes that all variables used in macro expressions
       * are memvar variables
       * NOTE: Clipper pushes the complete macro expression converted
       * to string in case complex expression is used, e.g.
       * My&var.1
       * is pushed as:
       * "MY&VAR.1"
       */
      pExpr = hb_compExprNew( HB_ET_MACRO );
      pExpr->value.asMacro.cMacroOp  = cMacroOp; /* '&' if variable or 0 if text */
      pExpr->value.asMacro.szMacro   = szName;   /* variable name or macro text */
      pExpr->value.asMacro.pExprList = NULL;     /* this is not a parenthesized expressions */
      pExpr->value.asMacro.SubType   = HB_ET_MACRO_VAR;

      if( cMacroOp == 0 )
      {
         /* check if variable with valid scope is used in macro text
          * (local, static and field variables are not allowed)
          * e.g.
          * LOCAL var
          * ? &var      // this is OK
          * ? &var.ext  // this is invalid
          */
         hb_compExprCheckMacroVar( szName );
      }
   }
   else
   {
      HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewMacro(&)"));

      /* Macro expression:  &( expression_list )
       */
      pExpr = hb_compExprNew( HB_ET_MACRO );
      pExpr->value.asMacro.pExprList = pMacroExpr;
      pExpr->value.asMacro.szMacro   = NULL; /* this is used to distinguish &(...) from &ident */
      pExpr->value.asMacro.SubType   = HB_ET_MACRO_EXPR;
   }

   return pExpr;
}

/* Creates new aliased variable
 *    aliasexpr -> identifier
 */
HB_EXPR_PTR hb_compExprNewAliasVar( HB_EXPR_PTR pAlias, HB_EXPR_PTR pVariable )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewAliasVar()"));

   pExpr = hb_compExprNew( HB_ET_ALIASVAR );

   pExpr->value.asAlias.pAlias    = pAlias;
   pExpr->value.asAlias.pVar      = pVariable;
   pExpr->value.asAlias.pExpList  = NULL;

   /* macro expressions in alias context require a special handling
    */
   if( pAlias->ExprType == HB_ET_MACRO )
      pAlias->value.asMacro.SubType = HB_ET_MACRO_ALIASED;
   if( pVariable->ExprType == HB_ET_MACRO )
      pVariable->value.asMacro.SubType = HB_ET_MACRO_ALIASED;

   return pExpr;
}

/* Creates new aliased expression
 *    alias_expr -> ( expression )
 */
HB_EXPR_PTR hb_compExprNewAliasExpr( HB_EXPR_PTR pAlias, HB_EXPR_PTR pExpList )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewAliasExpr()"));

   pExpr = hb_compExprNew( HB_ET_ALIASEXPR );

   pExpr->value.asAlias.pAlias    = pAlias;
   pExpr->value.asAlias.pExpList  = pExpList;
   pExpr->value.asAlias.pVar      = NULL;

   return pExpr;
}

/* Creates new send expression
 *    pObject : szMessage
 */
HB_EXPR_PTR hb_compExprNewSend( HB_EXPR_PTR pObject, char * szMessage )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewSend(%p, %s)", pObject, szMessage));

   pExpr = hb_compExprNew( HB_ET_SEND );
   pExpr->value.asMessage.szMessage = szMessage;
   pExpr->value.asMessage.pObject   = pObject;
   pExpr->value.asMessage.pParms     = NULL;

   return pExpr;
}

/* Creates new method call
 *    pObject : identifier ( pArgList )
 *
 *    pObject = is an expression returned by hb_compExprNewSend
 *    pArgList = list of passed arguments - it will be HB_ET_NONE if no arguments
 *                are passed
 */
HB_EXPR_PTR hb_compExprNewMethodCall( HB_EXPR_PTR pObject, HB_EXPR_PTR pArgList )
{
   pObject->value.asMessage.pParms = pArgList;

   return pObject;
}

/* Creates a list - all elements will be used
 * This list can be used to create an array or function's call arguments
 */
HB_EXPR_PTR hb_compExprNewList( HB_EXPR_PTR pFirstItem )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewList()"));

   pExpr = hb_compExprNew( HB_ET_LIST );
   pExpr->value.asList.pExprList = pFirstItem;
   return pExpr;
}

/* Creates a list of function call arguments
 */
HB_EXPR_PTR hb_compExprNewArgList( HB_EXPR_PTR pFirstItem )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewArgList()"));

   pExpr = hb_compExprNew( HB_ET_ARGLIST );
   pExpr->value.asList.pExprList = pFirstItem;
   return pExpr;
}

/* Adds new element to the list
 */
HB_EXPR_PTR hb_compExprAddListExpr( HB_EXPR_PTR pList, HB_EXPR_PTR pNewItem )
{
   if( pList->value.asList.pExprList )
   {
      HB_EXPR_PTR pExpr;

      /* add new item to the end of the list */
      pExpr = pList->value.asList.pExprList;
      while( pExpr->pNext )
         pExpr = pExpr->pNext;
      pExpr->pNext = pNewItem;
   }
   else
      pList->value.asList.pExprList = pNewItem;

   return pList;
}

HB_EXPR_PTR hb_compExprNewVar( char * szName )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewVar(%s)", szName));

   pExpr = hb_compExprNew( HB_ET_VARIABLE );
   pExpr->value.asSymbol = szName;
   return pExpr;
}

/* Create a new declaration of PUBLIC or PRIVATE variable.
 *
 * szName is a string with variable name if 'PUBLIC varname' context
 * pMacroVar is a macro expression if 'PUBLIC &varname' context
 */
HB_EXPR_PTR hb_compExprNewRTVar( char * szName, HB_EXPR_PTR pMacroVar )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewRTVar(%s, %p)", szName, pMacroVar));

   pExpr = hb_compExprNew( HB_ET_RTVAR );
   pExpr->value.asRTVar.szName = szName;
   pExpr->value.asRTVar.pMacro = pMacroVar;
   if( pMacroVar )
      pMacroVar->value.asMacro.SubType = HB_ET_MACRO_SYMBOL;
   return pExpr;
}

/* Create a new symbol used in function calls
 */
HB_EXPR_PTR hb_compExprNewFunName( char * szName )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewFunName(%s)", szName));

   pExpr = hb_compExprNew( HB_ET_FUNNAME );
   pExpr->value.asSymbol = szName;
   return pExpr;
}

/* Create a new symbol used in an alias expressions
 */
HB_EXPR_PTR hb_compExprNewAlias( char * szName )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewAlias(%s)", szName));

   pExpr = hb_compExprNew( HB_ET_ALIAS );
   pExpr->value.asSymbol = szName;
   return pExpr;
}


/* ************************************************************************* */

HB_EXPR_PTR hb_compExprNewEqual( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_EQUAL );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPlus( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_PLUS );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewMinus( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_MINUS );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewMult( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_MULT );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewDiv( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_DIV );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewMod( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_MOD );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPower( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_POWER );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPostInc( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_POSTINC );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPostDec( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_POSTDEC );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPreInc( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_PREINC );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPreDec( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_PREDEC );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPlusEq( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_PLUSEQ );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewMinusEq( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_MINUSEQ );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewMultEq( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_MULTEQ );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewDivEq( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_DIVEQ );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewModEq( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_MODEQ );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewExpEq( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_EXPEQ );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewAnd( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_AND );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewOr( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_OR );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewNot( HB_EXPR_PTR pNotExpr )
{
   HB_EXPR_PTR pExpr;

   if( pNotExpr->ExprType == HB_ET_LOGICAL )
   {
      pNotExpr->value.asLogical = ! pNotExpr->value.asLogical;
      pExpr = pNotExpr;
   }
   else
   {
      pExpr = hb_compExprNew( HB_EO_NOT );
      pExpr->value.asOperator.pLeft  = pNotExpr;
      pExpr->value.asOperator.pRight = NULL;
   }

   return pExpr;
}

HB_EXPR_PTR hb_compExprNewEQ( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_EQ );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewLT( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_LT );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewGT( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_GT );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewLE( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_LE );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewGE( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_GE );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewNE( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_NE );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewIN( HB_EXPR_PTR pLeftExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_IN );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

/* NOTE: all invalid cases are handled by yacc rules
 */
HB_EXPR_PTR hb_compExprNewNegate( HB_EXPR_PTR pNegExpr )
{
   HB_EXPR_PTR pExpr;

   if( pNegExpr->ExprType == HB_ET_NUMERIC )
   {
      if( pNegExpr->value.asNum.NumType == HB_ET_DOUBLE )
         pNegExpr->value.asNum.dVal = - pNegExpr->value.asNum.dVal;
      else
         pNegExpr->value.asNum.lVal = - pNegExpr->value.asNum.lVal;
      pExpr = pNegExpr;
   }
   else
   {
      pExpr = hb_compExprNew( HB_EO_NEGATE );
      pExpr->value.asOperator.pLeft = pNegExpr;
      pExpr->value.asOperator.pRight = NULL;
   }
   return pExpr;
}

/* ************************************************************************* */

/* Handles (expression := expression) syntax
 */
HB_EXPR_PTR hb_compExprAssign( HB_EXPR_PTR pLeftExpr, HB_EXPR_PTR pRightExpr )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprAssign()"));

   pExpr = hb_compExprNew( HB_EO_ASSIGN );
   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = pRightExpr;
   return pExpr;
}

/*  Return a number of elements on the linked list
 */
ULONG hb_compExprListLen( HB_EXPR_PTR pExpr )
{
   ULONG ulLen = 0;

   pExpr = pExpr->value.asList.pExprList;
   while( pExpr )
   {
      pExpr = pExpr->pNext;
      ++ulLen;
   }

   return ulLen;
}

