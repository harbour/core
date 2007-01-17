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

/* NOTE: This must be the first definition
 *    This is a common code shared by macro and standalone compiler
 */
#define  HB_COMMON_SUPPORT

#include <math.h>
#include "hbmacro.h"
#include "hbcomp.h"

/* memory allocation
 */
#define  HB_XGRAB( size )  hb_xgrab( (size) )
#define  HB_XFREE( pPtr )  hb_xfree( (void *)(pPtr) )

static const char * s_OperTable[ HB_EXPR_COUNT ] = {
   "",
   "NIL",
   "Numeric",
   "Date",
   "String",
   "Codeblock",
   "Logical",
   "SELF",
   "Array",
   "@",
   "@",
   "@",
   "IIF",
   ",",
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

/* Increase a reference counter (this allows to share the same expression
 * in more then one context)
 */
HB_EXPR_PTR hb_compExprClone( HB_EXPR_PTR pSrc )
{
   pSrc->Counter++;
   return pSrc;
}

const char * hb_compExprDescription( HB_EXPR_PTR pExpr )
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

int hb_compExprIsInteger( HB_EXPR_PTR pExpr )
{
   return ( pExpr->ExprType == HB_ET_NUMERIC && pExpr->value.asNum.NumType == HB_ET_LONG &&
            HB_LIM_INT16( pExpr->value.asNum.val.l ) );
}

int hb_compExprIsLong( HB_EXPR_PTR pExpr )
{
   return ( pExpr->ExprType == HB_ET_NUMERIC && pExpr->value.asNum.NumType == HB_ET_LONG );
}

int hb_compExprIsString( HB_EXPR_PTR pExpr )
{
   return ( pExpr->ExprType == HB_ET_STRING );
}

char * hb_compExprAsString( HB_EXPR_PTR pExpr )
{
   if( pExpr->ExprType == HB_ET_STRING )
      return pExpr->value.asString.string;
   return NULL;
}

int hb_compExprAsStringLen( HB_EXPR_PTR pExpr )
{
   if( pExpr->ExprType == HB_ET_STRING )
      return pExpr->ulLength;
   return 0;
}

int hb_compExprAsInteger( HB_EXPR_PTR pExpr )
{
   if( pExpr->ExprType == HB_ET_NUMERIC && pExpr->value.asNum.NumType == HB_ET_LONG )
      return ( int ) pExpr->value.asNum.val.l;
   else
      return 0;
}

HB_LONG hb_compExprAsLong( HB_EXPR_PTR pExpr )
{
   if( pExpr->ExprType == HB_ET_NUMERIC && pExpr->value.asNum.NumType == HB_ET_LONG )
      return pExpr->value.asNum.val.l;
   else
      return 0;
}

char *hb_compExprAsSymbol( HB_EXPR_PTR pExpr )
{
   switch( pExpr->ExprType )
   {
      case HB_ET_VARIABLE:
      case HB_ET_VARREF:
      case HB_ET_FUNNAME:
         return pExpr->value.asSymbol;

      case HB_ET_FUNCALL:
         return pExpr->value.asFunCall.pFunName->value.asSymbol;
   }
   return NULL;
}

/* ************************************************************************* */

HB_EXPR_PTR hb_compExprNewEmpty( HB_COMP_DECL )
{
   return hb_compExprNew( HB_ET_NONE, HB_COMP_PARAM );
}

HB_EXPR_PTR hb_compExprNewDouble( double dValue, BYTE ucWidth, BYTE ucDec,
                                  HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewDouble(%f, %i, %p)", dValue, ucDec, HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_NUMERIC, HB_COMP_PARAM );

   pExpr->value.asNum.val.d   = dValue;
   pExpr->value.asNum.bWidth  = ucWidth;
   pExpr->value.asNum.bDec    = ucDec;
   pExpr->value.asNum.NumType = HB_ET_DOUBLE;
   pExpr->ValType = HB_EV_NUMERIC;

   return pExpr;
}

HB_EXPR_PTR hb_compExprNewLong( HB_LONG lValue, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewLong(%" PFHL "d, %p)", lValue, HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_NUMERIC, HB_COMP_PARAM );

   pExpr->value.asNum.val.l   = lValue;
   pExpr->value.asNum.bDec    = 0;
   pExpr->value.asNum.NumType = HB_ET_LONG;
   pExpr->ValType = HB_EV_NUMERIC;

   return pExpr;
}

HB_EXPR_PTR hb_compExprNewDate( HB_LONG lValue, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewDate(%" PFHL "d, %p)", lValue, HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_DATE, HB_COMP_PARAM );

   pExpr->value.asNum.val.l = lValue;
   pExpr->ValType = HB_EV_DATE;

   return pExpr;
}

HB_EXPR_PTR hb_compExprNewString( char *szValue, ULONG ulLen, BOOL fDealloc, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewString(%s)", szValue));

   pExpr = hb_compExprNew( HB_ET_STRING, HB_COMP_PARAM );

   pExpr->value.asString.string = szValue;
   pExpr->value.asString.dealloc = fDealloc;
   pExpr->ulLength = ulLen;
   pExpr->ValType = HB_EV_STRING;

   return pExpr;
}

/* Creates a new literal array { item1, item2, ... itemN }
 *    'pArrList' is a list of array elements
 */
HB_EXPR_PTR hb_compExprNewArray( HB_EXPR_PTR pArrList, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewArray()"));

   pArrList->ExprType = HB_ET_ARRAY;   /* change type from ET_LIST */
   pArrList->ValType  = HB_EV_ARRAY;
   pArrList->ulLength = 0;
   pArrList->value.asList.reference = FALSE;

   pExpr = pArrList->value.asList.pExprList;   /* get first element on the list */
   /* Now we need to replace all EO_NONE expressions with ET_NIL expressions
    * If EO_NONE is the first expression and there is no more expressions
    * then it is an empty array {} and ET_NIL cannot be used
    */
   if( pExpr->ExprType == HB_ET_NONE && pExpr->pNext == NULL )
   {
      pArrList->value.asList.pExprList = NULL;
      HB_EXPR_PCODE1( hb_compExprDelete, pExpr );
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


HB_EXPR_PTR hb_compExprNewCodeBlock( char *string, int iLen, int iFlags, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewCodeBlock(%s,%d,%d,%p)",string, iLen, iFlags, HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_CODEBLOCK, HB_COMP_PARAM );

   pExpr->value.asCodeblock.pExprList = NULL;
   pExpr->value.asCodeblock.pLocals   = NULL;  /* this will hold local variables declarations */
   pExpr->ValType = HB_EV_CODEBLOCK;
   pExpr->value.asCodeblock.string = string;
   pExpr->value.asCodeblock.length = ( USHORT ) iLen;
   pExpr->value.asCodeblock.flags  = ( USHORT ) iFlags;
   return pExpr;
}

HB_EXPR_PTR hb_compExprAddCodeblockExpr( HB_EXPR_PTR pList, HB_EXPR_PTR pNewItem )
{
   if( pList->value.asCodeblock.pExprList )
   {
      HB_EXPR_PTR pExpr;

      /* add new item to the end of the list */
      pExpr = pList->value.asCodeblock.pExprList;
      while( pExpr->pNext )
         pExpr = pExpr->pNext;
      pExpr->pNext = pNewItem;
   }
   else
      pList->value.asCodeblock.pExprList = pNewItem;

   return pList;
}

HB_EXPR_PTR hb_compExprNewLogical( int iValue, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewLogical(%i,%p)", iValue, HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_LOGICAL, HB_COMP_PARAM );

   pExpr->value.asLogical = iValue;
   pExpr->ValType = HB_EV_LOGICAL;

   return pExpr;
}


HB_EXPR_PTR hb_compExprNewNil( HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewNil(%p)", HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_NIL, HB_COMP_PARAM );

   pExpr->ValType = HB_EV_NIL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewSelf( HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewSelf(%p)", HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_SELF, HB_COMP_PARAM );

   pExpr->ValType = HB_EV_OBJECT;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewVarRef( char * szVarName, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewVarRef(%s,%p)", szVarName, HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_VARREF, HB_COMP_PARAM );

   pExpr->value.asSymbol = szVarName;
   pExpr->ValType = HB_EV_VARREF;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewFunRef( char * szFunName, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewFunRef(%s,%p)", szFunName, HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_FUNREF, HB_COMP_PARAM );

   pExpr->value.asSymbol = szFunName;
   pExpr->ValType = HB_EV_FUNREF;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewRef( HB_EXPR_PTR pRefer, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewRef(%p,%p)", pRefer, HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_REFERENCE, HB_COMP_PARAM );

   pExpr->value.asReference = pRefer;
   pExpr->ValType = HB_EV_VARREF;
   return pExpr;
}

/* Creates new macro expression
 */
HB_EXPR_PTR hb_compExprNewMacro( HB_EXPR_PTR pMacroExpr,
                                 unsigned char cMacroOp, char * szName,
                                 HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   pExpr = hb_compExprNew( HB_ET_MACRO, HB_COMP_PARAM );
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
      pExpr->value.asMacro.cMacroOp  = cMacroOp; /* '&' if variable or 0 if text */
      pExpr->value.asMacro.szMacro   = szName;   /* variable name or macro text */
      pExpr->value.asMacro.pExprList = NULL;     /* this is not a parenthesized expressions */
      pExpr->value.asMacro.SubType   = HB_ET_MACRO_VAR;
   }
   else
   {
      HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewMacro(&)"));

      /* Macro expression:  &( expression_list )
       */
      pExpr->value.asMacro.cMacroOp  = 0;
      pExpr->value.asMacro.szMacro   = NULL; /* this is used to distinguish &(...) from &ident */
      pExpr->value.asMacro.pExprList = pMacroExpr;
      pExpr->value.asMacro.SubType   = HB_ET_MACRO_EXPR;
   }

   return pExpr;
}

/* Creates new aliased variable
 *    aliasexpr -> identifier
 */
HB_EXPR_PTR hb_compExprNewAliasVar( HB_EXPR_PTR pAlias, HB_EXPR_PTR pVariable,
                                    HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewAliasVar()"));

   pExpr = hb_compExprNew( HB_ET_ALIASVAR, HB_COMP_PARAM );

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
HB_EXPR_PTR hb_compExprNewAliasExpr( HB_EXPR_PTR pAlias, HB_EXPR_PTR pExpList,
                                     HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewAliasExpr()"));

   pExpr = hb_compExprNew( HB_ET_ALIASEXPR, HB_COMP_PARAM );

   pExpr->value.asAlias.pAlias    = pAlias;
   pExpr->value.asAlias.pExpList  = pExpList;
   pExpr->value.asAlias.pVar      = NULL;
   
   if( pAlias->ExprType == HB_ET_MACRO )
   {
      /* Is it a special case &variable->( expressionList ) */
      if( pAlias->value.asMacro.SubType == HB_ET_MACRO_VAR ||
          pAlias->value.asMacro.SubType == HB_ET_MACRO_EXPR )
        pAlias->value.asMacro.SubType = HB_ET_MACRO_ALIASED;
   }

   return pExpr;
}

/* Creates new method call
 *    pObject : identifier ( pArgList )
 *
 *    pObject  = is an expression returned by hb_compExprNewSend
 *    pArgList = list of passed arguments - it will be HB_ET_NONE if no arguments
 *                are passed
 */
HB_EXPR_PTR hb_compExprNewMethodCall( HB_EXPR_PTR pObject, HB_EXPR_PTR pArgList )
{
   pObject->value.asMessage.pParms = pArgList;

   return pObject;
}

/* Create a new IIF() expression
 * pExpr is a list of three expressions
 */
HB_EXPR_PTR hb_compExprNewIIF( HB_EXPR_PTR pExpr )
{
   pExpr->ExprType = HB_ET_IIF;

   return pExpr;
}

/* Creates a list - all elements will be used
 * This list can be used to create an array or function's call arguments
 */
HB_EXPR_PTR hb_compExprNewList( HB_EXPR_PTR pFirstItem, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewList()"));

   pExpr = hb_compExprNew( HB_ET_LIST, HB_COMP_PARAM );
   pExpr->value.asList.pExprList = pFirstItem;
   pExpr->value.asList.reference = FALSE;
   return pExpr;
}

/* Creates a list of function call arguments
 */
HB_EXPR_PTR hb_compExprNewArgList( HB_EXPR_PTR pFirstItem, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewArgList()"));

   pExpr = hb_compExprNew( HB_ET_ARGLIST, HB_COMP_PARAM );
   pExpr->value.asList.pExprList = pFirstItem;
   pExpr->value.asList.reference = FALSE;
   return pExpr;
}

/* Creates a reference to variable arguments
 */
HB_EXPR_PTR hb_compExprNewArgRef( HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewArgRef()"));

   pExpr = hb_compExprNew( HB_ET_ARGLIST, HB_COMP_PARAM );
   pExpr->value.asList.pExprList = NULL;
   pExpr->value.asList.reference = TRUE;
   return pExpr;
}

/* Creates a list of function call arguments
 */
HB_EXPR_PTR hb_compExprNewMacroArgList( HB_EXPR_PTR pFirstItem, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewMacroArgList()"));

   pExpr = hb_compExprNew( HB_ET_MACROARGLIST, HB_COMP_PARAM );
   pExpr->value.asList.pExprList = pFirstItem;
   pExpr->value.asList.reference = FALSE;
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

HB_EXPR_PTR hb_compExprNewVar( char * szName, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewVar(%s,%p)", szName, HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_VARIABLE, HB_COMP_PARAM );
   pExpr->value.asSymbol = szName;
   return pExpr;
}

/* Create a new declaration of PUBLIC or PRIVATE variable.
 *
 * szName is a string with variable name if 'PUBLIC varname' context
 * pMacroVar is a macro expression if 'PUBLIC &varname' context
 */
HB_EXPR_PTR hb_compExprNewRTVar( char * szName, HB_EXPR_PTR pMacroVar,
                                 HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewRTVar(%s, %p, %p)", szName, pMacroVar, HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_RTVAR, HB_COMP_PARAM );
   pExpr->value.asRTVar.szName = szName;
   pExpr->value.asRTVar.pMacro = pMacroVar;
   if( pMacroVar )
      pMacroVar->value.asMacro.SubType = HB_ET_MACRO_SYMBOL;
   return pExpr;
}

/* Create a new symbol used in function calls
 */
HB_EXPR_PTR hb_compExprNewFunName( char * szName, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewFunName(%s,%p)", szName, HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_FUNNAME, HB_COMP_PARAM );
   pExpr->value.asSymbol = szName;
   return pExpr;
}

/* Create a new symbol used in an alias expressions
 */
HB_EXPR_PTR hb_compExprNewAlias( char * szName, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewAlias(%s,%p)", szName, HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_ALIAS, HB_COMP_PARAM );
   pExpr->value.asSymbol = szName;
   return pExpr;
}


/* ************************************************************************* */

HB_EXPR_PTR hb_compExprNewEqual( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_EQUAL, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPlus( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_PLUS, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewMinus( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_MINUS, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewMult( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_MULT, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewDiv( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_DIV, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewMod( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_MOD, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPower( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_POWER, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPostInc( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_POSTINC, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPostDec( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_POSTDEC, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPreInc( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_PREINC, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPreDec( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_PREDEC, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewPlusEq( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_PLUSEQ, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewMinusEq( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_MINUSEQ, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewMultEq( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_MULTEQ, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewDivEq( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_DIVEQ, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewModEq( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_MODEQ, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewExpEq( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_EXPEQ, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewAnd( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_AND, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewOr( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_OR, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewNot( HB_EXPR_PTR pNotExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   if( pNotExpr->ExprType == HB_ET_LOGICAL )
   {
      pNotExpr->value.asLogical = ! pNotExpr->value.asLogical;
      pExpr = pNotExpr;
   }
   else
   {
      pExpr = hb_compExprNew( HB_EO_NOT, HB_COMP_PARAM );
      pExpr->value.asOperator.pLeft  = pNotExpr;
      pExpr->value.asOperator.pRight = NULL;
   }

   return pExpr;
}

HB_EXPR_PTR hb_compExprNewEQ( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_EQ, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewLT( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_LT, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewGT( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_GT, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewLE( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_LE, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewGE( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_GE, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewNE( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_NE, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

HB_EXPR_PTR hb_compExprNewIN( HB_EXPR_PTR pLeftExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_IN, HB_COMP_PARAM );
   pExpr->value.asOperator.pLeft = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

/* NOTE: all invalid cases are handled by yacc rules
 */
HB_EXPR_PTR hb_compExprNewNegate( HB_EXPR_PTR pNegExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   if( pNegExpr->ExprType == HB_ET_NUMERIC )
   {
      if( pNegExpr->value.asNum.NumType == HB_ET_DOUBLE )
      {
         pNegExpr->value.asNum.val.d = - pNegExpr->value.asNum.val.d;
         pNegExpr->value.asNum.bWidth = HB_DBL_LENGTH( pNegExpr->value.asNum.val.d );
      }
      else
      {
         pNegExpr->value.asNum.val.l = - pNegExpr->value.asNum.val.l;
         pNegExpr->value.asNum.bWidth = HB_LONG_LENGTH( pNegExpr->value.asNum.val.l );
      }
      pExpr = pNegExpr;
   }
   else
   {
      pExpr = hb_compExprNew( HB_EO_NEGATE, HB_COMP_PARAM );
      pExpr->value.asOperator.pLeft = pNegExpr;
      pExpr->value.asOperator.pRight = NULL;
   }
   return pExpr;
}

/* ************************************************************************* */

/* Handles prefix&macro-> and &macro.sufix-> in macro compiler
 * Clipper uses macro var directly as alias name in such case
 */
HB_EXPR_PTR hb_compExprMacroAsAlias( HB_EXPR_PTR pExpr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprMacroAsAlias()"));

   if( pExpr->ExprType == HB_ET_VARIABLE )
      pExpr->ExprType = HB_ET_ALIAS;

   return pExpr;
}

/* Handles (expression := expression) syntax
 */
HB_EXPR_PTR hb_compExprAssign( HB_EXPR_PTR pLeftExpr, HB_EXPR_PTR pRightExpr,
                               HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprAssign()"));

   pExpr = hb_compExprNew( HB_EO_ASSIGN, HB_COMP_PARAM );
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

/*  Return a number of parameters passed to function or method
 */
ULONG hb_compExprParamListLen( HB_EXPR_PTR pExpr )
{
   ULONG ulLen = 0;

   if( pExpr )
   {
      HB_EXPR_PTR pParam = pExpr->value.asList.pExprList;
      while( pParam )
      {
         pParam = pParam->pNext;
         ++ulLen;
      }
      /* NOTE: if method or function with no parameters is called then the
       * list of parameters contain only one expression of type HB_ET_NONE
       * There is no need to calculate this parameter
       */
      if( ulLen == 1 && pExpr->value.asList.pExprList->ExprType == HB_ET_NONE )
         ulLen = 0;
   }

   return ulLen;
}

/*  Return a number of macro group elements on the linked list
 */
ULONG hb_compExprMacroListLen( HB_EXPR_PTR pExpr )
{
   ULONG ulLen = 0, ulItems = 0;

   pExpr = pExpr->value.asList.pExprList;
   while( pExpr )
   {
      if( pExpr->ExprType == HB_ET_MACRO &&
          ( pExpr->value.asMacro.SubType | HB_ET_MACRO_LIST ) )
      {
         if( ulItems )
         {
            ulItems = 0;
            ++ulLen;
         }
         ++ulLen;
      }
      else
         ++ulItems;
      pExpr = pExpr->pNext;
   }
   if( ulItems )
      ++ulLen;

   return ulLen;
}

ULONG hb_compExprParamListCheck( HB_COMP_DECL, HB_EXPR_PTR pExpr )
{
   ULONG ulLen = 0, ulItems = 0;
   if( pExpr )
   {
      HB_EXPR_PTR pElem;

      pElem = pExpr->value.asList.pExprList;
      while( pElem )
      {
         if( ( pElem->ExprType == HB_ET_MACRO && HB_SUPPORT_XBASE &&
               pElem->value.asMacro.SubType != HB_ET_MACRO_SYMBOL &&
               pElem->value.asMacro.SubType != HB_ET_MACRO_REFER &&
               pElem->value.asMacro.SubType != HB_ET_MACRO_ALIASED ) ||
             ( pElem->ExprType == HB_ET_ARGLIST &&
               pElem->value.asList.reference ) )
         {
            /* &macro was passed
               or optional parameters list passed, f.e.: f(a,b,...)
               - handle it differently then in a normal statement */
            if( pElem->ExprType == HB_ET_MACRO )
               pElem->value.asMacro.SubType |= HB_ET_MACRO_LIST;
            if( ulItems )
            {
               ulItems = 0;
               ++ulLen;
            }
            ++ulLen;
         }
         else
            ++ulItems;
         pElem = pElem->pNext;
      }

      if( ulLen )
      {
         if( ulItems )
            ++ulLen;
         /* Note: direct type change */
         pExpr->ExprType = HB_ET_MACROARGLIST;
      }
      /* NOTE: if method or function with no parameters is called then the
       * list of parameters contain only one expression of type HB_ET_NONE
       * There is no need to calculate this parameter
       */
      else if( ulItems == 1 &&
               pExpr->value.asList.pExprList->ExprType == HB_ET_NONE )
         ulLen = 0;
      else
         ulLen = ulItems;
   }

   return ulLen;
}
