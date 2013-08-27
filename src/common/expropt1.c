/*
 * Harbour Project source code:
 * Compiler Expression Optimizer - common expressions
 *
 * Copyright 1999 Ryszard Glab
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbmacro.h"
#include "hbcomp.h"

static const char * s_OperTable[ HB_EXPR_COUNT ] = {
   "",
   "NIL",
   "Numeric",
   "Date",
   "Timestamp",
   "String",
   "Codeblock",
   "Logical",
   "SELF",
   "Array",
   "Hash",
   "@func()",
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
   "(:=)",     /* setget */
   ":",
   "",         /* symbol */
   "",         /* alias */
   "",         /* RunTime variable */
   "",         /* variable */
   "++",       /* post-operators -> lowest precedence */
   "--",
   ":=",       /* assigments */
   "+=",
   "-=",
   "*=",
   "/=",
   "%=",
   "^=",
   ".OR.",     /* logical operators */
   ".AND.",
   ".NOT.",
   "=",        /* relational operators */
   "==",
   "!=",
   "$",
   "<",
   ">",
   "<=",
   ">=",
   "+",        /* addition */
   "-",
   "*",        /* multiple */
   "/",
   "%",
   "^",
   "-",        /* sign operator */
   "++",
   "--"
};

/* Table with operators precedence
 * NOTE:
 *    HB_ET_NIL is used for an ordinary values and post- operators
 *    HB_ET_NONE is used for invalid syntax, e.g. var := var1 += 2
 */
static const HB_BYTE s_PrecedTable[ HB_EXPR_COUNT ] = {
   HB_ET_NIL,                 /*   HB_ET_NONE = 0,    */
   HB_ET_NIL,                 /*   HB_ET_NIL,         */
   HB_ET_NIL,                 /*   HB_ET_NUMERIC,     */
   HB_ET_NIL,                 /*   HB_ET_DATE,        */
   HB_ET_NIL,                 /*   HB_ET_TIMESTAMP,   */
   HB_ET_NIL,                 /*   HB_ET_STRING,      */
   HB_ET_NIL,                 /*   HB_ET_CODEBLOCK,   */
   HB_ET_NIL,                 /*   HB_ET_LOGICAL,     */
   HB_ET_NIL,                 /*   HB_ET_SELF,        */
   HB_ET_NIL,                 /*   HB_ET_ARRAY,       */
   HB_ET_NIL,                 /*   HB_ET_HASH,        */
   HB_ET_NIL,                 /*   HB_ET_FUNREF,      */
   HB_ET_NIL,                 /*   HB_ET_VARREF,      */
   HB_ET_NIL,                 /*   HB_ET_REFERENCE,   */
   HB_ET_NIL,                 /*   HB_ET_IIF,         */
   HB_ET_NIL,                 /*   HB_ET_LIST,        */
   HB_ET_NIL,                 /*   HB_ET_ARGLIST,     */
   HB_ET_NIL,                 /*   HB_ET_MACROARGLIST,*/
   HB_ET_NIL,                 /*   HB_ET_ARRAYAT,     */
   HB_ET_NIL,                 /*   HB_ET_MACRO,       */
   HB_ET_NIL,                 /*   HB_ET_FUNCALL,     */
   HB_ET_NIL,                 /*   HB_ET_ALIASVAR,    */
   HB_ET_NIL,                 /*   HB_ET_ALIASEXPR,   */
   HB_ET_NIL,                 /*   HB_ET_SETGET,      */
   HB_ET_NIL,                 /*   HB_ET_SEND,        */
   HB_ET_NIL,                 /*   HB_ET_FUNNAME,     */
   HB_ET_NIL,                 /*   HB_ET_ALIAS,       */
   HB_ET_NIL,                 /*   HB_ET_RTVARIABLE,  */
   HB_ET_NIL,                 /*   HB_ET_VARIABLE,    */
   HB_ET_NIL,                 /*   HB_EO_POSTINC,     post-operators */
   HB_ET_NIL,                 /*   HB_EO_POSTDEC,     */
   HB_ET_NONE,                /*   HB_EO_ASSIGN,      assigments */
   HB_ET_NONE,                /*   HB_EO_PLUSEQ,      Invalid syntax */
   HB_ET_NONE,                /*   HB_EO_MINUSEQ,     */
   HB_ET_NONE,                /*   HB_EO_MULTEQ,      */
   HB_ET_NONE,                /*   HB_EO_DIVEQ,       */
   HB_ET_NONE,                /*   HB_EO_MODEQ,       */
   HB_ET_NONE,                /*   HB_EO_EXPEQ,       */
   HB_EO_OR,                  /*   HB_EO_OR,          logical operators */
   HB_EO_AND,                 /*   HB_EO_AND,         */
   HB_ET_NIL,                 /*   HB_EO_NOT,         */
   HB_EO_EQUAL,               /*   HB_EO_EQUAL,       relational operators */
   HB_EO_EQUAL,               /*   HB_EO_EQ,          */
   HB_EO_EQUAL,               /*   HB_EO_NE,          */
   HB_EO_LT,                  /*   HB_EO_IN,          */
   HB_EO_LT,                  /*   HB_EO_LT,          */
   HB_EO_LT,                  /*   HB_EO_GT,          */
   HB_EO_LT,                  /*   HB_EO_LE,          */
   HB_EO_LT,                  /*   HB_EO_GE,          */
   HB_EO_PLUS,                /*   HB_EO_PLUS,        addition */
   HB_EO_PLUS,                /*   HB_EO_MINUS,       */
   HB_EO_MULT,                /*   HB_EO_MULT,        multiple */
   HB_EO_MULT,                /*   HB_EO_DIV,         */
   HB_EO_MULT,                /*   HB_EO_MOD,         */
   HB_EO_POWER,               /*   HB_EO_POWER,       */
   HB_ET_NIL,                 /*   HB_EO_NEGATE,      sign operator */
   HB_ET_NIL,                 /*   HB_EO_PREINC,      */
   HB_ET_NIL                  /*   HB_EO_PREDEC,      pre-operators */
};

/* ************************************************************************* */

const char * hb_compExprDescription( PHB_EXPR pExpr )
{
   if( pExpr )
      return s_OperTable[ pExpr->ExprType ];
   else
      return s_OperTable[ 0 ];
}

int hb_compExprType( PHB_EXPR pExpr )
{
   return ( int ) pExpr->ExprType;
}

int hb_compExprIsInteger( PHB_EXPR pExpr )
{
   return pExpr->ExprType == HB_ET_NUMERIC && pExpr->value.asNum.NumType == HB_ET_LONG &&
          HB_LIM_INT16( pExpr->value.asNum.val.l );
}

int hb_compExprIsLong( PHB_EXPR pExpr )
{
   return pExpr->ExprType == HB_ET_NUMERIC && pExpr->value.asNum.NumType == HB_ET_LONG;
}

int hb_compExprIsString( PHB_EXPR pExpr )
{
   return pExpr->ExprType == HB_ET_STRING;
}

const char * hb_compExprAsString( PHB_EXPR pExpr )
{
   if( pExpr->ExprType == HB_ET_STRING )
      return pExpr->value.asString.string;
   return NULL;
}

HB_SIZE hb_compExprAsStringLen( PHB_EXPR pExpr )
{
   if( pExpr->ExprType == HB_ET_STRING )
      return pExpr->nLength;
   return 0;
}

int hb_compExprAsNumSign( PHB_EXPR pExpr )
{
   if( pExpr->ExprType == HB_ET_NUMERIC )
   {
      if( pExpr->value.asNum.NumType == HB_ET_DOUBLE )
      {
         if( pExpr->value.asNum.val.d > 0 )
            return 1;
         else if( pExpr->value.asNum.val.d < 0 )
            return -1;
      }
      else
      {
         if( pExpr->value.asNum.val.l > 0 )
            return 1;
         else if( pExpr->value.asNum.val.l < 0 )
            return -1;
      }
   }
   return 0;
}

int hb_compExprAsInteger( PHB_EXPR pExpr )
{
   if( pExpr->ExprType == HB_ET_NUMERIC && pExpr->value.asNum.NumType == HB_ET_LONG )
      return ( int ) pExpr->value.asNum.val.l;
   else
      return 0;
}

HB_MAXINT hb_compExprAsLongNum( PHB_EXPR pExpr )
{
   if( pExpr->ExprType == HB_ET_NUMERIC )
   {
      if( pExpr->value.asNum.NumType == HB_ET_LONG )
         return pExpr->value.asNum.val.l;
      else
         return ( HB_MAXINT ) pExpr->value.asNum.val.d;
   }
   else
      return 0;
}

const char * hb_compExprAsSymbol( PHB_EXPR pExpr )
{
   switch( pExpr->ExprType )
   {
      case HB_ET_VARIABLE:
      case HB_ET_VARREF:
      case HB_ET_FUNNAME:
         return pExpr->value.asSymbol.name;

      case HB_ET_FUNCALL:
         if( pExpr->value.asFunCall.pFunName->ExprType == HB_ET_FUNNAME )
            return pExpr->value.asFunCall.pFunName->value.asSymbol.name;
   }
   return NULL;
}

/* ************************************************************************* */

PHB_EXPR hb_compExprNewEmpty( HB_COMP_DECL )
{
   return HB_COMP_EXPR_NEW( HB_ET_NONE );
}

PHB_EXPR hb_compExprNewDouble( double dValue, HB_BYTE ucWidth, HB_BYTE ucDec,
                               HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewDouble(%f, %i, %p)", dValue, ucDec, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_NUMERIC );

   pExpr->value.asNum.val.d   = dValue;
   pExpr->value.asNum.bWidth  = ucWidth;
   pExpr->value.asNum.bDec    = ucDec;
   pExpr->value.asNum.NumType = HB_ET_DOUBLE;
   pExpr->ValType = HB_EV_NUMERIC;

   return pExpr;
}

PHB_EXPR hb_compExprNewLong( HB_MAXINT nValue, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewLong(%" PFHL "d, %p)", nValue, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_NUMERIC );

   pExpr->value.asNum.val.l   = nValue;
   pExpr->value.asNum.bWidth  = HB_DEFAULT_WIDTH;
   pExpr->value.asNum.bDec    = 0;
   pExpr->value.asNum.NumType = HB_ET_LONG;
   pExpr->ValType = HB_EV_NUMERIC;

   return pExpr;
}

PHB_EXPR hb_compExprNewDate( long lDate, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewDate(%ld, %p)", lDate, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_DATE );

   pExpr->value.asDate.lDate = lDate;
   pExpr->value.asDate.lTime = 0;
   pExpr->ValType = HB_EV_DATE;

   return pExpr;
}

PHB_EXPR hb_compExprNewTimeStamp( long lDate, long lTime, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewTimeStamp(%ld, %ld, %p)", lDate, lTime, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_TIMESTAMP );

   pExpr->value.asDate.lDate = lDate;
   pExpr->value.asDate.lTime = lTime;
   pExpr->ValType = HB_EV_TIMESTAMP;

   return pExpr;
}

PHB_EXPR hb_compExprNewString( const char * szValue, HB_SIZE nLen, HB_BOOL fDealloc, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewString(%s)", szValue ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_STRING );

   pExpr->value.asString.string  = ( char * ) szValue;
   pExpr->value.asString.dealloc = fDealloc;
   pExpr->nLength = nLen;
   pExpr->ValType = HB_EV_STRING;

   return pExpr;
}

/* Creates a new literal array { item1, item2, ... itemN }
 *    'pArrList' is a list of array elements
 */
PHB_EXPR hb_compExprNewArray( PHB_EXPR pArrList, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewArray()" ) );

   pArrList->ExprType = HB_ET_ARRAY;   /* change type from ET_LIST */
   pArrList->ValType  = HB_EV_ARRAY;
   pArrList->nLength  = 0;
   pArrList->value.asList.reference = HB_FALSE;

   pExpr = pArrList->value.asList.pExprList;   /* get first element on the list */
   /* Now we need to replace all EO_NONE expressions with ET_NIL expressions
    * If EO_NONE is the first expression and there is no more expressions
    * then it is an empty array {} and ET_NIL cannot be used
    */
   if( pExpr->ExprType == HB_ET_NONE && pExpr->pNext == NULL )
   {
      pArrList->value.asList.pExprList = NULL;
      HB_COMP_EXPR_FREE( pExpr );
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
         ++pArrList->nLength;
      }
   }
   pArrList->value.asList.pIndex = NULL;

   return pArrList;
}

/* Creates a new literal hash { key1=>val1, key2=>val2, ... keyN=>valN }
 *    'pHashList' is a list of hash items
 */
PHB_EXPR hb_compExprNewHash( PHB_EXPR pHashList, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewHash()" ) );

   if( pHashList )
      pHashList->ExprType = HB_ET_HASH;   /* change type from ET_LIST */
   else
   {
      pHashList = HB_COMP_EXPR_NEW( HB_ET_HASH );
      pHashList->value.asList.pExprList = NULL;
   }
   pHashList->ValType = HB_EV_HASH;
   pHashList->nLength = 0;
   pHashList->value.asList.reference = HB_FALSE;
   pHashList->value.asList.pIndex    = NULL;

   /*
    * replace all EO_NONE expressions with ET_NIL expressions and
    * calculate the list length
    */
   pExpr = pHashList->value.asList.pExprList;
   while( pExpr )
   {
      if( pExpr->ExprType == HB_ET_NONE )
         pExpr->ExprType = HB_ET_NIL;
      pExpr = pExpr->pNext;
      ++pHashList->nLength;
   }

   return pHashList;
}

PHB_EXPR hb_compExprNewCodeBlock( char * string, HB_SIZE nLen, int iFlags, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewCodeBlock(%s,%" HB_PFS "u,%d,%p)", string, nLen, iFlags, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_CODEBLOCK );

   pExpr->value.asCodeblock.pExprList = NULL;
   pExpr->value.asCodeblock.pLocals   = NULL;  /* this will hold local variables declarations */
   pExpr->ValType = HB_EV_CODEBLOCK;
   pExpr->value.asCodeblock.flags  = ( HB_USHORT ) iFlags;
   pExpr->value.asCodeblock.string = string;
   pExpr->nLength = nLen;
   return pExpr;
}

PHB_EXPR hb_compExprAddCodeblockExpr( PHB_EXPR pList, PHB_EXPR pNewItem )
{
   if( pList->value.asCodeblock.pExprList )
   {
      PHB_EXPR pExpr;

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

PHB_EXPR hb_compExprNewLogical( int iValue, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewLogical(%i,%p)", iValue, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_LOGICAL );

   pExpr->value.asLogical = iValue;
   pExpr->ValType         = HB_EV_LOGICAL;

   return pExpr;
}


PHB_EXPR hb_compExprNewNil( HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewNil(%p)", HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_NIL );

   pExpr->ValType = HB_EV_NIL;
   return pExpr;
}

PHB_EXPR hb_compExprNewSelf( HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewSelf(%p)", HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_SELF );

   pExpr->ValType = HB_EV_OBJECT;
   return pExpr;
}

PHB_EXPR hb_compExprNewVarRef( const char * szVarName, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewVarRef(%s,%p)", szVarName, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_VARREF );

   pExpr->value.asSymbol.name = szVarName;
   pExpr->ValType = HB_EV_VARREF;
   return pExpr;
}

PHB_EXPR hb_compExprNewFunRef( const char * szFunName, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewFunRef(%s,%p)", szFunName, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_FUNREF );

   pExpr->value.asSymbol.name = hb_compGetFuncID( szFunName,
                                                  &pExpr->value.asSymbol.funcid,
                                                  &pExpr->value.asSymbol.flags );
   pExpr->ValType = HB_EV_FUNREF;
   return pExpr;
}

PHB_EXPR hb_compExprNewRef( PHB_EXPR pRefer, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewRef(%p,%p)", pRefer, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_REFERENCE );

   pExpr->value.asReference = pRefer;
   pExpr->ValType = HB_EV_VARREF;
   return pExpr;
}

/* Creates new macro expression
 */
PHB_EXPR hb_compExprNewMacro( PHB_EXPR pMacroExpr,
                              unsigned char cMacroOp, const char * szName,
                              HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   pExpr = HB_COMP_EXPR_NEW( HB_ET_MACRO );
   if( szName )
   {
      HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewMacro(%s)", szName ) );

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
      HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewMacro(&)" ) );

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
PHB_EXPR hb_compExprNewAliasVar( PHB_EXPR pAlias, PHB_EXPR pVariable,
                                 HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewAliasVar()" ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_ALIASVAR );

   pExpr->value.asAlias.pAlias   = pAlias;
   pExpr->value.asAlias.pVar     = pVariable;
   pExpr->value.asAlias.pExpList = NULL;

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
PHB_EXPR hb_compExprNewAliasExpr( PHB_EXPR pAlias, PHB_EXPR pExpList,
                                  HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewAliasExpr()" ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_ALIASEXPR );

   pExpr->value.asAlias.pAlias   = pAlias;
   pExpr->value.asAlias.pExpList = pExpList;
   pExpr->value.asAlias.pVar     = NULL;

   if( pAlias->ExprType == HB_ET_MACRO )
   {
      /* Is it a special case &variable->( expressionList ) */
      if( pAlias->value.asMacro.SubType == HB_ET_MACRO_VAR ||
          pAlias->value.asMacro.SubType == HB_ET_MACRO_EXPR )
         pAlias->value.asMacro.SubType = HB_ET_MACRO_ALIASED;
   }

   return pExpr;
}

/* Creates new send expression
 *    : <msgid> -> ( expression )
 */
PHB_EXPR hb_compExprNewSend( const char * szMessage, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewSend(%s,%p)", szMessage, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_SEND );
   pExpr->value.asMessage.pObject = NULL;
   pExpr->value.asMessage.pParms  = NULL;

   pExpr->value.asMessage.szMessage = szMessage;
   pExpr->value.asMessage.pMessage  = NULL;

   pExpr->nLength = 0;

   return pExpr;
}

/* Creates new macro send expression
 *    : &<msg> -> ( expression )
 */
PHB_EXPR hb_compExprNewMacroSend( PHB_EXPR pMessage, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewMacroSend(%p,%p)", pMessage, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_SEND );
   pExpr->value.asMessage.pObject = NULL;
   pExpr->value.asMessage.pParms  = NULL;

   pExpr->value.asMessage.szMessage = NULL;
   pExpr->value.asMessage.pMessage  = pMessage;

   pExpr->nLength = 0;

   if( pMessage->ExprType == HB_ET_MACRO )
   {
      /* Signal that macro compiler have to generate a pcode that will
       * return function name as symbol instead of usual value
       */
      pMessage->value.asMacro.SubType = HB_ET_MACRO_SYMBOL;
   }

   return pExpr;
}

/* Set object in send expression
 *    pObject : pExpr
 *
 *    pExpr   = is an expression returned by hb_compExprNewSend
 *    pObject = is an object
 */
PHB_EXPR hb_compExprNewMethodObject( PHB_EXPR pExpr, PHB_EXPR pObject )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewMethodObject(%p,%p)", pExpr, pObject ) );

   pExpr->value.asMessage.pObject = pObject;

   return pExpr;
}

/* Creates new method call
 *    pObject : identifier ( pArgList )
 *
 *    pObject  = is an expression returned by hb_compExprNewSend
 *    pArgList = list of passed arguments - it will be HB_ET_NONE if no arguments
 *                are passed
 */
PHB_EXPR hb_compExprNewMethodCall( PHB_EXPR pObject, PHB_EXPR pArgList )
{
   pObject->value.asMessage.pParms = pArgList;

   return pObject;
}

/* Create a new IIF() expression
 * pExpr is a list of three expressions
 */
PHB_EXPR hb_compExprNewIIF( PHB_EXPR pExpr )
{
   pExpr->ExprType = HB_ET_IIF;

   return pExpr;
}

/* Creates a list - all elements will be used
 * This list can be used to create an array or function's call arguments
 */
PHB_EXPR hb_compExprNewList( PHB_EXPR pFirstItem, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewList()" ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_LIST );
   pExpr->value.asList.pExprList = pFirstItem;
   pExpr->value.asList.reference = HB_FALSE;
   return pExpr;
}

/* Creates a list of function call arguments
 */
PHB_EXPR hb_compExprNewArgList( PHB_EXPR pFirstItem, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewArgList()" ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_ARGLIST );
   pExpr->value.asList.pExprList = pFirstItem;
   pExpr->value.asList.reference = HB_FALSE;
   return pExpr;
}

/* Creates a reference to variable arguments
 */
PHB_EXPR hb_compExprNewArgRef( HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewArgRef()" ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_ARGLIST );
   pExpr->value.asList.pExprList = NULL;
   pExpr->value.asList.reference = HB_TRUE;
   return pExpr;
}

/* Adds new element to the list
 */
PHB_EXPR hb_compExprAddListExpr( PHB_EXPR pList, PHB_EXPR pNewItem )
{
   if( pList->value.asList.pExprList )
   {
      PHB_EXPR pExpr;

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

PHB_EXPR hb_compExprNewVar( const char * szName, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewVar(%s,%p)", szName, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_VARIABLE );
   pExpr->value.asSymbol.name = szName;
   return pExpr;
}

/* Create a new declaration of PUBLIC or PRIVATE variable.
 *
 * szName is a string with variable name if 'PUBLIC varname' context
 * pMacroVar is a macro expression if 'PUBLIC &varname' context
 */
PHB_EXPR hb_compExprNewRTVar( const char * szName, PHB_EXPR pMacroVar,
                              HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewRTVar(%s, %p, %p)", szName, pMacroVar, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_RTVAR );
   pExpr->value.asRTVar.szName = szName;
   pExpr->value.asRTVar.pMacro = pMacroVar;
   if( pMacroVar )
      pMacroVar->value.asMacro.SubType = HB_ET_MACRO_SYMBOL;
   return pExpr;
}

/* Create a new symbol used in function calls
 */
PHB_EXPR hb_compExprNewFunName( const char * szName, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewFunName(%s,%p)", szName, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_FUNNAME );
   pExpr->value.asSymbol.name = hb_compGetFuncID( szName,
                                                  &pExpr->value.asSymbol.funcid,
                                                  &pExpr->value.asSymbol.flags );
   return pExpr;
}

/* Create a new symbol used in an alias expressions
 */
PHB_EXPR hb_compExprNewAlias( const char * szName, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprNewAlias(%s,%p)", szName, HB_COMP_PARAM ) );

   pExpr = HB_COMP_EXPR_NEW( HB_ET_ALIAS );
   pExpr->value.asSymbol.name = szName;
   return pExpr;
}


/* ************************************************************************* */

PHB_EXPR hb_compExprNewEqual( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_EQUAL );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewPlus( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_PLUS );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewMinus( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_MINUS );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewMult( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_MULT );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewDiv( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_DIV );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewMod( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_MOD );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewPower( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_POWER );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewPostInc( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_POSTINC );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewPostDec( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_POSTDEC );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewPreInc( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_PREINC );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewPreDec( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_PREDEC );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewPlusEq( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_PLUSEQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewMinusEq( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_MINUSEQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewMultEq( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_MULTEQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewDivEq( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_DIVEQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewModEq( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_MODEQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewExpEq( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_EXPEQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewAnd( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_AND );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewOr( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_OR );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewNot( PHB_EXPR pNotExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   if( pNotExpr->ExprType == HB_ET_LOGICAL )
   {
      pNotExpr->value.asLogical = ! pNotExpr->value.asLogical;
      pExpr = pNotExpr;
   }
   else
   {
      pExpr = HB_COMP_EXPR_NEW( HB_EO_NOT );
      pExpr->value.asOperator.pLeft  = pNotExpr;
      pExpr->value.asOperator.pRight = NULL;
   }

   return pExpr;
}

PHB_EXPR hb_compExprNewEQ( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_EQ );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewLT( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_LT );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewGT( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_GT );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewLE( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_LE );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewGE( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_GE );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewNE( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_NE );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

PHB_EXPR hb_compExprNewIN( PHB_EXPR pLeftExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr = HB_COMP_EXPR_NEW( HB_EO_IN );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = NULL;
   return pExpr;
}

/* NOTE: all invalid cases are handled by yacc rules
 */
PHB_EXPR hb_compExprNewNegate( PHB_EXPR pNegExpr, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   if( pNegExpr->ExprType == HB_ET_NUMERIC )
   {
      if( pNegExpr->value.asNum.NumType == HB_ET_DOUBLE )
      {
         pNegExpr->value.asNum.val.d  = -pNegExpr->value.asNum.val.d;
         pNegExpr->value.asNum.bWidth = ( HB_UCHAR ) HB_DBL_LENGTH( pNegExpr->value.asNum.val.d );
      }
      else
      {
#if -HB_VMLONG_MAX > HB_VMLONG_MIN
         if( pNegExpr->value.asNum.val.l < -HB_VMLONG_MAX )
         {
            pNegExpr->value.asNum.NumType = HB_ET_DOUBLE;
            pNegExpr->value.asNum.val.d   = -( double ) pNegExpr->value.asNum.val.l;
            pNegExpr->value.asNum.bWidth  = ( HB_UCHAR ) HB_DBL_LENGTH( pNegExpr->value.asNum.val.d );
            pNegExpr->value.asNum.bDec    = 0;
         }
         else
#endif
         {
            pNegExpr->value.asNum.val.l  = -pNegExpr->value.asNum.val.l;
            pNegExpr->value.asNum.bWidth = HB_DEFAULT_WIDTH;
         }
      }
      pExpr = pNegExpr;
   }
   else
   {
      pExpr = HB_COMP_EXPR_NEW( HB_EO_NEGATE );
      pExpr->value.asOperator.pLeft  = pNegExpr;
      pExpr->value.asOperator.pRight = NULL;
   }
   return pExpr;
}

/* Handles (expression := expression) syntax
 */
PHB_EXPR hb_compExprAssign( PHB_EXPR pLeftExpr, PHB_EXPR pRightExpr,
                            HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprAssign()" ) );

   pExpr = HB_COMP_EXPR_NEW( HB_EO_ASSIGN );
   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = pRightExpr;
   return pExpr;
}

void hb_compExprDelOperator( PHB_EXPR pExpr, HB_COMP_DECL )
{
   if( pExpr->value.asOperator.pLeft )
      HB_COMP_EXPR_FREE( pExpr->value.asOperator.pLeft );
   if( pExpr->value.asOperator.pRight )
      HB_COMP_EXPR_FREE( pExpr->value.asOperator.pRight );
}

/* Sets the argument of an operation found previously
 */
PHB_EXPR hb_compExprSetOperand( PHB_EXPR pExpr, PHB_EXPR pItem, HB_COMP_DECL )
{
   HB_BYTE ucRight;

   ucRight = s_PrecedTable[ pItem->ExprType ];

   if( ucRight == HB_ET_NIL )
   {
      /* the right side of an operator is an ordinary value
       * e.g. a := 1
       */
      pExpr->value.asOperator.pRight = pItem;
   }
   else if( ucRight == HB_ET_NONE )
   {
      /* the right side of an operator is an invalid expression
       * e.g.
       *    a := 1 + b:=2
       *    a := 1 + b += 2
       */

      if( pExpr->ExprType >= HB_EO_PLUSEQ && pExpr->ExprType <= HB_EO_EXPEQ )
      {
      }
      else
      {
         HB_COMP_ERROR_SYNTAX( pItem );
      }
      pExpr->value.asOperator.pRight = pItem; /* set it anyway */
   }
   else
   {
      /* the right side of an operator is an expression with other operator
       * e.g. a := 2 + b * 3
       *   We have to set the proper order of evaluation using
       * precedence rules
       */
      HB_BYTE ucLeft = s_PrecedTable[ pExpr->ExprType ];
      if( ucLeft < ucRight ||
          ( ucLeft == ucRight && HB_COMP_ISSUPPORTED( HB_COMPFLAG_SHORTCUTS ) &&
            ( ucLeft == HB_EO_OR || ucLeft == HB_EO_AND ) ) )
      {
         /* Left operator has a lower precedence then the right one
          * e.g.  a + b * c
          *    pItem -> b * c    -> L=b  R=c  O=*
          *    pExpr -> a +      -> l=a  r=   o=+
          *
          *    -> a + (b * c)    -> Left=a  Right=(b * c)  Oper=+
          *             Left  := l
          *             Right := L (O) R  := pItem
          *             Oper  := o
          */
         pExpr->value.asOperator.pRight = pItem;
      }
      else
      {
         /* Left operator has the same or higer precedence then the right one
          * e.g.  a * b + c
          *    pItem -> b + c   -> L=b  R=c  O=+
          *    pExpr -> a *     -> l=a  r=   o=*
          *
          *    -> (a * b) + c    -> Lelf=(a * b)  Right=c  Oper=+
          *             Left  := l (o) L
          *             Right := R
          *             Oper  := O
          */
         pItem->value.asOperator.pLeft = hb_compExprSetOperand( pExpr, pItem->value.asOperator.pLeft, HB_COMP_PARAM );
         pExpr = pItem;
      }
   }

   return pExpr;
}


/* ************************************************************************* */

/* Handles prefix&macro-> and &macro.sufix-> in macro compiler
 * Clipper uses macro var directly as alias name in such case
 */
PHB_EXPR hb_compExprMacroAsAlias( PHB_EXPR pExpr )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprMacroAsAlias()" ) );

   if( pExpr->ExprType == HB_ET_VARIABLE )
      pExpr->ExprType = HB_ET_ALIAS;

   return pExpr;
}

/*  Return a number of elements on the linked list
 */
HB_ULONG hb_compExprListLen( PHB_EXPR pExpr )
{
   HB_ULONG nLen = 0;

   pExpr = pExpr->value.asList.pExprList;
   while( pExpr )
   {
      pExpr = pExpr->pNext;
      ++nLen;
   }

   return nLen;
}

HB_BOOL hb_compExprListTypeCheck( PHB_EXPR pExpr, HB_EXPRTYPE ExprType )
{
   pExpr = pExpr->value.asList.pExprList;
   if( pExpr )
   {
      do
      {
         if( pExpr->ExprType != ExprType )
            break;
         pExpr = pExpr->pNext;
      }
      while( pExpr );

      return pExpr == NULL;
   }
   return HB_FALSE;
}

/*  Return a number of parameters passed to function or method
 */
HB_ULONG hb_compExprParamListLen( PHB_EXPR pExpr )
{
   HB_ULONG nLen = 0;

   if( pExpr )
   {
      PHB_EXPR pParam = pExpr->value.asList.pExprList;
      while( pParam )
      {
         pParam = pParam->pNext;
         ++nLen;
      }
      /* NOTE: if method or function with no parameters is called then the
       * list of parameters contain only one expression of type HB_ET_NONE
       * There is no need to calculate this parameter
       */
      if( nLen == 1 && pExpr->value.asList.pExprList->ExprType == HB_ET_NONE )
         nLen = 0;
   }

   return nLen;
}

HB_SIZE hb_compExprParamListCheck( HB_COMP_DECL, PHB_EXPR pExpr )
{
   HB_SIZE nLen = 0, nItems = 0;

   if( pExpr )
   {
      PHB_EXPR pElem;

      pElem = pExpr->value.asList.pExprList;
      while( pElem )
      {
         if( ( pElem->ExprType == HB_ET_MACRO && HB_SUPPORT_XBASE &&
               pElem->value.asMacro.SubType != HB_ET_MACRO_SYMBOL &&
               pElem->value.asMacro.SubType != HB_ET_MACRO_REFER &&
               pElem->value.asMacro.SubType != HB_ET_MACRO_ALIASED ) ||
             ( pElem->ExprType == HB_ET_ARGLIST &&
               pElem->value.asList.reference ) ||
             ( pElem->ExprType == HB_ET_FUNCALL &&
               pElem->value.asFunCall.pFunName->ExprType == HB_ET_FUNNAME &&
               pElem->value.asFunCall.pFunName->value.asSymbol.funcid ==
               HB_F_ARRAYTOPARAMS ) )
         {
            /* &macro was passed
               or optional parameters list passed, f.e.: f(a,b,...)
               or hb_ArrayToParams( aParams )
               - handle it differently then in a normal statement */
            if( pElem->ExprType == HB_ET_MACRO )
               pElem->value.asMacro.SubType |= HB_ET_MACRO_LIST;
            else if( pElem->ExprType == HB_ET_FUNCALL )
               pElem->value.asFunCall.pFunName->value.asSymbol.flags |= HB_FN_MULTIARG;
            if( nItems )
            {
               nItems = 0;
               ++nLen;
            }
            ++nLen;
         }
         else
            ++nItems;
         pElem = pElem->pNext;
      }

      if( nLen )
      {
         if( nItems )
            ++nLen;
         /* Note: direct type change */
         pExpr->ExprType = HB_ET_MACROARGLIST;
      }
      /* NOTE: if method or function with no parameters is called then the
       * list of parameters contain only one expression of type HB_ET_NONE
       * There is no need to calculate this parameter
       */
      else if( nItems == 1 &&
               pExpr->value.asList.pExprList->ExprType == HB_ET_NONE )
         nLen = 0;
      else
         nLen = nItems;
   }

   return nLen;
}

/* Create a new declaration for codeblock local variable
 */
static PHB_CBVAR hb_compExprCBVarNew( const char * szVarName, HB_BYTE bType )
{
   PHB_CBVAR pVar;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprCBVarNew(%s)", szVarName ) );

   pVar = ( PHB_CBVAR ) hb_xgrab( sizeof( HB_CBVAR ) );

   pVar->szName = szVarName;
   pVar->bType  = bType;
   pVar->pNext  = NULL;
   pVar->bUsed  = HB_FALSE;

   return pVar;
}

/* Add a new local variable declaration
 */
PHB_EXPR hb_compExprCBVarAdd( PHB_EXPR pCB, const char * szVarName, HB_BYTE bType,
                              HB_COMP_DECL )
{
   PHB_CBVAR pVar;

   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprCBVarAdd(%s)", szVarName ) );

   if( pCB->value.asCodeblock.pLocals )
   {
      /* add it to the end of the list
       */
      pVar = pCB->value.asCodeblock.pLocals;
      while( pVar )
      {
         if( strcmp( szVarName, pVar->szName ) == 0 )
            HB_COMP_ERROR_DUPLVAR( szVarName );

         if( pVar->pNext )
            pVar = pVar->pNext;
         else
         {
            pVar->pNext = hb_compExprCBVarNew( szVarName, bType );
            break;
         }
      }
   }
   else
      pCB->value.asCodeblock.pLocals = hb_compExprCBVarNew( szVarName, bType );

   return pCB;
}

/* NOTE: This deletes all linked variables
 */
void hb_compExprCBVarDel( PHB_CBVAR pVars )
{
   PHB_CBVAR pDel;

   while( pVars )
   {
      pDel  = pVars;
      pVars = pVars->pNext;
      hb_xfree( pDel );
   }
}

/* Creates a set/get codeblock for passed expression used in __GET
 *
 * {| ~1 | IIF( ~1 == NIL, <pExpr>, <pExpr> := ~1 ) }
 *
 * NOTE: "~1" is not a valid variable name so there will be no collisions
 */
PHB_EXPR hb_compExprSetGetBlock( PHB_EXPR pExpr, HB_COMP_DECL )
{
   PHB_EXPR pSet;

   /* create setget expression: IIF( var==NIL, <pExpr>, <pExpr>:=var ) */
   pSet = HB_COMP_EXPR_NEW( HB_ET_SETGET );
   pSet->value.asSetGet.pVar  = hb_compExprNewVar( "~1", HB_COMP_PARAM );
   pSet->value.asSetGet.pExpr = pExpr;
   /* create a codeblock */
   return hb_compExprAddCodeblockExpr( hb_compExprCBVarAdd(
                                          hb_compExprNewCodeBlock( NULL, 0, 0, HB_COMP_PARAM ),
                                          "~1", ' ', HB_COMP_PARAM ), pSet );
}
