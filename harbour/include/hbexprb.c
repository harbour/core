/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Expression Optimizer
 *
 * Copyright 1999 Ryszard Glab
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

/* TOFIX: Split the code, since MSC8 can't compile it, even in Huge model. */

/* TODO:
 *    - Correct post- and pre- operations to correctly handle the following code
 *    a[ i++ ]++
 *    Notice: in current implementation (an in Clipper too) 'i++' is evaluated
 *    two times! This causes that the new value (after incrementation) is
 *    stored in next element of the array.
 */

#include <math.h>
#include "hbcomp.h"

/* memory allocation
 */
#define  HB_XGRAB( size )  hb_xgrab( (size) )
#define  HB_XFREE( pPtr )  hb_xfree( (void *)(pPtr) )

/* Forward declarations
 */

#if defined( HB_MACRO_SUPPORT )
   void hb_compExprDelOperator( HB_EXPR_PTR, HB_MACRO_DECL );
   void hb_compExprUseOperEq( HB_EXPR_PTR, BYTE, HB_MACRO_DECL );
   void hb_compExprPushPreOp( HB_EXPR_PTR, BYTE, HB_MACRO_DECL );
   void hb_compExprPushPostOp( HB_EXPR_PTR, BYTE, HB_MACRO_DECL );
   void hb_compExprUsePreOp( HB_EXPR_PTR, BYTE, HB_MACRO_DECL );
   void hb_compExprUseAliasMacro( HB_EXPR_PTR, BYTE, HB_MACRO_DECL );
   void hb_compExprPushOperEq( HB_EXPR_PTR pSelf, BYTE bOpEq, HB_MACRO_DECL );
   ULONG hb_compExprReduceList( HB_EXPR_PTR, HB_MACRO_DECL );
#else
   void hb_compExprDelOperator( HB_EXPR_PTR );
   void hb_compExprUseOperEq( HB_EXPR_PTR, BYTE );
   void hb_compExprPushPreOp( HB_EXPR_PTR, BYTE );
   void hb_compExprPushPostOp( HB_EXPR_PTR, BYTE );
   void hb_compExprUsePreOp( HB_EXPR_PTR, BYTE );
   void hb_compExprUseAliasMacro( HB_EXPR_PTR, BYTE );
   void hb_compExprPushOperEq( HB_EXPR_PTR pSelf, BYTE bOpEq );
   ULONG hb_compExprReduceList( HB_EXPR_PTR );
#endif

HB_EXPR_PTR hb_compExprReduceMod( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReduceDiv( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReduceMult( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReduceMinus( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReducePlus( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReduceIN( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReduceNE( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReduceGE( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReduceLE( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReduceGT( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReduceLT( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReduceEQ( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReduceAnd( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReduceOr( HB_EXPR_PTR pSelf, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprReduceIIF( HB_EXPR_PTR, HB_MACRO_DECL );


/* forward declaration of callback functions
 */
static HB_EXPR_FUNC( hb_compExprUseDummy );
static HB_EXPR_FUNC( hb_compExprUseNil );
static HB_EXPR_FUNC( hb_compExprUseNumeric );
static HB_EXPR_FUNC( hb_compExprUseString );
static HB_EXPR_FUNC( hb_compExprUseCodeblock );
static HB_EXPR_FUNC( hb_compExprUseLogical );
static HB_EXPR_FUNC( hb_compExprUseSelf );
static HB_EXPR_FUNC( hb_compExprUseArray );
static HB_EXPR_FUNC( hb_compExprUseVarRef );
static HB_EXPR_FUNC( hb_compExprUseFunRef );
static HB_EXPR_FUNC( hb_compExprUseIIF );
static HB_EXPR_FUNC( hb_compExprUseList );
static HB_EXPR_FUNC( hb_compExprUseArgList );
static HB_EXPR_FUNC( hb_compExprUseArrayAt );
static HB_EXPR_FUNC( hb_compExprUseMacro );
static HB_EXPR_FUNC( hb_compExprUseFunCall );
static HB_EXPR_FUNC( hb_compExprUseAliasVar );
static HB_EXPR_FUNC( hb_compExprUseAliasExpr );
static HB_EXPR_FUNC( hb_compExprUseSend );
static HB_EXPR_FUNC( hb_compExprUseFunName );
static HB_EXPR_FUNC( hb_compExprUseAlias );
static HB_EXPR_FUNC( hb_compExprUseRTVariable );
static HB_EXPR_FUNC( hb_compExprUseVariable );
static HB_EXPR_FUNC( hb_compExprUseAssign );
static HB_EXPR_FUNC( hb_compExprUseEqual );
static HB_EXPR_FUNC( hb_compExprUsePlus );
static HB_EXPR_FUNC( hb_compExprUseMinus );
static HB_EXPR_FUNC( hb_compExprUseMult );
static HB_EXPR_FUNC( hb_compExprUseDiv );
static HB_EXPR_FUNC( hb_compExprUseMod );
static HB_EXPR_FUNC( hb_compExprUsePower );
static HB_EXPR_FUNC( hb_compExprUsePostInc );
static HB_EXPR_FUNC( hb_compExprUsePostDec );
static HB_EXPR_FUNC( hb_compExprUsePreInc );
static HB_EXPR_FUNC( hb_compExprUsePreDec );
static HB_EXPR_FUNC( hb_compExprUsePlusEq );
static HB_EXPR_FUNC( hb_compExprUseMinusEq );
static HB_EXPR_FUNC( hb_compExprUseMultEq );
static HB_EXPR_FUNC( hb_compExprUseDivEq );
static HB_EXPR_FUNC( hb_compExprUseModEq );
static HB_EXPR_FUNC( hb_compExprUseExpEq );
static HB_EXPR_FUNC( hb_compExprUseAnd );
static HB_EXPR_FUNC( hb_compExprUseOr );
static HB_EXPR_FUNC( hb_compExprUseNot );
static HB_EXPR_FUNC( hb_compExprUseEQ );
static HB_EXPR_FUNC( hb_compExprUseLT );
static HB_EXPR_FUNC( hb_compExprUseGT );
static HB_EXPR_FUNC( hb_compExprUseLE );
static HB_EXPR_FUNC( hb_compExprUseGE );
static HB_EXPR_FUNC( hb_compExprUseNE );
static HB_EXPR_FUNC( hb_compExprUseIN );
static HB_EXPR_FUNC( hb_compExprUseNegate );

HB_EXPR_FUNC_PTR hb_comp_ExprTable[] = {
   hb_compExprUseDummy,
   hb_compExprUseNil,
   hb_compExprUseNumeric,
   hb_compExprUseString,
   hb_compExprUseCodeblock,
   hb_compExprUseLogical,
   hb_compExprUseSelf,
   hb_compExprUseArray,
   hb_compExprUseVarRef,
   hb_compExprUseFunRef,
   hb_compExprUseIIF,
   hb_compExprUseList,
   hb_compExprUseArgList,
   hb_compExprUseArrayAt,
   hb_compExprUseMacro,
   hb_compExprUseFunCall,
   hb_compExprUseAliasVar,
   hb_compExprUseAliasExpr,
   hb_compExprUseSend,
   hb_compExprUseFunName,
   hb_compExprUseAlias,
   hb_compExprUseRTVariable,
   hb_compExprUseVariable,
   hb_compExprUsePostInc,      /* post-operators -> lowest precedence */
   hb_compExprUsePostDec,
   hb_compExprUseAssign,       /* assigments */
   hb_compExprUsePlusEq,
   hb_compExprUseMinusEq,
   hb_compExprUseMultEq,
   hb_compExprUseDivEq,
   hb_compExprUseModEq,
   hb_compExprUseExpEq,
   hb_compExprUseOr,           /* logical operators */
   hb_compExprUseAnd,
   hb_compExprUseNot,
   hb_compExprUseEqual,        /* relational operators */
   hb_compExprUseEQ,
   hb_compExprUseLT,
   hb_compExprUseGT,
   hb_compExprUseLE,
   hb_compExprUseGE,
   hb_compExprUseNE,
   hb_compExprUseIN,
   hb_compExprUsePlus,      /* addition */
   hb_compExprUseMinus,
   hb_compExprUseMult,      /* multiple */
   hb_compExprUseDiv,
   hb_compExprUseMod,
   hb_compExprUsePower,
   hb_compExprUseNegate,    /* sign operator */
   hb_compExprUsePreInc,
   hb_compExprUsePreDec     /* highest precedence */
};

/* ************************************************************************* */

static HB_EXPR_FUNC( hb_compExprUseDummy )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         break;
      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
      case HB_EA_ARRAY_INDEX:
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_PUSHNIL );
         break;
      case HB_EA_POP_PCODE:
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
      case HB_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for HB_ET_NIL expression
 */
static HB_EXPR_FUNC( hb_compExprUseNil )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         break;
      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( pSelf );     /* NIL cannot be used as index element */
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_PUSHNIL );
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( pSelf );
      case HB_EA_DELETE:
         break;
   }

   return pSelf;
}

/* actions for HB_ET_NUMERIC expression
 */
static HB_EXPR_FUNC( hb_compExprUseNumeric )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         break;
      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         if( pSelf->value.asNum.NumType == HB_ET_DOUBLE )
            HB_EXPR_PCODE3( hb_compGenPushDouble, pSelf->value.asNum.dVal, pSelf->value.asNum.bWidth, pSelf->value.asNum.bDec );
         else
            HB_EXPR_PCODE1( hb_compGenPushLong, pSelf->value.asNum.lVal );
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( pSelf );
      case HB_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for HB_ET_STRING expression
 */
static HB_EXPR_FUNC( hb_compExprUseString )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         break;
      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( pSelf );     /* string cannot be used as index element */
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_PCODE2( hb_compGenPushString, pSelf->value.asString.string, pSelf->ulLength );
#if !defined( HB_MACRO_SUPPORT )
                /* only memvar variables are allowed in macro compilation - there is no
                 * need to check for locals or static variables
                 */
         if( hb_compExprCheckMacroVar( pSelf->value.asString.string ) )
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_MACROTEXT );
#endif
         }
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( pSelf );
         break;
      case HB_EA_DELETE:
         if( pSelf->value.asString.dealloc )
            HB_XFREE( pSelf->value.asString.string );
         break;
   }
   return pSelf;
}

/* actions for HB_ET_CODEBLOCK expression
 */
static HB_EXPR_FUNC( hb_compExprUseCodeblock )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         break;
      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( pSelf );     /* codeblock cannot be used as index element */
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_PTR pExpr, pNext;
            HB_EXPR_PTR * pPrev;

            HB_EXPR_PCODE0( hb_compCodeBlockStart );
            /* Define requested local variables
             */
#if defined( HB_MACRO_SUPPORT )
            HB_PCODE_DATA->pLocals = ( HB_CBVAR_PTR ) pSelf->value.asList.pIndex;
#else
            {
               HB_CBVAR_PTR pVar;

               pVar = ( HB_CBVAR_PTR ) pSelf->value.asList.pIndex;
               while( pVar )
               {
                        hb_compVariableAdd( pVar->szName, pVar->bType );
                        pVar =pVar->pNext;
               }
            }
#endif
            pExpr = pSelf->value.asList.pExprList;
            pPrev = &pSelf->value.asList.pExprList;
            while( pExpr )
            {
               /* store next expression in case the current  will be reduced
                * NOTE: During reduction the expression can be replaced by the
                *    new one - this will break the linked list of expressions.
                */
               pNext = pExpr->pNext; /* store next expression in case the current  will be reduced */
               pExpr = HB_EXPR_USE( pExpr, HB_EA_REDUCE );
               /* Generate push/pop pcodes for all expresions except the last one
                * The value of the last expression is used as a return value
                * of a codeblock evaluation
                */
               /* NOTE: This will genereate warnings if constant value is
                * used as an expression - some operators will generate it too
                * e.g.
                * EVAL( {|| 3+5, func()} )
                */
               *pPrev = pExpr;   /* store a new expression into the previous one */
               pExpr->pNext = pNext;  /* restore the link to next expression */
               if( pNext )
                  HB_EXPR_USE( pExpr, HB_EA_PUSH_POP );
               else
                  HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
               pPrev  = &pExpr->pNext;
               pExpr  = pNext;
            }
            HB_EXPR_PCODE0( hb_compCodeBlockEnd );
         }
         break;
      case HB_EA_POP_PCODE:
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( pSelf );
         break;
      case HB_EA_DELETE:
         hb_compExprCBVarDel( ( HB_CBVAR_PTR ) pSelf->value.asList.pIndex );
         HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asList.pExprList );
         break;
   }
   return pSelf;
}

/* actions for HB_ET_LOGICAL expression
 */
static HB_EXPR_FUNC( hb_compExprUseLogical )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         break;
      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( pSelf );     /* logical cannot be used as array index element */
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_EXPR_PCODE1( hb_compGenPushLogical, pSelf->value.asLogical );
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( pSelf );
      case HB_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for HB_ET_SELF expression
 */
static HB_EXPR_FUNC( hb_compExprUseSelf )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         break;
      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );   /* QUESTION: Is this OK ? */
         break;
      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( pSelf );     /* SELF cannot be used as array index element */
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_PUSHSELF );
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( pSelf );
      case HB_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for a literal array { , , , ... }
 */
static HB_EXPR_FUNC( hb_compExprUseArray )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         HB_EXPR_PCODE1( hb_compExprReduceList, pSelf );
         break;

      case HB_EA_ARRAY_AT:
         break;

      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( pSelf );     /* array cannot be used as index element */
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
      {
         HB_EXPR_PTR pElem = pSelf->value.asList.pExprList;
         /* Push all elements of the array
         */
         if( ( pElem == NULL ) || ( pElem->ExprType == HB_ET_NONE && pElem->pNext == NULL ) )
            HB_EXPR_GENPCODE3( hb_compGenPCode3, HB_P_ARRAYGEN, 0, 0, ( BOOL ) 1 );
         else
         {
            while( pElem )
            {
               HB_EXPR_USE( pElem, HB_EA_PUSH_PCODE );
               pElem = pElem->pNext;
            }
            HB_EXPR_GENPCODE3( hb_compGenPCode3, HB_P_ARRAYGEN, HB_LOBYTE( pSelf->ulLength ), HB_HIBYTE( pSelf->ulLength ), ( BOOL ) 1 );
         }
      }
      break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      {
         HB_EXPR_PTR pElem = pSelf->value.asList.pExprList;
         /* Push non-constant values only
          */
         while( pElem )
         {
            HB_EXPR_USE( pElem, HB_EA_PUSH_POP );
            pElem = pElem->pNext;
         }
      }
      break;

      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( pSelf );
         break;

      case HB_EA_DELETE:
      {
         HB_EXPR_PTR pElem = pSelf->value.asList.pExprList;
         /* Delete all elements of the array
         */
         HB_EXPR_PTR pNext;
         while( pElem )
         {
            pNext = pElem->pNext;
            HB_EXPR_PCODE1( hb_compExprDelete, pElem );
            pElem = pNext;
         }
      }
      break;

   }

   return pSelf;
}

/* actions for HB_ET_VARREF expression
 */
static HB_EXPR_FUNC( hb_compExprUseVarRef )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         break;
      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_EXPR_PCODE1( hb_compGenPushVarRef, pSelf->value.asSymbol );
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( pSelf );
      case HB_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for HB_ET_FUNREF expression
 */
static HB_EXPR_FUNC( hb_compExprUseFunRef )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         break;
      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_EXPR_PCODE1( hb_compGenPushFunCall, pSelf->value.asSymbol );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_FUNCPTR );
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( pSelf );
      case HB_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for HB_ET_IIF expression
 */
static HB_EXPR_FUNC( hb_compExprUseIIF )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         HB_EXPR_PCODE1( hb_compExprReduceList, pSelf );
         pSelf = hb_compExprReduceIIF( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         {
            /* this is called if all three parts of IIF expression should be generated
            */
            LONG lPosFalse, lPosEnd;
            HB_EXPR_PTR pExpr = pSelf->value.asList.pExprList;

            HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
            lPosFalse = HB_EXPR_PCODE1( hb_compGenJumpFalse, 0 );
            pExpr =pExpr->pNext;

            HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
            lPosEnd = HB_EXPR_PCODE1( hb_compGenJump, 0 );
            pExpr =pExpr->pNext;

            HB_EXPR_PCODE1( hb_compGenJumpHere, lPosFalse );
            HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
            HB_EXPR_PCODE1( hb_compGenJumpHere, lPosEnd );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );  /* remove a value if used in statement */
         }
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asList.pExprList )
         {
            HB_EXPR_PTR pTmp, pExpr = pSelf->value.asList.pExprList;
            while( pExpr )
            {
               pTmp = pExpr->pNext;    /* store next expression */
               HB_EXPR_PCODE1( hb_compExprDelete, pExpr );
               pExpr =pTmp;
            }
            pSelf->value.asList.pExprList = NULL;
         }
         break;
   }
   return pSelf;  /* return self */
}

/* NOTE: In PUSH operation it leaves on the eval stack the last expression only
 */
static HB_EXPR_FUNC( hb_compExprUseList )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PCODE1( hb_compExprReduceList, pSelf );
            /* NOTE: if the list contains a single expression then the list
             * is not reduced to this expression - if you need that reduction
             * then call hb_compExprListStrip() additionaly
             */
         }
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         if( hb_compExprListLen( pSelf ) == 1 )
         {
            /* For example:
             * ( a ) := 4
             */
            hb_compErrorLValue( pSelf->value.asList.pExprList );
         }
         else
            hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_PTR pExpr = pSelf->value.asList.pExprList;

            if( pExpr->ExprType == HB_ET_NONE && pExpr->pNext == NULL )
            {
               /* Empty list was used ()
                */
               hb_compErrorSyntax( pExpr );
            }
            else
            {
               while( pExpr )
               {
                  if( pExpr->pNext )
                     HB_EXPR_USE( pExpr, HB_EA_PUSH_POP );
                  else
                     HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );   /* the last expression */
                  pExpr = pExpr->pNext;
               }
            }
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         {
            HB_EXPR_PTR pExpr = pSelf->value.asList.pExprList;

            while( pExpr )
            {
               HB_EXPR_USE( pExpr, HB_EA_PUSH_POP );
               pExpr = pExpr->pNext;
            }
         }
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asList.pExprList )
         {
            HB_EXPR_PTR pTmp, pExpr = pSelf->value.asList.pExprList;
            while( pExpr )
            {
               pTmp = pExpr->pNext;    /* store next expression */
               HB_EXPR_PCODE1( hb_compExprDelete, pExpr );
               pExpr =pTmp;
            }
            pSelf->value.asList.pExprList = NULL;
         }
         break;
   }
   return pSelf;
}

/* NOTE: In PUSH operation it leaves all expressions on the eval stack
 */
static HB_EXPR_FUNC( hb_compExprUseArgList )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         HB_EXPR_PCODE1( hb_compExprReduceList, pSelf );
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;

      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_PTR pExpr = pSelf->value.asList.pExprList;

            while( pExpr )
            {
               HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
               pExpr = pExpr->pNext;
            }
         }
         break;

      case HB_EA_POP_PCODE:
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asList.pExprList )
         {
            HB_EXPR_PTR pTmp, pExpr = pSelf->value.asList.pExprList;
            while( pExpr )
            {
               pTmp = pExpr->pNext;    /* store next expression */
               HB_EXPR_PCODE1( hb_compExprDelete, pExpr );
               pExpr =pTmp;
            }
            pSelf->value.asList.pExprList = NULL;
         }
         break;
   }
   return pSelf;
}

/* handler for ( ( array[ idx ] )[ idx ] )[ idx ]
 */
static HB_EXPR_FUNC( hb_compExprUseArrayAt )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pIdx;

            pSelf->value.asList.pExprList = HB_EXPR_USE( pSelf->value.asList.pExprList, HB_EA_REDUCE );
            pSelf->value.asList.pIndex = HB_EXPR_USE( pSelf->value.asList.pIndex, HB_EA_REDUCE );
            pIdx = pSelf->value.asList.pIndex;
            if( pIdx->ExprType == HB_ET_NUMERIC )
            {
               HB_EXPR_PTR pExpr = pSelf->value.asList.pExprList; /* the expression that holds an array */

               if( pExpr->ExprType == HB_ET_ARRAY )   /* is it a literal array */
               {
                  LONG lIndex;

                  pExpr = pExpr->value.asList.pExprList; /* the first element in the array */
                  if( pIdx->value.asNum.NumType == HB_ET_LONG )
                     lIndex = pIdx->value.asNum.lVal;
                  else
                     lIndex = ( LONG ) pIdx->value.asNum.dVal;

                  if( lIndex > 0 )
                  {
                     while( --lIndex && pExpr )
                        pExpr = pExpr->pNext;
                  }
                  else
                     pExpr = NULL;  /* index is <= 0 - generate bound error */

                  if( pExpr ) /* found ? */
                  {
                     /* extract a single expression from the array
                      */
                     HB_EXPR_PTR pNew = hb_compExprNew( HB_ET_NONE );
                     memcpy( pNew, pExpr, sizeof(  HB_EXPR ) );
                     /* This will suppres releasing of memory occupied by components of
                     * the expression - we have just copied them into the new expression.
                     * This method is simpler then traversing the list and releasing all
                     * but this choosen one.
                     */
                     pExpr->ExprType = HB_ET_NONE;
                     /* Here comes the magic */
                     HB_EXPR_PCODE1( hb_compExprDelete, pSelf );
                     pSelf = pNew;
                  }
                  else
                  {
                     hb_compErrorBound( pIdx );
                  }
               }
               else
               {
                  LONG lIndex;

                  if( pIdx->value.asNum.NumType == HB_ET_LONG )
                     lIndex = pIdx->value.asNum.lVal;
                  else
                     lIndex = ( LONG ) pIdx->value.asNum.dVal;

                  if( lIndex > 0 )
                     HB_EXPR_USE( pExpr, HB_EA_ARRAY_AT );
                  else
                     hb_compErrorBound( pIdx ); /* index <= 0 - bound error */
               }
            }
         }
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asList.pExprList, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asList.pIndex, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_ARRAYPUSH );
         }
         break;

      case HB_EA_POP_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asList.pExprList, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asList.pIndex, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_ARRAYPOP );
         }
         break;

      case HB_EA_PUSH_POP:
         {
            /* NOTE: This is highly optimized code - this will work even
             * if accessed value isn't an array. It will work also if
             * the index is invalid
             */
            HB_EXPR_USE( pSelf->value.asList.pExprList, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asList.pIndex, HB_EA_PUSH_POP );
         }
         /* no break */
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( pSelf );
         break;
      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asList.pExprList );
         HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asList.pIndex );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMacro )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;
      case HB_EA_PUSH_PCODE:
         {
            if( pSelf->value.asMacro.pExprList )
            {
               /* macro expression: &( expressions_list )
                * NOTE: only the last expression will be macro-compiled
                */
               HB_EXPR_USE( pSelf->value.asMacro.pExprList, HB_EA_PUSH_PCODE );
            }
            else
            {
               if( pSelf->value.asMacro.cMacroOp )
               {
                  /* simple macro variable expansion: &variable
                   * 'szMacro' is a variable name
                   */
                  HB_EXPR_PCODE1( hb_compGenPushVar, pSelf->value.asMacro.szMacro );
               }
               else
               {
                  /* complex macro expression: prefix&var.suffix
                   * all components should be placed as a string that will
                   * be compiled after text susbstitution
                   */
                  HB_EXPR_PCODE2( hb_compGenPushString, pSelf->value.asMacro.szMacro, strlen(pSelf->value.asMacro.szMacro) );
               }
            }
            /* compile & run - leave a result on the eval stack
             */
            if( pSelf->value.asMacro.SubType == HB_ET_MACRO_SYMBOL )
               HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_MACROSYMBOL );
            else if( pSelf->value.asMacro.SubType != HB_ET_MACRO_ALIASED )
               HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_MACROPUSH );
            /* NOTE: pcode for alias context is generated in
             * hb_compExprUseAliasVar()
             */
         }
         break;

      case HB_EA_POP_PCODE:
         {
            if( pSelf->value.asMacro.pExprList )
            {
               /* macro expression: &( expressions_list )
                * NOTE: only the last expression will be macro-compiled
                */
               HB_EXPR_USE( pSelf->value.asMacro.pExprList, HB_EA_PUSH_PCODE );
            }
            else
            {
               if( pSelf->value.asMacro.cMacroOp )
               {
                  /* simple macro variable expansion: &variable
                   * 'szMacro' is a variable name
                   */
                  HB_EXPR_PCODE1( hb_compGenPushVar, pSelf->value.asMacro.szMacro );
               }
               else
               {
                  /* complex macro expression: prefix&var.suffix
                   * all components should be placed as a string that will
                   * be compiled after text susbstitution
                   */
                  HB_EXPR_PCODE2( hb_compGenPushString, pSelf->value.asMacro.szMacro, strlen(pSelf->value.asMacro.szMacro) );
               }
            }
            /* compile & run - macro compiler will generate pcode to pop a value
             * from the eval stack
             */
            if( pSelf->value.asMacro.SubType != HB_ET_MACRO_ALIASED )
               HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_MACROPOP );
         }
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asMacro.pExprList )
            HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asMacro.pExprList );
/* NOTE: This will be released during releasing of symbols' table
 *
 *        if( pSelf->value.asMacro.szMacro );
 *           HB_XFREE( pSelf->value.asMacro.szMacro );
 */
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseFunCall )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            /* Reduce the expressions on the list of arguments
             */
            if( pSelf->value.asFunCall.pParms )
               pSelf->value.asFunCall.pParms = HB_EXPR_USE( pSelf->value.asFunCall.pParms, HB_EA_REDUCE );
         }
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         {
            USHORT usCount;

            HB_EXPR_USE( pSelf->value.asFunCall.pFunName, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_PUSHNIL );

            if( pSelf->value.asFunCall.pParms )
            {
               /* NOTE: pParms will be NULL in 'DO procname' (if there is
                * no WITH keyword)
                */
               usCount = ( USHORT ) hb_compExprListLen( pSelf->value.asFunCall.pParms );
               if( usCount == 1 && pSelf->value.asFunCall.pParms->value.asList.pExprList->ExprType == HB_ET_NONE )
                  --usCount;
               if( usCount )
                  HB_EXPR_USE( pSelf->value.asFunCall.pParms, HB_EA_PUSH_PCODE );
            }
            else
               usCount = 0;

            if( usCount > 255 )
               HB_EXPR_GENPCODE3( hb_compGenPCode3, HB_P_FUNCTION, HB_LOBYTE( usCount ), HB_HIBYTE( usCount ), ( BOOL ) 1 );
            else
               HB_EXPR_GENPCODE2( hb_compGenPCode2, HB_P_FUNCTIONSHORT, ( BYTE ) usCount, ( BOOL ) 1 );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         {
            USHORT usCount;

            HB_EXPR_USE( pSelf->value.asFunCall.pFunName, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_PUSHNIL );

            if( pSelf->value.asFunCall.pParms )
            {
               usCount = ( USHORT ) hb_compExprListLen( pSelf->value.asFunCall.pParms );
               if( usCount == 1 && pSelf->value.asFunCall.pParms->value.asList.pExprList->ExprType == HB_ET_NONE )
                  --usCount;
               if( usCount )
                  HB_EXPR_USE( pSelf->value.asFunCall.pParms, HB_EA_PUSH_PCODE );
            }
            else
               usCount = 0;

            if( usCount > 255 )
               HB_EXPR_GENPCODE3( hb_compGenPCode3, HB_P_DO, HB_LOBYTE( usCount ), HB_HIBYTE( usCount ), ( BOOL ) 1 );
            else
               HB_EXPR_GENPCODE2( hb_compGenPCode2, HB_P_DOSHORT, ( BYTE ) usCount, ( BOOL ) 1 );
         }
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asFunCall.pParms )
            HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asFunCall.pParms );
         HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asFunCall.pFunName );
         break;
   }
   return pSelf;
}

/* handler for expression->identifier syntax
 */
static HB_EXPR_FUNC( hb_compExprUseAliasVar )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;

      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_PTR pAlias = pSelf->value.asAlias.pAlias;
            BOOL bReduced = FALSE;

            if( pAlias->ExprType == HB_ET_LIST )
            {
               /* ( expr1, expr2, ... )->variable
                */
               pSelf->value.asAlias.pAlias = HB_EXPR_USE( pSelf->value.asAlias.pAlias, HB_EA_REDUCE );
               bReduced = TRUE;
            }

            if( pAlias->ExprType == HB_ET_MACRO || pSelf->value.asAlias.pVar->ExprType == HB_ET_MACRO )
            {
               /* Macro operator is used on the left or right side of an alias
                * operator - handle it with a special care
                */
               HB_EXPR_PCODE2( hb_compExprUseAliasMacro, pSelf, HB_EA_PUSH_PCODE );
            }
            else if( pAlias->ExprType == HB_ET_ALIAS )
            {
               /*
                * myalias->var
                * FIELD->var
                * MEMVAR->var
                *
                * NOTE: TRUE = push also alias
                */
                HB_EXPR_PCODE4( hb_compGenPushAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol, TRUE, pAlias->value.asSymbol, 0 );
            }
            else if( pAlias->ExprType == HB_ET_NUMERIC )
            {
               /* numeric alias
                * 2->var
                *
                * NOTE: only integer (long) values are allowed
                */
               if( pAlias->value.asNum.NumType == HB_ET_LONG )
                  HB_EXPR_PCODE4( hb_compGenPushAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol, TRUE, NULL, pAlias->value.asNum.lVal );
               else
                  hb_compErrorAlias( pAlias );
            }
            else if( bReduced )
            {
               /*
                * ( expression )->var
                *
                * NOTE: FALSE = don't push alias value
                */
               HB_EXPR_USE( pAlias, HB_EA_PUSH_PCODE );
               HB_EXPR_PCODE4( hb_compGenPushAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol, FALSE, NULL, 0 );
            }
            else
               hb_compErrorAlias( pAlias );
         }
         break;

      case HB_EA_POP_PCODE:
         {
            HB_EXPR_PTR pAlias = pSelf->value.asAlias.pAlias;
            BOOL bReduced = FALSE;

            if( pAlias->ExprType == HB_ET_LIST )
            {
               pSelf->value.asAlias.pAlias = HB_EXPR_USE( pSelf->value.asAlias.pAlias, HB_EA_REDUCE );
               bReduced = TRUE;
            }

            if( pAlias->ExprType == HB_ET_MACRO || pSelf->value.asAlias.pVar->ExprType == HB_ET_MACRO )
            {
               /* Macro operator is used on the left or right side of an alias
                * operator - handle it with a special care
                * (we need convert to a string the whole expression)
                */
               HB_EXPR_PCODE2( hb_compExprUseAliasMacro, pSelf, HB_EA_POP_PCODE );
            }
            else if( pAlias->ExprType == HB_ET_ALIAS )
            {
               /*
                * myalias->var
                * FIELD->var
                * MEMVAR->var
                */
               HB_EXPR_PCODE4( hb_compGenPopAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol, TRUE, pAlias->value.asSymbol, 0 );
            }
            else if( pAlias->ExprType == HB_ET_NUMERIC )
            {
               /* numeric alias
                * 2->var
                *
                * NOTE: only integer (long) values are allowed
                */
               if( pAlias->value.asNum.NumType == HB_ET_LONG )
                  HB_EXPR_PCODE4( hb_compGenPopAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol, TRUE, NULL, pAlias->value.asNum.lVal );
               else
                  hb_compErrorAlias( pAlias );
            }
            else if( bReduced )
            {
               /*
                * ( expression )->var
                *
                * NOTE: FALSE = don't push alias value
                */
               if( pAlias->ExprType == HB_ET_NONE )
               {
                  /* empty expression -> ()->var
                  */
                  hb_compErrorAlias( pAlias );
               }
               else
               {
                  HB_EXPR_USE( pAlias, HB_EA_PUSH_PCODE );
                  HB_EXPR_PCODE4( hb_compGenPopAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol, FALSE, NULL, 0 );
               }
            }
            else
               hb_compErrorAlias( pAlias );
         }
         break;


      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asAlias.pAlias );
          /* NOTE: variable name is released only during macro compilation */
#if defined( HB_MACRO_SUPPORT )
         if( pSelf->value.asAlias.pVar )
            HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asAlias.pVar );
#endif
         break;
   }
   return pSelf;
}

/* handler for expression->( exression, ... ) syntax
 */
static HB_EXPR_FUNC( hb_compExprUseAliasExpr )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            /* save currently selected workarea
             */
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_PUSHALIAS );
            /* push the expression that will return a new workarea
             */
            HB_EXPR_USE( pSelf->value.asAlias.pAlias, HB_EA_PUSH_PCODE );
            /* pop the value from the stack and select it as current workarea
             */
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POPALIAS );
            /* evaluate any expression
             */
            HB_EXPR_USE( pSelf->value.asAlias.pExpList, HB_EA_PUSH_PCODE );
            /* swap the two last items on the eval stack: one item is a
             * value returned by evaluated expression and the second item
             * is previously selected workarea. After swaping select again
             * the restored workarea.
             */
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_SWAPALIAS );
         }
         break;

      case HB_EA_POP_PCODE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         {
            /* save currently selected workarea
             */
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_PUSHALIAS );
            /* push the expression that will return a new workarea
             */
            HB_EXPR_USE( pSelf->value.asAlias.pAlias, HB_EA_PUSH_PCODE );
            /* pop the value from the stack and select it as current workarea
             */
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POPALIAS );
            /* evaluate any expression - it will not leave any return
             * value on the eval stack
             */
            HB_EXPR_USE( pSelf->value.asAlias.pExpList, HB_EA_PUSH_POP );
            /* Pop and select again the restored workarea.
             */
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POPALIAS );
         }
         break;
      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asAlias.pAlias );
         HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asAlias.pExpList );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseAlias )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;
      case HB_EA_PUSH_PCODE:
         HB_EXPR_PCODE2( hb_compGenPushSymbol, pSelf->value.asSymbol, 0 );
         break;
      case HB_EA_POP_PCODE:
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         break;
      case HB_EA_DELETE:
#if defined( HB_MACRO_SUPPORT )
             HB_XFREE( pSelf->value.asSymbol );
#endif
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseFunName )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;
      case HB_EA_PUSH_PCODE:
         HB_EXPR_PCODE1( hb_compGenPushFunCall, pSelf->value.asSymbol );
         break;
      case HB_EA_POP_PCODE:
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
      case HB_EA_DELETE:
#if defined( HB_MACRO_SUPPORT )
             HB_XFREE( pSelf->value.asSymbol );
#endif
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseRTVariable )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;
      case HB_EA_PUSH_PCODE:
         if( pSelf->value.asRTVar.szName )
            HB_EXPR_PCODE2( hb_compGenPushSymbol, pSelf->value.asRTVar.szName, 0 );  /* this is not a functio */
         else
            HB_EXPR_USE( pSelf->value.asRTVar.pMacro, HB_EA_PUSH_PCODE );
         break;
      case HB_EA_POP_PCODE:
         if( pSelf->value.asRTVar.szName )
            HB_EXPR_PCODE1( hb_compGenPopVar, pSelf->value.asRTVar.szName );
         else
            HB_EXPR_USE( pSelf->value.asRTVar.pMacro, HB_EA_POP_PCODE );
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
      case HB_EA_DELETE:
         break;
   }
   return pSelf;
}


static HB_EXPR_FUNC( hb_compExprUseVariable )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;
      case HB_EA_PUSH_PCODE:
#if defined( HB_MACRO_SUPPORT )
         {
            /* NOTE: When the following syntax is used:
            *    ( any_expr )->&var2
            * then macro compiler is compiling the right side of alias
            * operator only (if 'any_expr' is not a string) - an alias value
            * is placed on the eval stack before macro compilation.
            * The HB_MACRO_GEN_ALIASED flag is used to signal that we have to
            * genearate alias aware pcode even if we known a variable part only.
            */
            if( HB_MACRO_DATA->Flags & HB_MACRO_GEN_ALIASED )
               HB_EXPR_PCODE4( hb_compGenPushAliasedVar, pSelf->value.asSymbol, FALSE, NULL, 0 );
            else
               HB_EXPR_PCODE1( hb_compGenPushVar, pSelf->value.asSymbol );
         }
#else
         HB_EXPR_PCODE1( hb_compGenPushVar, pSelf->value.asSymbol );
#endif
          break;

       case HB_EA_POP_PCODE:
#if defined( HB_MACRO_SUPPORT )
         {
            if( HB_MACRO_DATA->Flags & HB_MACRO_GEN_ALIASED )
               HB_EXPR_PCODE4( hb_compGenPopAliasedVar, pSelf->value.asSymbol, FALSE, NULL, 0 );
            else
               HB_EXPR_PCODE1( hb_compGenPopVar, pSelf->value.asSymbol );
         }
#else
         HB_EXPR_PCODE1( hb_compGenPopVar, pSelf->value.asSymbol );
#endif
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_PCODE1( hb_compGenPushVar, pSelf->value.asSymbol );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
         break;

      case HB_EA_DELETE:
         /* NOTE: variable name should be released if macro compilation */
#if defined( HB_MACRO_SUPPORT )
             HB_XFREE( pSelf->value.asSymbol );
#endif
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseSend )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            pSelf->value.asMessage.pObject = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asMessage.pObject, HB_EA_REDUCE ), HB_MACRO_PARAM );
            if( pSelf->value.asMessage.pParms )  /* Is it a method call ? */
               pSelf->value.asMessage.pParms = HB_EXPR_USE( pSelf->value.asMessage.pParms, HB_EA_REDUCE );
         }
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;

      case HB_EA_PUSH_PCODE:
         {
            if( pSelf->value.asMessage.pParms )  /* Is it a method call ? */
            {
               int iParms = hb_compExprListLen( pSelf->value.asMessage.pParms );
               HB_EXPR_USE( pSelf->value.asMessage.pObject, HB_EA_PUSH_PCODE );
               HB_EXPR_PCODE1( hb_compGenMessage, pSelf->value.asMessage.szMessage );
               /* NOTE: if method with no parameters is called then the list
                * of parameters contain only one expression of type HB_ET_NONE
                * There is no need to push this parameter
                */
               if( iParms == 1 && pSelf->value.asMessage.pParms->value.asList.pExprList->ExprType == HB_ET_NONE )
                  --iParms;
               if( iParms )
                  HB_EXPR_USE( pSelf->value.asMessage.pParms, HB_EA_PUSH_PCODE );

               if( iParms > 255 )
                  HB_EXPR_GENPCODE3( hb_compGenPCode3, HB_P_SEND, HB_LOBYTE( iParms ), HB_HIBYTE( iParms ), ( BOOL ) 1 );
               else
                  HB_EXPR_GENPCODE2( hb_compGenPCode2, HB_P_SENDSHORT, ( BYTE ) iParms, ( BOOL ) 1 );
            }
            else
            {
               /* acces to instance variable */
               HB_EXPR_USE( pSelf->value.asMessage.pObject, HB_EA_PUSH_PCODE );
               HB_EXPR_PCODE1( hb_compGenMessage, pSelf->value.asMessage.szMessage );
               HB_EXPR_GENPCODE2( hb_compGenPCode2, HB_P_SENDSHORT, 0, ( BOOL ) 1 );
            }
         }
         break;

      case HB_EA_POP_PCODE:
         {
            /* NOTE: This is an exception from the rule - this leaves
             *    the return value on the stack
             */
            HB_EXPR_USE( pSelf->value.asMessage.pObject, HB_EA_PUSH_PCODE );
            HB_EXPR_PCODE1( hb_compGenMessageData, pSelf->value.asMessage.szMessage );
            HB_EXPR_USE( pSelf->value.asMessage.pParms, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE2( hb_compGenPCode2, HB_P_SENDSHORT, 1, ( BOOL ) 1 );
         }
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
         if( ! pSelf->value.asMessage.pParms )  /* Is it a method call ? */
         {
            /* instance variable */
            /* QUESTION: This warning can be misleading if nested messages
             * are used, e.g. a:b():c - should we generate it ?
             */
            hb_compWarnMeaningless( pSelf );
         }

      case HB_EA_DELETE:
#if defined( HB_MACRO_SUPPORT )
         {
            HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asMessage.pObject );
            if( pSelf->value.asMessage.pParms )
               HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asMessage.pParms );
            HB_XFREE( pSelf->value.asMessage.szMessage );
         }
#endif
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePostInc )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ), HB_MACRO_PARAM );
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_PCODE2( hb_compExprPushPostOp, pSelf, HB_P_INC );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         /* a++ used standalone as a statement is the same as ++a
          */
         HB_EXPR_PCODE2( hb_compExprUsePreOp, pSelf, HB_P_INC );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePostDec )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ), HB_MACRO_PARAM );
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_PCODE2( hb_compExprPushPostOp, pSelf, HB_P_DEC );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_PCODE2( hb_compExprUsePreOp, pSelf, HB_P_DEC );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseAssign )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ), HB_MACRO_PARAM );
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;

      case HB_EA_PUSH_PCODE:
         {
            /* NOTE: assigment to an object instance variable needs special handling
             */
            if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
            {
               HB_EXPR_PTR pObj = pSelf->value.asOperator.pLeft;
               pObj->value.asMessage.pParms = pSelf->value.asOperator.pRight;
               HB_EXPR_USE( pObj, HB_EA_POP_PCODE );
               pObj->value.asMessage.pParms = NULL; /* to suppress duplicated releasing */
            }
            else
            {
               /* it assigns a value and leaves it on the stack */
               HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
               /* QUESTION: Can  we replace DUPLICATE+POP with a single PUT opcode
               */
               HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_DUPLICATE );
               HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
            }
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         {
            /* NOTE: assigment to an object instance variable needs special handling
             */
            if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
            {
               HB_EXPR_PTR pObj = pSelf->value.asOperator.pLeft;
               pObj->value.asMessage.pParms = pSelf->value.asOperator.pRight;
               HB_EXPR_USE( pObj, HB_EA_POP_PCODE );
               pObj->value.asMessage.pParms = NULL; /* to suppress duplicated releasing */
               /* Remove the return value */
               HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
            }
            else
            {
               /* it assigns a value and removes it from the stack */
               HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
               HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
            }
         }
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePlusEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ), HB_MACRO_PARAM );
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_PCODE2( hb_compExprPushOperEq, pSelf, HB_P_PLUS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_PCODE2( hb_compExprUseOperEq, pSelf, HB_P_PLUS );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMinusEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ), HB_MACRO_PARAM );
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_PCODE2( hb_compExprPushOperEq, pSelf, HB_P_MINUS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_PCODE2( hb_compExprUseOperEq, pSelf, HB_P_MINUS );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMultEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ), HB_MACRO_PARAM );
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_PCODE2( hb_compExprPushOperEq, pSelf, HB_P_MULT );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_PCODE2( hb_compExprUseOperEq, pSelf, HB_P_MULT );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseDivEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ), HB_MACRO_PARAM );
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_PCODE2( hb_compExprPushOperEq, pSelf, HB_P_DIVIDE );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_PCODE2( hb_compExprUseOperEq, pSelf, HB_P_DIVIDE );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseModEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ), HB_MACRO_PARAM );
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_PCODE2( hb_compExprPushOperEq, pSelf, HB_P_MODULUS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_PCODE2( hb_compExprUseOperEq, pSelf, HB_P_MODULUS );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseExpEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ), HB_MACRO_PARAM );
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_PCODE2( hb_compExprPushOperEq, pSelf, HB_P_POWER );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_PCODE2( hb_compExprUseOperEq, pSelf, HB_P_POWER );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseOr )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf = hb_compExprReduceOr( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( pSelf );
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         if( hb_comp_bShortCuts )
         {
            LONG lEndPos;

            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_DUPLICATE );
            lEndPos = HB_EXPR_PCODE1( hb_compGenJumpTrue, 0 );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_OR );
            HB_EXPR_PCODE1( hb_compGenJumpHere, lEndPos );
         }
         else
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_OR );
         }
         break;


      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseAnd )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf = hb_compExprReduceAnd( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( pSelf );
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         if( hb_comp_bShortCuts )
         {
            LONG lEndPos;

            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_DUPLICATE );
            lEndPos = HB_EXPR_PCODE1( hb_compGenJumpFalse, 0 );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_AND );
            HB_EXPR_PCODE1( hb_compGenJumpHere, lEndPos );
         }
         else
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_AND );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseNot )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pExpr;

            pSelf->value.asOperator.pLeft = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ), HB_MACRO_PARAM );
            pExpr = pSelf->value.asOperator.pLeft;

            if( pExpr->ExprType == HB_ET_LOGICAL )
            {
               pExpr->value.asLogical = ! pExpr->value.asLogical;
               pSelf->ExprType = HB_ET_NONE;  /* do not delete operator parameter - we are still using it */
               HB_EXPR_PCODE1( hb_compExprDelete, pSelf );
               pSelf = pExpr;
            }
         }
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( pSelf );
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_NOT );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseEqual )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         }
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            /* '=' used in an expression - compare values
             */
            /* Try to optimize expression - we cannot optimize in HB_EA_REDUCE
             * because it is not decided yet if it is assigment or comparision
             */
            HB_EXPR_PTR pLeft, pRight;

            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == pRight->ExprType )
               switch( pLeft->ExprType )
               {
                  case HB_ET_LOGICAL:
                     HB_EXPR_PCODE1( hb_compGenPushLogical, (pLeft->value.asLogical == pRight->value.asLogical) );
                     break;

                  case HB_ET_STRING:
                     /* NOTE: the result depends on SET EXACT setting then it
                     * cannot be optimized except the case when NULL string are
                     * compared - the result is always TRUE regardless of EXACT
                     * setting
                     */
                     if( (pLeft->ulLength | pRight->ulLength) == 0 )
                        HB_EXPR_PCODE1( hb_compGenPushLogical, TRUE ); /* NOTE: COMPATIBILITY: Clipper doesn't optimize this */
                     else
                     {
                        HB_EXPR_USE( pLeft, HB_EA_PUSH_PCODE );
                        HB_EXPR_USE( pRight, HB_EA_PUSH_PCODE );
                        HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_EQUAL );
                     }
                     break;

                  case HB_ET_NIL:
                     /* NOTE: COMPATIBILITY: Clipper doesn't optimize this */
                     HB_EXPR_PCODE1( hb_compGenPushLogical, TRUE ); /* NIL = NIL is always TRUE */
                     break;

                  case HB_ET_NUMERIC:
                     switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
                     {
                        case HB_ET_LONG:
                           HB_EXPR_PCODE1( hb_compGenPushLogical, (pLeft->value.asNum.lVal == pRight->value.asNum.lVal) );
                           break;
                        case HB_ET_DOUBLE:
                           HB_EXPR_PCODE1( hb_compGenPushLogical, (pLeft->value.asNum.dVal == pRight->value.asNum.dVal) );
                           break;
                        default:
                           {
                              if( pLeft->value.asNum.NumType == HB_ET_LONG )
                                 HB_EXPR_PCODE1( hb_compGenPushLogical, (pLeft->value.asNum.lVal == pRight->value.asNum.dVal) );
                              else
                                 HB_EXPR_PCODE1( hb_compGenPushLogical, (pLeft->value.asNum.dVal == pRight->value.asNum.lVal) );
                           }
                           break;
                     }
                     break;

                  default:
                     {
                        HB_EXPR_USE( pLeft, HB_EA_PUSH_PCODE );
                        HB_EXPR_USE( pRight, HB_EA_PUSH_PCODE );
                        HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_EQUAL );
                     }
               }
            else
            {
               /* TODO: check for incompatible types
                */
               HB_EXPR_USE( pLeft, HB_EA_PUSH_PCODE );
               HB_EXPR_USE( pRight, HB_EA_PUSH_PCODE );
               HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_EQUAL );
            }
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         {
            /* '=' used standalone in a statement - assign a value
             * it assigns a value and removes it from the stack
             * */
            if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
            {
               /* Send messages are implemented as function calls
                */
               HB_EXPR_PTR pObj = pSelf->value.asOperator.pLeft;
               pObj->value.asMessage.pParms = pSelf->value.asOperator.pRight;
               HB_EXPR_USE( pObj, HB_EA_POP_PCODE );
               pObj->value.asMessage.pParms = NULL; /* to suppress duplicated releasing */
               /* Remove the return value */
               HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
            }
            else
            {
               HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
               HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
            }
         }
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

/* handler for == operator
 */
static HB_EXPR_FUNC( hb_compExprUseEQ )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf = hb_compExprReduceEQ( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_EXACTLYEQUAL );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf  );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseLT )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf = hb_compExprReduceLT( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_LESS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseGT )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf = hb_compExprReduceGT( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_GREATER );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseLE )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf = hb_compExprReduceLE( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_LESSEQUAL );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseGE )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf = hb_compExprReduceGE( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_GREATEREQUAL );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseNE )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf = hb_compExprReduceNE( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_NOTEQUAL );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseIN )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
            pSelf = hb_compExprReduceIN( pSelf, HB_MACRO_PARAM );
         }
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_INSTRING );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePlus )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf = hb_compExprReducePlus( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_PLUS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMinus )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf = hb_compExprReduceMinus( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_MINUS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMult )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf = hb_compExprReduceMult( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_MULT );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseDiv )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf = hb_compExprReduceDiv( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_DIVIDE );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMod )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ), HB_MACRO_PARAM );
         pSelf =hb_compExprReduceMod( pSelf, HB_MACRO_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_MODULUS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePower )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:      /* Clipper doesn't optimize it */
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POWER );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;
      case HB_EA_DELETE:
         HB_EXPR_PCODE1( hb_compExprDelOperator, pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseNegate )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pExpr;

            pSelf->value.asOperator.pLeft = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ), HB_MACRO_PARAM );
            pExpr = pSelf->value.asOperator.pLeft;

            if( pExpr->ExprType == HB_ET_NUMERIC )
            {
               if( pExpr->value.asNum.NumType == HB_ET_DOUBLE )
                  pExpr->value.asNum.dVal = - pExpr->value.asNum.dVal;
               else
                  pExpr->value.asNum.lVal = - pExpr->value.asNum.lVal;
               pSelf->ExprType = HB_ET_NONE;  /* do not delete operator parameter - we are still using it */
               HB_EXPR_PCODE1( hb_compExprDelete, pSelf );
               pSelf = pExpr;
            }
         }
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
            HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_NEGATE );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HB_C52_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_EXPR_GENPCODE1( hb_compGenPCode1, HB_P_POP );
#else
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
#endif
         break;

      case HB_EA_STATEMENT:
         hb_compErrorSyntax( pSelf );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePreInc )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ), HB_MACRO_PARAM );
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_PCODE2( hb_compExprPushPreOp, pSelf, HB_P_INC );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_PCODE2( hb_compExprUsePreOp, pSelf, HB_P_INC );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePreDec )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ), HB_MACRO_PARAM );
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_PCODE2( hb_compExprPushPreOp, pSelf, HB_P_DEC );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_PCODE2( hb_compExprUsePreOp, pSelf, HB_P_DEC );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            HB_EXPR_PCODE1( hb_compExprDelete, pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

