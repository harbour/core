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

/* TODO:
 *    Correct post- and pre- operations to correctly handle the following code
 *    a[ i++ ]++
 *    Notice: in current implementation (an in Clipper too) 'i++' is evaluated
 *    two times! This causes that the new value (after incrementation) is
 *    stored in next element of the array.
 */

#include <math.h>
#include "compiler.h"

/* memory allocation
 */
#define  HB_XGRAB( size )  hb_xgrab( (size) )
#define  HB_XFREE( pPtr )  hb_xfree( (void *)(pPtr) )

/* NOTE: We need to pass additional parameter if compilation for macro
 * compiler support. This parameter is a pointer to an internal macro
 * structure used to store all data needed for re-entrant compilation
 */
#ifdef HB_MACRO_SUPPORT

#define  hb_compGenPCode1( p1 )           hb_compGenPCode1( p1, HB_MACRO_PARAM )
#define  hb_compGenPCode3( p1, p2, p3 )   hb_compGenPCode3( p1, p2, p3, HB_MACRO_PARAM )
#define  hb_compGenPCodeN( p1, p2 )       hb_compGenPCodeN( p1, p2, HB_MACRO_PARAM )

#define  hb_compCodeBlockStart( )  hb_compCodeBlockStart( HB_MACRO_PARAM )
#define  hb_compCodeBlockEnd( )    hb_compCodeBlockEnd( HB_MACRO_PARAM )

#define  hb_compGenJump( p1 )             hb_compGenJump( p1, HB_MACRO_PARAM )
#define  hb_compGenJumpFalse( p1 )        hb_compGenJumpFalse( p1, HB_MACRO_PARAM )
#define  hb_compGenJumpTrue( p1 )         hb_compGenJumpTrue( p1, HB_MACRO_PARAM )
#define  hb_compGenJumpHere( p1 )         hb_compGenJumpHere( p1, HB_MACRO_PARAM )
#define  hb_compGenJumpThere( p1, p2 )    hb_compGenJumpThere( p1, p2, HB_MACRO_PARAM )

#define  hb_compGenMessage( p1 )       hb_compGenMessage( p1, HB_MACRO_PARAM )
#define  hb_compGenMessageData( p1 )   hb_compGenMessageData( p1, HB_MACRO_PARAM )
#define  hb_compGenPopVar( p1 )        hb_compGenPopVar( p1, HB_MACRO_PARAM )
#define  hb_compGenPushDouble( p1, p2 )   hb_compGenPushDouble( p1, p2, HB_MACRO_PARAM )
#define  hb_compGenPushFunCall( p1 )      hb_compGenPushFunCall( p1, HB_MACRO_PARAM )
#define  hb_compGenPushVar( p1 )          hb_compGenPushVar( p1, HB_MACRO_PARAM )
#define  hb_compGenPushVarRef( p1 )       hb_compGenPushVarRef( p1, HB_MACRO_PARAM )
#define  hb_compGenPushLogical( p1 )      hb_compGenPushLogical( p1, HB_MACRO_PARAM )
#define  hb_compGenPushLong( p1 )         hb_compGenPushLong( p1, HB_MACRO_PARAM )
#define  hb_compGenPushNil( )             hb_compGenPushNil( p1, HB_MACRO_PARAM )
#define  hb_compGenPushString( p1, p2 )   hb_compGenPushString( p1, p2, HB_MACRO_PARAM )
#define  hb_compGenPushSymbol( p1 )   hb_compGenPushSymbol( p1, HB_MACRO_PARAM )
#define  hb_compGenPushAliasedVar( p1, p2, p3, p4 )   hb_compGenPushAliasedVar( p1, p2, p3, p4, HB_MACRO_PARAM )
#define  hb_compGenPopAliasedVar( p1, p2 , p3, p4 )   hb_compGenPopAliasedVar( p1, p2, p3, p4, HB_MACRO_PARAM )
#define  hb_compGenPushFunRef( p1 )                   hb_compGenPushFunRef( p1, HB_MACRO_PARAM )

#endif   /* ifdef HB_MACRO_SUPPORT */

/* value types seen at language level
 */
#define  HB_EV_UNKNOWN     0
#define  HB_EV_NIL         1
#define  HB_EV_NUMERIC     2
#define  HB_EV_STRING      4
#define  HB_EV_CODEBLOCK   8
#define  HB_EV_LOGICAL     16
#define  HB_EV_OBJECT      32
#define  HB_EV_ARRAY       64
#define  HB_EV_SYMBOL      128
#define  HB_EV_VARREF      256
#define  HB_EV_FUNREF      512

/* messages sent to expressions
 */
typedef enum
{
   HB_EA_REDUCE = 0,    /* reduce the expression into optimized one */
   HB_EA_ARRAY_AT,      /* check if the expession can be used as array */
   HB_EA_ARRAY_INDEX,   /* check if the expession can be used as index */
   HB_EA_LVALUE,        /* check if the expression can be used as lvalue (left side of an assigment) */
   HB_EA_PUSH_PCODE,    /* generate the pcodes to push the value of expression */
   HB_EA_POP_PCODE,     /* generate the pcodes to pop the value of expression */
   HB_EA_PUSH_POP,      /* generate the pcodes to push and pop the expression */
   HB_EA_STATEMENT,     /* generate the pcodes for a statement */
   HB_EA_DELETE         /* delete components of the expression */
} HB_EXPR_MESSAGE;

/* additional definitions used to distinguish numeric expressions
 */
#define  HB_ET_LONG     1
#define  HB_ET_DOUBLE   2

/* additional definitions used to distinguish macro expressions
 */
#define  HB_ET_MACRO_VAR      0   /* &variable */
#define  HB_ET_MACRO_SYMBOL   1   /* &fimcall() */
#define  HB_ET_MACRO_ALIASED  2   /* &alias->&variable */

/* types of expressions
 * NOTE: the order of these definition is important - change it carefully
 *    All types <= HB_ET_FUNREF are constant values
 *    All types <= HB_ET_VARIABLE are a simple values
 *    All types > HB_ET_VARIABLE are operators
 */
typedef enum
{
   HB_ET_NONE = 0,
   HB_ET_NIL,
   HB_ET_NUMERIC,
   HB_ET_STRING,
   HB_ET_CODEBLOCK,
   HB_ET_LOGICAL,
   HB_ET_SELF,
   HB_ET_ARRAY,
   HB_ET_VARREF,
   HB_ET_FUNREF,
   HB_ET_IIF,
   HB_ET_LIST,
   HB_ET_ARGLIST,
   HB_ET_ARRAYAT,
   HB_ET_MACRO,
   HB_ET_FUNCALL,
   HB_ET_ALIASVAR,
   HB_ET_ALIASEXPR,
   HB_ET_SEND,
   HB_ET_FUNNAME,
   HB_ET_ALIAS,
   HB_ET_RTVAR,      /* PRIVATE or PUBLIC declaration of variable */
   HB_ET_VARIABLE,
   HB_EO_POSTINC,    /* post-operators -> lowest precedence */
   HB_EO_POSTDEC,
   HB_EO_ASSIGN,     /* assigments */
   HB_EO_PLUSEQ,
   HB_EO_MINUSEQ,
   HB_EO_MULTEQ,
   HB_EO_DIVEQ,
   HB_EO_MODEQ,
   HB_EO_EXPEQ,
   HB_EO_OR,         /* logical operators */
   HB_EO_AND,
   HB_EO_NOT,
   HB_EO_EQUAL,      /* relational operators */
   HB_EO_EQ,
   HB_EO_LT,
   HB_EO_GT,
   HB_EO_LE,
   HB_EO_GE,
   HB_EO_NE,
   HB_EO_IN,
   HB_EO_PLUS,       /* addition */
   HB_EO_MINUS,
   HB_EO_MULT,       /* multiple */
   HB_EO_DIV,
   HB_EO_MOD,
   HB_EO_POWER,
   HB_EO_NEGATE,     /* sign operator */
   HB_EO_PREINC,
   HB_EO_PREDEC      /* pre-operators -> the highest precedence */
} HB_EXPR_OPERATOR;

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

static HB_EXPR_FUNC_PTR s_ExprTable[] = {
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

/* Table with operators precedence
 * NOTE:
 *    HB_ET_NIL is used for an ordinary values and post- operators
 *    HB_ET_NONE is used for invalid syntax, e.g. var := var1 += 2
 */
static BYTE s_PrecedTable[] = {
   HB_ET_NIL,                 /*   HB_ET_NONE = 0,    */
   HB_ET_NIL,                 /*   HB_ET_NIL,         */
   HB_ET_NIL,                 /*   HB_ET_NUMERIC,     */
   HB_ET_NIL,                 /*   HB_ET_STRING,      */
   HB_ET_NIL,                 /*   HB_ET_CODEBLOCK,   */
   HB_ET_NIL,                 /*   HB_ET_LOGICAL,     */
   HB_ET_NIL,                 /*   HB_ET_SELF,        */
   HB_ET_NIL,                 /*   HB_ET_ARRAY,       */
   HB_ET_NIL,                 /*   HB_ET_VARREF,      */
   HB_ET_NIL,                 /*   HB_ET_FUNREF,      */
   HB_ET_NIL,                 /*   HB_ET_IIF,         */
   HB_ET_NIL,                 /*   HB_ET_LIST,        */
   HB_ET_NIL,                 /*   HB_ET_ARGLIST,     */
   HB_ET_NIL,                 /*   HB_ET_ARRAYAT,     */
   HB_ET_NIL,                 /*   HB_ET_MACRO,       */
   HB_ET_NIL,                 /*   HB_ET_FUNCALL,     */
   HB_ET_NIL,                 /*   HB_ET_ALIASVAR,    */
   HB_ET_NIL,                 /*   HB_ET_ALIASEXPR,   */
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
   HB_EO_EQUAL,               /*   HB_EO_LT,          */
   HB_EO_EQUAL,               /*   HB_EO_GT,          */
   HB_EO_EQUAL,               /*   HB_EO_LE,          */
   HB_EO_EQUAL,               /*   HB_EO_GE,          */
   HB_EO_EQUAL,               /*   HB_EO_NE,          */
   HB_EO_EQUAL,               /*   HB_EO_IN,          */
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

/* Forward declarations
 */

#ifdef HB_MACRO_SUPPORT

static void hb_compExprDelOperatorMC( HB_EXPR_PTR, HB_MACRO_DECL );
static ULONG hb_compExprListReduceMC( HB_EXPR_PTR, HB_MACRO_DECL );
static HB_EXPR_PTR hb_compExprListStripMC( HB_EXPR_PTR, HB_MACRO_DECL );
static void hb_compExprPushOperEqMC( HB_EXPR_PTR, BYTE, HB_MACRO_DECL );
static void hb_compExprUseOperEqMC( HB_EXPR_PTR, BYTE, HB_MACRO_DECL );
static void hb_compExprPushPreOpMC( HB_EXPR_PTR, BYTE, HB_MACRO_DECL );
static void hb_compExprPushPostOpMC( HB_EXPR_PTR, BYTE, HB_MACRO_DECL );
static void hb_compExprUsePreOpMC( HB_EXPR_PTR, BYTE, HB_MACRO_DECL );
static void hb_compExprUseAliasMacroMC( HB_EXPR_PTR, BYTE, HB_MACRO_DECL );

#define  hb_compExprDelOperator( p )   hb_compExprDelOperatorMC( p, HB_MACRO_PARAM )
#define  hb_compExprListReduce( pSelf ) hb_compExprListReduceMC( pSelf, HB_MACRO_PARAM )
#define  hb_compExprListStrip( p )  hb_compExprListStripMC( p, HB_MACRO_PARAM )
#define  hb_compExprPushOperEq( p, b ) hb_compExprPushOperEqMC( p, b, HB_MACRO_PARAM )
#define  hb_compExprUseOperEq( p, b )  hb_compExprUseOperEqMC( p, b, HB_MACRO_PARAM )
#define  hb_compExprPushPreOp( p, b )  hb_compExprPushPreOpMC( p, b, HB_MACRO_PARAM )
#define  hb_compExprPushPostOp( p, b ) hb_compExprPushPostOpMC( p, b, HB_MACRO_PARAM )
#define  hb_compExprUsePreOp( p, b )   hb_compExprUsePreOpMC( p, b, HB_MACRO_PARAM )
#define  hb_compExprUseAliasMacro( p, b ) hb_compExprUseAliasMacroMC( p, b, HB_MACRO_PARAM )

#define  hb_comp_bShortCuts   ( HB_MACRO_DATA->bShortCuts )

#else

static void hb_compExprDelOperator( HB_EXPR_PTR );
static ULONG hb_compExprListReduce( HB_EXPR_PTR );
static HB_EXPR_PTR hb_compExprListStrip( HB_EXPR_PTR );
static void hb_compExprPushOperEq( HB_EXPR_PTR, BYTE );
static void hb_compExprUseOperEq( HB_EXPR_PTR, BYTE );
static void hb_compExprPushPreOp( HB_EXPR_PTR, BYTE );
static void hb_compExprPushPostOp( HB_EXPR_PTR, BYTE );
static void hb_compExprUsePreOp( HB_EXPR_PTR, BYTE );
static void hb_compExprUseAliasMacro( HB_EXPR_PTR, BYTE );

#endif

static BOOL hb_compExprCheckMacroVar( char * );
static HB_CBVAR_PTR hb_compExprCBVarNew( char *, BYTE );
static void hb_compExprCBVarDel( HB_CBVAR_PTR );

/* ************************************************************************ */

HB_EXPR_PTR hb_compExprNew( int iType )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNew(%i)", iType));

   pExpr = ( HB_EXPR_PTR ) HB_XGRAB( sizeof( HB_EXPR ) );

   pExpr->ExprType = iType;
   pExpr->pNext    = NULL;
   pExpr->ValType  = HB_EV_UNKNOWN;

   return pExpr;
}

/* Delete all components and delete self
 */
#ifdef HB_MACRO_SUPPORT
void hb_compExprDelete( HB_EXPR_PTR pExpr, HB_MACRO_DECL )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprDelete()"));
   HB_EXPR_USE( pExpr, HB_EA_DELETE );
   HB_XFREE( pExpr );
}

#define  hb_compExprDelete( pParms )   hb_compExprDelete( pParms, HB_MACRO_PARAM )

#else
void hb_compExprDelete( HB_EXPR_PTR pExpr )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprDelete()"));
   HB_EXPR_USE( pExpr, HB_EA_DELETE );
   HB_XFREE( pExpr );
}
#endif

/* Delete self - all components will be deleted somewhere else
 */
void hb_compExprClear( HB_EXPR_PTR pExpr )
{
   HB_XFREE( pExpr );
}

char * hb_compExprDescription( HB_EXPR_PTR pExpr )
{
   if( pExpr )
      return s_OperTable[ pExpr->ExprType ];
   else
      return s_OperTable[ 0 ];
}

HB_EXPR_PTR hb_compExprNewEmpty( void )
{
   return hb_compExprNew( HB_ET_NONE );
}

HB_EXPR_PTR hb_compExprNewDouble( double dValue, BYTE ucDec )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewDouble(%f, %i)", dValue, ucDec));

   pExpr =hb_compExprNew( HB_ET_NUMERIC );

   pExpr->value.asNum.dVal    = dValue;
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
   pExpr->value.asNum.bDec     = 0;
   pExpr->value.asNum.NumType = HB_ET_LONG;
   pExpr->ValType = HB_EV_NUMERIC;

   return pExpr;
}

HB_EXPR_PTR hb_compExprNewString( char *szValue )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewString(%s)", szValue));

   pExpr =hb_compExprNew( HB_ET_STRING );

   pExpr->value.asString = szValue;
   pExpr->ulLength = strlen( szValue );
   pExpr->ValType = HB_EV_STRING;

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

/* Add a new local variable declaration
 */
#ifdef HB_MACRO_SUPPORT
HB_EXPR_PTR hb_compExprCBVarAdd( HB_EXPR_PTR pCB, char * szVarName, HB_MACRO_DECL )
#else
HB_EXPR_PTR hb_compExprCBVarAdd( HB_EXPR_PTR pCB, char * szVarName, BYTE bType )
#endif
{
   HB_CBVAR_PTR pVar;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprCBVarAdd(%s)", szVarName));

   if( pCB->value.asList.pIndex )
   {
      /* add it to the end of the list
      */
      pVar = ( HB_CBVAR_PTR ) pCB->value.asList.pIndex;
      while( pVar )
      {
         if( strcmp( szVarName, pVar->szName ) == 0 )
            hb_compErrorDuplVar( szVarName );

         if( pVar->pNext )
            pVar = pVar->pNext;
         else
         {
#ifdef HB_MACRO_SUPPORT
            pVar->pNext = hb_compExprCBVarNew( szVarName, ' ' );
#else
            pVar->pNext = hb_compExprCBVarNew( szVarName, bType );
#endif
            pVar = NULL;
         }
      }
   }
   else
#ifdef HB_MACRO_SUPPORT
      pCB->value.asList.pIndex = ( HB_EXPR_PTR ) hb_compExprCBVarNew( szVarName, ' ' );
#else
      pCB->value.asList.pIndex = ( HB_EXPR_PTR ) hb_compExprCBVarNew( szVarName, bType );
#endif

   return pCB;
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

/* Create a new IIF() expression or set arguments
 *
 * pIIF is a list of three expressions
 */
HB_EXPR_PTR hb_compExprNewIIF( HB_EXPR_PTR pExpr )
{
#ifndef HB_MACRO_SUPPORT
   HB_EXPR_PTR pTmp;

   pExpr->ExprType = HB_ET_IIF;

   pTmp = pExpr->value.asList.pExprList;  /* get first expression */
   if( pTmp->ExprType == HB_ET_NONE )
   {
      /* there is no conditional expression e.g. IIF( , true, false )
       */
      hb_compErrorSyntax( pExpr );
   }
#else
   pExpr->ExprType = HB_ET_IIF;
#endif

   return pExpr;
}

/* Create function call
 */
#ifdef HB_MACRO_SUPPORT
HB_EXPR_PTR hb_compExprNewFunCall( HB_EXPR_PTR pName, HB_EXPR_PTR pParms, HB_MACRO_DECL )
#else
HB_EXPR_PTR hb_compExprNewFunCall( HB_EXPR_PTR pName, HB_EXPR_PTR pParms )
#endif
{
   HB_EXPR_PTR pExpr = NULL;

   if( pName->ExprType == HB_ET_FUNNAME )
   {
      /* The name of a function is specified at compile time
       * e.g. MyFunc()
       *
       * NOTE:  'pName' can be a macro expression that will be resolved
       * at runtime - in this case pName is an expression of HB_ET_MACRO type
       * e.g. &MyVar()
       */
      int iCount;

      HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewFunCall(%s)", pName->value.asSymbol));

      if( pParms )
      {
         iCount = hb_compExprListLen( pParms );
         /* Check the special case when no parameters are passed - in this case
         * pParms is an expression of type HB_ET_NONE and we shouldn't
         * replace it with NIL value
         */
         if( iCount == 1 && pParms->value.asList.pExprList->ExprType == HB_ET_NONE )
            --iCount;
      }
      else
         iCount = 0;

#ifndef HB_MACRO_SUPPORT
      hb_compFunCallCheck( pName->value.asSymbol, iCount );
#endif

      /* TODO: AT() (also done by Clipper, already mentioned)
               LEN() (also done by Clipper)
               ASC() (not done by Clipper)
               EMPTY() (not done by Clipper) */

      if( ( strcmp( "CHR", pName->value.asSymbol ) == 0 ) && iCount )
      {
         /* try to change it into a string */
         HB_EXPR_PTR pArg = pParms->value.asList.pExprList;

         if( pArg->ExprType == HB_ET_NUMERIC )
         {
            /* NOTE: CA-Cl*pper's compiler optimizer will be wrong for those
                     CHR() cases where the passed parameter is a constant which
                     can be divided by 256 but it's not zero, in this case it
                     will return an empty string instead of a Chr(0). [vszel] */

            pExpr = hb_compExprNew( HB_ET_STRING );
            pExpr->ValType = HB_EV_STRING;
            if( pArg->value.asNum.NumType == HB_ET_LONG )
            {
               if( ( pArg->value.asNum.lVal % 256 ) == 0 && pArg->value.asNum.lVal != 0 )
               {
                  pExpr->value.asString = ( char * ) HB_XGRAB( 1 );
                  pExpr->value.asString[ 0 ] = '\0';
                  pExpr->ulLength = 0;
               }
               else
               {
                  pExpr->value.asString = ( char * ) HB_XGRAB( 2 );
                  pExpr->value.asString[ 0 ] = ( pArg->value.asNum.lVal % 256 );
                  pExpr->value.asString[ 1 ] = '\0';
                  pExpr->ulLength = 1;
               }
            }
            else
            {
               pExpr->value.asString = ( char * ) HB_XGRAB( 2 );
               pExpr->value.asString[ 0 ] = ( ( long ) pArg->value.asNum.dVal % 256 );
               pExpr->value.asString[ 1 ] = '\0';
               pExpr->ulLength = 1;
            }
            hb_compExprDelete( pParms );
            hb_compExprDelete( pName );
         }
      }
   }
   else if( pName->ExprType == HB_ET_MACRO )
   {
      /* Signal that macro compiler have to generate a pcode that will
       * return function name as symbol instead of usual value
       */
      pName->value.asMacro.SubType = HB_ET_MACRO_SYMBOL;

      HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewFunCall(&)"));
   }

   if( pExpr == NULL )
   {
      pExpr = hb_compExprNew( HB_ET_FUNCALL );
      pExpr->value.asFunCall.pParms = pParms;
      pExpr->value.asFunCall.pFunName = pName;
   }

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

/* Creates new array access expression
 *    pArray[ pIndex ]
 * NOTE: In case of multiple indexes it is called recursively
 *    array[ idx1, idx2 ] => ( array[ idx1 ] )[ idx2 ]
 */
#ifdef HB_MACRO_SUPPORT
HB_EXPR_PTR hb_compExprNewArrayAt( HB_EXPR_PTR pArray, HB_EXPR_PTR pIndex, HB_MACRO_DECL )
#else
HB_EXPR_PTR hb_compExprNewArrayAt( HB_EXPR_PTR pArray, HB_EXPR_PTR pIndex )
#endif
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewArrayAt()"));

   pExpr = hb_compExprNew( HB_ET_ARRAYAT );

   /* Check if this expression can be indexed */
   HB_EXPR_USE( pArray, HB_EA_ARRAY_AT );
   /* Check if this expression can be an index */
   HB_EXPR_USE( pIndex, HB_EA_ARRAY_INDEX );
   pExpr->value.asList.pExprList = pArray;
   pExpr->value.asList.pIndex = pIndex;

   return pExpr;
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
   }

   return pExpr;
}

/* Creates new aliased variable
 *    aliasexpr -> identifier
 */
HB_EXPR_PTR hb_compExprNewAliasVar( HB_EXPR_PTR pAlias, HB_EXPR_PTR pVariable )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_ET_ALIASVAR );

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
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_ET_ALIASEXPR );

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
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_ET_SEND );

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
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_ET_LIST );
   pExpr->value.asList.pExprList = pFirstItem;
   return pExpr;
}

/* Creates a list of function call arguments
 */
HB_EXPR_PTR hb_compExprNewArgList( HB_EXPR_PTR pFirstItem )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_ET_ARGLIST );
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
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_ET_VARIABLE );
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
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_ET_RTVAR );

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
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_ET_FUNNAME );
   pExpr->value.asSymbol = szName;
   return pExpr;
}

/* Create a new symbol used in an alias expressions
 */
HB_EXPR_PTR hb_compExprNewAlias( char * szName )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_ET_ALIAS );
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
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_ASSIGN );
   pExpr->value.asOperator.pLeft  = pLeftExpr;
   pExpr->value.asOperator.pRight = pRightExpr;
   return pExpr;
}

/* It initializes static variable.
 *    It is called in the following context:
 * STATIC sVar := expression
 *
 * pLeftExpr - is a variable name
 * pRightExpr - can be an expression of any type
 */
#ifndef HB_MACRO_SUPPORT
HB_EXPR_PTR hb_compExprAssignStatic( HB_EXPR_PTR pLeftExpr, HB_EXPR_PTR pRightExpr )
{
   HB_EXPR_PTR pExpr = hb_compExprNew( HB_EO_ASSIGN );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   /* Try to reduce the assigned value */
   pRightExpr = hb_compExprListStrip( HB_EXPR_USE( pRightExpr, HB_EA_REDUCE ) );
   pExpr->value.asOperator.pRight = pRightExpr;

   if( pRightExpr->ExprType > HB_ET_FUNREF )
   {
      /* Illegal initializer for static variable (not a constant value)
       */
      hb_compErrorStatic( pLeftExpr->value.asSymbol, pRightExpr );
   }
   else if( pRightExpr->ExprType == HB_ET_ARRAY )
   {
      /* Scan an array for illegal initializers.
       * An array item have to be a const value too.
       */
      HB_EXPR_PTR pElem = pRightExpr->value.asList.pExprList;
      HB_EXPR_PTR pNext;
      HB_EXPR_PTR * pPrev;

      pPrev = &pRightExpr->value.asList.pExprList;
      while( pElem )
      {
         /* NOTE: During reduction the expression can be replaced by the
          *    new one - this will break the linked list of expressions.
          * (classical case of replacing an item in a linked list)
          */
         pNext = pElem->pNext; /* store next expression in case the current  will be reduced */
         pElem = hb_compExprListStrip( HB_EXPR_USE( pElem, HB_EA_REDUCE ) );
         if( pElem->ExprType > HB_ET_FUNREF )
            hb_compErrorStatic( pLeftExpr->value.asSymbol, pElem );
         *pPrev = pElem;   /* store a new expression into the previous one */
         pElem->pNext = pNext;  /* restore the link to next expression */
         pPrev  = &pElem->pNext;
         pElem  = pNext;
      }
   }

   return pExpr;
}
#endif


/* Sets the argument of an operation found previously
 */
#ifdef HB_MACRO_SUPPORT
HB_EXPR_PTR hb_compExprSetOperand( HB_EXPR_PTR pExpr, HB_EXPR_PTR pItem, HB_MACRO_DECL )
#else
HB_EXPR_PTR hb_compExprSetOperand( HB_EXPR_PTR pExpr, HB_EXPR_PTR pItem )
#endif
{
   BYTE ucRight;

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
      hb_compErrorSyntax( pItem );
   }
   else
   {
      /* the right side of an operator is an expression with other operator
       * e.g. a := 2 + b * 3
       *   We have to set the proper order of evaluation using
       * precedence rules
       */
      BYTE ucLeft = s_PrecedTable[ pExpr->ExprType ];
      if( ucLeft >= ucRight )
      {
         /* Left operator has the same or lower precedence then the right one
          * e.g.  a * b + c
          *    pItem -> b + c   -> L=b  R=c  O=+
          *    pExpr -> a *     -> l=a  r=   o=*
          *
          *    -> (a * b) + c    -> Lelf=(a * b)  Right=c  Oper=+
          *             Left  := l (o) L
          *             Right := R
          *             Oper  := O
          */
#ifdef HB_MACRO_SUPPORT
         pItem->value.asOperator.pLeft = hb_compExprSetOperand( pExpr, pItem->value.asOperator.pLeft, HB_MACRO_PARAM );
#else
         pItem->value.asOperator.pLeft = hb_compExprSetOperand( pExpr, pItem->value.asOperator.pLeft );
#endif
         pExpr = pItem;
      }
      else
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
   }

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

/* ************************************************************************* */

/* Generates pcode for inline expression used as a statement
 * NOTE: It doesn't not leave any value on the eval stack
 */
#ifdef HB_MACRO_SUPPORT
HB_EXPR_PTR hb_compExprGenStatement( HB_EXPR_PTR pExpr, HB_MACRO_DECL )
#else
HB_EXPR_PTR hb_compExprGenStatement( HB_EXPR_PTR pExpr )
#endif
{
   pExpr = HB_EXPR_USE( pExpr, HB_EA_REDUCE );
   HB_EXPR_USE( pExpr, HB_EA_STATEMENT );
   return pExpr;
}

/* Generates pcode to push an expressions
 * NOTE: It pushes a value on the stack and leaves this value on the stack
 */
#ifdef HB_MACRO_SUPPORT
HB_EXPR_PTR hb_compExprGenPush( HB_EXPR_PTR pExpr, HB_MACRO_DECL )
#else
HB_EXPR_PTR hb_compExprGenPush( HB_EXPR_PTR pExpr )
#endif
{
   pExpr = HB_EXPR_USE( pExpr, HB_EA_REDUCE );
   HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
   return pExpr;
}

/* Generates pcode to pop an expressions
 */
#ifdef HB_MACRO_SUPPORT
HB_EXPR_PTR hb_compExprGenPop( HB_EXPR_PTR pExpr, HB_MACRO_DECL )
#else
HB_EXPR_PTR hb_compExprGenPop( HB_EXPR_PTR pExpr )
#endif
{
   return HB_EXPR_USE( pExpr, HB_EA_POP_PCODE );
}


/* ************************************************************************* */

static HB_EXPR_FUNC( hb_compExprUseDummy )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         break;
      case HB_EA_ARRAY_AT:
         hb_compErrorType( pSelf);
      case HB_EA_ARRAY_INDEX:
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         hb_compGenPCode1( HB_P_PUSHNIL );
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
         hb_compGenPCode1( HB_P_PUSHNIL );
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
            hb_compGenPushDouble( pSelf->value.asNum.dVal, pSelf->value.asNum.bDec );
         else
            hb_compGenPushLong( pSelf->value.asNum.lVal );
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
            hb_compGenPushString( pSelf->value.asString, pSelf->ulLength );
#ifndef HB_MACRO_SUPPORT
/* only memvar variables are allowed in macro compilation - there is no
 * need to check for locals or static variables
 */
            if( hb_compExprCheckMacroVar( pSelf->value.asString ) )
               hb_compGenPCode1( HB_P_MACROTEXT );
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
         HB_XFREE( pSelf->value.asString );
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
#ifndef HB_MACRO_SUPPORT
            HB_CBVAR_PTR pVar;
#endif

            hb_compCodeBlockStart();
            /* Define requested local variables
             */
#ifdef HB_MACRO_SUPPORT
            HB_PCODE_DATA->pLocals = ( HB_CBVAR_PTR ) pSelf->value.asList.pIndex;
#else
            pVar = ( HB_CBVAR_PTR ) pSelf->value.asList.pIndex;
            while( pVar )
            {
               hb_compVariableAdd( pVar->szName, pVar->bType );
               pVar =pVar->pNext;
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
            hb_compCodeBlockEnd();
         }
         break;
      case HB_EA_POP_PCODE:
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( pSelf );
         break;
      case HB_EA_DELETE:
         hb_compExprCBVarDel( ( HB_CBVAR_PTR ) pSelf->value.asList.pIndex );
         hb_compExprDelete( pSelf->value.asList.pExprList );
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
         hb_compGenPushLogical( pSelf->value.asLogical );
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
         hb_compGenPCode1( HB_P_PUSHSELF );
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
         hb_compExprListReduce( pSelf );
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
            hb_compGenPCode3( HB_P_ARRAYGEN, 0, 0 );
         else
         {
            while( pElem )
            {
               HB_EXPR_USE( pElem, HB_EA_PUSH_PCODE );
               pElem = pElem->pNext;
            }
            hb_compGenPCode3( HB_P_ARRAYGEN, HB_LOBYTE( pSelf->ulLength ), HB_HIBYTE( pSelf->ulLength ) );
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
            hb_compExprDelete( pElem );
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
         hb_compGenPushVarRef( pSelf->value.asSymbol );
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
         hb_compGenPushFunCall( pSelf->value.asSymbol );
         hb_compGenPCode1( HB_P_FUNCPTR );
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
         {
            HB_EXPR_PTR pExpr;

            hb_compExprListReduce( pSelf );

            pExpr =pSelf->value.asList.pExprList;  /* get conditional expression */
            if( pExpr->ExprType == HB_ET_LOGICAL )
            {
               /* the condition was reduced to a logical value: .T. or .F.
               */
               if( pExpr->value.asLogical )
               {
                  /* .T. was specified
                  */
                  pExpr = pExpr->pNext;   /* skip to TRUE expression */
                  /* delete condition  - it is no longer needed
                   */
                  hb_compExprDelete( pSelf->value.asList.pExprList );
                  /* assign NULL to a start of expressions list to suppress
                   * deletion of expression's components - we are deleting them
                   * here
                   */
                  pSelf->value.asList.pExprList = NULL;
                  hb_compExprDelete( pSelf );
                  /* store the TRUE expression as a result of reduction
                   */
                  pSelf = pExpr;
                  pExpr = pExpr->pNext;     /* skip to FALSE expression */
                  hb_compExprDelete( pExpr );   /* delete FALSE expr */
                  pSelf->pNext = NULL;
               }
               else
               {
                  /* .F. was specified
                  */
                  pExpr = pExpr->pNext;   /* skip to TRUE expression */
                  /* delete condition  - it is no longer needed
                   */
                  hb_compExprDelete( pSelf->value.asList.pExprList );
                  /* assign NULL to a start of expressions list to suppress
                   * deletion of expression's components - we are deleting them
                   * here
                   */
                  pSelf->value.asList.pExprList = NULL;
                  hb_compExprDelete( pSelf );
                  /* store the FALSE expression as a result of reduction
                   */
                  pSelf = pExpr->pNext;
                  hb_compExprDelete( pExpr );   /* delete TRUE expr */
                  pSelf->pNext = NULL;
               }
            }
            /* check if valid expression is passed
            */
            else if( ( pExpr->ExprType == HB_ET_DOUBLE ) ||
                ( pExpr->ExprType == HB_ET_LONG ) ||
                ( pExpr->ExprType == HB_ET_NIL ) ||
                ( pExpr->ExprType == HB_ET_STRING ) ||
                ( pExpr->ExprType == HB_ET_CODEBLOCK ) ||
                ( pExpr->ExprType == HB_ET_SELF ) ||
                ( pExpr->ExprType == HB_ET_ARRAY ) )
                {
                     hb_compErrorType( pExpr );
            }
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
            /* this is called if all three parts of IIF expression should be generated
            */
            LONG lPosFalse, lPosEnd;
            HB_EXPR_PTR pExpr = pSelf->value.asList.pExprList;

            HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
            lPosFalse = hb_compGenJumpFalse( 0 );
            pExpr =pExpr->pNext;

            HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
            lPosEnd = hb_compGenJump( 0 );
            pExpr =pExpr->pNext;

            hb_compGenJumpHere( lPosFalse );
            HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
            hb_compGenJumpHere( lPosEnd );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_POP );  /* remove a value if used in statement */
         }
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asList.pExprList )
         {
            HB_EXPR_PTR pTmp, pExpr = pSelf->value.asList.pExprList;
            while( pExpr )
            {
               pTmp = pExpr->pNext;    /* store next expression */
               hb_compExprDelete( pExpr );
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
            hb_compExprListReduce( pSelf );
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

            while( pExpr )
            {
               if( pExpr->pNext )
                  HB_EXPR_USE( pExpr, HB_EA_PUSH_POP );
               else
                  HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );   /* the last expression */
               pExpr = pExpr->pNext;
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
               hb_compExprDelete( pExpr );
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
         hb_compExprListReduce( pSelf );
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
               hb_compExprDelete( pExpr );
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
                     lIndex = pIdx->value.asNum.dVal;

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
                     hb_compExprDelete( pSelf );
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
                     lIndex = pIdx->value.asNum.dVal;

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
            hb_compGenPCode1( HB_P_ARRAYPUSH );
         }
         break;

      case HB_EA_POP_PCODE:
         {
            HB_EXPR_USE( pSelf->value.asList.pExprList, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asList.pIndex, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_ARRAYPOP );
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
         hb_compExprDelete( pSelf->value.asList.pExprList );
         hb_compExprDelete( pSelf->value.asList.pIndex );
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
                  hb_compGenPushVar( pSelf->value.asMacro.szMacro );
               }
               else
               {
                  /* complex macro expression: prefix&var.suffix
                   * all components should be placed as a string that will
                   * be compiled after text susbstitution
                   */
                  hb_compGenPushString( pSelf->value.asMacro.szMacro, strlen(pSelf->value.asMacro.szMacro) );
               }
            }
            /* compile & run - leave a result on the eval stack
             */
            if( pSelf->value.asMacro.SubType == HB_ET_MACRO_SYMBOL )
               hb_compGenPCode1( HB_P_MACROSYMBOL );
            else if( pSelf->value.asMacro.SubType != HB_ET_MACRO_ALIASED )
               hb_compGenPCode1( HB_P_MACROPUSH );
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
                  hb_compGenPushVar( pSelf->value.asMacro.szMacro );
               }
               else
               {
                  /* complex macro expression: prefix&var.suffix
                   * all components should be placed as a string that will
                   * be compiled after text susbstitution
                   */
                  hb_compGenPushString( pSelf->value.asMacro.szMacro, strlen(pSelf->value.asMacro.szMacro) );
               }
            }
            /* compile & run - macro compiler will generate pcode to pop a value
             * from the eval stack
             */
	    if( pSelf->value.asMacro.SubType != HB_ET_MACRO_ALIASED )
              hb_compGenPCode1( HB_P_MACROPOP );
         }
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asMacro.pExprList )
            hb_compExprDelete( pSelf->value.asMacro.pExprList );
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
            hb_compGenPCode1( HB_P_PUSHNIL );

            if( pSelf->value.asFunCall.pParms )
            {
               /* NOTE: pParms will be NULL in 'DO procname' (if there is
                * no WITH keyword)
                */
               usCount = hb_compExprListLen( pSelf->value.asFunCall.pParms );
               if( usCount == 1 && pSelf->value.asFunCall.pParms->value.asList.pExprList->ExprType == HB_ET_NONE )
                  --usCount;
               if( usCount )
                  HB_EXPR_USE( pSelf->value.asFunCall.pParms, HB_EA_PUSH_PCODE );
            }
            else
               usCount = 0;
            hb_compGenPCode3( HB_P_FUNCTION, HB_LOBYTE( usCount ), HB_HIBYTE( usCount  ) );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         {
            USHORT usCount;

            HB_EXPR_USE( pSelf->value.asFunCall.pFunName, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_PUSHNIL );

            if( pSelf->value.asFunCall.pParms )
            {
               usCount = hb_compExprListLen( pSelf->value.asFunCall.pParms );
               if( usCount == 1 && pSelf->value.asFunCall.pParms->value.asList.pExprList->ExprType == HB_ET_NONE )
                  --usCount;
               if( usCount )
                  HB_EXPR_USE( pSelf->value.asFunCall.pParms, HB_EA_PUSH_PCODE );
            }
            else
               usCount = 0;
            hb_compGenPCode3( HB_P_DO, HB_LOBYTE( usCount ), HB_HIBYTE( usCount ) );
         }
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asFunCall.pParms )
            hb_compExprDelete( pSelf->value.asFunCall.pParms );
         hb_compExprDelete( pSelf->value.asFunCall.pFunName );
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
               hb_compExprUseAliasMacro( pSelf, HB_EA_PUSH_PCODE );
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
                hb_compGenPushAliasedVar( pSelf->value.asAlias.pVar->value.asSymbol, TRUE, pAlias->value.asSymbol, 0 );
            }
            else if( pAlias->ExprType == HB_ET_NUMERIC )
            {
               /* numeric alias
                * 2->var
                *
                * NOTE: only integer (long) values are allowed
                */
               if( pAlias->value.asNum.NumType == HB_ET_LONG )
                  hb_compGenPushAliasedVar( pSelf->value.asAlias.pVar->value.asSymbol, TRUE, NULL, pAlias->value.asNum.lVal );
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
               hb_compGenPushAliasedVar( pSelf->value.asAlias.pVar->value.asSymbol, FALSE, NULL, 0 );
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
               hb_compExprUseAliasMacro( pSelf, HB_EA_POP_PCODE );
            }
            else if( pAlias->ExprType == HB_ET_ALIAS )
            {
               /*
                * myalias->var
                * FIELD->var
                * MEMVAR->var
                */
               hb_compGenPopAliasedVar( pSelf->value.asAlias.pVar->value.asSymbol, TRUE, pAlias->value.asSymbol, 0 );
            }
            else if( pAlias->ExprType == HB_ET_NUMERIC )
            {
               /* numeric alias
                * 2->var
                *
                * NOTE: only integer (long) values are allowed
                */
               if( pAlias->value.asNum.NumType == HB_ET_LONG )
                  hb_compGenPopAliasedVar( pSelf->value.asAlias.pVar->value.asSymbol, TRUE, NULL, pAlias->value.asNum.lVal );
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
               hb_compGenPopAliasedVar( pSelf->value.asAlias.pVar->value.asSymbol, FALSE, NULL, 0 );
            }
            else
               hb_compErrorAlias( pAlias );
         }
         break;


      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
         break;

      case HB_EA_DELETE:
         hb_compExprDelete( pSelf->value.asAlias.pAlias );
         /* NOTE: variable name is not released now */
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
            hb_compGenPCode1( HB_P_PUSHALIAS );
            /* push the expression that will return a new workarea
             */
            HB_EXPR_USE( pSelf->value.asAlias.pAlias, HB_EA_PUSH_PCODE );
            /* pop the value from the stack and select it as current workarea
             */
            hb_compGenPCode1( HB_P_POPALIAS );
            /* evaluate any expression
             */
            HB_EXPR_USE( pSelf->value.asAlias.pExpList, HB_EA_PUSH_PCODE );
            /* swap the two last items on the eval stack: one item is a
             * value returned by evaluated expression and the second item
             * is previously selected workarea. After swaping select again
             * the restored workarea.
             */
            hb_compGenPCode1( HB_P_SWAPALIAS );
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
            hb_compGenPCode1( HB_P_PUSHALIAS );
            /* push the expression that will return a new workarea
             */
            HB_EXPR_USE( pSelf->value.asAlias.pAlias, HB_EA_PUSH_PCODE );
            /* pop the value from the stack and select it as current workarea
             */
            hb_compGenPCode1( HB_P_POPALIAS );
            /* evaluate any expression - it will not leave any return
             * value on the eval stack
             */
            HB_EXPR_USE( pSelf->value.asAlias.pExpList, HB_EA_PUSH_POP );
            /* Pop and select again the restored workarea.
             */
            hb_compGenPCode1( HB_P_POPALIAS );
         }
         break;
      case HB_EA_DELETE:
         hb_compExprDelete( pSelf->value.asAlias.pAlias );
         hb_compExprDelete( pSelf->value.asAlias.pExpList );
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
#ifdef HB_MACRO_SUPPORT
         hb_compGenPushSymbol( pSelf->value.asSymbol );
#else
         hb_compGenPushSymbol( pSelf->value.asSymbol, FALSE );
#endif
         break;
      case HB_EA_POP_PCODE:
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
      case HB_EA_DELETE:
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
         hb_compGenPushFunCall( pSelf->value.asSymbol );
         break;
      case HB_EA_POP_PCODE:
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
      case HB_EA_DELETE:
#ifdef HB_MACRO_SUPPORT
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
#ifndef HB_MACRO_SUPPORT
            hb_compGenPushSymbol( pSelf->value.asRTVar.szName, 0 );  /* this is not a function name */
#else
            hb_compGenPushSymbol( pSelf->value.asRTVar.szName );
#endif
         else
            HB_EXPR_USE( pSelf->value.asRTVar.pMacro, HB_EA_PUSH_PCODE );
         break;
      case HB_EA_POP_PCODE:
         if( pSelf->value.asRTVar.szName )
            hb_compGenPopVar( pSelf->value.asRTVar.szName );
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
         hb_compGenPushVar( pSelf->value.asSymbol );
         break;
      case HB_EA_POP_PCODE:
         hb_compGenPopVar( pSelf->value.asSymbol );
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compGenPushVar( pSelf->value.asSymbol );
         hb_compGenPCode1( HB_P_POP );
         break;

      case HB_EA_DELETE:
#ifdef HB_MACRO_SUPPORT
         HB_XFREE( pSelf->value.asSymbol );
#else
         /* NOTE: variable name will be released after pcode generation */
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
            pSelf->value.asMessage.pObject = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asMessage.pObject, HB_EA_REDUCE ) );
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
               hb_compGenMessage( pSelf->value.asMessage.szMessage );
               /* NOTE: if method with no parameters is called then the list
                * of parameters contain only one expression of type HB_ET_NONE
                * There is no need to push this parameter
                */
               if( iParms == 1 && pSelf->value.asMessage.pParms->value.asList.pExprList->ExprType == HB_ET_NONE )
                  --iParms;
               if( iParms )
                  HB_EXPR_USE( pSelf->value.asMessage.pParms, HB_EA_PUSH_PCODE );
               hb_compGenPCode3( HB_P_FUNCTION, HB_LOBYTE( iParms ), HB_HIBYTE( iParms ) );
            }
            else
            {
               /* acces to instance variable */
               HB_EXPR_USE( pSelf->value.asMessage.pObject, HB_EA_PUSH_PCODE );
               hb_compGenMessage( pSelf->value.asMessage.szMessage );
               hb_compGenPCode3( HB_P_FUNCTION, 0, 0 );
            }
         }
         break;

      case HB_EA_POP_PCODE:
         {
            /* NOTE: This is an exception from the rule - this leaves
             *    the return value on the stack
             */
            HB_EXPR_USE( pSelf->value.asMessage.pObject, HB_EA_PUSH_PCODE );
            hb_compGenMessageData( pSelf->value.asMessage.szMessage );
            HB_EXPR_USE( pSelf->value.asMessage.pParms, HB_EA_PUSH_PCODE );
            hb_compGenPCode3( HB_P_FUNCTION, 1, 0 );
         }
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
         if( ! pSelf->value.asMessage.pParms )  /* Is it a method call ? */
         {
            /* instance variable */
            /* QUESTION: This warning can be misleading if nested messages
             * are used, e.g. a:b():c - should we generate it ?
             */
            hb_compWarnMeaningless( pSelf );
         }

      case HB_EA_DELETE:
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePostInc )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ) );
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         hb_compExprPushPostOp( pSelf, HB_P_INC );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         /* a++ used standalone as a statement is the same as ++a
          */
         hb_compExprUsePreOp( pSelf, HB_P_INC );
         break;

      case HB_EA_DELETE:
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePostDec )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ) );
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         hb_compExprPushPostOp( pSelf, HB_P_DEC );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUsePreOp( pSelf, HB_P_DEC );
         break;

      case HB_EA_DELETE:
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseAssign )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ) );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ) );
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
               hb_compGenPCode1( HB_P_DUPLICATE );
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
               hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePlusEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ) );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ) );
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
            hb_compExprPushOperEq( pSelf, HB_P_PLUS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUseOperEq( pSelf, HB_P_PLUS );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMinusEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ) );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ) );
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
            hb_compExprPushOperEq( pSelf, HB_P_MINUS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUseOperEq( pSelf, HB_P_MINUS );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMultEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ) );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ) );
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
            hb_compExprPushOperEq( pSelf, HB_P_MULT );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUseOperEq( pSelf, HB_P_MULT );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseDivEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ) );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ) );
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
            hb_compExprPushOperEq( pSelf, HB_P_DIVIDE );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUseOperEq( pSelf, HB_P_DIVIDE );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseModEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ) );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ) );
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
            hb_compExprPushOperEq( pSelf, HB_P_MODULUS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUseOperEq( pSelf, HB_P_MODULUS );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseExpEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ) );
         pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE ) );
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
            hb_compExprPushOperEq( pSelf, HB_P_POWER );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUseOperEq( pSelf, HB_P_POWER );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseOr )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pLeft, pRight;

            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == HB_ET_LOGICAL && pRight->ExprType == HB_ET_LOGICAL )
            {
               BOOL bResult;

               bResult = pLeft->value.asLogical || pRight->value.asLogical;
               hb_compExprDelete( pLeft );
               hb_compExprDelete( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            else if( pLeft->ExprType == HB_ET_LOGICAL && hb_comp_bShortCuts )
            {
               if( pLeft->value.asLogical )
               {
                  /* .T. .OR. expr => .T.
                   */
                  hb_compExprDelete( pLeft );
                  hb_compExprDelete( pRight );         /* discard expression */
                  pSelf->ExprType = HB_ET_LOGICAL;
                  pSelf->ValType  = HB_EV_LOGICAL;
                  pSelf->value.asLogical = TRUE;
               }
               else
               {
                  /* .F. .OR. expr => expr
                   */
                  hb_compExprDelete( pLeft );
                  pSelf->ExprType = HB_ET_NONE;    /* don't delete expression components */
                  hb_compExprDelete( pSelf );
                  pSelf = pRight;
               }
            }
            else if( pRight->ExprType == HB_ET_LOGICAL && hb_comp_bShortCuts )
            {
               if( pRight->value.asLogical )
               {
                  /* expr .OR. .T. => .T.
                   */
                  hb_compExprDelete( pLeft );         /* discard expression */
                  hb_compExprDelete( pRight );
                  pSelf->ExprType = HB_ET_LOGICAL;
                  pSelf->ValType  = HB_EV_LOGICAL;
                  pSelf->value.asLogical = TRUE;
               }
               else
               {
                  /* expr .OR. .F. => expr
                   */
                  hb_compExprDelete( pRight );
                  pSelf->ExprType = HB_ET_NONE;    /* don't delete expression components */
                  hb_compExprDelete( pSelf );
                  pSelf = pLeft;
               }
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
         if( hb_comp_bShortCuts )
         {
            LONG lEndPos;

            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_DUPLICATE );
            lEndPos = hb_compGenJumpTrue( 0 );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_OR );
            hb_compGenJumpHere( lEndPos );
         }
         else
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_OR );
         }
         break;


      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseAnd )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pLeft, pRight;

            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == HB_ET_LOGICAL && pRight->ExprType == HB_ET_LOGICAL )
            {
               BOOL bResult;

               bResult = pLeft->value.asLogical && pRight->value.asLogical;
               hb_compExprDelete( pLeft );
               hb_compExprDelete( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            else if( pLeft->ExprType == HB_ET_LOGICAL && hb_comp_bShortCuts )
            {
               if( pLeft->value.asLogical )
               {
                  /* .T. .AND. expr => expr
                   */
                  hb_compExprDelete( pLeft );
                  pSelf->ExprType = HB_ET_NONE;    /* don't delete expression components */
                  hb_compExprDelete( pSelf );
                  pSelf = pRight;
               }
               else
               {
                  /* .F. .AND. expr => .F.
                   */
                  hb_compExprDelete( pLeft );
                  hb_compExprDelete( pRight );         /* discard expression */
                  pSelf->ExprType = HB_ET_LOGICAL;
                  pSelf->ValType  = HB_EV_LOGICAL;
                  pSelf->value.asLogical = FALSE;
               }
            }
            else if( pRight->ExprType == HB_ET_LOGICAL && hb_comp_bShortCuts )
            {
               if( pRight->value.asLogical )
               {
                  /* expr .AND. .T. => expr
                   */
                  hb_compExprDelete( pRight );
                  pSelf->ExprType = HB_ET_NONE;    /* don't delete expression components */
                  hb_compExprDelete( pSelf );
                  pSelf = pLeft;
               }
               else
               {
                  /* expr .AND. .F. => .F.
                   */
                  hb_compExprDelete( pLeft );         /* discard expression */
                  hb_compExprDelete( pRight );
                  pSelf->ExprType = HB_ET_LOGICAL;
                  pSelf->ValType  = HB_EV_LOGICAL;
                  pSelf->value.asLogical = FALSE;
               }
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
         if( hb_comp_bShortCuts )
         {
            LONG lEndPos;

            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_DUPLICATE );
            lEndPos = hb_compGenJumpFalse( 0 );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_AND );
            hb_compGenJumpHere( lEndPos );
         }
         else
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_AND );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
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

            pSelf->value.asOperator.pLeft = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ) );
            pExpr = pSelf->value.asOperator.pLeft;

            if( pExpr->ExprType == HB_ET_LOGICAL )
            {
               pExpr->value.asLogical = ! pExpr->value.asLogical;
               pSelf->ExprType = HB_ET_NONE;  /* do not delete operator parameter - we are still using it */
               hb_compExprDelete( pSelf );
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
         hb_compGenPCode1( HB_P_NOT );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelete( pSelf->value.asOperator.pLeft );
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
            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
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
                     hb_compGenPushLogical( pLeft->value.asLogical == pRight->value.asLogical );
                     break;

                  case HB_ET_STRING:
                     /* NOTE: the result depends on SET EXACT setting then it
                     * cannot be optimized except the case when NULL string are
                     * compared - the result is always TRUE regardless of EXACT
                     * setting
                     */
                     if( (pLeft->ulLength | pRight->ulLength) == 0 )
                        hb_compGenPushLogical( TRUE ); /* NOTE: COMPATIBILITY: Clipper doesn't optimize this */
                     else
                     {
                        HB_EXPR_USE( pLeft, HB_EA_PUSH_PCODE );
                        HB_EXPR_USE( pRight, HB_EA_PUSH_PCODE );
                        hb_compGenPCode1( HB_P_EQUAL );
                     }
                     break;

                  case HB_ET_NIL:
                     /* NOTE: COMPATIBILITY: Clipper doesn't optimize this */
                     hb_compGenPushLogical( TRUE ); /* NIL = NIL is always TRUE */
                     break;

                  case HB_ET_NUMERIC:
                     switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
                     {
                        case HB_ET_LONG:
                           hb_compGenPushLogical( pLeft->value.asNum.lVal == pRight->value.asNum.lVal );
                           break;
                        case HB_ET_DOUBLE:
                           hb_compGenPushLogical( pLeft->value.asNum.dVal == pRight->value.asNum.dVal );
                           break;
                        default:
                           {
                              if( pLeft->value.asNum.NumType == HB_ET_LONG )
                                 hb_compGenPushLogical( pLeft->value.asNum.lVal == pRight->value.asNum.dVal );
                              else
                                 hb_compGenPushLogical( pLeft->value.asNum.dVal == pRight->value.asNum.lVal );
                           }
                           break;
                     }
                     break;

                  default:
                     {
                        HB_EXPR_USE( pLeft, HB_EA_PUSH_PCODE );
                        HB_EXPR_USE( pRight, HB_EA_PUSH_PCODE );
                        hb_compGenPCode1( HB_P_EQUAL );
                     }
               }
            else
            {
               /* TODO: check for incompatible types
                */
               HB_EXPR_USE( pLeft, HB_EA_PUSH_PCODE );
               HB_EXPR_USE( pRight, HB_EA_PUSH_PCODE );
               hb_compGenPCode1( HB_P_EQUAL );
            }
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
               hb_compGenPCode1( HB_P_POP );
            }
            else
            {
               HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
               HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
            }
         }
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf );
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
         {
            HB_EXPR_PTR pLeft, pRight;

            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == pRight->ExprType )
            {
               switch( pLeft->ExprType )
               {
                  case HB_ET_LOGICAL:
                     {
                        BOOL bResult = ( pLeft->value.asLogical == pRight->value.asLogical );
                        hb_compExprDelete( pSelf->value.asOperator.pLeft );
                        hb_compExprDelete( pSelf->value.asOperator.pRight );
                        pSelf->ExprType = HB_ET_LOGICAL;
                        pSelf->ValType  = HB_EV_LOGICAL;
                        pSelf->value.asLogical = bResult;
                     }
                     break;

                  case HB_ET_STRING:
                     {
                        BOOL bResult = FALSE;

                        if( pLeft->ulLength == pRight->ulLength )
                           bResult = ( strcmp( pLeft->value.asString, pRight->value.asString ) == 0 );
                        hb_compExprDelete( pSelf->value.asOperator.pLeft );
                        hb_compExprDelete( pSelf->value.asOperator.pRight );
                        pSelf->ExprType = HB_ET_LOGICAL;
                        pSelf->ValType  = HB_EV_LOGICAL;
                        pSelf->value.asLogical = bResult;
                     }
                     break;

                  case HB_ET_NUMERIC:
                     {
                        BOOL bResult;

                        switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
                        {
                           case HB_ET_LONG:
                              bResult = ( pLeft->value.asNum.lVal == pRight->value.asNum.lVal );
                              break;
                           case HB_ET_DOUBLE:
                              bResult = ( pLeft->value.asNum.dVal == pRight->value.asNum.dVal );
                              break;
                           default:
                              {
                                 if( pLeft->value.asNum.NumType == HB_ET_LONG )
                                    bResult = ( pLeft->value.asNum.lVal == pRight->value.asNum.dVal );
                                 else
                                    bResult = ( pLeft->value.asNum.dVal == pRight->value.asNum.lVal );
                              }
                              break;
                        }
                        hb_compExprDelete( pSelf->value.asOperator.pLeft );
                        hb_compExprDelete( pSelf->value.asOperator.pRight );
                        pSelf->ExprType = HB_ET_LOGICAL;
                        pSelf->ValType  = HB_EV_LOGICAL;
                        pSelf->value.asLogical = bResult;
                     }
                     break;
               }
            }
            /* TODO: add checking of incompatible types
            else
            {
            }
            */
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
            hb_compGenPCode1( HB_P_EXACTLYEQUAL );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseLT )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pLeft, pRight;

            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == pRight->ExprType )
               switch( pLeft->ExprType )
               {
                  case HB_ET_LOGICAL:
                     {
                        /* .F. < .T.  = .T.
                        * .T. < .T.  = .F.
                        * .F. < .F.  = .F.
                        * .T. < .F.  = .F.
                        */
                        BOOL bResult = ( ! pLeft->value.asLogical && pRight->value.asLogical );
                        hb_compExprDelete( pSelf->value.asOperator.pLeft );
                        hb_compExprDelete( pSelf->value.asOperator.pRight );
                        pSelf->ExprType = HB_ET_LOGICAL;
                        pSelf->ValType  = HB_EV_LOGICAL;
                        pSelf->value.asLogical = bResult;
                     }
                     break;

                  case HB_ET_NUMERIC:
                     {
                        BOOL bResult;

                        switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
                        {
                           case HB_ET_LONG:
                              bResult = ( pLeft->value.asNum.lVal < pRight->value.asNum.lVal );
                              break;
                           case HB_ET_DOUBLE:
                              bResult = ( pLeft->value.asNum.dVal < pRight->value.asNum.dVal );
                              break;
                           default:
                              {
                                 if( pLeft->value.asNum.NumType == HB_ET_LONG )
                                    bResult = ( pLeft->value.asNum.lVal < pRight->value.asNum.dVal );
                                 else
                                    bResult = ( pLeft->value.asNum.dVal < pRight->value.asNum.lVal );
                              }
                              break;
                        }
                        hb_compExprDelete( pSelf->value.asOperator.pLeft );
                        hb_compExprDelete( pSelf->value.asOperator.pRight );
                        pSelf->ExprType = HB_ET_LOGICAL;
                        pSelf->ValType  = HB_EV_LOGICAL;
                        pSelf->value.asLogical = bResult;
                     }
                     break;

                  default:
                     break;
               }
            /* TODO: add checking of incompatible types
            else
            {
            }
            */
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
            hb_compGenPCode1( HB_P_LESS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseGT )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pLeft, pRight;

            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == pRight->ExprType )
               switch( pLeft->ExprType )
               {
                  case HB_ET_LOGICAL:
                     {
                        /* .T. > .F.  = .T.
                        * .T. > .T.  = .F.
                        * .F. > .F.  = .F.
                        * .F. > .T.  = .F.
                        */
                        BOOL bResult = ( pLeft->value.asLogical && ! pRight->value.asLogical );
                        hb_compExprDelete( pSelf->value.asOperator.pLeft );
                        hb_compExprDelete( pSelf->value.asOperator.pRight );
                        pSelf->ExprType = HB_ET_LOGICAL;
                        pSelf->ValType  = HB_EV_LOGICAL;
                        pSelf->value.asLogical = bResult;
                     }
                     break;

                  case HB_ET_NUMERIC:
                     {
                        BOOL bResult;

                        switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
                        {
                           case HB_ET_LONG:
                              bResult = ( pLeft->value.asNum.lVal > pRight->value.asNum.lVal );
                              break;
                           case HB_ET_DOUBLE:
                              bResult = ( pLeft->value.asNum.dVal > pRight->value.asNum.dVal );
                              break;
                           default:
                              {
                                 if( pLeft->value.asNum.NumType == HB_ET_LONG )
                                    bResult = ( pLeft->value.asNum.lVal > pRight->value.asNum.dVal );
                                 else
                                    bResult = ( pLeft->value.asNum.dVal > pRight->value.asNum.lVal );
                              }
                              break;
                        }
                        hb_compExprDelete( pSelf->value.asOperator.pLeft );
                        hb_compExprDelete( pSelf->value.asOperator.pRight );
                        pSelf->ExprType = HB_ET_LOGICAL;
                        pSelf->ValType  = HB_EV_LOGICAL;
                        pSelf->value.asLogical = bResult;
                     }
                     break;

               }
            /* TODO: add checking of incompatible types
            else
            {
            }
            */
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
            hb_compGenPCode1( HB_P_GREATER );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseLE )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pLeft, pRight;

            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == pRight->ExprType )
               switch( pLeft->ExprType )
               {
                  case HB_ET_LOGICAL:
                     {
                        /* .T. <= .F.  = .F.
                        * .T. <= .T.  = .T.
                        * .F. <= .F.  = .T.
                        * .F. <= .T.  = .T.
                        */
                        BOOL bResult = ! ( pLeft->value.asLogical && ! pRight->value.asLogical );
                        hb_compExprDelete( pSelf->value.asOperator.pLeft );
                        hb_compExprDelete( pSelf->value.asOperator.pRight );
                        pSelf->ExprType = HB_ET_LOGICAL;
                        pSelf->ValType  = HB_EV_LOGICAL;
                        pSelf->value.asLogical = bResult;
                     }
                     break;

                  case HB_ET_NUMERIC:
                     {
                        BOOL bResult;

                        switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
                        {
                           case HB_ET_LONG:
                              bResult = ( pLeft->value.asNum.lVal <= pRight->value.asNum.lVal );
                              break;
                           case HB_ET_DOUBLE:
                              bResult = ( pLeft->value.asNum.dVal <= pRight->value.asNum.dVal );
                              break;
                           default:
                              {
                                 if( pLeft->value.asNum.NumType == HB_ET_LONG )
                                    bResult = ( pLeft->value.asNum.lVal <= pRight->value.asNum.dVal );
                                 else
                                    bResult = ( pLeft->value.asNum.dVal <= pRight->value.asNum.lVal );
                              }
                              break;
                        }
                        hb_compExprDelete( pSelf->value.asOperator.pLeft );
                        hb_compExprDelete( pSelf->value.asOperator.pRight );
                        pSelf->ExprType = HB_ET_LOGICAL;
                        pSelf->ValType  = HB_EV_LOGICAL;
                        pSelf->value.asLogical = bResult;
                     }
                     break;

               }
            /* TODO: add checking of incompatible types
            else
            {
            }
            */
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
            hb_compGenPCode1( HB_P_LESSEQUAL );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseGE )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pLeft, pRight;

            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == pRight->ExprType )
               switch( pLeft->ExprType )
               {
                  case HB_ET_LOGICAL:
                     {
                        /* .T. >= .F.  = .T.
                        * .T. >= .T.  = .T.
                        * .F. >= .F.  = .T.
                        * .F. >= .T.  = .f.
                        */
                        BOOL bResult = ! ( ! pLeft->value.asLogical && pRight->value.asLogical );
                        hb_compExprDelete( pSelf->value.asOperator.pLeft );
                        hb_compExprDelete( pSelf->value.asOperator.pRight );
                        pSelf->ExprType = HB_ET_LOGICAL;
                        pSelf->ValType  = HB_EV_LOGICAL;
                        pSelf->value.asLogical = bResult;
                     }
                     break;

                  case HB_ET_NUMERIC:
                     {
                        BOOL bResult;

                        switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
                        {
                           case HB_ET_LONG:
                              bResult = ( pLeft->value.asNum.lVal >= pRight->value.asNum.lVal );
                              break;
                           case HB_ET_DOUBLE:
                              bResult = ( pLeft->value.asNum.dVal >= pRight->value.asNum.dVal );
                              break;
                           default:
                              {
                                 if( pLeft->value.asNum.NumType == HB_ET_LONG )
                                    bResult = ( pLeft->value.asNum.lVal >= pRight->value.asNum.dVal );
                                 else
                                    bResult = ( pLeft->value.asNum.dVal >= pRight->value.asNum.lVal );
                              }
                              break;
                        }
                        hb_compExprDelete( pSelf->value.asOperator.pLeft );
                        hb_compExprDelete( pSelf->value.asOperator.pRight );
                        pSelf->ExprType = HB_ET_LOGICAL;
                        pSelf->ValType  = HB_EV_LOGICAL;
                        pSelf->value.asLogical = bResult;
                     }
                     break;

               }
            /* TODO: add checking of incompatible types
            else
            {
            }
            */
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
            hb_compGenPCode1( HB_P_GREATEREQUAL );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseNE )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pLeft, pRight;

            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == pRight->ExprType )
               switch( pLeft->ExprType )
               {
                  case HB_ET_LOGICAL:
                     {
                        /* .F. != .T.  = .T.
                        * .T. != .T.  = .F.
                        * .F. != .F.  = .F.
                        * .T. != .F.  = .T.
                        */
                        BOOL bResult = ( pLeft->value.asLogical != pRight->value.asLogical );
                        hb_compExprDelete( pSelf->value.asOperator.pLeft );
                        hb_compExprDelete( pSelf->value.asOperator.pRight );
                        pSelf->ExprType = HB_ET_LOGICAL;
                        pSelf->ValType  = HB_EV_LOGICAL;
                        pSelf->value.asLogical = bResult;
                     }
                     break;

                  case HB_ET_STRING:
                     /* NOTE: the result depends on SET EXACT setting then it
                     * cannot be optimized except the case when NULL string are
                     * compared - "" != "" is always FALSE regardless of EXACT
                     * setting
                     */
                     if( (pLeft->ulLength | pRight->ulLength) == 0 )
                        hb_compGenPushLogical( FALSE ); /* NOTE: COMPATIBILITY: Clipper doesn't optimize this */
                     break;

                  case HB_ET_NUMERIC:
                     {
                        BOOL bResult;

                        switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
                        {
                           case HB_ET_LONG:
                              bResult = ( pLeft->value.asNum.lVal != pRight->value.asNum.lVal );
                              break;
                           case HB_ET_DOUBLE:
                              bResult = ( pLeft->value.asNum.dVal != pRight->value.asNum.dVal );
                              break;
                           default:
                              {
                                 if( pLeft->value.asNum.NumType == HB_ET_LONG )
                                    bResult = ( pLeft->value.asNum.lVal != pRight->value.asNum.dVal );
                                 else
                                    bResult = ( pLeft->value.asNum.dVal != pRight->value.asNum.lVal );
                              }
                              break;
                        }
                        hb_compExprDelete( pSelf->value.asOperator.pLeft );
                        hb_compExprDelete( pSelf->value.asOperator.pRight );
                        pSelf->ExprType = HB_ET_LOGICAL;
                        pSelf->ValType  = HB_EV_LOGICAL;
                        pSelf->value.asLogical = bResult;
                     }
                     break;

               }
            /* TODO: add checking of incompatible types
            else
            {
            }
            */
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
            hb_compGenPCode1( HB_P_NOTEQUAL );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
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
            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );

            if( ( pSelf->value.asOperator.pLeft->ExprType == pSelf->value.asOperator.pRight->ExprType ) && pSelf->value.asOperator.pLeft->ExprType == HB_ET_STRING )
            {
               /* Both arguments are literal strings
                */
               BOOL bResult;

               /* NOTE: CA-Cl*pper has a bug where the $ operator returns .T.
                        when an empty string is searched [vszel] */

               if( pSelf->value.asOperator.pLeft->ulLength == 0 )
                  bResult = TRUE;
               else
                  bResult = ( hb_strAt( pSelf->value.asOperator.pLeft->value.asString, pSelf->value.asOperator.pLeft->ulLength,
                                        pSelf->value.asOperator.pRight->value.asString, pSelf->value.asOperator.pRight->ulLength ) != 0 );

               /* NOTE:
                * "" $ "XXX" = .T.
                * "" $ "" = .T.
                */
               hb_compExprDelete( pSelf->value.asOperator.pLeft );
               hb_compExprDelete( pSelf->value.asOperator.pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            /* TODO: add checking for incompatible types
             */
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
            hb_compGenPCode1( HB_P_INSTRING );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePlus )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pLeft, pRight;

            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC )
            {
               BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

               switch( bType )
               {
                  case HB_ET_LONG:
                  {
                     double dVal = ( double ) pLeft->value.asNum.lVal + ( double ) pRight->value.asNum.lVal;

                     if( ( double ) LONG_MIN <= dVal && dVal <= ( double ) LONG_MAX )
                     {
                        pSelf->value.asNum.lVal = ( long ) dVal;
                        pSelf->value.asNum.bDec = 0;
                        pSelf->value.asNum.NumType = HB_ET_LONG;
                     }
                     else
                     {
                        pSelf->value.asNum.dVal = dVal;
                        pSelf->value.asNum.bDec = 0;
                        pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                     }

                     break;
                  }

                  case HB_ET_DOUBLE:
                  {
                     pSelf->value.asNum.dVal = pLeft->value.asNum.dVal + pRight->value.asNum.dVal;
                     pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                     if( pLeft->value.asNum.bDec < pRight->value.asNum.bDec )
                        pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
                     else
                        pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;

                     break;
                  }

                  default:
                  {
                     if( pLeft->value.asNum.NumType == HB_ET_DOUBLE )
                     {
                        pSelf->value.asNum.dVal = pLeft->value.asNum.dVal + ( double ) pRight->value.asNum.lVal;
                        pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
                     }
                     else
                     {
                        pSelf->value.asNum.dVal = ( double ) pLeft->value.asNum.lVal + pRight->value.asNum.dVal;
                        pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
                     }
                     pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                  }
               }
               pSelf->ExprType = HB_ET_NUMERIC;
               pSelf->ValType  = HB_EV_NUMERIC;
               HB_EXPR_USE( pLeft,  HB_EA_DELETE );
               HB_EXPR_USE( pRight, HB_EA_DELETE );
            }
            else if( pLeft->ExprType == HB_ET_STRING && pRight->ExprType == HB_ET_STRING )
            {
               pSelf->ExprType = HB_ET_NONE; /* suppress deletion of operator components */
               hb_compExprDelete( pSelf );
               if( pRight->ulLength == 0 )
               {
                  pSelf = pLeft;
                  hb_compExprDelete( pRight );
               }
               else if( pLeft->ulLength == 0 )
               {
                  pSelf = pRight;
                  hb_compExprDelete( pLeft );
               }
               else
               {
                  pLeft->value.asString = (char *) hb_xrealloc( pLeft->value.asString, pLeft->ulLength + pRight->ulLength + 1 );
                  memcpy( pLeft->value.asString + pLeft->ulLength,
                     pRight->value.asString, pRight->ulLength );
                  pLeft->ulLength += pRight->ulLength;
                  pLeft->value.asString[ pLeft->ulLength ] = '\0';
                  pSelf = pLeft;
                  hb_compExprDelete( pRight );
               }
            }
            else
            {
               /* TODO: Check for incompatible types e.g. "txt" + 3
               */
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
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_PLUS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMinus )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pLeft, pRight;

            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC )
            {
               BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

               switch( bType )
               {
                  case HB_ET_LONG:
                  {
                     double dVal = ( double ) pLeft->value.asNum.lVal - ( double ) pRight->value.asNum.lVal;

                     if( ( double ) LONG_MIN <= dVal && dVal <= ( double ) LONG_MAX )
                     {
                        pSelf->value.asNum.lVal = ( long ) dVal;
                        pSelf->value.asNum.bDec = 0;
                        pSelf->value.asNum.NumType = HB_ET_LONG;
                     }
                     else
                     {
                        pSelf->value.asNum.dVal = dVal;
                        pSelf->value.asNum.bDec = 0;
                        pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                     }

                     break;
                  }

                  case HB_ET_DOUBLE:
                  {
                     pSelf->value.asNum.dVal = pLeft->value.asNum.dVal - pRight->value.asNum.dVal;
                     pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                     if( pLeft->value.asNum.bDec < pRight->value.asNum.bDec )
                        pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
                     else
                        pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;

                     break;
                  }

                  default:
                  {
                     if( pLeft->value.asNum.NumType == HB_ET_DOUBLE )
                     {
                        pSelf->value.asNum.dVal = pLeft->value.asNum.dVal - ( double ) pRight->value.asNum.lVal;
                        pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
                     }
                     else
                     {
                        pSelf->value.asNum.dVal = ( double ) pLeft->value.asNum.lVal - pRight->value.asNum.dVal;
                        pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
                     }
                     pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                  }
               }
               pSelf->ExprType = HB_ET_NUMERIC;
               pSelf->ValType  = HB_EV_NUMERIC;
               HB_EXPR_USE( pLeft,  HB_EA_DELETE );
               HB_EXPR_USE( pRight, HB_EA_DELETE );
            }
            else if( pLeft->ExprType == HB_ET_STRING && pRight->ExprType == HB_ET_STRING )
            {
               /* TODO:
                  */
            }
            else
            {
               /* TODO: Check for incompatible types e.g. "txt" - 3
               */
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
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_MINUS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMult )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pLeft, pRight;

            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC )
            {
               BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

               switch( bType )
               {
                  case HB_ET_LONG:
                  {
                     double dVal = ( double ) pLeft->value.asNum.lVal * ( double ) pRight->value.asNum.lVal;

                     if( ( double ) LONG_MIN <= dVal && dVal <= ( double ) LONG_MAX )
                     {
                        pSelf->value.asNum.lVal = ( long ) dVal;
                        pSelf->value.asNum.bDec = 0;
                        pSelf->value.asNum.NumType = HB_ET_LONG;
                     }
                     else
                     {
                        pSelf->value.asNum.dVal = dVal;
                        pSelf->value.asNum.bDec = 0;
                        pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                     }

                     break;
                  }

                  case HB_ET_DOUBLE:
                  {
                     pSelf->value.asNum.dVal = pLeft->value.asNum.dVal * pRight->value.asNum.dVal;
                     pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                     pSelf->value.asNum.bDec = pLeft->value.asNum.bDec + pRight->value.asNum.bDec;

                     break;
                  }

                  default:
                  {
                     if( pLeft->value.asNum.NumType == HB_ET_DOUBLE )
                     {
                        pSelf->value.asNum.dVal = pLeft->value.asNum.dVal * ( double ) pRight->value.asNum.lVal;
                        pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
                     }
                     else
                     {
                        pSelf->value.asNum.dVal = ( double ) pLeft->value.asNum.lVal * pRight->value.asNum.dVal;
                        pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
                     }
                     pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                  }
               }
               pSelf->ExprType = HB_ET_NUMERIC;
               pSelf->ValType  = HB_EV_NUMERIC;
               HB_EXPR_USE( pLeft,  HB_EA_DELETE );
               HB_EXPR_USE( pRight, HB_EA_DELETE );
            }
            else
            {
               /* TODO: Check for incompatible types e.g. 3 * "txt"
               */
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
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_MULT );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseDiv )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pLeft, pRight;

            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC )
            {
               BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

               switch( bType )
               {
                  case HB_ET_LONG:

                     if( pRight->value.asNum.lVal )
                     {
                        double dVal = ( double ) pLeft->value.asNum.lVal / ( double ) pRight->value.asNum.lVal;

                        if( fmod( dVal, 1.0 ) == 0.0 )
                        {
                           /* Return integer results as long */
                           pSelf->value.asNum.lVal = ( long ) dVal;
                           pSelf->value.asNum.bDec = 0;
                           pSelf->value.asNum.NumType = HB_ET_LONG;
                        }
                        else
                        {
                           /* Return non-integer results as double */
                           pSelf->value.asNum.dVal = dVal;
                           pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
                           pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                        }
                        pSelf->ExprType = HB_ET_NUMERIC;
                     }
                     break;

                  case HB_ET_DOUBLE:

                     if( pRight->value.asNum.dVal != 0.0 )
                     {
                        pSelf->value.asNum.dVal = pLeft->value.asNum.dVal / pRight->value.asNum.dVal;
                        pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                        pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
                        pSelf->ExprType = HB_ET_NUMERIC;
                     }
                     break;

                  default:

                     if( pLeft->value.asNum.NumType == HB_ET_DOUBLE )
                     {
                        if( pRight->value.asNum.lVal )
                        {
                           pSelf->value.asNum.dVal = pLeft->value.asNum.dVal / ( double ) pRight->value.asNum.lVal;
                           pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
                        }
                     }
                     else
                     {
                        if( pRight->value.asNum.dVal != 0.0 )
                        {
                           pSelf->value.asNum.dVal = ( double ) pLeft->value.asNum.lVal / pRight->value.asNum.dVal;
                           pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
                        }
                     }

                     pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                     pSelf->ExprType = HB_ET_NUMERIC;

               } /* switch bType */

               if( pSelf->ExprType == HB_ET_NUMERIC )
               {
                  /* The expression was reduced - delete old components */
                  pSelf->ValType = HB_EV_NUMERIC;
                  hb_compExprDelete( pLeft );
                  hb_compExprDelete( pRight );
               }
            }
            else
            {
               /* TODO: Check for incompatible types e.g.  3 / "txt"
               */
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
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_DIVIDE );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMod )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         {
            HB_EXPR_PTR pLeft, pRight;

            pSelf->value.asOperator.pLeft  = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_REDUCE ) );
            pSelf->value.asOperator.pRight = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pRight,  HB_EA_REDUCE ) );
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;

            if( pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC )
            {
               if( pLeft->value.asNum.NumType == HB_ET_LONG && pRight->value.asNum.NumType == HB_ET_LONG )
               {
                  if( pRight->value.asNum.lVal )
                  {
                     double dVal = pLeft->value.asNum.lVal % pRight->value.asNum.lVal;

                     if( ( double ) LONG_MIN <= dVal && dVal <= ( double ) LONG_MAX )
                     {
                        pSelf->value.asNum.lVal = ( long ) dVal;
                        pSelf->value.asNum.bDec = 0;
                        pSelf->value.asNum.NumType = HB_ET_LONG;
                     }
                     else
                     {
                        pSelf->value.asNum.dVal = dVal;
                        pSelf->value.asNum.bDec = 0;
                        pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                     }

                     pSelf->ExprType = HB_ET_NUMERIC;
                     pSelf->ValType  = HB_EV_NUMERIC;
                     HB_EXPR_USE( pLeft,  HB_EA_DELETE );
                     HB_EXPR_USE( pRight, HB_EA_DELETE );
                  }
               }
            }
            else
            {
               /* TODO: Check for incompatible types e.g.  3 % "txt"
               */
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
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            hb_compGenPCode1( HB_P_MODULUS );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
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
            hb_compGenPCode1( HB_P_POWER );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
         hb_compExprDelOperator( pSelf );
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

            pSelf->value.asOperator.pLeft = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ) );
            pExpr = pSelf->value.asOperator.pLeft;

            if( pExpr->ExprType == HB_ET_NUMERIC )
            {
               if( pExpr->value.asNum.NumType == HB_ET_DOUBLE )
                  pExpr->value.asNum.dVal = - pExpr->value.asNum.dVal;
               else
                  pExpr->value.asNum.lVal = - pExpr->value.asNum.lVal;
               pSelf->ExprType = HB_ET_NONE;  /* do not delete operator parameter - we are still using it */
               hb_compExprDelete( pSelf );
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
            hb_compGenPCode1( HB_P_NEGATE );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         hb_compGenPCode1( HB_P_POP );
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
            hb_compExprDelete( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePreInc )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ) );
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
         hb_compExprPushPreOp( pSelf, HB_P_INC );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUsePreOp( pSelf, HB_P_INC );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            hb_compExprDelete( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePreDec )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft = hb_compExprListStrip( HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE ) );
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
         hb_compExprPushPreOp( pSelf, HB_P_DEC );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUsePreOp( pSelf, HB_P_DEC );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            hb_compExprDelete( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

/* ************************************************************************* */

#ifdef HB_MACRO_SUPPORT
static void hb_compExprDelOperatorMC( HB_EXPR_PTR pExpr, HB_MACRO_DECL )
#else
static void hb_compExprDelOperator( HB_EXPR_PTR pExpr )
#endif
{
   if( pExpr->value.asOperator.pLeft )
      hb_compExprDelete( pExpr->value.asOperator.pLeft );
   if( pExpr->value.asOperator.pRight )
      hb_compExprDelete( pExpr->value.asOperator.pRight );
}

/* Reduces the list of expressions
 *
 * pExpr is the first expression on the list
 */
#ifdef HB_MACRO_SUPPORT
static ULONG hb_compExprListReduceMC( HB_EXPR_PTR pExpr, HB_MACRO_DECL )
#else
static ULONG hb_compExprListReduce( HB_EXPR_PTR pExpr )
#endif
{
   HB_EXPR_PTR pNext;
   HB_EXPR_PTR * pPrev;
   ULONG ulCnt = 0;

   /* NOTE: During optimalization an expression on the list can be
    * replaced by the new one
    */

   pPrev = &pExpr->value.asList.pExprList;
   pExpr = pExpr->value.asList.pExprList;
   while( pExpr )
   {
      pNext  = pExpr->pNext; /* store next expression in case the current  will be reduced */
      pExpr  = HB_EXPR_USE( pExpr, HB_EA_REDUCE );
      *pPrev = pExpr;   /* store a new expression into the previous one */
      pExpr->pNext = pNext;  /* restore the link to next expression */
      pPrev  = &pExpr->pNext;
      pExpr  = pNext;
      ++ulCnt;
   }
   return ulCnt;
}

/* replace the list containing a single expression with a simple expression
 * - strips parenthesis
 *  ( EXPR ) -> EXPR
 */
#ifdef HB_MACRO_SUPPORT
static HB_EXPR_PTR hb_compExprListStripMC( HB_EXPR_PTR pSelf, HB_MACRO_DECL )
#else
static HB_EXPR_PTR hb_compExprListStrip( HB_EXPR_PTR pSelf )
#endif
{

   if( pSelf->ExprType == HB_ET_LIST )
   {
      ULONG ulCount = hb_compExprListLen( pSelf );

      if( ulCount == 1 && pSelf->value.asList.pExprList->ExprType <= HB_ET_VARIABLE )
      {
         /* replace the list with a simple expression
          *  ( EXPR ) -> EXPR
         */
         HB_EXPR_PTR pExpr = pSelf;

         pSelf = pSelf->value.asList.pExprList;
         pExpr->value.asList.pExprList = NULL;
         hb_compExprDelete( pExpr );
      }
   }
   return pSelf;
}


/* Generates pcodes for compound operators    += -= *= /= %= ^=
 *
 * pExpr is an expression created by hb_compExprNew<operator>Eq functions
 */
#ifdef HB_MACRO_SUPPORT
static void hb_compExprPushOperEqMC( HB_EXPR_PTR pSelf, BYTE bOpEq, HB_MACRO_DECL )
#else
static void hb_compExprPushOperEq( HB_EXPR_PTR pSelf, BYTE bOpEq )
#endif
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
      HB_EXPR_PTR pObj = pSelf->value.asOperator.pLeft;

      /* Push object */
      HB_EXPR_USE( pObj->value.asMessage.pObject, HB_EA_PUSH_PCODE );
      /* Push _message for later use  */
      hb_compGenMessageData( pObj->value.asMessage.szMessage );

      /* Now push current value of variable */
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
      /* push object */
      HB_EXPR_USE( pObj->value.asMessage.pObject, HB_EA_PUSH_PCODE );
#else
      /* NOTE: this duplicate optimization requires that HB_P_MESSAGE
       * reverts items on the stack !
       * duplicate object on the stack
       */
      hb_compGenPCode1( HB_P_DUPLICATE );
#endif
      /* now send the message */
      hb_compGenMessage( pObj->value.asMessage.szMessage );
      hb_compGenPCode3( HB_P_FUNCTION, 0, 0 );

/* NOTE: COMPATIBILITY ISSUE:
 *  The above HARBOUR_STRICT_CLIPPER_COMPATIBILITY setting determines
 * the way the chained send messages are handled.
 * For example, the following code:
 *
 * a:b( COUNT() ):c += 1
 *
 * will be handled as:
 *
 * a:b( COUNT() ):c := a:b( COUNT() ):c + 1
 *
 * in strict Clipper compatibility mode and
 *
 * temp := a:b( COUNT() ), temp:c += 1
 *
 * in non-strict mode.
 *   In practice in Clipper it will call COUNT() function two times: the
 * first time before addition and the second one after addition - in Harbour,
 * COUNT() function will be called only once, before addition.
 * The Harbour (non-strict) method is:
 * 1) faster
 * 2) it guarantees that the same instance variable of the same object will
 *   be changed
 */

      /* push increment value */
      HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
      /* increase operation */
      hb_compGenPCode1( bOpEq );

      /* call pop message with one argument */
      hb_compGenPCode3( HB_P_FUNCTION, 1, 0 );
   }
   /* TODO: add a special code for arrays to correctly handle a[ i++ ]++
    */
   else
   {
      /* push old value */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
      /* push increment value */
      HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
      /* perform operation and duplicate the new value */
      hb_compGenPCode1( bOpEq );
      hb_compGenPCode1( HB_P_DUPLICATE );
      /* pop the new value into variable and leave the copy on the stack */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
   }
}

/* Generates pcodes for <operator>= syntax
 * used standalone as a statement (it cannot leave the value on the stack)
 */
#ifdef HB_MACRO_SUPPORT
static void hb_compExprUseOperEqMC( HB_EXPR_PTR pSelf, BYTE bOpEq, HB_MACRO_DECL )
#else
static void hb_compExprUseOperEq( HB_EXPR_PTR pSelf, BYTE bOpEq )
#endif
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
      HB_EXPR_PTR pObj = pSelf->value.asOperator.pLeft;

      /* Push object */
      HB_EXPR_USE( pObj->value.asMessage.pObject, HB_EA_PUSH_PCODE );
      /* Push _message for later use  */
      hb_compGenMessageData( pObj->value.asMessage.szMessage );

      /* Now push current value of variable */
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
      /* push object */
      HB_EXPR_USE( pObj->value.asMessage.pObject, HB_EA_PUSH_PCODE );
#else
      /* duplicate object on the stack */
      hb_compGenPCode1( HB_P_DUPLICATE );
#endif
      /* now send the message */
      hb_compGenMessage( pObj->value.asMessage.szMessage );
      hb_compGenPCode3( HB_P_FUNCTION, 0, 0 );

      /* push increment value */
      HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
      /* increase operation */
      hb_compGenPCode1( bOpEq );

      /* call pop message with one argument */
      hb_compGenPCode3( HB_P_FUNCTION, 1, 0 );
      /* pop the value from the stack */
      hb_compGenPCode1( HB_P_POP );
   }
   else
   {
      /* push old value */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
      /* push increment value */
      HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
      /* add */
      hb_compGenPCode1( bOpEq );
      /* pop the new value into variable and remove it from the stack */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
   }
}

/* Generates the pcodes for pre- increment/decrement expressions
 */
#ifdef HB_MACRO_SUPPORT
static void hb_compExprPushPreOpMC( HB_EXPR_PTR pSelf, BYTE bOper, HB_MACRO_DECL )
#else
static void hb_compExprPushPreOp( HB_EXPR_PTR pSelf, BYTE bOper )
#endif
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
      HB_EXPR_PTR pObj = pSelf->value.asOperator.pLeft;

      /* Push object */
      HB_EXPR_USE( pObj->value.asMessage.pObject, HB_EA_PUSH_PCODE );
      /* Push _message for later use */
      hb_compGenMessageData( pObj->value.asMessage.szMessage );

      /* Now push current value of variable */
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
      /* push object */
      HB_EXPR_USE( pObj->value.asMessage.pObject, HB_EA_PUSH_PCODE );
#else
      /* duplicate object on the stack */
      hb_compGenPCode1( HB_P_DUPLICATE );
#endif
      /* now send the message */
      hb_compGenMessage( pObj->value.asMessage.szMessage );
      hb_compGenPCode3( HB_P_FUNCTION, 0, 0 );

      /* increase/decrease operation */
      hb_compGenPCode1( bOper );

      /* call pop message with one argument - it leaves the value on the stack */
      hb_compGenPCode3( HB_P_FUNCTION, 1, 0 );
   }
   else
   {
      /* Push current value */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
      /* Increment */
      hb_compGenPCode1( bOper );
      /* duplicate a value */
      hb_compGenPCode1( HB_P_DUPLICATE );
      /* pop new value and leave the duplicated copy of it on the stack */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
   }
}

/* Generates the pcodes for post- increment/decrement expressions
 */
#ifdef HB_MACRO_SUPPORT
static void hb_compExprPushPostOpMC( HB_EXPR_PTR pSelf, BYTE bOper, HB_MACRO_DECL )
#else
static void hb_compExprPushPostOp( HB_EXPR_PTR pSelf, BYTE bOper )
#endif
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
      /* push current value - it will be a result of whole expression */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
      /* now increment the value */
      hb_compExprPushPreOp( pSelf, bOper );
      /* pop the value from the stack */
      hb_compGenPCode1( HB_P_POP );
   }
   else
   {
      /* Push current value */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
      /* Duplicate value */
      hb_compGenPCode1( HB_P_DUPLICATE );
      /* Increment */
      hb_compGenPCode1( bOper );
      /* pop new value from the stack */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
   }
}

/* Generates the pcodes for increment/decrement operations
 * used standalone as a statement
 */
#ifdef HB_MACRO_SUPPORT
static void hb_compExprUsePreOpMC( HB_EXPR_PTR pSelf, BYTE bOper, HB_MACRO_DECL )
#else
static void hb_compExprUsePreOp( HB_EXPR_PTR pSelf, BYTE bOper )
#endif
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
      hb_compExprPushPreOp( pSelf, bOper );
      /* pop the value from the stack */
      hb_compGenPCode1( HB_P_POP );
   }
   else
   {
      /* Push current value */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
      /* Increment */
      hb_compGenPCode1( bOper );
      /* pop new value from the stack */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
   }
}

/* Generate pcode for aliased expression which contains macro operator on
 * the left or right side of the alias operator
 * expression->&macro or &macro->expression or &macro->&macro
 */
#ifdef HB_MACRO_SUPPORT
static void hb_compExprUseAliasMacroMC( HB_EXPR_PTR pAliasedVar, BYTE bAction, HB_MACRO_DECL )
#else
static void hb_compExprUseAliasMacro( HB_EXPR_PTR pAliasedVar, BYTE bAction )
#endif
{
   HB_EXPR_PTR pAlias, pVar;

   /* Alias->Var
    */
   pAlias = pAliasedVar->value.asAlias.pAlias;
   pVar   = pAliasedVar->value.asAlias.pVar;
   if( pAlias->ExprType == HB_ET_ALIAS )
   {
      /* database alias */
      /* Push alias identifier as string so it can be joined with
       * variable at runtime
       * NOTE:
       *    ALIAS->&var is the same as &( "ALIAS->" + var )
       *
       */
      hb_compGenPushString( pAlias->value.asSymbol, strlen(pAlias->value.asSymbol) );
      HB_EXPR_USE( pVar, HB_EA_PUSH_PCODE );
      if( bAction == HB_EA_PUSH_PCODE )
         hb_compGenPCode1( HB_P_MACROPUSHALIASED );
      else
         hb_compGenPCode1( HB_P_MACROPOPALIASED );
   }
   else if( pVar->ExprType == HB_ET_VARIABLE )
   {
      /* NOTE:
       *    &macro->var is the  same as: &( macro + "->var" )
       */
      HB_EXPR_USE( pAlias, HB_EA_PUSH_PCODE );
      hb_compGenPushString( pVar->value.asSymbol, strlen(pVar->value.asSymbol) );
      if( bAction == HB_EA_PUSH_PCODE )
         hb_compGenPCode1( HB_P_MACROPUSHALIASED );
      else
         hb_compGenPCode1( HB_P_MACROPOPALIASED );
   }
   else
   {
      HB_EXPR_USE( pAlias, HB_EA_PUSH_PCODE );
      HB_EXPR_USE( pVar, HB_EA_PUSH_PCODE );
      if( bAction == HB_EA_PUSH_PCODE )
         hb_compGenPCode1( HB_P_MACROPUSHALIASED );
      else
         hb_compGenPCode1( HB_P_MACROPOPALIASED );
   }

}

static BOOL hb_compExprCheckMacroVar( char * szText )
{
   char * pTmp = szText;
   BOOL bTextSubst = FALSE;

   while( ( pTmp = strchr( pTmp, '&' ) ) )
   {
      /* Check if macro operator is used inside a string
       * Macro operator is ignored if it is the last char or
       * next char is '(' e.g. "this is &(ignored)"
       *
       * NOTE: This uses _a-zA-Z pattern to check for
       * variable name beginning
       */

      ++pTmp;
      bTextSubst = ( *pTmp == '_' || (*pTmp >= 'A' && *pTmp <= 'Z') || (*pTmp >= 'a' && *pTmp <= 'z') );
#ifndef HB_MACRO_SUPPORT
      /* NOTE: All variables are assumed memvars in macro compiler -
       * there is no need to check for a valid name
       */
      if( bTextSubst )
      {
         /* There is a valid character after '&' that can be used in
          * variable name - check if the whole variable name is valid
          * (local, static and field  variable names are invalid because
          * they are not visible at runtime)
          */
         char * pStart = pTmp;
         char cSave;

         /* NOTE: This uses _a-zA-Z0-9 pattern to check for
          * variable name
          */
         while( *pTmp && (*pTmp == '_' || (*pTmp >= 'A' && *pTmp <= 'Z') || (*pTmp >= 'a' && *pTmp <= 'z') || (*pTmp >= '0' && *pTmp <= '9')) )
            ++pTmp;

         cSave = *pTmp;
         *pTmp = '\0';
         hb_compVariableMacroCheck( pStart );
         *pTmp = cSave;
      }
#endif
   }
   return bTextSubst;
}

/* ************************************************************************* */

/* Create a new declaration for codeblock local variable
 */
static HB_CBVAR_PTR hb_compExprCBVarNew( char * szVarName, BYTE bType )
{
   HB_CBVAR_PTR pVar = ( HB_CBVAR_PTR ) HB_XGRAB( sizeof( HB_CBVAR ) );

   pVar->szName = szVarName;
   pVar->bType  = bType;
   pVar->pNext  = NULL;

   return pVar;
}

/* NOTE: This deletes all linked variable
 */
static void hb_compExprCBVarDel( HB_CBVAR_PTR pVars )
{
   HB_CBVAR_PTR pDel;

   while( pVars )
   {
      pDel  = pVars;
      pVars = pVars->pNext;
#ifdef HB_MACRO_SUPPORT
      HB_XFREE( pDel->szName );
#endif
      HB_XFREE( pDel );
   }
}
