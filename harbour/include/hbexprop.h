/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Harbour Compiler
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

#ifndef HB_EXPROP_H_
#define HB_EXPROP_H_

#include "hbapi.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

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
#define  HB_ET_MACRO_EXPR     4   /* &( expr ) */

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


typedef struct HB_EXPR_
{
   union
   {
      char *asString;      /* literal strings */
      char *asSymbol;      /* variable name */
      BOOL asLogical;      /* logical value */
      struct
      {
        struct HB_EXPR_ *pMacro;  /* macro variable */
        char *szName;             /* variable name  */
      } asRTVar;      /* PUBLIC or PRIVATE variable declaration */
      struct
      {
         long lVal;           /* long value */
         double dVal;         /* double value */
         unsigned char bDec;  /* unsigned char used intentionally */
         unsigned char NumType;    /* used to distinguish LONG and DOUBLE */
      } asNum;
      struct
      {
         unsigned char cMacroOp;       /* macro operator */
         unsigned char SubType;        /* context in which macro is used */
         char * szMacro;               /* identifier after the macro operator */
         struct HB_EXPR_ *pExprList;   /* list elements if &(...) was used */
      } asMacro;
      struct
      {
         struct HB_EXPR_ *pExprList;    /* list elements */
         struct HB_EXPR_ *pIndex;       /* array index, others */
      } asList;
      struct
      {
         struct HB_EXPR_ *pAlias;      /* alias expression */
         struct HB_EXPR_ *pVar;        /* aliased variable or macro */
         struct HB_EXPR_ *pExpList;    /* aliased expression list */
      } asAlias;
      struct
      {
         struct HB_EXPR_ *pFunName;     /* function name */
         struct HB_EXPR_ *pParms;       /* function call parameters */
      } asFunCall;
      struct
      {
         struct HB_EXPR_ *pObject;     /* object */
         char * szMessage;             /* message */
         struct HB_EXPR_ *pParms;      /* method parameters */
      } asMessage;
      struct
      {
         struct HB_EXPR_ *pLeft;       /* object */
         struct HB_EXPR_ *pRight;      /* object */
      } asOperator;
   } value;
   ULONG ulLength;
   unsigned char ExprType;  /* internal expression type */
   USHORT ValType;          /* language level value type */
   struct HB_EXPR_ *pNext;  /* next expression in the list of expressions */
} HB_EXPR, *HB_EXPR_PTR;

/* Definitions of function templates used in expression's message
 * handling
 */
#ifdef HB_MACRO_SUPPORT
/* Compilation for macro compiler
 */
#define  HB_EXPR_FUNC( proc )  HB_EXPR_PTR proc( HB_EXPR_PTR pSelf, int iMessage, void * pMacro )
typedef  HB_EXPR_FUNC( HB_EXPR_FUNC_ );
typedef  HB_EXPR_FUNC_ *HB_EXPR_FUNC_PTR;

extern HB_EXPR_FUNC_PTR hb_comp_ExprTable[];

#define  HB_EXPR_USE( pSelf, iMessage )  \
         hb_comp_ExprTable[ (pSelf)->ExprType ]( (pSelf), (iMessage), pMacro )

typedef  HB_EXPR_PTR HB_EXPR_ACTION( HB_EXPR_PTR pSelf, int iMessage, void * pMacro );

#define HB_EXPR_PCODE0( action ) action( pMacro )
#define HB_EXPR_PCODE1( action, p1 ) action( (p1), pMacro )
#define HB_EXPR_PCODE2( action, p1, p2 ) action( (p1), (p2), pMacro )
#define HB_EXPR_PCODE3( action, p1, p2, p3 ) action( (p1), (p2), (p3), pMacro )
#define HB_EXPR_PCODE4( action, p1, p2, p3, p4 ) action( (p1), (p2), (p3), (p4), pMacro )

#define HB_MACRO_VARNAME pMacro

#else

#define  HB_EXPR_FUNC( proc )  HB_EXPR_PTR proc( HB_EXPR_PTR pSelf, int iMessage )
typedef  HB_EXPR_FUNC( HB_EXPR_FUNC_ );
typedef  HB_EXPR_FUNC_ *HB_EXPR_FUNC_PTR;

extern HB_EXPR_FUNC_PTR hb_comp_ExprTable[];

#define  HB_EXPR_USE( pSelf, iMessage )  \
         hb_comp_ExprTable[ (pSelf)->ExprType ]( (pSelf), (iMessage) )

typedef  HB_EXPR_PTR HB_EXPR_ACTION( HB_EXPR_PTR pSelf, int iMessage );

#define HB_EXPR_PCODE0( action ) action( )
#define HB_EXPR_PCODE1( action, p1 ) action( (p1) )
#define HB_EXPR_PCODE2( action, p1, p2 ) action( (p1), (p2) )
#define HB_EXPR_PCODE3( action, p1, p2, p3 ) action( (p1), (p2), (p3) )
#define HB_EXPR_PCODE4( action, p1, p2, p3, p4 ) action( (p1), (p2), (p3), (p4) )

/* pass NULL instead of macro structure pointer */
#define HB_MACRO_DECL void *pMacro
#define HB_MACRO_PARAM NULL
#define HB_MACRO_VARNAME pMacro
#endif


HB_EXPR_PTR hb_compExprNew( int );
HB_EXPR_PTR hb_compExprNewEmpty( void );
HB_EXPR_PTR hb_compExprNewNil( void );
HB_EXPR_PTR hb_compExprNewDouble( double, BYTE );
HB_EXPR_PTR hb_compExprNewLong( LONG );
HB_EXPR_PTR hb_compExprNewString( char * );
HB_EXPR_PTR hb_compExprNewLogical( int );
HB_EXPR_PTR hb_compExprNewSelf( void );
HB_EXPR_PTR hb_compExprNewCodeBlock( void );
HB_EXPR_PTR hb_compExprNewArray( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewVar( char * );
HB_EXPR_PTR hb_compExprNewAliasVar( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewAliasExpr( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewMacro( HB_EXPR_PTR, unsigned char, char * );
HB_EXPR_PTR hb_compExprNewFunName( char * );
HB_EXPR_PTR hb_compExprNewRTVar( char *, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewAlias( char * );
HB_EXPR_PTR hb_compExprNewEQ( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewNE( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewLT( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewLE( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewGT( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewGE( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewIN( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPlus( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewMinus( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewMult( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewDiv( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewMod( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPower( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewAssign( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewEqual( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPlusEq( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewMinusEq( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewMultEq( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewDivEq( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewModEq( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewExpEq( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPostInc( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPostDec( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPreInc( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPreDec( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewAnd( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewOr( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewNot( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewNegate( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewVarRef( char * );
HB_EXPR_PTR hb_compExprNewFunRef( char * );
HB_EXPR_PTR hb_compExprNewCodeblockExpr( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewFunCallArg( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewSend( HB_EXPR_PTR, char * );
HB_EXPR_PTR hb_compExprNewMethodCall( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewList( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewArgList( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprAddListExpr( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewIIF( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprReduce( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprAssign( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprEqual( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprAssignStatic( HB_EXPR_PTR, HB_EXPR_PTR );
ULONG hb_compExprListLen( HB_EXPR_PTR );
void hb_compExprClear( HB_EXPR_PTR );
char * hb_compExprDescription( HB_EXPR_PTR );

void hb_compExprFree( HB_EXPR_PTR, HB_MACRO_DECL );
void hb_compExprErrorType( HB_EXPR_PTR, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprListStrip( HB_EXPR_PTR, HB_MACRO_DECL );
BOOL hb_compExprCheckMacroVar( char * );
void hb_compExprCBVarDel( HB_CBVAR_PTR );

#ifdef HB_MACRO_SUPPORT

HB_EXPR_PTR hb_compExprNewArrayAt( HB_EXPR_PTR, HB_EXPR_PTR, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprSetOperand( HB_EXPR_PTR, HB_EXPR_PTR, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprGenPop( HB_EXPR_PTR, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprGenPush( HB_EXPR_PTR, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprGenStatement( HB_EXPR_PTR, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprNewFunCall( HB_EXPR_PTR, HB_EXPR_PTR, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprCBVarAdd( HB_EXPR_PTR, char *, HB_MACRO_DECL );
void hb_compExprDelete( HB_EXPR_PTR, HB_MACRO_DECL );

#else

HB_EXPR_PTR hb_compExprNewArrayAt( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprSetOperand( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprGenPop( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprGenPush( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprGenStatement( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewFunCall( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprCBVarAdd( HB_EXPR_PTR, char *, BYTE );
void hb_compExprDelete( HB_EXPR_PTR );

#endif

#if defined(HB_EXTERN_C)
}
#endif

#endif  /* HB_EXPROP_H_ */
