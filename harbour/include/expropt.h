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

#ifndef HB_EXPROPT_H_
#define HB_EXPROPT_H_

#include "extend.h"

typedef struct HB_EXPR_
{
   union
   {
      char *asString;      /* literal strings */
      char *asSymbol;      /* variable name */
      BOOL asLogical;      /* logical value */
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
#define  HB_EXPR_USE( pSelf, iMessage )  \
         s_ExprTable[ (pSelf)->ExprType ]( (pSelf), (iMessage), pMacro )

typedef  HB_EXPR_PTR HB_EXPR_ACTION( HB_EXPR_PTR pSelf, int iMessage, void * pMacro );

#else

#define  HB_EXPR_FUNC( proc )  HB_EXPR_PTR proc( HB_EXPR_PTR pSelf, int iMessage )
typedef  HB_EXPR_FUNC( HB_EXPR_FUNC_ );
typedef  HB_EXPR_FUNC_ *HB_EXPR_FUNC_PTR;
#define  HB_EXPR_USE( pSelf, iMessage )  \
         s_ExprTable[ (pSelf)->ExprType ]( (pSelf), (iMessage) )

typedef  HB_EXPR_PTR HB_EXPR_ACTION( HB_EXPR_PTR pSelf, int iMessage );

#endif


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
HB_EXPR_PTR hb_compExprNewSymbol( char * );
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

#ifdef HB_MACRO_SUPPORT

HB_EXPR_PTR hb_compExprNewArrayAt( HB_EXPR_PTR, HB_EXPR_PTR, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprSetOperand( HB_EXPR_PTR, HB_EXPR_PTR, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprGenPop( HB_EXPR_PTR, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprGenPush( HB_EXPR_PTR, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprGenStatement( HB_EXPR_PTR, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprNewFunCall( HB_EXPR_PTR, HB_EXPR_PTR, HB_MACRO_DECL );
void hb_compExprDelete( HB_EXPR_PTR, HB_MACRO_DECL );
HB_EXPR_PTR hb_compExprCBVarAdd( HB_EXPR_PTR, char *, HB_MACRO_DECL );

#else

HB_EXPR_PTR hb_compExprNewArrayAt( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprSetOperand( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprGenPop( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprGenPush( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprGenStatement( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewFunCall( HB_EXPR_PTR, HB_EXPR_PTR );
void hb_compExprDelete( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprCBVarAdd( HB_EXPR_PTR, char *, BYTE );

#endif

#endif  /* HB_EXPROPT_H_ */
