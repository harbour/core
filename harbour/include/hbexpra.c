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


#include <math.h>
#include "hbcomp.h"

/* memory allocation
 */

/* Table with operators precedence
 * NOTE:
 *    HB_ET_NIL is used for an ordinary values and post- operators
 *    HB_ET_NONE is used for invalid syntax, e.g. var := var1 += 2
 */
static BYTE s_PrecedTable[ HB_EXPR_COUNT ] = {
   HB_ET_NIL,                 /*   HB_ET_NONE = 0,    */
   HB_ET_NIL,                 /*   HB_ET_NIL,         */
   HB_ET_NIL,                 /*   HB_ET_NUMERIC,     */
   HB_ET_NIL,                 /*   HB_ET_DATE,        */
   HB_ET_NIL,                 /*   HB_ET_STRING,      */
   HB_ET_NIL,                 /*   HB_ET_CODEBLOCK,   */
   HB_ET_NIL,                 /*   HB_ET_LOGICAL,     */
   HB_ET_NIL,                 /*   HB_ET_SELF,        */
   HB_ET_NIL,                 /*   HB_ET_ARRAY,       */
   HB_ET_NIL,                 /*   HB_ET_VARREF,      */
   HB_ET_NIL,                 /*   HB_ET_REFERENCE,   */
   HB_ET_NIL,                 /*   HB_ET_FUNREF,      */
   HB_ET_NIL,                 /*   HB_ET_IIF,         */
   HB_ET_NIL,                 /*   HB_ET_LIST,        */
   HB_ET_NIL,                 /*   HB_ET_ARGLIST,     */
   HB_ET_NIL,                 /*   HB_ET_MACROARGLIST,*/
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

static HB_CBVAR_PTR hb_compExprCBVarNew( char *, BYTE );

/* ************************************************************************ */

#if !defined( HB_MACRO_SUPPORT )
static HB_EXPR_PTR hb_compExprAlloc( HB_COMP_DECL )
{
   PHB_EXPRLST pExpItm = ( PHB_EXPRLST ) hb_xgrab( sizeof( HB_EXPRLST ) );

   pExpItm->pNext = HB_COMP_PARAM->pExprLst;
   HB_COMP_PARAM->pExprLst = pExpItm;
   if( pExpItm->pNext )
   {
      pExpItm->pPrev = pExpItm->pNext->pPrev;
      pExpItm->pNext->pPrev = pExpItm;
      pExpItm->pPrev->pNext = pExpItm;
   }
   else
      pExpItm->pPrev = pExpItm->pNext = pExpItm;

   return &pExpItm->Expression;
}

static void hb_compExprDealloc( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   if( HB_COMP_PARAM->pExprLst )
   {
      PHB_EXPRLST pExpItm = ( PHB_EXPRLST ) pExpr;

      pExpItm->pNext->pPrev = pExpItm->pPrev;
      pExpItm->pPrev->pNext = pExpItm->pNext;
      if( pExpItm == HB_COMP_PARAM->pExprLst )
      {
         if( pExpItm->pNext == pExpItm )
            HB_COMP_PARAM->pExprLst = NULL;
         else
            HB_COMP_PARAM->pExprLst = pExpItm->pNext;
      }
      hb_xfree( pExpItm );
   }
}

void hb_compExprLstDealloc( HB_COMP_DECL )
{
   if( HB_COMP_PARAM->pExprLst )
   {
      PHB_EXPRLST pExpItm, pExp;
      pExpItm = pExp = HB_COMP_PARAM->pExprLst;
      HB_COMP_PARAM->pExprLst = NULL;
      do
      {
         hb_compExprDelete( &pExp->Expression, HB_COMP_PARAM );
         pExp = pExp->pNext;
      }
      while( pExp != pExpItm );
      do
      {
         PHB_EXPRLST pFree = pExp;
         pExp = pExp->pNext;
         hb_xfree( pFree );
      }
      while( pExp != pExpItm );
   }
}

#endif

HB_EXPR_PTR hb_compExprNew( HB_EXPRTYPE iType, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNew(%i,%p)", iType, HB_COMP_PARAM));

#if defined( HB_MACRO_SUPPORT )
   pExpr = hb_macroExprNew( HB_COMP_PARAM );
#else
   pExpr = hb_compExprAlloc( HB_COMP_PARAM );
#endif
   pExpr->ExprType = iType;
   pExpr->pNext    = NULL;
   pExpr->ValType  = HB_EV_UNKNOWN;
   pExpr->Counter  = 1;

   return pExpr;
}

/* Delete self - all components will be deleted somewhere else
 */
void hb_compExprClear( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   if( --pExpr->Counter == 0 )
#if defined( HB_MACRO_SUPPORT )
      pExpr->ExprType = HB_ET_NONE;
   HB_SYMBOL_UNUSED( HB_COMP_PARAM );
#else
      hb_compExprDealloc( pExpr, HB_COMP_PARAM );
#endif
}

/* Delete all components and delete self
 */
void hb_compExprDelete( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprDelete(%p,%p)", pExpr, HB_COMP_PARAM));
   if( pExpr && --pExpr->Counter == 0 )
   {
      HB_EXPR_USE( pExpr, HB_EA_DELETE );
#if defined( HB_MACRO_SUPPORT )
      pExpr->ExprType = HB_ET_NONE;
#else
      hb_compExprDealloc( pExpr, HB_COMP_PARAM );
#endif
   }
}

/* Delete all components and delete self
 */
void hb_compExprFree( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprFree()"));
   if( --pExpr->Counter == 0 )
   {
      HB_EXPR_USE( pExpr, HB_EA_DELETE );
#if defined( HB_MACRO_SUPPORT )
      pExpr->ExprType = HB_ET_NONE;
#else
      hb_compExprDealloc( pExpr, HB_COMP_PARAM );
#endif
   }
}

void hb_compExprErrorType( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprErrorType()"));
   hb_compErrorType( HB_COMP_PARAM, pExpr );
   HB_SYMBOL_UNUSED( pExpr );
   HB_SYMBOL_UNUSED( HB_COMP_PARAM );
}

#ifndef HB_MACRO_SUPPORT
ULONG hb_compExprListEval( HB_COMP_DECL, HB_EXPR_PTR pExpr, HB_CARGO_FUNC_PTR pEval )
{
   ULONG ulLen = 0;
  
   if( pEval && ((pExpr->ExprType == HB_ET_LIST) || (pExpr->ExprType == HB_ET_ARGLIST)) )
   {
      pExpr = pExpr->value.asList.pExprList;
      while( pExpr )
      {
         (pEval)( HB_COMP_PARAM, (void *) pExpr );
         pExpr = pExpr->pNext;
         ++ulLen;
      }
   }
   return ulLen;
}

ULONG hb_compExprListEval2( HB_COMP_DECL, HB_EXPR_PTR pExpr1, HB_EXPR_PTR pExpr2, HB_CARGO2_FUNC_PTR pEval )
{
   ULONG ulLen = 0;

   if( !pEval )
      return ulLen;  
      
   if( (pExpr1->ExprType == HB_ET_LIST || pExpr1->ExprType == HB_ET_ARGLIST) 
       && 
       (pExpr2->ExprType == HB_ET_LIST || pExpr2->ExprType == HB_ET_ARGLIST) )
   {
      pExpr1 = pExpr1->value.asList.pExprList;
      pExpr2 = pExpr2->value.asList.pExprList;
      while( pExpr1 && pExpr2 )
      {
         (pEval)( HB_COMP_PARAM, (void *) pExpr1, (void *)pExpr2 );
         pExpr1 = pExpr1->pNext;
         pExpr2 = pExpr2->pNext;
         ++ulLen;
      }
   }
   else if( pExpr1->ExprType == HB_ET_LIST || pExpr1->ExprType == HB_ET_ARGLIST)
   {
      pExpr1 = pExpr1->value.asList.pExprList;
      while( pExpr1 )
      {
         (pEval)( HB_COMP_PARAM, (void *) pExpr1, (void *)pExpr2 );
         pExpr1 = pExpr1->pNext;
         ++ulLen;
      }
   }
   return ulLen;
}
#endif

/* Add a new local variable declaration
 */
HB_EXPR_PTR hb_compExprCBVarAdd( HB_EXPR_PTR pCB, char * szVarName, BYTE bType,
                                 HB_COMP_DECL )
{
   HB_CBVAR_PTR pVar;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprCBVarAdd(%s)", szVarName));

   if( pCB->value.asCodeblock.pLocals )
   {
      /* add it to the end of the list
      */
      pVar = pCB->value.asCodeblock.pLocals;
      while( pVar )
      {
         if( strcmp( szVarName, pVar->szName ) == 0 )
            hb_compErrorDuplVar( HB_COMP_PARAM, szVarName );

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

/* Create function call
 */
HB_EXPR_PTR hb_compExprNewFunCall( HB_EXPR_PTR pName, HB_EXPR_PTR pParms, HB_COMP_DECL )
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

      iCount = ( int ) hb_compExprParamListLen( pParms );

      /* TODO: EMPTY() (not done by Clipper) */
	if( iCount && strcmp( "EVAL", pName->value.asSymbol ) == 0 )
      {
         HB_EXPR_PTR pEval;
         /* Optimize Eval( bBlock, [ArgList] ) to: bBlock:Eval( [ArgList] ) */
         pEval = hb_compExprNewMethodCall( 
            hb_compExprNewSend( pParms->value.asList.pExprList, "EVAL", NULL, HB_COMP_PARAM ),
            hb_compExprNewArgList( pParms->value.asList.pExprList->pNext, HB_COMP_PARAM ) );
         pParms->value.asList.pExprList = NULL;
         HB_EXPR_PCODE1( hb_compExprDelete, pParms );
         HB_EXPR_PCODE1( hb_compExprDelete, pName ); 
         return pEval;
      }
      else if( iCount && strcmp( "_GET_", pName->value.asSymbol ) == 0 )
      {
         /* Reserved Clipper function used to handle GET variables
          */
         HB_EXPR_PTR pArg, pNext;
         USHORT uiCount;

         hb_compExprReduceList( pParms, HB_COMP_PARAM );
         pArg = pParms->value.asList.pExprList;

         if( pArg->ExprType == HB_ET_LIST )
         {
            pNext = pArg->pNext;
            pArg->pNext = NULL;
            pArg = pParms->value.asList.pExprList = hb_compExprListStrip( pArg, HB_COMP_PARAM );
            pArg->pNext = pNext;
         }

         if( pArg->ExprType == HB_ET_ARRAYAT )
         {
            /* replace:
               _GET_( a[1], "a[1]", , , )
               into:
               __GETA( {||a }, "a", , , , { 1 } )
            */
            HB_EXPR_PTR pIndex, pVar;
            HB_EXPR_PTR pBase;

            pName->value.asSymbol = "__GETA";
            /* NOTE: a[ i, j ] is stored as: (pExprList)->(pIndex)
             * ((a->[ i ])->[ j ])
             */
            pVar = HB_EXPR_USE( pArg->value.asList.pExprList, HB_EA_REDUCE );
            pBase = pVar->ExprType == HB_ET_ARRAYAT ? pVar : NULL;
            pIndex = HB_EXPR_USE( pArg->value.asList.pIndex, HB_EA_REDUCE );
            pIndex->pNext = NULL;
            while( pVar->ExprType == HB_ET_ARRAYAT )
            {
               /* traverse back to a leftmost expression and build a list
                * of index expressions
                */
               pVar->value.asList.pIndex->pNext = pIndex;
               pIndex = pVar->value.asList.pIndex;
               pVar = pVar->value.asList.pExprList;
            }

            /* create a set only codeblock */
            if( pVar->ExprType == HB_ET_MACRO )
            {
               /* &var[1] */
               hb_compExprFree( pVar, HB_COMP_PARAM );
               pVar = hb_compExprNewNil( HB_COMP_PARAM );
            }
            else
            {
               pVar = hb_compExprAddCodeblockExpr( hb_compExprNewCodeBlock( NULL, 0, 0, HB_COMP_PARAM ), pVar );
            }

            /* pVar will be the first argument now
             */
            pParms->value.asList.pExprList = pVar;
            /* link the rest of parameters
             */
            pVar->pNext = pArg->pNext;
            /* Delete an argument that was the first one
             */
            pArg->value.asList.pIndex = NULL;
            pArg->value.asList.pExprList = NULL;
            hb_compExprClear( pArg, HB_COMP_PARAM );
            /* Create an array with index elements
             */
            pIndex = HB_EXPR_PCODE1( hb_compExprNewArray, hb_compExprNewList( pIndex, HB_COMP_PARAM ) );
            /* The array with index elements have to be the sixth argument
             * of __GETA() call
             */
            uiCount = 1;
            while( ++uiCount < 6 )
            {
               if( pVar->pNext == NULL )
                  pVar->pNext = hb_compExprNewNil( HB_COMP_PARAM );
               pVar = pVar->pNext;
            }
            if( pVar->pNext ) /* Delete 6-th argument if present */
            {
               pIndex->pNext = pVar->pNext->pNext;
               HB_EXPR_PCODE1( hb_compExprDelete, pVar->pNext );
            }
            pVar->pNext = pIndex;   /* Set a new 6-th argument */

            /* Remove the index expression from a string representation
             */
            pVar = pParms->value.asList.pExprList->pNext;
            if( pVar->ExprType == HB_ET_STRING )
            {
               ULONG i = 0;
               char *szVar = pVar->value.asString.string;

               /* NOTE: Clipper strips a string at the first '[' character too
                */
               while( ++i < pVar->ulLength )
               {
                  if( szVar[ i ] == '[' )
                  {
                     szVar[ i ] = 0;
                     pVar->ulLength = i;
                     break;
                  }
               }
            }
            /* clear expressions no longer used */
            if( pBase )
            {
               while( pBase->ExprType == HB_ET_ARRAYAT )
               {
                  pVar = pBase->value.asList.pExprList;
                  pBase->value.asList.pExprList = NULL;
                  hb_compExprClear( pBase, HB_COMP_PARAM );
                  pBase = pVar;
               }
            }
         }
         else if( pArg->ExprType == HB_ET_MACRO )
         {
            /* @ 0,0 GET &var    => __GET( NIL, var,... )
             * @ 0,0 GET var&var => __GET( NIL, "var&var",... )
             */
            pName->value.asSymbol = "__GET";
            if( pArg->value.asMacro.pExprList == NULL )
            {
               /* Simple macro expansion (not a parenthesized expressions)
                */
               HB_EXPR_PTR pFirst;

               pFirst = pArg;                /* first argument  */
               pNext  = pFirst->pNext;       /* second argument */
               if( pNext )
                  pNext = pNext->pNext;      /* third argument */

               pArg = hb_compExprNewNil( HB_COMP_PARAM );   /* replace 1st with NIL */
               pParms->value.asList.pExprList = pArg;
               pArg->pNext = pFirst->pNext;
               if( pFirst->value.asMacro.cMacroOp == '&' )
               {
                  /* simple &variable - replace the second argument with
                   * a variable name
                   */
                  char *szName = pFirst->value.asMacro.szMacro;
                  if( pFirst->pNext )
                     HB_EXPR_PCODE1( hb_compExprDelete, pFirst->pNext );  /* delete a second argument */
                  pArg->pNext = hb_compExprNewVar( szName, HB_COMP_PARAM );
                  pArg->pNext->pNext = pNext;    /* restore third argument */
                  HB_EXPR_PCODE1( hb_compExprDelete, pFirst );
               }
               else
               {
                  /* text substitution text&variable - replace the second
                   * argument with a string
                   */
                  if( pArg->pNext == NULL )
                  {
                      /* no second argument */
                     char *szText = pFirst->value.asMacro.szMacro;
                     pArg->pNext = hb_compExprNewString( szText, strlen( szText ), FALSE, HB_COMP_PARAM );
                     pArg->pNext->pNext = pNext;
                  }
                  HB_EXPR_PCODE1( hb_compExprDelete, pFirst );  /* delete first argument */
               }
            }
            else
            {   /* @ 0,0 GET &(var)
                 * TODO: generate a compilation time error -
                 * invalid GET expression
                 */
            }
         }
         else
         {
            pName->value.asSymbol = "__GET";

            /* store second and a rest of arguments */
            pNext = pArg->pNext;
            pArg->pNext = NULL;
            /* replace first argument with a set/get codeblock */
#ifdef HB_MACRO_SUPPORT
            pArg = hb_compExprSetGetBlock( pArg, HB_COMP_PARAM );
#else
            if( pArg->ExprType == HB_ET_VARIABLE )
            {
               if( hb_compVariableScope( HB_COMP_PARAM, pArg->value.asSymbol ) > 0 )
                  pArg = hb_compExprSetGetBlock( pArg, HB_COMP_PARAM );
               else
               {
                  /* Undeclared variable name - create a set/get codeblock
                   * at runtime
                  */
                  hb_compExprFree( pArg, HB_COMP_PARAM );
                  pArg = hb_compExprNewNil( HB_COMP_PARAM );
               }
            }
            else
            {
               pArg = hb_compExprSetGetBlock( pArg, HB_COMP_PARAM );
            }
#endif
            /* restore next arguments */
            pArg->pNext = pNext;
            /* set an updated list of arguments */
            pParms->value.asList.pExprList = pArg;
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
      pExpr = hb_compExprNew( HB_ET_FUNCALL, HB_COMP_PARAM );
      pExpr->value.asFunCall.pParms = pParms;
      pExpr->value.asFunCall.pFunName = pName;
   }

   return pExpr;
}

/* Creates new send expression
 *    pObject : szMessage
 */
HB_EXPR_PTR hb_compExprNewSend( HB_EXPR_PTR pObject, char * szMessage,
                                HB_EXPR_PTR pMessage, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewSend(%p,%s,%p,%p)", pObject, szMessage, pMessage, HB_COMP_PARAM));

   pExpr = hb_compExprNew( HB_ET_SEND, HB_COMP_PARAM );
   pExpr->value.asMessage.pObject = pObject;
   pExpr->value.asMessage.pParms = NULL;

   if( szMessage != NULL )
   {
      pExpr->value.asMessage.szMessage = szMessage;
      pExpr->value.asMessage.pMessage = NULL;
#ifndef HB_MACRO_SUPPORT
      if( pObject && szMessage[ 0 ] == '_' )
      {
         if( strcmp( "__ENUMINDEX", szMessage ) == 0 ||
             strcmp( "__ENUMBASE",  szMessage ) == 0 ||
             strcmp( "__ENUMVALUE", szMessage ) == 0 )
         {
            if( pObject->ExprType == HB_ET_VARIABLE )
            {
               if( ! hb_compForEachVarError( HB_COMP_PARAM, pObject->value.asSymbol ) )
               {
                  /* pExpr->value.asMessage.pObject = hb_compExprNewVarRef( pObject->value.asSymbol, HB_COMP_PARAM ); */
                  /* NOTE: direct type change */
                  pObject->ExprType = HB_ET_VARREF;
               }
            }
         }
      }
#endif
   }
   else
   {
      pExpr->value.asMessage.pMessage = pMessage;
      pExpr->value.asMessage.szMessage = NULL;
      if( pMessage->ExprType == HB_ET_MACRO )
      {
         /* Signal that macro compiler have to generate a pcode that will
          * return function name as symbol instead of usual value
          */
         pMessage->value.asMacro.SubType = HB_ET_MACRO_SYMBOL;
      }
   }

   return pExpr;
}

/* Creates new array access expression
 *    pArray[ pIndex ]
 * NOTE: In case of multiple indexes it is called recursively
 *    array[ idx1, idx2 ] => ( array[ idx1 ] )[ idx2 ]
 */
HB_EXPR_PTR hb_compExprNewArrayAt( HB_EXPR_PTR pArray, HB_EXPR_PTR pIndex, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewArrayAt()"));

   pExpr = hb_compExprNew( HB_ET_ARRAYAT, HB_COMP_PARAM );

   /* Check if this expression can be indexed */
   HB_EXPR_USE( pArray, HB_EA_ARRAY_AT );
   /* Check if this expression can be an index */
   HB_EXPR_USE( pIndex, HB_EA_ARRAY_INDEX );
   pExpr->value.asList.pExprList = pArray;
   pExpr->value.asList.pIndex = pIndex;
   pExpr->value.asList.reference = FALSE;

   return pExpr;
}


/* ************************************************************************* */

#ifndef HB_MACRO_SUPPORT
static void hb_compExprCheckStaticInitializers( HB_EXPR_PTR pLeftExpr, HB_EXPR_PTR pRightExpr, HB_COMP_DECL )
{
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
      pElem = hb_compExprListStrip( HB_EXPR_USE( pElem, HB_EA_REDUCE ), HB_COMP_PARAM );
      if( pElem->ExprType > HB_ET_FUNREF )
         hb_compErrorStatic( HB_COMP_PARAM, pLeftExpr->value.asSymbol, pElem );
      *pPrev = pElem;   /* store a new expression into the previous one */
      pElem->pNext = pNext;  /* restore the link to next expression */
      pPrev  = &pElem->pNext;
      pElem  = pNext;
   }
}

/* It initializes static variable.
 *    It is called in the following context:
 * STATIC sVar := expression
 *
 * pLeftExpr - is a variable name
 * pRightExpr - can be an expression of any type
 */
HB_EXPR_PTR hb_compExprAssignStatic( HB_EXPR_PTR pLeftExpr, HB_EXPR_PTR pRightExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprAssignStatic()"));

   pExpr = hb_compExprNew( HB_EO_ASSIGN, HB_COMP_PARAM );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   /* Try to reduce the assigned value */
   pRightExpr = hb_compExprListStrip( HB_EXPR_USE( pRightExpr, HB_EA_REDUCE ), HB_COMP_PARAM );
   pExpr->value.asOperator.pRight = pRightExpr;

   if( pRightExpr->ExprType == HB_ET_ARGLIST )
   {
       /* HB_ET_ARGLIST is used in case of STATIC var[dim1, dim2, dimN]
        * was used - we have to check if all array dimensions are
        * constant values
        */
      hb_compExprCheckStaticInitializers( pLeftExpr, pRightExpr, HB_COMP_PARAM );
   }
   else if( pRightExpr->ExprType > HB_ET_FUNREF )
   {
      /* Illegal initializer for static variable (not a constant value)
       */
      hb_compErrorStatic( HB_COMP_PARAM, pLeftExpr->value.asSymbol, pRightExpr );
   }
   else if( pRightExpr->ExprType == HB_ET_ARRAY )
   {
      /* { elem1, elem2, elemN } was used as initializer
       * Scan an array for illegal initializers.
       * An array item have to be a const value too.
       */
      hb_compExprCheckStaticInitializers( pLeftExpr, pRightExpr, HB_COMP_PARAM );
   }

   return pExpr;
}
#endif


/* Sets the argument of an operation found previously
 */
HB_EXPR_PTR hb_compExprSetOperand( HB_EXPR_PTR pExpr, HB_EXPR_PTR pItem, HB_COMP_DECL )
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

      if( pExpr->ExprType >= HB_EO_PLUSEQ && pExpr->ExprType <= HB_EO_EXPEQ )
      {
      }
      else
      {
         hb_compErrorSyntax( HB_COMP_PARAM, pItem );
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
      BYTE ucLeft = s_PrecedTable[ pExpr->ExprType ];
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

/* Generates pcode for inline expression used as a statement
 * NOTE: It doesn't not leave any value on the eval stack
 */
HB_EXPR_PTR hb_compExprGenStatement( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprGenStatement(%p)", pExpr));
   if( pExpr )
   {
      if( pExpr->ExprType == HB_EO_EQUAL )
      {
         /* NOTE: direct type change */
         pExpr->ExprType = HB_EO_ASSIGN;
      }

      pExpr = HB_EXPR_USE( pExpr, HB_EA_REDUCE );
      HB_EXPR_USE( pExpr, HB_EA_STATEMENT );
   }
   return pExpr;
}

/* Generates pcode to push an expressions
 * NOTE: It pushes a value on the stack and leaves this value on the stack
 */
HB_EXPR_PTR hb_compExprGenPush( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprGenPush(%i)", pExpr->ExprType));

   pExpr = HB_EXPR_USE( pExpr, HB_EA_REDUCE );
   HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
   return pExpr;
}

/* Generates pcode to pop an expressions
 */
HB_EXPR_PTR hb_compExprGenPop( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprGenPop(%i)", pExpr->ExprType));

   return HB_EXPR_USE( pExpr, HB_EA_POP_PCODE );
}

/* ************************************************************************* */

/* Create a new declaration for codeblock local variable
 */
static HB_CBVAR_PTR hb_compExprCBVarNew( char * szVarName, BYTE bType )
{
   HB_CBVAR_PTR pVar;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprCBVarNew(%s)", szVarName));

   pVar = ( HB_CBVAR_PTR ) hb_xgrab( sizeof( HB_CBVAR ) );

   pVar->szName = szVarName;
   pVar->bType  = bType;
   pVar->pNext  = NULL;
   pVar->bUsed  = FALSE;

   return pVar;
}

/* NOTE: This deletes all linked variables
 */
void hb_compExprCBVarDel( HB_CBVAR_PTR pVars )
{
   HB_CBVAR_PTR pDel;

   while( pVars )
   {
      pDel  = pVars;
      pVars = pVars->pNext;
      hb_xfree( pDel );
   }
}

#ifndef HB_MACRO_SUPPORT
HB_EXPR_PTR hb_compExprReduce( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   return hb_compExprListStrip( HB_EXPR_USE( pExpr, HB_EA_REDUCE ), HB_COMP_PARAM );
}
#endif

/* Creates a set/get codeblock for passed expression used in __GET
 *
 * {|var| IIF( var==NIL, <pExpr>, <pExpr>:=var )}
 */
HB_EXPR_PTR hb_compExprSetGetBlock( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR pIIF;
   HB_EXPR_PTR pSet;

   /* create {|var|  expression 
    * NOTE: this is not a valid variable name so there will be no collisions
   */
   /* create var==NIL */
   pIIF = hb_compExprSetOperand( hb_compExprNewEQ( hb_compExprNewVar( "~1", HB_COMP_PARAM ), HB_COMP_PARAM ),
                                 hb_compExprNewNil( HB_COMP_PARAM ), HB_COMP_PARAM );
   /* create ( var==NIL, */
   pIIF = hb_compExprNewList( pIIF, HB_COMP_PARAM );
   /* create ( var==NIL, <pExpr>, */
   pIIF = hb_compExprAddListExpr( pIIF, pExpr );
   /* create var */
   pSet =hb_compExprNewVar( "~1", HB_COMP_PARAM );           
   /* create <pExpr>:=var */
   pSet = hb_compExprAssign( hb_compExprClone( pExpr ), pSet, HB_COMP_PARAM );
   /* create ( var==nil, <pExpr>, <pExpr>:=var ) */
   pIIF = hb_compExprAddListExpr( pIIF, pSet );
   /* create IIF() expression */
   pIIF = hb_compExprNewIIF( pIIF );
   /* create a codeblock
   */
   return hb_compExprAddCodeblockExpr( hb_compExprCBVarAdd(
                     hb_compExprNewCodeBlock( NULL, 0, 0, HB_COMP_PARAM ),
                     "~1", ' ', HB_COMP_PARAM ), pIIF );
}
