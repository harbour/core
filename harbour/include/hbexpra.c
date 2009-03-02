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

/* ************************************************************************ */

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

/* Create function call
 */
#ifdef HB_MACRO_SUPPORT
HB_EXPR_PTR hb_macroExprNewFunCall( HB_EXPR_PTR pName, HB_EXPR_PTR pParms, HB_COMP_DECL )
#else
HB_EXPR_PTR hb_compExprNewFunCall( HB_EXPR_PTR pName, HB_EXPR_PTR pParms, HB_COMP_DECL )
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
      int iLen;

      HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewFunCall(%s)", pName->value.asSymbol));

      iLen = strlen( pName->value.asSymbol );

#if !defined( HB_MACRO_SUPPORT ) && defined( HB_USE_ENUM_FUNCTIONS )
      if( iLen > 7 && memcmp( "HB_ENUM", pName->value.asSymbol, 7 ) == 0 )
      {
         char * szMessage = NULL;

         if( iLen == 12 && memcmp( "INDEX", pName->value.asSymbol + 7, 5 ) == 0 )
            szMessage = "__ENUMINDEX";
         else if( iLen == 12 && memcmp( "VALUE", pName->value.asSymbol + 7, 5 ) == 0 )
            szMessage = "__ENUMVALUE";
         else if( iLen == 11 && memcmp( "BASE", pName->value.asSymbol + 7, 4 ) == 0 )
            szMessage = "__ENUMBASE";
         else if( iLen == 10 && memcmp( "KEY", pName->value.asSymbol + 7, 3 ) == 0 )
            szMessage = "__ENUMKEY";

         if( szMessage )
         {
            int iCount = ( int ) hb_compExprParamListLen( pParms );
            char * szName = NULL;

            if( iCount == 0 )
            {
               HB_ENUMERATOR_PTR pForVar, pEnumVar = NULL;
               pForVar = HB_COMP_PARAM->functions.pLast->pEnum;
               if( pForVar )
               {
                  while( pForVar )
                  {
                     if( pForVar->bForEach )
                        pEnumVar = pForVar;
                     pForVar = pForVar->pNext;
                  }
                  if( pEnumVar )
                     szName = pEnumVar->szName;
               }
            }
            else if( iCount == 1 )
            {
               if( pParms->value.asList.pExprList->ExprType == HB_ET_VARIABLE ||
                   pParms->value.asList.pExprList->ExprType == HB_ET_VARREF )
                  szName = pParms->value.asList.pExprList->value.asSymbol;
            }
            if( szName )
            {
               HB_COMP_EXPR_DELETE( pParms );
               HB_COMP_EXPR_DELETE( pName ); 
               return hb_compExprNewMethodCall( hb_compExprNewSend(
                        hb_compExprNewVar( szName, HB_COMP_PARAM ),
                                    szMessage, NULL, HB_COMP_PARAM ), NULL );
            }
         }
      }
      else
#endif
      if( hb_compExprParamListLen( pParms ) == 0 )
      {
         /* nothing to do, both EVAL and _GET_ below need parameters */
      }
      else if( iLen == 4 && memcmp( "EVAL", pName->value.asSymbol, 4 ) == 0 )
      {
         HB_EXPR_PTR pEval;
         /* Optimize Eval( bBlock, [ArgList] ) to: bBlock:Eval( [ArgList] ) */
#ifdef HB_MACRO_SUPPORT
         pEval = hb_compExprNewMethodCall(
            hb_macroExprNewSend( pParms->value.asList.pExprList, "EVAL", NULL, HB_COMP_PARAM ),
            hb_compExprNewArgList( pParms->value.asList.pExprList->pNext, HB_COMP_PARAM ) );
#else
         pEval = hb_compExprNewMethodCall(
            hb_compExprNewSend( pParms->value.asList.pExprList, "EVAL", NULL, HB_COMP_PARAM ),
            hb_compExprNewArgList( pParms->value.asList.pExprList->pNext, HB_COMP_PARAM ) );
#endif
         pParms->value.asList.pExprList = NULL;
         HB_COMP_EXPR_DELETE( pParms );
         HB_COMP_EXPR_DELETE( pName ); 
         return pEval;
      }
      else if( iLen == 5 && memcmp( "_GET_", pName->value.asSymbol, 5 ) == 0 )
      {
         /* Reserved Clipper function used to handle GET variables
          */
         HB_EXPR_PTR pArg, pNext;
         USHORT uiCount;

         pParms->value.asList.pExprList = HB_EXPR_USE( pParms->value.asList.pExprList, HB_EA_REDUCE );
         pArg = pParms->value.asList.pExprList;

         /* When -kc switch is used expression list is not stripped
          * in reduce operation
          */
         if( !HB_SUPPORT_HARBOUR && pArg->ExprType == HB_ET_LIST )
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
               HB_COMP_EXPR_FREE( pVar );
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
            HB_COMP_EXPR_CLEAR( pArg );
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
               HB_COMP_EXPR_DELETE( pVar->pNext );
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
                  HB_COMP_EXPR_CLEAR( pBase );
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
                  const char *szName = pFirst->value.asMacro.szMacro;
                  if( pFirst->pNext )
                     HB_COMP_EXPR_DELETE( pFirst->pNext );  /* delete a second argument */
                  pArg->pNext = hb_compExprNewVar( szName, HB_COMP_PARAM );
                  pArg->pNext->pNext = pNext;    /* restore third argument */
                  HB_COMP_EXPR_DELETE( pFirst );
               }
               else
               {
                  /* text substitution text&variable - replace the second
                   * argument with a string
                   */
                  if( pArg->pNext == NULL )
                  {
                      /* no second argument */
                     const char *szText = pFirst->value.asMacro.szMacro;
                     pArg->pNext = hb_compExprNewString( szText, strlen( szText ), FALSE, HB_COMP_PARAM );
                     pArg->pNext->pNext = pNext;
                  }
                  HB_COMP_EXPR_DELETE( pFirst );  /* delete first argument */
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
#if !defined( HB_MACRO_SUPPORT )
            if( pArg->ExprType == HB_ET_VARIABLE )
            {
               if( hb_compVariableFind( HB_COMP_PARAM, pArg->value.asSymbol, NULL, NULL ) )
                  pArg = hb_compExprSetGetBlock( pArg, HB_COMP_PARAM );
               else
               {
                  /* Undeclared variable name - create a set/get codeblock
                   * at runtime
                  */
                  HB_COMP_EXPR_FREE( pArg );
                  pArg = hb_compExprNewNil( HB_COMP_PARAM );
               }
            }
            else
#endif
            {
               pArg = hb_compExprSetGetBlock( pArg, HB_COMP_PARAM );
            }
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
#ifdef HB_MACRO_SUPPORT
   else if( pName->ExprType == HB_ET_VARIABLE )
   {
      /* My&var.1() executed by macro compiler
       */
      pName->ExprType = HB_ET_FUNNAME;
   }
#endif

   if( pExpr == NULL )
   {
      pExpr = HB_COMP_EXPR_NEW( HB_ET_FUNCALL );
      pExpr->value.asFunCall.pParms = pParms;
      pExpr->value.asFunCall.pFunName = pName;
   }

   return pExpr;
}

/* Creates new send expression
 *    pObject : szMessage
 */
#ifdef HB_MACRO_SUPPORT
HB_EXPR_PTR hb_macroExprNewSend( HB_EXPR_PTR pObject, const char * szMessage,
                                 HB_EXPR_PTR pMessage, HB_COMP_DECL )
#else
HB_EXPR_PTR hb_compExprNewSend( HB_EXPR_PTR pObject, const char * szMessage,
                                HB_EXPR_PTR pMessage, HB_COMP_DECL )
#endif
{
   HB_EXPR_PTR pExpr;

   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewSend(%p,%s,%p,%p)", pObject, szMessage, pMessage, HB_COMP_PARAM));

   pExpr = HB_COMP_EXPR_NEW( HB_ET_SEND );
   pExpr->value.asMessage.pObject = pObject;
   pExpr->value.asMessage.pParms = NULL;

   if( szMessage != NULL )
   {
      pExpr->value.asMessage.szMessage = szMessage;
      pExpr->value.asMessage.pMessage = NULL;
#ifndef HB_MACRO_SUPPORT
      if( pObject && szMessage[ 0 ] == '_' && strncmp( "__ENUM", szMessage, 6 ) == 0 )
      {
         if( strcmp( "INDEX", szMessage + 6 ) == 0 ||
             strcmp( "KEY",   szMessage + 6 ) == 0 ||
             strcmp( "BASE",  szMessage + 6 ) == 0 ||
             strcmp( "VALUE", szMessage + 6 ) == 0 )
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
#ifdef HB_MACRO_SUPPORT
HB_EXPR_PTR hb_macroExprNewArrayAt( HB_EXPR_PTR pArray, HB_EXPR_PTR pIndex, HB_COMP_DECL )
#else
HB_EXPR_PTR hb_compExprNewArrayAt( HB_EXPR_PTR pArray, HB_EXPR_PTR pIndex, HB_COMP_DECL )
#endif
{
   HB_EXPR_PTR pExpr;

#ifdef HB_MACRO_SUPPORT
   HB_TRACE(HB_TR_DEBUG, ("hb_macroExprNewArrayAt()"));
#else
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprNewArrayAt()"));
#endif

   pExpr = HB_COMP_EXPR_NEW( HB_ET_ARRAYAT );

   /* Check if this expression can be indexed */
   if( ! HB_SUPPORT_ARRSTR )
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

/* List of functions which can be used as static initializers */
static const char * s_szStaticFun[] = {
   "HB_MUTEXCREATE"
};

#define STATIC_FUNCTIONS      ( sizeof( s_szStaticFun ) / sizeof( char * ) )

static BOOL hb_compStaticFunction( const char * szName )
{
   unsigned int ui;
   for( ui = 0; ui < STATIC_FUNCTIONS; ++ui )
   {
      if( strcmp( szName, s_szStaticFun[ ui ] ) == 0 )
         return TRUE;
   }
   return FALSE;
}


static void hb_compExprCheckStaticInitializer( HB_EXPR_PTR pLeftExpr, HB_EXPR_PTR pRightExpr, HB_COMP_DECL )
{
   if( ( pRightExpr->ExprType > HB_ET_FUNREF ||
         pRightExpr->ExprType == HB_ET_SELF ) &&
       !( pRightExpr->ExprType == HB_ET_FUNCALL &&
          pRightExpr->value.asFunCall.pFunName->ExprType == HB_ET_FUNNAME &&
          hb_compStaticFunction( pRightExpr->value.asFunCall.pFunName->
                                 value.asSymbol ) &&
          hb_compExprParamListLen( pRightExpr->value.asFunCall.pParms ) == 0 ) )
   {
      /* Illegal initializer for static variable (not a constant value)
       */
      hb_compErrorStatic( HB_COMP_PARAM, pLeftExpr->value.asSymbol, pRightExpr );
   }
}

static void hb_compExprCheckStaticListInitializers( HB_EXPR_PTR pLeftExpr, HB_EXPR_PTR pRightExpr, HB_COMP_DECL )
{
   HB_EXPR_PTR * pExpr = &pRightExpr->value.asList.pExprList;

   while( *pExpr )
   {
      if( !HB_SUPPORT_HARBOUR )
      {
         /* When -kc switch is used expression list is not stripped
          * in reduce operation
          */
         /* NOTE: During reduction the expression can be replaced by the
          *       new one - this will break the linked list of expressions.
          *       (classical case of replacing an item in a linked list)
          */
         HB_EXPR_PTR pNext = (*pExpr)->pNext;   /* store next expression in case the current will be reduced */
         *pExpr = hb_compExprListStrip( *pExpr, HB_COMP_PARAM );
         (*pExpr)->pNext = pNext;               /* restore the link to next expression */
      }

      if( (*pExpr)->ExprType == HB_ET_ARRAY ||
          (*pExpr)->ExprType == HB_ET_HASH )
      {
         hb_compExprCheckStaticListInitializers( pLeftExpr, *pExpr, HB_COMP_PARAM );
      }
      else
      {
         hb_compExprCheckStaticInitializer( pLeftExpr, *pExpr, HB_COMP_PARAM );
      }
      pExpr = &(*pExpr)->pNext;
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

   pExpr = HB_COMP_EXPR_NEW( HB_EO_ASSIGN );

   pExpr->value.asOperator.pLeft  = pLeftExpr;
   /* Try to reduce the assigned value */
   pRightExpr = HB_EXPR_USE( pRightExpr, HB_EA_REDUCE );
   /* When -kc switch is used expression list is not stripped
    * in reduce operation
    */
   if( !HB_SUPPORT_HARBOUR )
      pRightExpr = hb_compExprListStrip( pRightExpr, HB_COMP_PARAM );

   pExpr->value.asOperator.pRight = pRightExpr;

   if( pRightExpr->ExprType == HB_ET_ARGLIST )
   {
       /* HB_ET_ARGLIST is used in case of STATIC var[dim1, dim2, dimN]
        * was used - we have to check if all array dimensions are
        * constant values
        */
      hb_compExprCheckStaticListInitializers( pLeftExpr, pRightExpr, HB_COMP_PARAM );
   }
   else if( pRightExpr->ExprType == HB_ET_ARRAY )
   {
      /* { elem1, elem2, elemN } was used as initializer
       * Scan an array for illegal initializers.
       * An array item have to be a const value too.
       */
      hb_compExprCheckStaticListInitializers( pLeftExpr, pRightExpr, HB_COMP_PARAM );
   }
   else if( pRightExpr->ExprType == HB_ET_HASH )
   {
      /* { idx1=>var1, idx2=>var2, idxN=>varN } was used as initializer
       * Scan a hash array for illegal initializers.
       * A hash item have to be a const value too.
       */
      hb_compExprCheckStaticListInitializers( pLeftExpr, pRightExpr, HB_COMP_PARAM );
   }
   else
   {
      hb_compExprCheckStaticInitializer( pLeftExpr, pRightExpr, HB_COMP_PARAM );
   }

   return pExpr;
}

HB_EXPR_PTR hb_compExprSetCodeblockBody( HB_EXPR_PTR pExpr, BYTE * pCode, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compExprSetCodeblockBody(%p,%p,%lu)", pExpr, pCode, ulLen));

   pExpr->value.asCodeblock.string = ( char * ) hb_xgrab( ulLen + 1 );
   memcpy( pExpr->value.asCodeblock.string, pCode, ulLen );
   pExpr->value.asCodeblock.string[ ulLen ] = '\0';
   pExpr->ulLength = ulLen;

   return pExpr;
}
#endif

/* ************************************************************************* */

#if defined( HB_MACRO_SUPPORT )

/* Generates pcode to push an expressions
 * NOTE: It pushes a value on the stack and leaves this value on the stack
 */
HB_EXPR_PTR hb_macroExprGenPush( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_macroExprGenPush(%i)", pExpr->ExprType));

   pExpr = HB_EXPR_USE( pExpr, HB_EA_REDUCE );
   HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
   return pExpr;
}

/* Generates pcode to pop an expressions
 */
HB_EXPR_PTR hb_macroExprGenPop( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_macroExprGenPop(%i)", pExpr->ExprType));

   return HB_EXPR_USE( pExpr, HB_EA_POP_PCODE );
}

#else

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

HB_EXPR_PTR hb_compExprReduce( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   return HB_EXPR_USE( pExpr, HB_EA_REDUCE );
}
#endif

/* ************************************************************************* */
