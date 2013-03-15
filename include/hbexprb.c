/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Expression Optimizer
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

#include "hbcomp.h"
#include "hbmacro.ch"

#if ! defined( HB_HASH_USES_ARRAY_INDEXES )
#  define HB_HASH_USES_ARRAY_INDEXES
#endif

#define HB_USE_ARRAYAT_REF
#define HB_USE_OBJMSG_REF


/* Forward declarations
 */

/* forward declaration of callback functions
 */
static HB_EXPR_FUNC( hb_compExprUseDummy );
static HB_EXPR_FUNC( hb_compExprUseNil );
static HB_EXPR_FUNC( hb_compExprUseNumeric );
static HB_EXPR_FUNC( hb_compExprUseDate );
static HB_EXPR_FUNC( hb_compExprUseTimeStamp );
static HB_EXPR_FUNC( hb_compExprUseString );
static HB_EXPR_FUNC( hb_compExprUseCodeblock );
static HB_EXPR_FUNC( hb_compExprUseLogical );
static HB_EXPR_FUNC( hb_compExprUseSelf );
static HB_EXPR_FUNC( hb_compExprUseArray );
static HB_EXPR_FUNC( hb_compExprUseHash );
static HB_EXPR_FUNC( hb_compExprUseFunRef );
static HB_EXPR_FUNC( hb_compExprUseVarRef );
static HB_EXPR_FUNC( hb_compExprUseRef );
static HB_EXPR_FUNC( hb_compExprUseIIF );
static HB_EXPR_FUNC( hb_compExprUseList );
static HB_EXPR_FUNC( hb_compExprUseArgList );
static HB_EXPR_FUNC( hb_compExprUseMacroArgList );
static HB_EXPR_FUNC( hb_compExprUseArrayAt );
static HB_EXPR_FUNC( hb_compExprUseMacro );
static HB_EXPR_FUNC( hb_compExprUseFunCall );
static HB_EXPR_FUNC( hb_compExprUseAliasVar );
static HB_EXPR_FUNC( hb_compExprUseAliasExpr );
static HB_EXPR_FUNC( hb_compExprUseSetGet );
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

/* other helper functions
 */
#if defined( HB_MACRO_SUPPORT )
   static void hb_compExprCodeblockPush( PHB_EXPR, HB_COMP_DECL );
#else
   static HB_BOOL hb_compExprCodeblockPush( PHB_EXPR, int, HB_COMP_DECL );
   static void hb_compExprCodeblockEarly( PHB_EXPR, HB_COMP_DECL );
   static void hb_compExprCodeblockExtPush( PHB_EXPR pSelf, HB_COMP_DECL );
#endif

static void hb_compExprPushSendPop( PHB_EXPR pSelf, HB_COMP_DECL );
static void hb_compExprPushSendPush( PHB_EXPR pSelf, HB_COMP_DECL );
static void hb_compExprPushOperEq( PHB_EXPR pSelf, HB_BYTE bOpEq, HB_COMP_DECL );
static void hb_compExprUseOperEq( PHB_EXPR pSelf, HB_BYTE bOpEq, HB_COMP_DECL );
static void hb_compExprPushPreOp( PHB_EXPR pSelf, HB_BYTE bOper, HB_COMP_DECL );
static void hb_compExprPushPostOp( PHB_EXPR pSelf, HB_BYTE bOper, HB_COMP_DECL );
static void hb_compExprUsePreOp( PHB_EXPR pSelf, HB_BYTE bOper, HB_COMP_DECL );
static void hb_compExprUseAliasMacro( PHB_EXPR pAliasedVar, HB_BYTE bAction, HB_COMP_DECL );
static PHB_EXPR hb_compExprReduceList( PHB_EXPR pExpr, HB_COMP_DECL );
static PHB_EXPR hb_compExprReduceAliasString( PHB_EXPR pExpr, PHB_EXPR pAlias, HB_COMP_DECL );
static HB_BOOL hb_compExprIsMemvarAlias( const char * szAlias );


const PHB_EXPR_FUNC hb_comp_ExprTable[ HB_EXPR_COUNT ] = {
   hb_compExprUseDummy,
   hb_compExprUseNil,
   hb_compExprUseNumeric,
   hb_compExprUseDate,
   hb_compExprUseTimeStamp,
   hb_compExprUseString,
   hb_compExprUseCodeblock,
   hb_compExprUseLogical,
   hb_compExprUseSelf,
   hb_compExprUseArray,
   hb_compExprUseHash,
   hb_compExprUseFunRef,
   hb_compExprUseVarRef,
   hb_compExprUseRef,
   hb_compExprUseIIF,
   hb_compExprUseList,
   hb_compExprUseArgList,
   hb_compExprUseMacroArgList,
   hb_compExprUseArrayAt,
   hb_compExprUseMacro,
   hb_compExprUseFunCall,
   hb_compExprUseAliasVar,
   hb_compExprUseAliasExpr,
   hb_compExprUseSetGet,
   hb_compExprUseSend,
   hb_compExprUseFunName,
   hb_compExprUseAlias,
   hb_compExprUseRTVariable,
   hb_compExprUseVariable,
   hb_compExprUsePostInc,     /* post-operators -> lowest precedence */
   hb_compExprUsePostDec,
   hb_compExprUseAssign,      /* assigments */
   hb_compExprUsePlusEq,
   hb_compExprUseMinusEq,
   hb_compExprUseMultEq,
   hb_compExprUseDivEq,
   hb_compExprUseModEq,
   hb_compExprUseExpEq,
   hb_compExprUseOr,          /* logical operators */
   hb_compExprUseAnd,
   hb_compExprUseNot,
   hb_compExprUseEqual,       /* relational operators */
   hb_compExprUseEQ,
   hb_compExprUseNE,
   hb_compExprUseIN,
   hb_compExprUseLT,
   hb_compExprUseGT,
   hb_compExprUseLE,
   hb_compExprUseGE,
   hb_compExprUsePlus,        /* addition */
   hb_compExprUseMinus,
   hb_compExprUseMult,        /* multiple */
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
         HB_COMP_ERROR_TYPE( pSelf );
      case HB_EA_ARRAY_INDEX:
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_GEN_FUNC1( PCode1, HB_P_PUSHNIL );
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
         HB_COMP_ERROR_TYPE( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( HB_COMP_PARAM, pSelf );     /* NIL cannot be used as index element */
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_GEN_FUNC1( PCode1, HB_P_PUSHNIL );
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
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
         HB_COMP_ERROR_TYPE( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         if( pSelf->value.asNum.NumType == HB_ET_DOUBLE )
            HB_GEN_FUNC3( PushDouble, pSelf->value.asNum.val.d, pSelf->value.asNum.bWidth, pSelf->value.asNum.bDec );
         else
            HB_GEN_FUNC1( PushLong, pSelf->value.asNum.val.l );
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
      case HB_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for HB_ET_DATE expression
 */
static HB_EXPR_FUNC( hb_compExprUseDate )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         break;
      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
#ifdef HB_HASH_USES_ARRAY_INDEXES
         if( ! HB_SUPPORT_HARBOUR )
#endif
            hb_compErrorIndex( HB_COMP_PARAM, pSelf );     /* Date cannot be used as index element */
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_GEN_FUNC1( PushDate, pSelf->value.asDate.lDate );
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
      case HB_EA_DELETE:
         break;
   }

   return pSelf;
}

/* actions for HB_ET_TIMESTAMP expression
 */
static HB_EXPR_FUNC( hb_compExprUseTimeStamp )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         break;
      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
#ifdef HB_HASH_USES_ARRAY_INDEXES
         if( ! HB_SUPPORT_HARBOUR )
#endif
            hb_compErrorIndex( HB_COMP_PARAM, pSelf );     /* timestamp cannot be used as index element */
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_GEN_FUNC2( PushTimeStamp, pSelf->value.asDate.lDate,
                                      pSelf->value.asDate.lTime );
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
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
         HB_COMP_ERROR_TYPE( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
#ifdef HB_HASH_USES_ARRAY_INDEXES
         if( ! HB_SUPPORT_HARBOUR )
#endif
            hb_compErrorIndex( HB_COMP_PARAM, pSelf );     /* string cannot be used as index element */
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
#if ! defined( HB_MACRO_SUPPORT )
         if( ! HB_SUPPORT_MACROTEXT )
            HB_GEN_FUNC2( PushString, pSelf->value.asString.string,
                          pSelf->nLength + 1 );
         else
            hb_compPushMacroText( HB_COMP_PARAM,
                                  pSelf->value.asString.string,
                                  pSelf->nLength, HB_FALSE );
#else
         HB_GEN_FUNC2( PushString, pSelf->value.asString.string,
                       pSelf->nLength + 1 );
         if( hb_macroIsValidMacroText( pSelf->value.asString.string,
                                       pSelf->nLength ) )
         {
            HB_GEN_FUNC1( PCode1, HB_P_MACROTEXT );
         }
#endif
         break;
      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asString.dealloc )
            hb_xfree( pSelf->value.asString.string );
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
         pSelf->value.asCodeblock.flags |= HB_BLOCK_REDUCE;
         break;
      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( HB_COMP_PARAM, pSelf );     /* codeblock cannot be used as index element */
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
#if defined( HB_MACRO_SUPPORT )
         hb_compExprCodeblockPush( pSelf, HB_COMP_PARAM );
#else
         if( pSelf->value.asCodeblock.flags & HB_BLOCK_EXT )
            hb_compExprCodeblockExtPush( pSelf, HB_COMP_PARAM );
         else if( ( pSelf->value.asCodeblock.flags & HB_BLOCK_MACROVAR ) &&
                  ! ( pSelf->value.asCodeblock.flags & HB_BLOCK_VPARAMS ) )
            /* early evaluation of a macro */
            hb_compExprCodeblockEarly( pSelf, HB_COMP_PARAM );
         else
            hb_compExprCodeblockPush( pSelf, 0, HB_COMP_PARAM );
#endif
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_DELETE:
      {
         PHB_EXPR pExpr = pSelf->value.asCodeblock.pExprList;

         hb_compExprCBVarDel( pSelf->value.asCodeblock.pLocals );

         if( pSelf->value.asCodeblock.string )
            hb_xfree( pSelf->value.asCodeblock.string );

         /* Delete all expressions of the block. */
         while( pExpr )
         {
            PHB_EXPR pNext = pExpr->pNext;
            HB_COMP_EXPR_FREE( pExpr );
            pExpr = pNext;
         }
         break;
      }
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
         HB_COMP_ERROR_TYPE( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( HB_COMP_PARAM, pSelf );     /* logical cannot be used as array index element */
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_GEN_FUNC1( PushLogical, pSelf->value.asLogical );
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
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
         /* Clipper allows such operation and because some valid Clipper
          * code needs it then I disabled error message, [druzus]
          */
         /* HB_COMP_ERROR_TYPE( pSelf ); */
         break;
      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( HB_COMP_PARAM, pSelf );     /* SELF cannot be used as array index element */
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_GEN_FUNC1( PCode1, HB_P_PUSHSELF );
         break;
      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
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
         pSelf = hb_compExprReduceList( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         break;

      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( HB_COMP_PARAM, pSelf );     /* array cannot be used as index element */
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
      {
         HB_USHORT usItems = ( HB_USHORT ) hb_compExprParamListCheck( HB_COMP_PARAM, pSelf );

         if( usItems == 0 )
         {
            HB_GEN_FUNC3( PCode3, HB_P_ARRAYGEN, 0, 0 );
         }
         else
         {
            HB_BOOL fArgsList = pSelf->ExprType == HB_ET_MACROARGLIST;

            if( ! fArgsList )
            {
               /* Note: direct type change */
               pSelf->ExprType = HB_ET_ARGLIST;
            }
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            /* restore original expression type */
            pSelf->ExprType = HB_ET_ARRAY;

            if( fArgsList )
            {
               HB_GEN_FUNC3( PCode3, HB_P_MACROARRAYGEN,
                             HB_LOBYTE( usItems ), HB_HIBYTE( usItems ) );
            }
            else
            {
               HB_GEN_FUNC3( PCode3, HB_P_ARRAYGEN,
                             HB_LOBYTE( usItems ), HB_HIBYTE( usItems ) );
            }
         }
         break;
      }

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      {
         PHB_EXPR pElem = pSelf->value.asList.pExprList;
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
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_DELETE:
      {
         PHB_EXPR pElem = pSelf->value.asList.pExprList;
         /* Delete all elements of the array
          */
         while( pElem )
         {
            PHB_EXPR pNext = pElem->pNext;
            HB_COMP_EXPR_FREE( pElem );
            pElem = pNext;
         }
      }
      break;
   }

   return pSelf;
}

/* actions for HB_ET_HASH literal hash
 *    { key1=>val1, key2=>val2, ... keyN=>valN }
 */
static HB_EXPR_FUNC( hb_compExprUseHash )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf = hb_compExprReduceList( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         break;

      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( HB_COMP_PARAM, pSelf );     /* array cannot be used as index element */
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
      {
         HB_USHORT usItems = ( HB_USHORT ) ( pSelf->nLength >> 1 );
         /* Note: direct type change */
         pSelf->ExprType = HB_ET_ARGLIST;
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         /* restore original expression type */
         pSelf->ExprType = HB_ET_HASH;
         HB_GEN_FUNC3( PCode3, HB_P_HASHGEN, HB_LOBYTE( usItems ), HB_HIBYTE( usItems ) );
         break;
      }

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      {
         PHB_EXPR pElem = pSelf->value.asList.pExprList;
         /* Push non-constant values only */
         while( pElem )
         {
            HB_EXPR_USE( pElem, HB_EA_PUSH_POP );
            pElem = pElem->pNext;
         }
      }
      break;

      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_DELETE:
      {
         PHB_EXPR pElem = pSelf->value.asList.pExprList;
         /* Delete all elements of the hash array
          */
         while( pElem )
         {
            PHB_EXPR pNext = pElem->pNext;
            HB_COMP_EXPR_FREE( pElem );
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
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_GEN_FUNC1( PushVarRef, pSelf->value.asSymbol.name );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
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
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_GEN_FUNC1( PushFunRef, pSelf->value.asSymbol.name );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
      case HB_EA_DELETE:
         break;
   }
   return pSelf;
}

/* actions for HB_ET_REFERENCE expression
 */
static HB_EXPR_FUNC( hb_compExprUseRef )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asReference = HB_EXPR_USE( pSelf->value.asReference, HB_EA_REDUCE );
         if( pSelf->value.asReference->ExprType == HB_ET_IIF )
         {
            PHB_EXPR pCond, pIIF, pFalse;
            pIIF = pSelf->value.asReference;
            pCond = pIIF->value.asList.pExprList;
            pFalse = hb_compExprNewRef( pCond->pNext->pNext, HB_COMP_PARAM );
            pCond->pNext = hb_compExprNewRef( pCond->pNext, HB_COMP_PARAM );
            pCond->pNext->pNext = pFalse;
            HB_COMP_EXPR_CLEAR( pSelf );
            pSelf = pIIF;
         }
         break;
      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;
      case HB_EA_ARRAY_INDEX:
         break;
      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
      {
         PHB_EXPR pExp = pSelf->value.asReference;
         if( pExp->ExprType == HB_ET_MACRO )
         {
            pExp->value.asMacro.SubType = HB_ET_MACRO_REFER;
            HB_EXPR_USE( pExp, HB_EA_PUSH_PCODE );
            break;
         }
         else if( pExp->ExprType == HB_ET_ARRAYAT )
         {
            pExp->value.asList.reference = HB_TRUE;
            HB_EXPR_USE( pExp, HB_EA_PUSH_PCODE );
            break;
         }
         else if( pExp->ExprType == HB_ET_SEND )
         {
            /* PHB_EXPR pSend = pExp->value.asMessage.pObject;
            if( ! pSend || pSend->ExprType == HB_ET_VARIABLE ) */
            {
               hb_compExprPushSendPop( pExp, HB_COMP_PARAM );
               HB_GEN_FUNC1( PCode1, HB_P_PUSHOVARREF );
               break;
            }
         }
         else if( pExp->ExprType == HB_ET_VARIABLE )
         {
            /* Note: direct type change */
            pExp->ExprType = HB_ET_VARREF;
            HB_EXPR_USE( pExp, HB_EA_PUSH_PCODE );
            /* restore original expression type */
            pExp->ExprType = HB_ET_VARIABLE;
            break;
         }
         else if( pExp->ExprType == HB_ET_ALIASVAR )
         {
            if( pExp->value.asAlias.pVar->ExprType == HB_ET_VARIABLE &&
                pExp->value.asAlias.pAlias->ExprType == HB_ET_ALIAS &&
                hb_compExprIsMemvarAlias( pExp->value.asAlias.pAlias->value.asSymbol.name ) )
            {
               /* @M-> @MEMVAR-> or @MEMVA-> or @MEMV-> */
               HB_GEN_FUNC1( PushMemvarRef, pExp->value.asAlias.pVar->value.asSymbol.name );
               break;
            }
         }
         else if( pExp->ExprType == HB_ET_VARREF ||
                  pExp->ExprType == HB_ET_REFERENCE )
         {
            HB_EXPR_USE( pExp, HB_EA_PUSH_PCODE );
            break;
         }

         hb_compErrorRefer( HB_COMP_PARAM, NULL, hb_compExprDescription( pExp ) );
         break;
      }

      case HB_EA_POP_PCODE:
         break;
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
      case HB_EA_DELETE:
         HB_COMP_EXPR_FREE( pSelf->value.asReference );
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
         pSelf = hb_compExprReduceIIF( hb_compExprReduceList( pSelf, HB_COMP_PARAM ), HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         if( HB_SUPPORT_HARBOUR )
         {
            PHB_EXPR pExpr = pSelf->value.asList.pExprList->pNext;
            HB_EXPR_USE( pExpr, HB_EA_LVALUE );
            HB_EXPR_USE( pExpr->pNext, HB_EA_LVALUE );
         }
         else
            hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
      {
         /* this is called if all three parts of IIF expression should be generated
          */
         HB_ISIZ nPosFalse, nPosEnd;
         PHB_EXPR pExpr = pSelf->value.asList.pExprList;

         HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
         nPosFalse = HB_GEN_FUNC1( JumpFalse, 0 );
         pExpr = pExpr->pNext;

         HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
         nPosEnd = HB_GEN_FUNC1( Jump, 0 );
         pExpr = pExpr->pNext;

         HB_GEN_FUNC1( JumpHere, nPosFalse );
         HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( JumpHere, nPosEnd );
         break;
      }
      case HB_EA_POP_PCODE:
      {
         /* this is called if all three parts of IIF expression should be generated
          */
         HB_ISIZ nPosFalse, nPosEnd;
         PHB_EXPR pExpr = pSelf->value.asList.pExprList;

         HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
         nPosFalse = HB_GEN_FUNC1( JumpFalse, 0 );
         pExpr = pExpr->pNext;

         HB_EXPR_USE( pExpr, HB_EA_POP_PCODE );
         nPosEnd = HB_GEN_FUNC1( Jump, 0 );
         pExpr = pExpr->pNext;

         HB_GEN_FUNC1( JumpHere, nPosFalse );
         HB_EXPR_USE( pExpr, HB_EA_POP_PCODE );
         HB_GEN_FUNC1( JumpHere, nPosEnd );
         break;
      }

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
      {
#ifdef HB_CLP_STRICT
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_POP );  /* remove a value if used in statement */
#else
         HB_SIZE nPosFalse, nPosEnd;
         PHB_EXPR pExpr = pSelf->value.asList.pExprList;

         HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
         nPosFalse = HB_GEN_FUNC1( JumpFalse, 0 );
         pExpr = pExpr->pNext;

         /* do not generate warning about meaningless use of expression NIL */
         if( pExpr->ExprType != HB_ET_NIL )
            HB_EXPR_USE( pExpr, HB_EA_PUSH_POP );
         pExpr = pExpr->pNext;

#if defined( HB_MACRO_SUPPORT )
         if( HB_PCODE_DATA->nPCodePos == nPosFalse + 3 )
         {
            HB_PCODE_DATA->pCode[ nPosFalse - 1 ] = HB_P_JUMPTRUEFAR;
            nPosEnd = nPosFalse;
         }
#else
         if( HB_COMP_PARAM->functions.pLast->nPCodePos == nPosFalse + 3 )
         {
            HB_COMP_PARAM->functions.pLast->pCode[ nPosFalse - 1 ] = HB_P_JUMPTRUEFAR;
            nPosEnd = nPosFalse;
         }
#endif
         else
         {
            nPosEnd = HB_GEN_FUNC1( Jump, 0 );
            HB_GEN_FUNC1( JumpHere, nPosFalse );
         }
         /* do not generate warning about meaningless use of expression NIL */
         if( pExpr->ExprType != HB_ET_NIL )
            HB_EXPR_USE( pExpr, HB_EA_PUSH_POP );
         HB_GEN_FUNC1( JumpHere, nPosEnd );
#endif
         break;
      }
      case HB_EA_DELETE:
         if( pSelf->value.asList.pExprList )
         {
            PHB_EXPR pNext, pExpr = pSelf->value.asList.pExprList;
            while( pExpr )
            {
               pNext = pExpr->pNext;    /* store next expression */
               HB_COMP_EXPR_FREE( pExpr );
               pExpr = pNext;
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
         pSelf = hb_compExprReduceList( pSelf, HB_COMP_PARAM );

         if( HB_SUPPORT_HARBOUR )
            pSelf = hb_compExprListStrip( pSelf, HB_COMP_PARAM );

         if( HB_SUPPORT_XBASE && pSelf->ExprType == HB_ET_LIST )
         {
            if( hb_compExprListLen( pSelf ) == 1 )
            {
               PHB_EXPR pExpr = pSelf->value.asList.pExprList;
               if( pExpr->ExprType == HB_ET_MACRO &&
                   pExpr->value.asMacro.SubType != HB_ET_MACRO_SYMBOL &&
                   pExpr->value.asMacro.SubType != HB_ET_MACRO_REFER &&
                   pExpr->value.asMacro.SubType != HB_ET_MACRO_ALIASED )
               {
                  pExpr->value.asMacro.SubType |= HB_ET_MACRO_PARE;
               }
            }
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
            hb_compErrorLValue( HB_COMP_PARAM, pSelf->value.asList.pExprList );
         }
         else
            hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         {
            PHB_EXPR pExpr = pSelf->value.asList.pExprList;

            if( pExpr->ExprType == HB_ET_NONE && pExpr->pNext == NULL )
            {
               /* Empty list was used ()
                */
               HB_COMP_ERROR_SYNTAX( pExpr );
            }
            else
            {
               while( pExpr )
               {
                  if( HB_SUPPORT_XBASE )
                  {
                     if( pExpr->ExprType == HB_ET_MACRO &&
                         pExpr->value.asMacro.SubType != HB_ET_MACRO_SYMBOL &&
                         pExpr->value.asMacro.SubType != HB_ET_MACRO_REFER &&
                         pExpr->value.asMacro.SubType != HB_ET_MACRO_ALIASED )
                     {
                        pExpr->value.asMacro.SubType |= HB_ET_MACRO_PARE;
                     }
                  }

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
            PHB_EXPR pExpr = pSelf->value.asList.pExprList;

            while( pExpr )
            {
               if( HB_SUPPORT_XBASE )
               {
                  if( pExpr->ExprType == HB_ET_MACRO &&
                      pExpr->value.asMacro.SubType != HB_ET_MACRO_SYMBOL &&
                      pExpr->value.asMacro.SubType != HB_ET_MACRO_REFER &&
                      pExpr->value.asMacro.SubType != HB_ET_MACRO_ALIASED )
                  {
                      pExpr->value.asMacro.SubType |= HB_ET_MACRO_PARE;
                  }
               }

               HB_EXPR_USE( pExpr, HB_EA_PUSH_POP );
               pExpr = pExpr->pNext;
            }
         }
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asList.pExprList )
         {
            PHB_EXPR pNext, pExpr = pSelf->value.asList.pExprList;
            while( pExpr )
            {
               pNext = pExpr->pNext;    /* store next expression */
               HB_COMP_EXPR_FREE( pExpr );
               pExpr = pNext;
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
         pSelf = hb_compExprReduceList( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;

      case HB_EA_PUSH_PCODE:
         if( pSelf->value.asList.reference )
         {
#if defined( HB_MACRO_SUPPORT )
            if( ! HB_PCODE_DATA->fVParams )
#else
            if( ! HB_COMP_PARAM->functions.pLast->fVParams )
#endif
            {
               hb_compErrorVParams( HB_COMP_PARAM,
                                    HB_COMP_PARAM->functions.pLast->szName ?
                                    "Function" : "Codeblock" );
            }
            HB_GEN_FUNC1( PCode1, HB_P_PUSHVPARAMS );
         }
         else
         {
            PHB_EXPR pExpr = pSelf->value.asList.pExprList;
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
            PHB_EXPR pNext, pExpr = pSelf->value.asList.pExprList;
            while( pExpr )
            {
               pNext = pExpr->pNext;    /* store next expression */
               HB_COMP_EXPR_FREE( pExpr );
               pExpr = pNext;
            }
            pSelf->value.asList.pExprList = NULL;
         }
         break;
   }
   return pSelf;
}

/* NOTE: In PUSH operation it leaves all expressions on the eval stack,
 *       the expresions are divided into macro compiled blocks
 */
static HB_EXPR_FUNC( hb_compExprUseMacroArgList )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf = hb_compExprReduceList( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;

      case HB_EA_PUSH_PCODE:
         {
            PHB_EXPR pExpr = pSelf->value.asList.pExprList;
            HB_USHORT usItems = 0;

            while( pExpr )
            {
               if( ( pExpr->ExprType == HB_ET_MACRO &&
                     ( pExpr->value.asMacro.SubType & HB_ET_MACRO_LIST ) ) ||
                   ( pExpr->ExprType == HB_ET_ARGLIST &&
                     pExpr->value.asList.reference ) ||
                   ( pExpr->ExprType == HB_ET_FUNCALL &&
                     pExpr->value.asFunCall.pFunName->ExprType == HB_ET_FUNNAME &&
                     pExpr->value.asFunCall.pFunName->value.asSymbol.funcid ==
                     HB_F_ARRAYTOPARAMS ) )
               {
                  if( usItems )
                  {
                     HB_GEN_FUNC1( PushLong, usItems );
                     usItems = 0;
                  }
               }
               else
                  ++usItems;
               HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
               pExpr = pExpr->pNext;
            }
            if( usItems )
            {
               HB_GEN_FUNC1( PushLong, usItems );
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
            PHB_EXPR pNext, pExpr = pSelf->value.asList.pExprList;
            while( pExpr )
            {
               pNext = pExpr->pNext;    /* store next expression */
               HB_COMP_EXPR_FREE( pExpr );
               pExpr = pNext;
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
         PHB_EXPR pIdx;

         /* Clipper forces memvar context for undeclared variables used with
          * array index, f.e.: var[ n ]
          * but not for code like: ( var )[ n ]
          */
         if( pSelf->value.asList.pExprList->ExprType == HB_ET_VARIABLE )
         {
#if ! defined( HB_MACRO_SUPPORT )
            int iScope;
            hb_compVariableFind( HB_COMP_PARAM, pSelf->value.asList.pExprList->value.asSymbol.name, NULL, &iScope );
            if( iScope == HB_VS_UNDECLARED )
            {
               hb_compGenWarning( HB_COMP_PARAM, hb_comp_szWarnings, 'W', HB_COMP_WARN_MEMVAR_ASSUMED,
                                  pSelf->value.asList.pExprList->value.asSymbol.name, NULL );
#else
            if( hb_macroLocalVarGetPos( pSelf->value.asList.pExprList->value.asSymbol.name, HB_COMP_PARAM ) == 0 )
            {
#endif
               pSelf->value.asList.pExprList = hb_compExprNewAliasVar(
                              hb_compExprNewAlias( "MEMVAR", HB_COMP_PARAM ),
                              pSelf->value.asList.pExprList, HB_COMP_PARAM );
            }
         }
         pSelf->value.asList.pExprList = HB_EXPR_USE( pSelf->value.asList.pExprList, HB_EA_REDUCE );
         pSelf->value.asList.pIndex = HB_EXPR_USE( pSelf->value.asList.pIndex, HB_EA_REDUCE );
         pIdx = pSelf->value.asList.pIndex;
         if( pIdx->ExprType == HB_ET_NUMERIC )
         {
            PHB_EXPR pExpr = pSelf->value.asList.pExprList; /* the expression that holds an array */
            HB_ISIZ nIndex;

            if( pIdx->value.asNum.NumType == HB_ET_LONG )
               nIndex = ( HB_ISIZ ) pIdx->value.asNum.val.l;
            else
               nIndex = ( HB_ISIZ ) pIdx->value.asNum.val.d;

            if( pExpr->ExprType == HB_ET_ARRAY )   /* is it a literal array */
            {
               HB_SIZE nSize = hb_compExprParamListCheck( HB_COMP_PARAM, pExpr );

               if( pExpr->ExprType == HB_ET_MACROARGLIST )
                  /* restore original expression type */
                  pExpr->ExprType = HB_ET_ARRAY;
               else if( ! HB_IS_VALID_INDEX( nIndex, nSize ) )
               {
                  if( ! HB_SUPPORT_ARRSTR )
                     hb_compErrorBound( HB_COMP_PARAM, pIdx );
               }
               else
               {
                  pExpr = pExpr->value.asList.pExprList; /* the first element in the array */
                  while( --nIndex && pExpr )
                     pExpr = pExpr->pNext;

                  if( pExpr ) /* found ? */
                  {
                     /* extract a single expression from the array
                      */
                     PHB_EXPR pNew = HB_COMP_EXPR_NEW( HB_ET_NONE );
                     memcpy( pNew, pExpr, sizeof( HB_EXPR ) );
                     /* This will suppres releasing of memory occupied by components of
                      * the expression - we have just copied them into the new expression.
                      * This method is simpler then traversing the list and releasing all
                      * but this choosen one.
                      */
                     pExpr->ExprType = HB_ET_NONE;
                     /* Here comes the magic */
                     HB_COMP_EXPR_FREE( pSelf );
                     pSelf = pNew;
                  }
                  else if( ! HB_SUPPORT_ARRSTR )
                     hb_compErrorBound( HB_COMP_PARAM, pIdx );
               }
            }
#if 0
            else if( pExpr->ExprType == HB_ET_STRING && HB_SUPPORT_ARRSTR )   /* is it a literal string */
            {
               if( HB_IS_VALID_INDEX( nIndex, pExpr->nLength ) )
               {
                  HB_UCHAR ucValue = ( HB_UCHAR ) pExpr->value.asString.string[ nIndex - 1 ];

                  HB_COMP_EXPR_FREE( pSelf );
                  pSelf = hb_compExprNewLong( ucValue, HB_COMP_PARAM );
               }
               else
                  hb_compErrorBound( HB_COMP_PARAM, pIdx );
            }
#endif
            else if( ! HB_SUPPORT_ARRSTR )
            {
               HB_EXPR_USE( pExpr, HB_EA_ARRAY_AT );
            }
         }
         break;
      }

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;

      case HB_EA_PUSH_PCODE:
      {
         HB_BOOL fMacroIndex = HB_FALSE;

         if( pSelf->value.asList.pIndex->ExprType == HB_ET_MACRO )
         {
            if( HB_SUPPORT_XBASE )
            {
               if( pSelf->value.asMacro.SubType != HB_ET_MACRO_SYMBOL &&
                   pSelf->value.asMacro.SubType != HB_ET_MACRO_REFER &&
                   pSelf->value.asMacro.SubType != HB_ET_MACRO_ALIASED )
               {
                  pSelf->value.asList.pIndex->value.asMacro.SubType |= HB_ET_MACRO_LIST;
                  fMacroIndex = HB_TRUE;
               }
            }
         }
         else if( pSelf->value.asList.pIndex->ExprType == HB_ET_ARGLIST )
         {
            fMacroIndex = pSelf->value.asList.pIndex->value.asList.reference;
         }
         else if( pSelf->value.asList.pIndex->ExprType == HB_ET_FUNCALL &&
                  pSelf->value.asList.pIndex->value.asFunCall.pFunName->
                  value.asSymbol.funcid == HB_F_ARRAYTOPARAMS )
         {
            pSelf->value.asList.pIndex->value.asFunCall.pFunName->
                                        value.asSymbol.flags |= HB_FN_MULTIARG;
            fMacroIndex = HB_TRUE;
         }
         if( pSelf->value.asList.reference && HB_SUPPORT_ARRSTR )
         {
            PHB_EXPR pList = pSelf->value.asList.pExprList;
            if( pList->ExprType == HB_ET_VARIABLE )
            {
               /* Note: direct type change */
               pList->ExprType = HB_ET_VARREF;
               HB_EXPR_USE( pList, HB_EA_PUSH_PCODE );
               /* restore original expression type */
               pList->ExprType = HB_ET_VARIABLE;
            }
            else if( pList->ExprType == HB_ET_ALIASVAR &&
                     pList->value.asAlias.pVar->ExprType == HB_ET_VARIABLE &&
                     pList->value.asAlias.pAlias->ExprType == HB_ET_ALIAS &&
                     hb_compExprIsMemvarAlias( pList->value.asAlias.pAlias->value.asSymbol.name ) )
            {
               HB_GEN_FUNC1( PushMemvarRef, pList->value.asAlias.pVar->value.asSymbol.name );
            }
            else if( pList->ExprType == HB_ET_SEND )
            {
               hb_compExprPushSendPop( pList, HB_COMP_PARAM );
               HB_GEN_FUNC1( PCode1, HB_P_PUSHOVARREF );
            }
            else if( pList->ExprType == HB_ET_ARRAYAT &&
                     ! pList->value.asList.reference )
            {
               pList->value.asList.reference = HB_TRUE;
               HB_EXPR_USE( pList, HB_EA_PUSH_PCODE );
               pList->value.asList.reference = HB_FALSE;
            }
            else if( pList->ExprType == HB_ET_MACRO &&
                     pList->value.asMacro.SubType == HB_ET_MACRO_VAR )
            {
               pList->value.asMacro.SubType = HB_ET_MACRO_REFER;
               HB_EXPR_USE( pList, HB_EA_PUSH_PCODE );
               pList->value.asMacro.SubType = HB_ET_MACRO_VAR;
            }
            else
               HB_EXPR_USE( pList, HB_EA_PUSH_PCODE );
         }
         else
            HB_EXPR_USE( pSelf->value.asList.pExprList, HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asList.pIndex, HB_EA_PUSH_PCODE );
         if( fMacroIndex )
            HB_GEN_FUNC1( PCode1, HB_P_MACROPUSHINDEX );
         if( pSelf->value.asList.reference )
            HB_GEN_FUNC1( PCode1, HB_P_ARRAYPUSHREF );
         else
            HB_GEN_FUNC1( PCode1, HB_P_ARRAYPUSH );
         break;
      }

      case HB_EA_POP_PCODE:
      {
         HB_BOOL fMacroIndex = HB_FALSE;
         if( pSelf->value.asList.pIndex->ExprType == HB_ET_MACRO )
         {
            if( HB_SUPPORT_XBASE )
            {
               if( pSelf->value.asMacro.SubType != HB_ET_MACRO_SYMBOL &&
                   pSelf->value.asMacro.SubType != HB_ET_MACRO_REFER &&
                   pSelf->value.asMacro.SubType != HB_ET_MACRO_ALIASED )
               {
                  pSelf->value.asList.pIndex->value.asMacro.SubType |= HB_ET_MACRO_LIST;
                  fMacroIndex = HB_TRUE;
               }
            }
         }
         else if( pSelf->value.asList.pIndex->ExprType == HB_ET_ARGLIST )
         {
            fMacroIndex = pSelf->value.asList.pIndex->value.asList.reference;
         }
         else if( pSelf->value.asList.pIndex->ExprType == HB_ET_FUNCALL &&
                  pSelf->value.asList.pIndex->value.asFunCall.pFunName->
                  value.asSymbol.funcid == HB_F_ARRAYTOPARAMS )
         {
            pSelf->value.asList.pIndex->value.asFunCall.pFunName->
                                        value.asSymbol.flags |= HB_FN_MULTIARG;
            fMacroIndex = HB_TRUE;
         }
         /* to manage strings as bytes arrays, they must be pushed by reference */
         /* arrays also are passed by reference */
         if( HB_SUPPORT_ARRSTR )
         {
            PHB_EXPR pList = pSelf->value.asList.pExprList;
            if( pList->ExprType == HB_ET_VARIABLE )
            {
               /* Note: direct type change */
               pList->ExprType = HB_ET_VARREF;
               HB_EXPR_USE( pList, HB_EA_PUSH_PCODE );
               /* restore original expression type */
               pList->ExprType = HB_ET_VARIABLE;
            }
            else if( pList->ExprType == HB_ET_ALIASVAR &&
                     pList->value.asAlias.pVar->ExprType == HB_ET_VARIABLE &&
                     pList->value.asAlias.pAlias->ExprType == HB_ET_ALIAS &&
                     hb_compExprIsMemvarAlias( pList->value.asAlias.pAlias->value.asSymbol.name ) )
            {
               HB_GEN_FUNC1( PushMemvarRef, pList->value.asAlias.pVar->value.asSymbol.name );
            }
            else if( pList->ExprType == HB_ET_SEND )
            {
               hb_compExprPushSendPop( pList, HB_COMP_PARAM );
               HB_GEN_FUNC1( PCode1, HB_P_PUSHOVARREF );
            }
            else if( pList->ExprType == HB_ET_ARRAYAT &&
                     ! pList->value.asList.reference )
            {
               pList->value.asList.reference = HB_TRUE;
               HB_EXPR_USE( pList, HB_EA_PUSH_PCODE );
               pList->value.asList.reference = HB_FALSE;
            }
            else if( pList->ExprType == HB_ET_MACRO &&
                     pList->value.asMacro.SubType == HB_ET_MACRO_VAR )
            {
               pList->value.asMacro.SubType = HB_ET_MACRO_REFER;
               HB_EXPR_USE( pList, HB_EA_PUSH_PCODE );
               pList->value.asMacro.SubType = HB_ET_MACRO_VAR;
            }
            else
               HB_EXPR_USE( pList, HB_EA_PUSH_PCODE );
         }
         else
            HB_EXPR_USE( pSelf->value.asList.pExprList, HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asList.pIndex, HB_EA_PUSH_PCODE );
         if( fMacroIndex )
            HB_GEN_FUNC1( PCode1, HB_P_MACROPUSHINDEX );
         HB_GEN_FUNC1( PCode1, HB_P_ARRAYPOP );
         break;
      }

      case HB_EA_PUSH_POP:
         /* NOTE: This is highly optimized code - this will work even
          * if accessed value isn't an array. It will work also if
          * the index is invalid
          */
         HB_EXPR_USE( pSelf->value.asList.pExprList, HB_EA_PUSH_POP );
         HB_EXPR_USE( pSelf->value.asList.pIndex, HB_EA_PUSH_POP );
         /* no break */
      case HB_EA_STATEMENT:
         hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_DELETE:
         HB_COMP_EXPR_FREE( pSelf->value.asList.pExprList );
         HB_COMP_EXPR_FREE( pSelf->value.asList.pIndex );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMacro )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         if( pSelf->value.asMacro.pExprList )
            pSelf->value.asMacro.pExprList = HB_EXPR_USE( pSelf->value.asMacro.pExprList, HB_EA_REDUCE );
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;

      case HB_EA_PUSH_PCODE:
         if( pSelf->value.asMacro.SubType & HB_ET_MACRO_ASSIGN )
            HB_GEN_FUNC2( PushString, "_", 2 );

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
#if ! defined( HB_MACRO_SUPPORT )
               int iEarlyEvalPass = HB_COMP_PARAM->functions.pLast->iEarlyEvalPass;
               HB_GEN_FUNC1( PushVar, pSelf->value.asMacro.szMacro );
               HB_COMP_PARAM->functions.pLast->iEarlyEvalPass = iEarlyEvalPass;
#else
               HB_GEN_FUNC1( PushVar, pSelf->value.asMacro.szMacro );
#endif
            }
            else
            {
               /* complex macro expression: prefix&var.suffix
                * all components should be placed as a string that will
                * be compiled after text susbstitution
                */

               /* Check if macrotext variable does not refer to
                * local, static or field.
                */
#if ! defined( HB_MACRO_SUPPORT )
               hb_compPushMacroText( HB_COMP_PARAM,
                                     pSelf->value.asMacro.szMacro,
                                     strlen( pSelf->value.asMacro.szMacro ), HB_TRUE );
#else
               HB_GEN_FUNC2( PushString, pSelf->value.asMacro.szMacro, strlen( pSelf->value.asMacro.szMacro ) + 1 );
#endif
            }
         }

         if( pSelf->value.asMacro.SubType & HB_ET_MACRO_ASSIGN )
         {
            HB_GEN_FUNC1( PCode1, HB_P_PLUS );
            pSelf->value.asMacro.SubType &= ~HB_ET_MACRO_ASSIGN;
         }

         /* compile & run - leave a result on the eval stack
          */
         if( pSelf->value.asMacro.SubType == HB_ET_MACRO_SYMBOL )
            HB_GEN_FUNC1( PCode1, HB_P_MACROSYMBOL );

         else if( pSelf->value.asMacro.SubType == HB_ET_MACRO_REFER )
            HB_GEN_FUNC1( PCode1, HB_P_MACROPUSHREF );

         else if( pSelf->value.asMacro.SubType != HB_ET_MACRO_ALIASED )
         {
            if( HB_SUPPORT_XBASE )
            {
               if( pSelf->value.asMacro.SubType & HB_ET_MACRO_LIST )
               {
                  /* { &macro }, funCall( &macro ) or var[ &macro ] */
                  HB_GEN_FUNC1( PCode1, HB_P_MACROPUSHLIST );
               }
               else if( pSelf->value.asMacro.SubType & HB_ET_MACRO_PARE )
               {
                  /* var := (somevalue, &macro) - in Xbase++ compatibility mode
                   * EVAL( {|| &macro} ) - in all cases
                   */
                  HB_GEN_FUNC1( PCode1, HB_P_MACROPUSHPARE );
               }
               else
               {
                  /* usual &macro */
                  HB_GEN_FUNC1( PCode1, HB_P_MACROPUSH );
               }
            }
            else
               /* usual &macro */
               HB_GEN_FUNC1( PCode1, HB_P_MACROPUSH );

            /* Always add byte to pcode indicating requested macro compiler flag. */
            HB_GEN_FUNC1( PCode1, ( HB_BYTE ) HB_MACRO_GENFLAGS );
         }

         /* NOTE: pcode for alias context is generated in
          * hb_compExprUseAliasVar()
          */
         break;

      case HB_EA_POP_PCODE:
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
#if ! defined( HB_MACRO_SUPPORT )
               int iEarlyEvalPass = HB_COMP_PARAM->functions.pLast->iEarlyEvalPass;
               HB_GEN_FUNC1( PushVar, pSelf->value.asMacro.szMacro );
               HB_COMP_PARAM->functions.pLast->iEarlyEvalPass = iEarlyEvalPass;
#else
               HB_GEN_FUNC1( PushVar, pSelf->value.asMacro.szMacro );
#endif
            }
            else
            {
               /* complex macro expression: prefix&var.suffix
                * all components should be placed as a string that will
                * be compiled after text susbstitution
                */

               /* Check if macrotext variable does not refer to
                * local, static or field.
                */
#if ! defined( HB_MACRO_SUPPORT )
               hb_compPushMacroText( HB_COMP_PARAM,
                                     pSelf->value.asMacro.szMacro,
                                     strlen( pSelf->value.asMacro.szMacro ), HB_TRUE );
#else
               HB_GEN_FUNC2( PushString, pSelf->value.asMacro.szMacro, strlen( pSelf->value.asMacro.szMacro ) + 1 );
#endif
            }
         }
         /* compile & run - macro compiler will generate pcode to pop a value
          * from the eval stack
          */
         if( pSelf->value.asMacro.SubType != HB_ET_MACRO_ALIASED )
         {
            HB_GEN_FUNC1( PCode1, HB_P_MACROPOP );

            /* Always add byte to pcode indicating requested macro compiler flag. */
            HB_GEN_FUNC1( PCode1, ( HB_BYTE ) HB_MACRO_GENFLAGS );
         }
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_POP );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asMacro.pExprList )
            HB_COMP_EXPR_FREE( pSelf->value.asMacro.pExprList );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseFunCall )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         /* Reduce the expressions on the list of arguments
          */
         if( pSelf->value.asFunCall.pParms )
            pSelf->value.asFunCall.pParms = HB_EXPR_USE( pSelf->value.asFunCall.pParms, HB_EA_REDUCE );

         if( pSelf->value.asFunCall.pFunName->ExprType == HB_ET_FUNNAME )
         {
            HB_FUNC_ID funcID = pSelf->value.asFunCall.pFunName->value.asSymbol.funcid;
            PHB_EXPR pParms = pSelf->value.asFunCall.pParms;
            HB_USHORT usCount = ( HB_USHORT ) hb_compExprParamListLen( pParms );

#ifndef HB_MACRO_SUPPORT
            if( hb_compFunCallCheck( HB_COMP_PARAM, pSelf->value.asFunCall.pFunName->value.asSymbol.name, usCount ) )
#endif
            {
               switch( funcID )
               {
                  case HB_F_AT:
                     if( usCount == 2 )
                        hb_compExprReduceAT( pSelf, HB_COMP_PARAM );
                     break;
                  case HB_F_ASC:
                     if( usCount )
                        hb_compExprReduceASC( pSelf, HB_COMP_PARAM );
                     break;
                  case HB_F_CHR:
                     if( usCount )
                        hb_compExprReduceCHR( pSelf, HB_COMP_PARAM );
                     break;
                  case HB_F_LEN:
                     if( usCount )
                        hb_compExprReduceLEN( pSelf, HB_COMP_PARAM );
                     break;
                  case HB_F_UPPER:
                     if( usCount )
                        hb_compExprReduceUPPER( pSelf, HB_COMP_PARAM );
                     break;

                  case HB_F_EMPTY:
                     if( usCount && HB_SUPPORT_HARBOUR )
                        hb_compExprReduceEMPTY( pSelf, HB_COMP_PARAM );
                     break;
                  case HB_F_INT:
                     if( usCount == 1 && HB_SUPPORT_HARBOUR )
                        hb_compExprReduceINT( pSelf, HB_COMP_PARAM );
                     break;
                  case HB_F_MAX:
                     if( usCount == 2 && HB_SUPPORT_HARBOUR )
                        hb_compExprReduceMAX( pSelf, HB_COMP_PARAM );
                     break;
                  case HB_F_MIN:
                     if( usCount == 2 && HB_SUPPORT_HARBOUR )
                        hb_compExprReduceMIN( pSelf, HB_COMP_PARAM );
                     break;
                  case HB_F_STOD:
                     if( usCount < 2 && HB_SUPPORT_HARBOUR )
                        hb_compExprReduceSTOD( pSelf, usCount, HB_COMP_PARAM );
                     break;
                  case HB_F_STOT:
                     hb_compExprReduceSTOT( pSelf, usCount, HB_COMP_PARAM );
                     break;
                  case HB_F_DTOS:
                     if( usCount == 1 )
                        hb_compExprReduceDTOS( pSelf, HB_COMP_PARAM );
                     break;
                  case HB_F_CTOD:
                     if( usCount && HB_SUPPORT_HARBOUR )
                        hb_compExprReduceCTOD( pSelf, HB_COMP_PARAM );
                     break;

                  case HB_F_BCHAR:
                     if( usCount )
                        hb_compExprReduceBCHAR( pSelf, HB_COMP_PARAM );
                     break;
                  case HB_F_BCODE:
                     if( usCount )
                        hb_compExprReduceBCODE( pSelf, HB_COMP_PARAM );
                     break;

                  case HB_F_BITAND:
                  case HB_F_BITOR:
                  case HB_F_BITXOR:
                  case HB_F_BITSET:
                  case HB_F_BITRESET:
                  case HB_F_BITSHIFT:
                     if( usCount >= 2 &&
                         pParms->value.asList.pExprList->ExprType == HB_ET_NUMERIC &&
                         pParms->value.asList.pExprList->pNext->ExprType == HB_ET_NUMERIC )
                     {
                        PHB_EXPR pArg = pParms->value.asList.pExprList;
                        HB_MAXINT lResult = hb_compExprAsLongNum( pArg );
                        HB_BOOL fOptimize = HB_TRUE;

                        if( funcID == HB_F_BITAND )
                        {
                           while( --usCount )
                           {
                              pArg = pArg->pNext;
                              if( pArg->ExprType != HB_ET_NUMERIC )
                              {
                                 fOptimize = HB_FALSE;
                                 break;
                              }
                              lResult &= hb_compExprAsLongNum( pArg );
                           }
                        }
                        else if( funcID == HB_F_BITOR )
                        {
                           while( --usCount )
                           {
                              pArg = pArg->pNext;
                              if( pArg->ExprType != HB_ET_NUMERIC )
                              {
                                 fOptimize = HB_FALSE;
                                 break;
                              }
                              lResult |= hb_compExprAsLongNum( pArg );
                           }
                        }
                        else if( funcID == HB_F_BITXOR )
                        {
                           while( --usCount )
                           {
                              pArg = pArg->pNext;
                              if( pArg->ExprType != HB_ET_NUMERIC )
                              {
                                 fOptimize = HB_FALSE;
                                 break;
                              }
                              lResult ^= hb_compExprAsLongNum( pArg );
                           }
                        }
                        else if( funcID == HB_F_BITSET )
                        {
                           HB_MAXINT lBit = hb_compExprAsLongNum( pArg->pNext );
                           lResult |= ( HB_MAXINT ) 1 << lBit;
                        }
                        else if( funcID == HB_F_BITRESET )
                        {
                           HB_MAXINT lBit = hb_compExprAsLongNum( pArg->pNext );
                           lResult &= ~( ( HB_MAXINT ) 1 << lBit );
                        }
                        else /* if( funcID == HB_F_BITSHIFT ) */
                        {
                           HB_MAXINT lBits = hb_compExprAsLongNum( pArg->pNext );
                           if( lBits < 0 )
                              lResult >>= -lBits;
                           else
                              lResult <<= lBits;
                        }
                        if( fOptimize )
                           hb_compExprReduceBitFunc( pSelf, lResult, HB_FALSE, HB_COMP_PARAM );
                     }
                     break;
                  case HB_F_BITTEST:
                     if( usCount >= 2 &&
                         pParms->value.asList.pExprList->ExprType == HB_ET_NUMERIC &&
                         pParms->value.asList.pExprList->pNext->ExprType == HB_ET_NUMERIC )
                     {
                        PHB_EXPR pArg = pParms->value.asList.pExprList;
                        HB_MAXINT lBit = hb_compExprAsLongNum( pArg->pNext );
                        HB_MAXINT lResult = ( hb_compExprAsLongNum( pArg ) &
                                              ( ( HB_MAXINT ) 1 << lBit ) ) != 0;
                        hb_compExprReduceBitFunc( pSelf, lResult, HB_TRUE, HB_COMP_PARAM );
                     }
                     break;
                  case HB_F_BITNOT:
                     if( usCount && pParms->value.asList.pExprList->ExprType == HB_ET_NUMERIC )
                     {
                        HB_MAXINT lResult = ~hb_compExprAsLongNum( pParms->value.asList.pExprList );
                        hb_compExprReduceBitFunc( pSelf, lResult, HB_FALSE, HB_COMP_PARAM );
                     }
                     break;

#ifndef HB_MACRO_SUPPORT
                  case HB_F_I18N_GETTEXT:
                  case HB_F_I18N_GETTEXT_NOOP:
                  case HB_F_I18N_GETTEXT_STRICT:
                  case HB_F_I18N_NGETTEXT:
                  case HB_F_I18N_NGETTEXT_NOOP:
                  case HB_F_I18N_NGETTEXT_STRICT:
                  {
                     PHB_EXPR     pArg = pParms->value.asList.pExprList,
                                  pCount = NULL, pBadParam = NULL;
                     int          iWarning = 0;
                     const char * szExpect = NULL;
                     const char * szContext = NULL;
                     HB_BOOL      fStrict, fNoop, fPlural;

                     fStrict = funcID == HB_F_I18N_GETTEXT_STRICT ||
                               funcID == HB_F_I18N_NGETTEXT_STRICT;
                     fNoop   = funcID == HB_F_I18N_GETTEXT_NOOP ||
                               funcID == HB_F_I18N_NGETTEXT_NOOP;
                     fPlural = funcID == HB_F_I18N_NGETTEXT ||
                               funcID == HB_F_I18N_NGETTEXT_NOOP ||
                               funcID == HB_F_I18N_NGETTEXT_STRICT;

                     if( fPlural && usCount )
                     {
                        pCount = pArg;
                        pArg = pArg->pNext;
                        --usCount;
                        if( pCount->ExprType <= HB_ET_FUNREF &&
                            pCount->ExprType != HB_ET_NUMERIC )
                        {
                           iWarning = HB_COMP_WARN_PARAM_TYPE;
                           pBadParam = pCount;
                           szExpect = "Numeric expression";
                        }
                     }
                     if( usCount == 2 )
                     {
                        if( pArg->pNext->ExprType == HB_ET_STRING && pArg->pNext->nLength > 0 )
                        {
                           szContext = pArg->pNext->value.asString.string;
                           --usCount;
                        }
                        else
                        {
                           iWarning = HB_COMP_WARN_PARAM_TYPE;
                           pBadParam = pArg->pNext;
                           szExpect = "String";
                        }
                     }
                     if( iWarning == 0 )
                     {
                        const char * szPlurals[ HB_I18N_PLURAL_MAX ];
                        if( usCount == 1 )
                        {
                           if( pArg->ExprType == HB_ET_STRING )
                           {
                              if( HB_COMP_PARAM->fI18n && pArg->nLength > 0 )
                              {
                                 if( pCount )
                                 {
                                    szPlurals[ 0 ] = pArg->value.asString.string;
                                    hb_compI18nAddPlural( HB_COMP_PARAM, szPlurals, 1, szContext,
                                                          HB_COMP_PARAM->currModule, HB_COMP_PARAM->currLine );
                                 }
                                 else
                                    hb_compI18nAdd( HB_COMP_PARAM, pArg->value.asString.string, szContext,
                                                    HB_COMP_PARAM->currModule, HB_COMP_PARAM->currLine );
                              }
                           }
                           else if( pCount && pArg->ExprType == HB_ET_ARRAY &&
                                    hb_compExprListTypeCheck( pArg, HB_ET_STRING ) )
                           {
                              if( HB_COMP_PARAM->fI18n )
                              {
                                 HB_ULONG ulLen = hb_compExprListLen( pArg ), ul;
                                 PHB_EXPR pArgExp = pArg->value.asList.pExprList;

                                 if( ulLen > HB_I18N_PLURAL_MAX )
                                    ulLen = HB_I18N_PLURAL_MAX;
                                 for( ul = 0; ul < ulLen; ++ul )
                                 {
                                    szPlurals[ ul ] = pArgExp->value.asString.string;
                                    pArgExp = pArgExp->pNext;
                                 }
                                 hb_compI18nAddPlural( HB_COMP_PARAM, szPlurals, ulLen, szContext,
                                                       HB_COMP_PARAM->currModule, HB_COMP_PARAM->currLine );
                              }
                           }
                           else if( fStrict || fNoop || pArg->ExprType <= HB_ET_FUNREF )
                           {
                              iWarning = HB_COMP_WARN_PARAM_TYPE;
                              pBadParam = pArg;
                              szExpect = fPlural ? "String or Array of Strings" : "String";
                           }
                        }
                        else
                           iWarning = HB_COMP_WARN_PARAM_COUNT;
                     }
                     if( iWarning != 0 )
                     {
                        /* TODO: warning message does not fit very well, because it requires
                         *       type of used parameter. Let's print "unknown", to avoid deeper
                         *       analysis of parameter.
                         */
                        if( iWarning == HB_COMP_WARN_PARAM_TYPE )
                           hb_compGenWarning( HB_COMP_PARAM, hb_comp_szWarnings, 'W', HB_COMP_WARN_PARAM_TYPE,
                                              pBadParam && pBadParam->ExprType > HB_ET_NONE &&
                                              pBadParam->ExprType <= HB_ET_FUNREF ?
                                              hb_compExprDescription( pBadParam ) : "Unknown", szExpect );
                        else
                        {
                           char buf[ 16 ];
                           hb_snprintf( buf, sizeof( buf ), "%d", ( int ) usCount + ( pCount ? 1 : 0 ) + ( szContext ? 1 : 0 ) );
                           hb_compGenWarning( HB_COMP_PARAM, hb_comp_szWarnings, 'W', HB_COMP_WARN_PARAM_COUNT, buf, fPlural ? "2 or 3" : "1 or 2" );
                        }
                     }
                     /* hb_i18n_gettext_noop() is not a real function. It is used to
                        force writing of string to .pot file. So, we should try to
                        replace function call by first argument regardless fI18n flag
                        and warnings.
                      */
                     else if( fNoop && usCount )
                     {
                        pParms->value.asList.pExprList = pArg->pNext; /* skip first parameter */
                        pArg->pNext = NULL;
                        HB_COMP_EXPR_FREE( pParms );
                        HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
                        if( pCount )
                        {
                           if( pArg->ExprType == HB_ET_ARRAY )
                           {
                              if( hb_compExprListLen( pArg ) == 1 )
                              {
                                 HB_COMP_EXPR_FREE( pCount );
                                 pCount = pArg;
                                 pArg = pArg->value.asList.pExprList;
                                 pCount->value.asList.pExprList = NULL;
                              }
                              else
                              {
                                 /* build expression: pArray[ iif( pCount == 1, 1, 2 ) ] */
                                 PHB_EXPR pIndex;

                                 /* create pCount == 1 */
                                 pIndex = hb_compExprSetOperand( hb_compExprNewEQ( pCount, HB_COMP_PARAM ),
                                                                 hb_compExprNewLong( 1, HB_COMP_PARAM ), HB_COMP_PARAM );
                                 /* create: ( pCount == 1, */
                                 pIndex = hb_compExprNewList( pIndex, HB_COMP_PARAM );
                                 /* create: ( pCount == 1, 1, */
                                 pIndex = hb_compExprAddListExpr( pIndex, hb_compExprNewLong( 1, HB_COMP_PARAM ) );
                                 /* create: ( pCount == 1, 1, 2 )*/
                                 pIndex = hb_compExprAddListExpr( pIndex, hb_compExprNewLong( 2, HB_COMP_PARAM ) );
                                 /* create: IIF() expression */
                                 pIndex = hb_compExprNewIIF( pIndex );
                                 /* create: pArray[ iif( pCount == 1, 1, 2 ) ] */
                                 pArg = hb_compExprNewArrayAt( pArg, pIndex, HB_COMP_PARAM );
                                 /* reduce the final expression */
                                 pArg = HB_EXPR_USE( pArg, HB_EA_REDUCE );
                                 pCount = NULL;
                              }
                           }
                           if( pCount )
                              HB_COMP_EXPR_FREE( pCount );
                        }
                        memcpy( pSelf, pArg, sizeof( HB_EXPR ) );
                        /* free pArg expression body but without freeing its subexpressions */
                        HB_COMP_EXPR_CLEAR( pArg );
                     }

                     break;
                  }
#endif
                  default:
                     /* to pacify enum warning */
                     break;
               }
            }
         }
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
      {
         HB_BOOL fArgsList = HB_FALSE;
         HB_USHORT usCount = 0;

         if( pSelf->value.asFunCall.pFunName->ExprType == HB_ET_FUNNAME )
         {
            if( pSelf->value.asFunCall.pFunName->value.asSymbol.funcid == HB_F_ARRAYTOPARAMS )
            {
               usCount = ( HB_USHORT ) hb_compExprParamListCheck( HB_COMP_PARAM, pSelf->value.asFunCall.pParms );
               if( usCount == 1 &&
                   ( pSelf->value.asFunCall.pFunName->value.asSymbol.flags & HB_FN_MULTIARG ) != 0 &&
                   pSelf->value.asFunCall.pParms->ExprType != HB_ET_MACROARGLIST )
               {
                  HB_EXPR_USE( pSelf->value.asFunCall.pParms, HB_EA_PUSH_PCODE );
                  HB_GEN_FUNC1( PCode1, HB_P_PUSHAPARAMS );
                  break;
               }
            }
            HB_GEN_FUNC2( PushFunCall, pSelf->value.asFunCall.pFunName->value.asSymbol.name,
                                       pSelf->value.asFunCall.pFunName->value.asSymbol.flags );
         }
         else
         {
            HB_EXPR_USE( pSelf->value.asFunCall.pFunName, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_PUSHNIL );
         }

         /* NOTE: pParms will be NULL in 'DO procname' (if there is
          * no WITH keyword)
          */
         if( pSelf->value.asFunCall.pParms )
         {
            usCount = ( HB_USHORT ) hb_compExprParamListCheck( HB_COMP_PARAM, pSelf->value.asFunCall.pParms );
            fArgsList = pSelf->value.asFunCall.pParms->ExprType == HB_ET_MACROARGLIST;
            if( usCount )
               HB_EXPR_USE( pSelf->value.asFunCall.pParms, HB_EA_PUSH_PCODE );
         }

         if( fArgsList )
         {
            HB_GEN_FUNC3( PCode3, HB_P_MACROFUNC, HB_LOBYTE( usCount ), HB_HIBYTE( usCount ) );
            /* restore original expression type */
            pSelf->value.asFunCall.pParms->ExprType = HB_ET_ARGLIST;
         }
         else if( usCount > 255 )
            HB_GEN_FUNC3( PCode3, HB_P_FUNCTION, HB_LOBYTE( usCount ), HB_HIBYTE( usCount ) );
         else
            HB_GEN_FUNC2( PCode2, HB_P_FUNCTIONSHORT, ( HB_BYTE ) usCount );
         break;
      }
      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
      {
         HB_BOOL fArgsList = HB_FALSE;
         HB_USHORT usCount = 0;

         if( pSelf->value.asFunCall.pFunName->ExprType == HB_ET_FUNNAME )
         {
            HB_GEN_FUNC2( PushFunCall, pSelf->value.asFunCall.pFunName->value.asSymbol.name,
                                       pSelf->value.asFunCall.pFunName->value.asSymbol.flags );
         }
         else
         {
            HB_EXPR_USE( pSelf->value.asFunCall.pFunName, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_PUSHNIL );
         }

         if( pSelf->value.asFunCall.pParms )
         {
            usCount = ( HB_USHORT ) hb_compExprParamListCheck( HB_COMP_PARAM, pSelf->value.asFunCall.pParms );
            fArgsList = pSelf->value.asFunCall.pParms->ExprType == HB_ET_MACROARGLIST;
            if( usCount )
               HB_EXPR_USE( pSelf->value.asFunCall.pParms, HB_EA_PUSH_PCODE );
         }

         if( fArgsList )
         {
            HB_GEN_FUNC3( PCode3, HB_P_MACRODO, HB_LOBYTE( usCount ), HB_HIBYTE( usCount ) );
            /* restore original expression type */
            pSelf->value.asFunCall.pParms->ExprType = HB_ET_ARGLIST;
         }
         else if( usCount > 255 )
            HB_GEN_FUNC3( PCode3, HB_P_DO, HB_LOBYTE( usCount ), HB_HIBYTE( usCount ) );
         else
            HB_GEN_FUNC2( PCode2, HB_P_DOSHORT, ( HB_BYTE ) usCount );
         break;
      }
      case HB_EA_DELETE:
         if( pSelf->value.asFunCall.pParms )
            HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pParms );
         HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
         break;
   }
   return pSelf;
}

/* handler for expression->identifier syntax
 */
static HB_EXPR_FUNC( hb_compExprUseAliasVar )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_compExprUseAliasVar()" ) );

   switch( iMessage )
   {
      case HB_EA_REDUCE:
         /* NOTE: direct reduction not used for HB_ET_LIST to avoid
          *       list stripping before PUSH/POP operations
          */
         if( pSelf->value.asAlias.pAlias->ExprType == HB_ET_LIST )
         {
            pSelf->value.asAlias.pAlias = hb_compExprReduceList(
                                 pSelf->value.asAlias.pAlias, HB_COMP_PARAM );
            if( HB_SUPPORT_EXTOPT &&
                pSelf->value.asAlias.pAlias->value.asList.pExprList->ExprType == HB_ET_STRING &&
                pSelf->value.asAlias.pAlias->value.asList.pExprList->pNext == NULL )
            {
               pSelf->value.asAlias.pAlias = hb_compExprReduceAliasString(
                           pSelf->value.asAlias.pAlias,
                           pSelf->value.asAlias.pAlias->value.asList.pExprList,
                           HB_COMP_PARAM );
            }
         }
         else
            pSelf->value.asAlias.pAlias = HB_EXPR_USE( pSelf->value.asAlias.pAlias, HB_EA_REDUCE );
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;

      case HB_EA_PUSH_PCODE:
      {
         PHB_EXPR pAlias = pSelf->value.asAlias.pAlias;

         if( pAlias->ExprType == HB_ET_MACRO ||
             pSelf->value.asAlias.pVar->ExprType == HB_ET_MACRO )
         {
            /* Macro operator is used on the left or right side of an alias
             * operator - handle it with a special care
             */
            hb_compExprUseAliasMacro( pSelf, HB_EA_PUSH_PCODE, HB_COMP_PARAM );
         }
         else if( pAlias->ExprType == HB_ET_ALIAS )
         {
            /*
             * myalias->var
             * FIELD->var
             * MEMVAR->var
             *
             * NOTE: HB_TRUE = push also alias
             */
            HB_GEN_FUNC4( PushAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol.name, HB_TRUE, pAlias->value.asSymbol.name, 0 );
         }
         else if( pAlias->ExprType == HB_ET_NUMERIC )
         {
            /* numeric alias
             * 2->var
             *
             * NOTE: only integer (long) values are allowed
             */
            if( pAlias->value.asNum.NumType == HB_ET_LONG )
               HB_GEN_FUNC4( PushAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol.name, HB_TRUE, NULL, pAlias->value.asNum.val.l );
            else
               hb_compErrorAlias( HB_COMP_PARAM, pAlias );
         }
         else if( pAlias->ExprType == HB_ET_LIST )
         {
            /*
             * ( expression )->var
             *
             * NOTE: HB_FALSE = don't push alias value
             */
            HB_EXPR_USE( pAlias, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC4( PushAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol.name, HB_FALSE, NULL, 0 );
         }
         else
            hb_compErrorAlias( HB_COMP_PARAM, pAlias );
         break;
      }
      case HB_EA_POP_PCODE:
      {
         PHB_EXPR pAlias = pSelf->value.asAlias.pAlias;

         if( pAlias->ExprType == HB_ET_MACRO || pSelf->value.asAlias.pVar->ExprType == HB_ET_MACRO )
         {
            /* Macro operator is used on the left or right side of an alias
             * operator - handle it with a special care
             * (we need convert to a string the whole expression)
             */
            hb_compExprUseAliasMacro( pSelf, HB_EA_POP_PCODE, HB_COMP_PARAM );
         }
         else if( pAlias->ExprType == HB_ET_ALIAS )
         {
            /*
             * myalias->var
             * FIELD->var
             * MEMVAR->var
             */
            HB_GEN_FUNC4( PopAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol.name, HB_TRUE, pAlias->value.asSymbol.name, 0 );
         }
         else if( pAlias->ExprType == HB_ET_NUMERIC )
         {
            /* numeric alias
             * 2->var
             *
             * NOTE: only integer (long) values are allowed
             */
            if( pAlias->value.asNum.NumType == HB_ET_LONG )
               HB_GEN_FUNC4( PopAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol.name, HB_TRUE, NULL, pAlias->value.asNum.val.l );
            else
               hb_compErrorAlias( HB_COMP_PARAM, pAlias );
         }
         else if( pAlias->ExprType == HB_ET_LIST )
         {
            /*
             * ( expression )->var
             *
             * NOTE: HB_FALSE = don't push alias value
             */
            HB_EXPR_USE( pAlias, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC4( PopAliasedVar, pSelf->value.asAlias.pVar->value.asSymbol.name, HB_FALSE, NULL, 0 );
         }
         else
            hb_compErrorAlias( HB_COMP_PARAM, pAlias );
         break;
      }
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_POP );
         break;

      case HB_EA_DELETE:
         HB_COMP_EXPR_FREE( pSelf->value.asAlias.pAlias );
         if( pSelf->value.asAlias.pVar )
            HB_COMP_EXPR_FREE( pSelf->value.asAlias.pVar );
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
         pSelf->value.asAlias.pAlias   = HB_EXPR_USE( pSelf->value.asAlias.pAlias, HB_EA_REDUCE );
         pSelf->value.asAlias.pExpList = HB_EXPR_USE( pSelf->value.asAlias.pExpList, HB_EA_REDUCE );
         if( HB_SUPPORT_EXTOPT && pSelf->value.asAlias.pAlias->ExprType == HB_ET_STRING )
            pSelf->value.asAlias.pAlias = hb_compExprReduceAliasString(
                                 pSelf->value.asAlias.pAlias,
                                 pSelf->value.asAlias.pAlias, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         /* save currently selected workarea
          */
         HB_GEN_FUNC1( PCode1, HB_P_PUSHALIAS );
         /* push the expression that will return a new workarea
          */
         HB_EXPR_USE( pSelf->value.asAlias.pAlias, HB_EA_PUSH_PCODE );
         /* pop the value from the stack and select it as current workarea
          */
         HB_GEN_FUNC1( PCode1, HB_P_POPALIAS );
         /* evaluate any expression
          */
         HB_EXPR_USE( pSelf->value.asAlias.pExpList, HB_EA_PUSH_PCODE );
         /* swap the two last items on the eval stack: one item is a
          * value returned by evaluated expression and the second item
          * is previously selected workarea. After swaping select again
          * the restored workarea.
          */
         HB_GEN_FUNC1( PCode1, HB_P_SWAPALIAS );
         break;

      case HB_EA_POP_PCODE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         /* save currently selected workarea
          */
         HB_GEN_FUNC1( PCode1, HB_P_PUSHALIAS );
         /* push the expression that will return a new workarea
          */
         HB_EXPR_USE( pSelf->value.asAlias.pAlias, HB_EA_PUSH_PCODE );
         /* pop the value from the stack and select it as current workarea
          */
         HB_GEN_FUNC1( PCode1, HB_P_POPALIAS );
         /* evaluate any expression - it will not leave any return
          * value on the eval stack
          */
         HB_EXPR_USE( pSelf->value.asAlias.pExpList, HB_EA_PUSH_POP );
         /* Pop and select again the restored workarea.
          */
         HB_GEN_FUNC1( PCode1, HB_P_POPALIAS );
         break;

      case HB_EA_DELETE:
         HB_COMP_EXPR_FREE( pSelf->value.asAlias.pAlias );
         HB_COMP_EXPR_FREE( pSelf->value.asAlias.pExpList );
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
         HB_GEN_FUNC2( PushSymbol, pSelf->value.asSymbol.name, HB_SYM_ALIAS );
         break;

      case HB_EA_POP_PCODE:
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         break;

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
         HB_GEN_FUNC2( PushFunSym, pSelf->value.asSymbol.name,
                                   pSelf->value.asSymbol.flags );
         break;

      case HB_EA_POP_PCODE:
      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
      case HB_EA_DELETE:
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
            HB_GEN_FUNC2( PushSymbol, pSelf->value.asRTVar.szName, HB_SYM_MEMVAR );  /* this is not a function */
         else
            HB_EXPR_USE( pSelf->value.asRTVar.pMacro, HB_EA_PUSH_PCODE );
         break;
      case HB_EA_POP_PCODE:
         if( pSelf->value.asRTVar.szName )
            HB_GEN_FUNC1( PopMemvar, pSelf->value.asRTVar.szName );
         else
            HB_EXPR_USE( pSelf->value.asRTVar.pMacro, HB_EA_POP_PCODE );
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         break;
      case HB_EA_DELETE:
#if ! defined( HB_MACRO_SUPPORT )
         if( ! pSelf->value.asRTVar.szName )
            HB_COMP_EXPR_FREE( pSelf->value.asRTVar.pMacro );
#endif
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
         /* NOTE: When the following syntax is used:
          *    ( any_expr )->&var2
          * then macro compiler is compiling the right side of alias
          * operator only (if 'any_expr' is not a string) - an alias value
          * is placed on the eval stack before macro compilation.
          * The HB_MACRO_GEN_ALIASED flag is used to signal that we have to
          * genearate alias aware pcode even if we known a variable part only.
          */
         if( HB_MACRO_DATA->Flags & HB_MACRO_GEN_ALIASED )
            HB_GEN_FUNC4( PushAliasedVar, pSelf->value.asSymbol.name, HB_FALSE, NULL, 0 );
         else
            HB_GEN_FUNC1( PushVar, pSelf->value.asSymbol.name );
#else
         HB_GEN_FUNC1( PushVar, pSelf->value.asSymbol.name );
#endif
         break;

      case HB_EA_POP_PCODE:
#if defined( HB_MACRO_SUPPORT )
         if( HB_MACRO_DATA->Flags & HB_MACRO_GEN_ALIASED )
            HB_GEN_FUNC4( PopAliasedVar, pSelf->value.asSymbol.name, HB_FALSE, NULL, 0 );
         else
            HB_GEN_FUNC1( PopVar, pSelf->value.asSymbol.name );
#else
         HB_GEN_FUNC1( PopVar, pSelf->value.asSymbol.name );
#endif
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_GEN_FUNC1( PushVar, pSelf->value.asSymbol.name );
         HB_GEN_FUNC1( PCode1, HB_P_POP );
         break;

      case HB_EA_DELETE:
         break;
   }
   return pSelf;
}

/* IIF( <pVar>==NIL, <pExpr>, <pExpr>:=<pVar> ) */
static HB_EXPR_FUNC( hb_compExprUseSetGet )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asSetGet.pVar = HB_EXPR_USE( pSelf->value.asSetGet.pVar, HB_EA_REDUCE );
         pSelf->value.asSetGet.pExpr = HB_EXPR_USE( pSelf->value.asSetGet.pExpr, HB_EA_REDUCE );
#if ! defined( HB_MACRO_SUPPORT )
         if( ! HB_SUPPORT_HARBOUR )
            pSelf->value.asSetGet.pVar = hb_compExprListStrip( pSelf->value.asSetGet.pVar, HB_COMP_PARAM );
#endif
         HB_EXPR_USE( pSelf->value.asSetGet.pVar, HB_EA_LVALUE );
         break;
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;
      case HB_EA_PUSH_PCODE:
      {
         HB_ISIZ nPosFalse, nPosEnd;

         /* <pVar>==NIL */
         HB_EXPR_USE( pSelf->value.asSetGet.pVar, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_PUSHNIL );
         HB_GEN_FUNC1( PCode1, HB_P_EXACTLYEQUAL );
         nPosFalse = HB_GEN_FUNC1( JumpFalse, 0 );
         /* <pExpr> */
         HB_EXPR_USE( pSelf->value.asSetGet.pExpr, HB_EA_PUSH_PCODE );
         nPosEnd = HB_GEN_FUNC1( Jump, 0 );
         /* <pExpr>:=<pVar> */
         HB_GEN_FUNC1( JumpHere, nPosFalse );
         if( pSelf->value.asSetGet.pExpr->ExprType == HB_ET_SEND )
         {
            PHB_EXPR pObj, pParams;
            pObj = pSelf->value.asSetGet.pExpr;
            pParams = pObj->value.asMessage.pParms;
            pObj->value.asMessage.pParms = pSelf->value.asSetGet.pVar;
            HB_EXPR_USE( pObj, HB_EA_POP_PCODE );
            pObj->value.asMessage.pParms = pParams;
         }
         else
         {
            HB_EXPR_USE( pSelf->value.asSetGet.pVar, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_PUSHUNREF );
            HB_EXPR_USE( pSelf->value.asSetGet.pExpr, HB_EA_POP_PCODE );
         }
         HB_GEN_FUNC1( JumpHere, nPosEnd );
         break;
      }
      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
      {
         HB_ISIZ nPosFalse, nPosEnd;

         /* <pVar>==NIL */
         HB_EXPR_USE( pSelf->value.asSetGet.pVar, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_PUSHNIL );
         HB_GEN_FUNC1( PCode1, HB_P_EXACTLYEQUAL );
         nPosFalse = HB_GEN_FUNC1( JumpFalse, 0 );
         /* <pExpr> */
         HB_EXPR_USE( pSelf->value.asSetGet.pExpr, HB_EA_PUSH_PCODE );
         nPosEnd = HB_GEN_FUNC1( Jump, 0 );
         /* <pExpr>:=<pVar> */
         HB_GEN_FUNC1( JumpHere, nPosFalse );
         if( pSelf->value.asSetGet.pExpr->ExprType == HB_ET_SEND )
         {
            PHB_EXPR pObj, pParams;
            pObj = pSelf->value.asSetGet.pExpr;
            pParams = pObj->value.asMessage.pParms;
            pObj->value.asMessage.pParms = pSelf->value.asSetGet.pVar;
            HB_EXPR_USE( pObj, HB_EA_POP_PCODE );
            pObj->value.asMessage.pParms = pParams;
            /* Remove the return value */
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf->value.asSetGet.pVar, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asSetGet.pExpr, HB_EA_POP_PCODE );
         }
         HB_GEN_FUNC1( JumpHere, nPosEnd );
         break;
      }

      case HB_EA_DELETE:
         HB_COMP_EXPR_FREE( pSelf->value.asSetGet.pExpr );
         HB_COMP_EXPR_FREE( pSelf->value.asSetGet.pVar );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseSend )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         /* Clipper does not reduce object expressions */
         if( pSelf->value.asMessage.pObject &&
             ( HB_SUPPORT_HARBOUR || pSelf->nLength == 1 ) )
            pSelf->value.asMessage.pObject = HB_EXPR_USE( pSelf->value.asMessage.pObject, HB_EA_REDUCE );
         if( pSelf->value.asMessage.pParms )  /* Is it a method call ? */
            pSelf->value.asMessage.pParms = HB_EXPR_USE( pSelf->value.asMessage.pParms, HB_EA_REDUCE );
         break;

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         if( pSelf->value.asMessage.pParms )
            hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         if( pSelf->value.asMessage.pParms )  /* Is it a method call ? */
         {
            HB_BOOL fArgsList = HB_FALSE;
            int iParms = ( int ) hb_compExprParamListCheck( HB_COMP_PARAM, pSelf->value.asMessage.pParms );

            hb_compExprPushSendPush( pSelf, HB_COMP_PARAM );
            if( iParms )
            {
               HB_EXPR_USE( pSelf->value.asMessage.pParms, HB_EA_PUSH_PCODE );
               fArgsList = pSelf->value.asMessage.pParms->ExprType == HB_ET_MACROARGLIST;
            }

            if( fArgsList )
            {
               HB_GEN_FUNC3( PCode3, HB_P_MACROSEND, HB_LOBYTE( iParms ), HB_HIBYTE( iParms ) );
               /* restore original expression type */
               pSelf->value.asMessage.pParms->ExprType = HB_ET_ARGLIST;
            }
            else if( iParms > 255 )
               HB_GEN_FUNC3( PCode3, HB_P_SEND, HB_LOBYTE( iParms ), HB_HIBYTE( iParms ) );
            else
               HB_GEN_FUNC2( PCode2, HB_P_SENDSHORT, ( HB_BYTE ) iParms );
         }
         else
         {
            /* acces to instance variable */
            hb_compExprPushSendPush( pSelf, HB_COMP_PARAM );
            HB_GEN_FUNC2( PCode2, HB_P_SENDSHORT, 0 );
         }
         break;

      case HB_EA_POP_PCODE:
         hb_compExprPushSendPop( pSelf, HB_COMP_PARAM );
         if( pSelf->value.asMessage.pParms )
         {
            HB_EXPR_USE( pSelf->value.asMessage.pParms, HB_EA_PUSH_PCODE );
         }
         else
         {
            /* executed from macro compiler */
            HB_GEN_FUNC2( PCode2, HB_P_SWAP, 1 );
            HB_GEN_FUNC2( PCode2, HB_P_SWAP, 1 );
         }
         HB_GEN_FUNC2( PCode2, HB_P_SENDSHORT, 1 );
         if( ! pSelf->value.asMessage.pParms )
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_POP );
         if( ! pSelf->value.asMessage.pParms )  /* Is it a method call ? */
         {
            /* instance variable */
            /* QUESTION: This warning can be misleading if nested messages
             * are used, e.g. a:b():c - should we generate it ?
             */
            hb_compWarnMeaningless( HB_COMP_PARAM, pSelf );
         }
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asMessage.pObject )
            HB_COMP_EXPR_FREE( pSelf->value.asMessage.pObject );
         if( pSelf->value.asMessage.pParms )
            HB_COMP_EXPR_FREE( pSelf->value.asMessage.pParms );
         if( pSelf->value.asMessage.pMessage )
            HB_COMP_EXPR_FREE( pSelf->value.asMessage.pMessage );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePostInc )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
#if ! defined( HB_MACRO_SUPPORT )
         if( ! HB_SUPPORT_HARBOUR )
            pSelf->value.asOperator.pLeft = hb_compExprListStrip( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
#endif
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         hb_compExprPushPostOp( pSelf, HB_P_INC, HB_COMP_PARAM );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         /* a++ used standalone as a statement is the same as ++a
          */
         hb_compExprUsePreOp( pSelf, HB_P_INC, HB_COMP_PARAM );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            HB_COMP_EXPR_FREE( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePostDec )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
#if ! defined( HB_MACRO_SUPPORT )
         if( ! HB_SUPPORT_HARBOUR )
            pSelf->value.asOperator.pLeft = hb_compExprListStrip( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
#endif
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;
      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         hb_compExprPushPostOp( pSelf, HB_P_DEC, HB_COMP_PARAM );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUsePreOp( pSelf, HB_P_DEC, HB_COMP_PARAM );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            HB_COMP_EXPR_FREE( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseAssign )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
      {
         PHB_EXPR pExpr;

         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
#if ! defined( HB_MACRO_SUPPORT )
         if( ! HB_SUPPORT_HARBOUR )
            pSelf->value.asOperator.pLeft = hb_compExprListStrip( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
#endif
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );

         /* optimize:
          *    var := var <op> <exp>
          * to:
          *    var <op>= <exp>
          */
         pExpr = pSelf->value.asOperator.pRight;
         if( HB_SUPPORT_HARBOUR &&
             pSelf->value.asOperator.pLeft->ExprType == HB_ET_VARIABLE &&
             ( pExpr->ExprType == HB_EO_PLUS || pExpr->ExprType == HB_EO_MINUS ||
               pExpr->ExprType == HB_EO_MULT || pExpr->ExprType == HB_EO_DIV ||
               pExpr->ExprType == HB_EO_MOD  || pExpr->ExprType == HB_EO_POWER ) &&
             pExpr->value.asOperator.pLeft->ExprType == HB_ET_VARIABLE &&
             strcmp( pSelf->value.asOperator.pLeft->value.asSymbol.name,
                     pExpr->value.asOperator.pLeft->value.asSymbol.name ) == 0 )
         {
            /* NOTE: direct type change */
            switch( pExpr->ExprType )
            {
               case HB_EO_PLUS:
                  pSelf->ExprType = HB_EO_PLUSEQ;
                  break;
               case HB_EO_MINUS:
                  pSelf->ExprType = HB_EO_MINUSEQ;
                  break;
               case HB_EO_MULT:
                  pSelf->ExprType = HB_EO_MULTEQ;
                  break;
               case HB_EO_DIV:
                  pSelf->ExprType = HB_EO_DIVEQ;
                  break;
               case HB_EO_MOD:
                  pSelf->ExprType = HB_EO_MODEQ;
                  break;
               case HB_EO_POWER:
                  pSelf->ExprType = HB_EO_EXPEQ;
                  break;
            }
            pSelf->value.asOperator.pRight = pExpr->value.asOperator.pRight;
            pExpr->value.asOperator.pRight = NULL;
            HB_COMP_EXPR_FREE( pExpr );
         }
         break;
      }

      case HB_EA_ARRAY_AT:
      case HB_EA_ARRAY_INDEX:
      case HB_EA_LVALUE:
         break;

      case HB_EA_PUSH_PCODE:
         /* NOTE: assigment to an object instance variable needs special handling
          */
         if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
         {
            PHB_EXPR pObj, pParams;
            pObj = pSelf->value.asOperator.pLeft;
            pParams = pObj->value.asMessage.pParms;
            pObj->value.asMessage.pParms = pSelf->value.asOperator.pRight;
            HB_EXPR_USE( pObj, HB_EA_POP_PCODE );
            pObj->value.asMessage.pParms = pParams;
         }
         else
         {
            /* it assigns a value and leaves it on the stack */

            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            /* QUESTION: Can  we replace DUPLICATE+POP with a single PUT opcode
             */
            HB_GEN_FUNC1( PCode1, HB_P_PUSHUNREF );
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         /* NOTE: assigment to an object instance variable needs special handling
          */
         if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
         {
            PHB_EXPR pObj, pParams;
            pObj = pSelf->value.asOperator.pLeft;
            pParams = pObj->value.asMessage.pParms;
            pObj->value.asMessage.pParms = pSelf->value.asOperator.pRight;
            HB_EXPR_USE( pObj, HB_EA_POP_PCODE );
            pObj->value.asMessage.pParms = pParams;
            /* Remove the return value */
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         else
         {
            /* it assigns a value and removes it from the stack */
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
         }
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePlusEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
#if ! defined( HB_MACRO_SUPPORT )
         if( ! HB_SUPPORT_HARBOUR )
            pSelf->value.asOperator.pLeft = hb_compExprListStrip( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
#endif
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         hb_compExprPushOperEq( pSelf, HB_P_PLUS, HB_COMP_PARAM );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUseOperEq( pSelf, HB_P_PLUS, HB_COMP_PARAM );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMinusEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
#if ! defined( HB_MACRO_SUPPORT )
         if( ! HB_SUPPORT_HARBOUR )
            pSelf->value.asOperator.pLeft = hb_compExprListStrip( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
#endif
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         hb_compExprPushOperEq( pSelf, HB_P_MINUS, HB_COMP_PARAM );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUseOperEq( pSelf, HB_P_MINUS, HB_COMP_PARAM );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMultEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
#if ! defined( HB_MACRO_SUPPORT )
         if( ! HB_SUPPORT_HARBOUR )
            pSelf->value.asOperator.pLeft = hb_compExprListStrip( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
#endif
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         hb_compExprPushOperEq( pSelf, HB_P_MULT, HB_COMP_PARAM );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUseOperEq( pSelf, HB_P_MULT, HB_COMP_PARAM );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseDivEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
#if ! defined( HB_MACRO_SUPPORT )
         if( ! HB_SUPPORT_HARBOUR )
            pSelf->value.asOperator.pLeft = hb_compExprListStrip( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
#endif
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         hb_compExprPushOperEq( pSelf, HB_P_DIVIDE, HB_COMP_PARAM );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUseOperEq( pSelf, HB_P_DIVIDE, HB_COMP_PARAM );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseModEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
#if ! defined( HB_MACRO_SUPPORT )
         if( ! HB_SUPPORT_HARBOUR )
            pSelf->value.asOperator.pLeft = hb_compExprListStrip( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
#endif
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         hb_compExprPushOperEq( pSelf, HB_P_MODULUS, HB_COMP_PARAM );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUseOperEq( pSelf, HB_P_MODULUS, HB_COMP_PARAM );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseExpEq )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
#if ! defined( HB_MACRO_SUPPORT )
         if( ! HB_SUPPORT_HARBOUR )
            pSelf->value.asOperator.pLeft = hb_compExprListStrip( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
#endif
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         hb_compExprPushOperEq( pSelf, HB_P_POWER, HB_COMP_PARAM );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUseOperEq( pSelf, HB_P_POWER, HB_COMP_PARAM );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseOr )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceOr( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         if( HB_COMP_ISSUPPORTED( HB_COMPFLAG_SHORTCUTS ) )
         {
            HB_ISIZ nEndPos;

            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_DUPLICATE );
            nEndPos = HB_GEN_FUNC1( JumpTrue, 0 );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( JumpHere, nEndPos );
         }
         else
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_OR );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_COMP_ISSUPPORTED( HB_COMPFLAG_SHORTCUTS ) )
         {
            HB_ISIZ nEndPos;
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            nEndPos = HB_GEN_FUNC1( JumpTrue, 0 );
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
#if defined( HB_MACRO_SUPPORT )
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#else
            {
               HB_BOOL fMeaningful = HB_COMP_PARAM->fMeaningful;
               /* do not generate warning about meaningless expression usage */
               HB_COMP_PARAM->fMeaningful = HB_TRUE;
               HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
               HB_COMP_PARAM->fMeaningful = fMeaningful;
            }
#endif
            HB_GEN_FUNC1( JumpHere, nEndPos );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseAnd )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceAnd( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         if( HB_COMP_ISSUPPORTED( HB_COMPFLAG_SHORTCUTS ) )
         {
            HB_ISIZ nEndPos;

            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_DUPLICATE );
            nEndPos = HB_GEN_FUNC1( JumpFalse, 0 );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( JumpHere, nEndPos );
         }
         else
         {
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_AND );
         }
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_COMP_ISSUPPORTED( HB_COMPFLAG_SHORTCUTS ) )
         {
            HB_ISIZ nEndPos;
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            nEndPos = HB_GEN_FUNC1( JumpFalse, 0 );
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
#if defined( HB_MACRO_SUPPORT )
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
#else
            {
               HB_BOOL fMeaningful = HB_COMP_PARAM->fMeaningful;
               /* do not generate warning about meaningless expression usage */
               HB_COMP_PARAM->fMeaningful = HB_TRUE;
               HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
               HB_COMP_PARAM->fMeaningful = fMeaningful;
            }
#endif
            HB_GEN_FUNC1( JumpHere, nEndPos );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
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
         PHB_EXPR pExpr;

         pSelf->value.asOperator.pLeft = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pExpr = pSelf->value.asOperator.pLeft;

         if( pExpr->ExprType == HB_ET_LOGICAL )
         {
            pExpr->value.asLogical = ! pExpr->value.asLogical;
            HB_COMP_EXPR_CLEAR( pSelf );
            pSelf = pExpr;
         }
         else if( pExpr->ExprType == HB_EO_NOT && HB_SUPPORT_EXTOPT )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            pExpr->ExprType = HB_ET_NONE;  /* do not delete operator parameter - we are still using it */
            pExpr = pExpr->value.asOperator.pLeft;
            HB_COMP_EXPR_FREE( pSelf );
            pSelf = pExpr;
         }
         break;
      }
      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         hb_compErrorIndex( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_NOT );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         HB_COMP_EXPR_FREE( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

/* handler for = operator
 */
static HB_EXPR_FUNC( hb_compExprUseEqual )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceEQ( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_EQUAL );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
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
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceEQ( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_EXACTLYEQUAL );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseLT )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceLT( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_LESS );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseGT )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceGT( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;
      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_GREATER );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseLE )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceLE( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_LESSEQUAL );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseGE )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceGE( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_GREATEREQUAL );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseNE )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceNE( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_NOTEQUAL );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseIN )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceIN( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_INSTRING );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePlus )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReducePlus( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         if( HB_SUPPORT_EXTOPT )
         {
            PHB_EXPR pLeft, pRight;
            pLeft  = pSelf->value.asOperator.pLeft;
            pRight = pSelf->value.asOperator.pRight;
            if( pLeft->ExprType == HB_ET_NUMERIC )
            {
               if( pLeft->value.asNum.NumType == HB_ET_LONG ?
                   pLeft->value.asNum.val.l == 1 :
                   pLeft->value.asNum.val.d == 1 )
               {
                  HB_EXPR_USE( pRight, HB_EA_PUSH_PCODE );
                  HB_GEN_FUNC1( PCode1, HB_P_INC );
                  break;
               }
               else if( pLeft->value.asNum.NumType == HB_ET_LONG ?
                        pLeft->value.asNum.val.l == -1 :
                        pLeft->value.asNum.val.d == -1 )
               {
                  HB_EXPR_USE( pRight, HB_EA_PUSH_PCODE );
                  HB_GEN_FUNC1( PCode1, HB_P_DEC );
                  break;
               }
            }
            else if( pRight->ExprType == HB_ET_NUMERIC )
            {
               if( pRight->value.asNum.NumType == HB_ET_LONG ?
                   pRight->value.asNum.val.l == 1 :
                   pRight->value.asNum.val.d == 1 )
               {
                  HB_EXPR_USE( pLeft, HB_EA_PUSH_PCODE );
                  HB_GEN_FUNC1( PCode1, HB_P_INC );
                  break;
               }
               else if( pRight->value.asNum.NumType == HB_ET_LONG ?
                        pRight->value.asNum.val.l == -1 :
                        pRight->value.asNum.val.d == -1 )
               {
                  HB_EXPR_USE( pLeft, HB_EA_PUSH_PCODE );
                  HB_GEN_FUNC1( PCode1, HB_P_DEC );
                  break;
               }
            }
         }
         HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_PLUS );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMinus )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceMinus( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         if( HB_SUPPORT_EXTOPT )
         {
            PHB_EXPR pRight = pSelf->value.asOperator.pRight;
            if( pRight->ExprType == HB_ET_NUMERIC )
            {
               if( pRight->value.asNum.NumType == HB_ET_LONG ?
                   pRight->value.asNum.val.l == 1 :
                   pRight->value.asNum.val.d == 1 )
               {
                  HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
                  HB_GEN_FUNC1( PCode1, HB_P_DEC );
                  break;
               }
               else if( pRight->value.asNum.NumType == HB_ET_LONG ?
                        pRight->value.asNum.val.l == -1 :
                        pRight->value.asNum.val.d == -1 )
               {
                  HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
                  HB_GEN_FUNC1( PCode1, HB_P_INC );
                  break;
               }
            }
         }
         HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_MINUS );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMult )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceMult( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_MULT );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseDiv )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceDiv( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_DIVIDE );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseMod )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         pSelf = hb_compExprReduceMod( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_MODULUS );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePower )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft  = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf->value.asOperator.pRight = HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_REDUCE );
         if( HB_SUPPORT_HARBOUR )   /* Clipper doesn't optimize it */
            pSelf = hb_compExprReducePower( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_POWER );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         hb_compExprDelOperator( pSelf, HB_COMP_PARAM );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUseNegate )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
         pSelf = hb_compExprReduceNegate( pSelf, HB_COMP_PARAM );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         HB_EXPR_USE( pSelf->value.asOperator.pLeft,  HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, HB_P_NEGATE );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
         if( HB_SUPPORT_HARBOUR )
         {
            /* NOTE: This will not generate a runtime error if incompatible
             * data type is used
             */
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_POP );
         }
         else
         {
            HB_EXPR_USE( pSelf, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, HB_P_POP );
         }
         break;

      case HB_EA_STATEMENT:
         HB_COMP_ERROR_SYNTAX( pSelf );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            HB_COMP_EXPR_FREE( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePreInc )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
#if ! defined( HB_MACRO_SUPPORT )
         if( ! HB_SUPPORT_HARBOUR )
            pSelf->value.asOperator.pLeft = hb_compExprListStrip( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
#endif
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         hb_compExprPushPreOp( pSelf, HB_P_INC, HB_COMP_PARAM );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUsePreOp( pSelf, HB_P_INC, HB_COMP_PARAM );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            HB_COMP_EXPR_FREE( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

static HB_EXPR_FUNC( hb_compExprUsePreDec )
{
   switch( iMessage )
   {
      case HB_EA_REDUCE:
         pSelf->value.asOperator.pLeft = HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_REDUCE );
#if ! defined( HB_MACRO_SUPPORT )
         if( ! HB_SUPPORT_HARBOUR )
            pSelf->value.asOperator.pLeft = hb_compExprListStrip( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
#endif
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_LVALUE );
         break;

      case HB_EA_ARRAY_AT:
         HB_COMP_ERROR_TYPE( pSelf );
         break;

      case HB_EA_ARRAY_INDEX:
         break;

      case HB_EA_LVALUE:
         hb_compErrorLValue( HB_COMP_PARAM, pSelf );
         break;

      case HB_EA_PUSH_PCODE:
         hb_compExprPushPreOp( pSelf, HB_P_DEC, HB_COMP_PARAM );
         break;

      case HB_EA_POP_PCODE:
         break;

      case HB_EA_PUSH_POP:
      case HB_EA_STATEMENT:
         hb_compExprUsePreOp( pSelf, HB_P_DEC, HB_COMP_PARAM );
         break;

      case HB_EA_DELETE:
         if( pSelf->value.asOperator.pLeft )
            HB_COMP_EXPR_FREE( pSelf->value.asOperator.pLeft );
         break;
   }
   return pSelf;
}

/* ************************************************************************* */

/* This generates a push pcode for a codeblock (with no macro expression or
   with late evaluation of a macro)
 */
#if defined( HB_MACRO_SUPPORT )
static void hb_compExprCodeblockPush( PHB_EXPR pSelf, HB_COMP_DECL )
#else
static HB_BOOL hb_compExprCodeblockPush( PHB_EXPR pSelf, int iEarlyEvalPass, HB_COMP_DECL )
#endif
{
   PHB_EXPR pExpr, pNext;
   PHB_EXPR * pPrev;

   /* Define requested local variables
    */
#if defined( HB_MACRO_SUPPORT )
   hb_macroCodeBlockStart( HB_COMP_PARAM );
   HB_PCODE_DATA->pLocals = pSelf->value.asCodeblock.pLocals;
   HB_PCODE_DATA->fVParams =
                  ( pSelf->value.asCodeblock.flags & HB_BLOCK_VPARAMS ) != 0;
#else
   hb_compCodeBlockStart( HB_COMP_PARAM, iEarlyEvalPass );
   HB_COMP_PARAM->functions.pLast->fVParams =
                  ( pSelf->value.asCodeblock.flags & HB_BLOCK_VPARAMS ) != 0;

   {
      PHB_CBVAR pVar;

      HB_COMP_PARAM->iVarScope = HB_VSCOMP_PARAMETER;
      pVar = pSelf->value.asCodeblock.pLocals;
      while( pVar )
      {
         hb_compVariableAdd( HB_COMP_PARAM, pVar->szName, hb_compVarTypeNew( HB_COMP_PARAM, pVar->bType, NULL ) );
         pVar = pVar->pNext;
      }
   }

   hb_compLinePushIfDebugger( HB_COMP_PARAM );
#endif

   pExpr = pSelf->value.asCodeblock.pExprList;
   pPrev = &pSelf->value.asCodeblock.pExprList;
   while( pExpr )
   {
      if( pExpr->ExprType == HB_ET_MACRO &&
          pExpr->value.asMacro.SubType != HB_ET_MACRO_SYMBOL &&
          pExpr->value.asMacro.SubType != HB_ET_MACRO_REFER &&
          pExpr->value.asMacro.SubType != HB_ET_MACRO_ALIASED )
      {
         /* Clipper allows for list expressions in a codeblock
          * macro := "1,2"
          * EVAL( {|| &macro} )
          */
         pExpr->value.asMacro.SubType |= HB_ET_MACRO_PARE;
      }

      /* store next expression in case the current  will be reduced
       * NOTE: During reduction the expression can be replaced by the
       *    new one - this will break the linked list of expressions.
       */
      pNext = pExpr->pNext; /* store next expression in case the current will be reduced */
      if( ( pSelf->value.asCodeblock.flags & HB_BLOCK_REDUCE ) != 0 ||
          HB_SUPPORT_HARBOUR )
      {
         *pPrev = pExpr = HB_EXPR_USE( pExpr, HB_EA_REDUCE );
         pExpr->pNext = pNext;  /* restore the link to next expression */
      }

      /* Generate push/pop pcodes for all expresions except the last one
       * The value of the last expression is used as a return value
       * of a codeblock evaluation
       */
      /* NOTE: This will genereate warnings if constant value is
       * used as an expression - some operators will generate it too
       * e.g.
       * EVAL( {|| 3+5, func()} )
       */
#if defined( HB_MACRO_SUPPORT )
      if( pNext )
         HB_EXPR_USE( pExpr, HB_EA_PUSH_POP );
      else
         HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
#else
      if( pNext && ( iEarlyEvalPass == 0 || HB_SUPPORT_MACRODECL ) )
         HB_EXPR_USE( pExpr, HB_EA_PUSH_POP );
      else
         HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
#endif
      pPrev = &pExpr->pNext;
      pExpr = pNext;
   }
#if defined( HB_MACRO_SUPPORT )
   hb_macroCodeBlockEnd( HB_COMP_PARAM );
#else
   if( HB_COMP_PARAM->functions.pLast->iEarlyEvalPass == 0 )
   {
      hb_compCodeBlockEnd( HB_COMP_PARAM );
      return HB_TRUE;
   }
   else
   {
      hb_compCodeBlockRewind( HB_COMP_PARAM );
      return HB_FALSE;
   }
#endif
}

/* This generates a push pcode for early evaluation of a macro
 */
#if ! defined( HB_MACRO_SUPPORT )
static void hb_compExprCodeblockExtPush( PHB_EXPR pSelf, HB_COMP_DECL )
{
   hb_compGenPCodeN( ( HB_BYTE * ) pSelf->value.asCodeblock.string,
                     pSelf->nLength, HB_COMP_PARAM );
}

static void hb_compExprCodeblockEarly( PHB_EXPR pSelf, HB_COMP_DECL )
{
   PHB_EXPR pExpr;

   /* check first expression */
   pExpr = pSelf->value.asCodeblock.pExprList;
   if( pExpr->ExprType == HB_ET_MACRO && pExpr->value.asMacro.cMacroOp &&
       pExpr->pNext == NULL )
   {
      /* simple macro variable expansion: &variable
       * 'szMacro' is a variable name
       * {|| &variable} => &( '{||' + variable +'}' )
       */
      PHB_EXPR pVar, pNew;

      pVar = hb_compExprNewVar( pExpr->value.asMacro.szMacro, HB_COMP_PARAM );
      pNew = hb_compExprNewString( "{||", 3, HB_FALSE, HB_COMP_PARAM );
      pNew = hb_compExprSetOperand( hb_compExprNewPlus( pNew, HB_COMP_PARAM ), pVar, HB_COMP_PARAM );
      pNew = hb_compExprSetOperand( hb_compExprNewPlus( pNew, HB_COMP_PARAM ), hb_compExprNewString( "}", 1, HB_FALSE, HB_COMP_PARAM ), HB_COMP_PARAM );
      pNew = hb_compExprNewMacro( pNew, 0, NULL, HB_COMP_PARAM );
      HB_EXPR_USE( pNew, HB_EA_PUSH_PCODE );
      HB_COMP_EXPR_FREE( pNew );
   }
   else
   {
      /* everything else is macro compiled at runtime
       * {|| &variable+1} => &( '{|| &variable+1}' )
       */
      if( ! hb_compExprCodeblockPush( pSelf, 1, HB_COMP_PARAM ) )
      {
         HB_BOOL fMacroText = ( HB_COMP_PARAM->supported & HB_COMPFLAG_MACROTEXT ) != 0;
         HB_COMP_PARAM->supported |= HB_COMPFLAG_MACROTEXT;
         HB_COMP_PARAM->functions.pLast->iEarlyEvalPass = 2;
         pExpr = hb_compExprNewMacro( hb_compExprNewString( pSelf->value.asCodeblock.string, pSelf->nLength, HB_FALSE, HB_COMP_PARAM ), 0, NULL, HB_COMP_PARAM );
         HB_EXPR_USE( pExpr, HB_EA_PUSH_PCODE );
         HB_COMP_EXPR_FREE( pExpr );
         hb_compCodeBlockStop( HB_COMP_PARAM );
         if( ! fMacroText )
            HB_COMP_PARAM->supported &= ~HB_COMPFLAG_MACROTEXT;
      }
   }
}
#endif      /*HB_MACRO_SUPPORT*/


static void hb_compExprPushSendPop( PHB_EXPR pSelf, HB_COMP_DECL )
{
   if( pSelf->value.asMessage.pObject )
   {
      /* Push _message */
      if( pSelf->value.asMessage.szMessage )
      {
         HB_GEN_FUNC2( MessageData, pSelf->value.asMessage.szMessage, HB_TRUE );
      }
      else
      {
         if( pSelf->value.asMessage.pMessage->ExprType == HB_ET_MACRO )
            /* o:&macro := value
             * set ASSIGN flag in macro expression
             * it's cleared just after use
             */
            pSelf->value.asMessage.pMessage->value.asMacro.SubType |= HB_ET_MACRO_ASSIGN;

         HB_EXPR_USE( pSelf->value.asMessage.pMessage, HB_EA_PUSH_PCODE );
      }
      /* Push object */
      HB_EXPR_USE( pSelf->value.asMessage.pObject, HB_EA_PUSH_PCODE );
   }
   else /* WITH OBJECT */
   {
      /* Push _message and object */
      if( pSelf->value.asMessage.szMessage )
      {
         HB_GEN_FUNC2( MessageData, pSelf->value.asMessage.szMessage, HB_FALSE );
      }
      else
      {
         if( pSelf->value.asMessage.pMessage->ExprType == HB_ET_MACRO )
            /* o:&macro := value
             * set ASSIGN flag in macro expression
             * it's cleared just after use
             */
            pSelf->value.asMessage.pMessage->value.asMacro.SubType |= HB_ET_MACRO_ASSIGN;

         HB_EXPR_USE( pSelf->value.asMessage.pMessage, HB_EA_PUSH_PCODE );
         /* Push object using WITHOBJECTMESSAGE pcode */
         HB_GEN_FUNC2( Message, NULL, HB_FALSE );
      }
   }
}

static void hb_compExprPushSendPush( PHB_EXPR pSelf, HB_COMP_DECL )
{
   if( pSelf->value.asMessage.pObject )
   {
      /* Push message */
      if( pSelf->value.asMessage.szMessage )
      {
         HB_GEN_FUNC2( Message, pSelf->value.asMessage.szMessage, HB_TRUE );
      }
      else
      {
         HB_EXPR_USE( pSelf->value.asMessage.pMessage, HB_EA_PUSH_PCODE );
      }
      /* Push object */
      HB_EXPR_USE( pSelf->value.asMessage.pObject, HB_EA_PUSH_PCODE );
   }
   else /* WITH OBJECT */
   {
      if( pSelf->value.asMessage.szMessage )
      {
         /* Push message and object */
         HB_GEN_FUNC2( Message, pSelf->value.asMessage.szMessage, HB_FALSE );
      }
      else
      {
         /* Push message */
         HB_EXPR_USE( pSelf->value.asMessage.pMessage, HB_EA_PUSH_PCODE );
         /* Push object using WITHOBJECTMESSAGE pcode */
         HB_GEN_FUNC2( Message, NULL, HB_FALSE );
      }
   }
}

static void hb_compExprPushSendPopPush( PHB_EXPR pObj, PHB_EXPR pValue,
                                        HB_BOOL fPreOp, HB_BYTE bOper, HB_COMP_DECL )
{
   if( HB_SUPPORT_HARBOUR )
   {
      hb_compExprPushSendPop( pObj, HB_COMP_PARAM );
      /* duplicate object variable */
      HB_GEN_FUNC1( PCode1, HB_P_DUPLICATE );
      /* Push message */
      if( pObj->value.asMessage.szMessage )
      {
         /* HB_TRUE used intnetionally to not push object variable in WITH OBJECT */
         HB_GEN_FUNC2( Message, pObj->value.asMessage.szMessage, HB_TRUE );
      }
      else
      {
         HB_EXPR_USE( pObj->value.asMessage.pMessage, HB_EA_PUSH_PCODE );
      }
      HB_GEN_FUNC2( PCode2, HB_P_SWAP, 0 );
      HB_GEN_FUNC2( PCode2, HB_P_SENDSHORT, 0 );
      if( fPreOp )
      {
         /* push the result on the stack */
         HB_GEN_FUNC1( PCode1, HB_P_DUPLICATE );
         HB_GEN_FUNC2( PCode2, HB_P_SWAP, 2 );
      }
   }
   else
   {
      if( fPreOp )
      {
         /* push current value - it will be a result of whole expression */
         HB_EXPR_USE( pObj, HB_EA_PUSH_PCODE );
      }
      hb_compExprPushSendPop( pObj, HB_COMP_PARAM );
      hb_compExprPushSendPush( pObj, HB_COMP_PARAM );
      HB_GEN_FUNC2( PCode2, HB_P_SENDSHORT, 0 );
   }
   /* push increment value */
   if( pValue )
   {
      HB_EXPR_USE( pValue, HB_EA_PUSH_PCODE );
   }
   /* do operation */
   HB_GEN_FUNC1( PCode1, bOper );
   /* Now do the assignment - call pop message with one argument */
   HB_GEN_FUNC2( PCode2, HB_P_SENDSHORT, 1 );
   if( fPreOp )
   {
      /* pop the unneeded value left by assignment message from the stack */
      HB_GEN_FUNC1( PCode1, HB_P_POP );
   }
}

/* Generates pcodes for compound operators    += -= *= /= %= ^=
 *
 * pExpr is an expression created by hb_compExprNew<operator>Eq functions
 */
/* NOTE: COMPATIBILITY ISSUE:
 * The HB_SUPPORT_HARBOUR in code below determines
 * the way the chained send messages are handled.
 * For example, the following code:
 *
 * a:b( COUNT() ):c += 1
 *
 * will be handled as:
 *
 * a:b( COUNT() ):c := a:b( COUNT() ):c + 1
 *
 * in strict Clipper compatibility mode
 * (HB_SUPPORT_HARBOUR is not set: -kc compiler switch ) and
 *
 * temp := a:b( COUNT() ), temp:c += 1
 *
 * in non-strict mode (-kh).
 * In practice in Clipper it will call COUNT() function two times: the
 * first time before addition and the second one after addition - in Harbour,
 * COUNT() function will be called only once, before addition.
 * The Harbour (non-strict) method is:
 * 1) faster
 * 2) it guarantees that the same instance variable of the same object will
 *   be changed
 */

static void hb_compExprPushOperEq( PHB_EXPR pSelf, HB_BYTE bOpEq, HB_COMP_DECL )
{
   HB_BYTE bNewOp;

   if( HB_SUPPORT_HARBOUR )
   {
      switch( bOpEq )
      {
         case HB_P_PLUS:
            bNewOp = HB_P_PLUSEQ;
            break;
         case HB_P_MINUS:
            bNewOp = HB_P_MINUSEQ;
            break;
         case HB_P_MULT:
            bNewOp = HB_P_MULTEQ;
            break;
         case HB_P_DIVIDE:
            bNewOp = HB_P_DIVEQ;
            break;
         case HB_P_MODULUS:
            bNewOp = HB_P_MODEQ;
            break;
         case HB_P_POWER:
            bNewOp = HB_P_EXPEQ;
            break;
         default:
            bNewOp = bOpEq;
            break;
      }
   }
   else
      bNewOp = bOpEq;

   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
#ifdef HB_USE_OBJMSG_REF
      if( HB_SUPPORT_EXTOPT && bOpEq != bNewOp )
      {
         hb_compExprPushSendPop( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
         HB_GEN_FUNC1( PCode1, HB_P_PUSHOVARREF );
         /* push increment value */
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, bNewOp );
      }
      else
#endif
      {
         hb_compExprPushSendPopPush( pSelf->value.asOperator.pLeft,
                                     pSelf->value.asOperator.pRight,
                                     HB_FALSE, bOpEq, HB_COMP_PARAM );
      }
      return;
   }
   else if( bOpEq != bNewOp )
   {
      if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_MACRO )
      {
         HB_USHORT usType = pSelf->value.asOperator.pLeft->value.asMacro.SubType;
         if( usType == HB_ET_MACRO_VAR )
         {
            /* NOTE: direct type change */
            pSelf->value.asOperator.pLeft->value.asMacro.SubType = HB_ET_MACRO_REFER;
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, bNewOp );
            pSelf->value.asOperator.pLeft->value.asMacro.SubType = usType;
            return;
         }
      }
#ifdef HB_USE_ARRAYAT_REF
      /* NOTE: code for arrays is differ to correctly handle a[ i++ ]++ */
      else if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_ARRAYAT )
      {
         /* Note: change type to array reference */
         pSelf->value.asOperator.pLeft->value.asList.reference = HB_TRUE;
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         pSelf->value.asOperator.pLeft->value.asList.reference = HB_FALSE;

         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, bNewOp );
         return;
      }
#endif
      else if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_VARIABLE )
      {
#if defined( HB_MACRO_SUPPORT )
         {
#else
         int iVar, iScope;

         hb_compVariableFind( HB_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol.name, &iVar, &iScope );
         if( iScope != HB_VS_LOCAL_FIELD && iScope != HB_VS_GLOBAL_FIELD &&
             iScope != HB_VS_UNDECLARED )
         {
            if( iScope == HB_VS_LOCAL_VAR &&
                pSelf->value.asOperator.pRight->ExprType == HB_ET_NUMERIC &&
                ( bOpEq == HB_P_PLUS || bOpEq == HB_P_MINUS ) )
            {
               if( hb_compExprIsInteger( pSelf->value.asOperator.pRight ) )
               {
                  short iIncrement = ( short ) pSelf->value.asOperator.pRight->value.asNum.val.l;

                  if( bOpEq != HB_P_MINUS || iIncrement >= -INT16_MAX )
                  {
                     HB_BYTE buffer[ 5 ];

                     if( bOpEq == HB_P_MINUS )
                        iIncrement = -iIncrement;

                     buffer[ 0 ] = HB_P_LOCALADDINT;
                     buffer[ 1 ] = HB_LOBYTE( iVar );
                     buffer[ 2 ] = HB_HIBYTE( iVar );
                     buffer[ 3 ] = HB_LOBYTE( iIncrement );
                     buffer[ 4 ] = HB_HIBYTE( iIncrement );
                     HB_GEN_FUNC2( PCodeN, buffer, 5 );

                     HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
                     return;
                  }
               }
            }
#endif
            /* NOTE: direct type change */
            pSelf->value.asOperator.pLeft->ExprType = HB_ET_VARREF;

            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, bNewOp );
            return;
         }
      }
   }
   /* push old value */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
   /* push increment value */
   HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
   /* perform operation and duplicate the new value */
   HB_GEN_FUNC1( PCode1, bOpEq );
   HB_GEN_FUNC1( PCode1, HB_P_DUPLICATE );
   /* pop the new value into variable and leave the copy on the stack */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
}

/* Generates pcodes for <operator>= syntax
 * used standalone as a statement (it cannot leave the value on the stack)
 */
static void hb_compExprUseOperEq( PHB_EXPR pSelf, HB_BYTE bOpEq, HB_COMP_DECL )
{
   HB_BYTE bNewOp;

   if( HB_SUPPORT_HARBOUR )
   {
      switch( bOpEq )
      {
         case HB_P_PLUS:
            bNewOp = HB_P_PLUSEQPOP;
            break;
         case HB_P_MINUS:
            bNewOp = HB_P_MINUSEQPOP;
            break;
         case HB_P_MULT:
            bNewOp = HB_P_MULTEQPOP;
            break;
         case HB_P_DIVIDE:
            bNewOp = HB_P_DIVEQPOP;
            break;
         case HB_P_MODULUS:
            bNewOp = HB_P_MODEQPOP;
            break;
         case HB_P_POWER:
            bNewOp = HB_P_EXPEQPOP;
            break;
         default:
            bNewOp = bOpEq;
            break;
      }
   }
   else
      bNewOp = bOpEq;

   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
#ifdef HB_USE_OBJMSG_REF
      if( HB_SUPPORT_EXTOPT && bOpEq != bNewOp )
      {
         hb_compExprPushSendPop( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
         HB_GEN_FUNC1( PCode1, HB_P_PUSHOVARREF );
         /* push increment value */
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, bNewOp );
      }
      else
#endif
      {
         hb_compExprPushSendPopPush( pSelf->value.asOperator.pLeft,
                                     pSelf->value.asOperator.pRight,
                                     HB_FALSE, bOpEq, HB_COMP_PARAM );
         /* pop the unneeded value from the stack */
         HB_GEN_FUNC1( PCode1, HB_P_POP );
      }
      return;
   }
   else if( bOpEq != bNewOp )
   {
      if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_MACRO )
      {
         HB_USHORT usType = pSelf->value.asOperator.pLeft->value.asMacro.SubType;
         if( usType == HB_ET_MACRO_VAR )
         {
            /* NOTE: direct type change */
            pSelf->value.asOperator.pLeft->value.asMacro.SubType = HB_ET_MACRO_REFER;
            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, bNewOp );
            pSelf->value.asOperator.pLeft->value.asMacro.SubType = usType;
            return;
         }
      }
#ifdef HB_USE_ARRAYAT_REF
      /* NOTE: code for arrays is differ to correctly handle a[ i++ ]++ */
      else if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_ARRAYAT )
      {
         /* Note: change type to array reference */
         pSelf->value.asOperator.pLeft->value.asList.reference = HB_TRUE;
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         pSelf->value.asOperator.pLeft->value.asList.reference = HB_FALSE;

         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         HB_GEN_FUNC1( PCode1, bNewOp );
         return;
      }
#endif
      else if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_VARIABLE )
      {
         HB_EXPRTYPE iOldType;
#if defined( HB_MACRO_SUPPORT )
         {
#else
         int iVar, iScope;

         hb_compVariableFind( HB_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol.name, &iVar, &iScope );
         if( iScope != HB_VS_LOCAL_FIELD && iScope != HB_VS_GLOBAL_FIELD &&
             iScope != HB_VS_UNDECLARED )
         {
            if( iScope == HB_VS_LOCAL_VAR &&
                pSelf->value.asOperator.pRight->ExprType == HB_ET_NUMERIC &&
                ( bOpEq == HB_P_PLUS || bOpEq == HB_P_MINUS ) )
            {
               if( hb_compExprIsInteger( pSelf->value.asOperator.pRight ) )
               {
                  short iIncrement = ( short ) pSelf->value.asOperator.pRight->value.asNum.val.l;

                  if( bOpEq != HB_P_MINUS || iIncrement >= -INT16_MAX )
                  {
                     HB_BYTE buffer[ 5 ];

                     if( bOpEq == HB_P_MINUS )
                        iIncrement = -iIncrement;

                     buffer[ 0 ] = HB_P_LOCALADDINT;
                     buffer[ 1 ] = HB_LOBYTE( iVar );
                     buffer[ 2 ] = HB_HIBYTE( iVar );
                     buffer[ 3 ] = HB_LOBYTE( iIncrement );
                     buffer[ 4 ] = HB_HIBYTE( iIncrement );
                     HB_GEN_FUNC2( PCodeN, buffer, 5 );
                     return;
                  }
               }
            }
#endif
            /* NOTE: direct type change */
            iOldType = pSelf->value.asOperator.pLeft->ExprType;
            pSelf->value.asOperator.pLeft->ExprType = HB_ET_VARREF;

            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            HB_GEN_FUNC1( PCode1, bNewOp );
            /* restore original expression type */
            pSelf->value.asOperator.pLeft->ExprType = iOldType;
            return;
         }
      }
   }
   /* push old value */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
   /* push increment value */
   HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
   /* add */
   HB_GEN_FUNC1( PCode1, bOpEq );
   /* pop the new value into variable and remove it from the stack */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
}

/* Generates the pcodes for pre- increment/decrement expressions
 */
static void hb_compExprPushPreOp( PHB_EXPR pSelf, HB_BYTE bOper, HB_COMP_DECL )
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
#ifdef HB_USE_OBJMSG_REF
      if( HB_SUPPORT_EXTOPT )
      {
         hb_compExprPushSendPop( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
         HB_GEN_FUNC1( PCode1, HB_P_PUSHOVARREF );
         /* increase/decrease operation, leave unreferenced value on stack */
         HB_GEN_FUNC1( PCode1, ( HB_BYTE ) ( ( bOper == HB_P_INC ) ? HB_P_INCEQ : HB_P_DECEQ ) );
      }
      else
#endif
      {
         hb_compExprPushSendPopPush( pSelf->value.asOperator.pLeft, NULL,
                                     HB_FALSE, bOper, HB_COMP_PARAM );
      }
      return;
   }
   else if( HB_SUPPORT_HARBOUR )
   {
      if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_MACRO &&
          pSelf->value.asOperator.pLeft->value.asMacro.SubType == HB_ET_MACRO_VAR )
      {
         HB_USHORT usType = pSelf->value.asOperator.pLeft->value.asMacro.SubType;
         /* NOTE: direct type change */
         pSelf->value.asOperator.pLeft->value.asMacro.SubType = HB_ET_MACRO_REFER;
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         pSelf->value.asOperator.pLeft->value.asMacro.SubType = usType;

         /* increase/decrease operation, leave unreferenced value on stack */
         HB_GEN_FUNC1( PCode1, ( HB_BYTE ) ( bOper == HB_P_INC ? HB_P_INCEQ : HB_P_DECEQ ) );
         return;
      }
#ifdef HB_USE_ARRAYAT_REF
      /* NOTE: code for arrays is differ to correctly handle a[ i++ ]++ */
      else if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_ARRAYAT )
      {
         /* push reference to current value */
         /* Note: change type to array reference */
         pSelf->value.asOperator.pLeft->value.asList.reference = HB_TRUE;
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         pSelf->value.asOperator.pLeft->value.asList.reference = HB_FALSE;

         /* increase/decrease operation, leave unreferenced value on stack */
         HB_GEN_FUNC1( PCode1, ( HB_BYTE ) ( bOper == HB_P_INC ? HB_P_INCEQ : HB_P_DECEQ ) );
         return;
      }
#endif
#if ! defined( HB_MACRO_SUPPORT )
      else if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_VARIABLE )
      {
         int iVar, iScope;

         hb_compVariableFind( HB_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol.name, &iVar, &iScope );
         if( iScope != HB_VS_LOCAL_FIELD && iScope != HB_VS_GLOBAL_FIELD &&
             iScope != HB_VS_UNDECLARED )
         {
            if( iScope == HB_VS_LOCAL_VAR )
            {
               if( bOper == HB_P_INC )
               {
                  HB_GEN_FUNC3( PCode3, HB_P_LOCALINCPUSH, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ) );
               }
               else
               {
                  HB_GEN_FUNC3( PCode3, HB_P_LOCALDEC, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ) );
                  /* Push current value */
                  HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
               }
            }
            else
            {
               /* NOTE: direct type change */
               HB_EXPRTYPE iOldType = pSelf->value.asOperator.pLeft->ExprType;
               pSelf->value.asOperator.pLeft->ExprType = HB_ET_VARREF;
               HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
               HB_GEN_FUNC1( PCode1, ( HB_BYTE ) ( bOper == HB_P_INC ? HB_P_INCEQ : HB_P_DECEQ ) );
               /* restore original expression type */
               pSelf->value.asOperator.pLeft->ExprType = iOldType;
            }
            return;
         }
      }
#endif
   }

   /* Push current value */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
   /* Increment */
   HB_GEN_FUNC1( PCode1, bOper );
   /* duplicate a value */
   HB_GEN_FUNC1( PCode1, HB_P_DUPLICATE );
   /* pop new value and leave the duplicated copy of it on the stack */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
}

/* Generates the pcodes for post- increment/decrement expressions
 */
static void hb_compExprPushPostOp( PHB_EXPR pSelf, HB_BYTE bOper, HB_COMP_DECL )
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
#ifdef HB_USE_OBJMSG_REF
      if( HB_SUPPORT_EXTOPT )
      {
         /* push reference to current value */
         hb_compExprPushSendPop( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
         HB_GEN_FUNC1( PCode1, HB_P_PUSHOVARREF );
         /* Duplicate the reference and unref the original one -
          * it will be the result of whole expression
          */
         HB_GEN_FUNC1( PCode1, HB_P_DUPLUNREF );
         /* increment/decrement the value */
         HB_GEN_FUNC1( PCode1, ( HB_BYTE ) ( ( bOper == HB_P_INC ) ? HB_P_INCEQPOP : HB_P_DECEQPOP ) );
      }
      else
#endif
      {
         hb_compExprPushSendPopPush( pSelf->value.asOperator.pLeft, NULL,
                                     HB_TRUE, bOper, HB_COMP_PARAM );
      }
      return;
   }
   else if( HB_SUPPORT_HARBOUR )
   {
      if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_MACRO &&
          pSelf->value.asOperator.pLeft->value.asMacro.SubType == HB_ET_MACRO_VAR )
      {
         HB_USHORT usType = pSelf->value.asOperator.pLeft->value.asMacro.SubType;
         /* NOTE: direct type change */
         pSelf->value.asOperator.pLeft->value.asMacro.SubType = HB_ET_MACRO_REFER;
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         pSelf->value.asOperator.pLeft->value.asMacro.SubType = usType;

         /* Duplicate the reference and unref the original one -
          * it will be the result of whole expression
          */
         HB_GEN_FUNC1( PCode1, HB_P_DUPLUNREF );
         /* increase/decrease operation */
         HB_GEN_FUNC1( PCode1, ( HB_BYTE ) ( bOper == HB_P_INC ? HB_P_INCEQPOP : HB_P_DECEQPOP ) );
         return;
      }
#ifdef HB_USE_ARRAYAT_REF
      /* NOTE: code for arrays is differ to correctly handle a[ i++ ]++ */
      else if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_ARRAYAT )
      {
         /* push reference to current value */
         /* Note: change type to array reference */
         pSelf->value.asOperator.pLeft->value.asList.reference = HB_TRUE;
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         pSelf->value.asOperator.pLeft->value.asList.reference = HB_FALSE;

         /* Duplicate the reference and unref the original one -
          * it will be the result of whole expression
          */
         HB_GEN_FUNC1( PCode1, HB_P_DUPLUNREF );
         /* increase/decrease operation */
         HB_GEN_FUNC1( PCode1, ( HB_BYTE ) ( bOper == HB_P_INC ? HB_P_INCEQPOP : HB_P_DECEQPOP ) );
         return;
      }
#endif
#if ! defined( HB_MACRO_SUPPORT )
      else if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_VARIABLE )
      {
         int iVar, iScope;

         hb_compVariableFind( HB_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol.name, &iVar, &iScope );
         if( iScope != HB_VS_LOCAL_FIELD && iScope != HB_VS_GLOBAL_FIELD &&
             iScope != HB_VS_UNDECLARED )
         {
            if( iScope == HB_VS_LOCAL_VAR )
            {
               /* Push current value */
               HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
               HB_GEN_FUNC3( PCode3, ( HB_BYTE ) ( bOper == HB_P_INC ? HB_P_LOCALINC : HB_P_LOCALDEC ),
                             HB_LOBYTE( iVar ), HB_HIBYTE( iVar ) );
            }
            else
            {
               /* NOTE: direct type change */
               HB_EXPRTYPE iOldType = pSelf->value.asOperator.pLeft->ExprType;
               pSelf->value.asOperator.pLeft->ExprType = HB_ET_VARREF;
               HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
               HB_GEN_FUNC1( PCode1, HB_P_DUPLUNREF );
               HB_GEN_FUNC1( PCode1, ( HB_BYTE ) ( bOper == HB_P_INC ? HB_P_INCEQPOP : HB_P_DECEQPOP ) );
               /* restore original expression type */
               pSelf->value.asOperator.pLeft->ExprType = iOldType;
            }
            return;
         }
      }
#endif
   }

   /* Push current value */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
   /* Duplicate value */
   HB_GEN_FUNC1( PCode1, HB_P_DUPLICATE );
   /* Increment */
   HB_GEN_FUNC1( PCode1, bOper );
   /* pop new value from the stack */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
}

/* Generates the pcodes for increment/decrement operations
 * used standalone as a statement
 */
static void hb_compExprUsePreOp( PHB_EXPR pSelf, HB_BYTE bOper, HB_COMP_DECL )
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
#ifdef HB_USE_OBJMSG_REF
      if( HB_SUPPORT_EXTOPT )
      {
         /* push reference to current value */
         hb_compExprPushSendPop( pSelf->value.asOperator.pLeft, HB_COMP_PARAM );
         HB_GEN_FUNC1( PCode1, HB_P_PUSHOVARREF );
         /* increment/decrement the value */
         HB_GEN_FUNC1( PCode1, ( HB_BYTE ) ( ( bOper == HB_P_INC ) ? HB_P_INCEQPOP : HB_P_DECEQPOP ) );
      }
      else
#endif
      {
         hb_compExprPushSendPopPush( pSelf->value.asOperator.pLeft, NULL,
                                     HB_FALSE, bOper, HB_COMP_PARAM );
         /* pop the value from the stack */
         HB_GEN_FUNC1( PCode1, HB_P_POP );
      }
      return;
   }
   else if( HB_SUPPORT_HARBOUR )
   {
      if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_MACRO &&
          pSelf->value.asOperator.pLeft->value.asMacro.SubType == HB_ET_MACRO_VAR )
      {
         HB_USHORT usType = pSelf->value.asOperator.pLeft->value.asMacro.SubType;
         /* NOTE: direct type change */
         pSelf->value.asOperator.pLeft->value.asMacro.SubType = HB_ET_MACRO_REFER;
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         pSelf->value.asOperator.pLeft->value.asMacro.SubType = usType;

         /* increase/decrease operation */
         HB_GEN_FUNC1( PCode1, ( HB_BYTE ) ( bOper == HB_P_INC ? HB_P_INCEQPOP : HB_P_DECEQPOP ) );
         return;
      }
#ifdef HB_USE_ARRAYAT_REF
      /* NOTE: code for arrays is differ to correctly handle a[ i++ ]++ */
      else if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_ARRAYAT )
      {
         /* push reference to current value */
         /* Note: change type to array reference */
         pSelf->value.asOperator.pLeft->value.asList.reference = HB_TRUE;
         HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
         pSelf->value.asOperator.pLeft->value.asList.reference = HB_FALSE;
         /* increase/decrease operation */
         HB_GEN_FUNC1( PCode1, ( HB_BYTE ) ( bOper == HB_P_INC ? HB_P_INCEQPOP : HB_P_DECEQPOP ) );
         return;
      }
#endif
#if ! defined( HB_MACRO_SUPPORT )
      else if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_VARIABLE )
      {
         int iVar, iScope;

         hb_compVariableFind( HB_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol.name, &iVar, &iScope );
         if( iScope != HB_VS_LOCAL_FIELD && iScope != HB_VS_GLOBAL_FIELD &&
             iScope != HB_VS_UNDECLARED )
         {
            if( iScope == HB_VS_LOCAL_VAR )
            {
               HB_GEN_FUNC3( PCode3, ( HB_BYTE ) ( bOper == HB_P_INC ? HB_P_LOCALINC : HB_P_LOCALDEC ),
                             HB_LOBYTE( iVar ), HB_HIBYTE( iVar ) );
            }
            else
            {
               /* NOTE: direct type change */
               HB_EXPRTYPE iOldType = pSelf->value.asOperator.pLeft->ExprType;
               pSelf->value.asOperator.pLeft->ExprType = HB_ET_VARREF;
               HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
               HB_GEN_FUNC1( PCode1, ( HB_BYTE ) ( bOper == HB_P_INC ? HB_P_INCEQPOP : HB_P_DECEQPOP ) );
               /* restore original expression type */
               pSelf->value.asOperator.pLeft->ExprType = iOldType;
            }
            return;
         }
      }
#endif
   }

   /* Push current value */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
   /* Increment */
   HB_GEN_FUNC1( PCode1, bOper );
   /* pop new value from the stack */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
}

/* Generate pcode for aliased expression which contains macro operator on
 * the left or right side of the alias operator
 * expression->&macro or &macro->expression or &macro->&macro
 */
static void hb_compExprUseAliasMacro( PHB_EXPR pAliasedVar, HB_BYTE bAction, HB_COMP_DECL )
{
   PHB_EXPR pAlias, pVar;

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
      HB_GEN_FUNC2( PushString, pAlias->value.asSymbol.name, strlen( pAlias->value.asSymbol.name ) + 1 );
      HB_EXPR_USE( pVar, HB_EA_PUSH_PCODE );
      if( bAction == HB_EA_PUSH_PCODE )
         HB_GEN_FUNC1( PCode1, HB_P_MACROPUSHALIASED );
      else
         HB_GEN_FUNC1( PCode1, HB_P_MACROPOPALIASED );
   }
   else if( pVar->ExprType == HB_ET_VARIABLE )
   {
      /* NOTE:
       *    &macro->var is the  same as: &( macro + "->var" )
       */
      HB_EXPR_USE( pAlias, HB_EA_PUSH_PCODE );
      HB_GEN_FUNC2( PushString, pVar->value.asSymbol.name, strlen( pVar->value.asSymbol.name ) + 1 );
      if( bAction == HB_EA_PUSH_PCODE )
         HB_GEN_FUNC1( PCode1, HB_P_MACROPUSHALIASED );
      else
         HB_GEN_FUNC1( PCode1, HB_P_MACROPOPALIASED );
   }
   else
   {
      HB_EXPR_USE( pAlias, HB_EA_PUSH_PCODE );
      HB_EXPR_USE( pVar, HB_EA_PUSH_PCODE );
      if( bAction == HB_EA_PUSH_PCODE )
         HB_GEN_FUNC1( PCode1, HB_P_MACROPUSHALIASED );
      else
         HB_GEN_FUNC1( PCode1, HB_P_MACROPOPALIASED );
   }

   /* Always add byte to pcode indicating requested macro compiler flag. */
   HB_GEN_FUNC1( PCode1, ( HB_BYTE ) HB_MACRO_GENFLAGS );
}


/* Reduces the list of expressions
 *
 * pExpr is the first expression on the list
 */
static PHB_EXPR hb_compExprReduceList( PHB_EXPR pList, HB_COMP_DECL )
{
   PHB_EXPR * pExpr;

   /* NOTE: During optimalization an expression on the list can be
    * replaced by the new one
    */

   pExpr = &pList->value.asList.pExprList;
   while( *pExpr )
   {
      PHB_EXPR pNext = ( *pExpr )->pNext; /* store next expression in case the current will be reduced */
      *pExpr = HB_EXPR_USE( *pExpr, HB_EA_REDUCE );
      ( *pExpr )->pNext = pNext;             /* restore the link to next expression */
      pExpr = &( *pExpr )->pNext;
   }
   return pList;
}

/* reduce ( "alias" )-> to ALIAS->
 */
static PHB_EXPR hb_compExprReduceAliasString( PHB_EXPR pExpr, PHB_EXPR pAlias, HB_COMP_DECL )
{
   const char * szAlias = pAlias->value.asString.string;

   if( HB_ISFIRSTIDCHAR( *szAlias ) )
   {
      HB_SIZE nLen = pAlias->nLength;
      if( nLen <= HB_SYMBOL_NAME_LEN )
      {
         HB_BOOL fLower = HB_FALSE;
         while( nLen )
         {
            char c = szAlias[ nLen - 1 ];
            if( ! HB_ISNEXTIDCHAR( c ) )
               break;
            if( HB_ISLOWER( c ) )
               fLower = HB_TRUE;
            --nLen;
         }
         if( nLen == 0 )
         {
#if defined( HB_MACRO_SUPPORT )
            if( fLower )
               szAlias = hb_macroIdentNew( HB_COMP_PARAM, hb_strupr( hb_strdup( szAlias ) ) );
            else if( pAlias->value.asString.dealloc )
               szAlias = hb_macroIdentNew( HB_COMP_PARAM, hb_strdup( szAlias ) );
#else
            if( fLower )
               szAlias = hb_compIdentifierNew( HB_COMP_PARAM, hb_strupr( hb_strdup( szAlias ) ), HB_IDENT_FREE );
            else if( pAlias->value.asString.dealloc )
               szAlias = hb_compIdentifierNew( HB_COMP_PARAM, szAlias, HB_IDENT_COPY );
#endif
            HB_COMP_EXPR_FREE( pExpr );
            pExpr = hb_compExprNewAlias( szAlias, HB_COMP_PARAM );
         }
      }
   }
   return pExpr;
}

static HB_BOOL hb_compExprIsMemvarAlias( const char * szAlias )
{
   int iLen = ( int ) strlen( szAlias );

   /* @M-> @MEMVAR-> or @MEMVA-> or @MEMV-> */
   return ( iLen == 1 || ( iLen >= 4 && iLen <= 6 ) ) &&
          memcmp( szAlias, "MEMVAR", iLen ) == 0;
}
