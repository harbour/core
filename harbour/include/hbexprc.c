/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Expression Optimizer - utilities
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
#include "hbmacro.ch"

#ifdef __WATCOMC__
/* disable warnings for 'no reference to symbol' */
#pragma warning 14 9
#endif

/* ************************************************************************* */

static void hb_compExprSendPopPush( HB_EXPR_PTR pObj, HB_COMP_DECL )
{
   if( pObj->value.asMessage.pObject )
   {
      /* Push _message for later use */
      if( pObj->value.asMessage.szMessage )
      {
         HB_EXPR_PCODE2( hb_compGenMessageData, pObj->value.asMessage.szMessage, TRUE );
      }
      else
      {
         HB_EXPR_USE( pObj->value.asMessage.pMessage, HB_EA_PUSH_PCODE );
      }
      /* Push object */
      HB_EXPR_USE( pObj->value.asMessage.pObject, HB_EA_PUSH_PCODE );
#ifndef HB_USE_OBJMSG_REF
      /* Now push current value of variable */
      if( pObj->value.asMessage.szMessage )
      {
         HB_EXPR_PCODE2( hb_compGenMessage, pObj->value.asMessage.szMessage, TRUE );
      }
      else
      {
         HB_EXPR_USE( pObj->value.asMessage.pMessage, HB_EA_PUSH_PCODE );
      }
      /* Push object */
      HB_EXPR_USE( pObj->value.asMessage.pObject, HB_EA_PUSH_PCODE );
#endif
   }
   else
   {
      /* Push _message for later use */
      if( pObj->value.asMessage.szMessage )
      {
         HB_EXPR_PCODE2( hb_compGenMessageData, pObj->value.asMessage.szMessage, FALSE );
      }
      else
      {
         HB_EXPR_USE( pObj->value.asMessage.pMessage, HB_EA_PUSH_PCODE );
         /* Push WITHOBJECTMESSAGE pcode */
         HB_EXPR_PCODE2( hb_compGenMessage, NULL, FALSE );
      }
#ifndef HB_USE_OBJMSG_REF
      /* Now push current value of variable */
      if( pObj->value.asMessage.szMessage )
      {
         HB_EXPR_PCODE2( hb_compGenMessage, pObj->value.asMessage.szMessage, FALSE );
      }
      else
      {
         HB_EXPR_USE( pObj->value.asMessage.pMessage, HB_EA_PUSH_PCODE );
         /* Push WITHOBJECTMESSAGE pcode */
         HB_EXPR_PCODE2( hb_compGenMessage, NULL, FALSE );
      }
#endif
   }
}

void hb_compExprDelOperator( HB_EXPR_PTR pExpr, HB_COMP_DECL )
{
   if( pExpr->value.asOperator.pLeft )
      HB_EXPR_PCODE1( hb_compExprDelete, pExpr->value.asOperator.pLeft );
   if( pExpr->value.asOperator.pRight )
      HB_EXPR_PCODE1( hb_compExprDelete, pExpr->value.asOperator.pRight );
}


/* Generates pcodes for compound operators    += -= *= /= %= ^=
 *
 * pExpr is an expression created by hb_compExprNew<operator>Eq functions
 */
void hb_compExprPushOperEq( HB_EXPR_PTR pSelf, BYTE bOpEq, HB_COMP_DECL )
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {

/* NOTE: COMPATIBILITY ISSUE:
 *  The above HB_C52_STRICT setting determines
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

      HB_EXPR_PCODE1( hb_compExprSendPopPush, pSelf->value.asOperator.pLeft );
      /* Do it. */

      /* Temporary disabled optimization with references to object variables
         untill we will not have extended reference items in our HVM [druzus] */
#ifdef HB_USE_OBJMSG_REF
      if( bOpEq == HB_P_PLUS || bOpEq == HB_P_MINUS ||
          bOpEq == HB_P_MULT || bOpEq == HB_P_DIVIDE )
      {
         HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_PUSHOVARREF );

         /* push increment value */
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         /* increase operation */
         switch( bOpEq )
         {
            case HB_P_PLUS:
               bOpEq = HB_P_PLUSEQ;
               break;
            case HB_P_MINUS:
               bOpEq = HB_P_MINUSEQ;
               break;
            case HB_P_MULT:
               bOpEq = HB_P_MULTEQ;
               break;
            case HB_P_DIVIDE:
               bOpEq = HB_P_DIVEQ;
               break;
         }
         HB_EXPR_PCODE1( hb_compGenPCode1, bOpEq );
      }
      else
#endif
      {
         HB_EXPR_PCODE2( hb_compGenPCode2, HB_P_SENDSHORT, 0 );
         /* push increment value */
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         /* increase operation */
         HB_EXPR_PCODE1( hb_compGenPCode1, bOpEq );
         /* call pop message with one argument */
         HB_EXPR_PCODE2( hb_compGenPCode2, HB_P_SENDSHORT, 1 );
      }
      return;
   }
   /* TODO: add a special code for arrays to correctly handle a[ i++ ]++
    */
#if ! defined( HB_MACRO_SUPPORT )
   else if( ( bOpEq == HB_P_PLUS || bOpEq == HB_P_MINUS ||
              bOpEq == HB_P_MULT || bOpEq == HB_P_DIVIDE ) &&
            ( pSelf->value.asOperator.pLeft->ExprType == HB_ET_VARIABLE ) )
   {
      int iScope = hb_compVariableScope( HB_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol );

      if( iScope != HB_VS_LOCAL_FIELD && iScope != HB_VS_GLOBAL_FIELD &&
          iScope != HB_VS_UNDECLARED )
      {
         HB_EXPRTYPE iType = pSelf->value.asOperator.pRight->ExprType;
         
         if( iType == HB_ET_NUMERIC || iType == HB_ET_STRING )
         {
            if( iScope == HB_VS_LOCAL_VAR && iType == HB_ET_NUMERIC && 
                ( bOpEq == HB_P_PLUS || bOpEq == HB_P_MINUS ) )
            {
               int iLocal = hb_compLocalGetPos( HB_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol ); 

               if( iLocal < 256 && hb_compExprIsInteger( pSelf->value.asOperator.pRight ) )
               {
                  short iIncrement = ( short ) pSelf->value.asOperator.pRight->value.asNum.val.l;

                  if( bOpEq != HB_P_MINUS || iIncrement >= -INT16_MAX )
                  {
                     if( bOpEq == HB_P_MINUS )
                     {
                        iIncrement = -iIncrement;
                     }

                     hb_compGenPCode4( HB_P_LOCALNEARADDINT, ( BYTE ) iLocal, HB_LOBYTE( iIncrement ), HB_HIBYTE( iIncrement ), HB_COMP_PARAM );

                     HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );

                     return;
                  }
               }
            }
            /* NOTE: direct type change */
            pSelf->value.asOperator.pLeft->ExprType = HB_ET_VARREF;

            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            switch( bOpEq )
            {
               case HB_P_PLUS:
                  bOpEq = HB_P_PLUSEQ;
                  break;
               case HB_P_MINUS:
                  bOpEq = HB_P_MINUSEQ;
                  break;
               case HB_P_MULT:
                  bOpEq = HB_P_MULTEQ;
                  break;
               case HB_P_DIVIDE:
                  bOpEq = HB_P_DIVEQ;
                  break;
            }
            HB_EXPR_PCODE1( hb_compGenPCode1, bOpEq );
            return;
         }
         else if( iType == HB_ET_VARIABLE )
         {
            int iScope = hb_compVariableScope( HB_COMP_PARAM, pSelf->value.asOperator.pRight->value.asSymbol );

            if( iScope != HB_VS_LOCAL_FIELD && iScope != HB_VS_GLOBAL_FIELD &&
                iScope != HB_VS_UNDECLARED )
            {
               /* NOTE: direct type change */
               pSelf->value.asOperator.pLeft->ExprType = HB_ET_VARREF;
               pSelf->value.asOperator.pRight->ExprType = HB_ET_VARREF;

               HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
               HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
               switch( bOpEq )
               {
                  case HB_P_PLUS:
                     bOpEq = HB_P_PLUSEQ;
                     break;
                  case HB_P_MINUS:
                     bOpEq = HB_P_MINUSEQ;
                     break;
                  case HB_P_MULT:
                     bOpEq = HB_P_MULTEQ;
                     break;
                  case HB_P_DIVIDE:
                     bOpEq = HB_P_DIVEQ;
                     break;
               }
               HB_EXPR_PCODE1( hb_compGenPCode1, bOpEq );
               return;
            }
         }
      }
   }
#endif
   /* push old value */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
   /* push increment value */
   HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
   /* perform operation and duplicate the new value */
   HB_EXPR_PCODE1( hb_compGenPCode1, bOpEq );
   HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_DUPLICATE );
   /* pop the new value into variable and leave the copy on the stack */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
}

/* Generates pcodes for <operator>= syntax
 * used standalone as a statement (it cannot leave the value on the stack)
 */
void hb_compExprUseOperEq( HB_EXPR_PTR pSelf, BYTE bOpEq, HB_COMP_DECL )
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
      HB_EXPR_PCODE1( hb_compExprSendPopPush, pSelf->value.asOperator.pLeft );
      /* Do it.*/

      /* Temporary disabled optimization with references to object variables
         untill we will not have extended reference items in our HVM [druzus] */
#ifdef HB_USE_OBJMSG_REF
      if( bOpEq == HB_P_PLUS || bOpEq == HB_P_MINUS ||
          bOpEq == HB_P_MULT || bOpEq == HB_P_DIVIDE )
      {
         HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_PUSHOVARREF );

         /* push increment value */
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         /* increase operation and pop the unneeded value from the stack */
         switch( bOpEq )
         {
            case HB_P_PLUS:
               bOpEq = HB_P_PLUSEQPOP;
               break;
            case HB_P_MINUS:
               bOpEq = HB_P_MINUSEQPOP;
               break;
            case HB_P_MULT:
               bOpEq = HB_P_MULTEQPOP;
               break;
            case HB_P_DIVIDE:
               bOpEq = HB_P_DIVEQPOP;
               break;
         }
         HB_EXPR_PCODE1( hb_compGenPCode1, bOpEq );
      }
      else
#endif
      {
         HB_EXPR_PCODE2( hb_compGenPCode2, HB_P_SENDSHORT, 0 );
         /* push increment value */
         HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
         /* increase operation */
         HB_EXPR_PCODE1( hb_compGenPCode1, bOpEq );
         /* Now do the assignment - call pop message with one argument */
         HB_EXPR_PCODE2( hb_compGenPCode2, HB_P_SENDSHORT, 1 );
         /* pop the unneeded value from the stack */
         HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_POP );
      }
      return;
   }
   /* TODO: add a special code for arrays to correctly handle a[ i++ ]++
    */
#if ! defined( HB_MACRO_SUPPORT )
   else if( ( bOpEq == HB_P_PLUS || bOpEq == HB_P_MINUS ||
              bOpEq == HB_P_MULT || bOpEq == HB_P_DIVIDE ) &&
            ( pSelf->value.asOperator.pLeft->ExprType == HB_ET_VARIABLE ) )
   {
      int iScope = hb_compVariableScope( HB_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol );

      if( iScope != HB_VS_LOCAL_FIELD && iScope != HB_VS_GLOBAL_FIELD &&
          iScope != HB_VS_UNDECLARED )
      {
         HB_EXPRTYPE iType = pSelf->value.asOperator.pRight->ExprType, iOldType;

         if( iType == HB_ET_NUMERIC || iType == HB_ET_STRING )
         {
            if( iScope == HB_VS_LOCAL_VAR && iType == HB_ET_NUMERIC && 
                ( bOpEq == HB_P_PLUS || bOpEq == HB_P_MINUS ) )
            {
               int iLocal = hb_compLocalGetPos( HB_COMP_PARAM, pSelf->value.asOperator.pLeft->value.asSymbol ); 

               if( iLocal < 256 && hb_compExprIsInteger( pSelf->value.asOperator.pRight ) )
               {
                  short iIncrement = ( short ) pSelf->value.asOperator.pRight->value.asNum.val.l;

                  if( bOpEq != HB_P_MINUS || iIncrement >= -INT16_MAX )
                  {
                     if( bOpEq == HB_P_MINUS )
                     {
                        iIncrement = -iIncrement;
                     }

                     hb_compGenPCode4( HB_P_LOCALNEARADDINT, ( BYTE ) iLocal, HB_LOBYTE( iIncrement ), HB_HIBYTE( iIncrement ), HB_COMP_PARAM );
                     return;
                  }
               }
            }
            /* NOTE: direct type change */
            iOldType = pSelf->value.asOperator.pLeft->ExprType;
            pSelf->value.asOperator.pLeft->ExprType = HB_ET_VARREF;

            HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
            HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
            switch( bOpEq )
            {
               case HB_P_PLUS:
                  bOpEq = HB_P_PLUSEQPOP;
                  break;
               case HB_P_MINUS:
                  bOpEq = HB_P_MINUSEQPOP;
                  break;
               case HB_P_MULT:
                  bOpEq = HB_P_MULTEQPOP;
                  break;
               case HB_P_DIVIDE:
                  bOpEq = HB_P_DIVEQPOP;
                  break;
            }
            HB_EXPR_PCODE1( hb_compGenPCode1, bOpEq );
            pSelf->value.asOperator.pLeft->ExprType = iOldType;
            return;
         }
         else if( iType == HB_ET_VARIABLE )
         {
            int iScope = hb_compVariableScope( HB_COMP_PARAM, pSelf->value.asOperator.pRight->value.asSymbol );

            if( iScope != HB_VS_LOCAL_FIELD && iScope != HB_VS_GLOBAL_FIELD &&
                iScope != HB_VS_UNDECLARED )
            {
               /* NOTE: direct type change */
               iOldType = pSelf->value.asOperator.pLeft->ExprType;
               pSelf->value.asOperator.pLeft->ExprType = HB_ET_VARREF;
               pSelf->value.asOperator.pRight->ExprType = HB_ET_VARREF;
               HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
               HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
               switch( bOpEq )
               {
                  case HB_P_PLUS:
                     bOpEq = HB_P_PLUSEQPOP;
                     break;
                  case HB_P_MINUS:
                     bOpEq = HB_P_MINUSEQPOP;
                     break;
                  case HB_P_MULT:
                     bOpEq = HB_P_MULTEQPOP;
                     break;
                  case HB_P_DIVIDE:
                     bOpEq = HB_P_DIVEQPOP;
                     break;
               }
               HB_EXPR_PCODE1( hb_compGenPCode1, bOpEq );
               pSelf->value.asOperator.pLeft->ExprType = iOldType;
               pSelf->value.asOperator.pRight->ExprType = iType;
               return;
            }
         }
      }
   }
#endif
   /* push old value */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
   /* push increment value */
   HB_EXPR_USE( pSelf->value.asOperator.pRight, HB_EA_PUSH_PCODE );
   /* add */
   HB_EXPR_PCODE1( hb_compGenPCode1, bOpEq );
   /* pop the new value into variable and remove it from the stack */
   HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
}

/* Generates the pcodes for pre- increment/decrement expressions
 */
void hb_compExprPushPreOp( HB_EXPR_PTR pSelf, BYTE bOper, HB_COMP_DECL )
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
      HB_EXPR_PCODE1( hb_compExprSendPopPush, pSelf->value.asOperator.pLeft );
      /* Do it. */

      /* Temporary disabled optimization with references to object variables
         untill we will not have extended reference items in our HVM [druzus] */
#ifdef HB_USE_OBJMSG_REF
      HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_PUSHOVARREF );

      /* increase/decrease operation */
      /* We have to unreference the item on the stack, because we do not have
         such PCODE(s) then I'll trnaslate HB_P_INC/HB_P_DEC into
         HB_P_[PLUS|MINUS]EQ, Maybe in the future we will make it
         in differ way [druzus] */

      HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_ONE );
      bOper = ( bOper == HB_P_INC ) ? HB_P_PLUSEQ : HB_P_MINUSEQ;

      HB_EXPR_PCODE1( hb_compGenPCode1, bOper );
#else
      HB_EXPR_PCODE2( hb_compGenPCode2, HB_P_SENDSHORT, 0 );
      /* increase/decrease operation */
      HB_EXPR_PCODE1( hb_compGenPCode1, bOper );
      /* Now, do the assignment - call pop message with one argument - it leaves the value on the stack */
      HB_EXPR_PCODE2( hb_compGenPCode2, HB_P_SENDSHORT, 1 );
#endif
   }
   else
   {
      /* Push current value */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
      /* Increment */
      HB_EXPR_PCODE1( hb_compGenPCode1, bOper );
      /* duplicate a value */
      HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_DUPLICATE );
      /* pop new value and leave the duplicated copy of it on the stack */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
   }
}

/* Generates the pcodes for post- increment/decrement expressions
 */
void hb_compExprPushPostOp( HB_EXPR_PTR pSelf, BYTE bOper, HB_COMP_DECL )
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
      /* push current value - it will be a result of whole expression */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
      /* now increment the value */
      HB_EXPR_PCODE2( hb_compExprPushPreOp, pSelf, bOper );
      /* pop the value from the stack */
      HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_POP );
   }
   else
   {
      /* Push current value */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
      /* Duplicate value */
      HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_DUPLICATE );
      /* Increment */
      HB_EXPR_PCODE1( hb_compGenPCode1, bOper );
      /* pop new value from the stack */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
   }
}

/* Generates the pcodes for increment/decrement operations
 * used standalone as a statement
 */
void hb_compExprUsePreOp( HB_EXPR_PTR pSelf, BYTE bOper, HB_COMP_DECL )
{
   /* NOTE: an object instance variable needs special handling
    */
   if( pSelf->value.asOperator.pLeft->ExprType == HB_ET_SEND )
   {
      HB_EXPR_PCODE2( hb_compExprPushPreOp, pSelf, bOper );
      /* pop the value from the stack */
      HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_POP );
   }
   else
   {
      /* Push current value */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_PUSH_PCODE );
      /* Increment */
      HB_EXPR_PCODE1( hb_compGenPCode1, bOper );
      /* pop new value from the stack */
      HB_EXPR_USE( pSelf->value.asOperator.pLeft, HB_EA_POP_PCODE );
   }
}

/* Generate pcode for aliased expression which contains macro operator on
 * the left or right side of the alias operator
 * expression->&macro or &macro->expression or &macro->&macro
 */
void hb_compExprUseAliasMacro( HB_EXPR_PTR pAliasedVar, BYTE bAction, HB_COMP_DECL )
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
      HB_EXPR_PCODE2( hb_compGenPushString, pAlias->value.asSymbol, strlen(pAlias->value.asSymbol) + 1 );
      HB_EXPR_USE( pVar, HB_EA_PUSH_PCODE );
      if( bAction == HB_EA_PUSH_PCODE )
         HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_MACROPUSHALIASED );
      else
         HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_MACROPOPALIASED );
   }
   else if( pVar->ExprType == HB_ET_VARIABLE )
   {
      /* NOTE:
       *    &macro->var is the  same as: &( macro + "->var" )
       */
      HB_EXPR_USE( pAlias, HB_EA_PUSH_PCODE );
      HB_EXPR_PCODE2( hb_compGenPushString, pVar->value.asSymbol, strlen(pVar->value.asSymbol) + 1 );
      if( bAction == HB_EA_PUSH_PCODE )
         HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_MACROPUSHALIASED );
      else
         HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_MACROPOPALIASED );
   }
   else
   {
      HB_EXPR_USE( pAlias, HB_EA_PUSH_PCODE );
      HB_EXPR_USE( pVar, HB_EA_PUSH_PCODE );
      if( bAction == HB_EA_PUSH_PCODE )
         HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_MACROPUSHALIASED );
      else
         HB_EXPR_PCODE1( hb_compGenPCode1, HB_P_MACROPOPALIASED );
   }

   /* Always add byte to pcode indicating requested macro compiler flag. */
   HB_EXPR_PCODE1( hb_compGenPCode1, HB_MACRO_GENFLAGS );
}


/* Reduces the list of expressions
 *
 * pExpr is the first expression on the list
 */
ULONG hb_compExprReduceList( HB_EXPR_PTR pExpr, HB_COMP_DECL )
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

BOOL hb_compExprIsValidMacro( char * szText, ULONG ulLen, BOOL *pfUseTextSubst, HB_COMP_DECL )
{
   BOOL fValid = TRUE;

   *pfUseTextSubst = FALSE;   /* no valid macro expression */

   while( ulLen-- && fValid )
   {
      if( *szText++ == '&' && ulLen )
      {
         char ch = *szText;
         /* Check if macro operator is used inside a string
          * Macro operator is ignored if it is the last char or
          * next char is '(' e.g. "this is &(ignored)"
          * (except if strict Clipper compatibility mode is enabled)
          *
          * NOTE: This uses _a-zA-Z pattern to check for
          * beginning of a variable name
          */

         if( ch == '_' || ( ch >= 'A' && ch <= 'Z' ) || ( ch >= 'a' && ch <= 'z' ) )
#if defined( HB_MACRO_SUPPORT )
         {
            HB_SYMBOL_UNUSED( HB_COMP_PARAM );
            *pfUseTextSubst = TRUE; /* valid macro expression */
            return TRUE;            /*there is no need to check all '&' occurences */
         }
#else
         {
            char szSymName[ HB_SYMBOL_NAME_LEN + 1 ];
            int iSize = 0;
            do
            {
               if( ch >= 'a' && ch <= 'z' )
                  szSymName[ iSize++ ] = ch - ( 'a' - 'A' );
               else if( ch == '_' || ( ch >= 'A' && ch <= 'Z' ) ||
                                     ( ch >= '0' && ch <= '9' ) )
                  szSymName[ iSize++ ] = ch;
               else
                  break;
               ch = *(++szText);
            }
            while( --ulLen && iSize < HB_SYMBOL_NAME_LEN );
            szSymName[ iSize ] = '\0';

            /* NOTE: All variables are assumed memvars in macro compiler -
             * there is no need to check for a valid name but to be Clipper
             * compatible we should check if local, static or field name
             * is not use and generate error in such case
             */
            *pfUseTextSubst = TRUE;
            fValid = hb_compIsValidMacroVar( szSymName, HB_COMP_PARAM );
         }
         else if( ! HB_COMP_ISSUPPORTED(HB_COMPFLAG_HARBOUR) )
            *pfUseTextSubst = TRUE;	/* always macro substitution in Clipper */
#endif
      }
   }

   return fValid;
}

/* Reduces the list of expressions
 *
 * pExpr is the first expression on the list
 */
HB_EXPR_PTR hb_compExprReducePlusStrings( HB_EXPR_PTR pLeft, HB_EXPR_PTR pRight, HB_COMP_DECL )
{
   if( pLeft->value.asString.dealloc )
   {
      pLeft->value.asString.string = (char *) hb_xrealloc( pLeft->value.asString.string, pLeft->ulLength + pRight->ulLength + 1 );
      memcpy( pLeft->value.asString.string + pLeft->ulLength,
              pRight->value.asString.string, pRight->ulLength );
      pLeft->ulLength += pRight->ulLength;
      pLeft->value.asString.string[ pLeft->ulLength ] = '\0';
   }
   else
   {
      char *szString;
      szString = (char *) hb_xgrab( pLeft->ulLength + pRight->ulLength + 1 );
      memcpy( szString, pLeft->value.asString.string, pLeft->ulLength );
      memcpy( szString + pLeft->ulLength, pRight->value.asString.string, pRight->ulLength );
      pLeft->ulLength += pRight->ulLength;
      szString[ pLeft->ulLength ] = '\0';
      pLeft->value.asString.string = szString;
      pLeft->value.asString.dealloc = TRUE;
   }
   hb_compExprFree( pRight, HB_COMP_PARAM );
   return pLeft;
}

#ifdef __WATCOMC__
/* enable warnings for unreferenced symbols */
#pragma warning 14 2
#endif
