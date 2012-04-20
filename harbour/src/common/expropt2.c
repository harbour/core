/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Expression Optimizer - reducing expressions
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

#include "hbmacro.h"
#include "hbcomp.h"
#include "hbdate.h"
#include "hbmath.h"

static HB_BOOL hb_compExprHasMacro( const char * szText, HB_SIZE nLen, HB_COMP_DECL )
{
   while( nLen-- )
   {
      if( *szText++ == '&' )
      {
         if( ! HB_SUPPORT_HARBOUR || ( nLen && ( *szText == '_' ||
             ( *szText >= 'A' && *szText <= 'Z' ) ||
             ( *szText >= 'a' && *szText <= 'z' ) ) ) )
         {
            return HB_TRUE;
         }
      }
   }
   return HB_FALSE;
}

static HB_EXPR_PTR hb_compExprReducePlusStrings( HB_EXPR_PTR pLeft, HB_EXPR_PTR pRight, HB_COMP_DECL )
{
   if( pLeft->value.asString.dealloc )
   {
      pLeft->value.asString.string = ( char * ) hb_xrealloc( pLeft->value.asString.string, pLeft->nLength + pRight->nLength + 1 );
      memcpy( pLeft->value.asString.string + pLeft->nLength,
              pRight->value.asString.string, pRight->nLength );
      pLeft->nLength += pRight->nLength;
      pLeft->value.asString.string[ pLeft->nLength ] = '\0';
   }
   else
   {
      char * szString;
      szString = ( char * ) hb_xgrab( pLeft->nLength + pRight->nLength + 1 );
      memcpy( szString, pLeft->value.asString.string, pLeft->nLength );
      memcpy( szString + pLeft->nLength, pRight->value.asString.string, pRight->nLength );
      pLeft->nLength += pRight->nLength;
      szString[ pLeft->nLength ] = '\0';
      pLeft->value.asString.string = szString;
      pLeft->value.asString.dealloc = HB_TRUE;
   }
   HB_COMP_EXPR_FREE( pRight );
   return pLeft;
}

static HB_EXPR_PTR hb_compExprReduceMinusStrings( HB_EXPR_PTR pLeft, HB_EXPR_PTR pRight, HB_COMP_DECL )
{
   char * szText = pLeft->value.asString.string;
   HB_SIZE nLen = pLeft->nLength;

   while( nLen && szText[ nLen - 1 ] == ' ' )
      --nLen;

   if( pLeft->value.asString.dealloc )
   {
      pLeft->value.asString.string = (char *) hb_xrealloc( pLeft->value.asString.string, pLeft->nLength + pRight->nLength + 1 );
      memcpy( pLeft->value.asString.string + nLen,
              pRight->value.asString.string, pRight->nLength );
      memset( pLeft->value.asString.string + nLen + pRight->nLength, ' ',
              pLeft->nLength - nLen );
      pLeft->nLength += pRight->nLength;
      pLeft->value.asString.string[ pLeft->nLength ] = '\0';
   }
   else
   {
      char *szString;
      szString = (char *) hb_xgrab( pLeft->nLength + pRight->nLength + 1 );
      memcpy( szString, pLeft->value.asString.string, nLen );
      memcpy( szString + nLen, pRight->value.asString.string, pRight->nLength );
      memset( szString + nLen + pRight->nLength, ' ', pLeft->nLength - nLen );
      pLeft->nLength += pRight->nLength;
      szString[ pLeft->nLength ] = '\0';
      pLeft->value.asString.string = szString;
      pLeft->value.asString.dealloc = HB_TRUE;
   }
   HB_COMP_EXPR_FREE( pRight );
   return pLeft;
}

HB_EXPR_PTR hb_compExprReduceMod( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC )
   {
      switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
      {
         case HB_ET_LONG:
            if( pRight->value.asNum.val.l )
            {
               pSelf->value.asNum.val.l = pLeft->value.asNum.val.l % pRight->value.asNum.val.l;
               pSelf->value.asNum.bDec = 0;
               pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
               pSelf->value.asNum.NumType = HB_ET_LONG;
               pSelf->ExprType = HB_ET_NUMERIC;
               pSelf->ValType  = HB_EV_NUMERIC;
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
            }
            break;

         default:
            if( HB_SUPPORT_HARBOUR )
            {
               double dValue, dDivisor;

               dDivisor = pRight->value.asNum.NumType == HB_ET_LONG ?
                          ( double ) pRight->value.asNum.val.l :
                          pRight->value.asNum.val.d;
               if( dDivisor )
               {
                  dValue = pLeft->value.asNum.NumType == HB_ET_LONG ?
                           ( double ) pLeft->value.asNum.val.l :
                           pLeft->value.asNum.val.d;
                  pSelf->value.asNum.val.d = fmod( dValue, dDivisor );
                  pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
                  pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
                  pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                  pSelf->ExprType = HB_ET_NUMERIC;
                  pSelf->ValType  = HB_EV_NUMERIC;
                  HB_COMP_EXPR_FREE( pLeft );
                  HB_COMP_EXPR_FREE( pRight );
               }
            }
      }
   }
   else
   {
      /* TODO: Check for incompatible types e.g.  3 % "txt"
      */
   }
   return pSelf;
}

HB_EXPR_PTR hb_compExprReduceDiv( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC )
   {
      HB_BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

      switch( bType )
      {
         case HB_ET_LONG:

            if( pRight->value.asNum.val.l )
            {
               if( pLeft->value.asNum.val.l % pRight->value.asNum.val.l == 0 )
               {
                  /* Return integer results as long */
                  pSelf->value.asNum.val.l = pLeft->value.asNum.val.l / pRight->value.asNum.val.l;
                  pSelf->value.asNum.bDec = 0;
                  pSelf->value.asNum.NumType = HB_ET_LONG;
               }
               else
               {
                  /* Return non-integer results as double */
                  pSelf->value.asNum.val.d = ( double ) pLeft->value.asNum.val.l / ( double ) pRight->value.asNum.val.l;
                  pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
                  pSelf->value.asNum.NumType = HB_ET_DOUBLE;
               }
               pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
               pSelf->ExprType = HB_ET_NUMERIC;
            }
            break;

         case HB_ET_DOUBLE:

            if( pRight->value.asNum.val.d != 0.0 )
            {
               pSelf->value.asNum.val.d = pLeft->value.asNum.val.d / pRight->value.asNum.val.d;
               pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
               pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
               pSelf->value.asNum.NumType = HB_ET_DOUBLE;
               pSelf->ExprType = HB_ET_NUMERIC;
            }
            break;

         default:

            if( pLeft->value.asNum.NumType == HB_ET_DOUBLE )
            {
               if( pRight->value.asNum.val.l )
               {
                  pSelf->value.asNum.val.d = pLeft->value.asNum.val.d / ( double ) pRight->value.asNum.val.l;
                  pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
                  pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
                  pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                  pSelf->ExprType = HB_ET_NUMERIC;
               }
            }
            else
            {
               if( pRight->value.asNum.val.d != 0.0 )
               {
                  pSelf->value.asNum.val.d = ( double ) pLeft->value.asNum.val.l / pRight->value.asNum.val.d;
                  pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
                  pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
                  pSelf->value.asNum.NumType = HB_ET_DOUBLE;
                  pSelf->ExprType = HB_ET_NUMERIC;
               }
            }

      } /* switch bType */

      if( pSelf->ExprType == HB_ET_NUMERIC )
      {
         /* The expression was reduced - delete old components */
         pSelf->ValType = HB_EV_NUMERIC;
         HB_COMP_EXPR_FREE( pLeft );
         HB_COMP_EXPR_FREE( pRight );
      }
   }
   else
   {
      /* TODO: Check for incompatible types e.g.  3 / "txt"
      */
   }
   return pSelf;
}

HB_EXPR_PTR hb_compExprReduceMult( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC )
   {
      HB_BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

      switch( bType )
      {
         case HB_ET_LONG:
         {
            HB_MAXDBL dVal = ( HB_MAXDBL ) pLeft->value.asNum.val.l * ( HB_MAXDBL ) pRight->value.asNum.val.l;

            if( HB_DBL_LIM_LONG( dVal ) )
            {
               pSelf->value.asNum.val.l = pLeft->value.asNum.val.l * pRight->value.asNum.val.l;
               pSelf->value.asNum.NumType = HB_ET_LONG;
            }
            else
            {
               pSelf->value.asNum.val.d = ( double ) dVal;
               pSelf->value.asNum.NumType = HB_ET_DOUBLE;
            }
            pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
            pSelf->value.asNum.bDec = 0;
            break;
         }

         case HB_ET_DOUBLE:
         {
            pSelf->value.asNum.val.d = pLeft->value.asNum.val.d * pRight->value.asNum.val.d;
            pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
            pSelf->value.asNum.bDec = ( HB_UCHAR ) ( pLeft->value.asNum.bDec + pRight->value.asNum.bDec );
            pSelf->value.asNum.NumType = HB_ET_DOUBLE;
            break;
         }

         default:
         {
            if( pLeft->value.asNum.NumType == HB_ET_DOUBLE )
            {
               pSelf->value.asNum.val.d = pLeft->value.asNum.val.d * ( double ) pRight->value.asNum.val.l;
               pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
            }
            else
            {
               pSelf->value.asNum.val.d = ( double ) pLeft->value.asNum.val.l * pRight->value.asNum.val.d;
               pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
            }
            pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
            pSelf->value.asNum.NumType = HB_ET_DOUBLE;
         }
      }
      pSelf->ExprType = HB_ET_NUMERIC;
      pSelf->ValType  = HB_EV_NUMERIC;
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
   }
   else
   {
      /* TODO: Check for incompatible types e.g. 3 * "txt"
      */
   }
   return pSelf;
}

HB_EXPR_PTR hb_compExprReducePower( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC )
   {
      HB_BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

      switch( bType )
      {
         case HB_ET_LONG:
            pSelf->value.asNum.val.d = pow( ( double ) pLeft->value.asNum.val.l,
                                            ( double ) pRight->value.asNum.val.l );
            break;

         case HB_ET_DOUBLE:
            pSelf->value.asNum.val.d = pow( pLeft->value.asNum.val.d,
                                            pRight->value.asNum.val.d );
            break;

         default:
            if( pLeft->value.asNum.NumType == HB_ET_DOUBLE )
               pSelf->value.asNum.val.d = pow( pLeft->value.asNum.val.d,
                                               ( double ) pRight->value.asNum.val.l );
            else
               pSelf->value.asNum.val.d = pow( ( double ) pLeft->value.asNum.val.l,
                                               pRight->value.asNum.val.d );
            break;
      }
      pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
      pSelf->value.asNum.bDec = HB_DEFAULT_DECIMALS;
      pSelf->value.asNum.NumType = HB_ET_DOUBLE;
      pSelf->ExprType = HB_ET_NUMERIC;
      pSelf->ValType  = HB_EV_NUMERIC;
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
   }
   else
   {
      /* TODO: Check for incompatible types e.g. 3 * "txt"
      */
   }
   return pSelf;
}

static void hb_compExprReduceTimeStampPut( HB_EXPR_PTR pExpr, long lJulian, long lMilliSec )
{
   /* timestamp normalization */
   if( lJulian < 0 )
   {
      if( lMilliSec <= -HB_MILLISECS_PER_DAY )
      {
         lMilliSec += HB_MILLISECS_PER_DAY;
         --lJulian;
      }
      else if( lMilliSec > 0 )
      {
         lMilliSec -= HB_MILLISECS_PER_DAY;
         ++lJulian;
         if( lMilliSec > 0 )
         {
            lMilliSec -= HB_MILLISECS_PER_DAY;
            ++lJulian;
         }
      }
   }
   else
   {
      if( lMilliSec >= HB_MILLISECS_PER_DAY )
      {
         lMilliSec -= HB_MILLISECS_PER_DAY;
         ++lJulian;
      }
      else if( lMilliSec < 0 )
      {
         lMilliSec += HB_MILLISECS_PER_DAY;
         --lJulian;
         if( lMilliSec < 0 )
         {
            lMilliSec += HB_MILLISECS_PER_DAY;
            --lJulian;
         }
      }
   }

   pExpr->value.asDate.lDate = lJulian;
   pExpr->value.asDate.lTime = lMilliSec;
   pExpr->ExprType = HB_ET_TIMESTAMP;
   pExpr->ValType  = HB_EV_TIMESTAMP;
}

static void hb_compExprReduceTimeStampAdd( HB_EXPR_PTR pExpr, HB_EXPR_PTR pTimeStamp, double dValue )
{
   long lJulian, lMilliSec;

   hb_timeStampUnpackDT( dValue, &lJulian, &lMilliSec );

   lJulian += pTimeStamp->value.asDate.lDate;
   lMilliSec += pTimeStamp->value.asDate.lTime;

   hb_compExprReduceTimeStampPut( pExpr, lJulian, lMilliSec );
}

HB_EXPR_PTR hb_compExprReduceMinus( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == HB_ET_NUMERIC && pRight->ExprType == HB_ET_NUMERIC )
   {
      HB_BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

      switch( bType )
      {
         case HB_ET_LONG:
         {
            HB_MAXDBL dVal = ( HB_MAXDBL ) pLeft->value.asNum.val.l - ( HB_MAXDBL ) pRight->value.asNum.val.l;

            if( HB_DBL_LIM_LONG( dVal ) )
            {
               pSelf->value.asNum.val.l = pLeft->value.asNum.val.l - pRight->value.asNum.val.l;
               pSelf->value.asNum.NumType = HB_ET_LONG;
            }
            else
            {
               pSelf->value.asNum.val.d = ( double ) dVal;
               pSelf->value.asNum.NumType = HB_ET_DOUBLE;
            }
            pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
            pSelf->value.asNum.bDec = 0;

            break;
         }

         case HB_ET_DOUBLE:
         {
            pSelf->value.asNum.val.d = pLeft->value.asNum.val.d - pRight->value.asNum.val.d;
            pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
            if( pLeft->value.asNum.bDec < pRight->value.asNum.bDec )
               pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
            else
               pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
            pSelf->value.asNum.NumType = HB_ET_DOUBLE;

            break;
         }

         default:
         {
            if( pLeft->value.asNum.NumType == HB_ET_DOUBLE )
            {
               pSelf->value.asNum.val.d = pLeft->value.asNum.val.d - ( double ) pRight->value.asNum.val.l;
               pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
            }
            else
            {
               pSelf->value.asNum.val.d = ( double ) pLeft->value.asNum.val.l - pRight->value.asNum.val.d;
               pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
            }
            pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
            pSelf->value.asNum.NumType = HB_ET_DOUBLE;
         }
      }
      pSelf->ExprType = HB_ET_NUMERIC;
      pSelf->ValType  = HB_EV_NUMERIC;
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
   }
   else if( ( pLeft->ExprType == HB_ET_DATE || pLeft->ExprType == HB_ET_TIMESTAMP ) &&
            ( pRight->ExprType == HB_ET_DATE || pRight->ExprType == HB_ET_TIMESTAMP ) )
   {
      long lTime = pLeft->value.asDate.lTime - pRight->value.asDate.lTime,
           lDate = pLeft->value.asDate.lDate - pRight->value.asDate.lDate;
      if( lTime == 0 )
      {
         pSelf->value.asNum.val.l = lDate;
         pSelf->value.asNum.bDec = 0;
         pSelf->value.asNum.NumType = HB_ET_LONG;
      }
      else
      {
         pSelf->value.asNum.val.d = hb_timeStampPackDT( lDate, lTime );
         pSelf->value.asNum.bDec = HB_TIMEDIFF_DEC;
         pSelf->value.asNum.NumType = HB_ET_DOUBLE;
      }
      pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
      pSelf->ExprType = HB_ET_NUMERIC;
      pSelf->ValType  = HB_EV_NUMERIC;
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
   }
   else if( pLeft->ExprType == HB_ET_DATE && pRight->ExprType == HB_ET_NUMERIC )
   {
      if( pRight->value.asNum.NumType == HB_ET_LONG )
         pSelf->value.asDate.lDate =  pLeft->value.asDate.lDate - ( long ) pRight->value.asNum.val.l;
      else
         pSelf->value.asDate.lDate = pLeft->value.asDate.lDate - ( long ) ( unsigned long ) pRight->value.asNum.val.d;
      pSelf->value.asDate.lTime = 0;
      pSelf->ExprType = HB_ET_DATE;
      pSelf->ValType  = HB_EV_DATE;
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
   }
   else if( pLeft->ExprType == HB_ET_TIMESTAMP && pRight->ExprType == HB_ET_NUMERIC )
   {
      if( pRight->value.asNum.NumType == HB_ET_LONG )
         hb_compExprReduceTimeStampPut( pSelf, pLeft->value.asDate.lDate - ( long ) pRight->value.asNum.val.l,
                                        pLeft->value.asDate.lTime );
      else
         hb_compExprReduceTimeStampAdd( pSelf, pLeft, - pRight->value.asNum.val.d );
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
   }
   else if( pLeft->ExprType == HB_ET_STRING && pRight->ExprType == HB_ET_STRING )
   {
      if( pRight->nLength == 0 )
      {
         pSelf->ExprType = HB_ET_NONE; /* suppress deletion of operator components */
         HB_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft;
         HB_COMP_EXPR_FREE( pRight );
      }
      else if( pLeft->nLength == 0 )
      {
         pSelf->ExprType = HB_ET_NONE; /* suppress deletion of operator components */
         HB_COMP_EXPR_FREE( pSelf );
         pSelf = pRight;
         HB_COMP_EXPR_FREE( pLeft );
      }
      else
      {
         HB_BOOL fReduce = HB_TRUE;

         /* Do not reduce strings with the macro operator '&'
          */
         if( HB_SUPPORT_MACROTEXT )
         {
            char * szText = pLeft->value.asString.string;
            HB_SIZE nLen = pLeft->nLength;
            while( nLen && szText[ nLen - 1 ] == ' ' )
               --nLen;
            while( nLen-- )
            {
               if( *szText++ == '&' )
               {
                  char ch = nLen ? *szText : *pRight->value.asString.string;
                  if( ( ch >= 'A' && ch <= 'Z' ) ||
                      ( ch >= 'a' && ch <= 'z' ) || ch == '_' ||
                      ! HB_SUPPORT_HARBOUR )
                  {
                     fReduce = HB_FALSE;
                     break;
                  }
               }
            }
         }

         if( fReduce )
         {
            pSelf->ExprType = HB_ET_NONE; /* suppress deletion of operator components */
            HB_COMP_EXPR_FREE( pSelf );
            pSelf = hb_compExprReduceMinusStrings( pLeft, pRight, HB_COMP_PARAM );
         }
      }
   }
   else
   {
      /* TODO: Check for incompatible types e.g. "txt" - 3
      */
   }
   return pSelf;
}

static HB_BOOL hb_compExprReducePlusNums( HB_EXPR_PTR pSelf, HB_EXPR_PTR pAdd )
{
   HB_EXPR_PTR pLeft, pRight, pNum;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == HB_ET_NUMERIC )
      pNum = pLeft;
   else if( pRight->ExprType == HB_ET_NUMERIC )
      pNum = pRight;
   else if( pLeft->ExprType == HB_EO_PLUS )
      return hb_compExprReducePlusNums( pLeft, pAdd );
   else if( pRight->ExprType == HB_EO_PLUS )
      return hb_compExprReducePlusNums( pRight, pAdd );
   else
      return HB_FALSE;

   switch( pNum->value.asNum.NumType & pAdd->value.asNum.NumType )
   {
      case HB_ET_LONG:
      {
         HB_MAXDBL dVal = ( HB_MAXDBL ) pNum->value.asNum.val.l + ( HB_MAXDBL ) pAdd->value.asNum.val.l;
         if( HB_DBL_LIM_LONG( dVal ) )
            pNum->value.asNum.val.l += pAdd->value.asNum.val.l;
         else
         {
            pNum->value.asNum.val.d = ( double ) dVal;
            pNum->value.asNum.NumType = HB_ET_DOUBLE;
         }
         pNum->value.asNum.bWidth = HB_DEFAULT_WIDTH;
         pNum->value.asNum.bDec = 0;
         break;
      }

      case HB_ET_DOUBLE:
         pNum->value.asNum.val.d += pAdd->value.asNum.val.d;
         pNum->value.asNum.bWidth = HB_DEFAULT_WIDTH;
         if( pNum->value.asNum.bDec < pAdd->value.asNum.bDec )
            pNum->value.asNum.bDec = pAdd->value.asNum.bDec;
         break;

      default:
         if( pNum->value.asNum.NumType == HB_ET_DOUBLE )
            pNum->value.asNum.val.d += ( double ) pAdd->value.asNum.val.l;
         else
         {
            pNum->value.asNum.val.d = ( double ) pNum->value.asNum.val.l + pAdd->value.asNum.val.d;
            pNum->value.asNum.bDec = pAdd->value.asNum.bDec;
            pNum->value.asNum.NumType = HB_ET_DOUBLE;
         }
         pNum->value.asNum.bWidth = HB_DEFAULT_WIDTH;
         break;
   }

   return HB_TRUE;
}

HB_EXPR_PTR hb_compExprReducePlus( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == HB_ET_NUMERIC )
   {
      if( pRight->ExprType == HB_ET_NUMERIC )
      {
         HB_BYTE bType = ( pLeft->value.asNum.NumType & pRight->value.asNum.NumType );

         switch( bType )
         {
            case HB_ET_LONG:
            {
               HB_MAXDBL dVal = ( HB_MAXDBL ) pLeft->value.asNum.val.l + ( HB_MAXDBL ) pRight->value.asNum.val.l;

               if( HB_DBL_LIM_LONG( dVal ) )
               {
                  pSelf->value.asNum.val.l = pLeft->value.asNum.val.l + pRight->value.asNum.val.l;
                  pSelf->value.asNum.NumType = HB_ET_LONG;
               }
               else
               {
                  pSelf->value.asNum.val.d = ( double ) dVal;
                  pSelf->value.asNum.NumType = HB_ET_DOUBLE;
               }
               pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
               pSelf->value.asNum.bDec = 0;
               break;
            }

            case HB_ET_DOUBLE:
               pSelf->value.asNum.val.d = pLeft->value.asNum.val.d + pRight->value.asNum.val.d;
               pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
               if( pLeft->value.asNum.bDec < pRight->value.asNum.bDec )
                  pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
               else
                  pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
               pSelf->value.asNum.NumType = HB_ET_DOUBLE;
               break;

            default:
               if( pLeft->value.asNum.NumType == HB_ET_DOUBLE )
               {
                  pSelf->value.asNum.val.d = pLeft->value.asNum.val.d + ( double ) pRight->value.asNum.val.l;
                  pSelf->value.asNum.bDec = pLeft->value.asNum.bDec;
               }
               else
               {
                  pSelf->value.asNum.val.d = ( double ) pLeft->value.asNum.val.l + pRight->value.asNum.val.d;
                  pSelf->value.asNum.bDec = pRight->value.asNum.bDec;
               }
               pSelf->value.asNum.bWidth = HB_DEFAULT_WIDTH;
               pSelf->value.asNum.NumType = HB_ET_DOUBLE;
         }
         pSelf->ExprType = HB_ET_NUMERIC;
         pSelf->ValType  = HB_EV_NUMERIC;
         HB_COMP_EXPR_FREE( pLeft );
         HB_COMP_EXPR_FREE( pRight );
      }
      else if( pRight->ExprType == HB_ET_DATE )
      {
         if( pLeft->value.asNum.NumType == HB_ET_LONG )
            pSelf->value.asDate.lDate = pRight->value.asDate.lDate + ( long ) pLeft->value.asNum.val.l;
         else
            pSelf->value.asDate.lDate = pRight->value.asDate.lDate + ( long ) ( unsigned long ) pLeft->value.asNum.val.d;
         pSelf->value.asDate.lTime = 0;
         pSelf->ExprType = HB_ET_DATE;
         pSelf->ValType  = HB_EV_DATE;
         HB_COMP_EXPR_FREE( pLeft );
         HB_COMP_EXPR_FREE( pRight );
      }
      else if( pRight->ExprType == HB_ET_TIMESTAMP )
      {
         if( pLeft->value.asNum.NumType == HB_ET_LONG )
            hb_compExprReduceTimeStampPut( pSelf, pRight->value.asDate.lDate + ( long ) pLeft->value.asNum.val.l,
                                           pRight->value.asDate.lTime );
         else
            hb_compExprReduceTimeStampAdd( pSelf, pRight, pLeft->value.asNum.val.d );
         HB_COMP_EXPR_FREE( pLeft );
         HB_COMP_EXPR_FREE( pRight );
      }
      else if( HB_SUPPORT_EXTOPT &&
               ( pLeft->value.asNum.NumType == HB_ET_LONG ?
                 pLeft->value.asNum.val.l == 0 :
                 pLeft->value.asNum.val.d == 0 ) )
      {
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         pSelf->ExprType = HB_ET_NONE; /* suppress deletion of operator components */
         HB_COMP_EXPR_FREE( pSelf );
         pSelf = pRight;
         HB_COMP_EXPR_FREE( pLeft );
      }
      else if( HB_SUPPORT_EXTOPT && pRight->ExprType == HB_EO_PLUS )
      {
         if( hb_compExprReducePlusNums( pRight, pLeft ) )
         {
            pSelf->ExprType = HB_ET_NONE; /* suppress deletion of operator components */
            HB_COMP_EXPR_FREE( pSelf );
            pSelf = pRight;
            HB_COMP_EXPR_FREE( pLeft );
         }
      }
      else
      {
         /* TODO: Check for incompatible types e.g. "txt" + 3
          */
      }
   }
   else if( pRight->ExprType == HB_ET_NUMERIC )
   {
      if( pLeft->ExprType == HB_ET_DATE )
      {
         if( pRight->value.asNum.NumType == HB_ET_LONG )
            pSelf->value.asDate.lDate = pLeft->value.asDate.lDate + ( long ) pRight->value.asNum.val.l;
         else
            pSelf->value.asDate.lDate = pLeft->value.asDate.lDate + ( long ) ( unsigned long ) pRight->value.asNum.val.d;
         pSelf->value.asDate.lTime = 0;
         pSelf->ExprType = HB_ET_DATE;
         pSelf->ValType  = HB_EV_DATE;
         HB_COMP_EXPR_FREE( pLeft );
         HB_COMP_EXPR_FREE( pRight );
      }
      else if( pLeft->ExprType == HB_ET_TIMESTAMP )
      {
         if( pRight->value.asNum.NumType == HB_ET_LONG )
            hb_compExprReduceTimeStampPut( pSelf, pLeft->value.asDate.lDate + ( long ) pRight->value.asNum.val.l,
                                           pLeft->value.asDate.lTime );
         else
            hb_compExprReduceTimeStampAdd( pSelf, pLeft, pRight->value.asNum.val.d );
         HB_COMP_EXPR_FREE( pLeft );
         HB_COMP_EXPR_FREE( pRight );
      }
      else if( HB_SUPPORT_EXTOPT &&
               ( pRight->value.asNum.NumType == HB_ET_LONG ?
                 pRight->value.asNum.val.l == 0 :
                 pRight->value.asNum.val.d == 0 ) )
      {
         /* NOTE: This will not generate a runtime error if incompatible
          * data type is used
          */
         pSelf->ExprType = HB_ET_NONE; /* suppress deletion of operator components */
         HB_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft;
         HB_COMP_EXPR_FREE( pRight );
      }
      else if( HB_SUPPORT_EXTOPT && pLeft->ExprType == HB_EO_PLUS )
      {
         if( hb_compExprReducePlusNums( pLeft, pRight ) )
         {
            pSelf->ExprType = HB_ET_NONE; /* suppress deletion of operator components */
            HB_COMP_EXPR_FREE( pSelf );
            pSelf = pLeft;
            HB_COMP_EXPR_FREE( pRight );
         }
      }
      else
      {
         /* TODO: Check for incompatible types e.g. "txt" + 3
         */
      }
   }
   else if( ( pLeft->ExprType == HB_ET_DATE || pLeft->ExprType == HB_ET_TIMESTAMP ) &&
            ( pRight->ExprType == HB_ET_DATE || pRight->ExprType == HB_ET_TIMESTAMP ) )
   {
      if( pLeft->ExprType == HB_ET_TIMESTAMP || pRight->ExprType == HB_ET_TIMESTAMP )
      {
         hb_compExprReduceTimeStampPut( pSelf,
                           pLeft->value.asDate.lDate + pRight->value.asDate.lDate,
                           pLeft->value.asDate.lTime + pRight->value.asDate.lTime );
      }
      else
      {
         /* NOTE: This is not a bug. CA-Cl*pper does exactly that for DATEs. */
         pSelf->value.asDate.lDate = pLeft->value.asDate.lDate + pRight->value.asDate.lDate;
         pSelf->value.asDate.lTime = 0;
         pSelf->ExprType = HB_ET_DATE;
         pSelf->ValType  = HB_EV_DATE;
      }
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
   }
   else if( pLeft->ExprType == HB_ET_STRING && pRight->ExprType == HB_ET_STRING )
   {
      if( pRight->nLength == 0 )
      {
         pSelf->ExprType = HB_ET_NONE; /* suppress deletion of operator components */
         HB_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft;
         HB_COMP_EXPR_FREE( pRight );
      }
      else if( pLeft->nLength == 0 )
      {
         pSelf->ExprType = HB_ET_NONE; /* suppress deletion of operator components */
         HB_COMP_EXPR_FREE( pSelf );
         pSelf = pRight;
         HB_COMP_EXPR_FREE( pLeft );
      }
      else
      {
         HB_BOOL fReduce = HB_TRUE;

         /* Do not reduce strings with the macro operator '&'
          */
         if( HB_SUPPORT_MACROTEXT )
         {
            char * szText = pLeft->value.asString.string;
            HB_SIZE nLen = pLeft->nLength;

            while( nLen-- )
            {
               if( *szText++ == '&' )
               {
                  char ch = nLen ? *szText : *pRight->value.asString.string;
                  if( ( ch >= 'A' && ch <= 'Z' ) ||
                      ( ch >= 'a' && ch <= 'z' ) || ch == '_' ||
                      ! HB_SUPPORT_HARBOUR )
                  {
                     fReduce = HB_FALSE;
                     break;
                  }
               }
            }
         }
         if( fReduce )
         {
            pSelf->ExprType = HB_ET_NONE; /* suppress deletion of operator components */
            HB_COMP_EXPR_FREE( pSelf );
            pSelf = hb_compExprReducePlusStrings( pLeft, pRight, HB_COMP_PARAM );
         }
      }
   }
   else
   {
      /* TODO: Check for incompatible types e.g. "txt" + 3
      */
   }
   return pSelf;
}

HB_EXPR_PTR hb_compExprReduceNegate( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   pExpr = pSelf->value.asOperator.pLeft;

   if( pExpr->ExprType == HB_ET_NUMERIC )
   {
      if( pExpr->value.asNum.NumType == HB_ET_DOUBLE )
      {
         pExpr->value.asNum.val.d = - pExpr->value.asNum.val.d;
         pExpr->value.asNum.bWidth = HB_DEFAULT_WIDTH;
      }
      else
      {
#if -HB_VMLONG_MAX > HB_VMLONG_MIN
         if( pExpr->value.asNum.val.l < -HB_VMLONG_MAX )
         {
            pExpr->value.asNum.NumType = HB_ET_DOUBLE;
            pExpr->value.asNum.val.d = - ( double ) pExpr->value.asNum.val.l;
            pExpr->value.asNum.bDec = 0;
         }
         else
#endif
         {
            pExpr->value.asNum.val.l = - pExpr->value.asNum.val.l;
         }
         pExpr->value.asNum.bWidth = HB_DEFAULT_WIDTH;
      }
      pSelf->ExprType = HB_ET_NONE; /* suppress deletion of operator components */
      HB_COMP_EXPR_FREE( pSelf );
      pSelf = pExpr;
   }
   else if( pExpr->ExprType == HB_EO_NEGATE && HB_SUPPORT_EXTOPT )
   {
      /* NOTE: This will not generate a runtime error if incompatible
       * data type is used
       */
      pExpr->ExprType = HB_ET_NONE; /* suppress deletion of operator components */
      pExpr = pExpr->value.asOperator.pLeft;
      HB_COMP_EXPR_FREE( pSelf );
      pSelf = pExpr;
   }

   return pSelf;
}


HB_EXPR_PTR hb_compExprReduceIN( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == pRight->ExprType && pLeft->ExprType == HB_ET_STRING )
   {
      /* Both arguments are literal strings
       */

      /* NOTE: If macro substitiution is not didabled (-kM compiler
       *       switch) then we cannot reduce also strings which
       *       have macro operator '&'
       */
      if( !HB_SUPPORT_MACROTEXT ||
          ( !hb_compExprHasMacro( pLeft->value.asString.string,
                                  pLeft->nLength, HB_COMP_PARAM ) &&
            !hb_compExprHasMacro( pRight->value.asString.string,
                                  pRight->nLength, HB_COMP_PARAM ) ) )
      {
         HB_BOOL bResult;

         /* NOTE: CA-Cl*pper has a bug where the $ operator returns .T.
             *       when an empty string is searched [vszakats]
             *
             *       But this bug exist only in compiler and CA-Cl*pper macro
             *       compiler does not have optimizer. This bug is replicated
             *       by us only when Harbour extensions in compiler (-kh) are
             *       not enabled f.e. in strict Clipper cmpatible mode (-kc)
             *       [druzus]
             */
         if( pLeft->nLength == 0 )
            bResult = HB_COMP_PARAM->mode == HB_MODE_COMPILER &&
                      ! HB_SUPPORT_HARBOUR;
         else
            bResult = ( hb_strAt( pLeft->value.asString.string, pLeft->nLength,
                                  pRight->value.asString.string, pRight->nLength ) != 0 );

         HB_COMP_EXPR_FREE( pLeft );
         HB_COMP_EXPR_FREE( pRight );
         pSelf->ExprType = HB_ET_LOGICAL;
         pSelf->ValType  = HB_EV_LOGICAL;
         pSelf->value.asLogical = bResult;
      }
   }
   /* TODO: add checking for incompatible types
    */
   return pSelf;
}

HB_EXPR_PTR hb_compExprReduceNE( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == pRight->ExprType )
   {
      switch( pLeft->ExprType )
      {
         case HB_ET_LOGICAL:
            {
               /* .F. != .T.  = .T.
               * .T. != .T.  = .F.
               * .F. != .F.  = .F.
               * .T. != .F.  = .T.
               */
               HB_BOOL bResult = ( pLeft->value.asLogical != pRight->value.asLogical );
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

         case HB_ET_STRING:
            /* NOTE: the result depends on SET EXACT setting then it
            * cannot be optimized except the case when NULL string are
            * compared - "" != "" is always HB_FALSE regardless of EXACT
            * setting
            */
            if( ( pLeft->nLength | pRight->nLength ) == 0 )
            {
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = HB_FALSE;

               /* NOTE: COMPATIBILITY: Clipper doesn't optimize this */
            }
            break;

         case HB_ET_NUMERIC:
            {
               HB_BOOL bResult;

               switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
               {
                  case HB_ET_LONG:
                     bResult = ( pLeft->value.asNum.val.l != pRight->value.asNum.val.l );
                     break;
                  case HB_ET_DOUBLE:
                     bResult = ( pLeft->value.asNum.val.d != pRight->value.asNum.val.d );
                     break;
                  default:
                     {
                        if( pLeft->value.asNum.NumType == HB_ET_LONG )
                           bResult = ( pLeft->value.asNum.val.l != pRight->value.asNum.val.d );
                        else
                           bResult = ( pLeft->value.asNum.val.d != pRight->value.asNum.val.l );
                     }
                     break;
               }
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

         case HB_ET_DATE:
         case HB_ET_TIMESTAMP:
            {
               HB_BOOL bResult = pLeft->value.asDate.lDate != pRight->value.asDate.lDate ||
                                 pLeft->value.asDate.lTime != pRight->value.asDate.lTime;
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

         case HB_ET_NIL:
            HB_COMP_EXPR_FREE( pLeft );
            HB_COMP_EXPR_FREE( pRight );
            pSelf->ExprType = HB_ET_LOGICAL;
            pSelf->ValType  = HB_EV_LOGICAL;
            pSelf->value.asLogical = HB_FALSE;
            break;
      }
   }
   else if( ( pLeft->ExprType == HB_ET_TIMESTAMP &&
              pRight->ExprType == HB_ET_DATE ) ||
            ( pLeft->ExprType == HB_ET_DATE &&
              pRight->ExprType == HB_ET_TIMESTAMP ) )
   {
      pSelf->value.asLogical = pLeft->value.asDate.lDate != pRight->value.asDate.lDate;
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType  = HB_EV_LOGICAL;
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
   }
   else if( ( pLeft->ExprType == HB_ET_NIL &&
              ( pRight->ExprType == HB_ET_NUMERIC ||
                pRight->ExprType == HB_ET_LOGICAL ||
                pRight->ExprType == HB_ET_DATE ||
                pRight->ExprType == HB_ET_TIMESTAMP ||
                pRight->ExprType == HB_ET_STRING ||
                pRight->ExprType == HB_ET_CODEBLOCK ||
                pRight->ExprType == HB_ET_ARRAY ||
                pRight->ExprType == HB_ET_HASH ||
                pRight->ExprType == HB_ET_FUNREF ) ) ||
            ( pRight->ExprType == HB_ET_NIL &&
              ( pLeft->ExprType == HB_ET_NUMERIC ||
                pLeft->ExprType == HB_ET_LOGICAL ||
                pLeft->ExprType == HB_ET_DATE ||
                pLeft->ExprType == HB_ET_TIMESTAMP ||
                pLeft->ExprType == HB_ET_STRING ||
                pLeft->ExprType == HB_ET_CODEBLOCK ||
                pLeft->ExprType == HB_ET_ARRAY ||
                pLeft->ExprType == HB_ET_HASH ||
                pLeft->ExprType == HB_ET_FUNREF ) ) )
   {
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType  = HB_EV_LOGICAL;
      pSelf->value.asLogical = HB_TRUE;
   }
   /* TODO: add checking of incompatible types
   else
   {
   }
   */
   return pSelf;
}

HB_EXPR_PTR hb_compExprReduceGE( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

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
               HB_BOOL bResult = ! ( ! pLeft->value.asLogical && pRight->value.asLogical );
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

         case HB_ET_NUMERIC:
            {
               HB_BOOL bResult;

               switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
               {
                  case HB_ET_LONG:
                     bResult = ( pLeft->value.asNum.val.l >= pRight->value.asNum.val.l );
                     break;
                  case HB_ET_DOUBLE:
                     bResult = ( pLeft->value.asNum.val.d >= pRight->value.asNum.val.d );
                     break;
                  default:
                     {
                        if( pLeft->value.asNum.NumType == HB_ET_LONG )
                           bResult = ( pLeft->value.asNum.val.l >= pRight->value.asNum.val.d );
                        else
                           bResult = ( pLeft->value.asNum.val.d >= pRight->value.asNum.val.l );
                     }
                     break;
               }
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

         case HB_ET_DATE:
         case HB_ET_TIMESTAMP:
            {
               HB_BOOL bResult = ( pLeft->value.asDate.lDate > pRight->value.asDate.lDate ) ||
                                 ( pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                                   pLeft->value.asDate.lTime >= pRight->value.asDate.lTime );
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

      }
   else if( ( pLeft->ExprType == HB_ET_TIMESTAMP &&
              pRight->ExprType == HB_ET_DATE ) ||
            ( pLeft->ExprType == HB_ET_DATE &&
              pRight->ExprType == HB_ET_TIMESTAMP ) )
   {
      pSelf->value.asLogical = pLeft->value.asDate.lDate >= pRight->value.asDate.lDate;
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType  = HB_EV_LOGICAL;
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
   }
   /* TODO: add checking of incompatible types
   else
   {
   }
   */
   return pSelf;
}

HB_EXPR_PTR hb_compExprReduceLE( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

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
               HB_BOOL bResult = ! ( pLeft->value.asLogical && ! pRight->value.asLogical );
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

         case HB_ET_NUMERIC:
            {
               HB_BOOL bResult;

               switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
               {
                  case HB_ET_LONG:
                     bResult = ( pLeft->value.asNum.val.l <= pRight->value.asNum.val.l );
                     break;
                  case HB_ET_DOUBLE:
                     bResult = ( pLeft->value.asNum.val.d <= pRight->value.asNum.val.d );
                     break;
                  default:
                     {
                        if( pLeft->value.asNum.NumType == HB_ET_LONG )
                           bResult = ( pLeft->value.asNum.val.l <= pRight->value.asNum.val.d );
                        else
                           bResult = ( pLeft->value.asNum.val.d <= pRight->value.asNum.val.l );
                     }
                     break;
               }
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

         case HB_ET_DATE:
         case HB_ET_TIMESTAMP:
            {
               HB_BOOL bResult = ( pLeft->value.asDate.lDate < pRight->value.asDate.lDate ) ||
                                 ( pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                                   pLeft->value.asDate.lTime <= pRight->value.asDate.lTime );
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

      }
   else if( ( pLeft->ExprType == HB_ET_TIMESTAMP &&
              pRight->ExprType == HB_ET_DATE ) ||
            ( pLeft->ExprType == HB_ET_DATE &&
              pRight->ExprType == HB_ET_TIMESTAMP ) )
   {
      pSelf->value.asLogical = pLeft->value.asDate.lDate <= pRight->value.asDate.lDate;
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType  = HB_EV_LOGICAL;
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
   }
   /* TODO: add checking of incompatible types
   else
   {
   }
   */
   return pSelf;
}

HB_EXPR_PTR hb_compExprReduceGT( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

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
               HB_BOOL bResult = ( pLeft->value.asLogical && ! pRight->value.asLogical );
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

         case HB_ET_NUMERIC:
            {
               HB_BOOL bResult;

               switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
               {
                  case HB_ET_LONG:
                     bResult = ( pLeft->value.asNum.val.l > pRight->value.asNum.val.l );
                     break;
                  case HB_ET_DOUBLE:
                     bResult = ( pLeft->value.asNum.val.d > pRight->value.asNum.val.d );
                     break;
                  default:
                     {
                        if( pLeft->value.asNum.NumType == HB_ET_LONG )
                           bResult = ( pLeft->value.asNum.val.l > pRight->value.asNum.val.d );
                        else
                           bResult = ( pLeft->value.asNum.val.d > pRight->value.asNum.val.l );
                     }
                     break;
               }
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

         case HB_ET_DATE:
         case HB_ET_TIMESTAMP:
            {
               HB_BOOL bResult = ( pLeft->value.asDate.lDate > pRight->value.asDate.lDate ) ||
                                 ( pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                                   pLeft->value.asDate.lTime > pRight->value.asDate.lTime );
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

      }
   else if( ( pLeft->ExprType == HB_ET_TIMESTAMP &&
              pRight->ExprType == HB_ET_DATE ) ||
            ( pLeft->ExprType == HB_ET_DATE &&
              pRight->ExprType == HB_ET_TIMESTAMP ) )
   {
      pSelf->value.asLogical = pLeft->value.asDate.lDate > pRight->value.asDate.lDate;
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType  = HB_EV_LOGICAL;
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
   }
   /* TODO: add checking of incompatible types
   else
   {
   }
   */
   return pSelf;
}

HB_EXPR_PTR hb_compExprReduceLT( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

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
               HB_BOOL bResult = ( ! pLeft->value.asLogical && pRight->value.asLogical );
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

         case HB_ET_NUMERIC:
            {
               HB_BOOL bResult;

               switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
               {
                  case HB_ET_LONG:
                     bResult = ( pLeft->value.asNum.val.l < pRight->value.asNum.val.l );
                     break;
                  case HB_ET_DOUBLE:
                     bResult = ( pLeft->value.asNum.val.d < pRight->value.asNum.val.d );
                     break;
                  default:
                     {
                        if( pLeft->value.asNum.NumType == HB_ET_LONG )
                           bResult = ( pLeft->value.asNum.val.l < pRight->value.asNum.val.d );
                        else
                           bResult = ( pLeft->value.asNum.val.d < pRight->value.asNum.val.l );
                     }
                     break;
               }
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

         case HB_ET_DATE:
         case HB_ET_TIMESTAMP:
            {
               HB_BOOL bResult = ( pLeft->value.asDate.lDate < pRight->value.asDate.lDate ) ||
                                 ( pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                                   pLeft->value.asDate.lTime < pRight->value.asDate.lTime );
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

      }
   else if( ( pLeft->ExprType == HB_ET_TIMESTAMP &&
              pRight->ExprType == HB_ET_DATE ) ||
            ( pLeft->ExprType == HB_ET_DATE &&
              pRight->ExprType == HB_ET_TIMESTAMP ) )
   {
      pSelf->value.asLogical = pLeft->value.asDate.lDate < pRight->value.asDate.lDate;
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType  = HB_EV_LOGICAL;
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
   }
   /* TODO: add checking of incompatible types
   else
   {
   }
   */
   return pSelf;
}

HB_EXPR_PTR hb_compExprReduceEQ( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == pRight->ExprType )
   {
      switch( pLeft->ExprType )
      {
         case HB_ET_LOGICAL:
         {
            HB_BOOL bResult = ( pLeft->value.asLogical == pRight->value.asLogical );
            HB_COMP_EXPR_FREE( pLeft );
            HB_COMP_EXPR_FREE( pRight );
            pSelf->ExprType = HB_ET_LOGICAL;
            pSelf->ValType  = HB_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
            break;
         }

         case HB_ET_STRING:
            /* NOTE: when not exact comparison (==) is used
             * the result depends on SET EXACT setting then it
             * cannot be optimized except the case when NULL string are
             * compared - "" = "" is always TRUE regardless of EXACT
             * setting.
             * If macro substitiution is not didabled (-kM compiler
             * switch) then we cannot reduce also strings which
             * have macro operator '&'
             */
            if( ( pLeft->nLength | pRight->nLength ) == 0 ||
                ( pSelf->ExprType == HB_EO_EQ &&
                  ( !HB_SUPPORT_MACROTEXT ||
                    ( !hb_compExprHasMacro( pLeft->value.asString.string,
                                            pLeft->nLength, HB_COMP_PARAM ) &&
                      !hb_compExprHasMacro( pRight->value.asString.string,
                                            pRight->nLength, HB_COMP_PARAM ) ) ) ) )
            {
               HB_BOOL bResult = pLeft->nLength == pRight->nLength &&
                                 memcmp( pLeft->value.asString.string,
                                         pRight->value.asString.string,
                                         pLeft->nLength ) == 0;
               HB_COMP_EXPR_FREE( pLeft );
               HB_COMP_EXPR_FREE( pRight );
               pSelf->ExprType = HB_ET_LOGICAL;
               pSelf->ValType  = HB_EV_LOGICAL;
               pSelf->value.asLogical = bResult;
            }
            break;

         case HB_ET_NUMERIC:
         {
            HB_BOOL bResult;

            switch( pLeft->value.asNum.NumType & pRight->value.asNum.NumType )
            {
               case HB_ET_LONG:
                  bResult = ( pLeft->value.asNum.val.l == pRight->value.asNum.val.l );
                  break;
               case HB_ET_DOUBLE:
                  bResult = ( pLeft->value.asNum.val.d == pRight->value.asNum.val.d );
                  break;
               default:
                  if( pLeft->value.asNum.NumType == HB_ET_LONG )
                     bResult = ( pLeft->value.asNum.val.l == pRight->value.asNum.val.d );
                  else
                     bResult = ( pLeft->value.asNum.val.d == pRight->value.asNum.val.l );
                  break;
            }
            HB_COMP_EXPR_FREE( pLeft );
            HB_COMP_EXPR_FREE( pRight );
            pSelf->ExprType = HB_ET_LOGICAL;
            pSelf->ValType  = HB_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
            break;
         }

         case HB_ET_DATE:
         case HB_ET_TIMESTAMP:
         {
            HB_BOOL bResult = ( pLeft->value.asDate.lDate == pRight->value.asDate.lDate ) &&
                              ( pLeft->value.asDate.lTime == pRight->value.asDate.lTime );
            HB_COMP_EXPR_FREE( pLeft );
            HB_COMP_EXPR_FREE( pRight );
            pSelf->ExprType = HB_ET_LOGICAL;
            pSelf->ValType  = HB_EV_LOGICAL;
            pSelf->value.asLogical = bResult;
            break;
         }

         case HB_ET_NIL:
            HB_COMP_EXPR_FREE( pLeft );
            HB_COMP_EXPR_FREE( pRight );
            pSelf->ExprType = HB_ET_LOGICAL;
            pSelf->ValType  = HB_EV_LOGICAL;
            pSelf->value.asLogical = HB_TRUE;
            break;
      }
   }
   else if( ( pLeft->ExprType == HB_ET_TIMESTAMP &&
              pRight->ExprType == HB_ET_DATE ) ||
            ( pLeft->ExprType == HB_ET_DATE &&
              pRight->ExprType == HB_ET_TIMESTAMP ) )
   {
      pSelf->value.asLogical = pLeft->value.asDate.lDate == pRight->value.asDate.lDate &&
                               ( pLeft->value.asDate.lTime == pRight->value.asDate.lTime ||
                                 pSelf->ExprType != HB_EO_EQ );
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType  = HB_EV_LOGICAL;
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
   }
   else if( ( pLeft->ExprType == HB_ET_NIL &&
              ( pRight->ExprType == HB_ET_NUMERIC ||
                pRight->ExprType == HB_ET_LOGICAL ||
                pRight->ExprType == HB_ET_DATE ||
                pRight->ExprType == HB_ET_TIMESTAMP ||
                pRight->ExprType == HB_ET_STRING ||
                pRight->ExprType == HB_ET_CODEBLOCK ||
                pRight->ExprType == HB_ET_ARRAY ||
                pRight->ExprType == HB_ET_HASH ||
                pRight->ExprType == HB_ET_FUNREF ) ) ||
            ( pRight->ExprType == HB_ET_NIL &&
              ( pLeft->ExprType == HB_ET_NUMERIC ||
                pLeft->ExprType == HB_ET_LOGICAL ||
                pLeft->ExprType == HB_ET_DATE ||
                pLeft->ExprType == HB_ET_TIMESTAMP ||
                pLeft->ExprType == HB_ET_STRING ||
                pLeft->ExprType == HB_ET_CODEBLOCK ||
                pLeft->ExprType == HB_ET_ARRAY ||
                pLeft->ExprType == HB_ET_HASH ||
                pLeft->ExprType == HB_ET_FUNREF ) ) )
   {
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType  = HB_EV_LOGICAL;
      pSelf->value.asLogical = HB_FALSE;
   }
   /* TODO: add checking of incompatible types
   else
   {
   }
   */
   return pSelf;
}

HB_EXPR_PTR hb_compExprReduceAnd( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == HB_ET_LOGICAL && pRight->ExprType == HB_ET_LOGICAL )
   {
      HB_BOOL bResult;

      bResult = pLeft->value.asLogical && pRight->value.asLogical;
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType  = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
   }
   else if( pLeft->ExprType == HB_ET_LOGICAL &&
            HB_COMP_ISSUPPORTED( HB_COMPFLAG_SHORTCUTS ) )
   {
      if( pLeft->value.asLogical )
      {
         /* .T. .AND. expr => expr
          */
         HB_COMP_EXPR_FREE( pLeft );
         pSelf->ExprType = HB_ET_NONE;    /* don't delete expression components */
         HB_COMP_EXPR_FREE( pSelf );
         pSelf = pRight;
      }
      else
      {
         /* .F. .AND. expr => .F.
          */
         HB_COMP_EXPR_FREE( pLeft );
         HB_COMP_EXPR_FREE( pRight );         /* discard expression */
         pSelf->ExprType = HB_ET_LOGICAL;
         pSelf->ValType  = HB_EV_LOGICAL;
         pSelf->value.asLogical = HB_FALSE;
      }
   }
   else if( pRight->ExprType == HB_ET_LOGICAL &&
            HB_COMP_ISSUPPORTED( HB_COMPFLAG_SHORTCUTS ) &&
            ( HB_COMP_PARAM->mode == HB_MODE_COMPILER || HB_SUPPORT_HARBOUR ) )
   {
      if( pRight->value.asLogical )
      {
         /* expr .AND. .T. => expr
          */
         HB_COMP_EXPR_FREE( pRight );
         pSelf->ExprType = HB_ET_NONE;    /* don't delete expression components */
         HB_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft;
      }
      else
      {
         /* expr .AND. .F. => .F.
          */
         HB_COMP_EXPR_FREE( pLeft );      /* discard expression */
         HB_COMP_EXPR_FREE( pRight );
         pSelf->ExprType = HB_ET_LOGICAL;
         pSelf->ValType  = HB_EV_LOGICAL;
         pSelf->value.asLogical = HB_FALSE;
      }
   }
   return pSelf;
}

HB_EXPR_PTR hb_compExprReduceOr( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pLeft, pRight;

   pLeft  = pSelf->value.asOperator.pLeft;
   pRight = pSelf->value.asOperator.pRight;

   if( pLeft->ExprType == HB_ET_LOGICAL && pRight->ExprType == HB_ET_LOGICAL )
   {
      HB_BOOL bResult;

      bResult = pLeft->value.asLogical || pRight->value.asLogical;
      HB_COMP_EXPR_FREE( pLeft );
      HB_COMP_EXPR_FREE( pRight );
      pSelf->ExprType = HB_ET_LOGICAL;
      pSelf->ValType  = HB_EV_LOGICAL;
      pSelf->value.asLogical = bResult;
   }
   else if( pLeft->ExprType == HB_ET_LOGICAL &&
            HB_COMP_ISSUPPORTED( HB_COMPFLAG_SHORTCUTS ) )
   {
      if( pLeft->value.asLogical )
      {
         /* .T. .OR. expr => .T.
          */
         HB_COMP_EXPR_FREE( pLeft );
         HB_COMP_EXPR_FREE( pRight );     /* discard expression */
         pSelf->ExprType = HB_ET_LOGICAL;
         pSelf->ValType  = HB_EV_LOGICAL;
         pSelf->value.asLogical = HB_TRUE;
      }
      else
      {
         /* .F. .OR. expr => expr
          */
         HB_COMP_EXPR_FREE( pLeft );
         pSelf->ExprType = HB_ET_NONE;    /* don't delete expression components */
         HB_COMP_EXPR_FREE( pSelf );
         pSelf = pRight;
      }
   }
   else if( pRight->ExprType == HB_ET_LOGICAL &&
            HB_COMP_ISSUPPORTED( HB_COMPFLAG_SHORTCUTS ) &&
            ( HB_COMP_PARAM->mode == HB_MODE_COMPILER || HB_SUPPORT_HARBOUR ) )
   {
      if( pRight->value.asLogical )
      {
         /* expr .OR. .T. => .T.
          */
         HB_COMP_EXPR_FREE( pLeft );      /* discard expression */
         HB_COMP_EXPR_FREE( pRight );
         pSelf->ExprType = HB_ET_LOGICAL;
         pSelf->ValType  = HB_EV_LOGICAL;
         pSelf->value.asLogical = HB_TRUE;
      }
      else
      {
         /* expr .OR. .F. => expr
          */
         HB_COMP_EXPR_FREE( pRight );
         pSelf->ExprType = HB_ET_NONE;    /* don't delete expression components */
         HB_COMP_EXPR_FREE( pSelf );
         pSelf = pLeft;
      }
   }
   return pSelf;
}

HB_EXPR_PTR hb_compExprReduceIIF( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pExpr;

   /* get conditional expression */
   pExpr = pSelf->value.asList.pExprList;

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
         HB_COMP_EXPR_FREE( pSelf->value.asList.pExprList );
         /* assign NULL to a start of expressions list to suppress
          * deletion of expression's components - we are deleting them
          * here
          */
         pSelf->value.asList.pExprList = NULL;
         HB_COMP_EXPR_FREE( pSelf );
         /* store the TRUE expression as a result of reduction
          */
         pSelf = pExpr;
         pExpr = pExpr->pNext;     /* skip to HB_FALSE expression */
         HB_COMP_EXPR_FREE( pExpr );      /* delete HB_FALSE expr */
         pSelf->pNext = NULL;
      }
      else
      {
         /* .F. was specified
         */
         pExpr = pExpr->pNext;   /* skip to TRUE expression */
         /* delete condition  - it is no longer needed
          */
         HB_COMP_EXPR_FREE( pSelf->value.asList.pExprList );
         /* assign NULL to a start of expressions list to suppress
          * deletion of expression's components - we are deleting them
          * here
          */
         pSelf->value.asList.pExprList = NULL;
         HB_COMP_EXPR_FREE( pSelf );
         /* store the HB_FALSE expression as a result of reduction
            */
         pSelf = pExpr->pNext;
         HB_COMP_EXPR_FREE( pExpr );      /* delete TRUE expr */
         pSelf->pNext = NULL;
      }

      /* this will cause warning when IIF is used as statement */
      /*
      if( pSelf->ExprType == HB_ET_NONE )
      {
         pSelf->ExprType = HB_ET_NIL;
         pSelf->ValType = HB_EV_NIL;
      }
      */
   }
   /* check if valid expression is passed
   */
   else if( pExpr->ExprType == HB_ET_NIL ||
            pExpr->ExprType == HB_ET_NUMERIC ||
            pExpr->ExprType == HB_ET_DATE ||
            pExpr->ExprType == HB_ET_TIMESTAMP ||
            pExpr->ExprType == HB_ET_STRING ||
            pExpr->ExprType == HB_ET_CODEBLOCK ||
            pExpr->ExprType == HB_ET_ARRAY ||
            pExpr->ExprType == HB_ET_HASH ||
            pExpr->ExprType == HB_ET_VARREF ||
            pExpr->ExprType == HB_ET_REFERENCE ||
            pExpr->ExprType == HB_ET_FUNREF )
   {
      HB_COMP_ERROR_TYPE( pExpr );
   }
   return pSelf;
}

/* replace the list containing a single expression with a simple expression
 * - strips parenthesis
 *  ( EXPR ) -> EXPR
 */
HB_EXPR_PTR hb_compExprListStrip( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   while( pSelf->ExprType == HB_ET_LIST &&
          pSelf->value.asList.pExprList->ExprType <= HB_ET_VARIABLE &&
          hb_compExprListLen( pSelf ) == 1 )
   {
      /* replace the list with a simple expression
       *  ( EXPR ) -> EXPR
       */
      HB_EXPR_PTR pExpr = pSelf;

      pSelf = pSelf->value.asList.pExprList;
      pExpr->value.asList.pExprList = NULL;
      HB_COMP_EXPR_FREE( pExpr );
   }

   return pSelf;
}

HB_BOOL hb_compExprReduceAT( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pSub  = pParms->value.asList.pExprList;
   HB_EXPR_PTR pText = pSub->pNext;
   HB_EXPR_PTR pReduced;

   if( pSub->ExprType == HB_ET_STRING && pText->ExprType == HB_ET_STRING &&
       !HB_SUPPORT_USERCP )
   {
      /* NOTE: CA-Cl*pper has a bug in AT("",cText) compile time
       *       optimization and always set 1 as result in such cses.
       *       This bug exist only in compiler and CA-Cl*pper macro
       *       compiler does not have optimizer. This bug is replicated
       *       by us only when Harbour extensions in compiler (-kh) are
       *       not enabled f.e. in strict Clipper cmpatible mode (-kc)
       *       [druzus]
       */
      if( pSub->nLength == 0 )
      {
         pReduced = hb_compExprNewLong( ( HB_COMP_PARAM->mode == HB_MODE_COMPILER &&
                                          ! HB_SUPPORT_HARBOUR ) ? 1 : 0, HB_COMP_PARAM );
      }
      else
      {
         pReduced = hb_compExprNewLong( hb_strAt( pSub->value.asString.string,
                               pSub->nLength, pText->value.asString.string,
                               pText->nLength ), HB_COMP_PARAM );
      }

      HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pParms );

      memcpy( pSelf, pReduced, sizeof( HB_EXPR ) );
      HB_COMP_EXPR_CLEAR( pReduced );
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

HB_BOOL hb_compExprReduceCHR( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_BOOL fDoOpt = HB_FALSE;
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == HB_ET_NUMERIC )
   {
      if( HB_SUPPORT_USERCP )
      {
         int iVal = pArg->value.asNum.NumType == HB_ET_LONG ?
                    ( int ) pArg->value.asNum.val.l :
                    ( int ) pArg->value.asNum.val.d;
         fDoOpt = iVal >= 0 && iVal <= 127;
      }
      else
         fDoOpt = HB_TRUE;
   }

   /* try to change it into a string */
   if( fDoOpt )
   {
      /* NOTE: CA-Cl*pper's compiler optimizer will be wrong for those
       *       CHR() cases where the passed parameter is a constant which
       *       can be divided by 256 but it's not zero, in this case it
       *       will return an empty string instead of a Chr(0). [vszakats]
       *
       *       But this bug exist only in compiler and CA-Cl*pper macro
       *       compiler does not have optimizer. This bug is replicated
       *       by us only when Harbour extensions in compiler (-kh) are
       *       not enabled f.e. in strict Clipper cmpatible mode (-kc)
       *       [druzus]
       */

      HB_EXPR_PTR pExpr = HB_COMP_EXPR_NEW( HB_ET_STRING );

      pExpr->ValType = HB_EV_STRING;
      if( pArg->value.asNum.NumType == HB_ET_LONG )
      {
         if( HB_COMP_PARAM->mode == HB_MODE_COMPILER &&
             ! HB_SUPPORT_HARBOUR &&
             ( pArg->value.asNum.val.l & 0xff ) == 0 &&
               pArg->value.asNum.val.l != 0 )
         {
            pExpr->value.asString.string = ( char * ) "";
            pExpr->value.asString.dealloc = HB_FALSE;
            pExpr->nLength = 0;
         }
         else
         {
            pExpr->value.asString.string = ( char * ) hb_szAscii[ ( int ) pArg->value.asNum.val.l & 0xff ];
            pExpr->value.asString.dealloc = HB_FALSE;
            pExpr->nLength = 1;
         }
      }
      else
      {
         pExpr->value.asString.string = ( char * ) hb_szAscii[ ( unsigned int ) pArg->value.asNum.val.d & 0xff ];
         pExpr->value.asString.dealloc = HB_FALSE;
         pExpr->nLength = 1;
      }

      HB_COMP_EXPR_FREE( pParms );
      HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( HB_EXPR ) );
      HB_COMP_EXPR_CLEAR( pExpr );
      return HB_TRUE;
   }

   return HB_FALSE;
}

HB_BOOL hb_compExprReduceLEN( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pArg = pParms->value.asList.pExprList;

   /* TOFIX: do not optimize when array/hash args have user expressions */
   if( ( pArg->ExprType == HB_ET_STRING && !HB_SUPPORT_USERCP ) ||
       pArg->ExprType == HB_ET_ARRAY ||
       pArg->ExprType == HB_ET_HASH )
   {
      HB_EXPR_PTR pExpr = hb_compExprNewLong( pArg->ExprType == HB_ET_HASH ?
                        pArg->nLength >> 1 : pArg->nLength, HB_COMP_PARAM );

      HB_COMP_EXPR_FREE( pParms );
      HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( HB_EXPR ) );
      HB_COMP_EXPR_CLEAR( pExpr );
      return HB_TRUE;
   }
   return HB_FALSE;
}

HB_BOOL hb_compExprReduceEMPTY( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pArg = pParms->value.asList.pExprList;
   HB_BOOL fReduced = HB_TRUE, fResult = HB_FALSE;

   switch( pArg->ExprType )
   {
      case HB_ET_STRING:
         fResult = hb_strEmpty( pArg->value.asString.string, pArg->nLength );
         break;

      case HB_ET_ARRAY:
      case HB_ET_HASH:
         /* TOFIX: do not optimize when array/hash args have user expressions */
         fResult = pArg->nLength == 0;
         break;

      case HB_ET_NUMERIC:
         if( pArg->value.asNum.NumType == HB_ET_DOUBLE )
            fResult = pArg->value.asNum.val.d == 0.0;
         else
            fResult = pArg->value.asNum.val.l == 0;
         break;

      case HB_ET_LOGICAL:
         fResult = !pArg->value.asLogical;
         break;

      case HB_ET_NIL:
         fResult = HB_TRUE;
         break;

      case HB_ET_DATE:
         fResult = pArg->value.asDate.lDate == 0;
         break;

      case HB_ET_TIMESTAMP:
         fResult = pArg->value.asDate.lDate == 0 &&
                   pArg->value.asDate.lTime == 0;
         break;

      case HB_ET_CODEBLOCK:
         break;

      /* case HB_ET_FUNREF: */
      default:
         fReduced = HB_FALSE;
   }

   if( fReduced )
   {
      HB_EXPR_PTR pExpr = hb_compExprNewLogical( fResult, HB_COMP_PARAM );

      HB_COMP_EXPR_FREE( pParms );
      HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( HB_EXPR ) );
      HB_COMP_EXPR_CLEAR( pExpr );
      return HB_TRUE;
   }
   return HB_FALSE;
}

HB_BOOL hb_compExprReduceASC( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == HB_ET_STRING &&
       ( !HB_SUPPORT_USERCP ||
         ( HB_UCHAR ) pArg->value.asString.string[0] <= 127 ) )
   {
      HB_EXPR_PTR pExpr = hb_compExprNewLong(
                ( HB_UCHAR ) pArg->value.asString.string[0], HB_COMP_PARAM );

      HB_COMP_EXPR_FREE( pParms );
      HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( HB_EXPR ) );
      HB_COMP_EXPR_CLEAR( pExpr );
      return HB_TRUE;
   }
   return HB_FALSE;
}

HB_BOOL hb_compExprReduceINT( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == HB_ET_NUMERIC )
   {
      HB_EXPR_PTR pExpr;

      if( pArg->value.asNum.NumType == HB_ET_LONG )
         pExpr = hb_compExprNewLong( pArg->value.asNum.val.l, HB_COMP_PARAM );
      else
      {
         HB_MAXDBL dVal = ( HB_MAXDBL ) pArg->value.asNum.val.d;
         if( HB_DBL_LIM_LONG( dVal ) )
            pExpr = hb_compExprNewLong( ( HB_MAXINT ) pArg->value.asNum.val.d, HB_COMP_PARAM );
         else
            pExpr = hb_compExprNewDouble( pArg->value.asNum.val.d,
                                          pArg->value.asNum.bWidth, 0,
                                          HB_COMP_PARAM );
      }
      HB_COMP_EXPR_FREE( pParms );
      HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( HB_EXPR ) );
      HB_COMP_EXPR_CLEAR( pExpr );
      return HB_TRUE;
   }
   return HB_FALSE;
}

HB_BOOL hb_compExprReduceSTOT( HB_EXPR_PTR pSelf, HB_USHORT usCount, HB_COMP_DECL )
{
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pArg = pParms ? pParms->value.asList.pExprList : NULL;
   HB_EXPR_PTR pExpr = NULL;

   if( usCount == 0 )
   {
      pExpr = hb_compExprNewTimeStamp( 0, 0, HB_COMP_PARAM );
   }
   else if( pArg && pArg->ExprType == HB_ET_STRING )
   {
      long lDate, lTime;

      hb_timeStampStrRawGet( pArg->value.asString.string, &lDate, &lTime );
      pExpr = hb_compExprNewTimeStamp( lDate, lTime, HB_COMP_PARAM );
   }

   if( pExpr )
   {
      if( pSelf->value.asFunCall.pParms )
         HB_COMP_EXPR_FREE( pParms );
      HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( HB_EXPR ) );
      HB_COMP_EXPR_CLEAR( pExpr );
      return HB_TRUE;
   }

   return HB_FALSE;
}

HB_BOOL hb_compExprReduceSTOD( HB_EXPR_PTR pSelf, HB_USHORT usCount, HB_COMP_DECL )
{
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pArg = pParms ? pParms->value.asList.pExprList : NULL;
   HB_EXPR_PTR pExpr = NULL;

   if( usCount == 0 )
   {
      pExpr = hb_compExprNewDate( 0, HB_COMP_PARAM );
   }
   else if( pArg && pArg->ExprType == HB_ET_STRING &&
            ( pArg->nLength >= 7 || pArg->nLength == 0 ) )
   {
      pExpr = hb_compExprNewDate( pArg->nLength == 0 ? 0 :
                                  hb_dateEncStr( pArg->value.asString.string ),
                                  HB_COMP_PARAM );
   }

   if( pExpr )
   {
      if( pSelf->value.asFunCall.pParms )
         HB_COMP_EXPR_FREE( pParms );
      HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( HB_EXPR ) );
      HB_COMP_EXPR_CLEAR( pExpr );
      return HB_TRUE;
   }

   return HB_FALSE;
}

HB_BOOL hb_compExprReduceDTOS( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == HB_ET_DATE || pArg->ExprType == HB_ET_TIMESTAMP )
   {
      char szBuffer[ 9 ], * szDate;
      HB_EXPR_PTR pExpr;

      szDate = ( char * ) memcpy( hb_xgrab( 9 ),
            hb_dateDecStr( szBuffer, ( long ) pArg->value.asDate.lDate ), 9 );
      pExpr = hb_compExprNewString( szDate, 8, HB_TRUE, HB_COMP_PARAM );

      HB_COMP_EXPR_FREE( pParms );
      HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( HB_EXPR ) );
      HB_COMP_EXPR_CLEAR( pExpr );
      return HB_TRUE;
   }

   return HB_FALSE;
}

HB_BOOL hb_compExprReduceCTOD( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == HB_ET_STRING && pArg->nLength == 0 )
   {
      HB_EXPR_PTR pExpr = hb_compExprNewDate( 0, HB_COMP_PARAM );

      HB_COMP_EXPR_FREE( pParms );
      HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( HB_EXPR ) );
      HB_COMP_EXPR_CLEAR( pExpr );
      return HB_TRUE;
   }

   return HB_FALSE;
}

HB_BOOL hb_compExprReduceUPPER( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pArg = pParms->value.asList.pExprList;

   if( pArg->ExprType == HB_ET_STRING )
   {
      HB_SIZE nLen = pArg->nLength;
      HB_BOOL fLower = HB_FALSE;

      if( nLen )
      {
         const char * szValue = pArg->value.asString.string;
         do
         {
            char c = * szValue++;
            if( c >= 'a' && c <= 'z' )
               fLower = HB_TRUE;
            else if( !( ( c >= 'A' && c <= 'Z' ) ||
                        ( c >= '0' && c <= '9' ) || c == ' ' ) )
               break;
         }
         while( --nLen );
      }

      if( nLen == 0 )
      {
         HB_EXPR_PTR pExpr;
         char * szValue;
         HB_BOOL fDealloc;

         if( fLower )
         {
            if( pArg->nLength == 1 )
            {
               szValue = ( char * ) hb_szAscii[ HB_TOUPPER( ( unsigned char )
                                          pArg->value.asString.string[ 0 ] ) ];
               fDealloc = HB_FALSE;
            }
            else
            {
               if( pArg->value.asString.dealloc )
               {
                  szValue = pArg->value.asString.string;
                  pArg->value.asString.dealloc = HB_FALSE;
                  fDealloc = HB_TRUE;
               }
               else
               {
                  szValue = ( char * ) hb_xgrab( pArg->nLength + 1 );
                  memcpy( szValue, pArg->value.asString.string, pArg->nLength + 1 );
                  fDealloc = HB_TRUE;
               }
               do
                  szValue[ nLen ] = ( char ) HB_TOUPPER( ( unsigned char ) szValue[ nLen ] );
               while( ++nLen < pArg->nLength );
            }
         }
         else
         {
            szValue = pArg->value.asString.string;
            fDealloc = pArg->value.asString.dealloc;
            pArg->value.asString.dealloc = HB_FALSE;
         }

         pExpr = HB_COMP_EXPR_NEW( HB_ET_STRING );
         pExpr->ValType = HB_EV_STRING;
         pExpr->value.asString.string = szValue;
         pExpr->value.asString.dealloc = fDealloc;
         pExpr->nLength = pArg->nLength;

         HB_COMP_EXPR_FREE( pParms );
         HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
         memcpy( pSelf, pExpr, sizeof( HB_EXPR ) );
         HB_COMP_EXPR_CLEAR( pExpr );

         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

HB_BOOL hb_compExprReduceMIN( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pFirst = pParms->value.asList.pExprList;
   HB_EXPR_PTR pNext = pFirst->pNext;
   HB_EXPR_PTR pExpr = NULL;

   if( pFirst->ExprType == pNext->ExprType )
   {

      if( pFirst->ExprType == HB_ET_NUMERIC )
      {
         HB_BYTE bType = ( pFirst->value.asNum.NumType & pNext->value.asNum.NumType );

         switch( bType )
         {
            case HB_ET_LONG:
               pExpr = pFirst->value.asNum.val.l <= pNext->value.asNum.val.l ?
                       pFirst : pNext;
               break;

            case HB_ET_DOUBLE:
               pExpr = pFirst->value.asNum.val.d <= pNext->value.asNum.val.d ?
                       pFirst : pNext;
               break;

            default:
               if( pFirst->value.asNum.NumType == HB_ET_DOUBLE )
                  pExpr = ( pFirst->value.asNum.val.d <= ( double ) pNext->value.asNum.val.l ) ?
                          pFirst : pNext;
               else
                  pExpr = ( ( double ) pFirst->value.asNum.val.l <= pNext->value.asNum.val.d ) ?
                          pFirst : pNext;
         }
      }
      else if( pFirst->ExprType == HB_ET_DATE )
      {
         pExpr = pFirst->value.asDate.lDate <= pNext->value.asDate.lDate ?
                 pFirst : pNext;
      }
      else if( pFirst->ExprType == HB_ET_TIMESTAMP )
      {
         pExpr = ( pFirst->value.asDate.lDate < pNext->value.asDate.lDate ||
                   ( pFirst->value.asDate.lDate == pNext->value.asDate.lDate &&
                     pFirst->value.asDate.lTime <= pNext->value.asDate.lTime ) ) ?
                 pFirst : pNext;
      }
      else if( pFirst->ExprType == HB_ET_LOGICAL )
      {
         pExpr = !pFirst->value.asLogical ? pFirst : pNext;
      }
   }
   else if( pFirst->ExprType == HB_ET_DATE && pNext->ExprType == HB_ET_TIMESTAMP )
   {
      pExpr = pFirst->value.asDate.lDate <= pNext->value.asDate.lDate ?
              pFirst : pNext;
   }
   else if( pFirst->ExprType == HB_ET_TIMESTAMP && pNext->ExprType == HB_ET_DATE )
   {
      pExpr = pFirst->value.asDate.lDate < pNext->value.asDate.lDate ?
              pFirst : pNext;
   }

   if( pExpr )
   {
      HB_EXPR_PTR * pExprPtr = &pParms->value.asList.pExprList;

      while( *pExprPtr )
      {
         if( *pExprPtr == pExpr )
         {
            *pExprPtr = pExpr->pNext;
            break;
         }
         pExprPtr = &( *pExprPtr )->pNext;
      }
      HB_COMP_EXPR_FREE( pParms );
      HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( HB_EXPR ) );
      HB_COMP_EXPR_CLEAR( pExpr );
      return HB_TRUE;
   }

   return HB_FALSE;
}

HB_BOOL hb_compExprReduceMAX( HB_EXPR_PTR pSelf, HB_COMP_DECL )
{
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pFirst = pParms->value.asList.pExprList;
   HB_EXPR_PTR pNext = pFirst->pNext;
   HB_EXPR_PTR pExpr = NULL;

   if( pFirst->ExprType == pNext->ExprType )
   {

      if( pFirst->ExprType == HB_ET_NUMERIC )
      {
         HB_BYTE bType = ( pFirst->value.asNum.NumType & pNext->value.asNum.NumType );

         switch( bType )
         {
            case HB_ET_LONG:
               pExpr = pFirst->value.asNum.val.l >= pNext->value.asNum.val.l ?
                       pFirst : pNext;
               break;

            case HB_ET_DOUBLE:
               pExpr = pFirst->value.asNum.val.d >= pNext->value.asNum.val.d ?
                       pFirst : pNext;
               break;

            default:
               if( pFirst->value.asNum.NumType == HB_ET_DOUBLE )
                  pExpr = ( pFirst->value.asNum.val.d >= ( double ) pNext->value.asNum.val.l ) ?
                          pFirst : pNext;
               else
                  pExpr = ( ( double ) pFirst->value.asNum.val.l >= pNext->value.asNum.val.d ) ?
                          pFirst : pNext;
         }
      }
      else if( pFirst->ExprType == HB_ET_DATE )
      {
         pExpr = pFirst->value.asDate.lDate >= pNext->value.asDate.lDate ?
                 pFirst : pNext;
      }
      else if( pFirst->ExprType == HB_ET_TIMESTAMP )
      {
         pExpr = ( pFirst->value.asDate.lDate > pNext->value.asDate.lDate ||
                   ( pFirst->value.asDate.lDate == pNext->value.asDate.lDate &&
                     pFirst->value.asDate.lTime >= pNext->value.asDate.lTime ) ) ?
                 pFirst : pNext;
      }
      else if( pFirst->ExprType == HB_ET_LOGICAL )
      {
         pExpr = pFirst->value.asLogical ? pFirst : pNext;
      }

   }
   else if( pFirst->ExprType == HB_ET_DATE && pNext->ExprType == HB_ET_TIMESTAMP )
   {
      pExpr = pFirst->value.asDate.lDate >= pNext->value.asDate.lDate ?
              pFirst : pNext;
   }
   else if( pFirst->ExprType == HB_ET_TIMESTAMP && pNext->ExprType == HB_ET_DATE )
   {
      pExpr = pFirst->value.asDate.lDate > pNext->value.asDate.lDate ?
              pFirst : pNext;
   }

   if( pExpr )
   {
      HB_EXPR_PTR * pExprPtr = &pParms->value.asList.pExprList;

      while( * pExprPtr )
      {
         if( * pExprPtr == pExpr )
         {
            * pExprPtr = pExpr->pNext;
            break;
         }
         pExprPtr = &( *pExprPtr )->pNext;
      }
      HB_COMP_EXPR_FREE( pParms );
      HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
      memcpy( pSelf, pExpr, sizeof( HB_EXPR ) );
      HB_COMP_EXPR_CLEAR( pExpr );
      return HB_TRUE;
   }

   return HB_FALSE;
}

HB_BOOL hb_compExprReduceBitFunc( HB_EXPR_PTR pSelf, HB_MAXINT nResult, HB_BOOL fBool, HB_COMP_DECL )
{
   HB_EXPR_PTR pParms = pSelf->value.asFunCall.pParms;
   HB_EXPR_PTR pExpr = fBool ? hb_compExprNewLogical( nResult != 0, HB_COMP_PARAM ) :
                               hb_compExprNewLong( nResult, HB_COMP_PARAM );

   HB_COMP_EXPR_FREE( pParms );
   HB_COMP_EXPR_FREE( pSelf->value.asFunCall.pFunName );
   memcpy( pSelf, pExpr, sizeof( HB_EXPR ) );
   HB_COMP_EXPR_CLEAR( pExpr );
   return HB_TRUE;
}
