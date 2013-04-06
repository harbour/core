/*
 * Harbour Project source code:
 *   CT3 trigonometric functions
 *     - Pi()
 *     - Sin()
 *     - Cos()
 *     - Tan()
 *     - Cot()
 *     - Asin()
 *     - Acos()
 *     - Atan()
 *     - Sinh()
 *     - Cosh()
 *     - Tanh()
 *     - Atn2()
 *     - RToD()
 *     - DToR()
 *
 * Copyright 2001 Alejandro de Garate <alex_degarate@hotmail.com>
 *
 * Documentation and changes concerning error handling Copyright 2001
 *   IntTec GmbH, Freiburg, Germany, Author: Martin Vogel <vogel@inttec.de>
 *
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

#include "ct.h"
#include "ctmath.h"
#include "hbmather.h"

HB_FUNC( PI )
{
   hb_retnd( CT_PI );
}

HB_FUNC( SIN )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = sin( dArg );
      if( hb_mathGetError( &hb_exc, "SIN", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retndlen( HUGE_VAL, -1, -1 );
      }
      else
         hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_SIN, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( COS )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = cos( dArg );
      if( hb_mathGetError( &hb_exc, "COS", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retndlen( HUGE_VAL, -1, -1 );
      }
      else
         hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_COS, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( TAN )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = tan( dArg );
      if( hb_mathGetError( &hb_exc, "TAN", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retndlen( HUGE_VAL, -1, -1 );
      }
      else
         hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_TAN, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( COT )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = tan( dArg );
      if( hb_mathGetError( &hb_exc, "TAN", dArg, 0.0, dResult ) )
         dResult = hb_exc.handled ? hb_exc.retval : 0.0;

      dResult = dResult ? 1 / dResult : HUGE_VAL;
      hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_COT, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( ASIN )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = asin( dArg );
      if( hb_mathGetError( &hb_exc, "ASIN", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retndlen( HUGE_VAL, -1, -1 );
      }
      else
         hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_ASIN, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( ACOS )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = acos( dArg );
      if( hb_mathGetError( &hb_exc, "ACOS", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retndlen( HUGE_VAL, -1, -1 );
      }
      else
         hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_ACOS, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( ATAN )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = atan( dArg );
      if( hb_mathGetError( &hb_exc, "ATAN", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
         {
            /* atan normally don't error, but it's save to return Pi()/2
               or -Pi()/2, respectively, as these
               are the boundary result values */
            if( dArg < 0.0 )
               hb_retnd( -CT_PI / 2.0 );
            else
               hb_retnd( CT_PI / 2.0 );
         }
      }
      else
         hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_ATAN, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( ATN2 )
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dY = hb_parnd( 1 );
      double dX = hb_parnd( 2 );
      double dResult;

      hb_mathResetError( &hb_exc );
      dResult = atan2( dY, dX );
      if( hb_mathGetError( &hb_exc, "ATAN2", dY, dX, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
         {
            /* DOMAIN error: both arguments to atan2 have been 0 */
            /* CTIII behaves very strange here: atn2 (0.0, 0.0) == -PI
               atn2 (0.0, -0.0) == 0.0
               atn2 (-0.0, 0.0) == -PI
               atn2 (-0.0, -0.0) == -2*PI */
            if( dX >= 0.0 )
               hb_retnd( -CT_PI );
            else if( dY < 0.0 )
               hb_retnd( -2.0 * CT_PI );
            else
               hb_retnd( 0.0 );
         }
      }
      else
         hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_ATN2, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( SINH )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = sinh( dArg );
      if( hb_mathGetError( &hb_exc, "SINH", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
         {
            /* OVERFLOW error: we have no CTIII behaviour to follow,
               so return +INF or -INF, respectively */
            if( dArg < 0.0 )
               hb_retndlen( -HUGE_VAL, -1, -1 );
            else
               hb_retndlen( HUGE_VAL, -1, -1 );
         }
      }
      else
         hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_SINH, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( COSH )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = cosh( dArg );
      if( hb_mathGetError( &hb_exc, "COSH", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            /* OVERFLOW error: we have no CTIII behaviour to follow,
               so return +INF */
            hb_retndlen( HUGE_VAL, -1, -1 );
      }
      else
         hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_COSH, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( TANH )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = tanh( dArg );
      if( hb_mathGetError( &hb_exc, "TANH", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
         {
            /* normally, Tanh() doesn't give errors, but let's return -1 or +1,
               respectively, as these are the boundary result values */
            if( dArg < 0.0 )
               hb_retnd( -1.0 );
            else
               hb_retnd( 1.0 );
         }
      }
      else
         hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_TANH, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( RTOD )
{
   if( HB_ISNUM( 1 ) )
   {
      double dInput = hb_parnd( 1 );
      double dResult = ( 180.0 / CT_PI ) * dInput;

      hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_RTOD, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( DTOR )
{
   if( HB_ISNUM( 1 ) )
   {
      double dInput = hb_parnd( 1 );
      double dResult = ( CT_PI / 180.0 ) * dInput;

      hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_DTOR, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}
