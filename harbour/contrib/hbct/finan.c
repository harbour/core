/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Financial functions
 *     - PV()
 *     - FV()
 *     - PAYMENT()
 *     - PERIODS()
 *     - RATE()
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

#include "ct.h"
#include "ctmath.h"
#include "hbmather.h"

HB_FUNC( FV )
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      double dPayment = hb_parnd( 1 );
      double dRate = hb_parnd( 2 );
      double dTime = hb_parnd( 3 );
      double dResult;

      if( dRate == 0.0 )
      {
         /* NOTE: CT3 crashes with dRate == 0.0 */
         dResult = dPayment * dTime;
      }
      else
      {
         HB_MATH_EXCEPTION hb_exc;
         double dBase = 1.0 + dRate;

         hb_mathResetError( &hb_exc );
         dResult = pow( dBase, dTime );

         if( hb_mathGetError( &hb_exc, "POW", dBase, dTime, dResult ) )
            dResult = hb_exc.handled ? hb_exc.retval : 0.0;

         dResult = dPayment * ( dResult - 1.0 ) / dRate;
      }

      hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_FV, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( PV )
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      double dPayment = hb_parnd( 1 );
      double dRate = hb_parnd( 2 );
      double dTime = hb_parnd( 3 );
      double dResult;

      if( dRate == 0.0 )
      {
         /* NOTE: CT3 crashes with dRate == 0.0 */
         dResult = dPayment * dTime;
      }
      else
      {
         HB_MATH_EXCEPTION hb_exc;
         double dBase = 1.0 + dRate;

         hb_mathResetError( &hb_exc );
         dResult = pow( dBase, -dTime );

         if( hb_mathGetError( &hb_exc, "POW", dBase, -dTime, dResult ) )
            dResult = hb_exc.handled ? hb_exc.retval : 0.0;

         dResult = dPayment * ( 1.0 - dResult ) / dRate;
      }

      hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_PV, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( PAYMENT )
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      double dCapital = hb_parnd( 1 );
      double dRate = hb_parnd( 2 );
      double dTime = hb_parnd( 3 );
      double dResult;

      if( dRate == 0.0 )
      {
         /* NOTE: CT3 crashes with dRate == 0.0 */
         dResult = dCapital / dTime;
      }
      else
      {
         HB_MATH_EXCEPTION hb_exc;
         double dBase = 1.0 + dRate;

         hb_mathResetError( &hb_exc );
         dResult = pow( dBase, -dTime );

         if( hb_mathGetError( &hb_exc, "POW", dBase, -dTime, dResult ) )
            dResult = hb_exc.handled ? hb_exc.retval : 0.0;

         dResult = dCapital * dRate / ( 1.0 - dResult );
      }

      hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_PAYMENT, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( PERIODS )
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      double dCapital = hb_parnd( 1 );
      double dPayment = hb_parnd( 2 );
      double dRate = hb_parnd( 3 );
      double dResult;

      if( dPayment <= dCapital * dRate )
      {
         /* in this case infinite time is needed to cancel the loan */
         dResult = -1.0;
      }
      else if( dRate == 0.0 )
      {
         /* NOTE: CT3 crashes with dRate == 0.0 */
         dResult = dCapital / dPayment;
      }
      else
      {
         HB_MATH_EXCEPTION hb_exc;
         double dBase = 1.0 + dRate;

         hb_mathResetError( &hb_exc );
         dResult = log( dBase );
         if( hb_mathGetError( &hb_exc, "LOG", dBase, 0.0, dResult ) )
            dResult = hb_exc.handled ? hb_exc.retval : 0.0;

         if( dResult )
         {
            double dResult2;
            hb_mathResetError( &hb_exc );
            dBase = 1.0 - ( dCapital * dRate / dPayment );
            dResult2 = log( dBase );

            if( hb_mathGetError( &hb_exc, "LOG", dBase, 0.0, dResult2 ) )
               dResult2 = hb_exc.handled ? hb_exc.retval : 0.0;

            dResult = -dResult2 / dResult;
         }
      }

      hb_retnd( dResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_PERIODS, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( RATE )
{
   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      double dCapital = hb_parnd( 1 );
      double dPayment = hb_parnd( 2 );
      double dTime = hb_parnd( 3 );
      double dAux;              /* estimated payment to compare for */
      double dEpsilon = 0.00001;        /* mimimal to consider 2 numbers as equal */
      double dScale = 1.0;      /* fractional step */
      double r;                 /* temptative rate */
      double j = 1.0;           /* index */
      double dExp;

      while( j < 1020.0 )       /* maximum anual rate */
      {
         HB_MATH_EXCEPTION hb_exc;
         double dBase;

         r = j * 0.000833333;   /* j * ( 0.01 / 12.0)  mensual's rate */

         /* replace PAYMENT() function overhead */

         hb_mathResetError( &hb_exc );
         dBase = 1.0 + r;
         dExp = pow( dBase, dTime );
         if( hb_mathGetError( &hb_exc, "POW", dBase, dTime, dExp ) )
         {
            /* TODO: Check if this is a correct default correction value for pow() */
            dExp = hb_exc.handled ? hb_exc.retval : 0.0;
         }

         dAux = dCapital * ( ( dExp * r ) / ( dExp - 1.0 ) );

         if( dAux > dPayment )
         {
            j -= dScale;
            dScale = dScale * 0.10;

            if( ( dAux - dPayment ) < dEpsilon )
               break;
         }
         else
            j += dScale;
      }

      hb_retnd( j * 0.000833333 );      /* return as mensual's rate */
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_RATE, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}
