/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 *   CT3 mathematical functions
 *     - Floor()
 *     - Ceiling()
 *     - Sign()
 *     - Log10()
 *     - Fact()
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

HB_FUNC( FLOOR )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = floor( dArg );
      if( hb_mathGetError( &hb_exc, "FLOOR", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retnlen( 0, 0, 0 );
      }
      else
         hb_retnlen( dResult, 0, 0 );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst =
            ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_FLOOR, NULL, HB_ERR_FUNCNAME, 0,
                            EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( CEILING )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = ceil( dArg );
      if( hb_mathGetError( &hb_exc, "CEIL", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
            hb_retnlen( 0, 0, 0 );
      }
      else
         hb_retnlen( dResult, 0, 0 );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_CEILING, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}

HB_FUNC( SIGN )
{
   if( HB_ISNUM( 1 ) )
   {
      double dInput = hb_parnd( 1 );
      int iResult;

      if( dInput == 0.00 )
         iResult = 0;
      else
      {
         if( dInput > 0.00 )
            iResult = 1;
         else
            iResult = -1;
      }
      hb_retni( iResult );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_SIGN, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retni( 0 );
   }
}

HB_FUNC( LOG10 )
{
   if( HB_ISNUM( 1 ) )
   {
      HB_MATH_EXCEPTION hb_exc;
      double dResult, dArg = hb_parnd( 1 );

      hb_mathResetError( &hb_exc );
      dResult = log10( dArg );
      if( hb_mathGetError( &hb_exc, "LOG10", dArg, 0.0, dResult ) )
      {
         if( hb_exc.handled )
            hb_retndlen( hb_exc.retval, hb_exc.retvalwidth, hb_exc.retvaldec );
         else
         {
            /* math exception is up to the Harbour function, so do this as Clipper compatible as possible */
            switch( hb_exc.type )
            {
               case HB_MATH_ERR_SING:               /* argument to log was 0.0 */
               case HB_MATH_ERR_DOMAIN:             /* argument to log was < 0.0 */
                  hb_retndlen( -HUGE_VAL, -1, -1 ); /* return -infinity */
                  break;

               default:
                  hb_retnd( 0.0 );
                  break;
            }
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
                                  CT_ERROR_LOG10, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retni( 0 );
   }
}

HB_FUNC( FACT )
{
   if( HB_ISNUM( 1 ) )
   {
      int iInput = hb_parni( 1 );
      int i;
      double dResult = 1.0;

      if( ( iInput >= 0 ) && ( iInput < 22 ) )
      {
         for( i = 1; i <= iInput; i++ )
            dResult *= ( double ) i;
         hb_retnd( dResult );
      }
      else
         hb_retnd( -1.0 );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_FACT, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retnd( 0.0 );
   }
}
