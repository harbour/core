/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Financial functions
 *     - PV
 *     - FV
 *     - PAYMENT
 *     - PERIODS
 *     - RATE
 *
 * NOTE: All these functions were builded using Borland C++ 5.5 (free version)
 *
 * Copyright 2001  Alejandro de Garate  <alex_degarate@hotmail.com>
 *
 * Documentation and changes concerning error handling Copyright 2001 
 *   IntTec GmbH, Freiburg, Germany, Author: Martin Vogel <vogel@inttec.de>
 *
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


#include "ct.h"


/*  $DOC$
 *  $FUNCNAME$
 *      FV()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Future value of a capital
 *  $SYNTAX$
 *      FV (nDeposit, nInterest, nPeriods) --> nFutureValue
 *  $ARGUMENTS$
 *      <nDeposit>     amount of money invested per period
 *      <nInterest>    rate of interest per period, 1 == 100%
 *      <nPeriods>     period count
 *  $RETURNS$
 *      <nFutureValue> Total value of the capital after <nPeriods> of
 *                     paying <nDeposit> and <nInterest> interest being
 *                     paid every period and added to the capital (resulting
 *                     in compound interest)
 *  $DESCRIPTION$
 *      FV() calculates the value of a capital after <nPeriods> periods.
 *      Starting with a value of 0, every period, <nDeposit>
 *      (Dollars, Euros, Yens, ...) and an interest of <nInterest> for the
 *      current capital are added for the capital (<nInterest>=Percent/100).
 *      Thus, one gets the non-linear effects of compound interests:
 *      value in period 0 = 0
 *      value in period 1 = ((value in period 0)*(1+<nInterest>/100)) + <nDeposit>
 *      value in period 2 = ((value in period 1)*(1+<nInterest>/100)) + <nDeposit>
 *              etc....
 *      value in period <nPeriod> = ((value in period <nPeriod>-1)*(1+<nInterest>/100))< + <nDeposit>
 *                                = <nDeposit> * sum from i=0 to <nPeriod>-1 over (1+<nInterest>/100)^i
 *                                = <nDeposit> * ((1+<nInterest>/100)^n-1) / (<nInterest>/100)
 *  $EXAMPLES$
 *      // Payment of 1000 per year for 10 years at a interest rate
 *      // of 5 per cent per year
 *      
 *      ? fv (1000, 0.05, 10)  --> 12577.893
 *  $TESTS$
 *      fv (1000, 0.00, 10) == 10000.0
 *      fv (1000, 0.05, 10) == 12577.893
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      FV() is compatible with CT3's FV().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is finan.c, library is libct.
 *  $SEEALSO$
 *      PV(),PAYMENT(),PERIODS(),RATE()
 *  $END$
 */

HB_FUNC( FV )
{
  if( ISNUM(1) && ISNUM(2) && ISNUM(3) )
  {
    double dPayment = hb_parnd(1);
    double dRate    = hb_parnd(2);
    double dTime    = hb_parnd(3);
    double dResult;

    ct_matherrbegin();
        
    if (dRate == 0.0)
    {
      /* NOTE: CT3 crashes with dRate == 0.0 */
      dResult = dPayment*dTime;
    }
    else
    {
      dResult = dPayment*(pow (1.0+dRate, dTime)-1.0)/dRate;
    }

    ct_matherrend();

    hb_retnd (dResult);

  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_FV,
                               NULL, "FV", 0, EF_CANSUBSTITUTE, 3,
                               hb_paramError (1), hb_paramError (2),
                               hb_paramError (3));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }
  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      PV()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Present value of a loan
 *  $SYNTAX$
 *      PV (nPayment, nInterest, nPeriods) --> nPresentValue
 *  $ARGUMENTS$
 *      <nPayment>     amount of money paid back per period
 *      <nInterest>    rate of interest per period, 1 == 100%
 *      <nPeriods>     period count
 *  $RETURNS$
 *      <nPresentValue> Present value of a loan when one is paying back
 *                      <nDeposit> per period at a rate of interest of
 *                      <nInterest> per period
 *  $DESCRIPTION$
 *      PV() calculates the present value of a loan that is paid back
 *      in <nPeriods> payments of <nPayment> (Dollars, Euros, Yens,...)
 *      while the rate of interest is <nInterest> per period:
 *      debt in period 0 = <nPresentValue>
 *      debt in period 1 = ((debt in period 0)-<nPayment>)*(1+<nInterest>/100)
 *      debt in period 2 = ((debt in period 1)-<nPayment>)*(1+<nInterest>/100)
 *           etc...
 *      debt in period <nPeriod> = ((debt in period <nPeriod>-1)-<nPayment>)*(1+<nInterest>/100)
 *                                 -> has to be 0, so
 *      <nPresentValue> = <nPayment>*(1-(1+<nInterest>/100)^(-n))/(<nInterest>/100)
 *  $EXAMPLES$
 *      // You can afford to pay back 100 Dollars per month for 5 years
 *      // at a interest rate of 0.5% per month (6% per year), so instead
 *      // of 6000 Dollars (the amount you will pay back) the bank will pay
 *      // you
 *      
 *      ? pv (100, 0.005, 60)  --> 5172.56
 *  $TESTS$
 *      pv (100, 0.0, 60)   == 6000.0
 *      pv (100, 0.005, 60) == 5172.56
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      PV() is compatible with CT3's PV().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is finan.c, library is libct.
 *  $SEEALSO$
 *      FV(),PAYMENT(),PERIODS(),RATE()
 *  $END$
 */

HB_FUNC( PV )
{
  if( ISNUM(1) && ISNUM(2) && ISNUM(3) )
  {
    double dPayment = hb_parnd(1);
    double dRate    = hb_parnd(2);
    double dTime    = hb_parnd(3);
    double dResult;
      
    ct_matherrbegin();
      
    if (dRate == 0.0)
    {
      /* NOTE: CT3 crashes with dRate == 0.0 */
      dResult = dPayment*dTime;
    }
    else
    {
      dResult = dPayment*(1.0-pow (1.0+dRate, -dTime))/dRate;
    }

    ct_matherrend();

    hb_retnd (dResult);
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_PV,
                               NULL, "PV", 0, EF_CANSUBSTITUTE, 3,
                               hb_paramError (1), hb_paramError (2),
                               hb_paramError (3));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }
  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      PAYMENT()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Payments for a loan
 *  $SYNTAX$
 *      PAYMENT (nLoan, nInterest, nPeriods) --> nPayment
 *  $ARGUMENTS$
 *      <nLoan>        amount of money you get from the bank
 *      <nInterest>    rate of interest per period, 1 == 100%
 *      <nPeriods>     period count
 *  $RETURNS$
 *      <nPayment>     Periodical payment one has to make to pay the
 *                     loan <nLoan> back
 *  $DESCRIPTION$
 *      PAYMENT() calculates the payment one has to make periodically
 *      to pay back a loan <nLoan> within <nPeriods> periods and for a
 *      rate of interest <nInterest> per period.
 *      debt in period 0 = <nLoan>
 *      debt in period 1 = ((debt in period 0)-<nPayment>)*(1+<nInterest>/100)
 *      debt in period 2 = ((debt in period 1)-<nPayment>)*(1+<nInterest>/100)
 *           etc...
 *      debt in period <nPeriod> = ((debt in period <nPeriod>-1)-<nPayment>)*(1+<nInterest>/100)
 *                                 -> has to be 0, so
 *      <nPayment> = <nLoan>*(<nInterest>/100)/(1-(1+<nInterest>/100)^(-n))
 *  $EXAMPLES$
 *      // You get a loan of 5172.56 at a interest rate of 0.5% per
 *      // month (6% per year).
 *      // For 5 years, you have to pay back every month
 *      
 *      ? payment (5172.56, 0.005, 60)  --> 100.00
 *  $TESTS$
 *      payment (5172.56, 0.0, 60)   == 86.21
 *      payment (5172.56, 0.005, 60) == 100.00
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      PAYMENT() is compatible with CT3's PAYMENT().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is finan.c, library is libct.
 *  $SEEALSO$
 *      PV(),FV(),PERIODS(),RATE()
 *  $END$
 */

HB_FUNC( PAYMENT )
{
  if( ISNUM(1) && ISNUM(2) && ISNUM(3) )
  {
    double dCapital = hb_parnd(1);
    double dRate    = hb_parnd(2);
    double dTime    = hb_parnd(3);
    double dResult;

    if (dRate == 0.0)
    {
      /* NOTE: CT3 crashes with dRate == 0.0 */
      if (dTime == 0.0)
      {
        int iArgErrorMode = ct_getargerrormode();
        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_PAYMENT,
                    NULL, "PAYMENT", 0, EF_CANDEFAULT, 3,
                    hb_paramError (1), hb_paramError (2),
                    hb_paramError (3));
        };
        hb_retnd (0.0);
        return;
      }
      ct_matherrbegin();
      dResult = dCapital/dTime;
      ct_matherrend();
    }
    else
    {
      ct_matherrbegin();
      dResult = dCapital*dRate/(1.0-pow (1.0+dRate, -dTime));
      ct_matherrend();
    }

    hb_retnd (dResult);
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_PAYMENT,
                               NULL, "PAYMENT", 0, EF_CANSUBSTITUTE, 3,
                               hb_paramError (1), hb_paramError (2),
                               hb_paramError (3));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }

  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      PERIODS()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Number of periods for a loan
 *  $SYNTAX$
 *      PERIODS (nLoan, nPayment, nInterest) --> nPeriods
 *  $ARGUMENTS$
 *      <nLoan>        amount of money you get from the bank
 *      <nPayment>     amount of money you pay back per period
 *      <nInterest>    rate of interest per period, 1 == 100%
 *  $RETURNS$
 *      <nPeriods>     number of periods you need to pay the loan back
 *  $DESCRIPTION$
 *      PERIODS() calculates the number of periods one needs to pay back
 *      a loan of <nLoan> with periodical payments of <nPayment> and for a
 *      rate of interest <nInterest> per period.
 *      debt in period 0 = <nLoan>
 *      debt in period 1 = ((debt in period 0)-<nPayment>)*(1+<nInterest>/100)
 *      debt in period 2 = ((debt in period 1)-<nPayment>)*(1+<nInterest>/100)
 *           etc...
 *      debt in period <nPeriod> = ((debt in period <nPeriod>-1)-<nPayment>)*(1+<nInterest>/100)
 *                                 -> has to be 0, so
 *      <nPeriods> = -log(1-<nLoan>*(<nInterest>/100)/<nPayment>)/log(1+<nInterest>/100))
 *
 *      Note, however that in the case of nPayment <= <nLoan>*(<nInterest>/100),
 *      one would need infinite time to pay the loan back. The functions does
 *      then return -1.
 *  $EXAMPLES$
 *      // You get a loan of 5172.56 at a interest rate of 0.5% per
 *      // month (6% per year).
 *      // You can afford to pay 100 back every month, so you need
 *      
 *      ? periods (5172.56, 100, 0.005)  --> 60.0
 *
 *      // months to cancel the loan.
 *  $TESTS$
 *      periods (5172.56, 100, 0.005) == 60.0
 *      periods (5172.56, 100, 0.0) == 51.7256
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      PERIODS() is compatible with CT3's PERIODS().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is finan.c, library is libct.
 *  $SEEALSO$
 *      PV(),FV(),PAYMENT(),RATE()
 *  $END$
 */

HB_FUNC( PERIODS )
{
  if( ISNUM(1) && ISNUM(2) && ISNUM(3) )
  {
    double dCapital = hb_parnd(1);
    double dPayment = hb_parnd(2);
    double dRate    = hb_parnd(3);
    double dResult;

    if (dPayment <= dCapital*dRate)
    {
      /* in this case infinite time is needed to cancel the loan */
      hb_retnd (-1.0);
      return;
    }
    
    if (dRate == 0.0)
    {
      /* NOTE: CT3 crashes with dRate == 0.0 */
      if (dPayment == 0.0)
      {
        int iArgErrorMode = ct_getargerrormode();
        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_PERIODS,
                    NULL, "PERIODS", 0, EF_CANDEFAULT, 3,
                    hb_paramError (1), hb_paramError (2),
                    hb_paramError (3));
        };
        hb_retnd (0.0);
        return;
      }
      ct_matherrbegin();
      dResult = dCapital/dPayment;
      ct_matherrend();
    }
    else
    {
      ct_matherrbegin();
      dResult = -log(1.0-(dCapital*dRate/dPayment))/log(1+dRate);
      ct_matherrend();
    }

    hb_retnd( dResult );
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_PERIODS,
                               NULL, "PERIODS", 0, EF_CANSUBSTITUTE, 3,
                               hb_paramError (1), hb_paramError (2),
                               hb_paramError (3));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }

  return;
}


/*  $DOC$
 *  $FUNCNAME$
 *      RATE()
 *  $CATEGORY$
 *      CT3 math functions
 *  $ONELINER$
 *      Estimate rate of interest for a loan
 *  $SYNTAX$
 *      RATE (nLoan, nPayment, nPeriods) --> nRate
 *  $ARGUMENTS$
 *      <nLoan>          amount of money you get from the bank
 *      <nPayment>       amount of money you pay back per period
 *      <nPeriods>       number of periods you pay the loan back
 *  $RETURNS$
 *      <nInterest>    estimated rate of interest per period, 1 == 100%
 *  $DESCRIPTION$
 *      RATE() calculates the rate of interest per period for the given
 *      loan, payment per periods and number of periods. This is done with
 *      the same equation used in the PAYMENT() or PERIODS() function:
 *
 *      <nPayment> = <nLoan>*(<nInterest>/100)/(1-(1+<nInterest>/100)^(-<nPeriods>))
 *      
 *      However, this equation can not be solved for <nInterest> in a "closed"
 *      manner, i.e. <nInterest> = ..., so that the result can only be estimated.
 *  $EXAMPLES$
 *      // You get a loan of 5172.56, pay 100 back every month for
 *      // 5 years (60 months). The effective interest rate per
 *      // period (=month) is
 *      
 *      ? rate (5172.56, 100, 60)  --> 0.005
 *
 *  $TESTS$
 *      rate (5172.56, 100, 60.0) == 0.005
 *      rate (6000.0, 100, 60.0) == 0.0
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      RATE() is compatible with CT3's RATE().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is finan.c, library is libct.
 *  $SEEALSO$
 *      PV(),FV(),PAYMENT(),PERIODS()
 *  $END$
 */

HB_FUNC( RATE )
{
  if( ISNUM(1) && ISNUM(2) && ISNUM(3) )
  {
    double dCapital = hb_parnd(1);
    double dPayment = hb_parnd(2);
    double dTime    = hb_parnd(3);
    double dAux;                  /* estimated payment to compare for      */
    double dEpsilon = 0.00001;    /* mimimal to consider 2 numbers as equal*/
    double dScale   = 1.0;        /* fractional step                       */
    double r;                     /* temptative rate                       */
    double j        = 1.0;        /* index                                 */
    double dExp;

    while( j < 1020.0 )    /* maximum anual rate */
    {
       r = j * 0.000833333;    /* j * ( 0.01 / 12.0)  mensual's rate */

       /* replace PAYMENT() function overhead */
       dExp     = pow( (1.0 + r), dTime );
       dAux  = dCapital * ( (dExp * r) / (dExp - 1.0) );

       if( dAux > dPayment )
       {
           j = j - dScale;
           dScale = dScale * 0.10;

           if( (dAux - dPayment) < dEpsilon)
               break;
       }
       else
          j = j + dScale;

     } /* endwhile */

     hb_retnd( j * 0.000833333 );     /* return as mensual's rate */
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_RATE,
                               NULL, "RATE", 0, EF_CANSUBSTITUTE, 3,
                               hb_paramError (1), hb_paramError (2),
                               hb_paramError (3));
    }
    
    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      hb_retnd (0.0);
    }
  }

  return;
}





