/*
 * File......: NETPV.PRG
 * Author....: David Husnian
 * CIS ID....: ?
 *
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:04:06   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:30   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:50   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_NETPV()
 *  $CATEGORY$
 *     Math
 *  $ONELINER$
 *     Calculate net present value
 *  $SYNTAX$
 *     FT_NETPV( <nInitialInvestment>, <nInterestRate>, <aCashFlow> ;
 *               [, <nNoOfCashFlows> ] ) -> nNetPV
 *  $ARGUMENTS$
 *     <nInitialInvestment> is the amount of cash invested for purposes
 *     of generating the cash flows.
 *
 *     <nInterestRate> is the annual interest rate used to discount
 *     expected cash flows (10.5% = 10.5, not .105).
 *
 *     <aCashFlow> is an array of the expected cash receipts each year.
 *
 *     <nNoOfCashFlows> is the number of years cash flows are expected
 *     (optional, Len( aCashFlow ) ).
 *  $RETURNS$
 *     The difference between the initial investment and the discounted
 *     cash flow in dollars.
 *  $DESCRIPTION$
 *     This function calculates the net present value, the difference
 *     between the cost of an initial investment and the present value
 *     of the expected cash flow(s) from the investment.  The present
 *     value of the expected cashflow(s) is calculated at the specified
 *     interest rate, which is often referred to as the "cost of capital".
 *
 *     This function can be used to evaluate alternative investments.
 *     The larger the NPV, the more profitable the investment.  See
 *     also the FutureValue and PresentValue for further explanations.
 *     The formula to calculate the net present value is:
 *
 *     NetPresentValue = SUM(CashFlow[i] / ((1 + InterestRate) ** i))
 *                       FOR i = 1 TO NoOfCashFlows
 *  $EXAMPLES$
 *     nNetPresentValue := FT_NETPV(10000, 10, { 10000,15000,16000,17000 } )
 *  $END$
 */

#ifdef FT_TEST
  FUNCTION MAIN()
     ? FT_NETPV( 10000, 10, { 10000,15000,16000,17000 } )
  RETURN ( nil )
#endif


FUNCTION FT_NETPV(nInitialInvestment, nInterestRate, aCashFlow, nNoOfCashFlows)

   LOCAL nNetPresentValue := 0

   nNoOfCashFlows := iif( nNoOfCashFlows == nil, len( aCashFlow ), nNoOfCashFlows )

   AEVAL(aCashFlow, ;
         { | nElement, nElementNo | ;
           nNetPresentValue += nElement / ;
                               ((1 + (nInterestRate / 100)) ** nElementNo) }, ;
         1, nNoOfCashFlows)

   RETURN (nNetPresentValue -= nInitialInvestment)
