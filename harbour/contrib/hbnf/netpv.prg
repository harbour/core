/*
 * $Id$
 */

/*
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

#ifdef FT_TEST

PROCEDURE Main()

   ? FT_NETPV( 10000, 10, { 10000, 15000, 16000, 17000 } )

   RETURN

#endif

FUNCTION FT_NETPV( nInitialInvestment, nInterestRate, aCashFlow, nNoOfCashFlows )

   LOCAL nNetPresentValue := 0

   nNoOfCashFlows := iif( nNoOfCashFlows == NIL, Len( aCashFlow ), nNoOfCashFlows )

   AEval( aCashFlow, ;
      {| nElement, nElementNo | ;
      nNetPresentValue += nElement / ;
      ( ( 1 + ( nInterestRate / 100 ) ) ** nElementNo ) }, ;
      1, nNoOfCashFlows )

   RETURN nNetPresentValue -= nInitialInvestment
