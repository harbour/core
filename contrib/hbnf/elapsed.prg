/*
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: ?
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:37:56   GLENN
 * Jo French cleaned up.
 *
 *    Rev 1.2   15 Aug 1991 23:05:44   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:46   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:18   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION ft_Elapsed( dStart, dEnd, cTimeStart, cTimeEnd )

   LOCAL nTotalSec, nCtr, nConstant, nTemp, aRetVal[ 4 ][ 2 ]

   IF HB_ISSTRING( dStart )
      cTimeStart := dStart
      dStart := Date()
   ELSEIF ! HB_ISDATE( dStart )
      dStart := Date()
   ENDIF

   IF HB_ISSTRING( dEnd )
      cTimeEnd := dEnd
      dEnd := Date()
   ELSEIF ! HB_ISDATE( dEnd )
      dEnd := Date()
   ENDIF

   nTemp := nTotalSec := 86400 * ( ;
      hb_SToT( DToS( dEnd   ) + StrTran( hb_defaultValue( cTimeEnd  , "" ), ":" ) ) - ;
      hb_SToT( DToS( dStart ) + StrTran( hb_defaultValue( cTimeStart, "" ), ":" ) ) )

   FOR nCtr := 1 TO 4
      nConstant := iif( nCtr == 1, 86400, iif( nCtr == 2, 3600, iif( nCtr == 3, 60, 1 ) ) )
      aRetVal[ nCtr ][ 1 ] := Int( nTemp / nConstant )
      aRetval[ nCtr ][ 2 ] := nTotalSec / nConstant
      nTemp -= aRetVal[ nCtr ][ 1 ] * nConstant
   NEXT

   RETURN aRetVal
