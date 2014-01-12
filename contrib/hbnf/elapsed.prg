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

   LOCAL aRetVal[ 4 ]
   LOCAL nTotalSec, nConst, nTemp, tmp

   IF HB_ISSTRING( dStart )
      cTimeStart := dStart
   ENDIF
   IF HB_ISSTRING( dEnd )
      cTimeEnd := dEnd
   ENDIF

   IF ! HB_ISDATE( dStart )
      dStart := Date()
   ENDIF
   IF ! HB_ISDATE( dEnd )
      dEnd := Date()
   ENDIF

   nTemp := nTotalSec := 86400 * ( ;
      hb_SToT( DToS( dEnd   ) + StrTran( hb_defaultValue( cTimeEnd  , "" ), ":" ) ) - ;
      hb_SToT( DToS( dStart ) + StrTran( hb_defaultValue( cTimeStart, "" ), ":" ) ) )

   FOR EACH nConst IN { 86400, 3600, 60, 1 }
      aRetVal[ nConst:__enumIndex() ] := { tmp := Int( nTemp / nConst ), nTotalSec / nConst }
      nTemp -= tmp * nConst
   NEXT

   RETURN aRetVal
