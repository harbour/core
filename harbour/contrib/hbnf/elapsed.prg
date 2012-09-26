/*
 * $Id$
 */

/*
 * File......: elapsed.prg
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

#ifdef FT_TEST

FUNCTION DEMO()

   LOCAL dStart, dEnd, cTimeStart, cTimeEnd, n, aDataTest := {}

   dStart := SToD( '19901128' )
   dEnd   := SToD( '19901130' )
   cTimeStart := "08:00:00"
   cTimeEnd   := "12:10:30"

   aDataTest := FT_ELAPSED( dStart, dEnd, cTimeStart, cTimeEnd )
   FOR n := 1 TO 4
      ? aDataTest[n,1], Str( aDataTest[n,2], 12, 4 )
      ?? " "
      ?? iif( n == 1, 'Days', iif( n == 2, 'Hours', iif( n == 3, 'Mins.', 'Secs.' ) ) )
   NEXT

   RETURN NIL

#endif

FUNCTION FT_ELAPSED( dStart, dEnd, cTimeStart, cTimeEnd )

   LOCAL nTotalSec, nCtr, nConstant, nTemp, aRetVal[4,2]

   IF ! ( ValType( dStart ) $ 'DC' )
      dStart := Date()
   ELSEIF ValType( dStart ) == 'C'
      cTimeStart := dStart
      dStart     := Date()
   ENDIF

   IF ! ( ValType( dEnd ) $ 'DC' )
      dEnd := Date()
   ELSEIF ValType( dEnd ) == 'C'
      cTimeEnd := dEnd
      dEnd     := Date()
   ENDIF

   IF ValType( cTimeStart ) != 'C' ; cTimeStart := '00:00:00' ; ENDIF
   IF ValType( cTimeEnd )   != 'C' ; cTimeEnd   := '00:00:00' ; ENDIF

   nTotalSec  := ( dEnd - dStart ) * 86400                              + ;
      Val( cTimeEnd )   *  3600                              + ;
      Val( SubStr( cTimeEnd,At(':', cTimeEnd ) + 1,2 ) ) * 60     + ;
      iif( RAt( ':', cTimeEnd ) == At( ':', cTimeEnd ), 0,        ;
      Val( SubStr( cTimeEnd,RAt(':', cTimeEnd ) + 1 ) ) )          - ;
      Val( cTimeStart ) * 3600                               - ;
      Val( SubStr( cTimeStart,At(':', cTimeStart ) + 1,2 ) ) * 60 - ;
      iif( RAt( ':', cTimeStart ) == At( ':', cTimeStart ), 0,    ;
      Val( SubStr( cTimeStart,RAt(':', cTimeStart ) + 1 ) ) )

   nTemp := nTotalSec

   FOR nCtr := 1 TO 4
      nConstant := iif( nCtr == 1, 86400, iif( nCtr == 2, 3600, iif( nCtr == 3, 60, 1 ) ) )
      aRetVal[ nCtr, 1 ] := Int( nTemp / nConstant )
      aRetval[ nCtr, 2 ] := nTotalSec / nConstant
      nTemp -= aRetVal[ nCtr, 1 ] * nConstant
   NEXT

   RETURN aRetVal
