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
  dStart := CTOD('11/28/90')
  dEnd   := CTOD('11/30/90')
  cTimeStart := "08:00:00"
  cTimeEnd   := "12:10:30"

  aDataTest := FT_ELAPSED(dStart,dEnd,cTimeStart,cTimeEnd)
  FOR n := 1 to 4
    ? aDataTest[n,1], STR(aDataTest[n,2], 12, 4)
    ?? " "
    ?? iif(n == 1, 'Days', iif( n== 2, 'Hours', iif( n == 3, 'Mins.', 'Secs.')))
  NEXT
  RETURN NIL

#endif

FUNCTION FT_ELAPSED(dStart, dEnd, cTimeStart, cTimeEnd)
  LOCAL nTotalSec, nCtr, nConstant, nTemp, aRetVal[4,2]

  IF ! ( VALTYPE(dStart) $ 'DC' )
     dStart := DATE()
  ELSEIF VALTYPE(dStart) == 'C'
     cTimeStart := dStart
     dStart     := DATE()
  ENDIF

  IF ! ( VALTYPE(dEnd) $ 'DC' )
     dEnd := DATE()
  ELSEIF VALTYPE(dEnd) == 'C'
     cTimeEnd := dEnd
     dEnd     := DATE()
  ENDIF

  IF VALTYPE(cTimeStart) != 'C' ; cTimeStart := '00:00:00' ; ENDIF
  IF VALTYPE(cTimeEnd)   != 'C' ; cTimeEnd   := '00:00:00' ; ENDIF

  nTotalSec  := (dEnd - dStart) * 86400                              + ;
                VAL(cTimeEnd)   *  3600                              + ;
                VAL(SUBSTR(cTimeEnd,AT(':', cTimeEnd)+1,2)) * 60     + ;
                iif(RAT(':', cTimeEnd) == AT(':', cTimeEnd), 0,        ;
                VAL(SUBSTR(cTimeEnd,RAT(':', cTimeEnd)+1)))          - ;
                VAL(cTimeStart) * 3600                               - ;
                VAL(SUBSTR(cTimeStart,AT(':', cTimeStart)+1,2)) * 60 - ;
                iif(RAT(':', cTimeStart) == AT(':', cTimeStart), 0,    ;
                VAL(SUBSTR(cTimeStart,RAT(':', cTimeStart)+1)))

  nTemp := nTotalSec

  FOR nCtr := 1 to 4
     nConstant := iif(nCtr == 1, 86400, iif(nCtr == 2, 3600, iif( nCtr == 3, 60, 1)))
     aRetVal[nCtr,1] := INT(nTemp/nConstant)
     aRetval[nCtr,2] := nTotalSec / nConstant
     nTemp -= aRetVal[nCtr,1] * nConstant
  NEXT

RETURN aRetVal
