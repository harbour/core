/*
 * $Id$
 */

/*
 * File......: acctmnth.prg
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:24:54   GLENN
 * Jo French clean up.
 *
 *    Rev 1.2   15 Aug 1991 23:02:30   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:42   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:24   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_ACCTMONTH( dGivenDate, nMonthNum )

   LOCAL nYTemp, nMTemp, lIsMonth, aRetVal

   IF ! ( ValType( dGivenDate ) $ 'ND' )
      dGivenDate := Date()
   ELSEIF HB_ISNUMERIC( dGivenDate )
      nMonthNum := dGivenDate
      dGivenDate := Date()
   ENDIF

   aRetVal := FT_MONTH( dGivenDate )
   nYTemp := Val( SubStr( aRetVal[1],1,4 ) )
   nMTemp := Val( SubStr( aRetVal[1],5,2 ) )
   aRetVal[2] := FT_ACCTADJ( aRetVal[2] )
   aRetVal[3] := FT_ACCTADJ( aRetVal[3], .T. )

   IF dGivenDate < aRetVal[2]
      dGivenDate := FT_MADD( dGivenDate, - 1 )
      aRetVal    := FT_MONTH( dGivenDate )
      nMTemp     -= 1
      IF nMTemp  == 0
         nYTemp -= 1
         nMTemp := 12
      ENDIF
      aRetVal[2] := FT_ACCTADJ( aRetVal[2] )
      aRetVal[3] := FT_ACCTADJ( aRetVal[3], .T. )

   ELSEIF dGivenDate > aRetVal[3]

      dGivenDate := FT_MADD( dGivenDate, 1 )
      aRetVal    := FT_MONTH( dGivenDate )
      nMTemp     += 1
      IF nMTemp == 13
         nYTemp += 1
         nMTemp := 1
      ENDIF
      aRetVal[2] := FT_ACCTADJ( aRetVal[2] )
      aRetVal[3] := FT_ACCTADJ( aRetVal[3], .T. )

   ENDIF

   lIsMonth := HB_ISNUMERIC( nMonthNum )
   IF lIsMonth
      IF nMonthNum < 1 .OR. nMonthNum > 12
         nMonthNum := 12
      ENDIF
      aRetVal    := FT_MONTH( dGivenDate, nMonthNum )
      nYTemp     := Val( SubStr( aRetVal[1],1,4 ) )
      nMTemp     := Val( SubStr( aRetVal[1],5,2 ) )
      aRetVal[2] := FT_ACCTADJ( aRetVal[2] )
      aRetVal[3] := FT_ACCTADJ( aRetVal[3], .T. )
   ENDIF

   aRetVal[1] := Str( nYTemp, 4 ) + PadL( LTrim( Str( nMTemp, 2 ) ), 2, '0' )

   RETURN aRetVal
