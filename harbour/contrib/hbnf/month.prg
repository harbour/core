/*
 * $Id$
 */

/*
 * File......: month.prg
 * Author....: Jo W. French dba Practical Computing
 * CIS ID....: 74731,1751
 *
 * The functions contained herein are the original work of Jo W. French
 * and are placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   28 Sep 1992 00:40:00   GLENN
 * Jo French cleaned up.
 *
 *    Rev 1.2   15 Aug 1991 23:05:42   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:28   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:46   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_MONTH( dGivenDate, nMonthNum )

   LOCAL lIsMonth, nTemp, aRetVal

   IF !( ValType( dGivenDate ) $ 'ND' )
      dGivenDate := Date()
   ELSEIF HB_ISNUMERIC( dGivenDate )
      nMonthNum  := dGivenDate
      dGivenDate := Date()
   ENDIF

   aRetVal   := FT_YEAR( dGivenDate )

   lIsMonth  := HB_ISNUMERIC( nMonthNum )
   IF lISMonth
      IF nMonthNum < 1 .OR. nMonthNum > 12
         nMonthNum := 12
      ENDIF
      dGivenDate := FT_MADD( aRetVal[ 2 ], nMonthNum - 1 )
   ENDIF

   nTemp := Month( dGivenDate ) - Month( aRetVal[ 2 ] )
   nTemp += iif( nTemp >= 0, 1, 13 )

   aRetVal[ 1 ] += PadL( LTrim( Str( nTemp, 2 ) ), 2, '0' )
   aRetVal[ 2 ] := FT_MADD( aRetVal[ 2 ], nTemp - 1 )
   aRetVal[ 3 ] := FT_MADD( aRetVal[ 2 ], 1 ) - 1

   RETURN aRetVal
