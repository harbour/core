/*
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

FUNCTION ft_Month( dGivenDate, nMonthNum )

   LOCAL nTemp, aRetVal

   IF HB_ISNUMERIC( dGivenDate )
      nMonthNum  := dGivenDate
      dGivenDate := Date()
   ELSEIF ! HB_ISDATE( dGivenDate )
      dGivenDate := Date()
   ENDIF

   aRetVal := ft_Year( dGivenDate )

   IF HB_ISNUMERIC( nMonthNum )
      IF nMonthNum < 1 .OR. nMonthNum > 12
         nMonthNum := 12
      ENDIF
      dGivenDate := ft_MAdd( aRetVal[ 2 ], nMonthNum - 1 )
   ENDIF

   nTemp := Month( dGivenDate ) - Month( aRetVal[ 2 ] )
   nTemp += iif( nTemp >= 0, 1, 13 )

   aRetVal[ 1 ] += StrZero( nTemp, 2 )
   aRetVal[ 2 ] := ft_MAdd( aRetVal[ 2 ], nTemp - 1 )
   aRetVal[ 3 ] := ft_MAdd( aRetVal[ 2 ], 1 ) - 1

   RETURN aRetVal
