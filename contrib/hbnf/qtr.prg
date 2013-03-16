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
 *    Rev 1.3   28 Sep 1992 00:41:40   GLENN
 * Jo French cleaned up.
 *
 *    Rev 1.2   15 Aug 1991 23:04:28   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:44   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:04   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION ft_Qtr( dGivenDate, nQtrNum )

   LOCAL nTemp, aRetVal

   IF HB_ISNUMERIC( dGivenDate )
      nQtrNum    := dGivenDate
      dGivenDate := Date()
   ELSEIF ! HB_ISDATE( dGivenDate )
      dGivenDate := Date()
   ENDIF

   aRetval := ft_Year( dGivenDate )

   IF HB_ISNUMERIC( nQtrNum )
      IF nQtrNum < 1 .OR. nQtrNum > 4
         nQtrNum := 4
      ENDIF
      dGivenDate := ft_MAdd( aRetVal[ 2 ], 3 * ( nQtrNum - 1 ) )
   ENDIF

   nTemp := Month( dGivenDate ) - Month( aRetVal[ 2 ] )
   nTemp += iif( nTemp >= 0, 1, 13 )
   nTemp := Int( ( nTemp - 1 ) / 3 )

   aRetVal[ 1 ] += StrZero( nTemp + 1, 2 )
   aRetVal[ 2 ] := ft_MAdd( aRetVal[ 2 ], nTemp * 3 )
   aRetVal[ 3 ] := ft_MAdd( aRetVal[ 2 ], 3 ) - 1

   RETURN aRetVal
