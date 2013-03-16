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
 *    Rev 1.3   28 Sep 1992 00:27:50   GLENN
 * Jo French clean up.
 *
 *    Rev 1.2   15 Aug 1991 23:02:38   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:46   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:28   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION ft_AcctWeek( dGivenDate, nWeekNum )

   LOCAL nTemp, aRetVal

   IF HB_ISNUMERIC( dGivenDate )
      nWeekNum := dGivenDate
      dGivenDate := Date()
   ELSEIF ! HB_ISDATE( dGivenDate )
      dGivenDate := Date()
   ENDIF

   aRetVal := ft_AcctYear( dGivenDate )

   IF HB_ISNUMERIC( nWeekNum )
      nTemp := Int( ( aRetVal[ 3 ] - aRetVal[ 2 ] ) / 7 ) + 1
      IF nWeekNum < 1 .OR. nWeekNum > nTemp
         nWeekNum := nTemp
      ENDIF
      dGivenDate := aRetVal[ 2 ] + ( nWeekNum - 1 ) * 7
   ENDIF

   aRetVal[ 1 ] += StrZero( Int( ( dGivenDate - aRetVal[ 2 ] ) / 7 ) + 1, 2 )
   dGivenDate += ( 6 - ft_DayToBoW( dGivenDate ) )  // end of week
   aRetVal[ 2 ] := dGivenDate - 6
   aRetVal[ 3 ] := dGivenDate

   RETURN aRetVal
