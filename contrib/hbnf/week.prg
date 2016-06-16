/* This is an original work of Jo W. French (dba Practical Computing)
   and is placed in the public domain.

      Rev 1.3   28 Sep 1992 00:44:52   GLENN
   Jo French cleaned up and correct to bow().

      Rev 1.2   15 Aug 1991 23:05:26   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:53:16   GLENN
   Minor edit to file header

      Rev 1.0   01 Apr 1991 01:02:30   GLENN
   Nanforum Toolkit
 */

FUNCTION ft_Week( dGivenDate, nWeekNum )

   LOCAL nTemp, aRetVal, dTemp

   IF HB_ISNUMERIC( dGivenDate )
      nWeekNum := dGivenDate
   ENDIF

   hb_default( @dGivenDate, Date() )

   aRetVal      := ft_Year( dGivenDate )
   dTemp        := aRetVal[ 2 ]
   aRetVal[ 2 ] -= ft_DayToBoW( aRetVal[ 2 ] )

   IF HB_ISNUMERIC( nWeekNum )
      nTemp := Int( ( aRetVal[ 3 ] - aRetVal[ 2 ] ) / 7 ) + 1
      IF nWeekNum < 1 .OR. nWeekNum > nTemp
         nWeekNum := nTemp
      ENDIF
      dGivenDate := aRetVal[ 2 ] + ( nWeekNum - 1 ) * 7
   ENDIF

   dGivenDate += 6 - ft_DayToBoW( dGivenDate )  // end of week

   aRetVal[ 1 ] += StrZero( Int( ( dGivenDate - aRetVal[ 2 ] ) / 7 ) + 1, 2 )
   aRetVal[ 2 ] := Max( dGivenDate - 6, dTemp )
   aRetVal[ 3 ] := Min( dGivenDate, aRetVal[ 3 ] )

   RETURN aRetVal
