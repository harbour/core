/* This is an original work of Jo W. French (dba Practical Computing)
   and is placed in the public domain.

      Rev 1.3   28 Sep 1992 00:35:20   GLENN
   Jo French clean up.

      Rev 1.2   15 Aug 1991 23:03:08   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   10 May 1991 23:59:38   GLENN
   Minor adjustment to header.

      Rev 1.0   01 Apr 1991 01:01:02   GLENN
   Nanforum Toolkit
 */

FUNCTION ft_DayOfYr( dGivenDate, nDayNum, lIsAcct )

   LOCAL nTemp, aRetVal

   DO CASE
   CASE HB_ISNUMERIC( dGivenDate ) ; nDayNum := dGivenDate
   CASE HB_ISLOGICAL( dGivenDate ) ; lIsAcct := dGivenDate
   ENDCASE

   hb_default( @dGivenDate, Date() )

   aRetVal := iif( HB_ISLOGICAL( lIsAcct ), ft_AcctYear( dGivenDate ), ft_Year( dGivenDate ) )

   IF HB_ISNUMERIC( nDayNum )
      nTemp := aRetVal[ 3 ] - aRetVal[ 2 ] + 1
      IF nDayNum < 1 .OR. nDayNum > nTemp
         nDayNum := nTemp
      ENDIF
      aRetVal[ 1 ] := aRetVal[ 2 ] + nDayNum - 1
   ELSE
      aRetVal[ 1 ] += StrZero( dGivenDate - aRetVal[ 2 ] + 1, 3 )
   ENDIF

   RETURN aRetVal
