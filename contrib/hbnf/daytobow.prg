/* This is an original work of Jo W. French (dba Practical Computing)
   and is placed in the public domain.

      Rev 1.3   28 Sep 1992 00:36:46   GLENN
   Jo French clean up.

      Rev 1.2   15 Aug 1991 23:03:16   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:51:28   GLENN
   Minor edit to file header

      Rev 1.0   01 Apr 1991 01:01:04   GLENN
   Nanforum Toolkit
 */

FUNCTION ft_DayToBoW( dGivenDate )

   LOCAL nDOW_Start := ft_DateCnfg()[ 2 ]
   LOCAL nRetVal := DoW( hb_defaultValue( dGivenDate, Date() ) ) - nDOW_Start

   IF nRetVal < 0
      nRetVal += 7
   ENDIF

   RETURN nRetVal
