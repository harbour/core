/* This is an original work by David Husnian and is placed in the public domain.

      Rev 1.2   15 Aug 1991 23:04:08   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:52:32   GLENN
   Minor edit to file header

      Rev 1.0   01 Apr 1991 01:01:52   GLENN
   Nanforum Toolkit
 */

FUNCTION ft_NoOccur( cCheckFor, cCheckIn, lIgnoreCase )

   IF hb_defaultValue( lIgnoreCase, .T. )
      cCheckFor := Upper( cCheckFor )
      cCheckIn  := Upper( cCheckIn )
   ENDIF

   RETURN iif( HB_ISNULL( cCheckFor ) .OR. HB_ISNULL( cCheckIn ), 0, ;
      Int( ( Len( cCheckIn ) - Len( StrTran( cCheckIn, cCheckFor ) ) ) / Len( cCheckFor ) ) )
