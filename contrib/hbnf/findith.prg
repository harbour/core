/*
 * Author....: David Husnian
 * CIS ID....: ?
 *
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:36   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:52   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:22   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION ft_FindITh( cCheckFor, cCheckIn, nWhichOccurrence, lIgnoreCase )

   LOCAL nIthOccurrence

   // Is Case Important??

   IF ! HB_ISLOGICAL( lIgnoreCase ) .OR. lIgnoreCase
      cCheckFor := Upper( cCheckFor )
      cCheckIn  := Upper( cCheckIn )
   ENDIF

   RETURN iif( nWhichOccurrence == 1, ;
      At( cCheckFor, cCheckIn ), ;
      iif( ( nIthOccurrence := At( cCheckFor, ;
      StrTran( cCheckIn, cCheckFor, ;
      "", 1, ;
      nWhichOccurrence - 1 ) ) ) == 0, ;
      0, ;
      nIthOccurrence + ( ( nWhichOccurrence - 1 ) * Len( cCheckFor ) ) ) )
