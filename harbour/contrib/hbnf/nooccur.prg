/*
 * $Id$
 */

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
 *    Rev 1.2   15 Aug 1991 23:04:08   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:32   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:52   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION FT_NOOCCUR( cCheckFor, cCheckIn, lIgnoreCase )

   // Is Case Important??
   IF ! HB_ISLOGICAL( lIgnoreCase ) .OR. lIgnoreCase
      cCheckFor := Upper( cCheckFor )
      cCheckIn  := Upper( cCheckIn  )
   ENDIF

   RETURN iif( Len( cCheckFor ) == 0 .OR. Len( cCheckIn ) == 0, ;
      0, ;
      Int( ( Len( cCheckIn ) - Len( StrTran( cCheckIn, cCheckFor ) ) ) / ;
      Len( cCheckFor ) ) )
