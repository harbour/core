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
 *    Rev 1.2   15 Aug 1991 23:03:12   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:20   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:58   GLENN
 * Nanforum Toolkit
 *
 */

FUNCTION ft_SetCentury( lNewSetState )

   // Note that if CENTURY is ON then
   // DToC() Will Return a String of Length
   // 10, Otherwise it Will be of Length 8

   LOCAL lOldSetState := ( Len( DToC( Date() ) ) == 10 )

   IF HB_ISLOGICAL( lNewSetState )        // Did they want it set?
      SET CENTURY ( lNewSetState )        // Yes, set it
   ENDIF

   RETURN lOldSetState
