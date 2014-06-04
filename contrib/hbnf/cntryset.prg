/*
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
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

FUNCTION ft_SetCentury( lNewState )
   RETURN __SetCentury( iif( HB_ISLOGICAL( lNewState ), lNewState, ) )
