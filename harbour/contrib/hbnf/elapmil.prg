/*
 * $Id$
 */

/*
 * File......: elapmil.prg
 * Author....: Alexander B. Spencer
 * CIS ID....: 76276,1012
 *
 * This is an original work by Alexander B. Spencer and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:32   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:44   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   07 Jun 1991 23:39:42   GLENN
 * Initial revision.
 *
 */

FUNCTION FT_ELAPMIN( cTIME1, cTIME2 )

   RETURN ( ( Val( Left( cTIME2, 2 ) ) * 60 ) + ( Val( Right( cTIME2, 2 ) ) ) ) - ;
          ( ( Val( Left( cTIME1, 2 ) ) * 60 ) + ( Val( Right( cTIME1, 2 ) ) ) )
