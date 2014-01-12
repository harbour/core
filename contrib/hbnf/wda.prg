/*
 * Author....: Eric Splaver
 * CIS ID....: ?
 *
 * This is an original work by Eric Splaver and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:04:34   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   14 Jun 1991 04:25:46   GLENN
 * Initial revision.
 *
 */

FUNCTION ft_AddWkDy( dStart, nDys )

   LOCAL nDc := DoW( dStart )

   RETURN iif( nDc == 7, ;
      ( nDys - 1 )        % 5 + 7 * Int( ( nDys - 1 )        / 5 ) + 2, ;
      ( nDys + nDc - 2 )  % 5 + 7 * Int( ( nDys + nDc - 2 )  / 5 ) + 2 - nDc )
