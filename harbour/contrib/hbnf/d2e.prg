/*
 * $Id$
 */

/*
 * Author....: Gary Baren
 * CIS ID....: 75470,1027
 *
 * This is an original work by Gary Baren and is hereby placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:05:02   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:24   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   09 Jun 1991 00:27:06   GLENN
 * Initial revision.
 *
 */

#define _LOG10( num )         ( Log( num ) / Log( 10 ) )
#define DEFAULT_PRECISION     6

FUNCTION ft_D2E( nDec, nPrecision )

   LOCAL nExp

   __defaultNIL( @nPrecision, DEFAULT_PRECISION )

   IF nDec == 0
      nExp := 0
   ELSEIF Abs( nDec ) < 1
      nExp := Int( _LOG10( nDec ) ) - 1
   ELSE
      nExp := Int( _LOG10( Abs( nDec ) + 0.00001 ) )   /* 0.00001 == kludge */
   ENDIF           /* for imprecise logs */

   nDec /= 10 ^ nExp

   IF Round( Abs( nDec ), nPrecision ) >= 10
      nDec /= 10
      nExp++
   ENDIF // another kludge FOR stuff LIKE "999999999"

   RETURN LTrim( Str( nDec, nPrecision + 3, nPrecision ) ) + "E" + hb_ntos( nExp )
