/* This is an original work by Gary Baren and is placed in the public domain.

      Rev 1.2   15 Aug 1991 23:05:02   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.1   14 Jun 1991 19:51:24   GLENN
   Minor edit to file header

      Rev 1.0   09 Jun 1991 00:27:06   GLENN
   Initial revision.
 */

#define _LOG10( num )   ( Log( num ) / Log( 10 ) )

FUNCTION ft_D2E( nDec, nPrecision )

   LOCAL nExp

   __defaultNIL( @nPrecision, 6 )

   DO CASE
   CASE nDec == 0
      nExp := 0
   CASE Abs( nDec ) < 1
      nExp := Int( _LOG10( nDec ) ) - 1
   OTHERWISE
      nExp := Int( _LOG10( Abs( nDec ) + 0.00001 ) )  // 0.00001 == kludge for imprecise logs
   ENDCASE

   nDec /= 10 ^ nExp

   IF Round( Abs( nDec ), nPrecision ) >= 10
      nDec /= 10
      nExp++
   ENDIF  // another kludge for stuff like "999999999"

   RETURN LTrim( Str( nDec, nPrecision + 3, nPrecision ) ) + "E" + hb_ntos( nExp )
