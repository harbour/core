/*
 * This is an original work by Ralph Oliver (TRANSCOM SYSTEMS)
 * and is placed in the public domain.
 *
 * This program uses the preprocessor #defines and #command
 * by David Husnian.
 *
 * Modification history:
 *
 *    Rev 1.1   15 Aug 1991 23:05:22   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   07 Jun 1991 23:03:20   GLENN
 * Initial revision.
 *
 */

#define FORCE_BETWEEN( x, y, z )  ( y := Max( Min( y, z ), x ) )

FUNCTION ft_AMedian( aArray, nStart, nEnd )

   LOCAL nTemplen, aTemparray

   __defaultNIL( @nStart, 1 )
   __defaultNIL( @nEnd, Len( aArray ) )

   // Make sure bounds are in range
   FORCE_BETWEEN( 1, nEnd, Len( aArray ) )
   FORCE_BETWEEN( 1, nStart, nEnd )

   // Length of aTemparray
   nTemplen := ( nEnd - nStart ) + 1

   // Initialize and sort aTemparray
   aTemparray := ASort( ACopy( aArray, Array( nTemplen ), nStart, nTemplen ) )

   // Determine middle value(s)
   IF nTemplen % 2 == 0
      RETURN Int( ( ;
         aTemparray[ nTemplen / 2 ] + ;
         aTemparray[ Int( nTemplen / 2 ) + 1 ] ) / 2 )
   ENDIF

   RETURN aTemparray[ Int( nTemplen / 2 ) + 1 ]
