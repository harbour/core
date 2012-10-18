/*
 * $Id$
 */

/*
 * Author....: Ralph Oliver,  TRANSCOM SYSTEMS
 * CIS ID....: 74030,703
 *
 * This is an original work by Ralph Oliver and is placed in the
 * public domain.
 *
 * This program uses the preprocessor #defines and #command
 * by David Husnian.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:05:22   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   07 Jun 1991 23:03:20   GLENN
 * Initial revision.
 *
 */

#define FORCE_BETWEEN( x, y, z )         ( y := Max( Min( y, z ), x ) )

FUNCTION FT_AMEDIAN( aArray, nStart, nEnd )

   LOCAL nTemplen, aTemparray, nMiddle1, nMiddle2, nMedian

   __defaultNIL( @nStart, 1 )
   __defaultNIL( @nEnd, Len( aArray ) )

   // Make Sure Bounds are in Range
   FORCE_BETWEEN( 1, nEnd,   Len( aArray ) )
   FORCE_BETWEEN( 1, nStart, nEnd )

   // Length of aTemparray
   nTemplen := ( nEnd - nStart ) + 1

   // Initialize aTemparray
   aTemparray := ACopy( aArray, Array( nTemplen ), nStart, nTemplen )

   // Sort aTemparray
   aTemparray := ASort( aTemparray )

   // Determine middle value(s)
   IF ( nTemplen % 2 ) == 0
      nMiddle1 := aTemparray[ nTemplen / 2 ]
      nMiddle2 := aTemparray[ Int( nTemplen / 2 ) + 1 ]
      nMedian :=  Int( ( nMIddle1 + nMiddle2 ) / 2 )
   ELSE
      nMedian := aTemparray[ Int( nTemplen / 2 ) + 1 ]
   ENDIF

   RETURN nMedian
