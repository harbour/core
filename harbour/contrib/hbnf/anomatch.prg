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
 *    Rev 1.2   15 Aug 1991 23:02:44   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:52   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:32   GLENN
 * Nanforum Toolkit
 *
 */

#define FORCE_BETWEEN( x, y, z )         ( y := Max( Min( y, z ), x ) )

FUNCTION FT_ANOMATCHES( aArray, bCompareBlock, nStartIndex, nEndIndex )

   LOCAL nNoOfMatches := 0              // Number of Matches Found

   __defaultNIL( @nStartIndex, 1 )
   __defaultNIL( @nEndIndex, Len( aArray ) )

   // Make Sure Bounds are in Range
   FORCE_BETWEEN( 1, nEndIndex,   Len( aArray ) )
   FORCE_BETWEEN( 1, nStartIndex, nEndIndex )

   AEval( aArray, ;
      {| xElement | ;
      iif( Eval( bCompareBlock, xElement ), nNoOfMatches++, NIL ) }, ;
      nStartIndex, nEndIndex - nStartIndex + 1 )

   RETURN nNoOfMatches                // FT_ANoMatches
