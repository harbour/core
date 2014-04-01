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
 *    Rev 1.2   15 Aug 1991 23:02:42   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:50   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:30   GLENN
 * Nanforum Toolkit
 *
 */

#define FORCE_BETWEEN( x, y, z )         ( y := Max( Min( y, z ), x ) )

FUNCTION ft_ADesSort( aArray, nStartIndex, nEndIndex )

   __defaultNIL( @nStartIndex, 1 )
   __defaultNIL( @nEndIndex, Len( aArray ) )

   // Make Sure Bounds are in Range
   FORCE_BETWEEN( 1, nEndIndex,   Len( aArray ) )
   FORCE_BETWEEN( 1, nStartIndex, nEndIndex )

   RETURN ASort( aArray, nStartIndex, nEndIndex, {| xElement1, xElement2 | xElement1 > xElement2 } )
