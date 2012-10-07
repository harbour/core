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
 *    Rev 1.2   15 Aug 1991 23:02:48   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:56   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:36   GLENN
 * Nanforum Toolkit
 *
 */

#define CASE_AT( x, y, z )               z[ AT( x, y ) + 1 ]
#define FORCE_BETWEEN( x, y, z )         ( y := MAX( MIN( y, z ), x ) )

FUNCTION FT_ASUM( aArray, nStartIndex, nEndIndex )

   LOCAL nSumTotal := 0                 // Array Sum

   __defaultNIL( @nStartIndex, 1 )
   __defaultNIL( @nEndIndex, Len( aArray ) )

   // Make Sure Bounds are in Range
   FORCE_BETWEEN( 1, nEndIndex,   Len( aArray ) )
   FORCE_BETWEEN( 1, nStartIndex, nEndIndex )

   AEval( aArray, ;
      {| xElement | ;
      nSumTotal += ;
      CASE_AT( ValType( xElement ), "NC", ;
      { 0, xElement, ;
      iif( HB_ISSTRING( xElement ), Len( xElement ), 0 ) } ) }, ;
      nStartIndex, nEndIndex - nStartIndex + 1 )

   RETURN nSumTotal
