/*
 * $Id$
 */

/*
 * File......: aavg.prg
 * Author....: David Husnian
 * CIS ID....: ?
 *
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:04:54   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:38   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:20   GLENN
 * Nanforum Toolkit
 *
 */

#include "common.ch"

#define FORCE_BETWEEN( x, y, z )         ( y := MAX( MIN( y, z ), x ) )

FUNCTION FT_AAVG( aArray, nStartIndex, nEndIndex )

   DEFAULT nStartIndex TO 1
   DEFAULT nEndIndex   TO Len( aArray )

// Make Sure Bounds are in Range

   FORCE_BETWEEN( 1, nEndIndex,   Len( aArray ) )
   FORCE_BETWEEN( 1, nStartIndex, nEndIndex )

   RETURN iif( ! HB_ISARRAY( aArray ) .OR. Empty( aArray ), ;
      0, ;
      FT_ASUM( aArray, nStartIndex, nEndIndex ) / ;
      ( nEndIndex - nStartIndex + 1 ) )
