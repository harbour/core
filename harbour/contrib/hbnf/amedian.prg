/*
 * $Id$
 */

/*
 * File......: amedian.prg
 * Author....: Ralph Oliver,  TRANSCOM SYSTEMS
 * CIS ID....: 74030,703
 *
 * This is an original work by Ralph Oliver and is placed in the
 * public domain.
 *
 * This program uses the preprocessor #defines and #command in
 * Aavg.prg by David Husnian.
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

#ifdef FT_TEST

#include "directry.ch"

PROCEDURE Main()

   LOCAL var0, myarray0 := Directory(), myarray1 := {}

   CLS
   ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AMEDIAN"
   ?
   AEval( myarray0, {| x | AAdd( myarray1, x[ F_SIZE ] ) } )
   var0 := FT_AMEDIAN( myarray1 )
   ? PadR( 'FT_AMEDIAN( myarray1 ) ->', 35 )
   ?? var0
   ?
   var0 := FT_AMEDIAN( myarray1, 2 )
   ? PadR( 'FT_AMEDIAN( myarray1, 2 ) ->', 35 )
   ?? var0
   ?
   var0 := FT_AMEDIAN( myarray1, , 9 )
   ? PadR( 'FT_AMEDIAN( myarray1, , 9 ) ->', 35 )
   ?? var0
   ?
   var0 := FT_AMEDIAN( myarray1, 8, 40 )
   ? PadR( 'FT_AMEDIAN( myarray1, 8, 40 ) ->', 35 )
   ?? var0
   ?

   RETURN

#endif

#include "common.ch"

#define FORCE_BETWEEN( x, y, z )         ( y := MAX( MIN( y, z ), x ) )

FUNCTION FT_AMEDIAN( aArray, nStart, nEnd )

   LOCAL nTemplen, aTemparray, nMiddle1, nMiddle2, nMedian

   DEFAULT nStart TO 1
   DEFAULT nEnd   TO Len( aArray )

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
      nMiddle1 := aTemparray[ ( nTemplen / 2 ) ]
      nMiddle2 := aTemparray[ Int( nTemplen / 2 ) + 1 ]
      nMedian :=  Int( ( nMIddle1 + nMiddle2 ) / 2 )
   ELSE
      nMedian := aTemparray[ Int( nTemplen / 2 ) + 1 ]
   ENDIF

   RETURN nMedian
