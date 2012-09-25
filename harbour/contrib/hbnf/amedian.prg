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
   LOCAL var0, myarray0 := DIRECTORY(), myarray1 := {}
   CLS
   ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AMEDIAN"
   ?
   AEVAL( myarray0, { |x| AADD( myarray1, x[ F_SIZE ]) } )
   var0 := FT_AMEDIAN( myarray1 )
   ? PADR('FT_AMEDIAN( myarray1 ) ->',35)
   ?? var0
   ?
   var0 := FT_AMEDIAN( myarray1, 2 )
   ? PADR('FT_AMEDIAN( myarray1, 2 ) ->',35)
   ?? var0
   ?
   var0 := FT_AMEDIAN( myarray1, , 9 )
   ? PADR('FT_AMEDIAN( myarray1, , 9 ) ->',35)
   ?? var0
   ?
   var0 := FT_AMEDIAN( myarray1, 8, 40 )
   ? PADR('FT_AMEDIAN( myarray1, 8, 40 ) ->',35)
   ?? var0
   ?
   RETURN

#endif

#define FORCE_BETWEEN(x,y,z)         (y := MAX(MIN(y,z),x))

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := iif(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := iif(<ParamN> == NIL,<DefN>,<ParamN>)]

FUNCTION FT_AMEDIAN( aArray, nStart, nEnd )

   LOCAL nTemplen, aTemparray, nMiddle1, nMiddle2, nMedian

   DEFAULT nStart TO 1, ;
           nEnd   TO LEN( aArray )

   // Make Sure Bounds are in Range
   FORCE_BETWEEN(1, nEnd,   LEN( aArray ))
   FORCE_BETWEEN(1, nStart, nEnd)

   // Length of aTemparray
   nTemplen := ( nEnd - nStart ) + 1

   // Initialize aTemparray
   aTemparray := ACOPY( aArray, ARRAY( nTemplen ), nStart, nTemplen )

   // Sort aTemparray
   aTemparray := ASORT( aTemparray )

   // Determine middle value(s)
   IF ( nTemplen % 2 ) == 0
      nMiddle1 := aTemparray[ (nTemplen / 2) ]
      nMiddle2 := aTemparray[ INT(nTemplen / 2) +1 ]
      nMedian :=  INT( ( nMIddle1 + nMiddle2 ) / 2 )
   ELSE
      nMedian := aTemparray[ INT( nTemplen / 2 ) + 1 ]
   ENDIF

   RETURN nMedian
