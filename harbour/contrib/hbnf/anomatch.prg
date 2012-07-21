/*
 * $Id$
 */

/*
 * File......: anomatch.prg
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

#define FORCE_BETWEEN(x,y,z)         (y := MAX(MIN(y,z),x))

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := iif(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := iif(<ParamN> == NIL,<DefN>,<ParamN>)]

FUNCTION FT_ANOMATCHES(aArray, bCompareBlock, nStartIndex, nEndIndex)

   LOCAL nNoOfMatches := 0              // Number of Matches Found

   DEFAULT nStartIndex TO 1, ;
           nEndIndex   TO LEN(aArray)

                                        // Make Sure Bounds are in Range
   FORCE_BETWEEN(1, nEndIndex,   LEN(aArray))
   FORCE_BETWEEN(1, nStartIndex, nEndIndex)

   AEVAL(aArray, ;
         { | xElement | ;
           iif(EVAL(bCompareBlock, xElement), nNoOfMatches++, NIL) }, ;
         nStartIndex, nEndIndex - nStartIndex + 1)

   RETURN nNoOfMatches                // FT_ANoMatches
