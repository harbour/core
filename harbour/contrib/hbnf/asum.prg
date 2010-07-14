/*
 * $Id$
 */

/*
 * File......: asum.prg
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

#define CASE_AT(x,y,z)               z[AT(x,y)+1]
#define FORCE_BETWEEN(x,y,z)         (y := MAX(MIN(y,z),x))
#define IS_CHAR(x)                   (VALTYPE(x) == "C")

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := iif(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := iif(<ParamN> == NIL,<DefN>,<ParamN>)]

FUNCTION FT_ASUM(aArray, nStartIndex, nEndIndex)

   LOCAL nSumTotal := 0                 // Array Sum

   DEFAULT nStartIndex TO 1, ;
           nEndIndex   TO LEN(aArray)
                                        // Make Sure Bounds are in Range
   FORCE_BETWEEN(1, nEndIndex,   LEN(aArray))
   FORCE_BETWEEN(1, nStartIndex, nEndIndex)

   AEVAL(aArray, ;
         { | xElement | ;
           nSumTotal += ;
              CASE_AT(VALTYPE(xElement), "NC", ;
                      { 0, xElement, ;
                           iif(IS_CHAR(xElement),LEN(xElement),0) }) }, ;
         nStartIndex, nEndIndex - nStartIndex + 1)

   RETURN nSumTotal                   // FT_ASum
