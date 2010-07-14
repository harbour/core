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

#define FORCE_BETWEEN(x,y,z)         (y := MAX(MIN(y,z),x))
#define IS_NOT_ARRAY(x)              (VALTYPE(x) != "A")

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := iif(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := iif(<ParamN> == NIL,<DefN>,<ParamN>)]

FUNCTION FT_AAVG(aArray, nStartIndex, nEndIndex)

   DEFAULT nStartIndex TO 1, ;
           nEndIndex   TO LEN(aArray)

   // Make Sure Bounds are in Range

   FORCE_BETWEEN(1, nEndIndex,   LEN(aArray))
   FORCE_BETWEEN(1, nStartIndex, nEndIndex)

   RETURN iif(IS_NOT_ARRAY(aArray) .OR. LEN(aArray) == 0, ;
              0, ;
              FT_ASUM(aArray, nStartIndex, nEndIndex) / ;
                 (nEndIndex - nStartIndex + 1))
