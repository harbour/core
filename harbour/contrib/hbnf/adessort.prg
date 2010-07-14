/*
 * $Id$
 */

/*
 * File......: adessort.prg
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

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := iif(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := iif(<ParamN> == NIL,<DefN>,<ParamN>)]

#command    DEFAULT <Param1> TO <Def1> IF NOT <Type1> ;
                 [, <ParamN> TO <DefN> IF NOT <TypeN> ] ;
            => ;
            <Param1> := iif(VALTYPE(<Param1>) == <Type1>,<Param1>,<Def1>) ;
         [; <ParamN> := iif(VALTYPE(<ParamN>) == <TypeN>,<ParamN>,<DefN>)]

#define FORCE_BETWEEN(x,y,z)         (y := MAX(MIN(y,z),x))

FUNCTION FT_ADESSORT(aArray, nStartIndex, nEndIndex)

   DEFAULT nStartIndex TO 1, ;
           nEndIndex   TO LEN(aArray)

                                        // Make Sure Bounds are in Range
   FORCE_BETWEEN(1, nEndIndex,   LEN(aArray))
   FORCE_BETWEEN(1, nStartIndex, nEndIndex)

   RETURN (ASORT(aArray, nStartIndex, nEndIndex, ;
                 { | xElement1, xElement2 | xElement1 > xElement2 } ))
