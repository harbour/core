/*
 * File......: ANoMatch.Prg
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_ANOMATCHES()
 *  $CATEGORY$
 *     Array
 *  $ONELINER$
 *     Find the number of array elements meeting a condition
 *  $SYNTAX$
 *     FT_ANOMATCHES( <aArray>, <bCompareBlock> ;
 *                    [, <nStartIndex> [, <nEndIndex> ] ] ) -> nNoOfMatches
 *  $ARGUMENTS$
 *     <aArray> is the array to be searched
 *
 *     <bCompareBlock> is a code block containing the expression for
 *     the array elements to be tested with.  Each element is passed
 *     as a parameter to the block.  If the block returns .T., the
 *     number of matches will be incremented by one.
 *
 *     <nStartIndex> is the first array item to include in the search,
 *     defaults to first element.
 *
 *     <nEndIndex> is the last array element to include in the search,
 *     defaults to all elements.
 *  $RETURNS$
 *     The number of elements that cause the code block to return .T.
 *  $DESCRIPTION$
 *     This function returns the number of array elements that, when passed
 *     to the supplied code block, cause that code block to return a .T. value.
 *  $EXAMPLES$
 *     // Search the Entire Array
 *     FT_ANOMATCHES(aTries, { | x | x <= 100 } )
 *
 *     // Search from the 5th Element On
 *     FT_ANOMATCHES(aCodes, { | x | UPPER(x) == cCurrentCode }, 5)
 *
 *     // Search the 1st 10 Elements
 *     FT_ANOMATCHES(aDates, { | x | IS_BETWEEN(DATE()-7,x,DATE() + 7) }, 10)
 *
 *     // Search Elements 5-10
 *     FT_ANOMATCHES(aNames, { | x | x <= cLastGoodName }, 5, 10)
 *  $END$
 */

#define FORCE_BETWEEN(x,y,z)         (y := MAX(MIN(y,z),x))

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := IF(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := IF(<ParamN> == NIL,<DefN>,<ParamN>)]

FUNCTION FT_ANOMATCHES(aArray, bCompareBlock, nStartIndex, nEndIndex)

   LOCAL nNoOfMatches := 0              // Number of Matches Found

   DEFAULT nStartIndex TO 1, ;
           nEndIndex   TO LEN(aArray)

                                        // Make Sure Bounds are in Range
   FORCE_BETWEEN(1, nEndIndex,   LEN(aArray))
   FORCE_BETWEEN(1, nStartIndex, nEndIndex)

   AEVAL(aArray, ;
         { | xElement | ;
           IIF(EVAL(bCompareBlock, xElement), nNoOfMatches++, NIL) }, ;
         nStartIndex, nEndIndex - nStartIndex + 1)

   RETURN (nNoOfMatches)                // FT_ANoMatches                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
