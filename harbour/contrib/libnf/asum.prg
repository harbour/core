/*
 * File......: ASum.Prg
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_ASUM()
 *  $CATEGORY$
 *     Array
 *  $ONELINER$
 *     Sum the elements of an array
 *  $SYNTAX$
 *     FT_ASUM( <aArray> [, <nStartIndex> [, <nEndIndex> ] ] ) -> nSum
 *  $ARGUMENTS$
 *     <aArray> is the array containing the elements to be summed.
 *
 *     <nStartIndex> is the first array item to include,
 *     defaults to first element.
 *
 *     <nEndIndex> is the last array element to include,
 *     defaults to all elements.
 *  $RETURNS$
 *     The sum of the elements of the array or the lengths of the elements.
 *  $DESCRIPTION$
 *     This function is to sum the elements of a numeric array or to sum the
 *     lengths of a character array.
 *  $EXAMPLES$
 *
 *     FT_ASUM(aSubTotals)               // Sum the Entire Array
 *
 *     FT_ASUM(aSubTotals, 5)            // Sum from the 5th Element On
 *
 *     FT_ASUM(aSubTotals, , 10)         // Sum the 1st 10 Elements
 *
 *     FT_ASUM(aSubTotals, 5, 10)        // Sum Elements 5-10
 *  $END$
 */

#define CASE_AT(x,y,z)               z[AT(x,y)+1]
#define FORCE_BETWEEN(x,y,z)         (y := MAX(MIN(y,z),x))
#define IS_CHAR(x)                   (VALTYPE(x) == "C")

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := IF(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := IF(<ParamN> == NIL,<DefN>,<ParamN>)]


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
                           IF(IS_CHAR(xElement),LEN(xElement),0) }) }, ;
         nStartIndex, nEndIndex - nStartIndex + 1)

   RETURN (nSumTotal)                   // FT_ASum                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
