/*
 * File......: AAvg.Prg
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_AAVG()
 *  $CATEGORY$
 *     Array
 *  $ONELINER$
 *     Average numeric values in an array
 *  $SYNTAX$
 *     FT_AAVG( <aArray> [, <nStartIndex> [, <nEndIndex> ] ] ) -> nAverage
 *  $ARGUMENTS$
 *     <aArray> is the array containing the elements to be averaged.
 *
 *     <nStartIndex> is the first array item to include,
 *     defaults to first element.
 *
 *     <nEndIndex> is the last array element to include,
 *     defaults to all elements.
 *  $RETURNS$
 *     The average of the specified array elements.
 *  $DESCRIPTION$
 *     This function is used to get a numeric average of selected or all
 *     elements of an array.
 *
 *     This routine requires FT_ASUM().
 *  $EXAMPLES$
 *     FT_AAVG(aSubTotals)          // Get Average of Entire Array
 *
 *     FT_AAVG(aSubTotals, 5)       // Get Average of 5th Element On
 *
 *     FT_AAVG(aSubTotals, , 10)    // Get Average of 1st 10 Elements
 *
 *     FT_AAVG(aSubTotals, 5, 10)   // Get Average of Elements 5-10
 *  $END$
 */

#define FORCE_BETWEEN(x,y,z)         (y := MAX(MIN(y,z),x))
#define IS_NOT_ARRAY(x)              (VALTYPE(x) != "A")

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := IF(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := IF(<ParamN> == NIL,<DefN>,<ParamN>)]


         
FUNCTION FT_AAVG(aArray, nStartIndex, nEndIndex)

   DEFAULT nStartIndex TO 1, ;
           nEndIndex   TO LEN(aArray)

   // Make Sure Bounds are in Range

   FORCE_BETWEEN(1, nEndIndex,   LEN(aArray))
   FORCE_BETWEEN(1, nStartIndex, nEndIndex)

   RETURN (IF(IS_NOT_ARRAY(aArray) .OR. LEN(aArray) == 0, ;
              0, ;
              FT_ASUM(aArray, nStartIndex, nEndIndex) / ;
                 (nEndIndex - nStartIndex + 1)))
