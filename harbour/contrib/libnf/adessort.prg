/*
 * File......: ADesSort.Prg
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_ADESSORT()
 *  $CATEGORY$
 *     Array
 *  $ONELINER$
 *     Sort an array in descending order
 *  $SYNTAX$
 *     FT_ADESSORT( <aArray> [, <nStartIndex> [, <nEndIndex> ] ] ) -> aSorted
 *  $ARGUMENTS$
 *     <aArray> is the array to be sorted
 *
 *     <nStartIndex> is the first array item to include in the sort,
 *     defaults to first element
 *
 *     <nEndIndex> is the last array element to include in the sort,
 *     defaults to all elements
 *  $RETURNS$
 *     The array, sorted in descending order.
 *  $DESCRIPTION$
 *     This function is used to sort an array in descending order, i.e., Z-A
 *  $EXAMPLES$
 *     FT_ADESSORT(aNames)               // Sort the Entire Array
 *
 *     FT_ADESSORT(aNames, 5)            // Sort from the 5th Element On
 *
 *     FT_ADESSORT(aNames, , 10)         // Sort the 1st 10 Elements
 *
 *     FT_ADESSORT(aNames, 5, 10)        // Sort Elements 5-10
 *  $END$
 */


#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := IF(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := IF(<ParamN> == NIL,<DefN>,<ParamN>)]

#command    DEFAULT <Param1> TO <Def1> IF NOT <Type1> ;
                 [, <ParamN> TO <DefN> IF NOT <TypeN> ] ;
            => ;
            <Param1> := IF(VALTYPE(<Param1>) == <Type1>,<Param1>,<Def1>) ;
         [; <ParamN> := IF(VALTYPE(<ParamN>) == <TypeN>,<ParamN>,<DefN>)]


#define FORCE_BETWEEN(x,y,z)         (y := MAX(MIN(y,z),x))

FUNCTION FT_ADESSORT(aArray, nStartIndex, nEndIndex)

   DEFAULT nStartIndex TO 1, ;
           nEndIndex   TO LEN(aArray)

                                        // Make Sure Bounds are in Range
   FORCE_BETWEEN(1, nEndIndex,   LEN(aArray))
   FORCE_BETWEEN(1, nStartIndex, nEndIndex)

   RETURN (ASORT(aArray, nStartIndex, nEndIndex, ;
                 { | xElement1, xElement2 | xElement1 > xElement2 } ))
