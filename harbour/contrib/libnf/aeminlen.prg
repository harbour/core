/*
 * File......: AEminlen.prg
 * Author....: Ralph Oliver,  TRANSCOM SYSTEMS
 * CIS ID....: 74030,703
 *
 * This is an original work by Ralph Oliver and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:02:28   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   07 Jun 1991 23:03:16   GLENN
 * Initial revision.
 *
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_AEMINLEN()
 *  $CATEGORY$
 *     Array
 *  $ONELINER$
 *     Find shortest element within an array
 *  $SYNTAX$
 *     FT_AEMINLEN( <aArray> [, <nDimension> [, <nStart> [, <nCount> ] ] ] )
 *                -> nMinlen
 *  $ARGUMENTS$
 *     <aArray> is the array containing the elements to be measured.
 *
 *     <nDimension> is the array dimension to be measured,
 *                defaults to first dimension.
 *
 *     <nStart> is the starting array element to include,
 *                defaults to first array element.
 *
 *     <nCount> is the number of array elements to process from
 *                from <nStart>, defaults to remaining elements
 *                in array.
 *  $RETURNS$
 *     The length of the shortest size element of an array.
 *  $DESCRIPTION$
 *     This function will measure each element of an array
 *     dimension and return the shortest element.
 *  $EXAMPLES$
 *     FT_AEMINLEN(aArray)       // Measure the 1st dimension of an Array
 *
 *     FT_AEMINLEN(aArray,2)     // Measure the 2nd dimension of an Array
 *
 *     FT_AEMINLEN(aArray,2,,9)  // Measure Elements 1-9 of 2nd dimension
 *
 *     FT_AEMINLEN(aArray,3,5,9) // Measure Elements 5-9 of 3rd dimension
 *
 *     FT_AEMINLEN(aArray,3,5)   // Measure Elements 5 to end of 3rd dimension
 *  $SEEALSO$
 *     FT_AEMAXLEN()
 *  $END$
 */

#ifdef FT_TEST

FUNCTION MAIN()
   LOCAL var0, myarray1 := DIRECTORY()
   CLS
   ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AEMINLEN"
   ?
   ? "myarray1 = DIRECTORY()"
   ?
   aEval( myarray1, {|v| qout( padr(v[1],12), v[2], v[3], v[4], v[5] ) } )
   var0 := FT_AEMINLEN( myarray1 )
   ? PADR('FT_AEMINLEN( myarray1 ) ->',30)
   ?? var0
   ?
   var0 := FT_AEMINLEN( myarray1,2 )
   ? PADR('FT_AEMINLEN( myarray1,2 ) ->',30)
   ?? var0
   ?
   ?
   var0 := FT_AEMINLEN( myarray1[2] )
   ? PADR('FT_AEMINLEN( myarray1[2] ) ->',30)
   ?? var0
   ?
   ?
   var0 := FT_AEMINLEN( myarray1,3 )
   ? PADR('FT_AEMINLEN( myarray1,3 ) ->',30)
   ?? var0
   ?
   RETURN NIL

#endif


FUNCTION FT_AEminlen( aArray, nDimension, nStart, nCount )

   LOCAL i, nLast, cType, nMinlen := 65519

   // Set default parameters as necessary.
   IF nDimension == NIL
      nDimension := 1
   ENDIF

   IF nStart == NIL
      nStart := 1
   ENDIF

   IF nCount == NIL
      nCount := LEN( aArray ) - nStart + 1
   ENDIF

   nLast := MIN( nStart +nCount -1, LEN( aArray ))

   FOR i := nStart TO nLast
      cType := VALTYPE( aArray[i] )
      DO CASE
         CASE ( cType == "C" )
            nMinlen := MIN( nMinlen, LEN( aArray[i] ))

         CASE ( cType == "A" )
            nMinlen := MIN( nMinlen, ;
               LEN( LTRIM( TRANSFORM( aArray[i] [nDimension], "@X" ))))

         OTHERWISE
            nMinlen := MIN( nMinlen, ;
               LEN( LTRIM( TRANSFORM( aArray[i], "@X" ))))

      ENDCASE
   NEXT

   RETURN ( nMinlen )

