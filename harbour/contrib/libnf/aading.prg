/*
 * File......: Aadding.prg
 * Author....: Ralph Oliver,  TRANSCOM SYSTEMS
 * CIS ID....: 74030,703
 *
 * This is an original work by Ralph Oliver and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.1   15 Aug 1991 23:05:40   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   07 Jun 1991 23:03:08   GLENN
 * Initial revision.
 *
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_AADDITION()
 *  $CATEGORY$
 *     Array
 *  $ONELINER$
 *     Add elements unique of source array to target array
 *  $SYNTAX$
 *     FT_AADDITION( <aList1>, <aList2> [, <lTrimmer> [, <lCaseSens> ] ] ) ;
 *             -> aNewArray
 *  $ARGUMENTS$
 *     <aList1> is the primary array.
 *
 *     <aList2> is the secondary array.
 *
 *     <lTrimmer> is a logical value denoting whether leading or
 *             trailing spaces should be included in the
 *             comparison. If .T., then ignores spaces in
 *             comparison, defaults to .T., .F. includes spaces.
 *
 *     <lCaseSens> is a logical value denoting case sensitivity.
 *             If .T., then comparison is sensitive to case,
 *             defaults to .T., .F. ignores case.
 *  $RETURNS$
 *     An array of the union of aList1 and aList2.
 *  $DESCRIPTION$
 *     This function will add the elements unique of aList2 with aList1.
 *     It returns a new array including all the elements of aList1
 *     plus the unique elements of aList2.
 *  $EXAMPLES$
 *     aList1 := {"apple", "orange", "pear"}
 *     aList2 := {"apple ", "banana", "PEAR"}
 *
 *     FT_AADDITION( aList1, aList2 )
 *          // ignores spaces, sensitive to case
 *          // returns {"apple","orange","pear","banana","PEAR"}
 *
 *     FT_AADDITION( aList1, aList2, , .F. )
 *          // ignores spaces, not sensitive to case
 *          // returns {"apple","orange","pear","banana"}
 *
 *     FT_AADDITION( aList1, aList2, .F., .F. )
 *          // sensitive to spaces, not sensitive to case
 *          // returns {"apple","orange","pear","apple ","banana"}
 *  $END$
 */

#ifdef FT_TEST

FUNCTION MAIN()
   LOCAL aList1,aList2,var0,nstart,nstop,nelapsed,nCtr
   CLS
   ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AADDITION"
   ?
   aList1 := {"apple", "orange", "pear"}
   aList2 := {"apple ", "banana", "PEAR"}
   ? "aList1 : "
   AEVAL( aList1, { |x| QQOUT(x + ",") } )
   ?
   ? "aList2 : "
   AEVAL( aList2, { |x| QQOUT(x + ",") } )
   ?

   nstart := SECONDS()
   FOR nCtr := 1 to 100
      var0 := FT_AADDITION( aList1, aList2 )
   NEXT
   nstop := SECONDS()
   nelapsed := nstop - nstart
   ? "time for 100 merges:", nelapsed

   ? PADR("FT_AADDITION( aList1, aList2 ) ->",44)
   AEVAL( var0, { |x| QQOUT(x + ",") } )
   ?
   var0 := FT_AADDITION( aList1, aList2, , .F. )
   ? PADR("FT_AADDITION( aList1, aList2, , .F. ) ->",44)
   AEVAL( var0, { |x| QQOUT(x + ",") } )
   ?
   var0 := FT_AADDITION( aList1, aList2, .F., .F. )
   ? PADR("FT_AADDITION( aList1, aList2, .F., .F. ) ->",44)
   AEVAL( var0, { |x| QQOUT(x + ",") } )
   ?
   RETURN NIL

#endif


FUNCTION FT_AADDITION( aList1, aList2, lTrimmer, lCaseSens )

   LOCAL nElement, nPos, bScanCode
   LOCAL aNewArray := ACLONE( aList1 )

   // Set default parameters as necessary.
   IF lCaseSens == NIL
      lCaseSens := .T.
   ENDIF

   IF lTrimmer == NIL
      lTrimmer := .T.
   ENDIF

   // Assign code blocks according to case sensitivity and trim.
   IF lCaseSens

      IF lTrimmer                         // Ignore spaces.
         bScanCode := { |x| ;
                        ALLTRIM( x ) == ;
                        ALLTRIM( aList2[ nElement ]) }
      ELSE
         bScanCode := { |x| ( aList2[ nElement ]) }
      ENDIF

   ELSE                                   // Ignore case.

      IF lTrimmer                         // Ignore spaces.
         bScanCode := { |x| ;
                        UPPER( ALLTRIM( x )) == ;
                        UPPER( ALLTRIM( aList2[ nElement ] )) }
      ELSE
         bScanCode := { |x| ;
                        UPPER( x ) == ;
                        UPPER( aList2[ nElement ] ) }
      ENDIF
   ENDIF


   // Add the unique elements of aList2 to aList1.
   FOR nElement := 1 TO LEN( aList2 )

      nPos := ASCAN( aList1, bScanCode )

      // If unique, then add element to new array.
      IF nPos = 0
         AADD( aNewArray, aList2[ nElement ] )
      ENDIF

   NEXT

   RETURN ( aNewArray )
