/*
 * $Id$
 */

/*
 * File......: aadding.prg
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

#ifdef FT_TEST

PROCEDURE Main()

   LOCAL aList1, aList2, var0, nstart, nstop, nelapsed, nCtr

   CLS
   ? "TEST TO DEMONSTRATE EXAMPLES OF FT_AADDITION"
   ?
   aList1 := { "apple", "orange", "pear" }
   aList2 := { "apple ", "banana", "PEAR" }
   ? "aList1 : "
   AEval( aList1, {| x | QQOut( x + "," ) } )
   ?
   ? "aList2 : "
   AEval( aList2, {| x | QQOut( x + "," ) } )
   ?

   nstart := Seconds()
   FOR nCtr := 1 TO 100
      var0 := FT_AADDITION( aList1, aList2 )
   NEXT
   nstop := Seconds()
   nelapsed := nstop - nstart
   ? "time for 100 merges:", nelapsed

   ? PadR( "FT_AADDITION( aList1, aList2 ) ->", 44 )
   AEval( var0, {| x | QQOut( x + "," ) } )
   ?
   var0 := FT_AADDITION( aList1, aList2, , .F. )
   ? PadR( "FT_AADDITION( aList1, aList2, , .F. ) ->", 44 )
   AEval( var0, {| x | QQOut( x + "," ) } )
   ?
   var0 := FT_AADDITION( aList1, aList2, .F. , .F. )
   ? PadR( "FT_AADDITION( aList1, aList2, .F., .F. ) ->", 44 )
   AEval( var0, {| x | QQOut( x + "," ) } )
   ?

   RETURN

#endif

FUNCTION FT_AADDITION( aList1, aList2, lTrimmer, lCaseSens )

   LOCAL nElement, nPos, bScanCode
   LOCAL aNewArray := AClone( aList1 )

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
         bScanCode := {| x | ;
            AllTrim( x ) == ;
            AllTrim( aList2[ nElement ] ) }
      ELSE
         bScanCode := {| x | x == ( aList2[ nElement ] ) }
      ENDIF

   ELSE                                   // Ignore case.

      IF lTrimmer                         // Ignore spaces.
         bScanCode := {| x | ;
            Upper( AllTrim( x ) ) == ;
            Upper( AllTrim( aList2[ nElement ] ) ) }
      ELSE
         bScanCode := {| x | ;
            Upper( x ) == ;
            Upper( aList2[ nElement ] ) }
      ENDIF
   ENDIF

// Add the unique elements of aList2 to aList1.
   FOR nElement := 1 TO Len( aList2 )

      nPos := AScan( aList1, bScanCode )

      // If unique, then add element to new array.
      IF nPos == 0
         AAdd( aNewArray, aList2[ nElement ] )
      ENDIF

   NEXT

   RETURN aNewArray
