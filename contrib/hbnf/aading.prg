/*
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

FUNCTION ft_AAddition( aList1, aList2, lTrimmer, lCaseSens )

   LOCAL nElement, bScanCode
   LOCAL aNewArray := AClone( aList1 )

   __defaultNIL( @lCaseSens, .T. )
   __defaultNIL( @lTrimmer, .T. )

   // Assign code blocks according to case sensitivity and trim.
   IF lCaseSens
      IF lTrimmer // Ignore spaces
         bScanCode := {| x | ;
            AllTrim( x ) == ;
            AllTrim( aList2[ nElement ] ) }
      ELSE
         bScanCode := {| x | x == ( aList2[ nElement ] ) }
      ENDIF
   ELSE // Ignore case
      IF lTrimmer // Ignore spaces
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
      // If unique, then add element to new array.
      IF AScan( aList1, bScanCode ) == 0
         AAdd( aNewArray, aList2[ nElement ] )
      ENDIF
   NEXT

   RETURN aNewArray
