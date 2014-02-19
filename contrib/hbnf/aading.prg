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

   LOCAL element, bScanCode
   LOCAL aNewArray := AClone( aList1 )

   __defaultNIL( @lCaseSens, .T. )
   __defaultNIL( @lTrimmer, .T. )

   // Assign code blocks according to case sensitivity and trim.
   IF lCaseSens
      IF lTrimmer
         bScanCode := {| x | AllTrim( x ) == AllTrim( element ) }
      ELSE
         bScanCode := {| x | x == element }
      ENDIF
   ELSE // Ignore case
      IF lTrimmer
         bScanCode := {| x | Upper( AllTrim( x ) ) == Upper( AllTrim( element ) ) }
      ELSE
         bScanCode := {| x | Upper( x ) == Upper( element ) }
      ENDIF
   ENDIF

   // Add the unique elements of aList2 to aList1.
   FOR EACH element IN aList2
      // If unique, then add element to new array.
      IF AScan( aList1, bScanCode ) == 0
         AAdd( aNewArray, element )
      ENDIF
   NEXT

   RETURN aNewArray
