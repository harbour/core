/*
 * $Id$
 */

/*
 * Author....: David Husnian
 * CIS ID....: ?
 *
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:05:30   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:52:48   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:08   GLENN
 * Nanforum Toolkit
 *
 */

#define NEAREST_DECIMAL      "D"
#define NEAREST_FRACTION     "F"
#define NEAREST_WHOLE_NUMBER "W"
#define ROUND_DOWN           "D"
#define ROUND_NORMAL         "N"
#define ROUND_UP             "U"

FUNCTION ft_Round( nNumber, nRoundToAmount, cRoundType, cRoundDirection, ;
      nAcceptableError )

   LOCAL nResult := Abs( nNumber )        // The Result of the Rounding

   __defaultNIL( @nRoundToAmount, 2 )
   __defaultNIL( @cRoundType, NEAREST_DECIMAL )
   __defaultNIL( @cRoundDirection, ROUND_NORMAL )
   __defaultNIL( @nAcceptableError, 1 / ( nRoundToAmount ** 2 ) )

   // Are We Rounding to the Nearest Whole
   // Number or to Zero Decimal Places??
   IF !( Left( cRoundType, 1 ) == NEAREST_WHOLE_NUMBER ) .AND. ;
      ( nRoundToAmount := Int( nRoundToAmount ) ) != 0

      // No, Are We Rounding to the Nearest
      // Decimal Place??
      IF Left( cRoundType, 1 ) == NEAREST_DECIMAL

         // Yes, Convert to Nearest Fraction
         nRoundToAmount := 10 ** nRoundToAmount

      ENDIF

      // Are We Already Within the Acceptable
      // Error Factor??
      IF Abs( Int( nResult * nRoundToAmount ) - ( nResult * nRoundToAmount ) ) > nAcceptableError
         // No, Are We Rounding Down??
         nResult -= iif( Left( cRoundDirection, 1 ) == ROUND_DOWN, ;
            ; // Yes, Make Downward Adjustment
         1 / nRoundToAmount / 2, ;
            ; // Are We Rounding Up??
         iif( Left( cRoundDirection, 1 ) == ROUND_UP, ;
            ; // Yes, Make Upward Adjustment
         -1 / nRoundToAmount / 2, ;
            ; // No, Rounding Normal, No Adjustment
         0 ) )
         // Do the Actual Rounding
         nResult := Int( ( nRoundToAmount * nResult ) + .5 + nAcceptableError ) / ;
            nRoundToAmount

      ENDIF

   ELSE
      // Yes, Round to Nearest Whole Number
      // or to Zero Places

      nRoundToAmount := Max( nRoundToAmount, 1 )

      DO CASE                           // Do "Whole" Rounding

      CASE Left( cRoundDirection, 1 ) == ROUND_UP

         nResult := ( Int( nResult / nRoundToAmount ) * nRoundToAmount ) + ;
            nRoundToAmount

      CASE Left( cRoundDirection, 1 ) == ROUND_DOWN

         nResult := Int( nResult / nRoundToAmount ) * nRoundToAmount

      OTHERWISE                         // Round Normally

         nResult := Int( ( nResult + nRoundToAmount / 2 ) / nRoundToAmount ) * ;
            nRoundToAmount

      ENDCASE

   ENDIF

   IF nNumber < 0                       // Was the Number Negative??
      nResult := -nResult               // Yes, Make the Result Negative Also
   ENDIF

   RETURN nResult
