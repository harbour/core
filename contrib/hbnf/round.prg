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

   LOCAL nResult := Abs( nNumber )        // The result of the rounding

   __defaultNIL( @nRoundToAmount, 2 )
   __defaultNIL( @cRoundType, NEAREST_DECIMAL )
   __defaultNIL( @cRoundDirection, ROUND_NORMAL )
   __defaultNIL( @nAcceptableError, 1 / ( nRoundToAmount ^ 2 ) )

   // Are we rounding to the nearest whole number or to zero decimal places?
   IF ! hb_LeftEq( cRoundType, NEAREST_WHOLE_NUMBER ) .AND. ;
      ( nRoundToAmount := Int( nRoundToAmount ) ) != 0

      // No, are we rounding to the nearest decimal place?
      IF hb_LeftEq( cRoundType, NEAREST_DECIMAL )
         // Yes, convert to nearest fraction
         nRoundToAmount := 10 ^ nRoundToAmount
      ENDIF

      // Are we already within the acceptable error factor?
      IF Abs( Int( nResult * nRoundToAmount ) - ( nResult * nRoundToAmount ) ) > nAcceptableError
         // No, are we rounding down?
         nResult -= iif( hb_LeftEq( cRoundDirection, ROUND_DOWN ), ;
            ; // Yes, Make Downward Adjustment
         1 / nRoundToAmount / 2, ;
            ; // Are we rounding up?
         iif( hb_LeftEq( cRoundDirection, ROUND_UP ), ;
            ; // Yes, make upward adjustment
         -1 / nRoundToAmount / 2, ;
            ; // No, rounding normal, no adjustment
         0 ) )
         // Do the Actual Rounding
         nResult := Int( ( nRoundToAmount * nResult ) + .5 + nAcceptableError ) / ;
            nRoundToAmount
      ENDIF
   ELSE
      // Yes, round to nearest whole number or to zero places
      nRoundToAmount := Max( nRoundToAmount, 1 )

      // Do "whole" rounding
      DO CASE
      CASE hb_LeftEq( cRoundDirection, ROUND_UP )
         nResult := ( Int( nResult / nRoundToAmount ) * nRoundToAmount ) + nRoundToAmount
      CASE hb_LeftEq( cRoundDirection, ROUND_DOWN )
         nResult := Int( nResult / nRoundToAmount ) * nRoundToAmount
      OTHERWISE  // Round normally
         nResult := Int( ( nResult + nRoundToAmount / 2 ) / nRoundToAmount ) * nRoundToAmount
      ENDCASE
   ENDIF

   IF nNumber < 0                       // Was the number negative?
      nResult := -nResult               // Yes, make the result negative also
   ENDIF

   RETURN nResult
