/*
 * File......: Round.Prg
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_ROUND()
 *  $CATEGORY$
 *     Math
 *  $ONELINER$
 *     Rounds a number to a specific place
 *  $SYNTAX$
 *     FT_ROUND( <nNumber> [, <nRoundToAmount>           ;
 *               [, <cRoundType>  [, <cRoundDirection>   ;
 *               [, <nAcceptableError> ] ] ] ] )            -> nNumber
 *  $ARGUMENTS$
 *     <nNumber> is the number to round
 *
 *     <nRoundToAmount> is the fraction to round to or the number of places,
 *     default is 2.
 *
 *     <cRoundType> is the type of rounding desired
 *
 *        "D" for Decimal       (3 for thousandth, 1/1000)  (default)
 *        "F" for Fraction      (3 for thirds, 1/3)
 *        "W" for Whole numbers (3 for thousand, 1000)
 *
 *     <cRoundDirection> is the direction to round the number toward
 *
 *        "U" to round Up      1.31 ->  1.4
 *                            -1.31 -> -1.4
 *        "D" to round Down    1.36 ->  1.3
 *                            -1.36 -> -1.3
 *        "N" to round Normal  1.5  ->  2
 *                            -1.5  -> -2
 *                             1.49 ->  1
 *                            -1.49 -> -1
 *
 *     <nAcceptableError> is the amount that is considered acceptable
 *     to be within, i.e., if you're within this amount of the number
 *     you don't need to round
 *  $RETURNS$
 *     The number, rounded as specified.
 *  $DESCRIPTION$
 *     This function will allow you to round a number.  The following can
 *     be specified:
 *       a. Direction (up, down or normal - normal is 4/5 convention)
 *       b. Type (whole, decimal, fraction)
 *       c. Amount (100's, 5 decimals, 16th, etc.)
 *  $EXAMPLES$
 *     // round normal to 2 decimal places
 *     nDollars := FT_ROUND(nDollars)
 *
 *     // round normal to 6 decimal places
 *     nIntRate := FT_ROUND(nIntRate, 6)
 *
 *     // round to nearest thousands
 *     nPrice   := FT_ROUND(nPrice, 3, NEAREST_WHOLE_NUMBER)
 *
 *     // round Up to nearest third
 *     nAmount  := FT_ROUND(nAmount, 3, NEAREST_FRACTION, ROUND_UP)
 *
 *     // round down to 3 decimals Within .005
 *     nAvg     := FT_ROUND(nAvg, 3, , ROUND_DOWN, .005)
 *  $END$
 */


#define IS_NEGATIVE(x)       ((x) < 0)

#define NEAREST_DECIMAL      "D"
#define NEAREST_FRACTION     "F"
#define NEAREST_WHOLE_NUMBER "W"
#define ROUND_DOWN           "D"
#define ROUND_NORMAL         "N"
#define ROUND_UP             "U"

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := IF(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := IF(<ParamN> == NIL,<DefN>,<ParamN>)]

#command    DEFAULT <Param1> TO <Def1> IF NOT <Type1> ;
                 [, <ParamN> TO <DefN> IF NOT <TypeN> ] ;
            => ;
            <Param1> := IF(VALTYPE(<Param1>) == <Type1>,<Param1>,<Def1>) ;
         [; <ParamN> := IF(VALTYPE(<ParamN>) == <TypeN>,<ParamN>,<DefN>)]



FUNCTION FT_ROUND(nNumber, nRoundToAmount, cRoundType, cRoundDirection, ;
                  nAcceptableError)

   LOCAL nResult := ABS(nNumber)        // The Result of the Rounding

   DEFAULT nRoundToAmount   TO 2, ;
           cRoundType       TO NEAREST_DECIMAL, ;
           cRoundDirection  TO ROUND_NORMAL, ;
           nAcceptableError TO 1 / (nRoundToAmount ** 2)

                                        // Are We Rounding to the Nearest Whole
                                        // Number or to Zero Decimal Places??
   IF (LEFT(cRoundType,1) != NEAREST_WHOLE_NUMBER .AND. ;
       (nRoundToAmount := INT(nRoundToAmount)) != 0)

                                        // No, Are We Rounding to the Nearest
                                        // Decimal Place??
      IF (LEFT(cRoundType,1) == NEAREST_DECIMAL)

                                        // Yes, Convert to Nearest Fraction
         nRoundToAmount := 10 ** nRoundToAmount

      ENDIF                             // LEFT(cRoundType,1) == NEAREST_DECIMAL

                                        // Are We Already Within the Acceptable
                                        // Error Factor??
      IF (ABS(INT(nResult * nRoundToAmount) - (nResult * nRoundToAmount)) > ;
          nAcceptableError)
                                        // No, Are We Rounding Down??
         nResult -= IIF(LEFT(cRoundDirection,1) == ROUND_DOWN, ;
                                      ; // Yes, Make Downward Adjustment
                        1 / nRoundToAmount / 2, ;
                                      ; // Are We Rounding Up??
                        IIF(LEFT(cRoundDirection,1) == ROUND_UP , ;
                                      ; // Yes, Make Upward Adjustment
                            -1 / (nRoundToAmount) / 2, ;
                                      ; // No, Rounding Normal, No Adjustment
                            0))
                                        //Do the Actual Rounding
         nResult := INT((nRoundToAmount * nResult) + .5 + nAcceptableError) / ;
                    nRoundToAmount

      ENDIF                             // ABS(INT(nResult * nRoundToAmount) -
                                        //     (mResult * nRoundAmount)) >
                                        // nAcceptableError

   ELSE                                 // Yes, Round to Nearest Whole Number
                                        // or to Zero Places

      nRoundToAmount := MAX(nRoundToAmount, 1)

      DO CASE                           // Do "Whole" Rounding

         CASE LEFT(cRoundDirection,1) == ROUND_UP

            nResult := (INT(nResult / nRoundToAmount) * nRoundToAmount) + ;
                       nRoundToAmount

         CASE LEFT(cRoundDirection,1) = ROUND_DOWN

            nResult := INT(nResult / nRoundToAmount) * nRoundToAmount

         OTHERWISE                      // Round Normally

            nResult := INT((nResult + nRoundToAmount / 2) / nRoundToAmount) * ;
                       nRoundToAmount

      ENDCASE

   ENDIF                                // LEFT(cRoundType,1)!=NEAREST_WHOLE or
                                        // nRoundToAmount == 0
   IF IS_NEGATIVE(nNumber)              // Was the Number Negative??
      nResult := -nResult               // Yes, Make the Result Negative Also
   ENDIF                                // IS_NEGATIVE(nNumber)

   RETURN (nResult)                     // FT_Round
