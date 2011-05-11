/*
 * $Id$
 */

/*
 * File......: round.prg
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

#define IS_NEGATIVE(x)       ((x) < 0)

#define NEAREST_DECIMAL      "D"
#define NEAREST_FRACTION     "F"
#define NEAREST_WHOLE_NUMBER "W"
#define ROUND_DOWN           "D"
#define ROUND_NORMAL         "N"
#define ROUND_UP             "U"

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := iif(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := iif(<ParamN> == NIL,<DefN>,<ParamN>)]

#command    DEFAULT <Param1> TO <Def1> IF NOT <Type1> ;
                 [, <ParamN> TO <DefN> IF NOT <TypeN> ] ;
            => ;
            <Param1> := iif(VALTYPE(<Param1>) == <Type1>,<Param1>,<Def1>) ;
         [; <ParamN> := iif(VALTYPE(<ParamN>) == <TypeN>,<ParamN>,<DefN>)]

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

         CASE LEFT(cRoundDirection,1) == ROUND_DOWN

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

   RETURN nResult                       // FT_Round
