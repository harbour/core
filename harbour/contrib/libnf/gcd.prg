/*
 * File......: GCD.PRG
 * Author....: David Husnian
 * CIS ID....: ?
 *
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:03:40   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:51:56   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:01:26   GLENN
 * Nanforum Toolkit
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_GCD()
 *  $CATEGORY$
 *     Math
 *  $ONELINER$
 *     Calculate greatest common divisor of two numbers
 *  $SYNTAX$
 *     FT_GCD( <nNumber1>, <nNumber2> ) -> nGCD
 *  $ARGUMENTS$
 *     <nNumber1> is the first number to find the GCD of.
 *
 *     <nNumber2> is the second number to find the GCD of.
 *  $RETURNS$
 *     The greatest common divisor of the 2 numbers, or 0 if either is 0.
 *  $DESCRIPTION$
 *   This function calculates the greatest common divisor between 2 numbers,
 *   i.e., the largest number that will divide into both numbers evenly.  It
 *   will return zero (0) if either number is zero.
 *  $EXAMPLES$
 *     ? FT_GCD(10,15)                  // Result: 5
 *     ? FT_GCD(108,54)                 // Result: 54
 *     ? FT_GCD(102,54)                 // Result: 6
 *     ? FT_GCD(111,17)                 // Result: 1
 *  $END$
 */


#command    REPEAT ;
            => ;
            DO WHILE .T.

#command    UNTIL <Condition> ;
            => ;
            IF <Condition> ; EXIT ; END ; END

#ifdef FT_TEST
  FUNCTION MAIN( cNum1, cNum2 )
     RETURN OUTSTD( STR(FT_GCD( val(cNum1), val(cNum2) )) + CHR(13) + CHR(10) )
#endif

FUNCTION FT_GCD(nNumber1, nNumber2)

   LOCAL nHold1, ;                      // Temporarily Hold the Maximum Number
         nHold2, ;                      // Temporarily Hold the Minimum Number
         nResult                        // GCD

                                        // Either Number Zero??
   IF (nNumber1 == 0 .OR. nNumber2 == 0)
      nResult := 0                      // Yes, Can't Have a GCD
   ELSE                                 // No, Calculate the GCD

      nHold1 := MAX(ABS(nNumber1), ABS(nNumber2))
      nHold2 := MIN(ABS(nNumber1), ABS(nNumber2))

      REPEAT

         nResult := nHold1 % nHold2     // Get the Remainder
         nHold1  := nHold2              // Which Makes a New Maximum Number
         nHold2  := nResult             // and it's the Minimum Number

      UNTIL nResult <= 0

      nResult := nHold1                 // Maximum Number Should Be the Answer

   ENDIF                                // nNumber1 == 0 or nNumber2 == 0
   RETURN (nResult)                     // FT_GCD
