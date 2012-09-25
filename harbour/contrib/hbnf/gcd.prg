/*
 * $Id$
 */

/*
 * File......: gcd.prg
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

#command    REPEAT ;
            => ;
            DO WHILE .T.

#command    UNTIL <Condition> ;
            => ;
            IF <Condition> ; EXIT ; END ; END

#ifdef FT_TEST
  PROCEDURE Main( cNum1, cNum2 )
     OUTSTD( STR(FT_GCD( val(cNum1), val(cNum2) )) + hb_eol() )
     RETURN
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
   RETURN nResult                       // FT_GCD
