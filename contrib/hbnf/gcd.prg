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

FUNCTION ft_GCD( nNumber1, nNumber2 )

   LOCAL nHold1                      // Temporarily Hold the Maximum Number
   LOCAL nHold2                      // Temporarily Hold the Minimum Number
   LOCAL nResult                     // GCD

   // Either Number Zero??

   IF nNumber1 == 0 .OR. nNumber2 == 0
      nResult := 0                      // Yes, Can't Have a GCD
   ELSE                                 // No, Calculate the GCD

      nHold1 := Max( Abs( nNumber1 ), Abs( nNumber2 ) )
      nHold2 := Min( Abs( nNumber1 ), Abs( nNumber2 ) )

      DO WHILE .T.

         nResult := nHold1 % nHold2     // Get the Remainder
         nHold1  := nHold2              // Which Makes a New Maximum Number
         nHold2  := nResult             // and it's the Minimum Number

         IF nResult <= 0
            EXIT
         ENDIF
      ENDDO

      nResult := nHold1                 // Maximum Number Should Be the Answer

   ENDIF                                // nNumber1 == 0 or nNumber2 == 0

   RETURN nResult                       // FT_GCD
