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

   LOCAL nHold1                         // Temporarily hold the maximum number
   LOCAL nHold2                         // Temporarily hold the minimum number
   LOCAL nResult                        // GCD

   // Either number zero?
   IF nNumber1 == 0 .OR. nNumber2 == 0
      nResult := 0                      // Yes, can't have a GCD
   ELSE                                 // No, calculate the GCD

      nHold1 := Max( Abs( nNumber1 ), Abs( nNumber2 ) )
      nHold2 := Min( Abs( nNumber1 ), Abs( nNumber2 ) )

      DO WHILE .T.

         nResult := nHold1 % nHold2     // Get the remainder
         nHold1  := nHold2              // Which makes a new maximum number
         nHold2  := nResult             // and it's the minimum number

         IF nResult <= 0
            EXIT
         ENDIF
      ENDDO

      nResult := nHold1                 // Maximum number should be the answer
   ENDIF

   RETURN nResult
