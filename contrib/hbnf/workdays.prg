/* This is an original work of John Kaster and is placed in the public domain.

      Rev 1.2   07 Mar 1992 22:15:06   GLENN
   Mark K. Zechiel discovered a bug where the incorrect number of
   workdays was reported when <dStart> was a Tuesday through Friday and
   dStop was a multiple of 7 days away from dStart (i.e., 7, or 14, or
   21, etc).  Fixed.

      Rev 1.1   15 Aug 1991 23:05:48   GLENN
   Forest Belt proofread/edited/cleaned up doc

      Rev 1.0   12 Jun 1991 01:33:10   GLENN
   Initial revision.
 */

FUNCTION ft_Workdays( dStart, dStop )

   LOCAL nWorkDays := 0, nDays, nAdjust

   IF dStart != NIL .AND. dStop != NIL
      IF dStart != dStop
         IF dStart > dStop  // Swap the values
            nAdjust := dStop
            dStop   := dStart
            dStart  := nAdjust
         ENDIF

         DO CASE
         CASE ( nDays := DoW( dStart ) ) == 1  // Sunday (change to next Monday)
            dStart++
         CASE nDays == 7  // Saturday (change to next Monday)
            dStart += 2
         ENDCASE

         DO CASE
         CASE ( nDays := DoW( dStop ) ) == 1  // Sunday (change to prev Friday)
            dStop -= 2
         CASE nDays == 7  // Saturday (change to prev Friday)
            dStop--
         ENDCASE

         nAdjust := ( nDays := dStop - dStart + 1 ) % 7

         IF DoW( dStop ) + 1 < DoW( dStart )  // Weekend adjustment
            nAdjust -= 2
         ENDIF

         nWorkDays := Int( nDays / 7 ) * 5 + nAdjust

      ELSEIF DoW( dStart ) != 1 .AND. DoW( dStart ) != 7

         nWorkDays := 1

      ENDIF
   ENDIF

   RETURN Max( nWorkDays, 0 )
