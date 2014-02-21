/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm jsd-llc com>
 */

// Testing Harbour long string handling with device output

PROCEDURE Main()

   LOCAL cShort := "1234567890"
   LOCAL i, cLong, cBuffer

   // Create an 80 KB string (Clipper is limited to 64 KB).
   cLong := cShort
   FOR i := 1 TO 13
      cLong += cLong
   NEXT

   // Write the long string to a file
   hb_MemoWrit( "longdev.prn", cLong )

   // Confirm the string length and that a copy is exactly identical.
   ? "The length of the long string is", iif( Len( cLong ) == 80 * 1024, "correct", "wrong" )
   cBuffer := cLong
   ? "The length of a copy of the long string is", iif( Len( cLong ) == 80 * 1024, "correct", "wrong" )
   ? "The copy of the long string is", iif( cLong == cBuffer, "equal", "not equal" ), "to the long string"

   // Read the string back in and compare it to the original.
   cBuffer := hb_BLeft( hb_MemoRead( "longdev.prn" ), 90000 )
   ? "Original:", hb_ntos( Len( cLong ) )
   ? "From file:", hb_ntos( Len( cBuffer ) )
   ? "The strings are", iif( cLong == cBuffer, "equal", "not equal" )

   FErase( "longdev.prn" )

   RETURN
