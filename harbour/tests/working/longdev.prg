//
// $Id$
//

// Testing Harbour long string handling with device output.
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Copyright 1999 David G. Holm <dholm@jsd-llc.com>
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

FUNCTION Main()

   LOCAL cShort := "1234567890"
   LOCAL i, j, cLong, cBuffer, nHandle

   // Create an 80 KB string (Clipper is limited to 64 KB).
   cLong := cShort
   FOR i := 1 TO 13
      cLong += cLong
   NEXT

   // Write the long string to file long_str.prn
   SET PRINTER TO long_str
   SET DEVICE TO PRINTER
   DEVOUT( cLong )
   SET PRINTER OFF
   SET DEVICE TO SCREEN

   // Confirm the string length and that a copy is exactly identical.
   ? "The length of the long string is", IF( LEN( cLong ) == 80 * 1024, "correct", "wrong" )
   cBuffer := cLong
   ? "The length of a copy of the long string is", IF( LEN( cLong ) == 80 * 1024, "correct", "wrong" )
   ? "The copy of the long string is", IF( cLong == cBuffer, "equal", "not equal" ), "to the long string"

   // Read the string back in and compare it to the original.
   nHandle := FOPEN( "long_str.prn" )
   cBuffer := FREADSTR( nHandle, 90000 )
   ? "Original:", LEN( cLong )
   ? "From file:", LEN( cBuffer )
   ? "The strings are", IF( cLong == cBuffer, "equal", "not equal" )

return nil
