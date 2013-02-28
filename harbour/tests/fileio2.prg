/*
 * $Id$
 */

// Testing Harbour file io features
// using FReadStr() instead of FRead()

PROCEDURE Main()

   LOCAL h
   LOCAL cstr := " "

   h := FCreate( "test.txt" )
   ? "create handle", h

   FWrite( h, "This test worked if you can see this" )

   FClose( h )

   h := FOpen( "test.txt" )
   ? "open handle", h
   ?
   /* try to read what is there */
   DO WHILE Asc( cstr ) != 0
      cstr := FReadStr( h, 1 )
      ?? cstr
   ENDDO

   FClose( h )

   RETURN
