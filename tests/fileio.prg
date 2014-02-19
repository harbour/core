// Testing Harbour file I/O features

PROCEDURE Main()

   LOCAL h
   LOCAL cStr := Space( 1 )
   LOCAL tmp

   ? "create handle", h := FCreate( "test.txt" )

   FWrite( h, "This test worked if you can see this" )

   FClose( h )

   /* using FRead() */

   ? "open handle", h := FOpen( "test.txt" )
   ?
   /* try to read what is there */
   DO WHILE ( tmp := FRead( h, @cStr, Len( cStr ) ) ) != 0
      ?? cStr
   ENDDO

   FClose( h )

   /* using FReadStr() */

   ? "open handle", h := FOpen( "test.txt" )
   ?
   /* try to read what is there */
   DO WHILE Asc( cStr := FReadStr( h, 1 ) ) != 0
      ?? cStr
   ENDDO

   FClose( h )

   RETURN
