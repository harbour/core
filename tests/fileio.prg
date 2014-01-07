// Testing Harbour file I/O features

PROCEDURE Main()

   LOCAL h
   LOCAL cStr := Space( 1 )
   LOCAL tmp

   h := FCreate( "test.txt" )
   ? "create handle", h

   FWrite( h, "This test worked if you can see this" )

   FClose( h )

   /* using FRead() */

   h := FOpen( "test.txt" )
   ? "open handle", h
   ?
   /* try to read what is there */
   DO WHILE ( tmp := FRead( h, @cStr, Len( cStr ) ) ) != 0
      ?? cStr
   ENDDO

   FClose( h )

   /* using FReadStr() */

   h := FOpen( "test.txt" )
   ? "open handle", h
   ?
   /* try to read what is there */
   DO WHILE Asc( cStr := FReadStr( h, 1 ) ) != 0
      ?? cStr
   ENDDO

   FClose( h )

   RETURN
