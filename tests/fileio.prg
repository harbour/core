// Testing Harbour file I/O features

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   LOCAL h
   LOCAL cStr
   LOCAL tmp

   ? "create handle", h := FCreate( "test.txt" )
   FWrite( h, "This test worked if you can see this" )
   FClose( h )

   /* using FRead() */

   ? "open handle", h := FOpen( "test.txt" )
   ?
   /* try to read what is there */
   cStr := Space( 1 )
   DO WHILE ( tmp := FRead( h, @cStr, hb_BLen( cStr ) ) ) != 0
      ?? cStr
   ENDDO

   FClose( h )

   /* using FReadStr() */

   ? "open handle", h := FOpen( "test.txt" )
   ?
   /* try to read what is there */
   DO WHILE hb_BCode( cStr := FReadStr( h, 1 ) ) != 0
      ?? cStr
   ENDDO

   FClose( h )

   RETURN
