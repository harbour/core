/*
 * $Id$
 */

// Testing Harbour file io features

PROCEDURE Main()

   LOCAL h
   LOCAL cstr := " "
   LOCAL ntmp := 1

   h := FCreate( "test.txt" )
   ? "create handle", h

   FWrite( h, "This test worked if you can see this" )

   FClose( h )

   h := FOpen( "test.txt" )
   ? "open handle", h
   ?
   /* try to read what is there */
   DO WHILE ntmp != 0
      ntmp := FRead( h, @cstr, 1 )
      IF ntmp > 0
         ?? cstr
      ENDIF
   ENDDO
   ?

   FClose( h )

   RETURN
