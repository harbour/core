/*
 * $Id$
 */

// Testing Harbour file io features

PROCEDURE Main()

   LOCAL h    := 0
   LOCAL cstr := " "
   LOCAL ntmp := 1

   h := FCreate( "test.txt" )
   QOut( "create handle", h )

   FWrite( h, "This test worked if you can see this" )

   FClose( h )

   h := FOpen( "test.txt" )
   QOut( "open handle", h )
   QOut()
   /* try to read what is there */
   DO WHILE ntmp != 0
      ntmp := FRead( h, @cstr, 1 )
      IF ntmp > 0
         QQOut( cstr )
      ENDIF
   ENDDO
   QOut()

   FClose( h )

   RETURN
