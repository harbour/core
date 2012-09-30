/*
 * $Id$
 */

// Testing Harbour file io features
// using freadstr instead of fread

PROCEDURE Main()

   LOCAL h
   LOCAL cstr := " "

   h := FCreate( "test.txt" )
   QOut( "create handle", h )

   FWrite( h, "This test worked if you can see this" )

   FClose( h )

   h := FOpen( "test.txt" )
   QOut( "open handle", h )
   QOut()
   /* try to read what is there */
   DO WHILE Asc( cstr ) != 0
      cstr := FReadStr( h, 1 )
      QQOut( cstr )
   ENDDO

   FClose( h )

   RETURN
