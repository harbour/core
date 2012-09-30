/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL x, cUid

   QOut( "I am: [" + FT_NWUID() + "]" )
   QOut( "---------------------" )

   FOR x := 1 TO 100
      cUid := FT_NWUID( x )
      IF ! Empty( cUid )
         QOut( Str( x, 3 ) + Space( 3 ) + cUid )
      ENDIF
   NEXT

   RETURN
