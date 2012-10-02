/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   LOCAL x, cUid

   ? "I am: [" + FT_NWUID() + "]"
   ? "---------------------"

   FOR x := 1 TO 100
      cUid := FT_NWUID( x )
      IF ! Empty( cUid )
         ? Str( x, 3 ) + Space( 3 ) + cUid
      ENDIF
   NEXT

   RETURN
