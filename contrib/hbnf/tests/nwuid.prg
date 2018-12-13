#require "hbnf"

PROCEDURE Main()

   LOCAL x, cUid

   ? "I am: [" + ft_NWUID() + "]"
   ? "---------------------"

   FOR x := 1 TO 100
      cUid := ft_NWUID( x )
      IF ! Empty( cUid )
         ? Str( x, 3 ) + Space( 3 ) + cUid
      ENDIF
   NEXT

   RETURN
