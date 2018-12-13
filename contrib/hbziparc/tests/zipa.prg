#require "hbziparc"

PROCEDURE Main( cZip, ... )

   LOCAL a, b, c

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   ? hb_ZipFile( cZip, hb_AParams() )

   a := hb_GetFilesInZip( cZip, .T. )

   FOR EACH b IN a
      ?
      FOR EACH c IN b
         ?? c, ""
      NEXT
   NEXT

   RETURN
