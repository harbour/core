#require "hbziparc"

PROCEDURE Main( cZip )

   LOCAL d, f

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   ? hb_ZipFile( cZip, hb_AParams(),,,,, .T. )

   FOR EACH f IN hb_GetFilesInZip( cZip, .T. )
      ?
      FOR EACH d IN f
         ?? d, ""
      NEXT
   NEXT

   RETURN
