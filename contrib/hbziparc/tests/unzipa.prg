#require "hbziparc"

PROCEDURE Main( cZip )

   ? hb_UnzipFile( cZip,, .F.,,, hb_AParams(), {| x, y | QOut( Str( x / y * 100, 3 ) + "%" ) } )

   RETURN
