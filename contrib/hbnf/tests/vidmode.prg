#require "hbnf"

PROCEDURE Main( cMode )

   ft_SetMode( Val( hb_defaultValue( cMode, hb_ntos( 1 ) ) ) )
   ? "Video mode is:", ft_GetMode()

   RETURN
