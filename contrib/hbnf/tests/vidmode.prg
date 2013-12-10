#require "hbnf"

PROCEDURE Main( cMode )

   ft_SetMode( Val( cMode ) )
   ? "Video mode is:", ft_GetMode()

   RETURN
