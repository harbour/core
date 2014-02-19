#require "hbnf"

PROCEDURE Main( cMode )

   IF cMode == NIL
      cMode := "1"
   ENDIF

   ft_SetMode( Val( cMode ) )
   ? "Video mode is:", ft_GetMode()

   RETURN
