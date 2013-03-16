
#require "hbnf"

PROCEDURE Main( cNum, cPrec )

   __defaultNIL( @cPrec, "6" )

   ? ft_D2E( Val( cNum ), Val( cPrec ) )

   RETURN
