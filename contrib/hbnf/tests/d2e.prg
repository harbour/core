#require "hbnf"

PROCEDURE Main( cNum, cPrec )

   ? ft_D2E( Val( hb_defaultValue( cNum, "" ) ), Val( hb_defaultValue( cPrec, "6" ) ) )

   RETURN
