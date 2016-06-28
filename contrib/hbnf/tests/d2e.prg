#require "hbnf"

PROCEDURE Main( cNum, cPrec )

   ? ft_D2E( ;
      Val( hb_defaultValue( cNum, hb_ntos( 0.12345 ) ) ), ;
      Val( hb_defaultValue( cPrec, hb_ntos( 6 ) ) ) )

   RETURN
