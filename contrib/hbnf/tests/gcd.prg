#require "hbnf"

PROCEDURE Main( cNum1, cNum2 )

   ? ft_GCD( ;
      Val( hb_defaultValue( cNum1, hb_ntos( 108 ) ) ), ;
      Val( hb_defaultValue( cNum2, hb_ntos( 54 ) ) ) )

   RETURN
