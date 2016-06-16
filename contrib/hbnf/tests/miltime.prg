#require "hbnf"

PROCEDURE Main()

   LOCAL tmp

   ? "am-pm"
   ? tmp := ft_Civ2Mil( " 5:40 pm" ), ft_Mil2Civ( tmp )
   ? tmp := ft_Civ2Mil( "05:40 pm" ), ft_Mil2Civ( tmp )
   ? tmp := ft_Civ2Mil( " 5:40 PM" ), ft_Mil2Civ( tmp )
   ? tmp := ft_Civ2Mil( " 5:40 am" ), ft_Mil2Civ( tmp )
   ? tmp := ft_Civ2Mil( "05:40 am" ), ft_Mil2Civ( tmp )
   ? tmp := ft_Civ2Mil( " 5:40 AM" ), ft_Mil2Civ( tmp )
   ?

   ? "noon-midnight"
   ? tmp := ft_Civ2Mil( "12:00 m" ), ft_Mil2Civ( tmp )
   ? tmp := ft_Civ2Mil( "12:00 M" ), ft_Mil2Civ( tmp )
   ? tmp := ft_Civ2Mil( "12:00 m" ), ft_Mil2Civ( tmp )
   ? tmp := ft_Civ2Mil( "12:00 n" ), ft_Mil2Civ( tmp )
   ? tmp := ft_Civ2Mil( "12:00 N" ), ft_Mil2Civ( tmp )
   ? tmp := ft_Civ2Mil( "12:00 n" ), ft_Mil2Civ( tmp )
   ?

   ? "errors in noon-midnight"
   ? tmp := ft_Civ2Mil( "12:01 n" ), ft_Mil2Civ( tmp )
   ? tmp := ft_Civ2Mil( "22:00 n" ), ft_Mil2Civ( tmp )
   ? tmp := ft_Civ2Mil( "12:01 m" ), ft_Mil2Civ( tmp )
   ? tmp := ft_Civ2Mil( "22:00 n" ), ft_Mil2Civ( tmp )
   ?
   ? "sys to mil"
   ? Time()
   ? ft_Sys2Mil()

   RETURN
