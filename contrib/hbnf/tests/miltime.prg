#require "hbnf"

PROCEDURE Main()

   CLS
   ? "am-pm"
   ? ft_Civ2Mil( " 5:40 pm" )
   ? ft_Civ2Mil( "05:40 pm" )
   ? ft_Civ2Mil( " 5:40 PM" )
   ? ft_Civ2Mil( " 5:40 am" )
   ? ft_Civ2Mil( "05:40 am" )
   ? ft_Civ2Mil( " 5:40 AM" )
   ?
   Inkey( 0 )
   CLS
   ? "noon-midnight"
   ? ft_Civ2Mil( "12:00 m" )
   ? ft_Civ2Mil( "12:00 M" )
   ? ft_Civ2Mil( "12:00 m" )
   ? ft_Civ2Mil( "12:00 n" )
   ? ft_Civ2Mil( "12:00 N" )
   ? ft_Civ2Mil( "12:00 n" )
   ?
   Inkey( 0 )
   CLS
   ? "errors in noon-midnight"
   ? ft_Civ2Mil( "12:01 n" )
   ? ft_Civ2Mil( "22:00 n" )
   ? ft_Civ2Mil( "12:01 m" )
   ? ft_Civ2Mil( "22:00 n" )
   ?
   ? "sys to mil"
   ? Time()
   ? ft_Sys2Mil()

   RETURN
