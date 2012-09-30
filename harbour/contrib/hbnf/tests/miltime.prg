/*
 * $Id$
 */

PROCEDURE Main()

   CLS
   ? "am-pm"
   ? ft_civ2mil( " 5:40 pm" )
   ? ft_civ2mil( "05:40 pm" )
   ? ft_civ2mil( " 5:40 PM" )
   ? ft_civ2mil( " 5:40 am" )
   ? ft_civ2mil( "05:40 am" )
   ? ft_civ2mil( " 5:40 AM" )
   ?
   Inkey( 0 )
   CLS
   ? "noon-midnight"
   ? ft_civ2mil( "12:00 m" )
   ? ft_civ2mil( "12:00 M" )
   ? ft_civ2mil( "12:00 m" )
   ? ft_civ2mil( "12:00 n" )
   ? ft_civ2mil( "12:00 N" )
   ? ft_civ2mil( "12:00 n" )
   ?
   Inkey( 0 )
   CLS
   ? "errors in noon-midnight"
   ? ft_civ2mil( "12:01 n" )
   ? ft_civ2mil( "22:00 n" )
   ? ft_civ2mil( "12:01 m" )
   ? ft_civ2mil( "22:00 n" )
   ?
   ? "sys to mil"
   ? Time()
   ? ft_sys2mil()

   RETURN
