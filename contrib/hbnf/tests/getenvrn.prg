#require "hbnf"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL a
   LOCAL c

   LOCAL tmp

   a := Array( ft_GetE() )
   ft_GetE( @a )
   FOR EACH tmp IN a
      ? tmp
   NEXT

   ? "-------------------------------------"

   c := ""
   ft_GetE( @c )
   ? c

   RETURN
