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

   ? Replicate( "-", 40 )

   c := ""
   ft_GetE( @c )
   ? c

   RETURN
