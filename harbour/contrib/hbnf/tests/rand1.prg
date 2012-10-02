/*
 * $Id$
 */

#require "hbnf"

// Write 100 random numbers from 1 to 100 to stdout.
// Run it multiple times and redirect output to a file
// to check it

PROCEDURE Main()

   LOCAL x

   FOR x := 1 TO 100
      OutStd( Int( ft_rand1(100 ) ) )
      OutStd( hb_eol() )
   NEXT

   RETURN
