/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main()

   @ 0, 0 CLEAR
   FT_PENDING( "Message one", 20, 0, 3, "W+/G" ) // Displays "Message one."
   // sets row to 20, col to 0.
   // wait to 3 and color to
   // bright white over green.
   FT_PENDING( "Message two" )   // Displays "Message two", after 5 sec.
   FT_PENDING( "Message three" ) // Displays "Message three", after 5 sec.

   RETURN
