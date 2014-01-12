#require "hbnf"

PROCEDURE Main()

   CLS

   ft_Pending( "Message one", MaxRow() - 4, 0, 3, "W+/G" ) // Displays "Message one."
   // sets row. sets col to 0.
   // wait to 3 and color to
   // bright white over green.
   ft_Pending( "Message two" )   // Displays "Message two", after 5 sec.
   ft_Pending( "Message three" ) // Displays "Message three", after 5 sec.

   RETURN
