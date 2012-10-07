/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cNum, cPrec )

   __defaultNIL( @cPrec, "6" )

   ? ft_d2e( Val( cNum ), Val( cPrec ) )

   RETURN
