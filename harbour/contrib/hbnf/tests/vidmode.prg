/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cMode )

   ft_SetMode( Val( cMode ) )
   ? "Video mode is: " + Str( ft_GetMode() )

   RETURN
