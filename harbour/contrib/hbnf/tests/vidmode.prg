/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cMode )

   FT_SETMODE( Val( cMode ) )
   ? "Video mode is: " + Str( FT_GETMODE() )

   RETURN
