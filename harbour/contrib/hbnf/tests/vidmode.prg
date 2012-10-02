/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cMode )

   FT_SETMODE( Val( cMode ) )
   QOut( "Video mode is: " + Str( FT_GETMODE() ) )

   RETURN
