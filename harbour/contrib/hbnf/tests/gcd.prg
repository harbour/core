/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cNum1, cNum2 )

   OutStd( Str( FT_GCD( Val( cNum1 ), Val( cNum2 ) ) ) + hb_eol() )

   RETURN
