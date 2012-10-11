/*
 * $Id$
 */

#require "xhb"

PROCEDURE Main()

   XHB_COPYFILE( "testcp.prg", "testcp.bak", {| x | QOut( x ) } )

   RETURN
