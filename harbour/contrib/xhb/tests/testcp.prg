/*
 * $Id$
 */

#require "xhb"

PROCEDURE Main()

   XHB_COPYFILE( __FILE__, "testcp.bak", {| x | QOut( x ) } )

   RETURN
