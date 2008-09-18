/*
 * $Id$
 */

PROCEDURE Main()

   XHB_COPYFILE( "testcp.prg", "testcp.bak", {| x | QOut( x ) } )

   RETURN
