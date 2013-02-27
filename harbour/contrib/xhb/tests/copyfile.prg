/*
 * $Id$
 */

#require "xhb"

PROCEDURE Main()

   xhb_CopyFile( __FILE__, "testcp.bak", {| x | QOut( x ) } )

   RETURN
