/*
 * $Id$
 */

#require "xhb"

PROCEDURE Main()

   xhb_CopyFile( __FILE__, hb_FNameExtSet( __FILE__, ".bak" ), {| x | QOut( x ) } )

   RETURN
