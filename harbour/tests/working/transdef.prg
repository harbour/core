/*
 * $Id$
 */

procedure main
local a := 1, b := 5.22
   qout( "[" + transform( 1, "@B" ) + "]" )
   qout( "[" + transform( 1.0, "@B" ) + "]" )
   qout( "[" + transform( day( ctod("1/1/2000") ), "@B" ) + "]" )
quit