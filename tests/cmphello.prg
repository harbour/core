// This program compiles hello.prg
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain

PROCEDURE Main()

   ? "About to compile hello.prg"
   ?
   hb_run( hb_DirSepToOS( "../bin/harbour" ) + " hello.prg -gh" )
   ? "Finished compiling"

   RETURN
