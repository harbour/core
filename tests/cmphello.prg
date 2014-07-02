/* Written by Eddie Runia <eddie@runia.com>. Placed in the public domain. */

/* This program compiles hello.prg */

PROCEDURE Main()

   ? "About to compile hello.prg"
   ?
   hb_run( hb_DirSepToOS( "../bin/harbour" ) + " hello.prg -gh" )
   ? "Finished compiling"

   RETURN
