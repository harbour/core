/*
 * $Id$
 */

// Managing variables by reference

PROCEDURE Main()

   STATIC s_x := 10

   LOCAL x := 0

   ? "Managing LOCAL variables by reference"
   ? "In main before ref1 x=", x
   ref1( @x )
   ? " In main after ref1 x=", x

   ? "Managing STATIC variables by reference"
   ? "In main before ref1 s=", s_x
   ref1( @s_x )
   ? " In main after ref1 s=", s_x

   RETURN

PROCEDURE ref1( x )

   x++
   ? " In ref1 before ref2 =", x
   Ref2( @x )
   ? " In ref1 after ref2 =", x

   RETURN

PROCEDURE ref2( x )

   x++
   ? "  In ref2 before ref3 =", x
   Ref3( @x )
   ? "  In ref2 after ref3 =", x

   RETURN

PROCEDURE ref3( x )

   STATIC s_a

   x++
   ? "   In ref3 before ref4 =", x
   s_a := { x, x }
   Ref4( @s_a )
   ? "   In ref3 after ref4 =", x

   RETURN

PROCEDURE ref4( a )

   a[ 1 ]++
   ? "    In ref4 =", a[ 1 ]

   RETURN
