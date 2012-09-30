/*
 * $Id$
 */

/* TEST FOR USING VARIABLE NUMBER OF PARAMETERS */

MEMVAR iLoop

PROCEDURE Main()

   LOCAL l1 := 11, l2 := 22, l3 := 33, l4 := 44, l5 := 55, l6 := 66
   PRIVATE iLoop

   ? "passed 0:  "; TEST_0_0( )
   ? "passed 1:  "; TEST_0_0( 1 )
   ? "passed 2:  "; TEST_0_0( 1, 2 )
   ? "passed 3:  "; TEST_0_0( 1, 2, 3 )

   ? "---------------------------------------"
   ? "passed 0:  "; TEST_0_0v( )
   ? "passed 1:  "; TEST_0_0v( 1 )
   ? "passed 2:  "; TEST_0_0v( 1, 2 )
   ? "passed 3:  "; TEST_0_0v( 1, 2, 3 )

   ? "---------------------------------------"
   ? "passed 0:  "; TEST_1_0( )
   ? "passed 1:  "; TEST_1_0( 1 )
   ? "passed 2:  "; TEST_1_0( 1, 2 )
   ? "passed 3:  "; TEST_1_0( 1, 2, 3 )

   ? "---------------------------------------"
   ? "passed 0:  "; TEST_1_0v( )
   ? "passed 1:  "; TEST_1_0v( 1 )
   ? "passed 2:  "; TEST_1_0v( 1, 2 )
   ? "passed 3:  "; TEST_1_0v( 1, 2, 3 )

   ? "---------------------------------------"
   ? "passed 0:  "; TEST_1_1( )
   ? "passed 1:  "; TEST_1_1( 1 )
   ? "passed 2:  "; TEST_1_1( 1, 2 )
   ? "passed 3:  "; TEST_1_1( 1, 2, 3 )

   ? "---------------------------------------"
   ? "passed 0:  "; TEST_1_1v( )
   ? "passed 1:  "; TEST_1_1v( 1 )
   ? "passed 2:  "; TEST_1_1v( 1, 2 )
   ? "passed 3:  "; TEST_1_1v( 1, 2, 3 )

   ? "---------------------------------------"
   ? "passed 0:  "; TEST_1_3( )
   ? "passed 1:  "; TEST_1_3( 1 )
   ? "passed 2:  "; TEST_1_3( 1, 2 )
   ? "passed 3:  "; TEST_1_3( 1, 2, 3 )
   ? "passed 4:  "; TEST_1_3( 1, 2, 3, 4 )
   ? "passed 5:  "; TEST_1_3( 1, 2, 3, 4, 5 )
   ? "passed 6:  "; TEST_1_3( 1, 2, 3, 4, 5, 6 )

   ? "---------------------------------------"
   ? "passed 0:  "; TEST_1_3v( )
   ? "passed 1:  "; TEST_1_3v( 1 )
   ? "passed 2:  "; TEST_1_3v( 1, 2 )
   ? "passed 3:  "; TEST_1_3v( 1, 2, 3 )
   ? "passed 4:  "; TEST_1_3v( 1, 2, 3, 4 )
   ? "passed 5:  "; TEST_1_3v( 1, 2, 3, 4, 5 )
   ? "passed 6:  "; TEST_1_3v( 1, 2, 3, 4, 5, 6 )

   ? "---------------------------------------"
   ? "passed 0:  "; TEST_3_0( )
   ? "passed 1:  "; TEST_3_0( 1 )
   ? "passed 2:  "; TEST_3_0( 1, 2 )
   ? "passed 3:  "; TEST_3_0( 1, 2, 3 )
   ? "passed 4:  "; TEST_3_0( 1, 2, 3, 4 )
   ? "passed 5:  "; TEST_3_0( 1, 2, 3, 4, 5 )
   ? "passed 6:  "; TEST_3_0( 1, 2, 3, 4, 5, 6 )

   ? "---------------------------------------"
   ? "passed 0:  "; TEST_3_0v( )
   ? "passed 1:  "; TEST_3_0v( 1 )
   ? "passed 2:  "; TEST_3_0v( 1, 2 )
   ? "passed 3:  "; TEST_3_0v( 1, 2, 3 )
   ? "passed 4:  "; TEST_3_0v( 1, 2, 3, 4 )
   ? "passed 5:  "; TEST_3_0v( 1, 2, 3, 4, 5 )
   ? "passed 6:  "; TEST_3_0v( 1, 2, 3, 4, 5, 6 )

   ? "---------------------------------------"
   ? "passed 0:  "; TEST_3_3( )
   ? "passed 1:  "; TEST_3_3( 1 )
   ? "passed 2:  "; TEST_3_3( 1, 2 )
   ? "passed 3:  "; TEST_3_3( 1, 2, 3 )
   ? "passed 4:  "; TEST_3_3( 1, 2, 3, 4 )
   ? "passed 5:  "; TEST_3_3( 1, 2, 3, 4, 5 )
   ? "passed 6:  "; TEST_3_3( 1, 2, 3, 4, 5, 6 )

   ? "---------------------------------------"
   ? "passed 0:  "; TEST_3_3v( )
   ? "passed 1:  "; TEST_3_3v( 1 )
   ? "passed 2:  "; TEST_3_3v( 1, 2 )
   ? "passed 3:  "; TEST_3_3v( 1, 2, 3 )
   ? "passed 4:  "; TEST_3_3v( 1, 2, 3, 4 )
   ? "passed 5:  "; TEST_3_3v( 1, 2, 3, 4, 5 )
   ? "passed 6:  "; TEST_3_3v( 1, 2, 3, 4, 5, 6 )

   ? "---------------------------------------"
   ? "Passed 6 by ref:  "; TEST_0_0( @l1, @l2, @l3, @l4, @l5, @l6 )
   ? "Passed 6 by ref:  "; TEST_0_0v( @l1, @l2, @l3, @l4, @l5, @l6 )

   ? "---------------------------------------"
   ? "Passed 6 by ref:  "; TEST_1_0( @l1, @l2, @l3, @l4, @l5, @l6 )
   ? "Passed 6 by ref:  "; TEST_1_0v( @l1, @l2, @l3, @l4, @l5, @l6 )

   ? "---------------------------------------"
   ? "Passed 6 by ref:  "; TEST_1_1( @l1, @l2, @l3, @l4, @l5, @l6 )
   ? "Passed 6 by ref:  "; TEST_1_1v( @l1, @l2, @l3, @l4, @l5, @l6 )

   ? "---------------------------------------"
   ? "Passed 6 by ref:  "; TEST_1_3( @l1, @l2, @l3, @l4, @l5, @l6 )
   ? "Passed 6 by ref:  "; TEST_1_3v( @l1, @l2, @l3, @l4, @l5, @l6 )

   ? "---------------------------------------"
   ? "Passed 6 by ref:  "; TEST_3_0( @l1, @l2, @l3, @l4, @l5, @l6 )
   ? "Passed 6 by ref:  "; TEST_3_0v( @l1, @l2, @l3, @l4, @l5, @l6 )

   ? "---------------------------------------"
   ? "Passed 6 by ref:  "; TEST_3_3( @l1, @l2, @l3, @l4, @l5, @l6 )
   ? "Passed 6 by ref:  "; TEST_3_3v( @l1, @l2, @l3, @l4, @l5, @l6 )

   RETURN

PROCEDURE TEST_0_0v( ... )

   ?? ProcName( 0 ), " received: ", PCount()
   FOR m->iLoop := 1 TO PCount()
      ? m->iLoop, "=", hb_PValue( m->iLoop )
   NEXT

   Inkey( 0 )

   RETURN

PROCEDURE TEST_0_0( )

   ?? ProcName( 0 ), " received: ", PCount()
   FOR m->iLoop := 1 TO PCount()
      ? m->iLoop, "=", hb_PValue( m->iLoop )
   NEXT

   Inkey( 0 )

   RETURN

PROCEDURE TEST_1_0v( ... )

   LOCAL i := "i"

   ?? ProcName( 0 ), " received: ", PCount()
   ? "i=", i
   FOR i := 1 TO PCount()
      ? i, "=", hb_PValue( i )
   NEXT

   Inkey( 0 )

   RETURN

PROCEDURE TEST_1_0( )

   LOCAL i := "i"

   ?? ProcName( 0 ), " received: ", PCount()
   ? "i=", i
   FOR i := 1 TO PCount()
      ? i, "=", hb_PValue( i )
   NEXT

   Inkey( 0 )

   RETURN

PROCEDURE TEST_1_3v( a, b, c, ... )

   LOCAL i := "i"

   ?? ProcName( 0 ), " received: ", PCount()
   ? "i=", i
   ? "a=", a
   ? "b=", b
   ? "c=", c
   FOR i := 1 TO PCount()
      ? i, "=", hb_PValue( i )
   NEXT

   FOR EACH i IN hb_AParams()
      ? i:__enumindex, "-", i
   NEXT

   Inkey( 0 )

   RETURN

PROCEDURE TEST_1_3( a, b, c )

   LOCAL i := "i"

   ?? ProcName( 0 ), " received: ", PCount()
   ? "i=", i
   ? "a=", a
   ? "b=", b
   ? "c=", c
   FOR i := 1 TO PCount()
      ? i, "=", hb_PValue( i )
   NEXT

   FOR EACH i IN hb_AParams()
      ? i:__enumindex, "-", i
   NEXT

   Inkey( 0 )

   RETURN

PROCEDURE TEST_1_1( a )

   LOCAL i := "i"

   ?? ProcName( 0 ), " received: ", PCount()
   ? "i=", i
   ? "a=", a
   FOR i := 1 TO PCount()
      ? i, "=", hb_PValue( i )
   NEXT

   Inkey( 0 )

   RETURN

PROCEDURE TEST_1_1v( a, ... )

   LOCAL i := "i"

   ?? ProcName( 0 ), " received: ", PCount()
   ? "i=", i
   ? "a=", a
   FOR i := 1 TO PCount()
      ? i, "=", hb_PValue( i )
   NEXT

   Inkey( 0 )

   RETURN

PROCEDURE TEST_3_3v( a, b, c, ... )

   LOCAL x := "x", y := "y", z := "z"

   ?? ProcName( 0 ), " received: ", PCount()
   ? "x=", x
   ? "y=", y
   ? "z=", z
   ? "a=", a
   ? "b=", b
   ? "c=", c
   FOR m->iLoop := 1 TO PCount()
      ? m->iLoop, "=", hb_PValue( m->iLoop )
   NEXT

   test_ref( @a, @b, @c, @x, @y, @z )

   Inkey( 0 )

   RETURN

PROCEDURE TEST_3_3( a, b, c )

   LOCAL x := "x", y := "y", z := "z"

   ?? ProcName( 0 ), " received: ", PCount()
   ? "x=", x
   ? "y=", y
   ? "z=", z
   ? "a=", a
   ? "b=", b
   ? "c=", c
   FOR m->iLoop := 1 TO PCount()
      ? m->iLoop, "=", hb_PValue( m->iLoop )
   NEXT

   test_ref( @a, @b, @c, @x, @y, @z )

   Inkey( 0 )

   RETURN

PROCEDURE TEST_3_0( )

   LOCAL x := "x", y := "y", z := "z"

   ?? ProcName( 0 ), " received: ", PCount()
   ? "x=", x
   ? "y=", y
   ? "z=", z
   FOR m->iLoop := 1 TO PCount()
      ? m->iLoop, "=", hb_PValue( m->iLoop )
   NEXT

   Inkey( 0 )

   RETURN

PROCEDURE TEST_3_0v( ... )

   LOCAL x := "x", y := "y", z := "z"

   ?? ProcName( 0 ), " received: ", PCount()
   ? "x=", x
   ? "y=", y
   ? "z=", z
   FOR m->iLoop := 1 TO PCount()
      ? m->iLoop, "=", hb_PValue( m->iLoop )
   NEXT

   Inkey( 0 )

   RETURN

PROCEDURE TEST_REF( a, ... )

   LOCAL b

   ? "@@@"
   ? ProcName( 0 ), " received: ", PCount()
   ? "a= ", a
   ? "b= ", b
   FOR EACH b IN hb_AParams( 0 )
      ? b:__enumindex, "-", b
   NEXT

   RETURN
