/*
 * $Id$
 */

// Testing memory release

PROCEDURE Main()

   LOCAL a, b

   a := "Hello"
   b := 2

   HB_SYMBOL_UNUSED( a )
   HB_SYMBOL_UNUSED( b )

   RETURN
