/*
 * $Id$
 */

// Testing Harbour DO CASE

PROCEDURE Main()

   LOCAL n := 2

   ? "testing Harbour DO CASE"

   DO CASE
   CASE n == 1
      ? "n is 1"
      ? "first case"

   CASE n == 2
      ? "n is 2"
      ? "second case"

   CASE n == 3
      ? "n is 3"
      ? "third case"

   OTHERWISE
      ? "Sorry, I don't know what n is :-)"
      ? "otherwise"
   ENDCASE

   ? "Ok!"

   RETURN
