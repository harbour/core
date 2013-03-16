// releasing arrays test

PROCEDURE Main()

   LOCAL a := { 1 }

   a[ 1 ] := a
   a[ 1 ] := NIL

   ? "The array will try to be released now..."

   RETURN
