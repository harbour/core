PROCEDURE Main()

   LOCAL a, b, c

   a := { {, } }

   a[ 1, 2 ] := [Hello]

   c := { 1 }

   b := a[ c[ 1 ] ][ Val( [ 2 ] ) ]

   ? b

   RETURN
