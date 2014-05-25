// Array Index tests
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain

PROCEDURE Main()

   LOCAL aList := { 1, 2, 3, 4, 5, 6 }

   ? aList[ 1 ] += 5
   ? aList[ 1 ]
   ? aList[ 2 ] -= 5
   ? aList[ 2 ]
   ? aList[ 3 ] *= 5
   ? aList[ 3 ]
   ? aList[ 4 ] /= 5
   ? aList[ 4 ]
   ? aList[ 5 ] ^= 5
   ? aList[ 5 ]
   ? aList[ 6 ] %= 5
   ? aList[ 6 ]

   RETURN
