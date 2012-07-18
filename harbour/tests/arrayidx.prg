/*
 * $Id$
 */

//
// Array Index tests
//
// Date : 1999/05/14
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain
//

PROCEDURE Main()

   LOCAL aList := { 1, 2, 3, 4, 5, 6 }

   QOut( aList[ 1 ] += 5 )
   QOut( aList[ 1 ]      )
   QOut( aList[ 2 ] -= 5 )
   QOut( aList[ 2 ]      )
   QOut( aList[ 3 ] *= 5 )
   QOut( aList[ 3 ]      )
   QOut( aList[ 4 ] /= 5 )
   QOut( aList[ 4 ]      )
   QOut( aList[ 5 ] ^= 5 )
   QOut( aList[ 5 ]      )
   QOut( aList[ 6 ] %= 5 )
   QOut( aList[ 6 ]      )

   RETURN
