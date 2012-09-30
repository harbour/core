/*
 * $Id$
 */

// Testing Harbour For Next loops
//
// Written by Eddie Runia <eddie@runia.com>
// www - http://harbour-project.org
//
// Placed in the public domain
//

PROCEDURE Main()

   LOCAL n

   QOut( "Testing Harbour For Next loops. Going up quick" )

   FOR n := 1 TO 10 STEP 4
      QOut( n )
   NEXT n

   QOut( "Going down" )

   FOR n := 10 TO 1 STEP -1
      QOut( n )
   NEXT n

   QOut( "No step" )

   FOR n := 1 TO 10
      QOut( n )
   NEXT n

   QOut( "No production" )

   FOR n := 1 TO 10 STEP -1
      QOut( n )
   NEXT n

   QOut( "Ok!" )

   RETURN
