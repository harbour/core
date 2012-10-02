/*
 * $Id$
 */

// Testing Harbour For Next loops for Clipper compatibility

// ; Donated to the public domain by
//   Viktor Szakats (harbour syenar.net)

// TODO: add test for "step 0"

STATIC snFrom
STATIC snTo
STATIC snStep

PROCEDURE Main()

   LOCAL array
   LOCAL tmp, n

   ? "Testing Harbour For Next loops."

   array := { ;
      {  1, 10,  1 }, ;
      { 10,  1, -1 }, ;
      {  1, 10, -1 }, ;
      { 10,  1,  1 }, ;
      {  1, 10,  4 }, ;
      { 10,  1, -4 }, ;
      {  1, 10, -4 }, ;
      { 10,  1,  4 } }

   FOR tmp := 1 TO Len( array )

      snFrom := array[ tmp ][ 1 ]
      snTo   := array[ tmp ][ 2 ]
      snStep := array[ tmp ][ 3 ]

      ? " From:", snFrom,;
        "   To:", snTo,;
        " Step:", snStep

      FOR n := Eval( {|| ValFrom() } ) TO Eval( {|| ValTo() } ) STEP Eval( {|| ValStep() } )
         ? "Exec", n
      NEXT

   NEXT

   RETURN

STATIC FUNCTION ValFrom()

   ? "From"

   RETURN snFrom

STATIC FUNCTION ValTo()

   ? "To"

   RETURN snTo

STATIC FUNCTION ValStep()

   ? "Step"

   RETURN snStep
