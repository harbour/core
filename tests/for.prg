// Testing Harbour For Next loops for Clipper compatibility
// Donated to the public domain by Viktor Szakats (vszakats.net/harbour)

// TODO: add test for "step 0"

STATIC s_nFrom
STATIC s_nTo
STATIC s_nStep

PROCEDURE Main()

   LOCAL array := { ;
      {  1, 10,  1 }, ;
      { 10,  1, -1 }, ;
      {  1, 10, -1 }, ;
      { 10,  1,  1 }, ;
      {  1, 10,  4 }, ;
      { 10,  1, -4 }, ;
      {  1, 10, -4 }, ;
      { 10,  1,  4 } }

   LOCAL tmp, n

   ? "Testing FOR NEXT loops."

   FOR tmp := 1 TO Len( array )

      s_nFrom := array[ tmp ][ 1 ]
      s_nTo   := array[ tmp ][ 2 ]
      s_nStep := array[ tmp ][ 3 ]

      ? ;
         " From:", s_nFrom, ;
         "   To:", s_nTo, ;
         " Step:", s_nStep

      FOR n := Eval( {|| ValFrom() } ) TO Eval( {|| ValTo() } ) STEP Eval( {|| ValStep() } )
         ? "Loop", n
      NEXT

   NEXT

   RETURN

STATIC FUNCTION ValFrom()

   ? "From"

   RETURN s_nFrom

STATIC FUNCTION ValTo()

   ? "To"

   RETURN s_nTo

STATIC FUNCTION ValStep()

   ? "Step"

   RETURN s_nStep
