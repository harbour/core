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

   QOut( "Testing Harbour For Next loops." )

   array := { {  1, 10,  1 }, ;
      { 10,  1, - 1 }, ;
      {  1, 10, - 1 }, ;
      { 10,  1,  1 }, ;
      {  1, 10,  4 }, ;
      { 10,  1, - 4 }, ;
      {  1, 10, - 4 }, ;
      { 10,  1,  4 } }

   FOR tmp := 1 TO Len( array )

      snFrom := array[tmp][1]
      snTo   := array[tmp][2]
      snStep := array[tmp][3]

      OutStd( "From: " ) ; OutStd( snFrom )
      OutStd( "   To: " ) ; OutStd( snTo )
      OutStd( " Step: " ) ; OutStd( snStep )
      OutStd( Chr( 13 ) + Chr( 10 ) )

      FOR n := Eval( { || ValFrom() } ) TO Eval( { || ValTo() } ) STEP Eval( { || ValStep() } )
         OutStd( "Exec " ) ; OutStd( n ) ; OutStd( Chr( 13 ) + Chr( 10 ) )
      NEXT n

   NEXT

   RETURN

STATIC FUNCTION ValFrom()

   OutStd( "From" ) ; OutStd( Chr( 13 ) + Chr( 10 ) )

   RETURN snFrom

STATIC FUNCTION ValTo()

   OutStd( "To" ) ; OutStd( Chr( 13 ) + Chr( 10 ) )

   RETURN snTo

STATIC FUNCTION ValStep()

   OutStd( "Step" ) ; OutStd( Chr( 13 ) + Chr( 10 ) )

   RETURN snStep
