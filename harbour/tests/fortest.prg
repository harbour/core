/*
 * $Id$
 */

// Testing Harbour For Next loops for Clipper compatibility

// ; Donated to the public domain by
//   Viktor Szakats (harbour syenar.net)

// TODO: add test for "step 0"

#ifndef __HARBOUR__
   #xtranslate hb_eol() => ( Chr( 13 ) + Chr( 10 ) )
#endif

STATIC snFrom
STATIC snTo
STATIC snStep

PROCEDURE Main()

   LOCAL array
   LOCAL tmp, n

   QOut( "Testing Harbour For Next loops." )

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

      OutStd( " From: " ) ; OutStd( snFrom )
      OutStd( "   To: " ) ; OutStd( snTo )
      OutStd( " Step: " ) ; OutStd( snStep )
      OutStd( hb_eol() )

      FOR n := Eval( {|| ValFrom() } ) TO Eval( {|| ValTo() } ) STEP Eval( {|| ValStep() } )
         OutStd( "Exec " ) ; OutStd( n ) ; OutStd( hb_eol() )
      NEXT n

   NEXT

   RETURN

STATIC FUNCTION ValFrom()

   OutStd( "From" ) ; OutStd( hb_eol() )

   RETURN snFrom

STATIC FUNCTION ValTo()

   OutStd( "To" ) ; OutStd( hb_eol() )

   RETURN snTo

STATIC FUNCTION ValStep()

   OutStd( "Step" ) ; OutStd( hb_eol() )

   RETURN snStep
