STATIC s_cbStatic

PROCEDURE Main()

   LOCAL a := TestBlocks()
   LOCAL cb

   ? Eval( a[ 1 ] )       // 23
   ? Eval( a[ 2 ], 42 )   // 42
   ? Eval( a[ 1 ] )       // 42
   ? Eval( a[ 2 ], 15 )   // 15

   myout( 15, Eval( a[ 1 ] ) )      // 15 15
   myout( 14, Eval( a[ 1 ] ) )      // 14 15
   myout( 42, Eval( a[ 2 ], 42 ) )  // 42 42
   myout( 14, Eval( a[ 2 ], 42 ) )  // 14 42
   myout( 42, Eval( a[ 1 ] ) )      // 42 42
   myout( 14, Eval( a[ 1 ] ) )      // 14 42

   GetArray( @a )
   PrintArray( @a )

   ? "Test for variables passed by reference in a codeblock"
   DetachWithRefer()

   ? "Test for indirect detaching of local variables"
   DetachToStatic( 1 )
   myout( 2, Eval( s_cbStatic, 1 ) )
   myout( 3, Eval( s_cbStatic, 2 ) )
   cb := s_cbStatic
   DetachToStatic( 100 )
   myout( 200, Eval( s_cbStatic, 100 ) )
   myout( 300, Eval( s_cbStatic, 200 ) )
   myout( 4, Eval( cb, 3 ) )

   ReferParam()

   RETURN

STATIC FUNCTION TestBlocks()

   LOCAL nFoo := 23

   RETURN { {|| nFoo }, {| n | nFoo := n } }

STATIC FUNCTION myout( nExpected, nGot )

   ? nExpected, nGot

   RETURN NIL

//

STATIC PROCEDURE GetArray( a )

   LOCAL i

   a := Array( 100 )
   FOR i := 1 TO 100
      IF ( i % 6 ) == 0
         a[ i - 2 ] := NIL
         a[ i - 4 ] := NIL
      ENDIF
      a[ i ] := TestBlocks()
   NEXT

   RETURN

STATIC PROCEDURE PrintArray( a )

   LOCAL i

   FOR i := 1 TO 100
      IF a[ i ] != NIL
         Eval( a[ i ][ 2 ], i )
         myout( i, Eval( a[ i ][ 1 ] ) )
      ENDIF
   NEXT

   RETURN

//

STATIC FUNCTION DetachWithRefer()

   LOCAL nTest
   LOCAL bBlock1 := MakeBlock()
   LOCAL bBlock2 := {|| DoThing( @nTest ), QOut( nTest ) }

   Eval( bBlock1 )
   Eval( bBlock2 )

   RETURN NIL

STATIC FUNCTION MakeBlock()

   LOCAL nTest

   RETURN {|| DoThing( @nTest ), QOut( nTest ) }

STATIC FUNCTION DoThing( n )

   n := 42

   RETURN NIL

//

STATIC FUNCTION DetachToStatic( n )

   s_cbStatic := {| x | n + x }

   RETURN NIL

//

STATIC FUNCTION ReferParam()

   LOCAL bResult

   ? "Test for codeblock parameter passed by reference"

   PassByValue( {| lEnd | ;
      bResult := GetBlock( @lEnd ), ;
      SetByRef( @lEnd ) } )
   // Clipper & xHarbour it's .T.
   // In Harbour it is .F.
   ? "Printed value in Clipper  .T. =", Eval( bResult )
   ?
   // Notice the Clipper bug: GetBlock is receiving the reference to
   // the codeblock parameter than the value of Eval( bResult ) shouldn't
   // depend on the order of block creation/value changing (GetBlock/SetRef).

   PassByRef( {| lEnd | ;
      bResult := GetBlock( @lEnd ), ;
      SetByRef( @lEnd ) } )
   // Clipper & xHarbour it's .T.
   // In Harbour it is .F.
   ? "Printed value in Clipper  .T. =", Eval( bResult )
   ?

   ? "2nd test for codeblock parameter passed by reference"

   PassByValue( {| lEnd | ;
      SetByRef( @lEnd ), ;
      bResult := GetBlock( @lEnd ) } )
   // Clipper & Harbour it's .F.
   ? "Printed value in Clipper  .F. =", Eval( bResult )
   ?

   PassByRef( {| lEnd | ;
      SetByRef( @lEnd ), ;
      bResult := GetBlock( @lEnd ) } )
   // Clipper & Harbour it's .F.
   ? "Printed value in Clipper  .F. =", Eval( bResult )
   ?

   RETURN NIL

STATIC FUNCTION PassByValue( bBlock )

   LOCAL lSomeVar := .T.

   Eval( bBlock, lSomeVar )
   ? "lSomeVar value in Clipper .T. =", lSomeVar

   RETURN .T.

STATIC FUNCTION PassByRef( bBlock )

   LOCAL lSomeVar := .T.

   Eval( bBlock, @lSomeVar )
   ? "lSomeVar value in Clipper .F. =", lSomeVar

   RETURN .T.

STATIC FUNCTION SetByRef( lVar )

   lVar := .F.

   RETURN NIL

STATIC FUNCTION GetBlock( lVar )
   RETURN {|| lVar }
