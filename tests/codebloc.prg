#ifndef __HARBOUR__
#include "clipper.ch"
#endif

// codeblock tests

PROCEDURE Main()

   LOCAL B := "this will never print"
   LOCAL a := {| b, c | Out( "I am a codeblock" + b + c ) }
   LOCAL d
   LOCAL de
   LOCAL ar := { 1, 2 }
   LOCAL YY, X
   LOCAL x1, x2

   HB_SYMBOL_UNUSED( b )

   ?? "this should print first"
   ?

   Eval( a, " with parameters", " ... and it works!" )
   ?

   d := "with access to local variables"

   a := {| b, c | Out( "I am a second codeblock " + d + b + ;
      iif( c == NIL, " empty second parameter ", c ) ), Out( hb_eol() ), "WITH return value" }
   Eval( a, ", codeblock parameters" )
   ?

   Eval( a, ", codeblock parameters ", "and with second parameter" )
   ?

   ?? MyEval( a )
   ?

   OtherTest( a )
   ?

   AnotherTest( a, "==> Another " )
   ?

   a := {| c | iif( c == NIL, {| a | "First " + a }, {| a | "Second " + a } ) }
   a := Eval( a )
   ?
   ?? Eval( a, "codeblock created in a codeblock" )
   ?

   ?? ar[ 1 ]
   ?
   a := {|| ar[ 1 ]++ }
   Eval( a )
   ?? ar[ 1 ]
   ?

   yy := 5
   x  := {| xx | Out( hb_ntos( xx ) ), Out( "+" ), Out( hb_ntos( yy ) ), Out( "=" ), xx + yy }
   ?? Eval( x, 1 )       // this is OK
   ?
   ?? Eval( x, 1, 2 )    // this should ignore unnecesary parameters

   ? Eval( RetBlock(), 5 )

   // BugToFix()
   ?

   ?? "Trying to use detached variable..."
   ?
   x1 := 5
   x2 := 6
   de := DetachLocal( x1, x2 )
   ?? Eval( de )
   ?
   // changing the value of variables
   x1 := 10
   x2 := 11
   ? Eval( de )
   de := DetachLocal( x1, x2 )
   ? Eval( de )

   RETURN

STATIC FUNCTION MyEval( bCodeBlock )

   LOCAL D := "this is another variable"

   RETURN Eval( bCodeBlock, " from ", "MyEval Function" )

STATIC PROCEDURE OtherTest( cblock )

   LOCAL cb

   cb := {| a, b | Eval( cblock, a, b ) }

   Eval( cb, "--> with nested ", "EVAL" )

   RETURN

STATIC PROCEDURE AnotherTest( cb, a )

   ?? Eval( cb, a )
   ?
   ?? Eval( cb, a, "again and again" )
   ?

   RETURN

STATIC FUNCTION DetachLocal( x, y )

   // NOTE! this should work
   LOCAL z := x + y
   LOCAL cb := {|| OutCR( "z=x+y=" ), OutCR( z ), OutCR( "x*x=" ), OutCR( x * x ), OutCR( "x*x+z=" ), x * x + z }

   RETURN cb

STATIC PROCEDURE BugToFix()

   LOCAL b, a := {|| a + b }

   b := "bug "
   Eval( a )

   RETURN

STATIC FUNCTION RetBlock()
   RETURN {| x | x * x }

STATIC PROCEDURE OutCR( x )

   ? x

   RETURN

STATIC PROCEDURE Out( x )

   ?? x

   RETURN
