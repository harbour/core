/*
 * $Id$
 */

// codeblocks test

PROCEDURE Main()

   LOCAL B := "this will never print"
   LOCAL a := {| b, c | OutStd( "I am a codeblock" + b + c ) }
   LOCAL d
   LOCAL de
   LOCAL ar := { 1, 2 }
   LOCAL YY, X
   LOCAL x1, x2

   HB_SYMBOL_UNUSED( b )

   OutStd( "this should print first" )
   OutStd( hb_eol() )

   Eval( a, " with parameters", " ... and it works!" )
   OutStd( hb_eol() )

   d := "with access to local variables"

   a := {| b, c | OutStd( "I am a second codeblock " + d + b + ;
      iif( c == NIL, " empty second parameter ", c ) ), OutStd( hb_eol() ), "WITH return value" }
   Eval( a, ", codeblock parameters" )
   OutStd( hb_eol() )

   Eval( a, ", codeblock parameters ", "and with second parameter" )
   OutStd( hb_eol() )

   OutStd( MyEval( a ) )
   OutStd( hb_eol() )

   OtherTest( a )
   OutStd( hb_eol() )

   AnotherTest( a, "==> Another " )
   OutStd( hb_eol() )

   a := {| c | iif( c == NIL, {| a | "First " + a }, {| a | "Second " + a } ) }
   a := Eval( a )
   OutStd( hb_eol() )
   OutStd( Eval( a, "codeblock created in a codeblock" ) )
   OutStd( hb_eol() )

   OutStd( ar[ 1 ] )
   OutStd( hb_eol() )
   a := {|| ar[ 1 ]++ }
   Eval( a )
   OutStd( ar[ 1 ] )
   OutStd( hb_eol() )

   yy := 5
   x  := {| xx | OutStd( hb_ntos( xx ) ), OutStd( "+" ), OutStd( hb_ntos( yy ) ), OutStd( "=" ), xx + yy }
   OutStd( Eval( x, 1 ) )       //this is OK
   OutStd( hb_eol() )
   OutStd( Eval( x, 1, 2 ) )    //this should ignore unnecesary parameters

   QOut( Eval( RetBlock(), 5 ) )

   //   BugToFix()
   OutStd( hb_eol() )

   OutStd( "Trying to use detached variable ..." )
   OutStd( hb_eol() )
   x1 := 5
   x2 := 6
   de := DetachLocal( x1, x2 )
   OutStd( Eval( de ) )
   //changing the value of variables
   OutStd( hb_eol() )
   x1 := 10
   x2 := 11
   QOut( Eval( de ) )
   de := DetachLocal( x1, x2 )
   QOut( Eval( de ) )

   RETURN

FUNCTION MyEval( bCodeBlock )

   LOCAL D := "this is another variable"

   RETURN Eval( bCodeBlock, " from ", "MyEval Function" )

PROCEDURE OtherTest( cblock )

   LOCAL cb

   cb := {| a, b | Eval( cblock, a, b ) }

   Eval( cb, "--> with nested ", "EVAL" )

   RETURN

PROCEDURE AnotherTest( cb, a )

   OutStd( Eval( cb, a ) )
   OutStd( hb_eol() )
   OutStd( Eval( cb, a, "again and again" ) )
   OutStd( hb_eol() )

   RETURN

FUNCTION DetachLocal( x, y )

   //NOTE! this should work
   LOCAL z := x + y
   LOCAL cb := {|| QOut( "z=x+y=" ), QOut( z ), QOut( "x*x=" ), QOut( x * x ), QOut( "x*x+z=" ), x * x + z }

   RETURN cb

PROCEDURE BugToFix()

   LOCAL b, a := {|| a + b }

   b := "bug "
   Eval( a )

   RETURN

FUNCTION RetBlock()

   RETURN {| x | x * x }
