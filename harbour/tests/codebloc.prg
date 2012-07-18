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
   LOCAL crlf := Chr( 13 ) + Chr( 10 )
   LOCAL YY, X
   LOCAL x1, x2

   OutStd( "this should print first" )
   OutStd( crlf )

   Eval( a, " with parameters", " ... and it works!" )
   OutStd( crlf )

   d = "with access to local variables"

   a = {| b, c | OutStd( "I am a second codeblock " + d + b + ;
      IIF( c == NIL, ' empty second parameter ', c ) ), OutStd( crlf ), "WITH return value" }
   Eval( a, ", codeblock parameters" )
   OutStd( crlf )

   Eval( a, ", codeblock parameters ", "and with second parameter" )
   OutStd( crlf )

   OutStd( MyEval( a ) )
   OutStd( crlf )

   OtherTest( a )
   OutStd( crlf )

   AnotherTest( a, "==> Another " )
   OutStd( crlf )

   a = {| c | IIF( c == NIL, {| a | "First " + a }, {| a | "Second " + a } ) }
   a = Eval( a )
   OutStd( crlf )
   OutStd( Eval( a, "codeblock created in a codeblock" ) )
   OutStd( crlf )

   OutStd( ar[ 1 ] )
   OutStd( crlf )
   a := {|| ar[ 1 ] ++ }
   Eval( a )
   OutStd( ar[ 1 ] )
   OutStd( crlf )

   yy := 5
   x  := {| xx | OutStd( LTrim( Str(xx ) ) ), OutStd( "+" ), OutStd( LTrim( Str(yy ) ) ), OutStd( "=" ), xx + yy }
   OutStd( Eval( x, 1 ) )       //this is OK
   OutStd( CRLF )
   OutStd( Eval( x, 1, 2 ) )    //this should ignore unnecesary parameters

   QOut( Eval( RetBlock(), 5 ) )

   //   BugToFix()
   OutStd( crlf )

   OutStd( "Trying to use detached variable ..." )
   OutStd( crlf )
   x1 := 5
   x2 := 6
   de = DetachLocal( x1, x2 )
   OutStd( Eval( de ) )
   //changing the value of variables
   OutStd( crlf )
   x1 := 10
   x2 := 11
   QOut( Eval( de ) )
   de = DetachLocal( x1, x2 )
   QOut( Eval( de ) )

   RETURN

FUNCTION MyEval( bCodeBlock )

   LOCAL D := "this is another variable"

   RETURN( Eval( bCodeBlock, " from ", "MyEval Function" ) )

PROCEDURE OtherTest( cblock )

   LOCAL cb

   cb := {| a, b | Eval( cblock, a, b ) }

   Eval( cb, "--> with nested ", "EVAL" )

   RETURN

PROCEDURE AnotherTest( cb, a )

   OutStd( Eval( cb, a ) )
   OutStd( Chr( 13 ) + Chr( 10 ) )
   OutStd( Eval( cb, a, "again and again" ) )
   OutStd( Chr( 13 ) + Chr( 10 ) )

   RETURN

FUNCTION DetachLocal( x, y )

   //NOTE! this should work
   LOCAL z := x + y
   LOCAL cb := {|| QOut( "z=x+y=" ), QOut( z ), QOut( "x*x=" ), QOut( x * x ), QOut( "x*x+z=" ), x * x + z }

   RETURN( cb )

PROCEDURE BugToFix()

   LOCAL b, a := {|| a + b }

   b = "bug "
   Eval( a )

   RETURN

FUNCTION RetBlock()

   RETURN( {| x | x * x } )
