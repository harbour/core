// codeblocks test

function Main()
   local B := "this will never print"
   local a := { |b,c| OutStd( "I am a codeblock" + b + c ) }
   local d
   local de
   local ar := { 1, 2 }
   local crlf:=CHR(13)+chr(10)
   local YY, X
   local x1, x2

   OutStd( "this should print first" )
   OutStd( crlf )

   Eval( a, " with parameters", " ... and it works!" )
   OutStd( crlf )

   d ="with access to local variables"

   a ={ |b,c| OutStd( "I am a second codeblock " +d +b +;
	IIF(c==NIL, ' empty second parameter ', c)), OutStd(crlf), "WITH return value" }
   EVAL( a, ", codeblock parameters" )
   OutStd( crlf )

   EVAL( a, ", codeblock parameters ", "and with second parameter" )
   OutStd( crlf )

   OutStd( MyEval( a ) )
   OutStd( crlf )

   OtherTest( a )
   OutStd( crlf )

   AnotherTest( a, "==> Another " )
   OutStd( crlf )

   a ={|c| IIF( c=NIL, {|a| "First "+a}, {|a| "Second "+a}) }
   a =EVAL( a )
   OutStd( crlf )
   OutStd( EVAL( a, "codeblock created in a codeblock" ) )
   OutStd( crlf )

   OutStd( ar[ 1 ] )
   OutStd( crlf )
   a :={|| ar[ 1 ]++}
   EVAL( a )
   OutStd( ar[ 1 ] )
   OutStd( crlf )

   yy :=5
   x  :={|xx| OutStd(LTRIM(STR(xx))), OutStd("+"), OutStd(LTRIM(STR(yy))), OutStd("="), xx + yy }
   OutStd( EVAL( x, 1 ) )	//this is OK
   OutStd( CRLF )
   OutStd( EVAL( x, 1, 2 ) )	//this should ignore unnecesary parameters

   QOut( EVAL( RetBlock(), 5 ) )

//   BugToFix()
   OutStd( crlf )

   OutStd( "Traying to use detached variable ..." )
   OutStd( crlf )
   x1 :=5
   x2 :=6
   de =DetachLocal( x1, x2 )
   OutStd( EVAL( de ) )
   //changing the value of variables
   OutStd( crlf )
   x1 := 10
   x2 := 11
   QOut( EVAL( de ) )
   de =DetachLocal( x1, x2 )
   QOut( EVAL( de ) )

return nil

FUNCTION MyEval( bCodeBlock )
LOCAL D:="this is another variable"

RETURN( EVAL(bCodeBlock, " from ", "MyEval Function" ) )

PROCEDURE OtherTest( cblock )
LOCAL cb

  cb :={|a,b| EVAL( cblock,a,b ) }

  EVAL( cb, "--> with nested ", "EVAL" )

RETURN

PROCEDURE AnotherTest( cb, a )
  OutStd( EVAL( cb, a ) )
   OutStd( chr(13)+chr(10) )
  OutStd( EVAL( cb, a, "again and again" ) )
   OutStd( chr(13)+chr(10) )
RETURN

FUNCTION DetachLocal( x, y )
//NOTE! this should work
LOCAL z:=x+y
LOCAL cb:={|| QOut("z=x+y="), QOut(z), QOut("x*x="), QOut(x*x), QOut("x*x+z="), x*x+z}
RETURN( cb )

PROCEDURE BugToFix()

  LOCAL b, a := {|| a+b }

  b ="bug "
  EVAL( a )

RETURN

FUNCTION RetBlock()

RETURN( {|x| x*x} )

