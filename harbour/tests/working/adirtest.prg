/*
 * $Id$
 */

FUNCTION Main()

   TestIt( "*.*" )
   TestIt( "\" )
   TestIt( "..\" )
   TestIt( "..\*.*" )

   RETURN NIL

STATIC FUNCTION TestIt( cSpec )

   LOCAL a := ADIR( cSpec )
   LOCAL a1
   LOCAL a2
   LOCAL a3
   LOCAL a4
   LOCAL a5

   LOCAL cNewLine

   cOs := OS()

   IF "OS/2" $ OS() .OR. ;
      "DOS"  $ OS()
      cNewLine := Chr( 13 ) + Chr( 10 )
   ELSE
      cNewLine := Chr( 10 )
   ENDIF

   SET DATE ANSI
   SET CENTURY ON

   a1 := Array( a )
   a2 := Array( a )
   a3 := Array( iif( a >= 1, a - 1, a ) )
   a4 := Array( a )
   a5 := Array( a + 1 )

   OutStd( "--------------------------------------------------------" )
   OutStd( cNewLine )

   ADIR( cSpec , a1, a2, a3, a4, a5 )

   aEval(a1, {|tmp| OutStd( tmp ), OutStd( cNewLine ) } )
   aEval(a2, {|tmp| OutStd( tmp ), OutStd( cNewLine ) } )
   aEval(a3, {|tmp| OutStd( tmp ), OutStd( cNewLine ) } )
   aEval(a4, {|tmp| OutStd( tmp ), OutStd( cNewLine ) } )
   aEval(a5, {|tmp| OutStd( tmp ), OutStd( cNewLine ) } )

   ADIR( cSpec, 10, "A", NIL, NIL, NIL )

   RETURN NIL

