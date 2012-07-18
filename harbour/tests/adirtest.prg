/*
 * $Id$
 */

PROCEDURE Main()

   TestIt( "*.*" )
   TestIt( "\" )
   TestIt( "..\" )
   TestIt( "..\*.*" )

   RETURN

STATIC FUNCTION TestIt( cSpec )

   LOCAL a
   LOCAL a1
   LOCAL a2
   LOCAL a3
   LOCAL a4
   LOCAL a5

   // In order to account for documented behavour, this call will ensure
   // that Adir() returns the same length array in both cases.
   // ie: adir( cSpec ) could return a different length array than
   // ADIR( cSpec,,,,,{} )

   a := ADIR( cSpec,,,,,{} )

   SET DATE ANSI
   SET CENTURY ON

   a1 := Array( a )
   a2 := Array( a )
   a3 := Array( iif( a >= 1, a - 1, a ) )
   a4 := Array( a )
   a5 := Array( a + 1 )

   OutStd( "--------------------------------------------------------" )
   OutStd( hb_eol() )

   ADIR( cSpec , a1, a2, a3, a4, a5 )

   aEval(a1, {|tmp| OutStd( tmp ), OutStd( hb_eol() ) } )
   aEval(a2, {|tmp| OutStd( tmp ), OutStd( hb_eol() ) } )
   aEval(a3, {|tmp| OutStd( tmp ), OutStd( hb_eol() ) } )
   aEval(a4, {|tmp| OutStd( tmp ), OutStd( hb_eol() ) } )
   aEval(a5, {|tmp| OutStd( tmp ), OutStd( hb_eol() ) } )

   ADIR( cSpec, 10, "A", NIL, NIL, NIL )

   RETURN NIL
