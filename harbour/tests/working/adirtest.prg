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

   LOCAL a1 := Array(a)
   LOCAL a2 := 10
   LOCAL a3 := Array(a - 1)
   LOCAL a4 := NIL
   LOCAL a5 := Array(a + 1)

   OutStd( "--------------------------------------------------------" )
   OutStd( Chr(13) + Chr(10) )

   ADIR( cSpec , a1, a2, a3, a4, a5)

   aEval(a1, {|tmp| OutStd( tmp ), OutStd( Chr(13) + Chr(10) ) })
   aEval(a3, {|tmp| OutStd( tmp ), OutStd( Chr(13) + Chr(10) ) })
   aEval(a5, {|tmp| OutStd( tmp ), OutStd( Chr(13) + Chr(10) ) })

   RETURN NIL

