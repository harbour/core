/*
 * $Id$
 */

FUNCTION Main()

   OutStd( CurDir()     + Chr(13) + Chr(10) )
   OutStd( CurDir("C")  + Chr(13) + Chr(10) )
   OutStd( CurDir("C:") + Chr(13) + Chr(10) )
   OutStd( CurDir("D:") + Chr(13) + Chr(10) )
   OutStd( CurDir("A")  + Chr(13) + Chr(10) )

   RETURN NIL
