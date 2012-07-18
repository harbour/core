/*
 * $Id$
 */

PROCEDURE Main()

   OutStd( CurDir()       + hb_eol() )
   OutStd( CurDir( "C" )  + hb_eol() )
   OutStd( CurDir( "C:" ) + hb_eol() )
   OutStd( CurDir( "D:" ) + hb_eol() )
   OutStd( CurDir( "A" )  + hb_eol() )

   RETURN
