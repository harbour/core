/*
 * $Id$
 */

PROCEDURE Main()

   OutStd( cMonth( date() ) + hb_eol() )
   OutStd( cMonth( date() + 31 ) + hb_eol() )
   OutStd( cMonth( date() + 60 ) + hb_eol() )

   OutStd( cDow( date() ) + hb_eol() )
   OutStd( cDow( date() + 6 ) + hb_eol() )
   OutStd( cDow( date() + 7 ) + hb_eol() )

   RETURN
