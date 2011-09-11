/*
 * $Id$
 */

/* Testing the VERSION function */

/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain by David G. Holm <dholm@jsd-llc.com>.
*/

PROCEDURE Main()

   OutStd( Chr( 34 ) + Version() + Chr( 34 ) + hb_eol() )
   OutStd( Chr( 34 ) + hb_compiler() + Chr( 34 ) + hb_eol() )
   OutStd( Chr( 34 ) + OS() + Chr( 34 ) + hb_eol() )

   RETURN
