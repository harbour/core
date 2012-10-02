/*
 * $Id$
 */

/* Testing the VERSION function */

/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain by David G. Holm <dholm@jsd-llc.com>.
*/

PROCEDURE Main()

   ? Chr( 34 ) + Version() + Chr( 34 )
   ? Chr( 34 ) + hb_compiler() + Chr( 34 )
   ? Chr( 34 ) + OS() + Chr( 34 )

   RETURN
