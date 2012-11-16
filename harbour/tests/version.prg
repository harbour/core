/*
 * $Id$
 */

/* Testing the VERSION function */

/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain by David G. Holm <dholm@jsd-llc.com>.
*/

PROCEDURE Main()

   ? '"' + Version() + '"'
   ? '"' + hb_Compiler() + '"'
   ? '"' + OS() + '"'

   RETURN
