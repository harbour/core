/*
 * $Id$
 */

// Testing Harbour device management

/* Harbour Project source code
   http://harbour-project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>
*/

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

PROCEDURE Main()

   OutStd( hb_eol(), "Testing Harbour device management on", Date() )
   SET ALTERNATE TO output_a ADDITIVE
   Set( _SET_EXTRAFILE, "output_e.ext", .F. )
   SET PRINTER TO output_p
   SET MARGIN TO 5
   QOut( "SCREEN, EXTRA, NOT ALTERNATE, NOT PRINTER" )
   @ 5, 5 SAY "SCREEN, NOT EXTRA, NOT ALTERNATE NOT PRINTER"
   SET ALTERNATE ON
   SET PRINTER ON
   QOut( "SCREEN, EXTRA, ALTERNATE AND PRINTER" )
   @ 10, 10 SAY "SCREEN, NOT EXTRA, NOT ALTERNATE, NOT PRINTER"
   SET DEVICE TO PRINTER
   Set( _SET_EXTRAFILE, "" )
   QOut( "SCREEN, ALTERNATE AND PRINTER AGAIN, BUT NOT EXTRA" )
   SET PRINTER OFF
   Set( _SET_EXTRAFILE, "output_e.ext", .T. )
   QOut( "SCREEN, EXTRA, AND ALTERNATE, BUT NOT PRINTER" )
   @ 15, 15 SAY "PRINTER, NOT SCREEN, NOT ALTERNATE"
   EJECT

   RETURN
