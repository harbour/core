//
// $Id$
//

// Testing Harbour device management.
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Donated to the public domain on 2001-03-08 by David G. Holm <dholm@jsd-llc.com>
*/

#include "set.ch"

function Main()
local cNewLine := HB_OSNewLine()

   OUTSTD (cNewLine,  "Testing Harbour device management on", DATE())
   SET ALTERNATE TO OUTPUT_A ADDITIVE
   SET (_SET_EXTRAFILE, "OUTPUT_E.EXT", .F.)
   SET PRINTER TO OUTPUT_P
   SET MARGIN TO 5
   QOUT ("SCREEN, EXTRA, NOT ALTERNATE, NOT PRINTER")
   @ 5,5 SAY "SCREEN, NOT EXTRA, NOT ALTERNATE NOT PRINTER"
   SET ALTERNATE ON
   SET PRINTER ON
   QOUT ("SCREEN, EXTRA, ALTERNATE AND PRINTER")
   @ 10,10 SAY "SCREEN, NOT EXTRA, NOT ALTERNATE, NOT PRINTER"
   SET DEVICE TO PRINTER
   SET (_SET_EXTRAFILE, "")
   QOUT ("SCREEN, ALTERNATE AND PRINTER AGAIN, BUT NOT EXTRA")
   SET PRINTER OFF
   SET (_SET_EXTRAFILE, "OUTPUT_E.EXT", .T.)
   QOUT ("SCREEN, EXTRA, AND ALTERNATE, BUT NOT PRINTER")
   @ 15,15 SAY "PRINTER, NOT SCREEN, NOT ALTERNATE"
   EJECT

return nil
