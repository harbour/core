// Testing Harbour dates management.

#include "set.ch"
#define cNewLine CHR( 13 ) + CHR( 10 )

function Main()

   OUTSTD (cNewLine,  "Testing Harbour device management on", DATE())
   SET (_SET_ALTFILE, "output", .T.)
   SET (_SET_PRINTFILE, "output", .T.)
   QOUT ("SCREEN, NOT ALTERNATE, NOT PRINTER")
   DEVPOS (5, 5)
   DEVOUT ("SCREEN, NOT ALTERNATE NOT PRINTER")
   SET (_SET_ALTERNATE, .T.)
   SET (_SET_PRINTER, .T.)
   QOUT ("SCREEN, ALTERNATE AND PRINTER")
   DEVPOS (10, 10)
   DEVOUT ("SCREEN, NOT ALTERNATE, NOT PRINTER")
   SET (_SET_DEVICE, "PRINTER")
   QOUT ("SCREEN, ALTERNATE AND PRINTER AGAIN")
   SET (_SET_PRINTER, .F.)
   QOUT ("SCREEN AND ALTERNATE, BUT NOT PRINTER")
   DEVPOS (15, 15)
   DEVOUT ("PRINTER, NOT SCREEN, NOT ALTERNATE")
   EJECT()

return nil
