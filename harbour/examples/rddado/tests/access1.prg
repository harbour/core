/*
 * $Id$
 */

#include "adordd.ch"

REQUEST ADORDD

function Main()

   USE test.mdb VIA "ADORDD" TABLE "Tabla1"

   Browse()

   USE

return nil
