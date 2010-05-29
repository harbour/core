/*
 * $Id$
 */

#include "adordd.ch"

REQUEST ADORDD

PROCEDURE Main()
? hb_dirBase() + "test.mdb"
   USE ( hb_dirBase() + "test.mdb" ) VIA "ADORDD" TABLE "Tabla1"

   Browse()

   USE

   RETURN
