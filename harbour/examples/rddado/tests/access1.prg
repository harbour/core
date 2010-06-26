/*
 * $Id$
 */

#include "adordd.ch"

REQUEST ADORDD

PROCEDURE Main()

   SET DATE ANSI
   SET CENTURY ON

   USE ( hb_dirBase() + "test.mdb" ) VIA "ADORDD" TABLE "Table1"

   Browse()

   USE

   RETURN
