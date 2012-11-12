/*
 * $Id$
 */

#require "rddado"

#include "adordd.ch"

REQUEST ADORDD

PROCEDURE Main()

   SET DATE ANSI
   SET CENTURY ON

   USE ( hb_DirBase() + "test.mdb" ) VIA "ADORDD" TABLE "Table1"

   CLS
   Browse()

   USE

   RETURN
