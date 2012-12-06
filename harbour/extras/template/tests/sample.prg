/*
 * $Id$
 */

#require "hbtpl"

PROCEDURE Main()

   // Public API

   ? hbtpl_MyPublicFunction()
   ? hbtpl_MyPublicFunction_In_C()

   // Public constants

   ? HBTPL_MYCONSTANT

   // Public commands

   HBTPL_PRINT "Hello"

   RETURN
