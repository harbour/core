/*
 * $Id$
 */

#require "hbtpl"
#require "hbtest"

PROCEDURE Main()

   HBTEST hbtpl_MyPublicFunction()      IS "It works"
   HBTEST hbtpl_MyPublicFunction_In_C() IS "It works from C"
   HBTEST HBTPL_MYCONSTANT              IS 100

   RETURN
