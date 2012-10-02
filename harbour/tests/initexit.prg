/*
 * $Id$
 */

// Testing Harbour INIT and EXIT functions and initialization
// of static variables

STATIC static_var_accessed_in_INIT_function := 10000.15
MEMVAR _initStatics

PROCEDURE Main()

   STATIC static_var := "MAIN()"

   ? "Hello from:", static_var
   static_var_accessed_in_INIT_function++
   ? "global static=", static_var_accessed_in_INIT_function

   // Use PUBLIC variable created in INIT PROCEDURE
   ? "PUBLIC variable created in INIT PROCEDURE=", _initStatics

   RETURN

INIT FUNCTION SecondOne()
   STATIC static_var := "SECOND()"

   ? "Hello from:", static_var
   static_var_accessed_in_INIT_function++
   ? "global static=", static_var_accessed_in_INIT_function

   RETURN NIL

INIT FUNCTION Third()
   STATIC static_var := "THIRD()"

   ? "Hello from:", static_var
   static_var_accessed_in_INIT_function++
   ? "global static=", static_var_accessed_in_INIT_function

   RETURN NIL

EXIT FUNCTION Fifth()
   STATIC static_var := "FIFTH()"

   ? "Hello from:", static_var
   static_var_accessed_in_INIT_function--
   ? "global static=", static_var_accessed_in_INIT_function

   RETURN NIL

EXIT FUNCTION Sixth()
   STATIC static_var := "SIXTH()"

   ? "Hello from:", static_var
   static_var_accessed_in_INIT_function--
   ? "global static=", static_var_accessed_in_INIT_function

   RETURN NIL

INIT PROCEDURE _INITSTATICS()
   PUBLIC _initStatics := "_INITSTATICS"

   RETURN
