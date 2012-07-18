/*
 * $Id$
 */

// Testing Harbour INIT and EXIT functions and initialization
// of static variables

STATIC static_var_accessed_in_INIT_function := 10000.15
MEMVAR _initStatics

PROCEDURE Main()

   STATIC static_var := "MAIN()"

   QOut( "Hello from:", static_var )
   static_var_accessed_in_INIT_function ++
   QOut( "global static=", static_var_accessed_in_INIT_function )

   // Use PUBLIC variable created in INIT PROCEDURE
   QOut( "PUBLIC variable created in INIT PROCEDURE=", _initStatics )

   RETURN

INIT FUNCTION SecondOne()
   STATIC static_var := "SECOND()"

   QOut( "Hello from:", static_var )
   static_var_accessed_in_INIT_function ++
   QOut( "global static=", static_var_accessed_in_INIT_function )

   RETURN nil

INIT FUNCTION Third()
   STATIC static_var := "THIRD()"

   QOut( "Hello from:", static_var )
   static_var_accessed_in_INIT_function ++
   QOut( "global static=", static_var_accessed_in_INIT_function )

   RETURN nil

EXIT FUNCTION Fifth()
   STATIC static_var := "FIFTH()"

   QOut( "Hello from:", static_var )
   static_var_accessed_in_INIT_function --
   QOut( "global static=", static_var_accessed_in_INIT_function )

   RETURN nil

EXIT FUNCTION Sixth()
   STATIC static_var := "SIXTH()"

   QOut( "Hello from:", static_var )
   static_var_accessed_in_INIT_function --
   QOut( "global static=", static_var_accessed_in_INIT_function )

   RETURN nil

INIT PROCEDURE _INITSTATICS()
   PUBLIC _initStatics := "_INITSTATICS"

   RETURN
