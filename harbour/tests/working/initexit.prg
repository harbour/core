//
// $Id$
//

// Testing Harbour INIT and EXIT functions and initialization 
// of static variables

STATIC static_var_accessed_in_INIT_function:=10000.15
MEMVAR _initStatics

function Main()
STATIC static_var:="MAIN()"

   QOut( "Hello from:", static_var )
   static_var_accessed_in_INIT_function++
   Qout( "global static=", static_var_accessed_in_INIT_function )

   // Use PUBLIC variable created in INIT procedure
   Qout( "PUBLIC variable created in INIT procedure=", _initStatics )

return nil

init function SecondOne()
STATIC static_var:="SECOND()"

   QOut( "Hello from:", static_var )
   static_var_accessed_in_INIT_function++
   Qout( "global static=", static_var_accessed_in_INIT_function )

return nil

init function Third()
STATIC static_var:="THIRD()"

   QOut( "Hello from:", static_var )
   static_var_accessed_in_INIT_function++
   Qout( "global static=", static_var_accessed_in_INIT_function )

return nil

exit function Fifth()
STATIC static_var:="FIFTH()"

   QOut( "Hello from:", static_var )
   static_var_accessed_in_INIT_function--
   Qout( "global static=", static_var_accessed_in_INIT_function )

return nil

exit function Sixth()
STATIC static_var:="SIXTH()"

   QOut( "Hello from:", static_var )
   static_var_accessed_in_INIT_function--
   Qout( "global static=", static_var_accessed_in_INIT_function )

return nil

INIT PROCEDURE _INITSTATICS()
PUBLIC _initStatics:="_INITSTATICS"
RETURN