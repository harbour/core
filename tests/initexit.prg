// Testing Harbour INIT and EXIT functions and initialization
// of static variables

STATIC s_static_var_accessed_in_INIT_function := 10000.15
MEMVAR p_initStatics

PROCEDURE Main()

   STATIC s_static_var := "Main()"

   ? "Hello from:", s_static_var
   s_static_var_accessed_in_INIT_function++
   ? "global static=", s_static_var_accessed_in_INIT_function

   // Use PUBLIC variable created in INIT PROCEDURE
   ? "PUBLIC variable created in INIT PROCEDURE=", p_initStatics

   RETURN

INIT FUNCTION SecondOne()

   STATIC s_static_var := "SecondOne()"

   ? "Hello from:", s_static_var
   s_static_var_accessed_in_INIT_function++
   ? "global static=", s_static_var_accessed_in_INIT_function

   RETURN NIL

INIT FUNCTION Third()

   STATIC s_static_var := "Third()"

   ? "Hello from:", s_static_var
   s_static_var_accessed_in_INIT_function++
   ? "global static=", s_static_var_accessed_in_INIT_function

   RETURN NIL

EXIT FUNCTION Fifth()

   STATIC s_static_var := "Fifth()"

   ? "Hello from:", s_static_var
   s_static_var_accessed_in_INIT_function--
   ? "global static=", s_static_var_accessed_in_INIT_function

   RETURN NIL

EXIT FUNCTION Sixth()

   STATIC s_static_var := "Sixth()"

   ? "Hello from:", s_static_var
   s_static_var_accessed_in_INIT_function--
   ? "global static=", s_static_var_accessed_in_INIT_function

   RETURN NIL

INIT PROCEDURE initStatics()

   PUBLIC p_initStatics := "p_initStatics"

   RETURN
