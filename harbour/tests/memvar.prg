/*
 * $Id$
 */

// Basic test for memvars handling

MEMVAR MEMVAR
MEMVAR memvar1
MEMVAR memvar2
MEMVAR memvar3
MEMVAR memvar4
MEMVAR mempublic
MEMVAR memprivate
MEMVAR memparam
MEMVAR memfunc
MEMVAR public1
MEMVAR public2
MEMVAR public3
MEMVAR publCB
MEMVAR private1
MEMVAR private2
MEMVAR private3
MEMVAR privVar
MEMVAR para1
MEMVAR para2
MEMVAR para3
MEMVAR parameter1again
MEMVAR param1
MEMVAR param2
MEMVAR initmem

PROCEDURE Main()

   LOCAL main := 0

   HB_SYMBOL_UNUSED( main )

   Test1()
   __Accept( "press Enter..." )
   Test2()
   __Accept( "press Enter..." )
   Test3()
   __Accept( "press Enter..." )
   Test4()
   __Accept( "press Enter..." )
   Test5()
   __Accept( "press Enter..." )
   Test6()
   __Accept( "press Enter..." )
   Test7( "value1", 2, .T. )
   __Accept( "press Enter..." )
   Test8()
   __Accept( "press Enter..." )
   Test9()

   RETURN

//

PROCEDURE Test1()

   // PUBLIC overrided by PRIVATE overrided by uninitialized PUBLIC
   PUBL memvar1

   ? "==Test 1==PUBLIC -> PRIVATE -> PUBLIC"
   ? memvar1
   memvar1 := "main"
   ? "in MAIN : ", memvar1
   Scope( memvar1 )
   ? "back in MAIN : ", memvar1
   ?

   RETURN

FUNCTION Scope( value )

   PRIVA memvar1 := "scope"

   Scope2()
   ? "in SCOPE : ", memvar1

   RETURN value

PROCEDURE Scope2()

   PUBLIC memvar1

   ? "in SCOPE2 : ", memvar1

   RETURN

//

PROCEDURE Test2()

   // PUBLIC overrided by PUBLIC overrided by uninitialized PUBLIC
   PUBLIC memvar2

   ? "==Test 2==PUBLIC -> PUBLIC -> PUBLIC"
   ? memvar2
   memvar2 := "main"
   ? "in MAIN : ", memvar2
   Scope3( memvar2 )
   ? "back in MAIN : ", memvar2
   ?

   RETURN

FUNCTION Scope3( value )

   PUBLIC memvar2 := "scope"

   Scope4()
   ? "in SCOPE : ", memvar2

   RETURN value

PROCEDURE Scope4()

   PUBLIC memvar2

   ? "in SCOPE2 : ", memvar2

   RETURN

//

PROCEDURE Test3()

   // PUBLIC overrided by PRIVATE overrided by initialized PUBLIC
   PUBLIC memvar3

   ? "==Test 3==PUBLIC -> PRIVATE -> PUBLIC:="
   ? memvar3
   memvar3 := "main"
   ? "in MAIN : ", memvar3
   Scope5( memvar3 )
   ? "back in MAIN : ", memvar3
   ?

   RETURN

FUNCTION Scope5( value )

   PRIVATE memvar3 := "scope"

   Scope6()
   ? "in SCOPE : ", memvar3

   RETURN value

PROCEDURE Scope6()

   PUBLIC memvar3 := "scope2"

   ? "in SCOPE2 : ", memvar3

   RETURN

//

PROCEDURE Test4()

   // PUBLIC overrided by PUBLIC overrided by initialized PUBLIC
   PUBLIC memvar4

   ? "==Test 4==PUBLIC -> PUBLIC -> PUBLIC:="
   ? memvar4
   memvar4 := "main"
   ? "in MAIN : ", memvar4
   Scope7( memvar4 )
   ? "back in MAIN : ", memvar4
   ?

   RETURN

FUNCTION Scope7( value )

   PUBLIC memvar4 := "scope"

   Scope8()
   ? "in SCOPE : ", memvar4

   RETURN value

PROCEDURE Scope8()

   PUBLIC memvar4 := "scope2"

   ? "in SCOPE2 : ", memvar4

   RETURN

//

PROCEDURE TEST5()

   PUBLIC mempublic, public3 := 3

// PUBLIC public2[ 10 ]  //unsupported yet
   PRIVATE memprivate
   PARAMETERS memparam

   ? "==Test for memvars passed by reference and __PUBLIC/__PRIVATE "
   ? "   uninitialized PUBLIC : ", mempublic
// ? "uninitialized PUBLIC array (first item) : ", public2[ 1 ]
   ? "initialized PUBLIC : ", public3
   ? "  uninitialized PRIVATE : ", memprivate
   ? "uninitialized PARAMETER : ", memparam
// ? memnone

   mempublic := "PUBLIC"
   ? "   PUBLIC with new value : ", mempublic
   memprivate := "PRIVATE"
   ? "  PRIVATE with new value : ", memprivate
   memparam := "PARAMETER"
   ? "PARAMETER with new value : ", memparam
// memnone := 4
// ? memnone

   ? "   PUBLIC after passing by reference : ", UseVar( @mempublic )
   ? "  PRIVATE after passing by reference : ", UseVar( @memprivate )
   ? "PARAMETER after passing by reference : ", UseVar( @memparam )
// ? Use( @memnone )

#ifdef __HARBOUR__
   ? "PUBLIC created by __PUBLIC function : ", public1
#endif
   ?

   RETURN

FUNCTION UseVar( value )

   UseRef( @value )

#ifdef __HARBOUR__
   __mvPublic( "public1" )      // , "public21" )
// __mvPRIVATE( "private1", "private2", "private3" )
   __mvPrivate( { "private1", "private2", "private3" } )
   ? "undeclared PUBLIC created by __PUBLIC function : ", public1
   ? "undeclared PRIVATE created by __PRIVATE function : ", private1
   ? "undeclared PRIVATE created by __PRIVATE function : ", private2
   ? "undeclared PRIVATE created by __PRIVATE function : ", private3

   public1 := "public created by __PUBLIC"
#endif
   ?

   RETURN value

PROCEDURE UseRef( reference )

   reference += " variable"

   RETURN

//

PROCEDURE Test6()

   PUBLIC publCB
   PRIVATE privVar := " (PRIVATE in MAIN) "

   ? "== Test for detached PRIVATE variables"
   DetachMemvar( "detached memvar" )
   ? Eval( publCB, "in Main: " )

   RETURN

PROCEDURE DetachMemvar( cValue )

   PRIVATE privVar := " (PRIVATE in DetachMemvar) "

   publCB := {| x | x + privVar + cValue }
   ? Eval( publCB, "in DetachMemvar: " )

   RETURN

//

PROCEDURE Test7( )

   PARAMETERS para1, para2, para3
   PARAM parameter1again

   ? "Parameter 1 : ", para1
   ? "Parameter 2 : ", para2
   ? "Parameter 3 : ", para3
   ? "Parameter 4 : ", parameter1again

   RETURN

//

PROCEDURE Test8()

   PRIVATE private1 := "PRIVATE1"

   ? "In Test8 before UsePriv"
   ? "Private1 : ", private1
   UsePriv( private1 )
   ? "In Test8 after UsePriv"
   ? "Private1 : ", private1

   __Accept( "press Enter..." )

   ? "In Test8 before UsePriv with reference"
   ? "Private1 : ", private1
   UsePriv( @private1 )
   ? "In Test8 after UsePriv with reference"
   ? "Private1 : ", private1

   RETURN

PROCEDURE UsePriv()

   PARAMETERS param1

   ? "In UsePriv before UseParam"
   ? "Private1 : ", private1
   ? "Param1   : ", param1
   UseParam()
   ? "In UsePriv after UseParam"
   ? "Private1 : ", private1
   ? "Param1   : ", param1

   RETURN

PROCEDURE UseParam()

   PARAMETERS param2

   ? "In UseParam before assignment"
   ? "Private1 : ", private1
   ? "Param1   : ", param1
   ? "Param2   : ", param2
   param2 := "PARAM2"
   param1 := "new value"
   ? "In UseParam after assignment"
   ? "Private1 : ", private1
   ? "Param1   : ", param1
   ? "Param2   : ", param2

   RETURN

//

PROCEDURE Test9()

   PUBLIC MEMVAR
   PUBLIC memfunc

   memvar := 19

   ? "Variable with the name of module (memvar) : ", memvar

   memfunc := 33
   ? "Variable with the name of function : ", memfunc
   ? "Return value from a function : ", memfunc( 9 )

// mem()

   RETURN

STATIC FUNCTION memfunc( memfunc )

   RETURN memfunc * memfunc

INIT PROCEDURE initmem()

   PARAMETERS MEMVAR
   PARAMETERS initmem

   ? "Tests for PARAMETERS, PRIVATE nad PUBLIC variables"
   ?
   ? "in INIT function - Passed parameter : ", memvar
   ? "in INIT function - Passed parameter with different name : ", initmem
   ?

   RETURN
