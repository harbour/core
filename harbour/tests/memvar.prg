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
   Test7( 'value1', 2, .T. )
   __Accept( "press Enter..." )
   Test8()
   __Accept( "press Enter..." )
   Test9()

   RETURN

   /////////////////////////////////////////////////////////////////////////

PROCEDURE Test1()

   // PUBLIC overrided by PRIVATE overrided by uninitialized PUBLIC
   PUBL memvar1

   QOut( "==Test 1==PUBLIC -> PRIVATE -> PUBLIC" )
   QOut( memvar1 )
   memvar1 = 'main'
   QOut( 'in MAIN=', memvar1 )
   Scope( memvar1 )
   QOut( 'back in MAIN=', memvar1 )
   QOut( "" )

   RETURN

FUNCTION Scope( value )

   PRIVA memvar1 := 'scope'

   Scope2()
   QOut( "in SCOPE=", memvar1 )

   RETURN( value )

PROCEDURE Scope2()

   PUBLIC memvar1

   QOut( "in SCOPE2=", memvar1 )

   RETURN

   ///////////////////////////////////////////////////////////////////

PROCEDURE Test2()

   // PUBLIC overrided by PUBLIC overrided by uninitialized PUBLIC
   PUBLIC memvar2

   QOut( "==Test 2==PUBLIC -> PUBLIC -> PUBLIC" )
   QOut( memvar2 )
   memvar2 = 'main'
   QOut( 'in MAIN=', memvar2 )
   Scope3( memvar2 )
   QOut( 'back in MAIN=', memvar2 )
   QOut( "" )

   RETURN

FUNCTION Scope3( value )

   PUBLIC memvar2 := 'scope'

   Scope4()
   QOut( "in SCOPE=", memvar2 )

   RETURN( value )

PROCEDURE Scope4()

   PUBLIC memvar2

   QOut( "in SCOPE2=", memvar2 )

   RETURN

   ////////////////////////////////////////////////////////////////////////////

PROCEDURE Test3()

   // PUBLIC overrided by PRIVATE overrided by initialized PUBLIC
   PUBLIC memvar3

   QOut( "==Test 3==PUBLIC -> PRIVATE -> PUBLIC:=" )
   QOut( memvar3 )
   memvar3 = 'main'
   QOut( 'in MAIN=', memvar3 )
   Scope5( memvar3 )
   QOut( 'back in MAIN=', memvar3 )
   QOut( "" )

   RETURN

FUNCTION Scope5( value )

   PRIVATE memvar3 := 'scope'

   Scope6()
   QOut( "in SCOPE=", memvar3 )

   RETURN( value )

PROCEDURE Scope6()

   PUBLIC memvar3 := 'scope2'

   QOut( "in SCOPE2=", memvar3 )

   RETURN

   ///////////////////////////////////////////////////////////////////////

PROCEDURE Test4()

   // PUBLIC overrided by PUBLIC overrided by initialized PUBLIC
   PUBLIC memvar4

   QOut( "==Test 4==PUBLIC -> PUBLIC -> PUBLIC:=" )
   QOut( memvar4 )
   memvar4 = 'main'
   QOut( 'in MAIN=', memvar4 )
   Scope7( memvar4 )
   QOut( 'back in MAIN=', memvar4 )
   QOut( "" )

   RETURN

FUNCTION Scope7( value )

   PUBLIC memvar4 := 'scope'

   Scope8()
   QOut( "in SCOPE=", memvar4 )

   RETURN( value )

PROCEDURE Scope8()

   PUBLIC memvar4 := 'scope2'

   QOut( "in SCOPE2=", memvar4 )

   RETURN

   ///////////////////////////////////////////////////////////////////////

PROCEDURE TEST5()

   PUBLIC mempublic, public3 := 3

// PUBLIC public2[ 10 ]  //unsupported yet
   PRIVATE memprivate
   PARAMETERS memparam

   QOut( "==Test for memvars passed by reference and __PUBLIC/__PRIVATE " )
   QOut( "   uninitialized PUBLIC= ", mempublic )
   // QOut( "uninitialized PUBLIC array (first item)=", public2[1] )
   QOut( "initialized PUBLIC= ", public3 )
   QOut( "  uninitialized PRIVATE= ", memprivate )
   QOut( "uninitialized PARAMETER= ", memparam )
   // QOut( memnone )

   mempublic = 'PUBLIC'
   QOut( "   PUBLIC with new value= ", mempublic )
   memprivate = 'PRIVATE'
   QOut( "  PRIVATE with new value= ", memprivate )
   memparam = 'PARAMETER'
   QOut( "PARAMETER with new value= ", memparam )
   //  memnone =4
   //  Qout( memnone )

   QOut( "   PUBLIC after passing by reference= ", UseVar( @mempublic ) )
   QOut( "  PRIVATE after passing by reference= ", UseVar( @memprivate ) )
   QOut( "PARAMETER after passing by reference= ", UseVar( @memparam ) )
   //  Qout( Use( @memnone ) )

#ifdef __HARBOUR__
   QOut( "PUBLIC created by __PUBLIC function=", public1 )
#endif
   QOut( "" )

   RETURN

FUNCTION UseVar( value )

   UseRef( @value )

#ifdef __HARBOUR__
   __mvPublic( "public1" )      //, "public21" )
   //  __mvPRIVATE( "private1", "private2", "private3" )
   __mvPrivate( { "private1", "private2", "private3" } )
   QOut( "undeclared PUBLIC created by __PUBLIC function=", public1 )
   QOut( "undeclared PRIVATE created by __PRIVATE function=", private1 )
   QOut( "undeclared PRIVATE created by __PRIVATE function=", private2 )
   QOut( "undeclared PRIVATE created by __PRIVATE function=", private3 )

   public1 := 'public created by __PUBLIC'
#endif
   QOut( "" )

   RETURN( value )

PROCEDURE UseRef( reference )

   reference += ' variable'

   RETURN

   //////////////////////////////////////////////////////////////////////

PROCEDURE Test6()

   PUBLIC publCB
   PRIVATE privVar := ' (PRIVATE in MAIN) '

   QOut( "== Test for detached PRIVATE variables" )
   DetachMemvar( 'detached memvar' )
   QOut( Eval( publCB, 'in Main: ' ) )

   RETURN

PROCEDURE DetachMemvar( cValue )

   PRIVATE privVar := ' (PRIVATE in DetachMemvar) '

   publCB = {| x | x + privVar + cValue }
   QOut( Eval( publCB, "in DetachMemvar: " ) )

   RETURN

   ////////////////////////////////////////////////////////////////////////

PROCEDURE Test7( )

   PARAMETERS para1, para2, para3
   PARAM parameter1again

   QOut( "Parameter 1  =", para1 )
   QOut( "Parameter 2  =", para2 )
   QOut( "Parameter 3  =", para3 )
   QOut( "Parameter 4  =", parameter1again )

   RETURN

   /////////////////////////////////////////////////////////////////////////

PROCEDURE Test8()

   PRIVATE private1 := 'PRIVATE1'

   QOut( 'In Test8 before UsePriv' )
   QOut( "Private1 = ", private1 )
   UsePriv( private1 )
   QOut( 'In Test8 after UsePriv' )
   QOut( "Private1 = ", private1 )

   __Accept( "press Enter..." )

   QOut( 'In Test8 before UsePriv with reference' )
   QOut( "Private1 = ", private1 )
   UsePriv( @private1 )
   QOut( 'In Test8 after UsePriv with reference' )
   QOut( "Private1 = ", private1 )

   RETURN

PROCEDURE UsePriv()

   PARAMETERS param1

   QOut( 'In UsePriv before UseParam' )
   QOut( "Private1 = ", private1 )
   QOut( "Param1   = ", param1 )
   UseParam()
   QOut( 'In UsePriv after UseParam' )
   QOut( "Private1 = ", private1 )
   QOut( "Param1   = ", param1 )

   RETURN

PROCEDURE UseParam()

   PARAMETER param2

   QOut( 'In UseParam before assignment' )
   QOut( "Private1 = ", private1 )
   QOut( "Param1   = ", param1 )
   QOut( "Param2   = ", param2 )
   param2 := 'PARAM2'
   param1 := "new value"
   QOut( 'In UseParam after assignment' )
   QOut( "Private1 = ", private1 )
   QOut( "Param1   = ", param1 )
   QOut( "Param2   = ", param2 )

   RETURN

   //////////////////////////////////////////////////////////////////////

PROCEDURE TEST9()

   PUBLIC MEMVAR
   PUBLIC memfunc

   memvar := 19

   QOut( "Variable with the name of module (memvar)=", memvar )

   memfunc := 33
   QOut( "Variable with the name of function =", memfunc )
   QOut( "Return value from a function=", memfunc( 9 ) )

   //  mem()

   RETURN

STATIC FUNCTION memfunc( memfunc )

   RETURN memfunc * memfunc

   INIT PROCEDURE initmem()
   PARA MEMVAR
   PARA initmem
   QOut( "Tests for PARAMETERS, PRIVATE nad PUBLIC variables" )
   QOut( "" )
   QOut( 'in INIT function - Passed parameter = ', memvar )
   QOut( 'in INIT function - Passed parameter with different name = ', initmem )
   QOut( "" )

   RETURN
