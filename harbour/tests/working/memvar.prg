// Basic test for memvars handling
//
// $Id$
//
PROCEDURE MAIN()

    Test1()
    __accept( "press Enter..." )
    Test2()
    __accept( "press Enter..." )
    Test3()
    __accept( "press Enter..." )
    Test4()
    __accept( "press Enter..." )
    Test5()
    __accept( "press Enter..." )
    Test6()
    __accept( "press Enter..." )
    Test7( 'value1', 2, .T. )
    __accept( "press Enter..." )
    Test8()

RETURN

/////////////////////////////////////////////////////////////////////////

PROCEDURE Test1()
// PUBLIC overrided by PRIVATE overrided by uninitialized PUBLIC
PUBL memvar1

    Qout( "==Test 1==PUBLIC -> PRIVATE -> PUBLIC" )
  Qout( memvar1 )
  memvar1 ='main'
  Qout( 'in MAIN=', memvar1 )
  Scope( memvar1 )
  Qout( 'back in MAIN=', memvar1 )
  Qout( "" )

RETURN

FUNCTION Scope( value )
PRIVA memvar1:='scope'

  Scope2()
  Qout( "in SCOPE=", memvar1 )

RETURN( value )

PROCEDURE Scope2()
PUBLIC memvar1
   Qout( "in SCOPE2=", memvar1 )
RETURN

///////////////////////////////////////////////////////////////////

PROCEDURE Test2()
// PUBLIC overrided by PUBLIC overrided by uninitialized PUBLIC
PUBLIC memvar2

    Qout( "==Test 2==PUBLIC -> PUBLIC -> PUBLIC" )
  Qout( memvar2 )
  memvar2 ='main'
  Qout( 'in MAIN=', memvar2 )
  Scope3( memvar2 )
  Qout( 'back in MAIN=', memvar2 )
  Qout( "" )

RETURN

FUNCTION Scope3( value )
PUBLIC memvar2:='scope'

  Scope4()
  Qout( "in SCOPE=", memvar2 )

RETURN( value )

PROCEDURE Scope4()
PUBLIC memvar2
   Qout( "in SCOPE2=", memvar2 )
RETURN

////////////////////////////////////////////////////////////////////////////

PROCEDURE Test3()
// PUBLIC overrided by PRIVATE overrided by initialized PUBLIC
PUBLIC memvar3

    Qout( "==Test 3==PUBLIC -> PRIVATE -> PUBLIC:=" )
  Qout( memvar3 )
  memvar3 ='main'
  Qout( 'in MAIN=', memvar3 )
  Scope5( memvar3 )
  Qout( 'back in MAIN=', memvar3 )
  Qout( "" )

RETURN

FUNCTION Scope5( value )
PRIVATE memvar3:='scope'

  Scope6()
  Qout( "in SCOPE=", memvar3 )

RETURN( value )

PROCEDURE Scope6()
PUBLIC memvar3:='scope2'
   Qout( "in SCOPE2=", memvar3 )
RETURN

///////////////////////////////////////////////////////////////////////

PROCEDURE Test4()
// PUBLIC overrided by PUBLIC overrided by initialized PUBLIC
PUBLIC memvar4

    Qout( "==Test 4==PUBLIC -> PUBLIC -> PUBLIC:=" )
  Qout( memvar4 )
  memvar4 ='main'
  Qout( 'in MAIN=', memvar4 )
  Scope7( memvar4 )
  Qout( 'back in MAIN=', memvar4 )
  Qout( "" )

RETURN

FUNCTION Scope7( value )
PUBLIC memvar4:='scope'

  Scope8()
  Qout( "in SCOPE=", memvar4 )

RETURN( value )

PROCEDURE Scope8()
PUBLIC memvar4:='scope2'
   Qout( "in SCOPE2=", memvar4 )
RETURN

///////////////////////////////////////////////////////////////////////

PROCEDURE TEST5()
PUBLIC mempublic, public3:=3
//PUBLIC public2[ 10 ]		//unsupported yet
PRIVATE memprivate
PARAMETERS memparam

  Qout( "==Test for memvars passed by reference and __PUBLIC/__PRIVATE " )
  Qout( "   uninitialized PUBLIC= ", mempublic )
//  Qout( "uninitialized PUBLIC array (first item)=", public2[1] )
  Qout( "initialized PUBLIC= ", public3 )
  Qout( "  uninitialized PRIVATE= ", memprivate )
  Qout( "uninitialized PARAMETER= ", memparam )
//  Qout( memnone )

  mempublic ='PUBLIC'
  Qout( "   PUBLIC with new value= ", mempublic )
  memprivate ='PRIVATE'
  Qout( "  PRIVATE with new value= ", memprivate )
  memparam ='PARAMETER'
  Qout( "PARAMETER with new value= ", memparam )
//  memnone =4
//  Qout( memnone )

  Qout( "   PUBLIC after passing by reference= ", UseVar( @mempublic ) )
  Qout( "  PRIVATE after passing by reference= ", UseVar( @memprivate ) )
  Qout( "PARAMETER after passing by reference= ", UseVar( @memparam ) )
//  Qout( Use( @memnone ) )

  Qout( "PUBLIC created by __PUBLIC function=", public1 )
  Qout( "" )

RETURN


FUNCTION UseVar( value )

  UseRef( @value )

  __PUBLIC( "public1" )	//, "public21" )
//  __PRIVATE( "private1", "private2", "private3" )
  __PRIVATE( {"private1", "private2", "private3"} )
  Qout( "undeclared PUBLIC created by __PUBLIC function=", public1 )
  Qout( "undeclared PRIVATE created by __PRIVATE function=", private1 )
  Qout( "undeclared PRIVATE created by __PRIVATE function=", private2 )
  Qout( "undeclared PRIVATE created by __PRIVATE function=", private3 )

  public1 :='public created by __PUBLIC'

  Qout( "" )

RETURN( value )

PROCEDURE UseRef( reference )

    reference +=' variable'

RETURN

//////////////////////////////////////////////////////////////////////

PROCEDURE Test6()
PUBLIC publCB
PRIVATE privVar:=' (PRIVATE in MAIN) '

    Qout( "== Test for detached PRIVATE variables" )
    DetachMemvar( 'detached memvar' )
    Qout( EVAL( publCB, 'in Main: ' ) )

RETURN

PROCEDURE DetachMemvar( cValue )
PRIVATE privVar:=' (PRIVATE in DetachMemvar) '

    publCB ={|x| x+privVar+cValue}
    Qout( EVAL( publCB, "in DetachMemvar: " ) )

RETURN

////////////////////////////////////////////////////////////////////////

PROCEDURE Test7( )
PARAMETERS para1, para2, para3
PARAM parameter1again

  Qout( "Parameter 1  =", para1 )
  Qout( "Parameter 2  =", para2 )
  Qout( "Parameter 3  =", para3 )
  Qout( "Parameter 4  =", parameter1again )  

RETURN

/////////////////////////////////////////////////////////////////////////

PROCEDURE Test8()
PRIVATE private1:='PRIVATE1'

  Qout( 'In Test8 before UsePriv' )
  Qout( "Private1 = ", private1 )
  UsePriv( private1 )
  Qout( 'In Test8 after UsePriv' )
  Qout( "Private1 = ", private1 )

    __accept( "press Enter..." )

  Qout( 'In Test8 before UsePriv with reference' )
  Qout( "Private1 = ", private1 )
  UsePriv( @private1 )
  Qout( 'In Test8 after UsePriv with reference' )
  Qout( "Private1 = ", private1 )


RETURN

PROCEDURE UsePriv()
PARAMETERS param1

  Qout( 'In UsePriv before UseParam' )
  Qout( "Private1 = ", private1 )
  Qout( "Param1   = ", param1 )
  UseParam()
  Qout( 'In UsePriv after UseParam' )
  Qout( "Private1 = ", private1 )
  Qout( "Param1   = ", param1 )

RETURN

PROCEDURE UseParam()
PARAMETER param2

  Qout( 'In UseParam before assignment' )
  Qout( "Private1 = ", private1 )
  Qout( "Param1   = ", param1 )
  Qout( "Param2   = ", param2 )
  param2 :='PARAM2'
  param1 :="new value"
  Qout( 'In UseParam after assignment' )
  Qout( "Private1 = ", private1 )
  Qout( "Param1   = ", param1 )
  Qout( "Param2   = ", param2 )

RETURN
