//
// $Id$
//

// Testing Harbour classes and objects management
// be aware Harbour provides a much simpler way using Class TClass (source\rtl\class.prg)

#include "hboo.ch"

function Main()

   local oObject := TAny():New()

   QOut( ValType( oObject ) )
   QOut( Len( oObject ) )      // 3 datas !
   QOut( oObject:ClassH() )    // retrieves the handle of its class

   QOut( oObject:ClassName() ) // retrieves its class name

   oObject:Test()         // This invokes the below defined Test function
                          // See QSelf() and :: use
   QOut( oObject:cName )

   oObject:DoNothing()    // a virtual method does nothing,
                          // but it is very usefull for Classes building logic

return nil

function TAny()         /* builds a class */

   static hClass

   if hClass == nil
      hClass = __clsNew( "TANY", 3 )                 // cClassName, nDatas
      __clsAddMsg( hClass, "cName",      1, MET_DATA )  // retrieve data
      __clsAddMsg( hClass, "_cName",     1, MET_DATA )  // assign data. Note the '_'
      __clsAddMsg( hClass, "New",   @New(), MET_METHOD )
      __clsAddMsg( hClass, "Test", @Test(), MET_METHOD )
      __clsAddMsg( hClass, "DoNothing",  0, MET_VIRTUAL )
   endif

   /* warning: we are not defining datas names and methods yet */

return __clsInst( hClass )  // creates an object of this class

static function New()

   local Self := QSelf()

   QOut( ValType( Self ) )
   QOut( "Inside New()" )

   ::cName = "Harbour OOP"

return Self

static function Test()

   local Self := QSelf()        // We access Self for this method

   QOut( "Test method invoked!" )

   QOut( ::ClassName() )    // :: means Self:  It is a Harbour built-in operator

return nil

