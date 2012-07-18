/*
 * $Id$
 */

// Testing Harbour classes and objects management
// be aware Harbour provides a much simpler way using Class HBClass

#include "hboo.ch"

PROCEDURE Main()

   LOCAL oObject := TAny():New()

   QOut( ValType( oObject ) )
   QOut( Len( oObject ) )      // 3 datas !
   QOut( oObject:ClassH() )    // retrieves the handle of its class

   QOut( oObject:ClassName() ) // retrieves its class name

   oObject:Test()         // This invokes the below defined Test function
   // See QSelf() and :: use
   QOut( oObject:cName )

   oObject:DoNothing()    // a virtual method does nothing,
   // but it is very usefull for Classes building logic

   RETURN

FUNCTION TAny()         /* builds a class */

   STATIC hClass

   IF hClass == nil
      hClass := __clsNew( "TANY", 3 )                 // cClassName, nDatas
      __clsAddMsg( hClass, "cName",      1, HB_OO_MSG_DATA )  // retrieve data
      __clsAddMsg( hClass, "_cName",     1, HB_OO_MSG_DATA )  // assign data. Note the '_'
      __clsAddMsg( hClass, "New",   @New(), HB_OO_MSG_METHOD )
      __clsAddMsg( hClass, "Test", @Test(), HB_OO_MSG_METHOD )
      __clsAddMsg( hClass, "DoNothing",  0, HB_OO_MSG_VIRTUAL )
   ENDIF

   /* warning: we are not defining datas names and methods yet */

   RETURN __clsInst( hClass )  // creates an object of this class

STATIC FUNCTION New()

   LOCAL Self := QSelf()

   QOut( ValType( Self ) )
   QOut( "Inside New()" )

   ::cName := "Harbour OOP"

   RETURN Self

STATIC FUNCTION Test()

   LOCAL Self := QSelf()        // We access Self for this method

   QOut( "Test method invoked!" )

   QOut( ::ClassName() )    // :: means Self:  It is a Harbour built-in operator

   RETURN nil
