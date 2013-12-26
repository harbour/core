// Testing Harbour classes and objects management
// be aware Harbour provides a much simpler way using Class HBClass

#include "hboo.ch"

PROCEDURE Main()

   LOCAL oObject := TAny():New()

   ? ValType( oObject )
   ? Len( oObject )       // 3 datas !
   ? oObject:ClassH()     // retrieves the handle of its class

   ? oObject:ClassName()  // retrieves its class name

   oObject:Test()         // This invokes the below defined Test function
   // See QSelf() and :: use
   ? oObject:cName

   oObject:DoNothing()    // a virtual method does nothing,
   // but it is very usefull for Classes building logic

   RETURN

FUNCTION TAny()         /* builds a class */

   STATIC s_hClass

   IF s_hClass == NIL
      s_hClass := __clsNew( "TANY", 3 )                 // cClassName, nDatas
      __clsAddMsg( s_hClass, "cName",      1, HB_OO_MSG_DATA )  // retrieve data
      __clsAddMsg( s_hClass, "_cName",     1, HB_OO_MSG_DATA )  // assign data. Note the '_'
      __clsAddMsg( s_hClass, "New",   @New(), HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "Test", @Test(), HB_OO_MSG_METHOD )
      __clsAddMsg( s_hClass, "DoNothing",  0, HB_OO_MSG_VIRTUAL )
   ENDIF

   /* warning: we are not defining datas names and methods yet */

   RETURN __clsInst( s_hClass )  // creates an object of this class

STATIC FUNCTION New()

   LOCAL Self := QSelf()

   ? ValType( Self )
   ? "Inside New()"

   ::cName := "Harbour OOP"

   RETURN Self

STATIC FUNCTION Test()

   LOCAL Self := QSelf()        // We access Self for this method

   ? "Test method invoked!"

   ? ::ClassName()     // :: means Self:  It is a Harbour built-in operator

   RETURN NIL
