// Testing Harbour classes.ch commands

#include "classes.ch"

//--------------------------------------------------------------------//

function Main()

   local o := TTest():New( "one", "two" )

   ? o:ClassName()
   ? o:One
   ? o:Two

   o:Test()

return nil

//--------------------------------------------------------------------//

CLASS TTest

   DATA One, Two, Three

   METHOD New( One, Two )

   METHOD Test() INLINE QOut( "Hello" )

ENDCLASS

//--------------------------------------------------------------------//

METHOD New( One, Two ) CLASS TTest

   ::One = One
   ::Two = Two

return Self

//--------------------------------------------------------------------//
