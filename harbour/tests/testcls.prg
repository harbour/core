// Testing Harbour classes ON ERROR feature

#include "hbclass.ch"

function Main()

   local o := Test()

   o:Any() // Any message is not defined for Class Test, but
           // it will invoke ON ERROR Any() method

return nil

CLASS Test

   ON ERROR Any( cMsg )

ENDCLASS

METHOD Any( cMsg ) CLASS Test

   Alert( "Inside Any" )

return nil