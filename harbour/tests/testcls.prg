// Testing Harbour classes ON ERROR feature

#include "hbclass.ch"

function Main()

   local o := Test()

   o:Any( "Hello" ) // Any message is not defined for Class Test, but
           // it will invoke ON ERROR Any() method

   o:Any = 5  // Notice how __GetMessage() shows a underscored message
              // as we are setting a DATA value.
return nil

CLASS Test

   ON ERROR Any( uParam1 )

ENDCLASS

METHOD Any( uParam1 ) CLASS Test

   if PCount() > 0
      Alert( uParam1 )
   endif

   Alert( __GetMessage() )  // Shows the message that was sent to the object

return nil